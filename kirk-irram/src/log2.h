/*
 * Copyright 2009-2017 Franz Brausse
 *
 * LOG2, LOG2_CEIL, LOG2_U[L[L]], LOG2_k for k in {2,4,8,16,32,64}
 * using builtins if available, divide and conquer otherwise.
 *
 * LOG2_CEIL(n) returns ceil(log_2(n)); all other macros return floor(log_2(n)).
 * All except LOG2_k may not be used for n == 0.
 *
 * Macros may evaluate their arguments more than once!
 */

#ifndef LOG2_H
#define LOG2_H

/* CHAR_BIT and U(INT|LONG[_LONG])_MAX */
#ifdef __cplusplus
# include <climits>
#else
# include <limits.h>
#endif

/* LOG2_k(n) returns floor(log2(n)) and is valid for values 0 <= n < 1 << k */
#define LOG2_2(n)	((n)&0x2               ? 1                 :0)
#define LOG2_4(n)	((n)&0xc               ? 2+LOG2_2 ((n)>>2 ):LOG2_2(n))
#define LOG2_8(n)	((n)&0xf0              ? 4+LOG2_4 ((n)>>4 ):LOG2_4(n))
#define LOG2_16(n)	((n)&0xff00            ? 8+LOG2_8 ((n)>>8 ):LOG2_8(n))
#define LOG2_32(n)	((n)&0xffff0000        ?16+LOG2_16((n)>>16):LOG2_16(n))
#define LOG2_64(n)	((n)&0xffffffff00000000?32+LOG2_32((n)>>32):LOG2_32(n))

#if defined(__GNUC__) || defined(__clang__)
# define LOG2_U(n)	(CHAR_BIT*sizeof(unsigned)-1-__builtin_clz(n))
# define LOG2_UL(n)	(CHAR_BIT*sizeof(unsigned long)-1-__builtin_clzl(n))
# if defined(__cplusplus) || defined(__STDC__) && (__STDC_VERSION-0) >= 199901L
#  define LOG2_ULL(n)	(CHAR_BIT*sizeof(unsigned long long)-1-__builtin_clzll(n))
# endif

#else /* !defined(__GNUC__) && !defined(__clang__) */

# if defined(__cplusplus) || defined(__STDC__) && (__STDC_VERSION-0) >= 199901L
#  if ULLONG_MAX/2 <= 1ULL << (64-1)
#   define LOG2_ULL(n)	LOG2_64(n)
#  else
#   error unable to def LOG2_ULL
#  endif
# endif

# if ULONG_MAX/2 <= 1UL << (32-1)
#  define LOG2_UL(n)	LOG2_32(n)
# elif ULONG_MAX/2 <= 1UL << (64-1)
#  define LOG2_UL(n)	LOG2_64(n)
# else
#  error unable to def LOG2_UL
# endif

# if UINT_MAX/2 <= 1U << (16-1)
#  define LOG2_U(n)	LOG2_16(n)
# elif UINT_MAX/2 <= 1U << (32-1)
#  define LOG2_U(n)	LOG2_32(n)
# elif UINT_MAX/2 <= 1U << (64-1)
#  define LOG2_U(n)	LOG2_64(n)
# else
#  error unable to def LOG2_U
# endif
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__-0) >= 201112L
# define LOG2(n)							\
	((unsigned)_Generic((n),					\
		unsigned: LOG2_U((unsigned)(n)),			\
		const unsigned: LOG2_U((unsigned)(n)),			\
		unsigned long: LOG2_UL((unsigned long)(n)),		\
		const unsigned long: LOG2_UL((unsigned long)(n)),	\
		unsigned long long: LOG2_ULL((unsigned long long)(n)),	\
		const unsigned long long: LOG2_ULL((unsigned long long)(n))))
#elif defined(__cplusplus)
template <typename T> static inline unsigned LOG2(T);
# if __cplusplus < 201103L
template <> inline unsigned LOG2(unsigned n) { return LOG2_U(n); }
template <> inline unsigned LOG2(unsigned long n) { return LOG2_UL(n); }
template <> inline unsigned LOG2(unsigned long long n) { return LOG2_ULL(n); }
# else
template <> constexpr unsigned LOG2(unsigned n) { return LOG2_U(n); }
template <> constexpr unsigned LOG2(unsigned long n) { return LOG2_UL(n); }
template <> constexpr unsigned LOG2(unsigned long long n) { return LOG2_ULL(n); }
# endif
#else
# undef LOG2 /* can't express type-generic LOG2() */
#endif

#define LOG2_CEIL(n)	(((n) & ((n)-1)) ? 1+LOG2((n)-1) : LOG2(n))

unsigned k = LOG2_UL(5UL);

#endif
