
#ifndef KIRK_COMMON_H
#define KIRK_COMMON_H

#define KIRK_VER_MAJOR	0
#define KIRK_VER_MINOR	0
#define KIRK_VERSION	(KIRK_VER_MAJOR << 16 | KIRK_VER_MINOR << 8 | 0 << 0)

#if defined(KIRK_DLL) && (defined(_WIN32) || defined(__BEOS__))
# define KIRK_IMPORT	__declspec(dllimport)
# define KIRK_EXPORT	__declspec(dllexport)
#else
# define KIRK_IMPORT
# define KIRK_EXPORT
#endif

#ifdef __cplusplus
# if defined(__GNUC__) || defined(__clang__)
#  define restrict __restrict__
# elif defined(_MSC_VER)
#  define restrict __restrict
# else
#  define restrict
# endif
#elif !defined(__STDC_VERSION__) || (__STDC_VERSION__ -0) < 199901L
# error need at least C99
#endif

#ifndef ARRAY_SIZE
# define ARRAY_SIZE(...)	(sizeof(__VA_ARGS__)/sizeof(*(__VA_ARGS__)))
#endif

#endif
