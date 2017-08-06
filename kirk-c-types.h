
#ifndef KIRK_C_TYPES_H
#define KIRK_C_TYPES_H

#include <stdint.h>	/* [u]int32_t */
#include <string.h>	/* memcpy() */
#include <float.h>	/* DBL_MANT_DIG */
#include <math.h>	/* (ld|fr)exp() */
#include <mpfr.h>

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

#ifdef KIRK_INTERNAL
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#undef _KIRK_HAVE_CONTEXT /* no kirk_context_t support, yet */

#ifdef __cplusplus
# ifdef KIRK_INTERNAL
#  error support for extern-inline is required to compile kirk as C++; \
         it has not been implemented
# endif
extern "C" {
#elif !defined(__STDC_VERSION__) || (__STDC_VERSION__ -0) < 199901L
# error need at least C99
#endif

#define KIRK_CONCAT3(a,b,c)			a ## b ## c
#define KIRK_UINT_TYPE(b)			KIRK_CONCAT3(uint,b,_t)
#define KIRK_INT_TYPE(b)			KIRK_CONCAT3(int,b,_t)

/* <  0: error, negative errno
 * >= 0: success */
typedef int32_t                         kirk_ret_t;

#define KIRK_BOUND_MANT_BITS		GMP_NUMB_BITS
#define KIRK_BOUND_EXP_MIN		(kirk_bound_exp_t)KIRK_BOUND_MANT_1HALF
#define KIRK_BOUND_EXP_MAX		(kirk_bound_exp_t)~KIRK_BOUND_MANT_1HALF
typedef KIRK_UINT_TYPE(GMP_LIMB_BITS)	kirk_bound_mant_t;
typedef KIRK_INT_TYPE(GMP_LIMB_BITS)	kirk_bound_exp_t;

typedef struct kirk_bound_t             kirk_bound_t;
typedef struct kirk_apx_t               kirk_apx_t;
typedef struct kirk_real_t              kirk_real_t;
typedef struct kirk_real_class_t        kirk_real_class_t;

#ifdef _KIRK_HAVE_CONTEXT
typedef struct kirk_context_t           kirk_context_t;
typedef struct kirk_context_class_t     kirk_context_class_t;
#endif

typedef  int32_t kirk_abs_t; /* +infty -> -infty: absolute accuracy, fast Cauchy */
typedef uint32_t kirk_eff_t; /*      0 -> +infty: no rate specified */

/* ---------------------------------------------------------------------------
 * kirk API
 * -------------------------------------------------------------------------- */

KIRK_API        uint32_t  kirk_version(void);

#ifdef _KIRK_HAVE_CONTEXT
/* Implementations must provide an own "constructor" method with arbitrary
 * arguments, usually <NAME>_kirk_context_create(), where <NAME> is the string
 * returned by kirk_context_get_name(ctx), which returns a pointer to an
 * kirk_context_t structure */

KIRK_API inline kirk_context_t * kirk_context_ref  (kirk_context_t *);
KIRK_API inline void             kirk_context_unref(kirk_context_t *);
KIRK_API inline const char *     kirk_context_get_name(const kirk_context_t *);
/*
KIRK_API inline kirk_ret_t kirk_context_spawn   (kirk_context_t *,
                                                 kirk_computation_t *);
KIRK_API inline kirk_ret_t kirk_context_detach  (kirk_context_t *,
                                                 kirk_computation_t *);

*/
#endif

/* multi-valued: b <  c -> returns 1
 *               b == c -> returns 0 or 1
 *               b >  c -> returns 0 */
KIRK_API        int              kirk_bound_less     (const kirk_bound_t *,
                                                      const kirk_bound_t *);
KIRK_API        int              kirk_bound_less_2exp(const kirk_bound_t *,
                                                      kirk_bound_exp_t);
KIRK_API inline void             kirk_bound_set_zero(kirk_bound_t *b);
KIRK_API inline void             kirk_bound_set_d(kirk_bound_t *r, double d);
KIRK_API inline double           kirk_bound_get_d(const kirk_bound_t *b);
KIRK_API inline void             kirk_bound_nextafter(kirk_bound_t *r,
                                                      const kirk_bound_t *b);
KIRK_API        void             kirk_bound_add(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
KIRK_API inline void             kirk_bound_add_2exp(kirk_bound_t *r,
                                                     const kirk_bound_t *b,
                                                     kirk_bound_exp_t e);
KIRK_API        void             kirk_bound_mul(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
KIRK_API inline void             kirk_bound_shift(kirk_bound_t *r,
                                                  const kirk_bound_t *b,
                                                  kirk_bound_exp_t e);
KIRK_API inline void             kirk_bound_max(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
/* \requires mpfr_number_p(x): x must be neither NaN nor an infinity
 * \ensures  b <= |x| < (b->mantissa+1)/2^C * 2^(b->exponent) */
KIRK_API inline void             kirk_bound_mpfr_size(kirk_bound_t *b,
                                                      mpfr_srcptr x);

KIRK_API inline void kirk_apx_init (kirk_apx_t *);
KIRK_API inline void kirk_apx_init2(kirk_apx_t *, mpfr_prec_t);
KIRK_API inline void kirk_apx_cpy  (kirk_apx_t *, const kirk_apx_t *);
KIRK_API        void kirk_apx_set  (kirk_apx_t *, mpfr_srcptr, const kirk_bound_t *);
KIRK_API inline void kirk_apx_fini (kirk_apx_t *);

KIRK_API inline kirk_real_t * kirk_real_ref  (kirk_real_t *);
KIRK_API inline void          kirk_real_unref(kirk_real_t *);

KIRK_API inline void          kirk_real_apx_abs(const kirk_real_t *,
                                                kirk_apx_t *,
                                                kirk_abs_t);

KIRK_API inline void          kirk_real_apx_eff(const kirk_real_t *,
                                                kirk_apx_t *,
                                                kirk_eff_t);

/* helper functions to implement all of the required approx functions */

/* Use effort-based unbounded search to approximate r. */
KIRK_API        void          kirk_real_apx_abs_eff(const kirk_real_t *r,
                                                    kirk_apx_t *,
                                                    kirk_abs_t);

KIRK_API inline void          kirk_real_apx_eff_abs(const kirk_real_t *,
                                                    kirk_apx_t *,
                                                    kirk_eff_t);

/* ==========================================================================
 * KIRK data types
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * instantiated library / KIRK context
 * -------------------------------------------------------------------------- */

#ifdef _KIRK_HAVE_CONTEXT /* unused at the moment, may be required later */
struct kirk_context_class_t {
	/* mandatory interface */
	kirk_context_t * (*ref        )(kirk_context_t *);
	void             (*unref      )(kirk_context_t *);
	const char *     (*get_name   )(const kirk_context_t *);

	/* optional interface */
	kirk_ret_t       (*get_feature)(const kirk_context_t *, const char *key);
	kirk_ret_t       (*set_feature)(kirk_context_t *,
	                                const char *key, const char *value);
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

struct kirk_context_t {
	const kirk_context_class_t *clazz;
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};
#endif

/* --------------------------------------------------------------------------
 * Reals
 * -------------------------------------------------------------------------- */

struct kirk_real_class_t {
	kirk_real_t * (*ref    )(kirk_real_t *);
	void          (*unref  )(kirk_real_t *);

	/* signatures for approximating reals */
	/* all of the below three function pointers must be non-NULL, also see
	 * the kirk_real_apx_(abs|eff)_(abs|eff)() helper functions
	 * In order to represent Reals, these functions must not fail */
	void (*apx_abs)(const kirk_real_t *, kirk_apx_t *, kirk_abs_t);
	void (*apx_eff)(const kirk_real_t *, kirk_apx_t *, kirk_eff_t);

	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

/* a real */
struct kirk_real_t {
	const kirk_real_class_t *clazz;
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

/* --------------------------------------------------------------------------
 * approximations
 * -------------------------------------------------------------------------- */

#define KIRK_BOUND_MANT_1HALF		~(~(kirk_bound_mant_t)0 >> 1)

/* bounds: m/C * 2**e, where C = 2^KIRK_BOUND_MANT_BITS;
 * if m != 0 then highest bit of m is set */
struct kirk_bound_t {
	kirk_bound_exp_t  exponent;
	kirk_bound_mant_t mantissa;
};

#define KIRK_BOUND_INIT	{ 0, 0, }

/* approximation type: [center +/- radius] */
struct kirk_apx_t {
	kirk_bound_t radius;
	mpfr_t       center;
};

/* ==========================================================================
 * inline definitions
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * kirk_bound_t
 * -------------------------------------------------------------------------- */

#define KIRK_MPFR_N_LIMBS(p)	(1 + ((p)-1) / GMP_NUMB_BITS)
#define KIRK_MPFR_MSL(x)	(KIRK_MPFR_N_LIMBS(mpfr_get_prec(x))-1)

inline void kirk_bound_set_zero(kirk_bound_t *r)
{
	r->exponent = KIRK_BOUND_EXP_MIN;
	r->mantissa = 0;
}

inline void kirk_bound_nextafter(kirk_bound_t *r, const kirk_bound_t *b)
{
	r->exponent = b->exponent;
	r->mantissa = b->mantissa + 1;
	if (!r->mantissa) {
		r->mantissa = KIRK_BOUND_MANT_1HALF;
		r->exponent++;
	}
}

inline void kirk_bound_set_d(kirk_bound_t *r, double d)
{
	int e;
	d = frexp(d, &e);
	r->exponent = e;
	r->mantissa = ldexp(d, KIRK_BOUND_MANT_BITS);
	if (KIRK_BOUND_MANT_BITS < DBL_MANT_DIG)
		kirk_bound_nextafter(r, r);
}

inline double kirk_bound_get_d(const kirk_bound_t *b)
{
	double d = ldexp(b->mantissa, -KIRK_BOUND_MANT_BITS);
	if (DBL_MANT_DIG < KIRK_BOUND_MANT_BITS)
		d = nextafter(d, 1);
	return ldexp(d, b->exponent);
}

inline void kirk_bound_add_2exp(kirk_bound_t *r,
                                const kirk_bound_t *b,
                                kirk_bound_exp_t e)
{
	kirk_bound_t c = { e, 1 };
	kirk_bound_add(r, b, &c);
}

inline void kirk_bound_shift(kirk_bound_t *r,
                             const kirk_bound_t *b,
                             kirk_bound_exp_t e)
{
	if (b->mantissa) {
		r->exponent = b->exponent + e;
		r->mantissa = b->mantissa;
	} else {
		kirk_bound_set_zero(r);
	}
}

inline void kirk_bound_max(kirk_bound_t *r,
                           const kirk_bound_t *b,
                           const kirk_bound_t *c)
{
	kirk_bound_exp_t d = b->exponent - c->exponent;
	if (!d) {
		r->exponent = b->exponent;
		r->mantissa = b->mantissa > c->mantissa ? b->mantissa
		                                        : c->mantissa;
	} else {
		r->exponent = (d > 0 ? b : c)->exponent;
		r->mantissa = (d > 0 ? b : c)->mantissa;
	}
}

inline void kirk_bound_mpfr_size(kirk_bound_t *b, mpfr_srcptr x)
{
	if (mpfr_zero_p(x)) {
		kirk_bound_set_zero(b);
	} else {
		b->mantissa = x->_mpfr_d[KIRK_MPFR_MSL(x)];
		b->exponent = mpfr_get_exp(x);
	}
}

/* --------------------------------------------------------------------------
 * kirk_apx_t
 * -------------------------------------------------------------------------- */

inline void kirk_apx_init(kirk_apx_t *apx)
{
	memset(&apx->radius, 0, sizeof(apx->radius));
	mpfr_init(apx->center);
}

inline void kirk_apx_init2(kirk_apx_t *apx, mpfr_prec_t prec)
{
	memset(&apx->radius, 0, sizeof(apx->radius));
	mpfr_init2(apx->center, prec);
}

inline void kirk_apx_cpy(kirk_apx_t *tgt, const kirk_apx_t *src)
{
	mpfr_set_prec(tgt->center, mpfr_get_prec(src->center));
	mpfr_set(tgt->center, src->center, MPFR_RNDN);
	memcpy(&tgt->radius, &src->radius, sizeof(tgt->radius));
}

inline void kirk_apx_fini(kirk_apx_t *apx)
{
	mpfr_clear(apx->center);
}

/* --------------------------------------------------------------------------
 * kirk context
 * -------------------------------------------------------------------------- */

#ifdef _KIRK_HAVE_CONTEXT
inline kirk_context_t * kirk_context_ref(kirk_context_t *ctx)
{
	return ctx->clazz->ref(ctx);
}

inline void kirk_context_unref(kirk_context_t *ctx)
{
	ctx->clazz->unref(ctx);
}

inline const char * kirk_context_get_name(const kirk_context_t *ctx)
{
	return ctx->clazz->get_name(ctx);
}
#endif

/* --------------------------------------------------------------------------
 * kirk real
 * -------------------------------------------------------------------------- */

inline kirk_real_t * kirk_real_ref(kirk_real_t *r)
{
	return r->clazz->ref(r);
}

inline void kirk_real_unref(kirk_real_t *r)
{
	r->clazz->unref(r);
}

inline void kirk_real_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	r->clazz->apx_abs(r, apx, a);
}

inline void kirk_real_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	r->clazz->apx_eff(r, apx, e);
}

inline void kirk_real_apx_eff_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	kirk_real_apx_abs(r, apx, -e);
}

#ifdef __cplusplus
}
#endif

#endif
