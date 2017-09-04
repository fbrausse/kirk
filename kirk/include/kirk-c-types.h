
#ifndef KIRK_C_TYPES_H
#define KIRK_C_TYPES_H

#include <stdint.h>	/* [u]int32_t, intmax_t for MPFR */
#include <string.h>	/* memset() */
#include <float.h>	/* DBL_MANT_DIG */
#include <math.h>	/* (ld|fr)exp() */
#include <errno.h>	/* EINVAL */
#include <assert.h>

#if KIRK_BOUND_SIZE_GMP-0
# include <gmp.h>
# define KIRK_BOUND_MANT_BITS	GMP_NUMB_BITS
#else
# define KIRK_BOUND_MANT_BITS	64
#endif

#if ULONG_MAX/2 < (1UL << KIRK_BOUND_MANT_BITS-1)
# define KIRK_MPFR_NEED_INTMAX_T
#endif

#if defined(KIRK_MPFR_NEED_INTMAX_T) || defined(__cplusplus)
# define MPFR_USE_INTMAX_T
#endif

#include <mpfr.h>

#include "kirk-common.h"

#undef KIRK_API
#ifdef KIRK_INTERNAL_C_TYPES
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#ifdef __cplusplus
# ifdef KIRK_INTERNAL
#  error support for extern-inline is required to compile kirk as C++; \
         it has not been implemented
# endif
extern "C" {
#endif

#define KIRK_CONCAT3(a,b,c)			a ## b ## c
#define KIRK_UINT_TYPE(b)			KIRK_CONCAT3(uint,b,_t)
#define KIRK_INT_TYPE(b)			KIRK_CONCAT3(int,b,_t)
#define KIRK_ERR(no)				(-(int)((unsigned)(no) << 12))

typedef int kirk_ret_t;

/* <  0: error, negative errno(3) or a KIRK_ERR_* constant
 * >= 0: success */
enum {
	KIRK_SUCCESS      = 0,
	KIRK_ERR_NO_CONV  = KIRK_ERR(1), /* no convergence */
	KIRK_ERR_EXP_OVFL = KIRK_ERR(2), /* kirk_bound_exp_t overflow */
};

enum {
	KIRK_INFO_HAVE_IRRAM = 1U << 0,
	KIRK_INFO_HAVE_HMPFR = 1U << 1,
};

#if KIRK_BOUND_SIZE_GMP-0
typedef KIRK_UINT_TYPE(GMP_LIMB_BITS)		kirk_bound_mant_t;
typedef KIRK_INT_TYPE(GMP_LIMB_BITS)		kirk_bound_exp_t;
#else
typedef KIRK_UINT_TYPE(KIRK_BOUND_MANT_BITS)	kirk_bound_mant_t;
typedef KIRK_INT_TYPE(KIRK_BOUND_MANT_BITS)	kirk_bound_exp_t;
#endif

#define KIRK_BOUND_EXP_MIN		(kirk_bound_exp_t)KIRK_BOUND_MANT_1HALF
#define KIRK_BOUND_EXP_MAX		(kirk_bound_exp_t)~KIRK_BOUND_MANT_1HALF

typedef struct kirk_bound_t             kirk_bound_t;
typedef struct kirk_apx_t               kirk_apx_t;
typedef struct kirk_real_t              kirk_real_t;
typedef struct kirk_real_class_t        kirk_real_class_t;

typedef  int32_t kirk_abs_t; /* +infty -> -infty: absolute accuracy, fast Cauchy */
typedef uint32_t kirk_eff_t; /*      0 -> +infty: no rate specified */

/* ---------------------------------------------------------------------------
 * kirk API
 * -------------------------------------------------------------------------- */

KIRK_API        uint32_t    kirk_info(void);
KIRK_API        uint32_t    kirk_version(void);

/* multi-valued: b <  c -> returns 1
 *               b == c -> returns 0 or 1
 *               b >  c -> returns 0 */
KIRK_API        int              kirk_bound_less     (const kirk_bound_t *b,
                                                      const kirk_bound_t *c);

/* multi-valued check whether b < 2^e; c.f. kirk_bound_less() */
KIRK_API        int              kirk_bound_less_2exp(const kirk_bound_t *b,
                                                      kirk_bound_exp_t e);

/* sets r to a state representing zero:
 * mantissa is zero and exponent is minimal */
KIRK_API inline void             kirk_bound_set_zero(kirk_bound_t *r);

/* set r from a double */
KIRK_API inline void             kirk_bound_set_d(kirk_bound_t *r, double d);

/* interpret b as a double
 * warning: kirk_bound_t easily stores bounds not representable in type double,
 * this leads to over- and underflow; fpclassify() the result! */
KIRK_API inline double           kirk_bound_get_d(const kirk_bound_t *b);

/* store in r the next higher value than b representable in kirk_bound_t.
 * \requires (b.exponent < KIRK_BOUND_EXP_MAX || ~b.mantissa) */
KIRK_API inline kirk_ret_t       kirk_bound_nextafter(kirk_bound_t *r,
                                                      const kirk_bound_t *b);

/* store in r the result of adding b and c if it can be represented exactly in
 * a kirk_bound_t, otherwise round towards +infinity and store that.
 * \requires the sum to be representable in kirk_bound_t */
KIRK_API        kirk_ret_t       kirk_bound_add(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
KIRK_API inline kirk_ret_t       kirk_bound_add_2exp(kirk_bound_t *r,
                                                     const kirk_bound_t *b,
                                                     kirk_bound_exp_t e);
KIRK_API        kirk_ret_t       kirk_bound_mul(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
KIRK_API inline kirk_ret_t       kirk_bound_shift(kirk_bound_t *r,
                                                  const kirk_bound_t *b,
                                                  kirk_bound_exp_t e);
KIRK_API inline void             kirk_bound_max(kirk_bound_t *r,
                                                const kirk_bound_t *b,
                                                const kirk_bound_t *c);
/* \requires mpfr_number_p(x): x must be neither NaN nor an infinity
 * \ensures  b <= |x| < (b->mantissa+1)/2^C * 2^(b->exponent) */
KIRK_API inline kirk_ret_t       kirk_bound_set_mpfr(kirk_bound_t *b,
                                                     const mpfr_t x);
KIRK_API inline void             kirk_bound_get_mpfr(const kirk_bound_t *b,
                                                     mpfr_t x);

KIRK_API inline void kirk_apx_init (kirk_apx_t *);
KIRK_API inline void kirk_apx_init2(kirk_apx_t *, mpfr_prec_t /* TODO: std-C */);
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

/* Use effort-based unbounded search to approximate r. The signature of this
 * "helper" function does on purpose not exactly fit that of
 * kirk_real_class_t::*apx_eff to emphasize the need for eventual convergence.*/
KIRK_API        kirk_ret_t    kirk_real_apx_abs_eff(const kirk_real_t *r,
                                                    kirk_apx_t *,
                                                    kirk_abs_t);

KIRK_API inline void          kirk_real_apx_eff_abs(const kirk_real_t *,
                                                    kirk_apx_t *,
                                                    kirk_eff_t);

/* ==========================================================================
 * KIRK data types
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * Reals
 * -------------------------------------------------------------------------- */

typedef void kirk_apx_abs_f(const kirk_real_t *, kirk_apx_t *, kirk_abs_t);
typedef void kirk_apx_eff_f(const kirk_real_t *, kirk_apx_t *, kirk_eff_t);

struct kirk_real_class_t {
	kirk_real_t * (*ref    )(kirk_real_t *);
	void          (*unref  )(kirk_real_t *);

	/* signatures for approximating reals */
	/* all of the below three function pointers must be non-NULL, also see
	 * the kirk_real_apx_(abs|eff)_(abs|eff)() helper functions
	 * In order to represent Reals, these functions must not fail */
	kirk_apx_abs_f *apx_abs;
	kirk_apx_eff_f *apx_eff;

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
#ifdef MPFR_SIGN
# define KIRK_MPFR_SIGN(x)	MPFR_SIGN(x)
#else
# define KIRK_MPFR_SIGN(x)	mpfr_sgn(x)
#endif

inline void kirk_bound_set_zero(kirk_bound_t *r)
{
	r->exponent = KIRK_BOUND_EXP_MIN;
	r->mantissa = 0;
}

inline kirk_ret_t kirk_bound_nextafter(kirk_bound_t *r, const kirk_bound_t *b)
{
	r->exponent = b->exponent;
	r->mantissa = b->mantissa + 1;
	if (!r->mantissa) {
		r->mantissa = KIRK_BOUND_MANT_1HALF;
#if KIRK_CHECK_BOUND-0
		if (r->exponent == KIRK_BOUND_EXP_MAX)
			return KIRK_ERR_EXP_OVFL;
#endif
		r->exponent++;
	}
	return KIRK_SUCCESS;
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

inline kirk_ret_t kirk_bound_add_2exp(kirk_bound_t *r,
                                      const kirk_bound_t *b,
                                      kirk_bound_exp_t e)
{
#if KIRK_CHECK_BOUND-0
	if (e == KIRK_BOUND_EXP_MAX)
		return KIRK_ERR_EXP_OVFL;
#endif
	kirk_bound_t c = { e+1, KIRK_BOUND_MANT_1HALF };
	return kirk_bound_add(r, b, &c);
}

inline kirk_ret_t kirk_bound_shift(kirk_bound_t *r,
                                   const kirk_bound_t *b,
                                   kirk_bound_exp_t e)
{
	if (b->mantissa) {
		if (e < 0 && b->exponent < KIRK_BOUND_EXP_MIN - e) {
			r->exponent = KIRK_BOUND_EXP_MIN;
			r->mantissa = KIRK_BOUND_MANT_1HALF;
		}
#if KIRK_CHECK_BOUND-0
		else if (e > 0 && b->exponent > KIRK_BOUND_EXP_MAX - e)
			return KIRK_ERR_EXP_OVFL;
#endif
		else {
			r->exponent = b->exponent + e;
			r->mantissa = b->mantissa;
		}
	} else {
		kirk_bound_set_zero(r);
	}
	return KIRK_SUCCESS;
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

inline kirk_ret_t kirk_bound_set_mpfr(kirk_bound_t *b, const mpfr_t x)
{
	if (mpfr_zero_p(x)) {
		kirk_bound_set_zero(b);
	} else {
#if KIRK_CHECK_BOUND-0
		if (!mpfr_regular_p(x))
			return -EINVAL;
#endif
		size_t k = KIRK_MPFR_N_LIMBS(mpfr_get_prec(x));
		int n = KIRK_BOUND_MANT_BITS;
		b->mantissa = 0;
		b->exponent = mpfr_get_exp(x);
		while (k && n >= GMP_NUMB_BITS)
			b->mantissa |= (kirk_bound_mant_t)x->_mpfr_d[--k]
			               << (n -= GMP_NUMB_BITS);
		if (k && n > 0)
			b->mantissa |= x->_mpfr_d[--k] >> -(n -= GMP_NUMB_BITS);
		/* k n x | inc
		 * ------+----
		 * 0 < < | 0 ceil
		 * 0 < > | 1 floor
		 * 0 = < | 0 exact
		 * 0 = > | 0 exact
		 * 0 > < | 0 exact
		 * 0 > > | 0 exact
		 * 1 < < | 0 ceil
		 * 1 < > | 1 floor
		 * 1 = < | 0 ceil
		 * 1 = > | 1 floor
		 * 1 > < | x impossible
		 * 1 > > | x impossible
		 */
		int inc = 0;
		if (n < 0 || (!n && k))
			inc = KIRK_MPFR_SIGN(x) > 0;
		else
			assert(!k); /* exact */
		if (inc)
			return kirk_bound_nextafter(b, b);
	}
	return KIRK_SUCCESS;
}

KIRK_API inline void kirk_bound_get_mpfr(const kirk_bound_t *b, mpfr_t r)
{
	mpfr_set_prec(r, KIRK_BOUND_MANT_BITS);
#ifdef KIRK_MPFR_NEED_INTMAX_T
	mpfr_set_uj_2exp(r, b->mantissa, b->exponent, MPFR_RNDU);
#else
	mpfr_set_ui_2exp(r, b->mantissa, b->exponent, MPFR_RNDU);
#endif
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
#if 0
	kirk_apx_set(tgt, src->center, &src->radius);
#else
	mpfr_set_prec(tgt->center, mpfr_get_prec(src->center));
	mpfr_set(tgt->center, src->center, MPFR_RNDN);
	tgt->radius = src->radius;
#endif
}

inline void kirk_apx_fini(kirk_apx_t *apx)
{
	mpfr_clear(apx->center);
}

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
