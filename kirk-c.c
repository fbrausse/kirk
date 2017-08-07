
#define KIRK_INTERNAL

#include "kirk-c-types.h"

static kirk_bound_mant_t rshift0(kirk_bound_mant_t v, unsigned n)
{
	return n < KIRK_BOUND_MANT_BITS ? v >> n : 0;
}

static kirk_bound_mant_t rshift_ceil(kirk_bound_mant_t v, unsigned n)
{
	/* ceil(a/b) = 1+floor((a-1)/b) */
	return v ? rshift0(v-1, n) + 1 : 0;
}

int kirk_bound_less(const kirk_bound_t *_a, const kirk_bound_t *_b)
{
	kirk_bound_t a = *_a, b = *_b;
	if (!a.mantissa)
		return 1;
	if (!b.mantissa)
		return 0;
	return a.exponent < b.exponent
	       ? rshift0(a.mantissa, b.exponent - a.exponent) < b.mantissa
	       : rshift0(b.mantissa, a.exponent - b.exponent) > a.mantissa;
}

int kirk_bound_less_2exp(const kirk_bound_t *a, kirk_bound_exp_t be)
{
	return kirk_bound_less(a, &(kirk_bound_t){ be, 1 });
}

kirk_ret_t kirk_bound_add(kirk_bound_t *r,
                          const kirk_bound_t *b,
                          const kirk_bound_t *c)
{
	const kirk_bound_t *x = b, *y = c;
	if (x->exponent < y->exponent) {
		x = c;
		y = b;
	}

	kirk_bound_mant_t ys = rshift_ceil(y->mantissa,
	                                   x->exponent - y->exponent);
	r->exponent = x->exponent;
	r->mantissa = x->mantissa + ys;
	if (r->mantissa < ys) {
		r->mantissa >>= 1;
		r->mantissa  |= KIRK_BOUND_MANT_1HALF;
#ifdef KIRK_CHECK_BOUND
		if (r->exponent == KIRK_BOUND_EXP_MAX)
			return KIRK_ERR_EXP_OVFL;
#endif
		r->exponent++;
	}
	return KIRK_SUCCESS;
}

kirk_ret_t kirk_bound_mul(kirk_bound_t *r,
                          const kirk_bound_t *b,
                          const kirk_bound_t *c)
{
	if (!b->mantissa || !c->mantissa) {
		kirk_bound_set_zero(r);
		return KIRK_SUCCESS;
	}
	const unsigned h = KIRK_BOUND_MANT_BITS / 2;
	kirk_bound_mant_t bh, ch;
	kirk_bound_exp_t be = b->exponent, ce = c->exponent;
	if (be < 0 && ce < KIRK_BOUND_EXP_MIN - be) {
		r->exponent = KIRK_BOUND_EXP_MIN;
		r->mantissa = KIRK_BOUND_MANT_1HALF;
	}
#ifdef KIRK_CHECK_BOUND
	else if (be > 0 && ce > KIRK_BOUND_EXP_MAX - be)
		return KIRK_ERR_EXP_OVFL;
#endif
	else {
		r->exponent = be + ce;
		if ((bh = rshift_ceil(b->mantissa, h)) >> h) {
			/* bh == 2^h */
			r->mantissa = c->mantissa;
		} else if ((ch = rshift_ceil(c->mantissa, h)) >> h) {
			/* ch == 2^h */
			r->mantissa = b->mantissa;
		} else {
			r->mantissa = bh * ch;
			/* only the highest bit can be zero */
			if (!(r->mantissa & KIRK_BOUND_MANT_1HALF)) {
				if (r->exponent == KIRK_BOUND_EXP_MIN)
					r->mantissa = KIRK_BOUND_MANT_1HALF;
				else {
					r->mantissa <<= 1;
					r->exponent--;
				}
			}
		}
	}
	return KIRK_SUCCESS;
}

#include <stdio.h>

void kirk_apx_set(kirk_apx_t *tgt, mpfr_srcptr x, const kirk_bound_t *b)
{
	mpfr_ptr y = tgt->center;
	kirk_bound_t *c = &tgt->radius;

	mpfr_prec_t xp = mpfr_get_prec(x);
	mpfr_prec_t p;

	if (!b->mantissa) {
		kirk_bound_set_zero(c);
		p = xp;
	} else if (mpfr_regular_p(x)) {
		/* In these comments: g = GMP_NUMB_BITS */
		mpfr_exp_t  xe = mpfr_get_exp(x);
		/* k=ceil(xp/g) = 1+floor((xp-1)/g) */
		mpfr_prec_t xk = KIRK_MPFR_N_LIMBS(xp);
		kirk_bound_exp_t be = b->exponent;
		/* 0 < xp <= g*xk */

		/* |x| in 2^xe * [1/2 ; 1)
		 *  b  in 2^be * [1/2 ; 1) */
/*
		mpfr_printf("%RNf +/- (bm:%lu,be:%ld) ~ %g, xe: %d, xp: %u, xk: %u ~ %g\n",
		            x, b->mantissa, b->exponent, kirk_bound_get_d(b),
		            xe, xp, xk, ldexp(1, mpfr_get_exp(x)-mpfr_get_prec(x)));
*/
		if (be >= xe) {
			/* error larger than 2*|x| -> 0 in [x +/- b] */
			p = 0;
		} else if (be > xe - xp) {
			/* error in significant digits of x
			 * -> cut to precision p >= -(be-xe) s.t. mp_limb_t boundary */
			mpfr_prec_t l = KIRK_MPFR_N_LIMBS(1-(be - xe));
			p = l * GMP_NUMB_BITS;
			//p = 1-(be - xe);
			if (xp < p)
				p = xp;
//			printf(" -> l: %lu, p: %lu\n", l, p);
		} else if (be > xe - xk * GMP_NUMB_BITS) {
			/* error in insignificant digits of x -> no change */
			p = xp;
		} else {
			/* error smaller than limbs -> no change */
			p = xp;
		}
		if (p < MPFR_PREC_MIN)
			p = MPFR_PREC_MIN;
		if (p < xp)
			kirk_bound_add_2exp(c, b, xe - p);
		else
			memcpy(c, b, sizeof(*c));
	} else {
		/* x is +/-infinity, NaN or zero */
		p = mpfr_min_prec(x);
		if (p < MPFR_PREC_MIN)
			p = MPFR_PREC_MIN;
		memcpy(c, b, sizeof(*c));
	}

	mpfr_set_prec(y, p);
	mpfr_set(y, x, MPFR_RNDN);
}

kirk_ret_t kirk_real_apx_abs_eff(const kirk_real_t *r,
                                 kirk_apx_t *apx,
                                 kirk_abs_t a)
{
	for (kirk_eff_t e = a < 0 ? -a : 1; e; e++) {
		kirk_real_apx_eff(r, apx, e);
		if (kirk_bound_less_2exp(&apx->radius, a))
			return KIRK_SUCCESS;
	}
	/* should not happen, implementation has to ensure convergence */
	return KIRK_ERR_NO_CONV;
}

uint32_t kirk_version(void)
{
	return KIRK_VERSION;
}
