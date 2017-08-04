
#define KIRK_INTERNAL

#include <stdlib.h>

#include "kirk-c-types.h"

static uint32_t rshift0(uint32_t v, uint32_t n)
{
	return n < 32 ? v >> n : 0;
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

int kirk_bound_less_2exp(const kirk_bound_t *a, int32_t be)
{
	return kirk_bound_less(a, &(kirk_bound_t){ be, 1 });
}

void kirk_real_apx_abs_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	for (kirk_eff_t e = a < 0 ? -a : 1; e; e++) {
		kirk_real_apx_eff(r, apx, e);
		if (kirk_bound_less_2exp(&apx->radius, a))
			return;
	}
	/* must not happen, implementation has to ensure convergence */
	abort();
}

uint32_t kirk_version(void)
{
	return KIRK_VERSION;
}
