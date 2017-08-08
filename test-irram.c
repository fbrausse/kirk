
#include <iRRAM/common.h>

#include "kirk-irram-api.h"
#include "kirk-dyadic-real.h"

int main(int argc, char **argv)
{
	iRRAM_initialize(argc, argv);
	kirk_real_t *c = kirk_dyadic_test_real_d(3.75);

	kirk_real_t *ex0 = kirk_irram_exp(c);
	kirk_real_unref(c);

	kirk_apx_t apx;
	kirk_apx_init(&apx);
	kirk_real_apx_abs(ex0, &apx, -53);
	mpfr_printf("%RNf +/- %lu*2^%ld ~ %g, exp: %u, prec: %u ~ %g\n",
	            apx.center,
	            apx.radius.mantissa, apx.radius.exponent-KIRK_BOUND_MANT_BITS,
	            kirk_bound_get_d(&apx.radius),
	            mpfr_get_exp(apx.center), mpfr_get_prec(apx.center),
	            ldexp(1, mpfr_get_exp(apx.center)-mpfr_get_prec(apx.center)));
	kirk_apx_fini(&apx);
	kirk_real_unref(ex0);
}
