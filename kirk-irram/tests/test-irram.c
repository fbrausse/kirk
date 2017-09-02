
#define _POSIX_C_SOURCE	200809L
#include <time.h>

#include <iRRAM/common.h>

#include "kirk-irram-api.h"
#include "kirk-dyadic-real.h"

static void put_real_ln(const char *label, const kirk_real_t *r, kirk_abs_t acc)
{
	kirk_apx_t apx;
	kirk_apx_init(&apx);
	kirk_real_apx_abs(r, &apx, acc);
	mpfr_printf("%s[%RNg +/- %lu*2^(%ld)]\n",
	            label,
	            apx.center, apx.radius.mantissa, apx.radius.exponent - 64);
	kirk_apx_fini(&apx);
}

int main(int argc, char **argv)
{
	kirk_real_t *two = kirk_dyadic_test_real_d(2);
	put_real_ln("two: ", two, -10);

	kirk_real_t *kr = kirk_dyadic_test_real_d(123.456);

	iRRAM_initialize(argc, argv);

	kirk_real_t *kr_sqrt = kirk_irram_sqrt(kr);
	kirk_real_t *kr_     = kirk_irram_pow(kr_sqrt, 2);
	kirk_real_t *kr__    = kirk_irram_pow_r(kr_sqrt, two);

	put_real_ln("x           : ", kr, -10);
	put_real_ln("sqrt x      : ", kr_sqrt, -10);
	put_real_ln("(sqrt x)^2  : ", kr_, -10);
	put_real_ln("(sqrt x)^two: ", kr__, -10);

	kirk_real_t *pi = kirk_irram_pi();
	put_real_ln("pi          : ", pi, -10);

	kirk_real_unref(kr_);
	kirk_real_unref(kr__);
	kirk_real_unref(pi);

	/* give machine time to exit
	 * -> output should (depending on scheduling, but most of the time) show
	 *	test-real #0 unref'ed -> cnt: 0
	 *	finalize(test-real #0, cnt: 0)
	 *	test-real #1 unref'ed -> cnt: 0
	 *	finalize(test-real #1, cnt: 0)
	 */
	nanosleep(&(struct timespec){ .tv_sec = 0, .tv_nsec = 1e6 }, NULL);
}
