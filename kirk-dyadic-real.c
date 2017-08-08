
#include <stdlib.h>

#define KIRK_INTERNAL_DYADIC_REAL

#include "kirk-real-obj.h"
#include "kirk-dyadic-real.h"

static kirk_dyadic_test_real_t * dyadic_test_real_create(mpfr_prec_t prec)
{
	kirk_dyadic_test_real_t *tr;
	tr = malloc(sizeof(*tr));
	kirk_dyadic_test_real_init(tr, prec);
	((kirk_real_obj_t *)tr)->destroy = kirk_real_obj_destroy_free;
	return tr;
}

kirk_real_t * kirk_dyadic_test_real(mpfr_srcptr d)
{
	kirk_dyadic_test_real_t *tr = dyadic_test_real_create(mpfr_get_prec(d));
	mpfr_set(tr->dyadic, d, MPFR_RNDN);
	return &tr->parent.parent.parent;
}

kirk_real_t * kirk_dyadic_test_real_d(double d)
{
	kirk_dyadic_test_real_t *tr = dyadic_test_real_create(53);
	mpfr_set_d(tr->dyadic, d, MPFR_RNDN);
	return &tr->parent.parent.parent;
}

kirk_real_t * kirk_dyadic_test_real_str(const char *str, unsigned base, mpfr_prec_t prec)
{
	kirk_dyadic_test_real_t *tr = dyadic_test_real_create(prec);
	mpfr_set_str(tr->dyadic, str, base, MPFR_RNDN);
	return &tr->parent.parent.parent;
}

kirk_real_t * kirk_test_real(void)
{
	return kirk_dyadic_test_real_d(123.456);
}
