
#include <stdlib.h>
#include <stdio.h>

#include "kirk-c-types.h"

static kirk_real_t * ref(kirk_real_t *);
static void unref(kirk_real_t *);
static void apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a);
static void apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e);

static struct kirk_real_class_t test_real_class = {
	.ref = ref,
	.unref = unref,
	.apx_abs = apx_abs,
	.apx_eff = apx_eff,
};

struct test_real {
	kirk_real_t parent;
	size_t id;
	size_t refcnt;
};

static kirk_real_t * ref(kirk_real_t *r)
{
	struct test_real *tr = (struct test_real *)r;
	tr->refcnt++;
	fprintf(stderr, "test-real #%zu ref'ed -> cnt: %zu\n", tr->id, tr->refcnt);
	return r;
}

static void unref(kirk_real_t *r)
{
	struct test_real *tr = (struct test_real *)r;
	tr->refcnt--;
	fprintf(stderr, "test-real #%zu unref'ed -> cnt: %zu\n", tr->id, tr->refcnt);
	if (!tr->refcnt)
		free(tr);
}

static void apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	struct test_real *tr = (struct test_real *)r;
	fprintf(stderr, "apx_abs(test-real #%zu cnt: %zu, abs:%d)\n", tr->id, tr->refcnt, a);
	apx->radius.exponent = a;
	apx->radius.mantissa = KIRK_BOUND_MANT_1HALF;
	mpfr_set_d(apx->center, 123.456, MPFR_RNDN);
}

static void apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	struct test_real *tr = (struct test_real *)r;
	fprintf(stderr, "apx_eff(test-real #%zu cnt: %zu, eff:%u)\n", tr->id, tr->refcnt, e);
	apx->radius.exponent = -e;
	apx->radius.mantissa = KIRK_BOUND_MANT_1HALF | KIRK_BOUND_MANT_1HALF >> 1;
}

extern kirk_real_t * get_test_real(void)
{
	static size_t TEST_REAL_ID = 0;

	struct test_real *tr = malloc(sizeof(struct test_real));
	tr->parent.clazz = &test_real_class;
	tr->id = TEST_REAL_ID++;
	tr->refcnt = 1;
	fprintf(stderr, "test-real #%zu created -> cnt: %zu\n", tr->id, tr->refcnt);
	return &tr->parent;
}
