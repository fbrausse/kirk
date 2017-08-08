
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
	mpfr_t dyadic;
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
	if (!tr->refcnt) {
		mpfr_clear(tr->dyadic);
		free(tr);
	}
}

static void set_abs(struct test_real *tr, kirk_apx_t *apx, kirk_abs_t a)
{
	kirk_bound_t rad = { a, KIRK_BOUND_MANT_1HALF };
	kirk_apx_set(apx, tr->dyadic, &rad);
}

static void apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	struct test_real *tr = (struct test_real *)r;
	fprintf(stderr, "apx_abs(test-real #%zu cnt: %zu, abs:%d)\n", tr->id, tr->refcnt, a);
	set_abs(tr, apx, a);
}

static void apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	struct test_real *tr = (struct test_real *)r;
	fprintf(stderr, "apx_eff(test-real #%zu cnt: %zu, eff:%u)\n", tr->id, tr->refcnt, e);
	set_abs(tr, apx, -e);
}

extern kirk_real_t * make_test_real(double d)
{
	static size_t TEST_REAL_ID = 0;

	struct test_real *tr = malloc(sizeof(struct test_real));
	tr->parent.clazz = &test_real_class;
	tr->id = TEST_REAL_ID++;
	tr->refcnt = 1;
	mpfr_init2(tr->dyadic, 53);
	mpfr_set_d(tr->dyadic, d, MPFR_RNDN);
	fprintf(stderr, "test-real #%zu created -> cnt: %zu\n", tr->id, tr->refcnt);
	return &tr->parent;
}

extern kirk_real_t * get_test_real(void)
{
	return make_test_real(123.456);
}
