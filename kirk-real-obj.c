
#include <stdio.h>	/* fprintf() for kirk_real_test_t */
#include <stdlib.h>	/* free() for kirk_real_obj_destroy_free() */

#define KIRK_INTERNAL_REAL_OBJ

#include "kirk-real-obj.h"

void kirk_real_obj_destroy_free(kirk_real_obj_t *r) { free((void *)r); }

kirk_real_t * kirk_real_obj_default_ref(kirk_real_t *r)
{
	struct kirk_real_obj_t *tr = (void *)r;
	tr->refcnt++;
	return r;
}

void kirk_real_obj_default_unref(kirk_real_t *r)
{
	struct kirk_real_obj_t *tr = (void *)r;
	if (!--tr->refcnt)
		kirk_real_obj_finalize(tr);
}

void kirk_real_obj_default_finalize(kirk_real_obj_t *r)
{
	if (r->destroy)
		r->destroy(r);
}

static const kirk_real_obj_class_t kirk_real_obj_class = {
	.parent = {
		.ref     = kirk_real_obj_default_ref,
		.unref   = kirk_real_obj_default_unref,
		.apx_abs = NULL,
		.apx_eff = NULL,
	},
	.finalize = kirk_real_obj_default_finalize,
};

void kirk_real_obj_init(kirk_real_obj_t *r)
{
	r->parent.clazz = &kirk_real_obj_class.parent;
	r->destroy = NULL;
	r->refcnt = 1;
}

static kirk_real_t * test_ref(kirk_real_t *r)
{
	r = kirk_real_obj_default_ref(r);
	kirk_test_real_t *tr = (void *)r;
	fprintf(stderr, "test-real #%zu ref'ed -> cnt: %zu\n", tr->id, tr->parent.refcnt);
	return r;
}

static void test_unref(kirk_real_t *r)
{
	kirk_test_real_t *tr = (void *)r;
	fprintf(stderr, "test-real #%zu unref'ed -> cnt: %zu\n",
	        tr->id, tr->parent.refcnt-1);
	kirk_real_obj_default_unref(r);
}

static void test_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	(void)apx;
	kirk_test_real_t *tr = (void *)r;
	fprintf(stderr, "apx_abs(test-real #%zu cnt: %zu, abs:%d)\n",
	        tr->id, tr->parent.refcnt, a);
}

static void test_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	(void)apx;
	kirk_test_real_t *tr = (void *)r;
	fprintf(stderr, "apx_eff(test-real #%zu cnt: %zu, eff:%u)\n",
	        tr->id, tr->parent.refcnt, e);
}

static void test_finalize(kirk_real_obj_t *r)
{
	kirk_test_real_t *tr = (void *)r;
	fprintf(stderr, "finalize(test-real #%zu, cnt: %zu)\n",
	        tr->id, tr->parent.refcnt);
	kirk_real_obj_default_finalize(r);
}

const kirk_real_obj_class_t kirk_test_real_class = {
	.parent = {
		.ref     = test_ref,
		.unref   = test_unref,
		.apx_abs = test_apx_abs,
		.apx_eff = test_apx_eff,
	},
	.finalize = test_finalize,
};

void kirk_test_real_init(kirk_test_real_t *tr)
{
	kirk_real_obj_init(&tr->parent);
	static size_t TEST_REAL_ID = 0;
	tr->parent.parent.clazz = &kirk_test_real_class.parent;
	tr->id = TEST_REAL_ID++;
	fprintf(stderr, "test-real #%zu init'ed -> cnt: %zu\n",
	        tr->id, tr->parent.refcnt);
}

static void set_abs(kirk_apx_t *apx, mpfr_srcptr x, kirk_abs_t a)
{
	kirk_bound_t rad = { a, KIRK_BOUND_MANT_1HALF };
	kirk_apx_set(apx, x, &rad);
}

static void dyadic_test_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	kirk_dyadic_test_real_t *tr = (void *)r;
	test_apx_abs(r, apx, a);
	set_abs(apx, tr->dyadic, a);
}

static void dyadic_test_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	kirk_dyadic_test_real_t *tr = (void *)r;
	test_apx_eff(r, apx, e);
	set_abs(apx, tr->dyadic, -e);
}

static void dyadic_test_finalize(kirk_real_obj_t *r)
{
	kirk_dyadic_test_real_t *tr = (void *)r;
	mpfr_clear(tr->dyadic);
	test_finalize(r);
}

const kirk_real_obj_class_t kirk_dyadic_test_real_class = {
	.parent = {
		.ref     = test_ref,
		.unref   = test_unref,
		.apx_abs = dyadic_test_apx_abs,
		.apx_eff = dyadic_test_apx_eff,
	},
	.finalize = dyadic_test_finalize
};

void kirk_dyadic_test_real_init(kirk_dyadic_test_real_t *r,
                                mpfr_prec_t prec)
{
	kirk_test_real_init(&r->parent);
	((kirk_real_t *)r)->clazz = &kirk_dyadic_test_real_class.parent;
	mpfr_init2(r->dyadic, prec);
}
