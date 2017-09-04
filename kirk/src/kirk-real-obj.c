
#include <stdio.h>	/* fprintf() for kirk_real_test_t */
#include <stdlib.h>	/* free() for kirk_real_obj_destroy_free() */

#define KIRK_INTERNAL_REAL_OBJ

#include "kirk-real-obj.h"

void kirk_real_obj_destroy_free(kirk_real_obj_t *r) { free((void *)r); }

kirk_real_t * kirk_real_obj_default_ref(kirk_real_t *r)
{
	struct kirk_real_obj_t *tr = (void *)r;
	atomic_fetch_add(&tr->refcnt, atomic_flag_test_and_set(&tr->refsunk));
	return r;
}

void kirk_real_obj_default_unref(kirk_real_t *r)
{
	struct kirk_real_obj_t *tr = (void *)r;
	if (atomic_fetch_sub(&tr->refcnt, 1) == 1)
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
	atomic_init(&r->refcnt, (size_t)1);
	atomic_flag_clear(&r->refsunk);
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


#include "array.h"

typedef struct kirk_real_sv_t     kirk_real_sv_t;

struct kirk_bound_cache_t {
	kirk_bound_t *data;
	size_t size, valid;
};

struct kirk_idx_cache_t {
	size_t *data;
	size_t size, valid;
};

struct kirk_real_sv_cacc {
	kirk_abs_t acc;
	size_t idx;
};

struct kirk_real_sv_ceff {
	kirk_eff_t eff;
	size_t idx;
};

struct kirk_real_sv_t {
	kirk_real_obj_t parent;
	VARR_DECL_ANON(kirk_apx_t) apx;
	VARR_DECL_ANON(struct kirk_real_sv_cacc) acc;
	VARR_DECL_ANON(struct kirk_real_sv_ceff) eff;
	kirk_real_t *backend;
};

static int kirk_cmp_acc(const void *_a, const void *_b)
{
	const kirk_abs_t *a = _a, *b = _b;
	return *a > *b; /* minimize memcpy: larger absolute accuracy to front */
}

static int kirk_cmp_eff(const void *_a, const void *_b)
{
	const kirk_eff_t *a = _a, *b = _b;
	return *a < *b; /* minimize memcpy: lower efforts to front */
}

static void kirk_real_sv_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t acc)
{
	/* cast to non-const is OK since kirk_real_sv_t only exists malloc'ed */
	kirk_real_sv_t *tr = (kirk_real_sv_t *)r;
	intptr_t i = varr_ck_bsearch(&acc, &tr->acc, kirk_cmp_acc);
	if (i < 0) {
		/* not found -> insert */
		kirk_real_apx_abs(tr->backend, apx, acc);
		struct kirk_apx_t apx2;
		kirk_apx_init2(&apx2, mpfr_get_prec(apx->center));
		kirk_apx_cpy(&apx2, apx);
		struct kirk_real_sv_cacc cacc = { acc, tr->apx.valid };
		varr_append(&tr->apx,&apx2,1,1);
		varr_insert(&tr->acc,~i,&cacc,1,1);
	} else {
		kirk_apx_cpy(apx, &tr->apx.v[tr->acc.v[i].idx]);
	}
}

static void kirk_real_sv_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t eff)
{
	/* cast to non-const is OK since kirk_real_sv_t only exists malloc'ed */
	kirk_real_sv_t *tr = (kirk_real_sv_t *)r;
	intptr_t i = varr_ck_bsearch(&eff, &tr->eff, kirk_cmp_eff);
	if (i < 0) {
		/* not found -> insert */
		kirk_real_apx_eff(tr->backend, apx, eff);
		struct kirk_apx_t apx2;
		kirk_apx_init2(&apx2, mpfr_get_prec(apx->center));
		kirk_apx_cpy(&apx2, apx);
		struct kirk_real_sv_ceff ceff = { eff, tr->apx.valid };
		varr_append(&tr->apx,&apx2,1,1);
		varr_insert(&tr->eff,~i,&ceff,1,1);
	} else {
		kirk_apx_cpy(apx, &tr->apx.v[tr->eff.v[i].idx]);
	}
}

static const kirk_real_obj_class_t kirk_real_sv_class = {
	.parent = {
		.ref = kirk_real_obj_default_ref,
		.unref = kirk_real_obj_default_unref,
		.apx_abs = kirk_real_sv_apx_abs,
		.apx_eff = kirk_real_sv_apx_eff,
	},
	.finalize = kirk_real_obj_default_finalize,
};

static void kirk_real_sv_destroy(kirk_real_obj_t *r)
{
	kirk_real_sv_t *tr = (kirk_real_sv_t *)r;
	kirk_apx_t *apx;
	varr_forall(apx,&tr->apx)
		kirk_apx_fini(apx);
	varr_fini(&tr->apx);
	varr_fini(&tr->acc);
	varr_fini(&tr->eff);
	kirk_real_unref(tr->backend);
	free(r);
}

kirk_real_t * kirk_real_sv_create(kirk_real_t *backend, int apx_native, int eff_native)
{
	/* TODO: respect apx_native, eff_native */
	(void)apx_native;
	(void)eff_native;
	kirk_real_sv_t *tr = malloc(sizeof(kirk_real_sv_t));
	kirk_real_obj_init(&tr->parent);
	tr->parent.parent.clazz = &kirk_real_sv_class.parent;
	tr->parent.destroy = kirk_real_sv_destroy;
	memset(&tr->apx, 0, sizeof(tr->apx));
	memset(&tr->acc, 0, sizeof(tr->acc));
	memset(&tr->eff, 0, sizeof(tr->eff));
	tr->backend = kirk_real_ref(backend);
	return (kirk_real_t *)tr;
}
