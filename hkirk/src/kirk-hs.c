
#include <stdlib.h>
#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
#include "Data/Number/Kirk_stub.h"
#endif

#define KIRK_INTERNAL_HS

#include <kirk-real-obj.h>

#undef KIRK_API
#ifdef KIRK_INTERNAL_HS
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

typedef struct kirk_real_hs_t kirk_real_hs_t;

KIRK_API kirk_real_t * kirk_real_hs_create(HsStablePtr);

typedef void kirk_real_hs_apx_f(kirk_apx_t *, kirk_abs_t); /* apx_f */
typedef void kirk_real_hs_eff_f(kirk_apx_t *, kirk_eff_t); /* eff_f */

static void kirk_real_hs_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t abs);
static void kirk_real_hs_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t eff);
static void kirk_real_hs_destroy(kirk_real_obj_t *r);

const struct kirk_real_obj_class_t kirk_real_hs_class = {
	.parent = {
		kirk_real_obj_default_ref,
		kirk_real_obj_default_unref,
		kirk_real_hs_apx_abs,
		kirk_real_hs_apx_eff,
	},
	.finalize = kirk_real_obj_default_finalize,
};

struct kirk_real_hs_t {
	struct kirk_real_obj_t parent;
	HsStablePtr p;
};

kirk_real_t * kirk_real_hs_create(HsStablePtr p)
{
	kirk_real_hs_t *r = malloc(sizeof(kirk_real_hs_t));
	kirk_real_obj_init(&r->parent);

	hs_init(NULL, NULL);

	r->parent.parent.clazz = &kirk_real_hs_class.parent;
	r->parent.destroy = kirk_real_hs_destroy;
	r->p = p;
	return &r->parent.parent;
}

static void kirk_real_hs_destroy(kirk_real_obj_t *r)
{
	kirk_real_hs_t *tr = (kirk_real_hs_t *)r;

	hs_free_stable_ptr(tr->p);
	/* depends on https://git.haskell.org/ghc.git/commit/9cbcdb4863064753df0fff9054b7b7c6b3188b64
	 * available in GHC>=8.2 */
	hs_exit();

	free(tr);
}

static void kirk_real_hs_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t abs)
{
	kirk_real_hs_t *tr = (void *)r;
	kirk_hs_approx_abs(abs, tr->p, apx);
}

static void kirk_real_hs_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t eff)
{
	kirk_real_hs_t *tr = (void *)r;
	kirk_hs_approx_eff(eff, tr->p, apx);
}
