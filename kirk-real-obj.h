
#ifndef KIRK_REAL_OBJ_H
#define KIRK_REAL_OBJ_H

#include "kirk-c-types.h"

#undef KIRK_API
#ifdef KIRK_INTERNAL_REAL_OBJ
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct kirk_real_obj_class_t   kirk_real_obj_class_t;

typedef struct kirk_real_obj_t         kirk_real_obj_t;
typedef struct kirk_test_real_t        kirk_test_real_t;
typedef struct kirk_dyadic_test_real_t kirk_dyadic_test_real_t;

typedef void kirk_real_obj_destroy_f(kirk_real_obj_t *);

struct kirk_real_obj_class_t {
	kirk_real_class_t parent;
	kirk_real_obj_destroy_f *finalize; /* overridden by specializations */
};

struct kirk_real_obj_t {
	kirk_real_t parent; /* .clazz points to a kirk_real_obj_class_t */
	kirk_real_obj_destroy_f *destroy;  /* overridden for memory */
	/*_Atomic*/ size_t refcnt;/*
	unsigned acc_native : 1;
	unsigned eff_native : 1;*/
};

struct kirk_test_real_t {
	kirk_real_obj_t parent;
	size_t id;
};

struct kirk_dyadic_test_real_t {
	kirk_test_real_t parent;
	mpfr_t dyadic;
};

/* contains pointers to the kirk_real_obj_default_*() functions, NULL otherwise */
// KIRK_API const kirk_real_obj_class_t kirk_real_obj_class;
/*
KIRK_API const kirk_real_obj_class_t kirk_test_real_class;
KIRK_API const kirk_real_obj_class_t kirk_dyadic_test_real_class;
*/
KIRK_API kirk_real_t * kirk_real_obj_default_ref(kirk_real_t *);
KIRK_API        void   kirk_real_obj_default_unref(kirk_real_t *);
KIRK_API        void   kirk_real_obj_default_finalize(kirk_real_obj_t *r);
KIRK_API        void   kirk_real_obj_destroy_free(kirk_real_obj_t *r);
KIRK_API        void   kirk_real_obj_init(kirk_real_obj_t *r);

KIRK_API inline void kirk_real_obj_finalize(kirk_real_obj_t *);

KIRK_API        void kirk_test_real_init(kirk_test_real_t *r);

KIRK_API        void kirk_dyadic_test_real_init(kirk_dyadic_test_real_t *r,
                                                mpfr_prec_t prec);

/* caches approximations internally to produce deterministic results when the
 * backing kirk_real_t does not provide it */
KIRK_API kirk_real_t * kirk_real_sv_create(kirk_real_t *backend, int apx_native, int eff_native);

inline void kirk_real_obj_finalize(kirk_real_obj_t *r)
{
	((kirk_real_obj_class_t *)r->parent.clazz)->finalize(r);
}

#ifdef __cplusplus
}
#endif

#endif
