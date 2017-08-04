
#ifndef KIRK_TYPES_H
#define KIRK_TYPES_H

#include <stdint.h>	/* [u]int32_t */
#include <string.h>	/* memcpy() */
#include <mpfr.h>

#define KIRK_VER_MAJOR	0
#define KIRK_VER_MINOR	0
#define KIRK_VERSION	(KIRK_VER_MAJOR << 16 | KIRK_VER_MINOR << 8 | 0 << 0)

#if defined(KIRK_DLL) && (defined(_WIN32) || defined(__BEOS__))
# define KIRK_IMPORT	__declspec(dllimport)
# define KIRK_EXPORT	__declspec(dllexport)
#else
# define KIRK_IMPORT
# define KIRK_EXPORT
#endif

#ifdef KIRK_INTERNAL
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#undef _KIRK_HAVE_CONTEXT /* no kirk_context_t support, yet */

#ifdef __cplusplus
# ifdef KIRK_INTERNAL
#  error support for extern-inline is required to compile kirk as C++; \
         it has not been implemented
# endif
extern "C" {
#elif !defined(__STDC_VERSION__) || (__STDC_VERSION__ -0) < 199901L
# error need at least C99
#endif

/* <  0: error, negative errno
 * >= 0: success */
typedef int32_t                        kirk_ret_t;

#define KIRK_ERR(no)		(-(int32_t)((uint32_t)(no) << 12))
enum kirk_err_t {
	KIRK_SUCCESS = 0,
	KIRK_ERR_NO_CONV = KIRK_ERR(1), /* no convergence */
};

typedef struct kirk_bound_t             kirk_bound_t;
typedef struct kirk_apx_t               kirk_apx_t;
typedef struct kirk_real_t              kirk_real_t;
typedef struct kirk_real_class_t        kirk_real_class_t;

#ifdef _KIRK_HAVE_CONTEXT
typedef struct kirk_context_t           kirk_context_t;
typedef struct kirk_context_class_t     kirk_context_class_t;
#endif

typedef  int32_t kirk_abs_t; /* +infty -> -infty: absolute accuracy, fast Cauchy */
typedef uint32_t kirk_eff_t; /*      0 -> +infty: no rate specified */

/* ---------------------------------------------------------------------------
 * kirk API
 * -------------------------------------------------------------------------- */

KIRK_API        uint32_t  kirk_version(void);

#ifdef _KIRK_HAVE_CONTEXT
/* Implementations must provide an own "constructor" method with arbitrary
 * arguments, usually <NAME>_kirk_context_create(), where <NAME> is the string
 * returned by kirk_context_get_name(ctx), which returns a pointer to an
 * kirk_context_t structure */

KIRK_API inline kirk_context_t * kirk_context_ref  (kirk_context_t *);
KIRK_API inline void             kirk_context_unref(kirk_context_t *);
KIRK_API inline const char *     kirk_context_get_name(const kirk_context_t *);
/*
KIRK_API inline kirk_ret_t kirk_context_spawn   (kirk_context_t *,
                                                 kirk_computation_t *);
KIRK_API inline kirk_ret_t kirk_context_detach  (kirk_context_t *,
                                                 kirk_computation_t *);

*/
#endif

KIRK_API inline void kirk_apx_init (kirk_apx_t *);
KIRK_API inline void kirk_apx_init2(kirk_apx_t *, mpfr_prec_t);
KIRK_API inline void kirk_apx_cpy  (kirk_apx_t *, const kirk_apx_t *);
KIRK_API inline void kirk_apx_fini (kirk_apx_t *);

KIRK_API        int kirk_bound_less     (const kirk_bound_t *, const kirk_bound_t *);
KIRK_API        int kirk_bound_less_2exp(const kirk_bound_t *, int32_t);

KIRK_API inline kirk_real_t * kirk_real_ref  (kirk_real_t *);
KIRK_API inline void          kirk_real_unref(kirk_real_t *);

KIRK_API inline kirk_ret_t    kirk_real_apx_abs(const kirk_real_t *,
                                                kirk_apx_t *,
                                                kirk_abs_t);

KIRK_API inline kirk_ret_t    kirk_real_apx_eff(const kirk_real_t *,
                                                kirk_apx_t *,
                                                kirk_eff_t);

/* helper functions to implement all of the required approx functions */
KIRK_API        kirk_ret_t    kirk_real_apx_abs_eff(const kirk_real_t *,
                                                    kirk_apx_t *,
                                                    kirk_abs_t);

KIRK_API inline kirk_ret_t    kirk_real_apx_eff_abs(const kirk_real_t *,
                                                    kirk_apx_t *,
                                                    kirk_eff_t);

/* ==========================================================================
 * KIRK data types
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * instantiated library / KIRK context
 * -------------------------------------------------------------------------- */

#ifdef _KIRK_HAVE_CONTEXT /* unused at the moment, may be required later */
struct kirk_context_class_t {
	/* mandatory interface */
	kirk_context_t * (*ref        )(kirk_context_t *);
	void             (*unref      )(kirk_context_t *);
	const char *     (*get_name   )(const kirk_context_t *);

	/* optional interface */
	kirk_ret_t       (*get_feature)(const kirk_context_t *, const char *key);
	kirk_ret_t       (*set_feature)(kirk_context_t *,
	                                const char *key, const char *value);
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

struct kirk_context_t {
	const kirk_context_class_t *clazz;
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};
#endif

/* --------------------------------------------------------------------------
 * Reals
 * -------------------------------------------------------------------------- */

struct kirk_real_class_t {
	kirk_real_t * (*ref    )(kirk_real_t *);
	void          (*unref  )(kirk_real_t *);

	/* signatures for approximating reals */
	/* all of the below three function pointers must be non-NULL, also see
	 * the kirk_real_apx_(abs|eff)_(abs|eff)() helper functions */
	kirk_ret_t (*apx_abs)(const kirk_real_t *, kirk_apx_t *, kirk_abs_t);
	kirk_ret_t (*apx_eff)(const kirk_real_t *, kirk_apx_t *, kirk_eff_t);

	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

/* a real */
struct kirk_real_t {
	const kirk_real_class_t *clazz;
	/* further details are implementation-defined:
	 * do not use the size of this struct */
};

/* --------------------------------------------------------------------------
 * approximations
 * -------------------------------------------------------------------------- */

/* bounds: m * 2**e */
struct kirk_bound_t {
	int32_t  exponent;
	uint32_t mantissa;
};

#define KIRK_BOUND_INIT	{ 0, 0, }

/* approximation type: [center +/- radius] */
struct kirk_apx_t {
	kirk_bound_t radius;
	mpfr_t       center;
};

/* ==========================================================================
 * inline definitions
 * ========================================================================== */

inline void kirk_apx_init(kirk_apx_t *apx)
{
	memset(&apx->radius, 0, sizeof(apx->radius));
	mpfr_init(apx->center);
}

inline void kirk_apx_init2(kirk_apx_t *apx, mpfr_prec_t prec)
{
	memset(&apx->radius, 0, sizeof(apx->radius));
	mpfr_init2(apx->center, prec);
}

inline void kirk_apx_cpy(kirk_apx_t *tgt, const kirk_apx_t *src)
{
	mpfr_set_prec(tgt->center, mpfr_get_prec(src->center));
	mpfr_set(tgt->center, src->center, MPFR_RNDN);
	memcpy(&tgt->radius, &src->radius, sizeof(tgt->radius));
}

inline void kirk_apx_fini(kirk_apx_t *apx)
{
	mpfr_clear(apx->center);
}

/* --------------------------------------------------------------------------
 * kirk context
 * -------------------------------------------------------------------------- */

#ifdef _KIRK_HAVE_CONTEXT
inline kirk_context_t * kirk_context_ref(kirk_context_t *ctx)
{
	return ctx->clazz->ref(ctx);
}

inline void kirk_context_unref(kirk_context_t *ctx)
{
	ctx->clazz->unref(ctx);
}

inline const char * kirk_context_get_name(const kirk_context_t *ctx)
{
	return ctx->clazz->get_name(ctx);
}
#endif

/* --------------------------------------------------------------------------
 * kirk real
 * -------------------------------------------------------------------------- */

inline kirk_real_t * kirk_real_ref(kirk_real_t *r)
{
	return r->clazz->ref(r);
}

inline void kirk_real_unref(kirk_real_t *r)
{
	r->clazz->unref(r);
}

inline kirk_ret_t kirk_real_apx_abs(const kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	return r->clazz->apx_abs(r, apx, a);
}

inline kirk_ret_t kirk_real_apx_eff(const kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	return r->clazz->apx_eff(r, apx, e);
}

inline kirk_ret_t kirk_real_apx_eff_abs(const kirk_real_t *r,
                                        kirk_apx_t *apx,
                                        kirk_eff_t e)
{
	return kirk_real_apx_abs(r, apx, -e);
}

#ifdef __cplusplus
}
#endif

#endif
