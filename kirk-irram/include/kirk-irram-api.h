
#ifndef KIRK_IRRAM_API_H
#define KIRK_IRRAM_API_H

#include "kirk-c-types.h"

#undef KIRK_API
#ifdef KIRK_INTERNAL_IRRAM_API
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif

KIRK_API kirk_real_t * kirk_irram_add    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_neg    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sub    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_mul    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_inv    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_div    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_pi     (void);
KIRK_API kirk_real_t * kirk_irram_e      (void);
KIRK_API kirk_real_t * kirk_irram_ln2    (void);
KIRK_API kirk_real_t * kirk_irram_exp    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_pow    (kirk_real_t *, int);
KIRK_API kirk_real_t * kirk_irram_pow_r  (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_log    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sqrt   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_root   (kirk_real_t *, int);
KIRK_API kirk_real_t * kirk_irram_max    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_min    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_mod    (kirk_real_t *, kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sin    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_cos    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_tan    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sec    (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_cotan  (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_cosec  (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_asin   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_acos   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_atan   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_asec   (kirk_real_t *);
//KIRK_API kirk_real_t * kirk_irram_acotan (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_acosec (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sinh   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_cosh   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_tanh   (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_sech   (kirk_real_t *);
//KIRK_API kirk_real_t * kirk_irram_cotanh (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_cosech (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_asinh  (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_acosh  (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_atanh  (kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_asech  (kirk_real_t *);
//KIRK_API kirk_real_t * kirk_irram_acotanh(kirk_real_t *);
KIRK_API kirk_real_t * kirk_irram_acosech(kirk_real_t *);

#ifdef __cplusplus
}
#endif

#endif
