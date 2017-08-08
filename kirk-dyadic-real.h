
#ifndef KIRK_DYADIC_REAL_H
#define KIRK_DYADIC_REAL_H

#include "kirk-c-types.h"

#undef KIRK_API
#ifdef KIRK_INTERNAL_DYADIC_REAL
# define KIRK_API	extern KIRK_EXPORT
#else
# define KIRK_API	KIRK_IMPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif

KIRK_API kirk_real_t * kirk_dyadic_test_real(mpfr_srcptr d);
KIRK_API kirk_real_t * kirk_dyadic_test_real_d(double d);
KIRK_API kirk_real_t * kirk_dyadic_test_real_str(const char *str,
                                                 unsigned base,
                                                 mpfr_prec_t prec);
KIRK_API kirk_real_t * kirk_test_real(void);

#ifdef __cplusplus
}
#endif

#endif
