
#include <iRRAM/lib.h>

#define KIRK_INTERNAL_IRRAM_API

#include "kirk-irram-api.h"
#include "kirk-iRRAM.hh"

using kirk::irram::eval;

using iRRAM::REAL;

static kirk_real_t * run10(REAL (*f)())
{
	kirk_real_t *out[1];
	eval(nullptr, 0, out, 1, [f](const REAL *, REAL *out){ out[0] = f(); return 0; });
	return out[0];
}

template <typename F = REAL(*)(const REAL &)>
static kirk_real_t * run11(F f, kirk_real_t *x)
{
	kirk_real_t *in[] = { x }, *out[1];
	eval(in, 1, out, 1, [f](const REAL *in, REAL *out){ out[0] = f(in[0]); return 0; });
	return out[0];
}

static kirk_real_t * run12(REAL (*f)(const REAL &, const REAL &), kirk_real_t *x, kirk_real_t *y)
{
	kirk_real_t *in[] = { x, y }, *out[1];
	eval(in, 2, out, 1, [f](const REAL *in, REAL *out){ out[0] = f(in[0], in[1]); return 0; });
	return out[0];
}

extern "C" kirk_real_t * kirk_irram_add    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::operator+, x, y); }
extern "C" kirk_real_t * kirk_irram_neg    (kirk_real_t *x) { return run11([](const REAL &x){ return -x; }, x); }
extern "C" kirk_real_t * kirk_irram_sub    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::operator-, x, y); }
extern "C" kirk_real_t * kirk_irram_mul    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::operator*, x, y); }
extern "C" kirk_real_t * kirk_irram_inv    (kirk_real_t *x) { return run11([](const REAL &x) { return 1/x; }, x); }
extern "C" kirk_real_t * kirk_irram_div    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::operator/, x, y); }
extern "C" kirk_real_t * kirk_irram_pi     (void) { return run10(iRRAM::pi); }
extern "C" kirk_real_t * kirk_irram_e      (void) { return run10(iRRAM::euler); }
extern "C" kirk_real_t * kirk_irram_ln2    (void) { return run10(iRRAM::ln2); }
extern "C" kirk_real_t * kirk_irram_exp    (kirk_real_t *x) { return run11(iRRAM::exp, x); }
extern "C" kirk_real_t * kirk_irram_pow    (kirk_real_t *x, int n) { return run11([n](const REAL &x){ return iRRAM::power(x, n); }, x); }
extern "C" kirk_real_t * kirk_irram_pow_r  (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::power, x, y); }
extern "C" kirk_real_t * kirk_irram_log    (kirk_real_t *x) { return run11(iRRAM::log, x); }
extern "C" kirk_real_t * kirk_irram_sqrt   (kirk_real_t *x) { return run11(iRRAM::sqrt, x); }
extern "C" kirk_real_t * kirk_irram_root   (kirk_real_t *x, int n) { return run11([n](const REAL &x){ return iRRAM::root(x, n); }, x); }
extern "C" kirk_real_t * kirk_irram_max    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::maximum, x, y); }
extern "C" kirk_real_t * kirk_irram_min    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::minimum, x, y); }
extern "C" kirk_real_t * kirk_irram_mod    (kirk_real_t *x, kirk_real_t *y) { return run12(iRRAM::modulo, x, y); }
extern "C" kirk_real_t * kirk_irram_sin    (kirk_real_t *x) { return run11(iRRAM::sin    , x); }
extern "C" kirk_real_t * kirk_irram_cos    (kirk_real_t *x) { return run11(iRRAM::cos    , x); }
extern "C" kirk_real_t * kirk_irram_tan    (kirk_real_t *x) { return run11(iRRAM::tan    , x); }
extern "C" kirk_real_t * kirk_irram_sec    (kirk_real_t *x) { return run11(iRRAM::sec    , x); }
extern "C" kirk_real_t * kirk_irram_cotan  (kirk_real_t *x) { return run11(iRRAM::cotan  , x); }
extern "C" kirk_real_t * kirk_irram_cosec  (kirk_real_t *x) { return run11(iRRAM::cosec  , x); }
extern "C" kirk_real_t * kirk_irram_asin   (kirk_real_t *x) { return run11(iRRAM::asin   , x); }
extern "C" kirk_real_t * kirk_irram_acos   (kirk_real_t *x) { return run11(iRRAM::acos   , x); }
extern "C" kirk_real_t * kirk_irram_atan   (kirk_real_t *x) { return run11(iRRAM::atan   , x); }
extern "C" kirk_real_t * kirk_irram_asec   (kirk_real_t *x) { return run11(iRRAM::asec   , x); }
//extern "C" kirk_real_t * kirk_irram_acotan (kirk_real_t *x) { return run11(iRRAM::acotan , x); }
extern "C" kirk_real_t * kirk_irram_acosec (kirk_real_t *x) { return run11(iRRAM::acosec , x); }
extern "C" kirk_real_t * kirk_irram_sinh   (kirk_real_t *x) { return run11(iRRAM::sinh   , x); }
extern "C" kirk_real_t * kirk_irram_cosh   (kirk_real_t *x) { return run11(iRRAM::cosh   , x); }
extern "C" kirk_real_t * kirk_irram_tanh   (kirk_real_t *x) { return run11(iRRAM::tanh   , x); }
extern "C" kirk_real_t * kirk_irram_sech   (kirk_real_t *x) { return run11(iRRAM::sech   , x); }
//extern "C" kirk_real_t * kirk_irram_cotanh (kirk_real_t *x) { return run11(iRRAM::cotanh , x); }
extern "C" kirk_real_t * kirk_irram_cosech (kirk_real_t *x) { return run11(iRRAM::cosech , x); }
extern "C" kirk_real_t * kirk_irram_asinh  (kirk_real_t *x) { return run11(iRRAM::asinh  , x); }
extern "C" kirk_real_t * kirk_irram_acosh  (kirk_real_t *x) { return run11(iRRAM::acosh  , x); }
extern "C" kirk_real_t * kirk_irram_atanh  (kirk_real_t *x) { return run11(iRRAM::atanh  , x); }
extern "C" kirk_real_t * kirk_irram_asech  (kirk_real_t *x) { return run11(iRRAM::asech  , x); }
//extern "C" kirk_real_t * kirk_irram_acotanh(kirk_real_t *x) { return run11(iRRAM::acotanh, x); }
extern "C" kirk_real_t * kirk_irram_acosech(kirk_real_t *x) { return run11(iRRAM::acosech, x); }
