
#include "kirk-iRRAM.hh"

using iRRAM::REAL;
using iRRAM::kirk::process;
using iRRAM::kirk::out_real;
using std::shared_ptr;

struct cnst_real {
	kirk_real_t parent;
	double value;
};

static void set_approx(struct kirk_apx_t *apx, double d, kirk_abs_t a)
{
	mpfr_set_prec(apx->center, 53);
	mpfr_set_d(apx->center, d, MPFR_RNDN);
	apx->radius.mantissa = 1;
	apx->radius.exponent = a;
}

static kirk_ret_t cnst_approx_abs(const struct kirk_real_t *r, kirk_apx_t *apx, kirk_abs_t a)
{
	const struct cnst_real *cr = (const struct cnst_real *)r;
	set_approx(apx, cr->value, a);
	return KIRK_SUCCESS;
}

static kirk_ret_t cnst_approx_eff(const struct kirk_real_t *r, kirk_apx_t *apx, kirk_eff_t e)
{
	const struct cnst_real *cr = (const struct cnst_real *)r;
	set_approx(apx, cr->value, -e);
	return KIRK_SUCCESS;
}

static const struct kirk_real_class_t cnst_class = {
	/* .ref       = */ [](kirk_real_t *r){ return r; },
	/* .unref     = */ [](kirk_real_t * ){},
	/* .apx_abs   = */ cnst_approx_abs,
	/* .apx_eff   = */ cnst_approx_eff,
};

static void my_exp(const REAL *in, REAL *out) { out[0] = exp(in[0]); }

/* could be part of iRRAM's "kirk" C-API */
extern "C" kirk_real_t * iRRAM_kirk_exp(kirk_real_t *x)
{
	kirk_real_t *in[] = {x};
	shared_ptr<process> p = process::create(in, 1, 1, my_exp);
	return new out_real(move(p), 0);
}

int main(int argc, char **argv)
{
	iRRAM_initialize(argc, argv);
	cnst_real c  = { { &cnst_class }, 3.75 };

	kirk_real_t *ex0 = iRRAM_kirk_exp(&c.parent);

	kirk_apx_t apx;
	kirk_apx_init(&apx);
	kirk_real_apx_abs(ex0, &apx, -53);
	mpfr_printf("%RNf +/- %u*2^%d\n", apx.center, apx.radius.mantissa, apx.radius.exponent);
	kirk_apx_fini(&apx);
	kirk_real_unref(ex0);
}
