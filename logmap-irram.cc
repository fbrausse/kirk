
#include "kirk-iRRAM.hh"

/* don't export the symbol */
namespace {

using iRRAM::REAL;

struct logistic_map {

	uint32_t n;

	void operator()(const REAL *in, REAL *out) const
	{
		REAL c = in[0];
		REAL x = in[1];
		for (uint32_t i=0; i<n; i++)
			x = c * x * (1-x);
		out[0] = x;
	}
};

}

extern "C"
kirk_real_t * kirk_irram_logmap(uint32_t n,
                                kirk_real_t *c,
                                kirk_real_t *x0)
{
	kirk_real_t *in[] = { c, x0 };
	kirk_real_t *out[1];
	auto machine = kirk::irram::eval(in, ARRAY_SIZE(in),
	                                 out, ARRAY_SIZE(out),
	                                 logistic_map { n });
	/* can't make use of the machine, yet, just forget it */
	(void)machine;
	return out[0];
}
