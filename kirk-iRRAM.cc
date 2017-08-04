
#include "kirk-iRRAM.hh"

using std::unique_lock;
using std::shared_lock;
using std::vector;
using std::swap;
using std::unique_ptr;
using std::shared_ptr;
using std::weak_ptr;

using iRRAM::REAL;
using iRRAM::DYADIC;
using iRRAM::sizetype;

using namespace kirk::irram;

static ::kirk_abs_t to_accuracy(sizetype err)
{
	while (err.mantissa > 1) {
		err.exponent++;
		err.mantissa = (err.mantissa + 1) >> 1;
	}
	return err.exponent;
}

real_out_sock::real_out_sock()
: effort(0)
{
	::kirk_apx_init(&apx);
}

void real_out_sock::offer(const DYADIC &d, const sizetype &err)
{
	//KIRK_SOCK_DEBUG("::put w/ effort %u\n",e);
	std::unique_lock<decltype(mtx)> lock(mtx);

	/* set apx */
	mpfr_set_prec(apx.center, mpfr_get_prec(d.value));
	mpfr_set(apx.center, d.value, MPFR_RNDN);
	apx.radius.exponent = err.exponent;
	apx.radius.mantissa = err.mantissa;

	/* record accuracy and effort */
	accuracy = to_accuracy(err);
	effort = (unsigned)iRRAM::state.ACTUAL_STACK.prec_step;

	/* notify every out_real waiting on us */
	cond.notify_all();
}

/* --------------------------------------------------------------------------
 * machine
 * -------------------------------------------------------------------------- */

machine::machine(::kirk_real_t *const *in,
                 size_t n_in,
                 size_t n_out,
                 machine::hide_constructor)
: outputs(n_out)
, cancelled(false)
{
	inputs.reserve(n_in);
	for (size_t i=0; i<n_in; i++)
		inputs.emplace_back(::kirk_real_ref(in[i]));
}

machine::~machine()
{
	cancelled = true;
	output_requested.notify_one();
}

void machine::computation_prepare(vector<REAL> &in)
{
	in.reserve(inputs.size());
	::kirk_apx_t apx;
	::kirk_abs_t acc = iRRAM::state.ACTUAL_STACK.actual_prec;
//	::kirk_eff_t eff = (unsigned)iRRAM::state.ACTUAL_STACK.prec_step;
	::kirk_apx_init2(&apx, -acc);
	DYADIC dd;

	/* mtx_in: get lock on inputs busy */
	for (const auto &i : inputs) {
		::kirk_real_apx_abs(i.get(), &apx, acc);
		sizetype err = { apx.radius.mantissa, apx.radius.exponent };
		sizetype_normalize(err);
		swap(*dd.value, *apx.center);
		in.emplace_back(dd);
	}
	/* release mtx_in */

	::kirk_apx_fini(&apx);
}

void machine::computation_finished(const vector<REAL> &out)
{
	unique_lock<decltype(mtx_outputs)> lock(mtx_outputs);
	/* mtx_out: get lock on outputs busy */
	DYADIC d;
	sizetype err;
	for (size_t i=0; i<outputs.size(); i++) {
		out[i].to_formal_ball(d, err);
		outputs[i].offer(d, err);
	}
	/* release mtx_out */

	//KIRK_MACHINE_DEBUG("%s", " finished computation g()\n");
	output_requested.wait(lock, [this]{
		return cancelled ||
		       (unsigned)iRRAM::state.ACTUAL_STACK.prec_step < max_effort_requested ||
		       iRRAM::state.ACTUAL_STACK.actual_prec > max_accuracy_requested;
	});
}

int machine::compute(std::weak_ptr<machine> wp, func_type f)
{
	//KIRK_MACHINE_DEBUG(" iterating w/ effort %u...\n",
	//		    (effort_t)state.ACTUAL_STACK.prec_step);
	iRRAM::state.infinite = 0;
	std::vector<REAL> in, out;

	if (std::shared_ptr<machine> p = wp.lock()) {
		out.resize(p->outputs.size());
		p->computation_prepare(in);
	} else
		return 0;

	/*
	std::vector<BaseSock_t> locked_outputs(outputs.size());
	for (auto &s : outputs)
		locked_outputs.push_back(s.lock());*/
	f(in.data(), out.data());

	if (std::shared_ptr<machine> p = wp.lock()) {
		p->computation_finished(out);
		iRRAM::state.infinite = !p->cancelled;
	}

	return 0;
}

void machine::exec(std::function<void(const REAL *,REAL *)> f)
{
	std::weak_ptr<machine> wp = shared_from_this();
	std::thread([wp,f]{
		try {
			iRRAM::exec(compute, wp, f);
		} catch (const char *) {
		}
	}).detach();
}

void machine::run(::kirk_abs_t a)
{
	{
		std::unique_lock<decltype(mtx_outputs)> lock(mtx_outputs);
		if (a < max_accuracy_requested) {
			max_accuracy_requested = a;
			output_requested.notify_one();
		}
	}
	//KIRK_MACHINE_DEBUG("::run_abs(%u)\n", a);
}

void machine::run(::kirk_eff_t e)
{
	{
		std::unique_lock<decltype(mtx_outputs)> lock(mtx_outputs);
		if (e > max_effort_requested) {
			max_effort_requested = e;
			output_requested.notify_one();
		}
	}
	//KIRK_MACHINE_DEBUG("::run_eff(%u)\n", e);
}

/* --------------------------------------------------------------------------
 * kirk_real_t
 * -------------------------------------------------------------------------- */

static ::kirk_real_t * real_ref(::kirk_real_t *r)
{
	return new out_real(*static_cast<out_real *>(r));
}

static void real_unref(::kirk_real_t *r)
{
	delete static_cast<out_real *>(r);
}

static ::kirk_ret_t real_apx_abs(const ::kirk_real_t *r, ::kirk_apx_t *apx, ::kirk_abs_t a)
{
	return static_cast<const out_real *>(r)->request_abs(apx, a);
}
/*
static ::kirk_ret_t real_apx_eff(const ::kirk_real_t *r, ::kirk_apx_t *apx, ::kirk_eff_t e)
{
	return static_cast<const out_real *>(r)->request_eff(apx, e);
}
*/
static const ::kirk_real_class_t real_class = {
	real_ref,
	real_unref,
	real_apx_abs,
	kirk_real_apx_eff_abs, //	real_apx_eff,
};

/* --------------------------------------------------------------------------
 * out_real
 * -------------------------------------------------------------------------- */

out_real::out_real(std::shared_ptr<machine> proc, size_t out_idx)
: ::kirk_real_t { &real_class, }
, proc(move(proc))
, out_idx(out_idx)
{}

::kirk_ret_t out_real::request_abs(::kirk_apx_t *apx, ::kirk_abs_t a) const
{
	real_out_sock &os = proc->outputs[out_idx];
	//KIRK_SOCK_DEBUG("::get w/ effort %u\n", e);
	std::shared_lock<decltype(os.mtx)> lock(os.mtx);
	proc->run(a);
	os.cond.wait(lock, [&]{return a >= os.accuracy;});
	kirk_apx_cpy(apx, &os.apx);
	return KIRK_SUCCESS;
}
/*
::kirk_ret_t out_real::request_eff(::kirk_apx_t *apx, ::kirk_eff_t e) const
{
	real_out_sock &os = proc->outputs[out_idx];
	//KIRK_SOCK_DEBUG("::get w/ effort %u\n", e);
	std::shared_lock<decltype(os.mtx)> lock(os.mtx);
	proc->run(e);
	os.cond.wait(lock, [&]{return e <= os.effort;});
	kirk_apx_cpy(apx, &os.apx);
	return KIRK_SUCCESS;
}
*/
