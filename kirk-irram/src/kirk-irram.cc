
#include <shared_mutex>		/* std::shared_timed_mutex */
#include <atomic>		/* std::atomic_bool */
#include <thread>

#include <iRRAM/lib.h>
#include <iRRAM/version.h>	/* iRRAM_TLS_STD */

static_assert((iRRAM_HAVE_TLS-0) && (iRRAM_TLS_STD-0),
              "iRRAM configured with --with-tls=thread_local");

#include "kirk-real-obj.h"
#include "kirk-irram.hh"
#include "log2.h"

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

typedef std::shared_ptr<kirk::irram::machine> machine_t;

/* -------------------------------------------------------------------------- */

static ::kirk_eff_t current_iRRAM_effort()
{
	return (unsigned)iRRAM::actual_stack().prec_step;
}

static void convert(sizetype &e, const ::kirk_bound_t &b)
{
	using im_t = typename iRRAM::sizetype::mantissa_t;
	/* b = bm/2^C * 2^be
	 * e = em     * 2^ee       ; ee' = D+ee
	 */
	if (!b.mantissa) {
		sizetype_exact(e);
		return;
	}
	int dbits = KIRK_BOUND_MANT_BITS - iRRAM::MANTISSA_BITS;
	e.exponent = b.exponent - iRRAM::MANTISSA_BITS;
	if (dbits > 0) {
		e.mantissa = b.mantissa >> dbits;
		if (!++e.mantissa) {
			e.mantissa >>= 1;
			e.mantissa = (im_t)1 << (iRRAM::MANTISSA_BITS-1);
			e.exponent++;
		}
	} else {
		e.mantissa = (im_t)b.mantissa << -dbits;
	}
	sizetype_normalize(e);
}

static void convert(::kirk_bound_t &b, const sizetype &e)
{
	using im_t = typename iRRAM::sizetype::mantissa_t;
	im_t m = e.mantissa;
	if (!m) {
		kirk_bound_set_zero(&b);
		return;
	}
	unsigned ms = LOG2_CEIL(m);
	/* bm/2^C * 2^be = em * 2^ee */
	b.exponent = (kirk_bound_exp_t)e.exponent + (kirk_bound_exp_t)ms;
	if (KIRK_BOUND_MANT_BITS > ms) {
		b.mantissa = (kirk_bound_mant_t)e.mantissa
		             << (KIRK_BOUND_MANT_BITS - ms);
	} else {
		b.mantissa = e.mantissa >> -(KIRK_BOUND_MANT_BITS - ms);
		kirk_bound_nextafter(&b, &b);
	}
}

static iRRAM::REAL make_REAL(const ::kirk_real_t &kr,
                             bool apx_abs,
                             DYADIC &d,
                             ::kirk_apx_t &apx,
                             ::kirk_abs_t prec)
{
	if (apx_abs)
		::kirk_real_apx_abs(&kr, &apx, prec);
	else
		::kirk_real_apx_eff(&kr, &apx, current_iRRAM_effort());
	swap(*d.value, *apx.center);
	sizetype err;
	convert(err, apx.radius);
	REAL r = d;
	r.seterror(err);
	return r;
}

iRRAM::REAL kirk::irram::make_REAL(const ::kirk_real_t &kr, bool apx_abs)
{
	::kirk_abs_t prec = iRRAM::actual_stack().actual_prec;
	::kirk_apx_t apx;
	DYADIC d;
	::kirk_apx_init2(&apx, -prec);
	REAL r = make_REAL(kr, apx_abs, d, apx, prec);
	::kirk_apx_fini(&apx);
	return r;
}

/* --------------------------------------------------------------------------
 * helper classes:
 *   real_out_sock: iRRAM will store the computed approximations there
 *   out_real     : kirk_real_t-interface, connecting to a real_out_sock
 * -------------------------------------------------------------------------- */

namespace {

struct real_out_sock {
	std::shared_timed_mutex     mtx; /* protects apx, accuracy */
	std::condition_variable_any cond;
	::kirk_apx_t                apx;
	/* stored */
	::kirk_abs_t                accuracy;
	/* requested */
	std::atomic<::kirk_abs_t>   req_accuracy;

	real_out_sock();

	/* called by process::computation_finished() with a result */
	bool offer(const iRRAM::DYADIC &d, const iRRAM::sizetype &err);
};

struct out_real : ::kirk_real_obj_t {
	const machine_t     proc;
	const size_t        out_idx;

	explicit out_real(machine_t proc, size_t out_idx);
	~out_real();

	void request_abs(::kirk_apx_t *apx, ::kirk_abs_t a) const;
	void request_eff(::kirk_apx_t *apx, ::kirk_eff_t e) const;
};

struct kirk_real_unref_del {
	void operator()(::kirk_real_t *r) { ::kirk_real_unref(r); }
};

} /* end anon namespace */

/* --------------------------------------------------------------------------
 * machine
 * -------------------------------------------------------------------------- */

struct kirk::irram::machine : std::enable_shared_from_this<machine> {

	std::vector<std::unique_ptr<::kirk_real_t,kirk_real_unref_del>> inputs;
	std::vector<real_out_sock> outputs;

	std::atomic_bool cancelled;
	/* protects
	 * - max_effort_(requested|computed)
	 * - more_accuracy_requested and
	 * - output_requested */
	std::mutex mtx_outputs;
	::kirk_eff_t max_effort_computed;
	std::condition_variable output_requested;
	::kirk_eff_t max_effort_requested;
	bool more_accuracy_requested;

	// TODO: store eventual iRRAM_Numerical_Exception(::type)

	void computation_prepare(std::vector<iRRAM::REAL> &in);
	void computation_finished(const std::vector<iRRAM::REAL> &out);

	static void compute(std::weak_ptr<machine> wp, func_type f);

	void exec(func_type f, const char *name);
	void run(real_out_sock &os, ::kirk_abs_t a);
	void run(real_out_sock &os, ::kirk_eff_t e);

	machine(::kirk_real_t *const *in, size_t n_in, size_t n_out);
	~machine();
};

machine_t kirk::irram::eval(::kirk_real_t *const *in, size_t n_in,
                            ::kirk_real_t **out     , size_t n_out,
                            func_type f, const char *name)
{
	machine_t p = std::make_shared<machine>(in, n_in, n_out);
	p->exec(move(f), name);
	for (size_t i=0; i<n_out; i++)
		out[i] = &(new out_real(p, i))->parent;
	return p;
}

/* --------------------------------------------------------------------------
 * real_out_sock
 * -------------------------------------------------------------------------- */

static ::kirk_abs_t to_accuracy(sizetype err)
{
	while (err.mantissa > 1) {
		err.exponent++;
		err.mantissa = (err.mantissa + 1) >> 1;
	}
	return err.exponent;
}

real_out_sock::real_out_sock()
: accuracy(INT32_MAX)
, req_accuracy(INT32_MAX)
{
	::kirk_apx_init(&apx);
}

bool real_out_sock::offer(const DYADIC &d, const sizetype &err)
{
	//KIRK_SOCK_DEBUG("::put w/ effort %u\n",e);
	std::unique_lock<decltype(mtx)> lock(mtx);

	/* set apx */
	mpfr_set_prec(apx.center, mpfr_get_prec(d.value));
	mpfr_set(apx.center, d.value, MPFR_RNDN);
	convert(apx.radius, err);

	/* record accuracy */
	accuracy = to_accuracy(err);

	/* notify every out_real waiting on us */
	cond.notify_all();

	return accuracy <= req_accuracy;
}

/* --------------------------------------------------------------------------
 * machine
 * -------------------------------------------------------------------------- */

machine::machine(::kirk_real_t *const *in, size_t n_in, size_t n_out)
: outputs(n_out)
, cancelled(false)
, max_effort_computed(0)
, max_effort_requested(0)
, more_accuracy_requested(false)
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
	::kirk_abs_t acc = iRRAM::actual_stack().actual_prec;
//	::kirk_eff_t eff = (unsigned)iRRAM::actual_stack().prec_step;
	::kirk_apx_init2(&apx, -acc);
	DYADIC dd;

	/* mtx_in: get lock on inputs busy */
	for (const auto &i : inputs)
		in.emplace_back(make_REAL(*i.get(), true, dd, apx, acc));
	/* release mtx_in */

	::kirk_apx_fini(&apx);
}

void machine::computation_finished(const vector<REAL> &out)
{
	unique_lock<decltype(mtx_outputs)> lock(mtx_outputs);
	/* mtx_out: get lock on outputs busy */
	DYADIC d;
	sizetype err;
	bool all_enough = true;
	for (size_t i=0; i<outputs.size(); i++) {
		out[i].to_formal_ball(d, err);
		all_enough &= outputs[i].offer(d, err);
	}
	more_accuracy_requested = !all_enough;
	max_effort_computed = current_iRRAM_effort();
	/* release mtx_out */

	//KIRK_MACHINE_DEBUG("%s", " finished computation g()\n");
	output_requested.wait(lock, [this]{
		return cancelled ||
		       more_accuracy_requested ||
		       max_effort_computed < max_effort_requested;
	});
}

void machine::compute(std::weak_ptr<machine> wp, func_type f)
{
	//KIRK_MACHINE_DEBUG(" iterating w/ effort %u...\n",
	//		    (effort_t)iRRAM::actual_stack().prec_step);
	iRRAM::state->infinite = 0;
	std::vector<REAL> in, out;

	if (std::shared_ptr<machine> p = wp.lock()) {
		out.resize(p->outputs.size());
		p->computation_prepare(in);
	} else
		return;

	/*
	std::vector<BaseSock_t> locked_outputs(outputs.size());
	for (auto &s : outputs)
		locked_outputs.push_back(s.lock());*/
	f(in.data(), out.data());

	if (std::shared_ptr<machine> p = wp.lock()) {
		p->computation_finished(out);
		iRRAM::state->infinite = !p->cancelled;
	}
}

void machine::exec(std::function<void(const REAL *,REAL *)> f, const char *name)
{
	std::weak_ptr<machine> wp = shared_from_this();
	std::thread t([wp,f]{
//		try {
			iRRAM::exec(compute, wp, f);
//		} catch (const char *) {
//		}
	});
	if (name)
		pthread_setname_np(t.native_handle(), name);
	t.detach();
}

void machine::run(real_out_sock &os, ::kirk_abs_t a)
{
	std::unique_lock<decltype(mtx_outputs)> lock(mtx_outputs);
	bool more = a < os.req_accuracy;
	if (more)
		os.req_accuracy = a;
	if ((more_accuracy_requested |= more))
		output_requested.notify_one();
	//KIRK_MACHINE_DEBUG("::run_abs(%u)\n", a);
}

void machine::run(real_out_sock &, ::kirk_eff_t e)
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

static void real_apx_abs(const ::kirk_real_t *r, ::kirk_apx_t *apx, ::kirk_abs_t a)
{
	const ::kirk_real_obj_t *tr = (const ::kirk_real_obj_t *)r;
	return static_cast<const out_real *>(tr)->request_abs(apx, a);
}
/*
static void real_apx_eff(const ::kirk_real_t *r, ::kirk_apx_t *apx, ::kirk_eff_t e)
{
	return static_cast<const out_real *>(r)->request_eff(apx, e);
}
*/
static const ::kirk_real_obj_class_t real_class = {
	{
		kirk_real_obj_default_ref,
		kirk_real_obj_default_unref,
		real_apx_abs,
		kirk_real_apx_eff_abs, //	real_apx_eff,
	},
	/* .finalize = */ kirk_real_obj_default_finalize,
};

/* --------------------------------------------------------------------------
 * out_real
 * -------------------------------------------------------------------------- */

static void out_real_destroy(kirk_real_obj_t *p)
{
	delete static_cast<out_real *>(p);
}

out_real::out_real(machine_t proc, size_t out_idx)
: ::kirk_real_obj_t KIRK_REAL_OBJ_INIT(&real_class.parent,out_real_destroy)
, proc(move(proc))
, out_idx(out_idx)
{}

out_real::~out_real()
{
	if (proc.use_count() <= 2) {
		std::unique_lock<decltype(proc->mtx_outputs)> lock(proc->mtx_outputs);
		proc->cancelled = true;
		proc->output_requested.notify_one();
	}
}

void out_real::request_abs(::kirk_apx_t *apx, ::kirk_abs_t a) const
{
	real_out_sock &os = proc->outputs[out_idx];
	//KIRK_SOCK_DEBUG("::get w/ effort %u\n", e);
	proc->run(os, a);
	std::shared_lock<decltype(os.mtx)> lock(os.mtx);
	os.cond.wait(lock, [&]{return a >= os.accuracy;});
	kirk_apx_set(apx, os.apx.center, &os.apx.radius);
}
/*
void out_real::request_eff(::kirk_apx_t *apx, ::kirk_eff_t e) const
{
	real_out_sock &os = proc->outputs[out_idx];
	//KIRK_SOCK_DEBUG("::get w/ effort %u\n", e);
	std::shared_lock<decltype(os.mtx)> lock(os.mtx);
	proc->run(e);
	os.cond.wait(lock, [&]{return e <= os.effort;});
	kirk_apx_cpy(apx, &os.apx);
}
*/
