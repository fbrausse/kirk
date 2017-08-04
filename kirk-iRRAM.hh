
#ifndef KIRK_IRRAM_HH
#define KIRK_IRRAM_HH

#include <shared_mutex>		/* std::shared_timed_mutex */
#include <atomic>		/* std::atomic_bool */
#include <thread>

#include <iRRAM/lib.h>		/* iRRAM::REAL */

#include "kirk-c-types.h"

namespace iRRAM { namespace kirk {

struct real_out_sock {
	std::shared_timed_mutex     mtx;
	std::condition_variable_any cond;
	::kirk_apx_t                 apx;
	::kirk_eff_t                 effort;
	::kirk_abs_t                 accuracy;

	real_out_sock();

	/* called by process::computation_finished() with a result */
	void offer(const DYADIC &d, const sizetype &err);
};

struct kirk_real_unref_del {
	void operator()(::kirk_real_t *r) { ::kirk_real_unref(r); }
};

class process : public std::enable_shared_from_this<process> {
	std::vector<std::unique_ptr<::kirk_real_t,kirk_real_unref_del>> inputs;
	std::vector<real_out_sock> outputs;

	std::atomic_bool cancelled;
	/* protects max_(effort|accuracy)_requested and output_requested */
	std::mutex mtx_outputs;
	std::condition_variable output_requested;
	::kirk_eff_t max_effort_requested;
	::kirk_abs_t max_accuracy_requested;

	/* to disallow creation of processes w/o make_shared */
	struct hide_constructor {};

public:
	process(::kirk_real_t *const *in, size_t n_in, size_t n_out, hide_constructor);
	~process();

	static std::shared_ptr<process> create(::kirk_real_t *const *in,
	                                       size_t n_in,
	                                       size_t n_out,
	                                       std::function<void(const REAL *,REAL *)> f)
	{
		std::shared_ptr<process> p = std::make_shared<process>(in, n_in, n_out, hide_constructor{});
		p->exec(move(f));
		return p;
	}

private:
	void computation_prepare(std::vector<REAL> &in);
	void computation_finished(const std::vector<REAL> &out);

	static int compute(std::weak_ptr<process> wp, std::function<void(const REAL *,REAL *)> f);

	void exec(std::function<void(const REAL *,REAL *)> f);
	void run(::kirk_abs_t a);
	void run(::kirk_eff_t e);

	friend struct real_out_sock;
	friend struct out_real;
};

extern "C" struct out_real : ::kirk_real_t {
	std::shared_ptr<process> proc;
	size_t out_idx;

	explicit out_real(std::shared_ptr<process> proc, size_t out_idx);

	kirk_ret_t request_abs(::kirk_apx_t *apx, ::kirk_abs_t a) const;
	kirk_ret_t request_eff(::kirk_apx_t *apx, ::kirk_eff_t e) const;
};

}}

#endif
