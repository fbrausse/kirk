#ifndef ARIADNE_KIRK_REAL
#define ARIADNE_KIRK_REAL

#include <mpfr.h>

extern "C" {
#include "kirk-c-types.h"
}

#include "ariadne/numeric/real_interface.hpp"
#include "ariadne/numeric/floatdp.hpp"
#include "ariadne/numeric/floatmp.hpp"
#include "ariadne/numeric/float_bounds.hpp"


namespace Ariadne {

class KirkReal : RealInterface {
    ~KirkReal();
    KirkReal(kirk_real_t* r);
    FloatMPBounds _evaluate(MultiplePrecision pr) const;
    FloatDPBounds _evaluate(DoublePrecision pr) const;
    OutputStream& _write(OutputStream& os) const;
  private:
    kirk_real_t* _real;
};

//
inline FloatDP to_floatdp(kirk_bound_t const* err) {
    assert(err->exponent>-1023 && err->exponent<1023);
    return ldexp(static_cast<double>(err->mantissa),err->exponent);
}

inline
KirkReal::~KirkReal() {
    // Unreference the Kirk real
    kirk_real_unref(_real);
}

inline
KirkReal::KirkReal(kirk_real_t* r)
    : _real(kirk_real_ref(r)) // Claim a reference to the Kirk real
{
}

inline
FloatMPBounds KirkReal::_evaluate(MultiplePrecision pr) const {
    // Declare a Kirk struct bounds on a real number
    kirk_apx_t apx;
    // Compute the bounds on the real from Kirk using pr bits
    kirk_real_apx_eff(_real,&apx,pr.bits());
    // Construct an Ariadne MPFR number.
    //   The RawPtr() tag is to prevent the number '0' being interpreted
    //   as a pointer elsewhere in the code
    FloatMP c(apx.center,RawPtr());
    // Convert the Kirk radius to an Ariadne double-precision number
    FloatDP r(to_floatdp(&apx.radius));
    // Convert the centre-radius representation to lower and upper bounds
    return FloatMPBounds(sub(down,c,r),add(up,c,r));
}

inline
FloatDPBounds KirkReal::_evaluate(DoublePrecision pr) const {
    return FloatDPBounds(this->_evaluate(MultiplePrecision(pr)),pr);
}

inline
OutputStream& KirkReal::_write(OutputStream& os) const {
    return os << "KirkReal(?\?)";
}

} //namespace Ariadne

#endif /* ARIADNE_KIRK_REAL */
