
#ifndef KIRK_BOUND_HH
#define KIRK_BOUND_HH

#include "kirk-c-types.h"

namespace kirk {

struct bound : ::kirk_bound_t {

	typedef kirk_bound_exp_t  exponent_t;
	typedef kirk_bound_mant_t mantissa_t;

	static constexpr ::kirk_bound_exp_t EXPONENT_MIN  = KIRK_BOUND_EXP_MIN;
	static constexpr ::kirk_bound_exp_t EXPONENT_MAX  = KIRK_BOUND_EXP_MAX;
	static constexpr unsigned           MANTISSA_BITS = KIRK_BOUND_MANT_BITS;

	bound()
	: ::kirk_bound_t KIRK_BOUND_INIT
	{}

	bound(const ::kirk_bound_t &a)
	: ::kirk_bound_t(a)
	{}

	friend bool operator<(const bound &a, const bound &b)
	{
		return ::kirk_bound_less(&a, &b);
	}

	friend bool operator<=(const bound &a, const bound &b) { return a < b; }

	friend bool less_2exp(const bound &a, ::kirk_bound_exp_t e)
	{
		return ::kirk_bound_less_2exp(&a, e);
	}
/*
KIRK_API inline void             kirk_bound_set_zero(kirk_bound_t *b);
*/
	bound & operator=(double d)
	{
		::kirk_bound_set_d(this, d);
		return *this;
	}

	explicit operator double() const
	{
		return ::kirk_bound_get_d(this);
	}

	friend bound nextafter(const bound &b)
	{
		::kirk_bound_t r;
		::kirk_bound_nextafter(&r, &b);
		return r;
	}

	friend bound operator+(const bound &a, const bound &b)
	{
		::kirk_bound_t r;
		::kirk_bound_add(&r, &a, &b);
		return r;
	}

	friend bound & operator+=(bound &a, const bound &b)
	{
		::kirk_bound_add(&a, &a, &b);
		return a;
	}

	friend bound add_2exp(const bound &a, ::kirk_bound_exp_t e)
	{
		::kirk_bound_t r;
		::kirk_bound_add_2exp(&r, &a, e);
		return r;
	}

	friend bound max(const bound &a, const bound &b)
	{
		::kirk_bound_t r;
		::kirk_bound_max(&r, &a, &b);
		return r;
	}

	friend bound operator*(const bound &a, const bound &b)
	{
		::kirk_bound_t r;
		::kirk_bound_mul(&r, &a, &b);
		return r;
	}

	friend bound & operator*=(bound &a, const bound &b)
	{
		::kirk_bound_mul(&a, &a, &b);
		return a;
	}

	friend bound & operator<<=(bound &a, ::kirk_bound_exp_t e)
	{
		::kirk_bound_shift(&a, &a, e);
		return a;
	}

	friend bound operator<<(const bound &a, ::kirk_bound_exp_t e)
	{
		bound r = a;
		return r <<= e;
	}

	friend bound & operator>>=(bound &a, ::kirk_bound_exp_t e)
	{
		return a <<= -e;
	}

	friend bound operator>>(const bound &a, ::kirk_bound_exp_t e)
	{
		bound r = a;
		return r >>= e;
	}
};

}

#endif
