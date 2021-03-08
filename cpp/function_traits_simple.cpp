/*
  limitations:
    only one operator() in class (works with lambdas)
    return type and arg types must be concrete
*/

#include <iostream>
#include <string>
#include <vector>
#include <type_traits>

//
//
//
template<class ... Ts> struct type_pack;

//
//
//
template<class F> struct function_traits;

template<class R, class ... Args>
struct function_traits< R(Args...) > {
    enum { arity = sizeof ... (Args) };
    using return_type = R;
    using arg_types   = type_pack<Args...>;
};

template<class R, class ... Args>
struct function_traits< R (&)(Args...) >
    : public function_traits< R(Args...) >
{};

template<class R, class ... Args>
struct function_traits< R (*)(Args...) >
    : public function_traits< R(Args...) >
{};

template<class O, class R, class ... Args>
struct function_traits< R (O::*)(Args...) >
    : public function_traits< R(Args...) >
{};

template<class O, class R, class ... Args>
struct function_traits< R (O::*)(Args...) const >
    : public function_traits< R(Args...) >
{};

template<class O>
struct function_traits
    : public function_traits< decltype( &O::operator() ) >
{};


//
//
//
auto junk(int, float, char*) -> int;

using junk_traits = function_traits<decltype(junk)>;

static_assert( std::is_same_v< junk_traits::return_type, int > );
static_assert( std::is_same_v< junk_traits::arg_types, type_pack<int, float, char*> > );
static_assert( junk_traits::arity == 3 );

//
struct has_op {
    auto operator() (int, float, char*)  -> int;
};

using has_op_traits = function_traits<has_op>;

static_assert( std::is_same_v< has_op_traits::return_type, int > );
static_assert( std::is_same_v< has_op_traits::arg_types, type_pack<int, float, char*> > );
static_assert( has_op_traits::arity == 3 );


//
auto lambda = [](int, float, char*) -> int { return 0; };

using lambda_traits = function_traits<decltype(lambda)>;

static_assert( std::is_same_v< lambda_traits::return_type, int > );
static_assert( std::is_same_v< lambda_traits::arg_types, type_pack<int, float, char*> > );
static_assert( lambda_traits::arity == 3 );

auto main() -> int {
    return 0;
}
