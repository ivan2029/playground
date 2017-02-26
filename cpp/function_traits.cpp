#include <type_traits>
#include <functional>

namespace traits{ namespace detail {

  template<int N, class ... Ts>
  struct get_nth;

  template<int N, class H, class ... Ts>
  struct get_nth<N, H, Ts...>
  {
    using type = typename get_nth< N-1, Ts... >::type;
  };

  template<class H, class ... Ts>
  struct get_nth<0, H, Ts... >
  {
    using type = H;
  };
}} // detail / traits

namespace traits {

  template<class F>
  struct function_traits;
  
  template<class R, class ... Args>
  struct function_traits< R(Args...) >
  {
    enum { arity = sizeof...(Args) };
  
    using result_type = R;

    template<int N>
    using arg_type = typename detail::get_nth<N, Args...>::type;
  };

  template<class R, class ... Args>
  struct function_traits< R(*)(Args...) >
    : public function_traits< R(Args...) >
  {};

  template<class C, class R, class ... Args>
  struct function_traits< R(C::*)(Args...) >
    : public function_traits< R(C&, Args...) >
  {};

  template<class C, class R, class ... Args>
  struct function_traits< R(C::*)(Args...) const >
    : public function_traits< R(C const&, Args...) >
  {};

  template<class R, class ... Args>
  struct function_traits< std::function< R(Args...) > >
    : public function_traits< R(Args...) >
  {};

} // traits
  
namespace traits {
  // found this on StackOverflow
  template<class T>
  struct closure_traits
    : public closure_traits< decltype(&T::operator()) >
  {};

#define REM_CTOR(...) __VA_ARGS__
#define SPEC(cv, var, is_var)                                     \
template<class C, class R, class ... Args>                        \
struct closure_traits< R(C::*)(Args... REM_CTOR var) cv >         \
{                                                                 \
  enum : std::size_t                                              \
  {                                                               \
    arity = sizeof...(Args)                                       \
  };                                                              \
  enum : bool                                                     \
  {                                                               \
    is_variadic = std::integral_constant<bool, is_var>::value     \
  };                                                              \
  enum : bool                                                     \
  {                                                               \
    is_const = std::is_const<int cv>::value                       \
  };                                                              \
  using result_type = R;                                          \
  template<int N>                                                 \
  using arg_type = typename detail::get_nth<N, Args...>::type;    \
};

SPEC(const, (,...), 1)
SPEC(const, (), 0)
SPEC(, (,...), 1)
SPEC(, (), 0)

#undef REM_CTOR
#undef SPEC
};

using namespace traits;

int func(char, double);

static_assert(
  std::is_same<
    int
    , typename function_traits< decltype(func) >::result_type
  >::value
  , ""
);

static_assert(
  2 == function_traits< decltype(func) >::arity
  , ""
);

static_assert(
  std::is_same<
    char
    , function_traits< decltype(func) >::arg_type<0>
  >::value
  , ""
);

static_assert(
  std::is_same<
    double
    , function_traits< decltype(func) >::arg_type<1>
  >::value
  , ""
);

/* this one fails - as it should
static_assert(
  std::is_same<
    int
    , function_traits< decltype(func) >::arg_type<0>
  >::value
  , ""
);
*/

class C
{
public:
  void method(int, bool);
  void cmethod(int, bool) const;
};

using method_t = decltype(&C::method);
using cmethod_t = decltype(&C::cmethod);

static_assert(
  3 == function_traits< method_t >:: arity
  ,""
);

static_assert(
  std::is_same<
    void
    , typename function_traits< method_t >::result_type
  >::value
  , ""
);

static_assert(
  std::is_same<
    C&
    , typename function_traits< method_t >::arg_type<0>
  >::value
  , ""
);

static_assert(
  std::is_same<
    int
    , typename function_traits< method_t >::arg_type<1>
  >::value
  , ""
);

static_assert(
  std::is_same<
    bool
    , typename function_traits< method_t >::arg_type<2>
  >::value
  , ""
);

static_assert(
  3 == function_traits< cmethod_t >:: arity
  ,""
);

static_assert(
  std::is_same<
    void
    , typename function_traits< cmethod_t >::result_type
  >::value
  , ""
);

static_assert(
  std::is_same<
    C const&
    , typename function_traits< cmethod_t >::arg_type<0>
  >::value
  , ""
);

static_assert(
  std::is_same<
    int
    , typename function_traits< cmethod_t >::arg_type<1>
  >::value
  , ""
);

static_assert(
  std::is_same<
    bool
    , typename function_traits< cmethod_t >::arg_type<2>
  >::value
  , ""
);

using func_t = std::function<int(char, double, int*)> ;

static_assert(
  std::is_same<
    int
    , typename function_traits< func_t >::result_type
  >::value
  , ""
);

static_assert(
  3 == function_traits< func_t >::arity
  , ""
);

static_assert(
  std::is_same<
    char
    , typename function_traits< func_t >::arg_type<0>
  >::value
  , ""
);

static_assert(
  std::is_same<
    double
    , typename function_traits< func_t >::arg_type<1>
  >::value
  , ""
);

static_assert(
  std::is_same<
    int*
    , typename function_traits< func_t >::arg_type<2>
  >::value
  , ""
);


auto inc = [](int x) { return x + 1; };

static_assert(
  std::is_same<
    int
    , typename closure_traits< decltype(inc) >::result_type
  >::value
  , ""
);

static_assert(
  1 == closure_traits< decltype(inc) >::arity
  , ""
);

static_assert(
  std::is_same<
    int
    , closure_traits< decltype(inc) >::arg_type<0>
  >::value
  , ""
);
