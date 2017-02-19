//
//
//

#include <utility>

template<class F, class ... As>
void for_each_arg(F f, As&& ... as)
{
  using IntArr = int[sizeof...(As)];
  (void)IntArr{ (f(std::forward<As>(as)), 0)... };
}

//
//
//

#include <ostream>

namespace helpers
{
  void print_call_impl(std::ostream&)
  {}

  template<class H, class ... Ts>
  void print_call_impl(std::ostream& out, H const& h, Ts const& ... ts)
  {
    out << h;
    for_each_arg( [&](auto const& x){ out << ", " << x; }
                , ts... );
  }

} // helpers

template<class FN, class ... As>
void print_call(std::ostream& out, FN function_name, As const& ... as)
{
  out << function_name << "(";
  helpers::print_call_impl(out, as...);
  out << ")\n";
}

#define PRINT_CALL0(out) print_call(out, __FUNCTION__)
#define PRINT_CALL(out, ...) print_call(out, __FUNCTION__, __VA_ARGS__)

//
//
//
#include <iostream>
#include <string>

void empty_fn()
{
  PRINT_CALL0(std::cout);
}

void some_fn(int x, float y, std::string str)
{
  PRINT_CALL(std::cout, x, y, str);
}

int main()
{
  empty_fn();
  some_fn(42, 3.141, "hello");
  return 0;
}
