// from Joel Falcou's talk at Meeting C++ 2015 https://youtu.be/idPThkw2p6c?t=49m39s
#include <iostream>
#include <type_traits>

template<class ... Ts>
struct DT;

#define DEDUCED_TYPE(expr) DT< decltype(expr) > dt


template<class Rep>
struct Tuple
{
  Rep rep;
};

template<class ... Ts>
auto tuple(Ts ... ts)
{
  auto rep = [=](auto v) -> decltype(auto) { return v(ts...); }; 
  using rep_t = decltype(rep); 
  return Tuple<rep_t>{ rep };
}

template<class Tup>
std::size_t size(Tup const& tup)
{
  auto size_fn = [](auto const&... members){ 
    return sizeof...(members); 
  };
  return tup.rep(size_fn);
}

template<class H, class ... Ts>
auto get_nth_arg(std::integral_constant<std::size_t, 0>, H&& h, Ts&& ...) 
    -> decltype(auto)
{
  return std::forward<H>(h);
}

template<std::size_t N, class H, class ... Ts>
auto get_nth_arg(std::integral_constant<std::size_t, N>, H&&, Ts&& ... ts) 
    -> decltype(auto)
{
  return get_nth_arg(std::integral_constant<std::size_t, N - 1>{}, std::forward<Ts>(ts)...);
}

template<std::size_t N, class Tup>
auto get(Tup&& tup) -> decltype(auto)
{
  auto get_fn = [](auto&& ... members) -> decltype(auto) {
    return get_nth_arg(std::integral_constant<std::size_t, N>{}, members...);
  };
  return tup.rep(get_fn);
}

int main() {
  auto tup = tuple(1, "one", 3.416);
  
  std::cout << size(tup) << "\n";
  
  //DEDUCED_TYPE(get<0>(tup)); // deduced type is DT<int const&>
  //get<0>(tup) = 5; // this won't compile as a result
  
  std::cout << get<0>(tup) << "\n"
            << get<1>(tup) << "\n"
            << get<2>(tup) << "\n";
  
  return 0;
}