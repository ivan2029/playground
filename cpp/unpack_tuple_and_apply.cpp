#include <iostream>
#include <tuple>
#include <type_traits>

template<class T, class F, std::size_t ... Is>
void for_each_tuple_el_impl(T&& t, F&& f, std::index_sequence<Is...>)
{
  using IntS = int[sizeof...(Is)];
  (void)IntS{ (f(std::get<Is>(t)), 0) ... };
}

template<class T, class F>
void for_each_tuple_el(T&& t, F&& f)
{
  constexpr auto tuple_size = std::tuple_size<std::decay_t<T> >::value;
	for_each_tuple_el_impl( std::forward<T>(t)
	                      , std::forward<F>(f)
	                      , std::make_index_sequence< tuple_size >{} );
}

template<class T, class F, std::size_t ... Is>
auto apply_tuple_impl(T&& t, F&& f, std::index_sequence<Is...>)
{
  return f( std::get<Is>(t)... );
}

template<class T, class F>
auto apply_tuple(T&& t, F&& f)
{
  constexpr auto tuple_size = std::tuple_size<std::decay_t<T> >::value;
  return apply_tuple_impl( std::forward<T>(t)
                         , std::forward<F>(f)
                         , std::make_index_sequence< tuple_size > {} );
}


int main() {
  auto tup = std::make_tuple(42, 3.141, "hello");
	
  auto print = [](auto const& x){ std::cout << x << "\n"; };
  for_each_tuple_el(tup, print);

  auto action = [](int x, double d, char const* s){
    std::cout << x << ", " << d <<", " << s << "\n";
  };
  apply_tuple(tup, action);
  
	return 0;
}
