// g++-9 -std=c++2a -fconcepts

#include <type_traits>

//
// overloaded visitor
//

template<class ... Fns>
struct overloaded : public Fns ... {
  using Fns::operator() ... ;
};

template<class ... Fns>
overloaded(Fns&&...) -> overloaded< std::decay_t<Fns>...>;

//
//
//
#include <iostream>
#include <string>
#include <string_view>
#include <variant>

namespace concepts {
    
    template<class T>
    concept arithmetic = std::is_arithmetic_v<T>;

} // concepts

auto main() -> int {
  using namespace std::string_literals;
  std::variant<int, float, std::string> var{ "hello, world"s };
  
  auto const visitor = overloaded {
    [](std::string_view sv)  { 
        std::cout << "string: " << sv << "\n"; 
    },
    [](concepts::arithmetic x) { 
        std::cout << "number: " << x << "\n"; 
    }
  };
  
  std::visit(visitor, var); // prints: "string: hello, world\n"
  
  var = 42;
  
  std::visit(visitor, var); // prints: "number: 42\n"
  
  return 0;
}
