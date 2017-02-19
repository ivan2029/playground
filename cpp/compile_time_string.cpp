// very unsafe! heads and tails will crash if given empty lists!

#include <iostream>
#include <string>
#include <typeinfo>

// you need gcc for this
#include <cxxabi.h>

std::string demangle(std::string name)
{
  using namespace std;
  int status;
  char* realname = abi::__cxa_demangle(name.c_str(), 0,0, &status);

  string result(realname);
  free(realname);
  return result;
}

template<class T>
std::string type_name()
{
  return demangle( typeid(T).name() );
}

//
// static string
//
template<char ... Cs> struct static_string {};

// append str c = str ++ [c]
template<class Str, char C> struct append;

template<char C, char ... Cs>
struct append< static_string<Cs...>, C>
{
  using type = static_string<Cs..., C>;
};

// str
namespace str
{
  constexpr char head(char const* ptr)
  {
    return *ptr;
  }

  constexpr char const* tail(char const* ptr)
  {
    return ptr + 1;
  }

  constexpr std::size_t length(char const* ptr)
  {
    return
      head(ptr) == 0
      ? 0
      : 1 + length(tail(ptr))
      ;
  }

  constexpr bool empty(char const* ptr)
  {
    return length(ptr) == 0;
  }

  constexpr char nth(char const* ptr, std::size_t n)
  {
    return ptr[n];
  }
}

// from_str
#define TOKEN_PASTEx(x, y) x ## y
#define TOKEN_PASTE(x, y) TOKEN_PASTEx(x, y)

#define FROM_STR_IMPL(strS, typeT, typeT_ns)                 \
namespace typeT_ns {                                         \
  constexpr auto string_to_build_from = strS;                \
                                                             \
  template<std::size_t N>                                    \
  struct from_str_impl                                       \
  {                                                          \
    using type =                                             \
      typename append<                                       \
        typename from_str_impl< N - 1>::type                 \
        , str::nth( string_to_build_from, N - 1)             \
      >::type;                                               \
  };                                                         \
                                                             \
  template<>                                                 \
  struct from_str_impl<0>                                    \
  {                                                          \
    using type = static_string<>;                            \
  };                                                         \
}                                                            \
using typeT = typeT_ns::from_str_impl<str::length(typeT_ns::string_to_build_from)>::type

// put it in global namespace
#define FROM_STR(str, type) FROM_STR_IMPL(str, type, TOKEN_PASTE(type, __COUNTER__))

//
// test
//

namespace detail
{
  template<class Str> struct as_string_impl;

  template<char ... Cs>
  struct as_string_impl< static_string<Cs...> >
  {
    std::string operator()() const
    {
      return {Cs...};
    }
  };
}  

template<class Str>
std::string as_string()
{
  return detail::as_string_impl<Str>{}();
}

FROM_STR("compile time string!", built_string);

int main()
{
  std::cout << type_name<built_string>() << "\n";
  std::cout << as_string<built_string>() << "\n";
  
  return 0;
}
