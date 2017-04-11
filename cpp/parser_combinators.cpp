#include <cassert>
#include <iostream>
#include <utility>
#include <string>
#include <vector>
#include <cctype>
#include <tuple>
#include <type_traits>
#include <experimental/optional>

using namespace std::literals;
namespace stde = std::experimental;

struct void_t {};

bool operator== (void_t const, void_t const) { return true; }
bool operator!= (void_t const, void_t const) { return false; }

std::ostream& operator<< (std::ostream& out, void_t) {
  return out << "void";
}

//
//
//
struct str_view {
  std::string::const_iterator begin;
  std::string::const_iterator end;
};

bool empty(str_view  in) {
  return in.begin == in.end;
}

char peek(str_view in) {
  assert(!empty(in));
  return *in.begin;
}

str_view advance(str_view in) {
  return {in.begin + 1, in.end};
}

str_view to_str_view(std::string const& in) {
  return { in.begin(), in.end() };
}

std::string to_str(str_view in) {
  return std::string(in.begin, in.end);
}

//
//
//
template<class T>
using parse_result = stde::optional< std::pair<T, str_view> >;


/*
    parser is a function of type : String -> Maybe (a, String)

    for us parser is callable object that satisfies following:
    
    template<class P>
    concept bool Parser() = requires (P p) {
      typename P::type;
      p(str_view{}) -> parse_result<typename P::type>;
    }
*/

namespace parser {

  //
  //
  //
  struct parser_base {};
    
  template<class T>
  struct is_parser :
    public std::is_base_of<parser_base, T>
  {};
  
  template<class T>
  constexpr bool is_parser_v = is_parser<T>::value;
    
  //
  //
  //
  struct spaces_parser : public parser_base {
    // parser concept
    using type = void_t;
        
    parse_result<void_t> operator()(str_view in) const {
      int count = 0;
      for(; !empty(in) && std::isspace(peek(in)); in = advance(in)) {
        ++ count;
      }
      if( count == 0 ){
        return {};
      }
      else {
        return { std::make_pair(void_t{}, in) };
      }
    }
  };
    
  constexpr auto&& spaces = spaces_parser{};
    
  //
  //
  //
  struct number_parser : public parser_base {
    // parser concept
    using type = int;
        
    parse_result<int> operator () (str_view in) const {
      if(empty(in)) return {};
      
      int count = 0;
      int sign = 1;
      int res = 0;
            
      if( peek(in) == '-' ) {
        sign = -1;
        in = advance(in);
        ++ count;
      }
            
      for(; !empty(in) && std::isdigit(peek(in)); in = advance(in)) {
        res = res*10 + (peek(in) - '0');
        ++ count;
      }
            
      if(count != 0) {
        return { std::make_pair(res*sign, in) };
      }
      else {
        return {};
      }
    }
  };

  constexpr auto&& number = number_parser{};
    
  //
  //
  //
  struct ident_parser : public parser_base {
    // parser concept
    using type = std::string;
        
    parse_result<std::string> operator() (str_view in) const {
      if(empty(in) || !std::isalpha(peek(in))) return {};
      std::string buff;
      for(; !empty(in) && std::isalnum(peek(in)); in = advance(in)) {
        buff.push_back(peek(in));
      }
      return {std::make_pair(std::move(buff), in)};
    }
  };
    
  constexpr auto&& ident = ident_parser{};
    
  //
  //
  //
  namespace __detail {
    template<class T>
    struct to_opt { using type = stde::optional<T>; };
        
    template<template<class> class M, class Tup>
    struct tuple_map ;
        
    template<template<class> class M, class ... Ts>
    struct tuple_map<M, std::tuple<Ts...>> {
      using type = std::tuple< typename M<Ts>::type ... >;
    };
        
    template<template<class> class M, class T>
    using tuple_map_t = typename tuple_map<M, T>::type;
        
    template<class ... Ts>
    bool all(Ts ... ts) {
      return (true && ... && ts) ;
    }
        
    template<class Tup, std::size_t ... Is>
    bool all_have_some(Tup const& tup, std::index_sequence<Is...> ) {
      return all(static_cast<bool>(std::get<Is>(tup)) ...);
    }
        
    template<class Tup>
    bool all_have_some(Tup const& tup) {
      return all_have_some( tup
                          , std::make_index_sequence< std::tuple_size< std::decay_t<Tup> >::value >{} );
    }
        
    template<class Tup, std::size_t ... Is>
    auto from_opt(Tup tup, std::index_sequence<Is...>) {
      return std::make_tuple( *std::get<Is>(tup) ... );
    }
          
    template<class Tup>
    auto from_opt(Tup tup) {
      assert(all_have_some(tup));
      return from_opt( std::move(tup)
                     , std::make_index_sequence< std::tuple_size< std::decay_t<Tup> >::value >{} );
    }
        
    //
    //
    //
    template<std::size_t Cur, std::size_t Size, class Ps, class Res>
    struct apply_cat_parsers_t {
      void operator() (Ps const& ps, Res& res, str_view& in) const {
        auto parse_res = std::get<Cur>(ps)(in);
        if(parse_res) {
          in = parse_res->second;
          std::get<Cur>(res) = std::move(parse_res)->first;
          apply_cat_parsers_t<Cur + 1, Size, Ps, Res>{}(ps, res, in);
        }
      }
    };
        
    template<std::size_t Size, class Ps, class Res>
    struct apply_cat_parsers_t<Size, Size, Ps, Res> {
      void operator() (Ps const&, Res& , str_view& ) const {
      }
    };
        
    template<std::size_t Cur, std::size_t Size, class Ps, class Res>
    void apply_cat_parsers(Ps const& ps, Res& res, str_view& in) {
      apply_cat_parsers_t<Cur, Size, Ps, Res>{}(ps, res, in);
    }
        
  } // __detail
    
  template<class ... Ts>
  struct cat_parser : public parser_base {
    // parser concept
    using type = std::tuple<typename Ts::type ...>;
        
    parse_result<type> operator () (str_view in) const {
      using tuple_opt = __detail::tuple_map_t<__detail::to_opt, type>;
            
      tuple_opt res;
            
      __detail::apply_cat_parsers<0, sizeof...(Ts) >(m_parsers, res, in);
            
      if( __detail::all_have_some(res) ) {
        return { std::make_pair( __detail::from_opt(std::move(res)), in) };
      }
      else {
        return {};
      }
    }
        
    //
    cat_parser(cat_parser const&) = default;
    cat_parser& operator= (cat_parser const&) = default;
        
    cat_parser(Ts ... ts) 
      : m_parsers{ ts... } 
    {}

    //
    std::tuple<Ts...> m_parsers;
  };
    
  template<class ... Ps>
  auto cat(Ps ... ps) 
    -> decltype( (__detail::all(is_parser_v<Ps>...), cat_parser<Ps...>{ps...}) ) 
  {
    return cat_parser<Ps...>(ps...);
  }
    
  template<class T, class U>    
  auto operator>>(T first, U second) 
    -> decltype( (is_parser_v<T>, is_parser_v<U>, cat_parser<T, U>(first, second)) ) 
  {
    return cat_parser<T, U>(first, second);
  }
    
  //
  //
  //
    
  template<class P, class M, class T>
  struct fmap_parser : public parser_base {
    // parser concept
    using type = T;
        
    parse_result<type> operator() (str_view in) const {
      auto res = m_parser(in);
      if(res) {
        return { std::make_pair( m_mapping(res->first), res->second ) };
      }
      else {
        return {};
      }
    }
        
    //
    fmap_parser(P p, M m)
      : m_parser{p}
      , m_mapping{m}
    {}
        
    fmap_parser(fmap_parser const&) = default;
    fmap_parser& operator= (fmap_parser const&) = default;
        
    //
    P m_parser;
    M m_mapping;
  };
    
  template<class M, class P>
  auto fmap(M m, P p)
    -> decltype(( is_parser_v<P>, fmap_parser<P, M, decltype(m(std::declval<typename P::type>()))>(p, m)))
  {
    return fmap_parser<P, M, decltype(m(std::declval<typename P::type>()))>(p, m);
  }
    
  //
  //
  //
  namespace __detail {
        
    template<std::size_t Cur, std::size_t Size, class Ps, class Res>
    struct apply_alt_parsers_t {
      void operator() (Ps const& ps, Res& res, str_view& in) const {
        auto parse_res = std::get<Cur>(ps)(in);
        if(parse_res) {
          in = parse_res->second;
          res = std::move(parse_res)->first;
        } 
        else {
          apply_alt_parsers_t<Cur + 1, Size, Ps, Res>{}(ps, res, in);
        }
      }
    };
        
    template<std::size_t Size, class Ps, class Res>
    struct apply_alt_parsers_t<Size, Size, Ps, Res> {
      void operator() (Ps const& , Res& , str_view& ) const {
      }
    };
        
    template<std::size_t Cur, std::size_t Size, class Ps, class Res>
    void apply_alt_parsers(Ps const& ps, Res& res, str_view& in) {
      apply_alt_parsers_t<Cur, Size, Ps, Res>{}(ps, res, in);
    }
            
  } // __detail
    
  template<class ... Ps>
  struct alternative_parser : public parser_base {
    // parser concept
    using type = std::common_type_t<typename Ps::type ...>;
        
    parse_result<type> operator() (str_view in) const {
      stde::optional<type> res;
            
      __detail::apply_alt_parsers<0, sizeof...(Ps)>(m_parsers, res, in);
            
      if( res ) {
        return { std::make_pair( *std::move(res), in) };
      }
      else {
        return {};
      }     
    }
          
    //
    alternative_parser(Ps ... ps) 
      : m_parsers{ps...}
    {}
        
    alternative_parser(alternative_parser const&) = default;
    alternative_parser& operator= (alternative_parser const&) = default;
    
    //
    std::tuple<Ps...> m_parsers;
  };
    
  template<class ... Ps>
  auto alternative(Ps ... ps) 
    -> decltype( (__detail::all(is_parser_v<Ps>...), alternative_parser<Ps...>(ps...)) )
  {
    return alternative_parser<Ps...>(ps...);
  }
    
  template<class T, class U>
  auto operator| (T t, U u) 
    -> decltype( (is_parser_v<T>, is_parser_v<U>, alternative_parser<T, U>(t,u)) )
  {
    return alternative_parser<T, U>(t, u);
  }
    
} // parser


template<class Parser, class T>
void test_success( std::string const& input
                 , T const& expected, std::string const& rest
                 , Parser parser )
{
  auto res = parser(to_str_view(input));
  assert(res);
  assert(res->first == expected);
  assert(rest == to_str(res->second));
}


template<class Parser>
void test_failure(std::string const& input, Parser parser) {
  auto res = parser(to_str_view(input));
  assert(!res);
}

constexpr auto&& to_string_ = [](auto const& x) { return std::to_string(x); };

int main() {
  using namespace parser;
  
  // spaces
  test_success( "   1"
              , void_t{}, "1"
              , spaces );
  test_failure( "123", spaces );
  
  // number
  test_success( "-123abc"
              , -123, "abc"
              , number );
  test_failure( "abc", number );
  
  // ident
  test_success( "abc123 "
              , "abc123", " "
              , ident );
  test_failure( "", ident );
  
  // fmap
  test_success( "123abc"
              , "123", "abc"
              , fmap(to_string_, number) );
  test_failure( "abc", fmap(to_string_, number) );
  
  // cat 2
  test_success( "123abc "
              , std::make_tuple(123, "abc"s), " "
              , number >> ident );
  test_failure( "abc123", number >> ident );
  
  // cat variadic
  test_success( "123  abc "
              , std::make_tuple(123, void_t{}, "abc"s), " "
              , cat(number, spaces, ident) );
  test_failure( "abc123", cat(number, spaces, ident) );
  
  // alternative 2
  test_success( "123"
              , "123"s, ""
              , fmap(to_string_, number) | ident );
  test_success( "abc"
              , "abc"s, ""
              , fmap(to_string_, number) | ident );
  test_failure( "   ", fmap(to_string_, number) | ident );
  
  //
  std::cout << "Done!\n";
  
  return 0;
}
