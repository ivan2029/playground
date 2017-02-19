/*
  Compiler with good support for c++17 needed. Tested with g++ 6.1.1
*/

#include <utility>
#include <type_traits>
#include <tuple>
#include <experimental/optional>

// not currently implemented by gcc so, here it is...
namespace __apply_tuple {
  template<class Fn, class Tup, std::size_t ... Is>
  auto impl(Fn fn, Tup&& tup, std::index_sequence<Is...>) -> decltype(auto)  {
    return fn( std::get<Is>(tup)... );
  }
}

template<class Fn, class Tup>
auto apply_tuple(Fn fn, Tup&& tup) -> decltype(auto) {
  constexpr auto tuple_size = std::tuple_size<std::decay_t<Tup> >::value;
  return __apply_tuple::impl( std::move(fn)
                            , std::forward<Tup>(tup)
                            , std::make_index_sequence< tuple_size > {} );
}

//

/*
  Given: 
    f[1] :: A[0]   -> A[1]
    f[2] :: A[1]   -> A[2]
      ...
    f[n] :: A[n-1] -> A[n]

  Then compose(f[n], f[n-1], ... f[1]) is function object that is equivalent to:
    
    auto composed(A[0] arg) -> A[n] {
      return f[n]( f[n-1] ( ... f[1](arg) ) );
    }
*/
namespace __compose {
  
  template<class Fn>
  struct WrapFn {
    Fn fn;
  };

  template<class T, class Fn>
  auto operator >>= (WrapFn<Fn> wrap_fn, T&& t) -> decltype(auto) {
    return wrap_fn.fn(std::forward<T>(t));
  }
  
  template<class ... Fns>
  auto compose_wraps(WrapFn<Fns> ... wrap_fns)  {
    return [=](auto&& arg) -> decltype(auto) {
      return ( wrap_fns >>= ... >>= std::forward<decltype(arg)>(arg) );
    };
  }

} // __compose

template<class ... Fns>
auto compose(Fns ... fns) {
  return __compose::compose_wraps(
    __compose::WrapFn<Fns>{std::move(fns)}...
  );
}

/*
  Given: 
    f[1] :: A[0]   -> A[1]
    f[2] :: A[1]   -> A[2]
      ...
    f[n] :: A[n-1] -> A[n]

  Then chain(f[1], f[2], ... f[n]) is function object that is equivalent to:
    
    auto chained(A[0] arg) -> A[n] {
      return f[n]( f[n-1] ( ... f[1](arg) ) );
    }
    
  Note: chain(f[1], f[2], ... f[n]) behaves as compose(f[n], f[n-1], ... f[1]) and vice versa
*/

namespace __chain {
  
  template<class Fn>
    struct WrapFn {
    Fn fn;
  };

  template<class T, class Fn>
  auto operator >> (T&& t, WrapFn<Fn> wrap_fn) -> decltype(auto) {
    return wrap_fn.fn(std::forward<T>(t));
  }
  
  template<class ... Fns>
  auto chain_wraps(WrapFn<Fns> ... wrap_fns)  {
    return [=](auto&& arg) -> decltype(auto) {
      return ( std::forward<decltype(arg)>(arg) >> ... >> wrap_fns );
    };
  }

} // __chain

template<class ... Fns>
auto chain(Fns ... fns) {
  return __chain::chain_wraps(
    __chain::WrapFn<Fns>{std::move(fns)}...
  );
}

/*
  Partial application: given f :: ( A[1] , A[2], ... A[n] ) -> B , and k < n arguments, then
    
  partial_apply(f, x[1], ... x[k]) behaves as
    
  auto fp(A[k+1] y[k+1], ... A[n] y[n]) -> B {
    return f(x[1], x[2], ..., x[k], y[k+1], ... y[n]);
  }
    
*/

template<class Fn, class ... Args>
auto partial_apply(Fn fn, Args ... args) {
  return [=](auto&& ... rest){
    return fn(args..., std::forward<decltype(rest)>(rest)...);
  };
}

/*
  Curry
*/
namespace __curry {

  template<class Fn, class ... Args>
  auto is_complete_form_(Fn&& fn, Args&& ... args) -> decltype(fn(args...), std::true_type());
    
  auto is_complete_form_(...) -> std::false_type;

  //
  template<class Fn, class ... Args>
  class CompleteForm {
  public:
    CompleteForm(Fn fn, Args&& ... args)
      : m_fn(std::move(fn))
      , m_args(std::forward<Args>(args)...)
    {}
        
    template<class Tup, class ... Args2>
    CompleteForm(Fn fn, Tup&& tup, Args2&& ... args2)
      : m_fn(std::move(fn))
      , m_args( std::tuple_cat(std::forward<Tup>(tup), std::make_tuple(std::forward<Args2>(args2)...)) )
    {}
        
    CompleteForm(CompleteForm const&) = default;
    CompleteForm(CompleteForm&&) = default;
        
    ~CompleteForm() = default;
        
    CompleteForm& operator= (CompleteForm const&) = default;
    CompleteForm& operator= (CompleteForm&&) = default;
        
  public:
    
    auto operator() () const -> decltype(auto) {
      return apply_tuple(m_fn, m_args);
      // return std::apply(m_fn, m_args);
    }
        
  private:
    Fn                  m_fn;
    std::tuple<Args...> m_args;
  };
    
  //
  template<class Fn, class ... Args>
  class Curried {
  public:
      
    Curried(Fn fn, Args&& ... args)
      : m_fn(std::move(fn))
      , m_args(std::forward<Args>(args)...)
    {}
        
    template<class Tup, class ... Args2>
    Curried(Fn fn, Tup&& tup, Args2&& ... args2)
      : m_fn(std::move(fn))
      , m_args( std::tuple_cat(std::forward<Tup>(tup), std::make_tuple(std::forward<Args2>(args2)...)) )
    {}
        
    Curried(Curried const&) = default;
    Curried(Curried&&) = default;
        
    ~Curried() = default;
        
    Curried& operator= (Curried const&) = default;
    Curried& operator= (Curried&&) = default;
        
  public:
        
    template<class H, class ... Args1>
    auto operator() (H&& h, Args1 && ... args1) const {
      using IsCompleteForm = decltype( is_complete_form_(m_fn, std::declval<Args>()..., std::declval<H>(), std::declval<Args1>()...) );
      using RetType = std::conditional_t< IsCompleteForm::value
                                        , CompleteForm<Fn, Args..., H, Args1...>
                                        , Curried<Fn, Args..., H, Args1...> >;
      return RetType(m_fn, m_args, std::forward<H>(h), std::forward<Args1>(args1)...);
    }
        
  private:
    
    Fn                  m_fn;
    std::tuple<Args...> m_args;
  };

} // __curry


template<class Fn>
auto curry(Fn fn) {
  using IsCompleteForm = decltype( __curry::is_complete_form_(fn) );
  using RetType = std::conditional_t < IsCompleteForm::value
                                     , __curry::CompleteForm<Fn>
                                     , __curry::Curried<Fn> >;
  return RetType( std::move(fn) );
}

/*
  Lazily computed value
*/

template<class F>
struct LazyValue {
  using return_type = decltype( std::declval<F>()() );

  template<class F1>
  explicit LazyValue(F1&& f)
    : m_f(std::forward<F1>(f))
  {}
  
  operator return_type () {
    if(!m_cached_value) {
      m_cached_value = m_f(); 
    }
    return *m_cached_value;
  }
  
  F m_f;
  std::experimental::optional<return_type> m_cached_value;
};

template<class F>
auto lazy_value(F&& f) {
  return LazyValue<F>(std::forward<F>(f));
}

/*

  Tests

*/
#include <cassert>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include <iostream>

// helpers
int add_mul(int x, int y, int z) {
  return (x + y)*z;
}

constexpr auto&& to_string_ = [](auto&& x) { 
  return std::to_string(x); 
};

constexpr auto&& to_float = [](auto&& x) { 
  return static_cast<float>(x); 
};

auto enclose_with = [](auto lpar, auto rpar) {
  return [=](auto str){ return lpar + str + rpar; };
};

template<class R, class O, class T>
O transform(R&& r, O&& o, T&& t) {
  return std::transform( std::begin(r), std::end(r)
                       , std::forward<O>(o)
                       , std::forward<T>(t) );
}

template<class R, class F>
auto for_each(R&& r, F&& f) {
  return std::for_each( std::begin(r), std::end(r)
                      , std::forward<F>(f) );
}

// tests
void test_compose() {
  auto comp = compose( enclose_with("~", "~")
                     , enclose_with("<", ">")
                     , enclose_with("-", "-")
                     , to_string_ );
  assert( "~<-123->~" == comp(123) );
}

void test_chain() {
  auto ch = chain( to_string_
                 , enclose_with("-", "-")
                 , enclose_with("<", ">")
                 , enclose_with("~", "~") );
  assert( "~<-123->~" == ch(123) );               
}

void test_partial_apply() {
  /*
    add_mul :: (int, int, int) -> int
    am :: int -> int
  */
  auto am = partial_apply(add_mul, 1, 2);

  assert( am(3) == 9 );
}

void test_curry_multiple_args() {
  /*
    add_mul :: (int, int, int) -> int
    am :: int -> int -> int -> int
    am1 :: int -> int -> int
    am2 :: int -> int
    am3 :: () -> int
  */
  auto am = curry(add_mul);
  auto am1 = am(1); 
  auto am2 = am1(2);
  auto am3 = am2(3);
    
  assert( am3() == 9 );
}

void test_curry_no_args() {    
  /*
    bm :: () -> int
  */
    
  auto bm = curry([]{ return 42; });
  assert( bm() == 42 );
}

void test_compose_1() {
  std::vector<int> const         is = {1,2,3,4,5};
  std::vector<std::string>       ss;
  std::vector<std::string> const expected_ss = {"[1]", "[2]", "[3]", "[4]", "[5]" };

  // to_string_ :: Int -> String
  // enclose    ::        String -> String
  
  ::transform( is
             , back_inserter(ss)
             , compose(enclose_with("[", "]"), to_string_) );
  
  assert(expected_ss == ss);
}

void test_compose_2() {
  std::vector<int> const         is = {1,2,3,4,5};
  std::vector<std::string>       ss;
  std::vector<std::string> const expected_ss = {
      "~<[1]>~"
    , "~<[2]>~"
    , "~<[3]>~"
    , "~<[4]>~"
    , "~<[5]>~"
  };

  // to_string_ :: Int -> String
  // enclose1   ::        String -> String
  // enclose2   ::                  String -> String
  // enclose3   ::                            String -> String
  
  auto transform_fn = compose( enclose_with("~", "~")
                             , enclose_with("<", ">")
                             , enclose_with("[", "]")
                             , to_string_ ) ;
  ::transform( is
             , back_inserter(ss)
             , transform_fn);

  assert(expected_ss == ss);
}

struct Contact {
  std::string       display_name;
  std::string       group_name;
  std::string       uri;
  std::vector<int>  phone_numbers;
};

std::string const& get_uri(Contact const& contact) {
  return contact.uri;
}

std::string get_user_name(std::string const& uri) {
  auto iterator_to_at = std::find(uri.begin(), uri.end(), '@');
  return std::string( uri.begin(), iterator_to_at );
}

void test_compose_3() {
  std::vector<Contact> const contacts = {
      {"Name 1", "Group 1", "one@number.set", {}}
    , {"Name 2", "Group 2", "two@number.set", {}}
    , {"Name 3", "Group 4", "three@number.set", {}}
  };
    
  std::vector<std::string> names;
  std::vector<std::string> const expected_names = {
      "one"
    , "two"
    , "three"
  };

  auto io_action = [&](auto name) {
    names.push_back( std::move(name) );
  };

  // get_uri       :: Contact -> String
  // get_user_name ::            String -> String
  // io_action     ::                      String -> ()
  
  ::for_each( contacts
            , compose(io_action, get_user_name, get_uri) );
    
  assert(expected_names == names);
}

constexpr auto&& first = [](auto&& pair) -> decltype(auto) {
  return pair.first;
};

constexpr auto&& second = [](auto&& pair) -> decltype(auto) {
  return pair.second;
};

constexpr auto&& delete_fn = [](auto ptr) {
  delete ptr;
};

void test_compose_4() {
  struct OnDestroy {
    explicit OnDestroy(int& count_) : count{count_}{}
    ~OnDestroy(){ ++count; }
    int& count;
  };
  
  int count = 0;
  std::map<int, OnDestroy*> pairs {
      {1, new OnDestroy{count}}
    , {2, new OnDestroy{count}}
    , {3, new OnDestroy{count}}
    , {4, new OnDestroy{count}}
  };
    
  ::for_each( pairs
            , compose(delete_fn, second) );
  
  assert(4 == count);
}

void test_lazy() {
  int call_count = 0;

  auto lazy_answer = lazy_value( [&] {
                                    ++ call_count;
                                    return 42;
                                  });

  assert( 0 == call_count );

  // we are explicit about type of variable
  int x = lazy_answer;
  int y = lazy_answer;
  int z = lazy_answer;
  int t = lazy_answer;

  assert( 1 == call_count );
  assert( 42 == x );
  assert( 42 == y );
  assert( 42 == z );
  assert( 42 == y );
}

int main() {    
  test_compose();
  test_chain();
  test_partial_apply();
  test_curry_multiple_args();
  test_curry_no_args();
  test_compose_1();
  test_compose_2();
  test_compose_3();
  test_compose_4();
  test_lazy();
  
  return 0;
}
