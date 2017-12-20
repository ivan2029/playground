#include <cassert>
#include <iostream>

#include <string>
#include <stdexcept>
using namespace std::literals;

//#define NYI throw std::runtime_error("line "s + std::to_string(__LINE__) + ": Not yet implemented"s)

//
//
//

/*
    A type that can store one of two values: one representing success
    (Ok state) and one representing failure (Err state).

    When default constructed, Result is in Ok state.

    Type is Regular if T and E are Regular.

    Type is Swappable if T and E are Swappable.

    Do not use Result value that was moved from. Assign a value to it
    first (or emplace), then use it. This is standard practice, but
    compiler can't enforce it.

    C++14 compatible.

    TODO: valueless by exception?
*/

#include <type_traits>
#include <new>
#include <initializer_list>

struct in_place_ok_t {};
constexpr in_place_ok_t const in_place_ok;

struct in_place_err_t {};
constexpr in_place_err_t const in_place_err;

template<class T, class E>
class Result {
    
  enum class Type { Ok, Err, None };

  using Storage = std::aligned_union_t<1, T, E>;
    
public: // ctor, dtor, assign
    
  Result()
    : m_type{Type::Ok}
  {
    new (ptr_ok()) T{};
  }
    
  Result(Result const& other)
    : m_type{other.m_type}
  {
    if(other.is_ok()) {
      new (ptr_ok()) T{*other.ptr_ok()};
    }
    if(other.is_err()) {
      new (ptr_err()) E{*other.ptr_err()};
    }
  }   

  Result(Result&& other)
    : m_type{other.m_type}
  {
    if(other.is_ok()) {
      new (ptr_ok()) T{std::move(*other.ptr_ok())};
    }
    if(other.is_err()) {
      new (ptr_err()) E{std::move(*other.ptr_err())};
    }
    other.deinit();
  }

  template<class ... Args>
  Result(in_place_ok_t, Args&& ... args)
    : m_type{Type::Ok}
  {
    new (ptr_ok()) T{std::forward<Args>(args)...};
  }
    
  template<class U, class ... Args>
  Result(in_place_ok_t, std::initializer_list<U> ls, Args&& ... args)
    : m_type{Type::Ok}
  {
    new (ptr_ok()) T{ls, std::forward<Args>(args)...};
  }
    
  template<class ... Args>
  Result(in_place_err_t, Args&& ... args)
    : m_type{Type::Err}
  {
    new (ptr_err()) E{std::forward<Args>(args)...};
  }
    
  template<class U, class ... Args>
  Result(in_place_err_t, std::initializer_list<U> ls, Args&& ... args)
    : m_type{Type::Err}
  {
    new (ptr_err()) E{ls, std::forward<Args>(args)...};
  }
    
  ~Result() {
    deinit();
  }
    
  Result& operator= (Result const& other) {
    if(this != &other) {
      deinit();
      if(other.is_ok()) {
        new (ptr_ok()) T{*other.ptr_ok()};
      }
      if(other.is_err()) {
        new (ptr_err()) E{*other.ptr_err()};
      }
      m_type = other.m_type;
    }
    return *this;
  }
    
  Result& operator= (Result&& other) {
    if(this != &other) {
      deinit();
      if(other.is_ok()) {
        new (ptr_ok()) T{std::move(*other.ptr_ok())};
      }
      if(other.is_err()) {
        new (ptr_err()) E{std::move(*other.ptr_err())};
      }
      m_type = other.m_type;
      other.deinit();
    }
    return *this;
  }
    
public: // equality comparable

  friend auto operator== (Result const& a, Result const& b) -> bool {
    bool result = false;
    if(a.m_type == b.m_type) {
      if(a.is_ok()) {
        result = *a.ptr_ok() == *b.ptr_ok();
      }
      if(a.is_err()) {
        result = *a.ptr_err() == *b.ptr_err();
      }
      // two Nones are not equal! 
    }
    return result;
  }
    
  friend auto operator!= (Result const& a, Result const& b) -> bool {
    return !(a == b);
  }
    
public: // observers 

  auto is_ok() const -> bool {
    return Type::Ok == m_type;
  }

  auto is_err() const -> bool {
    return Type::Err == m_type;
  }
    
  auto ok() & -> T& {
    assert(is_ok());
    return *ptr_ok();
  }
    
  auto ok() const & -> T const& {
    assert(is_ok());
    return *ptr_ok();
  }
    
  auto ok() && -> T&& {
    assert(is_ok());
    return std::move(*ptr_ok());
  }

  /*
  auto ok() const && -> T const&& {
    assert(is_ok());
    return std::move(*ptr_ok());
  }
  */
    
  auto err() & -> E& {
    assert(is_err());
    return *ptr_err();
  }
    
  auto err() const & -> E const& {
    assert(is_err());
    return *ptr_err();
  }
    
  auto err() && -> E&& {
    assert(is_err());
    return std::move(*ptr_err());
  }
  
  /*
  auto err() const && -> E const&& {
    assert(is_err());
    return std::move(*ptr_err());
  }
  */
    
public: //    
    
  auto unwrap() & -> T {
    assert(is_ok());
    return *ptr_ok();
  }

  auto unwrap() const& -> T {
    assert(is_ok());
    return *ptr_ok();
  }

  auto unwrap() && -> T&& {
    assert(is_ok());
    return std::move(*ptr_ok());
  }

  /*
  auto unwrap() const&& -> T const&& {
    assert(is_ok());
    return std::move(*ptr_ok());
  }
  */
    
public: // swap

  auto swap(Result& other) {
    Result<T,E> temp(std::move(other));
    other = std::move(*this);
    *this = std::move(temp);
  }

public: // modifiers

  template<class ... Args>
  auto emplace_ok(Args&& ... args) -> T&  {
    deinit();
    m_type = Type::Ok;
    new (ptr_ok()) T{std::forward<Args>(args)...};
    return *ptr_ok();
  }
    
  template<class U, class ... Args>
  auto emplace_ok(std::initializer_list<U> ls, Args&& ... args) -> T& {
    deinit();
    m_type = Type::Ok;
    new (ptr_ok()) T{ls, std::forward<Args>(args)...};
    return *ptr_ok();
  }
    
  template<class ... Args>
  auto emplace_err(Args&& ... args) -> E& {
    deinit();
    m_type = Type::Err;
    new (ptr_err()) E{std::forward<Args>(args)...};
    return *ptr_err();
  }
    
  template<class U, class ... Args>
  auto emplace_err(std::initializer_list<U> ls, Args&& ... args) -> E& {
    deinit();
    m_type = Type::Err;
    new (ptr_err()) E{ls, std::forward<Args>(args)...};
    return *ptr_err();
  }
    
public: // 

  template<class Fn>
    // Fn :: T -> U
    // map :: (Result<T, E>, T -> U) -> Result<U, E>
  auto map(Fn&& fn) const& {
    using OkT = std::decay_t<decltype(fn(ok()))>;
    using Res = Result<OkT, E>;
    if(is_ok()) return Res{in_place_ok, fn(ok())};
    else        return Res{in_place_err, err()};
  }
  
  template<class Fn>
    // Fn :: T -> U
    // map :: (Result<T, E>, T -> U) -> Result<U, E>
  auto map(Fn&& fn) && {
    using OkT = std::decay_t<decltype(fn(ok()))>;
    using Res = Result<OkT, E>;
    if(is_ok()) return Res{in_place_ok, fn(std::move(*this).ok())};
    else        return Res{in_place_err, std::move(*this).err()};
  }
    
  template<class Fn>
    // Fn :: E -> U
    // map :: (Result<T, E>, E -> U) -> Result<T, U>
  auto map_err(Fn&& fn) const& {
    using ErrT = std::decay_t<decltype(fn(err()))>;
    using Res = Result<T, ErrT>;
    if(is_ok()) return Res{in_place_ok, ok()};
    else        return Res{in_place_err, fn(err())};
  }
  
  template<class Fn> 
    // Fn :: E -> U
    // map :: (Result<T, E>, E -> U) -> Result<T, U>
  auto map_err(Fn&& fn) && {
    using ErrT = std::decay_t<decltype(fn(err()))>;
    using Res = Result<T, ErrT>;
    if(is_ok()) return Res{in_place_ok, std::move(*this).ok()};
    else        return Res{in_place_err, fn(std::move(*this).err())};
  }

  template<class Fn> 
    // Fn :: T -> Result<U, E>
    // and_then :: ( Result<T, E>, T -> Result<U, E> ) -> Result<U, E> 
  auto and_then(Fn&& fn) const& {
    using Ret = std::decay_t<decltype(fn(ok()))>;
    if(is_ok()) return fn(ok());
    else        return Ret{in_place_err, err()};
  }
  
  template<class Fn> 
    // Fn :: T -> Result<U, E>
    // and_then :: ( Result<T, E>, T -> Result<U, E> ) -> Result<U, E> 
  auto and_then(Fn&& fn) && {
    using Ret = std::decay_t<decltype(fn(ok()))>;
    if(is_ok()) return fn(std::move(*this).ok());
    else        return Ret{in_place_err, std::move(*this).err()};
  }
  
private: // helpers

  auto ptr_ok() -> T* {
    return reinterpret_cast<T*>(&m_storage);
  }

  auto ptr_ok() const -> T const* {
    return reinterpret_cast<T const*>(&m_storage);
  }

  auto ptr_err() -> E* {
    return reinterpret_cast<E*>(&m_storage);
  }

  auto ptr_err() const -> E const* {
    return reinterpret_cast<E const*>(&m_storage);
  }

  auto deinit() -> void {
    if(is_ok()) {
      ptr_ok()->~T();
    }
    if(is_err()) {
      ptr_err()->~E();
    }
    m_type = Type::None;
  }
    
private: // fields

  Type     m_type;
  Storage  m_storage;

};

template<class T, class E>
auto swap(Result<T, E>& a, Result<T, E>& b) -> void {
  a.swap(b);
}

template<class T, class Fn>
auto try_result(Fn&& fn) -> Result<T, std::exception_ptr> {
    Result<T, std::exception_ptr> res{in_place_err};
    try {
        res.emplace_ok( fn() );
    }
    catch(...) {
        res.emplace_err(std::current_exception());
    }
    return res;
}

//
//
//
#include <vector>
#include <string>

struct Test {
  Test()
    : default_constructed{true}
  {}

  Test(Test const&)
    : copy_constructed{true}
  {}

  Test(Test&&)
    : move_constructed{true}
  {}

  ~Test() {}

  Test& operator= (Test const&) {
    copy_assigned = true;
    return *this;
  }

  Test& operator= (Test&&) {
    move_assigned = true;
    return *this;
  }

  //
  bool default_constructed{false};
  bool copy_constructed{false};
  bool move_constructed{false};
  bool copy_assigned{false};
  bool move_assigned{false};
};


auto main() -> int {

  //
  {
    Result<int, std::string> res;
        
    assert(res.is_ok());
    assert(!res.is_err());
  }
    
  //
  {
    std::vector<int> const expected = {1, 2, 3, 4, 5};
    Result<std::vector<int>, std::string> res{in_place_ok, {1, 2, 3, 4, 5}};
        
    assert(res.ok() == expected);
  }
    
  //
  {
    Result<std::vector<int>, std::string> res{in_place_err, "error"};
        
    assert(res.err() == "error");
  }
    
  {
    std::vector<int> const expected = {1, 2, 3, 4, 5};
    Result<std::vector<int>, std::string> res{in_place_ok, {1, 2, 3, 4, 5}};
        
    assert(std::move(res).ok() == expected);
  }
    
  //
  {
    Result<std::vector<int>, std::string> res{in_place_err, "error"};
        
    assert(std::move(res).err() == "error");
  }
    
  //
  {
    Result<int, int> a{in_place_ok, 42}, b{in_place_ok, 42}, c{in_place_ok, 53};
        
    assert(a == b);
    assert(a != c);
  }
  
  //
  {
    Result<int, int> res;
        
    assert(res.is_ok());
        
    res.emplace_ok(42);
        
    assert(res.is_ok() && res.ok() == 42);
        
    res.emplace_err(15);
        
    assert(res.is_err() && res.err() == 15);
  }
    
  //
  {
    Result<int, int> a{in_place_ok, 42}, b{in_place_err, 15};

    swap(a, b);

    assert(a.is_err() && a.err() == 15);
    assert(b.is_ok() && b.ok() == 42);
  }

  //
  {
    Result<Test, int> res{};

    assert(res.ok().default_constructed);

    res.emplace_ok();

    assert(res.ok().default_constructed);

    auto t1 = std::move(res).ok();

    assert(t1.move_constructed);

    res.emplace_ok();

    auto t2 = res.ok();

    assert(t2.copy_constructed);
  }
  
  //
  {
    Result<int, std::string> res{in_place_ok, 42};

    assert(res.unwrap() == 42);
  }
  
  //
  {
    Result<int, std::string> res{in_place_ok, 42};
    
    auto res1 = res.map([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res1.is_ok() && res1.ok() == "< 42 >"s);
    
    auto res2 = std::move(res).map([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res2.is_ok() && res2.ok() == "< 42 >"s);
  }
  
  //
  {
    auto const error_message = "error message"s;
    Result<int, std::string> res{in_place_err, error_message};
    
    auto res1 = res.map([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res1.is_err() && res1.err() == error_message);
  }
  
  //
  {
    Result<int, int> res{in_place_err, 42};
    
    auto res1 = res.map_err([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res1.is_err() && res1.err() == "< 42 >"s);
    
    auto res2 = std::move(res).map_err([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res2.is_err() && res2.err() == "< 42 >"s);
  }
  
  //
  {
    Result<int, int> res{in_place_ok, 42};
    
    auto res1 = res.map_err([](int x) { 
      return "< "s + std::to_string(x) + " >"s;
    });
    
    assert(res.is_ok() && res.ok() == 42);
  }
  
  //
  {
    auto res = try_result<int>([]{
      return 42;
    });

    assert(res.is_ok() && res.ok() == 42);
  }
  
  //
  {
    auto positive = [](int x) -> Result<std::string, std::string> {
        if(x > 0) return Result<std::string, std::string>{ in_place_ok, "positive"s};
        else      return Result<std::string, std::string>{in_place_err, "negative"s};
    };
    
    Result<int, std::string> ok_pos{in_place_ok, 42}
                           , ok_neg{in_place_ok, -42}
                           , err{in_place_err, "meh"} 
                           ;
    auto res1 = ok_pos.and_then(positive);
    assert(res1.is_ok() && res1.ok() == "positive");

    auto res2 = ok_neg.and_then(positive);
    assert(res2.is_err() && res2.err() == "negative");

    auto res3 = err.and_then(positive);
    assert(res3.is_err() && res3.err() == "meh");
  }
   
  //
  {
    auto const error_message = "test_error"s;
    auto res = try_result<int>([&]() -> int{
      throw std::runtime_error(error_message);
    });

    assert(res.is_err());
    bool exception_handled{false};
    try {
        std::rethrow_exception(res.err());
    }
    catch(std::exception const& e) {
        exception_handled = true;
        assert(e.what() == error_message);
    }
    assert(exception_handled);
  }
}
