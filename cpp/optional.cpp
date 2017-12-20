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
    A type that either carries a value or does not.

    When default constructed, Optional carries nothing.

    Type is Regular if T is Regular.

    Type is Swappable if T is Swappable.

    Do not use Optional value that was moved from. Assign a value to it
    first (or emplace), then use it. This is standard practice, but
    compiler can't enforce it.

    C++14 compatible.
*/

#include <type_traits>
#include <new>
#include <initializer_list>

struct in_place_t {};
constexpr in_place_t const in_place;

template<class T>
class Optional {
    
  using Storage = std::aligned_union_t<1, T>;
    
public: // ctor, dtor, assign
    
  Optional()
    : m_has_value{false}
  {}
    
  Optional(Optional const& other)
    : m_has_value{other.has_value()}
  {
    if(other.value()) {
      new (ptr()) T{*other.ptr()};
    }
  }   

  Optional(Optional&& other)
    : m_has_value{other.has_value()}
  {
    if(other.has_value()) {
      new (ptr()) T{std::move(*other.ptr())};
    }
    other.deinit();
  }

  template<class U>
  explicit Optional(U&& u)
    : m_has_value{true}
  {
    new (ptr()) T{std::forward<U>(u)};
  }

  template<class ... Args>
  Optional(in_place_t, Args&& ... args)
    : m_has_value{true}
  {
    new (ptr()) T{std::forward<Args>(args)...};
  }
    
  template<class U, class ... Args>
  Optional(in_place_t, std::initializer_list<U> ls, Args&& ... args)
    : m_has_value{true}
  {
    new (ptr()) T{ls, std::forward<Args>(args)...};
  }
    
    
  ~Optional() {
    deinit();
  }
    
  Optional& operator= (Optional const& other) {
    if(this != &other) {
      deinit();
      if(other.has_value()) {
        m_has_value = other.has_value();
        new (ptr()) T{other.value()};
      }
    }
    return *this;
  }
    
  Optional& operator= (Optional&& other) {
    if(this != &other) {
            deinit();
      if(other.has_value()) {
        m_has_value = other.has_value();
        new (ptr()) T{std::move(other).value()};
      }
      other.deinit();
    }
    return *this;
  }
    
public: // equality comparable

  friend auto operator== (Optional const& a, Optional const& b) -> bool {
    bool result = false;
    if(a.has_value() == b.has_value()) {
        if(a.has_value()) { // implies b.has_value() == true
            result = a.value() == b.value();
        }
        else { 
            result = true;
        }
    }
    return result;
  }
    
  friend auto operator!= (Optional const& a, Optional const& b) -> bool {
    return !(a == b);
  }
    
public: // observers 

  auto has_value() const -> bool {
    return m_has_value;
  }
  
  operator bool () const { 
    return m_has_value;
  }
  
  auto value() & -> T& {
    assert(has_value());
    return *ptr();
  }
    
  auto value() const & -> T const& {
    assert(has_value());
    return *ptr();
  }
    
  auto value() && -> T&& {
    assert(has_value());
    return std::move(*ptr());
  }

  auto operator* () & -> T& {
    assert(has_value());
    return *ptr();
  }
    
  auto operator* () const & -> T const& {
    assert(has_value());
    return *ptr();
  }
    
  auto operator* () && -> T&& {
    assert(has_value());
    return std::move(*ptr());
  }

  auto operator-> () -> T* {
    assert(has_value());
    return ptr();
  }
  
  auto operator-> () const -> T* {
    assert(has_value());
    return ptr();
  }
  
  template<class U>
  auto value_or(U&& u) const& -> T {
    if(has_value()) return value();
    else            return T{std::forward<U>(u)};
  }

  template<class U>
  auto value_or(U&& u) && -> T {
    if(has_value()) return std::move(*this).value();
    else            return T{std::forward<U>(u)};
  }

public: //    
    
  auto unwrap() & -> T {
    return *ptr();
  }

  auto unwrap() const& -> T {
    return *ptr();
  }

  auto unwrap() && -> T&& {
    return std::move(*ptr());
  }
    
public: // swap

  auto swap(Optional& other) -> void {
    auto tmp = std::move(other);
    other = std::move(*this);
    *this = std::move(tmp);
  }

public: // modifiers

  void reset() {
    deinit();
  }

  template<class ... Args>
  auto emplace(Args&& ... args) -> T&  {
    deinit();
    new (ptr()) T{std::forward<Args>(args)...};
    m_has_value = true;
    return value();
  }
    
  template<class U, class ... Args>
  auto emplace(std::initializer_list<U> ls, Args&& ... args) -> T& {
    deinit();
    new (ptr()) T{ls, std::forward<Args>(args)...};
    m_has_value = true;
    return value();
  }

public: //

  // TODO: map
  template<class Fn>
    // Fn :: T -> U
    // map :: ( Optional<T>, T -> U ) -> Optional<U>
  auto map(Fn&& fn) const& {
    using U = std::decay_t<decltype(fn(value()))>;
    using Ret = Optional<U>;
    if(has_value()) return Ret{fn(value())};
    else            return Ret{};
  }
 
  template<class Fn>
    // Fn :: T -> U
    // map :: ( Optional<T>, T -> U ) -> Optional<U>
  auto map(Fn&& fn) && {
    using U = std::decay_t<decltype(fn(value()))>;
    using Ret = Optional<U>;
    if(has_value()) return Ret{fn(std::move(*this).value())};
    else            return Ret{};
  }
  
  template<class Fn>
    // Fn :: T -> Optional<U>
    // and_then :: ( Optional<T>, T -> Optional<U> ) -> Optional<U>
  auto and_then(Fn&& fn) const& {
    using Ret = std::decay_t<decltype(fn(value()))>;
    if(has_value()) return fn(value());
    else            return Ret{};
  }
  
  template<class Fn>
    // Fn :: T -> Optional<U>
    // and_then :: ( Optional<T>, T -> Optional<U> ) -> Optional<U>
  auto and_then(Fn&& fn) && {
    using Ret = std::decay_t<decltype(fn(value()))>;
    if(has_value()) return fn(std::move(*this).value());
    else            return Ret{};
  }
  
private: // helpers

  auto ptr() -> T* {
    return reinterpret_cast<T*>(&m_storage);
  }

  auto ptr() const -> T const* {
    return reinterpret_cast<T const*>(&m_storage);
  }

  auto deinit() -> void {
    if(has_value()) {
      ptr()->~T();
    }
    m_has_value = false;
  }
    
private: // fields

  bool     m_has_value;
  Storage  m_storage;

};

template<class T>
auto swap(Optional<T>& a, Optional<T>& b) -> void {
  a.swap(b);
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
    Optional<int> opt;
    
    assert(!opt.has_value());
  }

  //
  {
    Optional<int> opt{42};
        
    assert(opt.has_value());
    assert(opt.value() == 42);
  }
   
  //
  {
    Optional<int> opt;
    
    assert(!opt);
  }

  //
  {
    Optional<int> opt{42};
        
    assert(opt);
    assert(*opt == 42);
  }
    
  //
  {
    std::vector<int> const expected = {1, 2, 3, 4, 5};
    Optional<std::vector<int>> opt{in_place, {1, 2, 3, 4, 5}};
        
    assert(opt.value() == expected);
  }
        
  {
    std::vector<int> const expected = {1, 2, 3, 4, 5};
    Optional<std::vector<int>> opt{in_place, {1, 2, 3, 4, 5}};
        
    assert(std::move(opt).value() == expected);
  }
    
    
  //
  {
    Optional<int> a{42}, b{42}, c{53};
        
    assert(a == b);
    assert(a != c);
  }
  
  //
  {
    Optional<int> opt;
        
    assert(!opt.has_value());
        
    opt.emplace(42);
        
    assert(opt.has_value() && opt.value() == 42);
        
    opt.reset();
        
    assert(!opt.has_value());
  }
    
  //
  {
    Optional<int> a{42}, b{15};

    swap(a, b);

    assert(a.value() == 15);
    assert(b.value() == 42);
  }

  //
  {
    Optional<Test> opt{in_place};

    assert(opt.value().default_constructed);

    opt.emplace();

    assert(opt.value().default_constructed);

    auto t1 = std::move(opt).value();

    assert(t1.move_constructed);

    opt.emplace();

    auto t2 = opt.value();

    assert(t2.copy_constructed);
  }
  
  //
  {
    Optional<Test> opt{in_place};

    assert(opt->default_constructed);

    opt.emplace();

    assert(opt->default_constructed);

    auto t1 = std::move(opt).value();

    assert(t1.move_constructed);

    opt.emplace();

    auto t2 = opt.value();

    assert(t2.copy_constructed);
  }
  
  //
  {
    Optional<int> opt{42};

    assert(opt.unwrap() == 42);
  }

  //
  {
    Optional<int> opt{};
    
    assert(opt.value_or(42) == 42);
    
    opt.emplace(10);
    
    assert(opt.value_or(42) == 10);
  }

  //
  auto to_string_ = [](auto const& x) { return "< "s + std::to_string(x) + ">"s; };

  //
  {
    auto const expected = to_string_(42);
    Optional<int> opt{42};
    
    auto opt1 = opt.map(to_string_);
    assert(opt1 && *opt1 == expected);
  
    auto opt2 = std::move(opt).map(to_string_);
    assert(opt2 && *opt2 == expected);
  }
  
  //
  {
    Optional<int> opt{};
    
    auto opt1 = opt.map(to_string_);
    assert(!opt1);
  
    auto opt2 = std::move(opt).map(to_string_);
    assert(!opt2);
  }
  
  //
  auto positive_ = [](int const x) { 
    if(x > 0) return Optional<int>{x};
    else      return Optional<int>{};
  };
  
  //
  {
    Optional<int> x{42}, y{-42}, z{};
    
    auto x1 = x.and_then(positive_);
    assert(x1 && *x == 42);

    auto y1 = y.and_then(positive_);
    assert(!y1);

    auto z1 = z.and_then(positive_);
    assert(!z1);    
  }
  
}