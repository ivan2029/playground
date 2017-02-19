// tested with g++ 4.9.1
#include <iostream>

#include <utility>
#include <type_traits>
#include <typeinfo>
#include <functional>
#include <memory>
#include <cstdint>
#include <map>
#include <array>

// lazy
template<class T>
using Lazy = std::function<T()>;

// cached lazy
namespace detail
{
  template<class T>
  class CallState
  {
    using Storage =
      typename std::aligned_storage <
          sizeof(T)
        , alignof(T)
      > :: type ;
      
  public: // construction, assignment, destruction

    template<class F>
    CallState(F&& f)
      : m_call{std::forward<F>(f)}
    {}

    CallState(CallState const& ) = delete;
    CallState(CallState&& ) = delete;

    CallState& operator= (CallState const& ) = delete;
    CallState& operator= (CallState&& ) = delete;

    ~CallState()
    {
      if(m_called) {
        ptr()->~T();  
      }
    }
    
  public:
 
    T call()
    {
      if(!m_called) {
        m_called = true;
        new (ptr()) T{m_call()};
      }
      return *ptr();
    }

  private: // methods

    inline T* ptr() { return reinterpret_cast<T*>(&m_storage); }
    
  private: // fields
  
    bool     m_called = false;
    Storage  m_storage;
    Lazy<T>  m_call;
  };
} // detail
  
template<class T>
class CachedCall
{    
public:
  CachedCall() = delete;

  template<class F>
  explicit CachedCall(F&& f)
    : m_state{ std::make_shared< detail::CallState<T> >(std::forward<F>(f)) }
  {
  }

  CachedCall(CachedCall const& other) = default;  
  CachedCall(CachedCall&& other) = default;

  CachedCall& operator= (CachedCall const& ) = default;
  CachedCall& operator= (CachedCall&& ) = default;

  ~CachedCall() = default;

public:

  T operator()() { return m_state->call(); }
  
private: // fields

  std::shared_ptr< detail::CallState<T> > m_state;
};

template<class F>
auto cached_call(F&& f)
{
  using type = std::decay_t< decltype(f()) >;
  using return_type = CachedCall< type >;

  return return_type{std::forward<F>(f)};
}

// lazy ulong and operators
Lazy<std::uint64_t> ulong(std::uint64_t x)
{ 
  return [=]() mutable {
    return x;
  };
}

Lazy<std::uint64_t> plus(Lazy<std::uint64_t> x, Lazy<std::uint64_t> y)
{
  return [=]() mutable {
    return x() + y();
  };
}

Lazy<std::uint64_t> minus(Lazy<std::uint64_t> x, Lazy<std::uint64_t> y)
{
  return [=] () mutable {
    return x() - y();
  };
}

Lazy<std::uint64_t> times(Lazy<std::uint64_t> x, Lazy<std::uint64_t> y)
{
  return [=]() mutable {
    return x() * y();
  };
}

Lazy<std::uint64_t> inc(Lazy<std::uint64_t> x)
{
  return [=]() mutable {
    return x() + 1;
  };
}

Lazy<std::uint64_t> dec(Lazy<std::uint64_t> x)
{
  return [=]() mutable{
    return x() - 1;
  };
}

// lazy conditional
template<class T>
Lazy<T> cond(Lazy<bool> c, Lazy<T> t, Lazy<T> f)
{
  return [=]() mutable {
    auto c_ = c();
    return c_ ? t() : f();
  };
}

Lazy<bool> is_zero(Lazy<std::uint64_t> n)
{
  return [=]() mutable {
    return n() == 0;
  };
}

// lazy factorial
Lazy<std::uint64_t> fac(Lazy<std::uint64_t> n)
{
  auto cn = cached_call(n);

  Lazy<std::uint64_t> rec_call =
    [=] () mutable {
      return fac( dec( cn ) )();
    };
  
  return cond<std::uint64_t> ( is_zero(cn)
                             , ulong(1)
                             , times( cn, rec_call ) );
}

// lazy fibb
namespace detail
{
  using FibbCache = std::map<std::uint64_t, std::uint64_t>;
  using FibbCachePtr = std::shared_ptr<FibbCache>;

  Lazy<std::uint64_t> memoized_fibb(FibbCachePtr cache, Lazy<std::uint64_t> n)
  {
    auto cn = cached_call(n);

    Lazy<bool> is_cached = [=]() mutable {
      return cache->find(cn()) != cache->end();
    };

    Lazy<std::uint64_t> get_from_cache = [=]() mutable {
      return cache->at(cn());
    };

    Lazy<std::uint64_t> rec_call = [=]() mutable {
      auto n_1 = memoized_fibb( cache, dec(cn) )();
      auto n_2 = memoized_fibb( cache, dec( dec(cn) ) )();

      cache->insert( std::make_pair(cn() - 1, n_1 ) );
      cache->insert( std::make_pair(cn() - 2, n_2 ) );
      
      return n_1 + n_2;
    };

    return cond<std::uint64_t> ( is_cached
                               , get_from_cache
                               , rec_call );
  }
  
} // detail

Lazy<std::uint64_t> fibb(Lazy<std::uint64_t> n)
{
  auto cache = std::make_shared< detail::FibbCache >();

  cache->insert(std::make_pair(0, 1));
  cache->insert(std::make_pair(1, 1));
  
  return detail::memoized_fibb(cache, n);
}

#include <cassert>

void test_cached_call()
{
  int call_count = 0;

  auto ci =
    cached_call( [&]{
      ++ call_count;
      return 42;
    });

  assert( ci() == 42 );
  assert( ci() == 42 );
  assert( ci() == 42 );
  assert( ci() == 42 );
  assert( call_count == 1 );
}

void test_lazy_cond()
{
  auto should_be_0 =
    cond<std::uint64_t> ( []{ return true; }
                        , ulong(0)
                        , ulong(1) );

  assert( should_be_0() == 0 );

  auto should_be_1 =
    cond<std::uint64_t> ( []{ return false; }
                        , ulong(0)
                        , ulong(1) );

  assert( should_be_1() == 1 );
}

std::uint64_t iterative_fac(std::uint64_t n)
{
  std::uint64_t p = 1;
  
  for(; n > 0; -- n) {
    p *= n;
  }
  
  return p;
}

void test_lazy_factorial()
{
  auto test = [](std::uint64_t n) {
    auto f = fac(ulong(n));
    auto f_res = f();
    //std::cout << "fac(" << n << ") = " << f_res << "\n";
    assert( f_res == iterative_fac(n) );
  };

  for(std::uint64_t n = 0; n < 11; ++ n) {
    test(n);
  }
}

std::uint64_t iterative_fibb(std::uint64_t n)
{
  std::uint64_t f_prev = 1;
  std::uint64_t f_next = 1;

  for(; n > 1; -- n) {
    auto next = f_prev + f_next;
    f_prev = f_next;
    f_next = next;
  }

  return f_next;
}

void test_lazy_fibb()
{
  auto test = [](std::uint64_t const count){
    auto f = fibb(ulong(count));
    auto f_res = f();
    //std::cout << "fibb(" << count << ") = " << f_res << "\n";
    assert( f_res == iterative_fibb(count) );
  };

  for(auto n: std::array<std::uint64_t, 5>{0, 1, 10, 100, 1000}){
    test(n);
  }
}

int main()
{
  test_cached_call();
  test_lazy_cond();
  test_lazy_factorial();
  test_lazy_fibb();
  
  return 0;
}
