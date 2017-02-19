#include <cassert>
#include <utility>
#include <type_traits>
#include <tuple>
#include <functional>
#include <vector>
#include <memory>
#include <iostream>
#include <sstream>
#include <string>

//
//
//

template<class ... Ts>
struct deduced_type;

#define DEDUCE(expr) deduced_type< decltype(expr) > dt

//
//
//

template<class F, class Tup, std::size_t ... Indices>
auto apply_tuple(F fn, Tup&& tup, std::index_sequence<Indices...> )
{
  return fn( std::get<Indices>(tup)... );
}

template<class F, class Tup>
auto apply_tuple(F fn, Tup&& tup)
{
  return apply_tuple( std::move(fn)
                    , std::forward<Tup>(tup)
                    , std::make_index_sequence< std::tuple_size< std::decay_t<Tup> >::value >{} );
}

template<class F, class Tup, std::size_t ... Indices>
void for_each_el(F f, Tup&& tup, std::index_sequence<Indices...>)
{
    using arr = int[sizeof...(Indices)];
    (void)arr{ (f(std::get<Indices>(tup)), 0)... };
}

template<class F, class Tup>
void for_each_el(F f, Tup&& tup)
{
    for_each_el( std::move(f)
               , std::forward<Tup>(tup)
               , std::make_index_sequence< std::tuple_size< std::decay_t<Tup> >::value>{} );
}

//
//
//
namespace meta
{
  //
  template<class R, class ... As>
  struct make_args_tuple;

  template<class R, class ... As>
  struct make_args_tuple< R(As...) >
  {
    using type = std::tuple< std::decay_t<As>... >;
  };

  template<class R, class ... As>
  struct make_args_tuple< R(&)(As...) >
    : public make_args_tuple< R(As...) >
  {};

  template<class R, class ... As>
  struct make_args_tuple< R(*)(As...) >
    : public make_args_tuple< R(As...) >
  {};

  template<class C, class R, class ... As>
  struct make_args_tuple< R(C::*)(As...) >
    : public make_args_tuple< R(As...) >
  {};

  template<class C, class R, class ... As>
  struct make_args_tuple< R(C::*)(As...) const >
    : public make_args_tuple< R(As...) >
  {};
  
  template<class R, class ... As>
  struct make_args_tuple< std::function< R(As...)> >
    : public make_args_tuple< R(As...) >
  {};

  template<class L>
  struct make_args_tuple<L>
    : public make_args_tuple< decltype( &L::operator() ) >
  {};
  
  template<class F>
  using make_args_tuple_t = typename make_args_tuple<F>::type;

  //
  template< template<class> class F, class Tup>
  struct map_tuple;

  template< template<class> class F, class ... Ts>
  struct map_tuple<F, std::tuple<Ts...> >
  {
    using type = std::tuple< F<Ts>... >;
  };

  template< template<class> class F, class Tup >
  using map_tuple_t = typename map_tuple<F, Tup>::type;
  
}// meta

//
//
//


//
//
//

namespace __gen
{
  template<class G, class F>
  struct Map
  {
    G m_gen_fn;
    F m_mapping;
    
    Map(G gen_fn, F mapping)
      : m_gen_fn(gen_fn)
      , m_mapping(mapping)
    {}

    Map() = default;
    Map(Map const&) = default;
    Map(Map&&) = default;

    ~Map() = default;

    Map& operator= (Map const&) = default;
    Map& operator= (Map&&) = default;

    auto operator()() const
    {
      return m_mapping(m_gen_fn());
    }
  };

  template<class G, class F>
  auto make_map(G gen_fn, F mapping) -> Map<G, F>
  {
    return { gen_fn, mapping };
  }
  
} // __gen

template<class T>
struct Gen
{
  using value  = T;
  using gen_fn = std::function<T()>;
  //
  Gen() = default;
  Gen(Gen const&) = default;
  Gen(Gen&&) = default;

  Gen(gen_fn gen)
    : m_gen(std::move(gen))
  {}
  
  ~Gen() = default;

  Gen& operator= (Gen const& ) = default;
  Gen& operator= (Gen&& ) = default;
  Gen& operator= (gen_fn gen)
  {
    m_gen = std::move(gen);
    return *this;
  }
  
  //
  bool is_valid() const
  {
    return static_cast<bool>(m_gen);
  }
  
  operator bool () const
  {
    return is_valid();
  }
  
  T sample() const
  {
    assert( is_valid() );
    return m_gen();
  }
  
  //
  
  // TODO: map, filter, zip, zip_with...
  template<class F>
  auto map(F mapping) const
  {
    using ret_t = Gen< decltype(mapping(std::declval<T>())) >;
    typename ret_t::gen_fn gen = __gen::make_map( m_gen, mapping ); 
    return ret_t( std::move(gen) );
  }


  
  // 
  gen_fn m_gen;
};

template<class G>
using value_of_gen_t = typename G::value;

template<class T>
struct DefaultGen;

template<>
struct DefaultGen<int>
{
    Gen<int> operator()() const
    {
        return {[c = 0]()mutable{ return c++; }};
    }
};

namespace __detail
{
  template<class Tup, std::size_t ... Indices>
  auto sample(Tup const& tup, std::index_sequence<Indices...>)
    -> meta::map_tuple_t<value_of_gen_t, Tup>
  {
    return std::make_tuple( std::get<Indices>(tup).sample()... );
  }
} // __detail

template<class ... Ts>
auto sample(std::tuple< Gen<Ts> ... > const& tup_of_gens)
  -> std::tuple<Ts...>
{
  return __detail::sample(tup_of_gens, std::make_index_sequence<sizeof...(Ts)>{});
}

template<class F>
auto make_gen_for(F)
  -> meta::map_tuple_t<Gen, meta::make_args_tuple_t<F> >
{
  return {};
}

//
//
//
struct TestResult
{
    bool        passed;
    std::string info;
};

struct PackedProperty
{
    virtual ~PackedProperty() = default;
    virtual TestResult test_once() = 0;
    
    std::string property_name;
};

template<class Tup>
std::string tup_to_string(Tup const& tup)
{
    std::ostringstream sout;
    sout << "tuple(";
    for_each_el([&](auto const& el){ sout << el << ", "; }, tup);
    sout << ")";
    return sout.str();
}

template<class P, class G>
struct PackedPropertyImpl
    : public PackedProperty
{
    PackedPropertyImpl(P property, G gen)
        : m_property(std::move(property))
        , m_gen(std::move(gen))
    {}
    
    TestResult test_once() override
    {
        TestResult tr;
        
        try{
            auto sampled = sample(m_gen);
            tr.passed = apply_tuple(m_property, sampled);
            tr.info   = tup_to_string(sampled);
        }
        catch(std::exception const& e){
            tr.passed = false;
            tr.info += "exception{";
            tr.info += e.what();
            tr.info += "}";
        }
        catch(...){
            tr.passed = false;
            tr.info += "unknown exception";
        }
        
        return tr;
    }
    
    P m_property;
    G m_gen;
};

template<class P, class G>
std::unique_ptr<PackedProperty> pack_property(P property, G gen)
{
    return std::make_unique< PackedPropertyImpl<P, G> >(std::move(property), std::move(gen));
}



struct Harness
{
    
    template<class P>
    void add_property_with_default(std::string name, P property)
    {
        using args_tup_t = meta::make_args_tuple_t< P >;
        using gens_tup_t = meta::map_tuple_t<Gen, args_tup_t>; 

        gens_tup_t gens_tup;

        auto default_gen = [](auto& gen){
            using gen_t = std::decay_t< decltype(gen) >;
            gen = DefaultGen< typename gen_t::value >{}();
        };
        for_each_el(default_gen, gens_tup);
        
        auto packed_property = pack_property(std::move(property), std::move(gens_tup));
        packed_property->property_name = std::move(name);
        packed_properties.push_back(std::move(packed_property));
    }
    
    template<class P, class ... Gens>
    void add_property(std::string name, P property, Gens... gens)
    {
        auto gens_tup = std::make_tuple(std::move(gens)...);
        auto packed_property = pack_property(std::move(property), std::move(gens_tup));
        packed_property->property_name = std::move(name);
        packed_properties.push_back(std::move(packed_property));
    }
    
    void run_tests(std::ostream& out)
    {
        for(auto const& prop: packed_properties){
            int passed = 0;
            for(int i = 0; i < count; ++ i){
                auto tr = prop->test_once();
                if(!tr.passed){
                    out << "failed: " << prop->property_name << ": " <<  tr.info << "\n";
                }
                else{
                    ++ passed;
                }
            }
            out << "passed: " << passed << " out of " << count << "\n";
        }
    }
    
    int count = 100;
    std::vector< std::unique_ptr< PackedProperty > > packed_properties;
};


//
//
//


void test_1(){}
void test_2(int) {}
void test_3(int, float, char*, char&){}

using tup_1 = meta::make_args_tuple_t< decltype(test_1) >;
using tup_2 = meta::make_args_tuple_t< decltype(test_2) >;
using tup_3 = meta::make_args_tuple_t< decltype(test_3) >;

static_assert( std::is_same< tup_1, std::tuple<> >::value, "");
static_assert( std::is_same< tup_2, std::tuple<int> >::value, "");
static_assert( std::is_same< tup_3, std::tuple<int, float, char*, char> >::value, "");

using gen_tup_1 = meta::map_tuple_t<Gen, tup_1>;
using gen_tup_2 = meta::map_tuple_t<Gen, tup_2>;
using gen_tup_3 = meta::map_tuple_t<Gen, tup_3>;

static_assert( std::is_same< gen_tup_1, std::tuple<> >::value, "");
static_assert( std::is_same< gen_tup_2, std::tuple< Gen<int> > >::value, "");
static_assert( std::is_same< gen_tup_3, std::tuple< Gen<int>, Gen<float>, Gen<char*>, Gen<char>> >::value, "");

using tup_1x = meta::map_tuple_t<value_of_gen_t, gen_tup_1>;
using tup_2x = meta::map_tuple_t<value_of_gen_t, gen_tup_2>;
using tup_3x = meta::map_tuple_t<value_of_gen_t, gen_tup_3>;

static_assert( std::is_same<tup_1, tup_1x>::value, "");
static_assert( std::is_same<tup_2, tup_2x>::value, "");
static_assert( std::is_same<tup_3, tup_3x>::value, "");


void test_sample()
{
  Gen<int> count_up([c = 0]()mutable{ return c++; });
  Gen<int> count_down([c = 0]()mutable{ return c--; });

  assert(count_up.sample() == 0);
  assert(count_up.sample() == 1);
  assert(count_up.sample() == 2);

  assert(count_down.sample() == 0);
  assert(count_down.sample() == -1);
  assert(count_down.sample() == -2);

  auto tup_of_gens = std::make_tuple(count_up, count_down);

  assert( std::make_tuple(3, -3) == sample(tup_of_gens) );
  assert( std::make_tuple(4, -4) == sample(tup_of_gens) );
  assert( std::make_tuple(5, -5) == sample(tup_of_gens) );
}


void test_fn()
{
  auto property = [](int x){ return x*x >= x + x; };
  auto gens = make_gen_for(property);
  std::get<0>(gens) = [c = -10]()mutable{ return c++; };
  
  assert( apply_tuple( property, sample(gens) ) );
  assert( apply_tuple( property, sample(gens) ) );
  assert( apply_tuple( property, sample(gens) ) );
  assert( apply_tuple( property, sample(gens) ) );
}

void test_harness()
{
    Harness harness;
    
    // this property doesn't hold always!
    harness.add_property( "multiply greater than addition"
                        , [](int x) { return x*x >= x+x; }
                        , Gen<int>{ [c = -50]()mutable{ return c++; } });
    
    harness.add_property_with_default( "multiply greater than addition 2"
                                     , [](int x) { return x*x >= x+x; } );
    
    
    harness.run_tests(std::clog);
}

void test_map()
{
  Gen<int> count_up([c = 0]()mutable{ return c++; });
  auto doubled = count_up.map([](int x){ return std::to_string(2*x); });

  assert( doubled.sample() == "0" );
  assert( doubled.sample() == "2" );
  assert( doubled.sample() == "4" );
  assert( doubled.sample() == "6" );
}


int main()
{
  test_sample();
  test_fn();
  test_harness();
  test_map();
  
  std::clog << "success!\n";
  
  return 0;
}

