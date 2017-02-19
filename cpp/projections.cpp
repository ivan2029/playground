// C++14 required, tested with g++ 6.1
// to compile with g++ 5.3 -std=c++14 see comments

#include <type_traits>
#include <utility>
#include <tuple>

namespace __combine_projections
{
  template<class Tup, class Val, std::size_t ... Indices>
  auto apply_each_to_impl(Tup&& fn_tup, Val&& val, std::index_sequence<Indices...>)
  {
    using ret_tup = std::tuple< decltype(std::get<Indices>(fn_tup)(val))... >;
    return ret_tup( std::get<Indices>(fn_tup)(val) ... );
  }

  template<class Tup, class Val>
  auto apply_each_to(Tup&& fn_tup, Val&& val)
  {
    using tup_size = std::tuple_size< std::decay_t<Tup> >;
    return apply_each_to_impl( std::forward<Tup>(fn_tup)
                             , std::forward<Val>(val)
                             , std::make_index_sequence< tup_size::value >{} );
  }
} // __combine_projections

template<class ... Ps>
constexpr auto combine_projections(Ps ... ps)
{
  auto fn_tup = std::make_tuple(ps...);
  return [=](auto&& value){
    using namespace __combine_projections;
    return apply_each_to(fn_tup, value);
  };
}

//
// test
//
#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
using namespace std;

struct Person
{
  string first_name;
  string last_name;
  string address;
  int    age;
};

auto first_name(Person& p) -> string&
{
  return p.first_name;
}

auto last_name(Person& p) -> string&
{
  return p.last_name;
}

// auto name = combine_projections(first_name, last_name); // g++ 5.3
constexpr auto&& name = combine_projections(first_name, last_name);

int main()
{
  // test 1
  {
    Person person;

    name(person) = make_tuple("ivan", "barisic");
        
    assert(person.first_name == "ivan");
    assert(person.last_name == "barisic");
  }

  // test 2
  {
    vector<Person> personae = {
        { "Reinhard",  "von Lohengramm", "Odin",      27 }
      , { "Siegfried", "Kirscheis",      "Odin",      26 }
      , { "Adalbert",  "von Fahrenheit", "Odin",      25 }
      , { "Paul",      "von Oberstein",  "Odin",      40 }
      , { "Adrian",    "Rubinsky",       "Phezzan",   50 }
      , { "Yang",      "Wen-li",         "Heinessen", 30 }
    };
        
    sort( personae.begin(), personae.end()
        , [=](auto& a, auto& b){ return name(a) < name(b); } );
        
    // for g++ 5.3 initialize all elements like this: tuple<string, string>{ "Adalbert", "von Fahrenheit" }
    vector< tuple<string, string> > const expected = {
        { "Adalbert", "von Fahrenheit" }
      , { "Adrian", "Rubinsky" }
      , { "Paul", "von Oberstein" }
      , { "Reinhard", "von Lohengramm" }
      , { "Siegfried", "Kirscheis" }
      , { "Yang", "Wen-li" }
    };
        
    vector< tuple<string, string> > actual;
    transform( personae.begin(), personae.end()
             , back_inserter(actual)
             , name );
                 
    assert(expected == actual);
  }
}
