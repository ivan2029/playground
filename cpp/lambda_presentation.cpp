#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <iterator>
#include <string>
#include <functional>

// template<class InIt, class OutIt, class Fun>
// void transform(InIt first, InIt last, OutIt out, Fun f)
// {
//    for(; first != last; ++ first){
//        *out = f(*first);
//    }
// }

//
//
//
int plus_free_function(int x, int y)
{
  return x + y;
}

struct Plus
{
  int operator() (int x, int y) const
  {
    return x + y;
  }
};

void lambda_no_capture_1()
{
  auto plus_lambda =
    [](int x, int y) -> int {
      return x + y;
    };

  std::cout << plus_free_function(1,2) << " "
            << Plus{}(1, 2)            << " "
            << plus_lambda(1,2)        << "\n";
}

//
//
//

struct Person
{
  std::string  first_name;
  std::string  last_name;
  int          birth_year;
};

std::string const& first_name(Person const& person)
{
  return person.first_name;
}

std::string const& last_name(Person const& person)
{
  return person.last_name;
}

int const& birth_year(Person const& person)
{
  return person.birth_year;
}

struct BirthYear
{
  int operator() (Person const& person) const
  {
    return person.birth_year;
  }
};

void lambda_no_capture_2()
{
  //
  std::vector<Person> const personae = {
      { "Marie",   "Curie",    1867 }
    , { "Nikola",  "Tesla",    1856 }
    , { "Ada",     "Lovelace", 1815 }
    , { "Richard", "Feynman",  1918 }
  };

  //
  std::transform( personae.begin(), personae.end()
                , std::ostream_iterator<std::string>(std::cout, ", ")
                , first_name );

  std::cout << "\n";

  //
  std::transform( personae.begin(), personae.end()
                , std::ostream_iterator<int>(std::cout, ", ")
                , BirthYear{} );

  std::cout << "\n";

  //
  auto lambda =
    [](Person const& person) -> std::string const& {
      return person.last_name;
    };

  std::transform( personae.begin(), personae.end()
                , std::ostream_iterator<std::string>(std::cout, ", ")
                , lambda );

  std::cout << "\n";
}

//
//
//
bool is_odd(int n)
{
  return n & 0x1;
}

bool is_even(int n)
{
  return !is_odd(n);
}

struct IncrementIfEven
{
  explicit IncrementIfEven(int& counter)
    : m_counter{counter}
  {}

  void operator()(int n) const
  {
    if( is_even(n) )
      ++ m_counter;
  }

  int& m_counter;
};

void lambda_capture_by_reference()
{
  std::vector<int> const numbers = {
    1,2,3,4,5,6,7,8
  };

  //
  int even_count_1 = 0;

  std::for_each( numbers.begin(), numbers.end()
               , IncrementIfEven{even_count_1} );

  std::cout << even_count_1 << "\n";

  //
  int even_count_2 = 0;
  auto lambda_2 =
    [&](int x) {
      if( is_even(x) )
        ++ even_count_2;
    };

  std::for_each( numbers.begin(), numbers.end()
               , lambda_2 );

  std::cout << even_count_2 << "\n";

  //
  int even_count_3 = 0;
  auto lambda_3 =
    [&even_count_3](int x) {
      if( is_even(x) )
        ++ even_count_3;
    };

  std::for_each( numbers.begin(), numbers.end()
               , lambda_3 );

  std::cout << even_count_3 << "\n";

  //
  int even_count_4 = std::count_if( numbers.begin(), numbers.end()
                                  , is_even );

  std::cout << even_count_4 << "\n";
}

//
//
//

void say_hello(std::ostream& out, Person const& person)
{
  out << "Hi, my name is " << person.first_name
      << " " << person.last_name << ".\n";
}

struct SayHello
{
  explicit SayHello(Person const& person)
    : m_person(person)
  {}

  void operator() (std::ostream& out) const
  {
    say_hello(out, m_person);
  }

  Person m_person;
};

struct SayHelloT
{
  explicit SayHelloT(Person const& person)
    : m_person(person)
  {}

  template<class T>
  void operator() (T& out) const
  {
    say_hello(out, m_person);
  }

  Person m_person;
};

using Action = std::function< void(std::ostream&) >;

void lambda_capture_by_value()
{
  Person max_planck = { "Max", "Planck", 1858 };

  //
  Action say_hello_1 = SayHello{max_planck};

  say_hello_1(std::cout);

  //
  Action say_hello_2 =
    [=](std::ostream& out) {
      say_hello(out, max_planck);
    };

  say_hello_2(std::cout);

  //
  Action say_hello_3 =
    [max_planck](std::ostream& out) {
      say_hello(out, max_planck);
    };

  say_hello_3(std::cout);

  // c++14, polymorphic lambda
  Action say_hello_4 =
    [max_planck](auto& out) {
      say_hello(out, max_planck);
    };

  say_hello_4(std::cout);

  // c++14, polymorphic lambda as structure
  Action say_hello_5 = SayHelloT{max_planck};

  say_hello_5(std::cout);
}

//
//
//
struct Iota
{
  explicit Iota(int counter)
    : m_counter{counter}
  {}

  int operator() ()
  {
    return m_counter++;
  }

  int m_counter;
};

void lambda_capture_by_value_mutable()
{
  int n = -2;

  //
  std::generate_n( std::ostream_iterator<int>(std::cout, ", ")
                 , 5
                 , Iota{n} );
  std::cout << "\n";

  //
  std::generate_n( std::ostream_iterator<int>(std::cout, ", ")
                 , 5
                 , [n]() mutable { return n++; } );
  std::cout << "\n";

  // c++14
  std::generate_n( std::ostream_iterator<int>(std::cout, ", ")
                 , 5
                 , [x = -2]() mutable { return x++; } );
  std::cout << "\n";
}

//
//
//
void test_auto()
{
  int x = 3;

  //
  auto cx = x;
  auto& rx = x;
  auto const& rcx = x;

  //
  static_assert( std::is_same< decltype(cx),  int        >::value , "cx is not int" );
  static_assert( std::is_same< decltype(rx),  int&       >::value , "rx is not int&" );
  static_assert( std::is_same< decltype(rcx), int const& >::value , "rcx is not int const&" );
}

void lambda_return_type_deduction()
{
    Person const person = {"name", "name", 0};

    auto lambda_1 = [](Person const& person)                       { return person.first_name; };
    auto lambda_2 = [](Person const& person) -> std::string const& { return person.first_name; };

    static_assert( std::is_same< decltype( lambda_1(person) ), std::string        >::value , "deduced type is not a value type");
    static_assert( std::is_same< decltype( lambda_2(person) ), std::string const& >::value , "deduced type is not a ref to const type");
}

//
//
//
void sums_and_products()
{
  //
  std::vector<int> numbers = {1,2,3,4,5,6,7,8,9,10};

  int sum_1 = std::accumulate( numbers.begin(), numbers.end()
                             , 0
                             , [](int x, int y) { return x + y; } );

  int sum_2 = std::accumulate( numbers.begin(), numbers.end()
                             , 0
                             , std::plus<int>{} );

  std::cout << sum_1 << " " << sum_2 << "\n";


  //
  int prod_1 = std::accumulate( numbers.begin(), numbers.end()
                              , 1
                              , [](int x, int y) { return x * y;} );

  int prod_2 = std::accumulate( numbers.begin(), numbers.end()
                              , 1
                              , std::multiplies<int>{} );

 std::cout << prod_1 << " " << prod_2 << "\n";
}

//
//
//
struct Triple
{
  int x, y, z;
};

std::ostream& operator<< ( std::ostream& out, Triple const& t )
{
  return out << "("  << std::setw(2) << t.x
             << ", " << std::setw(2) << t.y
             << ", " << std::setw(2) << t.z
             << ")";
}

template<class T>
std::ostream& operator<< ( std::ostream& out
                         , std::vector<T> const& vec )
{
  auto first = vec.begin();
  auto last  = vec.end();

  out << "[ ";

  if(first != last){
    out << *first;
    ++first;
    for(; first != last; ++ first){
      out << "\n, " << *first;
    }
  }

  out << " ]\n";

  return out;
}

void sort_by()
{
  // C++14 stuff used here
  std::vector< Triple > triples = {
      {  1,  1,  1 }
    , {  1,  2,  1 }
    , {  3,  1,  3 }
    , { -1,  2, -1 }
    , {  0,  0,  1 }
  };

  std::cout << triples << "\n";

  std::sort( triples.begin(), triples.end()
           , [](auto const& a, auto const& b){
               return a.x < b.x;
             } );

  std::cout << triples << "\n";

  std::sort( triples.begin(), triples.end()
           , [](auto const& a, auto const& b){
               return a.y < b.y;
             } );

  std::cout << triples << "\n";

  std::sort( triples.begin(), triples.end()
           , [](auto const& a, auto const& b){
               return a.z < b.z;
             } );

  std::cout << triples << "\n";
}

//
//
//
int main()
{
  lambda_no_capture_1();
  lambda_no_capture_2();
  lambda_capture_by_reference();
  lambda_capture_by_value();
  lambda_capture_by_value_mutable();
  sums_and_products();
  sort_by();

  return 0;
}
