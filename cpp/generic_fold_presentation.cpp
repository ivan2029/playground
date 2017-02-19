#include <cassert>

#include <algorithm>
#include <iterator>
#include <functional>

#include <vector>
#include <list>
#include <string>

#include <cmath>
#include <cctype>

#include <iostream>
#include <sstream>

//
//
//

template<class Real>
bool equal_with_precision(Real precision, Real x, Real y)
{
  assert(precision > 0);
  return std::abs(x - y) < precision;
}

//
// 1. Summing sequences of ints, no templates
//
// we have to define one for each type we want to iterate over
// but they are all the same
//

int sum_v1( int const* first, int const* last)
{
  int state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

int sum_v1( std::vector<int>::const_iterator first
          , std::vector<int>::const_iterator last )
{
  int state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

int sum_v1( std::list<int>::const_iterator first
          , std::list<int>::const_iterator last )
{
  int state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

int sum_v1( std::istream_iterator<int> first
          , std::istream_iterator<int> last )
{
  int state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

//
void test_sum_v1()
{
  // static arrays
  {
    int const values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v1( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // vectors
  {
    std::vector<int> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v1( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // lists
  {
    std::list<int> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v1( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // input_streams
  {
    std::istringstream in("1 2 3 4 5 6 7 8 9 10");
    int const sum = sum_v1( std::istream_iterator<int>(in)
                          , std::istream_iterator<int>() );
    assert( 55 == sum );
  }
}

//
// 2. Summing sequences of ints, with templates
//
// Notice that the code was always the same
// But, what if I want to sum sequences of floats?
//

template<class It>
int sum_v2( It first, It last )
{
  int state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

//
void test_sum_v2()
{
  // static arrays
  {
    int const values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v2( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // vectors
  {
    std::vector<int> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v2( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // lists
  {
    std::list<int> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v2( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // input_streams
  {
    std::istringstream in("1 2 3 4 5 6 7 8 9 10");
    int const sum = sum_v2( std::istream_iterator<int>(in)
                          , std::istream_iterator<int>() );               
    assert( 55 == sum );
  }
}

//
// 3. Summing sequences of arithmetic values
//
// Now we need iterator_traits
//

template<class It>
auto sum_v3( It first, It last )
  -> typename std::iterator_traits<It>::value_type // not needed with C++14
{
  using value_type = typename std::iterator_traits<It>::value_type;

  value_type state = 0;
  for(; first != last; ++ first){
    state = state + *first;
  }
  return state;
}

//
void test_sum_v3()
{
  // static arrays
  {
    int const values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = sum_v3( std::begin(values), std::end(values) );  
    assert( 55 == sum );
  }

  // vectors
  {
    std::vector<double> const values = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
    double const sum = sum_v3( std::begin(values), std::end(values) );
    assert( equal_with_precision(0.0001, 55.0, sum) );
  }

  // lists
  {
    std::list<unsigned long> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    unsigned long const sum = sum_v3( std::begin(values), std::end(values) );
    assert( 55 == sum );
  }

  // input_streams
  {
    std::istringstream in("1 2 3 4 5 6 7 8 9 10");
    float const sum = sum_v3( std::istream_iterator<float>(in)
                            , std::istream_iterator<float>() );                  
    assert( equal_with_precision(0.0001f, 55.0f, sum) );
  }
}

//
// 4. Now, let's do product
//

template<class It>
auto prod( It first, It last )
  -> typename std::iterator_traits<It>::value_type // not needed with C++14
{
  using value_type = typename std::iterator_traits<It>::value_type;

  value_type state = 1;
  for(; first != last; ++ first){
    state = state * (*first);
  }
  return state;
}

//
void test_prod()
{
  // static arrays
  {
    int const values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int const sum = prod( std::begin(values), std::end(values) );
    assert( 3628800 == sum );
  }

  // vectors
  {
    std::vector<double> const values = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
    double const sum = prod( std::begin(values), std::end(values) );
    assert( equal_with_precision(0.0001, 3628800.0, sum) );
  }

  // lists
  {
    std::list<unsigned long> const values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    unsigned long const sum = prod( std::begin(values), std::end(values) );
    assert( 3628800 == sum );
  }

  // input_streams
  {
    std::istringstream in("1 2 3 4 5 6 7 8 9 10");
    float const sum = prod( std::istream_iterator<float>(in)
                          , std::istream_iterator<float>() );         
    assert( equal_with_precision(0.0001f, 3628800.0f, sum) );
  }
}

//
// 5. Huh, product and sum are the same! Let's generalize
//

// Element = iterator_traits<It>::value_type
// Fold = (State, Element) -> State
template<class It, class State, class Fold>
auto fold(It first, It last, State init_state, Fold fold)
  -> decltype( fold(init_state, *first) ) // not needed with C++14
{
  State state = init_state;
  for(; first != last; ++ first){
    state = fold(state, *first);
  }
  return state;
}


//
constexpr auto&& plus = [](auto x, auto y){ return x + y; };
constexpr auto&& mul  = [](auto x, auto y){ return x * y; };

void test_fold()
{
  std::vector<int> values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  int const sum = fold( std::begin(values), std::end(values)
                      , 0
                      , plus );
  assert( 55 == sum );

  int const prod = fold( std::begin(values), std::end(values)
                       , 1
                       , mul );    
  assert( 3628800 == prod );
}

void test_fold_2()
{
  std::vector<std::string> words = { "one", "two", "three" };
  std::size_t const count = fold( std::begin(words), std::end(words)
                                , std::size_t{0}
                                , [](auto acc, auto const& el){ return acc + el.length(); });
  
  assert(11 == count);
}

//
// 6. If fold useful for something else? Try polynomial evaluation using Horner's rule
//

template<class It, class Num>
auto poly_val( It first, It last // coefficients, from highest degree to lowest
             , Num x )
  -> Num
{
  auto folding = [=](Num state, Num item) {
    return x*state + item;
  };
  return fold( first, last
             , Num{0}
             , folding );
}

void test_poly_val()
{
  // p(x) = x^2 + 1
  std::vector<int> p = {1, 0, 1};

  std::vector<int> const xs          = { -5,  -4,  -3,  -2,  -1,  0,  1,  2,  3,  4,  5};
  std::vector<int> const expected_ys = { 26,  17,  10,   5,   2,  1,  2,  5, 10, 17, 26};

  auto eval_p = [&](int x) { return poly_val(std::begin(p), std::end(p), x); };

  std::vector<int> ys;

  std::transform( std::begin(xs), std::end(xs)
                , std::back_inserter(ys)
                , eval_p );
                
  assert( expected_ys == ys );
}

//
// 7. find indices of spaces within the string
//
// template solution right away, this time with convenience interface
//
template<class InIt, class OutIt>
OutIt spaces_positions(InIt in_first, InIt in_last, OutIt out_first)
{
  std::size_t index = 0u;
  for(; in_first != in_last; ++in_first, ++ index){
    if(std::isspace(*in_first)){
      *out_first = index;
      ++out_first;
    }
  }
  return out_first;
}

template<class R, class OutIt>
OutIt spaces_positions(R const& range, OutIt out_first)
{
  return spaces_positions( std::begin(range), std::end(range)
                         , out_first );
}

//
void test_spaces_positions()
{
  std::string const test_str = "the quick brown fox jumps over the lazy dog";
  std::vector<std::size_t> const expected_indices = {3, 9, 15, 19, 25, 30, 34, 39};
  
  std::vector<std::size_t> indices;

  spaces_positions( test_str
                  , std::back_inserter(indices) );
                  
  assert( expected_indices == indices );
  assert( std::all_of( std::begin(indices), std::end(indices)
                     , [&](std::size_t i) { return std::isspace(test_str[i]); } ) );
}

//
// 8. finding positions of elements satisfying some predicate is useful operation!
//
// generalization of spaces_positions
//
template<class InIt, class OutIt, class Pred>
OutIt positions(InIt in_first, InIt in_last, OutIt out_first, Pred pred)
{
  std::size_t index = 0u;
  for(; in_first != in_last; ++in_first, ++ index){
    if( pred(*in_first) ){
      *out_first = index;
      ++out_first;
    }
  }
  return out_first;
}

template<class R, class OutIt, class Pred>
OutIt positions(R const& range, OutIt out_first, Pred&& pred)
{
  return positions( std::begin(range), std::end(range)
                  , out_first
                  , std::forward<Pred>(pred));
}

//
void test_positions()
{
  {
    std::string const test_str = "the quick brown fox jumps over the lazy dog";
    std::vector<std::size_t> const expected_indices = {3, 9, 15, 19, 25, 30, 34, 39};
  
    std::vector<std::size_t> indices;

    auto is_space = [](char c) { return std::isspace(c); };
    
    positions( test_str
             , std::back_inserter(indices)
             , is_space );
             
    assert( expected_indices == indices );
    assert( std::all_of( std::begin(indices), std::end(indices)
                     , [&](std::size_t i) { return is_space(test_str[i]); } ) );
  }

  {
    std::vector<int> const numbers = {1,2,3,4,5};
    std::vector<std::size_t> const expected_indices = {0, 2, 4};

    std::vector<std::size_t> indices;

    auto is_odd = [](int x) { return x & 1; };

    positions( numbers
             , std::back_inserter(indices)
             , is_odd );
             
    assert( expected_indices == indices );
    assert( std::all_of( std::begin(indices), std::end(indices)
                     , [&](std::size_t i) { return is_odd(numbers[i]); } ) );
  }
}


//
// MAIN
//
int main()
{
  test_sum_v1();
  test_sum_v2();
  test_sum_v3();
  test_prod();
  test_fold();
  test_fold_2();
  test_poly_val();
  test_spaces_positions();
  test_positions();
  
  return 0;
}

