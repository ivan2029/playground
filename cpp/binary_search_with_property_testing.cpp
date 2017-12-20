#include <cassert>
#include <iterator>
#include <optional>

namespace algo {
    
  template<class It, class T>
  auto binary_search_(It first, It last, T const& value) -> std::optional<It> {
    while(first != last) {
      auto middle = first + std::distance(first, last)/2;
      if(*middle == value) return {middle};
      if(*middle < value) first = std::next(middle);
      else                last  = middle;
    }
    return {};
  }
    
  template<class R, class T>
  auto binary_search_(R&& range, T const& value) {
    return binary_search_(std::begin(range), std::end(range), value);
  }
    
}

//
//
//
#include <iostream>
#include <iomanip>
#include <random>
#include <vector>
#include <algorithm>
#include <functional>

auto operator<< (std::ostream& out, std::vector<int> const& vec) -> std::ostream& {
  out << "[";
  if(!vec.empty()) {
    auto first{ vec.begin() };
    auto last{ vec.end() };
    out << *first;
    ++first;
    for(; first != last; ++ first) {
      out << ", " << *first;
    }
  }
  out << "]";
  return out;
}

auto property(std::vector<int> const& vec, int const value) -> bool {
  assert( std::is_sorted(vec.begin(), vec.end()) );
  auto it_by_find = std::find(vec.begin(), vec.end(), value);
  auto it_by_bin_search = algo::binary_search_(vec, value);
    
  auto result = false;
    
  if(it_by_find == vec.end() && !it_by_bin_search) {
    result = true;
  }
  if(it_by_find != vec.end() && it_by_bin_search && **it_by_bin_search == value) {
    result = true;
  }
    
  return result;
}

template<class SizeGen, class ValueGen>
  // SizeGen  :: IO size_t
  // ValueGen :: IO int
auto generate_vec(SizeGen size_gen, ValueGen& value_gen) -> std::vector<int> {
  auto size = size_gen();
  std::vector<int> values;
  values.reserve(size);
  for(decltype(size) i{0}; i < size; ++ i) {
    values.push_back(value_gen());
  }
  return values;
}

auto run_test(std::size_t const times) -> void {
  // generators
  std::random_device rd;
    
  std::uniform_int_distribution<std::size_t> size_dist{0, 10};
  std::uniform_int_distribution<int> value_dist{0, 10};
    
  auto size_gen  = [&]{ return size_dist(rd); };
  auto value_gen = [&]{ return value_dist(rd); };
    
  // run tests
  std::size_t i = 0;
  for(; i < times; ++ i) {
    auto const value = value_gen();
    auto vec = generate_vec(size_gen, value_gen);
    std::sort(vec.begin(), vec.end());
        
    // property is tested here
    auto result = property(vec, value);
        
    if(result) {
      std::cout << "success: value = " << std::setw(3) << value << ", vec = " << vec << "\n";
    }
    else {
      std::cout << "failure: value = " << std::setw(3) << value << ", vec = " << vec << "\n";
      break;
    }     
  }

  //
  if(i == times) {
    std::cout << "SUCCESS\n";
  }
  else {
    std::cout << "FAILURE, " << i << " tests of " << times << " passed\n";
  }
}

auto main() -> int {
  run_test(100000);
}
