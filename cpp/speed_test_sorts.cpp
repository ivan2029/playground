/*
  On test machine, g++ with -O2
    std::sort takes ~750ms
    qsort     takes ~1900ms
*/

#include <cassert>
#include <algorithm>
#include <random>
#include <vector>
#include <chrono>
#include <cstdlib>
#include <iostream>

using Gen = std::default_random_engine;
using Num = Gen::result_type;
 
template<class F>
void time(char const* msg, F&& f)
{
  using namespace std::chrono;
  auto begin = high_resolution_clock::now();
  f();
  auto end   = high_resolution_clock::now();
  auto diff  = duration_cast<milliseconds>(end - begin);
  std::cout << msg << ": " << diff.count() << "ms\n";
}

std::vector<Num> make_test_vector()
{
  std::vector<Num> ns(10000000);
  std::generate( ns.begin(), ns.end()
               , Gen{ std::random_device{}() });
  return ns;
}

int comp(void const* a, void const* b)
{
  Num const x = *reinterpret_cast<Num const*>(a);
  Num const y = *reinterpret_cast<Num const*>(b);

  if(x == y) return 0;
  else return x < y ? -1 : 1;
}

int main()
{
  auto numbers = make_test_vector();

  time("std::sort", [=]()mutable{
    std::sort(numbers.begin(), numbers.end());
    //assert(std::is_sorted(numbers.begin(), numbers.end()));
  });

  time("qsort", [=]()mutable {
    std::qsort(numbers.data(), numbers.size(), sizeof(Num), &comp);
    //assert(std::is_sorted(numbers.begin(), numbers.end()));
  });
  
  return 0;
}

