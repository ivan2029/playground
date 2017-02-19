/*
  On test machine, method 2 is way faster.
  cat.log is 50MB file.

  method 1 takes ~600 ms
  method 2 takes ~30 ms

  Compiled with g++ 4.9.2, with "-std=c++14 -O2"
  Without "-O2" method 1 takes ~7 seconds! Method 2 takes ~30 ms...
*/

#include <cassert>
#include <iostream>
#include <fstream>
#include <vector>
#include <chrono>
#include <algorithm>

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

std::vector<char> method_1(std::fstream& fin)
{
  return std::vector<char>( std::istreambuf_iterator<char>{fin}
                          , std::istreambuf_iterator<char>{} );
}

std::vector<char> method_2(std::fstream& fin)
{
  std::vector<char> contents;

  auto begin_pos = fin.tellg();
  fin.seekg(0, std::ios::end);
  auto end_pos = fin.tellg();
  fin.seekg(0, std::ios::beg);
  auto length = end_pos - begin_pos;
 
  contents.resize( length );

  fin.read(contents.data(), length);

  return contents;
}

int main()
{
  // method 1
  std::vector<char> contents_first;
  {
    std::fstream fin("cat.log", std::ios::in | std::ios::binary);

    time("method 1", [&]{
      contents_first = method_1(fin);
    });
  }

  // method 2
  std::vector<char> contents_second;
  {
    std::fstream fin("cat.log", std::ios::in | std::ios::binary);

    time("method 2", [&]{
      contents_second = method_2(fin);
    });
  }

  assert(contents_first == contents_second);

  return 0;
}
