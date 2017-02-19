/*
ask for 11 numbers to be read into a sequence S
reverse sequence S
for each item in sequence S
    result := call a function to do an operation
    if result overflows
        alert user
    else
        print result
*/
#include <cmath>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <sstream>
using namespace std::literals;

auto func(float x) -> float
{
  using namespace std;
  return pow(abs(x), 0.5f) + 5.0f * pow(x, 3.0f);
}

auto read_numbers(std::istream& in) -> std::vector<float>
{
  using InIt = std::istream_iterator<float>;
  
  std::vector<float> xs;
  std::copy( InIt(in), InIt()
           , back_inserter(xs) );

  if(xs.size() != 11u){
    throw std::runtime_error("11 numbers needed, got "s + std::to_string(xs.size()));
  }

  return xs;
}

template<class It>
auto process_and_write(It first, It last, std::ostream& out) -> void
{
  auto process_single = [&](auto x) {
    auto y = func(x);
    out << "func(" << x << ") = ";
    if(y > 400.0f) out << "overflow";
    else out << y;
    out << "\n";
  };

  std::reverse(first, last);
  std::for_each(first, last, process_single);
}

int main()
{
  std::string line;
  while(std::getline(std::cin, line)){
    try{
      std::istringstream str(line);
      auto xs = read_numbers(str);
      process_and_write(xs.begin(), xs.end(), std::cout);
    }
    catch(std::exception const& e){
      std::cerr << "error: " << e.what() << "\n";
    }
    catch(...){
      std::cerr << "error: unknown\n";
    }
  }

  return 0;
}
