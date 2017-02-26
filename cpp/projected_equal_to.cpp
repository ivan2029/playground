template<class Proj, class T>
struct ProjectedEqualTo
{
    ProjectedEqualTo(Proj projection, T value)
        : m_projection{ projection }
        , m_value{ value }
    {}

    template<class O>
    bool operator() (O const& obj) 
    { 
        return m_projection(obj) == m_value;
    }
    
    Proj m_projection;
    T    m_value;
};

template<class Proj, class T>
auto projected_equal_to(Proj proj, T value)
{
    return ProjectedEqualTo<Proj, T>{ proj, value };
}


#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <iterator>

struct City
{
  std::string name;
  std::string country;
};

std::string const& get_country(City const& city)
{
  return city.country;
}

int main()
{
  std::vector<City> cities = {
    {"Berlin",  "Germany"},
    {"Hamburg", "Germany"},
    {"Bern",    "Switzerland"},
    {"Luzerne", "Switzerland"},
    {"Zurich",  "Switzerland"},
    {"Geneva",  "Switzerland"},
    {"Paris",   "France"},
    {"Milan",   "Italy"},
    {"Rome",    "Italy"},
    {"Venice",  "Italy"}
  };

  std::random_shuffle(cities.begin(), cities.end()); // deprecated!

  std::cout << "all cities:\n";
  for(auto const& city: cities)
  {
    std::cout << "  " << city.name << "\n";
  }
  std::cout << "\n";
  
  std::vector<City> cities_in_italy;
  
  /*
        Which is more readable? For me, lambda is (marginally) less readable than explicit function object.
  */

  // example of projected_equal_to
  std::copy_if(
    cities.begin(), cities.end()
    , std::back_inserter(cities_in_italy)
    , projected_equal_to(get_country, "Italy")
  );
  // example
  
  std::cout << "in Italy:\n";
  for(auto const& city: cities_in_italy)
  {
    std::cout << "  " << city.name << "\n";
  }
  std::cout << "\n";
  
  // example with lambdas
  std::vector<City> cities_in_switzerland;

  std::copy_if(
    cities.begin(), cities.end()
    , std::back_inserter(cities_in_switzerland)
    , [](auto const& city) { return city.country == "Switzerland"; }
  );
  // end example

  std::cout << "in Switzerland:\n";
  for(auto const& city: cities_in_switzerland)
  {
    std::cout << "  " << city.name << "\n";
  }
  std::cout << "\n";
  
  return 0;
}
