#include <experimental/optional>

#define MIXIN_BUILDER_FIELD(type, name)       \
private:                                      \
std::experimental::optional<type> m_ ## name; \
public:                                       \
inline auto&& name (type v) &&                \
{                                             \
  m_ ## name = std::move(v);                  \
  return std::move(*this);                    \
}

#include <iostream>
#include <vector>
#include <string>

struct Person
{
  std::string first_name;
  std::string last_name;
};

//
// Default constructors and destructor are not used here because we want to see
// which ones are called.
//
// with gcc and clang default constructor and destructor are the only ones called.
//
struct PersonBuilder
{
//#define PRINT_CONSTRUCTION
#ifdef PRINT_CONSTRUCTION
  PersonBuilder()
  {
    std::cout << "default ctor\n";
  }
      
  PersonBuilder(PersonBuilder const& that)
    : m_first_name{that.m_first_name}
    , m_last_name{that.m_last_name}
  {
    std::cout << "copy ctor\n";
  } 
      
  PersonBuilder(PersonBuilder&& that)
    : m_first_name{std::move(that.m_first_name)}
    , m_last_name{std::move(that.m_last_name)}
  {
    std::cout << "move ctor\n";
  }

  ~PersonBuilder()
  {
    std::cout << "dtor\n";
  }
#endif

  MIXIN_BUILDER_FIELD(std::string, first_name)
  MIXIN_BUILDER_FIELD(std::string, last_name)
  
  Person build() &&
  {
    return { std::move(m_first_name).value_or("???")
           , std::move(m_last_name ).value_or("???") };
  }
};

int main() {
  auto p = PersonBuilder{}
            .first_name("ivan 1")
            .last_name("navi 1")
            .last_name("navi 1")
            .first_name("ivan 2")
            .first_name("ivan 3")
            .last_name("navi 1")
            .first_name("ivan 3")
            .last_name("navi 1")
            .build();
  std::cout << p.first_name << " " << p.last_name << "\n";
  return 0;
}
