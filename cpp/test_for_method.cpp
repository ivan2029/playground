#include <string>
#include <type_traits>

template<class T>
constexpr auto test_for_serialize(T&& t, std::string str = {}) 
    -> decltype( str = t.serialize(), std::true_type{} )
{ return {}; }

constexpr auto test_for_serialize(...) 
    -> std::false_type
{ return {}; }

template<class T>
using has_serialize = decltype( test_for_serialize(std::declval<T>()) );


struct HasSerialize
{
    std::string serialize() const { return "junk"; }
};

int main()
{
    static_assert( !has_serialize<int>::value,          "");
    static_assert(  has_serialize<HasSerialize>::value, "");
}