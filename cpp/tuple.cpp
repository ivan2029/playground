/*
    g++ v8.3.0 with --std=c++17
*/

#include <utility>
#include <type_traits>

namespace meta {

    //
    template<class ... Ts>
    struct type_list {};

    //
    template<std::size_t N, class TL>
    struct get_nth;
    
    template<std::size_t N, class H, class ... Ts>
    struct get_nth< N, type_list<H, Ts...> > 
        : public get_nth< N - 1, type_list<Ts...> >
    {};
    
    template<class H, class ... Ts>
    struct get_nth< 0, type_list<H, Ts...> > {
        using type = H;
    };
    
    template<std::size_t N, class TL>
    using get_nth_t = typename get_nth<N, TL>::type;
    

}

template<std::size_t N, class T>
struct tuple_single_value_holder {
    T value;
};

template<class IndexSeq, class TypeList>
struct tuple_values_holder;

template<std::size_t ... N, class TypeList>
struct tuple_values_holder< std::index_sequence<N...>, TypeList > 
    : public tuple_single_value_holder< 
        N, 
        meta::get_nth_t<N, TypeList> 
    >...
{};

template<class ... Ts>
class tuple {
    
    template<std::size_t N>
    using nth = meta::get_nth_t< N, meta::type_list<Ts...> >;
    
    using values_type = tuple_values_holder<
        std::make_index_sequence<sizeof...(Ts)>,
        meta::type_list<Ts...>
    >;
    
public:
    
    tuple() = default;
    tuple(tuple const& ) = default;
    tuple(tuple&& ) = default;
    ~tuple() = default;
    
    auto operator= (tuple const& ) -> tuple& = default;
    auto operator= (tuple&& ) -> tuple& = default;

    template<class ... Ts1>
    tuple(Ts1&& ... ts)
        : values{ std::forward<Ts1>(ts)...}
    {}

    template<std::size_t N>
    auto get() & -> nth<N> & {
        return values.tuple_single_value_holder< N, nth<N> >::value; 
    }
    
    template<std::size_t N>
    auto get() const& -> nth<N> const & {
        return values.tuple_single_value_holder< N, nth<N> >::value; 
    }
    
    template<std::size_t N>
    auto get() && -> nth<N> const & {
        return std::move(values.tuple_single_value_holder< N, nth<N> >::value); 
    }

private:
    values_type values;
};

//
//
//
#include <iostream>
#include <string>
#include <tuple>

auto main() -> int {
    using namespace std::string_literals;
    
    ::tuple<int, float, std::string> tup{};
    
    std::cout << sizeof(tup) << ", " << sizeof(std::tuple<int, float, std::string>) << "\n";

    tup.get<0>() = 42;
    tup.get<1>() = 1.608f;
    tup.get<2>() = "hello";
    
    std::cout 
        <<  tup.get<0>() << ", "
        <<  tup.get<1>() << ", "
        <<  tup.get<2>() << "\n";
        
    auto const& tup_cr = tup;

    std::cout 
        <<  tup_cr.get<0>() << ", "
        <<  tup_cr.get<1>() << ", "
        <<  tup_cr.get<2>() << "\n";
        
    ::tuple<int, float, char const*> tup_2{
        100, 3.141f, "hola!"
    };
        
    std::cout 
        <<  tup_2.get<0>() << ", "
        <<  tup_2.get<1>() << ", "
        <<  tup_2.get<2>() << "\n";
    
    return 0;
}
