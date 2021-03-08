#include <iostream>
#include <sstream>
#include <vector>
#include <set>
#include <string>
#include <string_view>
#include <algorithm>
#include <numeric>
#include <ranges>
#include <iterator>
#include <type_traits>

//
//
//
template<
    std::forward_iterator It, 
    std::sentinel_for<It> Sent
>
    requires std::convertible_to< std::iter_value_t<It>, std::string_view >
auto concatenate(It it, Sent sent) -> std::string {
    //
    auto const cat_length_accumulator = [](auto length, auto&& sv) { 
        return length + sv.length(); 
    };
    auto const cat_length = std::accumulate(
        it, sent,
        std::size_t{0},
        cat_length_accumulator
    );
    
    //
    auto const cat_accumulator = [](auto cat, auto&& sv) {
        std::copy(
            sv.begin(), sv.end(),
            std::back_inserter(cat)
        );
        return cat;
    };
    
    std::string cat;
    cat.reserve(cat_length);
    cat = std::accumulate(
        it, sent,
        std::move(cat),
        cat_accumulator
    );
    
    //
    return cat;
}

auto concatenate(std::ranges::input_range auto&& range) -> std::string {
    return concatenate(
        std::ranges::begin(range), 
        std::ranges::end(range)
    );
}

//
//
//
auto main() -> int {
    using namespace std::string_view_literals;
    using namespace std::string_literals;
    using namespace std::ranges;
    
    //
    std::set xs{
        "one"s, "two"s, "three"s
    };
    
    std::cout << concatenate(xs) << "\n";

    //
    auto long_ones = views::filter([](std::string_view v) { return v.length() > 3; });
        
    for(auto y: xs | long_ones) {
        std::cout << y << "\n";
    }
    
    //
    return 0;
}