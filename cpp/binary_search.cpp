#include <cassert>
#include <vector>
#include <algorithm>
#include <utility>
#include <experimental/optional>

template<class X, class Y>
using Pair = std::pair<X, Y>;

template<class X>
using Option = std::experimental::optional<X>;

template<class Fn, class O>
auto map(Fn fn, O&& option) -> Option<decltype(fn(*option))>
{
    if(option){
        return std::experimental::make_optional( fn( * std::forward<O>(option) ) );
    }
    else{
        return {};
    }
}

/*
    given sequence [a(0), a(1) ... a(n-1)] and value t 
    finds a pair of adjacent elements (a, b) in given sequence 
    such that a <= t && t < b if that pair exists  
*/
template<class It, class T> // requires RandomAccessIterator<It>
auto binary_range_search(It first, It last, T const& t) -> Option< Pair<It, It> >
{
    assert(std::is_sorted(first, last));
    
    if(first == last) return {};
    
    if(std::distance(first, last) >= 1){
        if(t < first[0]) return {};
        if(last[-1] < t) return {};
    }
    
    while(std::distance(first, last) > 1) {
        auto middle = first + std::distance(first, last)/2;
        if(t < *middle){
            last = middle;
        }
        else{
            first = middle;
        }
    }
    return std::make_pair(first, last);
}


template<class It, class T> // requires RandomAccessIterator<It>
auto binary_search(It first, It last, T const& t) -> Option<It>
{
    auto range = ::binary_range_search(first, last, t);
    
    if(range){
        if(std::distance(range->first, range->second) == 1){
            if(*(range->first) == t){
                return std::experimental::make_optional(range->first);
            }
            else{
                return {};
            }
        }
    }
    
    return {};
}

#include <iostream>

void test_empty()
{
    std::vector<int> const xs = {};
    auto res = ::binary_search(xs.begin(), xs.end(), 123);
    assert( !res );
}

void test_scaffold(std::vector<int> const& xs, int val, Option<int> target_index)
{
    auto res = ::binary_search(xs.begin(), xs.end(), val);
    auto opt = ::map( [&](auto it) -> int { return std::distance(xs.begin(), it); }
                    , res 
                    );
    assert( opt == target_index );
}

int main()
{
    test_empty();
    test_scaffold({1},                        1, {0}); 
    test_scaffold({1,2},                      1, {0}); 
    test_scaffold({1,2},                      2, {1}); 
    test_scaffold({1,5,9},                    5, {1}); 
    test_scaffold({1, 4, 8, 14, 89, 93, 99}, 14, {3}); 
    test_scaffold({1, 4, 8, 14, 89, 93, 99}, 23, {});
    test_scaffold({1,2},                      5, {});
    test_scaffold({1,2},                     -1, {});

    return 0;
}
