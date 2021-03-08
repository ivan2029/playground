#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <algorithm>
#include <numeric>
#include <tuple>
#include <stack>
#include <random>
#include <concepts>

template<std::random_access_iterator It>
auto quick_sort(It it, It sent) -> void {
    std::stack< std::tuple<It, It> > ranges_to_be_sorted;

    ranges_to_be_sorted.push( std::tuple{it, sent} );
    
    while( ! ranges_to_be_sorted.empty() ) {
        auto [f, l] = ranges_to_be_sorted.top();
        ranges_to_be_sorted.pop();
        
        if(std::distance(f, l) < 2) continue;
        
        auto m = std::partition(
            std::next(f), l,
            [&](auto const& x) { return x < *f; }
        );
        
        std::iter_swap(f, std::prev(m));
        
        ranges_to_be_sorted.push( std::tuple{f, m} );
        ranges_to_be_sorted.push( std::tuple{m, l} );
    }
}

int main()
{
    bool all_true = true;
    
    { // empty
        std::vector<int> xs;
        auto ys = xs;
        
        ::quick_sort(xs.begin(), xs.end());
        
        auto result = std::is_sorted(xs.begin(), xs.end());
        all_true &= result;
        
        std::cout << std::boolalpha << result  << " for ";
        std::copy(ys.begin(), ys.end(), std::ostream_iterator<int>{std::cout, " "});
        std::cout << "\n";
    }
    
    { // one
        std::vector<int> xs{1};
        auto ys = xs;
        
        ::quick_sort(xs.begin(), xs.end());
        
        auto result = std::is_sorted(xs.begin(), xs.end());
        all_true &= result;
        
        std::cout << std::boolalpha << result  << " for ";
        std::copy(ys.begin(), ys.end(), std::ostream_iterator<int>{std::cout, " "});
        std::cout << "\n";
    }
    
    { // sorted
        std::vector<int> xs(10, 0);
        std::iota(xs.begin(), xs.end(), 0);
        
        auto ys = xs;        
        
        ::quick_sort(xs.begin(), xs.end());
        
        auto result = std::is_sorted(xs.begin(), xs.end());
        all_true &= result;
        
        std::cout << std::boolalpha << result  << " for ";
        std::copy(ys.begin(), ys.end(), std::ostream_iterator<int>{std::cout, " "});
        std::cout << "\n";
    }
    
    { // reversed
        std::vector<int> xs(10, 0);
        std::iota(xs.begin(), xs.end(), 0);
        std::reverse(xs.begin(), xs.end());
        
        auto ys = xs;
        
        ::quick_sort(xs.begin(), xs.end());
        
        auto result = std::is_sorted(xs.begin(), xs.end());
        all_true &= result;
        
        std::cout << std::boolalpha << result  << " for ";
        std::copy(ys.begin(), ys.end(), std::ostream_iterator<int>{std::cout, " "});
        std::cout << "\n";
    }
    
    {   // randomized test
        std::random_device rd;
        std::uniform_int_distribution<std::size_t> len_dist{0, 1000};
        std::uniform_int_distribution<int> value_dist{-1000, 1000};
        
        for(int i = 0; i < 100; ++ i) {
            //
            auto const len = len_dist(rd);
            std::vector<int> xs(len, 0);
            std::generate_n(
                xs.begin(), len,
                [&]{ return value_dist(rd); }
            );
            auto ys = xs;
            //
        
            ::quick_sort(xs.begin(), xs.end());
        
            auto result = std::is_sorted(xs.begin(), xs.end());
            all_true &= result;
            
            std::cout << std::boolalpha << result  << " for ";
            std::copy(ys.begin(), ys.end(), std::ostream_iterator<int>{std::cout, " "});
            std::cout << "\n";
        }
    }
    
    std::cout << "all_true " << all_true;
}
