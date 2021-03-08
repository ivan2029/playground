#include <cassert>
#include <iostream>
#include <chrono>

#include <algorithm>
#include <numeric>
#include <execution>

#include <immintrin.h>

auto run(auto msg, auto f) {
    using namespace std::chrono;
    std::cout << "start: " << msg << "\n";

    auto begin = high_resolution_clock::now();
    
    auto const result = f();

    auto end = high_resolution_clock::now();

    std::cout << "result: " << result << "\n";
    std::cout << "time: " << duration_cast<milliseconds>(end - begin).count() << "ms\n";
    std::cout << "end: " << msg << "\n\n";
}

auto sum_simd(int const* data, std::size_t size) -> int {
    assert(size % 8 == 0);
    
    __m256i sum;

    for(std::size_t i = 0; i < size; i += 8) {
        auto ptr = data + i;
        __m256i x = _mm256_load_si256((__m256i const*)ptr);
        sum =  _mm256_add_epi32(sum, x);
    }

    int unpacked_sum[8];
    _mm256_store_si256((__m256i*)unpacked_sum, sum); 

    for(std::size_t i = 1; i < 8; ++ i) {
        unpacked_sum[0] += unpacked_sum[i];
    }

    return unpacked_sum[0];
}

auto main() -> int {
    std::vector<int> xs(100'000'000, 0);
    std::iota(xs.begin(), xs.end(), 1);

    run("std::reduce seq", 
        [&]{
            return std::reduce(
                    std::execution::seq,
                    xs.begin(), xs.end(),
                    0,
                    std::plus<int>{}
                );
        });

    run("std::reduce par", 
        [&]{
            return std::reduce(
                    std::execution::par,
                    xs.begin(), xs.end(),
                    0,
                    std::plus<int>{}
                );
        });

    run("std::reduce unseq", 
        [&]{
            return std::reduce(
                    std::execution::unseq,
                    xs.begin(), xs.end(),
                    0,
                    std::plus<int>{}
                );
        });

    run("std::reduce par_unseq", 
        [&]{
            return std::reduce(
                    std::execution::par_unseq,
                    xs.begin(), xs.end(),
                    0,
                    std::plus<int>{}
                );
        });

    run("sum simd",
        [&] {
            return ::sum_simd(xs.data(), xs.size());
        });

    return 0;
}
