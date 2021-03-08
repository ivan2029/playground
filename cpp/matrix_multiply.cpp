// compiles with gcc 8.2 (OpenMP 3+ required, because I'm using size_t for indices)
// version without OpemMP compiles nicely with VS2017
// command: gcc -std=c++17 matrix_multiply.cpp -O2 -ffast-math -fopenmp

#include <iostream>
#include <iomanip>
#include <chrono>
#include <utility>
#include <variant>
#include <cassert>
#include <algorithm>
#include <random>

// row-major ordering

//
//
//
auto matrix_multiply_naive(
    float const* src_a, std::size_t const a_rows, std::size_t const a_cols,
    float const* src_b, std::size_t const b_rows, std::size_t const b_cols,
    float* dst, std::size_t const dst_rows, std::size_t const dst_cols
    ) -> void 
{
    assert( a_cols == b_rows );
    assert( dst_rows == a_rows );
    assert( dst_cols == b_cols );

    for(std::size_t row{0}; row < dst_rows; ++ row) {
        for(std::size_t col{0}; col < dst_cols; ++ col) {
            float x{0.0f};
            for(std::size_t i{0}; i < a_cols; ++ i) {
                x += src_a[row*a_cols + i]*src_b[i*b_cols + col];
            }
            dst[row*dst_cols + col] = x;
        }
    }
}

//
//
//
auto matrix_multiply_naive_2(
    float const* src_a, std::size_t const a_rows, std::size_t const a_cols,
    float const* src_b, std::size_t const b_rows, std::size_t const b_cols,
    float* dst, std::size_t const dst_rows, std::size_t const dst_cols
    ) -> void 
{
    assert( a_cols == b_rows );
    assert( dst_rows == a_rows );
    assert( dst_cols == b_cols );

    for(std::size_t col{0}; col < dst_cols; ++ col) {
        for(std::size_t row{0}; row < dst_rows; ++ row) {
            float x{0.0f};
            for(std::size_t i{0}; i < a_cols; ++ i) {
                x += src_a[row*a_cols + i]*src_b[i*b_cols + col];
            }
            dst[row*dst_cols + col] = x;
        }
    }
}

//
//  
//
constexpr const std::size_t BLOCK_SIZE = 32;

auto matrix_multiply_kernel(
    float const* src_a, std::size_t const a_rows, std::size_t const a_cols,
    float const* src_b, std::size_t const b_rows, std::size_t const b_cols,
    float* dst, std::size_t const dst_rows, std::size_t const dst_cols
    ) -> void 
{
    assert( a_cols == b_rows );
    assert( dst_rows == a_rows );
    assert( dst_cols == b_cols );

    //
    std::fill_n( dst, dst_rows*dst_cols, 0.0f );

    //
    std::size_t const dst_blocks_per_row  = dst_cols/BLOCK_SIZE;
    std::size_t const dst_last_block_size = dst_cols%BLOCK_SIZE;

    //
    for(std::size_t row{0}; row < dst_rows; ++ row) {
        //
        for(std::size_t block{0}; block < dst_blocks_per_row; ++ block) {
            for(std::size_t i{0}; i < a_cols; ++ i) {
                // block compute
                for(std::size_t b{0}; b < BLOCK_SIZE; ++ b) {
                    dst[row*dst_cols + block*BLOCK_SIZE + b] +=
                        src_a[row*a_cols + i] *
                        src_b[i*b_cols + block*BLOCK_SIZE + b ];
                }
            }
        }
        //
        for(std::size_t i{0}; i < a_cols; ++ i) {
            // block compute
            for(std::size_t b{0}; b < dst_last_block_size; ++ b) {
                dst[row*dst_cols + dst_blocks_per_row*BLOCK_SIZE + b] +=
                    src_a[row*a_cols + i] *
                    src_b[i*b_cols + dst_blocks_per_row*BLOCK_SIZE + b ];
            }
        }
    }
}

auto matrix_multiply_kernel_omp(
    float const* src_a, std::size_t const a_rows, std::size_t const a_cols,
    float const* src_b, std::size_t const b_rows, std::size_t const b_cols,
    float* dst, std::size_t const dst_rows, std::size_t const dst_cols
    ) -> void 
{
    assert( a_cols == b_rows );
    assert( dst_rows == a_rows );
    assert( dst_cols == b_cols );

    //
    std::fill_n( dst, dst_rows*dst_cols, 0.0f );

    //
    std::size_t const dst_blocks_per_row  = dst_cols/BLOCK_SIZE;
    std::size_t const dst_last_block_size = dst_cols%BLOCK_SIZE;
    
    // TODO: Improvement idea: write a function that computes multiplication for block of destination matrix, then
    // parallelize on that...
    for(std::size_t row{0}; row < dst_rows; ++ row) {
        #pragma omp parallel
        {
        //
        #pragma omp  for
        for(std::size_t block = 0; block < dst_blocks_per_row; ++ block) {
            for(std::size_t i{0}; i < a_cols; ++ i) {
                // block compute
                for(std::size_t b{0}; b < BLOCK_SIZE; ++ b) {
                    dst[row*dst_cols + block*BLOCK_SIZE + b] +=
                        src_a[row*a_cols + i] *
                        src_b[i*b_cols + block*BLOCK_SIZE + b ];
                }
            }
        }

        //
        #pragma omp task
        for(std::size_t i = 0; i < a_cols; ++ i) {
            // block compute
            for(std::size_t b{0}; b < dst_last_block_size; ++ b) {
                dst[row*dst_cols + dst_blocks_per_row*BLOCK_SIZE + b] +=
                    src_a[row*a_cols + i] *
                    src_b[i*b_cols + dst_blocks_per_row*BLOCK_SIZE + b ];
            }
        }
        } // end pragma omp parallel
    }

}

//
//
//
template<class F>
auto run_and_time(F&& f) {
    using namespace std::chrono;
    auto begin = high_resolution_clock::now();
    auto const result = f();
    auto end   = high_resolution_clock::now();
    return std::make_pair(
        duration_cast<milliseconds>(end - begin),
        std::move(result)
    );
}

auto test_for_size(std::size_t const size) -> void {
    //
    float* a  = new float[size*size];
    float* b  = new float[size*size];
    float* c1 = new float[size*size];
    float* c2 = new float[size*size];
    float* c3 = new float[size*size];
    float* c4 = new float[size*size];

    //
    std::cout << "TEST initialize, size = " << size << "\n";

    std::random_device rd;
    std::uniform_real_distribution<float> dist{-1.0, 1.0};

    for(std::size_t i{0}; i < size*size; ++ i) {
        a[i] = dist(rd);
        b[i] = dist(rd);
    }

    //
    std::cout << "TEST start\n";

    //
    {
        auto const [elapsed, result] = run_and_time([=]{
            matrix_multiply_naive(
                a,  size, size,
                b,  size, size,
                c1, size, size
            );
            return std::monostate{};
        });
        std::cout << "naive multiply: " << elapsed.count() << "ms\n";
    }
    //
    {
        auto const [elapsed, result] = run_and_time([=]{
            matrix_multiply_naive_2(
                a,  size, size,
                b,  size, size,
                c2, size, size
            );
            return std::monostate{};
        });
        std::cout << "naive multiply (2): " << elapsed.count() << "ms\n";
    }
    //
    {
        auto const [elapsed, result] = run_and_time([=]{
            matrix_multiply_kernel(
                a,  size, size,
                b,  size, size,
                c3, size, size
            );
            return std::monostate{};
        });
        std::cout << "kernel multiply: " << elapsed.count() << "ms\n";
    }

    //
    {
        auto const [elapsed, result] = run_and_time([=]{
            matrix_multiply_kernel_omp(
                a,  size, size,
                b,  size, size,
                c4, size, size
            );
            return std::monostate{};
        });
        std::cout << "kernel multiply (omp): " << elapsed.count() << "ms\n";
    }

    /* */
    std::size_t diff_count{0};
    for(std::size_t i{0}; i < size*size; ++ i) {
        if ( std::fabs(c1[i] - c3[i]) > 0.00001 ) ++ diff_count;
    }

    std::cout << "diff count : " << diff_count << "\n";
/*
    for(std::size_t i{0}; i < size*size; ++ i) {
        if ( auto const dist = std::fabs(c1[i] - c3[i]); dist > 0.00001 )
            std::cout 
                << std::setw(10) << c1[i] << " " 
                << std::setw(10) << c3[i] << " "
                << std::setw(10) << dist << "\n";
    }
*/

    /* */
    std::size_t diff_count_omp{0};
    for(std::size_t i{0}; i < size*size; ++ i) {
        if ( std::fabs(c1[i] - c4[i]) > 0.00001 ) ++ diff_count_omp;
    }

    std::cout << "diff count (omp): " << diff_count_omp << "\n";
/*
    for(std::size_t i{0}; i < size*size; ++ i) {
        if ( auto const dist = std::fabs(c1[i] - c4[i]); dist > 0.00001 )
            std::cout 
                << std::setw(10) << c1[i] << " " 
                << std::setw(10) << c4[i] << " "
                << std::setw(10) << dist << "\n";
    }
*/

    std::cout << "\n";
    
    //
    delete[] a;
    delete[] b;
    delete[] c1;
    delete[] c2;
    delete[] c3;
    delete[] c4;
}

auto main() -> int {
    test_for_size(128);
    test_for_size(512);
    test_for_size(700);
    test_for_size(1024);
    return 0;
}
