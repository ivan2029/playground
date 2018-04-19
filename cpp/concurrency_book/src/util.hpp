#pragma once

#include "std.hpp"
/*
    puts calling thread to sleep for some time (from 100ms to 1000ms)
*/
auto sleep_some() -> void;

/*
    runs and times a piece of work
    result is duration type (exact type is implementation defined so use duration cast!)
*/
template<class W>
auto run_and_time(W&& work) {
    using namespace std::chrono;
    auto const begin = high_resolution_clock::now();
    work();
    auto const end   = high_resolution_clock::now();
    return end - begin;
}
