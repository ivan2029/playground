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


// chapter 2: Managing threads
auto managing_threads() -> void;

// Chapter 3: Sharing data between threads
 auto mutexes() -> void;
 auto threadsafe_stack() -> void;

// Chapter 4: Synchronizing concurrent operations
 auto waiting_with_condition_variable() -> void;
 auto threadsafe_queue() -> void;
 auto latches() -> void;
 auto barriers_waits() -> void;
 auto barriers_drops() -> void;

 // Chapter 5: The C++ memory model and operations on atomic types
 auto are_atomics_lock_free() -> void;
 auto spin_mutex() -> void;
 auto memory_orders() -> void;

 // Chapter 6: Designing lock based concurrent data structures
 auto lock_based_queue() -> void;