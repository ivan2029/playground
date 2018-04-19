#pragma once

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
 auto lock_based_global_lock_queue_try_pop() -> void;
 auto lock_based_global_lock_queue_pop() -> void;
 auto lock_based_fine_grained_queue_try_pop() -> void;
 auto lock_based_fine_grained_queue_pop() -> void;

 auto thread_safe_queue_stress_test_global_lock() -> void;
 auto thread_safe_queue_stress_test_fine_grained() -> void;

 auto lock_based_global_lock_lookup_table() -> void;
 auto lock_based_fine_grained_lookup_table() -> void;

 auto list_pushing_and_removing() -> void;

 // Chapter 7: Designing lock free concurrent data structures
 auto lock_free_bounded_spsc_queue() -> void;
 
