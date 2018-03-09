#include "declarations.hpp"

auto main() -> int {
    std::cout << std::boolalpha;
    try {
        //managing_threads();
        //mutexes();
        //threadsafe_stack();
        //waiting_with_condition_variable();
        //threadsafe_queue();
        //latches();
        //barriers_waits(); 
        //barriers_drops(); 
        //are_atomics_lock_free();
        //spin_mutex();
        //memory_orders();
        lock_based_global_lock_queue_try_pop();
        lock_based_global_lock_queue_pop();
        lock_based_fine_grained_queue_try_pop();
        lock_based_fine_grained_queue_pop();
        //lock_based_global_lock_lookup_table();
        //lock_based_fine_grained_lookup_table();

    }
    catch(std::exception const& e) {
        std::cerr << "error: " << e.what() << "\n";
    }
    catch(...) {
        std::cerr << "error: unknown\n";
    }
    std::cout << "\n|> Done\n";
    std::cin.get();
    return 0;
}
