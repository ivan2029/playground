#include "declarations.hpp"

//
//
//
#include "data_structures.hpp"

template<class T>
auto operator<< (std::ostream& out, std::optional<T> const& o) -> std::ostream& {
    if(o) {
        out << "Some(" << *o << ")";
    }
    else {
        out << "None";
    }
    return out;
}

//
//
//
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
        //lock_based_global_lock_queue_try_pop();
        //lock_based_global_lock_queue_pop();
        //lock_based_fine_grained_queue_try_pop();
        //lock_based_fine_grained_queue_pop();
        //thread_safe_queue_stress_test_global_lock();
        //thread_safe_queue_stress_test_fine_grained();
        //lock_based_global_lock_lookup_table();
        //lock_based_fine_grained_lookup_table();
        list_pushing_and_removing();

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
