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
        lock_based_queue();

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
