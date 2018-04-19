#include "declarations.hpp"
#include "std.hpp"

static auto free_function() -> void {
    std::cout << __FUNCTION__ << " " << std::this_thread::get_id() << "\n";
}

static auto lambda = []{
    std::cout << __FUNCTION__ << " " << std::this_thread::get_id() << "\n";
};

class CallableObject {
public:

    auto operator() () const -> void {
        std::cout << __FUNCTION__ << " " << std::this_thread::get_id() << "\n";
    }

};

//static auto throwing_function() -> void {
//    std::stringstream sout;
//    sout << __FUNCTION__ << " " << std::this_thread::get_id() << "\n";
//    throw std::runtime_error{sout.str()};
//}

auto managing_threads() -> void {
    //
    {
        std::cout << "hardware_concurrency: " << std::thread::hardware_concurrency() << "\n";
    }

    //
    {
        std::thread thd{free_function};
        thd.join();
    }
    //
    {
        std::thread thd{lambda};
        thd.join();
    }
    //
    {
        std::thread thd{CallableObject{}};
        thd.join();
    }

    //
    {
        std::thread thd{free_function};
        thd.detach();
        // give detached thread time to finish
        std::this_thread::sleep_for(1s);
    }
}
