#include "util.hpp"

auto sleep_some() -> void {
    static thread_local std::random_device rd;
    static thread_local std::uniform_int_distribution<> dist{10, 100};
    std::this_thread::sleep_for(std::chrono::milliseconds{dist(rd)});
}
