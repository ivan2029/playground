#include "declarations.hpp"

class SpinMutexAtomicBool {
public:

    auto lock() -> void {
        bool false_{false};
        while(!m_locked.compare_exchange_strong(false_, true)){
            false_ = false;
        }
    }

    auto unlock() -> void {
        bool true_{true};
        while(!m_locked.compare_exchange_strong(true_, false)){
            true_ = true;
        }
    }

    auto try_lock() -> bool {
        bool false_{false};
        return m_locked.compare_exchange_strong(false_, true);
    }

private:
    std::atomic<bool> m_locked{false};
};

class SpinMutexAtomicFlag {
public:

    auto lock() -> void {
        while(m_locked.test_and_set(std::memory_order_acquire)) {}        
    }

    auto unlock() -> void {
        m_locked.clear(std::memory_order_release);
    }

private:
    std::atomic_flag m_locked{ATOMIC_FLAG_INIT}; // starts as unlocked
};

template<class Mutex>
auto test_mutex() -> void {
    using namespace std::chrono;
 
    Mutex mutex;
    long long num{0};
    auto const time = run_and_time([&]{
        std::vector<std::thread> workers;
        for(int i = 0; i < 1024; ++ i) {
            workers.emplace_back([&]{
                for(int i = 0; i < 10000; ++ i) {
                    std::lock_guard<Mutex> lock{mutex};
                    ++ num;
                }
            });
        }
        for(auto& w: workers) w.join();
    });
    std::cout << duration_cast<milliseconds>(time).count() << " " 
        << num << "\n";
}

auto spin_mutex() -> void {
    test_mutex<std::mutex>();
    test_mutex<SpinMutexAtomicBool>();
    test_mutex<SpinMutexAtomicFlag>();
}