#include <mutex>
#include <condition_variable>
#include <optional>

template<class T>
// requires Semiregular<T>
class SyncVar {
public:
    SyncVar() = default;
    SyncVar(SyncVar const&) = delete;
    SyncVar(SyncVar&&) = delete;
    ~SyncVar() = default;

    SyncVar& operator= (SyncVar const&) = delete;
    SyncVar& operator= (SyncVar&&) = delete;

public:
    //
    auto put(T const& value) -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_get_cv.wait(lock, [&]{ return !static_cast<bool>(m_data); });
        m_data = value;
        m_put_cv.notify_one();
    }
  
    auto put(T&& value) -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_get_cv.wait(lock, [&]{ return !static_cast<bool>(m_data); });
        m_data = std::move(value);
        m_put_cv.notify_one();
    }

    auto get() -> T {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_put_cv.wait(lock, [&]{ return static_cast<bool>(m_data); });
        auto value = std::move(m_data).value();
        m_data = std::optional<T>{};
        m_get_cv.notify_one();
        return value;
    }
  
    //
    auto try_put(T const& value) -> bool {
        bool result = false;
    
        {
            std::unique_lock<std::mutex> lock{m_mutex};
            if(!m_data) {
                m_data = value;
                m_put_cv.notify_one();
                result = true;
            }
        }

        return result;
    }
  
    auto try_put(T&& value) -> bool {
        bool result = false;
    
        {
            std::unique_lock<std::mutex> lock{m_mutex};
            if(!m_data) {
                m_data = std::move(value);
                m_put_cv.notify_one();
                result = true;
            }
        }

        return result;
    }
  
    auto try_get() -> std::optional<T> {
        std::optional<T> result;
    
        {      
            std::unique_lock<std::mutex> lock{m_mutex};
            if(m_data) {
                result = std::move(m_data);
                m_data = std::optional<T>{};
                m_get_cv.notify_one();
            } 
        }
    
        return result;
    }

private:
    std::optional<T>         m_data;
    std::mutex               m_mutex;
    std::condition_variable  m_put_cv, m_get_cv;
};

//
//
//

#include <iostream>
#include <cassert>
#include <thread>
#include <vector>
#include <algorithm>
#include <chrono>
using namespace std::literals;

auto test_0() -> void {
    constexpr int const COUNT = 100000;

    //
    SyncVar<int> si;

    //
    auto producer_fn = [&]{
        for(int i = 0; i < COUNT; ++ i) {
            si.put(i);
        }
    };

    auto consumer_fn = [&]{
        for(int i = 0; i < COUNT; ++ i) {
            int x = si.get();
            assert(x == i);
        }
    };

    //
    std::thread producer{producer_fn};
    std::thread consumer{consumer_fn};

    producer.join();
    consumer.join();
}

auto test_1() -> void {
    constexpr int const PRODUCER_COUNT = 10;
    constexpr int const PER_PRODUCER_COUNT = 100;
    
    //
    SyncVar<int> si;

    //
    auto producer_fn = [&]{
        for(int j = 0; j < PER_PRODUCER_COUNT; ++j) {
            si.put(1);
        }
    };

    auto consumer_fn = [&]{
        for(int i = 0; i < (PRODUCER_COUNT*PER_PRODUCER_COUNT); ++ i) {
            int x = si.get();
            assert(x == 1);
        }
    };

    //
    std::vector<std::thread> producers;
    for(int i = 0; i < PRODUCER_COUNT; ++ i) {
        producers.emplace_back(producer_fn);
    }
    
    std::thread consumer(consumer_fn);
    
    //
    for(auto& p: producers) p.join();
    consumer.join();
}

auto test_2() -> void {
    constexpr int const PRODUCER_COUNT = 10;
    constexpr int const PER_PRODUCER_COUNT = 100;

    //
    SyncVar<int> si;
    std::vector<int> xs;
    
    //
    auto producer_fn = [&]{
        for(int i = 0; i != PER_PRODUCER_COUNT; ++ i) {
            si.put(i);
            //std::this_thread::sleep_for(10ms);
        }
    };

    auto consumer_fn = [&] {
        int caught{0};
        while(caught < PER_PRODUCER_COUNT*PRODUCER_COUNT) {
            auto oi = si.try_get();
            if(oi) {
                ++caught;
                xs.push_back(*oi);
            }
        }
    };
    
    //
    std::vector<std::thread> producers;
    for(int i = 0; i < PRODUCER_COUNT; ++ i) {
        producers.emplace_back(producer_fn);
    }
    
    std::thread consumer{consumer_fn};
    
    //
    for(auto& p: producers) p.join();
    consumer.join();
    
    //
    std::vector<int> expected;
    for(int i = 0; i < PER_PRODUCER_COUNT; ++ i) {
        for(int j = 0; j < PRODUCER_COUNT; ++ j) {
            expected.push_back(i);
        }
    }
    
    std::sort(xs.begin(), xs.end());
    
    assert(expected == xs);
}

auto test_3() -> void {
    constexpr int const PRODUCER_COUNT = 10;
    constexpr int const PER_PRODUCER_COUNT = 100;

    //
    SyncVar<int> si;
    std::vector<int> xs;
    
    //
    auto producer_fn = [&]{
        int put{0};
        while(put < PER_PRODUCER_COUNT) {
            if(si.try_put(put)) {
                ++ put;
            }
            //std::this_thread::sleep_for(10ms);
        }    
    };

    auto consumer_fn = [&] {
        int caught{0};
        while(caught < PER_PRODUCER_COUNT*PRODUCER_COUNT) {
            auto oi = si.try_get();
            if(oi) {
                ++caught;
                xs.push_back(*oi);
            }
        }
    };
    
    //
    std::vector<std::thread> producers;
    for(int i = 0; i < PRODUCER_COUNT; ++ i) {
        producers.emplace_back(producer_fn);
    }
    
    std::thread consumer{consumer_fn};
    
    //
    for(auto& p: producers) p.join();
    consumer.join();
    
    //
    std::vector<int> expected;
    for(int i = 0; i < PER_PRODUCER_COUNT; ++ i) {
        for(int j = 0; j < PRODUCER_COUNT; ++ j) {
            expected.push_back(i);
        }
    }
    
    std::sort(xs.begin(), xs.end());
    
    assert(expected == xs);
}

auto main() -> int {
    test_0();
    test_1();
    test_2();
    test_3();
    std::cout << "DONE\n";
    std::cin.get();
    return 0;
}
