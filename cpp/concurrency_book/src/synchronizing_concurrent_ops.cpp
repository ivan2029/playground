#include "declarations.hpp"

//
//
//
/*
    Condition that must be satisfied for consumer to acquire a lock is:
      queue must not be empty
*/
auto waiting_with_condition_variable() -> void {
    std::mutex              mutex;
    std::condition_variable data_put_cond;
    std::queue<int>         queue;

    auto put = [&](int x) -> void {
        std::cout << ("put(" + std::to_string(x) + ")\n");
        {
            std::unique_lock<std::mutex> lock{mutex};
            queue.push(x);
            data_put_cond.notify_one();
        }
    };

    auto get = [&]() -> int {
        std::cout << "get()\n";
        //
        int value{0};
        {
            std::unique_lock<std::mutex> lock{mutex};
            data_put_cond.wait(lock, [&]{ return !queue.empty(); });
            value = queue.front();
            queue.pop();
        }
        return value;
    };

    auto producer_fn = [&] {
        for(int i = 1; i < 10; ++ i) {
            sleep_some();
            put(i);
        }
        put(0);
    };

    auto consumer_fn = [&] {
        for(;;) {
            sleep_some();
            auto const value = get();
            std::cout << ("consumer got: " + std::to_string(value) + "\n");
            if(value == 0) break;
        }
    };

    std::thread producer{producer_fn};
    std::thread consumer{consumer_fn};

    producer.join();
    consumer.join();
}

//
//
//
template<class T>
class ThreadSafeQueue {
public:
    ThreadSafeQueue() = default;
    ThreadSafeQueue(ThreadSafeQueue const&) = delete;
    ~ThreadSafeQueue() = default;

    ThreadSafeQueue& operator= (ThreadSafeQueue const&) = delete;

public:

    auto push(T value) -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_queue.push(std::move(value));
        m_value_pushed.notify_one();
    }

    auto try_pop() -> std::optional<T> {
        std::optional<T> result;
        {
            std::unique_lock<std::mutex> lock{m_mutex};
            if(!m_queue.empty()) {
                result = std::move(m_queue.front());
                m_queue.pop();
            }
        }
        return result;
    }

    auto wait_and_pop() -> T {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_value_pushed.wait(lock, [&]{ return !m_queue.empty(); });
        auto const result = std::move(m_queue.front());
        m_queue.pop();
        return result;
    }

private:
    std::mutex               m_mutex;
    std::condition_variable  m_value_pushed;
    std::queue<T>            m_queue;
};


auto threadsafe_queue() -> void {
    //
    using Msg = std::pair<int, int>;

    auto to_string = [](Msg const& msg) {
        return "{" + std::to_string(msg.first) + ", " + std::to_string(msg.second) + "}";
    };

    //
    ThreadSafeQueue<Msg> ts_queue;

    //
    auto producer_fn = [&](int pid) {
        return [&, pid]{
            for(int i = 1; i < 10; ++ i) {
                sleep_some();
                ts_queue.push(Msg{pid, i});
            }
        };
    };

    auto consumer_fn = [&] (int cid) {
        return [&, cid]{
            int count{0};
            for(;;) {
                sleep_some();
                auto const value = ts_queue.wait_and_pop();
                std::cout << ("consumer " + std::to_string(cid) + " got " + to_string(value) + "\n");
                if(value.first == -1) break;
                ++count;
            }
            std::cout << ("consumer " + std::to_string(cid) + " consumed [ " + std::to_string(count) + " ] values\n");
        };
    };

    //
    constexpr int const PRODUCER_COUNT = 7;
    constexpr int const CONSUMER_COUNT = 21;

    std::vector<std::thread> producers;
    for(int pid = 0; pid != PRODUCER_COUNT; ++ pid) {
        producers.emplace_back(producer_fn(pid));
    }

    std::vector<std::thread> consumers;
    for(int cid = 0; cid != CONSUMER_COUNT; ++ cid) {
        consumers.emplace_back(consumer_fn(cid));
    }

    for(auto& p: producers) p.join();

    for(int cid = 0; cid != CONSUMER_COUNT; ++ cid) {
        ts_queue.push(Msg{-1, 0});
    }

    for(auto& c: consumers) c.join();
}

//
//
//
class Latch {
public:
    explicit Latch(std::size_t count)
        : m_count{count}
    {}

    // Behavior undefined when internal counter == 0
    auto count_down() -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        count_down(lock);
    }

    auto wait() -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        wait(lock);    
    }

    // Behavior undefined when internal counter == 0
    auto count_down_and_wait() -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        count_down(lock);
        wait(lock);
    }

private:

    auto count_down(std::unique_lock<std::mutex>& lock) -> void {
        assert(m_count > 0);
        --m_count;
        if(m_count == 0) {
            m_when_count_zero.notify_all();
        }
    }

    auto wait(std::unique_lock<std::mutex>& lock) -> void {
        m_when_count_zero.wait(lock, [&]{ return m_count == 0; });
    }

private:
    std::mutex  m_mutex;
    std::condition_variable m_when_count_zero;

    std::size_t m_count;
};

auto latches() -> void {
    constexpr std::size_t const THREAD_COUNT = 37;

    Latch done{THREAD_COUNT};

    std::vector<std::thread> threads;
    for(std::size_t tid = 0; tid != THREAD_COUNT; ++ tid) {
        threads.emplace_back([&, tid]{
            sleep_some();
            std::cout << ("thread " + std::to_string(tid) + " done\n");
            done.count_down_and_wait();
        });
    }

    done.wait();
    std::cout << "All threads finished\n";

    for(auto& t: threads) t.join();
}

//
//
//
class Barrier {
public:
    explicit Barrier(std::size_t thread_count)
        : m_thread_count{thread_count}
        , m_counter{thread_count}
    {
        assert(thread_count > 0);
    }

    auto arrive_and_wait() -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        arrive_and_wait(lock);
    }

    auto arrive_and_drop() -> void {
        std::unique_lock<std::mutex> lock{m_mutex};
        assert(m_thread_count > 0);
        -- m_thread_count;
        arrive_and_wait(lock);
    }

private:

    auto arrive_and_wait(std::unique_lock<std::mutex>& lock) -> void {
        --m_counter;

        if(m_counter == 0) {
            m_counter = m_thread_count;
            m_when_count_zero.notify_all();
        }
        else {
            m_when_count_zero.wait(lock, [&]{ return m_counter == m_thread_count; });
        }
    }

private:
    std::mutex              m_mutex;
    std::condition_variable m_when_count_zero;

    std::size_t m_thread_count;
    std::size_t m_counter;

};

auto barriers_waits() -> void {
    constexpr std::size_t const THREAD_COUNT = 17;

    Barrier sync{THREAD_COUNT};

    std::vector<std::thread> threads;
    for(std::size_t tid = 0; tid != THREAD_COUNT; ++ tid) {
        threads.emplace_back([&, tid]{
            for(int i = 0; i < 10; ++i) {
                sleep_some();
                std::cout << (
                    "thread "s 
                    + (tid < 10 ? " "s: ""s) 
                    + std::to_string(tid) 
                    + " waiting in iteration "s 
                    + std::to_string(i) 
                    + "\n"s
                    );
                sync.arrive_and_wait();
                
            }
        });
    }

    for(auto& t: threads) t.join();
}

auto barriers_drops() -> void {
    constexpr std::size_t const THREAD_COUNT = 10;

    Barrier sync{THREAD_COUNT};

    std::vector<std::thread> threads;
    for(std::size_t tid = 0; tid != THREAD_COUNT; ++ tid) {
        threads.emplace_back([&, tid]{
            for(std::size_t i = 0; i <= THREAD_COUNT ; ++i) {
                sleep_some();
                std::cout << (
                    "thread "s 
                    + (tid < 10 ? " "s: ""s) 
                    + std::to_string(tid) 
                    + " waiting in iteration "s 
                    + std::to_string(i) 
                    + "\n"s
                    );
                if(i == tid) {
                    sync.arrive_and_drop();
                    break;
                }
                else {
                    sync.arrive_and_wait();
                }
                
            }
        });
    }

    for(auto& t: threads) t.join();
}
