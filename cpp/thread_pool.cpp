
/*
 * Things to consider about this thread pool:
 *
 * 1. This is not work stealing pool. While every thread has its own queue, 
 * they are not taking work from others!
 *
 * 2. Current implementation doesn't support posting work from worker thread.
 * Or, rather, this will block hopelessly if work tries to post more work, and
 * it happens to be queued to the same worker thread (is this actually the case?
 * work is not under the mutex). UPDATE: not the case, you can post a task.
 *
 * 3. Task groups. Post and wait (add current thread to the worker pool?).
 *
 */

#include <cassert>
#include <memory>
#include <queue>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <random>

#include <iomanip>
#include <sstream>
#include <iostream>

//
template<class Num, class Num2>
auto greater_than_zero_or(Num n, Num2 alt) -> Num {
    return n > 0 ? n : alt;
}

//
class thread_pool {
public:

    struct work_concept {
        virtual ~work_concept() = default;
        virtual auto invoke() -> void = 0;
    };
    
    template<class F>
    struct work_model : public work_concept {
        work_model(F f)
            : m_f{std::move(f)}
        {}
        
        auto invoke() -> void override {
            m_f();
        }
        
        F m_f;
    };

    struct work {
        
        work() = default;
        ~work() = default;
        work(work&&) = default;
        work(work const& ) = delete;
        
        auto operator= (work&&) -> work& = default;
        auto operator= (work const&) -> work& = delete;
        
        template<class F>
        work(F f)
            : m_f{new work_model<F>{std::move(f)} }
        {}
    
        auto invoke() const {
            m_f->invoke();
        }
    
        std::unique_ptr<work_concept> m_f;
    };
    
public:

    explicit thread_pool(
        std::size_t worker_count = ::greater_than_zero_or( std::thread::hardware_concurrency(), 8 )
    );
    thread_pool(thread_pool const&) = delete;
    thread_pool(thread_pool&&) = delete;
    ~thread_pool();
    
    auto operator= (thread_pool const&) -> thread_pool& = delete;
    auto operator= (thread_pool&&) -> thread_pool& = delete;    
    
public:

    auto post(work w) -> void;

    auto stop_and_join() -> void;

private:

    struct worker_queue {
        std::condition_variable signal;
        std::mutex              mutex;
        
        bool                    running{true};
        std::queue<work>        queue;
    };

private:

    std::random_device                         m_random_device;
    std::uniform_int_distribution<std::size_t> m_distribution;
    
    std::vector<worker_queue>                  m_worker_queues;
    std::vector<std::thread>                   m_worker_threads;
};

thread_pool::thread_pool(std::size_t worker_count)
    : m_distribution{0, worker_count - 1}
    , m_worker_queues(worker_count)
{
    assert( worker_count > 1 );
    
    m_worker_threads.reserve(worker_count);
    
    for(std::size_t worker_id{0}; worker_id != worker_count; ++ worker_id) {
        m_worker_threads.emplace_back([&, worker_id] {
            auto& wq = m_worker_queues[worker_id];

            std::size_t work_count{0};

            while( true ) {
                work w;
                
                { // get next
                    std::unique_lock lock{wq.mutex};
                    
                    wq.signal.wait(lock, [&]{ return ! wq.running || ! wq.queue.empty(); });
                    
                    if(!wq.running) {
                        // diagnostics...
                        std::stringstream sout;
                        sout << "thread " << std::this_thread::get_id() << " did " << work_count << " work\n";
                        std::cout << sout.str();
                        return;
                    }
                    
                    w = std::move( wq.queue.front() );
                    wq.queue.pop();
                }
            
                w.invoke();
                ++work_count;
            }
        });
    }
}

thread_pool::~thread_pool() {
}

auto thread_pool::post(work w) -> void {
    auto const worker_id = m_distribution(m_random_device);
    
    {
        auto& wq = m_worker_queues[worker_id];
        std::unique_lock lock{wq.mutex};
        wq.queue.push(std::move(w));
        wq.signal.notify_one();
    }
}

auto thread_pool::stop_and_join() -> void {
    for(auto& wq: m_worker_queues) {
        std::unique_lock lock{wq.mutex};
        wq.running = false;
        wq.signal.notify_one();
    }
    for(auto& th: m_worker_threads) {
        th.join();
    }
}

//
// test
//

#include <vector>
#include <future>
#include <numeric>
#include <algorithm>
#include <iterator>

//
//
//
thread_local std::random_device                     g_random_device;
thread_local std::uniform_real_distribution<float>  g_distribution{-1.0f, 1.0f};



//
//
//
auto main() -> int {
    //
    constexpr std::size_t const ITEM_COUNT{1 << 15};
    constexpr std::size_t const WORK_SIZE{1 << 5};
    constexpr std::size_t const WORK_COUNT{ITEM_COUNT/WORK_SIZE};
    
    //
    std::vector<float> xs(ITEM_COUNT, 0.0f);

    //
    ::thread_pool pool{8};
    
    std::vector<std::future<void>> futures;
    futures.reserve(WORK_COUNT);
    
    for(std::size_t work_id{0}; work_id != WORK_COUNT; ++ work_id) {
        std::packaged_task<void()> work{[&xs, work_id]{
            auto const begin = work_id * WORK_SIZE;
            auto const end   = begin + WORK_SIZE;
                  
            // {
            //     std::stringstream sout;
            //     sout 
            //         << std::this_thread::get_id() 
            //         << " : beginning work " << std::setw(4) << work_id << " of " << WORK_COUNT 
            //         << ", begin = " << std::setw(4) << begin << ", end = " <<  std::setw(4) << end  
            //         << "\n";
            //     std::cout << sout.str();
            // }
            
            auto const it = xs.begin();
            std::generate(
                it + begin, it + end, 
                []{ return g_distribution(g_random_device); }
            );
        }};
        futures.push_back(work.get_future());
        pool.post(std::move(work));
    }
    
    for(auto& future: futures) {
        future.get();
    }

    pool.stop_and_join();

    //
    auto const sum = std::accumulate(
        xs.begin(), xs.end(),
        0.0f,
        std::plus<float>{}
    );
    std::cout << "sum: " << sum << "\n";
    
    // for(std::size_t i{0}; i < xs.size(); ++ i) {
    //     std::cout << i << " -> " << xs[i] << "\n";
    // }
    
    return 0;
}