#include "declarations.hpp"

//
//
//
auto mutexes() -> void {
    constexpr int const COUNT = 1000;
    constexpr int const THREAD_COUNT = 10;

    static_assert( COUNT % THREAD_COUNT == 0 );

    //
    std::list<int> xs;
    std::mutex     xs_mutex;

    //
    auto push = [&](int x) {
        std::lock_guard<std::mutex> lock{xs_mutex};
        xs.push_back(x);
    };

    // 
    auto pusher = [&] {
        for(int i = 0; i < COUNT/THREAD_COUNT; ++ i) {
            push(i);
        }
    };

    std::vector<std::thread> workers;
    
    for(int i = 0; i < THREAD_COUNT; ++ i) {
        workers.emplace_back(pusher);
    }

    for(auto& worker: workers) {
        worker.join();
    }

    //
    std::cout << "expected " << COUNT << " elements in the list, got: " 
        << xs.size() << ", are same: " << (COUNT == xs.size()) << "\n"; 

    //
    xs.sort();
    xs.erase(std::unique(xs.begin(), xs.end()), xs.end());
    
    std::list<int> diffs;
    std::adjacent_difference(xs.begin(), xs.end(), std::back_inserter(diffs));
    diffs.pop_front(); // because diffs.front() == xs.front() == 0, see definition of adjacent_difference

    std::cout << "all are 1: " 
        << std::all_of(diffs.begin(), diffs.end(), [](int x) { return x == 1; }) 
        << "\n";
}

//
//
//

template<class T>
class ThreadSafeStack {
public:


public:

    auto try_pop() -> std::optional<T> {
        std::optional<T> result;
        {
            std::unique_lock<std::mutex> lock{m_mutex};
            if(!m_stack.empty()) {
                result = std::move(m_stack.top());
                m_stack.pop();
            }
        }
        return result;
    }

    auto push(T const& value) {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_stack.push(value);
    }

    auto push(T&& value) {
        std::unique_lock<std::mutex> lock{m_mutex};
        m_stack.push(std::move(value));
    }

private:
    std::mutex    m_mutex;
    std::stack<T> m_stack;
};


auto threadsafe_stack() -> void {
    //
    constexpr int const PRODUCER_COUNT = 10;
    constexpr int const ITEMS_PER_PRODUCER_COUNT = 1000;

    //
    using ProducerId = int;
    using Value = std::pair<ProducerId, int>;

    //
    ThreadSafeStack<Value> st;

    //
    auto producer_fn = [&](int pid){
        return [&, pid] {
            for(int i = 0; i != ITEMS_PER_PRODUCER_COUNT; ++ i) {
                st.push( std::make_pair(pid, i) );
            }
            std::cout << ("producer " + std::to_string(pid) + " finished\n");
        };
    };

    auto consumer_fn = [&]{
        int popped{0};
        while(popped != ITEMS_PER_PRODUCER_COUNT*PRODUCER_COUNT) {
            auto ov = st.try_pop();
            if(ov) {
                ++popped;
            }
        }
        std::cout << "consumer finished\n";
    };

    //
    std::vector<std::thread> workers;
    for(int i = 0; i != PRODUCER_COUNT; ++ i) {
        workers.emplace_back(producer_fn(i));
    }
    workers.emplace_back(consumer_fn);

    //
    for(auto& w: workers) w.join();

    std::cout << "stack has something in it: " << st.try_pop().has_value() << "\n";
}
