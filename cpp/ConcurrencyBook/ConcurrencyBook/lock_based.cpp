#include "declarations.hpp"

namespace threadsafe {

    template<class T>
    class Queue {

        struct Node {
            T          value;
            Node*      next{nullptr};
        };

    public: // ctor, dtor, assign

        Queue() 
            : m_head{nullptr}
            , m_tail{&m_head}
        {}

        Queue(Queue const& ) = delete;

        Queue(Queue&& ) = delete;

        ~Queue() {
            auto head = m_head;
            while(head) {
                auto next = head->next;
                delete head;
                head = next;
            }
        }

        Queue& operator= (Queue const& ) = delete;

        Queue& operator= (Queue&& ) = delete;

    public: // 

        auto try_pop() -> std::optional<T> {
            std::optional<T> result;
            {
                std::lock_guard<std::mutex> head_lock{m_head_mutex};
                if(m_head != nullptr) {
                    auto node = m_head;
                    m_head = m_head->next;
                    if(m_head == nullptr) {
                        std::lock_guard<std::mutex> tail_lock{m_tail_mutex};
                        m_tail = &m_head;
                    }
                    result.emplace(std::move(node->value));
                    delete node;
                }
            }
            return result;
        }

        auto push(T value) -> void {
            auto node = new Node{std::move(value)};

            {
                std::lock_guard<std::mutex> tail_lock{m_tail_mutex};
                *m_tail = node;
                m_tail = &node->next;
            }
        }

    private:
        Node*  m_head;
        Node** m_tail;

        std::mutex m_head_mutex;
        std::mutex m_tail_mutex;
    };

}

template<class F, class S>
auto operator<< (std::ostream& out, std::pair<F, S> const& p) -> std::ostream& {
    return out << "(" << std::setw(8) << p.first << ", " << std::setw(8) << p.second << ")";
}

auto lock_based_queue() -> void {
    //
    using ProducerId = int;
    using Item       = int;
    using Value      = std::pair<ProducerId, Item>;

    constexpr int PRODUCER_COUNT     = 512;
    constexpr int ITEMS_PER_PRODUCER = 1 << 15;

    //
    threadsafe::Queue<Value> queue;
   
    auto producer_fn = [&](ProducerId const pid){
        return [&, pid]{
            for(Item i{0}; i < ITEMS_PER_PRODUCER; ++ i) {
                //sleep_some();
                queue.push(Value{pid, i});
            }
        };
    };

    // only one consumer
    std::deque<Value> values;
    auto consumer_fn = [&]{
        for(;;) {
            //sleep_some();
            auto value = queue.try_pop();
            if(value) {
                if(value->first == -1) return;
                values.push_back(*value);
            }
        }
    };

    // start threads
    std::vector<std::thread> producers;
    for(ProducerId pid{0}; pid < PRODUCER_COUNT; ++ pid) {
        producers.emplace_back(producer_fn(pid));
    }

    std::thread consumer{consumer_fn};

    // wait for all
    for(auto& p: producers) p.join();
    queue.push(Value{-1, 0});
    consumer.join();

    // run tests
    auto const expected = [=]{
        std::deque<Value> expected;
        for(ProducerId pid{0}; pid != PRODUCER_COUNT; ++pid) {
            for(Item item{0}; item != ITEMS_PER_PRODUCER; ++item) {
                expected.push_back(Value{pid, item});
            }
        }
        return expected;
    }();

    std::sort(values.begin(), values.end());

    //std::cout << values.size() << " " << expected.size() << "\n";

    //for(std::size_t i{0}; i < values.size(); ++ i) {
    //    std::cout << values[i] << " " << expected[i] << "\n";
    //}

    assert(expected == values);
}

