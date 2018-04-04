#pragma once

#include "std.hpp"

namespace threadsafe {

    //
    // based on "Correct and Efficient Bounded FIFO Queues" by Nhat Minh Le, Adrien Guatto, Albert Cohen, Antoniu Pop
    //
    template<class T>
    class Bounded_SPSC_Queue {
    public:

        explicit Bounded_SPSC_Queue(std::size_t const capacity = 10)
            : m_capacity{ capacity }
            , m_storage { new std::optional<T>[capacity] }
        {}

        Bounded_SPSC_Queue(Bounded_SPSC_Queue const& ) = delete;
        Bounded_SPSC_Queue(Bounded_SPSC_Queue&& ) = delete;

        auto operator= (Bounded_SPSC_Queue const& ) -> Bounded_SPSC_Queue = delete;
        auto operator= (Bounded_SPSC_Queue&& ) -> Bounded_SPSC_Queue = delete;

        ~Bounded_SPSC_Queue() {
            delete [] m_storage;
        }

    public:

        template<class T1>
        auto try_push(T1&& value) -> bool {
            auto b = m_back.load(std::memory_order_relaxed);
            if(m_pfront + m_capacity - b == 0) {
                m_pfront = m_front.load(std::memory_order_acquire);
                if(m_pfront + m_capacity - b == 0) {
                    return false;
                }
            }
            m_storage[b % m_capacity] = std::forward<T1>(value);
            m_back.store(b + 1, std::memory_order_release);
            return true;
        }

        auto try_pop() -> std::optional<T> {
            auto f = m_front.load(std::memory_order_relaxed);
            if(m_cback == f) {
                m_cback = m_back.load(std::memory_order_acquire);
                if(m_cback == f) {
                    return {};
                }
            }
            auto value = std::move(m_storage[ f % m_capacity ]);
            m_front.store(f + 1, std::memory_order_release);
            return value;
        }

    private:

    private:
        std::size_t       m_capacity;
        std::optional<T>* m_storage{nullptr};

        std::atomic<std::size_t> m_front{0};
        std::size_t              m_pfront{0};

        std::atomic<std::size_t> m_back{0};
        std::size_t              m_cback{0};
    };


}

