#pragma once

#include "std.hpp"

namespace threadsafe {

    //
    // Regular node-based queue with global lock
    //
    template<class T>
        // requires SemiRegular<T>
    class QueueGlobalLock {

        struct Node {
            T          value;
            Node*      next{nullptr};
        };

    public: // ctor, dtor, assign

        QueueGlobalLock() 
            : m_head{nullptr}
            , m_tail{&m_head}
        {}

        QueueGlobalLock(QueueGlobalLock const& ) = delete;

        QueueGlobalLock(QueueGlobalLock&& ) = delete;

        ~QueueGlobalLock() {
            auto head = m_head;
            while(head) {
                auto next = head->next;
                delete head;
                head = next;
            }
        }

        QueueGlobalLock& operator= (QueueGlobalLock const& ) = delete;

        QueueGlobalLock& operator= (QueueGlobalLock&& ) = delete;

    public: // 

        auto try_pop() -> std::optional<T> {
            std::optional<T> result;
            {
                std::lock_guard<std::mutex> head_lock{m_mutex};
                if(m_head != nullptr) {
                    auto const node = m_head;
                    m_head = m_head->next;
                    if(m_head == nullptr) {
                        m_tail = &m_head;
                    }
                    result.emplace(std::move(node->value));
                    delete node;
                }
            }
            return result;
        }

        auto pop() -> T {
            std::unique_lock<std::mutex> head_lock{m_mutex};
            m_head_pushed.wait(head_lock, [&]{ return m_head != nullptr; });
            
            auto const node = m_head;
            m_head = m_head->next;
            if(m_head == nullptr) {
                m_tail = &m_head;
            }
            T value{ std::move(node->value)};
            delete node;

            return value;
        }

        template<class T1>
        auto push(T1&& value) -> void  {
            std::lock_guard<std::mutex> lock{m_mutex};
            auto const node = new Node{std::forward<T1>(value)};
            auto const old_tail = m_tail;
            *m_tail = node;
            m_tail = &node->next;
            if(old_tail == &m_head){
                m_head_pushed.notify_one();
            }
        }

    private:
        Node*  m_head;
        Node** m_tail;

        std::mutex m_mutex;
        std::condition_variable m_head_pushed;
    };

    //
    // Regular node-based queue with fine-grained protection of front and back of the queue
    //
    template<class T>
        // requires SemiRegular<T>
    class QueueFineGrained {

        struct Node {
            T          value;
            Node*      next{nullptr};
        };

    public: // ctor, dtor, assign

        QueueFineGrained() 
            : m_head{nullptr}
            , m_tail{&m_head}
        {}

        QueueFineGrained(QueueFineGrained const& ) = delete;

        QueueFineGrained(QueueFineGrained&& ) = delete;

        ~QueueFineGrained() {
            auto head = m_head;
            while(head) {
                auto const next = head->next;
                delete head;
                head = next;
            }
        }

        QueueFineGrained& operator= (QueueFineGrained const& ) = delete;

        QueueFineGrained& operator= (QueueFineGrained&& ) = delete;

    public: // 

        auto try_pop() -> std::optional<T> {
            std::optional<T> result;
            {
                std::lock_guard<std::mutex> head_lock{m_head_mutex};
                if(m_head != nullptr) {
                    auto const node = m_head;
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

        auto pop() -> T {
            std::unique_lock<std::mutex> head_lock{m_head_mutex};
            m_head_pushed.wait(head_lock, [&]{ return m_head != nullptr; });
            
            // TODO: does this synchronize properly with push? (same in try_pop)
            // maybe using dummy node? or cycle?
            auto const node = m_head;
            m_head = m_head->next;
            if(m_head == nullptr) {
                std::lock_guard<std::mutex> tail_lock{m_tail_mutex};
                m_tail = &m_head;
            }
            T value{ std::move(node->value)};
            delete node;

            return value;
        }

        template<class T1>
        auto push(T1&& value) -> void {
            auto const node = new Node{std::forward<T1>(value)};

            {
                std::lock_guard<std::mutex> tail_lock{m_tail_mutex};
                auto const old_tail = m_tail;

                *m_tail = node;
                m_tail = &node->next;

                if(old_tail == &m_head) {
                    m_head_pushed.notify_one();
                }
            }
        }

    private:
        Node*  m_head;
        Node** m_tail;

        std::mutex m_head_mutex;
        std::mutex m_tail_mutex;

        std::condition_variable m_head_pushed;
    };

    //
    // A simple lookup-table (hash-table) with global lock
    //
    template<class Key, class Value>
        // requires Regular<Key> && Ordered<Key>
        // requires Hashable<Key>
        // requires SemiRegular<Value>
    class LookupTableGlobalLock {
        
        struct Node {
            Key    key;
            Value  value;
            Node*  next{nullptr};
        };

    public: // ctor, dtor, assign

        explicit LookupTableGlobalLock(std::size_t const bucket_count = 128)
            : m_bucket_count{bucket_count}
        {
            using NodePtr = Node*;
            m_buckets = new NodePtr[bucket_count];
            std::fill_n(m_buckets, bucket_count, nullptr);
        }

        LookupTableGlobalLock(LookupTableGlobalLock const& ) = delete;

        LookupTableGlobalLock(LookupTableGlobalLock&& ) = delete;

        ~LookupTableGlobalLock() {
            delete [] m_buckets;
        }

        LookupTableGlobalLock& operator= (LookupTableGlobalLock const& ) = delete;

        LookupTableGlobalLock& operator= (LookupTableGlobalLock&& ) = delete;

    public: // 

        auto bucket_count() const -> std::size_t { return m_bucket_count; }

        // returns true if inserted
        template<class Key1, class Value1>
        auto insert(Key1&& key, Value1&& value) -> bool {
            auto const bucket_head = find_bucket_head(key);
            bool new_node_inserted{false};
            {
                std::lock_guard<std::mutex> lock{m_mutex};

                auto const bucket_node = find_node(bucket_head, key);

                if(*bucket_node == nullptr) {
                    *bucket_node = new Node{
                        std::forward<Key1>(key),
                        std::forward<Value1>(value)
                    };
                    new_node_inserted = true;
                }
            }
            return new_node_inserted;
        }

        template<class Key1, class Value1>
        auto insert_or_update(Key1&& key, Value1&& value) -> void {
            auto const bucket_head = find_bucket_head(key);
            {
                std::lock_guard<std::mutex> lock{m_mutex};

                auto const bucket_node = find_node(bucket_head, key);

                if(*bucket_node == nullptr) {
                    *bucket_node = new Node{
                        std::forward<Key1>(key),
                        std::forward<Value1>(value)
                    };
                } else {
                    (*bucket_node)->value = std::forward<Value1>(value);
                }
            }
        }

        auto find(Key const& key) -> std::optional<Value> {
            std::optional<Value> value;
            auto const bucket_head = find_bucket_head(key);
            {
                std::lock_guard<std::mutex> lock{m_mutex};

                auto const bucket_node = find_node(bucket_head, key);

                if(*bucket_node != nullptr) {
                    value.emplace((*bucket_node)->value);
                }
            }
            return value;
        }

        auto remove(Key const& key) -> bool {
            auto const bucket_head = find_bucket_head(key);
            // TODO
        }

        auto keys() -> std::vector<Key> {
            std::vector<Key> keys_;
            {
                std::lock_guard<std::mutex> lock{m_mutex};

                for(std::size_t i{0}; i != m_bucket_count; ++ i) {
                    for(auto node = m_buckets[i]; node; node = node->next) {
                        keys_.push_back(node->key);
                    }
                }
            }
            return keys_;
        }

    private: 

        auto index_of(Key const& key) -> std::size_t {
            return std::hash<Key>{}(key) % m_bucket_count;
        }

        auto find_bucket_head(Key const& key) -> Node** {
            return &m_buckets[index_of(key)];
        }

        // returns node having given key, or place where new node is to be created
        auto find_node(Node** bucket_head, Key const& key) -> Node** {
            for(; *bucket_head ; bucket_head = &bucket_head->next) {
                if( (*bucket_head)->key == key ) {
                    break;
                }
            }
            return bucket_head;
        }

    private:
        std::size_t  m_bucket_count;
        Node**       m_buckets;
       
        std::mutex  m_mutex;
    };

}

