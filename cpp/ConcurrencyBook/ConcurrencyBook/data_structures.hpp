#pragma once

#include "std.hpp"

namespace threadsafe {

    //
    //
    //
    template<class T>
    class NodeBasedQueue {
        struct Node {
            T          value;
            Node*      next{nullptr};
        };

    public: // ctor, dtor, assign

        NodeBasedQueue() 
            : m_head{nullptr}
            , m_tail{&m_head}
        {}

        NodeBasedQueue(NodeBasedQueue const& ) = delete;

        NodeBasedQueue(NodeBasedQueue&& ) = delete;

        ~NodeBasedQueue() {
            auto head = m_head;
            while(head) {
                auto next = head->next;
                delete head;
                head = next;
            }
        }

        NodeBasedQueue& operator= (NodeBasedQueue const& ) = delete;

        NodeBasedQueue& operator= (NodeBasedQueue&& ) = delete;

    public: // 

        auto try_pop() -> std::optional<T> {
            std::optional<T> result;
            
            if( !is_empty() ) {
                auto const node = m_head;
                m_head = m_head->next;
                if(m_head == nullptr) {
                    m_tail = &m_head;
                }
                result.emplace(std::move(node->value));
                delete node;
            }
            return result;
        }

        auto pop() -> T {
            assert( !is_empty() );

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
            auto const node = new Node{std::forward<T1>(value)};      
            *m_tail = node;
            m_tail = &node->next;
        }

        auto is_empty() const -> bool {
            return m_head == nullptr;
        }

    private:
        Node*  m_head;
        Node** m_tail;
    };


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

        QueueGlobalLock() = default;

        QueueGlobalLock(QueueGlobalLock const& ) = delete;

        QueueGlobalLock(QueueGlobalLock&& ) = delete;

        ~QueueGlobalLock() = default;

        QueueGlobalLock& operator= (QueueGlobalLock const& ) = delete;

        QueueGlobalLock& operator= (QueueGlobalLock&& ) = delete;

    public: // 

        auto try_pop() -> std::optional<T> {
            std::unique_lock<std::mutex> lock{m_mutex};
            std::optional<T> result;
            if( !m_queue.is_empty() ) {
                result = m_queue.pop();
            }
            return result;
        }

        auto pop() -> T {
            std::unique_lock<std::mutex> lock{m_mutex};
            m_head_pushed.wait(lock, [&]{ return !m_queue.is_empty(); });
            return m_queue.pop();
        }

        template<class T1>
        auto push(T1&& value) -> void  {           
            std::unique_lock<std::mutex> lock{m_mutex};
            auto const was_empty = m_queue.is_empty();
            m_queue.push(std::forward<T1>(value));
            if(was_empty) {
                m_head_pushed.notify_one();
            }
        }

    private:
        NodeBasedQueue<T>        m_queue;
        std::mutex               m_mutex;
        std::condition_variable  m_head_pushed;
    };

    //
    // Regular node-based queue with fine-grained protection of front and back of the queue
    //
    template<class T>
        // requires SemiRegular<T>
    class QueueFineGrained {

        struct Node {
            std::optional<T>  value;
            Node*             next{nullptr};
        };

    public: // ctor, dtor, assign

        QueueFineGrained() 
        {
            auto const dummy_node = new Node{};
            m_head = dummy_node;
            m_tail = dummy_node;
        
        }

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
            std::unique_lock<std::mutex> head_lock{m_head_mutex};
            std::optional<T> result;
            if(m_head != tail()) {
                auto old_head = m_head;
                result = std::move(m_head->value).value();
                m_head = m_head->next;
                delete old_head;
            }
            return result;
        }

        auto pop() -> T {
            std::unique_lock<std::mutex> head_lock{m_head_mutex};
            m_pushed.wait(head_lock, [&]{ return m_head != tail(); });

            auto old_head = m_head;
            auto const value = std::move(m_head->value).value();
            m_head = m_head->next;
            delete old_head;

            return value;
        }

        template<class T1>
        auto push(T1&& value) -> void {
            std::unique_lock<std::mutex> tail_lock{m_tail_mutex};

            auto const dummy_node = new Node{};
           
            m_tail->value = std::forward<T1>(value);
            m_tail->next  = dummy_node;
            m_tail = dummy_node;

            m_pushed.notify_one();
        }

    private:

        auto tail() -> Node* {
            std::unique_lock<std::mutex> lock{m_tail_mutex};
            return m_tail;
        }

    private:
        Node* m_head;
        Node* m_tail;

        std::mutex m_head_mutex;
        std::mutex m_tail_mutex;

        std::condition_variable m_pushed;
    };

    //
    // A simple lookup-table (hash-table) with global lock
    // Node: this would be simpler if I just used std::vector, but what the hell...
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
            std::unique_lock<std::mutex> lock{m_mutex};
            auto const bucket_head = find_bucket_head(key);
            bool new_node_inserted{false};
          
            auto const bucket_node = find_node(bucket_head, key);

            if(*bucket_node == nullptr) {
                *bucket_node = new Node{
                    std::forward<Key1>(key),
                    std::forward<Value1>(value)
                };
                new_node_inserted = true;
            }
            
            return new_node_inserted;
        }

        template<class Key1, class Value1>
        auto insert_or_update(Key1&& key, Value1&& value) -> void {
            auto const bucket_head = find_bucket_head(key);
            {
                std::unique_lock<std::mutex> lock{m_mutex};

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

        auto get(Key const& key) -> std::optional<Value> {
            std::unique_lock<std::mutex> lock{m_mutex};

            std::optional<Value> value;

            auto const bucket_head = find_bucket_head(key);
            auto const node = find_node(bucket_head, key);

            if(*node != nullptr) {
                value = (*node)->value;
            }
            
            return value;
        }

        auto remove(Key const& key) -> void {
            std::unique_lock<std::mutex> lock{m_mutex};

            auto const bucket_head = find_bucket_head(key);
            auto const node = find_node(bucket_head, key);
            
            if(*node != nullptr) {
                auto const curr_node = *node;
                *node = curr_node->next;
                delete curr_node;
            }
        }

        auto keys() -> std::vector<Key> {
            std::vector<Key> keys_;
            {
                std::unique_lock<std::mutex> lock{m_mutex};

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
            for(; *bucket_head ; bucket_head = &(*bucket_head)->next) {
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

    //
    // A simple lookup-table (hash-table) with global lock
    // Node: this would be simpler if I just used std::vector, but what the hell...
    //
    template<class Key, class Value>
        // requires Regular<Key> && Ordered<Key>
        // requires Hashable<Key>
        // requires SemiRegular<Value>
    class LookupTableFineGrained {
       
        struct Node {
            Key    key;
            Value  value;
            Node*  next{nullptr};
        };

        struct Bucket {
            Node* head{nullptr};
            std::mutex mutex;
        };

    public: // ctor, dtor, assign

        explicit LookupTableFineGrained(std::size_t const bucket_count = 128)
            : m_bucket_count{bucket_count}
        {
            m_buckets = new Bucket[bucket_count];
        }

        LookupTableFineGrained(LookupTableFineGrained const& ) = delete;

        LookupTableFineGrained(LookupTableFineGrained&& ) = delete;

        ~LookupTableFineGrained() {
            delete [] m_buckets;
        }

        LookupTableFineGrained& operator= (LookupTableFineGrained const& ) = delete;

        LookupTableFineGrained& operator= (LookupTableFineGrained&& ) = delete;

    public: // 

        auto bucket_count() const -> std::size_t { return m_bucket_count; }

        // returns true if inserted
        template<class Key1, class Value1>
        auto insert(Key1&& key, Value1&& value) -> bool {
            auto& bucket = find_bucket(key);
          
            auto const bucket_head = &bucket.head;

            bool new_node_inserted{false};
          
            auto const bucket_node = find_node(bucket_head, key);

            if(*bucket_node == nullptr) {
                *bucket_node = new Node{
                    std::forward<Key1>(key),
                    std::forward<Value1>(value)
                };
                new_node_inserted = true;
            }
            
            return new_node_inserted;
        }

        template<class Key1, class Value1>
        auto insert_or_update(Key1&& key, Value1&& value) -> void {
            auto& bucket = find_bucket(key);
            std::unique_lock<std::mutex> lock{bucket.mutex};

            auto const bucket_head = &bucket.head;
           
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

        auto get(Key const& key) -> std::optional<Value> {
            auto& bucket = find_bucket(key);
            std::unique_lock<std::mutex> lock{bucket.mutex};

            std::optional<Value> value;

            auto const bucket_head = &bucket.head;
            auto const node = find_node(bucket_head, key);

            if(*node != nullptr) {
                value = (*node)->value;
            }
            
            return value;
        }

        auto remove(Key const& key) -> void {
            auto& bucket = find_bucket(key);
            std::unique_lock<std::mutex> lock{bucket.mutex};

            auto const bucket_head = &bucket.head;
            auto const node = find_node(bucket_head, key);
            
            if(*node != nullptr) {
                auto const curr_node = *node;
                *node = curr_node->next;
                delete curr_node;
            }
        }

        auto keys() -> std::vector<Key> {
            std::vector<Key> keys_;
            
            for(std::size_t i{0}; i != m_bucket_count; ++ i) {
                std::unique_lock<std::mutex> lock{m_buckets[i].mutex};
                for(auto node = m_buckets[i].head; node; node = node->next) {
                    keys_.push_back(node->key);
                }
            }
            
            return keys_;
        }

    private: 

        auto index_of(Key const& key) -> std::size_t {
            return std::hash<Key>{}(key) % m_bucket_count;
        }

        auto find_bucket(Key const& key) -> Bucket& {
            return m_buckets[index_of(key)];
        }

        // returns node having given key, or place where new node is to be created
        auto find_node(Node** bucket_head, Key const& key) -> Node** {
            for(; *bucket_head ; bucket_head = &(*bucket_head)->next) {
                if( (*bucket_head)->key == key ) {
                    break;
                }
            }
            return bucket_head;
        }

    private:
        std::size_t  m_bucket_count;
        Bucket*      m_buckets;
    };

}

