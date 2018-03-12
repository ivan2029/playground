#include "declarations.hpp"
#include "data_structures.hpp"


//
//
//
using ProducerId = int;
using Item       = int;
using Value      = std::pair<ProducerId, Item>;

constexpr int PRODUCER_COUNT     = 1 << 6;
constexpr int ITEMS_PER_PRODUCER = 1 << 15;

//
//
//
auto are_all_values_in(std::deque<Value> values) -> bool {
    //
    auto const expected = [=]{
        std::deque<Value> expected;
        for(ProducerId pid{0}; pid != PRODUCER_COUNT; ++pid) {
            for(Item item{0}; item != ITEMS_PER_PRODUCER; ++item) {
                expected.push_back(Value{pid, item});
            }
        }
        return expected;
    }();

    //
    std::sort(values.begin(), values.end());

    //
    return expected == values;
}

//
//
//
auto lock_based_global_lock_queue_try_pop() -> void {
    //
    threadsafe::QueueGlobalLock<Value> queue;
   
    auto producer_fn = [&](ProducerId const pid){
        return [&, pid]{
            for(Item i{0}; i < ITEMS_PER_PRODUCER; ++ i) {
                queue.push(Value{pid, i});
            }
        };
    };

    // only one consumer
    std::deque<Value> values;
    auto consumer_fn = [&]{
        for(;;) {
            auto value = queue.try_pop();
            if(value) {
                if(value->first == -1) return;
                values.push_back(*value);
            }
        }
    };

    // run
    auto const time = run_and_time([&]{
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
    });

    std::cout << "lock-based queue, global lock, try_pop " 
        << std::chrono::duration_cast<std::chrono::milliseconds>(time).count() << "ms"
        << "\n";

    // run tests
    if( ! are_all_values_in(std::move(values)) ) {
        throw std::runtime_error("not all values are in");
    }
}

auto lock_based_global_lock_queue_pop() -> void {
    //
    threadsafe::QueueGlobalLock<Value> queue;
   
    auto producer_fn = [&](ProducerId const pid){
        return [&, pid]{
            for(Item i{0}; i < ITEMS_PER_PRODUCER; ++ i) {
                queue.push(Value{pid, i});
            }
        };
    };

    // only one consumer
    std::deque<Value> values;
    auto consumer_fn = [&]{
        for(;;) {
            auto value = queue.pop();
            if(value.first == -1) return;
            values.push_back(value);
        }
    };

    // run
    auto const time = run_and_time([&]{
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
    });

    std::cout << "lock-based queue, global lock,     pop " 
        << std::chrono::duration_cast<std::chrono::milliseconds>(time).count() << "ms"
        << "\n";

    // run tests
    if( ! are_all_values_in(std::move(values)) ) {
        throw std::runtime_error("not all values are in");
    }
}

//
//
//
auto lock_based_fine_grained_queue_try_pop() -> void {
    //
    threadsafe::QueueFineGrained<Value> queue;
   
    auto producer_fn = [&](ProducerId const pid){
        return [&, pid]{
            for(Item i{0}; i < ITEMS_PER_PRODUCER; ++ i) {
                queue.push(Value{pid, i});
            }
        };
    };

    // only one consumer
    std::deque<Value> values;
    auto consumer_fn = [&]{
        for(;;) {
            auto value = queue.try_pop();
            if(value) {
                if(value->first == -1) return;
                values.push_back(*value);
            }
        }
    };

    // run
    auto const time = run_and_time([&]{
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
    });

    std::cout << "lock-based queue, fine grained, try_pop " 
        << std::chrono::duration_cast<std::chrono::milliseconds>(time).count() << "ms"
        << "\n";

    // run tests
    if( ! are_all_values_in(std::move(values)) ) {
        throw std::runtime_error("not all values are in");
    }
}

auto lock_based_fine_grained_queue_pop() -> void {
    //
    threadsafe::QueueFineGrained<Value> queue;
   
    auto producer_fn = [&](ProducerId const pid){
        return [&, pid]{
            for(Item i{0}; i < ITEMS_PER_PRODUCER; ++ i) {
                queue.push(Value{pid, i});
            }
        };
    };

    // only one consumer
    std::deque<Value> values;
    auto consumer_fn = [&]{
        for(;;) {
            auto value = queue.pop();
            if(value.first == -1) return;
            values.push_back(value);
        }
    };

    // run
    auto const time = run_and_time([&]{
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
    });

    std::cout << "lock-based queue, fine grained,     pop " 
        << std::chrono::duration_cast<std::chrono::milliseconds>(time).count() << "ms"
        << "\n";

    // run tests
    if( ! are_all_values_in(std::move(values)) ) {
        throw std::runtime_error("not all values are in");
    }
}

//
//
//
template<template<class, class> class LookupTable>
auto test_lookup_table(bool const do_comparison) -> void {
    using Key   = int;
    using Value = std::string;
    using Table = LookupTable<Key, Value>;
    using StdTable = std::map<Key, Value>;

    constexpr int const WORKER_COUNT = 10;
    constexpr int const ITEMS_PER_WORKER = 1000000;

    //
    auto worker_fn = [=](Table& table, StdTable& produced_table, std::mutex& produced_table_mutex) {
        return [&, ITEMS_PER_WORKER] {
            std::random_device random_device_;
            std::uniform_int_distribution<> dist{1, 100};

            for(int i = 0; i < ITEMS_PER_WORKER; ++ i) {
                auto const number = dist(random_device_);
                if(i % 3 == 0) {
                    table.remove(number);

                    if(do_comparison) {
                        std::unique_lock<std::mutex> lock{produced_table_mutex};
                        produced_table.erase(number);
                    }
                } 
                else {
                    table.insert_or_update(number, std::to_string(number));

                    if(do_comparison) {
                        std::unique_lock<std::mutex> lock{produced_table_mutex};
                        produced_table[number] = std::to_string(number);
                    }
                }
            }
        };
    };

    // tables
    StdTable   produced_table;
    std::mutex produced_table_mutex;

    Table    table;

    // start and join workers
    std::vector<std::thread> workers;
    for(int i{0}; i != WORKER_COUNT; ++ i) {
        workers.emplace_back(worker_fn(table, produced_table, produced_table_mutex));
    }

    for(auto& w: workers) w.join();

    // join all produced maps into one map
    if(do_comparison) {
        for(auto& kv: produced_table) {
            auto opt = table.get(kv.first);
            if(!opt) throw std::runtime_error("#2");
            if(*opt != kv.second) throw std::runtime_error("#3");
            //std::cout << std::setw(10) << kv.first << " -> " << std::setw(10) << kv.second << "\n";
        }
    }
}

constexpr const bool DO_COMPARISON = false;

auto lock_based_global_lock_lookup_table() -> void {
    using namespace std::chrono;
    auto const time = run_and_time([]{
        test_lookup_table<threadsafe::LookupTableGlobalLock>(DO_COMPARISON);
    });
    std::cout << "lookup table, global lock, took: " << duration_cast<milliseconds>(time).count() << "ms\n";
}

auto lock_based_fine_grained_lookup_table() -> void {
    using namespace std::chrono;
    auto const time = run_and_time([]{
        test_lookup_table<threadsafe::LookupTableFineGrained>(DO_COMPARISON);
    });
    std::cout << "lookup table, fine grained, took: " << duration_cast<milliseconds>(time).count() << "ms\n";
}


