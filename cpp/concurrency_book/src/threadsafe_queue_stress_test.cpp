#include "declarations.hpp"
#include "lock_based_data_structures.hpp"
#include "std.hpp"

//
//
//
template<template<class> class Queue>
auto test_queue( int const producer_count, 
                 int const items_per_producer,
                 int const consumer_count
               )
{
    using ProducerId = int;
    using Item       = int;
    using Value      = std::pair<ProducerId, Item>;
    using MsgQueue   = Queue<Value>;

    auto get_pid  = [](Value const value) { return value.first; };
    //auto get_item = [](Value const value) {return value.second; }; // not used 

    //
    std::vector<Value> expected;
    std::thread expected_builder{[&]{
        expected.reserve(producer_count*items_per_producer);
        for(ProducerId pid{0}; pid != producer_count; ++ pid) {
            for(Item item{0}; item != items_per_producer; ++item) {
                expected.push_back(Value{pid, item});
            }
        }
    }};

    //
    auto producer_fn = [](MsgQueue& queue, ProducerId const pid, int const items_per_producer) {
        return [&queue, pid, items_per_producer]{
            for(Item item{0}; item != items_per_producer; ++ item) {
                queue.push(Value{pid, item});
                //std::this_thread::sleep_for(1ms);
            }
        };
    };

    auto consumer_fn = [&](MsgQueue& queue, std::vector<Value>& values) {
        return [&values, &queue, get_pid] {
            for(;;) {
                auto const value = queue.pop();
                if(get_pid(value) == -1) break;
                values.push_back(value);
                //std::this_thread::sleep_for(1ms);
            }
        };
    };

    //
    MsgQueue queue;

    // start threads
    std::vector<std::thread> producers;

    for(ProducerId pid{0}; pid != producer_count; ++ pid) {
        producers.emplace_back(producer_fn(queue, pid, items_per_producer));
    }

    std::vector<std::thread> consumers;
    std::vector<std::vector<Value>> deques_of_values(consumer_count);
    
    for(std::size_t i{0}; i != consumer_count; ++ i) {
        consumers.emplace_back(consumer_fn(queue, deques_of_values[i]));
    }

    // wait for all 
    for(auto& p: producers) p.join();

    for(int i{0}; i != consumer_count; ++ i) {
        std::this_thread::sleep_for(1ms); // why is this needed?
        queue.push(Value{-1, 0});
    }

    for(auto& c: consumers) c.join();
    
    // prepare values for testing

    //
    std::vector<Value> values;
    values.reserve(producer_count*items_per_producer);

    for(auto& d: deques_of_values) values.insert(values.end(), d.begin(), d.end());
    deques_of_values.clear();

    std::sort(values.begin(), values.end());

    // test
    expected_builder.join();
    if(expected != values)
        throw std::runtime_error(
            "wrong values"
        );
}

template<template<class> class Queue>
auto test_queue() -> void {
    std::random_device producer_count_random_device;
    std::uniform_int_distribution<> producer_count_dist{1, 100};

    std::random_device items_per_producer_random_device;
    std::uniform_int_distribution<> items_per_producer_count_dist{5, 10};

    std::random_device consumer_count_random_device;
    std::uniform_int_distribution<> consumer_count_dist{1, 100};

    std::cout 
        << std::setw(10) << "test no" << " | "
        << std::setw(20) << "producer count" << " | "
        << std::setw(20) << "items per producer" << " | "
        << std::setw(20) << "consumer count" << " | "
        << "\n" ;

    constexpr int const TEST_COUNT = 10000;

    int test_passed{0};
    
    std::vector<std::tuple<int, int, int>> failed_inputs;


    for(int i{0}; i != TEST_COUNT; ++ i) {
        auto const producer_count     = producer_count_dist(producer_count_random_device);
        auto const items_per_producer = 1 << items_per_producer_count_dist(items_per_producer_random_device);
        auto const consumer_count     = consumer_count_dist(consumer_count_random_device);

        std::cout 
            << std::setw(10) << i << " | " 
            << std::setw(20) << producer_count << " | "
            << std::setw(20) << items_per_producer << " | "
            << std::setw(20) << consumer_count << " | "
            << "\n" ;

        try {
            test_queue<Queue>(producer_count, items_per_producer, consumer_count);
            ++ test_passed;
        } catch(std::exception const& ) {
            failed_inputs.emplace_back(producer_count, items_per_producer, consumer_count);
            std::cout << " test failed\n";
        }
    }

    std::cout 
        << test_passed << " of " << TEST_COUNT << " passed\n";

    if(test_passed != TEST_COUNT) {
        std::cout << "failed:\n";

        for(auto const& failed: failed_inputs) {
            std::cout << "   " 
                << std::setw(8) << std::get<0>(failed) << ", " 
                << std::setw(8) << std::get<1>(failed) << ", " 
                << std::setw(8) << std::get<2>(failed) << "\n";
        }
    }
}

//
//
//
auto thread_safe_queue_stress_test_global_lock() -> void {
    test_queue<threadsafe::QueueGlobalLock>();
}

auto thread_safe_queue_stress_test_fine_grained() -> void {
    test_queue<threadsafe::QueueFineGrained>();
}
