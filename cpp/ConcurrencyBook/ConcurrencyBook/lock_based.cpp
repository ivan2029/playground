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
auto lock_based_global_lock_lookup_table() -> void {
    threadsafe::LookupTableGlobalLock<int, std::string> lookup_table;


    NOT_YET_IMPLEMENTED();
}

auto lock_based_fine_grained_lookup_table() -> void {
    NOT_YET_IMPLEMENTED();
}


