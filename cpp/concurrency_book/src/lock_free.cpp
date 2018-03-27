#include "declarations.hpp"
#include "lock_free_data_structures.hpp"

using ProducerId = int;
using Item = int;
using Value = std::pair<ProducerId, Item>;

inline auto producer_id (Value const& v) -> ProducerId { return v.first;  }
inline auto item        (Value const& v) -> ProducerId { return v.second; }

auto lock_free_stack() -> void {
    //
    using Stack = threadsafe::StackLockFree<Value>;
    
    //
    constexpr const int PRODUCER_COUNT = 5;
    constexpr const int ITEMS_PER_PRODUCER = 100;

    //
    auto producer_fn = [=](Stack& stack, ProducerId const pid) {
        return [=, &stack] {
            for(Item item_{0}; item_ != ITEMS_PER_PRODUCER; ++ item_) {
                stack.push(Value{pid, item_});
            }
        };
    };

    // just one!
    auto consumer_fn = [=](Stack& stack, std::vector<Value>& values) {
        return [=, &stack, &values] {
            for(;;) {
                auto const value = stack.try_pop();
                if(value) {
                    if(producer_id(*value) == -1) return;
                    values.push_back(*value);
                }
            }
        };
    };

    //
    Stack stack;
    std::vector<Value> values;

    //
    std::vector<std::thread> producers;
    for(ProducerId pid{0}; pid != PRODUCER_COUNT; ++ pid) {
        producers.emplace_back(producer_fn(stack, pid));
    }
    std::thread consumer{consumer_fn(stack, values)};

    //
    for(auto& p: producers) p.join();

    stack.push(Value{-1, 0});
    consumer.join();

    //
    auto const expected = [=] {
        std::vector<Value> values;
        for(ProducerId pid{0}; pid != PRODUCER_COUNT; ++ pid) {
            for(Item item_{0}; item_ != ITEMS_PER_PRODUCER; ++ item_) {
                values.push_back(Value{pid, item_});
            }
        }
        return values;
    }();

    //std::sort(values.begin(), values.end());

    //assert(expected == values);

    //for(std::size_t i{0}; i != std::min(expected.size(), values.size()); ++ i) {
    //    std::cout 
    //        << std::setw(4) << producer_id(expected[i]) << " " << std::setw(4) << item(expected[i]) << " -> "
    //        << std::setw(4) << producer_id(values[i])   << " " << std::setw(4) << item(values[i])   << "\n"
    //        ;
    //}

    std::cout << expected.size() << " " << values.size() << "\n";
}