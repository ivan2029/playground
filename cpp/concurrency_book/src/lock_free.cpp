#include "declarations.hpp"
#include "lock_free_data_structures.hpp"
#include "std.hpp"

auto lock_free_bounded_spsc_queue() -> void {
    using Item = int;
    using Cont = threadsafe::Bounded_SPSC_Queue<Item>;

    std::size_t const ITEM_COUNT = 1000;

    //
    auto const producer_fn = [=](Cont& cont) {
        return [=, &cont] {
            for(Item item{0}; item != ITEM_COUNT; ++ item) {
                while( !cont.try_push(item) ){
                    //std::this_thread::sleep_for(1ms);
                }
            }
        };
    };

    auto const consumer_fn = [=](Cont& cont, std::vector<Item>& out) {
        return [=, &cont, &out] {
            while(out.size() != ITEM_COUNT) {
                auto oi = cont.try_pop();
                if(oi) {
                    out.push_back(*oi);
                }
                //std::this_thread::sleep_for(1ms);
            }
        };
    };

    //
    Cont              cont;
    std::vector<Item> actual;

    //
    std::thread producer{producer_fn(cont)};
    std::thread consumer{consumer_fn(cont, actual)};

    producer.join();
    consumer.join();

    //
    assert(actual.size() == ITEM_COUNT);
    assert(std::is_sorted(actual.begin(), actual.end()));
}
