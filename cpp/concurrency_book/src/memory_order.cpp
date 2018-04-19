#include "declarations.hpp"
#include "std.hpp"

static auto to_c_str(std::memory_order const mem_order) -> char const* {
    switch(mem_order) {
    case std::memory_order_seq_cst: return "memory_order_seq_cst";
    case std::memory_order_relaxed: return "memory_order_relaxed";
    case std::memory_order_acq_rel: return "memory_order_acq_rel";
    case std::memory_order_acquire: return "memory_order_acquire";
    case std::memory_order_consume: return "memory_order_consume";
    case std::memory_order_release: return "memory_order_release";
    }
    return "unknown";
}

static auto sequential_consistency() -> int {
    //
    std::atomic<bool> x{false}, y{false};
    std::atomic<int> z{0};

    //
    auto write_x = [&]{ x.store(true, std::memory_order_seq_cst); };
    auto write_y = [&]{ y.store(true, std::memory_order_seq_cst); };
    
    auto read_x_then_y = [&]{ 
        while(!x.load(std::memory_order_seq_cst)){}
        if(y.load(std::memory_order_seq_cst)){
            ++z;
        }
    };

    auto read_y_then_x = [&]{ 
        while(!y.load(std::memory_order_seq_cst)){}
        if(x.load(std::memory_order_seq_cst)){
            ++z;
        }
    };

    //
    std::vector<std::thread> threads;
    threads.reserve(4);
    threads.emplace_back(write_x);
    threads.emplace_back(write_y);
    threads.emplace_back(read_x_then_y);
    threads.emplace_back(read_y_then_x);
    for(auto& t: threads) t.join();

    return z.load();
}

static auto test_memory_order(std::memory_order const load, std::memory_order const store) -> void {
    constexpr int LOOP_COUNT = 10;

    //
    struct read_values {
        int x, y, z;
    };

    //
    std::atomic<int> x{0}, y{0}, z{0};
    std::atomic<bool> go{false};

    read_values values[5][LOOP_COUNT];

    //
    auto inc_fn = [&](std::atomic<int>* var_to_inc, read_values* values) {
        while(!go) std::this_thread::yield();
        for(int i = 0; i != LOOP_COUNT; ++ i) {
            values[i].x = x.load(load);
            values[i].y = y.load(load);
            values[i].z = z.load(load);
            var_to_inc->store(i + 1, store);
            std::this_thread::yield();
        }
    };

    auto read_val_fn = [&](read_values* values) {
        while(!go) std::this_thread::yield();
        for(int i = 0; i != LOOP_COUNT; ++ i) {
            values[i].x = x.load(load);
            values[i].y = y.load(load);
            values[i].z = z.load(load);
            std::this_thread::yield();
        }
    };

    auto print = [&](read_values* values) {
        for(int i = 0; i != LOOP_COUNT; ++ i) {
            std::cout << "(" 
                << std::setw(3) << values[i].x << ", " 
                << std::setw(3) << values[i].y << ", " 
                << std::setw(3) << values[i].z 
                << ") ";
        }
        std::cout << "\n";
    };

    //
    std::vector<std::thread> threads;
    threads.reserve(5);
    threads.emplace_back(std::bind(     inc_fn, &x, values[0]));
    threads.emplace_back(std::bind(     inc_fn, &y, values[1]));
    threads.emplace_back(std::bind(     inc_fn, &z, values[2]));
    threads.emplace_back(std::bind(read_val_fn,     values[3]));
    threads.emplace_back(std::bind(read_val_fn,     values[4]));

    go = true;
    
    for(auto& t: threads) t.join();

    std::cout << "uses: load = " << to_c_str(load) << ", store = " << to_c_str(store) << "\n";
    for(int i = 0; i != 5; ++ i) {
        print(values[i]);
    }
    std::cout << "\n";
}

auto memory_orders() -> void {
    // sequential consistency
    std::cout << "seq_cst = " << sequential_consistency() << ", should be 2\n\n";
    
    // relaxed order
    test_memory_order(std::memory_order_relaxed, std::memory_order_relaxed);
    test_memory_order(std::memory_order_acq_rel, std::memory_order_acq_rel);
    test_memory_order(std::memory_order_acquire, std::memory_order_release);
    test_memory_order(std::memory_order_seq_cst, std::memory_order_seq_cst);
}
