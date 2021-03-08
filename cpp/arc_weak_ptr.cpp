#include <cassert>
#include <atomic>
#include <type_traits>
#include <cstddef>
#include <initializer_list>
#include <new>
#include <utility>
#include <tuple>

//
//
//
namespace ptr {

    template<class T> class arc;
    template<class T> class weak;

    /*
     *
     */

    using __destroy_fn = void (*) (void*);

    struct __shared_state_base {
        virtual ~__shared_state_base() = default;
    
        std::atomic<std::size_t> strong_count{1};
        std::atomic<std::size_t> weak_count{0};
        __destroy_fn             destroy = nullptr;
        void*                    raw     = nullptr;
    };

    template<class T>
    struct __shared_state : public __shared_state_base {
        alignas(T) std::byte store[sizeof(T)];
    };

    /**
     *
     */
    struct in_place_t {} in_place;

    /**
     *
     */
    template<class T>
    class arc {
    public: // typedefs

        using element_type = T;
        using weak_type = weak<T>;

        template<class R>
        friend class arc;

        template<class R>
        friend class weak;
    
    public: // ctor

        template<class ... Args>
        arc(in_place_t, Args&& ... args);

        template<class V, class ... Args>
        arc(
            in_place_t,
            std::initializer_list<V> init_list,
            Args&& ... args
        );

        template<class R>
            requires std::convertible_to<R*, T*>
        arc(arc<R> other);
    
        arc();
        arc(arc const& other);
        arc(arc&& other);
        ~arc();

        auto operator= (arc const& other) -> arc&;
        auto operator= (arc&& other) -> arc&;

        template<class R>
            requires std::convertible_to<R*, T*>
        auto operator= (arc<R> other) -> arc<T>&;

    private: // to construct from weak

        arc(T* raw, __shared_state_base* state);
    
    public: // comparison

        template<class R>
        friend auto operator== (arc<R> const& a, arc<R> const& b) -> bool;

        template<class R>
        friend auto operator!= (arc<R> const& a, arc<R> const& b) -> bool;
    
    public: // modifiers

        auto reset() -> void;
        auto swap(arc& other) -> void;

    public: // observers

        auto get() const -> element_type*;
        auto operator* () const -> element_type&;
        auto operator-> () const -> element_type*;

        auto use_count() const -> std::size_t;
    
        operator bool () const;

    private:
        T*                   m_raw          = nullptr;
        __shared_state_base* m_shared_state = nullptr;
    };

    template<class T>
    arc(in_place_t, T) -> arc<T>; 


    /**
     *
     */
    template<class T>
    class weak {
    public: // typedefs
        using element_type = T;
        using shared_type = arc<T>;
    
    public: // ctor

        template<class R>
            requires std::convertible_to<R*, T*>
        weak(arc<R> const& shared);

        weak();
        weak(weak const& other);
        weak(weak&& other);
        ~weak();

        auto operator= (weak const& other) -> weak&;
        auto operator= (weak&& other) -> weak&;

        template<class R>
            requires std::convertible_to<R*, T*>
        auto operator= (arc<R> const& shared) -> weak<T>&;

    public: // modifiers

        auto reset() -> void;
        auto swap(weak& other) -> void;

    public: // observers
    
        auto use_count() const -> std::size_t;
        auto expired() const -> bool;
        auto lock() const -> shared_type;

    private:

        auto init(T* raw, __shared_state_base* shared_state) -> void;
    
    private:
        T*                   m_raw          = nullptr;
        __shared_state_base* m_shared_state = nullptr;
    };

} // namespace ptr

//
// __shared_state impl
//
namespace ptr {

    template<class T>
    auto __ptr_to_store(__shared_state<T>* state) -> T* {
        auto ptr = &state->store[0];
        return reinterpret_cast<T*>(ptr);
    }

    template<class T, class ... Args>
    auto __construct(Args&& ... args) -> std::pair<__shared_state_base*, T*> {
        auto state = new __shared_state<T>{};

        auto ptr = __ptr_to_store(state);

        state->raw = ptr;
    
        state->destroy = [](void* vptr) -> void {
            auto ptr = reinterpret_cast<T*>(vptr);
            ptr->~T();
        };
    
        new (ptr) T{std::forward<Args>(args)...};
    
        return std::pair{state, ptr};
    }

    template<class T, class V, class ... Args>
    auto __construct(
        std::initializer_list<V> init_list,
        Args&& ... args
    ) -> std::pair<__shared_state_base*, T*> {
        auto state = new __shared_state<T>{};

        auto ptr = __ptr_to_store(state);

        state->raw = ptr;
    
        state->destroy = [](void* vptr) -> void {
            auto ptr = reinterpret_cast<T*>(vptr);
            ptr->~T();
        };
    
        new (ptr) T{init_list, std::forward<Args>(args)...};
    
        return std::pair{state, ptr};   
    }

    inline
    auto __add_strong_ref(__shared_state_base* state) -> void {
        assert(state);
        ++ state->strong_count;
    }

    inline
    auto __add_weak_ref(__shared_state_base* state) -> void {
        assert(state);
        ++ state->weak_count;
    }

    inline
    auto __remove_strong_ref(__shared_state_base* state) -> void {
        assert(state);

        auto const count = -- state->strong_count;

        if(count == 0) {
            state->destroy(state->raw);
            if(state->weak_count.load() == 0) {
                delete state;
            }
        }
    }

    inline
    auto __remove_weak_ref(__shared_state_base* state) -> void {
        assert(state);

        auto const count = -- state->weak_count;
        if(count == 0 && state->strong_count.load() == 0) {
            delete state;
        }
    }

} // namespace ptr

//
// arc impl
//
namespace ptr {

    template<class T>
    template<class ... Args>
    arc<T>::arc(in_place_t, Args&& ... args)
    {
        std::tie(m_shared_state, m_raw) = __construct<T>(std::forward<Args>(args)...);
    }

    template<class T>
    template<class V, class ... Args>
    arc<T>::arc(
        in_place_t,
        std::initializer_list<V> init_list,
        Args&& ... args
    ) 
    {
        std::tie(m_shared_state, m_raw) = __construct<T>(init_list, std::forward<Args>(args)...);
    }

    template<class T>
    template<class R>
        requires std::convertible_to<R*, T*>
    arc<T>::arc(arc<R> other)
        : m_raw{static_cast<T*>(other.m_raw)}
        , m_shared_state{other.m_shared_state}
    {
        if(m_shared_state) __add_strong_ref(m_shared_state);
    }
    
    template<class T>
    arc<T>::arc()
    {}

    template<class T>
    arc<T>::arc(arc<T> const& other)
        : m_raw{other.m_raw}
        , m_shared_state{other.m_shared_state}
    {
        if(m_shared_state) __add_strong_ref(m_shared_state);
    }

    template<class T>
    arc<T>::arc(arc<T>&& other)
        : m_raw{other.m_raw}
        , m_shared_state{other.m_shared_state}      
    {
        other.m_raw = nullptr;
        other.m_shared_state = nullptr;
    }

    template<class T>
    arc<T>::~arc() {
        reset();
    }

    template<class T>
    auto arc<T>::operator= (arc<T> const& other) -> arc<T>& {
        if(this != &other) {
            reset();
            m_raw = other.m_raw;
            m_shared_state = other.m_shared_state;
            if(m_shared_state) __add_strong_ref(m_shared_state);
        }
        return *this;
    }

    template<class T>
    auto arc<T>::operator= (arc<T>&& other) -> arc<T>& {
        reset();
        m_raw = other.m_raw;
        m_shared_state = other.m_shared_state;
        other.m_raw = nullptr;
        other.m_shared_state = nullptr;
        return *this;
    }

    template<class T>
    template<class R>
        requires std::convertible_to<R*, T*>
    auto arc<T>::operator= (arc<R> other) -> arc<T>& {
        if(this != &other) {
            reset();
            m_raw = other.m_raw;
            m_shared_state = other.m_shared_state;
            if(m_shared_state) __add_strong_ref(m_shared_state);
        }
        return *this;
    }

    template<class T>
    arc<T>::arc(T* raw, __shared_state_base* shared_state)
        : m_raw{raw}
        , m_shared_state{shared_state}
    {
        if(m_shared_state) {
            __add_strong_ref(m_shared_state);
        }
    }
    
    template<class T>
    auto arc<T>::reset() -> void {
        if(m_raw && m_shared_state) __remove_strong_ref(m_shared_state);
        m_raw = nullptr;
        m_shared_state = nullptr;
    }

    template<class T>
    auto arc<T>::swap(arc<T>& other) -> void {
        std::swap(m_raw, other.m_raw);
        std::swap(m_shared_state, other.m_shared_state);
    }

    template<class T>
    auto arc<T>::get() const -> arc<T>::element_type* {
        return m_raw;
    }

    template<class T>
    auto arc<T>::operator* () const -> arc<T>::element_type& {
        assert( m_raw );
        return *m_raw;
    }

    template<class T>
    auto arc<T>::operator-> () const -> arc<T>::element_type* {
        assert( m_raw );
        return m_raw;
    }

    template<class T>
    auto arc<T>::use_count() const -> std::size_t {
        if(m_shared_state) return m_shared_state->strong_count.load();
        else               return 0;
    }

    template<class T>
    arc<T>::operator bool () const {
        return m_raw != nullptr;
    }

    template<class T>
    auto operator== (arc<T> const& a, arc<T> const& b) -> bool {
        return a.m_shared_state == b.m_shared_state;
    }

    template<class T>
    auto operator!= (arc<T> const& a, arc<T> const& b) -> bool {
        return !(a == b);
    }
    
} // namespace ptr

//
// weak impl
//
namespace ptr {


    template<class T>
    template<class R>
        requires std::convertible_to<R*, T*>
    weak<T>::weak(arc<R> const& shared) {
        init(shared.m_raw, shared.m_shared_state);
    }

    template<class T>
    weak<T>::weak()
    {}

    template<class T>
    weak<T>::weak(weak<T> const& other) {
        init(other.m_raw, other.m_shared_state);
    }

    template<class T>
    weak<T>::weak(weak<T>&& other)
        : m_raw{other.m_raw}
        , m_shared_state{other.m_shared_state}
    {
        other.m_raw = nullptr;
        other.m_shared_state = nullptr;
    }

    template<class T>
    weak<T>::~weak() {
        reset();
    }

    template<class T>
    auto weak<T>::operator= (weak<T> const& other) -> weak<T>& {
        if(this != &other) {
            reset();
            init(other.m_raw, other.m_shared_state);
        }
        return *this;
    }

    template<class T>
    auto weak<T>::operator= (weak<T>&& other) -> weak<T>& {
        reset();
        
        m_raw = other.m_raw;
        m_shared_state = other.m_shared_state;
    
        other.m_raw = nullptr;
        other.m_shared_state = nullptr;
    }

    template<class T>
    template<class R>
        requires std::convertible_to<R*, T*>
    auto weak<T>::operator= (arc<R> const& shared) -> weak<T>& {
        reset();
        init(shared.m_raw, shared.m_shared_state);
        return *this;
    }

    template<class T>
    auto weak<T>::reset() -> void {
        if(m_raw && m_shared_state) {
            __remove_weak_ref(m_shared_state);
        }
        m_raw = nullptr;
        m_shared_state = nullptr;
    }

    template<class T>
    auto weak<T>::swap(weak<T>& other) -> void {
        std::swap(m_raw, other.m_raw);
        std::swap(m_shared_state, other.m_shared_state);
    }

    template<class T>
    auto weak<T>::use_count() const -> std::size_t {
        if(m_raw && m_shared_state) {
            return m_shared_state->strong_count.load();
        }
        else {
            return 0;
        }
    }

    template<class T>
    auto weak<T>::expired() const -> bool {
        return use_count() == 0;
    }
    
    template<class T>
    auto weak<T>::lock() const -> weak<T>::shared_type {
        return arc<T>{m_raw, m_shared_state};
    }

    template<class T>
    auto weak<T>::init(T* raw, __shared_state_base* shared_state) -> void {
        assert( m_raw == nullptr );
        assert( m_shared_state == nullptr );
        
        m_raw = raw;
        m_shared_state = shared_state;
        if(m_raw && m_shared_state) {
            __add_weak_ref(m_shared_state);
        }
    }
    
} // namespace ptr


//
//
//

#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <thread>
#include <chrono>
#include <random>

using namespace std::string_literals;
using namespace std::chrono_literals;

auto sleep_some() -> void {
    using namespace std::chrono;

    thread_local std::random_device rd;
    thread_local std::uniform_int_distribution<> dist{10, 100};

    std::this_thread::sleep_for( milliseconds{ dist(rd) } );
}

auto test_arc_ctor_dtor() -> void {
    struct destructed {
        bool* m_value = nullptr;

        destructed(bool* value)
            : m_value{value}
        {}
        
        ~destructed() {
            assert(m_value);
            *m_value = true;
        }
    };

    bool is_destructed = false;

    {
        ptr::arc<destructed> x{ptr::in_place, &is_destructed};
    }
    
    assert(is_destructed);
}

auto test_arc_and_its_copy_imply_use_count_of_2() -> void {
    ptr::arc a{ptr::in_place, 42};
    auto     b = a;

    assert(a.use_count() == 2);
    assert(b.use_count() == 2);
}

auto test_moved_arc_keeps_total_use_count_of_1() -> void {
    ptr::arc      a{ptr::in_place, 42};
    ptr::arc<int> b;

    assert(a.use_count() == 1);
    assert(b.use_count() == 0);

    b = std::move(a);

    assert(a.use_count() == 0);
    assert(b.use_count() == 1);
}

auto test_arc_with_initializer_list() -> void {
    std::vector expected{1, 2, 3, 4};
    
    ptr::arc< std::vector<int> > a{ptr::in_place, {1, 2, 3, 4}};
    auto b = a;

    assert( *a == expected );
    assert( *b == expected );
}

auto test_copied_arcs_point_to_same_thing() -> void {
    ptr::arc a{ptr::in_place, 42};
    auto     b = a;

    assert( a == b );
    assert( a.get() == b.get() );
    assert( *a == *b );
}

auto test_arc_convertible_to_bool() -> void {
    ptr::arc      a{ptr::in_place, 42};
    ptr::arc<int> b;

    assert( static_cast<bool>(a) == true  );
    assert( static_cast<bool>(b) == false );
}

auto test_arc_reset_sets_arc_to_null() -> void {
    ptr::arc a{ptr::in_place, 42};
    auto     b = a;

    a.reset();

    assert( !a );
    assert( b );
}

auto test_arc_swap() -> void {
    ptr::arc a{ptr::in_place,  42};
    ptr::arc b{ptr::in_place, -42};

    a.swap(b);

    assert( *a == -42 );
    assert( *b == 42 );
}

auto test_arc_swap_manual() -> void {
    ptr::arc a{ptr::in_place,  42};
    ptr::arc b{ptr::in_place, -42};
    
    auto tmp = a;
    a = std::move(b);
    b = std::move(tmp);

    assert( *a == -42 );
    assert( *b ==  42 );
}

auto test_arc_inheritance() -> void {
    //
    struct iface {
        virtual ~iface() = default;
        virtual auto method() -> void = 0;
    };

    //
    struct impl : public iface {
        explicit impl(bool* is_destructed_)
            : m_is_destructed{is_destructed_}
        {}

        ~impl() {
            *m_is_destructed = true;
        }
    
        // iface
        auto method() -> void override {}

        //
        bool* m_is_destructed;
    };

    //
    bool is_destructed = false;
    
    {
        ptr::arc<impl>  a{ptr::in_place, &is_destructed};
        ptr::arc<iface> b = a;

        assert( a.use_count() == 2 );
        assert( b.use_count() == 2 );

        a.reset();

        assert( b.use_count() == 1 );
    }

    assert( is_destructed );
}

auto test_arc_multithreaded_one_arc_shared() -> void {
    ptr::arc<int> a{ptr::in_place, 42};

    {
        int const worker_count = 1000;
    
        std::vector<std::jthread> workers;
        workers.reserve(worker_count);

        for(int i = 0; i < worker_count; ++ i) {
            workers.emplace_back([a] () mutable {
                using namespace std::chrono;
                auto const begin = high_resolution_clock::now();

                ptr::arc<int> b;
                
                for(;;) {
                    sleep_some();

                    //
                    auto const now = high_resolution_clock::now();
                    if( now - begin > 5s) {
                        break;
                    }

                    //
                    auto t = a;
                    a = b;
                    b = t;
                }
            });
        }
    
    }
    
    assert( a.use_count() == 1 );
    assert( *a == 42 );
}

auto test_arc_multithreaded_many_arc_shared() -> void {
    constexpr std::size_t const shared_count = 1'000;
    // init vector of shareds
    std::vector< ptr::arc<std::size_t> > xs;

    xs.reserve(shared_count);
    for(std::size_t i = 0; i < shared_count; ++i) {
        xs.emplace_back(ptr::in_place, i);
    }

    // run workers
    {
        int const worker_count = 1000;
    
        std::vector<std::jthread> workers;
        workers.reserve(worker_count);

        for(int i = 0; i < worker_count; ++ i) {
            workers.emplace_back([xs] () mutable {
                using namespace std::chrono;
                auto const begin = high_resolution_clock::now();
                
                for(;;) {
                    sleep_some();

                    //
                    auto const now = high_resolution_clock::now();
                    if( now - begin > 5s) {
                        break;
                    }

                    //
                    std::rotate(
                        xs.begin(),
                        xs.begin() + xs.size() / 3,
                        xs.end()
                    );
                }
            });
        }
    
    }
    
    // make sure every shared has exactly one count
    for(std::size_t i = 0; i < xs.size(); ++ i) {
        assert( xs[i].use_count() == 1 );
        assert( *xs[i] == i );
    }
}

auto test_weak_ctor_dtor() -> void {
    ptr::arc<int> a{ptr::in_place, 42};
    ptr::weak<int> w;
    
    assert(w.use_count() == 0);
    assert(w.expired());

    w = a;

    assert( w.use_count() == 1 );
    assert( !w.expired() );

    auto const b = w.lock();

    assert(a.use_count() == 2);
    assert(b.use_count() == 2);

    assert( *a == *b );
    assert( a.get() == b.get() );
    
}

auto test_weak_inheritance() -> void {
    //
    struct iface {
        virtual ~iface() = default;
        virtual auto method() -> void = 0;
    };

    //
    struct impl : public iface {
        explicit impl(bool* is_destructed_)
            : m_is_destructed{is_destructed_}
        {}

        ~impl() {
            *m_is_destructed = true;
        }
    
        // iface
        auto method() -> void override {}

        //
        bool* m_is_destructed;
    };

    //
    bool is_destructed = false;
    
    {
        ptr::arc<impl>  a{ptr::in_place, &is_destructed};
        ptr::arc<iface> b = a;
        ptr::weak<iface> w = a;

        assert( a.use_count() == 2 );
        assert( b.use_count() == 2 );
        assert( w.use_count() == 2 );
        assert( !w.expired() );
        
        a.reset();

        assert( b.use_count() == 1 );
        assert( w.use_count() == 1 );
        assert( !w.expired() );
    }

    assert( is_destructed );
}

auto test_arc_weak_multithreaded_many() -> void {
    constexpr std::size_t const shared_count = 1'000;
    // init vector of shareds
    std::vector< ptr::arc<std::size_t> > xs;

    xs.reserve(shared_count);
    for(std::size_t i = 0; i < shared_count; ++i) {
        xs.emplace_back(ptr::in_place, i);
    }

    std::vector< ptr::weak<std::size_t> > ws;

    ws.reserve(shared_count);
    for(std::size_t i = 0; i < shared_count; ++i) {
        ws.emplace_back(xs[i]);
    }
    
    // run workers
    {
        int const worker_count = 1000;
    
        std::vector<std::jthread> workers;
        workers.reserve(worker_count);

        for(int i = 0; i < worker_count; ++ i) {
            workers.emplace_back([ws] () mutable {
                using namespace std::chrono;
                auto const begin = high_resolution_clock::now();

                bool do_clear = false;
                std::vector< ptr::arc<std::size_t> > xs;
                
                for(;;) {
                    sleep_some();

                    //
                    auto const now = high_resolution_clock::now();
                    if( now - begin > 5s) {
                        break;
                    }

                    //
                    if(do_clear) {
                        xs.clear();
                    }
                    else {
                        for(std::size_t i = 0; i < ws.size(); ++i) {
                            xs.push_back( ws[i].lock() );
                        }
                    
                        std::rotate(
                            xs.begin(),
                            xs.begin() + xs.size() / 3,
                            xs.end()
                        );
                    }
 
                    do_clear ^= true;
                }
            });
        }
    
    }
    
    // make sure every shared has exactly one count
    for(std::size_t i = 0; i < xs.size(); ++ i) {
        assert( xs[i].use_count() == 1 );
        assert( *xs[i] == i );
    }
}

auto main() -> int {

    test_arc_ctor_dtor();
    test_arc_and_its_copy_imply_use_count_of_2();
    test_moved_arc_keeps_total_use_count_of_1();
    test_arc_with_initializer_list();
    test_copied_arcs_point_to_same_thing();
    test_arc_convertible_to_bool();
    test_arc_reset_sets_arc_to_null();
    test_arc_swap();
    test_arc_swap_manual();
    test_arc_inheritance();
    test_arc_multithreaded_one_arc_shared();
    test_arc_multithreaded_many_arc_shared();

    test_weak_ctor_dtor();
    test_weak_inheritance();
    test_arc_weak_multithreaded_many();
        
    return 0;
}
