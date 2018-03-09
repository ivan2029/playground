#include "declarations.hpp"

template<template<class> class F, class ... Ts>
auto for_each_type() -> void {
    std::initializer_list<int>{ (F<Ts>{}(), 0) ... };
}

template<class T>
struct print_is_lock_free {
    auto operator() () const -> void {
        std::cout << std::setw(20) << typeid(T).name() 
            << " " << std::setw(10) << std::atomic<T>::is_always_lock_free
            << " " << std::setw(4) << sizeof(T)
            << "\n";
    }
};

struct Has1 { int a; };
struct Has2 { int a, b; };
struct Has3 { int a, b, c; };
struct Has4 { int a, b, c, d; };

auto are_atomics_lock_free() -> void {
  
    for_each_type<print_is_lock_free,
        bool, 
        char, signed char, unsigned char, 
        short, unsigned short,
        int, unsigned int,
        long, unsigned long,
        long long, unsigned long long,
        char16_t,
        char32_t,
        wchar_t,
        float, double, long double,
        void*,
        Has1, Has2, Has3, Has4
    >();
}
