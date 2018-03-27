#pragma once

#include <SDL.h>

#include <stdexcept>

template<class T, class V, class Msg>
auto assert_eq(T&& value, V&& expected, Msg&& message) -> void {
    if(value == expected) return;
    throw std::runtime_error{std::forward<Msg>(message)};
}

template<class T, class V, class Msg>
auto assert_not_eq(T&& value, V&& expected, Msg&& message) -> void {
    if(value != expected) return;
    throw std::runtime_error{std::forward<Msg>(message)};
}

template<class T, class V>
auto sdl_assert_eq(T const value, V const expected) -> void {
    if(value == expected) return;
    throw std::runtime_error( SDL_GetError() );
}

template<class T, class V>
auto sdl_assert_not_eq(T const value, V const expected) -> void {
    if(value != expected) return;
    throw std::runtime_error( SDL_GetError() );
}