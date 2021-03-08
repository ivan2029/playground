#include <iostream>
#include <string>

//
//
//
#include <cassert>
#include <vector>
#include <algorithm>

struct GapBuffer {
    std::size_t gap_begin{0};
    std::size_t gap_end{0};
    std::vector<char> buffer;
};

auto gap_size(GapBuffer const& gb) -> std::size_t {
    return gb.gap_end - gb.gap_begin;
}

auto append_gap(GapBuffer& gb, std::size_t const new_gap_size = 4) -> void {
    assert(gap_size(gb) == 0);
    
    auto const gap_begin = gb.buffer.size();
    
    gb.buffer.resize(gap_begin + new_gap_size, '#');
    gb.gap_begin = gap_begin;
    gb.gap_end   = gap_begin + new_gap_size;
}

auto move_gap_to(GapBuffer& gb, std::size_t pos) -> void {
    assert(pos < gb.gap_begin || gb.gap_end <= pos);
    if(pos < gb.gap_begin) {
        auto const gsize = gap_size(gb); 
        auto const beg = std::next(gb.buffer.begin(), pos);
        auto const mid = std::next(gb.buffer.begin(), gb.gap_begin);
        auto const end = std::next(gb.buffer.begin(), gb.gap_end);
        std::rotate(beg, mid, end);
        gb.gap_begin = pos;
        gb.gap_end   = pos + gsize;
    }
    else {
        auto const gsize = gap_size(gb); 
        auto const beg = std::next(gb.buffer.begin(), gb.gap_begin);
        auto const mid = std::next(gb.buffer.begin(), gb.gap_end);
        auto const end = std::next(gb.buffer.begin(), pos);
        auto const it = std::rotate(beg, mid, end);
        gb.gap_begin = std::distance(gb.buffer.begin(), it);
        gb.gap_end   = gb.gap_begin + gsize;
    }
}

auto insert(GapBuffer& gb, std::size_t const pos, char const c) -> std::size_t {
    if(gap_size(gb) == 0) append_gap(gb);
    if(pos != gb.gap_begin) move_gap_to(gb, pos);

    gb.buffer[gb.gap_begin] = c;
    ++gb.gap_begin;

    return gb.gap_begin;
}



//
//
//
auto print(GapBuffer const& gb) -> void {
    std::cout << "|";
    for(auto c : gb.buffer) {
        std::cout << c;
    }
    std::cout << "|\n";
}


auto main() -> int {
    GapBuffer gb;
    
    std::string const test1{"The quick brown fox jumps over the lazy dog"};
    std::string const test2{"WHAT?!"};
    std::string const test3{"Ok, then... I'll wait."};
    
    std::size_t pos{0};
    
    for(auto const c: test1) {
        pos = insert(gb, pos, c);
        print(gb);
    }
    
    pos -= 20;
    for(auto const c: test2) {
        pos = insert(gb, pos, c);
        print(gb);
    }
    
    pos += 10;
    for(auto const c: test3) {
        pos = insert(gb, pos, c);
        print(gb);
    }
    
}