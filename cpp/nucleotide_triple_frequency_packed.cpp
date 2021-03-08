#include <iostream>
#include <iomanip>
#include <string>
#include <string_view>
#include <vector>

auto nuc_to_idx(char const n) -> std::size_t {
    switch(n){
    case 'a': return 0b00;
    case 't': return 0b11;
    case 'c': return 0b01;
    case 'g': return 0b10;
    default: throw -1;
    }
}

auto idx_to_nuc(std::size_t const n) -> char {
    switch(n) {
    case 0b00: return 'a';
    case 0b11: return 't';
    case 0b01: return 'c';
    case 0b10: return 'g';
    default: throw -1;
    }
}

auto seq_to_idx(std::string_view const seq) -> std::size_t {
    return (nuc_to_idx(seq[0]) << 4) 
         | (nuc_to_idx(seq[1]) << 2)
         | nuc_to_idx(seq[2]);
}

auto idx_to_seq(std::size_t const idx) -> std::string_view {
    static auto const seqs = []{
        std::string seqs(4*4*4*3, '?');
        
        for(std::size_t i{0}; i < 4*4*4; ++ i) {
            seqs[3*i    ] = idx_to_nuc( i >> 4 );
            seqs[3*i + 1] = idx_to_nuc( (i >> 2) & 0b11);
            seqs[3*i + 2] = idx_to_nuc( i & 0b11 );        
        }
        
        return seqs;
    }();
    
    return std::string_view{seqs}.substr(idx*3, 3);
}

int main() {
    std::string const test_str{"acttgtccgttaggtactactttgggt"};
    
    std::vector<int> freqs(4*4*4, 0);
    
    std::string_view view{test_str};
    while(!view.empty()) {
        auto const current = view.substr(0, 3);
        view.remove_prefix(3);
    
        ++ freqs[seq_to_idx(current)];
    }
    
    for(std::size_t i{0}; i < freqs.size(); ++ i) {
        if(freqs[i]) {
            std::cout << std::setw(3) << std::hex << i << " " 
                      << idx_to_seq(i) << " "
                      << freqs[i] << " " 
                      << "\n";
        }
    }
}
