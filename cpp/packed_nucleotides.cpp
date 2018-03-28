/*
  To encode 4 letters (in our case A, C, T, and G) we need only 2
  bits. Because A pairs only with T and C pairs only with G (and
  vice versa), we pick following encoding for letters:

  A = 00
  T = 11
  C = 01
  G = 10

  ie, pairing letters are bitwise complements of each other.

  In here, an algorithm for computing complementary chain of RNA is
  implemented twice, once using one char per letter, other using
  packed letters. Then their speeds are compared.

  Expectation: packed version is going to do better simply because of
  cache on modern processors.

  To simplify things, we expect to always have n = 4*k
  nucleotides. That way we don't have partially filled packs!

  Results (average for 100 runs):

  1. g++ -std=c++17 -O0
    
    unpacked: 1110 ms
    packed:    220 ms

  2. g++ -std=c++17 -O2

    unpacked: 33 ms
    packed:   13 ms

*/

#include <cassert>
#include <iostream>
#include <cinttypes>
#include <array>
#include <vector>
#include <string>
#include <algorithm>
#include <tuple>

template<class It, class Sent, class T>
bool elem(It it, Sent sent, T const& t) {
  return std::find(it, sent, t) != sent;
}

template<class R, class T>
bool elem(R const& r, T const& t) {
  return elem(r.begin(), r.end(), t);
}

template<class It, class Sent, class T>
std::size_t index_of(It it, Sent sent, T const& t) {
  return std::distance( it
                      , std::find(it, sent, t) );
}

template<class R, class T>
std::size_t index_of(R const& r, T const& t) {
  return index_of(r.begin(), r.end(), t);
}

namespace gene {
  using rna_t = std::string;
  
  void complement(rna_t& rna) {
      auto const tr = [](char const c) {
        switch(c) {
        case 'A': return 'T';
        case 'T': return 'A';
        case 'C': return 'G';
        case 'G': return 'C';
        default: return '?';
        }
      };
      std::reverse(rna.begin(), rna.end());
      std::transform(rna.begin(), rna.end(), rna.begin(), tr);
  }
}

namespace packed_gene {
  using byte_t = std::uint8_t;

  using rna_t = std::vector<byte_t>;
  
  std::array<char, 4> nucleotide_letters = { 'A', 'C', 'G', 'T' };
  std::array<byte_t, 4> nucleotide_code  = { 0x0, 0x1, 0x2, 0x3 };

  std::ostream& write_pack(std::ostream& out, byte_t const pack) {
    return out << nucleotide_letters[(pack >> 6) & 0x3]
               << nucleotide_letters[(pack >> 4) & 0x3]
               << nucleotide_letters[(pack >> 2) & 0x3]
               << nucleotide_letters[pack        & 0x3]
               ;
  }

  byte_t pack(char const x, char const y, char const z, char const t) {
    assert( elem(nucleotide_letters, x) );
    assert( elem(nucleotide_letters, y) );
    assert( elem(nucleotide_letters, z) );
    assert( elem(nucleotide_letters, t) );
    return (nucleotide_code[index_of(nucleotide_letters, x)] << 6)
         | (nucleotide_code[index_of(nucleotide_letters, y)] << 4)
         | (nucleotide_code[index_of(nucleotide_letters, z)] << 2)
         | nucleotide_code[index_of(nucleotide_letters, t)]
         ;
  }

  rna_t pack(gene::rna_t const& rna) {
    rna_t packed_rna;
    packed_rna.reserve(rna.size()/4);
    for(std::size_t i = 0; i < rna.size()/4; ++ i) {
      packed_rna.push_back( pack(rna[i*4], rna[i*4 + 1], rna[i*4 + 2], rna[i*4 + 3]) );
    }
    return packed_rna;
  }

  std::tuple<char, char, char, char> unpack(byte_t const rna) {
    return std::make_tuple( nucleotide_letters[(rna >> 6) & 0b00000011]
                          , nucleotide_letters[(rna >> 4) & 0b00000011]
                          , nucleotide_letters[(rna >> 2) & 0b00000011]
                          , nucleotide_letters[       rna & 0b00000011]
                          );
  }
  
  gene::rna_t unpack(rna_t const& rna) {
    gene::rna_t unpacked_rna;
    unpacked_rna.reserve(rna.size()*4);
    for(auto const b: rna) {
      auto const [x, y, z, t] = unpack(b);
      unpacked_rna.push_back(x);
      unpacked_rna.push_back(y);
      unpacked_rna.push_back(z);
      unpacked_rna.push_back(t);
    }
    return unpacked_rna;
  }

  std::uint8_t rev(std::uint8_t const x) {
    return ( (x >> 6) & 0b00000011 )
         | ( (x >> 2) & 0b00001100 )
         | ( (x << 2) & 0b00110000 )
         | ( (x << 6) & 0b11000000 )
         ;
  }
  
  void complement(rna_t& rna) {
    std::reverse(rna.begin(), rna.end());
    std::transform( rna.begin(), rna.end()
                  , rna.begin()
                  , [](byte_t const x) {
                      return ~rev(x);
                    } );
  }
}

//
// tests
//
namespace gn = gene;
namespace pg = packed_gene;

void test_1() {
  gn::rna_t unpacked("ACGT");
  pg::rna_t packed = pg::pack(unpacked);
  
  gn::complement(unpacked);
  pg::complement(packed);

  auto unpacked_1 = pg::unpack(packed);

//  std::cout << unpacked << " " << unpacked_1 << "\n";
  
  assert(unpacked_1 == unpacked);
}

#include <chrono>
#include <random>

template<class Fn>
auto time(Fn fn) {
  using namespace std::chrono;
  auto begin = high_resolution_clock::now();
  fn();
  auto end = high_resolution_clock::now();
  return duration_cast<milliseconds>(end - begin).count();
}

void test_2() {
  constexpr int TEST_COUNT = 100;

  constexpr std::size_t COUNT = 0x1 << 25;
    gn::rna_t const source = []{
      std::random_device rd;
      std::uniform_int_distribution<> dist(0, 3);
      gn::rna_t rna;
      rna.reserve(COUNT);
      for(std::size_t i = 0; i < COUNT; ++ i) {
        auto index = dist(rd);
        rna.push_back(pg::nucleotide_letters[index]);
      }
      return rna;
    }();
  
  auto run_single_test = [&](bool do_assert = false){
    gn::rna_t unpacked = source;
    pg::rna_t packed = pg::pack(unpacked);

    auto unpacked_time = time([&]{ gn::complement(unpacked); });
    auto packed_time   = time([&]{ pg::complement(packed);   });

    if(do_assert){
      assert(unpacked == pg::unpack(packed));
    }
    
    return std::make_tuple(unpacked_time, packed_time);
  };

  run_single_test(true);
  
  long long unpacked_time{0ll}, packed_time{0ll};

  for(int i = 0; i < TEST_COUNT; ++ i) {
    std::cout << "test: " << i << "\n";
    auto const [u, p] = run_single_test();
    unpacked_time += u;
    packed_time += p;
  }

  unpacked_time /= TEST_COUNT;
  packed_time /= TEST_COUNT;
  
  std::cout << unpacked_time << "\n"
            << packed_time << "\n";
}

int main() {
  test_1();
  test_2();
  return 0;
}
