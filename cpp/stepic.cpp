#include <cassert>

#include <iostream>
#include <fstream>

#include <tuple>
#include <string>
#include <vector>
#include <set>
#include <unordered_map>
#include <unordered_set>

#include <algorithm>
#include <numeric>
#include <iterator>
using namespace std::literals;

//
//
//

// Tr :: A -> B
// Pr :: A -> bool
template<class It, class S, class OutIt, class Tr, class Pr>
OutIt transform_if(It first, S last, OutIt out, Tr tr, Pr pred) {
  for(; first != last; ++first) {
    if( pred(*first) ) {
      *out = tr(*first);
      +out;
    }
  }
  return out;
}

template<class It, class Size, class Fn>
void window(It f, It l, Size n, Fn fn) {
  assert( std::distance(f, l) >= n );
  auto s = std::next(f, n);
  for(; s != l; ++f, ++ s) {
    fn(f, s);
  }
  fn(f, s);
}

/*
template<class It, class Size, class Fn>
void window_par(It first, It last, Size n, Fn fn) {
  assert(std::distance(first, last) > n);
  int d = std::distance(first, last) + 1 - n;
  #pragma omp parallel for
  for(int i = 0; i < d; ++ i){
    fn(first + i, first + n + i);
  }
}
*/

template<class It, class S, class T>
auto index_of(It first, S last, T const& t) {
  return std::distance(first, std::find(first, last, t));
}

template<class R, class T>
auto index_of(R const& r, T const& t) {
  return index_of(std::begin(r), std::end(r), t);
}

template<class It, class S, class OutIt>
OutIt max_element_indices(It first, S last, OutIt out) {
  assert( first != last );
  auto const& max_val = *std::max_element(first, last);
  for(auto it = first; it != last; ++ it) {
    if( max_val == *it ) {
      *out = std::distance(first, it);
      ++ out;
    }
  }
  return out;
}

template<class It, class S, class OutIt>
OutIt min_element_indices(It first, S last, OutIt out) {
  assert( first != last );
  auto const& min_val = *std::min_element(first, last);
  for(auto it = first; it != last; ++ it) {
    if( min_val == *it ) {
      *out = std::distance(first, it);
      ++ out;
    }
  }
  return out;
}

//
//
//

/*
    A common precondition for all these algorithms is this: Since
    chars represent nucleotides, they have to be one of 'A', 'C', 'G',
    'T'. If this precondition is not met, behavior of algorithms is
    undefined.
*/


constexpr std::array<char, 4> NUCLEOTIDES = {'A', 'C', 'G', 'T'};

int nucleotide_index(char c) {
  assert(c == 'A' || c == 'C' || c == 'G' || c == 'T');
  switch(c) {
  case 'A': return 0;
  case 'C': return 1;
  case 'G': return 2;
  case 'T': return 3;
  }
  return -1;
}
/*  
*/
template<class It, class S>
long long pattern_to_number(It first, S last) {
  auto const folding = [](auto state, auto c) {
    return (state << 2) + index_of(NUCLEOTIDES, c);
  };
  return std::accumulate(first, last, 0ll, folding);
}

template<class R>
long long pattern_to_number(R const& pattern) {
  return pattern_to_number( std::begin(pattern), std::end(pattern) );
}

/*
*/
std::string number_to_pattern(long long number, long long length) {
    std::string pattern(length, ' ');;
    for(long long i = 0; i < length; ++ i) {
        pattern[length - 1 - i] =  NUCLEOTIDES[number & 0x3]; // number % 4
        number >>= 2; // number / 2
    }
    return pattern;
}

/*
*/
template<class It, class S>
void reverse_complement(It f, S l) {
  auto const tr = [](char const c) {
    switch(c){
    case 'A': return 'T';
    case 'T': return 'A';
    case 'C': return 'G';
    case 'G': return 'C';
    }
    return c;
  };
  std::transform(f, l, f, tr);
  std::reverse(f, l);
}

template<class R>
void reverse_complement(R& text)
{
  reverse_complement(std::begin(text), std::end(text));
}

/*
    counts occurrences of pattern in text
*/
template<class It1, class S1, class It2, class S2>
int pattern_count( It1 text_f, S1 text_l, It2 pattern_f, S2 pattern_l ) {
  int count = 0;
  window( text_f, text_l
        , std::distance(pattern_f, pattern_l)
        , [&](auto f, auto /*l*/) {
            if( std::equal(pattern_f, pattern_l, f) ) {
              ++ count;
            }
          });
  return count;
}

template<class R1, class R2>
int pattern_count(R1 const& text, R2 const& pattern) {
  return pattern_count( std::begin(text), std::end(text)
                      , std::begin(pattern), std::end(pattern) );
}

/*
    finds all occurrences of pattern in text
*/
template<class It1, class S1, class It2, class S2, class OutIt>
OutIt occurrences(It1 text_f, S1 text_l, It2 pattern_f, S2 pattern_l, OutIt out) {
  window( text_f, text_l
        , std::distance(pattern_f, pattern_l)
        , [&](auto f, auto /*l*/) {
            if( std::equal(pattern_f, pattern_l, f) ) {
              *out = std::distance(text_f, f);
              ++out;
            }
          });
  return out;
}

template<class R1, class R2, class OutIt>
OutIt occurrences(R1 const& text, R2 const& pattern, OutIt out) {
  return occurrences( std::begin(text), std::end(text)
                    , std::begin(pattern), std::end(pattern)
                    , out );
}

/*
    Computes frequencies of all patterns of length k in text. To get
    pattern for index i, use number_to_pattern(i, k);
*/
template<class It, class S>
std::vector<int> frequencies(It first, S last, int k) {
  int const size = 0x1 << (k << 1);
  std::vector<int> freqs(size, 0);
  window( first, last
        , k
        , [&](auto f, auto l) {
            ++ freqs[ pattern_to_number(f, l) ];
          });
  return freqs;
}

std::vector<int> frequencies(std::string const& text, int k) {
  return frequencies(std::begin(text), std::end(text), k);
}

/*
    finds most frequent patterns of length k occuring in text
*/
std::vector<std::string> most_frequent_words(std::string const& text, int k) {
  auto freqs = frequencies(text, k);
  std::vector<int> indices;
  max_element_indices( freqs.begin(), freqs.end(), std::back_inserter(indices) );
  std::vector<std::string> result(indices.size());
  std::transform( indices.begin(), indices.end()
                , result.begin()
                , [=](int index){ return number_to_pattern(index, k); });
  return result;
}

/*
    finds k-mers that occur at least t times in window size of l
*/

std::vector<std::string> find_clumps( std::string const& text
                                    , int const k
                                    , int const l
                                    , int const t ) {
  auto first = text.begin();
  auto last   = text.end();

  std::unordered_set<long long> clumpsS;
  window( first, last
         , l
         , [&](auto f, auto l) {
             auto freqs = frequencies(f, l, k);
             for(auto it = freqs.begin(); it != freqs.end(); ++ it) {
               if(*it >= t) {
                 clumpsS.insert(std::distance(freqs.begin(), it)); 
               }
             }
           });

  std::vector<std::string> clumps;
  std::transform( clumpsS.begin(), clumpsS.end()
                , std::back_inserter(clumps)
                , [=](long long i){ return number_to_pattern(i, k); });
  return clumps;
}

/* parallelized version - needed this to make the 5 minute limit for given dataset
   not the best parallelization... how to do this better?
std::vector<std::string> find_clumps( std::string const& text
                                    , int const k
                                    , int const l
                                    , int const t ) {
  auto first = text.begin();
  auto last   = text.end();

  std::unordered_set<long long> clumpsS;
  window_par( first, last
            , l
            , [&](auto f, auto l) {
                auto freqs = frequencies(f, l, k);
                for(long long i = 0; i < static_cast<long long>(freqs.size()); ++ i){
                  if(freqs[i] >= t) {
                    #pragma omp critical
                    { clumpsS.insert(i); }
                  }
                }
              });

  std::vector<std::string> clumps;
  std::transform( clumpsS.begin(), clumpsS.end()
                , std::back_inserter(clumps)
                , [=](long long i){ return number_to_pattern(i, k); });
  return clumps;
}
*/

/*
    compute skew and minimum skew
*/

template<class It, class S, class OutIt>
OutIt skew(It first, S last, OutIt out) {
  int current = 0;
  *out = current; ++ out;
  for(; first != last; ++first, ++out) {
    switch( *first ) {
      case 'G': ++current; break;
      case 'C': --current; break;
    }
    *out = current;
  }
  return out;
}

std::vector<int> skew(std::string const& genome) {
  std::vector<int> xs;
  xs.reserve(genome.size() + 1);
  skew(genome.begin(), genome.end(), std::back_inserter(xs));
  return xs;
}

std::vector<int> min_skew(std::string const& genome) {
  auto const& skews = skew(genome);
  std::vector<int> xs;
  min_element_indices(skews.begin(), skews.end(), std::back_inserter(xs));
  return xs;
}

/*
*/
template<class It1, class S1, class It2, class S2>
int hamming_distance(It1 a_f, S1 a_l, It2 b_f, S2 b_l) {
  assert( std::distance(a_f, a_l) == std::distance(b_f, b_l) );
  int dist = 0;
  for(; a_f != a_l; ++a_f, ++b_f) {
    if(*a_f != *b_f) {
      ++ dist;
    }
  }
  return dist;
}

int hamming_distance(std::string const& as, std::string const& bs) {
  return hamming_distance(as.begin(), as.end(), bs.begin(), bs.end());
}

/*
*/
template<class It1, class S1, class It2, class S2, class OutIt>
OutIt approx_occurrences( It1 text_f, S1 text_l
                        , It2 pattern_f, S2 pattern_l
                        , int const max_dist
                        , OutIt out ) {
  window( text_f, text_l
        , std::distance(pattern_f, pattern_l)
        , [&](auto f, auto l) {
            if(hamming_distance( pattern_f, pattern_l, f, l ) <= max_dist) {
              *out = std::distance(text_f, f);
              ++ out;
            }
          });
  return out;
}                        

std::vector<int> approx_occurrences( std::string const&  text
                                   , std::string const&  pattern 
                                   , int const           max_dist) {
  std::vector<int> occ;
  approx_occurrences( text.begin(), text.end()
                    , pattern.begin(), pattern.end()
                    , max_dist
                    , std::back_inserter(occ) );
  return occ;
}


/*
*/
template<class It1, class S1, class It2, class S2>
int count_approx_occurrences( It1 text_f, S1 text_l
                            , It2 pattern_f, S2 pattern_l
                            , int const max_dist ) {
  int c = 0;
  window( text_f, text_l
        , std::distance(pattern_f, pattern_l)
        , [&](auto f, auto l) {
            if(hamming_distance( pattern_f, pattern_l, f, l ) <= max_dist) {
              ++ c;
            }
          });
  return c;
}

int count_approx_occurrences( std::string const&  text
                            , std::string const&  pattern
                            , int const           max_dist ) {
  return count_approx_occurrences( text.begin(), text.end()
                                 , pattern.begin(), pattern.end()
                                 , max_dist );
}

/*
*/
std::vector<std::string> neighbours(std::string const& pattern, int const dist) {
  if(dist == 0) {
    return { pattern };
  }
  if(pattern.size() == 1) {
    return {"A", "C", "G", "T"};
  }
  std::set<std::string> ns;
  auto const suffix = pattern.substr(1, std::string::npos);
  auto const suffix_ns = neighbours(suffix, dist);
  for( auto const& suffix_n: suffix_ns ) {
    if( hamming_distance(suffix_n, suffix) < dist ){
      for( auto n: NUCLEOTIDES ) {
        ns.insert( n + suffix_n );
      }
    }
    else {
      ns.insert( pattern[0] + suffix_n );
    }
  }
  return std::vector<std::string>(ns.begin(), ns.end());
}

/*
*/
std::vector<std::string> most_frequent_kmer( int const k
                                           , int const max_dist
                                           , std::string const& text ) {
  std::vector<int> freqs( 0x1 << (k*2), 0 );
  window( text.begin(), text.end()
        , k
        , [&](auto f, auto l) {
            std::string kmer(f, l);
            auto ns = neighbours(kmer, max_dist);
            for(auto const& n: ns) {
              if(hamming_distance(kmer, n) <= max_dist) {
                ++freqs[ pattern_to_number(n) ];
              }
            }
          });
  //
  auto it = std::max_element(freqs.begin(), freqs.end());
  std::vector<std::string> kmers;
  for(auto f = freqs.begin(); f != freqs.end(); ++ f) {
    if(*f == *it) {
      kmers.push_back(number_to_pattern(std::distance(freqs.begin(), f), k));
    }
  }
  return kmers;
}                                           

/*
*/
std::vector<std::string> most_frequent_kmer_v2( int const k
                                              , int const max_dist
                                              , std::string const& text ) {
  std::vector<int> freqs( 0x1 << (k*2), 0 );
  window( text.begin(), text.end()
        , k
        , [&](auto f, auto l) {
            std::string kmer(f, l);
            auto ns = neighbours(kmer, max_dist);
            for(auto const& n: ns) {
              if(hamming_distance(kmer, n) <= max_dist) {
                ++freqs[ pattern_to_number(n) ];
              }
            }
            reverse_complement(kmer);
            ns = neighbours(kmer, max_dist);
            for(auto const& n: ns) {
              if(hamming_distance(kmer, n) <= max_dist) {
                ++freqs[ pattern_to_number(n) ];
              }
            }
          });
  //
  auto it = std::max_element(freqs.begin(), freqs.end());
  std::vector<std::string> kmers;
  for(auto f = freqs.begin(); f != freqs.end(); ++ f) {
    if(*f == *it) {
      kmers.push_back(number_to_pattern(std::distance(freqs.begin(), f), k));
    }
  }
  return kmers;
}


//
//
//
void test_pattern_to_number() {
  assert( 11ll == pattern_to_number( "AGT"s ) );
}

void test_number_to_pattern() {
  assert( "AGTC" == number_to_pattern(45, 4) );
}

void test_reverse_complement() {
  std::string const expected = "ACCGGGTTTT";
  std::string text = "AAAACCCGGT";
  reverse_complement(text);
  assert(expected == text);
}

void test_pattern_count() {
  std::string const text = "GCGCG";
  std::string const pattern = "GCG";
  assert( 2 == pattern_count(text, pattern) );
}

void test_occurrences() {
 std::string const text = "GATATATGCATATACTT";
 std::string const pattern = "ATAT";
 std::vector<int> const expected = {1,3,9};
 std::vector<int> actual;
 occurrences(text, pattern, std::back_inserter(actual));
 assert(expected == actual);
}

void test_frequencies() {
  std::string const text = "ACGCGGCTCTGAAA";
  std::vector<int> const expected = {2,1,0,0,0,0,2,2,1,2,1,0,0,1,1,0};
  assert( expected == frequencies(text, 2) );
}

void test_most_frequent_words() {
  std::string const text = "ACGTTGCATGTCGCATGATGCATGAGAGCT";
  std::set<std::string> const expected = {"CATG", "GCAT"};
  auto const actual = most_frequent_words(text, 4);
  auto const actualS = std::set<std::string>(actual.begin(), actual.end());
  assert( expected == actualS );
}

void test_find_clumps() {
  std::string const text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA";
  std::set<std::string> const expected = {"CGACA", "GAAGA"};
  auto const actual = find_clumps(text, 5, 50, 4);
  auto const actualS = std::set<std::string>(actual.begin(), actual.end());
  assert( expected == actualS );
}

void test_min_skew() {
  std::string const text = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT";
  std::vector<int> const expected = {11, 24};
  assert( expected == min_skew(text) );
}

void test_hamming_distance() {
  assert( 3 == hamming_distance("GGGCCGTTGGT", "GGACCGTTGAC") );
}

void test_approx_occurrences() {
  std::string const text = "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT";
  std::string const pattern = "ATTCTGGA";
  int const max_dist = 3;
  std::vector<int> const expected = {6, 7, 26, 27};
  auto const actual = approx_occurrences(text, pattern, max_dist);
  assert( expected == actual );
}

void test_count_approx_occurrences() {
  std::string const text = "TTTAGAGCCTTCAGAGG";
  std::string const pattern = "GAGG";
  int const max_dist = 2;
  assert( 4 == count_approx_occurrences(text, pattern, max_dist) );
}

void test_neighbours() {
  std::set<std::string> const expected = {
    "ATG", "AGG", "ACT", "ACC", "ACA", "CCG", "ACG", "GCG", "AAG", "TCG"
  };
  auto const actual = neighbours("ACG"s, 1);  
  auto const actualS = std::set<std::string>(actual.begin(), actual.end());
  assert( expected == actualS );
}

void test_most_frequent_kmer() {
  // TODO
}

void test_most_frequent_kmer_v2() {
  // TODO
}



//
void test_all() {
  test_pattern_to_number();
  test_number_to_pattern();
  test_reverse_complement();
  test_pattern_count();
  test_occurrences();
  test_frequencies();
  test_most_frequent_words();
  test_find_clumps();
  test_min_skew();
  test_hamming_distance();
  test_approx_occurrences();
  test_count_approx_occurrences();
  test_neighbours();
  test_most_frequent_kmer();
  test_most_frequent_kmer_v2();
}

//
// new stuff, sort it out later
//

#include <cctype>

void skip_spaces(std::istream& in) {
  while( std::isspace(in.peek()) ){
    in.get();
  }
}

int read_int(std::istream& in) {
  int x;
  in >> x;
  return x;
}

std::string read_line(std::istream& in) {
  std::string line;
  std::getline(in, line);
  return line;
}

std::vector<std::string> read_lines(std::istream& in) {
  std::vector<std::string> lines;
  std::string line;
  while(std::getline(in, line)) {
    lines.push_back(std::move(line));
  }
  return lines;
}

// given collection of genomes and integer d, k-mer is (k,d) motif if
// it appears in every genome with at most d mismatches
std::vector<std::string> enumerate_motifs( std::vector<std::string> const& dnas
                                         , int const k
                                         , int const d ) {
  int const kmer_count = 0x1 << (k << 1); 

  std::vector<std::string> result;
  for(int i = 0; i < kmer_count; ++ i) {
    auto kmer = number_to_pattern(i, k);
    bool occurrs_in_all = true;
    for(auto const& dna: dnas) {
      int const c = count_approx_occurrences(dna, kmer, d);
      occurrs_in_all &= c > 0;
    }
    if(occurrs_in_all) {
      result.push_back(std::move(kmer));
    }
  }

  return result;
}



void test_enumerate_motifs(std::istream& in, std::ostream& out) {
  auto k = read_int(in);
  auto d = read_int(in);
  skip_spaces(in);
  auto dnas = read_lines(in);
  
  auto res = enumerate_motifs(dnas, k, d);
  
  for(auto const& d: res) {
    out << d << "\n";
  }  
}

/*
*/
int d_fn(std::string const& pattern, std::string const& dna) {
  int min_val = std::numeric_limits<int>::max();
  window( dna.begin(), dna.end()
        , static_cast<int>(pattern.size())
        , [&](auto f, auto l) {
            int hd = hamming_distance(f, l, pattern.begin(), pattern.end());
            if( min_val > hd ) {
              min_val = hd;
            }
          });
  return min_val;
}

int d_fn(std::string const& pattern, std::vector<std::string> const& dnas) {
  return std::accumulate( dnas.begin(), dnas.end()
                        , 0
                        , [&](auto state, auto const& el) {
                            return state + d_fn(pattern, el);
                          });
}

std::string median_string( std::vector<std::string> const& dnas
                         , int const                       k ) {
  int const kmer_count = 0x1 << (k << 1); 

  int distance_ = std::numeric_limits<int>::max();
  std::string kmer;
  for(int i = 0; i < kmer_count; ++ i) {
    std::string current_kmer = number_to_pattern(i, k);
    int d = d_fn(current_kmer, dnas);
    if(distance_ > d) {
      distance_ = d;
      kmer = std::move(current_kmer);
    }
  }
  return kmer;
}                         

void test_median_string(std::istream& in, std::ostream& out) {
  auto k = read_int(in);
  skip_spaces(in);
  auto dnas = read_lines(in);
  auto res = median_string(dnas, k);
  out << res << "\n";
}

/*
*/
template<class T>
class Matrix {
public:
  Matrix(int const rows, int const cols)
    : m_rows(rows)
    , m_cols(cols)
    , m_data(rows * cols) {
    assert( rows > 0 && cols > 0 );
  }

  Matrix(int const rows, int const cols, std::initializer_list<T> ts)
    : m_rows(rows)
    , m_cols(cols)
    , m_data(ts) {
    assert( rows > 0 && cols > 0 );
    assert( static_cast<int>(m_data.size()) == rows*cols );
  }
  
  Matrix()
    : m_data(1)
  {}

  Matrix(Matrix const& ) = default;
  Matrix(Matrix&& ) = default;

  ~Matrix() = default;

  Matrix& operator= (Matrix const&) = default;
  Matrix& operator= (Matrix&&) = default;

public:
  int rows() const { return m_rows; }
  int cols() const { return m_cols; }

  T& el(int const row, int const col) {
    assert(0 <= row && row < m_rows);
    assert(0 <= col && col < m_cols);
    return m_data[row*m_cols + col];
  }

  T const& el(int const row, int const col) const {
    assert(0 <= row && row < m_rows);
    assert(0 <= col && col < m_cols);
    return m_data[row*m_cols + col];
  }
private:
  int m_rows = 1;
  int m_cols = 1;
  std::vector<T> m_data;
};

template<class T>
Matrix<T> read_matrix(std::istream& in, int const rows, int const cols) {
  Matrix<T> m(rows, cols);
  for(int row = 0; row < rows; ++ row) {
    for(int col = 0; col < cols; ++ col) {
      T val;
      in >> val;
      m.el(row, col) = val;
    }
  }
  return m;
}



/*
  computes P(pattern | profile)
*/
template<class It, class S>
double probability(It pattern_f, S pattern_l, Matrix<double> const& profile) {
  assert(profile.rows() == 4);
  assert(profile.cols() == std::distance(pattern_f, pattern_l));
  double prod = 1.0;
  for(auto it = pattern_f; it != pattern_l; ++ it) {
    prod *= profile.el( nucleotide_index(*it)
                      , std::distance(pattern_f, it) );
  }
  return prod;
}

double probability(std::string const& pattern, Matrix<double> const& profile) {
  return probability(pattern.begin(), pattern.end(), profile);
}

void test_probability() {
  Matrix<double> const profile(
    4, 12,
    { 0.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.9, 0.1, 0.1, 0.1, 0.3, 0.0
    , 0.1, 0.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.1, 0.2, 0.4, 0.6
    , 0.0, 0.0, 1.0, 1.0, 0.9, 0.9, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0
    , 0.7, 0.2, 0.0, 0.0, 0.1, 0.1, 0.0, 0.5, 0.8, 0.7, 0.3, 0.4 });
  std::string const pattern = "TCGTGGATTTCC";
  auto prob = probability(pattern, profile);
//  std::cout << prob << "\n";
  assert(prob == 0.0); // beware, floating-point comparison!
}

/*
*/

std::string most_probable_kmer( std::string const&    text
                              , int const             k
                              , Matrix<double> const& profile ) {
  double prob = 0.0;
  int index = 0;
  window( text.begin(), text.end()
        , k
        , [&](auto f, auto l) {
            auto cur_prob = probability(f, l, profile);
            if( cur_prob > prob ){
              prob = cur_prob;
              index = std::distance(text.begin(), f);
            }
          });
  return text.substr(index, k);  
}

void test_most_probable_kmer(std::istream& in, std::ostream& out) {
  auto text = read_line(in);
  auto k = read_int(in);
  auto profile = read_matrix<double>(in, 4, k);
  out << most_probable_kmer(text, k, profile) << "\n";
}

/*
GreedyMotifSearch(Dna, k, t)
        BestMotifs ← motif matrix formed by first k-mers in each string from Dna
        for each k-mer Motif in the first string from Dna
            Motif1 ← Motif
            for i = 2 to t
                form Profile from motifs Motif1, …, Motifi - 1
                Motifi ← Profile-most probable k-mer in the i-th string in Dna
            Motifs ← (Motif1, …, Motift)
            if Score(Motifs) < Score(BestMotifs)
                BestMotifs ← Motifs
        return BestMotifs

...
*/



//
//
//
int main() {
  test_all();

  
  //test_probability();
  //std::ifstream fin("sample_in.txt");
  //test_enumerate_motifs(fin, std::cout);
  //test_median_string(fin, std::cout);
  //test_most_probable_kmer(fin, std::cout);
  
  return 0;
}
