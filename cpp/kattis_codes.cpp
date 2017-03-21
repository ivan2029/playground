/*
  https://open.kattis.com/problems/codes

  example input:

    2
    7 4
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1
    0 1 1 1
    1 0 1 1
    1 1 0 1
    3 2
    1 1
    0 0
    1 0

  output:

    3
    1
*/

#include <cinttypes>
#include <iostream>
#include <iomanip>
#include <vector>
#include <stdexcept>

#if defined (NDEBUG)
  #define throw_assert(expr) ((void)0)
#else
  template<class File, class Line>
  void throw_assert_fn(File file, Line line, std::string msg) {
    throw std::runtime_error(std::string(file) + ":" + std::to_string(line) + ": " + msg);
  }
  #define throw_assert(expr) if( !(expr) ) throw_assert_fn(__FILE__, __LINE__, #expr);
#endif


/*
  sums bits modulo 2...
  essentially, result is 1 if number of bits set is odd, otherwise it is 0
*/
std::uint32_t sum_bits_mod2(std::uint32_t x) {
  x = x ^ (x >> 16);
  x = x ^ (x >>  8); 
  x = x ^ (x >>  4);
  x = x ^ (x >>  2);
  x = x ^ (x >>  1);
  return x & 0x1;
}

std::size_t count_bits(std::uint32_t const& x) {
  std::size_t count = 0;
  for(std::size_t i = 0; i < 32; ++ i) {
    if( x & (0x1 << i) ) {
      ++ count;
    }
  }
  return count;
}

std::size_t hamming_distance(std::uint32_t const& x, std::uint32_t const& y) {
  return count_bits( x ^ y );
}

std::uint32_t set_bit(std::uint32_t const x, std::size_t const bit, std::uint32_t const val) {
  // preconditions
  throw_assert(bit < 32);
  throw_assert(val == 0x0 || val == 0x1);
  // body
  std::uint32_t const mask = ~(0x1 << bit);
  return (x & mask) | (val << bit);
}


// matrix size is 32x32 bits ( fits the task constraints of 0 < k < n < 31, k < 16 )
class bit_matrix {
public:
  bit_matrix()
    : m_rows{32}
    , m_cols{32}
    , m_data(32, 0)
  {}

  bit_matrix(bit_matrix const&) = default;
  bit_matrix(bit_matrix&&) = default;
  ~bit_matrix() = default;

  bit_matrix& operator= (bit_matrix const&) = default;
  bit_matrix& operator= (bit_matrix&&) = default;

  bit_matrix(std::size_t const rows, std::size_t const cols)
    : m_rows{rows}
    , m_cols{cols}
  {
    throw_assert(rows < 32);
    throw_assert(cols < 32);
    m_data = std::vector<std::uint32_t>(rows*cols, 0);
  }
  
public:

  std::size_t rows() const { return m_rows; }
  std::size_t cols() const { return m_cols; }
  
  std::uint32_t get(std::size_t const row, std::size_t const col) const {
    // preconditions
    throw_assert(row < m_rows);
    throw_assert(col < m_cols);
    // body
    return (m_data[row] >> ((m_cols - 1) - col)) & 0x1;
  }

  void set(std::size_t const row, std::size_t const col, std::size_t const val) {
    // precondition
    throw_assert(row < m_rows);
    throw_assert(col < m_cols);
    // body
    m_data[row] = set_bit(m_data[row], (m_cols - 1) - col, val);
  }

  std::uint32_t encode(std::uint32_t const x) const {
    std::uint32_t res = 0x0;
    for(std::size_t i = 0; i < m_rows; ++ i) {
      res |= sum_bits_mod2(x & m_data[i]) << i;
    }
    return res;
  }
  
private:
  std::size_t m_rows;
  std::size_t m_cols;
  std::vector<std::uint32_t> m_data;
};

std::ostream& operator << (std::ostream& out, bit_matrix const& m) {
  for(std::size_t row = 0; row < m.rows(); ++ row) {
    for(std::size_t col = 0; col < m.cols(); ++ col) {
      out << m.get(row, col);
    }
    out << "\n";
  }
  return out;
}

/*
  example input:
    7 4
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1
    0 1 1 1
    1 0 1 1
    1 1 0 1
*/
bit_matrix load_bit_matrix(std::istream& in) {
  std::size_t rows{0}, cols{0};
  in >> rows >> cols;
  throw_assert(0 < rows && rows < 32);
  throw_assert(0 < cols && cols < 32);
  bit_matrix m(rows, cols);
  for(std::size_t row = 0; row < rows; ++ row) {
    for(std::size_t col = 0; col < cols; ++ col) {
      std::size_t val{0};
      in >> val;
      m.set(row, col, val);
    }
  }
  return m;
}

int main () {
  try {
    // input
    std::size_t test_case_count{0};
    std::cin >> test_case_count;
    throw_assert(test_case_count > 0);

    std::vector<bit_matrix> generator_matrices;
    
    for(std::size_t i = 0; i < test_case_count; ++ i) {
      generator_matrices.push_back(load_bit_matrix(std::cin));
    }

    // process
    std::vector<std::size_t> counts;
    for(auto const& m : generator_matrices) {
      std::uint32_t const bound = 0x1 << m.cols();
      std::size_t count = 32; // max possible bound -- actually it is m.rows() but what the hey!
      for(std::uint32_t str = 1; str < bound; ++ str) {
        auto curr_count = hamming_distance(0x0, m.encode(str)); // same as count_bits(m.encode(str))
        count = curr_count < count ? curr_count : count;
      }
      counts.push_back(count);
    }

    // output
    for(auto const c : counts) {
      std::cout << c << "\n";
    }
  }
  catch(std::exception const& e) {
    std::cerr << "error: " << e.what() << "\n";
  }
  return 0;
}
