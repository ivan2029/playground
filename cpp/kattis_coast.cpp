/*

  https://open.kattis.com/problems/coast

  example input:

    5 6
    011110
    010110
    111000
    000010
    000000

  output:

    20

*/


#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <stdexcept>
#include <string>

#if defined (NDEBUG)
  #define throw_assert(expr) ((void)0)
#else
  template<class File, class Line>
  void throw_assert_fn(File file, Line line, std::string msg) {
    throw std::runtime_error(std::string(file) + ":" + std::to_string(line) + ": " + msg);
  }
  #define throw_assert(expr) if( !(expr) ) throw_assert_fn(__FILE__, __LINE__, #expr);
#endif

template<class T>
class grid {
public:
  using iterator = typename std::vector<T>::iterator;
  using const_iterator = typename std::vector<T>::const_iterator;
  
public:
  grid() = default;
  grid(grid const&) = default;
  grid(grid&&) = default;
  ~grid() = default;
  grid& operator= (grid const&) = default;
  grid& operator= (grid&&) = default;

  grid(std::size_t rows, std::size_t cols)
    : m_rows{rows}, m_cols{cols}, m_els(rows*cols)
  {
    throw_assert(rows > 0);
    throw_assert(cols > 0);
  }

public:

  T& el(std::size_t row, std::size_t col) {
    throw_assert(row < m_rows);
    throw_assert(col < m_cols);
    return m_els[row*m_cols + col];
  }

  T const& el(std::size_t row, std::size_t col) const {
    throw_assert(row < m_rows);
    throw_assert(col < m_cols);
    return m_els[row*m_cols + col];
  }

  std::size_t rows() const { return m_rows; }
  std::size_t cols() const { return m_cols; }

  iterator begin() { return m_els.begin(); }
  iterator end()   { return m_els.end(); }

  const_iterator begin() const { return m_els.begin(); }
  const_iterator end() const   { return m_els.end(); }
  
private:
  std::size_t     m_rows = 0;
  std::size_t     m_cols = 0;
  std::vector<T>  m_els;
};

template<class T>
std::ostream& operator << (std::ostream& out, grid<T> const& grid_) {
  for(std::size_t row = 0; row < grid_.rows(); ++ row) {
    for(std::size_t col = 0; col < grid_.cols(); ++ col) {
      out << grid_.el(row, col);
    }
    out << "\n";
  }
  return out;
}

grid<char> load_grid(std::istream& in) {
  std::size_t rows{0}, cols{0};

  in >> rows >> cols;
  
  throw_assert( 0 < rows && rows < 1001 );
  throw_assert( 0 < cols && cols < 1001 );

  // assumes row-major representation
  grid<char> grid_(rows, cols);
  std::copy_n( std::istream_iterator<char>(in), rows*cols, grid_.begin() );

  return grid_;
}

struct coord {
  std::size_t row, col;
};

template<class T, class P>
std::size_t westmost_if( grid<T> const& grid_
                       , std::size_t const row, std::size_t const col
                       , P pred )
{
  std::size_t west = col;
  while(west > 0 && pred( grid_.el(row, west - 1) ) ) {
    -- west;
  }
  return west;
}

template<class T, class P>
std::size_t eastmost_if( grid<T> const& grid_
                       , std::size_t const row, std::size_t const col
                       , P pred )
{
  std::size_t east = col;
  while(east + 1 < grid_.cols() && pred( grid_.el(row, east + 1) ) ) {
    ++ east;
  }
  return east;
}


void flood_fill( grid<char>& grid_
               , std::size_t const row, std::size_t const col
               , char const target, char const replacement )
{ 
  throw_assert(target != replacement);

  if(target != grid_.el(row, col)) {
    return;
  }
  
  std::vector<coord> queue_ = { coord{row, col} };

  while( !queue_.empty() ) {
    std::vector<coord> new_queue_;
    for(auto const& curr: queue_) {
      if(grid_.el(curr.row, curr.col) != target) continue;
      auto const west = westmost_if(grid_, curr.row, curr.col, [=](char const c){ return c == target; });
      auto const east = eastmost_if(grid_, curr.row, curr.col, [=](char const c){ return c == target; });
      for(std::size_t col = west; col <= east; ++ col) {
        grid_.el(curr.row, col) = replacement;
        if(curr.row != grid_.rows() - 1 && target == grid_.el(curr.row + 1, col)) {
          new_queue_.push_back(coord{curr.row + 1, col});
        }
        if(curr.row != 0 && target == grid_.el(curr.row - 1, col)) {
          new_queue_.push_back(coord{curr.row - 1, col});
        }
      }
    }
    queue_ = std::move(new_queue_);
  }
  
  /* recursive solution
  if(target != grid_.el(row, col)) {
    return;
  }
  grid_.el(row, col) = replacement;
  if( row + 1 != grid_.rows() ) flood_fill(grid_, row + 1,     col, target, replacement);
  if( row != 0 )                flood_fill(grid_, row - 1,     col, target, replacement);
  if( col + 1 != grid_.cols() ) flood_fill(grid_,     row, col + 1, target, replacement);
  if( col != 0 )                flood_fill(grid_,     row, col - 1, target, replacement);
  */
}

template<class Act>
void over_grid_border(std::size_t const rows, std::size_t const cols, Act act) {
  std::size_t const last_row = rows - 1;
  std::size_t const last_col = cols - 1;
  for(std::size_t col = 0; col < last_col ; ++ col) {
    act(0, col);
  }
  for(std::size_t row = 0; row < last_row; ++ row) {
    act(row, last_col);
  }
  for(std::size_t col = last_col; col > 0; -- col) {
    act(last_row, col);
  }
  for(std::size_t row = last_row; row > 0; -- row) {
    act(row, 0);
  }
}

template<class Act>
void over_grid_nodes(std::size_t const rows, std::size_t const cols, Act act) {
  for(std::size_t row = 0; row < rows; ++ row) {
    for(std::size_t col = 0; col < cols; ++ col) {
      act(row, col);
    }
  }
}

void mark_seas(grid<char>& grid_) {
  over_grid_border( grid_.rows(), grid_.cols()
                  , [&](std::size_t const row, std::size_t const col) {
                      flood_fill(grid_, row, col, '0', 's');
                  });
}

std::size_t shore_edges(grid<char> const& grid_, std::size_t const row, std::size_t const col) {
  if( '1' != grid_.el(row, col) ) return 0;

  std::size_t count = 0;

  // check east, west, south and north, if they lie within grid
  if( row != 0                && 's' == grid_.el(row - 1,     col) ) ++ count;
  if( row != grid_.rows() - 1 && 's' == grid_.el(row + 1,     col) ) ++ count;
  if( col != 0                && 's' == grid_.el(    row, col - 1) ) ++ count;
  if( col != grid_.cols() - 1 && 's' == grid_.el(    row, col + 1) ) ++ count;

  // check east, west, south and north, if they lies outside of grid
  if( row == 0 ) ++ count;
  if( row == grid_.rows() - 1) ++ count;
  if( col == 0 ) ++ count;
  if( col == grid_.cols() - 1) ++ count;

  //
  return count;
}

// run mark_seas first!!!
std::size_t shore_length(grid<char> const& grid_) {
  std::size_t length = 0;
  over_grid_nodes( grid_.rows(), grid_.cols()
                 , [&](std::size_t const row, std::size_t const col) {
                   length += shore_edges(grid_, row, col);
                 });
  return length;
}

int main() {
  try {
    auto grid_ = load_grid(std::cin);
    mark_seas(grid_);
    std::cout << shore_length(grid_) << "\n";
  }
  catch(std::exception const& e) {
    std::cerr << "error: " << e.what() << "\n";
  }
  
  return 0;
}
