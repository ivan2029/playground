/*
	https://open.kattis.com/problems/pizza
*/

#include <cassert>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <limits>

bool between(int x, int y, int t) {
  return x < t && t < y;
}

struct coord { int row, col; };

template<class T>
class grid {
public:
  using iterator       = typename std::vector<T>::iterator;
  using const_iterator = typename std::vector<T>::const_iterator;
  
public:
  grid() = default;
  grid(grid const&) = default;
  grid(grid&&) = default;
  ~grid() = default;
  grid& operator= (grid const&) = default;
  grid& operator= (grid&&) = default;

  grid(int const rows, int const cols)
    : m_rows{rows}, m_cols{cols}, m_els(rows*cols)
  {
    assert(rows > 0);
    assert(cols > 0);
  }

public:

  bool in(int const row, int const col) const {
    return between(-1, m_rows, row)
        && between(-1, m_cols, col);
  }

  T& el(int const row, int const col) {
    assert(in(row, col));
    return m_els[row*m_cols + col];
  }

  T const& el(int const row, int const col) const {
    assert(in(row, col));
    return m_els[row*m_cols + col];
  }

  
  int rows() const { return m_rows; }
  int cols() const { return m_cols; }

  iterator begin() { return m_els.begin(); }
  iterator end()   { return m_els.end(); }

  const_iterator begin() const { return m_els.begin(); }
  const_iterator end() const   { return m_els.end(); }
  
private:
  int             m_rows = 0;
  int             m_cols = 0;
  std::vector<T>  m_els;
};


struct empty_action {
  template<class ... Ts>
  void operator() (Ts&&...) const {}
};

template<class Act, class NL = empty_action>
void over_grid_nodes( int const rows, int const cols
                    , Act act
                    , NL nl = empty_action{} )
{
  for(int row = 0; row < rows; ++ row) {
    for(int col = 0; col < cols; ++ col) {
      act(row, col);
    }
    nl(row);
  }
}

template<class T>
std::ostream& operator<< (std::ostream& out, grid<T> const& grid_) {
  over_grid_nodes( grid_.rows(), grid_.cols()
                 , [&](int const row, int const col) {
                     out << std::setw(4) << grid_.el(row, col) << " ";
                   }
                 , [&](int const) { out << "\n"; } );
  return out;
}

template<class T, class Act>
void for_each_neigh( grid<T> const& grid_
                   , int const row, int const col
                   , Act action )
{
  static coord DIR[8] = {
      {-1, -1}
    , { 0, -1}
    , { 1, -1}
    , { 1,  0}
    , { 1,  1}
    , { 0,  1}
    , {-1,  1}
    , {-1,  0}
  };

  for(auto c: DIR) {
    int row_ = row + c.row;
    int col_ = col + c.col;
    if( grid_.in(row_, col_) ) {
      action(row_, col_);
    }
  };               
}

int dist( int x1, int y1
        , int x2, int y2 )
{
  return std::abs(x1 - x2) + std::abs(y1 - y2);
}

grid<int> load_grid(std::istream& in) {
  int x{0}, y{0};

  in >> x >> y;
  
  assert( between(0, 101, x) );
  assert( between(0, 101, y) );

  // assumes row-major representation
  grid<int> grid_(y, x);
  std::copy_n( std::istream_iterator<int>(in), x*y, grid_.begin() );

  return grid_;
}

int price_from_point( grid<int> const& grid_
                    , int const row_, int const col_ )
{
  int price{0};
  over_grid_nodes( grid_.rows(), grid_.cols()
                 , [&](int const row, int const col) {  
                     if( row == row_ && col == col_ ) return;
                     price += grid_.el(row, col) * dist(row_, col_, row, col);
                   });
  return price;
}

void print_prices(grid<int> const& grid_ , int const mp = 0) {
    over_grid_nodes( grid_.rows(), grid_.cols()
                   , [&](int const row, int const col) {
                       std::cout << std::setw(4)
                                 << price_from_point(grid_, row, col) - mp
                                 << " ";
                     }
                   , [&](int const) { std::cout << "\n"; } );

}


std::pair<int, int> find_max(grid<int> const& grid_) {
  int row{0}, col{0};
  over_grid_nodes( grid_.rows(), grid_.cols()
                 , [&](int const r, int const c) {
                     if( grid_.el(r, c) > grid_.el(row, col) ) {
                       row = r;
                       col = c;
                     }
                   });
  return std::make_pair(row, col);
}

//#define PRINT_SEARCH_PATH

int min_price( grid<int> const& grid_ ) {
	/*
		The idea here is this: we are using gradient descent from node
		that gives biggest income.

		This is done because grid can have more than one local minima, so
		we start from node with biggest income since we expect to put our
		warehouse nearby. This does not guarantee that we will find global
		minima!
	*/


#if defined(PRINT_SEARCH_PATH)
  grid<int> prices(grid_.rows(), grid_.cols());
#endif
	
  int row{0}, col{0}; // starting point
  std::tie(row, col) = find_max(grid_);
  int old_row{-1}, old_col{-1}; // point from previous iteration

  int price{ price_from_point(grid_, row, col) };
  
  while( row != old_row || col != old_col ) {
#if defined(PRINT_SEARCH_PATH)
    prices.el(row, col) = price;
    std::cout << prices << "\n";
#endif
    old_row = row;
    old_col = col;
    for_each_neigh( grid_
                  , row, col
                  , [&](int const row_, int const col_) {                 
                      int const neigh_price = price_from_point(grid_, row_, col_);
                      if( neigh_price < price ) {
                        price = neigh_price;
                        row = row_;
                        col = col_;
                      }
                    });
  }

  return price;
}



void run(std::istream& in) {
  int test_count{0};
  in >> test_count;
  assert( between(0, 21, test_count) );
  for(int i = 0; i < test_count; ++ i) {
    auto grid_ = load_grid(in);
    int const mp = min_price(grid_); 
    std::cout << mp << " blocks\n";
//    print_prices(grid_, mp);
  }
}


//#define TEST_VERSION
#if defined(TEST_VERSION)
#include <sstream>

int main() {
  /* 
    expected output

      55 blocks
      162 blocks
  */
  std::stringstream str(
      " 2"
      " 4 4"
      " 0 8 2 0"
      " 1 4 5 0"
      " 0 1 0 1"
      " 3 9 2 0"
      " 6 7"
      " 0 0 0 0 0 0"
      " 0 1 0 3 0 1"
      " 2 9 1 2 1 2"
      " 8 7 1 3 4 3"
      " 1 0 2 2 7 7"
      " 0 1 0 0 1 0"
      " 0 0 0 0 0 0"
  );
  run(str);
  return 0;
}

#else

int main() {
  run(std::cin);
  return 0;
}

#endif
