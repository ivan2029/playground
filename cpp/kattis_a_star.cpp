/*
	https://paradox.kattis.com/problems/paradoxpath
*/
#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include <utility>
#include <tuple>
#include <initializer_list>

struct vec {
  vec() = default;
  vec(int x_, int y_) : x{x_}, y{y_} {}
  int x{0}, y{0};
};

bool operator== (vec const u, vec const v) {
  return u.x == v.x && u.y == v.y;
}

bool operator!= (vec const u, vec const v) {
  return !(u == v);
}

int axis_dist(vec const u, vec const v) {
  return std::abs(u.x - v.x) + std::abs(u.y - v.y);
}

struct map_view {
    
  map_view(unsigned char const* map_, int const width_, int const height_) 
    : map{map_}, width{width_}, height{height_}
  {}
    
  bool in(vec const u) const {
    return 0 <= u.x && u.x < width
        && 0 <= u.y && u.y < height;
  }
    
  int pos(vec const u) const {
    assert(in(u));
    return u.y*width + u.x;
  }
    
  unsigned char el(vec const u) const {
    return map[pos(u)];
  }
    
  vec coord(int const pos) const {
    assert(0 <= pos && pos < size());
    return { pos%width, pos/width }; 
  }
    
  int size() const { return width*height; }
    
  //
  unsigned char const* map{nullptr};
  int const width{0};
  int const height{0};
};

static std::pair<bool, vec> next_neigh ( map_view const map
                                       , std::vector<bool> const& marked
                                       , vec const goal
                                       , vec const u )
{
  bool has_next = false;
  vec next;
  for(auto v: { vec{u.x, u.y - 1}, vec{u.x, u.y + 1}, vec{u.x - 1, u.y}, vec{u.x + 1, u.y}} ) {
    if(!map.in(v)) continue;
    if( map.el(v) == 1 && !marked[map.pos(v)] ) {
      if(has_next) {
        // heuristic
        next = axis_dist(goal, v) < axis_dist(goal, next)
             ? v
             : next ;
      }
      else {
        has_next = true;
        next = v;
      }
    }
  }
  return std::make_pair(has_next, next);    
}

static int find_path( vec const start, vec const goal, map_view const map, int* out, const int out_n ) {
  std::vector<int> path = { map.pos(start) };
  std::vector<bool> marks(map.size(), false); 
  marks[map.pos(start)] = true;
  vec cur = start;
  while(cur != goal) {
    bool has_next;
    vec next;
    std::tie(has_next, next) = next_neigh(map, marks, goal, cur);
    if(has_next) {
      marks[map.pos(next)] = true;
      path.push_back(map.pos(next));
      cur = next;
    }
    else {
      if(path.size() < 2) return -1;
      path.pop_back();
      cur = map.coord(path.back());
    }
  }
    
  for(int i = 1; i < static_cast<int>(path.size()) && i < out_n; ++ i){
    out[i - 1] = path[i]; 
  }
  return static_cast<int>(path.size()) - 1;
}

//
//
//
int FindPath( const int start_x, const int start_y
            , const int goal_x, const int goal_y
            , const unsigned char* map, const int map_w, const int map_h
            , int* out, const int out_n )
{
  return find_path( {start_x, start_y}
	                , {goal_x, goal_y}
									, {map, map_w, map_h}
									, out, out_n  );
}

//#define TEST_VERSION
#if defined(TEST_VERSION)

int main() {
  {
    unsigned char pMap[] = {
      1, 1, 1, 1, 
      0, 1, 0, 1, 
      0, 1, 1, 1
    }; // path of length 3
    int pOutBuffer[12];
    int len = FindPath(0, 0, 1, 2, pMap, 4, 3, pOutBuffer, 12);
    std::cout << len << "\n";
    for(int i = 0; i < len; ++ i) {
      std::cout << pOutBuffer[i] << " ";
    }
    std::cout << "\n";
  }
  {
    unsigned char pMap[] = {
      0, 0, 1, 
      0, 1, 1, 
      1, 0, 1
    }; // path of length 0
    int pOutBuffer[7] = {0};
    int len = FindPath(2, 0, 2, 0, pMap, 3, 3, pOutBuffer, 7);
    std::cout << len << "\n";
    for(int i = 0; i < len; ++ i) {
      std::cout << pOutBuffer[i] << " ";
    }
    std::cout << "\n";
  }
  {
    unsigned char pMap[] = {
      0, 0, 1, 
      0, 1, 1, 
      1, 0, 1
    }; // no path
    int pOutBuffer[7] = {0};
    int len = FindPath(2, 0, 0, 2, pMap, 3, 3, pOutBuffer, 7);
    std::cout << len << "\n";
    for(int i = 0; i < len; ++ i) {
      std::cout << pOutBuffer[i] << " ";
    }
    std::cout << "\n";
  }
  {
    unsigned char pMap[] = {
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 0, 0, 0, 0, 0, 0, 0, 1,
      1, 1, 1, 0, 0, 0, 0, 0, 0, 1,
      1, 0, 1, 1, 0, 1, 1, 1, 0, 1,
      1, 0, 0, 1, 1, 1, 0, 1, 0, 1,
      1, 0, 0, 0, 1, 1, 0, 1, 0, 1,
      1, 0, 0, 0, 0, 0, 0, 1, 0, 1,
      1, 0, 0, 0, 0, 0, 0, 1, 0, 1,
      1, 0, 0, 0, 0, 0, 0, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    }; // expected path length 18 (there is path with length 22 that is produced by A*)
    int pOutBuffer[100] = {0};
    int len = FindPath(0, 0, 9, 9, pMap, 10, 10, pOutBuffer, 100);
    std::cout << len << "\n";
    for(int i = 0; i < len; ++ i) {
      std::cout << pOutBuffer[i] << " ";
    }
    std::cout << "\n";
  }

  return 0;
}
#endif

