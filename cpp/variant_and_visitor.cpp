//
// Visitor
//
#include <vector>
#include <memory>
#include <iterator>
#include <initializer_list>
#include <iostream>

namespace vis {
  //
  class Shape;
  class ConstVisitor;
    
  class Point;
  class Line;
  class Triangle;
  class Circle;
  class Block;
    
  //
  class Shape {
  public: 
    virtual ~Shape() = default;
    virtual auto accept(ConstVisitor& visitor) const -> void = 0;
    virtual auto box() const -> std::unique_ptr<Shape> = 0; // maybe clone is better name?
  };
    
  class ConstVisitor {
  public:
    virtual ~ConstVisitor() = default;
    virtual auto visit(Point    const& p) -> void = 0;
    virtual auto visit(Line     const& l) -> void = 0;
    virtual auto visit(Triangle const& t) -> void = 0;
    virtual auto visit(Circle   const& c) -> void = 0;
    virtual auto visit(Block    const& b) -> void = 0;
  };
    
    
  //
  //
  //
  class Point : public Shape {
  public:
  
    Point() = default;
        
    Point(float x, float y)
      : m_x{x}, m_y{y}
    {}
        
    Point(Point const&) = default;
        
    ~Point() = default;
        
    Point& operator= (Point const&) = default;
    
    //
    
    inline auto x() const -> float { return m_x; } 
    
    inline auto y() const -> float { return m_y; }
    
    // Shape
    auto accept(ConstVisitor& visitor) const -> void override {
      visitor.visit(*this);
    }
        
    auto box() const -> std::unique_ptr<Shape> override {
      return std::make_unique<Point>(*this);
    }
        
  private:
    float m_x{0}, m_y{0};
  };
    
  class Line : public Shape {
  public:
  
    Line() = default;
        
    Line(Point a, Point b)
      : m_a{a}, m_b{b}
    {}
    
    Line(Line const&) = default;
        
    ~Line() = default;
        
    Line& operator= (Line const&) = default;
    
    //
    
    inline auto a() const -> Point const& { return m_a; }
    
    inline auto b() const -> Point const& { return m_b; }
    
    // Shape
    auto accept(ConstVisitor& visitor) const -> void override {
      visitor.visit(*this);
    }
        
    auto box() const -> std::unique_ptr<Shape> override {
      return std::make_unique<Line>(*this);
    }
        
  private:
    Point m_a{}, m_b{};
  };
    
  class Triangle : public Shape {
  public:
    //
    Triangle() = default;
        
    Triangle(Point a, Point b, Point c)
      : m_a{a}, m_b{b}, m_c{c}
    {}
        
    Triangle(Triangle const&) = default;
        
    ~Triangle() = default;
        
    Triangle& operator= (Triangle const& ) = default;
        
    //
        
    inline auto a() const -> Point const& { return m_a; }
        
    inline auto b() const -> Point const& { return m_b; }
        
    inline auto c() const -> Point const& { return m_c; }
        
    // visitor
    auto accept(ConstVisitor& visitor) const -> void override {
      visitor.visit(*this);
    }
        
    auto box() const -> std::unique_ptr<Shape> override {
      return std::make_unique<Triangle>(*this);
    }
        
  private:
    Point m_a{}, m_b{}, m_c{};
  };
    
  class Circle : public Shape {
  public:
  
    Circle() = default;
        
    Circle(Point center, float radius)
      : m_center{center}
      , m_radius{radius}
    {}
        
    Circle(Circle const&) = default;
        
    ~Circle() = default;
        
    Circle& operator= (Circle const&) = default;
    
    //
    
    inline auto center() const-> Point const& { return m_center; }
        
    inline auto radius() const -> float { return m_radius; }
    
    // Shape
    auto accept(ConstVisitor& visitor) const -> void override {
      visitor.visit(*this);
    }
        
    auto box() const -> std::unique_ptr<Shape> override {
      return std::make_unique<Circle>(*this);
    }
        
  private:
    Point m_center{};
    float m_radius{0};
  };
    
  namespace detail {
        
    //
    template<class It>
    struct DerefConstIter {
    
      explicit DerefConstIter(It it)
        : m_it{it}
      {}

      DerefConstIter(DerefConstIter const&) = default;

      ~DerefConstIter() = default;

      DerefConstIter& operator= (DerefConstIter const&) = default;
      
      auto operator++ () -> DerefConstIter {
        m_it++;
        return *this;
      }
    
      auto operator++ (int) -> DerefConstIter {
        auto prev = *this;
        m_it++;
        return prev;
      }
    
      auto operator* () -> decltype(auto) {
        return **m_it;
      }
    
      auto operator-> () -> decltype(auto) {
        return **m_it;
      }
    
      friend bool operator== (DerefConstIter const& a, DerefConstIter const& b) {
        return a.m_it == b.m_it;
      }
    
      friend bool operator!= (DerefConstIter const& a, DerefConstIter const& b) {
        return !(a == b);
      }
      
      It m_it;
    };
    
    template<class It>
    struct DerefConstRange {
    
      DerefConstRange(It begin, It end)
        : m_begin{begin}
        , m_end{end}
      {}
        
      auto begin() -> DerefConstIter<It> { return m_begin; }
      auto end()   -> DerefConstIter<It> { return m_end;   }
            
      DerefConstIter<It> m_begin, m_end;
    };
    
    //
    struct Inserter {
      explicit Inserter(std::vector< std::unique_ptr<Shape>> & shapes)
        : m_shapes{shapes}
      {}
        
      template<class H, class ... Ts>
      auto insert(H&& h, Ts&& ... ts) -> void {
        m_shapes.push_back(h.box());
        insert(std::forward<Ts>(ts)...);
      }
            
      auto insert() -> void {}
            
      std::vector< std::unique_ptr<Shape> >& m_shapes;
    };
  }
    
  class Block : public Shape {
  public:
  
    using const_range = detail::DerefConstRange<
                          std::vector< std::unique_ptr<Shape> >::const_iterator
                        >;
    
    //
    template<class ... Ts>
    explicit Block(Ts&& ... ts) {
      m_shapes.reserve(sizeof...(Ts));
      detail::Inserter inserter{m_shapes};
      inserter.insert(std::forward<Ts>(ts)...);
    }
    
    Block(Block const& other) {
      m_shapes.reserve(other.m_shapes.size());
      for(auto&& shape: other.shapes()) {
        m_shapes.push_back(shape.box());
      }
    }
        
    Block(Block&& ) = default;
          
    ~Block() = default;
        
    Block& operator= (Block const& other) {
      if(this != &other) {
        m_shapes.clear();
        m_shapes.reserve(other.m_shapes.size());
        for(auto&& shape: other.shapes()) {
          m_shapes.push_back(shape.box());
        }
      }
      return *this;
    }
        
    Block& operator= (Block&& other) = default;
    
    //
    inline auto shapes() const -> const_range { 
      return const_range{m_shapes.begin(), m_shapes.end()}; 
    }
    
    // Shape
    auto accept(ConstVisitor& visitor) const -> void override {
      visitor.visit(*this);
    }
        
    auto box() const -> std::unique_ptr<Shape> override {
      return std::make_unique<Block>(*this);
    }
        
  private:
    std::vector< std::unique_ptr< Shape > > m_shapes;
  };
    
  //
  //
  //
  namespace detail {
    //
    struct Spaces {
      int count{0};
    };
        
    std::ostream& operator<< (std::ostream& out, Spaces const& s) {
      for(int i = 0; i < s.count; i++) {
        out << ' ';
      }
      return out;
    }
        
    //
    struct Draw : public ConstVisitor {
        
      explicit Draw(std::ostream& out = std::cout)
        : m_out{out}
      {}
            
      //
      auto visit (Point const& p) -> void override {
        m_out << Spaces{m_level} << "Point{" << p.x() << ", " << p.y() << "}\n";
      }
        
      auto visit (Line const& l) -> void override{
        m_out << Spaces{m_level} << "Line{\n";
        m_level += 2;
        l.a().accept(*this);
        l.b().accept(*this);
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";   
      }
            
      auto visit (Triangle const& t) -> void override {
        m_out << Spaces{m_level} << "Triangle{\n";
        m_level += 2;
        t.a().accept(*this);
        t.b().accept(*this);
        t.c().accept(*this);
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";
      }
            
      auto visit (Circle const& c) -> void override {
        m_out << Spaces{m_level} 
              << "Circle{ " 
                << "Point{" << c.center().x() << ", " << c.center().y() << "}" 
                << ", " << c.radius() 
              << "}\n";
      }
            
      auto visit (Block const& b) -> void override {
        m_out << Spaces{m_level} << "Block{\n";
        m_level += 2;
        for(auto&& shape: b.shapes()) {
          shape.accept(*this);
        }
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";
      }
            
      // will match anything not yet added
      template<class T>
      auto visit (T const&) -> void {
        m_out << Spaces{m_level} << "Unknown node type\n";    
      }
        
      //
      std::ostream& m_out;
      int           m_level{0};
    };
  }
    
  auto draw(std::ostream& out, Shape const& shape) {
    detail::Draw draw{out};
    shape.accept(draw);
  }
    
  //
  //
  //
  auto test() -> void {
    std::cout << "visitor:\n";
    auto shape =
      Block{ Point{1, 1}
           , Line{{-1,-1}, {1,1}}
           , Triangle{{-1, 0}, {1, 0}, {0, 1}}
           , Circle{}
           , Block{ Point{-1, 1}, Point{1, -1} } 
      };
    draw(std::cout, shape);
  }
}

//
//
//
#include <variant>
#include <vector>
#include <initializer_list>
#include <memory>
#include <iostream>

namespace var {
    
  struct Shape;
    
  struct Point {
    float x{0}, y{0};
  };
    
  struct Line {
    Point a{}, b{};
  };
    
  struct Triangle {
    Point a{}, b{}, c{};
  };
    
  struct Circle {
    Point center{};
    float radius{1};
  };
    
  class Block {
  public:
     
    Block() = default;
     
    Block(std::initializer_list<Shape> shapes)
      : m_shapes{shapes}
    {}
     
    Block(Block const&) = default;
     
    Block(Block&& ) = default;
     
    ~Block() = default;
     
    Block& operator= (Block const&) = default;

    Block& operator= (Block&&) = default;

  public:
    
    auto shapes() const -> std::vector<Shape> const& {
      return m_shapes;
    }
    
  private:
    std::vector<Shape> m_shapes;
  };
    
    
  class Shape {
  public:
  
    Shape() = default;
        
    template<class T> // T is one of shapes
    Shape(T shape)
      : m_shape{std::move(shape)}
    {}
        
    Shape(Shape const&) = default;
        
    Shape(Shape &&) = default;
        
    ~Shape() = default;
        
    Shape& operator= (Shape const& ) = default;
        
    Shape& operator= (Shape&& ) = default;
        
    template<class T>
    Shape& operator= (T&& shape) {
      m_shape = std::forward<T>(shape);
      return *this;
    }

  public:

    template<class V>
    auto accept(V&& visitor) {
      return std::visit(std::forward<V>(visitor), m_shape);
    }

    template<class V>
    auto accept(V&& visitor) const {
      return std::visit(std::forward<V>(visitor), m_shape);
    }

  private:
    std::variant< Point
                , Line
                , Triangle
                , Circle
                , Block
                > m_shape;
  };
    
  //
  // drawing
  //
        
  namespace detail {
        
    //
    struct Spaces {
      int count{0};
    };
        
    std::ostream& operator<< (std::ostream& out, Spaces const& s) {
      for(int i = 0; i < s.count; i++) {
        out << ' ';
      }
      return out;
    }
        
    //
    struct Draw {
    
      explicit Draw(std::ostream& out = std::cout)
        : m_out{out}
      {}
            
      //
      auto operator() (Point const& p) -> void {
        m_out << Spaces{m_level} << "Point{" << p.x << ", " << p.y << "}\n";
      }
        
      auto operator() (Line const& l) -> void {
        m_out << Spaces{m_level} << "Line{\n";
        m_level += 2;
        (*this)(l.a);
        (*this)(l.b);
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";   
      }
            
      auto operator() (Triangle const& t) -> void {
        m_out << Spaces{m_level} << "Triangle{\n";
        m_level += 2;
        (*this)(t.a);
        (*this)(t.b);
        (*this)(t.c);
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";
      }
            
      auto operator() (Circle const& c) -> void {
        m_out << Spaces{m_level} 
              << "Circle{ " 
                << "Point{" << c.center.x << ", " << c.center.y << "}" 
                << ", " << c.radius 
              << "}\n";
      }
            
      auto operator() (Block const& b) -> void {
        m_out << Spaces{m_level} << "Block{\n";
        m_level += 2;
        for(auto&& shape: b.shapes()) {
          shape.accept(*this);
        }
        m_level -= 2;
        m_out << Spaces{m_level} << "}\n";
      }
            
      template<class T>
      auto operator() (T const&) -> void {
        m_out << Spaces{m_level} << "Unknown node type\n";    
      }
        
      //
      std::ostream& m_out;
      int           m_level{0};
    };
  }
    
  auto draw(std::ostream& out, Shape const& s) {
    detail::Draw d;
    s.accept(d);
  }
    
  //
  //
  //
  auto test() -> void {
    std::cout << "variant:\n";
    Shape shape { Block { Point{1, 1}
                        , Line{{-1,-1}, {1,1}}
                        , Triangle{{-1, 0}, {1, 0}, {0, 1}}
                        , Circle{}
                        , Block{ Point{-1, 1}, Point{1, -1} } 
                        }
                };
    draw(std::cout, shape);
  }
}

auto main() -> int {
  vis::test();
  var::test();
}