
template<class ... > struct DeducedType;

#define MIXIN_DEFAULT_NO_DEFAULT_CTOR(name)  \
name(name const&) = default;                 \
name(name&&) = default;                      \
~name() = default;                           \
name& operator= (name const&) = default;     \
name& operator= (name&&) = default

#define MIXIN_DEFAULT(name)         \
name() = default;                   \
MIXIN_DEFAULT_NO_DEFAULT_CTOR(name)

#include <cassert>
#include <exception>
#include <memory>
#include <vector>
#include <algorithm>
#include <functional>
#include <utility>

/*
    Basic Observable contract: on_next are called zero or more times until one of 
    on_error or on_complete are called. on_error and on_complete can be called 
    exactly once.

    All types must be semi regular! Observables should be cheap to copy (similar to standard iterators)!

    O is Observer of T if:
        let { O obs; T val; }
        in {
            obs.on_next(val) -> void
            obs.on_error(exception_ptr{}) -> void
            obs.on_complete() -> void
        }

    O is Observable of T, given Observer<Ob, T>:
        let { O obs; Ob ob; }
        in {
            obs.subscribe(ob) -> Connection
        }

    TODO: define concepts: Observer, Observable, Connection, Subscriber
    TODO: define traits: observer_traits, observable_traits
*/

namespace observable {
  struct NullConn {
    void unsubscribe() {}
  };
} // observable

namespace observable {
  
  namespace __unsub_guard {

    // concept
    struct UnsubscribeConcept {
      virtual ~UnsubscribeConcept() = default;
      virtual void unsubscribe() = 0;
    };

    // model
    template<class Sub>
    struct UnsubscribeModel : public UnsubscribeConcept {
      explicit UnsubscribeModel(Sub sub)
        : m_sub(std::move(sub)) 
      {}

      void unsubscribe() override {
        m_sub.unsubscribe();
      }

      Sub m_sub;
    };

    // ptr
    struct UnsubscribeConceptDeleter {
      void operator()(UnsubscribeConcept* concept_) {
        if(!concept_) return;
        concept_->unsubscribe();
        std::default_delete<UnsubscribeConcept>{}(concept_);
      }
    };

    using UnsubscribeConceptPtr = std::unique_ptr<UnsubscribeConcept, UnsubscribeConceptDeleter>;


  } // __unsub_guard

  
  
  class UnsubGuard {
  public:

    UnsubGuard() = default;
    UnsubGuard(UnsubGuard const&) = delete;
    UnsubGuard(UnsubGuard&&) = default;
    ~UnsubGuard() = default;

    UnsubGuard& operator= (UnsubGuard const&) = delete;
    UnsubGuard& operator= (UnsubGuard&& ) = default;

    template<class Sub>
    UnsubGuard(Sub sub) 
      : m_sub( new __unsub_guard::UnsubscribeModel< Sub > (std::move(sub)) )
    {}

    template<class Sub>
    UnsubGuard& operator= (Sub sub) {
      m_sub.reset( new __unsub_guard::UnsubscribeModel<Sub>(std::move(sub)) );
      return *this;
    }

  public:

    void detach() {
      m_sub.reset();
    }

  private:
    __unsub_guard::UnsubscribeConceptPtr m_sub;
  };

} // observable

namespace observable::__detail {

  template<class T>
  struct ObserverConcept
  {
    virtual ~ObserverConcept() = default;
    virtual void on_next(T) = 0;
    virtual void on_error(std::exception_ptr) = 0;
    virtual void on_complete() = 0;
  };

  template<class T, class O>
  struct ObserverModel 
    : public ObserverConcept<T> {

    explicit ObserverModel(O obs)
      : m_obs(std::move(obs))
    {}

    void on_next(T t) override {
      m_obs.on_next(t);
    }

    void on_error(std::exception_ptr e) {
      m_obs.on_error(e);
    }

    virtual void on_complete() {
      m_obs.on_complete();
    }

    O m_obs;
  };

  template<class T> 
  using ObserverConceptPtr = std::unique_ptr< ObserverConcept<T> >;

  template<class T, class Obs>
  auto make_observer_concept(Obs&& obs) {
    return ObserverConceptPtr<T>{
      std::make_unique<ObserverModel< T, std::decay_t<Obs> > >(std::forward<Obs>(obs))
    };
  }

} // observable::__detail

namespace observable::sources {

  template<class Cont>
  class Vec {
  public:

    explicit Vec(Cont const& cont)
      : m_cont(&cont)
    {}

    MIXIN_DEFAULT_NO_DEFAULT_CTOR(Vec);

  public: // Observable

    template<class Obs> // requires Observer
    auto subscribe(Obs obs) {
      for(auto const& el: *m_cont) {
        obs.on_next(el);
      }
      obs.on_complete();
      return NullConn{};
    }

  private:

    Cont const* m_cont;
  };

  template<class Cont, class Builder>
  auto operator| (Vec<Cont> vec, Builder builder) {
    return builder(vec);
  }

  template<class Cont>
  auto make_vec(Cont&& cont) {
    return Vec< std::decay_t<Cont> >(std::forward<Cont>(cont));
  }
} // observable::sources

namespace observable::sources {
  
  namespace __stream {
    
    //
    //
    template<class T>
    struct StreamImpl;

    //
    //
    template<class T>
    class Unsubscribe {
      friend StreamImpl<T>;

    private:
      Unsubscribe(std::weak_ptr< StreamImpl<T> > stream, __detail::ObserverConcept<T>* observer)
        : m_stream(std::move(stream))
        , m_observer(observer)
      {}

    public:
      
      MIXIN_DEFAULT(Unsubscribe);

    public:
      void unsubscribe();

    private:
      std::weak_ptr< StreamImpl<T> >  m_stream;
      __detail::ObserverConcept<T>*   m_observer = nullptr;
    };

    //
    //
    template<class T>
    struct StreamImpl
      : public std::enable_shared_from_this< StreamImpl<T> > {

      StreamImpl() = default;
        
      StreamImpl(StreamImpl const&) = delete;
      StreamImpl(StreamImpl&&) = delete;

      ~StreamImpl() {
        for(auto const& o: m_observers){
        o->on_complete();
        }
      }

      StreamImpl& operator= (StreamImpl const&) = delete;
      StreamImpl& operator= (StreamImpl&&) = delete;

      //
      void on_next_(T t) {
        for(auto const& o: m_observers) {
          o->on_next(t);
        }
      }

      //
      template<class Obs>
      auto subscribe_(Obs obs) {
        auto wrapped = __detail::make_observer_concept<T>(std::forward<Obs>(obs));
        Unsubscribe<T> conn( this->shared_from_this(), wrapped.get() );
        m_observers.push_back(std::move(wrapped));
        return conn;
      }

      void unsubscribe_(__detail::ObserverConcept<T>* observer) {
        assert( observer );
        auto it = std::find_if( m_observers.begin(), m_observers.end()
                              , [=](auto const& obs){ return observer == obs.get(); } );
        if(it != m_observers.end()) {
          m_observers.erase(it);
        }
      }

      //
      std::vector< __detail::ObserverConceptPtr<T> > m_observers;
    };

    //
    //
    template<class T>
    void Unsubscribe<T>::unsubscribe() {
        if(!m_observer) return;
        if(auto shared = m_stream.lock()){
            shared->unsubscribe_(m_observer);
        }
    }

  } // __stream

  //
  //
  template<class T>
  class Sink;

  //
  //
  template<class T>
  class Stream {

    friend class Sink<T>;

  public:

    using Connection = __stream::Unsubscribe<T>;

  private:

    explicit Stream(std::weak_ptr<__stream::StreamImpl<T> > weak_stream)
      : m_weak_stream(std::move(weak_stream))
    {}

  public:

    MIXIN_DEFAULT(Stream);

  public:

    template<class Obs>
    auto subscribe(Obs obs) -> Connection {
      if(auto shared = m_weak_stream.lock()){
        return shared->subscribe_(std::forward<Obs>(obs));
      }
      else{
        return {};
      }
    }

  private:
    std::weak_ptr<__stream::StreamImpl<T> > m_weak_stream;
  };

  template<class T, class Builder>
  auto operator| (Stream<T> stream, Builder builder) {
    return builder(stream);
  }

  //
  //
  //
  template<class T>
  class Sink {
  public:
    Sink()
    : m_stream_impl( std::make_shared< __stream::StreamImpl<T> >() )
    {}

    MIXIN_DEFAULT_NO_DEFAULT_CTOR(Sink);

  public:

    auto stream() { 
      assert(m_stream_impl);
      return Stream<T>(m_stream_impl); 
    }

    void push(T t) {
      assert(m_stream_impl);
      m_stream_impl->on_next_(std::move(t));
    }

  private:
    std::shared_ptr< __stream::StreamImpl<T> > m_stream_impl;
  };

} // observable::sources

namespace observable::sources {

  namespace __cell {

    //
    //
    template<class T>
    struct CellImpl;

    //
    //
    template<class T>
    class Unsubscribe {
      friend CellImpl<T>;

    private:
      Unsubscribe(std::weak_ptr< CellImpl<T> > stream, __detail::ObserverConcept<T>* observer)
        : m_stream(std::move(stream))
        , m_observer(observer)
      {}

    public:
        
      Unsubscribe() = default;
      Unsubscribe(Unsubscribe const&) = delete;
      Unsubscribe(Unsubscribe&&) = default;
      ~Unsubscribe() = default;

      Unsubscribe& operator= (Unsubscribe const&) = delete;
      Unsubscribe& operator= (Unsubscribe&&) = default;

    public:
      
      void unsubscribe();

    private:
      std::weak_ptr< CellImpl<T> >  m_stream;
      __detail::ObserverConcept<T>* m_observer = nullptr;
    };

    //
    //

    template<class T>
    struct CellImpl
      : public std::enable_shared_from_this< CellImpl<T> >
    {
      explicit CellImpl(T value)
        : m_value(std::move(value))
      {}

      CellImpl() = default;
        
      CellImpl(CellImpl const&) = delete;
        
      CellImpl(CellImpl&&) = delete;

      ~CellImpl() {
        for(auto const& o: m_observers){
          o->on_complete();
        }
      }

      CellImpl& operator= (CellImpl const&) = delete;
      CellImpl& operator= (CellImpl&&) = delete;

      //
      T const& get() {
        return m_value;
      }

      void set(T value) {
        m_value = std::move(value);
        on_next_(m_value);
      }

      //
      void on_next_(T t) {
        for(auto const& o: m_observers){
          o->on_next(t);
        }
      }

      //
      template<class Obs>
      auto subscribe_(Obs obs) {
        auto wrapped = __detail::make_observer_concept<T>(std::move(obs));
        Unsubscribe<T> conn( this->shared_from_this(), wrapped.get() );
        wrapped->on_next(m_value);
        m_observers.push_back(std::move(wrapped));
        return conn;
      }

      void unsubscribe_(__detail::ObserverConcept<T>* observer) {
        assert( observer );
        auto it = std::find_if( m_observers.begin(), m_observers.end()
                              , [=](auto const& obs){ return observer == obs.get(); } );
        if(it != m_observers.end()) {
          m_observers.erase(it);
        }
      }

      //
      T                                              m_value;
      std::vector< __detail::ObserverConceptPtr<T> > m_observers;
    };

    //
    //
    template<class T>
    void Unsubscribe<T>::unsubscribe()    {
      if(!m_observer) return;
      if(auto shared = m_stream.lock()) {
        shared->unsubscribe_(m_observer);
      }
    }

  } // __cell

  //
  //
  template<class T>
  class CellStream {
  public:

    using Connection = __cell::Unsubscribe<T>;
  
  public:

    explicit CellStream(std::weak_ptr< __cell::CellImpl<T> > impl)
      : m_impl(std::move(impl))
    {}
  
    MIXIN_DEFAULT(CellStream);
  
  public:

    template<class Obs>
    auto subscribe(Obs obs) -> Connection {
      if(auto shared = m_impl.lock()){
        return shared->subscribe_(std::move(obs));
      }
      else{
        return {};
      }
    }

  private:
    std::weak_ptr< __cell::CellImpl<T> > m_impl;
  };

  template<class T, class Builder>
  auto operator| (CellStream<T> stream, Builder builder){
    return builder(stream);
  }

  //
  //
  template<class T>
  class Cell {
  public:
    explicit Cell(T value)
      : m_impl(std::make_shared<__cell::CellImpl<T> >(std::move(value)))
    {}

    Cell()
      : m_impl(std::make_shared<__cell::CellImpl<T> >())
    {}

    Cell(Cell const&) = delete;
    Cell(Cell&&) = default;
    ~Cell() = default;

    Cell& operator= (Cell const&) = delete;
    Cell& operator= (Cell&&) = default;

  public:

    T const& get() const {
      assert(m_impl);
      return m_impl->get();
    }

    void set(T value) {
      assert(m_impl);
      m_impl->set(std::move(value));
    }

  public:

    auto stream() {
      return CellStream<T>(m_impl);
    }

  private:

    std::shared_ptr< __cell::CellImpl<T> > m_impl;
  };

} // observable::sources

namespace observable::observers {

  template<class Fn>
  class FnObserver {
  public:
    explicit FnObserver(Fn fn)
      : m_fn(std::move(fn))
    {}

    MIXIN_DEFAULT_NO_DEFAULT_CTOR(FnObserver);

  public: // Observer
    
    template<class T>
    void on_next(T&& t){
      m_fn( std::forward<T>(t) );
    }

    void on_error(std::exception_ptr){}
    
    void on_complete(){}

  private:
    Fn m_fn;
  };

  template<class Fn>
  auto from_fn(Fn fn) {
    return FnObserver<Fn>(std::move(fn));
  }

}

namespace observable::op { 

  template<class Obs>
  auto subscribe(Obs obs) {
    return [obs=std::move(obs)](auto o) {
      return o.subscribe(obs);
    };
  }

  template<class Fn>
  auto for_each(Fn fn) {
    return [fn=std::move(fn)](auto o) {
      return o.subscribe(observers::from_fn(fn));
    };
  }

} // observable::op

namespace observable::op {
    
  namespace __map {

    template<class Fn, class Obs>
    class Map {
    public:
      Map(Fn fn, Obs obs)
        : m_fn(std::move(fn))
        , m_obs(std::move(obs))
      {}

      MIXIN_DEFAULT_NO_DEFAULT_CTOR(Map);

    public:

      template<class T>
      void on_next(T value) {
        m_obs.on_next(m_fn(std::move(value)));
      }

      void on_error(std::exception_ptr e){
        m_obs.on_error(e);
      }
    
      void on_complete(){
        m_obs.on_complete();
      }

    private:
      Fn  m_fn;
      Obs m_obs;
    };

    template<class Fn, class SourceObs>
    class MapSubscriber {
    public:

      MapSubscriber(Fn fn, SourceObs source)
        : m_fn(std::move(fn))
        , m_source(std::move(source))
      {}

      MIXIN_DEFAULT_NO_DEFAULT_CTOR(MapSubscriber);

    public:

      template<class Obs>
      auto subscribe(Obs obs) {
        return m_source.subscribe(Map<Fn, Obs>(m_fn, std::move(obs)));
      }

    private:
      Fn         m_fn;
      SourceObs  m_source;
    };

    template<class Fn, class Obs, class Builder>
    auto operator| (MapSubscriber<Fn, Obs> m, Builder builder) {
      return builder(m);
    }

  } // __map

  template<class Fn>
  auto map(Fn fn) {
    return [fn=std::move(fn)](auto o) {
      return __map::MapSubscriber<Fn, decltype(o)>(fn, std::move(o));
    };
  }

} // observable::op

namespace observable::op {

  namespace __filter {

    template<class Fn, class Obs>
    class Filter {
    public:
      Filter(Fn fn, Obs obs)
        : m_fn(std::move(fn))
        , m_obs(std::move(obs))
      {}

      MIXIN_DEFAULT_NO_DEFAULT_CTOR(Filter);

    public:

      template<class T>
      void on_next(T&& value) {
        if( m_fn(value) ) {
          m_obs.on_next( std::forward<T>(value) );
        }
      }

      void on_error(std::exception_ptr e){
        m_obs.on_error(e);
      }
    
      void on_complete(){
        m_obs.on_complete();
      } 

    private:
      Fn  m_fn;
      Obs m_obs;
    };

    template<class Fn, class SourceObs>
    class FilterSubscriber {
    public:

      FilterSubscriber(Fn fn, SourceObs source)
        : m_fn(std::move(fn))
        , m_source(std::move(source))
      {}

      MIXIN_DEFAULT_NO_DEFAULT_CTOR(FilterSubscriber);

    public:

      template<class Obs>
      auto subscribe(Obs obs) {
        return m_source.subscribe(Filter<Fn, Obs>(m_fn, std::move(obs)));
      }

    private:
      Fn         m_fn;
      SourceObs  m_source;
    };

    template<class Fn, class Obs, class Builder>
    auto operator| (FilterSubscriber<Fn, Obs> m, Builder builder) {
      return builder(m);
    }
  }// __filter

  template<class Fn>
  auto filter(Fn fn) {
    return [fn=std::move(fn)](auto o) {
      return __filter::FilterSubscriber<Fn, decltype(o)>(fn, std::move(o));
    };
  }
}

namespace observable::op {
	namespace __scan {
		template<class Fn, class T, class Obs>
		class Scan {
		public:
			Scan(Fn folding, T init_val, Obs obs)
				: m_folding(folding)
				, m_val(std::move(init_val))
				, m_obs(std::move(obs))
			{}

      MIXIN_DEFAULT_NO_DEFAULT_CTOR(Scan);

		public:
			template<class T1>
      void on_next(T1&& value) {
				m_val = m_folding(m_val, value);
				m_obs.on_next(m_val);
      }

      void on_error(std::exception_ptr e){
        m_obs.on_error(e);
      }
    
      void on_complete(){
        m_obs.on_complete();
      }
			
		private:
			Fn  m_folding;
			T   m_val;
			Obs m_obs;
		};

		template<class Fn, class T, class SourceObs>
		class ScanSubscriber {
		public:

			ScanSubscriber(Fn folding, T init_val, SourceObs source)
				: m_folding(folding)
				, m_init_val(std::move(init_val))
				, m_source(std::move(source))
			{}

		
      MIXIN_DEFAULT_NO_DEFAULT_CTOR(ScanSubscriber);

		public:

			template<class Obs>
      auto subscribe(Obs obs) {
        return m_source.subscribe(Scan<Fn, T, Obs>( m_folding
				                                          , std::move(m_init_val)
																									, std::move(obs)));
      }
			
		private:
			Fn         m_folding;
			T          m_init_val;
			SourceObs  m_source;
		};

		template<class Fn, class T,  class Obs, class Builder>
    auto operator| (ScanSubscriber<Fn, T, Obs> m, Builder builder) {
      return builder(m);
    }
	} // __scan


	template<class Fn, class T>
	auto scan(Fn folding, T init_val) {
		return [fn=std::move(folding), iv=std::move(init_val)](auto o) {
			return __scan::ScanSubscriber<Fn, T, decltype(o)>(fn, std::move(iv), std::move(o));
		};
	}
}

namespace observable::op {
	namespace __switcher {
		template<class Obs>
		class Switcher {
		public:
			explicit Switcher(Obs obs)
				: m_obs(std::move(obs))
			{}

		public:
		  template<class O>
			void on_next(O o){
				auto obs = m_obs;
				m_guard = o | op::for_each([obs](auto x) mutable {
					obs.on_next(x);
				});
      }

      void on_error(std::exception_ptr e){
				m_guard.detach();
				m_obs.on_error( e );
			}
    
      void on_complete(){
				m_guard.detach();
				m_obs.on_complete();
			}
			
		private:
			Obs         m_obs;
			UnsubGuard  m_guard;
		};
	
	
		template<class SourceObs>
		class SwitcherSubscriber {
		public:
			explicit SwitcherSubscriber(SourceObs source)
				: m_source(std::move(source))
			{}
			
		  MIXIN_DEFAULT_NO_DEFAULT_CTOR(SwitcherSubscriber);
					
		public:

			template<class Obs>
			auto subscribe(Obs obs) {
				return m_source.subscribe(Switcher<Obs>(std::move(obs)));
			}
		
		private:
			SourceObs m_source;
		};

		template< class Obs, class Builder>
    auto operator| (SwitcherSubscriber<Obs> m, Builder builder) {
      return builder(m);
    }
	} // __switcher

	auto switcher() {
		return [](auto o) {
			return __switcher::SwitcherSubscriber<decltype(o)>(std::move(o));
		};
	}
}



/////////////////////////////////////*/////////////////////////////////////////////////////////////
////////////////////////////////////***////////////////////////////////////////////////////////////
///////////////////////////////////*****///////////////////////////////////////////////////////////
////////////////////////////////////|||////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////


#include <cassert>
#include <iostream>
#include <string>

#include <vector>
#include <functional>
#include <sstream>

#ifdef _WIN32
#include <windows.h>
#endif

template<class T> void maybe_ignored(T&&){}

//
//
//
enum class Color {
    Default, Red, Green
};

class ConsoleTextManip
{
public:
    ConsoleTextManip()
    {
#ifdef _WIN32
        m_console = ::GetStdHandle(STD_OUTPUT_HANDLE);

        CONSOLE_SCREEN_BUFFER_INFO console_info;
        if(::GetConsoleScreenBufferInfo(m_console, &console_info)){
            m_default_color = console_info.wAttributes;
        }
#endif
    }

    void set_text_color(Color color)
    {
#ifdef _WIN32
        int color_impl = 0;
        switch(color){
        case Color::Green : color_impl = 2; break;
        case Color::Red   : color_impl = 4; break;
        default: color_impl = m_default_color;
        }
        ::SetConsoleTextAttribute(m_console, color_impl);
#else
				maybe_ignored(color);
#endif
    }

private:
#ifdef _WIN32
    ::HANDLE  m_console = nullptr;
    int       m_default_color = 0;
#endif
};

//
//
//

struct Test {
    std::string            name;
    std::function<void()>  fn;
};

static std::vector<Test> g_tests;

static void add_test(std::string name, std::function<void()> fn) {
    g_tests.push_back({std::move(name), std::move(fn)});
}

static void run_all_tests() {
    ConsoleTextManip cmanip;
    for(auto& test: g_tests) {
        std::cout << "test " << test.name;
        try {
            test.fn();
            cmanip.set_text_color(Color::Green);
            std::cout << " SUCCEEDED";
            cmanip.set_text_color(Color::Default);
        }
        catch(std::exception const& e){
            cmanip.set_text_color(Color::Red);
            std::cout << " FAILED ";
            cmanip.set_text_color(Color::Default);
            std::cout << "at \n  " << e.what();
        }
        std::cout << "\n";
    }
}

template<class File, class Line, class Str>
void throw_at(File file, Line line, Str str) {
    std::stringstream out;
    out << file << ":" << line << ":  " << str;
    throw std::logic_error(out.str());
}

#define ASSERT(expr)  if( !(expr) ) throw_at(__FILE__, __LINE__, #expr)

#define TEST(test_name) void test_ ## test_name();                       \
struct t_Test_ ## test_name {                                            \
    t_Test_ ## test_name(){ add_test( #test_name, test_ ## test_name); } \
};                                                                       \
t_Test_ ## test_name instance_ ## test_name ;                            \
void test_ ## test_name ()

//
int main() {
    run_all_tests();
    return 0;
}

//
//
//


template<class Cont>
auto push_back(Cont& cont) {
  return [&](auto elem) {
    cont.push_back(elem);
  };
}

template<class Cont>
auto push_back_observable(Cont& cont) {
  return observable::observers::from_fn([&](auto elem){
    cont.push_back(elem);
  });
}

constexpr auto&& print = [](auto const& x) {
	std::cout << x << ", ";
};

TEST(subscribe_to_vec) {
  using namespace observable;

  std::vector<int> const expected = {1,2,3,4};
  std::vector<int> actual;

  auto vec = sources::make_vec(expected);

  auto fn_obs = observers::from_fn(push_back(actual));
  vec.subscribe(fn_obs);

  ASSERT( expected == actual );
}

TEST(op_subscribe_to_vec) {
  using namespace observable;

  std::vector<int> const expected = {1,2,3,4};
  std::vector<int> actual;

  auto vec = sources::make_vec(expected);

  auto fn_obs = observers::from_fn(push_back(actual));
  vec | op::subscribe(fn_obs);

  ASSERT( expected == actual );
}

TEST(op_for_each_to_vec) {
  using namespace observable;

  std::vector<int> const expected = {1,2,3,4};
  std::vector<int> actual;

  auto vec = sources::make_vec(expected);

  auto fn = push_back(actual);
  vec | op::for_each(fn);

  ASSERT( expected == actual );
}

TEST(op_map_to_vec) {
  using namespace observable;

  std::vector<int> const source = {1,2,3,4};
  std::vector<std::string> const expected = {"1","2","3","4"};
  std::vector<std::string> actual;

  auto vec = sources::make_vec(source);

  auto tr = [](int x) { return std::to_string(x); };
  auto fn = push_back(actual);
  vec
		| op::map(tr)
		| op::for_each(fn) ;

  ASSERT( expected == actual );
}

TEST(op_filter_to_vec) {
  using namespace observable;

  std::vector<int> const source = {1,2,3,4};
  std::vector<int> const expected = {2,4};
  std::vector<int> actual;

  auto vec = sources::make_vec(source);

  auto pred = [](int x) { return x%2 == 0; };
  auto fn = push_back(actual);
  vec | op::filter(pred) | op::for_each(fn);

  ASSERT( expected == actual );
}

TEST(op_filter_map_to_vec) {
  using namespace observable;

  std::vector<int> const source = {1,2,3,4};
  std::vector<std::string> const expected = {"2","4"};
  std::vector<std::string> actual;

  auto vec = sources::make_vec(source);

  auto pred = [](int x) { return x%2 == 0; };
  auto tr = [](int x) { return std::to_string(x); };
  auto fn = push_back(actual);
  vec
		| op::filter(pred)
		| op::map(tr)
		| op::for_each(fn) ;

  ASSERT( expected == actual );
}

TEST(op_for_each_to_stream_int) {
  using namespace observable;

  std::vector<int> const expected = {1,2};
  std::vector<int> actual;

  sources::Sink<int> sink;

  auto fn = push_back(actual);
  auto conn = sink.stream() | op::for_each(fn);

  sink.push(1);
  sink.push(2);

  conn.unsubscribe();

  sink.push(3);
  sink.push(4);
  
  ASSERT( expected == actual );
}

TEST(op_for_each_to_stream_int_with_unsub_guard) {
  using namespace observable;

  std::vector<int> const expected = {1,2};
  std::vector<int> actual;

  sources::Sink<int> sink;

  {
    auto fn = push_back(actual);
    UnsubGuard conn = sink.stream() | op::for_each(fn) ;

    sink.push(1);
    sink.push(2);
  }

  sink.push(3);
  sink.push(4);
  
  ASSERT( expected == actual );
}

TEST(op_filter_map_to_stream_int_with_unsub_guard) {
  using namespace observable;

  std::vector<std::string> const expected = {"2"};
  std::vector<std::string> actual;

  sources::Sink<int> sink;

  {
    auto pred = [](int x) { return x%2 == 0; };
    auto tr = [](int x) { return std::to_string(x); };
    auto fn = push_back(actual);
    
    UnsubGuard conn =
			sink.stream()
			| op::filter(pred)
			| op::map(tr)
			| op::for_each(fn) ;

    sink.push(1);
    sink.push(2);
  }

  sink.push(3);
  sink.push(4);
  
  ASSERT( expected == actual );
}

TEST(op_for_each_to_cell_int) {
  using namespace observable;

  std::vector<int> const expected = {1,2};
  std::vector<int> actual;

  sources::Cell<int> cell(1);

  {
    auto fn = push_back(actual);
    UnsubGuard conn =
			cell.stream()
			| op::for_each(fn) ;

    cell.set(2);

  }

  cell.set(3);

  ASSERT( expected == actual );
}

TEST(op_scan_int) {
	using namespace observable;

	std::vector<int> const source = {1,2,3,4,5};
	std::vector<int> const expected = { 1
	                                  , 1 + 2
																		, 1 + 2 + 3
																		, 1 + 2 + 3 + 4
																		, 1 + 2 + 3 + 4 + 5 };
	std::vector<int> actual;
	
	sources::make_vec(source)
		| op::scan(std::plus<int>{}, 0)
		| op::for_each(push_back(actual)) ;

	ASSERT( expected == actual );
}

class Replicate {
public:
	Replicate(int value, int times)
		: m_value(value)
		, m_times(times)
	{
		assert(times > 0);
	}

  MIXIN_DEFAULT_NO_DEFAULT_CTOR(Replicate);
	
public:

	template<class Obs>
	auto subscribe(Obs obs) {
		for(int n = 0; n < m_times; ++ n) {
			obs.on_next(m_value);
		}
		obs.on_complete();
		return observable::NullConn{};
	}

private:
	int m_value;
	int m_times;
};

template<class Builder>
auto operator| (Replicate repl, Builder builder) {
  return builder(repl);
}

constexpr auto&& rep = [](int n) {
	assert(n > 0);
	return Replicate(n, n);
};

TEST(op_switcher) {
	using namespace observable;

	std::vector<int> const source = {1,2,3,4};
	std::vector<int> const expected = {1,2,2,3,3,3,4,4,4,4};
	std::vector<int> actual;

	sources::make_vec(source)
		| op::map(rep)
		| op::switcher()
		| op::for_each(push_back(actual));

	ASSERT(expected == actual);
}

TEST(op_switcher_2) {
	using namespace observable;
	namespace src = observable::sources;
	
	std::vector<int> const expected = {1,2,1};
	std::vector<int> actual;

	src::Sink<int> s1, s2;
	src::Cell<src::Stream<int>> cell(s1.stream());

	cell.stream()
		| op::switcher()
		| op::for_each(push_back(actual));

	s1.push(1);

	cell.set(s2.stream());

	s1.push(1);
	s2.push(2);

	cell.set(s1.stream());

	s1.push(1);
 	s2.push(2);	
	
	ASSERT(expected == actual);
}
