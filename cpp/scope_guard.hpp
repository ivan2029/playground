/*
  Usage example:
  
  {
    auto window = SDL_CreateWindow("Title", 100, 100, 800, 600, SDL_WINDOW_OPENGL);
    assert(window);
    SCOPE_GUARD( SDL_DestroyWindow(window); ); // #1
    
    auto renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    assert(renderer);
    SCOPE_GUARD( SDL_DestroyRenderer(renderer); ); // #2
    
    //
    //
    if(something_went_wrong){
      throw exception; // #2 then #1 executed
    }
    
    ... // do some work
  } // #2 then #1 executed
*/
#include <utility>

namespace __scope_guard
{
  template<class F>
  class ScopeGuard
  {
  public:
  	ScopeGuard(F f)
  		: m_f{std::move(f)}
  	{}

  	ScopeGuard(ScopeGuard const&) = delete;
  	ScopeGuard(ScopeGuard&&) = default;
  
  	~ScopeGuard() { m_f(); }
  	
  	ScopeGuard& operator= (ScopeGuard const&) = delete;
  	ScopeGuard& operator= (ScopeGuard&&) = default;
  
  private:
  	F m_f;
  };
  
  template<class F>
  auto make_scope_guard(F f)
  {
    return ScopeGuard<F>(std::move(f));
  }
}// __scope_guard

#define __TOKEN_CONCAT_impl(x, y) x ## y
#define __TOKEN_CONCAT(x, y) __TOKEN_CONCAT_impl(x, y)

#define __SCOPE_GUARD_impl(counter, ...) auto __TOKEN_CONCAT(__scope_guard, counter) = __scope_guard::make_scope_guard([&]{ __VA_ARGS__ })

#define SCOPE_GUARD( ... ) __SCOPE_GUARD_impl(__COUNTER__, __VA_ARGS__)