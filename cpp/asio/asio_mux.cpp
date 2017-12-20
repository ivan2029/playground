/*
    Multiplexer idea to build on top of boost asio + boost beast...
*/

#include <iostream>
#include <functional>
#include <map>
#include <string>
#include <tuple>

using Uri = std::string;

auto operator"" _uri (const char* str, std::size_t n) -> Uri {
    return Uri{str, str + n};
}

struct Request {
    Uri uri;  
};

struct Response {
    std::string body;    
};

namespace v1 {
    

    
    class Mux {
    public:
    
        using HandlerCallback  = std::function< void(Response) >;
        using Handler          = std::function< void(Request, HandlerCallback) >; 
        using HandlerMap       = std::map< Uri, Handler >;
    
    public:
    
    public:
    
        template<class Handler>
        auto add_handler(Uri const uri, Handler handler) -> void {
            m_handler_map[std::move(uri)] = std::move(handler);
        }
    
        template<class Handler>
        auto serve_one(Request const& request, Handler handler) {
            auto it = m_handler_map.find(request.uri);
            if(it != m_handler_map.end()) {
                it->second(request, std::move(handler));
            }
            else {
                handler(Response{"path not found"});
            }
        }
        
    private:
        HandlerMap m_handler_map;
    };
} //


auto test_v1() -> void {
    using namespace v1;
    
    Mux mux;
    
    mux.add_handler("/test", [](auto const& request, auto handler) {
        handler(Response{"you hit test"});
    });
    
    
    mux.serve_one(Request{"/wrong_path"}, [](auto const& response) {
        std::cout << response.body << "\n";
    });

    mux.serve_one(Request{"/test"}, [](auto const& response) {
        std::cout << response.body << "\n";
    });
}


namespace v2 {

} //

auto test_v2() {
    using namespace v2;
    
    #if 0
    
    auto mux_ = mux()
      .add("/test_1", [](Request const& request, auto handler) {
          handler(Response{"you hit test_1"});
      })
      .add("/test_2", [](Request const& request, auto handler) {
          handler(Response{"you hit test_2"});
      })
      .unmatched([](auto handler) {
          handler(Response{"oy! wrong path!"});
      });
      ;
      
    mux_.serve_one(Request{"/wrong_path"}, [](auto const& response) {
        std::cout << response.body << "\n";
    });

    mux_.serve_one(Request{"/test_1"}, [](auto const& response) {
        std::cout << response.body << "\n";
    });
    
    mux_.serve_one(Request{"/test_2"}, [](auto const& response) {
        std::cout << response.body << "\n";
    });
    
    #endif
}

template<class ... Ts> struct dt;

template<class Tup, class Handler, std::size_t N = 0>
auto serve_one(Tup const& mux, Request const& request, Handler handler) -> void {
    if constexpr ( N == std::tuple_size_v<std::decay_t<Tup>> ) {
        handler(Response{"not found"});
    }
    else {
        auto const& sub_tup = std::get<N>(mux);
        if( std::get<0>(sub_tup) == request.uri ) {
            std::get<1>(sub_tup)(request, std::move(handler));
        }
        else {
            serve_one<Tup, Handler, N + 1>(mux, request, std::move(handler));
        }
    }
}

auto main() -> int {
    test_v1();
    test_v2();
    
    auto mux = std::make_tuple(
        std::tuple( "/test_1"_uri, [](Request const& request, auto handler) {
            handler(Response{"/test_1 success!"});
        }),
        std::tuple( "/test_2"_uri, [](Request const& request, auto handler) {
            handler(Response{"/test_2 success!"});
        })
    );

    serve_one(mux, Request{""_uri}, [](auto const& response) {
        std::cout << response.body << "\n";
    });
    serve_one(mux, Request{"/test_1"_uri}, [](auto const& response) {
        std::cout << response.body << "\n";
    });
}
