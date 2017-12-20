/*
  Requires C++17 (tested with gcc 7.2) and boost 1.66 .
  Compile command (change paths as needed):

  g++ <THIS FILE> \
      -std=c++17 -Wall -Wextra -pedantic \
      -isystem <PATH TO STLAB> \
      -isystem <PATH TO BOOST INCLUDES> \
      -L <PATH TO BOOST LIBS> \
      -lboost_system \
      -lboost_coroutine \
      -lboost_context \
      -lboost_thread \
      -pthread
*/

#include <iostream>
#include <string>
#include <chrono>
using namespace std::literals;

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#define BOOST_COROUTINES_NO_DEPRECATION_WARNING
#include <boost/asio/spawn.hpp>
namespace asio = boost::asio;
using boost::asio::ip::tcp;

#include <boost/beast.hpp>
namespace beast = boost::beast;
namespace http = boost::beast::http;

#include <boost/core/typeinfo.hpp>

#include <stlab/concurrency/future.hpp>

//
auto test_timer_1(asio::io_context& ioc) -> void {
  asio::spawn( ioc, [&](asio::yield_context yield) {
    asio::steady_timer first  { ioc, 2s };
    asio::steady_timer second { ioc, 4s };
    asio::steady_timer third  { ioc, 6s };

    boost::system::error_code ec;

    //
    first.async_wait(yield[ec]);
    if(ec) return;
    
    std::cout << "1: first expired\n";

    //
    second.async_wait(yield[ec]);
    if(ec) return;
    
    std::cout << "1: second expired\n";

    //
    third.async_wait(yield[ec]);
    if(ec) return;
    
    std::cout << "1: third expired\n";
  });
}

//
auto test_timer_2(asio::io_context& ioc) -> void {
  asio::spawn( ioc, [&](asio::yield_context yield) {
    try {
      asio::steady_timer first  { ioc, 1s };
      asio::steady_timer second { ioc, 3s };
      asio::steady_timer third  { ioc, 5s };

      //
      first.async_wait(yield);
      std::cout << "2: first expired\n";

      //
      second.async_wait(yield);
      std::cout << "2: second expired\n";

      //
      third.async_wait(yield);
      std::cout << "2: third expired\n";
    }
    catch(std::exception const& e) {
      std::cerr << "error: " << e.what() << "\n";
    }
  });
}

//
auto test_get_google(asio::io_context& ioc) -> void {
  asio::spawn(ioc, [&](asio::yield_context yield) {
    char const* const host = "www.google.com";
    char const* const port = "80";

    try {
      tcp::resolver resolver{ioc};
      tcp::socket   socket{ioc};

      auto resolve_result = resolver.async_resolve(host, port, yield);

      asio::async_connect(socket, resolve_result, yield);

      //
      http::request<http::empty_body> request {http::verb::get, "/", 11};
      request.set(http::field::host, host);
      request.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);

      http::async_write(socket, request, yield);

      //
      http::response<http::dynamic_body> response;

      beast::flat_buffer buffer;

      http::async_read(socket, buffer, response, yield);

      //
      std::cout << response << "\n";
    }
    catch(std::exception const& e) {
      std::cerr << "error: " << e.what() << "\n";
    }
  });
}

//
struct asio_executor_adaptor {
  explicit asio_executor_adaptor(asio::executor executor)
    : m_executor{executor}
  {}

  auto operator() (stlab::task<void()> task) -> void {
    asio::post(m_executor, [task=std::move(task)] () mutable {
      task();
    });
  }

  asio::executor m_executor;
};

auto test_stlab_future(asio::executor executor) -> void {
  asio_executor_adaptor adaptor(executor);
  auto [task, f1] = stlab::package<int()>(adaptor, []{
    return 42;
  });

  f1
    .then([](int x) { return "<"s + std::to_string(x) + ">"s; })
    .then([](std::string const& s){ std::cout << ("### "s + s + "\n"); })
    .detach();
      
  task();
}

//
auto main() -> int {
  //
  asio::io_context ioc;
  auto work_guard = asio::make_work_guard(ioc);

  asio::thread_pool workers{};
  
  //
  asio::signal_set kill_signal_set{ ioc };
  kill_signal_set.add(SIGINT);
  kill_signal_set.add(SIGTERM);
  
  kill_signal_set.async_wait([&](boost::system::error_code const& ec, int /*sig*/) {
    if( !ec ) {
      work_guard.reset();
      ioc.stop();
    }
  });
  
  //
  test_timer_1(ioc);
  test_timer_2(ioc);
  test_get_google(ioc);
  test_stlab_future(workers.get_executor());
  
  //
  ioc.run();
  return 0;
}
