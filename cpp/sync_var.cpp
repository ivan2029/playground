#include <mutex>
#include <condition_variable>
#include <optional>

template<class T>
// requires Semiregular<T>
class SyncVar {
public:
  SyncVar() = default;
  SyncVar(SyncVar const&) = delete;
  SyncVar(SyncVar&&) = delete;
  ~SyncVar() = default;

  SyncVar& operator= (SyncVar const&) = delete;
  SyncVar& operator= (SyncVar&&) = delete;

public:
  void put(T value) {
    std::unique_lock lock(m_mutex);
    m_get_cv.wait(lock, [&]{ return !static_cast<bool>(m_data); });
    m_data = std::move(value);
    m_put_cv.notify_one();
  }

  T get() {
    std::unique_lock lock(m_mutex);
    m_put_cv.wait(lock, [&]{ return static_cast<bool>(m_data); });
    auto value = std::move(m_data).value();
    m_data = std::optional<T>{};
    m_get_cv.notify_one();
    return value;
  }

private:
  std::optional<T>         m_data;
  std::mutex               m_mutex;
  std::condition_variable  m_put_cv, m_get_cv;
};

//
//
//

#include <iostream>
#include <cassert>
#include <thread>
#include <vector>

void test_0() {
  constexpr int const COUNT = 100000;
  SyncVar<int> si;

  std::thread producer([&]{
    for(int i = 0; i < COUNT; ++ i) {
      si.put(i);
    }
  });

  std::thread consumer([&]{
    for(int i = 0; i < COUNT; ++ i) {
      int x = si.get();
      assert(x == i);
    }
  });

  producer.join();
  consumer.join();

  //
  si.put(42);
  assert(si.get() == 42);

  // 
  std::cout << "test 0 done\n";
}

void test_1() {
  constexpr int const PRODUCER_COUNT = 100;
  constexpr int const PER_PRODUCER_COUNT = 100;
  
  SyncVar<int> si;
  //
  std::vector<std::thread> producers;
  producers.reserve(PRODUCER_COUNT);
  for(int i = 0; i < PRODUCER_COUNT; ++ i) {
    producers.emplace_back([&si]{
      for(int j = 0; j < PER_PRODUCER_COUNT; ++j) {
        si.put(1);
      }
    });
  }
    
  std::thread consumer([&si]{
    for(int i = 0; i < (PRODUCER_COUNT*PER_PRODUCER_COUNT); ++ i) {
      int x = si.get();
      assert(x == 1);
    }
  });
    
  //
  for(auto& p: producers) p.join();
  consumer.join();
    
  //
  si.put(42);
  assert(si.get() == 42);
  
  // 
  std::cout << "test 1 done\n";
}

int main() {
  test_0();
  test_1();
  
  std::cout << "Done!";

  return 0;
}
