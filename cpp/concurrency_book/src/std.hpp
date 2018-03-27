#pragma once

/*
    Explanation (taken from 'atomic' header):

    #ifndef _ENABLE_ATOMIC_ALIGNMENT_FIX
	static_assert(alignof(_Ty) >= sizeof(_My_int),
		"You've instantiated std::atomic<T> with sizeof(T) equal to 2/4/8 and alignof(T) < sizeof(T). "
		"Before VS 2015 Update 2, this would have misbehaved at runtime. "
		"VS 2015 Update 2 was fixed to handle this correctly, "
		"but the fix inherently changes layout and breaks binary compatibility. "
		"Please define _ENABLE_ATOMIC_ALIGNMENT_FIX to acknowledge that you understand this, "
		"and that everything you're linking has been compiled with VS 2015 Update 2 (or later).");
    #endif 
*/
#define _ENABLE_ATOMIC_ALIGNMENT_FIX 1


#include <cassert>
#include <exception>
#include <stdexcept>

#include <memory>

#include <functional>

#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <utility>
#include <optional>
#include <variant>
#include <tuple>
#include <any>
#include <initializer_list>

#include <thread>
#include <mutex>
#include <shared_mutex>
#include <future>
#include <condition_variable>

#include <atomic>

#include <iostream>
#include <sstream>
#include <iomanip>

#include <cmath>
#include <complex>
#include <random>
#include <algorithm>
#include <numeric>
//#include <execution>

#include <vector>
#include <array>
#include <list>
#include <queue>
#include <deque>
#include <map>
#include <stack>

#include <iterator>

#include <string>
#include <string_view>

#include <chrono>
using namespace std::literals;

#define NOT_YET_IMPLEMENTED() throw std::runtime_error{__FILE__ + ":"s + std::to_string(__LINE__) + ":"s + __FUNCTION__ + ": not yet implemented"s}