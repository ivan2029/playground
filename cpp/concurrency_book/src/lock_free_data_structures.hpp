#pragma once

#include "std.hpp"

namespace threadsafe {

    //
    //
    //
    template<class T>
    class StackLockFree {

        struct Node {
            T     value;
            Node* next{nullptr};
        };

    public: // ctor, dtor, assign

        StackLockFree() = default;

        StackLockFree(StackLockFree const& ) = delete;

        StackLockFree(StackLockFree&& ) = delete;

        ~StackLockFree() = default;

        StackLockFree& operator= (StackLockFree const& ) = delete;

        StackLockFree& operator= (StackLockFree&& ) = delete;

    public:

        template<class T1>
        auto push(T1&& value) -> void {
            NOT_YET_IMPLEMENTED();
        }

        auto try_pop() -> std::optional<T> {
            NOT_YET_IMPLEMENTED();
        }

    private:

    };


}
