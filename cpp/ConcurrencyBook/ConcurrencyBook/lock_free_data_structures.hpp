#pragma once

#include "std.hpp"

namespace threadsafe {

    //
    //
    //
    template<class T>
    class StackLockFree {
    public: // ctor, dtor, assign

        StackLockFree() = default;

        StackLockFree(StackLockFree const& ) = delete;

        StackLockFree(StackLockFree&& ) = delete;

        ~StackLockFree() = default;

        StackLockFree& operator= (StackLockFree const& ) = delete;

        StackLockFree& operator= (StackLockFree&& ) = delete;

    public:

    private:
    };


}
