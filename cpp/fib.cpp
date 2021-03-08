#include <iostream>
#include <string>
#include <vector>
#include <stack>

using num = std::uint64_t;


auto fib(num n) -> num {
    // $0
    if(n < 2) return n;
    else {
        // $1
        num const a = fib(n - 1);
        // $2 
        num const b = fib(n - 2);
        // $3
        return a + b;
    }
}

auto fib_iter(num n) -> num {
  //  
    struct fib_state {
        //
        num n = 0;
        //
        int state = 0;
        num a = 0;
        num b = 0;
    };

    //
    int r = 0;
    std::stack<fib_state> st;
    
    st.push(fib_state{n});

    //
    while( ! st.empty() ) {
        auto const nc = st.top().n;
        switch(st.top().state) {
        case 0: // $1
            if(nc < 2) {
                r = nc;
                st.pop();
            } else {
                st.top().state = 1;    
            }
            break;
        case 1: // $1
            st.top().state = 2;
            st.push(fib_state{nc - 1});
            break;
        case 2: // $2
            st.top().state = 3;
            st.top().a = r;
            st.push(fib_state{nc - 2});
            break;
        case 3: // $3
            st.top().b = r;
            r = st.top().a + st.top().b;
            st.pop();
            break;
        default:
            break;
        }
    }

    //
    return r;
}

auto fib_good(num n) -> num {
    num prev{0}, next{1};
    for(; n > 0; -- n) {
        auto const new_next = prev + next;
        prev = next;
        next = new_next;
    }
    return prev;
}

namespace __detail {
    auto fib_less_good_inner(num n, num prev, num next) -> num {
        if(n == 0) return prev;
        else       return fib_less_good_inner(n - 1, next, prev + next);
    }
}

auto fib_less_good(num n) -> num {
    return __detail::fib_less_good_inner(n, 0, 1);
}

int main() {
    std::cout << fib(10) << "\n";
    std::cout << fib_iter(10) << "\n";
    std::cout << fib_good(10) << "\n";
    std::cout << fib_less_good(10) << "\n";
}
