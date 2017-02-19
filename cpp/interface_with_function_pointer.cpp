/*
    Tested with gcc 6.2.0
*/

#include <cassert>
#include <typeinfo>
#include <iostream>

class WritableRef {
public:
    // not owning anything so this defaults are fine
    WritableRef() = default;
    WritableRef(WritableRef const&) = default;
    WritableRef(WritableRef && other) = default;    
    WritableRef& operator= (WritableRef const&) = default;
    WritableRef& operator= (WritableRef&&) = default;
    ~WritableRef() = default;

    // 
    template<class T>
    WritableRef(T const& value) 
        : m_ref{ &value }
    {
        m_write_fn = [](std::ostream& out, void const* value) {
            auto value_ = reinterpret_cast<T const*>(value);
            out << typeid(T).name() << ": " << *value_ << "\n";
        };
    }

public:

    bool is_valid() const {
        return m_ref && m_write_fn;
    }

    void write_to(std::ostream& out) const {
        assert(is_valid());
        m_write_fn(out, m_ref);
    }
        
private:
    void const* m_ref = nullptr;
    void (*m_write_fn)(std::ostream&, void const*) = nullptr;
};


void write_to_cout(WritableRef writable) {
    writable.write_to(std::cout);
}

//
// test
//
#include <vector>
#include <list>
#include <string>

// helpers
template<class It, class S>
std::ostream& write_to_stream(std::ostream& out, It first, S sentinel) {
    out << "[";
    if(first != sentinel) {
        out << *(first++);
        for(; first != sentinel; ++ first) {
            out << ", " << *first;
        }
    }
    out << "]";
    return out;
}

template<class T>
std::ostream& operator<< (std::ostream& out, std::vector<T> const& xs) {
    return write_to_stream(out, xs.begin(), xs.end());
}

template<class T>
std::ostream& operator<< (std::ostream& out, std::list<T> const& xs) {
    return write_to_stream(out, xs.begin(), xs.end());
}

// actual test
int main() {
    write_to_cout(42);
    write_to_cout("hello");    
    write_to_cout(std::vector<int>{1,2,3,4,5});
    write_to_cout(std::list<std::string>{"hello", " from ", " gcc ", __VERSION__});
}
