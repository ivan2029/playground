/*
    Naive implementation
*/

#include <cassert>
#include <thread>
#include <memory>
#include <initializer_list>

namespace __detail {
    template<class T> struct Node;
    template<class T> using NodePtr = std::shared_ptr< Node<T> >;
    
    template<class T>
    struct Node {
        T          value;
        NodePtr<T> next;
    };
} // __detail

template<class T>
class List {
private:
    using Node = __detail::Node<T>;
    using NodePtr = __detail::NodePtr<T>;
    
    template<class U> friend class List;
    
    List(NodePtr node) 
        : m_node{std::move(node)}
    {}
    
public:
    List() = default;
    List(List const&) = default;
    List(List&&) = default;
    ~List(){
        for(; m_node ; m_node = m_node->next) {}
    }
    List& operator= (List const&) = default;
    List& operator= (List&&) = default;
    
    template<class U>
    List(std::initializer_list<U> el) {
        for(std::size_t i = 0; i < el.size(); ++ i) {
            auto new_node = std::make_shared<Node>();
            new_node->value = el.begin()[el.size() - (i + 1)];
            new_node->next = std::move(m_node);
            m_node = std::move(new_node);
        }
    }
    
public:
  
    List<T> push(T const& value) const {
        auto new_node = std::make_shared<Node>();
        new_node->value = value;
        new_node->next = m_node;
        return List<T>(new_node);
    }
    
    List<T> push(T&& value) const {
        auto new_node = std::make_shared<Node>();
        new_node->value = std::move(value);
        new_node->next = m_node;
        return List<T>(new_node);
    }
    
    bool is_empty() const {
        return nullptr != m_node;
    }
    
    std::size_t size() const {
        std::size_t s = 0;
        for_each([&](auto const&){ ++s; });
        return s;
    }
    
    T at(std::size_t pos) const {
        auto node = node_at(pos);
        return node->value;        
    }
    
    T head() const {
        assert(m_node);
        return m_node->value;
    }
    
    List<T> tail() const {
        assert(m_node);
        return List<T>(m_node->next);
    }
    
    List<T> remove_at(std::size_t pos) const {
        if(pos == 0) {
            return tail();
        }
        else {
            auto node = node_at(pos);
            
            NodePtr new_head;
            Node*   new_last;
            
            for(auto curr = m_node; curr != node; curr = curr->next) {
                if(!new_head) {
                    new_head = std::make_shared<Node>();
                    new_last = new_head.get();
                }
                else {
                    new_last->next = std::make_shared<Node>();
                    new_last = new_last->next.get();
                }
                new_last->value = curr->value;
            }
            
            new_last->next = node->next;
        
            return List<T>(std::move(new_head));
        }
    }
    
    template<class Act>
    void for_each(Act action) const {
        for(auto current = m_node.get(); current; current = current->next.get()){
            auto const& value = current->value; // values in list must be constant to outside world
            action(value);
        }
    }
    
    List<T> reverse() const {
        List<T> ts;
        for_each([&](auto el) {
            ts = ts.push(std::move(el));
        });
        return ts;
    }
    
    template<class Pred>
    List<T> filter(Pred predicate) const {
        NodePtr new_head;
        Node*   new_last = nullptr;
        
        for_each([&](auto const& el) {
            if( predicate(el) ) {
                if(!new_head) {
                    new_head = std::make_shared<Node>();
                    new_last = new_head.get();
                }
                else {
                    new_last->next = std::make_shared<Node>();
                    new_last = new_last->next.get();
                }
                new_last->value = el;
            }
        });
        
        return List<T>(std::move(new_head));
    }
    
    template<class Tr>
    auto map(Tr transform) const -> List<decltype(transform(std::declval<T>()))> {
        using U = decltype(transform(std::declval<T>()));
        __detail::NodePtr<U> new_head;
        __detail::Node<U>*   new_last = nullptr;
        
        for_each([&](auto const& el) {
            if(!new_head) {
                new_head = std::make_shared<__detail::Node<U>>();
                new_last = new_head.get();
            }
            else {
                new_last->next = std::make_shared<__detail::Node<U>>();
                new_last = new_last->next.get();
            }
            new_last->value = transform(el);
        });
        
        return List<U>(std::move(new_head));
    }
    
    template<class Fold, class U>
    U fold_left(Fold folding, U init) const {
        U acc = std::move(init);
        
        for_each([&](auto const& el) {
            acc = folding(acc, el);
        });
        
        return acc;
    }

private:
    
    NodePtr node_at(std::size_t pos) const {
        assert( pos < size() );
        auto node = m_node;
        for(std::size_t index = 0; index < pos && node; ++index, node = node->next) {}
        return node;
    }
        
private:
    NodePtr      m_node;
};


#include <iostream>
#include <string>

auto print = [](auto const& x) { std::cout << x << ", "; }; 

int main(){
    List<int> xs{1,2,3,4,5,6,7,8,9,10};

    // prints: 10
    std::cout << xs.size() << "\n";

    // prints: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
    xs.for_each(print);
    std::cout << "\n";

    // prints: 5
    std::cout << xs.at(5) << "\n";
    
    // prints: 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 
    auto reversed = xs.reverse();
    reversed.for_each(print);
    std::cout << "\n";
    
    // prints: 1, 3, 5, 7, 9, 
    auto filtered = xs.filter([](int x) { return (x&1) != 0; });
    filtered.for_each(print);
    std::cout << "\n";
    
    // prints: <1>, <2>, <3>, <4>, <5>, <6>, <7>, <8>, <9>, <10>,
    auto mapped = xs.map([](auto x) { return "<" + std::to_string(x) + ">"; });
    mapped.for_each(print);
    std::cout << "\n";
    
    // prints: 55
    auto sum = xs.fold_left([](int acc, int el) { return acc + el; }, 0);
    std::cout << sum << "\n";
    
    // prints: 1, 2, 3, 4, 5, 7, 8, 9, 10, 
    auto removed = xs.remove_at(5);
    removed.for_each(print);
    std::cout << "\n";
    
    // prints: -1, -2, -3, -4, 
    List<int> js;
    {
        List<int> is{-1,-2,-3,-4};
        js = is;
        for(int i = 0; i < 1000000; ++ i) {
            is = is.push(i);
        }
    }
    js.for_each(print);

    
    return 0;    
}