//
//
//
#include <cassert>
#include <tuple>
#include <variant>

#include <cctype>
#include <vector>
#include <type_traits>

/*
    TODO: basic idea
    TODO: Concepts in use

    Concepts:
        
        parser
        many_combiner
        cat_combiner
        
    TODO: generalize over char type (see if you can support binary blob parsing)

*/

// TODO: change name of this to parcomb
namespace parser {

    /*
     *
     */
    enum class error {
        unexpected_end_of_input,
        unexpected_token
    };

    /*
     *
     */    
    struct in_place_t {};
    constexpr in_place_t const in_place;

    template<class T>
        // requires semi_regular<T>
        //       && forward_iterator<It>
    class parse_value {
    public: // types
        using value_type = T;
    
    private: // types

        struct success {
            value_type value;
        };

    public: // ctor
    
        parse_value() = default;
        parse_value(parse_value const&) = default;
        parse_value(parse_value&&) = default;
        ~parse_value() = default;

        auto operator= (parse_value const&) -> parse_value& = default;
        auto operator= (parse_value&&) -> parse_value& = default;

        parse_value(error err)
            : m_result{err}
        {}

        parse_value(in_place_t, value_type value)
            : m_result{ success{ .value = std::move(value) } }
        {}
        
    public: // observers

        auto is_success() const -> bool {
            return std::holds_alternative<success>(m_result);
        }

        auto is_error() const -> bool {
            return std::holds_alternative<error>(m_result);
        }

        auto success_value() const& -> value_type const& {
            assert(is_success());
            return std::get<success>(m_result).value;
        }

        auto success_value() && -> value_type&& {
            assert(is_success());
            return std::get<success>(std::move(m_result)).value;
        }

        auto error_value() const -> error {
            assert(is_error());
            return std::get<error>(m_result);
        }
        
    private: // fields
        std::variant<error, success> m_result;
    };

    template<class It, class T>
    using parse_result = std::tuple<
            It,
            parse_value<T>
        >;

    /*
     * combiners
     */
    template<class T>
    struct void_combiner {
        using value_type = std::monostate;

        auto init() const -> value_type {
            return {};
        }

        template<class T1>
        auto combine(value_type, T1&&) const -> value_type {
            return {};
        }
    };

    template<class T>
    struct vector_collector {
        using value_type = std::vector<T>;

        auto init() const -> value_type {
            return {};
        }

        template<class T1>
        auto combine(value_type value, T1&& x) -> value_type {
            value.push_back(std::forward<T1>(x));
            return value;
        }
    };
    
    /*
     *
     */
    struct lit {
        //
        using value_type = std::monostate; // TODO: change to char type

        //
        lit() = default;
        lit(lit const&) = default;
        ~lit() = default;
        
        auto operator= (lit const&) -> lit& = default;
        
        explicit lit(char c)
            : m_c{c}
        {}

        //
        template<class It, class Sent>
            // requires forward_iterator<It>
            //       && sentinel<It, Sent>
        auto parse(It it, Sent sent) const -> parse_result<It, value_type> {
            if(it == sent) {
                return { it, { error::unexpected_end_of_input } };
            }

            if(*it != m_c) {
                return { it, { error::unexpected_token }};
            }

            ++ it;

            return {it, {in_place, {}} };
        }

        //
        char m_c{'\0'};
    };

    // TODO: generalize to any character class 
    struct digit {
        //
        using value_type = int;

        //
        template<class It, class Sent>
            // requires forward_iterator<It>
            //       && sentinel<It, Sent>
        auto parse(It it, Sent sent) const -> parse_result<It, value_type> {
            if(it == sent) {
                return { it, { error::unexpected_end_of_input } };
            }

            if( !std::isdigit(*it) ) {
                return { it, { error::unexpected_token }};
            }

            value_type const value = *it - '0'; 
            ++ it;
            
            return { it, { in_place, value } };
        }
    };

    // TODO: add many1
    template<
        class Parser,
        class Combiner = vector_collector<typename Parser::value_type>
        >
    struct many {
        //
        using value_type = typename Combiner::value_type;
        
        //
        many() = default;
        many(many const&) = default;
        ~many() = default;

        auto operator= (many const& ) -> many& = default;
        
        explicit many(Parser parser, Combiner combiner = {})
            : m_parser{std::move(parser)}
            , m_combiner{std::move(combiner)}
        {}
        
        //
        template<class It, class Sent>
            // requires forward_iterator<It>
            //       && sentinel<It, Sent>
        auto parse(It it, Sent sent) const -> parse_result<It, value_type> {
            auto value = m_combiner.init();

            while(true) {
                auto [it1, r] = m_parser.parse(it, sent);

                if( r.is_error() ) {
                    break;
                }

                it = it1;
                value = m_combiner.combine(
                    std::move(value),
                    std::move(r).success_value()
                );
            }
            
            return {it, { in_place, std::move(value) } };
        }

        //
        Parser   m_parser;
        Combiner m_combiner;
    };

    // TODO: add combiner
    template<class ... Parsers>
    struct cat {
        static_assert( sizeof...(Parsers) > 1);
        
        //
        using value_type = std::tuple< typename Parsers::value_type ... >;
    
        //
        cat() = default;
        cat(cat const&) = default;
        ~cat() = default;

        cat(Parsers... parsers)
            : m_parsers{ std::move(parsers)... }
        {}

        //
        template<class It, class Sent>
            // requires forward_iterator<It>
            //       && sentinel<It, Sent>
        auto parse(It it, Sent sent) const -> parse_result<It, value_type> {
            return parse_nth<0>(it, sent, std::tuple<>{});
        }

        //
        template<std::size_t N, class Tup, class It, class Sent>
        auto parse_nth(It it, Sent sent, Tup tup) const -> parse_result<It, value_type> {
            if constexpr (N == sizeof...(Parsers)) {
                return {
                    it,
                    { in_place, std::move(tup) }
                };
            } else {
                auto&& current_parser = std::get<N>(m_parsers);
                auto [it1, r] = current_parser.parse(it, sent);
                if( r.is_error() ) {
                    return { it, { r.error_value() } };
                }
                return parse_nth<N + 1>(
                    it1, sent,
                    std::tuple_cat(
                        std::move(tup),
                        std::tuple{ std::move(r).success_value() }
                    )
                );
            }
        }

        //
        std::tuple<Parsers ...> m_parsers;
    };

    // TODO: alternative parser
    
    // TODO: try parser
    
    // TODO: list parser
} // parser

//
//
//
#include <iostream>
#include <string>
#include <string_view>

using namespace std::literals;

struct number_combiner {
    using value_type = int;

    auto init() const -> value_type {
        return 0;
    }

    auto combine(value_type acc, int x) const -> value_type {
        return acc*10 + x;
    }
};

auto main() -> int {
    using namespace parser;

    auto const test_str = "10+20*30"sv;

    auto const number_par = many{ digit{}, number_combiner{}  };
    auto const par = cat{ number_par, lit{'+'} };
    
    auto [it, r] = par.parse(test_str.begin(), test_str.end());

    if( r.is_success() ) {
        std::cout << std::string{ it, test_str.end() } << "\n";
        std::cout << std::get<0>( r.success_value() ) << "\n";
    } else {
        std::cout << "bug!\n";
    }
    
    return 0;
}
