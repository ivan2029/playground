#include <cassert>
#include <string>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <cctype>
#include <optional>
#include <variant>


template<class It, class Sent>
auto debug_str(long long const max, It it, Sent sent) -> std::string {
  if(std::distance(it, sent) > max) {
    return std::string(it, it + max) + "...";
  }
  else {
    return std::string(it, sent);
  }
}

template<class It, class Sent, class We>
// requires InputIterator<It>() && Sentinel<It, Sent>
//       && Callable< We >
auto write_sequence( std::ostream& out
                   , char const* lpar, char const* rpar, char const* sep
                   , It it, Sent sent
                   , We write_element 
                   ) -> std::ostream& 
{
  out << lpar;
           
  if(it != sent) {
    write_element(out, *it);
    ++ it;
    for(; it != sent; ++it) {
      out << sep;
      write_element(out, *it);
    }
  }
     
  out << rpar;
  return out;
}

namespace json {

  //
  // 
  // 
  struct value;
  
  inline bool operator== (value const& a, value const& b);
  inline bool operator!= (value const& a, value const& b);

  //
  //
  //
  struct null {};
    
  struct boolean {
    bool val;
  };
  
  struct string {
    std::string val;
  };
  
  struct number {
    std::string val;
  };
      
  struct object {
    std::map<std::string, value> members;
  };
  
  struct array {
    std::vector<value> elements;
  };

  
  //
  //
  //
  struct value {
    std::variant< object
                 , null
                 , boolean
                 , number
                 , string
                 , array     > val;
  };

  //
  // comparisons
  //
  inline bool operator== (null const& , null const& ) {
    return true;
  }

  inline bool operator!= (null const& , null const& ) {
    return false;
  }

  
  inline bool operator== (boolean const& a, boolean const& b) {
    return a.val == b.val;
  }

  inline bool operator!= (boolean const& a, boolean const& b) {
    return a.val != b.val;
  }
  
  inline bool operator== (value const& a, value const& b) {
    return a.val == b.val;
  }

  inline bool operator!= (value const& a, value const& b) {
    return a.val != b.val;
  }

  inline bool operator== (string const& a, string const& b) {
    return a.val == b.val;
  }

  inline bool operator!= (string const& a, string const& b) {
    return a.val != b.val;
  }
  
  inline bool operator== (number const& a, number const& b) {
    return a.val == b.val;
  }

  inline bool operator!= (number const& a, number const& b) {
    return a.val != b.val;
  }
  
  inline bool operator== (object const& a, object const& b) {
    return a.members == b.members;
  }

  inline bool operator!= (object const& a, object const& b) {
    return a.members != b.members;
  }
  
  inline bool operator== (array const& a, array const& b) {
    return a.elements == b.elements;
  }

  inline bool operator!= (array const& a, array const& b) {
    return a.elements != b.elements;
  }
  
  //
  //
  //
  
  template<class It, class S>
  // require Forward_iterator<It>() && Sentinel<It, S>()
  auto parse_value(It& it, S sent) -> std::optional<value>;
  
  namespace __parse {
    /*
      JSON grammar:

      value -> number 
             | string 
             | constants
             | array 
             | object

      number -> ...

      string -> '"' ... '"'

      constants -> 'true' | 'false' | 'null'

      array -> '[' *value ']'

      object -> '{' *member '}'

      member -> string ':' value

     */

    // helper parsers
    template<class It, class Sent>
    auto skip_whitespaces(It& it, Sent sent) -> int {
      int skipped = 0;
      for(; it != sent && std::isspace(*it); ++ it){
        ++skipped;
      }
      return skipped;
    }

    template<class It, class Sent>
    auto skip_digits(It& it, Sent sent) -> int {
      int skipped = 0;
      for(; it != sent && std::isdigit(*it); ++it){
        ++skipped;
      }
      return skipped;
    }

    template<class LitIt, class LitSent, class It, class Sent>
    auto skip_literal(LitIt lit_it, LitSent lit_sent, It& it, Sent sent) -> void {
      auto it1 = it;
      for(; lit_it != lit_sent && it1 != sent && *lit_it == *it1; ++ lit_it, ++it1){
      }
      if(lit_it == lit_sent) {
        it = it1;
      }
    }

    template<class It, class Sent>
    auto skip_literal_p(char const* lit, std::size_t n, It& it, Sent sent) -> void {
      skip_literal(lit, lit + n, it, sent);
    }

    
    // parsers
    template<class It, class Sent>
    auto parse_number(It& it, Sent sent) -> std::optional<number> {
      assert( it != sent );
      assert( std::isdigit(*it) || '-' == *it );
      
      auto it1 = it;

      // optional '-'
      if(*it == '-'){
        ++ it1;
      }

      // one or more digits (required!) 
      if(0 == skip_digits(it1, sent)) {
        return {};
      }

      // optional fractional part (if it exists, digits are required);
      if(it1 != sent && *it1 == '.'){
        ++ it1;
        if(0 == skip_digits(it1, sent)){
          return {};
        }
      }

      // exponent
      if(it1 != sent && std::tolower(*it1) == 'e') {
        ++ it1; 
        if(it1 != sent && (*it1 == '+' || *it1 == '-')) {
          ++it1;
        }
        if(0 == skip_digits(it1, sent)){
          return {};
        }
      }
      
      // build value
      number num{ std::string(it, it1) };
      
      it = it1;
      
      return num;
    }

    template<class It, class Sent>
    auto parse_string(It& it, Sent sent) -> std::optional<string> {
      assert( it != sent );
      assert( *it == '"' );

      auto it1 = it;
      ++it1;

      for(; it1 != sent && *it1 != '"'; ++ it1) {
        if(*it1 == '\\'){
          ++it1;
        }
      }

      if(it1 == sent){
        return {};
      }
      
      string str{ std::string(it + 1, it1) };

      ++it1;
      it = it1;
      
      return str;
    }

    template<class It, class Sent>
    auto parse_null(It& it, Sent sent) -> std::optional<null> {
      assert( it != sent );
      assert( *it == 'n' );
      
      auto it1 = it;
      
      skip_literal_p("null", 4, it1, sent);
      
      if(it == it1) return {};
      
      it = it1;
      return { null{} };
    }

    template<class It, class Sent>
    auto parse_boolean(It& it, Sent sent) -> std::optional<boolean> {
      assert( it != sent );
      assert( *it == 't' || *it == 'f' );
      
      auto it1 = it;

      bool possible_value;
      switch(*it) {
      case 't': 
        skip_literal_p("true", 4, it1, sent);
        possible_value = true;
        break;
      case 'f':
        skip_literal_p("false", 5, it1, sent);
        possible_value = false;
        break;
      }

      if(it == it1) return {};

      it = it1;
      return { boolean{possible_value} };
    }
   
    template<class It, class Sent>
    auto parse_member(It& it, Sent sent) -> std::optional<std::pair<std::string, value>> {
      assert( it != sent );

      auto it1 = it;

      // key
      skip_whitespaces(it1, sent);
      auto key = parse_string(it1, sent);
      if(!key) return {};

      // ':'
      skip_whitespaces(it1, sent);
      if(it1 == sent || *it1 != ':') return {};
      ++it1;

      // value
      auto val = parse_value(it1, sent);
      if(!val) return {};

      it = it1;
      
      return { std::make_pair( std::move(key)->val, *std::move(val)) };
    }
    
    template<class It, class Sent>
    auto parse_object(It& it, Sent sent) -> std::optional<object> {  
      assert( it != sent );
      assert( *it == '{' );

      object obj;
      auto it1 = it;

      auto parse_element = [&]{
        auto val = parse_member(it1, sent);
        if(!val) return false;
        else obj.members.insert(*std::move(val));
        skip_whitespaces(it1, sent);
        return true;
      };
      
      // '{'
      ++it1;
      
      // elements
      skip_whitespaces(it1, sent);
      if(it1 == sent) return {};

      if(*it1 != '}') {
        if( !parse_element() ) return {};
        for(; it1 != sent && *it1 == ','; ){
          // ','
          ++it1;
          //
          if(!parse_element()) return {};
        }
        
      }

      if(it1 == sent || *it1 != '}') return {};
      
      // ']'
      ++it1;
      it = it1;
      
      return { std::move(obj) };
    }

    template<class It, class Sent>
    auto parse_array(It& it, Sent sent) -> std::optional<array> {
      assert( it != sent );
      assert( *it == '[' );
      
      array arr;
      auto it1 = it;

      auto parse_element = [&]{
        auto val = parse_value(it1, sent);
        if(!val) return false;
        else arr.elements.push_back(*std::move(val));
        skip_whitespaces(it1, sent);
        return true;
      };
      
      // '['
      ++it1;
      
      // elements
      skip_whitespaces(it1, sent);
      if(it1 == sent) return {};

      if(*it1 != ']') {
        if( !parse_element() ) return {};
        for(; it1 != sent && *it1 == ','; ){
          // ','
          ++it1;
          //
          if(!parse_element()) return {};
        }
        
      }

      if(it1 == sent || *it1 != ']') return {};
      
      // ']'
      ++it1;
      it = it1;
      
      return { std::move(arr) };
    }
    
  } // __parse
    
  
  template<class It, class S>
  // require Forward_iterator<It>() && Sentinel<It, S>()
  auto parse_value(It& it, S sent) -> std::optional<value> {    
    __parse::skip_whitespaces(it, sent);
    if(it == sent) return {};

    std::optional<value> val;

    char const c = *it;
    if( c == '{') {
      auto res = __parse::parse_object(it, sent);
      if(res) val = value{ *std::move(res) };
    }
    else if(c == '[') {
      auto res = __parse::parse_array(it, sent);
      if(res) val = value{ *std::move(res) };
    }
    else if(c == '"') {
      auto res = __parse::parse_string(it, sent);
      if(res) val = value{ *std::move(res) };
    }
    else if(std::isdigit(c) || c == '-') {
      auto res = __parse::parse_number(it, sent);
      if(res) val = value{ *std::move(res) };
    }
    else if(c == 't' || c == 'f') {
      auto res = __parse::parse_boolean(it, sent);
      if(res) val = value{ *res };
    }
    else if(c == 'n') {
      auto res = __parse::parse_null(it, sent);
      if(res) val = value{ null{} };
    }
    
    return val;
  }


  //
  //
  //
  auto write_to_ostream(std::ostream& sout, value const& val) -> std::ostream& ;
  
  struct ostream_writer_visitor {
    
    explicit ostream_writer_visitor(std::ostream& out)
      : m_out(out)
    {}

    // visits
    auto operator() ( null const ) const -> void {
      m_out << "null";
    }

    auto operator() ( boolean const b) const -> void {
      m_out << (b.val ? "true" : "false");
    }

    auto operator() ( string const& str ) const -> void {
      m_out << '"' << str.val << '"';
    }

    auto operator() ( number const& num ) const -> void {
      m_out << num.val;
    }

    auto operator() ( object const& val) const -> void {
      write_sequence( m_out
                    , "{", "}", ", "
                    , val.members.begin(), val.members.end()
                    , [](std::ostream& out, auto const& el) {
                        out << '"' << el.first << "\": ";
                        write_to_ostream(out, el.second); 
                      });
    }

    auto operator() ( array const& val) const -> void {
      write_sequence( m_out
                    , "[", "]", ", "
                    , val.elements.begin(), val.elements.end()
                    , write_to_ostream);
    }
    
    // fields
    std::ostream& m_out;
  };
  

  auto write_to_ostream(std::ostream& sout, value const& val) -> std::ostream&  {
    std::visit(ostream_writer_visitor(sout), val.val);
    return sout;
  }

} // json


//
// test
//

auto operator<< (std::ostream& out, json::value const& val) -> std::ostream& {
  return json::write_to_ostream(out, val);
}

 
auto main() -> int {
  // data
  std::string const example = R"+++({
	"number": 1234,
	"string": "hello",
	"array": [null, true, false, 42, "hello"],
	"object": {
		"one": 1,
		"two": 2,
		"three": 3
	}
  })+++";

  auto const expected_value = []{
    json::array arr{ {
        json::value{ json::null{} },
        json::value{ json::boolean{true} },
        json::value{ json::boolean{false} },
        json::value{ json::number{"42"} },
        json::value{ json::string{"hello"} },
    } };
      
    json::object sub_obj;
    sub_obj.members["one"] = json::value{json::number{"1"}};
    sub_obj.members["two"] = json::value{json::number{"2"}};
    sub_obj.members["three"] = json::value{json::number{"3"}};
      
    json::object object;
    object.members["number"] = json::value { json::number{"1234"} };
    object.members["string"] = json::value { json::string{"hello"} };
    object.members["array"]  = json::value { std::move(arr) };
    object.members["object"] = json::value { std::move(sub_obj) };
        
    return json::value{std::move(object)};
  }();

  // short for : it - iterator, sent - sentinel
  auto       it   = example.begin();
  auto const sent = example.end();
  
  auto actual_value = json::parse_value(it, sent);
   
  assert( actual_value && expected_value == *actual_value );
  assert( std::distance(it, sent) == 0 );
 
  return 0;
}
