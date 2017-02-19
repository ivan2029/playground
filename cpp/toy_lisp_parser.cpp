//
// recursive descent parser for lisp expression (subset at least)
//

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <memory>
#include <cctype>

#include <boost/variant.hpp>

// Value
struct Value;
using ValuePtr = std::shared_ptr<Value const>;

struct Atom   { std::string           value; };
struct Int    { int                   value; };
struct Real   { double                value; };
struct String { std::string           value; };
struct List   { std::vector<ValuePtr> value; };

struct Value { boost::variant<Atom, Int, Real, String, List> value; };

enum class Type { Atom, Int, Real, String, List };

Type get_type(Value const& val)
{
  class GetType : public boost::static_visitor<Type> {
  public:
    Type operator()(Atom   const&) const { return Type::Atom;   }
    Type operator()(Int    const&) const { return Type::Int;    }
    Type operator()(Real   const&) const { return Type::Real;   }
    Type operator()(String const&) const { return Type::String; }
    Type operator()(List   const&) const { return Type::List;   }
   };

  return boost::apply_visitor(GetType{}, val.value);
}

Value make_atom(std::string value) { return Value{ Atom{std::move(value) } }; }
Value make_int(int value) { return Value{ Int{ value } }; }
Value make_real(double value) { return Value{ Real{ value } }; }
Value make_string(std::string value) { return Value{ String{std::move(value) } }; }
Value make_list(std::vector<ValuePtr> value) { return Value{ List{std::move(value) } }; }

template<class T>
auto box(T t) -> std::shared_ptr<T const>
{
  return std::make_shared<T const>(std::move(t));
}


// Show

std::ostream& operator<<(std::ostream& out, Value const& v);

namespace __show
{
  class Show : public boost::static_visitor<void>
  {
  public:
    Show(std::ostream& out)
      : m_out{out}
    {}

    // variant visitor
    void operator()(Atom   const& v) const {
      m_out << v.value;
    }
    void operator()(Int    const& v) const {
      m_out << v.value;
    }
    void operator()(Real   const& v) const {
      m_out << v.value;
    }
    void operator()(String const& v) const {
      m_out << '"' << v.value << '"';
    }
    void operator()(List   const& v) const {
      m_out << "(";

      auto       f = v.value.begin();
      auto const l = v.value.end();

      if(f != l){
        m_out << **f;
        ++ f;
        for(; f != l; ++ f){
          m_out << " " << **f;
        }
      }

      m_out << ")";
    }
    
  private:
    std::ostream& m_out;
  };

} // __show

std::ostream& operator<<(std::ostream& out, Value const& v)
{
  boost::apply_visitor( __show::Show(out), v.value);
  return out;
}

std::string show(Value const& val)
{
  std::stringstream sout;
  sout << val;
  return sout.str();
};

// Parse
struct ParseOk  { Value       value; };
struct ParseErr { std::string value; };

struct parse_ok_tag {} parse_ok;
struct parse_err_tag {} parse_err;

class ParseResult
{
public: // ctor, assign, dtor
  ParseResult()
    : m_value{ParseErr{""}}
  {}

  ParseResult(parse_ok_tag, Value val)
    : m_value{ParseOk{std::move(val)}}
  {}

  ParseResult(parse_err_tag, std::string err)
    : m_value{ParseErr{std::move(err)}}
  {}


  ParseResult(ParseResult const&) = default;
  ParseResult(ParseResult&&) = default;

  ~ParseResult() = default;

  ParseResult& operator= (ParseResult const&) = default;
  ParseResult& operator= (ParseResult&&) = default;
  
public: // methods

  Value const& get_value() const
  {
    assert(is_success());
    return boost::get<ParseOk>(m_value).value;
  }

  std::string const& get_error() const
  {
    assert(is_success());
    return boost::get<ParseErr>(m_value).value;
  }

  bool is_success() const
  {
    return boost::get<ParseOk>(&m_value) != nullptr;
  }

private: // fields
  boost::variant<ParseOk, ParseErr> m_value; 
};


// parsers
namespace __parse
{
  template<class It>
  // where S: Sentinel<It>
  void skip_while_spaces(It& first, It last)
  {
    for(; first != last; ++ first){
      if(! std::isspace(*first) ){
        return;
      }
    }
  }

  template<class It, class S>
  // where S: Sentinel<It>
  ParseResult parse_atom(It& first, S last)
  {
    if(first == last) return ParseResult(parse_err, "parse_atom: input consumed");
  
    std::string atom_name;

    for(;first != last; ++first){
      if(std::isalnum(*first)){
        atom_name.push_back(*first);
      }
      else{
        break;
      }
    }
  
    return ParseResult(parse_ok, make_atom(std::move(atom_name)));
  }

  template<class It, class S>
  // where S: Sentinel<It>
  ParseResult parse_number(It& first, S last) // both Int and Real
  {
    // incomplete float parser
    if(first == last) return ParseResult(parse_err, "parse_number: input consumed");

    int integer_part = 0;

    for(;first != last; ++ first){
      if(std::isdigit(*first)){
        integer_part = integer_part*10 + (*first - '0');
      }
      else{
        break;
      }
    }

    if(first != last && *first == '.'){
      ++first;
      double div_by = 1.0;
      for(;first != last; ++ first){
        if(std::isdigit(*first)){
          integer_part = integer_part*10 + (*first - '0');
          div_by *= 10.0;
        }
        else{
          break;
        }
      }

      double result = integer_part / div_by;
      return ParseResult(parse_ok, make_real(result));
    }
    else{
      return ParseResult(parse_ok, make_int(integer_part));
    }
  }

  template<class It, class S>
  // where S: Sentinel<It>
  ParseResult parse_string(It& first, S last)
  {
    if(first == last) return ParseResult(parse_err, "parse_string: input consumed");
    if(*first != '"') return ParseResult(parse_err, "parse_string: '\"' expected");
    ++ first;

    std::string str;
    for(; first != last; ++first){
      if(*first != '"'){
        str.push_back(*first);
      }
      else{
        break;
      }
    }

    if(first == last) return ParseResult(parse_err, "parse_string: input consumed");
    if(*first != '"') return ParseResult(parse_err, "parse_string: '\"' expected");
    ++ first;
    
    return ParseResult(parse_ok, make_string(std::move(str)));
  }

  template<class It, class S>
  ParseResult parse_value(It& first, S last);
  
  template<class It, class S>
  // where S: Sentinel<It>
  ParseResult parse_list(It& first, S last)
  {
    if(first == last) return ParseResult(parse_err, "parse_list: input consumed");
    if(*first != '(') return ParseResult(parse_err, "parse_list: '(' expected");
    ++first;

    std::vector<ValuePtr> values;
  
    skip_while_spaces(first, last);
    while(first != last && *first != ')'){
      auto pr = parse_value(first, last);
    
      if(pr.is_success()){
        values.push_back(box(std::move(pr.get_value())));
      }
      else{
        return pr;
      }
    
      skip_while_spaces(first, last);
    }

    if(first == last) return ParseResult(parse_err, "parse_list: input consumed");
    if(*first != ')') return ParseResult(parse_err, "parse_list: ')' expected");
    ++ first;
  
    return ParseResult(parse_ok, make_list(std::move(values)));
  }

  template<class It, class S>
  // where S: Sentinel<It>
  ParseResult parse_value(It& first, S last)
  {
    skip_while_spaces(first, last);
    if(first == last) return ParseResult(parse_err, "parse_value: input consumed");

    ParseResult pr;
  
    if(std::isdigit(*first)){
      pr = parse_number(first, last);
    }
    else if(std::isalpha(*first)){
      pr = parse_atom(first, last);
    }
    else if(*first == '"'){
      pr = parse_string(first, last);
    }
    else if(*first == '('){
      pr = parse_list(first, last);
    }
    else{
      pr = ParseResult(parse_err, "parse_value: invalid character");
    }

    return pr;
  }
} // __parse

template<class It, class S>
// where S: Sentinel<It>
ParseResult parse(It& first, S last)
{
  return __parse::parse_value(first, last);
}

//
// test
//
static std::string const EXAMPLE = "(command arg1 123 3.141 \"arg3\" (1 2 3))";
static Value const VALUE = make_list({
      box( make_atom("command") )
    , box( make_atom("arg1") )
    , box( make_int(123) )
    , box( make_real(3.142) )
    , box( make_string("arg3") )
    , box( make_list({
          box( make_int(1) )
        , box( make_int(2) )
        , box( make_int(3) )
    }) )
  });

int main()
{
  //
  std::cout << "parsing: " << EXAMPLE << "\n";

  //
  auto       f = EXAMPLE.begin();
  auto const l = EXAMPLE.end();

  auto parse_result = parse(f, l);

  if(parse_result.is_success()){
    std::cout << "success: " << parse_result.get_value() << ", "
              << std::string(f, l) << "\n";
  }
  else{
    std::cout << "error: " << std::string(f, l) << "\n";
  }
  
  return 0;
}
