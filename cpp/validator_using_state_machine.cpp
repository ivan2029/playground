#include <type_traits>
#include <utility>
#include <cctype>
#include <cassert>
#include <algorithm>
#include <iterator>
#include <iostream>

template<class T>
auto toU(T t)
{
	static_assert(std::is_enum<T>::value, "");
	return static_cast<typename std::underlying_type<T>::type>(t);
}

/*
	state machine for email recognizer
  regex: [a-z][a-z0-9]*@[a-z]+(\.[a-z]+)+
*/

enum class State : std::size_t
{
	Start   = 0
	, _1    = 1
	, _2    = 2
	, _3    = 3
	, _4    = 4
	, _5    = 5
	, End   = 6
	, Error = 7
};

char const* toString(State s)
{
	switch(s)
	{
	case State::Start: return "Start";
	case State::_1:    return "_1";
	case State::_2:    return "_2";
	case State::_3:    return "_3";
	case State::_4:    return "_4";
	case State::_5:    return "_5";
	case State::End:   return "Start";
	default:           return "Error";
	}
	return "";
}

enum class Input : std::size_t
{
	Letter       = 0 // [a-z]
	, Digit      = 1 // [0-9]
	, At         = 2 // '@'
	, Dot        = 3 // '.'
	, EndOfInput = 4 // 
	, Invalid    = 5 // everything else
};

// newtype Transition = (Input, State)
// newtype AutomataTable = [(State, Transition)]

static State const g_automataTable[8][6] = {
              //     Letter    |    Digit    |      At      |     Dot    |  EndOfInput  |    Invalid
	        { State::_1,    State::Error, State::Error, State::Error, State::Error, State::Error} // Start
	,	{ State::_1,    State::_1,    State::_2,    State::Error, State::Error, State::Error} // _1
	,	{ State::_3,    State::Error, State::Error, State::Error, State::Error, State::Error} // _2
	,	{ State::_3,    State::Error, State::Error, State::_4,    State::Error, State::Error} // _3
	,	{ State::_5,    State::Error, State::Error, State::Error, State::Error, State::Error} // _4
	,	{ State::_5,    State::Error, State::Error, State::_4,    State::End,   State::Error} // _5
	,	{ State::Error, State::Error, State::Error, State::Error, State::Error, State::Error} // End
	,	{ State::Error, State::Error, State::Error, State::Error, State::Error, State::Error} // Error
};

State drive(State s, Input i)
{
	return g_automataTable[toU(s)][toU(i)];
}

template<class It>
// It models InputIterator
// std::is_same<iterator_traits<It>::type, char>::value == true
Input categorize(It const& f, It const& l)
{
	if(f == l) return Input::EndOfInput;
	char c = *f;
	if(std::isalpha(c)) return Input::Letter;
	if(std::isdigit(c)) return Input::Digit;
	if(c == '@')        return Input::At;
	if(c == '.')        return Input::Dot;
	if(c == '\0')       return Input::EndOfInput;
	return Input::Invalid;
}

template<class It>
// It models InputIterator
// is_same<iterator_traits<It>::type, char>::value == true
bool validate(It f, It l)
{
	auto s = State::Start;

	for(; f != l; ++ f)
		s = drive(s, categorize(f, l));

	return s == State::End;
}

template<class R>
// R models InputRange
// is_same<range_traits<R>::type, char>::value == true
bool validate(R& r)
{
	return validate(std::begin(r), std::end(r));
}

int main()
{
	assert(validate("ivan2029@gmail.com"));
	assert(validate("ivan2029@verify.something.org.com"));
	assert(validate("i@gmail.com"));
	assert(validate("ivan@gmail.com"));
	
	assert(!validate("2029@gmail.com"));
	assert(!validate("ivan2029@gmail"));
	assert(!validate("@gmail.com"));
	assert(!validate("gmail.com"));

	return 0;
}
