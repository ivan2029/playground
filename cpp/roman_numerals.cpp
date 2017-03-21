/*
  Roman Numerals

  isValidRoman :: String -> Bool
  toRoman :: Int -> String
  fromRoman :: String -> Int

  Roman Numerals are based on these symbols:
  
    I = 1
    V = 5
    X = 10
    L = 50
    C = 100
    D = 500
    M = 1000

  Numerals are formed with following combinations (ordered from
  strongest to weakest):
  
    MMM  = 3000   
    MM   = 2000       
    M    = 1000

    CM   = 900
    DCCC = 800
    DCC  = 700
    DC   = 600
    D    = 500
    CD   = 400
    CCC  = 300
    CC   = 200
    C    = 100

    XC   = 90
    LXXX = 80
    LXX  = 70
    LX   = 60
    L    = 50
    XL   = 40
    XXX  = 30
    XX   = 20
    X    = 10

    IX   = 9
    VIII = 8
    VII  = 7
    VI   = 6
    V    = 5
    IV   = 4
    III  = 3
    II   = 2
    I    = 1

  optional one of                           { MMM, MM, M } then
  optional one of { CM, DCCC, DCC, DC, D, CD, CCC, CC, C } then
  optional one of { XC, LXXX, LXX, LX, L, XL, XXX, XX, X } then
  optional one of { IX, VIII, VII, VI, V, IV, III, II, I }

  Matchers for last 3 groups are the same, parametrized on 3 symbols -
  lets call them 1, 5, 0 (as in I, V, X or X, L, C or C, D, M).

  1 followed by (nothing or 1 or 11 or 5 or 0) 
  or
  5 followed by (nothing or 1 or 11 or 111)

*/
#include <cassert>
#include <string>
#include <sstream>
#include <iostream>
#include <utility>

using RomanNumeral = std::string;

namespace __impl {

  using iterator = RomanNumeral::const_iterator;

  bool checkLetter(char const c) {
    switch(c){
    case 'M':
    case 'D':
    case 'C':
    case 'L':
    case 'X':
    case 'V':
    case 'I':
      return true;
    default:
    return false;
    }
  }

  bool checkLetters(iterator first, iterator last) {
    for(; first != last; ++ first ) {
        if(!checkLetter(*first)) return false;
    }
    return true;
  }

  // greadily consumes up to 3 Ms (ie. M, MM or MMM)
  iterator matchM(iterator first, iterator last) {
    for(int i = 0; i < 3 && first != last; ++i, ++first) {
      if('M' != *first) break;
    }
    return first;
  }

  // greadily consumes one of 1, 11, 111, 15, 5, 51, 511, 5111, 10
  // (where 1,5 and 0 are replaced with I,V,X or X,L,C or C,D,M)
  iterator matchTemplate( char const one, char const five, char const ten
                        , iterator first, iterator last)
  {
    /*
      This function uses following state machine:

         one  five ten
      0   1    2    _
      1   3    5    6
      2   7    _    _
      3   4    _    _
      4   _    _    _
      5   _    _    _
      6   _    _    _
      7   8    _    _
      8   9    _    _
      9   _    _    _

      Note: this state machine is not minimized (though it might be
      minimal already)!
    */
    for(int state = 0; state != 10 && first != last; ++first) {
      switch(state){
      case 0: 
        if      ( one == *first) state = 1;
        else if (five == *first) state = 2;
        else                     state = 10;
        break;
      case 1: 
        if      ( one == *first) state = 3;
        else if (five == *first) state = 5;
        else if ( ten == *first) state = 6;
        else                     state = 10;       
        break;
      case 2: 
        if (one == *first) state = 7;
        else               state = 10;
        break;
      case 3: 
        if (one == *first) state = 4;
        else               state = 10;
        break; 
      case 7: 
        if (one == *first) state = 8;
        else               state = 10;
        break;
      case 8: 
        if (one == *first) state = 9;
        else               state = 10;
        break;
      default:
        state = 10;
        break;
      }
    }
    return first;
  }

  int symbolValue(char const c) {
    switch(c){
      case 'M': return 1000;
      case 'D': return 500;
      case 'C': return 100;
      case 'L': return 50;
      case 'X': return 10;
      case 'V': return 5;
      case 'I': return 1;
      default:
      return -100000000;
    }
  }

} // namespace __impl

bool isValidRoman(RomanNumeral const& number) {
  auto first = std::begin(number);
  auto last  = std::end(number);

  // empty string is not valid roman numeral
  if(first == last) return false;

  // check that letters are valid
  if(!__impl::checkLetters(first, last)) return false;

  // match thousands
  first = __impl::matchM(first, last);

  // match hundreds
  first = __impl::matchTemplate('C', 'D', 'M', first, last);

  // match tens
  first = __impl::matchTemplate('X', 'L', 'C', first, last);

  // match ones
  first = __impl::matchTemplate('I', 'V', 'X', first, last);

  // if something left
  if(first != last) return false;

  return true;
}

int fromRoman(RomanNumeral const& number) {
  assert(isValidRoman(number));

  if ( number.size() == 1 ) {
    return __impl::symbolValue( number[0] );
  }

  auto first = begin(number);
  auto last  = end(number);

  int value = 0;
  
  for(;first + 1 != last && first != last; ++ first) {
    auto next = first + 1;

    int fst = __impl::symbolValue(*first);
    int snd = __impl::symbolValue(*next);

    if( fst >= snd ) {
      value += fst;
    }
    else {
      value += snd - fst;
      ++ first;
    }
  }

  if(first != last) {
    value += __impl::symbolValue(*first);
  }

  return value;
}

namespace __impl {

  int toRomanUpdate ( std::ostream& out
                    , int number
                    , int  const mul
                    , char const one 
                    , char const five
                    , char const ten  )                    
  {
    if( number >= 9*mul ) {
      out << one << ten;
      number -= 9*mul;
    }
    else if( number >= 5*mul ) {
      out << five;
      for(; number > 6*mul; number -= 1*mul) {
        out << one;
      }
      number -= 5*mul;
    }
    else if( number >= 4*mul ) {
      out << one << five;
      number -= 4*mul;
    }
    else {
      for(; number > 1*mul; number -= 1*mul) {
        out << one;
      }
      number -= 1*mul;
    }
    return number;
  }


} // namespace __impl

RomanNumeral toRoman(int number) {
  // largers Roman Numeral is 3999 : MMMCMXCIX
  assert(number > 0 && number < 4000); 

  std::stringstream builder;

  for(; number > 1000; number -= 1000) {
    builder << "M";
  }
  
  // now: number < 1000
  number = __impl::toRomanUpdate(builder, number, 100, 'C', 'D', 'M');
  
  // now: number < 100
  number = __impl::toRomanUpdate(builder, number, 10,  'X', 'L', 'C');
  
  // now: number < 10
  number = __impl::toRomanUpdate(builder, number, 1,   'I', 'V', 'X');

  assert( number == 0 );
  
  return builder.str();
}

int main() {
  {
    assert(!isValidRoman(""));
    assert(!isValidRoman("MMMMMMM"));
    assert(isValidRoman("XCVIII"));
  }

  {
    RomanNumeral roman{"MCMLXXXV"};
    int arabic{fromRoman(roman)};
    assert(1985 == arabic);
  }
  
  {
    int arabic{1985};
    RomanNumeral roman{toRoman(arabic)};
    assert("MCMLXXXV" == roman);
  }

  return 0;
}
