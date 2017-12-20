#include <iostream>
#include <cinttypes>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>

template<class It, class Sent>
It find_matching_rbracket(It ins, Sent sent) {
  int level = 0;
  for(; ins != sent; ++ ins) {
    switch(*ins) {
    case '[': ++level; break;
    case ']': --level; break;
    default: break;
    }
    // if(level < 0) then error
    if(level == 0) break;
  }
  return ins;
}

template<class It>
It find_matching_lbracket(It first, It ins) {
  int level = 0;
  for(; first != ins; -- ins) {
    switch(*ins) {
    case '[': --level; break;
    case ']': ++level; break;
    default: break;
    }
    // if(level < 0) then error
    if(level == 0) break;
  }
  /* this is needed for error handling
  if(level == 1 && first == ins && *first == '[') {
  }
  */
  return ins;
}

template<class It, class Sent>
void interpret(std::istream& in, std::ostream& out, It it, Sent sent) {
  //
  std::vector<std::uint8_t> memory(30000, 0);
  auto pointer = memory.begin();
  
  //
  auto ins = it;
  for(; ins != sent; ++ ins) {
    switch(*ins) {
    case '>':
      ++ pointer;
      break;
    case '<':
      -- pointer;
      break;
    case '+':
      ++ (*pointer);   
      break;
    case '-':
      -- (*pointer);
      break;
    case '.':
      out << *pointer;
      break;
    case ',': 
      if(in) in >> *pointer;
      break;
    case '[':
      if(*pointer == 0) {
        ins = find_matching_rbracket(ins, sent);
      }
      break;
    case ']':
      if(*pointer != 0) {
        ins = find_matching_lbracket(it, ins);
      }
      break;
    default:
      break;
    }
  }
}

template<class R>
void interpret(std::istream& in, std::ostream& out, R const& program) {
  interpret(in, out, std::begin(program), std::end(program));
}

//
// tests
//
std::string const hello_world = R"++(
    ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+
    [<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.----
    --.--------.>>+.>++."
)++";

std::string const rot13 = R"++(
    -,+[                     Read first character and start outer character reading loop
    -[                       Skip forward if character is 0
        >>++++[>++++++++<-]  Set up divisor (32) for division loop
                             (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
        <+<-[                Set up dividend (x minus 1) and enter division loop
            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
            <<<<<-           Decrement dividend
        ]                    End division loop
    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
    >--[-[<->+++[-]]]<[      Zero that flag unless quotient was 2 or 3; zero quotient; check flag
        ++++++++++++<[       If flag then set up divisor (13) for second division loop
                             (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
            >-[>+>>]         Reduce divisor; Normal case: increase remainder
            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
            <<<<<-           Decrease dividend
        ]                    End division loop
        >>[<+>-]             Add remainder back to divisor to get a useful 13
        >[                   Skip forward if quotient was 0
            -[               Decrement quotient and skip forward if quotient was 1
                -<<[-]>>     Zero quotient and divisor if quotient was 2
            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
    <[-]                     Clear remainder from first division if second division was skipped
    <.[-]                    Output ROT13ed character from copy and clear it
    <-,+                     Read next character
]                            End character reading loop
)++";

int main() {
  //
  {
    std::istringstream sin;
    std::ostringstream sout;
        
    interpret(sin, sout, hello_world);
        
    assert(sout.str() == "Hello World!\n");
  }
     
  //
  {
    std::istringstream sin{"brainfuck"};
    std::ostringstream sout;
        
    interpret(sin, sout, rot13);
    assert(sout.str() == "oenvashpx");
  }
    
  //
  {
    std::istringstream sin{"oenvashpx"};
    std::ostringstream sout;
        
    interpret(sin, sout, rot13);
    assert(sout.str() == "brainfuck");
  }
}