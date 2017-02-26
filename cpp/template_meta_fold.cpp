/*
  using compile time fold over parameter pack to build tuple type (of sorts)
  test stuff requires gcc
*/

// foldleft :: (b -> a -> b) -> b -> [a] -> b
// foldleft reducer init [] = init
// foldleft reducer init (x:xs) = foldl reducer (reducer init x) xs
template<template<class, class> class Reducer, class Init, class ... Args>
struct FoldLeft;

template<template<class, class> class Reducer, class Init>
struct FoldLeft<Reducer, Init>
{
  using Type = Init;
};

template<template<class, class> class Reducer, class Init, class H, class ... Ts>
struct FoldLeft<Reducer, Init, H, Ts...>
{
  using Type =
    typename FoldLeft<
      Reducer
      , typename Reducer<Init, H>::Type
      , Ts ...
    >::Type;
};

// foldright :: (a -> b -> b) -> b -> [a] -> b
// foldright reducer init [] = init
// foldright reducer init (x:xs) = reducer x (foldr' reducer init xs)
template<template<class, class> class Reducer, class Init, class ... Args>
struct FoldRight;

template<template<class, class> class Reducer, class Init>
struct FoldRight<Reducer, Init>
{
  using Type = Init;
};

template<template<class, class> class Reducer, class Init, class H, class ... Ts>
struct FoldRight<Reducer, Init, H, Ts...>
{
  using Type =
    typename Reducer<
      H
      , typename FoldRight <
          Reducer
          , Init
          , Ts...
        >::Type
    >::Type;
};


//
struct Unit {};

template<class Type, class Next>
struct RevTupleCons : public Next
{
  Type value;
};

template<class B, class A>
struct ReverseTupleReducer
{
  using Type = RevTupleCons<A, B>;
};


template<class ... Ts>
using ReverseTuple =
  typename FoldLeft<
    ReverseTupleReducer
    , Unit
    , Ts ...
  >::Type;

template<class A, class B>
struct TupleCons : public B
{
  A value;
};

template<class A, class B>
struct TupleReducer
{
  using Type = TupleCons<A, B>;
};

template<class ... Ts>
using Tuple =
  typename FoldRight <
    TupleReducer
    , Unit
    , Ts...
  >::Type;

  
#include <iostream>
#include <typeinfo>
#include <string>
#include <cxxabi.h>

std::string demangle(std::string name)
{
  using namespace std;
  int status;
  char* realname = abi::__cxa_demangle(name.c_str(), 0,0, &status);

  string result(realname);
  free(realname);
  return result;
}

template<class T>
std::string type_name()
{
  return demangle(typeid(T).name());
}

struct TestStruct
{
  int x;
  double y;
  char c;
};

int main()
{
  using RevTup = ReverseTuple<int, double, char>;
  using Tup = Tuple<int, double, char>;
  
  std::cout << type_name<RevTup>() << "\n";
  std::cout << sizeof(RevTup) << "\n";

  std::cout << type_name<Tup>() << "\n";
  std::cout << sizeof(Tup) << "\n";
  
  std::cout << type_name<TestStruct>() << "\n";
  std::cout << sizeof(TestStruct) << "\n";
  
  return 0;
}
/*
  when compiled with g++ 4.9.1, the output of the program is this:
  
  RevTupleCons<char, RevTupleCons<double, RevTupleCons<int, Unit> > >
  24
  TupleCons<int, TupleCons<double, TupleCons<char, Unit> > >
  24
  TestStruct
  24

*/

