// Zamenite ??? . Rešenja su ispod.

// primer
int func(int, string, float);

// bind - vezati
// bound - vezan
// `bind` vraća objekat koji se ponaša kao funkcija `bound`
// `bind` uzima kopije svojih argumenata!

function< ???(???) > res = bind(func, _1,"hello", 0.123);

??? bound(???)
{
  return func(???);
}

// rešenje
function< int(int) > res = bind(func, _1,"hello", 0.123);

int bound(int _1)
{
  return func(_1,"hello", 0.123);
}

// 1.
int func(int, string, float);

function< ???(???) > res = bind(func, 42, _2, _1); 

??? bound(???)
{
  return func(???);
}

// 2.
int func(int, string, float);

function< ???(???) > res = bind(func, _3, _2, _1)

??? bound(???)
{
  return func(???);
}

// 4.
int plus(int x, int y);

function< ???(???) > res = bind(plus, _1, _1);

??? bound(???)
{
  return plus(???);
}

// 5. 
string serialize(int, float, string, string, double);

function< ???(???) > res = bind(serialize, 42, _2, _1, _1, _3 );

??? bound(???)
{
  return serialize(???);
}

// 6.
struct Person
{
  int get_shoe_size() const;
}

Person p;

function< ???(???) > res = bind(&Person::get_shoe_size, &p);

??? bound(???)
{
  // remember: `int Person::get_shoe_size() const` is equivalent to `int get_shoe_size(Person const*)`
  int (Person::*fptr)() const = &Person::get_shoe_size();
  return (p->*fptr)(???);
}

// 7.
struct Person
{
  int get_shoe_size() const;
}

function< ???(???) > res = bind(&Person::get_shoe_size, _1);

??? bound(???)
{
  // remember: `int Person::get_shoe_size() const` is equivalent to `int get_shoe_size(Person const*)`
  int (Person::*fptr)() const = &Person::get_shoe_size();
  return (???->*fptr)(???);
}

//
//
// Solutions
//
//

// 1.
int func(int, string, float);

function< int(float, string) > res = bind(func, 42, _2, _1); 

int bound(float _1, string_2)
{
  return func(42, _2, _1);
}

// 2.
int func(int, string, float);

function< int(float, string, int) > res = bind(func, _3, _2, _1)

int bound(float _1, string _2, int _3)
{
  return func(_3, _2, _1);
}

// 4.
int plus(int x, int y);

function< int(int) > res = bind(plus, _1, _1);

int bound(int _1)
{
  return plus(_1, _1);
}

// 5. 
string serialize(int, float, string, string, double);

function< string(string, float, double) > res = bind(serialize, 42, _2, _1, _1, _3 );

string bound(string _1, float _2, double _3)
{
  return serialize(42, _2, _1, _1, _3);
}

// 6.
struct Person
{
  int get_shoe_size() const;
}

Person p;

function< int() > res = bind(&Person::get_shoe_size, &p);

int bound()
{
  // remember: int Person::get_shoe_size() is equivalent to int get_shoe_size(Person*)
  int (Person::*fptr)() const = &Person::get_shoe_size();
  return (p->*fptr)();
}

// 7.
struct Person
{
  int get_shoe_size() const;
}

function< int(Person*) > res = bind(&Person::get_shoe_size, _1);

int bound(Person* _1)
{
  // remember: int Person::get_shoe_size() is equivalent to int get_shoe_size(Person*)
  int (Person::*fptr)() const = &Person::get_shoe_size();
  return (_1->*fptr)();
}