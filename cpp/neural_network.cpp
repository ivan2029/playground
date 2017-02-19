/*
  
  A simple feed-forward neural network with sigmoid neurons, for
  recognizing digits in MNIST dataset.

  https://www.willamette.edu/~gorr/classes/cs449/backprop.html

  Compiles with g++ 6.1
  Command: g++ main.cpp -std=c++1z -isystem"./libs" -O2 -Wall -Wextra -fopenmp -DNDEBUG

  With default parameters: 89% accuracy. TODO: improve...

*/

#include <cassert>
#include <cstdint>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <stdexcept>
#include <chrono>
  #include <tuple>

//
//
//
template<class ... Ts>
struct deduced_type;


//
//
//
template<class Fn>
auto run_and_time(Fn&& fn) // microseconds
{
  using namespace std::chrono;
  auto begin = high_resolution_clock::now();
  fn();
  auto end = high_resolution_clock::now();
  auto diff = end - begin;
  return duration_cast<microseconds>(diff).count();
}

//
//
//
template<class File, class Line, class Function, class Msg>
void throw_logic_error(File file, Line line, Function function, Msg msg)
{
  std::stringstream sout;
  sout << file << ":" << line << ":" << function << ": " << msg;
  throw std::logic_error(sout.str());
}

#define THROW_LOGIC_ERROR(msg) throw_logic_error(__FILE__, __LINE__, __FUNCTION__, msg)

#define UNIMPLEMENTED() THROW_LOGIC_ERROR("not yet implemented")

//
// 
//
constexpr int TRAIN_COUNT = 60000;
constexpr char const* TRAIN_IMAGES = "data/train-images.idx3-ubyte";
constexpr char const* TRAIN_LABELS = "data/train-labels.idx1-ubyte";

constexpr int TEST_COUNT = 10000;
constexpr char const* TEST_IMAGES = "data/t10k-images.idx3-ubyte";
constexpr char const* TEST_LABELS = "data/t10k-labels.idx1-ubyte";


//
// data loading helpers, partial idx loader implementation
// bit of an overkill for what I need here, but what the hey!
//
namespace idx
{
  /*
    basically, ntoh functions
  */

  // N is number of bytes, as returned by sizeof
  template<std::size_t N> struct rep_type_n;
  template<> struct rep_type_n<1> { using type = std::uint8_t ; };
  template<> struct rep_type_n<2> { using type = std::uint16_t; };
  template<> struct rep_type_n<4> { using type = std::uint32_t; };
  template<> struct rep_type_n<8> { using type = std::uint64_t; };

  template<class T>
  using rep_type_t = typename rep_type_n< sizeof(T) >::type;

  //
  std::uint8_t reverse_bytes_impl(std::uint8_t x)
  {
    return x;
  }

  std::uint16_t reverse_bytes_impl(std::uint16_t x)
  {
    return ( ( x & 0xff00 ) >> 8 )
         | ( ( x & 0x00ff ) << 8 )
         ;
  }

  std::uint32_t reverse_bytes_impl(std::uint32_t x)
  {
    return ( ( x & 0x000000ff ) << 24 )
         | ( ( x & 0x0000ff00 ) << 8  )
         | ( ( x & 0x00ff0000 ) >> 8  )
         | ( ( x & 0xff000000 ) >> 24 )
         ;
  }

  std::uint64_t reverse_bytes_impl(std::uint64_t x)
  {
    return ( ( x & 0xff00000000000000ull ) >> 56 ) 
         | ( ( x & 0x00ff000000000000ull ) >> 40 )
         | ( ( x & 0x0000ff0000000000ull ) >> 24 )				      
         | ( ( x & 0x000000ff00000000ull ) >> 8  )				      
         | ( ( x & 0x00000000ff000000ull ) << 8  )				      
         | ( ( x & 0x0000000000ff0000ull ) << 24 )				      
         | ( ( x & 0x000000000000ff00ull ) << 40 )
         | ( ( x & 0x00000000000000ffull ) << 56 )
         ;
  }

  //
  template<class T>
  T reverse_bytes(T t)
  {
    using Rep = rep_type_t<T>;
    auto pt = reinterpret_cast<Rep*>(&t);
    *pt = reverse_bytes_impl(*pt);
    return t;
  }

  //
  /*
    TODO: static_assert this! 

    T is one of: uint8_t, int8_t, int16_t, int32_t, float, double

    input stream is opened in binary reading mode
  */
  template<class T>
  T read_chunk(std::istream& in)
  {
    T value;
    auto buffer = reinterpret_cast<char*>(&value);
    if( !in.read(buffer, sizeof(T)) ){
      THROW_LOGIC_ERROR("input stream is empty");
    }
    /*
      TODO: This assumes machine is little endian, but data is written
      in big endian. This code is tested on x86_64 machine where this
      is the case.
    */
    return reverse_bytes(value);
  }

  //
  void seek_image_data_begin(std::istream& in)
  {
    in.seekg(16);
  }

  void seek_label_data_begin(std::istream& in)
  {
    in.seekg(8);
  }

  //
  void seek_image_data(int index, std::istream& in)
  {
    in.seekg(16 + index*28*28);
  }
  
  void seek_label_data(int index, std::istream& in)
  {
    in.seekg(8 + index);
  }

  // read data into
  template<class Rep, class Count, class OutIt>
  OutIt read_n(std::istream& in, Count count, OutIt out)
  {
    for(Count i{0}; i < count; ++ i){
      *out = read_chunk<Rep>(in);
      ++out;
    }
    return out;
  }

  template<class Rep, class Fn, class Count, class OutIt>
  OutIt read_transform_n(Fn fn, std::istream& in, Count count, OutIt out)
  {
    for(Count i{0}; i < count; ++ i){
      *out = fn( read_chunk<Rep>(in) );
      ++out;
    }
    return out;
  }
  
} // idx

//
// 
//
#include <Eigen/Dense>

//
//
//
void write_m( std::string const& filename, Eigen::MatrixXd const& m)
{
  auto fix_num = [](double const min, double const len, double const x) {
    double const x_normed = (x - min)/len;
    int const y = (int) (255*x_normed);
    if(0 <= y && y < 256) return y;
    else return 0;
  };

  std::ofstream fout( filename + ".ppm");
  fout << "P3\n";
  fout << m.cols() << " " << m.rows() << "\n";
  fout << "255\n";

  double const min = m.minCoeff();
  double const len = m.maxCoeff() - min;

  for(int row = 0; row < m.rows(); ++ row) {
    for(int col = 0; col < m.cols(); ++ col) {
      double const x = fix_num(min, len, 0.5*(1 + m(row, col)));
      fout << x << " " << x << " " << x << "    "; 
    }
    fout << "\n";
  }  
}

void write_nn( std::string const&                  prefix
             , std::vector<Eigen::MatrixXd> const& weights
             //, std::vector<Eigen::MatrixXd> const& biases
             )
{

  for(std::size_t i = 0; i < weights.size(); ++ i) {
    write_m( prefix + "_" + std::to_string(i), weights[i]);
  }
}

//
// data loading
//
int read_label(int index, std::istream& in)
{
  idx::seek_label_data(index, in);
  return static_cast<int>( idx::read_chunk<std::uint8_t>(in) );
}

std::vector<double> read_image_into_std_vector(int index, std::istream& in)
{
  auto tr = [](std::uint8_t x){
    return 1.0 - static_cast<double>(x)/255.0;
  };
  
  idx::seek_image_data(index, in);
  std::vector<double> data(28*28);
  idx::read_transform_n<std::uint8_t>( tr
                                     , in
                                     , data.size()
                                     , data.begin() );
  return data;
}

Eigen::MatrixXd read_image_into_vector(int index, std::istream& in)
{
  auto data = read_image_into_std_vector(index, in);
  Eigen::Map<Eigen::MatrixXd> image_view(data.data(), 28*28, 1);
  return image_view;
}

Eigen::MatrixXd read_image_into_matrix(int index, std::istream& in)
{ 
  auto data = read_image_into_std_vector(index, in);
  Eigen::Map<Eigen::MatrixXd> image_view(data.data(), 28, 28);
  return image_view.transpose();
}

//
//  
//
Eigen::MatrixXd output_from_label(int label)
{
  assert( 0 <= label && label <= 9);
  Eigen::MatrixXd output = Eigen::MatrixXd::Zero(10, 1);
  output(label, 0) = 1;
  return output;
}

int label_from_output(Eigen::MatrixXd const& output)
{
  assert(output.rows() == 10 && output.cols() == 1);

  int index = 0;

  for(int i = 1; i < 10; ++ i){
    if(output(i, 0) > output(index, 0)){
      index = i;
    }
  }
  
  return index;
}

//
template<class It, class S>
auto make_weights(It first, S last) -> std::vector<Eigen::MatrixXd>
{
  assert(first != last && std::next(first) != last);
  
  std::vector<Eigen::MatrixXd> weights;
  weights.reserve( std::distance(first, last) );
  
  auto second = std::next(first);
  for(; second != last; ++ first, ++ second) {
    Eigen::MatrixXd m =
      Eigen::MatrixXd::Random(*second, *first)
      ;
    weights.push_back( std::move(m) ); 
  }

  return weights;
}

template<class R>
auto make_weights(R const& r)
{
  return make_weights(std::begin(r), std::end(r));
}

template<class T>
auto make_weights(std::initializer_list<T> ls)
{
  return make_weights(ls.begin(), ls.end());
}

//
template<class It, class S>
auto make_biases(It first, S last) -> std::vector<Eigen::MatrixXd>
{
  assert(first != last && std::next(first) != last);
  
  std::vector<Eigen::MatrixXd> biases;
  biases.reserve( std::distance(first, last) );
  
  for(first = std::next(first); first != last; ++ first ) {
    Eigen::MatrixXd m =
      Eigen::MatrixXd::Random(*first, 1)
      ;
    biases.push_back( std::move(m) );
  }

  return biases;  
}

template<class R>
auto make_biases(R const& r)
{
  return make_biases(std::begin(r), std::end(r));
}

template<class T>
auto make_biases(std::initializer_list<T> ls)
{
  return make_biases(ls.begin(), ls.end());
}

//
// math functions
//
double sig(double x)
{
  return 1.0/(1 + std::exp(-x));
}

double sig_prim(double x)
{
  double const t = sig(x);
  return t*(1 - t);
}

// Op :: double -> double
template<class Op, class E>
auto cwise_op(Op op, Eigen::MatrixBase<E> const& m)
{
  return m.unaryExpr( [=](double x){ return op(x); } );
}


//
// feed-forward 
//

auto feed_forward( std::vector<Eigen::MatrixXd> const& weights
                 , std::vector<Eigen::MatrixXd> const& biases
                 , Eigen::MatrixXd const&              input )
  -> Eigen::MatrixXd
{
  Eigen::MatrixXd output = input;
  
  for(std::size_t i = 0; i < weights.size(); ++ i) {
    output = cwise_op( sig
                     , weights[i]*output + biases[i] );
  }
 
  return output;
}

auto feed_forward_intermediate( std::vector<Eigen::MatrixXd> const& weights
                              , std::vector<Eigen::MatrixXd> const& biases
                              , Eigen::MatrixXd const&              input )
  -> std::pair< std::vector<Eigen::MatrixXd>, std::vector<Eigen::MatrixXd> >
{
  std::vector<Eigen::MatrixXd> nets(weights.size());
  std::vector<Eigen::MatrixXd> outputs(weights.size() + 1);
  
  outputs[0] = input;

  for(std::size_t i = 0; i < weights.size(); ++ i){
    nets[i] = weights[i] * outputs[i] + biases[i];
    outputs[i + 1] = cwise_op(sig, nets[i]);
  }
  
  return std::make_pair(std::move(nets), std::move(outputs));
}

//
// back-propagate
//

void back_propagate( double const                   learning_rate
                   , std::vector<Eigen::MatrixXd>&  weights
                   , std::vector<Eigen::MatrixXd>&  biases
                   , Eigen::MatrixXd const&         input
                   , Eigen::MatrixXd const&         expected_output )
{
  std::vector<Eigen::MatrixXd> nets, outputs;
  std::tie(nets, outputs) = feed_forward_intermediate(weights, biases, input);

  std::vector<Eigen::MatrixXd> deltas(weights.size());

  // compute deltas 
  std::size_t index = deltas.size() - 1;


  deltas[index] = (expected_output - outputs[index + 1])
                  .cwiseProduct(cwise_op(sig_prim, outputs[index + 1]));
  
  for(; index != 0; -- index) {
    deltas[index - 1] = weights[index].transpose() * deltas[index];
    deltas[index - 1] = deltas[index - 1]
                        .cwiseProduct(cwise_op(sig_prim, outputs[index]));
  }

  // update weights
  for(index = 0; index != deltas.size(); ++ index) {
    weights[index] += learning_rate*(deltas[index]*outputs[index].transpose());
    biases[index] += learning_rate*deltas[index];
  }
}

//
//
//

int main(int argc, char* argv[])
{
  double learning_rate = 0.1;
  std::vector<int> conf;

  // first param: learning rate
  if(argc > 1){    
    try{
      learning_rate = std::stof(argv[1]);
    }catch(...){}
  }

  // rest: layer sizes (first 
  if(argc > 2){
    try{
      std::vector<int> tmp;
      tmp.push_back(784);
      for(int i = 2; i < argc; ++ i){
        tmp.push_back(std::stoi(argv[i]));
      }
      conf = std::move(tmp);
    }catch(...){
    }
  }

  
  if(conf.empty()){
    conf = { 784, 200, 200, 10 };
  }

  std::cout << "network has following layers: ";
  for(auto const i: conf){
    std::cout << i << " ";
  }
  std::cout << "\n";
  std::cout << "learning rate is: " << learning_rate << "\n";
  
  // prepare neural network
  auto weights = make_weights(conf);
  auto biases  = make_biases(conf);
  
  { // train
    std::ifstream fin_images(TRAIN_IMAGES, std::ios::binary);
    std::ifstream fin_labels(TRAIN_LABELS, std::ios::binary);

    long long total_train_time = run_and_time([&]{
      for(std::size_t i = 0; i < TRAIN_COUNT; ++ i){
        auto const input = read_image_into_vector(i, fin_images)
                         + Eigen::MatrixXd::Random(784, 1)*0.1
                         ;
        auto const label = read_label(i, fin_labels);
        auto const expected_output = output_from_label(label);
        
        back_propagate( learning_rate
                      , weights
                      , biases
                      , input
                      , expected_output );
      }
    });

    std::cout << "total train time: \n"
              << "  "  << total_train_time/1000 << " ms\n"
              << "  "  << total_train_time/(1000*1000) << " s\n"
              << "  "  << total_train_time/(1000*1000*60) << " min\n"
              ;
  }

  write_nn("post", weights);
  
  { // test
    std::ifstream fin_images(TEST_IMAGES, std::ios::binary);
    std::ifstream fin_labels(TEST_LABELS, std::ios::binary);

    double passed = 0.0;
    
    long long total_test_time = run_and_time([&]{
      for(std::size_t i = 0; i < TEST_COUNT; ++ i){
        auto const input = read_image_into_vector(i, fin_images);
        auto const expected_label = read_label(i, fin_labels);
        auto const output = feed_forward(weights, biases, input);
        auto const label = label_from_output(output);

        if(label == expected_label){
          passed += 1.0;
        }
      }
    });

    std::cout << "total test time: \n"
              << "  " <<  total_test_time/1000 << " ms\n"
              << "  " <<  total_test_time/(1000*1000) << " s\n"
              << "  " <<  total_test_time/(1000*1000*60) << " min\n"
              ;
    std::cout << "passed: " << (100 * passed / TEST_COUNT) << "%\n";
  }
  
  return 0;
}
