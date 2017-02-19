#include <cassert>
#include <complex>

int mandelbrot_point_iteration_count ( std::complex<float> const& c
                                     , int iteration_limit )
{
  assert( iteration_limit > 0 );

  std::complex<float> z{0,0};

  int count = 0;
  for(; count < iteration_limit && std::norm(z) < 2.0f; ++ count){
    z = z*z + c;
  }

  return count;
}

#include <vector>

using Mandelbrot = std::vector< std::vector< int > >;

Mandelbrot mandelbrot_iterations( int width, int height
                                , float x_min, float x_width
                                , float y_min, float y_width
                                , int iteration_count )
{
  assert( width > 0 );
  assert( width > 0 );
  assert( iteration_count > 0 );
  
  Mandelbrot mandelbrot(height, std::vector<int>(width, 0));

  #pragma omp parallel for
  for(int i = 0; i < width*height; ++ i)
  {
    int row = i/width;
    int col = i%width;

    std::complex<float> const c { x_min + col*(x_width/width)
                                , y_min + row*(y_width/height) };
      
    int iterations = mandelbrot_point_iteration_count(c, iteration_count);

    mandelbrot[row][col] = iterations;
  }

  return mandelbrot;
}


#include <fstream>

void write_to_ppm( std::ostream& str
                 , Mandelbrot const& mandelbrot
                 , int const width, int const height
                 , int const max_iteration_count )
{
  str << "P3\n";
  str << width << " " << height << "\n";
  str << max_iteration_count << "\n";

  for(int row = 0; row < height; ++ row)
  {
    for(int col = 0; col < width; ++ col)
    {
      int x = max_iteration_count
            - mandelbrot[row][col];
      str << x << " " << x << " " << x << "    ";
    }
    str << "\n";
  }
}

int main()
{
  constexpr int width  = 4000;
  constexpr int height = 4000;

  constexpr float x_min = -1.75f;
  constexpr float x_width = 2.5f;

  constexpr float y_min = -1.25f;
  constexpr float y_width = 2.5f;

  constexpr float iteration_count = 50;
  
  auto mandelbrot = mandelbrot_iterations( width, height
                                         , x_min, x_width
                                         , y_min, y_width
                                         , iteration_count );

  std::ofstream image("image.ppm");

  write_to_ppm(image, mandelbrot, width, height, iteration_count);
 
  return 0;
}
