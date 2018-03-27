cmake_minimum_required(VERSION 3.10)

#
project(concurrency_book)

# sources
file(GLOB files_SRC
    "src/*.hpp"
    "src/*.cpp"
)

# executable
add_executable(concurrency_book ${files_SRC})

set_property(TARGET concurrency_book PROPERTY CXX_STANDARD 17)
set_property(TARGET concurrency_book PROPERTY CXX_STANDARD_REQUIRED ON)


