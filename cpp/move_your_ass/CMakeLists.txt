cmake_minimum_required(VERSION 3.10)

#
project(move_your_ass)

#
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

# libraries
find_package(Qt5Widgets)

# sources
file(GLOB files_SRC
    "src/*.hpp"
    "src/*.cpp"
)

# executable

# on win32
#add_executable(move_your_ass WIN32 ${files_SRC})

add_executable(move_your_ass ${files_SRC})

target_link_libraries(move_your_ass Qt5::Widgets)

set_property(TARGET move_your_ass PROPERTY CXX_STANDARD 17)
set_property(TARGET move_your_ass PROPERTY CXX_STANDARD_REQUIRED ON)
