cmake_minimum_required(VERSION 3.10)

#
project(vulkan_test)

# libraries
find_package(SDL2 REQUIRED)
include_directories(${SDL2_INCLUDE_DIRS})

find_package(Vulkan REQUIRED)
include_directories(${Vulkan_INCLUDE_DIRS})

# sources
file(GLOB vulkan_test_SRC
    "src/*.hpp"
    "src/*.cpp"
)

#
file( COPY        "${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv"
      DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/shaders"
)

file( COPY        "${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv"
      DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/shaders"
)

# executable
add_executable(vulkan_test ${vulkan_test_SRC})

target_link_libraries(vulkan_test ${SDL2_LIBRARIES} ${Vulkan_LIBRARY})

set_property(TARGET vulkan_test PROPERTY CXX_STANDARD 17)
set_property(TARGET vulkan_test PROPERTY CXX_STANDARD_REQUIRED ON)
