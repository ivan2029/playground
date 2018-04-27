cmake_minimum_required(VERSION 2.8)

add_subdirectory(metas)

add_executable(try_metas
  src/main.cpp
  )
target_link_libraries(try_metas metas)

