cmake_minimum_required(VERSION 2.8)

include("${CMAKE_SOURCE_DIR}/Gen.cmake")

generate_sources(meta_headers meta_sources meta_include_dir
  test.meta
  another_test.meta
  )

add_library(metas STATIC ${meta_sources} ${meta_headers})
target_include_directories(metas PUBLIC ${meta_include_dir})
