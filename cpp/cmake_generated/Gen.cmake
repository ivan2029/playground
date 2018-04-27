cmake_minimum_required(VERSION 2.8)

#
#
#
set(generate_tool ${CMAKE_SOURCE_DIR}/gen/generate.py)

#
#
#
function(generate_sources headers sources meta_dir)
  set(p_metas ${ARGN})
  
  set(p_headers)
  set(p_sources)

  set(p_meta_dir ${CMAKE_CURRENT_BINARY_DIR}/meta)
  
  file(MAKE_DIRECTORY ${p_meta_dir})

  foreach(p_meta ${p_metas})
    get_filename_component(p_meta_name ${p_meta} NAME_WE)
    get_filename_component(p_meta_abs  ${p_meta} ABSOLUTE)

    set(p_meta_header ${p_meta_dir}/${p_meta_name}.hpp)
    set(p_meta_source ${p_meta_dir}/${p_meta_name}.cpp)
    
    list(APPEND p_headers ${p_meta_header})
    list(APPEND p_sources ${p_meta_source})

    add_custom_command(
      OUTPUT  ${p_meta_header} ${p_meta_source}
      COMMAND ${generate_tool} ${CMAKE_CURRENT_SOURCE_DIR}/${p_meta} ${p_meta_dir}/${p_meta_name}
      DEPENDS ${generate_tool} ${p_meta_abs}
      COMMENT "Running generate.py on ${p_meta}" VERBATIM
      )
    
  endforeach()

  # export values
  set(${headers}  ${p_headers}  PARENT_SCOPE)
  set(${sources}  ${p_sources}  PARENT_SCOPE)
  set(${meta_dir} ${p_meta_dir} PARENT_SCOPE)
endfunction()
