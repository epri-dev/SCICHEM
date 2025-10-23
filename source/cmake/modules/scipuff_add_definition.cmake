# Use this macro to add new definitions to the Fortran
# definition list, preprocessor argument list, and compile options list

macro(scipuff_add_definition definition)
  # Don't want to add duplicates
  if(NOT "${definition}" IN_LIST client_options)
    list(APPEND client_options
      ${definition}
    )
  endif()

  # Add to client_preprocess_args if preprocessing is enabled
  if(client_preprocess)
    if(NOT "-D${definition}" IN_LIST client_preprocess_args)
      list(APPEND client_preprocess_args
        "-D${definition}"
      )
    endif()
  endif()

  # add_compile_definitions requires CMake 3.12+
  get_directory_property(current_defines DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)

  if(NOT "$<$<COMPILE_LANGUAGE:Fortran>:${definition}>" IN_LIST current_defines)
    add_compile_definitions(
      "$<$<COMPILE_LANGUAGE:Fortran>:${definition}>"
    )
  endif()
endmacro()