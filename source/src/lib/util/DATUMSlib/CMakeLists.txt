# fdatums
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  fdatums_mod.f90
  fdatums.f90
  tconpc.f90
  tconst.f90
  tmgeod.f90
  tmgrid.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(fdatums ${source_list_f90})

  # Create an alias to match target name of external version
  add_library(datums::datums ALIAS fdatums)

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(fdatums)

  # Install
  install(
    TARGETS fdatums
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
