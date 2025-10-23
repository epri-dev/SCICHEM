# clippednormal
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  ClippedNormal.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(clippednormal ${source_list_f90})

  target_link_libraries(clippednormal
    PRIVATE
      arap
      scipuff
      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(clippednormal)

  # Install
  # Optional to allow partial SWIM install for SCIP
  install(
    TARGETS clippednormal
    OPTIONAL
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
