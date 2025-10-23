# landuse
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  # landuse_fd.f90
  landuse.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(landuse ${source_list_f90})
  
  target_link_libraries(landuse
    PRIVATE
      arap
      scipuff
      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(landuse)

  # Install
  install(
    TARGETS landuse
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
