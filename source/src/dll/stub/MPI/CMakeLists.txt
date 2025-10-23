# MPI
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  localMPI.F90
  stubMPI.f90
)

set(source_list_c
  stubMPIc.c
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
  COPY source_list_c
)

if(NOT client_preprocess_only)
  add_library(stubmpi_c OBJECT ${source_list_c})
  add_library(stubmpi STATIC
    ${source_list_f90}
    $<TARGET_OBJECTS:stubmpi_c>
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(stubmpi)

  # Install
  if(scipuff_install_static_libs)
    install(
      TARGETS stubmpi
      EXPORT "${TARGETS_EXPORT_NAME}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      PUBLIC_HEADER DESTINATION "${include_install_dir}"
    )
  endif()
endif()
