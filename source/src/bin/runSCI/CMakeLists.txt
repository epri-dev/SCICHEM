# runsci
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  LoadSen.f90
  runSCI.f90
)

  list(APPEND source_list_f90
    UpdateContRel.f90
    UpdateRelList_fd.f90
    UpdateRelList_fi.f90
  )

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_executable(runsci ${source_list_f90})

  target_link_libraries(runsci
    PRIVATE
      arap
      SCIPtool
      landuse
      messages
      swim
      systool
      xpp
      datums::datums

      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"

      # Optional libraries

      "scipuffdriver"
      "stubmpi"

      # External libraries (or aliased stubs)
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(runsci)

  # Install
  install(
    TARGETS runsci
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
