# sciDosPost
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  getAllFieldValues.f90
  mod_netcdf_tools.f90
  mod_sciDOSpost_tools.f90
  mod_time_tools.f90
  sciDOSpost.f90

  ../PostProcess/ClassData.f90
  ../PostProcess/Errors.f90
  ../PostProcess/Memory.f90
  ../PostProcess/sppCallback.f90

  # See note below
  ../PostProcess/Extract_fi.f90
  ../PostProcess/GetTimes_fi.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_executable(sciDosPost
    ${source_list_f90}
  )

  # sciDosPost needs modules from scipp, but since we can't link executables we are
  # just going to build them again here. Send them to a different directory to avoid
  # errors when using ninja.
  set_target_properties(sciDosPost
    PROPERTIES
      Fortran_MODULE_DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}/sciDosPost"
  )

  # Still need to include modules built in other libraries though
  target_include_directories(sciDosPost
    PRIVATE
      "${CMAKE_Fortran_MODULE_DIRECTORY}"
  )

  target_link_libraries(sciDosPost
    PRIVATE
      $<$<PLATFORM_ID:Windows>:arap>
      $<$<PLATFORM_ID:Windows>:sag>
      $<$<PLATFORM_ID:Windows>:landuse>
      $<$<PLATFORM_ID:Windows>:messages>
      $<$<PLATFORM_ID:Windows>:scipuff>
      $<$<PLATFORM_ID:Windows>:swim>

      SCIPtool
      systool
      datums::datums

      netCDF::netcdf
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(sciDosPost)

  # Install
  install(
    TARGETS sciDosPost
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
