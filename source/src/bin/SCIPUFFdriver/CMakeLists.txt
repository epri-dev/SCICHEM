# scipuffdriver
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  AERMODmet_fd.f90
  AERMODreceptor_fd.f90
  AERMODrel_fd.f90
  ComputeDuration.f90
  FileInput.f90
  GenSCIPUFFAreaSource.f90
  GenSCIPUFFSensor.f90
  GenSCIPUFFWeather.f90
  GetDomRefFromWeather.f90
  GetRelTime.f90
  GetTimeFromWeather.f90
  GetZrufFromWeather.f90
  InitAERMOD.f90
  InitSCIPUFFProject.f90
  ParseDataCO.f90
  ParseDataEV.f90
  ParseDataMA.f90
  ParseDataME.f90
  ParseDataOU.f90
  ParseDataRE.f90
  ParseDataSO.f90
  ParseDataTE.f90
  ReadAERMODInput.f90
  ReadArrayLimits.f90
  ReadEmissionFile.f90
  RunAERMODInp.f90
  SCIPUFFdriver_fi.f90
  SetSCIPUFFDomain.f90
  SetTimeZone.f90
  UpdateStackEmission.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(scipuffdriver STATIC ${source_list_f90})

  target_link_libraries(scipuffdriver
    PRIVATE
      arap
      SCIPtool
      landuse
      messages
      scipuff
      swim
      systool
      xpp
      datums::datums

      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"

      # Optional libraries

      # External libraries (or aliased stubs)
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(scipuffdriver)

  # Install
  if(scipuff_install_static_libs)
    install(
      TARGETS scipuffdriver
      EXPORT "${TARGETS_EXPORT_NAME}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      PUBLIC_HEADER DESTINATION "${include_install_dir}"
    )
  endif()
endif()
