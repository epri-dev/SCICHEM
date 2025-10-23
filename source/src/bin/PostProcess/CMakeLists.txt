# scipp
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  BubbleMom.f90
  ClassData.f90
  CommandLine.f90
  Contours.f90
  CreateFields.f90
  Errors.f90
  Extract_fi.f90
  ExtractContours.f90
  ExtractGrid.f90
  ExtractHorzLines.f90
  ExtractOutput.f90
  ExtractProfiles.f90
  GetPlotLists.f90
  GetPuffs.f90
  GetSrcFunc_fi.f90
  GetSrcFunc.f90
  GetTimes_fi.f90
  Initialization.f90
  LocalPuffs_fi.f90
  MainEntryPoint.f90
  Memory.f90
  NativeGrid.f90
  Output3D.f90
  PufMom.f90
  Readpuf.f90
  RegularGrid.f90
  SelectField.f90
  sppCallback.f90
  Tables.f90
  TotalMass_fi.f90
  TotalMass.f90
  WriteFiles.f90
)




# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_executable(scipp ${source_list_f90})

  target_link_libraries(scipp
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
      
      "stubmpi"

      # External libraries (or aliased stubs)
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(scipp)

  # Install
  install(
    TARGETS scipp
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
