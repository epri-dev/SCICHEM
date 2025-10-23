# SCIPtool
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  SCIPButton.f90
  SCIPCheck.f90
  SCIPContour.f90
  SCIPCount.f90
  SCIPCurrent.f90
  SCIPDefault.f90
  SCIPDelete.f90
  SCIPDraw.f90
  SCIPError.f90
  SCIPExcept.f90
  SCIPExitTool.f90
  SCIPExitToolSimple.f90
  SCIPField.f90
  SCIPInitTool.f90
  SCIPInitToolSimple.f90
  SCIPLoad.f90
  SCIPNewProject.f90
  SCIPPlotList.f90
  SCIPProject.f90
  SCIPRestart.f90
  SCIPRun.f90
  SCIPSubstrate.f90
  SCIPTable.f90
  SCIPTerrain.f90
  SCIPTransform.f90
  SCIPVersion.f90
  SCIPWrite.f90
  
  inc/SCIPtool.f90
)






  list(APPEND source_list_f90
    SCIPAdjointFilter.f90
  )



# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(SCIPtool ${source_list_f90})
  target_link_libraries(SCIPtool
    PRIVATE
      arap
      clippednormal
      landuse
      messages
      sag
      scipuff
      swim
      systool
      
      datums::datums

      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"

      # Optional libraries

      # External libraries (or aliased stubs)
  )
    
  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(SCIPtool)

  # Install
  # Optional to allow partial SWIM install for SCIP
  install(
    TARGETS SCIPtool
    OPTIONAL
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()

