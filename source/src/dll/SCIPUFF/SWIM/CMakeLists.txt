# swim
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  SWIMaddNest.f90
  SWIMaddPolarField.f90
  SWIMaddSmoothField.f90
  SWIMallocField.f90
  SWIMbuildCliList.f90
  SWIMbuildFixedObs.f90
  SWIMbuildObsGrid.f90
  SWIMbuildObsList.f90
  SWIMcalcBL.f90
  SWIMcheckTerrainHeader.f90
  SWIMcombObsField.f90
  SWIMCurrentSrfMet.f90
  SWIMexit.f90
  SWIMexpandMet.f90
  SWIMFFTMcWIF.f90
  SWIMFFTMcWIFsub.f90
  SWIMgetGamma.f90
  SWIMgetSunFac.f90
  SWIMgetTopog.f90
  SWIMhalt.f90
  SWIMinit.f90
  SWIMinitFixed.f90
  SWIMinitGridded.f90
  SWIMinitSCIP.f90
  SWIMinitLSV.f90
  SWIMinitMcWIF.f90
  SWIMinitMEDOC.f90
  SWIMinitMG.f90
  SWIMinitObs.f90
  SWIMinitObsAssim.f90
  SWIMinitObsField.f90
  SWIMinitRun.f90
  SWIMinitWRF.f90
  SWIMinterpGridded.f90
  SWIMinterpNest.f90
  SWIMinterpObs.f90
  SWIMInterpPointer.f90
  SWIMinterpTime.f90
  SWIMlogMessage.f90
  SWIMMcWIF.f90
  SWIMMcWIFmod.f90
  SWIMmessage.f90
  SWIMnest.f90
  SWIMnextField.f90
  SWIMobsSort.f90
  SWIMobsSrfLayer.f90
  SWIMobsVertScale.f90
  SWIMoutput.f90
  SWIMoutputMEDOC.f90
  SWIMprofileBL.f90
  SWIMpuffTopog.f90
  SWIMreadSCIP.f90
  SWIMreadMEDOC.f90
  SWIMreadTerrain.f90
  SWIMreadTerrainNest.f90
  SWIMreadWRF.f90
  SWIMreallocFields.f90
  SWIMrelaxMcWIF.f90
  SWIMreportError.f90
  SWIMsetGridInterp.f90
  SWIMsetObsWt.f90
  SWIMsetPrecipProb.f90
  SWIMshearGrad.f90
  SWIMsimpleBL.f90
  SWIMsmoothFilter.f90
  SWIMstandardAtmos.f90
  SWIMturbProfile.f90
  SWIMupdateBL.f90
  SWIMupdateGriddedMet.f90
  SWIMupdateMet.f90
  SWIMupdateObsField.f90
  SWIMupdatePolarField.f90
  SWIMupdateSmoothField.f90
  SWIMutil.f90
  SWIMutilArrayPointer.f90
  SWIMutilBL.f90
  SWIMutilInterface.f90
  SWIMutilMap.f90
  SWIMversion.f90
  SWIMvertWt.f90
  
  inc/SWIM_fi.f90
  inc/SWIMchar8Array_fd.f90
  inc/SWIMcurrentMet_fi.f90
  inc/SWIMgridCoord_fd.f90
  inc/SWIMobsInterp_fd.f90
  inc/SWIMvertGrid_fd.f90
  inc/Zsort.f90
)



  # Also used in uwm
  list(APPEND source_list_f90
    inc/SWIM_fd.f90
    inc/SWIMinterp_fd.f90
  )


  list(APPEND source_list_f90
    SWIMcurrentMet.f90
  )

  list(APPEND source_list_f90
    SWIMinitAERpfl.f90
    SWIMinitAERsfc.f90
    SWIMinitASOS1min.f90
    SWIMinitMEDOClist.f90

    inc/logMessage_fd.f90
  )

    list(APPEND source_list_f90
      ../uwm/SmoothFilter.f90
    )




  list(APPEND source_list_f90
    ../../util/ClimoReader/netcdf_fd.f90
  )
  list(APPEND source_list_inc
    ../../util/ClimoReader/netcdf.inc
  )

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
  COPY source_list_inc
)

if(NOT client_preprocess_only)
  add_library(swim ${source_list_f90})

  target_link_libraries(swim
    PRIVATE
      arap
      landuse
      messages
      sag
      scipuff
      systool
      datums::datums

      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"

      # Optional libraries

      # External libraries (or aliased stubs)
      "netCDF::netcdf"
  )
  
  target_include_directories(swim
    PRIVATE
      "${CURRENT_SOURCE_DIR}/src/dll/util/ClimoReader"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(swim)

  # Install
  install(
    TARGETS swim
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
