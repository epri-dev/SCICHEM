# tersci
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  mod_control.f90
  mod_main1.f90
  mod_tifftags.f90
  sub_calchc.f90
  sub_chkadj.f90
  sub_chkext.f90
  sub_cnrcnv.f90
  sub_demchk.f90
  sub_demrec.f90
  sub_demsrc.f90
  sub_domcnv.f90
  sub_initer_dem.f90
  sub_initer_ned.f90
  sub_nadcon.f90
  sub_nedchk.f90
  sub_read_tifftags.f90
  sub_reccnv.f90
  sub_recelv.f90
  sub_srccnv.f90
  sub_srcelv.f90
  sub_utmgeo.f90
  tersci.f90
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_executable(tersci ${source_list_f90})

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(tersci)

  # Install
  install(
    TARGETS tersci
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
