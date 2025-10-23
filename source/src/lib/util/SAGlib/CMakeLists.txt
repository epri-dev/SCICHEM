# sag
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  sag_back.f90
  sag_botm.f90
  sag_cell.f90
  sag_cntr.f90
  sag_comb.f90
  sag_conn.f90
  sag_draw.f90
  sag_dzon.f90
  sag_err.f90
  sag_fill.f90
  sag_find.f90
  sag_func.f90
  sag_grad.f90
  sag_init.f90
  sag_node.f90
  sag_open.f90
  sag_read.f90
  sag_skip.f90
  sag_tri.f90
  sag_util.f90
  sag_val.f90
  sag_wrt.f90
  
  inc/sagbck_fi.f90
  inc/sagcel_fd.f90
  inc/sagcnt_fd.f90
  inc/sagdef_fd.f90
  inc/sagdrw_fd.f90
  inc/sagerr_fd.f90
  inc/sagerr_fi.f90
  inc/sagfil_fd.f90
  inc/sagfun_usr.f90
  inc/saggrd_fi.f90
  inc/saglst_fd.f90
  inc/sagnod_fd.f90
  inc/sagstr_fd.f90
  inc/sagtri_fd.f90
  inc/sagwrt_fd.f90
  
  ../../SCIPUFFlib/SCIPUFF/inc/coordinate_fd.f90
  ../../SCIPUFFlib/SCIPUFF/inc/param_fd.f90
  ../../SCIPUFFlib/SCIPUFF/inc/step_p_fi.f90
  ../../SCIPUFFlib/SCIPUFF/inc/struct_fd.f90
)




# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
    list(APPEND source_list_f90
      $<TARGET_OBJECTS:odepack>
    )

  add_library(sag STATIC ${source_list_f90})
  target_link_libraries(sag
    PRIVATE
      arap
      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(sag)

  # Install
  if(scipuff_install_static_libs)
    install(
      TARGETS sag
      EXPORT "${TARGETS_EXPORT_NAME}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      PUBLIC_HEADER DESTINATION "${include_install_dir}"
    )
  endif()
endif()
