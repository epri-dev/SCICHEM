# arap
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  cfft.f90
  cformat.f90
  clnpar.f90
  clower.f90
  cupper.f90
  deblank.f90
  erfc.f90
  erfci.f90
  erfcinv.f90
  fzero.f90
  get_next_data.f90
  getfile.f90
  getvalue.f90
  heavi.f90
  imtql2.f90
  interp_2d.f90
  interp.f90
  julian.f90
  locate.f90
  lognorm.f90
  MapProjections.f90
  matchstr.f90
  matinv.f90
  MatRot.f90
  nblank.f90
  pythag.f90
  radb2.f90
  radb3.f90
  radb4.f90
  radb5.f90
  radbg.f90
  radf2.f90
  radf3.f90
  radf4.f90
  radf5.f90
  radfg.f90
  random.f90
  reallocate.f90
  rfftb.f90
  rfftb1.f90
  rfftf.f90
  rfftf1.f90
  rffti.f90
  rffti1.f90
  sspev.f90
  tom624.f90
  tqlrat.f90
  trbak3.f90
  tred3.f90
  trig_degrees.f90
  untabify.f90
  vsort.f90
)

set(source_list_f
  dogleg.f
  dpmpar.f
  enorm.f
  fdjac1.f
  hybrd.f
  hybrj.f
  qform.f
  qrfac.f
  r1mpyq.f
  r1updt.f
)

set(source_list_c
  C2Fortran.c
)

set(source_list_h
  C2Fortran.h
)

if(TARGET SCIPUFFgui)
  # These files only work for Windows builds
  list(APPEND source_list_c
    dib.c
    pcx.c
  )

  list(APPEND source_list_c
    showdib.h
  )
endif()




# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
  COPY source_list_f source_list_c source_list_c_copy source_list_h source_list_h_copy
)

if(NOT client_preprocess_only)
  # Compile C separately so that we don't get compile flags mixed up.
  # Create as an object library so that we can combine everything back
  # together in the main library.
  add_library(arap_c OBJECT ${source_list_c})

  # Add C definitions
  target_compile_definitions(arap_c PRIVATE
    "$<$<PLATFORM_ID:Linux>:Linux>"
    "$<$<PLATFORM_ID:Linux>:__linux>"
    NOFUNCDEF
  )

  add_library(arap STATIC
    ${source_list_f90}
    ${source_list_f}
    $<TARGET_OBJECTS:arap_c>
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(arap arap_c)

  # Install
  if(scipuff_install_static_libs)
    install(
      TARGETS arap
      EXPORT "${TARGETS_EXPORT_NAME}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      PUBLIC_HEADER DESTINATION "${include_install_dir}"
    )
  endif()
endif()

