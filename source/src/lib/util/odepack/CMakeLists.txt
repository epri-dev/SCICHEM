# odepack
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f
  adjlr.f
  aigbt.f
  ainvg.f
  bnorm.f
  cdrv.f
  cfode.f
  cntnzu.f
  decbt.f
  ewset.f
  fnorm.f
  intdy.f
  iprep.f
  isamax.f
  jgroup.f
  lsoda.f
  lsodar.f
  lsode.f
  lsode_bd.f
  lsode_sl.f
  lsodes.f
  lsodi.f
  lsoibt.f
  md.f
  mdi.f
  mdm.f
  mdp.f
  mdu.f
  nnfc.f
  nnsc.f
  nntc.f
  nroc.f
  nsfc.f
  odrv.f
  pjibt.f
  prep.f
  prepj.f
  prepji.f
  prja.f
  prjs.f
  r1mach.f
  rchek.f
  roots.f
  saxpy.f
  scopy.f
  sdot.f
  sgbfa.f
  sgbsl.f
  sgefa.f
  sgesl.f
  slsbt.f
  slss.f
  solbt.f
  solsy.f
  srcar.f
  srcma.f
  srcms.f
  srcom.f
  sro.f
  sscal.f
  stoda.f
  stode.f
  stodi.f
  vmnorm.f
  vnorm.f
  xerrwv.f
  xsetf.f
  xsetun.f
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(odepack OBJECT ${source_list_f})

  # Use OpenMP if requested
  if(client_use_openmp)
    target_link_libraries(odepack
      PRIVATE
        OpenMP::OpenMP_Fortran
    )
  endif()

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(odepack)
endif()
