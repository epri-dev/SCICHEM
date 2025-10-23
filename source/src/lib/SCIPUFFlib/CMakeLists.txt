# scipuff
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  FileMgr/init_rel.f90
  FileMgr/load_ctrl.f90
  FileMgr/load_domain.f90
  FileMgr/load_end.f90
  FileMgr/load_flags.f90
  FileMgr/load_msc.f90
  FileMgr/load_mtl.f90
  FileMgr/load_options.f90
  FileMgr/load_rel.f90
  FileMgr/load_start.f90
  FileMgr/read_ctrl.f90
  FileMgr/read_domain.f90
  FileMgr/read_end.f90
  FileMgr/read_flags.f90
  FileMgr/read_matdef.f90
  FileMgr/read_met.f90
  FileMgr/read_msc.f90
  FileMgr/read_mtl.f90
  FileMgr/read_options.f90
  FileMgr/read_rel.f90
  FileMgr/read_scn.f90
  FileMgr/read_start.f90
  FileMgr/util_mgr.f90
  FileMgr/util_msc.f90
  FileMgr/util_mtl.f90
  FileMgr/util_rel.f90
  
  FileMgr/inc/basic_fd.f90
  FileMgr/inc/convert_fd.f90
  FileMgr/inc/flags_fd.f90
  FileMgr/inc/list_fd.f90
  FileMgr/inc/matdef_fi.f90
  FileMgr/inc/material_fd.f90
  FileMgr/inc/message_fd.f90
  FileMgr/inc/options_fd.f90
  FileMgr/inc/release_fd.f90
  FileMgr/inc/status_fd.f90
  FileMgr/inc/terrain_fd.f90
  FileMgr/inc/time_fd.f90
  FileMgr/inc/version_fd.f90
  FileMgr/inc/weather_fd.f90
  
  PlotMgr/adjoint_util.f90
  PlotMgr/adjoint.f90
  PlotMgr/clipnorm.f90
  PlotMgr/getpoint.f90
  PlotMgr/metplot.f90
  PlotMgr/plotcopy.f90
  PlotMgr/plotlist.f90
  PlotMgr/plotmet_fi.f90
  PlotMgr/plotread.f90
  PlotMgr/poparea.f90
  PlotMgr/readProject.f90
  PlotMgr/slice.f90
  PlotMgr/sliceh.f90
  PlotMgr/slicev.f90
  PlotMgr/slicevint.f90
  PlotMgr/writeAVS.f90
  PlotMgr/writeCTS.f90
  PlotMgr/writeEIS.f90
  PlotMgr/writeOIL.f90
  PlotMgr/writeOVL.f90
  PlotMgr/writeUSA.f90
  
  PlotMgr/inc/adjoint_fi.f90
  PlotMgr/inc/cellstr_fd.f90
  PlotMgr/inc/classdata_fd.f90
  PlotMgr/inc/contour_fd.f90
  PlotMgr/inc/contourlist_fd.f90
  PlotMgr/inc/field_fd.f90
  PlotMgr/inc/plotlist_fd.f90
  PlotMgr/inc/plotlist_fi.f90
  PlotMgr/inc/plotmet_fd.f90
  PlotMgr/inc/pointval_fi.f90
  PlotMgr/inc/poparea_fd.f90
  PlotMgr/inc/poparea_fi.f90
  PlotMgr/inc/slice_fd.f90
  PlotMgr/inc/slice_fi.f90
  PlotMgr/inc/type_fd.f90
  PlotMgr/inc/write_fd.f90
  PlotMgr/inc/write_fi.f90
  
  SCIMgr/abort.f90
  SCIMgr/adjoint_filter.f90
  SCIMgr/button.f90
  SCIMgr/callback.f90
  SCIMgr/check.f90
  SCIMgr/checkerr.f90
  SCIMgr/checkinp.f90
  SCIMgr/checkutil.f90
  SCIMgr/contour.f90
  SCIMgr/convert.f90
  SCIMgr/count.f90
  SCIMgr/ctrl.f90
  SCIMgr/current.f90
  SCIMgr/defaultinp.f90
  SCIMgr/delete.f90
  SCIMgr/domain.f90
  SCIMgr/drawfield.f90
  SCIMgr/drawgrid.f90
  SCIMgr/end.f90
  SCIMgr/except.f90
  SCIMgr/exitsimple.f90
  SCIMgr/exittool.f90
  SCIMgr/fileutil.f90
  SCIMgr/flags.f90
  SCIMgr/getlandUseError.f90
  SCIMgr/initsimple.f90
  SCIMgr/inittool.f90
  SCIMgr/input.f90
  SCIMgr/loadinp.f90
  SCIMgr/loadmet.f90
  SCIMgr/loadprj.f90
  SCIMgr/loadpuf.f90
  SCIMgr/loadter.f90
  SCIMgr/material.f90
  SCIMgr/newprj.f90
  SCIMgr/options.f90
  SCIMgr/plotfield.f90
  SCIMgr/plotinit.f90
  SCIMgr/plottable.f90
  SCIMgr/relcheck_fi.f90
  SCIMgr/relcheck.f90
  SCIMgr/release.f90
  SCIMgr/rstprj.f90
  SCIMgr/runprj.f90
  SCIMgr/runscipuff.f90
  SCIMgr/sendmsg.f90
  SCIMgr/state.f90
  SCIMgr/substrate.f90
  SCIMgr/time_util.f90
  SCIMgr/time.f90
  SCIMgr/toolerror.f90
  SCIMgr/toolrun.f90
  SCIMgr/toolstart.f90
  SCIMgr/toolutil.f90
  SCIMgr/toolversion.f90
  SCIMgr/transform.f90
  SCIMgr/update.f90
  SCIMgr/utm.f90
  SCIMgr/weather.f90
  SCIMgr/writefield.f90
  SCIMgr/writeinp.f90
  
  SCIMgr/inc/adjoint_filter_fi.f90
  SCIMgr/inc/checkerr_fd.f90
  SCIMgr/inc/datums_mod.f90
  SCIMgr/inc/domstruct_fd.f90
  SCIMgr/inc/environment_fd.f90
  SCIMgr/inc/SCIPresults_fd.f90
  SCIMgr/inc/inpstruct_fd.f90
  SCIMgr/inc/limitT_fd.f90
  SCIMgr/inc/metstruct_fd.f90
  SCIMgr/inc/mtlstruct_fd.f90
  SCIMgr/inc/plotfunc_fi.f90
  SCIMgr/inc/prjstruct_fd.f90
  SCIMgr/inc/relstruct_fd.f90
  SCIMgr/inc/SCIMgr_fd.f90
  SCIMgr/inc/SCIMgr_fi.f90
  SCIMgr/inc/SCIMgrParam_fd.f90
  SCIMgr/inc/SCIMgrState.f90
  SCIMgr/inc/sensor_fd.f90
  SCIMgr/inc/spcstruct_fd.f90
  SCIMgr/inc/state_fd.f90
  SCIMgr/inc/statstruct_fd.f90
  SCIMgr/inc/structure_fd.f90
  SCIMgr/inc/timstruct_fd.f90
  SCIMgr/inc/tooluser_fd.f90
  SCIMgr/inc/update_fd.f90
  
  SCIPUFF/accumsrf.f90
  SCIPUFF/aerosol.f90
  SCIPUFF/cont_rel_dyn.f90
  SCIPUFF/cont_rel_init.f90
  SCIPUFF/cont_rel_static.f90
  SCIPUFF/cont_rel_util.f90
  SCIPUFF/deallocate.f90
  SCIPUFF/dezone.f90
  SCIPUFF/error.f90
  SCIPUFF/get_matl.f90
  SCIPUFF/get_metSWIM.f90
  SCIPUFF/get_topogSTUB.f90
  SCIPUFF/get_topSWIM.f90
  SCIPUFF/halt.f90
  SCIPUFF/init_met.f90
  SCIPUFF/init_srf.f90
  SCIPUFF/init_wash.f90
  SCIPUFF/initial.f90
  SCIPUFF/inp_puff.f90
  SCIPUFF/inst_rel_dyn.f90
  SCIPUFF/inter_dyn.f90
  SCIPUFF/los_sensor.f90
  SCIPUFF/mapfac.f90
  SCIPUFF/merge.f90
  SCIPUFF/output.f90
  SCIPUFF/puff_file.f90
  SCIPUFF/puff_rel_dyn.f90
  SCIPUFF/read_prj.f90
  SCIPUFF/rebin.f90
  SCIPUFF/reflect.f90
  SCIPUFF/restart.f90
  SCIPUFF/run.f90
  SCIPUFF/sampler.f90
  SCIPUFF/SamplerGridOutput.f90
  SCIPUFF/SamplerInput.f90
  SCIPUFF/SamplerInputUtil.f90
  SCIPUFF/SamplerUtil.f90
  SCIPUFF/sat_humid.f90
  SCIPUFF/scipuff.f90
  SCIPUFF/set_grid.f90
  SCIPUFF/set_ip.f90
  SCIPUFF/set_tlev.f90
  SCIPUFF/settle.f90
  SCIPUFF/split.f90
  SCIPUFF/splitz.f90
  SCIPUFF/srf_evap.f90
  SCIPUFF/start.f90
  SCIPUFF/step_drop.f90
  SCIPUFF/step_p_dyn.f90
  SCIPUFF/step_pool.f90
  SCIPUFF/step.f90
  SCIPUFF/surface.f90
  SCIPUFF/time_cnv.f90
  SCIPUFF/ufall.f90
  SCIPUFF/util_evap.f90
  SCIPUFF/util_inter.f90
  SCIPUFF/util_ip.f90
  SCIPUFF/util_is.f90
  SCIPUFF/util_matl.f90
  SCIPUFF/util_met.f90
  SCIPUFF/util_puff.f90
  SCIPUFF/util_srf.f90
  SCIPUFF/util.f90
  SCIPUFF/utilmtlaux.f90
  SCIPUFF/vd_slinn.f90
  SCIPUFF/version.f90
  SCIPUFF/wrt_prj.f90
  
  SCIPUFF/inc/aerosol_fi.f90
  SCIPUFF/inc/class_fd.f90
  SCIPUFF/inc/constants_fd.f90
  SCIPUFF/inc/cont_rel_fi.f90
  SCIPUFF/inc/default_fd.f90
  SCIPUFF/inc/dezone_fi.f90
  SCIPUFF/inc/error_fi.f90
  SCIPUFF/inc/errorParam_fd.f90
  SCIPUFF/inc/files_fi.f90
  SCIPUFF/inc/inter_fi.f90
  SCIPUFF/inc/ipgrd_fi.f90
  SCIPUFF/inc/MapCoord_fd.f90
  SCIPUFF/inc/met_fi.f90
  SCIPUFF/inc/metparam_fd.f90
  SCIPUFF/inc/refl_fi.f90
  SCIPUFF/inc/relparam_fd.f90
  SCIPUFF/inc/sampler_fd.f90
  SCIPUFF/inc/sampler_fi.f90
  SCIPUFF/inc/scipuff_fi.f90
  SCIPUFF/inc/search_fd.f90
  SCIPUFF/inc/search_fi.f90
  SCIPUFF/inc/srfaux_fi.f90
  SCIPUFF/inc/srfdos_fd.f90
  SCIPUFF/inc/srfevap_fi.f90
  SCIPUFF/inc/srfparam_fd.f90
  SCIPUFF/inc/structv04_fd.f90
  SCIPUFF/inc/structv13_fd.f90
  SCIPUFF/inc/surface_fd.f90
  SCIPUFF/inc/surface_fi.f90

  SCIutil/combine_filter.f90
  SCIutil/combine.f90

  SCIutil/inc/combine_fd.f90
  SCIutil/inc/simple_fd.f90
  SCIutil/inc/simple_fi.f90
  
  ../../dll/SCIPUFF/LandUse/landuse_fd.f90
  
  ../../dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.f90
  ../../dll/SCIPUFF/SWIM/inc/SWIMinit_fd.f90
  ../../dll/SCIPUFF/SWIM/inc/SWIMparam_fd.f90
  ../../dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.f90
)

  list(APPEND source_list_f90
    # Also in fHDF5
    FileMgr/inc/domain_fd.f90

    SCIMgr/inc/charT_fd.f90
    SCIMgr/inc/uniformGridT_fd.f90
    SCIMgr/inc/domainCoord_fd.f90
  )





  list(APPEND source_list_f90
    PlotMgr/plotchem.f90

    SCIPUFF/ambchem.f90
    SCIPUFF/chemReactions.f90
    SCIPUFF/initchem.f90
    SCIPUFF/interchem.f90
    SCIPUFF/stepchem.f90
    SCIPUFF/utilchem.f90

    SCIPUFF/inc/chem_fi.f90
  )


    list(APPEND source_list_f90
      SCIPUFF/stepChemAmb.f90
      SCIPUFF/StepChemMPI.f90
    )

    list(APPEND source_list_f90
      SCIPUFF/inc/multcomp_fd.f90
    )





  list(APPEND source_list_f90
    SCIPUFF/drydep.f90
    SCIPUFF/SamplerBinaryOut.f90
    SCIPUFF/set_stack_rel_prime.f90
    SCIPUFF/set_stack_rel_prise.f90

    SCIPUFF/inc/MPIFunc_fi.f90
    SCIPUFF/inc/sciprime_fi.f90

    ../AQAER/AqAerIface.f90
    
    ../AQAER/AE5/aero/inc/aer_sections_inc.f90
    ../AQAER/AE5/aero/inc/aero_consts_inc.f90
    ../AQAER/AE5/aero/inc/aero_species_inc.f90

    ../AQAER/AE5/aero/aerosol_chem.f90
    ../AQAER/AE5/aero/init_aero.f90
    ../AQAER/AE5/aero/init_aero_map.f90
    ../AQAER/AE5/aero/inorg_aero.f90
    ../AQAER/AE5/aero/set_vdry.f90

    ../AQAER/AE5/aqueous/inc/aqueous_consts_inc.f90
    ../AQAER/AE5/aqueous/inc/aqueous_species_inc.f90

    ../AQAER/AE5/aqueous/init_aqueous.f90
    ../AQAER/AE5/aqueous/step_aqueous.f90

    ../PRIME/isc_prime.f90
  )

  list(APPEND source_list_f
    ../AQAER/AE5/aero/AERO_INFO.f
    ../AQAER/AE5/aero/aero_subs.f
    ../AQAER/AE5/aero/getdep_v.f
    ../AQAER/AE5/aero/hetchem.f
    ../AQAER/AE5/aero/isocom.f
    ../AQAER/AE5/aero/isofwd.f
    ../AQAER/AE5/aero/isorev.f
    ../AQAER/AE5/aero/orgaer5.f
    ../AQAER/AE5/aero/SOA_NEWT.f

    ../AQAER/AE5/aqueous/inc/cmaq_species_inc.f

    ../AQAER/AE5/aqueous/aqradm.f
    ../AQAER/AE5/aqueous/getalpha.f
    ../AQAER/AE5/aqueous/hlconst.f
    ../AQAER/AE5/aqueous/indexn.f
    ../AQAER/AE5/aqueous/vode.f

    ../PRIME/modules.f
    ../PRIME/prime_util.f
    ../PRIME/prime.f
  )

  # INCLUDEd files
  list(APPEND source_list_inc
    ../AQAER/AE5/aero/inc/isrpia.inc

    ../AQAER/AE5/aqueous/inc/AE_A2AQ.EXT
    ../AQAER/AE5/aqueous/inc/AE_SCAV.EXT
    ../AQAER/AE5/aqueous/inc/AE_SPC.EXT
    ../AQAER/AE5/aqueous/inc/AQ_PARAMS.EXT
    ../AQAER/AE5/aqueous/inc/GC_G2AQ.EXT
    ../AQAER/AE5/aqueous/inc/GC_SCAV.EXT
    ../AQAER/AE5/aqueous/inc/GC_SPC.EXT
    ../AQAER/AE5/aqueous/inc/NR_N2AQ.EXT
    ../AQAER/AE5/aqueous/inc/NR_SCAV.EXT
    ../AQAER/AE5/aqueous/inc/NR_SPC.EXT
  )





















# Add any libraries compiled as object libraries
set(source_list_obj "")


  list(APPEND source_list_obj
    $<TARGET_OBJECTS:odepack>
  )


# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_f source_list_cmake
  COPY source_list_inc
)

if(NOT client_preprocess_only)
  add_library(scipuff STATIC
    ${source_list_f90}
    ${source_list_f}
    ${source_list_obj}
  )

  target_link_libraries(scipuff
    PRIVATE
      arap
      sag
      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"

      "stubmpi"
  )

  target_include_directories(scipuff
    PRIVATE
      "${PROJECT_SOURCE_DIR}/src/lib/AQAER/AE5/aero/inc"
      "${PROJECT_SOURCE_DIR}/src/lib/AQAER/AE5/aqueous/inc"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(scipuff)

  # Install
  if(scipuff_install_static_libs)
    install(
      TARGETS scipuff
      EXPORT "${TARGETS_EXPORT_NAME}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      PUBLIC_HEADER DESTINATION "${include_install_dir}"
    )
  endif()
endif()
