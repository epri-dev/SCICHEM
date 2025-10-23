# SCIPUFFgui
cmake_minimum_required(VERSION 3.20)

if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  color_message(FATAL_ERROR red "SCIPUFFgui can only be compiled on a Windows system!")
endif()

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  ConTri/inc/contri_fd.f90
  ConTri/inc/contri_fi.f90

  ConTri/conbck.f90
  ConTri/condrw.f90
  ConTri/confun.f90
  ConTri/conmap.f90
  ConTri/conovl.f90
  ConTri/conpal.f90
  ConTri/conplt.f90
  ConTri/conpts.f90
  ConTri/conres.f90
  ConTri/conrmk.f90
  ConTri/conset.f90

  lib/inc/animate_fi.f90
  lib/inc/commons.f90
  lib/inc/create_fd.f90
  lib/inc/create_fi.f90
  lib/inc/defineok_fd.f90
  lib/inc/dialog_fd.f90
  lib/inc/dialog_fi.f90
  lib/inc/dib_mod.f90
  lib/inc/FXgui_mod.f90
  lib/inc/guiAPI.f90
  lib/inc/GUIerror_fi.f90
  lib/inc/guimatl_fd.f90
  lib/inc/guimatl_fi.f90
  lib/inc/GUIparam_fd.f90
  lib/inc/guistruct_fd.f90
  lib/inc/guitool_fi.f90
  lib/inc/guiversion_fd.f90
  lib/inc/hascal_fd.f90
  lib/inc/hascal_fi.f90
  lib/inc/metflag_fd.f90
  lib/inc/mettype_fd.f90
  lib/inc/mouse_fd.f90
  lib/inc/mouse_fi.f90
  lib/inc/pcscipuf_fi.f90
  lib/inc/plotdlg_fd.f90
  lib/inc/plotdlg_fi.f90
  lib/inc/plotopt_fd.f90
  lib/inc/pltchoice_fi.f90
  lib/inc/pltopt_fd.f90
  lib/inc/printdev_fi.f90
  lib/inc/project_fd.f90
  lib/inc/reldef_fd.f90
  lib/inc/release_gui_fd.f90
  lib/inc/resource_fd.f90
  lib/inc/restart_fi.f90
  lib/inc/SCIAPIversion_fd.f90
  lib/inc/script_fd.f90
  lib/inc/script_fi.f90
  lib/inc/units_fd.f90
  lib/inc/urban_fd.f90
  lib/inc/urban_fi.f90

  lib/abort.f90
  lib/animate.f90
  lib/audit.f90
  lib/axes.f90
  lib/bitmap.f90
  lib/buildlst.f90
  lib/buttons.f90
  lib/checkbox.f90
  lib/combobox.f90
  lib/comptime.f90
  lib/convert_domain.f90
  lib/convert_inp.f90
  lib/convert_met.f90
  lib/convert_mtl.f90
  lib/convert_scn.f90
  lib/convert_time.f90
  lib/dbox_can.f90
  lib/dbox_chk.f90
  lib/dbox_ini.f90
  lib/dbox_sav.f90
  lib/default.f90
  lib/dialproc.f90
  lib/display.f90
  lib/domdef.f90
  lib/editbox.f90
  lib/error.f90
  lib/filedlg.f90
  lib/files.f90
  lib/findfile.f90
  lib/FX.f90
  lib/getinfo.f90
  lib/hookproc.f90
  lib/hwndlist.f90
  lib/incident.f90
  lib/intbox.f90
  lib/loadpltcombo.f90
  lib/mainproc.f90
  lib/material.f90
  lib/metdef_p1.f90
  lib/metdef_p2.f90
  lib/mouse.f90
  lib/open_prj.f90
  lib/options.f90
  lib/owndraw.f90
  lib/plotcas.f90
  lib/plotproc.f90
  lib/plotstat.f90
  lib/plottri.f90
  lib/plotwind.f90
  lib/pltchoice.f90
  lib/print.f90
  lib/progproc.f90
  lib/progress.f90
  lib/radiobtn.f90
  lib/read_nml.f90
  lib/readcmd.f90
  lib/realbox.f90
  lib/release.f90
  lib/savefile.f90
  lib/scipuf_c.f90
  lib/scipuf_r.f90
  lib/scrollbar.f90
  lib/settitle.f90
  lib/showcntl.f90
  lib/statproc.f90
  lib/table.f90
  lib/timdef.f90
  lib/toolproc.f90
  lib/urbdef.f90
  lib/utility.f90
  lib/verify.f90
  lib/version.f90

  NCARlib/inc/lastset_fi.f90
  NCARlib/inc/ncar_mod.f90
  NCARlib/inc/ncarlib_fi.f90

  NCARlib/exitncar.f90
  NCARlib/getncar.f90
  NCARlib/gfa_gpl.f90
  NCARlib/gridal.f90
  NCARlib/gscr.f90
  NCARlib/gssetc.f90
  NCARlib/initncar.f90
  NCARlib/labmod.f90
  NCARlib/pens.f90
  NCARlib/plchhqx.f90
  NCARlib/set.f90
  NCARlib/setmap.f90
  NCARlib/setncar.f90

  SCIPUFFgui.f90
)

set(source_list_c
  ../../lib/util/ARAPlib/dib.c
  ../../lib/util/ARAPlib/pcx.c
)

set(source_list_h
  ../../lib/util/ARAPlib/showdib.h
)

set(source_list_rc
  rc/SCIPUFFgui.rc
)

set(source_list_rc_copy
  rc/brush.bmp
  rc/check3.ico
  rc/clock1.cur
  rc/clock2.cur
  rc/clock3.cur
  rc/clock4.cur
  rc/clock5.cur
  rc/clock6.cur
  rc/clock7.cur
  rc/clock8.cur
  rc/compass1.ico
  rc/compass2.ico
  rc/dropper.cur
  rc/fast.ico
  rc/hascal.ico
  rc/SCIPlogo.bmp
  rc/magnify2.cur
  rc/nb_symbol11.ico
  rc/nb_symbol12.ico
  rc/nb_symbol1.ico
  rc/nb_symbol2.ico
  rc/nb_symbol3.ico
  rc/nb_symbol4.ico
  rc/nb_symbol6.ico
  rc/nb_symbol8.ico
  rc/nb_symbol9.ico
  rc/nweapon.ico
  rc/resource.h
  rc/resrc1.fd
  rc/resrc1.h
  rc/scipuff0.ico
  rc/scipuff1.ico
  rc/scipuff2.ico
  rc/scipuff3.ico
  rc/scipuff4.ico
  rc/SCIPUFFgui_pd.rc
  rc/SCIPUFFlogo.bmp
  rc/scissors2.cur
  rc/symbol10.ico
  rc/symbol13.ico
  rc/symbol5.ico
  rc/symbol7.ico
  rc/version.h
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
  COPY source_list_c source_list_h source_list_rc source_list_rc_copy
)

if(NOT client_preprocess_only)
  # Check for Windows system libraries
  find_package(MFC REQUIRED)
  set(CMAKE_MFC_FLAG 1) # Statically link MFC
  find_package(HTMLHelp REQUIRED)

  add_library(SCIPUFFgui_c STATIC
    ${source_list_c}
  )

  # WIN32 application!
  add_executable(SCIPUFFgui WIN32
    ${source_list_f90}
    ${source_list_rc}
  )

  target_link_libraries(SCIPUFFgui
    PRIVATE
      SCIPUFFgui_c
      
      arap
      SCIPtool
      landuse
      messages
      swim
      scipuff
      systool
      datums::datums

      # External libraries
      "${HTML_HELP_LIBRARY}"
  )

  target_compile_definitions(SCIPUFFgui PRIVATE -D_AFXDLL)
  set_target_properties(SCIPUFFgui PROPERTIES LINKER_LANGUAGE Fortran)

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(SCIPUFFgui)

  # Install
  install(
    TARGETS SCIPUFFgui
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
