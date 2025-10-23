# systool
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  windows/nullFunc.f90
  windows/winAPI_fd.f90
  windows/winAPI_mod.f90

  windows/SYStool/INI_mod.f90
  windows/SYStool/sysfunc.f90
)

# if(NOT "__INTEL_COMPILER" IN_LIST client_options)
#   list(APPEND source_list_f90
#     windows/args.f90
#   )
# endif()


# Only want to pick one of these
# Windows
if("WININI_OPT" IN_LIST client_options)
  list(APPEND source_list_f90
    windows/SYStool/WININI_mod.f90
  )
# Linux or Windows
elseif("FINI_OPT" IN_LIST client_options)
  list(APPEND source_list_f90
    windows/SYStool/FINI_mod.f90
  )
# Linux or Windows
elseif("CINI_OPT" IN_LIST client_options)
  list(APPEND source_list_f90
    windows/SYStool/CINI_mod.f90
  )
else()
  color_message(FATAL_ERROR red
    "Valid definition option not detected in systool. Must have one of WININI_OPT, FINI_OPT, or CINI_OPT.")
endif()


# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
)

if(NOT client_preprocess_only)
  add_library(systool ${source_list_f90})

  target_link_libraries(systool
    PRIVATE
      scipuff
      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"
  )

  # Use external C library instead of internal or Windows functions

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(systool)

  # Install
  install(
    TARGETS systool
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
