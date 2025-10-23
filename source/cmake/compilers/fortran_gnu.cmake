# gfortran

################################################################################
# Compiler flags
################################################################################

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(fc_flags
    -ffree-line-length-none
    # -fno-second-underscore
    # -fno-align-commons

    # Turn errors with argument calls to ADDRESS_OF function (C function,
    # void* arg) in ARAPlib into warnings
    -fallow-argument-mismatch
  )

  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:${fc_flags}>")
else()
  color_message(FATAL_ERROR red "gfortran is only supported on Linux")
endif()

################################################################################
# Client configuration
################################################################################
client_configure_compiler()