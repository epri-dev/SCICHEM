# Configure compiler flags and options based on detected compiler

set(compiler_config_dir ${CMAKE_SOURCE_DIR}/cmake/compilers)

# SCIPUFF creates a mix of static and dynamic libraries linked together. Need PIC.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

################################################################################
# Fortran
################################################################################

# ifort and ifx
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  include(${compiler_config_dir}/fortran_intel.cmake)

# gfortran
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  include(${compiler_config_dir}/fortran_gnu.cmake)

# unknown
else()
  color_message(FATAL_ERROR red "Unknown Fortran compiler id, ${CMAKE_Fortran_COMPILER_ID}. Please add a new compiler configuration file.")
endif()

################################################################################
# C / C++
################################################################################

set(CMAKE_C_STANDARD 99)

# Need C++11 support to avoid undefined type errors
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# GNU C
if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  include(${compiler_config_dir}/c_gnu.cmake)
  
endif()