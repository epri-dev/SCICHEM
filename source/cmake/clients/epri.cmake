# Configuration for EPRI client

# Client name for build system checks
set(client_name
  EPRI
)

# Quick options
set(BUILD_SHARED_LIBS ON)

option(client_build_exec "Build CLI executables (runsci and scipp)" ON)
option(client_build_util "Build validation utilites" OFF)

set(client_preprocess OFF)

# Use C-style preprocessor ifdef
set(client_preprocess_flags -B T -C T -M epri)

# Use compiler preprocessing of all source to handle C-style #ifdef
set(CMAKE_Fortran_PREPROCESS ON)

set(client_arch x64)

# Client options
set(client_options
  AEROSOL
  CHEM
  DENSE
  EPA
  EPRI
  INTEL # Is this even needed?
  LIQUID
  MULT
  OPVIEW
  SAMPAVG
  SCM
  SKEW
  METPLOT
  PROCESS
  REVERSE
  VALIDATION
  WRF
)

# Windows options
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  list(APPEND client_options
    WININI_OPT
  )
endif()

# Linux options
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  list(APPEND client_options
    FINI_OPT
  )
endif()

# List of additional libraries and executables to build for this client
macro(client_add_subdirectories)
  add_subdirectory(src/dll/stub/MPI)
  add_subdirectory(src/bin/met2sci)
  add_subdirectory(src/bin/tersci)
  add_subdirectory(src/bin/SciDOSpost)

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    add_subdirectory(src/bin/SCIPUFFgui)
  endif()

    # The output name needs to be changed during the initial processing run. The
    # processed CMake will have this changed correctly.
    if(client_preprocess)
      set_target_properties(SCIPtool PROPERTIES OUTPUT_NAME SCIPtool)
    endif()
endmacro()
