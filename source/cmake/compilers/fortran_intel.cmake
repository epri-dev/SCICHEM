# ifort

################################################################################
# Compiler flags
################################################################################

# Linux and MacOS
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(fc_flags
    "SHELL:-names lowercase"
    "SHELL:-fp-model consistent"
    -fimf-use-svml # Use Intel Short Vector Math Library to try to regain some speed lost from fp:consistent
    -fp-speculation=safe

    "SHELL:-assume byterecl,recursion"
    -mixed-str-len-arg
    -traceback
    -diag-disable=7841 # Disable DLLEXPORT warnings on systems with no DLLs
    -Qoption,fpp,-macro=no # Do not allow for macro expansion (causes directives to overwrite variable and module names)
    "SHELL:-extend-source 132"
    "SHELL:-align sequence"
    "SHELL:-reentrancy threaded"
  )
  # Check for uninitialized vars, standards violations
  set(fc_check_flags
    -init=snan,arrays
    -O0
    "SHELL:-check all"
    "SHELL:-stand f18"
  )
  add_link_options(
    -Bsymbolic
  )

# Windows
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(fc_flags
    /fp:consistent
    /Qimf-use-svml # Use Intel Short Vector Math Library to try to regain some speed lost from fp:consistent
    /Qfp-speculation=safe

    /assume:byterecl,recursion
    /iface:mixed_str_len_arg
    /traceback
    /Qoption,fpp,/macro=no # Do not allow for macro expansion (causes directives to overwrite variable and module names)
    /extend-source:132
    /align:sequence
    /reentrancy:threaded

    /iface:cref
    /names:uppercase
  )
  # Check for uninitialized vars, standards violations
  set(fc_check_flags
    /Qinit:snan,arrays
    /Od
    /check:all
    /stand:f18
  )

endif()

# Disable ifort deprecation warnings
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel$")
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    list(APPEND fc_flags -diag-disable=10448)
    list(APPEND flink_flags -diag-disable=10448)
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    list(APPEND fc_flags /Qdiag-disable:10448)
    list(APPEND flink_flags /Qdiag-disable:10448)
  endif()
endif()

option(scipuff_intel_varcheck
  "Use uninitialized variable and F18 standard checks for Intel Fortran compiler"
  OFF
)
add_compile_options(
  "$<$<COMPILE_LANGUAGE:Fortran>:${fc_flags}>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<BOOL:${scipuff_intel_varcheck}>>:${fc_check_flags}>"
)
add_link_options(
  "$<$<LINK_LANG_AND_ID:Fortran,Intel>:${flink_flags}>"
)

# Preprocessor compiler flag
if(client_preprocess)
  list(APPEND client_options __INTEL_COMPILER)
  list(APPEND client_preprocess_args -D__INTEL_COMPILER)
endif()

################################################################################
# Client configuration
################################################################################
client_configure_compiler()
