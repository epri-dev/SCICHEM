# General C
# set(cc_general_flags
#  -c
#  -O2
# )
# add_compile_options("$<$<COMPILE_LANGUAGE:C>:${cc_general_flags}>")

# GNU C (cc)
if("${CMAKE_C_COMPILER_ID}" MATCHES "GNU")
  set(cc_specific_flags
    -DNOFUNCDEF
    -DUTIME_H
    #  -DX64BIT
  )

  # Output more warnings for bad code
  add_compile_options(
    "$<$<COMPILE_LANGUAGE:C>:-Wall;-Wextra;-Wpedantic>"
  )
endif()

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
  # Output more warnings for bad code
  add_compile_options(
    "$<$<COMPILE_LANGUAGE:CXX>:-Wall;-Wextra;-Wpedantic>"
  )
endif()

add_compile_options(
  "$<$<COMPILE_LANGUAGE:C>:${cc_specific_flags}>"
  "$<$<COMPILE_LANGUAGE:CXX>:${cxx_specific_flags}>"
)