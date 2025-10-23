# Settings that are common to all clients

if(client_validation)
  list(APPEND client_options VALIDATION)
endif()

# Linux
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  list(APPEND client_options
    LINUX
    # STRICTF90  # This appears to be tweaks for the old Lahey compiler
  )
endif()

# OpenMP
if(client_use_openmp)
  # The _OPENMP compiler definition is added automatically
  # by FindOpenMP. Skip adding it here to avoid a warning
  # about a second definition.
  list(APPEND client_options
    OMP
    OMP_SWIM
    OMP_SPLIT
    OMP_PLIST
  )

    list(APPEND client_options
      OMP_CHEM
    )

  # Add OpenMP options to preprocessor argument list
  if(client_preprocess)
    # _OPENMP will need to be added here though.
    list(APPEND client_preprocess_args
      -D_OPENMP
      -OMP T
    )
  endif()
  
endif()


# Use client_options list to generate preprocessor arguments and
# preprocessor defines
foreach(client_opt ${client_options})
  scipuff_add_definition(${client_opt})
endforeach()


# If any of the client hook macros don't exist, create an
# empty one to prevent errors.
set(client_hooks
  client_add_subdirectories
  client_configure_compiler
)

foreach(client_hook ${client_hooks})
  if(NOT COMMAND ${client_hook})
    macro(${client_hook})
    endmacro()
  endif()
endforeach()
