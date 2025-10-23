#[=======================================================================[.rst:
FindDREAM
-------

Finds the DREAM libraries. Assumes HPAC build system NAR directory layout.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``DREAM::DREAM``

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``DREAM_FOUND``
  True if the system found all requested DREAM components.
``DREAM_LIBRARIES``
  Libraries needed to link to DREAM.
#]=======================================================================]

if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  message(FATAL_ERROR "DREAM library currently only supported on Windows")
endif()

if(DREAM_FIND_QUIETLY)
  set(_DREAM_quiet_arg QUIET)
endif()

set(DREAM_lib_list
  DREAM
)

if(DREAM_FIND_COMPONENTS)
  set(_DREAM_FIND ${DREAM_FIND_COMPONENTS})
else()
  set(_DREAM_FIND ${DREAM_lib_list})
endif()

# Find libraries and set imported targets
foreach(DREAM_lib ${_DREAM_FIND})
  if(${DREAM_lib} IN_LIST DREAM_lib_list)

    set(${DREAM_${DREAM_lib}_FOUND} FALSE)

    find_library(
      ${DREAM_lib}_LIBRARY
      NAMES ${DREAM_lib}
      PATH_SUFFIXES lib
      ${_DREAM_quiet_arg}
    )
    list(APPEND DREAM_LIBRARIES "${${DREAM_lib}_LIBRARY}")

    if(${DREAM_lib}_LIBRARY)
      set(DREAM_${DREAM_lib}_FOUND TRUE)
      mark_as_advanced(DREAM_${DREAM_lib}_FOUND)
    endif()

    # Library found, add target
    if(${DREAM_${DREAM_lib}_FOUND})

      if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        # Find DLLs as well on Windows
        find_file(
          ${DREAM_lib}_DLL
          NAMES ${DREAM_lib}.dll
          PATH_SUFFIXES bin
          ${_DREAM_quiet_arg}
        )

        add_library(DREAM::${DREAM_lib} SHARED IMPORTED)

        set_target_properties(DREAM::${DREAM_lib}
          PROPERTIES
            IMPORTED_IMPLIB "${${DREAM_lib}_LIBRARY}"
            IMPORTED_LOCATION "${${DREAM_lib}_DLL}"
        )
      else()
        add_library(DREAM::${DREAM_lib} SHARED IMPORTED)

        # The IMPORTED_NO_SONAME property may need to be set depending on implementation
        set_target_properties(DREAM::${DREAM_lib}
          PROPERTIES
            IMPORTED_LOCATION "${${DREAM_lib}_LIBRARY}"
        )
      endif()
      mark_as_advanced(${DREAM_lib}_DLL)
      
    endif()

    list(APPEND DREAM_COMPONENTS DREAM::${DREAM_lib})

    mark_as_advanced(${DREAM_lib}_LIBRARY)

  else()
    message(WARNING "Unknown component requested in FindDREAM: ${DREAM_lib}")
  endif()
endforeach()

unset(_DREAM_FIND)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(DREAM
  FOUND_VAR DREAM_FOUND
  HANDLE_COMPONENTS
  REQUIRED_VARS
    DREAM_LIBRARIES
)

mark_as_advanced(
  DREAM_LIBRARIES
)