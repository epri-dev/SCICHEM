#[=======================================================================[.rst:
FindUSS
-------

Finds the USS libraries.  Assumes HPAC build system NAR directory layout.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``USS::BVIM_Wrapper``
``USS::DMA_UDM_Tool``
``USS::DMA_UDM_Tool_UDM``
``USS::QUIC_Wrapper``
``USS::DMA_UDM_Tool_USS``
``USS::USS_JNI``
``USS::CONTAM_Wrapper``
``USS::UDM_Wrapper``
``USS::USS_BootStrapper``
``USS::USS``

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``USS_FOUND``
  True if the system found all requested USS components.
``USS_LIBRARIES``
  Libraries needed to link to USS.
#]=======================================================================]

if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  message(FATAL_ERROR "USS library currently only supported on Windows")
endif()

if(USS_FIND_QUIETLY)
  set(_USS_quiet_arg QUIET)
endif()

set(USS_lib_list
  BVIM_Wrapper
  DMA_UDM_Tool
  DMA_UDM_Tool_UDM
  QUIC_Wrapper
  DMA_UDM_Tool_USS
  USS_JNI
  CONTAM_Wrapper
  UDM_Wrapper
  USS_BootStrapper
  USS
)

if(USS_FIND_COMPONENTS)
  set(_USS_FIND ${USS_FIND_COMPONENTS})
else()
  set(_USS_FIND ${USS_lib_list})
endif()

# Find libraries and set imported targets
foreach(USS_lib ${_USS_FIND})
  if(${USS_lib} IN_LIST USS_lib_list)

    set(${USS_${USS_lib}_FOUND} FALSE)

    find_library(
      ${USS_lib}_LIBRARY
      NAMES ${USS_lib}
      PATH_SUFFIXES lib
      ${_USS_quiet_arg}
    )
    list(APPEND USS_LIBRARIES "${${USS_lib}_LIBRARY}")

    if(${USS_lib}_LIBRARY)
      set(USS_${USS_lib}_FOUND TRUE)
      mark_as_advanced(USS_${USS_lib}_FOUND)
    endif()

    # Library found, add target
    if(${USS_${USS_lib}_FOUND})

      if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        # Find DLLs as well on Windows
        find_file(
          ${USS_lib}_DLL
          NAMES ${USS_lib}.dll
          PATH_SUFFIXES bin
          ${_USS_quiet_arg}
        )

        add_library(USS::${USS_lib} SHARED IMPORTED)

        set_target_properties(USS::${USS_lib}
          PROPERTIES
            IMPORTED_IMPLIB "${${USS_lib}_LIBRARY}"
            IMPORTED_LOCATION "${${USS_lib}_DLL}"
        )
      else()
        add_library(USS::${USS_lib} SHARED IMPORTED)

        # The IMPORTED_NO_SONAME property may need to be set depending on implementation
        set_target_properties(USS::${USS_lib}
          PROPERTIES
            IMPORTED_LOCATION "${${USS_lib}_LIBRARY}"
        )
      endif()
      mark_as_advanced(${USS_lib}_DLL)
      
    endif()

    list(APPEND USS_COMPONENTS USS::${USS_lib})

    mark_as_advanced(${USS_lib}_LIBRARY)

  else()
    message(WARNING "Unknown component requested in FindUSS: ${USS_lib}")
  endif()
endforeach()

unset(_USS_FIND)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(USS
  FOUND_VAR USS_FOUND
  HANDLE_COMPONENTS
  REQUIRED_VARS
    USS_LIBRARIES
)

mark_as_advanced(
  USS_LIBRARIES
)