#[=======================================================================[.rst:
FindGEDIS
-------

Finds the GEDIS libraries.  Assumes HPAC build system NAR directory layout.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``GEDIS::GEDIS_DatabaseUpgradeTool_JE``
``GEDIS::GEDIS_GDM_JE``
``GEDIS::GEDIS_DPM_JE``
``GEDIS::GEDIS_Types_JE``
``GEDIS::GEDIS_ImportExport_JE``
``GEDIS::GEDIS_UDC_JE``
``GEDIS::GEDIS_QAManager_JE``
``GEDIS::GEDIS_SpatialAnalyst_JE``
``GEDIS::GEDIS_SOM_JE``

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``GEDIS_FOUND``
  True if the system found all requested GEDIS components.
``GEDIS_LIBRARIES``
  Libraries needed to link to GEDIS.
``GEDIS_INCLUDE_DIR``
  Include directories for C/C++ headers.
#]=======================================================================]

if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  message(FATAL_ERROR "GEDIS library currently only supported on Windows")
endif()

if(GEDIS_FIND_QUIETLY)
  set(_GEDIS_quiet_arg QUIET)
endif()

set(GEDIS_lib_list
  GEDIS_DatabaseUpgradeTool_JE
  GEDIS_GDM_JE
  GEDIS_DPM_JE
  GEDIS_Types_JE
  GEDIS_ImportExport_JE
  GEDIS_UDC_JE
  GEDIS_QAManager_JE
  GEDIS_SpatialAnalyst_JE
  GEDIS_SOM_JE
)

if(GEDIS_FIND_COMPONENTS)
  set(_GEDIS_FIND ${GEDIS_FIND_COMPONENTS})
else()
  set(_GEDIS_FIND ${GEDIS_lib_list})
endif()

# Find libraries and set imported targets
foreach(GEDIS_lib ${_GEDIS_FIND})
  if(${GEDIS_lib} IN_LIST GEDIS_lib_list)

    set(${GEDIS_${GEDIS_lib}_FOUND} FALSE)

    find_library(
      ${GEDIS_lib}_LIBRARY
      NAMES ${GEDIS_lib}
      PATH_SUFFIXES lib
      ${_GEDIS_quiet_arg}
    )
    list(APPEND GEDIS_LIBRARIES "${${GEDIS_lib}_LIBRARY}")

    if(${GEDIS_lib}_LIBRARY)
      set(GEDIS_${GEDIS_lib}_FOUND TRUE)
      mark_as_advanced(GEDIS_${GEDIS_lib}_FOUND)
    endif()

    # Library found, add target
    if(${GEDIS_${GEDIS_lib}_FOUND})

      if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        # Find DLLs as well on Windows
        find_file(
          ${GEDIS_lib}_DLL
          NAMES ${GEDIS_lib}.dll
          PATH_SUFFIXES bin
          ${_GEDIS_quiet_arg}
        )

        add_library(GEDIS::${GEDIS_lib} SHARED IMPORTED)

        set_target_properties(GEDIS::${GEDIS_lib}
          PROPERTIES
            IMPORTED_IMPLIB "${${GEDIS_lib}_LIBRARY}"
            IMPORTED_LOCATION "${${GEDIS_lib}_DLL}"
        )
      else()
        add_library(GEDIS::${GEDIS_lib} SHARED IMPORTED)

        # The IMPORTED_NO_SONAME property may need to be set depending on implementation
        set_target_properties(GEDIS::${GEDIS_lib}
          PROPERTIES
            IMPORTED_LOCATION "${${GEDIS_lib}_LIBRARY}"
        )
      endif()
      mark_as_advanced(${GEDIS_lib}_DLL)
      
    endif()

    list(APPEND GEDIS_COMPONENTS GEDIS::${GEDIS_lib})

    mark_as_advanced(${GEDIS_lib}_LIBRARY)

  else()
    message(WARNING "Unknown component requested in FindGEDIS: ${GEDIS_lib}")
  endif()
endforeach()

unset(_GEDIS_FIND)

# Add include dir to all imported targets
find_path(GEDIS_INCLUDE_DIR
  NAMES GEDIS_Types.hpp
  PATH_SUFFIXES include
)
foreach(_GEDIS_COMP ${GEDIS_COMPONENTS})
  target_include_directories(${_GEDIS_COMP}
    INTERFACE ${GEDIS_INCLUDE_DIR}
  )
endforeach()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(GEDIS
  FOUND_VAR GEDIS_FOUND
  HANDLE_COMPONENTS
  REQUIRED_VARS
    GEDIS_LIBRARIES
    GEDIS_INCLUDE_DIR
)

mark_as_advanced(
  GEDIS_LIBRARIES
  GEDIS_INCLUDE_DIR
)