#[=======================================================================[.rst:
Finddelfic
-------

Finds the delfic library.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``delfic::sadeadapter``
  The delfic library

On Linux, these imported targets will be added as well:
``delfic::daemmodel``
``delfic::waspjson``
``delfic::waspcore``
``delfic::radixio`` 
``delfic::radixmath``
``delfic::radixalgorithmlib``
``delfic::radixcore``

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``delfic_FOUND``
  True if the system has the delfic library.
``delfic_LIBRARIES``
  Libraries needed to link to delfic.
#]=======================================================================]

if(delfic_FIND_QUIETLY)
  set(_delfic_quiet_arg QUIET)
endif()

find_library(delfic_sadeadapter_LIBRARY
  NAMES sadeadapter
  HINTS ${CMAKE_SOURCE_DIR}/lib/vendor/x64
  PATH_SUFFIXES delfic
  ${_delfic_quiet_arg}
)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  find_file(delfic_sadeadapter_DLL
    NAMES sadeadapter.dll
    HINTS ${CMAKE_SOURCE_DIR}/bin/vendor/x64
    ${_delfic_quiet_arg}
  )
endif()

# Main delfic library found, gather all the dependencies
if(delfic_sadeadapter_LIBRARY)
  set(delfic_LIBRARIES ${delfic_sadeadapter_LIBRARY})

  if(NOT TARGET delfic::sadeadapter)
    set(delfic_lib_type SHARED)

    add_library(delfic::sadeadapter ${delfic_lib_type} IMPORTED)

    if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
      set_target_properties(delfic::sadeadapter PROPERTIES
        IMPORTED_LOCATION "${delfic_sadeadapter_DLL}"
        IMPORTED_IMPLIB "${delfic_sadeadapter_LIBRARY}"
      )

    elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
      # Use IMPORTED_NO_SONAME to prevent linker from using full file path
      set_target_properties(delfic::sadeadapter PROPERTIES
        IMPORTED_LOCATION "${delfic_sadeadapter_LIBRARY}"
        IMPORTED_NO_SONAME TRUE
      )

    endif()
  endif()

  # Linux builds require all of the dependencies to be added as well
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    # Dependencies
    set(delfic_add_lib_names
      daemmodel
      waspjson
      waspcore
      radixio
      radixmath
      radixalgorithmlib
      radixcore
    )
    
    foreach(dlib ${delfic_add_lib_names})
      if(NOT TARGET delfic::${dlib})
        find_library(delfic_${dlib}_LIBRARY
          NAMES ${dlib}
          PATH_SUFFIXES delfic/lib vendor/x64
          ${_delfic_quiet_arg}
        )
        list(APPEND delfic_LIBRARIES ${delfic_${dlib}_LIBRARY})

        add_library(delfic::${dlib} UNKNOWN IMPORTED)

        # Use IMPORTED_NO_SONAME to prevent linker from using full file path
        set_target_properties(delfic::${dlib} PROPERTIES
          IMPORTED_LOCATION "${delfic_${dlib}_LIBRARY}"
          IMPORTED_NO_SONAME TRUE
        )
      endif()

      set_property(TARGET delfic::sadeadapter
        APPEND PROPERTY
          INTERFACE_LINK_LIBRARIES delfic::${dlib}
      )
    endforeach()
  endif()
endif()


include(FindPackageHandleStandardArgs)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  find_package_handle_standard_args(delfic
    FOUND_VAR delfic_FOUND
    REQUIRED_VARS
      delfic_sadeadapter_LIBRARY
  )

  mark_as_advanced(
    delfic_sadeadapter_LIBRARY
  )

elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  find_package_handle_standard_args(delfic
    FOUND_VAR delfic_FOUND
    REQUIRED_VARS
      delfic_sadeadapter_LIBRARY
      delfic_daemmodel_LIBRARY
      delfic_waspjson_LIBRARY
      delfic_waspcore_LIBRARY
      delfic_radixio_LIBRARY
      delfic_radixmath_LIBRARY
      delfic_radixalgorithmlib_LIBRARY
      delfic_radixcore_LIBRARY
  )

  mark_as_advanced(
    delfic_sadeadapter_LIBRARY
    delfic_daemmodel_LIBRARY
    delfic_waspjson_LIBRARY
    delfic_waspcore_LIBRARY
    delfic_radixio_LIBRARY
    delfic_radixmath_LIBRARY
    delfic_radixalgorithmlib_LIBRARY
    delfic_radixcore_LIBRARY
  )

else()
  message(FATAL_ERROR "Unsupported system detected when importing DELFIC libraries")

endif()