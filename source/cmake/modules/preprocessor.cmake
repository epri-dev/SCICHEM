# Use custom preprocessor

# Initialize variables and target for preprocessing.
# We need a target to drive preprocessing so that any source files
# that appear in more than one target will not generate duplicate
# preprocessed files (ninja build system throws an error on this).

macro(init_preprocess)
  if(client_preprocess)

    # Create master lists of all source files that need to be
    # preprocessed or copied. Use global properties to avoid issues with scope.
    define_property(GLOBAL PROPERTY _GLOBAL_PREP_LIST)
    define_property(GLOBAL PROPERTY _GLOBAL_COPY_LIST)

    # Place the preprocessed source in the build directory. Get
    # both full path and path relative to CMAKE_SOURCE_DIR.
    cmake_path(SET _prep_dir "${CMAKE_BINARY_DIR}/preprocessed")

    cmake_path(RELATIVE_PATH
      _prep_dir
      BASE_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE _prep_rel_dir
    )
  endif()
endmacro()


# Use master lists to set custom commands to run preprocessing step
macro(set_preprocess_commands)

  # First, make sure that there are no duplicates in each list
  get_property(_prep_list GLOBAL PROPERTY _GLOBAL_PREP_LIST)
  get_property(_copy_list GLOBAL PROPERTY _GLOBAL_COPY_LIST)
  list(REMOVE_DUPLICATES _prep_list)
  list(REMOVE_DUPLICATES _copy_list)

  # Preprocess
  foreach(_source_out ${_prep_list})

    # Get full path to original source file
    string(REPLACE ${_prep_rel_dir} "" _source_in ${_source_out})
    cmake_path(NORMAL_PATH _source_in)
    cmake_path(RELATIVE_PATH _source_in
      BASE_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE _source_in_rel
    )

    # Reapply client rules from add_to_preprocess
    if("${client_name}" STREQUAL "EPRI")
      string(REGEX REPLACE "[Hh][Pp][Aa][Cc]" "SCIP" _source_out "${_source_out}")
    endif()

    # Get the directory for the output file
    cmake_path(GET _source_out PARENT_PATH _source_out_dir)

    list(APPEND _deps_list "${_source_out}")

    # Set output extension based on file name
    if("${_source_out}" MATCHES "CMakeLists\.txt")
      set(_source_out_ext -E cmake)
    else()
      cmake_path(GET _source_out EXTENSION _file_extp)
      string(REGEX REPLACE "^\\." "" _file_ext ${_file_extp})
      set(_source_out_ext -E ${_file_ext})
    endif()

    # Run preprocessor on source files and save to output directory. Add
    # dependency on preprocessor target to ensure that the preprocessor is built before
    # any preprocessing begins.
    add_custom_command(
      OUTPUT "${_source_out}"
      COMMAND "${CMAKE_COMMAND}" -E make_directory "${_source_out_dir}"
      COMMAND $<TARGET_FILE:sciprep> ${client_preprocess_args} ${client_preprocess_flags} ${_source_out_ext} -P "${_source_in}" -O "${_source_out}"
      COMMENT "Preprocessing ${_source_in_rel}"
      DEPENDS sciprep "${_source_in}"
      VERBATIM
    )
  endforeach()

  # Copy
  foreach(_source_out ${_copy_list})

    # Get full path to original source file
    string(REPLACE ${_prep_rel_dir} "" _source_in "${_source_out}")
    cmake_path(NORMAL_PATH _source_in)
    cmake_path(RELATIVE_PATH _source_in
      BASE_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE _source_in_rel
    )

    # Reapply client rules from add_to_preprocess
    if("${client_name}" STREQUAL "EPRI")
      string(REGEX REPLACE "[Hh][Pp][Aa][Cc]" "SCIP" _source_out "${_source_out}")
    endif()

    list(APPEND _deps_list "${_source_out}")

    # Need a different command for directories vs files
    if(IS_DIRECTORY "${_source_in}")
      set(copy_command copy_directory)

    else()
      set(copy_command copy)
    endif()

    # Copy file to output directory without preprocessing
    add_custom_command(
      OUTPUT "${_source_out}"
      COMMAND ${CMAKE_COMMAND} -E ${copy_command} "${_source_in}" "${_source_out}"
      COMMENT "Copying ${_source_in_rel} to preprocessed directory"
      DEPENDS "${_source_in}"
      VERBATIM
    )
  endforeach()

  # Create the driver target
  add_custom_target(preprocess ALL
    DEPENDS ${_deps_list}
  )

  # Allow clean target to remove directories that may still have files in them
  foreach(_dep ${_deps_list})
    if(IS_DIRECTORY "${_dep}")
      set_property(TARGET preprocess
        APPEND PROPERTY
          ADDITIONAL_CLEAN_FILES "${_dep}"
      )
    endif()
  endforeach()
endmacro()


# use: add_to_preprocess(
#        [PREPROCESS <prep_list_name> [prep_list_name_2 ...]]
#        [COPY <copy_list_name> [copy_list_name_2 ...]]
#      )
#
# Sets up preprocessing command and replaces source file list with
# list of preprocessed files. Additional lists can also be passed to
# be copied to the location of the preprocessed files without
# preprocessing.
macro(add_to_preprocess)
  # Skip if not using preprocessor
  if(client_preprocess)

    # Process arguments
    set(multi_value_args PREPROCESS COPY)
    cmake_parse_arguments(
      _PREP
      ""
      ""
      "${multi_value_args}"
      ${ARGN}
    )

    # Ensure source lists are only in one of PREPROCESS or COPY
    foreach(_prep_source_list ${_PREP_PREPROCESS})
      if("${_prep_source_list}" IN_LIST _PREP_COPY)
        color_message(FATAL_ERROR red "${CMAKE_CURRENT_LIST_FILE}: Source file list ${_prep_source_list} cannot be in both PREPROCESS and COPY")
      endif()
    endforeach()

    # Get relative directory of calling subdirectory
    cmake_path(RELATIVE_PATH
      CMAKE_CURRENT_SOURCE_DIR
      BASE_DIRECTORY "${CMAKE_SOURCE_DIR}"
      OUTPUT_VARIABLE _subdir_relative_dir
    )

    # Preprocess each list of files
    foreach(_prep_source_list ${_PREP_PREPROCESS} ${_PREP_COPY})

      # Clear out any previous source list preprocessing
      unset(_source_out_list)

      # Loop over individual files
      foreach(source ${${_prep_source_list}})
        # Get the full file path for the output file
        cmake_path(SET _source_out
          "${_prep_dir}/${_subdir_relative_dir}/${source}"
          NORMALIZE
        )

        # Create list of preprocessed files to return to calling subdirectory
        list(APPEND _source_out_list ${_source_out})
      endforeach()

      # Update master preprocessing lists
      get_directory_property(_has_parent PARENT_DIRECTORY) # Check to see if we need to pass lists to parent scope
      if("${_prep_source_list}" IN_LIST _PREP_PREPROCESS)
        set_property(GLOBAL APPEND PROPERTY _GLOBAL_PREP_LIST ${_source_out_list})
      elseif("${_prep_source_list}" IN_LIST _PREP_COPY)
        set_property(GLOBAL APPEND PROPERTY _GLOBAL_COPY_LIST ${_source_out_list})
      else()
        color_message(FATAL_ERROR red "preprocessor: file list, ${_prep_source_list}, not found in either PREPROCESS or COPY")
      endif()

      # Update file and path names with client-specific rules. Do this after updating global list
      # so that custom commands can find the unprocessed files correctly.
      if("${client_name}" STREQUAL "EPRI")
        string(REGEX REPLACE "[Hh][Pp][Aa][Cc]" "SCIP" _source_out_list "${_source_out_list}")
      endif()

      # Update source file list from calling subdirectory
      set(${_prep_source_list} ${_source_out_list})

    endforeach()
  endif(client_preprocess)
endmacro()


# Add dependency on _preprocess to targets if they exist
macro(set_preprocess_dependency)
  if(client_preprocess)
    # Loop over each target argument
    foreach(tgt ${ARGN})
      if(TARGET ${tgt})
        add_dependencies(${tgt} preprocess)
      endif()
    endforeach()
  endif()
endmacro()
