# messages
cmake_minimum_required(VERSION 3.20)

set(source_list_cmake
  CMakeLists.txt
)

set(source_list_f90
  InitMessage.f90
  Messages.f90
  PostMessage.f90
)

set(source_list_c
  callbackhandler.c
)

set(source_list_h
  callbackhandler.h
)

# Use custom preprocessor to preprocess source if requested
add_to_preprocess(
  PREPROCESS source_list_f90 source_list_cmake
  COPY source_list_c source_list_h
)

if(NOT client_preprocess_only)
  # Compile C separately so that we don't get compile flags mixed up.
  # Create as an object library so that we can combine everything back
  # together in the main library.
  add_library(messages_c OBJECT
    ${source_list_c}
  )

  # Add Linux definition to C code
  target_compile_definitions(messages_c PRIVATE
    "$<$<PLATFORM_ID:Linux>:Linux;_linux>"
  )

  # Add the now compiled C objects back in
  add_library(messages
    ${source_list_f90}
    $<TARGET_OBJECTS:messages_c>
  )

  target_link_libraries(messages
    PRIVATE
      arap
      systool

      "$<$<BOOL:client_use_openmp>:OpenMP::OpenMP_Fortran>"
  )

  # Make sure code has finished preprocessing before use
  set_preprocess_dependency(messages messages_c)

  # Install
  install(
    TARGETS messages
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endif()
