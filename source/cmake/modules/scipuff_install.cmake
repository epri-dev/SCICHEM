# macro used to consolidate all target install options in one place

macro(scipuff_install)
  install(TARGETS ${ARGV}
    RUNTIME_DEPENDENCY_SET runtime_set
    EXPORT "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    PUBLIC_HEADER DESTINATION "${include_install_dir}"
  )
endmacro()