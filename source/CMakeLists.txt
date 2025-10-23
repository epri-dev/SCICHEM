cmake_minimum_required(VERSION 3.20)
################################################################################
# SCIPUFF build system for CMake
#
# Static libraries are linked privately so that the install step doesn't have an
# error when trying to exclude them.
#
################################################################################

################################################################################
# Configuration and module includes
################################################################################

# Enable MSVC runtime selection (must come before project call)
cmake_policy( SET CMP0091 NEW )
if(scipuff_static_msvc_runtime)
  # Use static MSVC runtime
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>") # /MT, /MTd
else()
  # Use shared MSVC runtime
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL") # /MD, /MDd
endif()

# Project name and info
# Most libraries and executables are pure Fortran; exceptions are
# C: arap, messages
# CXX: fxmgr
project(scipuff
  LANGUAGES Fortran C CXX
)

include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

# Location of all CMake files
set(scipuff_cmake_file_dir "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Location of CMake modules
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${scipuff_cmake_file_dir}/modules")

# Implement colored messages for readability
include(colorfuncs)

# Preprocess Fortran files using custom preprocessor if requested
include(preprocessor)

# Add preprocessor definitions to all appropriate places
include(scipuff_add_definition)

# Client configuration file and common client settings
# Common include must come after the client options!
option(client_preprocess "Use the custom preprocessor before building" OFF)

option(client_preprocess_only "Preprocess the source using the custom preprocessor without building" OFF)
if(client_preprocess_only)
  set(client_preprocess ON)
endif()

option(client_validation "Build client with validation features" OFF)

option(client_use_openmp "Build SCIPUFF with OpenMP support" ON)

option(client_use_cuda "Build SCIPUFF with NVIDIA CUDA support" OFF)

include(${scipuff_cmake_file_dir}/clients/${build_client}.cmake)
include(${scipuff_cmake_file_dir}/clients/common.cmake)

# Initialize custom preprocessor preprocessing if requested
init_preprocess()

# Compiler options
include(${scipuff_cmake_file_dir}/compilers/compiler_setup.cmake)

# Output directories
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")

# Install step setup
include(GNUInstallDirs)

option(scipuff_install_static_libs "Install SCIPUFF static (and stub) libraries" OFF)
mark_as_advanced(scipuff_install_static_libs)

set(include_install_dir "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}")
set(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")

################################################################################
# Display request client and build type
################################################################################

color_message(STATUS yellow "Using settings for ${client_name} client")

get_property(_IsMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(_IsMultiConfig)
  color_message(STATUS yellow "Build Type: Multi-Config")
  color_message(STATUS yellow " -- Configuration types: ${CMAKE_CONFIGURATION_TYPES}")
else()
  color_message(STATUS yellow "Build Type: ${CMAKE_BUILD_TYPE}")
endif()

if(client_preprocess AND client_preprocess_only)
  color_message(STATUS yellow " -- Preprocessing only; compile disabled")
endif()

color_message(STATUS yellow "Platform: ${CMAKE_SYSTEM_NAME}")
color_message(STATUS yellow "Architecture: ${client_arch}")
color_message(STATUS yellow "OpenMP: ${client_use_openmp}")
color_message(STATUS yellow "CUDA: ${client_use_cuda}")

################################################################################
# Find external libraries
################################################################################

if(NOT client_preprocess_only)
  # These are essential external libraries with no stubs. Any external libraries
  # that have an optional stub form should be added in the cmake/client/ files.
  # Make sure to load all external libraries prior to adding subdirectories so
  # that any optional libraries are correctly found.

  # OpenMP
  # Note that OpenMP is linked PRIVATEly (to prevent non-Fortran consuming
  # projects needing a compile-time link to OpenMP Fortran components), so
  # anything linking against static libraries that use OpenMP (i.e. SAG,
  # SCIPUFFlib, etc.) must also have an OpenMP link as well.
  if(client_use_openmp)
    find_package(OpenMP COMPONENTS Fortran REQUIRED)
  endif()

    find_package(HDF5 REQUIRED)
    find_package(netCDF REQUIRED)


  if(client_use_cuda)
    set(CMAKE_CUDA_STANDARD 17) # C++17
    set(CMAKE_CUDA_ARCHITECTURES 52) # CUDA Compute Capability 5.2
    set(CMAKE_CUDA_SEPARABLE_COMPILATION ON)
    set(CMAKE_CUDA_RESOLVE_DEVICE_SYMBOLS ON)
    set(CMAKE_CUDA_RUNTIME_LIBRARY Static)

    set(CMAKE_CUDA_FLAGS "--allow-unsupported-compiler")
    enable_language(CUDA)

    find_package(CUDAToolkit REQUIRED) # CUDA::cudart, Thrust includes
  endif()
endif()

################################################################################
# Libraries and Executables
################################################################################

# Build custom preprocessor if requested
if(client_preprocess)
  add_subdirectory(util/sciprep)

  # If we're using the preprocessor, add all top-level CMake files to the
  # preprocessed directory as well.
  set(source_list_cmake
    CMakeLists.txt
    cmake/clients/common.cmake
    cmake/clients/${build_client}.cmake
  )
  set(source_list_cmake_copy
    CMakePresets.base.json
    CMakePresets.json

    cmake/compilers
    cmake/modules
    cmake/Config.cmake.in
  )
  add_to_preprocess(
    PREPROCESS source_list_cmake
    COPY source_list_cmake_copy
  )
  add_custom_target(cmake_files ALL)
  set_preprocess_dependency(cmake_files)
endif()

# Wrapper for glibc memcpy used to fix issues with old versions on Linux
# Uses memcpy from glibc 2.2.5
if(scipuff_use_memcpy_wrapper)
  add_subdirectory(util/MEMCPY)
  link_libraries(-Wl,--wrap=memcpy $<TARGET_OBJECTS:memcpy-wrapper>)
endif()

# Create list of subdirectories to add. Creating the list before adding
# allows for dynamically checking which libraries are available at
# the linking stage.

# Libraries common to all clients
add_subdirectory(src/lib/util/ARAPlib)
add_subdirectory(src/lib/util/SAGlib)
add_subdirectory(src/lib/SCIPUFFlib)
add_subdirectory(src/sys)
add_subdirectory(src/dll/util/Messages)
add_subdirectory(src/dll/SCIPUFF/LandUse)
add_subdirectory(src/dll/SCIPUFF/SWIM)
add_subdirectory(src/dll/SCIP/SCIPtool)
add_subdirectory(src/dll/util/ClippedNormal)

# Clients may choose to load datums from an external C-library. Fall back to
# the internal Fortran fdatums library if the C version was not chosen.
if(NOT TARGET datums::datums)
  color_message(STATUS cyan "datums external library not found, using internal fdatums")
  add_subdirectory(src/lib/util/DATUMSlib)
endif()

# Library only linked by executables below
if(client_build_exec OR client_build_util)
  add_subdirectory(src/bin/PostProcess/xpp)
endif()

# Standalone SCIPUFF executables common to all clients
if(client_build_exec)
  add_subdirectory(src/bin/runSCI)
  add_subdirectory(src/bin/PostProcess)
endif()

# Utility executables
if(client_build_util)
  add_subdirectory(src/bin/util_mod)
  add_subdirectory(src/bin/crosspuf)
  add_subdirectory(src/bin/crossv)
  add_subdirectory(src/bin/readsrf)
  add_subdirectory(src/bin/samppuf)
  add_subdirectory(src/bin/slice_time)
  add_subdirectory(src/bin/slicev)
  add_subdirectory(src/bin/slicev_int)
endif()


# Conditionally added libraries and executables taken from the client configuration.
# ONLY ADD HERE SUBDIRS THAT DEPEND ON OPTIONS, NOT CLIENTS

  add_subdirectory(src/lib/util/odepack)





  if(client_build_exec)
    add_subdirectory(src/bin/SCIPUFFdriver)
  endif()






# Any additional client specific libraries not added using client_options
client_add_subdirectories()

################################################################################
# Display final build settings
################################################################################

# Sort for readability
list(SORT client_options CASE INSENSITIVE)
color_message(STATUS yellow "Client options: ${client_options}")

if(client_preprocess)
  color_message(STATUS yellow
    "Fortran code will be preprocessed using custom preprocessor")

  list(SORT client_preprocess_args CASE INSENSITIVE)
  color_message(STATUS yellow "preprocessor flags: ${client_preprocess_args};${client_preprocess_flags}")
endif()

################################################################################
# Set requested build configuration
################################################################################

# Everything has been added, now we can setup preprocessing commands.
# This must come after all subdirs have been added!
set_preprocess_commands()

################################################################################
# Installation
################################################################################

# Creates CMake package files so that other projects can use the
# find_package() function to easily import built libraries. Uses
# the GNU install directory format, but this is cross-platform
# compatible. Libraries and executables are installed in subdirectories.

include(CMakePackageConfigHelpers)
set(config_install_dir "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")
set(project_config "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake")
set(namespace "${PROJECT_NAME}::")

if(NOT client_preprocess_only)
  configure_package_config_file(
    "cmake/Config.cmake.in"
    "${project_config}"
    INSTALL_DESTINATION "${config_install_dir}"
    PATH_VARS include_install_dir
  )

  # Config
  install(
    FILES "${project_config}"
    DESTINATION "${config_install_dir}"
  )

  install(
    EXPORT "${TARGETS_EXPORT_NAME}"
    NAMESPACE "${namespace}"
    DESTINATION "${config_install_dir}"
  )
endif()
