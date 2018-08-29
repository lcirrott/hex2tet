CMAKE_MINIMUM_REQUIRED(VERSION 2.8.0)

PROJECT (hex2tet)

###############################################################################
#####
#####         Release version and date
#####
###############################################################################
SET (CMAKE_RELEASE_VERSION_MAJOR "1")
SET (CMAKE_RELEASE_VERSION_MINOR "0")
SET (CMAKE_RELEASE_VERSION_PATCH "0")
SET (CMAKE_RELEASE_DATE "July 2, 2016")

SET (CMAKE_RELEASE_VERSION
  "${CMAKE_RELEASE_VERSION_MAJOR}.${CMAKE_RELEASE_VERSION_MINOR}.${CMAKE_RELEASE_VERSION_PATCH}")

# Create hex2tet.h file with the good release infos.
FILE ( MAKE_DIRECTORY  ${PROJECT_BINARY_DIR}/src )
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/src/hex2tet.h.in
  ${PROJECT_BINARY_DIR}/src/hex2tet.h @ONLY)
INCLUDE_DIRECTORIES ( ${PROJECT_BINARY_DIR}/src )

###############################################################################
#####
#####         Main CMake compilation variables
#####
###############################################################################

# To see flags and options of compilation
#SET(CMAKE_VERBOSE_MAKEFILE TRUE)

# Executable path
SET(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)
SET(H2T_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src)

# Find "exotic" compilers
IF (${CMAKE_C_COMPILER} MATCHES Clang OR ${CMAKE_C_COMPILER} MATCHES clang)
  # using clang
  SET(CMAKE_COMPILER_IS_CLANG TRUE)
ELSEIF(${CMAKE_C_COMPILER} MATCHES Icc OR ${CMAKE_C_COMPILER} MATCHES icc)
  # using icc
  SET(CMAKE_COMPILER_IS_INTEL TRUE)
ENDIF()

# Hide some options
MARK_AS_ADVANCED(CMAKE_OSX_ARCHITECTURES
  CMAKE_OSX_DEPLOYMENT_TARGET
  CMAKE_OSX_SYSROOT)

IF(CMAKE_COMPILER_IS_GNUCC)
  SET(CMAKE_C_FLAGS " -Wno-char-subscripts ${CMAKE_C_FLAGS}")
  IF(APPLE)
    # Add flags to the compiler to work on old mac
    ADD_DEFINITIONS( -mmacosx-version-min=10.4 -arch x86_64)

    # To avoid pbs with binary files...
    SET(CMAKE_EXE_LINKER_FLAGS "-arch x86_64 ${CMAKE_EXE_LINKER_FLAGS}")

    # Determine if the processor supports 64bit execution
    EXECUTE_PROCESS(
      COMMAND sysctl hw.cpu64bit_capable
      ERROR_QUIET
      OUTPUT_VARIABLE 64_CMD
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    STRING(REGEX REPLACE "^hw.cpu64bit_capable: (.*)" "\\1" 64_BIT "${64_CMD}")
    #  ELSEIF(UNIX)# UNIX must be after APPLE becaus APPLE is UNIX too
  ENDIF()
ENDIF()

IF(NOT CMAKE_COMPILER_IS_CLANG)
  # Compiler options for profiling... but not possible with clang
  OPTION ( PROFILING "Enable/Disable PROFILING" OFF )
  IF(PROFILING)
    ADD_DEFINITIONS(-pg)
    SET(CMAKE_EXE_LINKER_FLAGS "-pg ${CMAKE_EXE_LINKER_FLAGS}")
  ENDIF(PROFILING)
ENDIF(NOT CMAKE_COMPILER_IS_CLANG)

###############################################################################
#####
#####         Choose executable target to compile
#####
###############################################################################
IF(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  MESSAGE(STATUS "Setting build type to 'Release' as none was specified.")
  seT(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
  SET(CMAKE_CONFIGURATION_TYPES ${CMAKE_BUILD_TYPE} )
ENDIF()

############################################################################
#####
#####        MMG (for mesh data structure)
#####
############################################################################
LIST(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

SET(MMG_DIR "" CACHE PATH "Installation directory for mmg")

FIND_PACKAGE(Mmg)

IF ( NOT MMG_FOUND )
  MESSAGE ( FATAL_ERROR "ERROR: The installation directory for mmg is required:"
    "(see https://github.com/MmgTools/mmg and  download the branch develop)."
    "If you have already installed Mmg and want to use it, "
    "please set the CMake variable or environment variable MMG_DIR "
    "to your mmg directory.")     # Set flags for building test program
ELSE ( )
  INCLUDE_DIRECTORIES(${MMG_INCLUDE_DIRS})

  MESSAGE(STATUS
    "Compilation with Mmg: ${MMG_LIBRARIES}")
  SET( LIBRARIES ${MMG_LIBRARIES} ${LIBRARIES})

ENDIF ( )

 ############################################################################
  #####
  #####         Scotch
  #####
  ############################################################################
  # add SCOTCH library?
  SET(SCOTCH_DIR "" CACHE PATH "Installation directory for scotch")
  INCLUDE(cmake/modules/FindScotch.cmake)

  IF(SCOTCH_FOUND)
    OPTION ( USE_SCOTCH "Use SCOTCH TOOL for renumbering" ON)
    IF( USE_SCOTCH )
      # Set flags for building test program
      INCLUDE_DIRECTORIES(${SCOTCH_INCLUDE_DIR})

      SET(CMAKE_REQUIRED_INCLUDES ${SCOTCH_INCLUDE_DIR})
      SET(CMAKE_REQUIRED_LIBRARIES ${SCOTCH_LIBRARY})
      SET(CMAKE_REQUIRED_LIBRARIES ${SCOTCHERR_LIBRARY})

      SET(CMAKE_C_FLAGS "-DUSE_SCOTCH ${CMAKE_C_FLAGS}")
      SET(SCOTCH_LINK_FLAGS "-lpthread")
      MESSAGE(STATUS
        "Compilation with scotch: ${SCOTCH_LIBRARY}")
      SET( LIBRARIES ${SCOTCH_LINK_FLAGS} ${LIBRARIES})
      SET( LIBRARIES ${SCOTCHERR_LIBRARY} ${LIBRARIES})
      SET( LIBRARIES ${SCOTCH_LIBRARY} ${LIBRARIES})
    ENDIF()
  ELSE ( )
    MESSAGE ( WARNING "Scotch library not found:"
      "if you compile mmg platform without scotch, it is not a problem otherwise"
      " see https://gforge.inria.fr/frs/?group_id=248 to download it."
      "If you have already installed Scotch and want to use it, "
      "please set the CMake variable or environment variable SCOTCH_DIR "
      "to your scotch directory.")
  ENDIF ( )

  ############################################################################
  #####
  #####         Elastic
  #####
  ############################################################################
  # add SUscElas library?
  SET(SUSCELAS_DIR "" CACHE PATH "Installation directory for SUscElas")
  INCLUDE(cmake/modules/FindSUscElas.cmake)

  IF(SUSCELAS_FOUND)
      # Set flags for building test program
      INCLUDE_DIRECTORIES(${SUSCELAS_INCLUDE_DIR})

      SET(CMAKE_REQUIRED_INCLUDES ${SUSCELAS_INCLUDE_DIR})
      SET(CMAKE_REQUIRED_LIBRARIES ${SUSCELAS_LIBRARY})

      SET(CMAKE_C_FLAGS "-DUSE_SUSCELAS ${CMAKE_C_FLAGS}")
      MESSAGE(STATUS
        "Compilation with the Elas library: ${SUSCELAS_LIBRARY} ")
      SET( LIBRARIES ${SUSCELAS_LINK_FLAGS} ${LIBRARIES})
      SET( LIBRARIES ${SUSCELAS_LIBRARY} ${LIBRARIES})
  ELSE ( )
    MESSAGE ( WARNING "SUscElas is a library to solve the linear elasticity "
      "if you compile mmg platform without SUscElas, it is not a problem otherwise"
      " see https://github.com/SUscTools/SUscElas to download it. "
      "This library is needed to use the lagrangian motion option. "
      "If you have already installed SUscElas and want to use it, "
      "please set the CMake variable or environment variable SUSCELAS_DIR "
      "to your SUscElas directory.")
  ENDIF ( )


###############################################################################
#####
#####         Sources and libraries
#####
###############################################################################
# Header files
INCLUDE_DIRECTORIES(${H2T_SOURCE_DIR})
# Source files
FILE(
  GLOB
  source_files
  ${H2T_SOURCE_DIR}/*.c   ${H2T_SOURCE_DIR}/*.h
  )
LIST(REMOVE_ITEM source_files
  ${H2T_SOURCE_DIR}/hex2tet.c
  ${REMOVE_FILE})
FILE(
  GLOB
  main_file
  ${H2T_SOURCE_DIR}/hex2tet.c
  ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}.c
  ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}f.c
   )
FILE(
  GLOB
  lib_file
  ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}.c
  ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}f.c
  )
IF (NOT WIN32)
  FIND_LIBRARY(M_LIB m)
  SET( LIBRARIES ${M_LIB} ${LIBRARIES})
ENDIF()

 ############################################################################
  #####
  #####Set the full RPATH to find libraries independently from LD_LIBRARY_PATH
  #####
  ############################################################################
  SET(CMAKE_MACOSX_RPATH TRUE)

  # use, i.e. don't skip the full RPATH for the build tree
  SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

  # when building, don't use the install RPATH already
  # (but later on when installing)
  SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

  SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

  # add the automatically determined parts of the RPATH
  # which point to directories outside the build tree to the install RPATH
  SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

  # the RPATH to be used when installing, but only if it's not a system
  # directory
  LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES
    "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
  IF("${isSystemDir}" STREQUAL "-1")
    SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  ENDIF("${isSystemDir}" STREQUAL "-1")

  ############################################################################
  #####
  #####         Compile hex2tet libraries
  #####
  ############################################################################
  SET(LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/lib)

  OPTION ( LIBH2T_STATIC "Compile static library" ON)
  IF ( LIBH2T_STATIC )
    ADD_LIBRARY(${PROJECT_NAME}_a  STATIC
      ${source_files} ${lib_file} )
    SET_TARGET_PROPERTIES(${PROJECT_NAME}_a PROPERTIES OUTPUT_NAME
      ${PROJECT_NAME})
    TARGET_LINK_LIBRARIES(${PROJECT_NAME}_a ${LIBRARIES})
    INSTALL(FILES ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}.h DESTINATION
      include)
    INSTALL(TARGETS ${PROJECT_NAME}_a
      ARCHIVE DESTINATION lib
      LIBRARY DESTINATION lib)
  ENDIF()

  # Compile shared library
  OPTION ( LIBH2T_SHARED "Compile dynamic library" OFF)
  IF ( LIBH2T_SHARED )
    ADD_LIBRARY(${PROJECT_NAME}_so SHARED
      ${source_files} ${lib_file})
    SET_TARGET_PROPERTIES(${PROJECT_NAME}_so PROPERTIES
      OUTPUT_NAME ${PROJECT_NAME})
    TARGET_LINK_LIBRARIES(${PROJECT_NAME}_so ${LIBRARIES})
    INSTALL(FILES ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}.h
      DESTINATION include)
    INSTALL(TARGETS ${PROJECT_NAME}_so
      ARCHIVE DESTINATION
      LIBRARY DESTINATION lib)
  ENDIF()


 ###############################################################################
  #####
  #####         Compile H2T executable
  #####
  ###############################################################################
  ADD_EXECUTABLE(${PROJECT_NAME}
 ${H2T_SOURCE_DIR}/lib${PROJECT_NAME}.h
    ${source_files} ${main_file})
  TARGET_LINK_LIBRARIES(${PROJECT_NAME} ${LIBRARIES})
  INSTALL(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin)

  # in debug mode we name the executable h2t_debug
  SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES DEBUG_POSTFIX _debug)
  # in Release mode we name the executable h2t_O3
  SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES RELEASE_POSTFIX _O3)
  # in RelWithDebInfo mode we name the executable h2t_d
  SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES RELWITHDEBINFO_POSTFIX _O)
  # in MinSizeRel mode we name the executable h2t_Os
  SET_TARGET_PROPERTIES(${PROJECT_NAME} PROPERTIES MINSIZEREL_POSTFIX _Os)
