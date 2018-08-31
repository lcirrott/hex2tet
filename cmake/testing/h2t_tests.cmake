IF ( BUILD_TESTING )

  INCLUDE ( CTest )

  SET ( CI_DIR  ${PROJECT_SOURCE_DIR}/ci_tests )
  SET ( LIB_DIR  ${PROJECT_SOURCE_DIR}/libexamples )
  SET ( CTEST_OUTPUT_DIR ${PROJECT_BINARY_DIR}/TEST_OUTPUTS )
  FILE ( MAKE_DIRECTORY  ${CTEST_OUTPUT_DIR} )

  IF ( LIBHEX2TET_STATIC )
    SET ( lib_name lib${PROJECT_NAME}_a )
  ELSEIF ( LIBHEX2TET_SHARED )
    SET ( lib_name lib${PROJECT_NAME}_so )
  ELSE ()
    MESSAGE(WARNING "You must activate the compilation of the static or"
      " shared ${PROJECT_NAME} library to compile this tests." )
  ENDIF ( )


  IF ( ${lib_name} )
    SET ( test_name  libhex2tet_C_example )
    SET ( main_path  ${LIB_DIR}/C/ex1.c )
    SET ( out_file    "${CTEST_OUTPUT_DIR}/ex1.o.mesh" )

    ADD_LIBRARY_TEST ( ${test_name} ${main_path} "copy_hex2tet_headers" "${lib_name}" )
    ADD_TEST ( NAME ${test_name} COMMAND $<TARGET_FILE:${test_name}> ${out_file} )
  ENDIF ( )


ENDIF ( )
