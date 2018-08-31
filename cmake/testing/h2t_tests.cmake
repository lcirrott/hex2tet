IF ( BUILD_TESTING )

  INCLUDE ( CTest )

  SET ( CI_DIR  ${PROJECT_SOURCE_DIR}/ci_tests )
  SET ( LIB_DIR  ${PROJECT_SOURCE_DIR}/libexamples )
  SET ( CTEST_OUTPUT_DIR ${PROJECT_BINARY_DIR}/TEST_OUTPUTS )
  FILE ( MAKE_DIRECTORY  ${CTEST_OUTPUT_DIR} )


  ADD_TEST ( NAME hex2tet_exec_test COMMAND $<TARGET_FILE:hex2tet>
    ${CI_DIR}/hexa.mesh ${CTEST_OUTPUT_DIR}/hexa.o.mesh )

  ###############################################################################
  #####
  #####        Tests that needs the PARMMG LIBRARY
  #####
  ###############################################################################

  IF ( LIBHEX2TET_STATIC )
    SET ( lib_name lib${PROJECT_NAME}_a )
  ELSEIF ( LIBHEX2TET_SHARED )
    SET ( lib_name lib${PROJECT_NAME}_so )
  ELSE ()
    MESSAGE(WARNING "You must activate the compilation of the static or"
      " shared ${PROJECT_NAME} library to compile this tests." )
  ENDIF ( )

  IF ( LIBHEX2TET_STATIC OR LIBHEX2TET_SHARED )

    SET ( H2T_LIB_TESTS            libhex2tet_C_example )
    SET ( H2T_LIB_TESTS_MAIN_PATH  ${LIB_DIR}/C/ex1.c   )
    SET ( H2T_LIB_TESTS_OUTPUTMESH "${CTEST_OUTPUT_DIR}/ex1-C.o.mesh" )

    IF ( CMAKE_Fortran_COMPILER )
      ENABLE_LANGUAGE ( Fortran )

      LIST ( APPEND H2T_LIB_TESTS            libhex2tet_Fortran_example )
      LIST ( APPEND H2T_LIB_TESTS_MAIN_PATH  ${LIB_DIR}/Fortran/ex1.F90 )
      LIST ( APPEND H2T_LIB_TESTS_OUTPUTMESH "${CTEST_OUTPUT_DIR}/ex1-Fortran.o.mesh" )

    ENDIF ( )

    LIST(LENGTH H2T_LIB_TESTS nbTests_tmp)
    MATH(EXPR nbTests "${nbTests_tmp} - 1")

    FOREACH ( test_idx RANGE ${nbTests} )
      LIST ( GET H2T_LIB_TESTS            ${test_idx} test_name )
      LIST ( GET H2T_LIB_TESTS_MAIN_PATH  ${test_idx} main_path )
      LIST ( GET H2T_LIB_TESTS_OUTPUTMESH ${test_idx} out_file )


      ADD_LIBRARY_TEST ( ${test_name} ${main_path} "copy_hex2tet_headers" "${lib_name}" )
      ADD_TEST ( NAME ${test_name} COMMAND $<TARGET_FILE:${test_name}> ${out_file} )
    ENDFOREACH ( )

  ENDIF ( )

ENDIF ( )
