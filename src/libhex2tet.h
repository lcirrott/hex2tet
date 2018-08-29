/**
 * \file libhex2tet.h
 * \brief C API for hex2tet library.
 * \author C. Dobrzynski (Bx INP/Inria/UBordeaux)
 *
 * \version 1
 * \copyright GNU Lesser General Public License.
 *
 */

#ifndef _H2TLIB_H
#define _H2TLIB_H

#ifdef __cplusplus
extern "C" {
#endif

#define H2T_VER   "0.0.0"" c"
#define H2T_REL   "Feb. 22, 2016"
#define H2T_CPY   "Copyright (c) Bx INP-Inria-UBordeaux, 2016-"
#define H2T_STR   "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"


#include "mmg/mmg3d/libmmg3d.h"

#define  MG_NUL       (1 << 14)

/**
 * \def H2T_SUCCESS
 *
 * Return value for success.
 *
 */
#define H2T_SUCCESS       0
/**
 * \def H2T_LOWFAILURE
 *
 * Return value if the remesh process failed but we can save a conform
 * mesh.
 *
 */
#define H2T_LOWFAILURE    1
/**
 * \def H2T_STRONGFAILURE
 *
 * Return value if the remesh process failed and the mesh is
 * non-conform.
 *
 */
#define H2T_STRONGFAILURE 2

int H2T_libhex2tet(MMG5_pMesh ,int* ,int );

#ifdef __cplusplus
}
#endif

#endif
