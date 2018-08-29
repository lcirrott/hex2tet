/**
 * \file hex2tet.h
 * \brief hex2tet.c header file
 *
 * \author Cecile Dobrzynski (Bx INP/Inria/UBordeaux)
 *
 * \version 1
 * \copyright GNU Lesser General Public License.
 */

#include "libhex2tet.h"

/** \brief hidir[i]: vertices of the face i */
static const unsigned char H2T_hidir[6][4] = { {0,3,2,1},{0,4,7,3},{0,1,5,4},{4,5,6,7},{1,2,6,5},{2,3,7,6} };
/** \brief hidiropp[i]: vertices of the face opposite to the face i */
static const unsigned char  H2T_hidirop[6][4] = { {7,4,5,6}, {5,1,2,6}, {7,3,2,6}, {1,0,3,2}, {3,0,4,7}, {0,1,5,4} };
/** \brief hopp[i][j]: vertex of face i opposite to the vertex j  */
static const unsigned char H2T_hopp [6][4] = { {2,1,0,3},{7,3,0,4},{5,4,0,1},{6,7,4,5},{6,5,1,2},{7,6,2,3} };
/** \brief hied[i][.]: diagonal starting from the vertex i of one of the tetra face */
static const unsigned char H2T_hied[8][3] = { {2,5,7}, {3,4,6}, {0,5,7}, {1,4,6}, {1,3,6}, {0,2,7}, {1,3,4}, {0,2,5} };
/** \brief hop[i]: vertex opposite to the vertex i */
static const unsigned char H2T_hop[8] = { 6,7,4,5,2,3,0,1 };

/**
 * \warning to replace with _MMG5_hedge struct
 */
typedef struct {
  int      min,max,iel,nxt;
} hedge;


/**
 * \warning to replace with _MMG5_Hash struct
 */
typedef struct {
  int      size,nhmax,hnxt;
  hedge   *item;
} Hedge;
typedef Hedge * pHedge;

int    H2T_chkorient(MMG5_pMesh ,int* ,int );
double H2T_quickvol(double *,double *,double *,double *);
double H2T_voltet(MMG5_pMesh ,int );

int    H2T_hashHexa(int* ,int* ,int );
int    H2T_edgePut(pHedge ,int ,int ,int );
int    H2T_edgePoint(pHedge ,int ,int );

int    H2T_cuthex(MMG5_pMesh ,pHedge ,int* ,int* ,int ) ;
