/**
 * \file libhex2tet.c
 * \brief main library function: cut an hexahedral mesh into a tetrahedral one
 *
 * \author Cecile Dobrzynski (Bx INP/Inria/UBordeaux)
 * \author Algiane Froehly (InriaSoft)
 *
 * \version 1
 * \copyright GNU Lesser General Public License.
 */

#include "hex2tet.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/**
 * \param mmgMesh mesh structure with only vertices.
 * \param hexa tab of hexahedra (size 9*(nbhexa+1) : 8 vertices per hexa + one ref).
 * \param nbhexa number of hexa
 *
 * \return \ref H2T_SUCCESS if success.
 * \return \ref H2T_LOWFAILURE if failed but a conform mesh is saved.
 * \return \ref H2T_STRONGFAILURE if failed and we can't save the mesh.
 *
 * main library function: cut hexa into tetra.
 *
 */
int H2T_libhex2tet(MMG5_pMesh mmgMesh,int* hexa,int nbhexa) {
  Hedge             hed2;
  int               norient;
  int              *adjahex,k,ier;

  if ( mmgMesh->info.imprim ) {
    fprintf(stdout,"\n  -- HEX2TET, Release %s (%s) \n",H2T_VER,H2T_REL);
    fprintf(stdout,"     %s\n",H2T_CPY);
    fprintf(stdout,"     %s %s\n",__DATE__,__TIME__);
  }

  /* chk orientation */
  norient = H2T_chkorient(mmgMesh,hexa,nbhexa);
  fprintf(stdout,"\n  -- WARNING: %8d HEXA REORIENTED\n",norient);

  /* hexa adjacency */
  adjahex = (int*)calloc(6*nbhexa+7,sizeof(int));
  assert(adjahex);

  if(!H2T_hashHexa(hexa,adjahex,nbhexa)) return H2T_STRONGFAILURE;

  /* cut hexa into tet */
  hed2.size  = 6*nbhexa;
  hed2.hnxt  = 6*nbhexa;
  hed2.nhmax = (int)(16*6*nbhexa);
  hed2.item  = (hedge*)calloc(hed2.nhmax+1,sizeof(hedge));

  for (k=6*nbhexa; k<hed2.nhmax; k++)
    hed2.item[k].nxt = k+1;

  if ( 6*nbhexa >= mmgMesh->nemax ) {
    printf("\n  -- ERROR: Not enough memory to store the final mesh"
           " (max number of tetra=%d while the number of hexa times 6 is %d.\n",
           mmgMesh->nemax,nbhexa*6);
    return H2T_STRONGFAILURE;
  }

  ier = H2T_cuthex(mmgMesh,&hed2,hexa,adjahex,nbhexa);

  if ( !ier )
    ier = H2T_STRONGFAILURE;
  else
    ier = H2T_SUCCESS;

  free(adjahex);
  free(hed2.item);

  return ier;
}
