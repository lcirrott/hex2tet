#include "hex2tet.h"

/**
 * \param mmgMesh mesh structure with only vertices.
 * \param hexa tab of hexahedra (size 9*(nbhexa+1) : 8 vertices per hexa + one ref).
 * \param nbhexa number of hexa
 * \return \ref H2T_SUCCESS if success.
 * \return \ref H2T_LOWFAILURE if failed but a conform mesh is saved.
 * \return \ref H2T_STRONGFAILURE if failed and we can't save the mesh.
 *
 * hex2tet library.
 *
 */
int H2T_libhex2tet(MMG5_pMesh mmgMesh,int* hexa,int nbhexa) {
  Hedge             hed2;
  int               norient;
  int              *adjahex,k;


  /*chk orientation*/
  norient = H2T_chkorient(mmgMesh,hexa,nbhexa);
  fprintf(stdout,"%8d HEXA REORIENTED\n",norient);

  /*hexa adjacency*/
  adjahex = (int*)calloc(6*nbhexa+7,sizeof(int));
  assert(adjahex);
  if(!H2T_hashHexa(hexa,adjahex,nbhexa)) return(0);

  /*cut hexa into tet*/
  hed2.size  = 6*nbhexa;
  hed2.hnxt  = 6*nbhexa;
  hed2.nhmax = (int)(16*6*nbhexa);
  hed2.item  = (hedge*)calloc(hed2.nhmax+1,sizeof(hedge));
  //assert(hedg->item);

  for (k=6*nbhexa; k<hed2.nhmax; k++)
    hed2.item[k].nxt = k+1;
  
  if(6*nbhexa >= mmgMesh->nemax) {
    printf("on va avoir un pbs de mem avec mmg ==> revoir API\n");
    exit(0);
  }
  H2T_cuthex(mmgMesh,&hed2,hexa,adjahex,nbhexa);

  free(adjahex);
  free(hed2.item);
 
  return(H2T_SUCCESS);
}
