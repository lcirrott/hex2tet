#include "libhex2tet.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <limits.h>

int H2T_loadMesh(MMG5_pMesh mmgMesh,int* tabhex,char *filename) {
  FILE*            inm;
  char             data[128],chaine[128];
  float            fx,fy,fz;
  double           x,y,z;
  int              dim,np,nhex,ref,k,iadr;

  strcpy(data,filename);
  if( !(inm = fopen(data,"r")) ) {
    fprintf(stderr,"  ** %s  NOT FOUND.\n",data);
    return(0);
  }
  fprintf(stdout,"  %%%% %s OPENED\n",data);
   
  strcpy(chaine,"D");
  while(fscanf(inm,"%s",&chaine[0])!=EOF && strncmp(chaine,"End",strlen("End")) ) {
    if(!strncmp(chaine,"Dimension",strlen("Dimension"))) {
      fscanf(inm,"%d",&dim);
      if(dim!=3) {
	fprintf(stdout,"BAD DIMENSION : %d\n",dim);
	return(0);
      }
      continue;
    } else if(!strncmp(chaine,"Vertices",strlen("Vertices"))) {
      fscanf(inm,"%d",&np);
      fprintf(stdout,"  READING %d VERTICES\n",np);
      if ( MMG3D_Set_meshSize(mmgMesh,np,0,0,0,0,0) != 1 )  exit(EXIT_FAILURE);
      for (k=1; k<=np; k++) {
      	fscanf(inm,"%lf %lf %lf %d",&x,&y,&z,&ref);
	if ( MMG3D_Set_vertex(mmgMesh,x  ,y  ,z  ,ref,  k) != 1 )  exit(EXIT_FAILURE);

	mmgMesh->point[k].tag = 0;//&= ~MG_NUL;
      }
      continue;
    } else if(!strncmp(chaine,"Hexahedra",strlen("Hexahedra"))) {
      fscanf(inm,"%d",&nhex);
      fprintf(stdout,"  READING %d HEXA\n",nhex);
      for (k=1; k<=nhex; k++) {
      	iadr = 9*k;
      	fscanf(inm,"%d %d %d %d %d %d %d %d %d",&tabhex[iadr+0],&tabhex[iadr+1]
      	       ,&tabhex[iadr+2],&tabhex[iadr+3],&tabhex[iadr+4],
      	       &tabhex[iadr+5],&tabhex[iadr+6],&tabhex[iadr+7],&tabhex[iadr+8]);
      }
      continue;
    }
  }
  fclose(inm);
  return(nhex);
}
  
/**
 * \param argc number of command line arguments.
 * \param argv command line arguments.
 * \return \ref H2T_SUCCESS if success.
 * \return \ref H2T_LOWFAILURE if failed but a conform mesh is saved.
 * \return \ref H2T_STRONGFAILURE if failed and we can't save the mesh.
 *
 * Exemple for use of hex2tet library.
 *
 */

int main(int argc,char *argv[]) {
  FILE*            inm;
  MMG5_pMesh      mmgMesh;
  MMG5_pSol       mmgSol;
  char            chaine[128],filename[128];
  int             *hexa,nbhex;

  fprintf(stdout,"\n  -- H2T, Release %s (%s) \n",H2T_VER,H2T_REL);
  fprintf(stdout,"     %s\n",H2T_CPY);
  fprintf(stdout,"     %s %s\n\n",__DATE__,__TIME__);
  fprintf(stdout,"FILENAME ?\n");
  fscanf(stdin,"%s",filename);
  /*initialisation of the mmg mesh*/
  mmgMesh = NULL;
  mmgSol  = NULL;
  hexa    = NULL;

  MMG3D_Init_mesh(MMG5_ARG_start,
  		  MMG5_ARG_ppMesh,&mmgMesh,MMG5_ARG_ppMet,&mmgSol,
  		  MMG5_ARG_end);

  /*add vertex + creation of hexa tab*/
  if( !(inm = fopen(filename,"r")) ) {
    fprintf(stderr,"  ** %s  NOT FOUND.\n",filename);
    return(0);
  }
  strcpy(chaine,"D");
  while(fscanf(inm,"%s",&chaine[0])!=EOF && strncmp(chaine,"End",strlen("End")) ) {
    if(!strncmp(chaine,"Hexahedra",strlen("Hexahedra"))) {
      fscanf(inm,"%d",&nbhex);
      fprintf(stdout,"  READING %d HEXA\n",nbhex);
      hexa = (int*) malloc(9*(nbhex+1)*sizeof(int));
      assert(hexa);
      break;
    }
  }
  fclose(inm);

  nbhex = H2T_loadMesh(mmgMesh,hexa,filename);

  /*call hex2tet library*/
  H2T_libhex2tet(mmgMesh,hexa,nbhex);
   
  MMG3D_saveMesh(mmgMesh,"h2t.mesh");
  /*free structure*/
  free(hexa);
  MMG3D_Free_all(MMG5_ARG_start,
                 MMG5_ARG_ppMesh,&mmgMesh,MMG5_ARG_ppMet,&mmgSol,
                 MMG5_ARG_end);

  return(0);
}



