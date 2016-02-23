#include "hex2tet.h"

/* quick volume check */
double H2T_voltet(MMG5_pMesh mesh,int iel) {
  MMG5_pTetra pt;

  pt = &mesh->tetra[iel];

  return(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
}
/* quick volume check */
double H2T_quickvol(double *c1,double *c2,double *c3,double *c4) {
  double   ax,ay,az,bx,by,bz,vol;

  ax = c3[0] - c1[0];
  ay = c3[1] - c1[1];
  az = c3[2] - c1[2];
  
  bx = c4[0] - c1[0];
  by = c4[1] - c1[1];
  bz = c4[2] - c1[2];
  
  vol = (c2[0]-c1[0]) * (ay*bz - az*by) \
      + (c2[1]-c1[1]) * (az*bx - ax*bz) \
      + (c2[2]-c1[2]) * (ax*by - ay*bx);

  return(vol);
}

int H2T_chkorient(MMG5_pMesh mmgMesh,int* hexa,int nhex) {
  int     nbado,k,i,iadr,tmp,ph[8];
  double  volref,volhex;
  
  nbado = 0;
  for (k=1; k<=nhex; k++) {
    iadr = 9*k;
    for(i=0 ; i<8 ; i++)
      ph[i] = hexa[iadr+i];
     
    //check orientability of the hexahedra : vol of tet p0 p1 p3 p4
    volhex = H2T_quickvol(mmgMesh->point[ph[0]].c,mmgMesh->point[ph[1]].c
			  ,mmgMesh->point[ph[2]].c,mmgMesh->point[ph[3]].c);
    if(k==1) {
      volref = volhex;
      //printf("vol %e\n",volref);
    }
    else {
      if(volref*volhex < 0) {
	fprintf(stdout,"BAD ORIENTATION OF HEXAHEDRON %d : %d %d %d %d %d %d %d %d\n",k,ph[0],ph[1],ph[2],ph[3],ph[4],ph[5],ph[6],ph[7]);
	nbado++;
	tmp = ph[3];
	ph[3] = ph[1];
	ph[1] = tmp;
	tmp = ph[5];
	ph[5] = ph[7];
	ph[7] = tmp;
      }
    }
  }
  return(nbado);

}
