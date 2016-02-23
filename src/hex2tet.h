#include "libhex2tet.h"

typedef struct {
  int      min,max,iel,nxt;
} hedge;
typedef struct {
  int      size,nhmax,hnxt;
  hedge   *item;  
} Hedge;
typedef Hedge * pHedge;

int H2T_chkorient(MMG5_pMesh ,int* ,int );
double H2T_quickvol(double *,double *,double *,double *);
double H2T_voltet(MMG5_pMesh ,int );

int H2T_hashHexa(int* ,int* ,int );
int H2T_edgePut(pHedge ,int ,int ,int );
int H2T_edgePoint(pHedge ,int ,int );

int H2T_cuthex(MMG5_pMesh ,pHedge ,int* ,int* ,int ) ;
