#include "hex2tet.h"


static int H2T_decouphex(MMG5_pMesh mesh, pHedge hed,int* p,int ref) {
  MMG5_pTetra  pt;
  int     i,nu1,nu2,iel;

  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[0];
  pt->v[1] = p[1];
  pt->v[2] = p[3];
  pt->v[3] = p[7];
  if(H2T_quickvol(mesh->point[p[0]].c,mesh->point[p[1]].c,
		  mesh->point[p[3]].c,mesh->point[p[7]].c) < 0) {
    printf("ahhhh %e\n",H2T_quickvol(mesh->point[p[0]].c,mesh->point[p[1]].c,
				     mesh->point[p[3]].c,mesh->point[p[7]].c));
  }
  pt->ref  = ref;
 
  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[7];
  pt->v[1] = p[2];
  pt->v[2] = p[6];
  pt->v[3] = p[1];
  if(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c) < 0) {
    printf("ahhhh2 %e\n",H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
  } 
  pt->ref  = ref;

  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[1];
  pt->v[1] = p[4];
  pt->v[2] = p[5];
  pt->v[3] = p[7];
  if(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c) < 0) {
    printf("ahhhh32 %e\n",H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
  } 
  pt->ref  = ref;

  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[7];
  pt->v[1] = p[4];
  pt->v[2] = p[0];
  pt->v[3] = p[1];
  if(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c) < 0) {
    printf("ahhhh42 %e\n",H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
  } 
  pt->ref  = ref;
 
  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[1];
  pt->v[1] = p[6];
  pt->v[2] = p[7];
  pt->v[3] = p[5];
  if(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c) < 0) {
    printf("ahhhh52 %e\n",H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
  } 
  pt->ref  = ref;

  if(mesh->ne+1 >= mesh->nemax) {
    fprintf(stdout,"mem problem = change mmg API\n");
    exit(0);
  }
  iel = ++mesh->ne;
  mesh->nenil = mesh->tetra[iel].v[3];
  mesh->tetra[iel].v[3] = 0;
  mesh->tetra[iel].mark=0;
  pt = &mesh->tetra[iel];
  pt->v[0] = p[1];
  pt->v[1] = p[3];
  pt->v[2] = p[7];
  pt->v[3] = p[2];
  if(H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c) < 0) {
    printf("ahhhh62 %e\n",H2T_quickvol(mesh->point[pt->v[0]].c,mesh->point[pt->v[1]].c,
		  mesh->point[pt->v[2]].c,mesh->point[pt->v[3]].c));
  } 
  pt->ref  = ref;
 
  //add edges at hashtable
  H2T_edgePut(hed,p[0],p[7],2);
  H2T_edgePut(hed,p[1],p[3],2);
  H2T_edgePut(hed,p[2],p[7],2);
  H2T_edgePut(hed,p[1],p[6],2);
  H2T_edgePut(hed,p[1],p[4],2);
  H2T_edgePut(hed,p[5],p[7],2);
  /* printf("put %d %d\n",p[0],p[7]); */
  /* printf("put %d %d\n",p[1],p[3]); */
  /* printf("put %d %d\n",p[2],p[7]); */
  /* printf("put %d %d\n",p[1],p[6]); */
  /* printf("put %d %d\n",p[1],p[4]); */
  /* printf("put %d %d\n",p[5],p[7]); */

  return(1);
}



static int H2T_checkcaseopp(int ph[8],int nu1,int nu2,pHedge hed) {
  int i,nu3,nu4;
  unsigned char H2T_hied[8][3] = { {2,5,7}, {3,4,6}, {0,5,7}, {1,4,6}, {1,3,6}, {0,2,7}, {1,3,4}, {0,2,5} };
  unsigned char H2T_hop[8] = { 6,7,4,5,2,3,0,1 };
  nu4 = H2T_hop[nu1];
  for(i=0;i<3;i++) {
    nu3 = H2T_hied[nu4][i];
    if(nu3==H2T_hop[nu2]) continue;
    if(H2T_edgePoint(hed,ph[nu4],ph[nu3])) break;
  }
  if(i<3) return(1);
  else return(0);
}

static int H2T_checkcase(int ph[8],int nu1,int nu2,pHedge hed) {
  int i,nu3;
  unsigned char H2T_hied[8][3] = { {2,5,7}, {3,4,6}, {0,5,7}, {1,4,6}, {1,3,6}, {0,2,7}, {1,3,4}, {0,2,5} };

  for(i=0;i<3;i++) {
    nu3 = H2T_hied[nu1][i];
    if(nu3==nu2) continue;
    if(H2T_edgePoint(hed,ph[nu1],ph[nu3])) break;
  }
  if(i<3) return(1);
  else return(0);

}
int H2T_cuthex(MMG5_pMesh mesh,pHedge hed,int* listhexa,int* adjahex,int nhex) {
  MMG5_pTetra    pt;
  MMG5_pPoint    ppt;
  int            i,ih,k,nu1,nu2,nu3,nu4,adj,icas0,icasopp,nncut;
  int            *list,*mark,p[8],mini,minnumber,ipil,icurc,iface,iadr;
  int            iel,ip,ph[8];
  unsigned char  H2T_hidir[6][4] = { {0,3,2,1}, {0,4,7,3}, {0,1,5,4}, {4,5,6,7}, {1,2,6,5}, {2,3,7,6} };
  unsigned char  H2T_hidirop[6][4] = { {7,4,5,6}, {5,1,2,6}, {7,3,2,6}, {1,0,3,2}, {3,0,4,7}, {0,1,5,4} };
  unsigned char  H2T_hied[8][3] = { {2,5,7}, {3,4,6}, {0,5,7}, {1,4,6}, {1,3,6}, {0,2,7}, {1,3,4}, {0,2,5} };
  unsigned char  H2T_hop[8] = { 6,7,4,5,2,3,0,1 };
  double         volhex,c[3];
  int           ddebug;

  /*alloc*/
  list = (int*) calloc(7*nhex+1,sizeof(int));
  assert(list);                             
  mark = (int*) calloc(nhex+1,sizeof(int));
  assert(mark);                             

  /*init pile*/
  mark[1] = -1;
  for(ih=0;ih<8;ih++) p[ih] = listhexa[9+ih];
  //printf("p %d %d %d %d %d %d %d %d\n",p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]);
  H2T_decouphex(mesh,hed,p,listhexa[9+8]);
  
  icurc = 0;
  ipil  = 0;
  for(i=0;i<6;i++) {
    iadr = 1;
    adj = adjahex[iadr + i];
    if(!adj) continue;
    list[ipil++] = adj;
  }

  
  while(icurc++ < ipil) {
    k = list[icurc-1]/6;

    if(!k) continue;
    for(ih=0;ih<8;ih++) ph[ih] = listhexa[9*k+ih];
    //printf("ph %d %d %d %d %d %d %d %d\n",ph[0],ph[1],ph[2],ph[3],ph[4],ph[5],ph[6],ph[7]);
   if(mark[k] < 0) continue;
   //printf("on depile %d et %d\n",k,list[15620]);
    mark[k] = -1;
    
    iface = list[icurc-1]%6;
    if(iface < 0) {
      printf("ahhhhhhhhhhhhhhhhhhhhhhhhhhh %d %d %d == %d\n",iface,icurc-1,list[icurc-1],list[15620]);
      exit(0);
    }
    ddebug=0;
    
    nu1 = H2T_hidir[iface][0];
    nu2 = H2T_hidir[iface][2];
    if(H2T_edgePoint(hed,ph[nu1],ph[nu2])) {
      //if edge opp sur face opp exist, on degage tout de suite
      nu3 = H2T_hidirop[iface][0];
      nu4 = H2T_hidirop[iface][2];
      if(H2T_edgePoint(hed,ph[nu3],ph[nu4])) {
	mark[k] = -10;
	continue;
      }
      if(iface==1 || iface==5) {
	//find if other edge with ph->v[MMG_hidir[iface][0]], if yes->renum
	icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	  icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	if(icas0) {
	  //debug check
	  for(i=0;i<3;i++) {
	    nu3 = H2T_hied[nu2][i];
	    if(nu3==nu1) continue;
	    if(H2T_edgePoint(hed,ph[nu2],ph[nu3])) break;
	  }
	  assert(i==3);
	  //printf("iface %d on a trouve une autre arete---> renum\n",iface);
	  if(iface==1) {
	    p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	    p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  } else {
	    p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	    p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  }
	  H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
	} else {
	  H2T_decouphex(mesh,hed,ph,listhexa[9*k+8]);
	}
      } else if (iface==4) {
	icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	  icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	if(icas0) {
	  //check debug
	  for(i=0;i<3;i++) {
	    nu3 = H2T_hied[nu1][i];
	    if(nu3==nu2) continue;
	    if(H2T_edgePoint(hed,ph[nu1],ph[nu3])) break;
	  }
	  assert(i==3);
	  //printf("iface 4 on a trouve une autre arete---> renum\n");
	  p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	  p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
	} else {
	  H2T_decouphex(mesh,hed,ph,listhexa[9*k+8]);
	}
      } else {
	if(ddebug)  printf("il faut renum iface %d\n",iface);//iface 0,2,3
	icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	  icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	if(icas0) {
	  //check debug
	  for(i=0;i<3;i++) {
	    nu3 = H2T_hied[nu2][i];
	    if(nu3==nu1) continue;
	    if(H2T_edgePoint(hed,ph[nu2],ph[nu3])) break;
	  }
	  assert(i==3);
	  icas0=1;
	}
	if(ddebug) printf("icas %d\n",icas0);
	switch(iface) {
	case(0):
	  if(icas0) {
	    p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	    p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  } else {
	    p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	    p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  }
	  break;
	case(2):
	  if(icas0) {
	    p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	    p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  } else {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  }
	  break;
	case(3):
	  if(icas0){// ph->v[MMG_hidir[iface][0]]<ph->v[MMG_hidir[iface][2]]) {
	    p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	    p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  } else {
	    p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	    p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  }
	  break;
	}
       
	H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
      }
    }  else if (H2T_edgePoint(hed,ph[H2T_hidir[iface][1]],ph[H2T_hidir[iface][3]])) {
      nu1 = H2T_hidir[iface][1];
      nu2 = H2T_hidir[iface][3];
     
      //if edge opp face opp exist on degage
      nu3 = H2T_hidirop[iface][1];
      nu4 = H2T_hidirop[iface][3];
      if(H2T_edgePoint(hed,ph[nu3],ph[nu4])) {
	mark[k] = -10;
	continue;
      }
      if(iface==0 || iface==3) {
	icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	  icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	if(ddebug) printf("icas0 %d\n",icas0);
	if(icas0)       {
	  //check debug
	  for(i=0;i<3;i++) {
	    nu3 = H2T_hied[nu2][i];
	    if(nu3==nu1) continue;
	    if(H2T_edgePoint(hed,ph[nu2],ph[nu3])) {printf("on trouve arg %d %d\n",ph[nu2],ph[nu3]);break;    }
	  }
	  assert(i==3);
	  if(iface==0) {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  } else {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  }
	  H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
	} else {
	  H2T_decouphex(mesh,hed,ph,listhexa[9*k+8]);
	}
      } else if(iface==2){
	icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	  icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	if(icas0) {
	  //check debug
	  for(i=0;i<3;i++) {
	    nu3 = H2T_hied[nu1][i];
	    if(nu3==nu2) continue;
	    if(H2T_edgePoint(hed,ph[nu1],ph[nu3])) break;
	  }
	  assert(i==3);
	  p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	  p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
	} else {
	  H2T_decouphex(mesh,hed,ph,listhexa[9*k+8]);
	}
      }
      else {
	if(ddebug)  printf("il faut renum iface %d\n",iface);//iface 1,4,5
	icas0 = H2T_checkcase(ph,nu1,nu2,hed);
	icasopp = H2T_checkcaseopp(ph,nu1,nu2,hed);
	if(!icas0 && !icasopp) {
	  icas0 = 0;
	} else {
	  icas0 = H2T_checkcase(ph,nu2,nu1,hed);
	  icasopp = H2T_checkcaseopp(ph,nu2,nu1,hed);
	  if(icas0 || icasopp) {
	    mark[k] = -10;
	    continue;
	  }
	  icas0 = 1;
	}
	switch(iface) {
	case(1):
	  if(icas0){//ph[H2T_hidir[iface][1]]<ph[H2T_hidir[iface][3]]) {
	    p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	    p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  } else {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  }
	  break;
	case(4):
	  if(ddebug) printf("at the beginning %d : %d %d %d %d %d %d %d %d\n",k,ph[0],ph[1],ph[2],ph[3],ph[4],ph[5],ph[6],ph[7]);
	  if(icas0) {
	    p[0] = ph[1]; p[1] = ph[2]; p[2] = ph[3]; p[3] = ph[0];
	    p[4] = ph[5]; p[5] = ph[6]; p[6] = ph[7]; p[7] = ph[4];
	  } else {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  }
	  if(ddebug)  printf("at the end %d : %d %d %d %d %d %d %d %d\n",k,p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7]);
	  break;
	case(5):
	  if(icas0) {
	    p[0] = ph[2]; p[1] = ph[3]; p[2] = ph[0]; p[3] = ph[1];
	    p[4] = ph[6]; p[5] = ph[7]; p[6] = ph[4]; p[7] = ph[5];
	  } else {
	    p[0] = ph[3]; p[1] = ph[0]; p[2] = ph[1]; p[3] = ph[2];
	    p[4] = ph[7]; p[5] = ph[4]; p[6] = ph[5]; p[7] = ph[6];
	  }
	  break;
	}
	H2T_decouphex(mesh,hed,p,listhexa[9*k+8]);
      }
    } else {
      printf("ya un pbs!!!!! %d %d et %d %d\n",ph[H2T_hidir[iface][1]],ph[H2T_hidir[iface][3]],
	     ph[H2T_hidir[iface][0]],ph[H2T_hidir[iface][2]]);
   printf("ph %d %d %d %d %d %d %d %d\n",ph[0],ph[1],ph[2],ph[3],ph[4],ph[5],ph[6],ph[7]);
   printf("iface %d hidir %d %d\n",iface,H2T_hidir[iface][1],H2T_hidir[iface][3]);
      exit(0);
    }
    for(i=0;i<6;i++) {
      iadr = 6*(k-1)+1;
      adj = adjahex[iadr + i];
      if(!adj) continue;
      if(mark[adj/6]!=0) continue;
      if(mark[adj/6]==0) mark[adj/6] = 10;
      list[ipil++] = adj;
      //printf("k= %d on rajoute hexa %d (iface %d) en %d -- par face %d \n",k,adj/6,adj%6,ipil-1,i);
    }
    // printf("---- end ipil %d icurc %d / %d\n",ipil,icurc,nhex);
  }

  printf("----------------------------------------------\n\n");
  //stay few hexa not cutted...
  nncut = 0;
  for(k=1 ; k<=nhex ; k++) {
    if(mark[k]==-1) continue;
    for(i=0;i<8;i++) ph[i] = listhexa[9*k+i];    
    nncut++;
    //create new vertex
    c[0] = c[1] = c[2] = 0.;
    for(i=0 ; i<8 ; i++) {
      ppt = &mesh->point[ph[i]];
      c[0] += ppt->c[0];        c[1] += ppt->c[1]; c[2] += ppt->c[2];
    }
    c[0] /= 8.; c[1] /= 8.; c[2] /= 8.;
    if(mesh->np+1 >= mesh->npmax) {
      fprintf(stdout,"mem problem = change mmg API\n");
      exit(0);
    }
    ip   = ++mesh->np;
    ppt   = &mesh->point[ip];
    memcpy(ppt->c,c,3*sizeof(double));
    mesh->npnil = ppt->tmp;
    ppt->tmp    = 0;
    ppt->ref = 0;
    ppt->xp = 0;
    ppt->flag = 0;
    ppt->n[0]   = 0;
    ppt->n[1]   = 0;
    ppt->n[2]   = 0;
    ppt->tag    = 0;
    ppt->tagdel = 0;
   
    //create 2 tets per faces
    for(i=0 ; i<6 ; i++) {
      nu1 = H2T_hidir[i][0];
      nu2 = H2T_hidir[i][2];
      if(H2T_edgePoint(hed,ph[nu1],ph[nu2])) {
	if(mesh->ne+1 >= mesh->nemax) {
	       fprintf(stdout,"mem problem = change mmg API\n");
	       exit(0);
	}
    	iel = ++mesh->ne;
	mesh->nenil = mesh->tetra[iel].v[3];
	mesh->tetra[iel].v[3] = 0;
	mesh->tetra[iel].mark=0;
    	pt = &mesh->tetra[iel];
    	pt->v[0] = ip;
    	pt->v[1] = ph[nu1];
    	pt->v[2] = ph[nu2];
    	pt->v[3] = ph[H2T_hidir[i][1]];
    	pt->ref  = listhexa[9*k+8]; 
    	if(H2T_voltet(mesh,iel)<0) {
    	  pt->v[3] = ph[nu2];
    	  pt->v[2] = ph[H2T_hidir[i][1]];
    	}
	if(mesh->ne+1 >= mesh->nemax) {
	       fprintf(stdout,"mem problem = change mmg API\n");
	       exit(0);
	}
    	iel = ++mesh->ne;
	mesh->nenil = mesh->tetra[iel].v[3];
	mesh->tetra[iel].v[3] = 0;
	mesh->tetra[iel].mark=0;
    	pt = &mesh->tetra[iel];
    	pt->v[0] = ip;
    	pt->v[1] = ph[nu1];
    	pt->v[2] = ph[H2T_hidir[i][3]];
    	pt->v[3] = ph[nu2];
    	pt->ref  = listhexa[9*k+8];;
    	if(H2T_voltet(mesh,iel)<0) {
    	  pt->v[3] = ph[H2T_hidir[i][3]];
    	  pt->v[2] = ph[nu2];
    	}
      } else {
    	nu1 = H2T_hidir[i][1];
    	nu2 = H2T_hidir[i][3];
    	if(!H2T_edgePoint(hed,ph[nu1],ph[nu2])) H2T_edgePut(hed,ph[nu1],ph[nu2],2);
	if(mesh->ne+1 >= mesh->nemax) {
	       fprintf(stdout,"mem problem = change mmg API\n");
	       exit(0);
	}
    	iel = ++mesh->ne;
	mesh->nenil = mesh->tetra[iel].v[3];
	mesh->tetra[iel].v[3] = 0;
	mesh->tetra[iel].mark=0;
   	pt = &mesh->tetra[iel];
    	pt->v[0] = ip;
    	pt->v[1] = ph[nu1];
    	pt->v[2] = ph[H2T_hidir[i][0]];
    	pt->v[3] = ph[nu2];
    	pt->ref  = listhexa[9*k+8];;
    	if(H2T_voltet(mesh,iel)<0) {
    	  pt->v[3] = ph[H2T_hidir[i][0]];
    	  pt->v[2] = ph[nu2];
    	}
	if(mesh->ne+1 >= mesh->nemax) {
	       fprintf(stdout,"mem problem = change mmg API\n");
	       exit(0);
	}
    	iel = ++mesh->ne;
	mesh->nenil = mesh->tetra[iel].v[3];
	mesh->tetra[iel].v[3] = 0;
	mesh->tetra[iel].mark=0;
    	pt = &mesh->tetra[iel];
    	pt->v[0] = ip;
    	pt->v[1] = ph[nu1];
    	pt->v[2] = ph[nu2];
    	pt->v[3] = ph[H2T_hidir[i][2]];
    	pt->ref  = listhexa[9*k+8];

    	if(H2T_voltet(mesh,iel)<0) {
    	  pt->v[2] = ph[H2T_hidir[i][2]];
    	  pt->v[3] = ph[nu2];
    	}
      }
    }
  }
  if(nncut) fprintf(stdout,"  $$ %8d ADDED VERTEX\n",nncut);
  return(1);
}

