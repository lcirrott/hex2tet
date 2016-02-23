#include "hex2tet.h"

//adjacency for hexahedra
int H2T_hashHexa(int* listhexa,int* adjahex,int nhex) {
  int       k,kk,pp,l,ll,mins,mins1,opps,opps1,sum,sum1,iadr;
  int      *hcode,*link,hsize,imins,imins1;
  int       ph[8],ph1[8];
  long int  inival;
  unsigned char  i,ii,iii,i1,i2,i3,i4;
  unsigned int    key;
  unsigned char H2T_hidir[6][4] = { {0,3,2,1}, {0,4,7,3}, {0,1,5,4}, {4,5,6,7}, {1,2,6,5}, {2,3,7,6} };
  unsigned char H2T_hopp[6][4] = { {2,1,0,3}, {7,3,0,4}, {5,4,0,1}, {6,7,4,5}, {6,5,1,2}, {7,6,2,3} };
  int KA=31;
  int KB=57;
  int KC=79;


  /* default */
  fprintf(stdout,"  ** SETTING HEXA ADJACENCIES\n");
  fflush(stdout);
  /* memory alloc */
  hcode = (int*)calloc(nhex+1,sizeof(int));
  assert(hcode);
  link  = adjahex;
  hsize = nhex;

  /* init */
  inival = 2147483647;
  for (k=0; k<=nhex; k++)
    hcode[k] = -inival;

  /* build hash table */
  for (k=1; k<=nhex; k++) {
    iadr = 9*k;
    for(iii=0 ; iii<8 ; iii++)
      ph[iii] = listhexa[iadr+iii];

    for (i=0; i<6; i++) {
      i1 = H2T_hidir[i][0];
      i2 = H2T_hidir[i][1];
      i3 = H2T_hidir[i][2];
      i4 = H2T_hidir[i][3];
      mins = ph[i1];
      imins = 0;
      if(ph[i2]<mins) {
	mins = ph[i2];
	imins = 1;
      }
      if(ph[i3]<mins) {
	mins = ph[i3];
	imins = 2;
      }
      if(ph[i4]<mins) {
	mins = ph[i4];
	imins = 3;
      }
      opps = ph[H2T_hopp[i][imins]];
      /* compute key */
      sum = ph[i1] + ph[i2] + ph[i3] + ph[i4];
      key = KA*mins + KB*opps + KC*sum;
      key = key % hsize + 1;
     
      /* insert */
      iadr = 6*(k-1) + i+1;
      link[iadr] = hcode[key];
      hcode[key] = -iadr;
    }
  }

  /* set adjacency */
  for (l=6*nhex; l>0; l--) {
    if ( link[l] >= 0 )  continue;
    k = ((l-1) /6) + 1;
    i = (l-1) % 6;
    i1 = H2T_hidir[i][0];
    i2 = H2T_hidir[i][1];
    i3 = H2T_hidir[i][2];
    i4 = H2T_hidir[i][3];
    iadr = 9*k;
    for(iii=0 ; iii<8 ; iii++)
      ph[iii] = listhexa[iadr+iii];
    sum  = ph[i1] + ph[i2] + ph[i3] + ph[i4];
    mins = ph[i1];
    imins = 0;
    if(ph[i2]<mins) {
      mins = ph[i2];
      imins = 1;
    }
    if(ph[i3]<mins) {
      mins = ph[i3];
      imins = 2;
    }
    if(ph[i4]<mins) {
      mins = ph[i4];
      imins = 3;
    }
    opps = ph[H2T_hopp[i][imins]];

    /* accross link */
    ll = -link[l];
    pp = 0;
    link[l] = 0;
    while ( ll != inival ) {
      kk = ((ll-1) /6) + 1;
      ii = (ll-1) % 6;
      i1 = H2T_hidir[ii][0];
      i2 = H2T_hidir[ii][1];
      i3 = H2T_hidir[ii][2];
      i4 = H2T_hidir[ii][3];
      iadr = 9*kk;
      for(iii=0 ; iii<8 ; iii++)
	ph1[iii] = listhexa[iadr+iii];
      sum1 = ph1[i1] + ph1[i2] + ph1[i3] + ph1[i4];
      if ( sum1 == sum ) {
	mins1 = ph1[i1];
	imins1 = 0;
	if(ph1[i2]<mins1) {
	  mins1 = ph1[i1];
	  imins1 = 1;
	}
	if(ph1[i3]<mins1) {
	  mins1 = ph1[i3];
	  imins1 = 2;
	}
	if(ph1[i4]<mins1) {
	  mins1 = ph1[i4];
	  imins1 = 3;
	}
	opps1 = ph1[H2T_hopp[ii][imins1]];

	if ( mins1 == mins ) {
	  if ( opps1 == opps ) {
	    /* adjacent found */
	    if ( pp != 0 )  link[pp] = link[ll];
	    link[l] = 6*kk + ii;
	    link[ll]= 6*k + i;
	    break;
	  }
	}
      }
      pp = ll;
      ll = -link[ll];
    }
  }

  free(hcode);
  return(1);
}

int H2T_edgePoint(pHedge hash,int a,int b) {
  int        key,mins,maxs;
  hedge     *ha;
  int KA=31;
  int KB=57;

  /* compute key */
  mins = a;
  maxs = b;
  if ( a > b ) {
    mins = b;
    maxs = a;
  }
  key = KA*mins + KB*maxs;
  key = key % hash->size;
  ha  = &hash->item[key];
  //printf("cherche %d %d\n",mins,maxs);
  if ( !ha->min )  return(0);
  else if ( ha->min == mins && ha->max == maxs ) {
    return(ha->iel);
  }
  else if ( ha->nxt ) {
    do {
      ha = &hash->item[ha->nxt];
      if ( ha->min == mins && ha->max == maxs ) {
	return(ha->iel);
      }
    }
    while ( ha->nxt && ha->nxt < hash->nhmax );
  }
  return(0);
}

/*put np on edge a-b*/
int H2T_edgePut(pHedge hash,int a,int b,int np) {
  int        key,mins,maxs;
  hedge     *ha;
  int KA=31;
  int KB=57;

  mins = a;
  maxs = b;
  if ( a > b ) {
    mins = b;
    maxs = a;
  }
  key = KA*mins + KB*maxs;
  key = key % hash->size;
  ha  = &hash->item[key];

  if ( ha->min ) {
    /* identical edge */
    if ( ha->min == mins && ha->max == maxs ) {
      return(ha->iel);
    }
    else {
      while ( ha->nxt && ha->nxt < hash->nhmax ) {
	ha = &hash->item[ha->nxt];
	if ( ha->min == mins && ha->max == maxs ) {
	  return(ha->iel);
	}
      }
    }
    ha->nxt = hash->hnxt;
    ha      = &hash->item[hash->hnxt];
    ++hash->hnxt;
    if ( hash->hnxt >= hash->nhmax ) {
      fprintf(stdout,"  ## Memory alloc problem (edge): %d\n",hash->nhmax);
      return(0);
    }
  }
  //printf("insert %d %d\n",mins,maxs);
  /* insert */
  ha->min = mins;
  ha->max = maxs;
  ha->iel = np;
  ha->nxt = 0;

  return(1);
}
