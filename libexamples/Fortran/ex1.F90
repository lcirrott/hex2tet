  !>
  ! Example of call from a Fortran code of the hex2tet library (convert an
  ! array of hexa into a MMG5 tetrahedral mesh)
  !
  ! \author Vivien Pianet
  ! \author Algiane Froehly (InriaSoft)
  ! \version 1
  ! \copyright GNU Lesser General Public License.
  !

PROGRAM main

  IMPLICIT NONE

#include "hex2tet/libhex2tetf.h"

  MMG5_DATA_PTR_T                  :: mmgMesh
  MMG5_DATA_PTR_T                  :: mmgSol
  REAL(Kind=8)                     :: v(3)
  INTEGER,DIMENSION(:),ALLOCATABLE :: hexTab
  INTEGER                          :: ier,argc
  INTEGER                          :: Width,Height,Depth,nbVertices,nbHex
  INTEGER                          :: jmax,imax,kmax,ref,i,j,k,pointNumber
  INTEGER                          :: hexNumber,hexTabPosition
  INTEGER                          :: fdl,fdr,bdl,bdr,ful,fur,bul,bur,ierlib
  CHARACTER(len=300)               :: exec_name,fileout

  WRITE(*,*) "  -- TEST HEX2TETLIB"

  argc =  COMMAND_ARGUMENT_COUNT();
  CALL get_command_argument(0, exec_name)

  IF ( argc /=1 ) THEN
     PRINT*," Usage: ",TRIM(exec_name)," output_filename"
     CALL EXIT(1);
  ENDIF

  ! Name and path of the mesh file
  CALL get_command_argument(1, fileout)

  !> 1)  Step 1: Mesh allocation
  mmgMesh = 0
  mmgSol  = 0

  Width  = 50
  Height = 20
  Depth  = 10

  nbVertices = Width*Height*Depth
  nbHex = (Width - 1)    * (Height - 1) * (Depth-1)

  CALL MMG3D_Init_mesh(MMG5_ARG_start, &
       MMG5_ARG_ppMesh,mmgMesh,MMG5_ARG_ppMet,mmgSol, &
       MMG5_ARG_end)

  !> 2) Step 2: Set the mesh size by giving the number of vertices and
  ! the number of hexa of the hexahedral mesh
  CALL H2T_Set_meshSize(mmgMesh, nbVertices, nbHex, 0, 0, ier)
  IF ( ier==0 ) CALL EXIT(1);

  !> 3) Step 3: Give the mesh vertices to Hex2tet (hexahedra vertices)
  imax = Width
  jmax = Height
  kmax = Depth

  ref = 0

  DO k=0,kmax-1
     DO j=0,jmax-1
        DO i=0,imax-1
           ! Vertices insertion
           pointNumber = (k*jmax*imax) + (j*imax) + i + 1
           v(1) = i
           v(2) = j
           v(3) = k

           CALL H2T_Set_vertex(mmgMesh, v(1), v(2), v(3), ref, pointNumber, ier)
           IF ( ier==0 ) CALL EXIT(2)

        ENDDO
     ENDDO
  ENDDO


  !> 4) Step 4: Fill the array of the hexa connectivity (hexTab) such as:
  ! hexTab [ 9*k + i] is the vertex number i of the k^th hexahedra
  ! (for k from 1 to nbHex and i from 0 to 7) and  hexTab [ 9*k + 8] is the
  ! reference of the k^th hexa */
  ALLOCATE ( hexTab ( 9*(nbHex+1) ) )

  DO k=0,kmax-2
     DO j=0,jmax-2
        DO i=0,imax-2

           hexNumber      = (k*(jmax-1)*(imax-1)) + (j*(imax-1)) + i + 1
           hexTabPosition = 9 * hexNumber

           ! Hexahedra definition and storage
           ! f = front b = back || u = up vs. d = down || l = left vs. r = right
           fdl = (k * imax * jmax) + (j * imax) + i + 1
           fdr = fdl + 1
           bdl = ((k+1) * imax * jmax) + (j * imax) + i + 1
           bdr = bdl + 1
           ful = (k * imax * jmax) + ((j+1) * imax) + i + 1
           fur = ful + 1
           bul = ((k+1) * imax * jmax) + ((j+1) * imax) + i + 1
           bur = bul + 1

           hexTab(hexTabPosition)   = fdl
           hexTab(hexTabPosition+1) = fdr
           hexTab(hexTabPosition+2) = bdr
           hexTab(hexTabPosition+3) = bdl
           hexTab(hexTabPosition+4) = ful
           hexTab(hexTabPosition+5) = fur
           hexTab(hexTabPosition+6) = bur
           hexTab(hexTabPosition+7) = bul
           hexTab(hexTabPosition+8) = ref

        ENDDO
     ENDDO
  ENDDO

  !> 5) Step 5: converts hexa into a MMG5 tetrahedral mesh */
  CALL H2T_libhex2tet(mmgMesh,hexTab(0:nbHex-1),nbHex,ierlib)

  IF ( ier /= H2T_STRONGFAILURE ) THEN
     CALL MMG3D_saveMesh(mmgMesh,TRIM(fileout),LEN(TRIM(fileout)),ier);
     IF ( ier /= 1 ) CALL Exit(3)
  ELSE
     PRINT*, "Hex2tet Fail: unable to save mesh."
  ENDIF

  DEALLOCATE(hexTab)

  CALL MMG3D_Free_all(MMG5_ARG_start, &
       MMG5_ARG_ppMesh,mmgMesh,MMG5_ARG_ppMet,mmgSol, &
       MMG5_ARG_end)

  IF ( ierlib /= H2T_SUCCESS ) STOP ierlib

END PROGRAM
