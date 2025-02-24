PROGRAM test_slint

!to test slint working

  USE slint, ONLY: slint_init,bilinear_interp,nn_interp

  IMPLICIT NONE

  INTEGER, PARAMETER :: nx=5,ny=5,nin=nx*ny, nout=3
  REAL, PARAMETER  :: pi=ATAN(1.0)*4.

  REAL, DIMENSION(nin,2) :: latlonin 
  REAL, DIMENSION(nin) :: topoin

  REAL, DIMENSION(nout,2) :: latlonout 
  REAL, DIMENSION(nout) :: topoout

  INTEGER :: i,j,ij

  ij=0
  DO i=1,nx
     DO j=1,ny
        ij=ij+1
        latlonin(ij,1)=REAL(i-1)**1.25*5./180.*pi
        latlonin(ij,2)=REAL(j-1)**1.15*5./180.*pi
        topoin(ij)=(-1)**ij*5.*REAL(ij)**1.5
     ENDDO
  ENDDO

  latlonout(1,1)=2.*pi/180.
  latlonout(1,2)=2.*pi/180.

  latlonout(2,1)=7.*pi/180.
  latlonout(2,2)=8.*pi/180.
  
  latlonout(3,1)=15.*pi/180.
  latlonout(3,2)=19.*pi/180.

  CALL slint_init(latlonin,nin,latlonout,nout)

  CALL bilinear_interp(topoin,topoout)

  DO i=1,nin
     PRINT *,latlonin(i,1),latlonin(i,2),topoin(i)
  ENDDO

  PRINT *,'***'

  DO i=1,nout
     PRINT *,latlonout(i,1),latlonout(i,2),topoout(i)
  ENDDO

  PRINT *,'***'

  CALL nn_interp(topoin,topoout)

  DO i=1,nout
     PRINT *,latlonout(i,1),latlonout(i,2),topoout(i)
  ENDDO

END PROGRAM test_slint
