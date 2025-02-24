PROGRAM test_akima

!to test akima working

  IMPLICIT NONE

  INTEGER, PARAMETER :: nx=5,ny=5,nin=nx*ny, nout=3
  REAL, PARAMETER  :: pi=ATAN(1.0)*4.

  REAL, DIMENSION(nin,2) :: latlonin 
  REAL, DIMENSION(nin) :: topoin

  REAL, DIMENSION(nout,2) :: latlonout 
  REAL, DIMENSION(nout) :: topoout

  INTEGER ( kind = 4 ), parameter :: dncp = 4
  INTEGER ( kind = 4 ), parameter :: dnin = nin
  INTEGER ( kind = 4 ), parameter :: dnout = nout
  
  REAL ( kind = 8 ) error
  INTEGER ( kind = 4 ) iwk(MAX(31,27+dncp)*dnin+dnout)
  INTEGER ( kind = 4 ) md
  REAL ( kind = 8 ) temp
  REAL ( kind = 8 ) wk(8*dnin)

  REAL ( kind = 8 ) :: dlatlonin(dnin,2),dtopoin(dnin),dlatlonout(dnout,2),dtopoout(dnout)

  INTEGER :: i,j,ij

  ij=0
  DO i=1,nx
     DO j=1,ny
        ij=ij+1
        latlonin(ij,1)=REAL(i-1)**1.25*2.5/180.*pi
        latlonin(ij,2)=REAL(j-1)**1.15*2.5/180.*pi
        topoin(ij)=5.*REAL(ij)**1.5
     ENDDO
  ENDDO


  latlonout(1,1)=2.*pi/180.
  latlonout(1,2)=8.*pi/180.

  latlonout(2,1)=7.*pi/180.
  latlonout(2,2)=8.*pi/180.
  
  latlonout(3,1)=5.*pi/180.
  latlonout(3,2)=9.*pi/180.

  md = 1

  dlatlonin=latlonin
  dlatlonout=latlonout
  dtopoin=topoin


  CALL idbvip ( md, dncp, dnin, dlatlonin(:,1), dlatlonin(:,2), dtopoin, &
       &dnout,dlatlonout(:,1),dlatlonout(:,2),dtopoout,iwk, wk )

  DO i=1,nin
     PRINT *,latlonin(i,1),latlonin(i,2),topoin(i)
  ENDDO

  PRINT *,'***'

  topoout=dtopoout

  DO i=1,nout
     PRINT *,latlonout(i,1),latlonout(i,2),topoout(i)
  ENDDO

  PRINT *,'***'

END PROGRAM test_akima
