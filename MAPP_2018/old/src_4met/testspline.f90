PROGRAM testspline

  USE tension_mod

  INTEGER, PARAMETER :: nwaves=5

  DOUBLE PRECISION, DIMENSION(nwaves) :: x,y,yp,sigma
  DOUBLE PRECISION :: xi=550.,yi,aod_550

  INTEGER :: ier,siger

  x=[413.,496.,670.,868.,1624.]
  y=[0.752,0.605,0.351,0.200,0.050]

  CALL TSPSI (nwaves,x,y,yp,sigma,ier,siger)  

  aod_550=HVAL(xi,nwaves,x,y,yp,sigma,ier)

  PRINT *,aod_550

END PROGRAM testspline
