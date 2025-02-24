PROGRAM test_tspack

  USE TENSION_MOD

  IMPLICIT NONE

  INTEGER :: n,m,ier,siger

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: x,y,yp,sigma
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: xi,yi

  INTEGER :: i

  n=6
  m=15


  ALLOCATE(x(n),y(n),yp(n),sigma(n))
  ALLOCATE(xi(m),yi(m))

  x=(/1.,3.,7.,8., 10.,15./)
  y=(/0.,2.,3.,4.5,-1.5,0./)

  DO i=1,m
     xi(i)=i
  ENDDO

  CALL TSPSI (n,x,y,yp,sigma,ier,siger)

  IF (ier /= 0) THEN
     PRINT *,ier
     STOP
  ENDIF

  IF (siger /= 0) THEN
     PRINT *,siger
     STOP
  ENDIF

  DO i=1,m
     yi(i)=HVAL (xi(i),n,x,y,yp,sigma,ier)
  ENDDO

  DO i=1,n
     WRITE(81,*)x(i),y(i)
  ENDDO

  DO i=1,m
     WRITE(82,*)xi(i),yi(i)
  ENDDO

END PROGRAM test_tspack
