PROGRAM test_spline

  USE module_interpolations

  IMPLICIT NONE

  INTEGER :: n,m,ier,siger

  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: x,y,yp,sigma
  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: xi,yi
  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: b,c,d

  INTEGER :: i

  n=6
  m=15


  ALLOCATE(x(n),y(n),yp(n),b(n),c(n),d(n))
  ALLOCATE(xi(m),yi(m))

  x=(/1.,3.,7.,8., 10.,15./)
  y=(/0.,2.,3.,4.5,-1.5,0./)

  DO i=1,m
     xi(i)=i
  ENDDO

  CALL spline(n,x,y,b,c,d)

  DO i=1,m
     yi(i)=seval (n,xi(i),x,y,b,c,d)
  ENDDO

  DO i=1,n
     WRITE(83,*)x(i),y(i)
  ENDDO

  DO i=1,m
     WRITE(84,*)xi(i),yi(i)
  ENDDO

END PROGRAM test_spline

