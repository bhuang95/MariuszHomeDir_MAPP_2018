PROGRAM rednoise

  USE module_rednoise

  IMPLICIT NONE

  INTEGER :: i,n=1000

  real :: dt=150.

  CALL init_alpha(dt)

  DO i=1,n
     CALL gen_alpha
     WRITE(500,*)i,alpha
  ENDDO

  CALL finish_alpha

END PROGRAM rednoise
