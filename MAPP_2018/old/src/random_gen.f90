PROGRAM random_gen

!to test random generator

  IMPLICIT NONE

  REAL,dimension(10) :: x

  INTEGER, ALLOCATABLE :: seed_array(:)
  INTEGER :: seed_size,seed_value
  CHARACTER(LEN=10) :: date, time
  INTEGER, DIMENSION(8) :: dates

  INTEGER :: i

  CALL RANDOM_SEED(SIZE = seed_size)

  ALLOCATE(seed_array(seed_size))

  DO i=1,10
     CALL DATE_AND_TIME(date,time,VALUES=dates)
     seed_value=dates(1)+dates(2)+dates(3)+dates(5)+&
          dates(6)+dates(7)+dates(8)

     CALL SYSTEM_CLOCK(seed_value)

!     CALL extra(seed_value)
     PRINT *,seed_value
     seed_array=(/seed_value,seed_value+10/)
     CALL RANDOM_SEED(put=seed_array(1:seed_size))
!     CALL RANDOM_SEED()
     CALL RANDOM_NUMBER(x)
    PRINT *,x
  ENDDO

END PROGRAM random_gen

SUBROUTINE extra(val)

  IMPLICIT NONE

  INTEGER :: val

  INTEGER, SAVE :: seed_value
  DATA seed_value /0/

  seed_value=seed_value+1
  val=seed_value

END SUBROUTINE extra

