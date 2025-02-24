MODULE module_rednoise

!think about rescaling variance of the random variable
  
  IMPLICIT NONE

  REAL, PARAMETER :: alpha_mean=1,time_corr_scale=3600.,&
       &alpha_min=0.6,alpha_max=1.4,scale=.075

  INTEGER, PARAMETER :: random_unit=151

  INTEGER :: seq_length

  REAL, ALLOCATABLE, DIMENSION(:) :: random_sequence

  REAL, SAVE :: alpha
  
  REAL :: corr 

  CHARACTER(len=100) :: fname='random_sequence.txt'

CONTAINS
  
  SUBROUTINE init_alpha(dt)

    REAL, INTENT(in) :: dt

    INTEGER :: i

    corr=EXP(-dt/time_corr_scale)

    OPEN(unit=random_unit,file=fname,form='formatted')
    
    READ(random_unit,*)seq_length
    ALLOCATE(random_sequence(seq_length))

    DO i=1,seq_length
       READ(random_unit,*)random_sequence(i)
    ENDDO

    CLOSE(random_unit)

!recenter and rescale random_sequence

    random_sequence=scale*(random_sequence-0.5)/&
         &(alpha_max-alpha_min)

    alpha=1.

  END SUBROUTINE init_alpha

  SUBROUTINE gen_alpha
    
    INTEGER :: tcounter
    
    DATA tcounter /0/

    tcounter=tcounter+1

    IF (tcounter > seq_length) tcounter=0

    alpha=MIN(MAX(alpha_min,&
         &alpha_mean+corr*(alpha-alpha_mean)+&
         &random_sequence(tcounter)),alpha_max)

  END SUBROUTINE gen_alpha
  
  SUBROUTINE finish_alpha

    DEALLOCATE(random_sequence)

  END SUBROUTINE finish_alpha

END MODULE module_rednoise

