module random_normal
contains
FUNCTION rnorm() RESULT( fn_val )

!   Generate a random normal deviate using the polar method.
!   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
!              normal variables', Siam Rev., vol.6, 260-264, 1964.

IMPLICIT NONE
REAL  :: fn_val

! Local variables

REAL            :: u, sum
REAL, SAVE      :: v, sln
LOGICAL, SAVE   :: second = .FALSE.
REAL, PARAMETER :: one = 1.0, vsmall = TINY( one )

IF (second) THEN
! If second, use the second random number generated on last call

  second = .false.
  fn_val = v*sln

ELSE
! First call; generate a pair of random normals

  second = .true.
  DO
    CALL RANDOM_NUMBER( u )
    CALL RANDOM_NUMBER( v )
    u = SCALE( u, 1 ) - one
    v = SCALE( v, 1 ) - one
    sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(zero) / zero
    IF(sum < one) EXIT
  END DO
  sln = SQRT(- SCALE( LOG(sum), 1 ) / sum)
  fn_val = u*sln
END IF

RETURN
END FUNCTION rnorm

subroutine iran(L,N,ran_int)
! generate an array of N random integers between 0 and L-1.
 real :: rnd
 integer :: L,i,N
 integer :: ran_int(N)
 do i = 1,N
    call random_number(rnd)
    ran_int(i) = nint(float(L-1)*rnd)
 end do
end subroutine iran

subroutine set_random_seed ( iseed , myrank)
!
!*******************************************************************************
!
!! SET_RANDOM_SEED initializes the FORTRAN 90 random number generator.
!
!
!  Discussion:
!
!    If ISEED is nonzero, then that value is used to construct a seed.
!
!    If ISEED is zero, then the seed is determined by calling the date 
!    and time routine.  Thus, if the code is run at different times, 
!    different seed values will be set.
!
!  Parameters:
!
!    Input, integer ISEED, is nonzero for a user seed, or 0 if the
!    seed should be determined by this routine.
!
  implicit none
!
  integer date_time(8)
  logical, parameter :: debug = .false.
  integer i
  integer iseed
  integer j
  integer k
  integer, intent(in), optional :: myrank
  integer, allocatable :: seed(:)

!
!  Initialize the random seed routine.
!
  call random_seed
!
!  Request the size of a typical seed.
!  (On the DEC ALPHA, K is returned as 2.)
!
  call random_seed ( size = k )

  if ( debug ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_RANDOM_SEED:'
    write ( *, '(a,i6)' ) '  Random seed size is K = ', k
  end if
!
!  Set up space for a seed vector.
!
  allocate ( seed(k) )

  if ( iseed /= 0 ) then

    seed(1:k) = iseed

  else
!
!  Make up a "random" value based on date and time information.
!
    call date_and_time ( values = date_time )

    do i = 1, k

      seed(i) = 0

      do j = 1, 8
        if (present(myrank)) then
        seed(i) = seed(i) + ( j + i ) * date_time(j) + myrank * 100
        else
        seed(i) = seed(i) + ( j + i ) * date_time(j)
        endif
        seed(i) = ishftc ( seed(i), 4 * ( j - 1 ) )
      end do

    end do

  end if

  if  ( debug ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_RANDOM_SEED:'
    write ( *, '(a)' ) '  The random seed vector:'
    write ( *, '(a)' ) ' '

    do i = 1, k
      write ( *, '(i12)' ) seed(i)
    end do

  end if
!
!  Send this random value back to the RANDOM_SEED routine, to be
!  used as the seed of the random number generator.
!
  call random_seed ( put = seed(1:k) )

  deallocate ( seed )

  end subroutine set_random_seed
end module random_normal
