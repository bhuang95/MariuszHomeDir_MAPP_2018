MODULE module_function_fitting

  IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------------------
  SUBROUTINE gauss_scale_length( sl, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to Gaussian
!          correlation function
!
! Note   : cov(r) = cov(0) * exp(-r**2 / 8*sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 17/05/05 Dale Barker
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.
    REAL(kind=8)           :: coeff1, coeff2          ! Curve-fitting coefficients.
    REAL(kind=8)           :: ml, sl                  ! Gradient, scalelength.

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit Gaussian curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '
    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(0), cov(n), yr(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and negative logs.

          IF ( cov(d2) / cov(0) < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = SQRT( 8.0 * LOG(cov(0) / cov(d2)) )
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(n), cov(d2), yr(n)
       END IF
    END DO
    nmax = n
    IF (nmax > 0) THEN

!  Now perform curve-fitting when more than 2 points available:

!-----Steps of fitting Gaussian Distribution:

!     B(r) = B(0) exp(-d**2/(8*s**2)      (1)

!     Log at both side of (1):

!        ln[B(0)/B(d)] = d**2/(8*s**2)   (2)

!        {8*ln[B(0)/B(d)]}**0.5 = d/s = m * d

!     Let:

!        y(d) = {8*ln[B(0)/B(d)]}**0.5

!        m = sum[d * y(d)]/sum[d*d]

       coeff1 = 0.0
       coeff2 = 0.0

       DO n = 1, nmax
!     WRITE(UNIT=6,FMT='("n, nrr, d, yr:",i3,3e15.6)') n, nrr(n), d(n), yr(n)
          coeff1 = coeff1 + nrr(n) * d(n) * yr(n)
          coeff2 = coeff2 + nrr(n) * d(n) * d(n)
       END DO

       IF (coeff2 > 0.0) THEN
          ml = coeff1 / coeff2
          sl = 1.0 / ml
       ELSE
!        When no fitting could be completed, set the missing value = 0.0 (YRG 06/30/2005):
          ml = 0.0
          sl = 0.0
       ENDIF

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0
       ml = 0.0

    END IF


  END SUBROUTINE gauss_scale_length
!------------------------------------------------------------------------------------------
  SUBROUTINE soar_scale_length( sl, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a SOAR
!          correlation function
!
! Note   : cov(r) = cov(0) (1 + r/sl) * exp(-r/ sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.
    REAL(kind=8), INTENT(inout):: sl                  ! Scalelength.

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.

    INTEGER                :: ilist, ierr
    REAL                   :: minv

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit SOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and overshoot in covariances.
          IF ( cov(d2) / cov(0)  < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = cov(d2)
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       END IF

    END DO
    nmax = n
    IF (nmax > 0) THEN

!     Now perform curve-fitting on the selected data:
       ilist=1
       minv=0.01
       ierr=0
       IF ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
       CALL soarfit(d,yr,nrr,nn,nmax,cov(0),sl,minv,ilist,ierr)

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0

    END IF

  END SUBROUTINE soar_scale_length

!------------------------------------------------------------------------------


  SUBROUTINE soarfit(x,y,w,n,nmax,a,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER,            INTENT(in)  :: n
    INTEGER,            INTENT(in)  :: nmax
    INTEGER,            INTENT(in)  :: ilist
    INTEGER,            INTENT(inout)  :: ierr

    REAL(kind=8),       INTENT(in) :: x(0:n)
    REAL(kind=8),       INTENT(in) :: y(0:n)
    REAL(kind=8),       INTENT(in) :: w(0:n)
    REAL,               INTENT(in) :: minv
    REAL(kind=8),       INTENT(inout) :: param

    REAL,               INTENT(in) :: a

    INTEGER, PARAMETER    :: icon = 100   ! max iteration
    INTEGER, PARAMETER    :: lbis = 10    ! max bisection

    REAL(kind=8)      :: d(0:n)
    REAL(kind=8)      :: g(0:n)
    REAL(kind=8)      :: m
    REAL(kind=8)      :: gtd
    REAL(kind=8)      :: gtg
    REAL(kind=8)      :: gtginv
    REAL(kind=8)      :: parold
    REAL(kind=8)      :: ssqold, ssq, rootmsq

    INTEGER           :: iter,nbis,i                     ! Loop counters.

! Initialize variables
    ierr   = 0          ! Error flags
    iter   = 0          ! Iteration counter
    nbis   = 0          ! Bisection counter
    m      = 9.9E9      ! dummy value to initialize m
    ssqold = 9.9E9      ! dummy value to initialize ssqold

!    PRINT*
!    PRINT*,' SOAR function fitting using an iterative method'
!    PRINT*

    DO WHILE ( (ABS(m).GT.minv) .AND. (iter.LE.icon) .AND. (nbis.LE.lbis) )

! Form vector D; i.e. a data point minus the isolated Taylor Series
! term for that data point. In addition calculate the sum of the
! square of the residuals

       ssq = 0.d0
       DO i = 0, nmax
          d(i)=y(i)-f0(x(i),a,param)   ! f0 = SOAR function
          ssq = ssq + d(i)**2 *w(i)
       END DO
!rizvi      rootmsq=dsqrt(ssq)
       rootmsq=SQRT(ssq)

       IF (ssq.LT.ssqold) THEN         ! Convergence

! The matrix formation in the following program segment follows
! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
! Inverse Theory by Menke (1984)

! Form matrix G
          DO i = 0, nmax
             g(i)=f1(x(i),a,param)     ! f1 = SOAR function derivative wrt to param
          END DO

! Form matrix GTD
          gtd=0.0
          DO i = 0, nmax
             gtd=gtd+d(i)*g(i)*w(i)
          END DO

! Form matrix GTG
          gtg=0.0
          DO i = 0, nmax
             gtg=gtg+g(i)*g(i)*w(i)
          END DO

! Find the inverse of matrix GTG
          gtginv=1.0/gtg

! Find vector M (i.e. increment of PARAM)
          m=gtginv*gtd

          ssqold=ssq          ! keep

          iter=iter+1

       ELSE                           ! divergence

          m = m/2.0           ! Bisect the correction
          nbis = nbis + 1

       ENDIF

! Update the parameter
       param=param+m

!       IF (ilist.EQ.1) PRINT 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20     FORMAT(2X,A2,I6,2X,A2,I3,2X,A2,G11.4,2X,A5,G11.4,2X,A2,G11.4)

    END DO

!    PRINT*
!    PRINT*," SOAR estimated lenghtscale = ",param
!    PRINT*

  END SUBROUTINE soarfit
!------------------------------------------------------------------------------------------
  FUNCTION f0(x,a,param)  ! calculate SOAR function
    INTEGER       :: l
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f0
    f0 = a * (1.0 + x/param) * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f0
!------------------------------------------------------------------------------------------
  FUNCTION f1(x,a,param)  ! calculate SOAR function derivative wrt param
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f1
    f1 = (a * x**2 / (param)**3) * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f1

!------------------------------------------------------------------------------------------------
! A fast self-contained routine to find the median

  SUBROUTINE median(x, n, xmed)

! Find the median of X(1), ... , X(N), using as much of the quicksort
! algorithm as is needed to isolate it.
! N.B. On exit, the array X is partially ordered.

!     Latest revision - 26 November 1996
    IMPLICIT NONE

    INTEGER, INTENT(IN)                :: n
    REAL, INTENT(IN OUT), DIMENSION(:) :: x
    REAL, INTENT(OUT)                  :: xmed

! Local variables

    REAL    :: temp, xhi, xlo, xmax, xmin
    LOGICAL :: odd
    INTEGER :: hi, lo, nby2, nby2p1, mid, i, j, k

    nby2 = n / 2
    nby2p1 = nby2 + 1
    odd = .TRUE.

!     HI & LO are position limits encompassing the median.

    IF (n == 2 * nby2) odd = .FALSE.
    lo = 1
    hi = n
    IF (n < 3) THEN
       IF (n < 1) THEN
          xmed = 0.0
          RETURN
       END IF
       xmed = x(1)
       IF (n == 1) RETURN
       xmed = 0.5*(xmed + x(2))
       RETURN
    END IF

!     Find median of 1st, middle & last values.

10  mid = (lo + hi)/2
    xmed = x(mid)
    xlo = x(lo)
    xhi = x(hi)
    IF (xhi < xlo) THEN          ! Swap xhi & xlo
       temp = xhi
       xhi = xlo
       xlo = temp
    END IF
    IF (xmed > xhi) THEN
       xmed = xhi
    ELSE IF (xmed < xlo) THEN
       xmed = xlo
    END IF

! The basic quicksort algorithm to move all values <= the sort key (XMED)
! to the left-hand end, and all higher values to the other end.

    i = lo
    j = hi
50  DO
       IF (x(i) >= xmed) EXIT
       i = i + 1
    END DO
    DO
       IF (x(j) <= xmed) EXIT
       j = j - 1
    END DO
    IF (i < j) THEN
       temp = x(i)
       x(i) = x(j)
       x(j) = temp
       i = i + 1
       j = j - 1

!     Decide which half the median is in.

       IF (i <= j) GO TO 50
    END IF

    IF (.NOT. odd) THEN
       IF (j == nby2 .AND. i == nby2p1) GO TO 130
       IF (j < nby2) lo = i
       IF (i > nby2p1) hi = j
       IF (i /= j) GO TO 100
       IF (i == nby2) lo = nby2
       IF (j == nby2p1) hi = nby2p1
    ELSE
       IF (j < nby2p1) lo = i
       IF (i > nby2p1) hi = j
       IF (i /= j) GO TO 100

! Test whether median has been isolated.

       IF (i == nby2p1) RETURN
    END IF
100 IF (lo < hi - 1) GO TO 10

    IF (.NOT. odd) THEN
       xmed = 0.5*(x(nby2) + x(nby2p1))
       RETURN
    END IF
    temp = x(lo)
    IF (temp > x(hi)) THEN
       x(lo) = x(hi)
       x(hi) = temp
    END IF
    xmed = x(nby2p1)
    RETURN

! Special case, N even, J = N/2 & I = J + 1, so the median is
! between the two halves of the series.   Find max. of the first
! half & min. of the second half, then average.

130 xmax = x(1)
    DO k = lo, j
       xmax = MAX(xmax, x(k))
    END DO
    xmin = x(n)
    DO k = i, hi
       xmin = MIN(xmin, x(k))
    END DO
    xmed = 0.5*(xmin + xmax)

    RETURN
  END SUBROUTINE median

  SUBROUTINE foar_scale_length( sl, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a FOAR
!          correlation function
!
! Note   : cov(r) = cov(0)  * exp(-r/ sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
! modified form soar by mzp
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.
    REAL(kind=8), INTENT(inout):: sl                  ! Scalelength.

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.

    INTEGER                :: ilist, ierr
    REAL                   :: minv

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit FOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and overshoot in covariances.
          IF ( cov(d2) / cov(0)  < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = cov(d2)
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       END IF

    END DO
    nmax = n
    IF (nmax > 0) THEN

!     Now perform curve-fitting on the selected data:
       ilist=1
       minv=0.01
       ierr=0
       IF ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
       CALL foarfit(d,yr,nrr,nn,nmax,cov(0),sl,minv,ilist,ierr)

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0

    END IF

  END SUBROUTINE foar_scale_length

!------------------------------------------------------------------------------


  SUBROUTINE foarfit(x,y,w,n,nmax,a,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!modified for foar by mzp
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER,            INTENT(in)  :: n
    INTEGER,            INTENT(in)  :: nmax
    INTEGER,            INTENT(in)  :: ilist
    INTEGER,            INTENT(inout)  :: ierr

    REAL(kind=8),       INTENT(in) :: x(0:n)
    REAL(kind=8),       INTENT(in) :: y(0:n)
    REAL(kind=8),       INTENT(in) :: w(0:n)
    REAL,               INTENT(in) :: minv
    REAL(kind=8),       INTENT(inout) :: param

    REAL,               INTENT(in) :: a

    INTEGER, PARAMETER    :: icon = 100   ! max iteration
    INTEGER, PARAMETER    :: lbis = 10    ! max bisection

    REAL(kind=8)      :: d(0:n)
    REAL(kind=8)      :: g(0:n)
    REAL(kind=8)      :: m
    REAL(kind=8)      :: gtd
    REAL(kind=8)      :: gtg
    REAL(kind=8)      :: gtginv
    REAL(kind=8)      :: parold
    REAL(kind=8)      :: ssqold, ssq, rootmsq

    INTEGER           :: iter,nbis,i                     ! Loop counters.

! Initialize variables
    ierr   = 0          ! Error flags
    iter   = 0          ! Iteration counter
    nbis   = 0          ! Bisection counter
    m      = 9.9E9      ! dummy value to initialize m
    ssqold = 9.9E9      ! dummy value to initialize ssqold

!    PRINT*
!    PRINT*,' FOAR function fitting using an iterative method'
!    PRINT*

    DO WHILE ( (ABS(m).GT.minv) .AND. (iter.LE.icon) .AND. (nbis.LE.lbis) )

! Form vector D; i.e. a data point minus the isolated Taylor Series
! term for that data point. In addition calculate the sum of the
! square of the residuals

       ssq = 0.d0
       DO i = 0, nmax
          d(i)=y(i)-f0_foar(x(i),a,param)   ! f0 = FOAR function
          ssq = ssq + d(i)**2 *w(i)
       END DO
!rizvi      rootmsq=dsqrt(ssq)
       rootmsq=SQRT(ssq)

       IF (ssq.LT.ssqold) THEN         ! Convergence

! The matrix formation in the following program segment follows
! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
! Inverse Theory by Menke (1984)

! Form matrix G
          DO i = 0, nmax
             g(i)=f1_foar(x(i),a,param)     ! f1 = FOAR function derivative wrt to param
          END DO

! Form matrix GTD
          gtd=0.0
          DO i = 0, nmax
             gtd=gtd+d(i)*g(i)*w(i)
          END DO

! Form matrix GTG
          gtg=0.0
          DO i = 0, nmax
             gtg=gtg+g(i)*g(i)*w(i)
          END DO

! Find the inverse of matrix GTG
          gtginv=1.0/gtg

! Find vector M (i.e. increment of PARAM)
          m=gtginv*gtd

          ssqold=ssq          ! keep

          iter=iter+1

       ELSE                           ! divergence

          m = m/2.0           ! Bisect the correction
          nbis = nbis + 1

       ENDIF

! Update the parameter
       param=param+m

!       IF (ilist.EQ.1) PRINT 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20     FORMAT(2X,A2,I6,2X,A2,I3,2X,A2,G11.4,2X,A5,G11.4,2X,A2,G11.4)

    END DO

!    PRINT*
!    PRINT*," FOAR estimated lenghtscale = ",param
!    PRINT*

  END SUBROUTINE foarfit
!------------------------------------------------------------------------------------------
  FUNCTION f0_foar(x,a,param)  ! calculate FOAR function
    INTEGER       :: l
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f0_foar
!    f0 = a * (1.0 + x/param) * EXP(-1.0*x/param)
    f0_foar = a * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f0_foar
!------------------------------------------------------------------------------------------
  FUNCTION f1_foar(x,a,param)  ! calculate FOAR function derivative wrt param
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f1_foar
!    f1 = (a * x**2 / (param)**3) * EXP(-1.0*x/param)
    f1_foar = (a * x / (param)**2) * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f1_foar

!---------------------------------------------------------------------------------------------

  SUBROUTINE sboar_scale_length( sl, nn, nr, cov, b )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a SOAR
!          correlation function
!
! Note   : cov(r) = cov(0) (1 + (r/sl)**b) * exp((-r/ sl)**b)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
! modified for b exponent by mzp

!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.
    REAL(kind=8), INTENT(inout):: sl                  ! Scalelength.
    REAL, INTENT(in)       :: b                       ! exponent 

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.

    INTEGER                :: ilist, ierr
    REAL                   :: minv

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit SOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and overshoot in covariances.
          IF ( cov(d2) / cov(0)  < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = cov(d2)
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       END IF

    END DO
    nmax = n
    IF (nmax > 0) THEN

!     Now perform curve-fitting on the selected data:
       ilist=1
       minv=0.01
       ierr=0
       IF ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
       CALL sboarfit(d,yr,nrr,nn,nmax,cov(0),b,sl,minv,ilist,ierr)

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0

    END IF

  END SUBROUTINE sboar_scale_length

!------------------------------------------------------------------------------


  SUBROUTINE sboarfit(x,y,w,n,nmax,a,b,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER,            INTENT(in)  :: n
    INTEGER,            INTENT(in)  :: nmax
    INTEGER,            INTENT(in)  :: ilist
    INTEGER,            INTENT(inout)  :: ierr

    REAL(kind=8),       INTENT(in) :: x(0:n)
    REAL(kind=8),       INTENT(in) :: y(0:n)
    REAL(kind=8),       INTENT(in) :: w(0:n)
    REAL,               INTENT(in) :: minv
    REAL(kind=8),       INTENT(inout) :: param

    REAL,               INTENT(in) :: a
    REAL,               INTENT(in) :: b

    INTEGER, PARAMETER    :: icon = 100   ! max iteration
    INTEGER, PARAMETER    :: lbis = 10    ! max bisection

    REAL(kind=8)      :: d(0:n)
    REAL(kind=8)      :: g(0:n)
    REAL(kind=8)      :: m
    REAL(kind=8)      :: gtd
    REAL(kind=8)      :: gtg
    REAL(kind=8)      :: gtginv
    REAL(kind=8)      :: parold
    REAL(kind=8)      :: ssqold, ssq, rootmsq

    INTEGER           :: iter,nbis,i                     ! Loop counters.

! Initialize variables
    ierr   = 0          ! Error flags
    iter   = 0          ! Iteration counter
    nbis   = 0          ! Bisection counter
    m      = 9.9E9      ! dummy value to initialize m
    ssqold = 9.9E9      ! dummy value to initialize ssqold

!    PRINT*
!    PRINT*,' SOAR function fitting using an iterative method'
!    PRINT*

    DO WHILE ( (ABS(m).GT.minv) .AND. (iter.LE.icon) .AND. (nbis.LE.lbis) )

! Form vector D; i.e. a data point minus the isolated Taylor Series
! term for that data point. In addition calculate the sum of the
! square of the residuals

       ssq = 0.d0
       DO i = 0, nmax
          d(i)=y(i)-f0_sboar(x(i),a,b,param)   ! f0 = SOAR function
          ssq = ssq + d(i)**2 *w(i)
       END DO
!rizvi      rootmsq=dsqrt(ssq)
       rootmsq=SQRT(ssq)

       IF (ssq.LT.ssqold) THEN         ! Convergence

! The matrix formation in the following program segment follows
! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
! Inverse Theory by Menke (1984)

! Form matrix G
          DO i = 0, nmax
             g(i)=f1_sboar(x(i),a,b,param)     ! f1 = SOAR function derivative wrt to param
          END DO

! Form matrix GTD
          gtd=0.0
          DO i = 0, nmax
             gtd=gtd+d(i)*g(i)*w(i)
          END DO

! Form matrix GTG
          gtg=0.0
          DO i = 0, nmax
             gtg=gtg+g(i)*g(i)*w(i)
          END DO

! Find the inverse of matrix GTG
          gtginv=1.0/gtg

! Find vector M (i.e. increment of PARAM)
          m=gtginv*gtd

          ssqold=ssq          ! keep

          iter=iter+1

       ELSE                           ! divergence

          m = m/2.0           ! Bisect the correction
          nbis = nbis + 1

       ENDIF

! Update the parameter
       param=param+m

!       IF (ilist.EQ.1) PRINT 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20     FORMAT(2X,A2,I6,2X,A2,I3,2X,A2,G11.4,2X,A5,G11.4,2X,A2,G11.4)

    END DO

!    PRINT*
!    PRINT*," SOAR estimated lenghtscale = ",param
!    PRINT*

  END SUBROUTINE sboarfit
!------------------------------------------------------------------------------------------
  FUNCTION f0_sboar(x,a,b,param)  ! calculate SOAR function
    INTEGER       :: l
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL          :: b
    REAL(kind=8)  :: f0_sboar
    f0_sboar = a * (1.0 + (x/param)**b) * EXP(-1.0*(x/param)**b)
    RETURN
  END FUNCTION f0_sboar
!------------------------------------------------------------------------------------------
  FUNCTION f1_sboar(x,a,b,param)  ! calculate SOAR function derivative wrt param
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL          :: b
    REAL(kind=8)  :: f1_sboar
    f1_sboar = (a * x**(2.*b) / (param)**(2.*b+1)) * EXP(-1.0*(x/param)**b)
    RETURN
  END FUNCTION f1_sboar

  SUBROUTINE toar_scale_length( sl, nn, nr, cov )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a SOAR
!          correlation function
!
! Note   : cov(r) = cov(0) (1 + r/sl + r**2/(3.*sl)) * exp(-r/ sl)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.
    REAL(kind=8), INTENT(inout):: sl                  ! Scalelength.

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.

    INTEGER                :: ilist, ierr
    REAL                   :: minv

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit SOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and overshoot in covariances.
          IF ( cov(d2) / cov(0)  < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = cov(d2)
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       END IF

    END DO
    nmax = n
    IF (nmax > 0) THEN

!     Now perform curve-fitting on the selected data:
       ilist=1
       minv=0.01
       ierr=0
       IF ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
       CALL toarfit(d,yr,nrr,nn,nmax,cov(0),sl,minv,ilist,ierr)

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0

    END IF

  END SUBROUTINE toar_scale_length

!------------------------------------------------------------------------------


  SUBROUTINE toarfit(x,y,w,n,nmax,a,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER,            INTENT(in)  :: n
    INTEGER,            INTENT(in)  :: nmax
    INTEGER,            INTENT(in)  :: ilist
    INTEGER,            INTENT(inout)  :: ierr

    REAL(kind=8),       INTENT(in) :: x(0:n)
    REAL(kind=8),       INTENT(in) :: y(0:n)
    REAL(kind=8),       INTENT(in) :: w(0:n)
    REAL,               INTENT(in) :: minv
    REAL(kind=8),       INTENT(inout) :: param

    REAL,               INTENT(in) :: a

    INTEGER, PARAMETER    :: icon = 100   ! max iteration
    INTEGER, PARAMETER    :: lbis = 10    ! max bisection

    REAL(kind=8)      :: d(0:n)
    REAL(kind=8)      :: g(0:n)
    REAL(kind=8)      :: m
    REAL(kind=8)      :: gtd
    REAL(kind=8)      :: gtg
    REAL(kind=8)      :: gtginv
    REAL(kind=8)      :: parold
    REAL(kind=8)      :: ssqold, ssq, rootmsq

    INTEGER           :: iter,nbis,i                     ! Loop counters.

! Initialize variables
    ierr   = 0          ! Error flags
    iter   = 0          ! Iteration counter
    nbis   = 0          ! Bisection counter
    m      = 9.9E9      ! dummy value to initialize m
    ssqold = 9.9E9      ! dummy value to initialize ssqold

!    PRINT*
!    PRINT*,' SOAR function fitting using an iterative method'
!    PRINT*

    DO WHILE ( (ABS(m).GT.minv) .AND. (iter.LE.icon) .AND. (nbis.LE.lbis) )

! Form vector D; i.e. a data point minus the isolated Taylor Series
! term for that data point. In addition calculate the sum of the
! square of the residuals

       ssq = 0.d0
       DO i = 0, nmax
          d(i)=y(i)-f0_toar(x(i),a,param)   ! f0 = SOAR function
          ssq = ssq + d(i)**2 *w(i)
       END DO
!rizvi      rootmsq=dsqrt(ssq)
       rootmsq=SQRT(ssq)

       IF (ssq.LT.ssqold) THEN         ! Convergence

! The matrix formation in the following program segment follows
! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
! Inverse Theory by Menke (1984)

! Form matrix G
          DO i = 0, nmax
             g(i)=f1_toar(x(i),a,param)     ! f1 = SOAR function derivative wrt to param
          END DO

! Form matrix GTD
          gtd=0.0
          DO i = 0, nmax
             gtd=gtd+d(i)*g(i)*w(i)
          END DO

! Form matrix GTG
          gtg=0.0
          DO i = 0, nmax
             gtg=gtg+g(i)*g(i)*w(i)
          END DO

! Find the inverse of matrix GTG
          gtginv=1.0/gtg

! Find vector M (i.e. increment of PARAM)
          m=gtginv*gtd

          ssqold=ssq          ! keep

          iter=iter+1

       ELSE                           ! divergence

          m = m/2.0           ! Bisect the correction
          nbis = nbis + 1

       ENDIF

! Update the parameter
       param=param+m

!       IF (ilist.EQ.1) PRINT 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20     FORMAT(2X,A2,I6,2X,A2,I3,2X,A2,G11.4,2X,A5,G11.4,2X,A2,G11.4)

    END DO

!    PRINT*
!    PRINT*," TOAR estimated lenghtscale = ",param
!    PRINT*

  END SUBROUTINE toarfit
!------------------------------------------------------------------------------------------
  FUNCTION f0_toar(x,a,param)  ! calculate SOAR function
    INTEGER       :: l
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f0_toar
    f0_toar = a * (1.0 + x/param + ((x/param)**2)/3.) * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f0_toar
!------------------------------------------------------------------------------------------
  FUNCTION f1_toar(x,a,param)  ! calculate SOAR function derivative wrt param
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL(kind=8)  :: f1_toar
    f1_toar = a * (x**2 / (param)**3) * ( 1. +x ) * EXP(-1.0*x/param)
    RETURN
  END FUNCTION f1_toar


  SUBROUTINE fboar_scale_length( sl, nn, nr, cov, b )

!----------------------------------------------------------------------
! Purpose: Calculate fit of input covariance data to a FOAR
!          correlation function
!
! Note   : cov(r) = cov(0)  * exp(-(r/ sl)**b)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
! modified form soar by mzp
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(in)    :: nn                      ! Dimension of radii bins.
    INTEGER, INTENT(in )   :: nr(0:nn)                ! Number of points in each bin.
    REAL, INTENT(in)       :: cov(0:nn)               ! Covariance of each bin.
    REAL, INTENT(in)       :: b
    REAL(kind=8), INTENT(inout):: sl                  ! Scalelength.

    REAL(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
    REAL(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
    REAL(kind=8)           :: d(0:nn)                 ! Distance for each bin.

    INTEGER                :: n, d2                   ! Loop counters.
    INTEGER                :: nmax                    ! Number of points available for curve fitting.
    REAL                   :: yr_cutoff               ! Noise cut-off criterion.
    REAL                   :: corr_min                ! Corresponding correlation value.

    INTEGER                :: ilist, ierr
    REAL                   :: minv

    yr(0:nn) = 0.0
    nrr(0:nn) = 0.0
    d(0:nn) = 0.0

    yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
    corr_min = EXP( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)

!    WRITE(UNIT=6,FMT='(a,1pe15.5)') &
!         ' Fit FOAR curve to data for correlations >= ', corr_min

!   write(UNIT=6,FMT='(5a)') &
!      '  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '

    n = 0
    nrr(n) = REAL(nr(n))
!   write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(0), cov(n)

    DO d2 = 1, nn

       IF ( nr(d2) > 0 .AND. cov(d2) < cov(0) ) THEN ! Omit bins with no data and overshoot in covariances.
          IF ( cov(d2) / cov(0)  < corr_min ) EXIT ! Yong-Run's noise cut-off criterion.
          n = n + 1
          yr(n) = cov(d2)
          nrr(n) = REAL(nr(d2))
          d(n) = SQRT(REAL(d2))                  ! Distance
!         write(UNIT=6,FMT='(i6,3e13.5)') n, nrr(n), d(n), yr(n)
       END IF

    END DO
    nmax = n
    IF (nmax > 0) THEN

!     Now perform curve-fitting on the selected data:
       ilist=1
       minv=0.01
       ierr=0
       IF ( sl == 0.0 ) sl = 10.0 ! prevent using 0 value if gaussian fit failed
       CALL fboarfit(d,yr,nrr,nn,nmax,cov(0),b,sl,minv,ilist,ierr)

    ELSE

       WRITE(UNIT=6,FMT='(a)') &
            'All corelation values lower than corr_min. Setting SL to 0'
       sl = 0.0

    END IF

  END SUBROUTINE fboar_scale_length

!------------------------------------------------------------------------------


  SUBROUTINE fboarfit(x,y,w,n,nmax,a,b,param,minv,ilist,ierr)

!----------------------------------------------------------------------
! Purpose: This subroutine compute the lenghtscale (sl, here: param) of a
!          SOAR function that best fit (in a least-square perspective) a
!          data set. The method follow routine CURVEFIT from:
!          Heeswijk, M.V., and C.G. Fox, 1988: Iterative Method and Fortran
!          Code for Nonlinear Curve Fitting, Computers and Geosciences, 14,
!          4, pp. 489-503.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 16/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!modified for foar by mzp
!----------------------------------------------------------------------

    IMPLICIT NONE

    INTEGER,            INTENT(in)  :: n
    INTEGER,            INTENT(in)  :: nmax
    INTEGER,            INTENT(in)  :: ilist
    INTEGER,            INTENT(inout)  :: ierr

    REAL(kind=8),       INTENT(in) :: x(0:n)
    REAL(kind=8),       INTENT(in) :: y(0:n)
    REAL(kind=8),       INTENT(in) :: w(0:n)
    REAL,               INTENT(in) :: minv
    REAL(kind=8),       INTENT(inout) :: param

    REAL,               INTENT(in) :: a
    REAL,               INTENT(in) :: b

    INTEGER, PARAMETER    :: icon = 100   ! max iteration
    INTEGER, PARAMETER    :: lbis = 10    ! max bisection

    REAL(kind=8)      :: d(0:n)
    REAL(kind=8)      :: g(0:n)
    REAL(kind=8)      :: m
    REAL(kind=8)      :: gtd
    REAL(kind=8)      :: gtg
    REAL(kind=8)      :: gtginv
    REAL(kind=8)      :: parold
    REAL(kind=8)      :: ssqold, ssq, rootmsq

    INTEGER           :: iter,nbis,i                     ! Loop counters.

! Initialize variables
    ierr   = 0          ! Error flags
    iter   = 0          ! Iteration counter
    nbis   = 0          ! Bisection counter
    m      = 9.9E9      ! dummy value to initialize m
    ssqold = 9.9E9      ! dummy value to initialize ssqold

!    PRINT*
!    PRINT*,' FOAR function fitting using an iterative method'
!    PRINT*

    DO WHILE ( (ABS(m).GT.minv) .AND. (iter.LE.icon) .AND. (nbis.LE.lbis) )

! Form vector D; i.e. a data point minus the isolated Taylor Series
! term for that data point. In addition calculate the sum of the
! square of the residuals

       ssq = 0.d0
       DO i = 0, nmax
          d(i)=y(i)-f0_fboar(x(i),a,b,param)   ! f0 = FOAR function
          ssq = ssq + d(i)**2 *w(i)
       END DO
!rizvi      rootmsq=dsqrt(ssq)
       rootmsq=SQRT(ssq)

       IF (ssq.LT.ssqold) THEN         ! Convergence

! The matrix formation in the following program segment follows
! equation 3.12 on page 41 of Geophysical Data Analysis: Discrete
! Inverse Theory by Menke (1984)

! Form matrix G
          DO i = 0, nmax
             g(i)=f1_fboar(x(i),a,b,param)     ! f1 = FOAR function derivative wrt to param
          END DO

! Form matrix GTD
          gtd=0.0
          DO i = 0, nmax
             gtd=gtd+d(i)*g(i)*w(i)
          END DO

! Form matrix GTG
          gtg=0.0
          DO i = 0, nmax
             gtg=gtg+g(i)*g(i)*w(i)
          END DO

! Find the inverse of matrix GTG
          gtginv=1.0/gtg

! Find vector M (i.e. increment of PARAM)
          m=gtginv*gtd

          ssqold=ssq          ! keep

          iter=iter+1

       ELSE                           ! divergence

          m = m/2.0           ! Bisect the correction
          nbis = nbis + 1

       ENDIF

! Update the parameter
       param=param+m

!       IF (ilist.EQ.1) PRINT 20,'i=',iter,'n=',nbis,'m=',m,'rmsd=',rootmsq,'s=',param
20     FORMAT(2X,A2,I6,2X,A2,I3,2X,A2,G11.4,2X,A5,G11.4,2X,A2,G11.4)

    END DO

!    PRINT*
!    PRINT*," FOAR estimated lenghtscale = ",param
!    PRINT*

  END SUBROUTINE fboarfit
!------------------------------------------------------------------------------------------
  FUNCTION f0_fboar(x,a,b,param)  ! calculate FOAR function
    INTEGER       :: l
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL          :: b
    REAL(kind=8)  :: f0_fboar
!    f0 = a * (1.0 + x/param) * EXP(-1.0*x/param)
    f0_fboar = a * EXP(-1.0*(x/param)**b)
    RETURN
  END FUNCTION f0_fboar
!------------------------------------------------------------------------------------------
  FUNCTION f1_fboar(x,a,b,param)  ! calculate FOAR function derivative wrt param
    REAL(kind=8)  :: x
    REAL(kind=8)  :: param
    REAL          :: a
    REAL          :: b
    REAL(kind=8)  :: f1_fboar
    f1_fboar = (a * b * (x**b) / (param)**(b+1)) * EXP(-1.0*(x/param)**b)
    RETURN
  END FUNCTION f1_fboar

END MODULE module_function_fitting
