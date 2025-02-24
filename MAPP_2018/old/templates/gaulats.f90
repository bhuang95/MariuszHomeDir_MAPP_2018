      subroutine gaulats(lats,k)
!-----------------------------------------------------------------------
!
! Calculate latitudes a(k) for the gaussian
! quadrature. The algorithm is described in Davis and Rabinowitz,
! Journal of Research of the NBS, V 56, Jan 1956.
! The zeros of the bessel function j0, which are obtained from bsslzr,
! are used as a first guess for the abscissa.
!
!-----------------------------------------------------------------------
      implicit none
!------------------------------Arguments--------------------------------
!
! Input argument
!
      integer k            ! number of latitudes pole to pole
!
! Output arguments
!
      real lats(k)
!
!---------------------------Local workspace-----------------------------
!
      real*8 sinlat(k)    ! sine of latitudes
      real*8 one          ! 1. in real*8.  Needed by atan
      real*8 eps          ! convergence criterion
      real*8 pi           ! value of pi
      real*8 c            ! constant combination
      real*8 fk           ! real k
      real*8 xz           ! abscissa estimate
      real*8 pkm1         ! |
      real*8 pkm2         ! |-polynomials
      real*8 pkmrk        ! |
      real*8 pk           ! |
      real*8 sp           ! current iteration latitude increment
      real*8 avsp         ! |sp|
      real*8 fn           ! real n
      parameter (one = 1.)
      parameter (eps = 1.D-15)

      integer kk           ! k/2 (number of latitudes in hemisphere)
      integer is           ! latitude index
      integer iter         ! iteration counter
      integer n,l          ! indices
!
!------------------------------Externals--------------------------------
!
      external bsslzr      ! provides zeroes of Bessel function or 
!                          ! estimates thereof
!
!-----------------------------------------------------------------------
!
      pi  = 4.*atan(one)
!
! The value eps, used for convergence tests in the iterations,
! can be changed.  Newton iteration is used to find the abscissas.
!
      c = (1.-(2./pi)**2)*0.25
      fk = k
      kk = k/2
      call bsslzr(sinlat,kk)
      do is=1,kk
        xz = cos(sinlat(is)/sqrt((fk+0.5)**2+c))
!
! This is the first approximation to xz
!
        iter = 0
   10   pkm2 = 1.
        pkm1 = xz
        iter = iter + 1
        if (iter.gt.10) then
!
! Error exit
!
          write(6,*)'GAULATS:Error exit,no convergence in 10 iterations'
          stop
        end if
!
! Computation of the legendre polynomial
!
        do n=2,k
          fn = n
          pk = ((2.*fn-1.)*xz*pkm1-(fn-1.)*pkm2)/fn
          pkm2 = pkm1
          pkm1 = pk
        enddo
        pkm1 = pkm2
        pkmrk = (fk*(pkm1-xz*pk))/(1.-xz**2)
        sp = pk/pkmrk
        xz = xz - sp
        avsp = abs(sp)
        if (avsp.gt.eps) go to 10
        sinlat(is) = xz
      end do
!
      if (k.ne.kk*2) then
!
! For odd k computation of weight at the equator
!
        sinlat(kk+1) = 0.
      end if
!
! Complete the sets of abscissas and weights, using the symmetry.
! Also note truncation from real*16 to real*8
!
      do n=1,kk
        l = k + 1 - n
        lats(n) = asin(sinlat(n))
        lats(l) = asin(-sinlat(n))
      end do

      return
      end

      subroutine bsslzr(bes,n)
!-----------------------------------------------------------------------
!
! Return n zeros (or if n>50, approximate zeros), of the Bessel function
! j0,in the array bes. The first 50 zeros will be given exactly, and the
! remaining zeros are computed by extrapolation,and therefore not exact.
!
! Modified 1/23/97 by Jim Rosinski to use real*16 arithmetic
!
!---------------------------Code history--------------------------------
!
! Original version:  CCM1
! Standardized:      J. Rosinski, June 1992
! Reviewed:          J. Hack, D. Williamson, August 1992
! Reviewed:          J. Hack, D. Williamson, April 1996
!
!-----------------------------------------------------------------------
      implicit none
!------------------------------Arguments--------------------------------
!
! Input arguments
!
      integer n              ! Number of zeros to return
!
! Output arguments
!
      real*8 bes(n)
!
!---------------------------Local workspace-----------------------------
!
      real*8 one
      real*8 pi
      real*8 bz(50)
      save bz                ! ensure re-entrancy
      parameter (one = 1.)
      integer j,nn           ! loop indices
!
      data bz           / 2.4048255577,   5.5200781103,&
      8.6537279129,  11.7915344391,  14.9309177086,  18.0710639679,&
     21.2116366299,  24.3524715308,  27.4934791320,  30.6346064684,&
     33.7758202136,  36.9170983537,  40.0584257646,  43.1997917132,&
     46.3411883717,  49.4826098974,  52.6240518411,  55.7655107550,&
     58.9069839261,  62.0484691902,  65.1899648002,  68.3314693299,&
     71.4729816036,  74.6145006437,  77.7560256304,  80.8975558711,&
     84.0390907769,  87.1806298436,  90.3221726372,  93.4637187819,&
     96.6052679510,  99.7468198587, 102.8883742542, 106.0299309165,&
    109.1714896498, 112.3130502805, 115.4546126537, 118.5961766309,&
    121.7377420880, 124.8793089132, 128.0208770059, 131.1624462752,&
    134.3040166383, 137.4455880203, 140.5871603528, 143.7287335737,&
    146.8703076258, 150.0118824570, 153.1534580192, 156.2950342685/
!
      pi = 4.*atan(one)
      nn = n
      if (n.gt.50) then
        bes(50) = bz(50)
        do j=51,n
          bes(j) = bes(j-1) + pi
        end do
        nn = 49
      end if
      do j=1,nn
        bes(j) = bz(j)
      end do
      return
      end
