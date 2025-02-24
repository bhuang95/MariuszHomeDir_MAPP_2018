!--------------------------------------------------------i----
! This file contains the routines needed to perform linear
! interpolation on sphere.
!
!
! Ning Wang, Jan 2007, init version
! modified by Jeff Whitaker  Nov 2009
!
!-------------------------------------------------------------

SUBROUTINE bilinear_init(src_llpoints, nsrc, tgt_llpoints, ntgt)
    IMPLICIT NONE
    INTEGER, intent(in) :: nsrc,ntgt
    REAL, intent(in) :: src_llpoints(nsrc,2),tgt_llpoints(ntgt,2)
    
    CALL init_intern(src_llpoints,nsrc,tgt_llpoints,ntgt,0) ! 0 means bilinear

END SUBROUTINE bilinear_init

SUBROUTINE bilinear_update(tgt_llpoints, ntgt)

    IMPLICIT NONE
    INTEGER, intent(in) :: ntgt
    REAL, intent(in) :: tgt_llpoints(ntgt,2)
    
    CALL update_intern(tgt_llpoints,ntgt,0) ! 0 means bilinear
 
END SUBROUTINE bilinear_update

SUBROUTINE nn_init(src_llpoints, nsrc, tgt_llpoints, ntgt)
    IMPLICIT NONE
    INTEGER, intent(in) :: nsrc,ntgt
    REAL, intent(in) :: src_llpoints(nsrc,2),tgt_llpoints(ntgt,2)
    
    CALL init_intern(src_llpoints,nsrc,tgt_llpoints,ntgt,1) ! 1 means nn

END SUBROUTINE nn_init

SUBROUTINE nn_update(tgt_llpoints, ntgt)

    IMPLICIT NONE
    INTEGER, intent(in) :: ntgt
    REAL, intent(in) :: tgt_llpoints(ntgt,2)
    
    CALL update_intern(tgt_llpoints,ntgt,1) ! 0 means nn
 
END SUBROUTINE nn_update

SUBROUTINE init_intern(llpoints1, n1, llpoints2, n2, nn_weight)
    USE slintdatastru
    use module_kd,only: init_kd_tree
    IMPLICIT NONE
   
    integer, intent(in) :: n1,n2,nn_weight
    REAL, intent(in) :: llpoints1(n1,2),llpoints2(n2,2)
    integer i

    CALL init_kd_tree(llpoints1, n1, 3)

    src_grid%ngp = n1
    ALLOCATE(src_grid%latlon(2, n1))
    ALLOCATE(src_grid%data(n1))

    DO i = 1, n1
      src_grid%latlon(1,i) = llpoints1(i, 1) 
      src_grid%latlon(2,i) = llpoints1(i, 2) 
    END DO

    tgt_grid%ngp = n2

    ALLOCATE(tgt_grid%latlon(2, n2))
    ALLOCATE(tgt_grid%nn(3, n2))
    ALLOCATE(tgt_grid%coeffs(3, n2))
    ALLOCATE(tgt_grid%data(n2))
      
    DO i = 1, n2
      tgt_grid%latlon(1,i) = llpoints2(i, 1) 
      tgt_grid%latlon(2,i) = llpoints2(i, 2) 
    END DO

    CALL coeff_comp(nn_weight)

END SUBROUTINE init_intern

SUBROUTINE update_intern(llpoints2, n2, nn_weight)
    USE slintdatastru
    use module_kd,only: init_kd_tree
    IMPLICIT NONE
   
    integer, intent(in) :: n2,nn_weight
    REAL, intent(in) :: llpoints2(n2,2)
    integer i

    tgt_grid%ngp = n2

    !ALLOCATE(tgt_grid%latlon(2, n2))
    !ALLOCATE(tgt_grid%nn(3, n2))
    !ALLOCATE(tgt_grid%coeffs(3, n2))
    !ALLOCATE(tgt_grid%data(n2))
      
    DO i = 1, n2
      tgt_grid%latlon(1,i) = llpoints2(i, 1) 
      tgt_grid%latlon(2,i) = llpoints2(i, 2) 
    END DO

    CALL coeff_comp(nn_weight)

END SUBROUTINE update_intern


SUBROUTINE interp_intern() 
    USE slintdatastru
    IMPLICIT NONE

    INTEGER i
    REAL*4 v(3), c(3)
   
    DO i = 1, tgt_grid%ngp
      c = tgt_grid%coeffs(1:3,i)
      v(1) = src_grid%data(tgt_grid%nn(1, i))
      v(2) = src_grid%data(tgt_grid%nn(2, i))
      v(3) = src_grid%data(tgt_grid%nn(3, i))
      tgt_grid%data(i) = c(1) * v(1) + c(2) * v(2) + c(3) * v(3) 
    END DO

END SUBROUTINE interp_intern

SUBROUTINE coeff_comp(nn_w)
    USE slintdatastru
    use module_kd,only: knn_search
    IMPLICIT NONE

    INTEGER nn_w

    REAL*8 latlon(2, 3), intsec(2), gcd1, gcd2, part_gcd1, part_gcd2
    INTEGER i, nn(3)
    REAL*8 min_dist, epsilon, t1, t2, theta1, theta2

    epsilon = 0.00000000001

    DO i = 1, tgt_grid%ngp
      CALL knn_search(tgt_grid%latlon(1,i), nn, min_dist)
      latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
      latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
      latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
      CALL intersection (latlon(1,1), tgt_grid%latlon(1,i), latlon(1,2), latlon(1,3), intsec) 

      CALL gcd_ratio(latlon(1,2), latlon(1,3), intsec, gcd1, part_gcd1)
      CALL gcd_ratio(latlon(1,1), intsec, tgt_grid%latlon(1,i), gcd2, part_gcd2)

      tgt_grid%nn(1:3, i) = nn(1:3)
      IF (min_dist < epsilon) THEN 
        tgt_grid%coeffs(1, i) = 1.0  
        tgt_grid%coeffs(2, i) = 0.0  
        tgt_grid%coeffs(3, i) = 0.0  
      ELSE 
        t1 = part_gcd1
        t2 = part_gcd2 
        tgt_grid%coeffs(1, i) = (1.0 - t2)   
        tgt_grid%coeffs(2, i) = t2 * (1.0 - t1)    
        tgt_grid%coeffs(3, i) = t2 * t1 
        IF (nn_w == 1) THEN
          tgt_grid%coeffs(1, i) = 1.0
          tgt_grid%coeffs(2, i) = 0.0 
          tgt_grid%coeffs(3, i) = 0.0 
        ENDIF
      END IF
    END DO

END SUBROUTINE coeff_comp

SUBROUTINE interp (src_data, tgt_data) 
    USE slintdatastru
    IMPLICIT NONE

    REAL src_data(*)
    REAL tgt_data(*)
  
    INTEGER i, n

    n = src_grid%ngp
    DO i = 1, n
      src_grid%data(i) = src_data(i) 
    END DO

    CALL interp_intern()

    n = tgt_grid%ngp
    DO i = 1, n
      tgt_data(i) = tgt_grid%data(i) 
    END DO
END SUBROUTINE interp

! subroutines for spherical curve interpolation 
SUBROUTINE gcd_ratio (p1, p2, p, gcd, p_gcd)
    IMPLICIT NONE

    REAL*8 p1(2), p2(2), p(2), gcd, p_gcd 
    REAL*8 gcdp1p2, gcdp1p, gcdp2p, gc_dist

    gcdp1p2 = gc_dist(p1, p2)
    gcdp1p = gc_dist(p1, p) 
    gcdp2p = gc_dist(p2, p) 
    IF (gcdp2p < gcdp1p2) THEN
      p_gcd = gcdp1p / gcdp1p2
    ELSE
      p_gcd = gcdp1p/ (gcdp1p + gcdp2p) 
    ENDIF 
    gcd = gcdp1p2

END SUBROUTINE gcd_ratio

FUNCTION gc_dist(p1, p2)
    IMPLICIT NONE

    REAL*8 gc_dist,x
    REAL*8 p1(2), p2(2)

    x = (COS(p1(1)) * COS(p2(1)) * COS(p1(2) - p2(2)) + SIN(p1(1)) * SIN(p2(1))) 
    if(abs(x) >= 1.0D0) then
      gc_dist = 0.0D0
    else
      gc_dist = ACOS(x)
    endif

END FUNCTION gc_dist

SUBROUTINE intersection (p1, p2, p3, p4, p)
    IMPLICIT NONE
    REAL*8 p1(2), p2(2), p3(2), p4(2), p(2)
    REAL*8 gc1(3), gc2(3), e1(3), e2(3), e3(3), e4(3), e(3)
    REAL*8 pi, gc_dist

    pi = ATAN(1.0) * 4.0

    CALL cross_product1(p1, p2, gc1)
    CALL cross_product1(p3, p4, gc2)
    CALL cross_product2(gc1, gc2, e)
    IF (gc1(1) == 0.0 .AND. gc1(2) == 0.0 .AND. gc1(3) == 0.0) THEN
      CALL ll2xyz_d(p3, e3)
      CALL ll2xyz_d(p4, e4)
      e = (e3 + e4) / 2.0 
    ENDIF
    CALL xyz2ll(e, p)

!    IF (ABS(p(2) - p1(2)) > pi / 2 .AND. & 
!      ((p(1) < 0 .AND. p1(1) > 0) .OR. (p(1) > 0 .AND. p1(1) < 0))) THEN
!      p(2) = p(2) + pi
!      p(1) = -p(1)
!    END IF

    IF (gc_dist(p2, p) > pi / 4.0) THEN
      p(2) = p(2) + pi
      p(1) = -p(1)
    END IF
 
    IF (p(2) < 0.0) THEN
      p(2) = p(2) + 2.0 * pi
    END IF

END SUBROUTINE intersection


SUBROUTINE cross_product1(p1, p2, gc)
    IMPLICIT NONE

    REAL*8 p1(2), p2(2), gc(3)
    REAL*8 a, b, c, d, e, f, g

    a = SIN(p1(1) + p2(1))
    b = SIN(p1(1) - p2(1))
    c = SIN((p1(2) + p2(2))/ 2.0)
    d = SIN((p1(2) - p2(2))/ 2.0)
    e = COS((p1(2) + p2(2))/ 2.0)
    f = COS((p1(2) - p2(2))/ 2.0)
    g = COS(p1(1)) * COS(p2(1)) 

    gc(1) = b * c * f  - a * e * d
    gc(2) = b * e * f  + a * c * d
    gc(3) = 2.0 * g * d * f

END SUBROUTINE cross_product1

SUBROUTINE cross_product2(e1, e2, e)
    IMPLICIT NONE
    REAL*8 e1(3), e2(3), e(3)
 
    e(1) = e1(2) * e2(3) - e2(2) * e1(3)
    e(2) = e1(3) * e2(1) - e2(3) * e1(1)
    e(3) = e1(1) * e2(2) - e1(2) * e2(1)

END SUBROUTINE cross_product2

SUBROUTINE xyz2ll(e, p)
    IMPLICIT NONE
    REAL*8 e(3), p(2)

    IF (e(1) == 0.0 .AND. e(2) == 0.0) THEN
      PRINT*, 'pole!!'
      p(2) = 0
    ELSE
      p(1) = atan2(e(3), SQRT(e(1) * e(1) + e(2) * e(2)))
      p(2) = atan2(-e(2), e(1))
    ENDIF

END SUBROUTINE xyz2ll

SUBROUTINE ll2xyz(p, e)
    IMPLICIT NONE
    REAL*8 p(2)
    REAL e(3)

    e(1) = cos(p(1)) * cos(p(2))
    e(2) = cos(p(1)) * sin(p(2))
    e(3) = sin(p(1))

END SUBROUTINE ll2xyz


SUBROUTINE ll2xyz_d(p, e)
    IMPLICIT NONE
    REAL*8 p(2)
    REAL*8 e(3)

    e(1) = cos(p(1)) * cos(p(2))
    e(2) = cos(p(1)) * sin(p(2))
    e(3) = sin(p(1))

END SUBROUTINE ll2xyz_d
