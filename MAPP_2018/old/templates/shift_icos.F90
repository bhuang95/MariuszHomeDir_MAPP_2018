!--------------------------------------------------------i----
! This file contains the routines needed to perform linear
! interpolation on sphere.
!
!
! Ning Wang, Jan 2007, init version
!
!-------------------------------------------------------------

SUBROUTINE bilinear_init_i2r_jsw(mx, my, lons, lats, nip, gaussian, interp_type)
    USE slintdatastru
    use module_kd,only: init_kd_tree
    IMPLICIT NONE
   
    INTEGER, intent(in) :: mx, my, nip
    REAL   , intent(in) :: lons(nip), lats(nip)
    REAL*8 pi
    REAL*8 lat, lon, d2r, r2d
    INTEGER i, j, g_idx, seq
    real, allocatable,dimension(:,:) :: llpoints
    real, allocatable,dimension(:) :: glats(:)
    logical, intent(in) :: gaussian ! if true, gaussian grid, otherwise reg
    integer, intent(in) :: interp_type ! 1 for nearest neighbor, 0 for bilinear


    pi = 4.0*ATAN(1.0)
    d2r = pi/180.0
    r2d = 1 / d2r

    ! src_grid is icos.
     ALLOCATE(llpoints(nip,2))
    llpoints(:,1) = lats; llpoints(:,2) = lons

     ! dying here!

    if (.not.ALLOCATED(tgt_grid%latlon)) CALL init_kd_tree(llpoints, nip, 3)
    src_grid%type = 1
    src_grid%ngp = nip
 
    if (.not.ALLOCATED(src_grid%latlon)) ALLOCATE(src_grid%latlon(2, nip))
 
    if (.not.ALLOCATED(src_grid%data)) ALLOCATE(src_grid%data(nip))
    DO i = 1, nip
      src_grid%latlon(1,i) = llpoints(i, 1) 
      src_grid%latlon(2,i) = llpoints(i, 2) 
    END DO
    DEALLOCATE(llpoints)

    ! tgt grid is regular lat/lon, or gaussian.
    tgt_grid%type = 0
    tgt_grid%mx = mx
    tgt_grid%my = my
   

    if (.not.ALLOCATED(tgt_grid%latlon)) ALLOCATE(tgt_grid%latlon(2, mx * my))
    if (.not.ALLOCATED(tgt_grid%nn)) ALLOCATE(tgt_grid%nn(3, mx * my))
    if (.not.ALLOCATED(tgt_grid%coeffs)) ALLOCATE(tgt_grid%coeffs(3, mx * my))
    if (.not.ALLOCATED(tgt_grid%data)) ALLOCATE(tgt_grid%data(mx * my))

    
 if (gaussian) then ! gaussian
    ALLOCATE(glats(my))
    
    call gaulats(glats,my)
    
    DO i = 1, mx 
      DO j = 1, my
        g_idx = (i + (j - 1) * mx)
        tgt_grid%latlon(1, g_idx) = glats(j)
        tgt_grid%latlon(2, g_idx) = REAL(i - 1) * 2.0 * pi / REAL(mx) 
      END DO
    END DO
    DEALLOCATE(glats)
 else ! reg lat/lon
    DO i = 1, mx 
      DO j = 1, my
        g_idx = (i + (j - 1) * mx)
        ! j=1 ==> 0.5*pi; j=my ==> -0.5*pi
        tgt_grid%latlon(1, g_idx) = (REAL(j - 1) - REAL(my - 1) * 0.5)  * pi / REAL(my - 1) 
        tgt_grid%latlon(1, g_idx) = -tgt_grid%latlon(1, g_idx) 
        tgt_grid%latlon(2, g_idx) = REAL(i - 1) * 2.0 * pi / REAL(mx) 
      END DO
    END DO
 endif
    print*,'calling coeff_comp',interp_type  
    CALL coeff_comp(interp_type) ! 0 for bilinear, 1 for nearest-neighbor
    print*,'done'
END SUBROUTINE bilinear_init_i2r_jsw

SUBROUTINE nn_init(grid_file1, n1, unit2, n2)
    IMPLICIT NONE
    INTEGER :: n1, n2
    CHARACTER *(*),intent(in) :: grid_file1
    integer       ,intent(in) :: unit2
    
    CALL init_intern(grid_file1, n1, unit2, n2, 1)

END SUBROUTINE nn_init

SUBROUTINE bilinear_interp (src_data, tgt_data) 

    REAL src_data(*)
    REAL tgt_data(*)
    CALL interp(src_data, tgt_data)
  
END SUBROUTINE bilinear_interp

SUBROUTINE nn_interp (src_data, tgt_data) 

    REAL src_data(*)
    REAL tgt_data(*)
    CALL interp(src_data, tgt_data)
  
END SUBROUTINE nn_interp

SUBROUTINE init_intern(grid_file1, n1, unit2, n2, nn_weight)
    USE slintdatastru
    use module_kd,only: init_kd_tree
    IMPLICIT NONE
   
    INTEGER :: n1, n2, nn_weight
    CHARACTER *(*),intent(in) :: grid_file1
    integer       ,intent(in) :: unit2

    REAL*8 lat, lon, d2r, r2d
    INTEGER i, j, g_idx, seq
    REAL, ALLOCATABLE :: llpoints(:,:) 

    d2r = 4.0*ATAN(1.0)/180.0
    r2d = 1 / d2r

    ALLOCATE(llpoints(n1, 2))
    OPEN (10,file=grid_file1,status='old',form='unformatted')
    READ (10) llpoints(:, 1), llpoints(:, 2)
    CLOSE(10)
    CALL init_kd_tree(llpoints, n1, 3)

    src_grid%type = 1
    src_grid%ngp = n1
    ALLOCATE(src_grid%latlon(2, n1))
    ALLOCATE(src_grid%data(n1))

    DO i = 1, n1
      src_grid%latlon(1,i) = llpoints(i, 1) 
      src_grid%latlon(2,i) = llpoints(i, 2) 
    END DO

    DEALLOCATE(llpoints)

    tgt_grid%type = 1
    tgt_grid%ngp = n2

    ALLOCATE(tgt_grid%latlon(2, n2))
    ALLOCATE(tgt_grid%nn(3, n2))
    ALLOCATE(tgt_grid%coeffs(3, n2))
    ALLOCATE(tgt_grid%data(n2))
    ALLOCATE(llpoints(n2, 2))
      
    READ (unit2) llpoints(:, 1)
    READ (unit2) llpoints(:, 2)
    DO i = 1, n2
      tgt_grid%latlon(1,i) = llpoints(i, 1) 
      tgt_grid%latlon(2,i) = llpoints(i, 2) 
    END DO

    CALL coeff_comp(nn_weight)

END SUBROUTINE init_intern

SUBROUTINE interp_intern() 
    USE slintdatastru
    IMPLICIT NONE

    INTEGER i, j, mx, my, g_idx
    REAL*4 v(3), c(3)
   
    IF (src_grid%type == 1 .AND. tgt_grid%type == 0) THEN
      mx = tgt_grid%mx
      my = tgt_grid%my
      DO i = 1, mx
        DO j = 1, my
          g_idx = (i + (j - 1) * mx)
          c = tgt_grid%coeffs(1:3,g_idx)
          v(1) = src_grid%data(tgt_grid%nn(1, g_idx))
          v(2) = src_grid%data(tgt_grid%nn(2, g_idx))
          v(3) = src_grid%data(tgt_grid%nn(3, g_idx))
          tgt_grid%data(g_idx) = c(1) * v(1) + c(2) * v(2) + c(3) * v(3) 
        END DO
      END DO
    ELSE IF (src_grid%type == 1 .AND. tgt_grid%type == 1) THEN
      DO i = 1, tgt_grid%ngp
        c = tgt_grid%coeffs(1:3,i)
        v(1) = src_grid%data(tgt_grid%nn(1, i))
        v(2) = src_grid%data(tgt_grid%nn(2, i))
        v(3) = src_grid%data(tgt_grid%nn(3, i))
        tgt_grid%data(i) = c(1) * v(1) + c(2) * v(2) + c(3) * v(3) 
      END DO
    END IF

END SUBROUTINE interp_intern

SUBROUTINE coeff_comp(nn_w)
    USE slintdatastru
    use module_kd,only: knn_search
    IMPLICIT NONE

    INTEGER nn_w

    REAL*8 latlon(2, 3), intsec(2), gcd1, gcd2, part_gcd1, part_gcd2
    INTEGER i, j,mx, my, g_idx, nn(3)
    REAL*8 min_dist, epsilon, r2d , t1, t2, theta1, theta2

    epsilon = 0.00000000001
    r2d = 180.0/(ATAN(1.0) * 4.0)
    print*,'in coeff_comp',nn_w
    IF (src_grid%type == 1 .AND. tgt_grid%type == 0) THEN
      mx = tgt_grid%mx
      my = tgt_grid%my
      DO i = 1, mx
        DO j = 1, my
          g_idx = (i + (j - 1) * mx)
          CALL knn_search(tgt_grid%latlon(1,g_idx), nn, min_dist)
          latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
          latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
          latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
          CALL intersection (latlon(1,1), tgt_grid%latlon(1,g_idx), latlon(1,2), latlon(1,3), intsec) 

          CALL gcd_ratio(latlon(1,2), latlon(1,3), intsec, gcd1, part_gcd1)
          CALL gcd_ratio(latlon(1,1), intsec, tgt_grid%latlon(1,g_idx), gcd2, part_gcd2)

          tgt_grid%nn(1:3, g_idx) = nn(1:3)
          IF (min_dist < epsilon) THEN 
            tgt_grid%coeffs(1, g_idx) = 1.0  
            tgt_grid%coeffs(2, g_idx) = 0.0  
            tgt_grid%coeffs(3, g_idx) = 0.0  
          ELSE 
            t1 = part_gcd1
            t2 = part_gcd2 
!            theta1 = gcd1
!            theta2 = gcd2 
!            tgt_grid%coeffs(1, g_idx) = SIN((1.0 - t2) * theta2) / SIN(theta2)  
!            tgt_grid%coeffs(2, g_idx) = SIN(t2 * theta2) / SIN(theta2) * &
!                                        SIN((1.0 - t1) * theta1) / SIN(theta1)   
!            tgt_grid%coeffs(3, g_idx) = SIN(t2 * theta2) / SIN(theta2) * &  
!                                        SIN(t1 * theta1) / SIN(theta1)

            tgt_grid%coeffs(1, g_idx) = (1.0 - t2)   
            tgt_grid%coeffs(2, g_idx) = t2 * (1.0 - t1)    
            tgt_grid%coeffs(3, g_idx) = t2 * t1 
            IF (nn_w == 1) THEN
              tgt_grid%coeffs(1, g_idx) = 1.0
              tgt_grid%coeffs(2, g_idx) = 0.0 
              tgt_grid%coeffs(3, g_idx) = 0.0 
            ENDIF
          END IF
        END DO
      END DO
    ELSE IF (src_grid%type == 1 .AND. tgt_grid%type == 1) THEN
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
    END IF

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

! two legacy subroutines, keep them for now
SUBROUTINE nn_int (src_data, tgt_data) 

    REAL src_data(*)
    REAL tgt_data(*)
    CALL interp(src_data, tgt_data)
  
END SUBROUTINE nn_int

SUBROUTINE bl_int (src_data, tgt_data) 

    REAL src_data(*)
    REAL tgt_data(*)
    CALL interp(src_data, tgt_data)
  
END SUBROUTINE bl_int

! two subroutines specially interface the pop main program
SUBROUTINE bilinear_init_i2r(mx, my, llpoints, nip)
    USE slintdatastru
    use module_kd,only: init_kd_tree
    IMPLICIT NONE
   
    INTEGER, intent(in) :: mx, my, nip
    REAL   , intent(in) :: llpoints(nip,2) 
    REAL*8 pi
    REAL*8 lat, lon, d2r, r2d
    INTEGER i, j, g_idx, seq


    d2r = 4.0*ATAN(1.0)/180.0
    r2d = 1 / d2r

    CALL init_kd_tree(llpoints, nip, 3)

    src_grid%type = 1
    src_grid%ngp = nip
    ALLOCATE(src_grid%latlon(2, nip))
    ALLOCATE(src_grid%data(nip))

    DO i = 1, nip
      src_grid%latlon(1,i) = llpoints(i, 1) 
      src_grid%latlon(2,i) = llpoints(i, 2) 
    END DO

    tgt_grid%type = 0
    tgt_grid%mx = mx
    tgt_grid%my = my

    ALLOCATE(tgt_grid%latlon(2, mx * my))
    ALLOCATE(tgt_grid%nn(3, mx * my))
    ALLOCATE(tgt_grid%coeffs(3, mx * my))
    ALLOCATE(tgt_grid%data(mx * my))
    pi = 4.0*ATAN(1.0)
    DO i = 1, mx 
      DO j = 1, my
        g_idx = (i + (j - 1) * mx)
        tgt_grid%latlon(1, g_idx) = (REAL(j - 1) - REAL(my - 1) * 0.5)  * pi / REAL(my - 1) 
        tgt_grid%latlon(1, g_idx) = -tgt_grid%latlon(1, g_idx) 
        tgt_grid%latlon(2, g_idx) = REAL(i - 1) * 2.0 * pi / REAL(mx) 
      END DO
    END DO
      
    CALL coeff_comp(0)

END SUBROUTINE bilinear_init_i2r

SUBROUTINE bilinear_interp_i2r(k, nlevels, vardata, data_xyz) 
    USE slintdatastru
    IMPLICIT NONE

    INTEGER k, nlevels
    REAL vardata(*)
    REAL data_xyz(*)
  
    INTEGER i, j, n, mx, my

    n = src_grid%ngp
    DO i = 1, n
      src_grid%data(i) = vardata(k + (i - 1) * nlevels) 
    END DO

    CALL interp_intern()

    mx = tgt_grid%mx
    my = tgt_grid%my
    DO i = 1, mx
      DO j = 1, my
        data_xyz((k - 1) * mx * my + (j - 1) * mx + i) =  &
        tgt_grid%data((j - 1) * mx + i) 
      END DO
    END DO

END SUBROUTINE bilinear_interp_i2r

! subrountines for spherical curve interpolation 
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




