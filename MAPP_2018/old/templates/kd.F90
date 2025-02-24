module module_kd
contains
!==========================================================================
! Subroutines for the k-d tree
!
!  The algorithm is based on Bentley's optimal k-d tree, plus 
!  some modifications of mine. The priority queue was omitted
!  since the number of nearest neighbors to be searched is 
!  always relatively small (7-11). The dimension selection 
!  subroutine will be enhanced with a more advanced algorithm soon.  
!
!
!  Ning Wang, Nov. 2006
!
!==========================================================================

    SUBROUTINE BuildTree(num, dim, points)
        USE kd_datastru

        IMPLICIT NONE

        INTEGER :: num, dim
        REAL, TARGET :: points(dim, num)
!       TYPE(Node), POINTER :: BuildTreeRec
        INTEGER :: i
        !print*,'allocating nodes',num,dim
        ALLOCATE(nodes(2 * num))
        !print*,'allocating perm'
        ALLOCATE(perm(num))
	!print*,'alocating os'
        ALLOCATE(os(dim))

        DO i = 1, num
          perm(i) = i
        END DO

        n = num
        d = dim
        num_k = 1
        gc = 1
        !print*,'in renaming'
        ppoints => points
        bottom_level = log(REAL(n)) / log(2.0) + 1
        root => BuildTreeRec(points, 1, n, 1)

    END SUBROUTINE BuildTree


    RECURSIVE FUNCTION BuildTreeRec(points, l, u, level) RESULT(theNode)
        USE kd_datastru

        IMPLICIT NONE

        REAL points(d, n)
        INTEGER :: l, u, level
        TYPE(Node), POINTER :: theNode
!       TYPE(Node), POINTER :: newNode

!       INTEGER :: m, dir2cut
        INTEGER :: m

        integer :: i
        ! get a new node
        theNode => NewNode()

        IF (level >= bottom_level) THEN
          theNode%bucket = .true.
          theNode%lopt = l
          theNode%hipt = u
        ELSE 
          theNode%bucket = .false.
          m = (l + u ) / 2 
          theNode%discrim = dir2cut(l, u, points)
          CALL partition(points,l, u, m, theNode%discrim) 
          theNode%cutval = points(theNode%discrim, perm(m))
          theNode%loson => BuildTreeRec(points, l, m, level + 1)
          theNode%hison => BuildTreeRec(points, m+1, u, level + 1) 
        END IF 

    END FUNCTION BuildTreeRec

    SUBROUTINE Set_k(k)
        USE kd_datastru
                                                                                   
        IMPLICIT NONE

        INTEGER::k

        num_k = k

        ALLOCATE(nni(k))
        ALLOCATE(nnd(k))

    END SUBROUTINE

    subroutine Search(query) 
        USE kd_datastru
  
        IMPLICIT NONE

        REAL :: query(d)
  !      INTEGER, POINTER :: Search(:)

        INTEGER :: i
        REAL :: partsum

        partsum = 0
        DO i = 1, d
          os(i) = 0.0
        END DO
        DO i = 1, num_k
          nnd(i) = 10000.0 * 10000.0
          nni(i) = 0
        END DO
        curDistSq = 10000.0 * 10000.0 

        CALL SearchRec(query, root,  partsum, os)
  !      Search => nni

    END subroutine Search
 
    RECURSIVE SUBROUTINE SearchRec(query, tnode, partsum, o_s) 
        USE kd_datastru

        IMPLICIT NONE

        REAL :: query(d), partsum, o_s(d)
        TYPE(Node) :: tnode

        INTEGER :: i
!       REAL :: distSq, inp, d2o, ps
        REAL :: distSq, d2o, ps
        IF (tnode%bucket) THEN
          DO i = tnode%lopt, tnode%hipt
            distSq = inp(query, ppoints(:, perm(i))) 
            IF (distSq < curDistSq) THEN
              curDistSq = distSq
              CALL insert(perm(i))
            END IF
          END DO   
        ELSE 
          d2o = query(tnode%discrim) - tnode%cutval
          IF (d2o < 0.0) THEN
            ps = partsum
            CALL SearchRec(query, tnode%loson, ps, o_s)
            ps = partsum + d2o * d2o - o_s(tnode%discrim) 
            cur_o_s = o_s(tnode%discrim)
            o_s(tnode%discrim) = d2o * d2o 
            IF (ps < curDistSq) THEN
              CALL SearchRec(query, tnode%hison, ps, o_s) 
            END IF
            o_s(tnode%discrim) = cur_o_s
          ELSE
            ps = partsum
            CALL SearchRec(query, tnode%hison, ps, o_s)
            ps = partsum + d2o * d2o - o_s(tnode%discrim) 
            cur_o_s = o_s(tnode%discrim)
            o_s(tnode%discrim) = d2o * d2o 
            IF (ps < curDistSq) THEN
              CALL SearchRec(query, tnode%loson, ps, o_s) 
            END IF
            o_s(tnode%discrim) = cur_o_s
          END IF
        END IF

     END SUBROUTINE SearchRec


! Subroutines and functions that are helps creatation and searching k-d tree.
! get a new node
     FUNCTION NewNode() 
        USE kd_datastru

        TYPE(Node), POINTER :: NewNode

        NewNode => nodes(gc)
        gc = gc + 1
   
     END FUNCTION NewNode

! partition the points along dir 'discrim' into lower and upper parts 
     SUBROUTINE partition(points, l, u, m, discrim)

        USE kd_datastru
                                                                                                   
        IMPLICIT NONE

        REAL :: points(d, n)
        INTEGER :: l, u, m, discrim

        REAL :: v
        INTEGER :: i, j, t, r, lo

        r = u
        lo = l

        DO WHILE ( r > lo)
          v = points(discrim, perm(r))
          i = lo
          j = r - 1
          DO WHILE (.true.)
            DO WHILE (points(discrim,perm(i)) < v) 
              i = i + 1
            END DO
            DO WHILE (points(discrim, perm(j)) >= v .AND. j > lo)
              j = j - 1
            END DO
            IF (i >= j) EXIT
            t = perm(i)
            perm(i) = perm(j)
            perm(j) = t
          END DO
          t = perm(i)
          perm(i) = perm(r)
          perm(r) = t
          IF (i >= m) r = i - 1;
          IF (i <= m) lo = i + 1
        END DO
      
     END SUBROUTINE partition

! function returns the direction to divide
     FUNCTION dir2cut(l, u, points)
        USE kd_datastru
                                                                                                   
        IMPLICIT NONE

        REAL :: points(d, n)
        INTEGER :: l, u

        INTEGER :: dir2cut

        dir = dir + 1
        IF (dir > d) THEN
          dir = 1
        END IF
        dir2cut = dir
     
     END FUNCTION dir2cut

! function to compute the inner product
     FUNCTION inp(p1, p2) 
        USE kd_datastru, ONLY : d
        IMPLICIT NONE
        
        REAL :: p1(d), p2(d)
        REAL :: inp

        REAL sum, dif
        INTEGER i

        sum = 0
        DO i = 1, d
          dif = p1(i) - p2(i)
          sum = sum + dif * dif
        END DO 

        inp = sum

     END FUNCTION inp

! subroutine to insert the current nn  
     SUBROUTINE insert(pt_idx)
        USE kd_datastru, ONLY : num_k, nni, nnd, curDistSq

        IMPLICIT NONE

        INTEGER :: pt_idx
 
        INTEGER :: i, j

        DO i = 1, num_k	
          IF (curDistSq < nnd(i)) THEN
            DO j = num_k, i + 1, -1
              nni(j) = nni(j - 1)
              nnd(j) = nnd(j - 1)
            END DO
            nni(i) = pt_idx
            nnd(i) = curDistSq
            EXIT            
          END IF
        END DO
      
        curDistSq = nnd(num_k)
              
      END SUBROUTINE insert
          

! function to return the index array

      SUBROUTINE result()
         USE kd_datastru, ONLY : num_k, nni, nnd, ppoints

         IMPLICIT NONE
         INTEGER :: i
      
         DO i = 1, num_k
           PRINT*, nni(i)
           PRINT*, ppoints(:, nni(i)), nnd(i)
         END DO 

      END SUBROUTINE result

SUBROUTINE init_kd_tree(llpoints, n, k)
    IMPLICIT NONE
    integer,intent(in) :: n,k
    REAL   ,intent(in) :: llpoints(n,2) 
    REAL d2r
    REAL, ALLOCATABLE, SAVE :: points(:,:) 
    INTEGER :: i, seq, dim
    REAL*8 lat, lon

    d2r = 4.0*ATAN(1.0)/180.0
    dim = 3
    if (.not.allocated(points)) ALLOCATE(points(dim,n))
    CALL lls2xyzs(llpoints, points, n)
    CALL BuildTree(n, dim, points)
    CALL set_k(k)
END SUBROUTINE init_kd_tree
         

SUBROUTINE knn_search(ll, nn, min_dist)
    USE kd_datastru
    IMPLICIT NONE
    
    REAL*8 ll(2) 
    INTEGER nn(num_k)
    REAL*8 min_dist

    REAL :: q(3)
    INTEGER i
    
    CALL ll2xyz(ll, q)
    CALL Search (q) 
    DO i = 1, num_k
      nn(i) = nni(i)
    END DO 
    min_dist = nnd(1)

END SUBROUTINE knn_search


SUBROUTINE lls2xyzs(llpts, xyzpts, n)

   INTEGER :: n
   REAL :: llpts(n, 2), xyzpts(3, n)  
   
   DO i = 1, n
     xyzpts(1,i) = cos(llpts(i,1)) * cos(llpts(i,2))
     xyzpts(2,i) = cos(llpts(i,1)) * sin(llpts(i,2))
     xyzpts(3,i) = sin(llpts(i,1))
   END DO 

END SUBROUTINE lls2xyzs
end module module_kd
