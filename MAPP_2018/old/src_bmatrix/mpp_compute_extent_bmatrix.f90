SUBROUTINE mpp_compute_extent(isg,ieg,ndivs,ibegin,iend)

  INTEGER,                 INTENT(in)          :: isg, ieg, ndivs
  INTEGER, DIMENSION(0:ndivs-1), INTENT(out)          :: ibegin, iend

  INTEGER :: ndiv, imax, ndmax, ndmirror
  INTEGER :: is, ie, n
  LOGICAL :: symmetrize
!statement functions
  LOGICAL :: even, odd
  even(n) = (MOD(n,2).EQ.0)
  odd (n) = (MOD(n,2).EQ.1)

  is = isg

  DO ndiv=0,ndivs-1
     symmetrize = ( even(ndivs) .AND. even(ieg-isg+1) ) .OR. &
          (  odd(ndivs) .AND.  odd(ieg-isg+1) ) .OR. &
          (  odd(ndivs) .AND. even(ieg-isg+1) .AND. ndivs.LT.(ieg-isg+1)/2 )
     
     IF( ndiv.EQ.0 )THEN
        imax = ieg
        ndmax = ndivs
     END IF
!do bottom half of decomposition, going over the midpoint for odd ndivs

     IF( ndiv.LT.(ndivs-1)/2+1 )THEN
!domain is sized by dividing remaining points by remaining domains
        ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
        ndmirror = (ndivs-1) - ndiv !mirror domain
        IF( ndmirror.GT.ndiv .AND. symmetrize )THEN !only for domains over the midpoint
!mirror extents, the max(,) is to eliminate overlaps
           ibegin(ndmirror) = MAX( isg+ieg-ie, ie+1 )
           iend(ndmirror)   = MAX( isg+ieg-is, ie+1 )
           imax = ibegin(ndmirror) - 1
           ndmax = ndmax - 1
        END IF
     ELSE
        IF( symmetrize )THEN
!do top half of decomposition by retrieving saved values
           is = ibegin(ndiv)
           ie = iend(ndiv)
        ELSE
           ie = is + CEILING( REAL(imax-is+1)/(ndmax-ndiv) ) - 1
        END IF
     END IF
     ibegin(ndiv) = is
     iend(ndiv) = ie

     IF( ie.LT.is ) THEN
        PRINT *,'domain extents must bepositive definite - Stopping'
        STOP
     ENDIF
     
     IF( ndiv.EQ.ndivs-1 .AND. iend(ndiv).NE.ieg ) THEN
        PRINT *,'domain extents do not span space completely Stopping'
        STOP
     ENDIF

     is = ie + 1

  END DO

END SUBROUTINE mpp_compute_extent

