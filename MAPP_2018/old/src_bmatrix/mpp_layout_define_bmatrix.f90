SUBROUTINE mpp_define_layout( global_indices, nglobal_indices, ndivs, &
     &layout, nlayout )
  INTEGER, INTENT(in) :: global_indices(nglobal_indices) !(/ isg, ieg, jsg, jeg /)
  INTEGER, INTENT(in) :: ndivs !number of divisions to divide global domain
  INTEGER, INTENT(out) :: layout(nlayout)

  INTEGER :: isg, ieg, jsg, jeg, isz, jsz, idiv, jdiv

  IF(SIZE(global_indices(:)) .NE. 4) THEN
     PRINT *,'size of global_indices should be 4 - Stopping'
     STOP
  ENDIF
  
  IF(SIZE(layout(:)) .NE. 2) THEN
     PRINT *,'size of layout should be 2 - Stopping'
     STOP
  ENDIF

  isg = global_indices(1)
  ieg = global_indices(2)
  jsg = global_indices(3)
  jeg = global_indices(4)

  isz = ieg - isg + 1
  jsz = jeg - jsg + 1
!first try to divide ndivs in the domain aspect ratio: if imperfect aspect, reduce idiv till it divides ndivs
  idiv = NINT( SQRT(float(ndivs*isz)/jsz) )
  idiv = MAX(idiv,1) !for isz=1 line above can give 0
  DO WHILE( MOD(ndivs,idiv).NE.0 )
     idiv = idiv - 1
  END DO                 !will terminate at idiv=1 if not before
  jdiv = ndivs/idiv

  layout = (/ idiv, jdiv /)
  RETURN
END SUBROUTINE mpp_define_layout
