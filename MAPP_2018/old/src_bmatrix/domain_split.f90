PROGRAM domain_split

  USE module_domain, ONLY: domain2d
  USE namelist_bmatrix, ONLY: npx,npy,nlayout,layout,ntiles,&
       &namelist_bmatrix_sub

  IMPLICIT NONE

  INTEGER, PARAMETER :: nglobal_indices=4
  INTEGER :: isg,ieg,jsg,jeg,ndivs,nprocs,nprocs_per_tile
  INTEGER, DIMENSION(nglobal_indices) :: global_indices
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ibegin,iend
  INTEGER, ALLOCATABLE, DIMENSION(:) :: jbegin,jend
  INTEGER :: layout_x,layout_y
  INTEGER :: itile,i,j,iproc

  TYPE(domain2d), ALLOCATABLE, DIMENSION(:) :: mydomains

  CALL namelist_bmatrix_sub

  isg=1
  ieg=npx-1
  jsg=1
  jeg=npy-1
  
  layout_x=layout(1)
  layout_y=layout(2)

  nprocs=6*layout_x*layout_y

  IF (MOD(nprocs,ntiles) /= 0) THEN
     PRINT *,'nprocs must divide by ntiles - Stopping'
     STOP
  ENDIF

  nprocs_per_tile=nprocs/ntiles

  global_indices(1)=isg
  global_indices(2)=ieg
  global_indices(3)=jsg
  global_indices(4)=jeg

  IF (layout_x == 0 .OR. layout_x == 0 ) THEN
     CALL mpp_define_layout( global_indices, nglobal_indices, &
          &nprocs_per_tile, layout,nlayout )
  ELSE
     IF (ntiles*layout_x*layout_y /= nprocs) THEN
        PRINT *,'layout_x and layout_y not consistent with nprocs - &
             &Stopping'
        STOP
     ENDIF
  ENDIF

  ndivs=layout(1)
  ALLOCATE(ibegin(ndivs),iend(ndivs))
  
  CALL mpp_compute_extent(isg,ieg,ndivs,ibegin,iend)

!  PRINT *,ibegin,iend

  ndivs=layout(2)
  ALLOCATE(jbegin(ndivs),jend(ndivs))
  CALL mpp_compute_extent(isg,ieg,ndivs,jbegin,jend)

!  PRINT *,jbegin,jend

  ALLOCATE(mydomains(0:nprocs-1))

  iproc=0

!processors inner loop in x, outer loop in y
  DO itile=1,ntiles
     DO j=1,layout(2)
        DO i=1,layout(1)
           mydomains(iproc)%itile=itile
           mydomains(iproc)%ibeg=ibegin(i)
           mydomains(iproc)%iend=iend(i)
           mydomains(iproc)%jbeg=jbegin(j)
           mydomains(iproc)%jend=jend(j)
           mydomains(iproc)%pe=iproc
           PRINT *,iproc,mydomains(iproc)%itile,&
                &mydomains(iproc)%ibeg,mydomains(iproc)%iend,&
                &mydomains(iproc)%jbeg,mydomains(iproc)%jend,&
                &mydomains(iproc)%pe
           iproc=iproc+1
        ENDDO
     ENDDO
  ENDDO

  DEALLOCATE(ibegin,iend,jbegin,jend)

END PROGRAM domain_split

