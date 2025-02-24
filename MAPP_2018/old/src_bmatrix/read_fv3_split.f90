PROGRAM read_fv3_split

  USE module_domain, ONLY: domain2d
  USE module_var !, ONLY: var
  USE module_namelist_bmatrix, ONLY: npx,npy,npz,nlayout,layout,ntiles,&
       &namelist_bmatrix_sub,nxblocks,nyblocks
  USE module_parse_table
  USE kinds
  USE mpi_interface
  USE netcdf
  USE netcdfio_interface


  IMPLICIT NONE

  INTEGER, PARAMETER :: nglobal_indices=4
  INTEGER :: isg,ieg,jsg,jeg,ndivs,nprocs,nprocs_per_tile
  INTEGER, DIMENSION(nglobal_indices) :: global_indices
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ibegin,iend
  INTEGER, ALLOCATABLE, DIMENSION(:) :: jbegin,jend
  INTEGER :: layout_x,layout_y
  INTEGER :: itile,i,j,iproc

!nvars,nx,ny,nz,ntimes

  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: varvalues4d
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:,:) :: varvalues3d !psfc
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: varvalues2d !orog
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: varvalues1d !pk,bk

  CHARACTER(len=NF90_MAX_NAME) :: varname_ucomp='ucomp'
  CHARACTER(len=NF90_MAX_NAME) :: varname_vcomp='vcomp'

  TYPE(domain2d), ALLOCATABLE, DIMENSION(:) :: mydomains

  CHARACTER(len=500) :: filename_template=&
       &'./INPUT/20150810060000.fv3_history.tile?.nc',&
       &filename

  INTEGER :: ntimes,ndims

  INTEGER, PARAMETER :: nc_nmaxdims=4
  INTEGER, DIMENSION(nc_nmaxdims) :: dimids,dimlens
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ncvarids
  INTEGER :: ncstatus,ncfileid,ncvarid
  INTEGER, DIMENSION(nc_nmaxdims) :: start_index,count_index,counts
  INTEGER :: ivar,idim

  INTEGER :: tindex,interval_x,interval_y
  CHARACTER(len=1) :: ctile

  REAL(r_single) :: clip

!  clip = TINY(1._r_single)
  clip = 1.e-16

  CALL namelist_bmatrix_sub
  CALL parse_table_sub

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

  interval_x=iend(1)-ibegin(1)+1
  interval_y=jend(1)-jbegin(1)+1

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
!           PRINT *,iproc,mydomains(iproc)%itile,&
!                &mydomains(iproc)%ibeg,mydomains(iproc)%iend,&
!                &mydomains(iproc)%jbeg,mydomains(iproc)%jend,&
!                &mydomains(iproc)%pe
           iproc=iproc+1
        ENDDO
     ENDDO
  ENDDO

  DEALLOCATE(ibegin,iend,jbegin,jend)

  tindex=INDEX(filename_template,'tile')
  ctile='1'
  filename=filename_template(1:tindex+3)//ctile//&
       &filename_template(tindex+5:)
  
  call netcdfio_dimension(filename,'time',ntimes)

  IF (nf90_noerr /= nf90_open(path=TRIM(ADJUSTL(filename)),&
       &mode=nf90_nowrite,ncid=ncfileid)) STOP('read_fv3_split.1')

  DO ivar=1,nvars1d
     ncstatus = nf90_inq_varid(ncfileid,myvars1d(ivar)%name,ncvarid)
     ncstatus = nf90_inquire_variable(ncid=ncfileid,&
          &varid=ncvarid,ndims=ndims,dimids=dimids)
     IF (ndims /= 1) THEN
        PRINT *,'error in dimension 1d'
        STOP
     ENDIF

     DO idim=1,ndims
        ncstatus = nf90_inquire_dimension(ncid=ncfileid, &
             &dimid=dimids(idim),len=dimlens(idim))
     ENDDO

     IF (dimlens(1) /= npz+1) THEN
        PRINT *,'error in dimension 1d npz'
        STOP
     ENDIF

     
     IF (.NOT. ALLOCATED(varvalues1d)) THEN
        ALLOCATE(varvalues1d(nvars1d,dimlens(1))) !pk,bk
        start_index(1)=1
        count_index(1)=dimlens(1)
     ENDIF
     ncstatus = nf90_get_var(ncfileid,ncvarid,&
          &varvalues1d(ivar,:),&
          &(/start_index(1)/),(/count_index(1)/))
     PRINT *,myvars1d(ivar)%name,ncvarid
     PRINT *,MINVAL(varvalues1d(ivar,:)),MAXVAL(varvalues1d(ivar,:))
  ENDDO

  ncstatus = nf90_close(ncfileid)  

  ALLOCATE(varvalues2d(nvars2d,interval_x,interval_y)) !orog
  ALLOCATE(varvalues3d(nvars3d,interval_x,interval_y,ntimes)) !psfc
  ALLOCATE(varvalues4d(nvars4d,interval_x,interval_y,npz,ntimes)) !4dvars

  DO iproc=0,nprocs-1
     
     PRINT *,'%',iproc

     WRITE(ctile,'(i1)')mydomains(iproc)%itile
     filename=filename_template(1:tindex+3)//ctile//&
          &filename_template(tindex+5:)

     ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),&
          &mode=nf90_nowrite,ncid=ncfileid)
     
     DO ivar=1,nvars2d

        start_index(1:2)=&
             &(/mydomains(iproc)%ibeg,mydomains(iproc)%jbeg/)
        count_index(1:2)=(/&
             &mydomains(iproc)%iend-mydomains(iproc)%ibeg+1,&
             &mydomains(iproc)%jend-mydomains(iproc)%jbeg+1/)
        
        ncstatus = nf90_inq_varid(ncfileid,myvars2d(ivar)%name,ncvarid)
        ncstatus = nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,ndims=ndims)
        IF (ndims /= 2) THEN
           PRINT *,'error in dimension 2d'
           STOP
        ENDIF

        ncstatus = nf90_get_var(ncfileid,ncvarid,&
             &varvalues2d(ivar,:,:),&
             &start_index(1:ndims),count_index(1:ndims))
        PRINT *,TRIM(myvars2d(ivar)%name)
        PRINT *,MINVAL(varvalues2d(ivar,:,:)),&
             &MAXVAL(varvalues2d(ivar,:,:))
     ENDDO

     DO ivar=1,nvars3d
        
        start_index(1:3)=&
             &(/mydomains(iproc)%ibeg,mydomains(iproc)%jbeg,1/)
        count_index(1:3)=(/&
             &mydomains(iproc)%iend-mydomains(iproc)%ibeg+1,&
             &mydomains(iproc)%jend-mydomains(iproc)%jbeg+1,&
             &ntimes/)

        ncstatus = nf90_inq_varid(ncfileid,myvars3d(ivar)%name,ncvarid)
        ncstatus = nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,ndims=ndims)
        IF (ndims /= 3) THEN
           PRINT *,'error in dimension 3d'
           STOP
        ENDIF

        ncstatus = nf90_get_var(ncfileid,ncvarid,&
             &varvalues3d(ivar,:,:,:),&
             &start_index(1:ndims),count_index(1:ndims))
        PRINT *,TRIM(myvars3d(ivar)%name)
        PRINT *,MINVAL(varvalues3d(ivar,:,:,:)),&
             &MAXVAL(varvalues3d(ivar,:,:,:))
     ENDDO

     DO ivar=1,nvars4d
        
        start_index(1:4)=&
             &(/mydomains(iproc)%ibeg,mydomains(iproc)%jbeg,1,1/)
        count_index(1:4)=(/&
             &mydomains(iproc)%iend-mydomains(iproc)%ibeg+1,&
             &mydomains(iproc)%jend-mydomains(iproc)%jbeg+1,&
             &npz,ntimes/)

        ncstatus = nf90_inq_varid(ncfileid,myvars4d(ivar)%name,ncvarid)
        ncstatus = nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarid,ndims=ndims)
        IF (ndims /= 4) THEN
           PRINT *,'error in dimension 4d'
           STOP
        ENDIF
        
        ncstatus = nf90_get_var(ncfileid,ncvarid,&
             &varvalues4d(ivar,:,:,:,:),&
             &start_index(1:ndims),count_index(1:ndims))
           IF (&
                &TRIM(myvars4d(ivar)%name) /= TRIM(varname_ucomp) .AND.&
                &TRIM(myvars4d(ivar)%name) /= TRIM(varname_vcomp)) &
                &WHERE (varvalues4d(ivar,:,:,:,:) < 0) &
                &varvalues4d(ivar,:,:,:,:)=clip
           
        PRINT *,TRIM(myvars4d(ivar)%name)
        PRINT *,MINVAL(varvalues4d(ivar,:,:,:,:)),&
             &MAXVAL(varvalues4d(ivar,:,:,:,:))
     ENDDO

     ncstatus = nf90_close(ncfileid)
  
  ENDDO
  
END PROGRAM read_fv3_split

