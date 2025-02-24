PROGRAM read_fv3_split

  USE module_domain, ONLY: domain2d
  USE module_var !, ONLY: var
  USE module_namelist_bmatrix, ONLY: npx,npy,npz,nlayout,layout,ntiles,&
       &namelist_bmatrix_sub,nxblocks,nyblocks
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
  INTEGER :: itile,i,j,iproc,i1,i2,i3,i4

  CHARACTER(len=NF90_MAX_NAME), ALLOCATABLE, DIMENSION(:) :: &
       &varnames1d,varnames2d,varnames3d,varnames

  CHARACTER(len=NF90_MAX_NAME) :: varname_psfc='psfc'
  CHARACTER(len=NF90_MAX_NAME) :: varname_orog='orog'
  CHARACTER(len=NF90_MAX_NAME) :: varname_pk='pk'
  CHARACTER(len=NF90_MAX_NAME) :: varname_bk='bk'

  TYPE(domain2d), ALLOCATABLE, DIMENSION(:) :: mydomains
  TYPE(var4d), ALLOCATABLE, DIMENSION(:) :: myvars4d
  TYPE(var3d), ALLOCATABLE, DIMENSION(:) :: myvars3d
  TYPE(var2d), ALLOCATABLE, DIMENSION(:) :: myvars2d
  TYPE(var2d), ALLOCATABLE, DIMENSION(:) :: myvars1d

  CHARACTER(len=500) :: filename_template=&
       &'./INPUT/20150810060000.fv3_history.tile?.nc',&
       &filename

  INTEGER :: nvars,nvars1d,nvars2d,nvars3d,nvars4d,ntimes,ndims

  INTEGER, PARAMETER :: nc_nmaxdims=4
  INTEGER, DIMENSION(nc_nmaxdims) :: dimids,dimlens
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ncvarids
  CHARACTER(len=NF90_MAX_NAME), DIMENSION(nc_nmaxdims) :: dimnames
  INTEGER :: ncstatus,ncfileid,ncvarid
  INTEGER, DIMENSION(nc_nmaxdims) :: start_index,count_index,counts
  INTEGER :: ivar,idim

  INTEGER :: tindex
  CHARACTER(len=1) :: ctile

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

  ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),&
       &mode=nf90_nowrite,ncid=ncfileid)
  
  ncstatus = nf90_inquire(ncid=ncfileid,nvariables=nvars)
  ALLOCATE(ncvarids(nvars),varnames(nvars))
  
  ncstatus = nf90_inq_varids(ncfileid,nvars,ncvarids)

  nvars1d=0
  nvars2d=0
  nvars3d=0
  nvars4d=0

  DO ivar=1,nvars
     
     ncstatus = nf90_inquire_variable(ncid=ncfileid,&
          &varid=ncvarids(ivar),ndims=ndims,name=varnames(ivar))
     ncstatus = nf90_inquire_variable(ncfileid,&
          &varid=ncvarids(ivar),dimids=dimids)
     
     DO idim=1,ndims
        ncstatus = nf90_inquire_dimension(ncid=ncfileid, &
             &dimid=dimids(idim), &
             &name=dimnames(idim), len=dimlens(idim))
     ENDDO
     
     IF (ndims == 1) THEN
        nvars1d=nvars1d+1
     ELSEIF (ndims == 2) THEN
        nvars2d=nvars2d+1
     ELSEIF (ndims == 3) THEN
        nvars3d=nvars3d+1
     ELSEIF (ndims == 4) THEN
        nvars4d=nvars4d+1
     ENDIF
  ENDDO

  ncstatus = nf90_close(ncfileid)  

  ALLOCATE(myvars1d(nvars1d),myvars2d(nvars2d),&
       &myvars3d(nvars3d),myvars4d(nvars4d))

  DO iproc=0,nprocs-1

     WRITE(ctile,'(i1)')mydomains(iproc)%itile
     filename=filename_template(1:tindex+3)//ctile//&
          &filename_template(tindex+5:)

     ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),&
          &mode=nf90_nowrite,ncid=ncfileid)
     
     start_index=(/mydomains(iproc)%ibeg,mydomains(iproc)%jbeg,1,1/)
     count_index=(/&
          &mydomains(iproc)%iend-mydomains(iproc)%ibeg+1,&
          &mydomains(iproc)%jend-mydomains(iproc)%jbeg+1,&
          &npz,ntimes/)
     
     i1=0
     i2=0
     i3=0
     i4=0

     DO ivar=1,nvars
        
        ncstatus = nf90_inquire_variable(ncid=ncfileid,&
             &varid=ncvarids(ivar),ndims=ndims,name=varnames(ivar))
        ncstatus = nf90_inquire_variable(ncfileid,&
             &varid=ncvarids(ivar),dimids=dimids)

        DO idim=1,ndims
           ncstatus = nf90_inquire_dimension(ncid=ncfileid, &
                &dimid=dimids(idim), &
                &name=dimnames(idim), len=dimlens(idim))
        ENDDO

        IF (ndims == 1) THEN
           i1=i1+1
           counts(1)=dimlens(1)
           PRINT *,ndims,TRIM(varnames(ivar))
           myvars1d(i1)%rank=1
           myvars1d(i1)%name=TRIM(varnames(ivar))
           myvars1d(i1)%size=counts(1)
           CALL sub_allocate1d()
!           ncstatus = nf90_get_var(ncfileid,ncvarid,&
!                &myvars1d(i1)%values,&
!                &start_index(1),counts(1))
        ELSEIF (ndims == 2) THEN
           i2=i2+1
           counts(1:2)=dimlens(1:2)
           PRINT *,ndims,TRIM(varnames(ivar))
           myvars2d(i2)%rank=2
           ALLOCATE(myvars2d(i2)%values(counts(1),counts(2)))
           ncstatus = nf90_get_var(ncfileid,ncvarid,&
                &myvars2d(i2)%values,&
                &start_index(1:2),counts(1:2))
!           ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,&
!                &start_index(1:2),dimlens(1))
        ELSEIF (ndims == 3) THEN
           i3=i3+1
           counts(1:3)=dimlens(1:3)
!           ALLOCATE(myvars3d(i3)%values(counts(1),counts(2),counts(3)))
           PRINT *,ndims,TRIM(varnames(ivar))
!           ALLOCATE(var1d(dimlens(1))
!           ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,&
!                &start_index(1:2),dimlens(1))
        ELSEIF (ndims == 4) THEN
           i4=i4+1
           counts(1:4)=dimlens(1:4)
           ALLOCATE(myvars4d(i2)%values(counts(1),counts(2),&
                &counts(3),counts(4)))
           PRINT *,ndims,TRIM(varnames(ivar))
!           ALLOCATE(var1d(dimlens(1))
!           ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,&
!                &start_index(1:2),dimlens(1))
        ENDIF

!        ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,&
!             &start_index,count_index)
     ENDDO

     ncstatus = nf90_close(ncfileid)

     stop
   
  ENDDO

END PROGRAM read_fv3_split

