PROGRAM covinflate2nc

!to convert covinflate.dat files from enkf direct access to netcdf

  USE kinds

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: nvars=7,nvardims=4
  CHARACTER(len=12), DIMENSION(nvars+1) :: varnames = &
       &(/'U','V','T','QVAPOR','W','PH','PM2_5_DRY','MU'/)
  CHARACTER(len=20), DIMENSION(nvardims) ::&
       &  dname=(/'west_east','south_north','bottom_top','Time'/)
  CHARACTER(len=20) :: date

  INTEGER, DIMENSION(nvardims) :: dname_id
  INTEGER, DIMENSION(nvardims-1) :: dname_id_mu
  INTEGER, DIMENSION(nvars+1) :: var_id

  INTEGER, DIMENSION(nvardims) :: vardims,start_dims
  INTEGER, DIMENSION(nvardims-1) :: vardims_mu
  INTEGER :: iargc,nx,ny,nlevs,ndim,inunit=51,i,j,k,mcid,status
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: covinfglobal
  CHARACTER(len=120) :: infname,outfname
  CHARACTER(len=5) cnx,cny,cnlevs
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: inflvar3d
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:)  :: inflvar1d

  CALL getarg(1,infname)
  CALL getarg(2,cnx)
  CALL getarg(3,cny)
  CALL getarg(4,cnlevs)
  CALL getarg(5,date)
  CALL getarg(6,outfname)
  
  READ(cnx,*)nx
  READ(cny,*)ny
  READ(cnlevs,*)nlevs

  vardims(1)=nx
  vardims(2)=ny
  vardims(3)=nlevs
  vardims(4)=1

  vardims_mu(1:2)=vardims(1:2)
  vardims_mu(3)=vardims(4)

  ndim = nlevs*nvars+1

  ALLOCATE(covinfglobal(nx,ny,ndim),inflvar3d(nvars,nx,ny,nlevs),&
       &inflvar1d(1,nx,ny))

  OPEN(unit=inunit,form='unformatted',file=infname,access='direct',&
       &recl=nx*ny*ndim*4)
  READ(inunit,rec=1) covinfglobal
  CLOSE(inunit)

  DO i=1,nvars
     inflvar3d(i,:,:,:)=covinfglobal(:,:,(i-1)*nlevs+1:i*nlevs)
  ENDDO

  inflvar1d(1,:,:)=covinfglobal(:,:,ndim)

  status = nf_create(outfname, 0, mcid)
  IF (status /= nf_noerr) THEN 
     PRINT *,'error 0'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF
  
  DO i=1,nvardims
     IF ( dname(i) == "Time" ) THEN
        status = nf_def_dim(mcid, dname(i), NF_UNLIMITED, dname_id(i))
        IF (status /= nf_noerr) THEN 
           PRINT *,'error 1'
           PRINT *, 'Error: ', nf_strerror(status)
        ENDIF
     ELSE
        status = nf_def_dim(mcid, dname(i), vardims(i), dname_id(i))
        IF (status /= nf_noerr) THEN 
           PRINT *,'error 2'
           PRINT *, 'Error: ', nf_strerror(status)
        ENDIF
     ENDIF
  ENDDO

  dname_id_mu(1:2)=dname_id(1:2)
  dname_id_mu(3)=dname_id(4)

  start_dims = 1
  
  DO i = 1, nvars
     status = nf_def_var(mcid, varnames(i), NF_REAL, nvardims, &
          &dname_id,var_id(i))
     IF (status /= nf_noerr) THEN 
        PRINT *,'error 3'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF
  ENDDO
  
  i=nvars+1
  status = nf_def_var(mcid, varnames(i), NF_REAL, nvardims-1, &
       &dname_id_mu,var_id(i))
  IF (status /= nf_noerr) THEN
     PRINT *,'error 3a'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  status=nf_put_att_text(mcid, nf_global,'Date',20,date(1:20))

  IF (status /= nf_noerr) THEN
     PRINT *,'error 3b'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  status = nf_enddef(mcid)

  IF (status /= nf_noerr) THEN 
     PRINT *,'error 4'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF
  
  DO i = 1, nvars
     status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
          &inflvar3d(i,:,:,:))
     IF (status /= nf_noerr) THEN 
        PRINT *,'error 5'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF
  ENDDO

  i=nvars+1

  status = nf_put_vara_real(mcid,var_id(i),start_dims(1:3),vardims_mu,&
       &inflvar1d(1,:,:))
  IF (status /= nf_noerr) THEN
     PRINT *,'error 5a'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  status = nf_close(mcid)

  IF (status /= nf_noerr) THEN
     PRINT *,'error 7'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

END PROGRAM covinflate2nc
