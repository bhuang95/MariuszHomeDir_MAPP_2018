PROGRAM be_tracer2nc

!to convert lscale files for tracers to netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: nvars=5,nvardims=4
  CHARACTER(len=12), DIMENSION(nvars) :: varnames = &
       &(/'player','stdev','lscalex','lscaley','lscalez'/)
  CHARACTER(len=20), DIMENSION(nvardims) ::&
       &  dname=(/'west_east','south_north','bottom_top','Time'/)
  CHARACTER(len=20) :: date

  INTEGER, DIMENSION(nvardims) :: dname_id
  INTEGER, DIMENSION(nvars) :: var_id

  INTEGER, DIMENSION(nvardims) :: vardims,start_dims
  INTEGER :: iargc,ndim,inunit=51,i,j,k,mcid,status
  CHARACTER(len=250) :: fname_in_base,fname_in_tracer,fname_out

  INTEGER :: nlons,nlats,nlevs,nanals 
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: pressl,stdev,lscalex,lscaley,lscalez
  REAL, ALLOCATABLE, DIMENSION(:)  :: lats

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: reverse_ns

  CALL getarg(1,fname_in_base)
  CALL getarg(2,fname_in_tracer)
  CALL getarg(3,fname_out)
  
  OPEN(unit=inunit,form='unformatted',file=fname_in_base)
  READ(inunit) nlons,nlats,nlevs,nanals
  ALLOCATE(lats(nlats),&
       &pressl(nlons,nlats,nlevs),stdev(nlons,nlats,nlevs),&
       &lscalex(nlons,nlats,nlevs),lscaley(nlons,nlats,nlevs),&
       &lscalez(nlons,nlats,nlevs),reverse_ns(nlons,nlats,nlevs))
  READ(inunit)lats

  READ(inunit)pressl
  READ(inunit)
  CLOSE(inunit)
  
  vardims(1)=nlons
  vardims(2)=nlats
  vardims(3)=nlevs
  vardims(4)=1

  OPEN(unit=inunit,form='unformatted',file=fname_in_tracer)
  READ(inunit) nlons,nlats,nlevs,nanals

  IF (vardims(1) /=nlons .OR. vardims(2) /= nlats .OR. vardims(3) /= nlevs) THEN
     PRINT *,'unmatched dimensions in base and tracer files - Stopping'
     STOP
  ENDIF

  READ(inunit)stdev
  READ(inunit)lscalex
  READ(inunit)lscaley
  READ(inunit)lscalez
  CLOSE(inunit)

  status = nf_create(fname_out, 0, mcid)
  IF (status /= nf_noerr) THEN 
     PRINT *,'error 0'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF
  
  DO i=1,nvardims
     IF ( dname(i) == "Time" ) THEN
        status = nf_def_dim(mcid, dname(i), NF_UNLIMITED, dname_id(i))
        IF (status /= nf_noerr) THEN 
           PRINT *, 'Error: ', nf_strerror(status)
        ENDIF
     ELSE
        status = nf_def_dim(mcid, dname(i), vardims(i), dname_id(i))
        IF (status /= nf_noerr) THEN 
           PRINT *, 'Error: ', nf_strerror(status)
        ENDIF
     ENDIF
  ENDDO

  start_dims = 1
  
  DO i = 1, nvars
     status = nf_def_var(mcid, varnames(i), NF_REAL, nvardims, &
          &dname_id,var_id(i))
     IF (status /= nf_noerr) THEN 
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF
  ENDDO

  date='nodate for now'

  status=nf_put_att_text(mcid, nf_global,'Date',20,date(1:20))

  IF (status /= nf_noerr) THEN
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  status = nf_enddef(mcid)

  IF (status /= nf_noerr) THEN 
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  i=1
  DO j=1,nlats
     reverse_ns(:,j,:)=pressl(:,nlats-j+1,:)
  ENDDO
  status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
       &reverse_ns)
  IF (status /= nf_noerr) THEN 
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  i=2
  DO j=1,nlats
     reverse_ns(:,j,:)=stdev(:,nlats-j+1,:)
  ENDDO
  status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
       &reverse_ns)
  IF (status /= nf_noerr) THEN 
     PRINT *,'error 5'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF



  i=3
  DO j=1,nlats
     reverse_ns(:,j,:)=lscalex(:,nlats-j+1,:)
  ENDDO
  status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
       &reverse_ns)
  IF (status /= nf_noerr) THEN 
     PRINT *,'error 5'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  i=4
  DO j=1,nlats
     reverse_ns(:,j,:)=lscaley(:,nlats-j+1,:)
  ENDDO
  status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
       &reverse_ns)
  IF (status /= nf_noerr) THEN 
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  i=5
  DO j=1,nlats
     reverse_ns(:,j,:)=lscalez(:,nlats-j+1,:)
  ENDDO
  status = nf_put_vara_real(mcid,var_id(i),start_dims,vardims,&
       &reverse_ns)
  IF (status /= nf_noerr) THEN 
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  status = nf_close(mcid)

  IF (status /= nf_noerr) THEN
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF

  DEALLOCATE(lats,pressl,stdev,lscalex,lscaley,lscalez,reverse_ns)

END PROGRAM be_tracer2nc
