PROGRAM aodnnr_hl_final

!  to calculate average and diagonalize 
!  covariance and correlation matrices for nasa's nnr multichannel 
!  aods usign H-L method using output produced with concat_hl.sh
!  MP, Nov 2018

  USE netcdf
  USE lapack95
  USE tension_mod
  
  USE module_misc
  USE module_utils, ONLY: sphere_distance
  USE module_netcdf_handles
  USE module_function_fitting

  IMPLICIT NONE

  INTEGER, ALLOCATABLE, DIMENSION(:) :: covar_index
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: counts_in_bins
  REAL, ALLOCATABLE, DIMENSION(:) :: distance_bins, channels
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: covar
  REAL, ALLOCATABLE, DIMENSION(:,:) :: covar_mean
  REAL, ALLOCATABLE, DIMENSION(:,:) :: ratios_in_bins_total
  INTEGER, ALLOCATABLE, DIMENSION(:) :: counts_in_bins_total

  INTEGER :: nbins,ncount,ncovar,nchannels,nrecords

  INTEGER :: stderr = 0
  INTEGER :: Read_Status
  INTEGER :: Open_Status

  CHARACTER (len=max_name_length)  :: input_dir,input_file,output_dir,output_file

  INTEGER, DIMENSION(max_dims) :: dimids,dims
  CHARACTER(len = max_name_length) :: fname_in,fname_out
  
  INTEGER :: ncid,status,varid,numdims,i,j,k,l,nvars
  CHARACTER(len = max_name_length) :: aname, varname
  INTEGER, DIMENSION(max_vars) :: varids

  INTEGER :: mcid,dim_nchannels_id,dim_ncovar_id,dim_nbins_id,&
       &channels_id,covar_id,covar_index_id,distance_bins_id,count_in_bins_id,&
       &gaussian_id,soar_id,foar_id,sboar_id,toar_id,fboar_id,tension_id

  REAL(kind=8), ALLOCATABLE, DIMENSION(:) :: lengthscales_gauss, lengthscales_soar,&
       &lengthscales_foar,lengthscales_sboar,lengthscales_toar,lengthscales_fboar
  REAL, ALLOCATABLE, DIMENSION(:,:) :: gaussian_values,soar_values,foar_values,&
       &sboar_values,toar_values,fboar_values,tension_values
  INTEGER, ALLOCATABLE, DIMENSION(:) :: icount,irecs
  CHARACTER(len = max_name_length) :: attname,attvalue
  REAL :: dx

  CHARACTER(len = max_name_length) :: varname_channel="channels",&
       &varname_covar="covar",varname_covar_index="covar_index",&
       &varname_distance_bins="distance_bins",varname_counts_in_bins="counts_in_bins"

  CHARACTER(len = max_name_length) :: satellite, obstype, fcst_hour, logaod, epsilon
  REAL :: b_boar

  INTEGER, PARAMETER :: dp = KIND(1.0d0)
  REAL(kind=dp), DIMENSION(:), ALLOCATABLE :: ddistance_bins,dcovar,yp,sigma
  INTEGER :: ier,siger

  NAMELIST /record_inout/ &
       &input_dir, input_file, output_dir, output_file, b_boar
  
  OPEN (unit=unit_namelist, file = "namelist.modis_nnr_hl_final", &
       &status="old",action = "read",iostat=open_status)
  
  IF (open_status /= 0) THEN
     WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_hl_final)"
     STOP
  END IF
  
  WRITE(stderr,*) 'Reading namelist.modis_nnr_hl_final'
  READ (unit_namelist, NML=record_inout, IOSTAT=Read_Status)

  IF (Read_Status /= 0) THEN
     WRITE(stderr,*) 'Error reading record_inout of namelist.modis_nnr_hl'
     STOP
  END IF

  fname_in=TRIM(input_dir)//'/'//input_file
  fname_out=TRIM(output_dir)//'/'//output_file

  status = nf90_open(fname_in, nf90_nowrite, ncid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varids(ncid, nvars, varids)
  IF (status .NE. nf90_noerr) CALL handle_err(status)
  
  DO i=1,nvars

     status = nf90_inquire_variable(ncid, varids(i),varname)
     IF (status .NE. nf90_noerr) CALL handle_err(status)
     
     IF (INDEX(TRIM(varname),TRIM(varname_channel)) > 0) THEN 
        obstype=TRIM(varname(LEN_TRIM(varname_channel)+2:))
        varname_channel=varname
        EXIT
     ENDIF

  ENDDO

  status = nf90_inq_varid(ncid, varname_channel, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inquire_variable(ncid, varid, aname, ndims=numdims,dimids=dimids(:))
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  DO i=1,numdims
     status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
     IF (status .NE. nf90_noerr) CALL handle_err(status)
  ENDDO
  
  nchannels=dims(1)

  ALLOCATE(channels(nchannels))

  status = nf90_get_var(ncid,varid,channels)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid, varname_covar, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inquire_variable(ncid, varid, aname, ndims=numdims,dimids=dimids(:))
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  DO i=1,numdims
     status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
     IF (status .NE. nf90_noerr) CALL handle_err(status)
  ENDDO

  ncovar=dims(1)
  nbins=dims(2)
  nrecords=dims(3)

  ALLOCATE(covar(ncovar,nbins,nrecords))

  status = nf90_get_var(ncid,varid,covar)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  attname="Forecast_hour"
  status = nf90_get_att(ncid, varid, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  fcst_hour=TRIM(attvalue)

!  attname="satellite"
!  status = nf90_get_att(ncid, varid, TRIM(attname),attvalue)
!  IF (status /= nf90_noerr) CALL handle_err(status)

!  satellite=TRIM(attvalue)

  attname="logaod"
  status = nf90_get_att(ncid, varid, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)
  logaod=TRIM(attvalue)

  attname="epsilon"
  status = nf90_get_att(ncid, varid, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)
  epsilon=TRIM(attvalue)

  status = nf90_inq_varid(ncid, varname_covar_index, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  ALLOCATE(covar_index(ncovar))

  status = nf90_get_var(ncid,varid,covar_index)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid, varname_distance_bins, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  ALLOCATE(distance_bins(nbins))

  status = nf90_get_var(ncid,varid,distance_bins)
  IF (status .NE. nf90_noerr) CALL handle_err(status)


  status = nf90_inq_varid(ncid, varname_counts_in_bins, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  ALLOCATE(counts_in_bins(nbins,nrecords))

  status = nf90_get_var(ncid,varid,counts_in_bins)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_close(ncid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

!end ncdf read

  ALLOCATE(lengthscales_gauss(ncovar), lengthscales_soar(ncovar),&
       &lengthscales_foar(ncovar),lengthscales_sboar(ncovar),lengthscales_toar(ncovar),&
       &lengthscales_fboar(ncovar))

  ALLOCATE(ddistance_bins(nbins),dcovar(nbins-1),yp(nbins-1),&
       &sigma(nbins-1),tension_values(ncovar,nbins))

  ddistance_bins=distance_bins

  lengthscales_gauss=0.
  lengthscales_soar=0.
  lengthscales_foar=0.
  lengthscales_sboar=0.
  lengthscales_toar=0.
  lengthscales_fboar=0.

  ALLOCATE(irecs(nrecords))
  irecs=1

  WHERE(ANY(covar(:,1,:) < covar(:,2,:),dim=1) .OR.&
       &ANY(covar(:,2,:) < covar(:,3,:),dim=1)) irecs=0

!  irecs=0
!  WHERE(covar(1,1,:) > covar(1,2,:) .AND. covar(1,2,:) > covar(1,3,:)) irecs=1
!  PRINT *,irecs
!  STOP

  ALLOCATE(counts_in_bins_total(nbins),ratios_in_bins_total(nbins,nrecords))

  FORALL (i=1:nbins)
     counts_in_bins_total(i)=SUM(counts_in_bins(i,:)*irecs)
  END FORALL

!  counts_in_bins_total=SUM(counts_in_bins,dim=2)

  FORALL (i=1:nbins)
     ratios_in_bins_total(i,1:nrecords)=irecs(1:nrecords)*&
          &REAL(counts_in_bins(i,1:nrecords))/REAL(counts_in_bins_total(i))
  END FORALL

  ALLOCATE(covar_mean(ncovar,nbins))

  FORALL (i=1:ncovar,j=1:nbins)
!     covar_mean(i,j)=SUM(covar(i,j,:))/nrecords
     covar_mean(i,j)=SUM(covar(i,j,:)*ratios_in_bins_total(j,:))
  END FORALL


  ALLOCATE(icount(nbins))
  icount(1)=0
  icount(2:)=counts_in_bins_total(2:)

  DO i=1,ncovar

     CALL gauss_scale_length(lengthscales_gauss(i), nbins, icount, covar_mean(i,:))
     lengthscales_soar(i)=lengthscales_gauss(i)
     CALL soar_scale_length(lengthscales_soar(i), nbins, icount, covar_mean(i,:))
     lengthscales_foar(i)=lengthscales_soar(i)     
     CALL foar_scale_length(lengthscales_foar(i), nbins, icount, covar_mean(i,:))
     lengthscales_sboar(i)=lengthscales_soar(i)
     CALL sboar_scale_length(lengthscales_sboar(i), nbins, icount, covar_mean(i,:), b_boar)
     lengthscales_toar(i)=lengthscales_soar(i)     
     CALL toar_scale_length(lengthscales_toar(i), nbins, icount, covar_mean(i,:))
     lengthscales_fboar(i)=lengthscales_foar(i)     
     CALL fboar_scale_length(lengthscales_fboar(i), nbins, icount, covar_mean(i,:), b_boar)


     WRITE(*,'(a10,i5,1x,5(a8,f7.3,1x))') 'ch_index: ',covar_index(i),&
          &'l_gauss=',lengthscales_gauss(i),&
          &'l_soar=',lengthscales_soar(i),&
          &'l_foar=',lengthscales_foar(i),&
          &'l_sboar=',lengthscales_sboar(i),&
          &'l_toar=',lengthscales_toar(i),&
          &'l_fboar=',lengthscales_fboar(i)
     
  ENDDO

  icount(1)=counts_in_bins_total(1)

  ALLOCATE(gaussian_values(ncovar,nbins),soar_values(ncovar,nbins),&
       &foar_values(ncovar,nbins),sboar_values(ncovar,nbins),toar_values(ncovar,nbins),&
       &fboar_values(ncovar,nbins))

  dx=distance_bins(2)-distance_bins(1)

  DO i=1,ncovar

     DO j=2,nbins
        gaussian_values(i,j)=EXP(-distance_bins(j)**2/&
             &((8.*lengthscales_gauss(i)*dx)**2))
        soar_values(i,j)=(1.+distance_bins(j)/(lengthscales_soar(i)*dx))*&
             &EXP(-distance_bins(j)/(lengthscales_soar(i)*dx))
        foar_values(i,j)=EXP(-distance_bins(j)/(lengthscales_foar(i)*dx))
        sboar_values(i,j)=(1.+(distance_bins(j)/(lengthscales_sboar(i)*dx))**b_boar)*&
                &EXP(-(distance_bins(j)/(lengthscales_sboar(i)*dx))**b_boar)
        toar_values(i,j)=(1.+distance_bins(j)/(lengthscales_toar(i)*dx)+&
             &((distance_bins(j)/(lengthscales_toar(i)*dx))**2)/3.)*&
             &EXP(-distance_bins(j)/(lengthscales_toar(i)*dx))
        fboar_values(i,j)=EXP(-(distance_bins(j)/(lengthscales_fboar(i)*dx))**b_boar)
     ENDDO

     gaussian_values(i,1)=covar_mean(i,2)*gaussian_values(i,2)/&
          &(1.-.125/(lengthscales_gauss(i)**2)*&
          &EXP(-1./(32.*lengthscales_gauss(i)**2)))
     gaussian_values(i,2:nbins)=gaussian_values(i,1)*&
          &gaussian_values(i,2:nbins)
     
     soar_values(i,1)=covar_mean(i,2)*soar_values(i,2)/&
          &(1.-0.5*EXP(-0.5/lengthscales_soar(i))/lengthscales_soar(i)**2)
     soar_values(i,2:nbins)=soar_values(i,1)*&
          &soar_values(i,2:nbins)

     foar_values(i,1)=covar_mean(i,2)*foar_values(i,2)/&
          &(1.-EXP(-0.5/lengthscales_foar(i))/lengthscales_foar(i))
     foar_values(i,2:nbins)=foar_values(i,1)*&
          &foar_values(i,2:nbins)

     sboar_values(i,1)=covar_mean(i,2)*sboar_values(i,2)/&
          &(1.-2.*b_boar*EXP(-(0.5*lengthscales_sboar(i))**b_boar)/&
          &(2.*lengthscales_sboar(i))**(2.*b_boar))
     sboar_values(i,2:nbins)=sboar_values(i,1)*&
          &sboar_values(i,2:nbins)

     toar_values(i,1)=covar_mean(i,2)*toar_values(i,2)/&
          &(1.-EXP(-0.5/lengthscales_toar(i))*1./(6.*lengthscales_toar(i)**2)*&
          &(-1.+0.5/lengthscales_toar(i)))
     toar_values(i,2:nbins)=toar_values(i,1)*&
          &toar_values(i,2:nbins)

     fboar_values(i,1)=covar_mean(i,2)*fboar_values(i,2)/&
          &(1.-b_boar/((2.*lengthscales_fboar(i))**b_boar*lengthscales_fboar(i))*&
          &EXP(-1./(2.*lengthscales_fboar(i))**b_boar))
     fboar_values(i,2:nbins)=fboar_values(i,1)*&
          &fboar_values(i,2:nbins)
     
     dcovar(:)=covar_mean(i,2:)
     CALL tspsi(nbins-1,ddistance_bins(2:),dcovar,yp,sigma,ier,siger)
     
     DO j=1,nbins
        tension_values(i,j)=hval(ddistance_bins(j),nbins-1,&
             &ddistance_bins(2:),dcovar,yp,sigma,ier)
     ENDDO

  ENDDO

  status = nf90_create(fname_out,nf90_write,mcid)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_dim(mcid,'nbins',NF90_UNLIMITED,dim_nbins_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_dim(mcid,varname_channel,&
       &nchannels,dim_nchannels_id)
  IF (status /= nf90_noerr) CALL handle_err(status)
  
  status = nf90_def_var(mcid,varname_channel,NF90_REAL,&
       &(/dim_nchannels_id/),channels_id)
  IF (status /= nf90_noerr) CALL handle_err(status)
  
  attname="unit"
  attvalue="nm"
  status = nf90_put_att(mcid, channels_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_dim(mcid,'ncovar',&
       &ncovar,dim_ncovar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'covar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),covar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

!  attname="satellite"
!  attvalue="Aqua MYD04 + Terra MOD04"
!  attvalue=TRIM(satellite)
!  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
!  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="Forecast_hour"
  attvalue=TRIM(fcst_hour)
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="logaod"
  attvalue=TRIM(logaod)
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="epsilon"
  attvalue=TRIM(epsilon)
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="boar-exponent"
  WRITE(attvalue,'(f5.3)')b_boar
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'Gaussian',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),gaussian_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'soar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),soar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'foar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),foar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'sboar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),sboar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'toar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),toar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'fboar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),fboar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'tension',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),tension_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'covar_index',NF90_INT,&
       &(/dim_ncovar_id/),covar_index_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'distance_bins',NF90_REAL,&
       &(/dim_nbins_id/),distance_bins_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="unit"
  attvalue="km"
  status = nf90_put_att(mcid, distance_bins_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'counts_in_bins',NF90_INT,&
       &(/dim_nbins_id/),count_in_bins_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_enddef(mcid)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,channels_id,channels)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,distance_bins_id,distance_bins)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,count_in_bins_id,icount)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,covar_index_id,covar_index)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,covar_id,covar_mean)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,gaussian_id,gaussian_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,soar_id,soar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,foar_id,foar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,sboar_id,sboar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,toar_id,toar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,fboar_id,fboar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,tension_id,tension_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_close(mcid)
  IF (status /= nf90_noerr) CALL handle_err(status)

  DEALLOCATE(channels,covar,covar_index,covar_mean,&
       &gaussian_values,soar_values,foar_values,sboar_values,toar_values,fboar_values,&
       &icount,irecs,counts_in_bins_total,ratios_in_bins_total,&
       &counts_in_bins,distance_bins)
  
  DEALLOCATE(tension_values,dcovar,yp,sigma)

END PROGRAM aodnnr_hl_final

