PROGRAM aodnnr_hl

!  to calculate correlation matrix for nasa's nnr multichannel aods usign H-L method
!  for single time and satellite processing
!  MP, Oct 2018

  USE netcdf
  
  USE module_misc
  USE module_utils, ONLY: sphere_distance
  USE module_netcdf_handles
  USE module_function_fitting

  IMPLICIT NONE

  REAL, PARAMETER :: distance_max=1000. 
  INTEGER, PARAMETER :: nbins=40
!also tested distance_max=1500. nbins=60 with similar results

  REAL, PARAMETER :: dx=distance_max/REAL(nbins)
  REAL, PARAMETER :: distance_min=0.5*dx 

  INTEGER :: ii
  REAL, DIMENSION(0:nbins), PARAMETER :: &
       &distance_right_edge_bins = (/ (MAX(REAL(ii)*dx+0.5*dx,0.),ii=0,nbins) /),&
       &distance_center_bins = (/ (REAL(ii)*dx,ii=0,nbins) /)

  TYPE aod_obs_model
     CHARACTER(len=max_name_length) :: satellite
     CHARACTER(len=max_name_length) :: obstype !ocean, land, or deep
     REAL, ALLOCATABLE :: channels(:)
     INTEGER, ALLOCATABLE :: dtimes(:) !in minutes time_obs - time_model 
     REAL, ALLOCATABLE :: lats(:)
     REAL, ALLOCATABLE :: lons(:)
     REAL, ALLOCATABLE :: aod_obs(:,:) ! obs for all channels
     REAL, ALLOCATABLE :: aod_model(:,:) ! model for all channels
  END TYPE aod_obs_model
  
  INTEGER :: stderr = 0
  INTEGER :: Read_Status
  INTEGER :: Open_Status

  INTEGER, DIMENSION(0:nbins) :: ibins, icount, counts
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: covar
  REAL(kind=8), ALLOCATABLE, DIMENSION(:,:) :: lengthscales_gauss, lengthscales_soar,&
       &lengthscales_foar, lengthscales_boar
  INTEGER,  ALLOCATABLE, DIMENSION(:) :: covar_index
  REAL, ALLOCATABLE, DIMENSION(:,:) :: covar_values,gaussian_values,soar_values,&
       &foar_values,boar_values

  REAL :: low_bound,high_bound

  CHARACTER (len=max_name_length)  :: input_dir,input_file,output_dir,output_file_covar

  LOGICAL :: logaod=.TRUE., calc_scales=.FALSE.
  REAL :: epsilon=0. ! offset for AOD when logaod
  REAL :: b_boar=1. ! exponent - order of the SOAR regression function
  INTEGER, DIMENSION(max_dims) :: dimids,dims
  CHARACTER(len = max_name_length) :: fname_in,fname_out_covar
  
  REAL, ALLOCATABLE, DIMENSION(:,:) :: obs,model
  REAL, ALLOCATABLE, DIMENSION(:) :: channels,lats,lons

  INTEGER :: ncid,status,varid,numdims,i,j,k,l,nvars,nchannels,nobs
  CHARACTER(len = max_name_length) :: aname, varname
  INTEGER, DIMENSION(max_vars) :: varids

  INTEGER :: mcid,dim_nchannels_id,dim_ncovar_id,dim_nbins_id,&
       &channels_id,covar_id,covar_index_id,distance_bins_id,count_in_bins_id,&
       &gaussian_id,soar_id,foar_id,boar_id
  CHARACTER(len = max_name_length) :: attname,attvalue

  REAL :: distance

  CHARACTER(len = max_name_length) :: varname_channel="channels",&
       &varname_lat="lat",varname_lon="lon",varname_obs="AOD_obs",&
       &varname_model="AOD_model"

  CHARACTER(len = max_name_length) :: satellite, obstype,fcst_hour

  NAMELIST /record_inout/ &
       &input_dir, input_file, output_dir, output_file_covar,logaod,epsilon,b_boar,&
       &calc_scales,low_bound,high_bound
  
  OPEN (unit=unit_namelist, file = "namelist.modis_nnr_hl_intervals", &
       &status="old",action = "read",iostat=open_status)
  
  IF (open_status /= 0) THEN
     WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_hl)"
     STOP
  END IF
  
  WRITE(stderr,*) 'Reading namelist.modis_nnr_hl'
  READ (unit_namelist, NML=record_inout, IOSTAT=Read_Status)

  IF (Read_Status /= 0) THEN
     WRITE(stderr,*) 'Error reading record_inout of namelist.modis_nnr_hl'
     STOP
  END IF

  fname_in=TRIM(input_dir)//'/'//input_file
  fname_out_covar=TRIM(output_dir)//'/'//output_file_covar

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

  status = nf90_inq_varid(ncid, varname_obs, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inquire_variable(ncid, varid, aname, ndims=numdims,dimids=dimids(:))
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  dims=1
  
  DO i=1,numdims
     status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
     IF (status .NE. nf90_noerr) CALL handle_err(status)
  ENDDO

  nchannels=dims(1)
  nobs=dims(2)

  ALLOCATE(channels(nchannels),lats(nobs),lons(nobs),&
       &obs(nchannels,nobs),model(nchannels,nobs),covar(0:nbins,nchannels,nchannels))

  status = nf90_get_var(ncid,varid,obs)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_att(ncid,varid,'satellite',satellite)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid, varname_model, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_att(ncid,varid,'Forecast_hour',fcst_hour)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_var(ncid,varid,model)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid, varname_lat, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_var(ncid,varid,lats)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid, varname_lon, varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_var(ncid,varid,lons)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_inq_varid(ncid,varname_channel,varid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_var(ncid,varid,channels)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_close(ncid)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  lats=lats*d2r
  lons=lons*d2r


  ALLOCATE(lengthscales_gauss(nchannels,nchannels),&
       &lengthscales_soar(nchannels,nchannels),&
       &lengthscales_foar(nchannels,nchannels),&
       &lengthscales_boar(nchannels,nchannels))
  
  lengthscales_gauss=0.
  lengthscales_soar=0.
  lengthscales_foar=0.
  lengthscales_boar=0.
  covar=0.
  
  PRINT *,'Calculating covariances for all channels'

!$OMP PARALLEL DEFAULT (NONE) &
!$OMP SHARED (nchannels,covar,b_boar,lengthscales_gauss,lengthscales_soar,&
!$OMP lengthscales_foar, lengthscales_boar, calc_scales,&
!$OMP distance_center_bins,obs,model,nobs,lats,lons,logaod,epsilon,counts,&
!$OMP low_bound,high_bound) &

!$OMP PRIVATE (k,l,i,j,ii,ibins,distance,icount) 

!$OMP DO COLLAPSE(2)

  DO k=1,nchannels
     DO l=1,nchannels
!collapse much faster despite longer loop, 
!also averaged covariances for (k,l) and (l,k) as differ due to single precision
!if not collapse or serial
!DO l=k,nchannels

!        PRINT *,'Calculating covariance for channels ',k,l

        icount=0
        ibins=0

        DO i=1,nobs
           DO j=i,nobs

              IF ((obs(k,i) < low_bound .OR. obs(k,i) >= high_bound) .AND. .NOT. logaod) &
                   &CYCLE  

              IF (  (ABS(lats(i)-lats(j)) < small_value ) .AND. &
                   &(ABS(lons(i)-lons(j)) < small_value ) ) THEN 
                 ii=0
                 IF (logaod) THEN
                    covar(ii,k,l)=covar(ii,k,l)+&
                         &(LOG(obs(k,i)+epsilon)-LOG(model(k,i)+epsilon))*&
                         &(LOG(obs(l,j)+epsilon)-LOG(model(l,j)+epsilon))
                 ELSE    
                    covar(ii,k,l)=covar(ii,k,l)+(obs(k,i)-model(k,i))*(obs(l,j)-model(l,j))
                 ENDIF
              ELSE 
                 distance=sphere_distance(lats(i),lats(j),lons(i),lons(j))
                 IF ( (distance < distance_min) .or. (distance > distance_max) ) THEN
                    CYCLE
                 ELSE 
                    WHERE (distance_right_edge_bins <= distance) ibins=1
                    ii=SUM(ibins)
                    IF (logaod) THEN
                       covar(ii,k,l)=covar(ii,k,l)+&
                            &(LOG(obs(k,i)+epsilon)-LOG(model(k,i)+epsilon))*&
                            &(LOG(obs(l,j)+epsilon)-LOG(model(l,j)+epsilon))
                    ELSE                    
                       covar(ii,k,l)=covar(ii,k,l)+(obs(k,i)-model(k,i))*(obs(l,j)-model(l,j))
                    ENDIF
                 ENDIF
              ENDIF

              icount(ii)=icount(ii)+1

              ibins=0

           ENDDO
        ENDDO

        WHERE (icount > 0) 
           covar(:,k,l)=covar(:,k,l)/REAL(icount)
        ELSEWHERE
           covar(:,k,l)=0.
        END WHERE

!        DO i=0,nbins
!           PRINT *,i,covar(i,k,l),distance_center_bins(i),icount(i)
!        ENDDO

        icount(0)=0

        IF (calc_scales) THEN

           CALL gauss_scale_length(lengthscales_gauss(k,l), nbins, icount, covar(:,k,l))
           lengthscales_soar(k,l)=lengthscales_gauss(k,l)
           CALL soar_scale_length(lengthscales_soar(k,l), nbins, icount, covar(:,k,l))
           lengthscales_foar(k,l)=lengthscales_soar(k,l)
           CALL foar_scale_length(lengthscales_foar(k,l), nbins, icount, covar(:,k,l))
           lengthscales_boar(k,l)=lengthscales_soar(k,l)
           CALL sboar_scale_length(lengthscales_boar(k,l), nbins, icount, covar(:,k,l), b_boar)
           

!alternatively 
!        CALL gauss_scale_length(lengthscales_gauss(k,l), nbins-1, icount(1:), covar(1:,k,l))
!        lengthscales_soar(k,l)=lengthscales_gauss(k,l)
!        CALL soar_scale_length(lengthscales_soar(k,l), nbins-1, icount(1:), covar(1:,k,l))
!        lengthscales_soar(k,l)=0.

           WRITE(*,'(a5,2(i1,1x),1x,4(a8,f9.5,1x))') 'ch: ',k,l,&
                &'l_gauss=',lengthscales_gauss(k,l),&
                &'l_soar=',lengthscales_soar(k,l),&
                &'l_foar=',lengthscales_foar(k,l),&
                &'l_boar=',lengthscales_boar(k,l)
           
        ENDIF

        counts=icount

     ENDDO
  ENDDO

!$OMP END DO

!$OMP END PARALLEL

  icount=counts

  icount(0)=nobs

  ALLOCATE(covar_index(nchannels*(nchannels+1)/2),&
       &covar_values((nchannels*(nchannels+1)/2),0:nbins))

  IF (calc_scales) THEN
     ALLOCATE(gaussian_values((nchannels*(nchannels+1)/2),0:nbins),&
          &soar_values((nchannels*(nchannels+1)/2),0:nbins),&
          &foar_values((nchannels*(nchannels+1)/2),0:nbins),&
          &boar_values((nchannels*(nchannels+1)/2),0:nbins))
  ENDIF

  ii=0
  
  DO k=1,nchannels
     DO l=k,nchannels

        ii=ii+1
        covar_index(ii)=k*100+l
        covar_values(ii,:)=0.5*(covar(:,k,l)+covar(:,l,k))

!correct for inaccuracies because of single precision in calculating covariance
!by averaging scales

        IF (calc_scales) THEN

           lengthscales_gauss(k,l)=0.5*(lengthscales_gauss(k,l)+lengthscales_gauss(l,k))
           lengthscales_soar(k,l)=0.5*(lengthscales_soar(k,l)+lengthscales_soar(l,k))
           lengthscales_foar(k,l)=0.5*(lengthscales_foar(k,l)+lengthscales_foar(l,k))
           lengthscales_boar(k,l)=0.5*(lengthscales_boar(k,l)+lengthscales_boar(l,k))
           
           DO i=1,nbins
              gaussian_values(ii,i)=EXP(-distance_center_bins(i)**2/&
                   &((8.*lengthscales_gauss(k,l)*dx)**2))
              soar_values(ii,i)=(1.+distance_center_bins(i)/(lengthscales_soar(k,l)*dx))*&
                   &EXP(-distance_center_bins(i)/(lengthscales_soar(k,l)*dx))
              foar_values(ii,i)=EXP(-distance_center_bins(i)/(lengthscales_foar(k,l)*dx))
              boar_values(ii,i)=(1.+(distance_center_bins(i)/(lengthscales_boar(k,l)*dx))**b_boar)*&
                   &EXP(-(distance_center_bins(i)/(lengthscales_boar(k,l)*dx))**b_boar)
              
           ENDDO

           gaussian_values(ii,0)=covar_values(ii,1)*gaussian_values(ii,1)/&
                &(1.-.125/(lengthscales_gauss(k,l)**2)*&
                &EXP(-1./(32.*lengthscales_gauss(k,l)**2)))
           gaussian_values(ii,1:nbins)=gaussian_values(ii,0)*&
                &gaussian_values(ii,1:nbins)
           
           soar_values(ii,0)=covar_values(ii,1)*soar_values(ii,1)/&
                &(1.-0.5*EXP(-0.5/lengthscales_soar(k,l))/lengthscales_soar(k,l)**2)
           soar_values(ii,1:nbins)=soar_values(ii,0)*&
                &soar_values(ii,1:nbins)
           
           foar_values(ii,0)=covar_values(ii,1)*foar_values(ii,1)/&
                &(1.-EXP(-0.5/lengthscales_foar(k,l))/lengthscales_foar(k,l))
           foar_values(ii,1:nbins)=foar_values(ii,0)*&
                &foar_values(ii,1:nbins)
           
           boar_values(ii,0)=covar_values(ii,1)*boar_values(ii,1)/&
                &(1.-2.*b_boar*EXP(-(0.5*lengthscales_boar(k,l))**b_boar)/&
                &(2.*lengthscales_boar(k,l))**(2.*b_boar))
           boar_values(ii,1:nbins)=boar_values(ii,0)*&
                &boar_values(ii,1:nbins)
        ENDIF

     ENDDO
  ENDDO

  status = nf90_create(fname_out_covar,nf90_write,mcid)
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
       &(nchannels*(nchannels+1))/2,dim_ncovar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_def_var(mcid,'covar',NF90_REAL,&
       &(/dim_ncovar_id,dim_nbins_id/),covar_id)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="nobs"
  WRITE(attvalue,'(i10)')nobs
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)
  
  attname="satellite"
  attvalue=TRIM(satellite)
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="Forecast_hour"
  attvalue=TRIM(fcst_hour)
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="logaod"
  IF (logaod) THEN
     attvalue="T"
  ELSE
     attvalue="F"
  ENDIF
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="epsilon"
  WRITE(attvalue,'(f10.8)')epsilon
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  attname="boar-exponent"
  WRITE(attvalue,'(f5.3)')b_boar
  status = nf90_put_att(mcid, covar_id, TRIM(attname),attvalue)
  IF (status /= nf90_noerr) CALL handle_err(status)

  IF (calc_scales) THEN

     status = nf90_def_var(mcid,'Gaussian',NF90_REAL,&
          &(/dim_ncovar_id,dim_nbins_id/),gaussian_id)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_def_var(mcid,'soar',NF90_REAL,&
          &(/dim_ncovar_id,dim_nbins_id/),soar_id)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_def_var(mcid,'foar',NF90_REAL,&
          &(/dim_ncovar_id,dim_nbins_id/),foar_id)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_def_var(mcid,'boar',NF90_REAL,&
          &(/dim_ncovar_id,dim_nbins_id/),boar_id)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
  ENDIF

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

  status = nf90_put_var(mcid,distance_bins_id,distance_center_bins)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,count_in_bins_id,icount)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,covar_index_id,covar_index)
  IF (status /= nf90_noerr) CALL handle_err(status)

  status = nf90_put_var(mcid,covar_id,covar_values)
  IF (status /= nf90_noerr) CALL handle_err(status)

  IF (calc_scales) THEN

     status = nf90_put_var(mcid,gaussian_id,gaussian_values)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_put_var(mcid,soar_id,soar_values)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_put_var(mcid,foar_id,foar_values)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
     status = nf90_put_var(mcid,boar_id,boar_values)
     IF (status /= nf90_noerr) CALL handle_err(status)
     
  ENDIF

  status = nf90_close(mcid)
  IF (status /= nf90_noerr) CALL handle_err(status)

  DEALLOCATE(channels,lats,lons,obs,model,covar,covar_index,covar_values)

  IF (calc_scales) DEALLOCATE(gaussian_values,soar_values,foar_values,boar_values)
  
END PROGRAM aodnnr_hl

