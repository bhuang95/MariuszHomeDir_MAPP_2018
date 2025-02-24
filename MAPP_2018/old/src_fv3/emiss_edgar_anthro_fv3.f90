PROGRAM emiss_edgar_anthro_fv3

!MZP to convert EDGAR hdf to netcdf with format for fv3 interpolation
!emissions are surface only dims nlon * nlat * ntimes * nspiecies
  
  USE hdf5
  USE netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

!neglect agriculture (contains only NH3 species) for GOCART
  INTEGER, PARAMETER :: nspecies_edgar=5,ndims=4, nsectors=4

  CHARACTER(len=30), DIMENSION(nsectors) :: sectors=(/&
       &'ENERGY','INDUSTRY','RESIDENTIAL','TRANSPORT'/)
!       &'AGRICULTURE','ENERGY','INDUSTRY','RESIDENTIAL','TRANSPORT'/)


  CHARACTER(len=32), DIMENSION(nspecies_edgar), PARAMETER :: &
       &spc_names_edgar= (/'BC','OC','PM2.5','PM10','SO2'/)

  CHARACTER(len=32), DIMENSION(nspecies_edgar), PARAMETER :: &
       &spc_names_out= (/'ea_bc','ea_oc','ea_pm25','ea_pm10','ea_so2'/)

  CHARACTER(len=30) :: prefix='EDGAR-HTAP',suffix='2010.h5'

  CHARACTER(len=32) :: dsetname

  INTEGER(HID_T) :: file_id, dset_id, space_id
  INTEGER(HSIZE_T) :: data_dims(ndims), maxdims(ndims)
  INTEGER :: hdferr

  CHARACTER(len=80) :: filenamehdf,&
       &filenamencdf="emiss_edgar_anthro_fv3.nc"

  INTEGER :: nx,ny,i,j,k
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:):: RAWsrc
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:):: src_dummy
  REAL :: dlon,dlat
  INTEGER :: nlon,nlat
  REAL, ALLOCATABLE, DIMENSION(:) :: longEdgar,latEdgar
  INTEGER :: mcid,status

  INTEGER, PARAMETER :: nmonths=12
  REAL, PARAMETER :: daysecs=3600.*24., kg2ug=1.e9,kg2ppmv_so2=1.e6*28.97/64.
  REAL, DIMENSION(nmonths) :: months=(/(i,i=1,nmonths)/)
  INTEGER, DIMENSION(1) :: start_dims1d=(/1/),end_dims1d
  INTEGER, DIMENSION(ndims-1) :: start_dims3d=(/1,1,1/),&
       &end_dims3d
  CHARACTER(len=80) :: attname,attvalue
  INTEGER :: dimtime_id,time_id, dimlon_id,lon_id, dimlat_id,lat_id

  INTEGER, DIMENSION(nspecies_edgar) :: var_id

  CALL h5open_f(hdferr)

  DO i=1,nsectors
     filenamehdf=TRIM(prefix)//'_'//TRIM(sectors(i))//'_'//TRIM(suffix)

     CALL h5fopen_f(TRIM(filenamehdf),H5F_ACC_RDONLY_F, file_id, hdferr)

     DO j=1,nspecies_edgar
        dsetname=spc_names_edgar(j)  
        CALL h5dopen_f (file_id, dsetname, dset_id, hdferr)
        IF (hdferr /= 0) THEN 
           CALL h5dclose_f(dset_id, hdferr)
           PRINT *,dsetname,sectors(i)
           CYCLE
        ENDIF
        IF (.NOT. ALLOCATED(RAWsrc)) THEN
           CALL h5dget_space_f (dset_id, space_id, hdferr)
           CALL h5sget_simple_extent_dims_f(space_id, data_dims,maxdims,hdferr)
           CALL h5sclose_f(space_id, hdferr)        
           IF (data_dims(3) /= 1) THEN
              PRINT *,'multilevel emissions - correct ncwrite - stopping'
              STOP
           ENDIF
           ALLOCATE(RAWsrc(data_dims(1),data_dims(2),&
                &data_dims(3),data_dims(4),nspecies_edgar),&
                &src_dummy(data_dims(1),data_dims(2),&
                &data_dims(3),data_dims(4)))
           RAWsrc=0.
        ENDIF
        CALL h5dread_f(dset_id, H5T_NATIVE_REAL,src_dummy,data_dims,hdferr)
        CALL h5dclose_f(dset_id, hdferr)

        RAWsrc(:,:,:,:,j)=RAWsrc(:,:,:,:,j)+src_dummy

        PRINT *,dsetname,sectors(i)
        PRINT *,'sector ',MINVAL(src_dummy),MAXVAL(src_dummy)
        PRINT *,'sum ',MINVAL(RAWsrc(:,:,:,:,j)),MAXVAL(RAWsrc(:,:,:,:,j))
!
!        DO k=1,12
!           PRINT *,k,MINVAL(src_dummy(:,:,:,k)),MAXVAL(src_dummy(:,:,:,k))
!        ENDDO
        
     ENDDO

     CALL h5fclose_f(file_id, hdferr)

  ENDDO

  CALL h5close_f(hdferr)

!  RAWsrc=RAWsrc*daysecs

  DO j=1,nspecies_edgar
     IF (TRIM(spc_names_edgar(j)) == 'SO2') THEN
        RAWsrc(:,:,:,:,j)=RAWsrc(:,:,:,:,j)*kg2ppmv_so2
     ELSE
        RAWsrc(:,:,:,:,j)=RAWsrc(:,:,:,:,j)*kg2ug
     ENDIF
  ENDDO

  nlon=SIZE(src_dummy,1)
  nlat=SIZE(src_dummy,2)
  dlon=360./nlon
  dlat=180./nlat

  DEALLOCATE(src_dummy)

  ALLOCATE(longEdgar(nlon),latEdgar(nlat))

  longEdgar(1)=-180+0.5*dlon
  DO j=2,nlon  
     longEdgar(j)=longEdgar(1) + (j-1)*dlon
  ENDDO

  latEdgar(1)=-90+0.5*dlat
  DO i=2,nlat
     latEdgar(i)= latEdgar(1) + (i-1)*dlat
  ENDDO


  status = nf_create(filenamencdf,ncwrite,mcid)
  IF (status /= NF_NOERR) PRINT*,nf90_strerror(status)

  status = nf_redef(mcid)

  status = nf_def_dim(mcid,'time',NF_UNLIMITED,dimtime_id)
  status = nf_def_var(mcid,'time',NF_REAL,1,dimtime_id,time_id)
  attname="calendar"
  attvalue="none"
  status = nf_put_att_text(mcid, time_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  status = nf_def_dim(mcid,'lon',nlon,dimlon_id)
  status = nf_def_var(mcid,'lon',NF_REAL,1,dimlon_id,lon_id)

  IF (status /= NF_NOERR) THEN 
     PRINT*,nf90_strerror(status),'lon'
     STOP
  ENDIF
  
  attname="long_name"
  attvalue="longitude at grid cell midpoints"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="units"
  attvalue="degrees_east"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="standard_name"
  attvalue="longitude"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  status = nf_def_dim(mcid,'lat',nlat,dimlat_id)
  status = nf_def_var(mcid,'lat',NF_REAL,1,dimlat_id,lat_id)

  attname="long_name"
  attvalue="latitude at grid cell midpoints"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="units"
  attvalue="degrees_north"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="standard_name"
  attvalue="latitude"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  DO i=1,nspecies_edgar
     status = nf_def_var(mcid,spc_names_out(i),NF_REAL,ndims-1,&
          &(/dimlon_id,dimlat_id,dimtime_id/),var_id(i))

     attname="long_name"
     attvalue="emiss_anthro_"//TRIM(spc_names_out(i))
     status = nf_put_att_text(mcid, var_id(i), attname,&
          &LEN_TRIM(attvalue),attvalue)
     attname="units"
     IF (TRIM(spc_names_edgar(i)) == 'SO2') THEN
        attvalue="ppmv kg m-2 s-1"
     ELSE
        attvalue="ug m-2 s-1"
     ENDIF
     status = nf_put_att_text(mcid, var_id(i), attname,&
          &LEN_TRIM(attvalue),attvalue)

  ENDDO

  status = nf_enddef(mcid)
     
  end_dims1d=(/nmonths/)
  status = nf_put_vara_real(mcid,time_id,start_dims1d,end_dims1d,&
       &months)

  end_dims1d=(/nlon/)
  status = nf_put_vara_real(mcid,lon_id,start_dims1d,end_dims1d,&
       &longEdgar)

!  IF (status /= NF_NOERR) THEN 
!     PRINT*,nf90_strerror(status),'lon'
!     STOP
!  ENDIF

  end_dims1d=(/nlat/)
  status = nf_put_vara_real(mcid,lat_id,start_dims1d,end_dims1d,&
       &latEdgar)

  end_dims3d=(/nlon,nlat,nmonths/)

  DO i=1,nspecies_edgar
     status = nf_put_vara_real(mcid,var_id(i),start_dims3d,end_dims3d,&
          &RAWsrc(:,:,1,:,i))
  ENDDO

  status = nf_close(mcid)

  DEALLOCATE(longEdgar,latEdgar,RAWsrc)

END PROGRAM emiss_edgar_anthro_fv3
