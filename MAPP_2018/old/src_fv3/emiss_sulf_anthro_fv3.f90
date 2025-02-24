PROGRAM emiss_sulf_anthro_fv3

!MZP to convert sulf binary to ncdf
!origin of the input file is unknown - and so are the units
  
  USE hdf5
  USE netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

!neglect agriculture (contains only NH3 species) for GOCART
  INTEGER, PARAMETER :: ndims=3, nspecies_junk=25

  CHARACTER(len=32), PARAMETER :: sulf_name='e_sulf',sulf_name_out='ea_sulf'

  CHARACTER(len=80) :: filenamebin="anthro.bin",&
       &filenamencdf="emiss_sulf_anthro_fv3.nc"

  CHARACTER(len=32), DIMENSION(nspecies_junk), PARAMETER :: &
       &spc_names_junk= (/&
       &'e_so2','e_no', 'e_ald', 'e_hcho','e_ora2',&
       &'e_nh3','e_hc3','e_hc5', 'e_hc8', 'e_eth',&
       &'e_co', 'e_ol2','e_olt', 'e_oli', 'e_tol',&
       &'e_xyl','e_ket','e_csl', 'e_iso', 'e_pm_25',&
       &'e_pm_10','e_oc','e_bc','e_dms', 'e_sulf'/)

  INTEGER, PARAMETER :: nlon=288, nlat=181
  REAL, PARAMETER :: lon1=-180.,lat1=-89.75


  INTEGER :: nx,ny,i,j,k,junk
  CHARACTER(LEN=20) :: cjunk
  REAL, DIMENSION(nlon,nlat,1) :: src_dummy
  REAL :: dlon,dlat
  REAL, DIMENSION(nlon) :: longSulf
  REAL, DIMENSION(nlat) :: latSulf
  INTEGER :: mcid,status

  REAL, PARAMETER :: daysecs=3600.*24.
  INTEGER, DIMENSION(1) :: start_dims1d=(/1/),end_dims1d
  INTEGER, DIMENSION(ndims) :: start_dims3d=(/1,1,1/),&
       &end_dims3d
  CHARACTER(len=80) :: attname,attvalue
  INTEGER :: dimtime_id,time_id, dimlon_id,lon_id, dimlat_id,lat_id

  INTEGER :: var_id

  dlon=360./nlon
  dlat=180./(nlat-1)

  longSulf(1)=lon1
  DO j=2,nlon  
     longSulf(j)=longSulf(1) + (j-1)*dlon
  ENDDO

  latSulf(1)=lat1
  latSulf(2)=lat1+0.75
  DO i=3,nlat-1
     latSulf(i)= latSulf(2) + (i-2)*dlat
  ENDDO
  latSulf(nlat)=-lat1

  OPEN(unit=101,file=filenamebin,convert='big_endian',&
       &form='unformatted',status='old')
  READ(101)junk
  PRINT *,junk
  READ(101)cjunk
  PRINT *,cjunk
  READ(101)junk
  PRINT *,junk
  DO i=1,nspecies_junk
     READ(101)src_dummy 
     PRINT *,spc_names_junk(i),MINVAL(src_dummy), MAXVAL(src_dummy)
     IF (TRIM(spc_names_junk(i)) == TRIM(sulf_name)) EXIT
  ENDDO

!remove some junk over the Atlantic
  WHERE(src_dummy > 2.) src_dummy=0.

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

  status = nf_def_var(mcid,sulf_name_out,NF_REAL,ndims,&
       &(/dimlon_id,dimlat_id,dimtime_id/),var_id)
  
  attname="long_name"
  attvalue="emiss_anthro_"//TRIM(sulf_name)
  status = nf_put_att_text(mcid, var_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="units"
  attvalue="ppmv kg m-2 s-1"
  status = nf_put_att_text(mcid, var_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  
  status = nf_enddef(mcid)
     
  end_dims1d=(/1/)
  status = nf_put_vara_real(mcid,time_id,start_dims1d,end_dims1d,&
       &1)

  end_dims1d=(/nlon/)
  status = nf_put_vara_real(mcid,lon_id,start_dims1d,end_dims1d,&
       &longSulf)

!  IF (status /= NF_NOERR) THEN 
!     PRINT*,nf90_strerror(status),'lon'
!     STOP
!  ENDIF

  end_dims1d=(/nlat/)
  status = nf_put_vara_real(mcid,lat_id,start_dims1d,end_dims1d,&
       &latSulf)

  end_dims3d=(/nlon,nlat,1/)

  status = nf_put_vara_real(mcid,var_id,start_dims3d,end_dims3d,&
          &src_dummy(:,:,1))

  status = nf_close(mcid)

END PROGRAM emiss_sulf_anthro_fv3
