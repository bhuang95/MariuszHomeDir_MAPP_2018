PROGRAM emiss_bburn_fv3

!MZP to convert daily latlon bburn files from prep_chem_src to ncdf for fv3 interpolation
!will need rework some time to avoid double interpolation
!plume alsoe called plum

!emissions are surface only dims nlon * nlat * nspecies
  
  USE hdf5
  USE netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: nspecies_bburn=7,ndims=3,nplume=8

  CHARACTER(len=32), DIMENSION(nspecies_bburn), PARAMETER :: &
       &spc_names= (/'BC','OC','PM2.5','PM10','SO2','SO4','DMS'/)

  CHARACTER(len=32), DIMENSION(nspecies_bburn), PARAMETER :: &
       &spc_names_out= (/'eb_bc','eb_oc','eb_pm25','eb_pm10','eb_so2',&
       &'eb_sulf','eb_dms'/)

  CHARACTER(len=32), DIMENSION(nplume), PARAMETER :: &
       &plume_names_out= (/&
       &'mean_fct_agtf','mean_fct_agef','mean_fct_agsv','mean_fct_aggr',&
       &'firesize_agtf','firesize_agef','firesize_agsv','firesize_aggr'/)

  CHARACTER(len=80) :: prefix,filename,cdatelong,suffix='-bb.bin',&
       &prefixncdf,filenamencdf

  CHARACTER(len=10) :: cdate

  INTEGER :: nx,ny,nveg,i,j,k
  REAL, ALLOCATABLE, DIMENSION(:,:,:):: rawsrc
  REAL, ALLOCATABLE, DIMENSION(:,:,:):: plume
  REAL :: dlon,dlat
  INTEGER :: nlon,nlat
  REAL, ALLOCATABLE, DIMENSION(:) :: lonbburn,latbburn
  INTEGER :: mcid,status

  INTEGER, DIMENSION(1) :: start_dims1d=(/1/),end_dims1d
  INTEGER, DIMENSION(ndims) :: start_dims3d=(/1,1,1/),&
       &end_dims3d
  CHARACTER(len=80) :: attname,attvalue
  INTEGER :: dimtime_id,time_id, dimlon_id,lon_id, dimlat_id,lat_id

  INTEGER, DIMENSION(nspecies_bburn+nplume) :: var_id
  
  INTEGER :: iargc

  IF (iargc() /= 3) THEN
     PRINT *,'needs infile prefix, date (10 digs), and outfile prefix ',&
          &'STOPPING'
     STOP
  ENDIF

  CALL getarg(1,prefix)
  CALL getarg(2,cdate)
  CALL getarg(3,prefixncdf)
  
  cdatelong="-T-"//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//&
       &"-000000"

  filename=TRIM(prefix)//TRIM(cdatelong)//'-lonlat'//TRIM(suffix)
  OPEN(unit=101,file=filename,form="unformatted")
  READ(101)nlon
  ALLOCATE(lonbburn(nlon))
  READ(101)lonbburn
  READ(101)nlat
  ALLOCATE(latbburn(nlat))
  READ(101)latbburn
  CLOSE(101)

  nx=nlon
  ny=nlat

  end_dims3d=(/nlon,nlat,1/)

  ALLOCATE(rawsrc(nx,ny,nspecies_bburn),plume(nx,ny,nplume))
  
  DO i=1,nspecies_bburn
     filename=TRIM(prefix)//TRIM(cdatelong)//'-'//TRIM(spc_names(i))//TRIM(suffix)
     PRINT *,TRIM(filename)
     OPEN(unit=101,file=filename,form="unformatted")
     READ(101)nlon,nlat
     IF (nlon /= nx .OR. nlat /= ny) THEN
        PRINT *,'latlon dimensions not matching species - stopping'
        STOP
     ENDIF
     READ(101)rawsrc(:,:,i)
     CLOSE(101)
     PRINT *,TRIM(spc_names(i)),MINVAL(rawsrc(:,:,i)),&
          &MAXVAL(rawsrc(:,:,i))
     PRINT *,'   '
  ENDDO

  filename=TRIM(prefix)//TRIM(cdatelong)//'-plume'//TRIM(suffix)
  PRINT *,TRIM(filename)
  OPEN(unit=101,file=filename,form="unformatted")
  READ(101)nlon,nlat,nveg
  IF (nlon /= nx .OR. nlat /= ny .OR. nveg /= nplume) THEN
     PRINT *,nlon,nlat,nveg
     PRINT *,'latlon dimensions not matching species - stopping'
     STOP
  ENDIF

  DO i=1,nplume
     READ(101)plume(:,:,i)
     PRINT *,TRIM(plume_names_out(i)),MINVAL(plume(:,:,i)),&
          &MAXVAL(plume(:,:,i))
     PRINT *,'   '
  ENDDO

  filenamencdf=TRIM(prefixncdf)//'_'//cdate//'.nc'

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

  DO i=1,nspecies_bburn
     status = nf_def_var(mcid,spc_names_out(i),NF_REAL,ndims,&
          &(/dimlon_id,dimlat_id,dimtime_id/),var_id(i))

     attname="long_name"
     attvalue="emiss_bburn_"//TRIM(spc_names_out(i))
     status = nf_put_att_text(mcid, var_id(i), attname,&
          &LEN_TRIM(attvalue),attvalue)
     attname="units"
     IF (TRIM(spc_names(i)) == 'SO2' .OR. &
          &TRIM(spc_names(i)) == 'SO4' .OR. &
          &TRIM(spc_names(i)) == 'DMS') THEN
        attvalue="ppmv kg m-2 s-1"
     ELSE
        attvalue="ug m-2 s-1"
     ENDIF
     status = nf_put_att_text(mcid, var_id(i), attname,&
          &LEN_TRIM(attvalue),attvalue)

  ENDDO

  DO i=1,nplume

     status = nf_def_var(mcid,plume_names_out(i),NF_REAL,ndims,&
          &(/dimlon_id,dimlat_id,dimtime_id/),var_id(i+nspecies_bburn))

     attname="long_name"
     attvalue="plume_"//TRIM(plume_names_out(i))
     status = nf_put_att_text(mcid, var_id(i+nspecies_bburn), attname,&
          &LEN_TRIM(attvalue),attvalue)
     attname="units"
     attvalue="none"
     status = nf_put_att_text(mcid, var_id(i+nspecies_bburn), attname,&
          &LEN_TRIM(attvalue),attvalue)

  ENDDO

  status = nf_enddef(mcid)
     
  end_dims1d=(/1/)
  status = nf_put_vara_real(mcid,time_id,start_dims1d,end_dims1d,&
       &1)

  end_dims1d=(/nlon/)
  status = nf_put_vara_real(mcid,lon_id,start_dims1d,end_dims1d,&
       &lonbburn)

!  IF (status /= NF_NOERR) THEN 
!     PRINT*,nf90_strerror(status),'lon'
!     STOP
!  ENDIF

  end_dims1d=(/nlat/)
  status = nf_put_vara_real(mcid,lat_id,start_dims1d,end_dims1d,&
       &latbburn)

  end_dims3d=(/nlon,nlat,1/)

  DO i=1,nspecies_bburn
     status = nf_put_vara_real(mcid,var_id(i),start_dims3d,end_dims3d,&
          &rawsrc(:,:,i))
  ENDDO

  DO i=1,nplume
     status = nf_put_vara_real(mcid,var_id(i+nspecies_bburn),start_dims3d,end_dims3d,&
          &plume(:,:,i))
     IF (status /= NF_NOERR) THEN
        PRINT*,nf90_strerror(status),plume_names_out(i)
        STOP
     ENDIF

  ENDDO

  status = nf_close(mcid)

  DEALLOCATE(lonbburn,latbburn,rawsrc)

END PROGRAM emiss_bburn_fv3
