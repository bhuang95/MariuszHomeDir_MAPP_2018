PROGRAM modis2ioda

!mzp jul 2021 to read/thin MODIS 6.1
!----------------------------------------------------------------------------------------!

  USE module_constants
  USE modis2ioda_init
  USE modis2ioda_utils
  USE datetime_mod
  USE timedelta_mod

  IMPLICIT NONE

  CHARACTER( len=MAX_NC_NAME ) names_in(MAX_NAMES), name_in, names_out(MAX_NAMES), name_out
  INTEGER :: ncid, nvarsid, nlocsid, varids(MAX_NAMES)
  INTEGER :: status
  CHARACTER(len=4) :: cchannel
  TYPE(datetime_type) :: datatime
  TYPE(timedelta_type) :: dtime
  REAL, ALLOCATABLE :: aod(:,:),qc(:,:),sfc(:,:),alg(:,:),values_retained(:)
  REAL, ALLOCATABLE :: tmp1(:,:),tmp2(:,:)

  INTEGER i,j

  LOGICAL :: fail=.FALSE.

  datatime_modis=create_datetime(year=yyyy_modis,month=mm_modis,day=dd_modis,hour=hh_modis)

  CALL init

  WRITE(cchannel,'(i4)')ichannel

  names_in="" ; names_out=""

  names_out(1)="frequency@VarMetaData"
  names_out(2)="sensor_channel@VarMetaData"
  names_in(3)="Longitude"
  names_out(3)="longitude@MetaData"
  names_in(4)="Latitude"
  names_out(4)="latitude@MetaData"
  names_in(5)="Land_sea_Flag"
  names_out(5)="surface_type@MetaData" !int
  names_in(6)="AOD_550_Dark_Target_Deep_Blue_Combined_Algorithm_Flag"
  names_out(6)="algorithm@MetaData" !int
  names_in(7)="Scan_Start_Time"
  names_out(7)="time@MetaData" 
  names_in(8)="AOD_550_Dark_Target_Deep_Blue_Combined" 
  names_out(8)="aerosol_optical_depth_"//TRIM(ADJUSTL(cchannel))//"@ObsValue"
  names_in(9)="AOD_550_Dark_Target_Deep_Blue_Combined_QA_Flag"
  names_out(9)="aerosol_optical_depth_"//TRIM(ADJUSTL(cchannel))//"@PreQc"
  names_in(10)="Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty"
  names_out(10)="aerosol_optical_depth_"//TRIM(ADJUSTL(cchannel))//"@ObsError"

  name_in="AOD_550_Dark_Target_Deep_Blue_Combined"
  CALL read_modis(infile,name_in,aod,fail)
  IF (fail) STOP 'Skipping file'

  
  name_in="AOD_550_Dark_Target_Deep_Blue_Combined_QA_Flag"
  CALL read_modis(infile,name_in,qc,fail)
  IF (fail) STOP 'Skipping file'

  nobs_out=COUNT( aod >= 0. .AND. NINT(qc) == 3 )

  IF (nobs_out == 0) THEN
     PRINT *, "No observations to retain - Stopping for ",validtimeint
     STOP
  ENDIF

  status=nf90_create(path=outfile,cmode=nf90_netcdf4,ncid=ncid)
  status=nf90_def_dim(ncid,"nlocs",NF90_UNLIMITED,nlocsid)
  status=nf90_def_dim(ncid,"nvars",nvars,nvarsid)

  status=nf90_put_att(ncid,NF90_GLOBAL,"observation_type","Aod")
  status=nf90_put_att(ncid,NF90_GLOBAL,"date_time",validtimeint)
  status=nf90_put_att(ncid,NF90_GLOBAL,"satellite",TRIM(satellite))
  status=nf90_put_att(ncid,NF90_GLOBAL,"sensor","v."//TRIM(instrument)//"_"//TRIM(satellite))
  status=nf90_put_att(ncid,NF90_GLOBAL,"surface_type","ocean=0,land=1,costal=2")
  status=nf90_put_att(ncid,NF90_GLOBAL,"algorithm","DT=0,DB=1,DBT=2")
  
  status=nf90_def_var(ncid,TRIM(names_out(1)),nf90_real,nvarsid,varids(1)) !freq
  status=nf90_def_var(ncid,TRIM(names_out(2)),nf90_int,nvarsid,varids(2)) !channel
  status=nf90_def_var(ncid,TRIM(names_out(3)),nf90_real,nlocsid,varids(3)) !lon
  status=nf90_def_var(ncid,TRIM(names_out(4)),nf90_real,nlocsid,varids(4)) !lat
  status=nf90_def_var(ncid,TRIM(names_out(5)),nf90_int,nlocsid,varids(5)) !sfc
  status=nf90_def_var(ncid,TRIM(names_out(6)),nf90_int,nlocsid,varids(6)) !algorithm
  status=nf90_def_var(ncid,TRIM(names_out(7)),nf90_real,nlocsid,varids(7)) !time
  status=nf90_def_var(ncid,TRIM(names_out(8)),nf90_real,nlocsid,varids(8)) !aod
  status=nf90_def_var(ncid,TRIM(names_out(9)),nf90_int,nlocsid,varids(9)) !preqc
  status=nf90_def_var(ncid,TRIM(names_out(10)),nf90_real,nlocsid,varids(10)) !obserror
  status = nf90_enddef(ncid)

  status=nf90_put_var(ncid,varids(1),speed_of_light/wavelength)
  status=nf90_put_var(ncid,varids(2),ichannel)


  ALLOCATE(values_retained(nobs_out))

  name_in="Land_sea_Flag"
  CALL read_modis(infile,name_in,sfc,fail)
  IF (fail) STOP 'Skipping file'

  values_retained=PACK(sfc,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(5),values_retained) !sfc

  values_retained=0
  status=nf90_put_var(ncid,varids(9),values_retained) !qc

  values_retained=PACK(aod,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(8),values_retained) !aod

!0=Dark TARGET, 1=Deep Blue, 2=Mixed algorithm
  name_in="AOD_550_Dark_Target_Deep_Blue_Combined_Algorithm_Flag"
  CALL read_modis(infile,name_in,alg,fail)
  IF (fail) STOP 'Skipping file'

  values_retained=PACK(alg,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(6),values_retained) !algorithm

  ALLOCATE(tmp1(SIZE(aod,1),SIZE(aod,2)))

  WHERE (aod >= 0. .AND. NINT(qc) == 3 .AND. NINT(alg) /= 1) 
     WHERE (sfc > 0) 
!unc over land DT i.e. other than deep blue
!Levy 2010
        tmp1=0.05+0.15*aod
     ELSEWHERE (sfc == 0)
!unc over ocean Levy, 2013
!We have computed expected error (EE) over both land and ocean for our test DATA, maintaining ±(0.05 + 15 %) 
!over land, but changing to (+(0.04 + 10 %, −(0.02 + 10 %)) over ocean to reflect consistent asymmetry.
!since there is no consistent way for ocean take 0.03 + 10%
        tmp1=0.03+0.10*aod
     ELSEWHERE
        tmp1=rmissing
     END WHERE
  ELSEWHERE
     tmp1=rmissing
  END WHERE

!  PRINT *, COUNT( aod >= 0. .AND. NINT(qc) == 3 .AND. sfc > 0 .AND. NINT(alg) /= 1)
!  PRINT *, COUNT( aod >= 0. .AND. NINT(qc) == 3 .AND. NINT(sfc) == 0 .AND. NINT(alg) /= 1)
!  PRINT *, COUNT( aod >= 0. .AND. NINT(qc) == 3 .AND. sfc > 0 .AND. NINT(alg) == 1)

!unc over land for deep blue
  name_in="Deep_Blue_Aerosol_Optical_Depth_550_Land_Estimated_Uncertainty"
  CALL read_modis(infile,name_in,tmp2,fail)
  IF (fail) STOP 'Skipping file'

!merge uncertainties of DT and DB where DB available and DT not available

  WHERE ( NINT(tmp1) == NINT(rmissing) .AND. aod >= 0. .AND. NINT(qc) == 3 .AND. sfc > 0 &
       &.AND. tmp2 >= 0. ) tmp1=tmp2

  IF (ALLOCATED(sfc)) DEALLOCATE(sfc)
  IF (ALLOCATED(alg)) DEALLOCATE(alg)
  IF (ALLOCATED(tmp2)) DEALLOCATE(tmp2)

  values_retained=PACK(tmp1,tmp1 > rmissing)
  IF (ANY(values_retained < 0.)) THEN
     PRINT *,"Should not happen"
     PRINT *,"Negative uncertainty/error - assigning ocean uncertainty as default"
     WHERE ( aod >= 0. .AND. NINT(qc) == 3 .AND. NINT(tmp1) == NINT(rmissing) ) tmp1=0.03+0.10*aod
     values_retained=PACK(tmp1,tmp1 > rmissing)
  ENDIF

  status=nf90_put_var(ncid,varids(10),values_retained) ! uncertainty/error

  name_in="Scan_Start_Time"
  CALL read_modis(infile,name_in,tmp1,fail)
  IF (fail) STOP 'Skipping file'

  DO i=1,SIZE(aod,1)
     DO j=1,SIZE(aod,2)
        dtime = datatime_modis+timedelta(seconds=NINT(tmp1(i,j)))-validtime
        tmp1(i,j) = dtime%total_hours()
     ENDDO
  ENDDO


  values_retained=PACK(tmp1,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(7),values_retained) ! time

  name_in="Longitude"
  CALL read_modis(infile,name_in,tmp1,fail)
  IF (fail) STOP 'Skipping file'

  values_retained=PACK(tmp1,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(3),values_retained) ! longitude

  name_in="Latitude"
  CALL read_modis(infile,name_in,tmp1,fail)
  IF (fail) STOP 'Skipping file'

  values_retained=PACK(tmp1,(aod >= 0. .AND. NINT(qc) == 3))
  status=nf90_put_var(ncid,varids(4),values_retained) ! latitude

  status=nf90_close(ncid)

  PRINT *,"Retained ",nobs_out," highest quality retrievals"

  IF (ALLOCATED(aod)) DEALLOCATE(aod)
  IF (ALLOCATED(qc)) DEALLOCATE(qc)
  IF (ALLOCATED(tmp1)) DEALLOCATE(tmp1)
  IF (ALLOCATED(values_retained)) DEALLOCATE(values_retained)

END PROGRAM modis2ioda
