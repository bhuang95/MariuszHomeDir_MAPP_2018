PROGRAM grib2nc4crtm
  
!Mariusz Pagowski Nov 2016
!convert nc smoke obtained from grib with ncl_convert2nc to regular nc smoke
  
  USE netcdf_io
  USE constants, ONLY: rd_over_cp_mass, h300, init_constants, init_constants_derived
  USE module_date_sub

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: nvargrib=5,nvarnc=5
  
  INTEGER :: i,j,k,it,idate,itime,dd,mm,yyyy
  CHARACTER(len=25), DIMENSION(nvargrib) :: &
       &varnamegrib = (/&
       &"PRES_P0_L1_GLC0","TMP_P0_L105_GLC0","SPFH_P0_L105_GLC0",&
       &"MASSDEN_P48_L105_GLC0","MASSDEN_P48_L105_GLC0_1"/)

  CHARACTER(len=10), DIMENSION(nvarnc) :: &
       &varnamenc = (/&
       &"MU","T","QVAPOR","tracer_1a","tracer_2a"/)
  
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: varnc
  REAL, ALLOCATABLE, DIMENSION(:,:) :: mu,mub
  REAL, ALLOCATABLE, DIMENSION(:,:) :: znu,znw
  
  INTEGER :: nx,ny,nz,nt
  INTEGER :: mcid,status
  REAL :: small,ptop
  
  INTEGER, PARAMETER :: nvardims =4, natts=6,nvardimsgrib=3
  CHARACTER(len=20), DIMENSION(nvardims) :: dimstring=&
       &(/"west_east","south_north","bottom_top","Time"/)
  CHARACTER(len=20), DIMENSION(nvardims-1) :: dimstring2d=&
       &(/"west_east","south_north","Time"/)
  CHARACTER(len=20), DIMENSION(2) :: dimstringtime=&
       &(/"DateStrLen","Time"/)
  CHARACTER(len=50), DIMENSION(natts) :: attribnames =(/&
       &"FieldType","MemoryOrder","description","units","stagger","coordinates"/)
  CHARACTER(len=50), DIMENSION(natts) :: attribvals
  CHARACTER(len=50) :: attribute,inittime
  INTEGER :: fcsttime
  
  CHARACTER(len=20), DIMENSION(nvardimsgrib) :: dimstringgrib=&
       &(/"xgrid_0","ygrid_0","lv_HYBL0"/)

  INTEGER, DIMENSION(nvardims) :: dimid
  INTEGER, DIMENSION(nvardims) :: end_dims, start_dims=(/1,1,1,1/)
  INTEGER, DIMENSION(nvardims-1) :: dimid2d
  INTEGER, DIMENSION(nvardims-1) :: end_dims2d, start_dims2d=(/1,1,1/)
  INTEGER, DIMENSION(2) :: dimidtime
  INTEGER, DIMENSION(2) :: end_dimstime, start_dimstime=(/1,1/)



  INTEGER, DIMENSION(nvardimsgrib) :: end_dimsgrib
  INTEGER, DIMENSION(nvarnc) :: varid
  CHARACTER(len=25) :: varstringname
  CHARACTER(len=80) :: string,stringu


  INTEGER, PARAMETER :: date_strlen=19
  CHARACTER(len=date_strlen), ALLOCATABLE, DIMENSION(:) :: wrftimes  
  INTEGER :: timeid

  CHARACTER(len=250) :: filencgrib,filencout
  
  INTEGER :: iargc

  IF (iargc() < 2) THEN
     PRINT *,"needs two input files : grib, output nc"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filencgrib)
  CALL getarg(2,filencout)

  small=TINY(1.)
  
  CALL init_constants(.TRUE.)
  CALL init_constants_derived()

  CALL netcdfdimension(filencgrib,nvardimsgrib,dimstringgrib,end_dimsgrib)

  nx=end_dimsgrib(1)
  ny=end_dimsgrib(2)
  nz=end_dimsgrib(3)

  CALL netcdfdimension(filencout,nvardims,dimstring,end_dims)  

  nt=end_dims(4)

  end_dims2d(1:2)=end_dims(1:2)
  end_dims2d(3)=end_dims(4)

  end_dimstime(1)=date_strlen
  end_dimstime(2)=end_dims(4)

  IF ( nx /= end_dims(1) .OR. ny /= end_dims(2) .OR. nz /= end_dims(3) .OR. nt /= 1) THEN
     PRINT *,end_dims(1),end_dims(2),end_dims(3)
     PRINT *,'File sizes of grib and constant do no match. Stopping'
     STOP
  ENDIF

  ALLOCATE(mu(nx,ny),mub(nx,ny),varnc(nx,ny,nz,nt),znu(nz,nt),znw(nz+1,nt))

  ALLOCATE(wrftimes(nt))

!strictly speaking this is psfc with moisture but there is bug somewhere in UPP (?)
!that produces to small mu in from grib file so that compensates a little

  varstringname=varnamegrib(1)

  CALL readnetcdfdata3(filencgrib,varnc(:,:,1,1),varstringname,nx,ny,1)

  attribute='initial_time'
  CALL variableattribute_text(filencgrib,varstringname,&
       &attribute,inittime)

  attribute='forecast_time'
  CALL variableattribute_int(filencgrib,varstringname,&
       &attribute,fcsttime)

  READ(inittime(1:2),'(i2)')mm
  READ(inittime(4:5),'(i2)')dd
  READ(inittime(7:10),'(i4)')yyyy
  READ(inittime(13:14),'(i4)')itime

  idate=jd(yyyy,mm,dd)

  itime=itime+fcsttime
  IF (itime >= 24) THEN
     itime=itime-24
     idate=idate+1
  ENDIF

  CALL cdate(idate,yyyy,mm,dd)
  WRITE(wrftimes(:)(1:4),'(i4)')yyyy
  wrftimes(:)(5:5)='-'
  WRITE(wrftimes(:)(6:7),'(i2.2)')mm
  wrftimes(:)(8:8)='-'
  WRITE(wrftimes(:)(9:10),'(i2.2)')dd
  wrftimes(:)(11:11)='_'
  WRITE(wrftimes(:)(12:13),'(i2.2)')itime
  wrftimes(:)(14:)=':00:00'

  mu=varnc(:,:,1,1) !this is moist surface pressure

  varstringname='P_TOP'
  CALL readnetcdfdata3(filencout,varnc(1,1,1,1),varstringname,1,1,1)

  ptop=varnc(1,1,1,1)

  varstringname='ZNU'
  CALL readnetcdfdata3(filencout,varnc(:,1,1,1),varstringname,nz,1,1)

  znu(:,1)=varnc(:,1,1,1)
  
  varstringname='ZNW'
  CALL readnetcdfdata3(filencout,varnc(:,1,1,1),varstringname,nz,1,1)

  znw(1:nz,1)=varnc(:,1,1,1)
  znw(nz+1,:)=0.

  varstringname='MUB'
  CALL readnetcdfdata3(filencout,varnc(:,:,1,1),varstringname,nx,ny,1)

  mub=varnc(:,:,1,1)
  mu=mu-mub-ptop

  DO i=1,nt
     znu(:,i)=znu(:,1)
     znw(:,i)=znw(:,1)
  ENDDO

  status = nf_open(filencout,ncwrite,mcid)
  DO i=1,nvardims
     status = nf_inq_dimid(mcid,dimstring(i),dimid(i))
  ENDDO

  DO i=1,nvardims-1
     status = nf_inq_dimid(mcid,dimstring2d(i),dimid2d(i))
  ENDDO

  status = nf_open(filencout,ncwrite,mcid)

!  status = nf_inq_varid(mcid,'Times',timeid)

  status = nf_redef(mcid)

  i=1
  status = nf_def_dim(mcid,dimstringtime(i),date_strlen,dimidtime(i))
  i=2
  status = nf_inq_dimid(mcid,dimstringtime(i),dimidtime(i))

  status = nf_def_var(mcid, 'Times', NF_CHAR, 2, &
       &dimidtime,timeid)
  PRINT *, 'def ','Times'

  DO i = 1, nvarnc

     PRINT *, 'def ',varnamenc(i)
     
     IF (varnamenc(i) == "MU") THEN
        status = nf_def_var(mcid, varnamenc(i), NF_REAL, nvardims-1, &
             &dimid2d,varid(i))
     ELSE
        status = nf_def_var(mcid, varnamenc(i), NF_REAL, nvardims, &
             &dimid,varid(i))
     ENDIF
     
     IF (status /= nf_noerr) THEN
        PRINT *,'error 1'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF

     j=1
     status = nf_put_att(mcid, varid(i), attribnames(j),NF_INT, 1, 104)
     j=2
     IF (varnamenc(i) == "MU") THEN
        string="XY"
     ELSE
        string="XYZ"
     ENDIF

     status = nf_put_att_text(mcid, varid(i), attribnames(j),LEN_TRIM(string),string)
     j=3

     SELECT CASE ( TRIM(varnamenc(i)) )
     CASE ('MU')
        string="perturbation dry air mass in column"
        stringu="Pa"
     CASE ('T')
        string="perturbation potential temperature (theta-t0)"
        stringu="K"
     CASE ('QVAPOR')
        string="Water vapor mixing ratio"
        stringu="kg kg-1"
     CASE ('tracer_1a')
        string="tracer1a conc."
        stringu="ug/kg-dryair"
     CASE ('tracer_2a')
        string="tracer2a conc."
        stringu="ug/kg-dryair"
     END SELECT

     status = nf_put_att_text(mcid, varid(i), attribnames(j),LEN_TRIM(string),string)
     j=4
     status = nf_put_att_text(mcid, varid(i), attribnames(j),LEN_TRIM(stringu),stringu)
     j=5
     string=""
     status = nf_put_att_text(mcid, varid(i), attribnames(j),LEN_TRIM(string),string)
     j=6
     string="XLONG XLAT"
     status = nf_put_att_text(mcid, varid(i), attribnames(j),LEN_TRIM(string),string)
     
  ENDDO
  
  status = nf_enddef(mcid)

  PRINT *,'write ','Time'
  status = nf_put_vara_text(mcid,timeid,start_dimstime,end_dimstime,wrftimes)

  DO i=1,nt
     varnc(:,:,1,i)=mu
  ENDDO

  i=1
  
  status = nf_put_vara_real(mcid,varid(i),start_dims2d,end_dims2d,&
       &varnc(:,:,1,:))
  IF (status /= nf_noerr) THEN
     PRINT *,'error 2'
     PRINT *, 'Error: ', nf_strerror(status)
  ENDIF
  
  DO i = 2, nvarnc

     PRINT *, 'write ',varnamenc(i)

     varstringname=varnamegrib(i)
     
     CALL readnetcdfdata3(filencgrib,varnc(:,:,:,1),varstringname,nx,ny,nz)

     SELECT CASE ( TRIM(varnamenc(i)) )
     CASE ('T')
        DO k=1,nz
           varnc(:,:,k,1)=varnc(:,:,k,1)*&
                &(100000./(znu(k,1)*(mu+mub)+ptop))**rd_over_cp_mass-h300
        ENDDO
        DO it=1,nt
           varnc(:,:,:,it)=varnc(:,:,:,1)
        ENDDO
     CASE ('QVAPOR')
        varnc(:,:,:,1)=varnc(:,:,:,1)/(1.-varnc(:,:,:,1))
        DO it=1,nt
           varnc(:,:,:,it)=MAX(varnc(:,:,:,1),small)
        ENDDO
     CASE ('tracer_1a','tracer_2a')
        DO it=1,nt
           varnc(:,:,:,it)=MAX(varnc(:,:,:,1),small)
        ENDDO
     CASE default
        PRINT *,'CASE - should not be here - stopping'
        STOP
     END SELECT

     PRINT *,varnamenc(i),MINVAL(varnc),MAXVAL(varnc)
     
     status = nf_put_vara_real(mcid,varid(i),start_dims,end_dims,&
          &varnc(:,:,:,:))
     
     IF (status /= nf_noerr) THEN
        PRINT *,'error 2'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF
  ENDDO

  status = nf_close(mcid)
  
  DEALLOCATE(varnc)

END PROGRAM grib2nc4crtm

