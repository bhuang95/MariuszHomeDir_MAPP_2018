PROGRAM cmaq2gsi

! to read and output cmaq files to binary for gsi
! Mariusz Pagowski, CIRA/NOAA Boulder,Sept 20, 2010

  USE kinds

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'      ! i/o API
  INCLUDE 'FDESC3.EXT'      ! i/o API
  INCLUDE 'IODECL3.EXT'     ! i/o API
  
  INTEGER, PARAMETER :: ngridcross=4,npcross=3,&
       &nmetcross=2,naero=33,maxlength=16,&
       &nskip=7 !how many records (grid info) to skip to start reading met variables

  REAL(r_kind),PARAMETER :: g = 9.81_r_kind ! from gsi
  REAL(r_kind),PARAMETER :: pt_ll=5000._r_kind ! nam top is at 5 hPa

!name order for grid and met variables is mandatory if to avoid 
!problems with gsi 

  CHARACTER(len=maxlength), DIMENSION(ngridcross) :: gridcrossnames=(/&
       &'LAT       ','LON       ','MSFX2     ','HT        '/)

  CHARACTER(len=maxlength), DIMENSION(npcross) :: pcrossnames=(/&
       &'PRES-F_lvl','DENS      ','ZF        '/)

  CHARACTER(len=maxlength), DIMENSION(nmetcross) :: metcrossnames=(/&
       &'TA        ','QV        '/)


  CHARACTER(len=maxlength), DIMENSION(naero) :: aeronames=(/&
       'ASO4I',  'ANO3I',  'ANH4I',  'AORGPAI',  'AECI',   'ACLI', &
       'ASO4J',  'ANO3J',  'ANH4J',  'AORGPAJ',  'AECJ',   'ANAJ',&
       'ACLJ',   'A25J',   'AXYL1J', 'AXYL2J',   'AXYL3J', 'ATOL1J',&
       'ATOL2J', 'ATOL3J', 'ABNZ1J', 'ABNZ2J',   'ABNZ3J', 'AALKJ',&
       'AOLGAJ', 'AISO1J', 'AISO2J', 'AISO3J',   'ATRP1J', 'ATRP2J',&
       'ASQTJ',  'AOLGBJ', 'AORGCJ'/)
  

!  CHARACTER(len=maxlength), DIMENSION(naero) :: aeronames=(/&
!       &'ASO4I     ','ASO4J     ','ANH4I     ','ANH4J     ',&
!       &'ANO3I     ','ANO3J     ','AORGAI    ','AORGAJ    ',&
!       &'AORGPAI   ','AORGPAJ   ','AORGBI    ','AORGBJ    ',&
!       &'AECI      ','AECJ      ','A25I      ','A25J      '/)

  CHARACTER(len=200) :: gridcrossfile,metcrossfile,&
       &cmaqfile,etafile,outgsifile
  CHARACTER(len=8) :: yyyymmdd
  CHARACTER(len=6) :: hhmmss
  CHARACTER(len=30) :: etaheader

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: tmp
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: vars
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: gridcross
  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: aeta1,eta1,aeta2,eta2

  REAL :: dx
  INTEGER :: i,j,k,ii,jj,iargc,n,year,month,mday,hour,min,sec,&
       &jdate,jtime,julian,&
       &outgsiunit=51,etaunit=52
  LOGICAL :: iflag,setenvvar
  CHARACTER(len=3) :: coutgsiunit

  INTEGER(i_kind), DIMENSION(6) :: regional_time
  INTEGER(i_kind) :: nlon_regional,nlat_regional,nsig
  REAL(r_single) :: ptop,pdtop,psfc  

  IF(iargc().NE.6) THEN
     PRINT*,' need seven string inputs, got ',iargc()
     STOP
  ENDIF

  WRITE(coutgsiunit,'(i3)')outgsiunit
!  CALL setrteopts('ufmt_littleendian='//coutgsiunit)

  CALL getarg(1,gridcrossfile)
  CALL getarg(2,metcrossfile)
  CALL getarg(3,cmaqfile)
  CALL getarg(4,yyyymmdd) !YYYYMMDD
  CALL getarg(5,hhmmss) !HHMMSS
  CALL getarg(6,outgsifile) 

  READ(yyyymmdd(1:4),'(i4)')year
  READ(yyyymmdd(5:6),'(i2)')month
  READ(yyyymmdd(7:8),'(i4)')mday

  READ(hhmmss(1:2),'(i4)')hour
  READ(hhmmss(3:4),'(i2)')min
  READ(hhmmss(5:6),'(i4)')sec

  regional_time(1)=year
  regional_time(2)=month
  regional_time(3)=mday
  regional_time(4)=hour
  regional_time(5)=min
  regional_time(6)=sec

!WRITE(ck,'(i2)')k 
!READ(month,'(i2)') imonth

  jdate = 1000 * year  +  julian (year, month, mday)
  jtime=10000*hour+100*min+sec

  n=init3()

  iflag=setenvvar('NOTCDF_FATAL','F')

  iflag=setenvvar('INPUT',TRIM(gridcrossfile))

  IF (.NOT. OPEN3('INPUT',FSREAD3,'gridcrossread') ) THEN
     PRINT*,'failed opening ',gridcrossfile
     STOP
  ENDIF
  
  IF(.NOT.desc3('INPUT')) THEN
     PRINT*,'failed geting information from ',TRIM(gridcrossfile)
     STOP
  ENDIF

  ALLOCATE(tmp(ncols3d,nrows3d,1),gridcross(ncols3d,nrows3d,ngridcross))

  DO i=1,ngridcross
     IF (read3('INPUT',gridcrossnames(i),ALLAYS3,jdate,jtime,tmp)) THEN
        PRINT *,'Read ',gridcrossnames(i),' from ', TRIM(gridcrossfile)
        PRINT *,MAXVAL(tmp),MINVAL(tmp)           
        gridcross(:,:,i)=tmp(:,:,1)
     ELSE
        PRINT *,'Error reading ',gridcrossnames(i),' from ', &
             &TRIM(gridcrossfile)
        PRINT *,'Stopping'
        STOP
     ENDIF
  ENDDO

  IF (.NOT. close3('INPUT') ) THEN
     PRINT*,'failed closing ',gridcrossfile
     STOP
  ENDIF

  DEALLOCATE(tmp)
  
  iflag=setenvvar('INPUT',TRIM(metcrossfile))

  IF (.NOT. OPEN3('INPUT',FSREAD3,'metcrossread') ) THEN
     PRINT*,'failed opening ',metcrossfile
     STOP
  ENDIF
  
  IF(.NOT.desc3('INPUT')) THEN
     PRINT*,'failed geting information from ',TRIM(metcrossfile)
     STOP
  ENDIF

  dx=xcell3d
  nlon_regional=ncols3d
  nlat_regional=nrows3d
  nsig=nlays3d

  ALLOCATE(tmp(ncols3d,nrows3d,nlays3d),&
       &vars(nlon_regional,nlat_regional,nsig),&
       &eta1(nsig+1),aeta1(nsig),eta2(nsig+1),aeta2(nsig))


  eta1=(/&
       &1.000000, 0.995253, 0.990479, 0.985679, 0.980781,&
       &0.970684, 0.960187, 0.948991, 0.936895, 0.923599,&
       &0.908404, 0.876814, 0.829314, 0.762114, 0.676614,&
       &0.582114, 0.484394, 0.388094, 0.270694, 0.223694,&
       &0.169294, 0.127094, 0.089794/)

  eta2=0.
  aeta2=0.

  ptop=pt_ll
  pdtop=0.

  DO k=1,nsig
     aeta1(k)=0.5*(eta1(k)+eta1(k+1))
  ENDDO

  DO i=1,npcross
     
     IF (read3('INPUT',pcrossnames(i),ALLAYS3,jdate,jtime,tmp)) THEN
        PRINT *,'Read ',pcrossnames(i),' from ', TRIM(metcrossfile)
        PRINT *,MAXVAL(tmp),MINVAL(tmp)
        
        IF (i==1) THEN
           
           PRINT *,'Top level pressure = ',ptop
           
!write grid info for gsi (similar to other binary formats)
           
           OPEN(outgsiunit,file=outgsifile,form='unformatted')

           WRITE(outgsiunit)nskip 

           WRITE(outgsiunit)regional_time,nlon_regional,nlat_regional,&
                &nsig,ptop,pdtop        ! 1
           
           WRITE(outgsiunit)aeta1,aeta2 ! 2
           WRITE(outgsiunit)eta1,eta2  ! 3
           
!convert mapscale factor to dx_mc for gsi, i.e. real distance on the earth
           
           gridcross(:,:,3)=dx/SQRT(gridcross(:,:,3))
           
           WRITE(outgsiunit)gridcross(:,:,1) ! 4
           WRITE(outgsiunit)gridcross(:,:,3) ! 5
           WRITE(outgsiunit)gridcross(:,:,2) ! 6
           WRITE(outgsiunit)gridcross(:,:,3) ! 7
!here end writeup for grid output
!write out continues for met variables later on for selected variables
           
        ENDIF

!still need to calculate surface pressure from hydrostatic relation

!flip and save surface values PRES-F_lvl,DENS,ZF to vars

        vars(:,:,i)=tmp(:,:,1)
        
     ENDIF
     
  ENDDO
  
!psfc=PRES-F_lvl(1)+grav*DENS(1)*ZF(1)        
  vars(:,:,1)=vars(:,:,1)+g*vars(:,:,2)*vars(:,:,3)
  
!after grid info write out terrain height and psfc
  
  PRINT *,'Topography min,max= ',MINVAL(gridcross(:,:,4)),&
       &MAXVAL(gridcross(:,:,4))
  
  WRITE(outgsiunit)gridcross(:,:,4)
  
  PRINT *,'psfc min,max= ',MINVAL(vars(:,:,1)),MAXVAL(vars(:,:,1))
  
  WRITE(outgsiunit)vars(:,:,1)
  
!and follow with met variables
  
  DO i=1,nmetcross
     IF (read3('INPUT',metcrossnames(i),ALLAYS3,jdate,jtime,tmp)) THEN
        PRINT *,'Read ',metcrossnames(i),' from ', TRIM(metcrossfile)
        PRINT *,MAXVAL(tmp),MINVAL(tmp)
        vars=tmp
        DO k=1,nsig
           WRITE(outgsiunit)vars(:,:,k)
        ENDDO
     ELSE
        PRINT *,'Error reading ',metcrossnames(i),' from ', &
             &TRIM(metcrossfile)
        PRINT *,'Stopping'
        STOP
     ENDIF
  ENDDO
  
  IF (.NOT. close3('INPUT') ) THEN
     PRINT*,'failed closing ',metcrossfile
     STOP
  ENDIF
  
  DEALLOCATE(tmp)

!@mzp 23012015 

  vars=1.

  DO k=1,nsig
     WRITE(outgsiunit)vars(:,:,k)
  ENDDO

  DO k=1,nsig
     WRITE(outgsiunit)vars(:,:,k)
  ENDDO

  iflag=setenvvar('INPUT',TRIM(cmaqfile))
  
  IF (.NOT. OPEN3('INPUT',FSREAD3,'cmaqread') ) THEN
     PRINT*,'failed opening ',cmaqfile
     STOP
  ENDIF
  
!  IF(.NOT.desc3('INPUT')) THEN
!     PRINT*,'failed geting information from ',TRIM(cmaqfile)
!     STOP
!  ENDIF
!for some reason the above fails


 
!@mzp 23012015 
!  DEALLOCATE(tmp)

  ALLOCATE(tmp(ncols3d,nrows3d,nlays3d))

  DO i=1,naero
     IF (read3('INPUT',aeronames(i),ALLAYS3,jdate,jtime,tmp)) THEN
        PRINT *,'Read ',aeronames(i),' from ', TRIM(cmaqfile),i
        PRINT *,MAXVAL(tmp),MINVAL(tmp)
        vars=tmp
        DO k=1,nsig
           WRITE(outgsiunit)vars(:,:,k)
        ENDDO
     ELSE
        PRINT *,'Error reading ',aeronames(i),' from ', TRIM(cmaqfile)
        PRINT *,'Stopping'
        STOP
     ENDIF
  ENDDO

  IF (.NOT. close3('INPUT') ) THEN
     PRINT*,'failed closing ',cmaqfile
     STOP
  ENDIF

  CLOSE(outgsiunit)

  IF (.NOT. shut3()) then
     PRINT*,'failed shutting '
     STOP
  ENDIF
  
END PROGRAM cmaq2gsi
