PROGRAM gsi2cmaq

! to read and output gsi cmaq files to cmaq binary input
! Mariusz Pagowski, CIRA/NOAA Boulder,Sept 20, 2010

  USE kinds

  IMPLICIT NONE

  INCLUDE 'PARMS3.EXT'      ! i/o API
  INCLUDE 'FDESC3.EXT'      ! i/o API
  INCLUDE 'IODECL3.EXT'     ! i/o API
  
  INTEGER, PARAMETER :: &
       &naero=33,maxlength=12,nother=6
  REAL :: tiny=1.e-7,maxratio=10.
  
!this needs to match order of GSI output
  CHARACTER(len=maxlength), DIMENSION(naero) :: aeronames
  CHARACTER(len=maxlength), DIMENSION(nother) :: othernames =&
       &(/'NUMATKN     ','NUMACC      ','SRFATKN     ','SRFACC      ',&
       &  'AH2OI       ','AH2OJ       '/)

  CHARACTER(len=200) :: cmaqfile,ingsifile
  CHARACTER(len=8) :: yyyymmdd
  CHARACTER(len=6) :: hhmmss

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: tmp
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: ratio


  INTEGER :: i,j,k,ii,jj,iargc,n,year,month,mday,hour,mins,sec,&
       &jdate,jtime,julian,&
       &ingsiunit=51
  LOGICAL :: iflag,setenvvar,fail=.FALSE.
  CHARACTER(len=3) :: cingsiunit

  INTEGER(i_kind), DIMENSION(6) :: regional_time,regional_time0
  INTEGER(i_kind) :: nlon_regional,nlat_regional,nsig
  INTEGER(i_kind) :: nlon_regional0,nlat_regional0,nsig0

  IF(iargc().NE.4) THEN
     PRINT*,' need seven string inputs, got ',iargc()
     STOP
  ENDIF

  WRITE(cingsiunit,'(i3)')ingsiunit
!  CALL setrteopts('ufmt_littleendian='//cingsiunit)

  CALL getarg(1,cmaqfile)
  CALL getarg(2,ingsifile)
  CALL getarg(3,yyyymmdd) !YYYYMMDD
  CALL getarg(4,hhmmss) !HHMMSS

  READ(yyyymmdd(1:4),'(i4)')year
  READ(yyyymmdd(5:6),'(i2)')month
  READ(yyyymmdd(7:8),'(i4)')mday

  READ(hhmmss(1:2),'(i4)')hour
  READ(hhmmss(3:4),'(i2)')mins
  READ(hhmmss(5:6),'(i4)')sec

  regional_time(1)=year
  regional_time(2)=month
  regional_time(3)=mday
  regional_time(4)=hour
  regional_time(5)=mins
  regional_time(6)=sec

  jdate = 1000 * year  +  julian (year, month, mday)
  jtime=10000*hour+100*mins+sec

  n=init3()

  iflag=setenvvar('NOTCDF_FATAL','F')

  iflag=setenvvar('INOUT',TRIM(cmaqfile))
  
  IF (.NOT. OPEN3('INOUT',FSRDWR3,'cmaqread') ) THEN
     PRINT*,'failed opening ',cmaqfile
     STOP
  ENDIF
  
  IF(.NOT.desc3('INOUT')) THEN
     PRINT*,'failed getting information from ',TRIM(cmaqfile)
     STOP
  ENDIF

  nlon_regional=ncols3d
  nlat_regional=nrows3d
  nsig=nlays3d

!for some reason this fails for 4.7


  OPEN(unit=ingsiunit,file=ingsifile,form='unformatted')
  
  READ(ingsiunit)regional_time0,nlon_regional0,nlat_regional0,&
          &nsig0
  DO i=1,6
     IF (regional_time0(i) /= regional_time(i)) fail=.TRUE.
  ENDDO

  nlon_regional=nlon_regional0
  nlat_regional=nlat_regional0
  nsig=nsig0


  ncols3d=nlon_regional
  nrows3d=nlat_regional
  nlays3d=nsig

  IF (fail .OR. &
       &nlon_regional0 /= nlon_regional .OR. &
       &nlat_regional0 /= nlat_regional .OR. &
       &nsig0 /= nsig) THEN

     PRINT *,'Inconsistencies between gsiinput and cmaq files'
     PRINT *,'Times'
     PRINT *,regional_time0
     PRINT *,regional_time
     PRINT *,nlon_regional0,nlon_regional, 'nlon_regional'
     PRINT *,nlat_regional0,nlat_regional, 'nlat_regional'
     PRINT *,nsig0,nsig, 'nsig'
     PRINT *,'Stopping'

     IF (.NOT. close3('INOUT') ) THEN
        PRINT*,'failed closing ',cmaqfile
        STOP
     ENDIF
     
     CLOSE(ingsiunit)
     
     IF (.NOT. shut3()) THEN
        PRINT*,'failed shutting '
        STOP
     ENDIF
     
     STOP
  ENDIF
  
  ALLOCATE(tmp(ncols3d,nrows3d,nlays3d),&
       &ratio(ncols3d,nrows3d,nlays3d))

  IF (read3('INOUT','A25J',ALLAYS3,jdate,jtime,ratio)) THEN
     PRINT *,'Read ','A25J',' from ', TRIM(cmaqfile)
  ELSE
     PRINT *,'Error reading ','A25J',' from ', TRIM(cmaqfile)
     PRINT *,'Stopping'
     STOP
  endif

  DO i=1,naero
     READ(ingsiunit)aeronames(i)
     DO k=1,nsig
        READ(ingsiunit)tmp(:,:,k)
     ENDDO
     
     if (aeronames(i)=='A25J') ratio=max(min(tmp/(max(ratio,tiny)),&
          &maxratio),tiny)
     
     PRINT *,aeronames(i),i

     IF (write3('INOUT',aeronames(i),jdate,jtime,tmp)) THEN
        PRINT *,'Read ',aeronames(i),' from ', TRIM(ingsifile)
        PRINT *,MAXVAL(tmp),MINVAL(tmp)
     ELSE
        PRINT *,'Error writing ',aeronames(i),' to ', TRIM(cmaqfile)
        PRINT *,'Stopping'
        STOP
     ENDIF
  ENDDO

  do k=1,nsig
     print *,'Ratio level minval maxval mean'
     print *,minval(ratio(:,:,k)),maxval(ratio(:,:,k)),&
          &sum(ratio(:,:,k))/(ncols3d*nrows3d)
  enddo

  DO i=1,nother
     IF (read3('INOUT',othernames(i),ALLAYS3,jdate,jtime,tmp)) THEN
        PRINT *,'Read ',othernames(i),' from ', TRIM(cmaqfile)
        PRINT *,MAXVAL(tmp),MINVAL(tmp)
        tmp=tmp*ratio

        IF (write3('INOUT',othernames(i),jdate,jtime,tmp)) THEN
           PRINT *,'Read ',othernames(i),' from ', TRIM(ingsifile)
           PRINT *,MAXVAL(tmp),MINVAL(tmp)
        ELSE
           PRINT *,'Error writing ',othernames(i),' to ', TRIM(cmaqfile)
           PRINT *,'Stopping'
           STOP
        ENDIF

     ELSE
        PRINT *,'Error reading ',othernames(i),' from ', TRIM(cmaqfile)
        PRINT *,'Stopping'
        STOP
     ENDIF
  ENDDO

  IF (.NOT. close3('INOUT') ) THEN
     PRINT*,'failed closing ',cmaqfile
     STOP
  ENDIF

  CLOSE(ingsiunit)

  IF (.NOT. shut3()) then
     PRINT*,'failed shutting '
     STOP
  ENDIF
  
END PROGRAM gsi2cmaq
