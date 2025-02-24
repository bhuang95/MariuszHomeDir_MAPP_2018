PROGRAM airnow_txt2bufr

!MP modified from Ming's bufr_encode_anow
!
! encode pm2_5.txt file to bufr
! add pm10 readin and output to the same bufr file
! uses original files output from airnow2gsi_bundle.f90
! with valid times of obs at 1/2 hour 
! since it is a one-time process don't bother with complicated encoding
! and just read input file for every cycle time

!  USE kinds, ONLY : r_double

  IMPLICIT NONE
 
  CHARACTER(80):: hdstr='PTID CLONH CLATH TPHR TYPO COPOPM'
  INTEGER, PARAMETER :: nfields=6
  REAL(8) :: hdr(6,1),sid
  
  CHARACTER(len=8) :: subset
  INTEGER :: unit_out=10,unit_table=20,inunit=30
  INTEGER :: iret
  
  INTEGER, PARAMETER :: nmaxobs=2000000 !monthly files

  REAL, PARAMETER :: unknown=-9999.,unknownp=unknown+1.
  REAL, PARAMETER :: timewindow=3. ! 3hr window is plenty - normally 1hr

  INTEGER,ALLOCATABLE, DIMENSION(:) :: year,month,day,hour,mins
  INTEGER :: iy,im,idd,ihh
  INTEGER :: nobs,i,ii,idate
  REAL, ALLOCATABLE, DIMENSION(:) :: lat,lon,height,conc
  INTEGER, ALLOCATABLE, DIMENSION(:) :: sitechar
  INTEGER, ALLOCATABLE, DIMENSION(:) :: icode
  CHARACTER(len=10) :: cdate
  CHARACTER(len=1) :: trash
  REAL :: dhr
  INTEGER :: mincycle,minobs
  INTEGER,DIMENSION(5):: idate5
  
  CHARACTER(len=250) :: infile_pm2_5,infile_pm10,outfile,outdir
  INTEGER :: ist,iargc

  LOGICAL :: pm10_exist
  
  IF (iargc() < 4) THEN
     PRINT *,'Requires 4 input parameters: see script'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile_pm2_5)
  CALL getarg(2,infile_pm10)
  CALL getarg(3,outdir)
  CALL getarg(4,cdate)
  READ(cdate,'(i10)')idate

  ALLOCATE(icode(nmaxobs),lat(nmaxobs),lon(nmaxobs),&
       height(nmaxobs),sitechar(nmaxobs))
  ALLOCATE(conc(nmaxobs))
  ALLOCATE(year(nmaxobs),month(nmaxobs))
  ALLOCATE(day(nmaxobs),hour(nmaxobs),mins(nmaxobs))

  OPEN(inunit,file=TRIM(infile_pm2_5),form='formatted')
  READ(inunit,*)trash
  
  i=1
  
  DO WHILE (.TRUE.)
     
     READ(inunit,'(5i5,2f11.5,f7.2,i3,f10.1,i11)',END=101)&
          year(i),month(i),day(i),hour(i),mins(i),&
          lat(i),lon(i),conc(i),sitechar(i),height(i),icode(i)
     i=i+1

  ENDDO

101 CONTINUE

  CLOSE(inunit)

  nobs=i-1

! set data values
  subset='NC008032'   
  
  outfile=TRIM(outdir)//'/anow.'//TRIM(cdate)//'.bufr'
  WRITE(*,'(a,a)') 'generate airnow BUFR file:',TRIM(outfile)
  
  WRITE(*,'(a)') 'write out pm2.5 data'

  READ(cdate,'(i4,3i2)') iy,im,idd,ihh
  idate5(1)=iy
  idate5(2)=im
  idate5(3)=idd
  idate5(4)=ihh
  idate5(5)=0
  CALL w3fs21(idate5,mincycle)    !  cycle ref time in minutes relative
  
! encode
  OPEN(unit_table,file='bufrtab.008')
  OPEN(unit_out,file=TRIM(outfile),action='write' &
       ,form='unformatted')
  CALL datelen(10)
  CALL openbf(unit_out,'OUT',unit_table)
  CALL openmb(unit_out,subset,idate)
  DO i=1,nobs
     dhr=0.0
     idate5(1)=year(i)
     idate5(2)=month(i)
     idate5(3)=day(i)
     idate5(4)=hour(i)
     idate5(5)=mins(i)
     CALL w3fs21(idate5,minobs)    !  analysis ref time in minutes
      
     dhr=float(minobs-mincycle)/60.0
     IF(ABS(dhr) <= timewindow) THEN
        hdr(1,1)=icode(i)
        hdr(2,1)=lon(i)
        hdr(3,1)=lat(i)
        hdr(4,1)=dhr+0.5
!make 30min offset to be consistent with anow read in gsi
        hdr(5,1)=11.0
        hdr(6,1)=conc(i)*1.0e-9
!        WRITE(*,'(I11,f5.1,4I5,f10.5)') icode(i),dhr,idate5(1:4),conc(i)
        CALL ufbint(unit_out,hdr,nfields,1,iret,hdstr)
        CALL writsb(unit_out)
     ENDIF
  ENDDO


  INQUIRE(file=TRIM(infile_pm10),exist=pm10_exist)

  IF (.NOT. pm10_exist) GOTO 103

  OPEN(inunit,file=TRIM(infile_pm10),form='formatted')
  READ(inunit,*)trash
  
  i=1
  
  DO WHILE (.TRUE.)
     
     READ(inunit,'(5i5,2f11.5,f7.2,i3,f10.1,i11)',END=102)&
          year(i),month(i),day(i),hour(i),mins(i),&
          lat(i),lon(i),conc(i),sitechar(i),height(i),icode(i)
     i=i+1

  ENDDO

102 CONTINUE

  CLOSE(inunit)

  nobs=i-1

! set data values
  subset='NC008033'   
  
  WRITE(*,'(a)') 'write out pm10 data'

  READ(cdate,'(i4,3i2)') iy,im,idd,ihh
  idate5(1)=iy
  idate5(2)=im
  idate5(3)=idd
  idate5(4)=ihh
  idate5(5)=0
  CALL w3fs21(idate5,mincycle)    !  cycle ref time in minutes relative
  
! encode

  CALL openmb(unit_out,subset,idate)
  DO i=1,nobs
     dhr=0.0
     idate5(1)=year(i)
     idate5(2)=month(i)
     idate5(3)=day(i)
     idate5(4)=hour(i)
     idate5(5)=mins(i)
     CALL w3fs21(idate5,minobs)    !  analysis ref time in minutes
      
     dhr=float(minobs-mincycle)/60.0
     IF(ABS(dhr) <= timewindow) THEN
        hdr(1,1)=icode(i)
        hdr(2,1)=lon(i)
        hdr(3,1)=lat(i)
        hdr(4,1)=dhr+0.5
!make 30min offset to be consistent with anow read in gsi
        hdr(5,1)=11.0
        hdr(6,1)=conc(i)*1.0e-9
!        WRITE(*,'(I11,f5.1,4I5,f10.5)') icode(i),dhr,idate5(1:4),conc(i)
        CALL ufbint(unit_out,hdr,nfields,1,iret,hdstr)
        CALL writsb(unit_out)
     ENDIF
  ENDDO

103 CONTINUE

  CALL closmg(unit_out)
  CALL closbf(unit_out)
  CLOSE(unit_table)

  
END PROGRAM airnow_txt2bufr
