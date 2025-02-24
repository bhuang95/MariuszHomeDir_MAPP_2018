PROGRAM aeronet_txt2bufr

!MP modified from Ming's bufr_encode_anow
!
! encode aeronet.txt file to bufr
! since it is a one-time process don't bother with complicated encoding
! and just read input file for every cycle time
!for some reason reading in GSI does not wok properly


!  USE kinds, ONLY : r_double

  IMPLICIT NONE
 
  CHARACTER(80):: hdstr='SAID SID CLATH CLONH YEAR MNTH DAYS HOUR MINU AOT1640 AOT1020 AOT870 AOT675 AOT500 AOT440 AOT340'
  INTEGER, PARAMETER :: nfields=16
  REAL(8) :: hdr(nfields,1)
  
  CHARACTER(len=8) :: subset
  INTEGER :: unit_out=10,unit_table=20,inunit=30
  INTEGER :: iret
  
  INTEGER, PARAMETER :: nmaxobs=100000 !3-month file has 26k obs
  INTEGER, PARAMETER :: naods=7

  REAL, PARAMETER :: unknown=-9999.,unknownp=unknown+1.
  REAL, PARAMETER :: timewindow=48. 

  INTEGER,ALLOCATABLE, DIMENSION(:) :: year,month,day,hour,mins
  INTEGER :: iy,im,idd,ihh
  INTEGER :: nobs,i,ii,idate
  REAL, ALLOCATABLE, DIMENSION(:) :: lat,lon,height
  REAL, ALLOCATABLE, DIMENSION(:,:) :: aod
  INTEGER, ALLOCATABLE, DIMENSION(:) :: sitechar
  REAL(8), ALLOCATABLE, DIMENSION(:) :: rcode
  REAL(8) :: rccode
  CHARACTER(len=11) :: ccode
  CHARACTER(len=10) :: cdate
  CHARACTER(len=1) :: trash
  REAL :: dhr
  INTEGER :: mincycle,minobs
  INTEGER,DIMENSION(5):: idate5
  
  CHARACTER(len=250) :: infile_aeronet,outfile,outdir
  INTEGER :: ist,iargc

  EQUIVALENCE(rccode,ccode)
  
  PRINT *,'for some reason reading in GSI does not wok properly'

  stop

  IF (iargc() < 3) THEN
     PRINT *,'Requires 3 input parameters: see script'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile_aeronet)
  CALL getarg(2,outdir)
  CALL getarg(3,cdate)
  READ(cdate,'(i10)')idate

  ALLOCATE(rcode(nmaxobs),lat(nmaxobs),lon(nmaxobs),&
       height(nmaxobs))
  ALLOCATE(aod(nmaxobs,naods))
  ALLOCATE(year(nmaxobs),month(nmaxobs),sitechar(nmaxobs))
  ALLOCATE(day(nmaxobs),hour(nmaxobs),mins(nmaxobs))

  OPEN(inunit,file=TRIM(infile_aeronet),form='formatted')
  READ(inunit,*)trash
  
  i=1
  
  DO WHILE (.TRUE.)
     
     READ(inunit,'(5i5,2f11.5,7e15.7,i3,f10.1,a11)',END=101)&
          year(i),month(i),day(i),hour(i),mins(i),&
          lat(i),lon(i),aod(i,:),sitechar(i),height(i),ccode

     rcode(i)=rccode

     i=i+1

  ENDDO

101 CONTINUE

  CLOSE(inunit)

  nobs=i-1

! set data values
  subset='NC008043'   
  
  outfile=TRIM(outdir)//'/aeronet.'//TRIM(cdate)//'.bufr'
  WRITE(*,'(a,a)') 'generate aeronet BUFR file:',TRIM(outfile)
  
  WRITE(*,'(a)') 'write out aeronet data'

  READ(cdate,'(i4,3i2)') iy,im,idd,ihh
  idate5(1)=iy
  idate5(2)=im
  idate5(3)=idd
  idate5(4)=ihh
  idate5(5)=0
  CALL w3fs21(idate5,mincycle)    !  cycle ref time in minutes relative
  
! encode
  OPEN(unit_table,file='bufrtab_aeronet.008')
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
        hdr(1,1)=783
        hdr(2,1)=rcode(i)
        hdr(3,1)=lat(i)
        hdr(4,1)=lon(i)
        hdr(5,1)=year(i)
        hdr(6,1)=month(i)
        hdr(7,1)=day(i)
        hdr(8,1)=hour(i)
        hdr(9,1)=mins(i)
        hdr(10:16,1)=aod(i,1:naods)

        WRITE(*,'(e15.7,5I5,7e15.7)') rcode(i),idate5(1:5),aod(i,:)
        CALL ufbint(unit_out,hdr,nfields,1,iret,hdstr)
        CALL writsb(unit_out)
     ENDIF
  ENDDO

  CALL closmg(unit_out)
  CALL closbf(unit_out)
  CLOSE(unit_table)

  
END PROGRAM aeronet_txt2bufr
