program bufr_encode_sample
!
!  example of writing one value into a bufr file
!
 implicit none
 
 character(80):: hdstr='PTID CLONH CLATH TPHR TYPO COPOPM'
 INTEGER, PARAMETER :: nfields=6
 real(8) :: hdr(6,1)
 
 character(8) subset
 integer :: unit_out=10,unit_table=20,inunit=30
 integer :: idate,iret
 
  INTEGER, PARAMETER :: nmaxstations=500000.
  REAL, PARAMETER :: unknown=-9999.,unknownp=unknown+1.

  INTEGER,ALLOCATABLE, DIMENSION(:) :: year,month,day,hour,min
  INTEGER :: iy,im,idd,ihh
  INTEGER :: nstations,i,ii
  REAL, ALLOCATABLE, DIMENSION(:) :: lat,lon,height,conc
  INTEGER, ALLOCATABLE, DIMENSION(:) :: sitechar
  CHARACTER(len=9), ALLOCATABLE, DIMENSION(:) :: code
  CHARACTER(len=10) :: cdate
  INTEGER :: sid
  real:: timewindow, dhr
  integer :: mincycle,minobs
  integer,dimension(5):: idate5

  CHARACTER(len=80) :: cfile
  integer:: ist
 
  namelist/setup/ idate,timewindow

  timewindow=3.0
  idate=2012053103  ! YYYYMMDDHH

  open(11,file='namelist.anow')
     read(11,setup)
  close(11)

  ALLOCATE(code(nmaxstations),lat(nmaxstations),lon(nmaxstations),&
       height(nmaxstations),sitechar(nmaxstations))
  ALLOCATE(conc(nmaxstations))
  ALLOCATE(year(nmaxstations),month(nmaxstations))
  ALLOCATE(day(nmaxstations),hour(nmaxstations),min(nmaxstations))

  open(inunit,file='PM2_5_DRY_data_bundle')
  ii=0
  DO i=1,nmaxstations
      read(inunit,'(5i5,2f11.5,f7.2,i3,f10.1,a11)',iostat=ist)&
          year(i),month(i),day(i),hour(i),min(i),&
          lat(i),lon(i),conc(i),sitechar(i),height(i),code(i)
      if(ist==0) then
         ii=ii+1
      else
         exit
      endif
  ENDDO
  close(inunit)
  nstations=ii
!  DO i=1,nstations
!      write(*,'(5i5,2f11.5,f7.2,i3,f10.1,a11)')&
!          year,month,day,hour,min,&
!          lat(i),lon(i),conc(i),sitechar(i),height(i),code(i)
!  ENDDO

  
! set data values
  subset='NC008031'   ! upper-air reports

  write(cdate,'(I10)') idate
  cfile='anow.'//trim(cdate)//'.bufr'
  write(*,'(a,a)') 'generate airnow BUFR file:',trim(cfile)

  read (cdate,'(i4,3i2)') iy,im,idd,ihh
  idate5(1)=iy
  idate5(2)=im
  idate5(3)=idd
  idate5(4)=ihh
  idate5(5)=0
  call w3fs21(idate5,mincycle)    !  cycle ref time in minutes relative

! encode
  open(unit_table,file='bufrtab.008')
  open(unit_out,file=trim(cfile),action='write' &
               ,form='unformatted')
  call datelen(10)
  call openbf(unit_out,'OUT',unit_table)
    call openmb(unit_out,subset,idate)
       DO i=1,nstations
          dhr=0.0
          idate5(1)=year(i)
          idate5(2)=month(i)
          idate5(3)=day(i)
          idate5(4)=hour(i)
          idate5(5)=0
          call w3fs21(idate5,minobs)    !  analysis ref time in minutes
          dhr=float(minobs-mincycle)/60.0

          if(abs(dhr) <= timewindow) then
             read(code(i),'(I9)') sid
             hdr(1,1)=float(sid)
             hdr(2,1)=lon(i)
             hdr(3,1)=lat(i)
             hdr(4,1)=dhr
             hdr(5,1)=11.0
             hdr(6,1)=conc(i)*1.0e-9
             write(*,'(I10,f5.1,4I5,f10.5)') sid,dhr,idate5(1:4),conc(i)
             call ufbint(unit_out,hdr,nfields,1,iret,hdstr)
             call writsb(unit_out)
          endif
       ENDDO
    call closmg(unit_out)
  call closbf(unit_out)

end program
