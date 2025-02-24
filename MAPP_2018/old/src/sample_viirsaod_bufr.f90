PROGRAM sample_viirsaod_bufr
!
!
! converted to read_viirsaod_bufr by mzp
! corrected for NC008043 (later version of bufr table)
! sample write bufr file for viirs


  IMPLICIT NONE

  CHARACTER(80):: &
       &hdstr_1='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI',&
       &hdstr_2='CHWL',&
       &hdstr_3='AOPT',&
       &hdstr_4='RSST AOTQ'

  CHARACTER(len=250) :: filename_aot_in,tablename,filename_aot_out

  INTEGER, PARAMETER  :: nchannels=12

  REAL(8) :: hdr_1(10,1),hdr_2(1,nchannels),hdr_3(1,nchannels),hdr_4(2,1)

  INTEGER :: ireadmg,ireadsb
  CHARACTER(8) subset
  INTEGER :: unit_in=10,unit_out=30,unit_table=20
  INTEGER :: idate,iret,num_message,num_subset,iargc

  CHARACTER(len=2) :: csample
  INTEGER :: isample

  IF (iargc() < 4) THEN
     PRINT *,'needs for inputs - stopping'
     STOP
  ENDIF
  

  CALL getarg(1,filename_aot_in)
  CALL getarg(2,tablename)
  CALL getarg(3,csample) !sampling frequency
  CALL getarg(4,filename_aot_out)
  
  READ(csample,'(i2.2)')isample

  OPEN(unit_in,file=TRIM(filename_aot_in),action='read',form='unformatted')
  CALL openbf(unit_in,'IN',unit_in)

  OPEN(unit_table,file=tablename,form='formatted')

  OPEN(unit_out,file=TRIM(filename_aot_out),action='write' &
       ,form='unformatted')
  CALL openbf(unit_out,'OUT',unit_table)

  CALL datelen(10)
  num_message=0
  msg_report: DO WHILE (ireadmg(unit_in,subset,idate) == 0)
     IF (num_message == 0) CALL openmb(unit_out,subset,idate)
     num_message=num_message+1
!     WRITE(*,'(I10,I7,a10)') idate,num_message,subset
     num_subset = 0
     sb_report: DO WHILE (ireadsb(unit_in) == 0)
        num_subset = num_subset+1
        CALL ufbint(unit_in,hdr_1,10,1,iret,hdstr_1)
        CALL ufbrep(unit_in,hdr_2,1,nchannels,iret,hdstr_2)
        CALL ufbrep(unit_in,hdr_3,1,nchannels,iret,hdstr_3)
        CALL ufbint(unit_in,hdr_4,2,1,iret,hdstr_4)
        IF (MOD(num_subset,isample) == 0) THEN
!           PRINT *,'writing ',hdstr_1
!           WRITE(*,'(2I5,12e15.7)') num_subset,iret,hdr_1
           CALL ufbint(unit_out,hdr_1,10,1,iret,hdstr_1)
           CALL ufbrep(unit_out,hdr_2,1,nchannels,iret,hdstr_2)        
           CALL ufbrep(unit_out,hdr_3,1,nchannels,iret,hdstr_3)
           CALL ufbint(unit_out,hdr_4,2,1,iret,hdstr_4)
           CALL writsb(unit_out)
        ENDIF

     ENDDO sb_report
  ENDDO msg_report

  CALL closbf(unit_in)
  CALL closbf(unit_out)

  CLOSE(unit_table)
  
END PROGRAM sample_viirsaod_bufr
