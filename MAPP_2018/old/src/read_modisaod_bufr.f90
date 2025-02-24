PROGRAM read_modisaod_bufr
!
! example of reading observations from bufr
! converted to read_viirsaod_bufr by mzp
! corrected for NC008043 (later version of bufr table)

  IMPLICIT NONE

  CHARACTER(80):: &
       &hdstr_1='SAID AODS',&
       &hdstr_2='CLONH CLATH YEAR MNTH DAYS HOUR MINU SECO SOZA SOLAZI SCATTA OPTD AEROTP',&
       &hdstr_3='STYP DBCF QAOD'

  REAL(8) :: hdr_1(2,1),hdr_2(13,1),hdr_3(3,1)

  INTEGER :: ireadmg,ireadsb
  CHARACTER(8) subset
  INTEGER :: unit_in=10
  INTEGER :: idate,iret,num_message,num_subset

! decode
  OPEN(unit_in,file='modisaod.bufr',action='read',form='unformatted')
  CALL openbf(unit_in,'IN',unit_in)
  CALL datelen(10)
  num_message=0
  msg_report: DO WHILE (ireadmg(unit_in,subset,idate) == 0)
     num_message=num_message+1
     num_subset = 0
     WRITE(*,'(I10,I7,a10)') idate,num_message,subset
     sb_report: DO WHILE (ireadsb(unit_in) == 0)
        num_subset = num_subset+1
        CALL ufbint(unit_in,hdr_1,2,1,iret,hdstr_1)
        WRITE(*,'(2I5,20e15.7)') num_subset,iret,hdr_1
        PRINT *,hdstr_1
        CALL ufbrep(unit_in,hdr_2,13,1,iret,hdstr_2)
        WRITE(*,'(2i5,20e15.7)') num_subset,iret,hdr_2
        PRINT *,hdstr_2
        CALL ufbrep(unit_in,hdr_3,3,1,iret,hdstr_3)
        WRITE(*,'(2i5,20e15.7)') num_subset,iret,hdr_3,hdr_2(12,1)
        PRINT *,hdstr_3
     ENDDO sb_report
  ENDDO msg_report

  CALL closbf(unit_in)
  
END PROGRAM read_modisaod_bufr
