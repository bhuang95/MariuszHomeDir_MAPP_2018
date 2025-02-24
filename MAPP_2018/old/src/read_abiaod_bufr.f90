PROGRAM read_abiaod_bufr
!
! example of reading observations from bufr
! converted to read_abiaod_bufr by mzp
! corrected for NC008042 (after Brad)

  IMPLICIT NONE

  CHARACTER(80):: &
       &hdstr_1='SAID YEAR MNTH DAYS HOUR MINU CLATH CLONH OPTD STYP QAOD'

  CHARACTER (len=53) :: aerogstr = &
       'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI'

  INTEGER, PARAMETER  :: nchannels=1

  REAL(8) :: hdr_1(11,1)
  REAL(8), dimension( 10) :: hdraerog

  INTEGER :: ireadmg,ireadsb
  CHARACTER(8) subset
  INTEGER :: unit_in=10
  INTEGER :: idate,iret,num_message,num_subset

! decode
  OPEN(unit_in,file='abiaod.bufr',action='read',form='unformatted')
  CALL openbf(unit_in,'IN',unit_in)
  CALL datelen(10)
  num_message=0
  msg_report: DO WHILE (ireadmg(unit_in,subset,idate) == 0)
     num_message=num_message+1
     num_subset = 0
     WRITE(*,'(I10,I7,a10)') idate,num_message,subset
     sb_report: DO WHILE (ireadsb(unit_in) == 0)
        num_subset = num_subset+1
!        CALL ufbint(unit_in,hdr_1,11,1,iret,hdstr_1)
!        WRITE(*,'(2I5,12e15.7)') num_subset,iret,hdr_1
!        PRINT *,hdstr_1
        CALL ufbint(unit_in,hdraerog,10,1,iret,aerogstr)
        WRITE(*,'(2I5,12e15.7)') num_subset,iret,hdraerog
        PRINT *,aerogstr
        CYCLE
!        CALL ufbrep(unit_in,hdr_2,1,nchannels,iret,hdstr_2)
!        WRITE(*,'(2i5,12e15.7)') num_subset,iret,hdr_2
!        PRINT *,hdstr_2
!        CALL ufbrep(unit_in,hdr_3,1,nchannels,iret,hdstr_3)
!        WRITE(*,'(2i5,12e15.7)') num_subset,iret,hdr_3
!        PRINT *,hdstr_3
!        CALL ufbint(unit_in,hdr_4,2,1,iret,hdstr_4)
!        WRITE(*,'(2I5,12e15.7)') num_subset,iret,hdr_4
!        PRINT *,hdstr_4
     ENDDO sb_report
  ENDDO msg_report

  CALL closbf(unit_in)
  
END PROGRAM read_abiaod_bufr
