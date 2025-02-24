PROGRAM read_anow_bufr
!
! example of reading observations from bufr
!
! converted to read read_anow_bufr by mp

 implicit none

 character(80):: hdstr='PTID CLONH CLATH TPHR TYPO COPOPM'
 REAL(8) :: hdr(6,1)

 integer :: ireadmg,ireadsb
 character(8) subset
 integer :: unit_in=10
 integer :: idate,iret,num_message,num_subset

! decode
 open(unit_in,file='sample.bufr',action='read',form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call datelen(10)
   num_message=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     num_message=num_message+1
     num_subset = 0
     write(*,'(I10,I4,a10)') idate,num_message,subset
     sb_report: do while (ireadsb(unit_in) == 0)
       num_subset = num_subset+1
       call ufbint(unit_in,hdr,6,1 ,iret,hdstr)
       write(*,'(2I5,6e15.7)') num_subset,iret,hdr
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

END PROGRAM read_anow_bufr
