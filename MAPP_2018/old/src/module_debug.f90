module module_debug

   integer, parameter :: IO_NODE = 0 
   integer :: my_proc_id = 0 

   integer, parameter :: QUIET=-100, LOGFILE=-2, DEBUG=0, INFORM=1, WARN=2, ERROR=3, STDOUT=100

   integer :: the_debug_level = DEBUG

   logical :: have_set_logname = .false.

   contains

   subroutine set_debug_level(ilev)

      implicit none
     
      ! Arguments
      integer, intent(in) :: ilev

      the_debug_level = ilev

   end subroutine set_debug_level


   subroutine mprintf(assertion, level, fmtstring, &
                      i1, i2, i3, i4, i5, i6, &
                      f1, f2, f3, f4, f5, f6, &
                      s1, s2, s3, s4, s5, s6)

      implicit none

      ! Arguments
      integer, intent(in) :: level
      logical, intent(in) :: assertion
      character (len=*), intent(in) :: fmtstring
      integer, intent(in), optional :: i1, i2, i3, i4, i5, i6
      real, intent(in), optional :: f1, f2, f3, f4, f5, f6
      character (len=*), intent(in), optional :: s1, s2, s3, s4, s5, s6

      ! Local variables 
      integer :: idxi, idxf, idxs, istart, i, iend, ia
      real :: fa
      character (len=8) :: cur_date
      character (len=10) :: cur_time
      character (len=10) :: print_date
      character (len=12) :: print_time
      character (len=128) :: sa
      character (len=1024) :: ctemp

!      if (.not. have_set_logname) then
!         write(ctemp,'(a)') 'logfile.log'
!         call cio_set_log_filename(ctemp,len_trim(ctemp))
!         have_set_logname = .true.
!      end if

      idxi = 1
      idxf = 1
      idxs = 1
      istart = 1
      iend = len_trim(fmtstring)

      if (assertion) then

         ! If this is a debug message give up if level is not high enough
         if (level == DEBUG .and. the_debug_level > DEBUG) return 

         if (level /= STDOUT) then 
            call date_and_time(date=cur_date,time=cur_time)
            write(print_date,'(a10)') cur_date(1:4)//'-'//cur_date(5:6)//'-'//cur_date(7:8)
            write(print_time,'(a12)') cur_time(1:2)//':'//cur_time(3:4)//':'//cur_time(5:10)
            write(ctemp,'(a)') print_date//' '//print_time//' --- '
!            call cio_prints(1,ctemp,len(print_date//' '//print_time//' --- '))
         end if

         if (level == DEBUG) then
            write(ctemp,'(a)') 'DEBUG: '
!            call cio_prints(1,ctemp,7)
         else if (level == INFORM) then
            write(ctemp,'(a)') 'INFORM: '
!            if (level >= the_debug_level) &
!               call cio_prints(0,ctemp,8)
!            call cio_prints(1,ctemp,8)
         else if (level == WARN) then
            write(ctemp,'(a)') 'WARNING: '
!            if (level >= the_debug_level) &
!               call cio_prints(0,ctemp,9)
!            call cio_prints(1,ctemp,9)
         else if (level == ERROR) then
            write(ctemp,'(a)') 'ERROR: '
!            if (level >= the_debug_level) &
!               call cio_prints(0,ctemp,7)
!            call cio_prints(1,ctemp,7)
         end if
      
         i = index(fmtstring(istart:iend),'%')
         do while (i > 0 .and. i < iend)
            i = i + istart - 1
            write(ctemp,'(a)') fmtstring(istart:i-1)
!            if (level >= the_debug_level .and. level /= DEBUG) &
!               call cio_prints(0,ctemp,i-istart)
!            if (level /= STDOUT) &
!               call cio_prints(1,ctemp,i-istart)
   
            if (fmtstring(i+1:i+1) == '%') then
               write(ctemp,'(a)') '%'
!               if (level >= the_debug_level .and. level /= DEBUG) &
!                  call cio_prints(0,ctemp,1)
!               if (level /= STDOUT) &
!                  call cio_prints(1,ctemp,1)
                            
            else if (fmtstring(i+1:i+1) == 'i') then
               if (idxi == 1 .and. present(i1)) then
                  ia = i1
               else if (idxi == 2 .and. present(i2)) then
                  ia = i2
               else if (idxi == 3 .and. present(i3)) then
                  ia = i3
               else if (idxi == 4 .and. present(i4)) then
                  ia = i4
               else if (idxi == 5 .and. present(i5)) then
                  ia = i5
               else if (idxi == 6 .and. present(i6)) then
                  ia = i6
               end if
   
!               if (level >= the_debug_level .and. level /= DEBUG) &
!                  call cio_printi(0,ia)
!               if (level /= STDOUT) &
!                  call cio_printi(1,ia)

               idxi = idxi + 1
   
            else if (fmtstring(i+1:i+1) == 'f') then
               if (idxf == 1 .and. present(f1)) then
                  fa = f1
               else if (idxf == 2 .and. present(f2)) then
                  fa = f2
               else if (idxf == 3 .and. present(f3)) then
                  fa = f3
               else if (idxf == 4 .and. present(f4)) then
                  fa = f4
               else if (idxf == 5 .and. present(f5)) then
                  fa = f5
               else if (idxf == 6 .and. present(f6)) then
                  fa = f6
               end if
   
!               if (level >= the_debug_level .and. level /= DEBUG) &
!                  call cio_printf(0,fa)
!               if (level /= STDOUT) &
!                  call cio_printf(1,fa)

               idxf = idxf + 1
   
            else if (fmtstring(i+1:i+1) == 's') then
               if (idxs == 1 .and. present(s1)) then
                  sa = s1
               else if (idxs == 2 .and. present(s2)) then
                  sa = s2
               else if (idxs == 3 .and. present(s3)) then
                  sa = s3
               else if (idxs == 4 .and. present(s4)) then
                  sa = s4
               else if (idxs == 5 .and. present(s5)) then
                  sa = s5
               else if (idxs == 6 .and. present(s6)) then
                  sa = s6
               end if
   
               write(ctemp,'(a)') trim(sa)
!               if (level >= the_debug_level .and. level /= DEBUG) &
!                  call cio_prints(0,ctemp,len_trim(sa))
!               if (level /= STDOUT) &
!                  call cio_prints(1,ctemp,len_trim(sa))
               idxs = idxs + 1
   
            end if
   
            istart = i+2
            i = index(fmtstring(istart:iend),'%')
         end do
   
         write(ctemp,'(a)') fmtstring(istart:iend)//achar(10)  ! Add newline character 0xA
!         if (level >= the_debug_level .and. level /= DEBUG) &
!            call cio_prints(0,ctemp,iend-istart+2)
!         if (level /= STDOUT) &
!            call cio_prints(1,ctemp,iend-istart+2)

         if (level == ERROR) then
            stop
         end if

      end if


   end subroutine mprintf

end module module_debug
