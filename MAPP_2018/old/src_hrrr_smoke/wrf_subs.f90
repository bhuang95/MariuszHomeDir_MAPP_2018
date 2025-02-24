MODULE module_wrf_error
!$$$   module documentation block
!
! module:  module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add module and subroutine doc blocks
!
! subroutines included:
!   wrf_at_debug_level           ---
!   init_module_wrf_error        ---
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  USE kinds, ONLY: i_kind
  IMPLICIT NONE

! set default to private
  PRIVATE
! set subroutines to public
  PUBLIC :: wrf_at_debug_level
  PUBLIC :: init_module_wrf_error
! set passed variables to public
  PUBLIC :: wrf_debug_level

  INTEGER(i_kind) :: wrf_debug_level = 0

CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  wrf_at_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    USE kinds, ONLY: i_kind
    IMPLICIT NONE

    INTEGER(i_kind) , INTENT(IN   ) :: level

    wrf_at_debug_level = ( level <= wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
!$$$   subprogram documentation block
!
! subprogram:  init_module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    IMPLICIT NONE
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

SUBROUTINE set_wrf_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  set_wrf_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  USE kinds, ONLY: i_kind
  IMPLICIT NONE

  INTEGER(i_kind) , INTENT(IN   ) :: level

  wrf_debug_level = level
  RETURN
END SUBROUTINE set_wrf_debug_level

SUBROUTINE get_wrf_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  get_wrf_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!
!   output argument list:
!     level    - debug level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  USE kinds, ONLY: i_kind
  IMPLICIT NONE

  INTEGER(i_kind) , INTENT(  OUT) :: level

  level = wrf_debug_level
  RETURN
END SUBROUTINE get_wrf_debug_level


SUBROUTINE wrf_debug( level , str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_debug
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  USE kinds, ONLY: i_kind
  IMPLICIT NONE

  CHARACTER*(*) str
  INTEGER(i_kind) , INTENT (IN   ) :: level
  INTEGER(i_kind)                  :: debug_level

  CALL get_wrf_debug_level( debug_level )
  IF ( level <= debug_level ) THEN
! old behavior
     CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_message
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!    str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*), INTENT(in   ) :: str

  WRITE(6,*) TRIM(str)
  PRINT*, TRIM(str)
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( TRIM(str), WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_message2
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!    str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*), INTENT(in   ) :: str

  WRITE(6,*) str
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( str, WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_error_fatal3
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     file_str -
!     line     -
!     str      -
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  USE kinds, ONLY: i_kind
  IMPLICIT NONE

  CHARACTER*(*)   , INTENT (in   ) :: file_str
  INTEGER(i_kind) , INTENT (IN   ) :: line  ! only print file and line if line > 0
  CHARACTER*(*)   , INTENT (in   ) :: str
  CHARACTER*256 :: line_str

  WRITE(line_str,'(i6)') line
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
! only print file and line if line is positive
  IF ( line > 0 ) THEN
     CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
! CALL wrf_abort
  STOP(199)
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_error_fatal
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     str      -
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*),INTENT(in   ) :: str

  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
!$$$   subprogram documentation block
!
! subprogram:  wrf_check_error
!
! abstract:   Check to see if expected value == actual value
!             If not, print message and exit.  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     expected -
!     actual   -
!     line     -
!     str
!     file_str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  USE kinds, ONLY: i_kind
  IMPLICIT NONE

  INTEGER(i_kind) , INTENT (IN   ) :: expected
  INTEGER(i_kind) , INTENT (IN   ) :: actual
  CHARACTER*(*)   , INTENT (in   ) :: str
  CHARACTER*(*)   , INTENT (in   ) :: file_str
  INTEGER(i_kind) , INTENT (IN   ) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected /= actual ) THEN
     WRITE (rc_str,*) '  Routine returned error code = ',actual
     str_with_rc = TRIM(str // rc_str)
     CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error

