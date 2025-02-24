MODULE variable_interface

!=======================================================================

! Define associated modules and subroutines

!-----------------------------------------------------------------------

  USE kinds

!-----------------------------------------------------------------------

  USE mpi_interface
!  USE namelist_def

!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------

! Define interfaces and attributes for module routines

  PRIVATE
  PUBLIC :: varinfo
!public :: variable_lookup
  PUBLIC :: variable_clip

!-----------------------------------------------------------------------

! Define all data and structure types for routine; these variables
! are variables required by the subroutines within this module

  TYPE varinfo
     CHARACTER(len=20)                                                 :: var_name
     CHARACTER(len=20)                                                 :: nems_name
     CHARACTER(len=20)                                                 :: nems_levtyp
     INTEGER                                                           :: nems_lev
     CHARACTER(len=20)                                                 :: itrptyp
     LOGICAL                                                           :: clip
     INTEGER                                                           :: ndims
  END TYPE varinfo ! type varinfo

!-----------------------------------------------------------------------

CONTAINS

!=======================================================================

  SUBROUTINE variable_clip(grid)


    REAL(r_double)                                                       :: grid(:)
    REAL(r_double)                                                       :: clip

    clip = TINY(grid(1))
    WHERE(grid .LE. DBLE(0.0)) grid = clip

  END SUBROUTINE variable_clip

!=======================================================================

END MODULE variable_interface
