MODULE module_jedi_obs

!obs data

  TYPE, ABSTRACT :: obs_data
   CONTAINS
     PROCEDURE(obsdata_create), DEFERRED :: create  ! Constructor
     PROCEDURE(obsdata_delete), DEFERRED :: delete  ! Destructor
     PROCEDURE(obsdata_read), DEFERRED :: read    ! Read data
     PROCEDURE(obsdata_write), DEFERRED :: write   ! Write data
     PROCEDURE(obsdata_print), DEFERRED  :: print   ! Prints human readable info
  END TYPE obs_data

  ABSTRACT INTERFACE 
     SUBROUTINE obsdata_create(this)
       IMPORT obs_data
       CLASS(obs_data) :: this
     END SUBROUTINE obsdata_create
  END INTERFACE

  ABSTRACT INTERFACE 
     SUBROUTINE obsdata_delete(this)
       IMPORT obs_data
       CLASS(obs_data) :: this
     END SUBROUTINE obsdata_delete
  END INTERFACE

  ABSTRACT INTERFACE 
     SUBROUTINE obsdata_read(this)
       IMPORT obs_data
       CLASS(obs_data) :: this
     END SUBROUTINE obsdata_read
  END INTERFACE

  ABSTRACT INTERFACE 
     SUBROUTINE obsdata_write(this)
       IMPORT obs_data
       CLASS(obs_data) :: this
     END SUBROUTINE obsdata_write
  END INTERFACE

  ABSTRACT INTERFACE 
     SUBROUTINE obsdata_print(this)
       IMPORT obs_data
       CLASS(obs_data) :: this
     END SUBROUTINE obsdata_print
  END INTERFACE

!obs error
  
  TYPE, ABSTRACT :: obs_error
   CONTAINS
     PROCEDURE(obserr_create), DEFERRED :: create          ! Constructor
     PROCEDURE(obserr_delete), DEFERRED :: delete          ! Destructor
     PROCEDURE(obserr_qc),     DEFERRED :: quality_control ! Apply QC
!     PROCEDURE(obserr_covar),  DEFERRED :: linearize       ! Generate covariance matrix
     PROCEDURE(obserr_print),  DEFERRED :: print           ! Prints human readable info
  END TYPE obs_error

  ABSTRACT INTERFACE
     SUBROUTINE obserr_create(self)!, obsdb, config)
       IMPORT obs_error
       CLASS(obs_error), INTENT(inout) :: self
!       TYPE(obs_space), INTENT(in)     :: obsdb
!       TYPE(config), INTENT(in)        :: config
     END SUBROUTINE obserr_create
  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE obserr_delete(self)
       IMPORT obs_error
       CLASS(obs_error), INTENT(inout) :: self
     END SUBROUTINE obserr_delete
  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE obserr_qc(self)!, ???, ???)  ! What does QC need?
       IMPORT obs_error
       CLASS(obs_error), INTENT(in)     :: self  
     END SUBROUTINE obserr_qc
  END INTERFACE

!  ABSTRACT INTERFACE
!     FUNCTION obserr_linearize(self, config)
!       CLASS(obs_error_covariance), POINTER :: obserr_linearize
!       CLASS(obs_error), INTENT(in)         :: self  
!       TYPE(config), INTENT(in)             :: config
!     END FUNCTION obserr_linearize
!  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE obserr_print(self)
       IMPORT obs_error
       CLASS(obs_error), INTENT(in) :: self
     END SUBROUTINE obserr_print
  END INTERFACE

END MODULE module_jedi_obs

