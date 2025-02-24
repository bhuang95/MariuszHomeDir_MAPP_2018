MODULE module_aod
  
  USE module_jedi_obs
  USE module_jedi_oper
  USE module_jedi_state
  USE kinds, ONLY : r_kind, i_kind
  USE crtm_module, ONLY : crtm_channelinfo_n_channels
  USE crtm_interface, ONLY: init_crtm
  USE module_base

  IMPLICIT NONE

  TYPE, EXTENDS(obs_data) :: obs_data_aod
     INTEGER(i_kind) :: nlocs,nwavelengths
     REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: lat,lon
     CHARACTER(len=satname_length), ALLOCATABLE, DIMENSION(:) :: satnames
     REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: wavelengths
     REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: aods
     REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: quality
     REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: sfc

   CONTAINS
     PROCEDURE :: create => create_obs_aod
     PROCEDURE :: read => read_obs_aod
     PROCEDURE :: write => write_obs_aod
     PROCEDURE :: print => print_obs_aod
     PROCEDURE :: delete => delete_obs_aod
  END TYPE obs_data_aod

  TYPE, EXTENDS(obs_operator) :: op_aod
     LOGICAL :: init_pass
!     INTEGER(i_kind) :: nchanl,mype_diaghdr,mype
     CHARACTER(20)  :: isis !!length as in crtm
     CHARACTER(10)  :: obstype !length as in crtm
   CONTAINS
     PROCEDURE :: create => create_oper_aod
     PROCEDURE :: delete => delete_oper_aod
     PROCEDURE :: h_oper => h_oper_aod
     PROCEDURE :: input_variables => input_variables_aod
     PROCEDURE :: print => print_oper_aod
  END TYPE op_aod

CONTAINS

!aod opers  

  SUBROUTINE create_oper_aod(this) !not self
!initialize crtm, after allocation, fill gom & hofx with fill_model_vars
!from module_jedi_state

    CLASS(op_aod), INTENT(inout) :: this !not self
    TYPE(state_at_locations) :: gom
    PRINT *,'create_oper_aod: initialize crtm, allocate gom & hofx, fill gom fill_model_vars'
    CALL fill_model_vars(gom)

  END SUBROUTINE create_oper_aod

  SUBROUTINE h_oper_aod(this,gom,hofx)
!call_crtm directly rther than use subroutine call_crtm
    CLASS(op_aod) :: this
    TYPE(state_at_locations) :: gom
    TYPE(hofx_at_locations) :: hofx
    PRINT *,'h_oper_aod: execute forward aod operator (crtm)'
  END SUBROUTINE h_oper_aod

  FUNCTION input_variables_aod(this)
    CLASS(op_aod) :: this
    TYPE(state_at_locations), POINTER :: input_variables_aod
  END FUNCTION input_variables_aod

  SUBROUTINE print_oper_aod(this)
    CLASS(op_aod) :: this
  END SUBROUTINE print_oper_aod

  SUBROUTINE delete_oper_aod(this)
!destroy crtm & deallocate gom & hofx
    CLASS(op_aod) :: this
    PRINT *,'delete_oper_aod: destroy crtm & deallocate gom & hofx'
  END SUBROUTINE delete_oper_aod

!aod obs

  SUBROUTINE create_obs_aod(this)
    CLASS(obs_data_aod) :: this
    PRINT *,'create_obs_aod: allocate aod(wavelengths) and number of obs'
    ALLOCATE(this%wavelengths(10),this%aods(2,10))
  END SUBROUTINE create_obs_aod

  SUBROUTINE delete_obs_aod(this)
    CLASS(obs_data_aod) :: this
    PRINT *,'delete_obs_aod: delete aod obs'
    DEALLOCATE(this%wavelengths,this%aods)

  END SUBROUTINE delete_obs_aod

  SUBROUTINE read_obs_aod(this)
    CLASS(obs_data_aod) :: this
    PRINT *,'read_obs_aod: read obs'
    this%wavelengths(:)=5.
    this%aods(:,:)=1.
  END SUBROUTINE read_obs_aod

  SUBROUTINE write_obs_aod(this)
    CLASS(obs_data_aod) :: this
    PRINT *,'write_obs_aod: can write aod obs to a file' 
    OPEN(10,file='aod_test.txt',form='formatted')
    WRITE(10,*)this%wavelengths
    WRITE(10,*)this%aods
    CLOSE(10)
  END SUBROUTINE write_obs_aod

  SUBROUTINE print_obs_aod(this)
    CLASS(obs_data_aod) :: this
    PRINT *,'print_obs_aod: can print aod obs'
!    PRINT *,this%wavelengths
!    PRINT *,this%aods
  END SUBROUTINE print_obs_aod

END MODULE module_aod
