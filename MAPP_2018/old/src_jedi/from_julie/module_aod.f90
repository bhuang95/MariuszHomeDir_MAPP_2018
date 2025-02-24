MODULE module_aod
  
  USE module_jedi_obs
  USE module_jedi_opers
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
     PROCEDURE :: create => create_aod_obs
     PROCEDURE :: read => read_aod_obs
     PROCEDURE :: write => write_aod_obs
     PROCEDURE :: print => print_aod_obs
     PROCEDURE :: delete => delete_aod_obs
  END TYPE obs_data_aod

  TYPE, EXTENDS(obs_operator) :: op_aod
     LOGICAL :: init_pass
     INTEGER(i_kind) :: nchanl,mype_diaghdr,mype
     CHARACTER(20)  :: isis
     CHARACTER(10)  :: obstype
   CONTAINS
     PROCEDURE :: create => create_aod_oper
     PROCEDURE :: delete => delete_aod_oper
     PROCEDURE :: h_oper => h_aod_oper
     PROCEDURE :: input_variables => input_variables_aod
     PROCEDURE :: print => print_aod_oper
  END TYPE op_aod

CONTAINS

!aod opers  

  SUBROUTINE create_aod_oper(this)
!initialize crtm, after allocation, fill gom & hofx with fill_model_vars
!from module_jedi_state
    CLASS(op_aod), INTENT(inout) :: this
    PRINT *,'abc'
  END SUBROUTINE create_aod_oper

  SUBROUTINE delete_aod_oper(this)
!destroy crtm & deallocate gom & hofx
    CLASS(op_aod) :: this
  END SUBROUTINE delete_aod_oper
  
  SUBROUTINE h_aod_oper(this,gom,hofx)
!call_crtm
    CLASS(op_aod) :: this
    TYPE(state_at_locations) :: gom
    TYPE(hofx_at_locations) :: hofx
  END SUBROUTINE h_aod_oper

  FUNCTION input_variables_aod(this)
    CLASS(op_aod) :: this
    TYPE(state_at_locations), POINTER :: input_variables_aod
  END FUNCTION input_variables_aod

  SUBROUTINE print_aod_oper(this)
    CLASS(op_aod) :: this
  END SUBROUTINE print_aod_oper

!aod obs

  SUBROUTINE create_aod_obs(this)
    CLASS(obs_data_aod) :: this
    ALLOCATE(this%wavelengths(10),this%aods(2,10))
  END SUBROUTINE create_aod_obs

  SUBROUTINE delete_aod_obs(this)
    CLASS(obs_data_aod) :: this
    DEALLOCATE(this%wavelengths,this%aods)
  END SUBROUTINE delete_aod_obs

  SUBROUTINE read_aod_obs(this)
    CLASS(obs_data_aod) :: this
    this%wavelengths(:)=5.
    this%aods(:,:)=1.
  END SUBROUTINE read_aod_obs

  SUBROUTINE write_aod_obs(this)
    CLASS(obs_data_aod) :: this
    OPEN(10,file='aod_test.txt',form='formatted')
    WRITE(10,*)this%wavelengths
    WRITE(10,*)this%aods
    CLOSE(10)
  END SUBROUTINE write_aod_obs

  SUBROUTINE print_aod_obs(this)
    CLASS(obs_data_aod) :: this
    PRINT *,this%wavelengths
    PRINT *,this%aods
  END SUBROUTINE print_aod_obs

END MODULE module_aod
