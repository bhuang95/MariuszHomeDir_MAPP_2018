MODULE module_jedi_state

  USE kinds, ONLY : r_kind, i_kind
  USE module_base, ONLY: max_varname_length
  USE module_jedi_obs

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: state_at_locations,fill_model_vars,hofx_at_locations

  TYPE state_at_locations

     CHARACTER(len=max_varname_length), ALLOCATABLE, DIMENSION(:) :: varnames
     REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: model_vars

   CONTAINS

     PROCEDURE :: fill_model_vars

  END TYPE state_at_locations

  TYPE ::  hofx_at_locations
     REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: hofx
  END TYPE hofx_at_locations

CONTAINS

  SUBROUTINE fill_model_vars(this)
    
    CLASS(state_at_locations) :: this

    INTEGER(i_kind) :: nlocs,nvars,nl,nt

    OPEN(unit=10,file='bckg.bin',form='unformatted')
    READ(10)nlocs,nvars,nl,nt
    ALLOCATE(this%varnames(nvars),this%model_vars(nlocs,nvars,nl,nt))
    READ(10)this%varnames
    READ(10)this%model_vars
    CLOSE(10)
    
  END SUBROUTINE fill_model_vars

END MODULE module_jedi_state
