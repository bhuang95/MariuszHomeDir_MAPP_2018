MODULE module_jedi_state

  USE kinds, ONLY : r_kind, i_kind
  USE module_base, ONLY: max_varname_length
  USE module_jedi_obs

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: state_at_locations,fill_model_vars,hofx_at_locations

!can be made abstract and extended in aod module
  TYPE state_at_locations
     
     CHARACTER(len=max_varname_length), ALLOCATABLE, DIMENSION(:) :: varnames
!dimension for nlocs,nvars,nz,nt      
     REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: model_vars
     
   CONTAINS
     
     PROCEDURE :: fill_model_vars
     
  END TYPE state_at_locations

  TYPE ::  hofx_at_locations
!dimension for nlocs,ntypes (e.g. wavelengths for AOD),nz,nt     
     REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: hofx
  END TYPE hofx_at_locations

CONTAINS

  SUBROUTINE fill_model_vars(this)
    
    CLASS(state_at_locations) :: this

    INTEGER(i_kind) :: nlocs,nvars,nz,nt

    INTEGER(i_kind) :: i

    nlocs=1
    nvars=10
    nz=50
    nt=1
    PRINT *,'fill_model_vars: allocate and fill model(gom)'
    ALLOCATE(this%varnames(nvars),this%model_vars(nlocs,nvars,nz,nt))
    DO i=1,nvars
       WRITE(this%varnames(i),'(i0.3)')i
       this%model_vars(:,i,:,:)=i*1.e-7
    ENDDO

    RETURN

!read a real profile
    OPEN(unit=10,file='bckg.bin',form='unformatted')
    READ(10)nlocs,nvars,nz,nt
    ALLOCATE(this%varnames(nvars),this%model_vars(nlocs,nvars,nz,nt))
    READ(10)this%varnames
    READ(10)this%model_vars
    CLOSE(10)
    
  END SUBROUTINE fill_model_vars

END MODULE module_jedi_state
