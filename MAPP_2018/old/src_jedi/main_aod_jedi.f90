PROGRAM main_aod_jedi

  USE module_jedi_obs
  USE module_jedi_oper
  USE module_jedi_state
  USE module_aod

  IMPLICIT NONE

  TYPE(state_at_locations) :: mymodel
  TYPE(hofx_at_locations) :: myhofx
  TYPE(obs_data_aod) :: myobs
  TYPE(op_aod) :: myoper

  CALL myobs%create()
  CALL myobs%read()
  CALL myobs%write()
  CALL myobs%print()

  CALL myoper%create()
  CALL myoper%h_oper(mymodel,myhofx)

  PRINT *,'calculate innovations: myhofx vs. myobs%wavelengths, myobs%aods'

!  PRINT *,myobs%wavelengths
!  PRINT *,myobs%aods

  CALL myobs%delete()  
  CALL myoper%delete()

END PROGRAM main_aod_jedi

