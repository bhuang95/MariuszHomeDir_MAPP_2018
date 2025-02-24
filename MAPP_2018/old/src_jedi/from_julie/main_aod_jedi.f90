PROGRAM main_aod_jedi

  USE module_jedi_obs
  USE module_jedi_opers
!  USE module_jedi_state
  USE module_aod

  IMPLICIT NONE

  TYPE(obs_data_aod) :: myobs

  CALL myobs%create()
  CALL myobs%read()
  CALL myobs%write()
  CALL myobs%print()
  CALL myobs%delete()  

END PROGRAM main_aod_jedi

