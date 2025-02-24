MODULE  module_jedi_oper
!obs operator

  USE module_jedi_state, ONLY : state_at_locations,hofx_at_locations
  PRIVATE
  PUBLIC :: obs_operator
  
  TYPE, ABSTRACT :: obs_operator
   CONTAINS
     PROCEDURE(op_create), DEFERRED :: create           ! Constructor
     PROCEDURE(op_delete), DEFERRED :: delete           ! Destructor
     PROCEDURE(op_h_oper), DEFERRED :: h_oper           ! Observation
! operator
     PROCEDURE(op_inputs), DEFERRED :: input_variables  ! input
! variables required by observation operator
     PROCEDURE(op_print),  DEFERRED :: print            ! Prints
! human readable info
  END TYPE obs_operator
  
  ABSTRACT INTERFACE
     SUBROUTINE op_create(this)!, config)
       IMPORT obs_operator
       CLASS(obs_operator), INTENT(inout) :: this
!       TYPE(config), INTENT(in) :: config
     END SUBROUTINE op_create
  END INTERFACE
  
  ABSTRACT INTERFACE
     SUBROUTINE op_delete(this)
       IMPORT obs_operator
       CLASS(obs_operator), INTENT(inout) :: this
     END SUBROUTINE op_delete
  END INTERFACE

  ABSTRACT INTERFACE
     SUBROUTINE op_h_oper(this, gom, hofx)!, bias)
       IMPORT obs_operator
       IMPORT state_at_locations
       IMPORT hofx_at_locations
       CLASS(obs_operator), INTENT(in) :: this
       TYPE(state_at_locations), INTENT(in)         :: gom    ! Model
! values after interpolations
!       TYPE(obs_data), INTENT(inout)                :: hofx   ! H(x) !cannot be abstract type
       TYPE(hofx_at_locations), INTENT(inout)  :: hofx   ! H(x)
! output values
!       TYPE(obs_aux_variable), OPTIONAL, INTENT(in) :: bias   ! Bias
! correction predictors
     END SUBROUTINE op_h_oper
  END INTERFACE

  ABSTRACT INTERFACE
     FUNCTION op_inputs(this)
       IMPORT obs_operator
       IMPORT state_at_locations
       CLASS(state_at_locations), POINTER    :: op_inputs
       CLASS(obs_operator), INTENT(in) :: this
!       CLASS(variables), POINTER    :: op_inputs
!       CLASS(obs_error), INTENT(in) :: this
     END FUNCTION op_inputs
  END INTERFACE
  
  ABSTRACT INTERFACE
     SUBROUTINE op_print(this)
       IMPORT obs_operator
       CLASS(obs_operator), INTENT(in) :: this
     END SUBROUTINE op_print
  END INTERFACE
  
END MODULE  module_jedi_oper
