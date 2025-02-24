PROGRAM extract_names

  USE ufo_misc

  CHARACTER(len=max_name_length) :: obs,model

  INTEGER :: nvars
  CHARACTER(len=max_name_length),DIMENSION(max_vars) :: names

  obs='Aod'
  model='fv3'

  CALL get_names(obs,model,nvars,names)

END PROGRAM extract_names
