MODULE module_var

  USE kinds
  USE netcdf

  TYPE, PUBLIC :: var1d
     CHARACTER(len=NF90_MAX_NAME) :: name
     CHARACTER(len=NF90_MAX_NAME) :: usage
  END TYPE var1d

  TYPE, PUBLIC :: var2d
     CHARACTER(len=NF90_MAX_NAME) :: name
     CHARACTER(len=NF90_MAX_NAME) :: usage
  END TYPE var2d

  TYPE, PUBLIC :: var3d
     CHARACTER(len=NF90_MAX_NAME) :: name
     CHARACTER(len=NF90_MAX_NAME) :: usage
  END TYPE var3d

  TYPE, PUBLIC :: var4d
     CHARACTER(len=NF90_MAX_NAME) :: name
     CHARACTER(len=NF90_MAX_NAME) :: usage
  END TYPE var4d

END MODULE module_var
