MODULE module_var

  USE kinds
  USE netcdf

  TYPE, PUBLIC :: var1d
     INTEGER :: rank
     CHARACTER(len=NF90_MAX_NAME) :: name
     INTEGER, DIMENSION(1) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:) :: values
  END TYPE var1d

  TYPE, PUBLIC :: var2d
     INTEGER :: rank
     CHARACTER(len=NF90_MAX_NAME) :: name
     INTEGER, DIMENSION(2) :: size     
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: values
  END TYPE var2d

  TYPE, PUBLIC :: var3d
     INTEGER :: rank
     CHARACTER(len=NF90_MAX_NAME) :: name
     INTEGER, DIMENSION(3) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: values
  END TYPE var3d

  TYPE, PUBLIC :: var4d
     INTEGER :: rank
     CHARACTER(len=NF90_MAX_NAME) :: name
     INTEGER, DIMENSION(4) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:,:) :: values
  END TYPE var4d

END MODULE module_var
