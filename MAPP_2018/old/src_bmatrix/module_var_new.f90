MODULE module_var

  USE kinds

  TYPE, PUBLIC :: var1d
     INTEGER :: rank
     INTEGER :: name
     INTEGER, DIMENSION(1) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:) :: values
  END TYPE var1d

  TYPE, PUBLIC :: var2d
     INTEGER :: rank
     INTEGER :: name
     INTEGER, DIMENSION(2) :: size     
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: values
  END TYPE var2d

  TYPE, PUBLIC :: var3d
     INTEGER :: rank
     INTEGER :: name
     INTEGER, DIMENSION(3) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: values
  END TYPE var3d

  TYPE, PUBLIC :: var4d
     INTEGER :: rank
     INTEGER :: name
     INTEGER, DIMENSION(4) :: size
     REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:,:) :: values
  END TYPE var4d

CONTAINS

  SUBROUTINE sub_allocate1d
    ALLOCATE(var1d%values(var1d%size(1)))
  END SUBROUTINE sub_allocate1d
    
END MODULE module_var
