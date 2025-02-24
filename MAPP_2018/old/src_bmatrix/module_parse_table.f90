MODULE module_parse_table

  USE netcdf
  USE module_var

  IMPLICIT NONE

  INTEGER :: nvars1d,nvars2d,nvars3d,nvars4d,nvars
  INTEGER, ALLOCATABLE,DIMENSION(:) :: dims

  TYPE(var1d), ALLOCATABLE, DIMENSION(:) :: myvars1d
  TYPE(var2d), ALLOCATABLE, DIMENSION(:) :: myvars2d
  TYPE(var3d), ALLOCATABLE, DIMENSION(:) :: myvars3d
  TYPE(var4d), ALLOCATABLE, DIMENSION(:) :: myvars4d

CONTAINS

  SUBROUTINE parse_table_sub

    INTEGER :: tabunit=101
    CHARACTER(len=NF90_MAX_NAME),DIMENSION(NF90_MAX_VARS) :: names,usages
    INTEGER, DIMENSION(NF90_MAX_VARS) :: dims

    INTEGER :: i,i1,i2,i3,i4

    nvars1d=0
    nvars2d=0
    nvars3d=0
    nvars4d=0

    OPEN(unit=tabunit,form='formatted',file='input_split.table')
    READ(tabunit,*)
    i=0
    DO WHILE (.TRUE.)
       i=i+1
       READ(tabunit,*,END=100)names(i),usages(i),dims(i)
       IF (dims(i)==1) nvars1d=nvars1d+1
       IF (dims(i)==2) nvars2d=nvars2d+1
       IF (dims(i)==3) nvars3d=nvars3d+1
       IF (dims(i)==4) nvars4d=nvars4d+1
    ENDDO

100 CONTINUE

    ALLOCATE(myvars1d(nvars1d),myvars2d(nvars2d),&
         &myvars3d(nvars3d),myvars4d(nvars4d))

    nvars=i-1

    i1=0
    i2=0
    i3=0
    i4=0
    
    DO i=1,nvars
       IF (dims(i)==1) THEN
          i1=i1+1
          myvars1d(i1)%name=names(i)
          myvars1d(i1)%usage=usages(i)
       ELSEIF (dims(i)==2) THEN
          i2=i2+1
          myvars2d(i2)%name=names(i)
          myvars2d(i2)%usage=usages(i)
       ELSEIF (dims(i)==3) THEN
          i3=i3+1
          myvars3d(i3)%name=names(i)
          myvars3d(i3)%usage=usages(i)
       ELSEIF (dims(i)==4) THEN
          i4=i4+1
          myvars4d(i4)%name=names(i)
          myvars4d(i4)%usage=usages(i)
       ENDIF

    ENDDO
    
  END SUBROUTINE parse_table_sub

END MODULE module_parse_table
