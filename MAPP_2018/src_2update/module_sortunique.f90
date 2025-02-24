MODULE module_SortUnique

!from https://stackoverflow.com/questions/44198212/a-fortran-equivalent-to-unique

CONTAINS

  RECURSIVE SUBROUTINE MergeSort(temp, Begin, Finish, list)
! 1st 3 arguments are input, 4th is output sorted list
    IMPLICIT NONE
    INTEGER(kind=4),INTENT(inout) :: Begin,list(:),temp(:)
    INTEGER(kind=4),INTENT(in) :: Finish
    INTEGER(kind=4) :: Middle
    IF (Finish-Begin<2) THEN    !if run size =1
       RETURN                   !it is sorted
    ELSE
! split longer runs into halves
       Middle = (Finish+Begin)/2
! recursively sort both halves from list into temp
       CALL MergeSort(list, Begin, Middle, temp)
       CALL MergeSort(list, Middle, Finish, temp)
! merge sorted runs from temp into list
       CALL MERGE(temp, Begin, Middle, Finish, list)
    ENDIF
  END SUBROUTINE MergeSort

  SUBROUTINE MERGE(list, Begin, Middle, Finish, temp)
    IMPLICIT NONE
    INTEGER(kind=4),INTENT(inout) :: list(:),temp(:)
    INTEGER(kind=4),INTENT(in) ::Begin,Middle,Finish
    INTEGER(kind=4)    :: kx,ky,kz
    ky=Begin
    kz=Middle
!! While there are elements in the left or right runs...
    DO kx=Begin,Finish-1

!! If left run head exists and is <= existing right run head.
       IF (ky.LT.Middle.AND.(kz.GE.Finish.OR.list(ky).LE.list(kz))) THEN
          temp(kx)=list(ky)
          ky=ky+1
       ELSE
          temp(kx)=list(kz)
          kz = kz + 1
       END IF
    END DO

  END SUBROUTINE Merge

  FUNCTION Unique(list)
!! usage sortedlist=Unique(list)
    IMPLICIT NONE
    INTEGER(kind=4) :: strt,fin,N
    INTEGER(kind=4), INTENT(inout) :: list(:)
    INTEGER(kind=4), ALLOCATABLE  :: unique(:),work(:)
    LOGICAL,ALLOCATABLE :: mask(:)
! sort
    work=list;strt=1;N=SIZE(list);fin=N+1
    CALL MergeSort(work,strt,fin,list)
! cull duplicate indices
    ALLOCATE(mask(N));
    mask=.FALSE.
    mask(1:N-1)=list(1:N-1)==list(2:N)
    unique=PACK(list,.NOT.mask)

  END FUNCTION Unique

END MODULE Module_SortUnique
