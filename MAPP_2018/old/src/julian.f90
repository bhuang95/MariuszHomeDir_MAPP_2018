FUNCTION jdate (year,month,day)
 
  !! Fliegel and van Flandern (1968) 
  !!---computes the julian date (jd) given a gregorian calendar
  !!   date (year,month,day).

  IMPLICIT NONE

  INTEGER, PARAMETER :: i8=SELECTED_INT_KIND(8)
  INTEGER(kind=i8) :: jdate
  INTEGER :: year,month,day,i,j,k

  i= year
  j= month
  k= day

  jdate= k-32075+1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12)&
  & /12-3*((i+4900+(j-14)/12)/100)/4
  
END FUNCTION jdate

SUBROUTINE gdate (jd, year,month,day)

  !!
  !!---computes the gregorian calendar date (year,month,day)
  !!   given the julian date (jd).
  !!

  IMPLICIT NONE

  INTEGER, PARAMETER :: i8=SELECTED_INT_KIND(8)
  INTEGER(kind=i8) :: jd,i,j,k,l,n

  INTEGER :: year,month,day

  l= jd+68569
  n= 4*l/146097
  l= l-(146097*n+3)/4
  i= 4000*(l+1)/1461001
  l= l-1461*i/4+31
  j= 80*l/2447
  k= l-2447*j/80
  l= j/11
  j= j+2-12*l
  i= 100*(n-49)+i+l

  year= i
  month= j
  day= k

END SUBROUTINE gdate


