PROGRAM testindexx

  USE module_interpolations

  IMPLICIT NONE

  INTEGER(i_kind), PARAMETER :: n=20,npoints=5
  INTEGER(i_kind) :: i

  REAL(r_single), DIMENSION(n) :: vector,values
  REAL(r_single), DIMENSION(n) :: newvector,newvalues
  REAL(r_single), DIMENSION(n) :: b,c,d
  INTEGER(i_kind),  DIMENSION(n) :: indx
  REAL(r_single), DIMENSION(npoints) :: points,pointvalues


  vector(1)=6.4414096
  vector(2)=8.5585627
  vector(3)=4.6561629
  vector(4)=5.5332872
  vector(5)=12.425428
  vector(6)=16.289967
  vector(7)=21.127173
  vector(8)=4.1089916
  vector(9)=4.4193828
  vector(10)=4.8727976
  vector(11)=5.2974236
  vector(12)=5.4648044
  vector(13)=6.6624365
  vector(14)=6.7679393
  vector(15)=7.4624404
  vector(16)=8.6646985
  vector(17)=9.0402034
  vector(18)=9.3584131
  vector(19)=9.3532930
  vector(20)=13.812895


  values(1)=2.544917818800444E-002
  values(2)=2.003942675340424E-002
  values(3)=3.157008833163841E-002
  values(4)=2.836979830120581E-002
  values(5)=1.381039017135239E-002
  values(6)=1.025644098122980E-002
  values(7)=7.289545255943560E-003
  values(8)=3.376097957957596E-002
  values(9)=3.250083714819482E-002
  values(10)=3.074790276857407E-002

  values(11)=2.920455052189350E-002
  values(12)=2.860883050418841E-002
  values(13)=2.480254308266245E-002
  values(14)=2.450003803742733E-002
  values(15)=2.261070212083675E-002
  values(16)=1.981481582883476E-002
  values(17)=1.904973429680824E-002
  values(18)=1.843498562415833E-002
  values(19)=1.844464785254685E-002
  values(20)=1.232593504231201E-002



  CALL indexx(n,vector,indx)

  OPEN(unit=11,file='splinetest_in.txt',form='formatted')

  DO i=1,n
     newvector(i)=vector(indx(i))
     newvalues(i)=values(indx(i))
     WRITE(11,*)newvector(i),newvalues(i)     
  ENDDO

  CLOSE(11)

  CALL spline(n,newvector,newvalues,b,c,d)

  points(1)=5.0
  points(2)=6.7
  points(3)=8.7
  points(4)=9.4
  points(5)=10.2
  
!will also extrapolate

  
  OPEN(unit=11,file='splinetest_out.txt',form='formatted')

  DO i=1,npoints
     pointvalues(i)=seval(n, points(i), newvector, newvalues, b, c, d)
     WRITE(11,*)points(i),pointvalues(i)
  ENDDO

  CLOSE(11)

  OPEN(unit=11,file='lineartest_out.txt',form='formatted')

  DO i=1,npoints
     pointvalues(i)=linear(newvector, newvalues, n,points(i))
     WRITE(11,*)points(i),pointvalues(i)
  ENDDO

  CLOSE(11)


END PROGRAM testindexx
