PROGRAM test
  
  IMPLICIT NONE

  REAL :: x,y
  INTEGER :: i

  OPEN(unit=101,file='/scratch3/BMC/chem-var/pagowski/enkf_runs/trunk_r79009/run/inoutdata/aod_scatter.bin',&
       &convert='big_endian',form='unformatted')
  OPEN(103,file='scatter.txt',form='formatted')

  i=0
  DO WHILE (.TRUE.)
     READ(101,END=102)x,y
     WRITE(103,'(2f15.7)')x,y
     i=i+1
  END DO
  
102 CONTINUE

  PRINT *,i

  CLOSE(101)
  CLOSE(103)

END PROGRAM test


  
