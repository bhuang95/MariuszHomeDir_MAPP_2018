PROGRAM dusk_mask_writeout

!read ds mask in binary and output line by line
!this code was changed to read r_kind and big endian


  USE kinds, ONLY: i_kind,r_kind

  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: dsmask,dsindex

  INTEGER(i_kind) :: nx,ny

  OPEN(unit=101,file='ds_mask_domain.bin',form='unformatted')
  READ(101)nx,ny
  ALLOCATE(dsmask(nx,ny),dsindex(nx,ny))
  READ(101)dsmask
  READ(101)dsindex
  CLOSE(101)

  OPEN(unit=102,file='ds_mask_domain.txt',form='formatted')
  WRITE(102,'(2i5)')nx,ny
  DO i=1,nx
     DO j=1,ny
        WRITE(102,'(2f15.7)')dsmask(i,j),dsindex(i,j)
     ENDDO
  ENDDO
  CLOSE(102)

  DEALLOCATE(dsmask)

END PROGRAM dusk_mask_writeout
