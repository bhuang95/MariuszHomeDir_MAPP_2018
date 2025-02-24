      SUBROUTINE write_bin(var_name,yyyymmddhhmm,mx,my,nlevels,data_xyz,time)
        IMPLICIT NONE
        
        CHARACTER*(*),INTENT(IN)  :: var_name
        CHARACTER(12),INTENT(IN)  :: yyyymmddhhmm
        INTEGER :: mx, my, nlevels, time
        REAL :: data_xyz(mx,my,nlevels)

        CHARACTER(80) :: header(10)
    
        CALL WriteHeaderPost(var_name,yyyymmddhhmm,mx,my,nlevels,time,header)
        WRITE(10) header
        WRITE(10) data_xyz
      ENDSUBROUTINE

      subroutine WriteHeaderPost(VarName,yyyymmddhhmm,mx,my,nz,time,header)
        implicit none

        character*(*),intent(IN)  :: VarName
        character(12),intent(IN)  :: yyyymmddhhmm
        integer      ,intent(IN)  :: mx,my,nz,time
        character(80),intent(OUT) :: header(10)

!        PRINT*, TRIM(VarName)," ",yyyymmddhhmm,nz,mx,my,nz,time
        header(:)(1:80) =' '
        write(header,10) TRIM(VarName),yyyymmddhhmm,mx,my,nz,time
10      format('NIM ',A,' Forecast initial time YYYYMMDDHHMM: ',A12,/,&
          'Nx=', I0, 'Ny=', I0, ' Level ',I0, / &
          I0,' hours',/,&
          '4',/,&
          '5',/,&
          '6',/,&
          '7',/,&
          '8',/,&
          '9',/,&
          '10')
         return
      end subroutine WriteHeaderPOST
