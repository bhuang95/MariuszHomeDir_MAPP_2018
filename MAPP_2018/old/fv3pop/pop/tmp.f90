SUBROUTINE read_aod(ncids, nt, var_name, vardata, nz, vlevels)
      IMPLICIT NONE
      
      INTEGER ncids(:), nt, nz
      CHARACTER (len=*) :: var_name
      REAL vardata(:), vlevels(nz)

      INTEGER varid, varid_vl, i, numTimes, grid_xt, grid_yt, nvlevs
      INTEGER, DIMENSION(nf90_max_var_dims) :: dimIDs
      REAL datatmp(cres*cres*nz)

      stat = nf90_inq_varid(ncids(1),trim(var_name),varid)
      CALL nc_opchk(stat,'nf90_inq_varid, in read_1var')

      IF (nz == 1) THEN
        nvlevs = 1
        stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
        CALL nc_opchk(stat,'nf90_inquire_variable, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(3), len = numTimes)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(2), len = grid_yt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(1), len = grid_xt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
      ELSE 
        stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
        CALL nc_opchk(stat,'nf90_inquire_variable, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(4), len = numTimes)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(3), len = nvlevs)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(2), len = grid_yt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(1), len = grid_xt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        IF (nvlevs == 63) THEN
          stat = nf90_inq_varid(ncids(1),'pfull',varid_vl)
          CALL nc_opchk(stat,'nf90_inq_varid - pfull, in read_1var')
          stat = nf90_get_var(ncids(1),varid_vl,vlevels, &
                  start = (/ 1 /) , count = (/ nvlevs /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var - pfull, nz /= 1')
        ELSE IF (nvlevs == 64) THEN 
          stat = nf90_inq_varid(ncids(1),'phalf',varid_vl)
          CALL nc_opchk(stat,'nf90_inq_varid - phalf, in read_1var')
          stat = nf90_get_var(ncids(1),varid_vl,vlevels, &
                 start = (/ 1 /) , count = (/ nvlevs /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var - phalf, nz /= 1')
        ENDIF
      ENDIF

      
      DO i = 1, 6 

        IF (nvlevs == 1) THEN
          stat = nf90_get_var(ncids(i),varid,datatmp, &
                 start = (/ 1, 1, nt /), &
                 count = (/ cres, cres, 1 /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var')
          vardata((i-1)*cres*cres*nz+1:i*cres*cres*nz) =  &
            datatmp(1:cres*cres*nz)
         ELSE
          stat = nf90_get_var(ncids(i),varid,datatmp, &
                 start = (/ 1, 1, 1, nt /), &
                 count = (/ cres, cres, nvlevs, 1 /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var, nz /= 1')
          vardata((i-1)*cres*cres*nz+1:i*cres*cres*nz) =  &
            datatmp(1:cres*cres*nz)
         END IF
 
      END DO
      
END SUBROUTINE read_aod
