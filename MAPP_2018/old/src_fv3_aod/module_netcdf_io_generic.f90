MODULE module_netcdf_io_generic

  USE netcdf
  USE module_kinds

  PRIVATE
  PUBLIC :: ndims_max
  PUBLIC :: netcdf_read,netcdf_write_fv3_aod

  INTEGER, PARAMETER :: ndims_max=5

CONTAINS
  
  SUBROUTINE netcdf_read(fnamein,varname,vardata,dim2reverse)

    IMPLICIT NONE
    
    CHARACTER(len=NF90_MAX_NAME), INTENT(in)    :: varname,fnamein
    REAL(SINGLE), ALLOCATABLE, INTENT(out)      :: vardata(:,:,:,:,:)
    INTEGER, INTENT(in), OPTIONAL               :: dim2reverse

    INTEGER                         :: ncid,varid,status,ndims
    INTEGER                         :: dimids(1:ndims_max),dims(1:ndims_max)
    REAL(SINGLE), ALLOCATABLE       :: tmp(:,:,:,:,:)
    
    INTEGER :: i,n

    PRINT *,'in netcdf_read'

    dims=1

    status = nf90_open(fnamein, nf90_nowrite, ncid)
    IF (status /= nf90_noerr) THEN
       PRINT *,'0 ',ncid,TRIM(fnamein)
       CALL handle_err(status)
    ENDIF

    status = nf90_inq_varid(ncid,TRIM(varname),varid)
    IF (status /= nf90_noerr ) THEN 
       PRINT *,'1 ',TRIM(varname)
       CALL handle_err(status)
    ENDIF

    status = nf90_inquire_variable(ncid, varid, ndims=ndims)
    status = nf90_inquire_variable(ncid, varid, dimids=dimids(:ndims))    
    IF (status /= nf90_noerr ) THEN
       PRINT *,'3 ',varid
       CALL handle_err(status)
    ENDIF

    DO i=1,ndims
       status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
       IF (status /= nf90_noerr ) THEN
          PRINT *,'4 ',i,dims(i)
          CALL handle_err(status)
       ENDIF
    ENDDO

!always convert all vars to real     

    IF (ALLOCATED(vardata)) DEALLOCATE(vardata)

    ALLOCATE(vardata(dims(1),dims(2),dims(3),dims(4),dims(5)),&
         &tmp(dims(1),dims(2),dims(3),dims(4),dims(5)))

    CALL handle_err(status)
    status = nf90_get_var(ncid,varid,tmp)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'5 ',varid
       CALL handle_err(status)
    ENDIF

    status = nf90_close(ncid)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'6 ',ncid,TRIM(fnamein)
       CALL handle_err(status)
    ENDIF

    IF (PRESENT(dim2reverse)) THEN
       IF (dim2reverse > ndims) THEN
          PRINT *,'Stopping: dimension to reverse larger than max dimension number'
          STOP
       ELSE
          DO i=1,ndims
             IF (i == dim2reverse) THEN 
                n=SIZE(vardata,i)
                SELECT CASE (i)
                CASE (1)
                   vardata=tmp(n:1:-1,:,:,:,:)
                CASE (2)
                   vardata=tmp(:,n:1:-1,:,:,:)
                CASE (3)
                   vardata=tmp(:,:,n:1:-1,:,:)
                CASE (4)
                   vardata=tmp(:,:,:,n:1:-1,:)
                CASE (5)
                   vardata=tmp(:,:,:,:,n:1:-1)
                END SELECT
             ENDIF
          ENDDO
       ENDIF
    ELSE
       vardata=tmp
    ENDIF

    DEALLOCATE(tmp)

  END SUBROUTINE netcdf_read

  SUBROUTINE netcdf_write_fv3_aod(fnameout,aodsfc,aodlayers,channels)

    IMPLICIT NONE
    
    CHARACTER(len=NF90_MAX_NAME), INTENT(in)    :: fnameout
    REAL(SINGLE), INTENT(in)      :: aodsfc(:,:,:,:),aodlayers(:,:,:,:,:),channels(:)

    INTEGER                         :: mcid,aodsfcid,aodlayersid,channelsid,status
    INTEGER                         :: dimids(1:ndims_max),dims(1:ndims_max)
    CHARACTER(len=NF90_MAX_NAME), PARAMETER    :: dimstring(1:ndims_max)=&
         &(/'xaxis_1','yaxis_1','zaxis_1','nchannels','Time'/)

    CHARACTER(len=NF90_MAX_NAME) :: attname,attvalue

    INTEGER :: i

    PRINT *,'in netcdf_write_fv3_aod'

    status = nf90_create(fnameout, nf90_write, mcid)
    IF (status /= nf90_noerr) THEN
       PRINT *,'0 ',mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    DO i=1,ndims_max-1
       status = nf90_def_dim(mcid,dimstring(i),SIZE(aodlayers,i),dimids(i))
       IF (status /= nf90_noerr) THEN
          PRINT *,'1 ',dimids(i),TRIM(dimstring(i))
          CALL handle_err(status)
       ENDIF
    ENDDO

    i=ndims_max
    status = nf90_def_dim(mcid,dimstring(i),NF90_UNLIMITED,dimids(i))
    IF (status /= nf90_noerr) THEN
       PRINT *,'1 ',dimids(i),TRIM(dimstring(i))
       CALL handle_err(status)
    ENDIF
    
    i=ndims_max-1

    status = nf90_def_var(mcid,'channels',NF90_REAL,dimids(i),channelsid)
    IF (status /= nf90_noerr) THEN
       PRINT *,'2 ',dimids(i),TRIM(dimstring(i))
       CALL handle_err(status)
    ENDIF
    
    attname="units"
    attvalue="nm"
    status = nf90_put_att(mcid, channelsid, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) THEN 
       PRINT *,'3 ',TRIM(attname),attvalue
       CALL handle_err(status)
    ENDIF

    status = nf90_def_var(mcid,'AOD',NF90_REAL,(/dimids(1:2),dimids(4:5)/),aodsfcid)
    IF (status /= nf90_noerr) THEN
       PRINT *,'4 ','AOD',aodsfcid
       CALL handle_err(status)
    ENDIF
    
    attname="units"
    attvalue=""
    status = nf90_put_att(mcid, aodsfcid, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) THEN
       PRINT *,'5 ',TRIM(attname),attvalue
       CALL handle_err(status)
    ENDIF

    status = nf90_def_var(mcid,'AOD_layers',NF90_REAL,dimids,aodlayersid)
    IF (status /= nf90_noerr) THEN
       PRINT *,'6 ','AOD_layers',aodlayersid
       CALL handle_err(status)
    ENDIF
    
    attname="units"
    attvalue=""
    status = nf90_put_att(mcid, aodlayersid, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) THEN
       PRINT *,'7 ',TRIM(attname),attvalue
       CALL handle_err(status)
    ENDIF

    status = nf90_enddef(mcid)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'8 ',mcid
       CALL handle_err(status)
    ENDIF

    status = nf90_put_var(mcid,channelsid,channels)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'9 ',channelsid,channels
       CALL handle_err(status)
    ENDIF

    status = nf90_put_var(mcid,aodsfcid,aodsfc)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'10 ',aodsfcid,MINVAL(aodsfc),MAXVAL(aodsfc)
       CALL handle_err(status)
    ENDIF

    status = nf90_put_var(mcid,aodlayersid,aodlayers)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'11 ',aodlayersid,MINVAL(aodlayers),MAXVAL(aodlayers)
       CALL handle_err(status)
    ENDIF

    status = nf90_close(mcid)
    IF (status /= nf90_noerr ) THEN
       PRINT *,'12 ',mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

  END SUBROUTINE    netcdf_write_fv3_aod

  SUBROUTINE handle_err(status)
    INTEGER status
    IF (status /= nf90_noerr) THEN
       WRITE(6,*) 'Error number ',status
       WRITE(6,*) 'Error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE handle_err
  
END MODULE module_netcdf_io_generic
