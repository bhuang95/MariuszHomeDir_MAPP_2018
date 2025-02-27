    status = nf90_put_att(mcid,NF90_GLOBAL,"date_time",validtime)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    status = nf90_put_att(mcid,NF90_GLOBAL,"satellite",satellite_id)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    status = nf90_put_att(mcid,NF90_GLOBAL,"sensor",sensor_id)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    status = nf90_def_dim(mcid,'nlocs',NF90_UNLIMITED,nlocsid)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    status = nf90_def_dim(mcid,'nobs',nchannels_out*nobs,nobsid)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    status = nf90_def_dim(mcid,'nrecs',1,nrecsid)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF
    
    status = nf90_def_dim(mcid,'nvars',nchannels_out,nvarsid)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=0

    ivar=ivar+1
    status = nf90_def_var(mcid,'sensor_channel@VarMetaData',nf90_int,&
         &nvarsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'frequency@VarMetaData',nf90_real,&
         &nvarsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'wavenumber@VarMetaData',nf90_real,&
         &nvarsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'polarization@VarMetaData',nf90_int,&
         &nvarsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'latitude@MetaData',nf90_real,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'longitude@MetaData',nf90_real,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'sol_zenith_angle@MetaData',nf90_real,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'sol_azimuth_angle@MetaData',nf90_real,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'surface_type@MetaData',nf90_int,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_def_var(mcid,'time@MetaData',nf90_real,&
         &nlocsid,varids(ivar))
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF
    

    DO i=ichan_low,ichan_high
       DO j=1,nvaraod
          ivar=ivar+1
          WRITE(cj,'(i3)')channel_ids(i)
          varname=replace_text(varnames(j),'?',TRIM(ADJUSTL(cj)))
!          PRINT *,TRIM(varname),ivar
          status = nf90_def_var(mcid,TRIM(varname),nf90_real,&
               &nlocsid,varids(ivar))
          IF (status /= nf90_noerr) THEN
             iout=iout+1
             PRINT *,iout,mcid,TRIM(fnameout)
             CALL handle_err(status)
          ENDIF
       ENDDO
    ENDDO

    status = nf90_enddef(mcid)
    IF (status /= nf90_noerr) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF
    
    ivar=0

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),&
         &channel_ids(ichan_low:ichan_high))
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF


    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),&
         &frequencies(ichan_low:ichan_high))
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),&
         &wavenumbers(ichan_low:ichan_high))
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),polar(ichan_low:ichan_high))
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),aod_nnr_record(:)%lat)
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),aod_nnr_record(:)%lon)
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),sol_zenith_angle)
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),sol_azimuth_angle)
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF


    ivar=ivar+1
    status = nf90_put_var(mcid,varids(ivar),isfc)
    IF (status /= nf90_noerr ) THEN
       iout=iout+1
       PRINT *,iout,mcid,TRIM(fnameout)
       CALL handle_err(status)
    ENDIF

    ivar=ivar+1


       status = nf90_put_var(mcid,varids(ivar),values)
!            &aod_nnr_record(:)%values(i)) ! why crashes ?
       IF (status /= nf90_noerr) THEN
          iout=iout+1
          PRINT *,iout,mcid,TRIM(fnameout)
          CALL handle_err(status)
       ENDIF
       
!ObsError
       ivar=ivar+1
       status = nf90_put_var(mcid,varids(ivar),uncertainty(:,i))
       IF (status /= nf90_noerr) THEN
          iout=iout+1
          PRINT *,iout,mcid,TRIM(fnameout)
          CALL handle_err(status)
       ENDIF

!PreQc
       ivar=ivar+1
       status = nf90_put_var(mcid,varids(ivar),qc(:))
       IF (status /= nf90_noerr) THEN
          iout=iout+1
          PRINT *,iout,mcid,TRIM(fnameout)
          CALL handle_err(status)
       ENDIF
       
!KnownObsBias
       ivar=ivar+1
       status = nf90_put_var(mcid,varids(ivar),bias(:,i))
       IF (status /= nf90_noerr) THEN
          iout=iout+1
          PRINT *,iout,mcid,TRIM(fnameout)
          CALL handle_err(status)
       ENDIF
