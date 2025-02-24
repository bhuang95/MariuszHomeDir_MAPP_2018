PROGRAM add_aero_bckg

!this is for background using dust_1, seas_1 etc - but should work with
! dust1, seas1 as well except for file names

  USE kinds, ONLY : r_single, r_kind, i_kind 

  IMPLICIT NONE

  REAL(r_single), PARAMETER :: stdvar_min=1.e-16,&
       &hlenscale_min=10.e3,hlenscale_max=1000.e3,&
       &vlenscale_min=1.e-2,vlenscale_max=10.

  REAL(r_single), ALLOCATABLE,DIMENSION(:,:,:) :: agvin
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: corzin,vscalesin,hscalesin
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: corp,corq2,&
       wgvin,bvin,corsstin,hsstin

  REAL(r_single), ALLOCATABLE,DIMENSION(:,:,:) :: allbes,allbes_y
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: allbes_zonal,allbes_zonal_sn

  
  CHARACTER(len=5) :: varname
  INTEGER(i_kind) :: inerr,nlat,nlon,nsig,nsigstat,mlat,mlon,iret
  INTEGER(i_kind) :: outerr,iter,isig,ier,istat
  INTEGER(i_kind) :: i,j,k,ii,jj
  INTEGER(i_kind) :: cwoption
  
  CHARACTER(len=250) :: file_bckg_in,prefix_aero_bckg_in,file_bckg_out
  INTEGER(i_kind) :: nlon_aero


!! t878      ; t574        t382       t190quad   t254       t170       t126       t62
!! nlat=882  ; nlat=578  ; nlat=386 ; nlat=290 ; nlat=258 ; nlat=192 ; nlat=130 ; nlat=96 
!! nlon=1760 ; nlon=1152 ; nlon=768 ; nlon=576 ; nlon=512 ; nlon=384 ; nlon=256 ; nlon=192

!! t1148
!! nlat=1154
!! nlon=2304

  INTEGER, PARAMETER :: ntracers_gocart=15
  CHARACTER(len=12), DIMENSION(ntracers_gocart), PARAMETER :: tracer_names_gocart=(/&
!       &'dms','so2','msa',&
       &'sulf','bc1','bc2','oc1','oc2','p25',&
       &'dust_1','dust_2','dust_3','dust_4','dust_5',&
       &'seas_1','seas_2','seas_3','seas_4'&
       &/)

  CHARACTER(len=5), DIMENSION(ntracers_gocart) :: tracer_names_short_gocart

  DO i=1,ntracers_gocart
     tracer_names_short_gocart(i)=Replace_Text(tracer_names_gocart(i),'_','')
  ENDDO

  inerr=15
  outerr=17      
  cwoption=0

  IF (iargc() < 3) THEN
     PRINT *,'Requires name of original global bckg, prefix of aero files file'
     PRINT *,'and name for the output - Stopping'
     STOP
  ENDIF

  CALL getarg(1,file_bckg_in)
  CALL getarg(2,prefix_aero_bckg_in)
  CALL getarg(3,file_bckg_out)

! Open background error statistics file
  OPEN(inerr,file=TRIM(file_bckg_in),form='unformatted')
  OPEN(outerr,file=TRIM(file_bckg_out),form='unformatted')
  
! Read amplitudes
  READ(inerr) nsigstat,mlat,mlon
  WRITE(outerr)nsigstat,mlat,mlon

  WRITE(6,*) 'nsigstat,nlat = ',nsigstat,mlat
  nsig=nsigstat
  nlat=mlat
  
  IF (nlat==96) THEN
     nlon=192
  ELSE IF(nlat==130) THEN
     nlon=256
  ELSE IF(nlat==192) THEN
     nlon=384
  ELSE IF(nlat==258) THEN
     nlon=512
  ELSE IF(nlat==290) THEN
     nlon=576
  ELSE IF(nlat==386) THEN
     nlon=768
  ELSE IF(nlat==578) THEN
     nlon=1152
  ELSE IF(nlat==882) THEN
     nlon=1760
  ELSE IF(nlat==1154) THEN
     nlon=2304
  ELSE
     WRITE(6,*) nlat, 'number of latitudes not supported, CANNOT CONVERT'
     STOP
  END IF
  
  ALLOCATE( agvin(nlat,nsig,nsig) )
  ALLOCATE( wgvin(nlat,nsig),bvin(nlat,nsig) )
  ALLOCATE( corsstin(nlat,nlon),hsstin(nlat,nlon) )

  READ(inerr,iostat=ier) agvin,bvin,wgvin
  WRITE(outerr,iostat=ier) agvin,bvin,wgvin
  
  DEALLOCATE( agvin,wgvin,bvin )

  DO WHILE (.TRUE.) 
     
     READ(inerr,iostat=istat,END=110) varname, isig

     IF (istat/=0) THEN
        PRINT *,'varname, isig problem - Stopping'
        STOP
     ENDIF

     WRITE(outerr,iostat=istat) varname, isig

     ALLOCATE ( corzin(nlat,isig) )
     IF (varname=='q' .OR. varname=='cw') ALLOCATE ( corq2(nlat,isig) )
     ALLOCATE ( hscalesin(nlat,isig) )
     IF (isig>1) ALLOCATE (vscalesin(nlat,isig) )
     
     IF (varname/='sst') THEN
        IF (varname=='q' .OR. varname=='Q' .OR. (varname=='cw' .AND. cwoption==2)) THEN
           READ(inerr,iostat=ier) corzin,corq2
           IF(ier/=0) THEN
              PRINT *,'corzin,corq2 problem - Stopping'
              STOP
           ENDIF
           WRITE(outerr,iostat=ier) corzin,corq2
        ELSE
           READ(inerr,iostat=ier) corzin
           IF(ier/=0) THEN
              PRINT *,'corzin problem - Stopping'
              STOP
           ENDIF
           WRITE(outerr,iostat=ier) corzin
        END IF
        READ(inerr,iostat=ier) hscalesin
        IF(ier/=0) THEN
           PRINT *,'hscalesin problem - Stopping'
           STOP
        ENDIF
        WRITE(outerr,iostat=ier) hscalesin

        IF (isig>1) THEN
           READ(inerr,iostat=ier) vscalesin
           IF(ier/=0) THEN
              PRINT *,'vscalesin problem - Stopping'
              STOP
           ENDIF
           WRITE(outerr,iostat=ier) vscalesin
        ENDIF

        PRINT *,varname,isig,MINVAL(corzin),MAXVAL(corzin),MINVAL(hscalesin),MAXVAL(hscalesin)
        IF (isig>1) PRINT *,MINVAL(vscalesin),MAXVAL(vscalesin)


     ELSE
        READ(inerr,iostat=ier) corsstin
        IF(ier/=0) THEN
           PRINT *,'corsst problem - Stopping'
           STOP
        ENDIF
        WRITE(outerr,iostat=ier) corsstin

        READ(inerr,iostat=ier) hsstin
        IF(ier/=0) THEN
           PRINT *,'hsst problem - Stopping'
           STOP
        ENDIF
        WRITE(outerr,iostat=ier) hsstin
        PRINT *,varname,isig,MINVAL(corsstin),MAXVAL(corsstin),MINVAL(hsstin),MAXVAL(hsstin)
        DEALLOCATE( corsstin,hsstin)
     END IF

    
     DEALLOCATE(corzin,hscalesin)
     IF (isig>1) DEALLOCATE(vscalesin)
     IF (varname=='q' .OR. varname=='cw') DEALLOCATE(corq2)
     
  ENDDO
  
110 CONTINUE

  CLOSE(inerr)
  
  ALLOCATE(allbes(nlon,nlat-2,nsig),allbes_y(nlon,nlat-2,nsig),&
       &allbes_zonal(nlat,nsig),allbes_zonal_sn(nlat,nsig))

  DO i=1,ntracers_gocart

     PRINT *,'Writing bes for ',tracer_names_gocart(i)

     file_bckg_in=TRIM(prefix_aero_bckg_in)//'_'//TRIM(tracer_names_gocart(i))//'.bin'
     OPEN(unit=inerr,file=file_bckg_in,form='unformatted')
     READ(inerr)nlon_aero,mlat,nsigstat
     IF (nlon_aero /= nlon .OR. mlat+2 /= nlat .OR. nsigstat /= nsig) THEN
        PRINT *,'BE array sizes not matching nlon,nlat,nsig ',nlon_aero,nlon,&
             &mlat,nlat,nsigstat,nsig
        STOP
     ENDIF

     varname=tracer_names_short_gocart(i)
     WRITE(outerr)varname,nsig

     READ(inerr)allbes

     allbes_zonal=0.
     DO k=1,nsig
        DO j=2,nlat-1
           jj=0
           DO ii=1,nlon
              IF (allbes(ii,j-1,k) > stdvar_min) THEN
                 allbes_zonal(j,k)=allbes_zonal(j,k)+allbes(ii,j-1,k)
                 jj=jj+1
              ENDIF
           ENDDO
           
           IF (jj==0) THEN
              PRINT *,varname
              PRINT *,'All stdev outside limits - assigning default at level ',k
              allbes_zonal(j,k)=stdvar_min
           ELSE
              allbes_zonal(j,k)=allbes_zonal(j,k)/REAL(jj)
           ENDIF

        ENDDO
        allbes_zonal(1,k)=allbes_zonal(2,k)
        allbes_zonal(nlat,k)=allbes_zonal(nlat-1,k)

     ENDDO

     DO j=1,nlat
        allbes_zonal_sn(j,:)=allbes_zonal(nlat-j+1,:)
     ENDDO

     WRITE(outerr)allbes_zonal_sn
     
     READ(inerr)allbes
     READ(inerr)allbes_y

     allbes_zonal=0.
     DO k=1,nsig
        DO j=2,nlat-1

           jj=0
           DO ii=1,nlon
              IF (allbes(ii,j-1,k) > hlenscale_min .AND. &
                   &allbes(ii,j-1,k) < hlenscale_max .AND.&
                   &allbes_y(ii,j-1,k) > hlenscale_min .AND. &
                   &allbes_y(ii,j-1,k) < hlenscale_max) &
                   &THEN
                 allbes_zonal(j,k)=allbes_zonal(j,k)+&
                      &SQRT(allbes(ii,j-1,k)**2 + allbes_y(ii,j-1,k)**2)
                 jj=jj+1
              ENDIF
           ENDDO
           
           IF (jj==0) THEN
              PRINT *,varname
              PRINT *,'All hscales outside limits - assigning default at level ',k
              allbes_zonal(j,k)=hlenscale_max
           ELSE
              allbes_zonal(j,k)=allbes_zonal(j,k)/REAL(jj)
           ENDIF

        ENDDO

        allbes_zonal(1,k)=allbes_zonal(2,k)
        allbes_zonal(nlat,k)=allbes_zonal(nlat-1,k)

     ENDDO

     DO j=1,nlat
        allbes_zonal_sn(j,:)=allbes_zonal(nlat-j+1,:)
     ENDDO

     WRITE(outerr)allbes_zonal_sn

     READ(inerr)allbes

     allbes_zonal=0.
     DO k=1,nsig
        DO j=2,nlat-1
           jj=0
           DO ii=1,nlon
              IF (allbes(ii,j-1,k) > vlenscale_min .AND. &
                   &allbes(ii,j-1,k) < vlenscale_max) THEN
                 allbes_zonal(j,k)=allbes_zonal(j,k)+allbes(ii,j-1,k)
                 jj=jj+1
              ENDIF
           ENDDO
           
           IF (jj==0) THEN
              PRINT *,varname
              PRINT *,'All vlenscales outside limits - assigning default at level ',k
              allbes_zonal(j,k)=vlenscale_max
           ELSE
              allbes_zonal(j,k)=allbes_zonal(j,k)/REAL(jj)
           ENDIF
           
        ENDDO

        allbes_zonal(1,k)=allbes_zonal(2,k)
        allbes_zonal(nlat,k)=allbes_zonal(nlat-1,k)
        
     ENDDO
     
     DO j=1,nlat
        allbes_zonal_sn(j,:)=allbes_zonal(nlat-j+1,:)
     ENDDO

     WRITE(outerr)1./allbes_zonal_sn
     
  ENDDO

  CLOSE(outerr)

  STOP

CONTAINS
  
  FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
    CHARACTER(*)        :: s,text,rep
    CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
    INTEGER             :: i, nt, nr
    
    outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
    DO
       i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
       outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
    END DO
  END FUNCTION Replace_Text
  
END PROGRAM add_aero_bckg
