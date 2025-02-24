PROGRAM read_bckg
!to read global bckg file

  USE kinds, ONLY : r_single, r_kind, i_kind 

  IMPLICIT NONE

  REAL(r_single), PARAMETER :: stdvar_min=1.e-16,&
       &hlenscale_min=10.e3,hlenscale_max=1000.e3,&
       &vlenscale_min=1.e-5,vlenscale_max=10.

  REAL(r_single), ALLOCATABLE,DIMENSION(:,:,:) :: agvin
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: corzin,vscalesin,hscalesin
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: corp,corq2,&
       wgvin,bvin,corsstin,hsstin

  REAL(r_single), ALLOCATABLE,DIMENSION(:,:,:) :: allbes,allbes_y
  REAL(r_single), ALLOCATABLE,DIMENSION(:,:) :: allbes_zonal

  
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

  inerr=15
  outerr=17      
  cwoption=0

  IF (iargc() < 1) THEN
     PRINT *,'Requires name of original global bckg - Stopping'
     STOP
  ENDIF

  CALL getarg(1,file_bckg_in)

! Open background error statistics file
  OPEN(inerr,file=TRIM(file_bckg_in),form='unformatted')
  
! Read amplitudes
  READ(inerr) nsigstat,mlat,mlon

  WRITE(6,*) 'nsigstat,nlat,mlon = ',nsigstat,mlat,mlon
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
  
  DEALLOCATE( agvin,wgvin,bvin )

  DO WHILE (.TRUE.) 
     
     READ(inerr,iostat=istat,END=110) varname, isig

     IF (istat/=0) THEN
        PRINT *,'varname, isig problem - Stopping'
        STOP
     ENDIF

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
        ELSE
           READ(inerr,iostat=ier) corzin
           IF(ier/=0) THEN
              PRINT *,'corzin problem - Stopping'
              STOP
           ENDIF
        END IF
        READ(inerr,iostat=ier) hscalesin
        IF(ier/=0) THEN
           PRINT *,'hscalesin problem - Stopping'
           STOP
        ENDIF

        IF (isig>1) THEN
           READ(inerr,iostat=ier) vscalesin
           IF(ier/=0) THEN
              PRINT *,'vscalesin problem - Stopping'
              STOP
           ENDIF
        ENDIF
        PRINT *,varname,isig,MINVAL(corzin),MAXVAL(corzin),MINVAL(hscalesin),MAXVAL(hscalesin)
        IF (isig>1) PRINT *,MINVAL(vscalesin),MAXVAL(vscalesin)

     ELSE
        READ(inerr,iostat=ier) corsstin
        IF(ier/=0) THEN
           PRINT *,'corsst problem - Stopping'
           STOP
        ENDIF

        READ(inerr,iostat=ier) hsstin
        IF(ier/=0) THEN
           PRINT *,'hsst problem - Stopping'
           STOP
        ENDIF
        PRINT *,varname,isig,MINVAL(corsstin),MAXVAL(corsstin),MINVAL(hsstin),MAXVAL(hsstin)
        DEALLOCATE( corsstin,hsstin)
     END IF

    
     DEALLOCATE(corzin,hscalesin)
     IF (isig>1) DEALLOCATE(vscalesin)
     IF (varname=='q' .OR. varname=='cw') DEALLOCATE(corq2)
     
  ENDDO
  
110 CONTINUE

  CLOSE(inerr)
  
END PROGRAM read_bckg
