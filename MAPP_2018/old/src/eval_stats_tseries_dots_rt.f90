PROGRAM eval_stats_tseries_dots_rt

!program to calculate bias, pattern rsme and correlation
!for dot plots
!Mariusz Pagowski, Aug 2012

  IMPLICIT NONE

  INTEGER, PARAMETER :: idp=KIND(1.d0)
  INTEGER, PARAMETER :: i8=SELECTED_INT_KIND(8)
  INTEGER, PARAMETER :: istatsmin=5,nstats=9
  INTEGER, PARAMETER :: nmaxstations=5000
  REAL, PARAMETER :: hdiff_max=100.,height_min=-500.,&
       &model_fire_threshold=100.,unknown=-9999.

  REAL(kind=idp) :: bias_s,patrmse_s,stdevobs_s,stdevfcst_s,corr_s,rmse_s
  REAL, DIMENSION(nmaxstations) :: obs_s,fcst_s,&
       &height_s,geopt_s,lat_s,lon_s,&
       &gridiobs_s,gridjobs_s
  
  INTEGER, DIMENSION(nmaxstations) :: site_s,code_s
  REAL :: date=-1.,fcstave_s,obsave_s,newdate,deltat

!max 100m height difference allowed
!min depression -500.
!will evaluate stations where height is unknown=-9999.
  LOGICAL :: loghdiff,logheight
  INTEGER :: nf,nst,nstin,nfcst,nfcst1,&
       &i,ii,ihour,imin,j,k,kk,l,ll,iargc,nobsvalid_s,code_new,code_old

  INTEGER :: inunit=55, outunit=56,utcmin,utcmax
  CHARACTER(len=1) :: cutc,delim=','
  INTEGER, ALLOCATABLE, DIMENSION(:) :: iyearinit,imonthinit,&
       &idayinit,ihourinit
  INTEGER, ALLOCATABLE, DIMENSION(:) :: nobsvalid
  INTEGER, ALLOCATABLE, DIMENSION(:) :: site,utc

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: obs,fcst
  REAL, ALLOCATABLE, DIMENSION(:) :: height,geopt,lat,lon,&
       &gridiobs,gridjobs
  
  REAL(kind=idp), ALLOCATABLE, DIMENSION(:) :: fcstave,obsave,&
       &bias,patrmse,stdevobs,stdevfcst,corr,rmse
  REAL(kind=idp) :: obsdiff,fcstdiff

  CHARACTER(len=250) :: flatlons,fconc,infile,clogheight,outfile

  IF (iargc() < 3) THEN
     PRINT *,'Requires 3 input parameters: see script'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,clogheight)
  CALL getarg(3,outfile)

  IF (clogheight(1:1)=='T' .OR. clogheight(1:1)=='t') THEN
     logheight=.TRUE.
     loghdiff=.FALSE.
  ELSE
     logheight=.FALSE.
     loghdiff=.TRUE.
  ENDIF

  OPEN(inunit,file=infile,form='formatted')
  OPEN(outunit,file=outfile,form='formatted')

  WRITE(outunit,'(a)')'# site_code,  lat,  lon, site_char, bias, prmse, corr, rmse  '

  i=1

  READ(inunit,FMT=101,END=100)l,&
       &obs_s(i),height_s(i),&
       &fcst_s(i),geopt_s(i),site_s(i),lat_s(i),lon_s(i),&
       &gridiobs_s(i),gridjobs_s(i),code_old
  
  code_new=code_old

  REWIND(inunit)

  j=0

  DO WHILE (.TRUE.)

     i=0

     DO WHILE (code_new==code_old) 

        i=i+1

        READ(inunit,FMT=101,END=100)l,&
             &obs_s(i),height_s(i),&
             &fcst_s(i),geopt_s(i),site_s(i),lat_s(i),lon_s(i),&
             &gridiobs_s(i),gridjobs_s(i),code_new
        
        IF (fcst_s(i) >= model_fire_threshold) i=i-1

!        PRINT *,l,obs_s(i),height_s(i),&
!             &fcst_s(i),geopt_s(i),site_s(i),lat_s(i),lon_s(i),&
!             &gridiobs_s(i),gridjobs_s(i),code_new

     ENDDO

     code_s(1)=code_old

     code_old=code_new

     nobsvalid_s=i-1

     fcstave_s=0.
     obsave_s=0.
     patrmse_s=0.
     stdevobs_s=0.
     stdevfcst_s=0.
     corr_s=0.
     bias_s=0.
     rmse_s=0.

     IF (logheight) THEN
        loghdiff=(height_s(1) < height_min) .OR. &
             &(ABS(height_s(1)-geopt_s(1)) < hdiff_max)
     ENDIF

     IF (nobsvalid_s > istatsmin .AND. loghdiff) THEN
        fcstave_s=SUM(fcst_s(1:nobsvalid_s))/REAL(nobsvalid_s)
        obsave_s=SUM(obs_s(1:nobsvalid_s))/REAL(nobsvalid_s)

        bias_s=fcstave_s-obsave_s

        DO i=1,nobsvalid_s
           
           obsdiff=obs_s(i)-obsave_s
           fcstdiff=fcst_s(i)-fcstave_s
           stdevobs_s=stdevobs_s+obsdiff**2
           stdevfcst_s=stdevfcst_s+fcstdiff**2
           patrmse_s=patrmse_s+(fcstdiff-obsdiff)**2
           corr_s=corr_s+obsdiff*fcstdiff

        ENDDO
           
        stdevobs_s=SQRT(1./REAL(nobsvalid_s-1)*stdevobs_s)
        stdevfcst_s=SQRT(1./REAL(nobsvalid_s-1)*stdevfcst_s)
        patrmse_s=SQRT(1./REAL(nobsvalid_s)*patrmse_s)
        rmse_s=SQRT(bias_s**2+patrmse_s**2)
        corr_s=1./REAL(nobsvalid_s)*corr_s/&
             &(stdevobs_s*stdevfcst_s)

!        PRINT *,code_s(1),delim,lat_s(1),delim,lon_s(1),&
!             &delim,site_s(1),delim,&
!             &bias_s,delim,patrmse_s,delim,corr_s,delim,rmse_s

        WRITE(outunit,FMT=102)code_s(1),delim,lat_s(1),delim,lon_s(1),&
             &delim,site_s(1),delim,&
             &bias_s,delim,patrmse_s,delim,corr_s,delim,rmse_s


     ENDIF
     
     IF (code_new < 0) THEN
        CLOSE(inunit)
        CLOSE(outunit)
        STOP
     ENDIF

     BACKSPACE(inunit)

  ENDDO
  
100 CONTINUE 

  CLOSE(inunit)
  CLOSE(outunit)

101 FORMAT(i7,2(f10.3,f10.1),i5,4f10.4,i10)

102 FORMAT(i10,2(a,f10.3),a,i5,4(a,f10.4))

END PROGRAM eval_stats_tseries_dots_rt

