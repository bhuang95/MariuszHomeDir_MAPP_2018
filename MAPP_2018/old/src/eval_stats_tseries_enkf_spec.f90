PROGRAM eval_stats_tseries_enkf

!program to calculate bias, pattern rsme and correlation
!for time series plots
!Mariusz Pagowski, May 2011

  IMPLICIT NONE

  INTEGER, PARAMETER :: idp=KIND(1.d0)
  INTEGER, PARAMETER :: i8=SELECTED_INT_KIND(8)
  INTEGER, PARAMETER :: istatsmin=10,nstats=9
  INTEGER, PARAMETER :: nmaxstations=5000
  REAL, PARAMETER :: hdiff_max=100.,height_min=-500.,maxconc=100.,&
       &unknown=-9999.

  REAL(kind=idp) :: bias_s,patrmse_s,stdevobs_s,stdevfcst_s,corr_s
  REAL, DIMENSION(nmaxstations) :: obs_s,fcst_s,&
       &height_s,geopt_s,lat_s,lon_s,&
       &gridiobs_s,gridjobs_s
  
  INTEGER, DIMENSION(nmaxstations) :: site_s
  REAL :: date=-1.,fcstave_s,obsave_s,newdate

!max 100m height difference allowed
!min depression -500.
!will evaluate stations where height is unknown=-9999.
  LOGICAL :: loghdiff,logheight
  INTEGER :: nf,nst,nstin,nfcst,nfcst1,&
       &i,ii,ihour,imin,j,k,kk,l,ll,iargc,nobsvalid_s
  INTEGER :: inunit=55, outunit=56,utcmin,utcmax
  CHARACTER(len=1) :: cutc
  INTEGER, ALLOCATABLE, DIMENSION(:) :: iyearinit,imonthinit,&
       &idayinit,ihourinit
  INTEGER, ALLOCATABLE, DIMENSION(:) :: nobsvalid
  INTEGER, ALLOCATABLE, DIMENSION(:) :: site,utc

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: obs,fcst
  REAL, ALLOCATABLE, DIMENSION(:) :: height,geopt,lat,lon,&
       &gridiobs,gridjobs
  
  REAL(kind=idp), ALLOCATABLE, DIMENSION(:) :: fcstave,obsave,&
       &bias,patrmse,stdevobs,stdevfcst,corr
  REAL(kind=idp) :: obsdiff,fcstdiff

  CHARACTER(len=120) :: flatlons,fconc,infile,clogheight,outfile

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

  i=1
  
  DO WHILE (.TRUE.)

     READ(inunit,FMT=101,END=100)l,&
          &obs_s(i),height_s(i),&
          &fcst_s(i),geopt_s(i),site_s(i),lat_s(i),lon_s(i),&
          &gridiobs_s(i),gridjobs_s(i)

!     PRINT *,l,obs_s(i),height_s(i),&
!          &fcst_s(i),geopt_s(i),site_s(i),lat_s(i),lon_s(i),&
!          &gridiobs_s(i),gridjobs_s(i)
     

     IF (l < 0) THEN
     
        fcstave_s=0.
        obsave_s=0.
        nobsvalid_s=0
        patrmse_s=0.
        stdevobs_s=0.
        stdevfcst_s=0.
        corr_s=0.
        bias_s=0.
        
        j=i-1

        IF (j > istatsmin) THEN

           DO ii=1,j

              IF (logheight) THEN
                 loghdiff=(height(ii) < height_min) .OR. &
                      &(ABS(height(ii)-geopt(ii)) < hdiff_max)
              ENDIF
              
              IF (loghdiff) THEN
                 nobsvalid_s=nobsvalid_s+1
                 fcstave_s=fcstave_s+fcst_s(ii)
                 obsave_s=obsave_s+obs_s(ii)
              ENDIF
           ENDDO

           IF (nobsvalid_s > istatsmin) THEN

              fcstave_s=fcstave_s/REAL(nobsvalid_s)
              obsave_s=obsave_s/REAL(nobsvalid_s)
              bias_s=fcstave_s-obsave_s

              DO ii=1,j
                 
                 IF (logheight) THEN
                    loghdiff=(height(ii) < height_min) .OR. &
                         &(ABS(height(ii)-geopt(ii)) < hdiff_max)
                 ENDIF
                 
                 IF (loghdiff) THEN
                    obsdiff=obs_s(ii)-obsave_s
                    fcstdiff=fcst_s(ii)-fcstave_s
                    stdevobs_s=stdevobs_s+obsdiff**2
                    stdevfcst_s=stdevfcst_s+fcstdiff**2
                    patrmse_s=patrmse_s+(fcstdiff-obsdiff)**2
                    corr_s=corr_s+obsdiff*fcstdiff
                 ENDIF
                 
              ENDDO
           
              stdevobs_s=SQRT(1./REAL(nobsvalid_s-1)*stdevobs_s)
              stdevfcst_s=SQRT(1./REAL(nobsvalid_s-1)*stdevfcst_s)
              patrmse_s=SQRT(1./REAL(nobsvalid_s)*patrmse_s)
              corr_s=1./REAL(nobsvalid_s)*corr_s/&
                   &(stdevobs_s*stdevfcst_s)
           ENDIF
     
        ELSE
           fcstave_s=0.
           obsave_s=0.
        ENDIF
        
        newdate=obs_s(i)

        IF (date > 0.) THEN

           IF (j > istatsmin) THEN 

              PRINT *,'Writing out for date ', date
              WRITE(outunit,'(f7.2,i10,7e15.7)')date,nobsvalid_s,&
                   &bias_s,patrmse_s,corr_s,stdevobs_s,stdevfcst_s,&
                   &obsave_s,fcstave_s
              j=0
           ENDIF
        ENDIF

        date=newdate
        
        i=1
        
     ELSE
        i=i+1
     ENDIF
     
  ENDDO
  
100 CONTINUE 

  CLOSE(outunit)


101 FORMAT(i7,2(f10.3,f10.1),i5,4f10.4)

END PROGRAM eval_stats_tseries_enkf

