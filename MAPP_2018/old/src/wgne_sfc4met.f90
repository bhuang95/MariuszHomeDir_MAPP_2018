PROGRAM wgne_sfc4met

!to format wgne sfc obs for met ascii convertor to ncdf

  IMPLICIT NONE

  REAL, PARAMETER :: const_pq0   = 379.90516, const_a2    = 17.2693882,&
       &const_a3    = 273.16, const_a4    = 35.86
  
  INTEGER,  PARAMETER :: ngcodes=7,stlength=5,nhours=6
  REAL, PARAMETER :: unknown=-999.0,unknownelev=-99.0,unknownmet=-9999.,small=0.01
  
  INTEGER, DIMENSION(ngcodes), PARAMETER :: gcodes=(/32,31,11,17,53,61,71/)
!windspeed, winddir,  temp (K), dewpoint(K), mixing ratio, 6-hr precip, total cloud cover 

  CHARACTER(len=250) :: infile,outfile
  INTEGER :: iargc,i,inunit=51,outunit=61

  INTEGER :: year,month,day,hour,junk
  REAL :: lat,lon,elev,wspeed,wdir,temp,dewpoint,pres,mslp,precip,ccover,mixr
  CHARACTER(len=stlength) :: stid
  INTEGER :: istid
  CHARACTER(len=4) :: cyear
  CHARACTER(len=2) :: cmonth,cday,chour

  IF (iargc() < 2) THEN
     PRINT *,'Needs input and output file names - Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,outfile)
  
  OPEN(inunit,file=infile,form='formatted')
  DO i=1,11
     READ(inunit,*)
  ENDDO

  OPEN(outunit,file=outfile,form='formatted')

  DO WHILE (.TRUE.)
     READ(inunit,*,END=110)year,month,day,hour,stid,lat,lon,elev,wspeed,junk,wdir,junk,&
          &temp,junk,dewpoint,junk,pres,junk,mslp,junk,precip,junk,ccover

     WRITE(cyear,'(i4)')year
     WRITE(cmonth,'(i2.2)')month
     WRITE(cday,'(i2.2)')day
     WRITE(chour,'(i2.2)')hour/100

     IF (ABS(pres-unknown) > small) THEN 
        pres=pres*1.e-2 ! Pa -> hPa
     ELSE
        pres=unknownmet
     ENDIF
     
     IF (ABS(elev-unknownelev) < small) elev=unknownmet

     IF (ABS(wspeed-unknown) > small .AND. ABS(wdir-unknown) > small) THEN
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(1),pres,NINT(elev)
        WRITE(outunit,'(f6.2)',advance='YES')wspeed
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(2),pres,NINT(elev)
        WRITE(outunit,'(f6.0)',advance='YES')wdir
     ENDIF
     
     IF (ABS(temp-unknown) > small) THEN
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(3),pres,NINT(elev)
        WRITE(outunit,'(f6.2)',advance='YES')temp+const_a3
     ENDIF

     IF (ABS(dewpoint-unknown) > small ) THEN
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(4),pres,NINT(elev)
        WRITE(outunit,'(f6.2)',advance='YES')dewpoint+const_a3
     ENDIF

     IF (ABS(dewpoint-unknown) > small .AND. ABS(pres-unknownmet) > small) THEN
        mixr=const_pq0/(pres*1.e2)*EXP(const_a2*dewpoint/(dewpoint+const_a3-const_a4))
        mixr=MAX(0.,mixr/(1.-mixr))
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(5),pres,NINT(elev)
        WRITE(outunit,'(f6.3)',advance='YES')mixr
     ENDIF
     
     IF (ABS(precip-unknown) > small)  THEN
        WRITE(outunit,112,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(6),nhours,NINT(elev)
        WRITE(outunit,'(f6.2)',advance='YES')precip 
     ENDIF
     
     IF (ABS(ccover-unknown) > small) THEN
        WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
             &cyear//cmonth//cday//'_'//chour//'0000',&
             &lat,lon,elev,gcodes(7),pres,NINT(elev)
        WRITE(outunit,'(i3)',advance='YES')NINT(ccover)
     ENDIF
     
  ENDDO

110 CONTINUE

  CLOSE(outunit)

111 FORMAT(a6,1x,a5,1x,a15,1x,f7.2,1x,f7.2,1x,f7.1,1x,i2,1x,f7.1,1x,i5,1x)
112 FORMAT(a6,1x,a5,1x,a15,1x,f7.2,1x,f7.2,1x,f7.1,1x,i2,1x,i7,1x,i5,1x)

END PROGRAM wgne_sfc4met
