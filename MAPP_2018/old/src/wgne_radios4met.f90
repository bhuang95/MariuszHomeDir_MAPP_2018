PROGRAM wgne_rawind4met

!to format wgne rawindsondes obs for met ascii convertor to ncdf

  IMPLICIT NONE

  REAL, PARAMETER :: const_pq0   = 379.90516, const_a2    = 17.2693882,&
       &const_a3    = 273.16, const_a4    = 35.86
  
  INTEGER,  PARAMETER :: ngcodes=6,stlength=5
  REAL, PARAMETER :: unknown=9999.99,small=0.01
  
  INTEGER, DIMENSION(ngcodes), PARAMETER :: gcodes=(/7,32,31,11,17,53/)
!geop,windspeed, winddir, temp (K), dewpoint (K), mixing ratio


  CHARACTER(len=250) :: infile,outfile
  INTEGER :: iargc,i,inunit=51,outunit=61

  REAL :: lat,lon,elev,wspeed,wdir,temp,dewpoint,pres,geop,mixr
  CHARACTER(len=stlength) :: stid,stid_old=''
  CHARACTER(len=8) :: date
  CHARACTER(len=4) :: hour

  IF (iargc() < 2) THEN
     PRINT *,'Needs input and output file names - Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,outfile)
  
  OPEN(inunit,file=infile,form='formatted')
  READ(inunit,*)

  OPEN(outunit,file=outfile,form='formatted')

  DO WHILE (.TRUE.)
     READ(inunit,*,END=110)stid,lon,lat,pres,geop,wdir,wspeed,temp,dewpoint,date,hour
     IF (stid /= stid_old) elev=geop

!sometimes 1000mb pressure is screwed in the data for rawinsondes 

     IF (ABS(pres - 1000.0) < small) CYCLE

     IF (ABS(pres-unknown) > small .AND. ABS(geop-unknown) > small) THEN

        WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
             &date//'_'//hour//'00',&
             &lat,lon,elev,gcodes(1),pres,NINT(geop)
        WRITE(outunit,'(i5)',advance='YES'),NINT(geop)

        IF (ABS(wspeed-unknown) > small .AND. ABS(wdir-unknown) > small) THEN
           WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
                &date//'_'//hour//'00',&
                &lat,lon,elev,gcodes(2),pres,NINT(geop)
           WRITE(outunit,'(f6.1)',advance='YES')wspeed
           WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
                &date//'_'//hour//'00',&
                &lat,lon,elev,gcodes(3),pres,NINT(geop)
           WRITE(outunit,'(f6.1)',advance='YES')wdir
        ENDIF

        IF (ABS(temp-unknown) > small) THEN
           WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
                &date//'_'//hour//'00',&
                &lat,lon,elev,gcodes(4),pres,NINT(geop)
           WRITE(outunit,'(f6.2)',advance='YES')temp+const_a3
        ENDIF

        IF (ABS(dewpoint-unknown) > small) THEN
           WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
                &date//'_'//hour//'00',&
                &lat,lon,elev,gcodes(5),pres,NINT(geop)
           WRITE(outunit,'(f6.2)',advance='YES')dewpoint+const_a3
        ENDIF

        IF (ABS(dewpoint-unknown) > small) THEN
           mixr=const_pq0/(pres*1.e2)*EXP(const_a2*dewpoint/(dewpoint+const_a3-const_a4))
           mixr=MAX(0.,mixr/(1.-mixr))
           WRITE(outunit,111,advance='NO')'ADPUPA',stid,&
                &date//'_'//hour//'00',&
                &lat,lon,elev,gcodes(6),pres,NINT(geop)
           WRITE(outunit,'(f6.3)',advance='YES')mixr
        ENDIF

     ENDIF
     
     stid_old=stid

  ENDDO

110 CONTINUE

  CLOSE(outunit)

111 FORMAT(a6,1x,a5,1x,a15,1x,f7.2,1x,f7.2,1x,f7.1,1x,i2,1x,f7.1,1x,i5,1x)

END PROGRAM wgne_rawind4met
