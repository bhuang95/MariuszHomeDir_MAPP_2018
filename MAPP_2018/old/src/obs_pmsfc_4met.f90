PROGRAM obs_pmsfc_4met

!to format pm2.5 sfc obs from AIRNow for met ascii convertor to ncdf

  IMPLICIT NONE

  INTEGER,  PARAMETER :: stlength=5,hour_interval=6
  REAL, PARAMETER :: unknown=-999.0,unknownelev=-9999.0,unknownmet=-9999.,small=0.01,&
       &concmax=150.,pres=unknownmet
  
  INTEGER, PARAMETER :: gcode=157
!PMTF - pm2.5

  CHARACTER(len=250) :: infile,outfile
  INTEGER :: iargc,i,inunit=51,outunit=61

  INTEGER, PARAMETER :: i8=SELECTED_INT_KIND(8)
  INTEGER(kind=i8) :: jdate,julmin,julmino

  INTEGER :: year,month,day,hour,mins,junk,yyyymmddhh,sitechar
  REAL :: lat,lon,elev,conc,jul,julo,bracket,rhh
  CHARACTER(len=11) :: stid
  CHARACTER(len=10) :: cyyyymmddhh
  INTEGER :: yyyy,mm,dd,hh
  CHARACTER(len=4) :: cyear
  CHARACTER(len=2) :: cmonth,cday,chour,cmins

  IF (iargc() < 3) THEN
     PRINT *,'Needs input and output file names and time of forecast as ident - Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,outfile)
  CALL getarg(3,cyyyymmddhh)

  READ(cyyyymmddhh(1:4),'(i4)')yyyy
  READ(cyyyymmddhh(5:6),'(i2)')mm
  READ(cyyyymmddhh(7:8),'(i2)')dd
  READ(cyyyymmddhh(9:10),'(i2)')hh

  julmin=jdate(yyyy,1,1)-1

  rhh=hh/24.
  jul=jdate(yyyy,mm,dd)-julmin+rhh
!compiler bug jdate(yyyy,mm,dd)-julmin+hh/24.

  bracket=(REAL(hour_interval)/2.+1.)/24.

  OPEN(inunit,file=infile,form='formatted')
  READ(inunit,*)

  OPEN(outunit,file=outfile,form='formatted')

  DO WHILE (.TRUE.)
     READ(inunit,'(5i5,2f11.5,f7.2,i3,f10.1,a11)',END=110)&
          year,month,day,hour,mins,&
          lat,lon,conc,sitechar,elev,stid

     julmino=jdate(year,1,1)-1
     
     rhh=(hour+mins/60.)/24.
     julo=jdate(year,month,day)-julmino+rhh

     IF (ABS(jul-julo) > bracket) CYCLE

     IF (conc > concmax .OR. conc < 0. ) CYCLE

     WRITE(cyear,'(i4)')year
     WRITE(cmonth,'(i2.2)')month
     WRITE(cday,'(i2.2)')day
     WRITE(chour,'(i2.2)')hour
     WRITE(cmins,'(i2.2)')mins

     IF (ABS(elev-unknownelev) < small) elev=unknownmet

     WRITE(outunit,111,advance='YES')'ADPSFC',stid(11-stlength+1:11),&
          &cyear//cmonth//cday//'_'//chour//cmins//'00',&
          &lat,lon,elev,gcode,pres,NINT(elev),conc

  ENDDO

110 CONTINUE

  CLOSE(outunit)

111 FORMAT(a6,1x,a5,1x,a15,1x,f7.2,1x,f7.2,1x,f7.1,1x,i3,1x,f7.1,1x,i5,1x,f6.2)

END PROGRAM obs_pmsfc_4met
