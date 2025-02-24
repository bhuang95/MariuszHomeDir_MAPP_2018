SUBROUTINE genqsat(qsat,tsen,prsl,nlat,nlon,nsig,ice)

!   input argument list:
!     tsen      - input sensibile temperature field (nlat,nlon,nsig)
!     prsl      - input layer mean pressure field (nlat,nlon,nsig)
!     nlat      - number of latitudes                              
!     nlon      - number of longitudes                             
!     nsig      - number of levels                              
!     ice       - logical flag:  T=include ice and ice-water effects,
!                 depending on t, in qsat calcuations.
!                 otherwise, compute qsat with respect to water surface
!
!   output argument list:
!     qsat      - saturation specific humidity (output)
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  USE kinds, ONLY: r_kind,i_kind
  USE constants, ONLY: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp,half,one_tenth

  LOGICAL                               ,INTENT(in   ) :: ice
  REAL(r_kind),DIMENSION(nlat,nlon,nsig),INTENT(  out) :: qsat
  REAL(r_kind),DIMENSION(nlat,nlon,nsig),INTENT(in   ) :: tsen,prsl
  INTEGER(i_kind)                       ,INTENT(in   ) :: nlat,nlon,nsig

  INTEGER(i_kind) k,j,i
  REAL(r_kind) pw,tdry,tr,es,es2
  REAL(r_kind) w,onep3,esmax
  REAL(r_kind) desidt,deswdt,dwdt,desdt,esi,esw
  REAL(r_kind),DIMENSION(nlat):: mint,estmax
  INTEGER(i_kind),DIMENSION(nlat):: lmint

! Declare local parameters
  REAL(r_kind),PARAMETER:: r015 = 0.15_r_kind

  onep3 = 1.e3_r_kind

  DO j=1,nlon
     DO i=1,nlat
        mint(i)=340._r_kind
        lmint(i)=1
     END DO
     DO k=1,nsig
        DO i=1,nlat
           IF((prsl(i,j,k) < 30._r_kind .AND.  &
                prsl(i,j,k) > 2._r_kind) .AND.  &
                tsen(i,j,k) < mint(i))THEN
              lmint(i)=k
              mint(i)=tsen(i,j,k)
           END IF
        END DO
     END DO
     DO i=1,nlat
        tdry = mint(i)
        tr = ttp/tdry
        IF (tdry >= ttp .OR. .NOT. ice) THEN
           estmax(i) = psat * (tr**xa) * EXP(xb*(one-tr))
        ELSEIF (tdry < tmix) THEN
           estmax(i) = psat * (tr**xai) * EXP(xbi*(one-tr))
        ELSE
           w  = (tdry - tmix) / (ttp - tmix)
           estmax(i) =  w * psat * (tr**xa) * EXP(xb*(one-tr)) &
                + (one-w) * psat * (tr**xai) * EXP(xbi*(one-tr))
        ENDIF
     END DO

     DO k = 1,nsig
        DO i = 1,nlat

           tdry = tsen(i,j,k)
           tr = ttp/tdry
           IF (tdry >= ttp .OR. .NOT. ice) THEN
              es = psat * (tr**xa) * EXP(xb*(one-tr))
           ELSEIF (tdry < tmix) THEN
              es = psat * (tr**xai) * EXP(xbi*(one-tr))
           ELSE
              esw = psat * (tr**xa) * EXP(xb*(one-tr)) 
              esi = psat * (tr**xai) * EXP(xbi*(one-tr)) 
              w  = (tdry - tmix) / (ttp - tmix)
!             es =  w * esw + (one-w) * esi
              es =  w * psat * (tr**xa) * EXP(xb*(one-tr)) &
                   + (one-w) * psat * (tr**xai) * EXP(xbi*(one-tr))

           ENDIF

           pw = onep3*prsl(i,j,k)
           esmax = es
           IF(lmint(i) < k)THEN
              esmax=0.1_r_kind*pw
              esmax=MIN(esmax,estmax(i))
           END IF
           es2=MIN(es,esmax)
           qsat(i,j,k) = eps * es2 / (pw - omeps * es2)

        END DO
     END DO
  END DO

  RETURN

END SUBROUTINE genqsat

