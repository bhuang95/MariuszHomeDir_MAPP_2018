MODULE module_layers

  USE module_domain, ONLY : nsig
  USE kinds, ONLY: i_kind,r_kind

  PRIVATE

  PUBLIC :: msig, calc_msig, add_rtm_layers

  INTEGER(i_kind) :: msig

  INTEGER(i_kind),DIMENSION(100):: nlayers        ! number of RTM layers per model layer

CONTAINS

  FUNCTION calc_msig RESULT(msig)
  
    CHARACTER(len=20) :: myname='calc_msig'
  
    msig = 0
    IF(SIZE(nlayers)<nsig) WRITE(6,*)TRIM(myname)//' insufficient size of nlayers',99

    DO k=1,nsig
       nlayers(k)=1
    ENDDO

    DO k=1,nsig
       msig = msig + nlayers(k)
    end do

  END FUNCTION calc_msig


  SUBROUTINE add_rtm_layers(prsitmp,prsltmp,prsitmp_ext,prsltmp_ext,klevel)

! !USES:

    USE constants, ONLY: half,ten,one_tenth
    USE crtm_module, ONLY: toa_pressure

    IMPLICIT NONE

! !INPUT PARAMETERS:
    INTEGER(i_kind),DIMENSION(msig)  ,INTENT(  out) :: klevel

    REAL(r_kind)   ,DIMENSION(nsig+1),INTENT(in   ) :: prsitmp
    REAL(r_kind)   ,DIMENSION(nsig)  ,INTENT(in   ) :: prsltmp

    REAL(r_kind)   ,DIMENSION(msig+1),INTENT(  out) :: prsitmp_ext
    REAL(r_kind)   ,DIMENSION(msig)  ,INTENT(  out) :: prsltmp_ext
    INTEGER(i_kind),DIMENSION(100):: nlayers 

!   Declare local variables
    INTEGER(i_kind) k,kk,l
    REAL(r_kind) dprs,toa_prs_kpa

    DO k=1,SIZE(nlayers)
       nlayers(k) = 1
    END DO


!   Convert toa_pressure to kPa
!   ---------------------------
    toa_prs_kpa = toa_pressure*one_tenth

!   Check if model top pressure above rtm top pressure, where prsitmp
!   is in kPa and toa_pressure is in hPa.

    IF (prsitmp(nsig) < toa_prs_kpa)THEN
       WRITE(6,*)'ADD_RTM_LAYERS:  model top pressure(hPa)=', &
            ten*prsitmp(nsig),&
            ' above rtm top pressure(hPa)=',toa_pressure
       STOP(35)
    END IF

!   Linear in pressure sub-divsions
    kk=0
    DO k = 1,nsig
       IF (nlayers(k)<=1) THEN
          kk = kk + 1
          prsltmp_ext(kk) = prsltmp(k)
          prsitmp_ext(kk) = prsitmp(k)
          klevel(kk) = k
       ELSE
          IF (k/=nsig) THEN
             dprs = (prsitmp(k+1)-prsitmp(k))/nlayers(k)
          ELSE
             dprs = (toa_prs_kpa -prsitmp(k))/nlayers(k)
          END IF
          prsitmp_ext(kk+1) = prsitmp(k)
          DO l=1,nlayers(k)
             kk=kk + 1
             prsitmp_ext(kk+1) = prsitmp(k) + dprs*l
             prsltmp_ext(kk) = half*(prsitmp_ext(kk+1)+prsitmp_ext(kk))
             klevel(kk) = k
          END DO
       ENDIF
    END DO

!   Set top of atmosphere pressure
    prsitmp_ext(msig+1) = toa_prs_kpa

  END SUBROUTINE add_rtm_layers

END MODULE module_layers
