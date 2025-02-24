SUBROUTINE Set_CRTM_Aerosol_ ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)


!   input argument list:
!     km        : number of CRTM levels
!     na        : number of aerosols
!     na_crtm   : number of aerosols seen by CRTM
!     aero_name : GOCART aerosol names
!     aero_conc : aerosol concentration (Kg/m2)
!     rh        : relative humdity [0,1]
!     aerosol   : CRTM Aerosol object
!
!   output argument list:
!     aero_conc : aerosol concentration (Kg/m2)
!     aerosol   : CRTM Aerosol object
!

! USES:

  USE kinds, ONLY: i_kind,r_kind
  USE constants, ONLY: tiny_r_kind
  USE CRTM_Aerosol_Define, ONLY: CRTM_Aerosol_type
  USE module_utils, ONLY: getindex
  USE crtm_module, ONLY: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
       DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL

  IMPLICIT NONE

! !ARGUMENTS:

  INTEGER(i_kind) , INTENT(in)    :: km                ! number of levels
  INTEGER(i_kind) , INTENT(in)    :: na                ! number of aerosols
  INTEGER(i_kind) , INTENT(in)    :: na_crtm           ! number of aerosols seen by CRTM
  CHARACTER(len=*), INTENT(in)    :: aero_name(na)     ! [na]    GOCART aerosol names
  REAL(r_kind),     INTENT(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
  REAL(r_kind),     INTENT(in)    :: rh(km)            ! [km]    relative humdity [0,1]

  TYPE(CRTM_Aerosol_type), INTENT(inout) :: aerosol(na_crtm)! [na]   CRTM Aerosol object

  INTEGER(i_kind) :: i, k, ii
  INTEGER(i_kind) :: indx_p25, indx_dust1, indx_dust2, indx_dust3, indx_dust4, indx_dust5
  INTEGER(i_kind) :: indx_bc1, indx_oc1

  indx_bc1=-1; indx_oc1=-1; indx_dust1=-1; indx_dust2=-1
  indx_dust3=-1; indx_dust4=-1; indx_dust5=-1; indx_p25=-1

  indx_p25   = getindex(aero_name,'p25')
  indx_dust1 = getindex(aero_name,'dust1')
  indx_dust2 = getindex(aero_name,'dust2')
  indx_dust3 = getindex(aero_name,'dust3')
  indx_dust4 = getindex(aero_name,'dust4')
  indx_dust5 = getindex(aero_name,'dust5')
  indx_bc1   = getindex(aero_name,'bc1')
  indx_oc1   = getindex(aero_name,'oc1')

  ii=0

  DO i = 1, na

     IF ( TRIM(aero_name(i)) == 'p25' ) CYCLE
     ii=ii+1

! assign aerosol type
     SELECT CASE ( TRIM(aero_name(i)) )
     CASE ('sulf')
        aerosol(ii)%type  = SULFATE_AEROSOL
     CASE ('bc1','bc2')
        aerosol(ii)%type  = BLACK_CARBON_AEROSOL
     CASE ('oc1','oc2')
        aerosol(ii)%type  = ORGANIC_CARBON_AEROSOL
     CASE ('dust1','dust2','dust3','dust4','dust5')
        aerosol(ii)%type  = DUST_AEROSOL
     CASE ('seas1')
        aerosol(ii)%type  = SEASALT_SSAM_AEROSOL
     CASE ('seas2')
        aerosol(ii)%type  = SEASALT_SSCM1_AEROSOL
     CASE ('seas3')
        aerosol(ii)%type  = SEASALT_SSCM2_AEROSOL
     CASE ('seas4')
        aerosol(ii)%type  = SEASALT_SSCM3_AEROSOL
     END SELECT

     IF ( indx_p25 > 0 ) THEN
! partition p25 to dust1 and dust2
        IF ( i == indx_dust1 ) THEN
           aero_conc(:,i) = aero_conc(:,i)+ 0.78_r_kind*aero_conc(:,indx_p25)
        ENDIF
        IF ( i == indx_dust2 ) THEN
           aero_conc(:,i) = aero_conc(:,i)+ 0.22_r_kind*aero_conc(:,indx_p25)
        ENDIF
     ENDIF

! crtm aerosol structure
     DO k = 1, km
        aerosol(ii)%concentration(k) = MAX(tiny_r_kind, aero_conc(k,i))
! calculate effective radius
        IF (aerosol(ii)%type  /= DUST_AEROSOL) THEN
           aerosol(ii)%effective_radius(k) &
                = GOCART_Aerosol_size(i, aerosol(i)%type, rh(k))
        ELSE
! 5 dust bins
           aerosol(indx_dust1)%effective_radius(k) = 0.55_r_kind
           aerosol(indx_dust2)%effective_radius(k) = 1.4_r_kind
           aerosol(indx_dust3)%effective_radius(k) = 2.4_r_kind
           aerosol(indx_dust4)%effective_radius(k) = 4.5_r_kind
           aerosol(indx_dust5)%effective_radius(k) = 8.0_r_kind
        ENDIF
     ENDDO

  ENDDO  ! na

CONTAINS

  FUNCTION GOCART_Aerosol_size( kk, itype,  & ! Input
       eh ) & ! Input in 0-1
       RESULT( R_eff  )   ! in micrometer
    USE crtm_aerosolcoeff, ONLY: AeroC
    IMPLICIT NONE
!
!   modified from a function provided by Quanhua Liu
!
    INTEGER(i_kind) ,INTENT(in) :: kk, itype
    REAL(r_kind)    ,INTENT(in) :: eh

    INTEGER(i_kind) :: j1,j2,k
    REAL(r_kind)    :: h1
    REAL(r_kind)    :: R_eff

    IF ( itype==DUST_AEROSOL ) THEN
       RETURN
    ELSE IF ( itype==BLACK_CARBON_AEROSOL .AND. kk==indx_bc1 ) THEN
       R_eff = AeroC%Reff(1,itype )
       RETURN
    ELSE IF ( itype==ORGANIC_CARBON_AEROSOL .AND. kk==indx_oc1 ) THEN
       R_eff = AeroC%Reff(1,itype )
       RETURN
    ENDIF

    j2 = 0
    IF ( eh < AeroC%RH(1) ) THEN
       j1 = 1
    ELSE IF ( eh > AeroC%RH(AeroC%n_RH) ) THEN
       j1 = AeroC%n_RH
    ELSE
       DO k = 1, AeroC%n_RH-1
          IF ( eh <= AeroC%RH(k+1) .AND. eh > AeroC%RH(k) ) THEN
             j1 = k
             j2 = k+1
             h1 = (eh-AeroC%RH(k))/(AeroC%RH(k+1)-AeroC%RH(k))
             EXIT
          ENDIF
       ENDDO
    ENDIF

    IF ( j2 == 0 ) THEN
       R_eff = AeroC%Reff(j1,itype )
    ELSE
       R_eff = (1.0_r_kind-h1)*AeroC%Reff(j1,itype ) + h1*AeroC%Reff(j2,itype )
    ENDIF

    RETURN
  END FUNCTION GOCART_Aerosol_size

END SUBROUTINE Set_CRTM_Aerosol_
