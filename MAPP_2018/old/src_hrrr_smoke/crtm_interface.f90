MODULE crtm_interface

  USE kinds,ONLY: r_kind,i_kind,r_single
  USE crtm_module, ONLY: crtm_atmosphere_type,crtm_surface_type,crtm_geometry_type, &
       crtm_options_type,crtm_rtsolution_type,crtm_destroy,crtm_options_destroy, &
       crtm_options_create,crtm_options_associated,success,crtm_atmosphere_create, &
       crtm_surface_create,crtm_k_matrix,crtm_forward, &   
       ssu_input_setvalue, &
       crtm_channelinfo_type, &
       crtm_surface_destroy, crtm_surface_associated, crtm_surface_zero, &
       crtm_atmosphere_associated, &
       crtm_atmosphere_destroy,crtm_atmosphere_zero, &
       crtm_rtsolution_type, crtm_rtsolution_create, &
       crtm_rtsolution_destroy, crtm_rtsolution_associated, &
       crtm_irlandcoeff_classification, &
       crtm_kind => fp, &
       crtm_microwave_sensor => microwave_sensor

  USE crtm_aod_module, ONLY: crtm_aod_k
  USE module_domain
  USE module_utils, only: getindex
  USE module_wrf_gocart, ONLY: naero_gocart_wrf, n_aerosols_crtm, aeronames_gocart_wrf_gsi

  USE module_layers, ONLY:  msig, calc_msig, add_rtm_layers

  IMPLICIT NONE

  PRIVATE
  PUBLIC init_crtm            ! Subroutine initializes crtm for specified instrument
  PUBLIC call_crtm            ! Subroutine creates profile for crtm, calls crtm, then adjoint of create
  PUBLIC destroy_crtm         ! Subroutine destroys initialization for crtm
  PUBLIC sensorindex
  PUBLIC surface
  PUBLIC isatid               ! = 1  index of satellite id
  PUBLIC itime                ! = 2  index of analysis relative obs time
  PUBLIC ilon                 ! = 3  index of grid relative obs location (x)
  PUBLIC ilat                 ! = 4  index of grid relative obs location (y)
  PUBLIC ilzen_ang            ! = 5  index of local (satellite) zenith angle (radians)
  PUBLIC ilazi_ang            ! = 6  index of local (satellite) azimuth angle (radians)
  PUBLIC iscan_ang            ! = 7  index of scan (look) angle (radians)
  PUBLIC iscan_pos            ! = 8  index of integer scan position
  PUBLIC iszen_ang            ! = 9  index of solar zenith angle (degrees)
  PUBLIC isazi_ang            ! = 10 index of solar azimuth angle (degrees)
  PUBLIC logwrf_gocart
  PUBLIC nsigaerojac

!  Note other module variables are only used within this routine

  CHARACTER(len=*), PARAMETER :: myname='crtm_interface'

! Indices for the CRTM NPOESS EmisCoeff file
  INTEGER(i_kind), PARAMETER :: INVALID_LAND = 0
  INTEGER(i_kind), PARAMETER :: COMPACTED_SOIL = 1
  INTEGER(i_kind), PARAMETER :: TILLED_SOIL = 2
  INTEGER(i_kind), PARAMETER :: IRRIGATED_LOW_VEGETATION = 5
  INTEGER(i_kind), PARAMETER :: MEADOW_GRASS = 6
  INTEGER(i_kind), PARAMETER :: SCRUB = 7
  INTEGER(i_kind), PARAMETER :: BROADLEAF_FOREST = 8
  INTEGER(i_kind), PARAMETER :: PINE_FOREST = 9
  INTEGER(i_kind), PARAMETER :: TUNDRA = 10
  INTEGER(i_kind), PARAMETER :: GRASS_SOIL = 11
  INTEGER(i_kind), PARAMETER :: BROADLEAF_PINE_FOREST = 12
  INTEGER(i_kind), PARAMETER :: GRASS_SCRUB = 13
  INTEGER(i_kind), PARAMETER :: URBAN_CONCRETE = 15
  INTEGER(i_kind), PARAMETER :: BROADLEAF_BRUSH = 17
  INTEGER(i_kind), PARAMETER :: WET_SOIL = 18
  INTEGER(i_kind), PARAMETER :: SCRUB_SOIL = 19

  CHARACTER(len=20),SAVE,ALLOCATABLE,DIMENSION(:)   :: aero_names   ! aerosol names
  REAL(r_kind)   , SAVE ,ALLOCATABLE,DIMENSION(:,:) :: aero         ! aerosol (guess) profiles at obs location
  REAL(r_kind)   , SAVE ,ALLOCATABLE,DIMENSION(:,:) :: aero_conc    ! aerosol (guess) concentrations at obs location
  REAL(r_kind)   , SAVE ,ALLOCATABLE,DIMENSION(:)   :: auxrh        ! temporary array for rh profile as seen by CRTM

  INTEGER(i_kind),SAVE :: isatid,itime,ilon,ilat,ilzen_ang,ilazi_ang,iscan_ang
  INTEGER(i_kind),SAVE :: iscan_pos,iszen_ang,isazi_ang
  INTEGER(i_kind),SAVE :: sensorindex
  INTEGER(i_kind),SAVE :: n_aerosols_jac     ! number of aerosols in jocabian
  INTEGER(i_kind),SAVE :: n_aerosols         ! number of aerosols considered
  INTEGER(i_kind),SAVE :: nsigaerojac

  INTEGER(i_kind), PARAMETER :: min_n_absorbers = 2
  INTEGER(i_kind),SAVE :: indx_p25, indx_dust1, indx_dust2
  

  TYPE(crtm_atmosphere_type),SAVE,DIMENSION(1)   :: atmosphere
  TYPE(crtm_surface_type),SAVE,DIMENSION(1)      :: surface
  TYPE(crtm_geometry_type),SAVE,DIMENSION(1)     :: geometryinfo
  TYPE(crtm_options_type),SAVE,DIMENSION(1)      :: options
  TYPE(crtm_channelinfo_type),SAVE,DIMENSION(1)  :: channelinfo

  TYPE(crtm_atmosphere_type),SAVE,ALLOCATABLE,DIMENSION(:,:):: atmosphere_k
  TYPE(crtm_surface_type),SAVE,ALLOCATABLE,DIMENSION(:,:):: surface_k
  TYPE(crtm_rtsolution_type),SAVE,ALLOCATABLE,DIMENSION(:,:):: rtsolution
  TYPE(crtm_rtsolution_type),SAVE,ALLOCATABLE,DIMENSION(:,:):: rtsolution0              
  TYPE(crtm_rtsolution_type),SAVE,ALLOCATABLE,DIMENSION(:,:):: rtsolution_k
  LOGICAL :: logwrf_gocart


CONTAINS

  SUBROUTINE init_crtm(nchanl,n_aeros,isis,obstype)
!     nchanl       - number of channels    
!     isis         - instrument/sensor character string 
!     obstype      - observation type
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


    USE kinds,ONLY: r_kind,i_kind,r_single
    USE crtm_module, ONLY: mass_mixing_ratio_units,co2_id,o3_id,crtm_init, &
         toa_pressure,max_n_layers, &
         volume_mixing_ratio_units,h2o_id,ch4_id,n2o_id,co_id,success
    USE constants, ONLY: zero,tiny_r_kind

    IMPLICIT NONE

! argument 

    CHARACTER(20)  ,INTENT(in) :: isis
    CHARACTER(10)  ,INTENT(in) :: obstype
    INTEGER(i_kind),INTENT(in)  :: nchanl,n_aeros

    CHARACTER(len=256) :: crtm_coeffs_path
    CHARACTER(len=20) :: myname='init_crtm'

! local variables
    INTEGER(i_kind) :: ier,error_status
    LOGICAL :: ice,Load_AerosolCoeff,Load_CloudCoeff
    CHARACTER(len=20),DIMENSION(1) :: sensorlist
    INTEGER(i_kind) :: icf4crtm,indx,ii,icloud4crtm,icount
! ...all "additional absorber" variables
    INTEGER(i_kind) :: i,j
    INTEGER(i_kind) :: ig
    INTEGER(i_kind) :: n_absorbers, n_aerosols_jac,n_clouds

! Get indexes of variables composing the jacobian_aero

    n_aerosols=n_aeros

    msig=calc_msig()

    IF(n_aerosols>0)THEN
       ALLOCATE(aero(nsig,n_aerosols),aero_conc(msig,n_aerosols),auxrh(msig))
       ALLOCATE(aero_names(n_aerosols))
       Load_AerosolCoeff=.TRUE.
       IF (n_aerosols == naero_gocart_wrf) THEN
          n_aerosols_jac=n_aerosols_crtm+1
          logwrf_gocart=.TRUE.
          aero_names=aeronames_gocart_wrf_gsi
          indx_p25   = getindex(aero_names,'p25')
          indx_dust1 = getindex(aero_names,'dust1')
          indx_dust2 = getindex(aero_names,'dust2')
       ELSE
          indx_p25   = -1
          n_aerosols_jac=n_aerosols_crtm
          logwrf_gocart=.FALSE.
          j=0
          DO i=1,n_aerosols_crtm
             IF (aeronames_gocart_wrf_gsi(i) /= 'p25' ) THEN
                j=j+1
                aero_names(j)=aeronames_gocart_wrf_gsi(i)
             ENDIF
          ENDDO
       ENDIF
    ELSE
       Load_AerosolCoeff=.FALSE.
    ENDIF

! Inquire presence of extra fields in MetGuess

    Load_CloudCoeff = .FALSE.
    n_clouds=0

! Set up index for input satellite data array

    isatid    = 1  ! index of satellite id
    itime     = 2  ! index of analysis relative obs time
    ilon      = 3  ! index of grid relative obs location (x)
    ilat      = 4  ! index of grid relative obs location (y)
    ilzen_ang = 5  ! index of local (satellite) zenith angle (radians)
    ilazi_ang = 6  ! index of local (satellite) azimuth angle (radians)
    iscan_ang = 7  ! index of scan (look) angle (radians)
    iscan_pos = 8  ! index of integer scan position
    iszen_ang = 9  ! index of solar zenith angle (degrees)
    isazi_ang = 10 ! index of solar azimuth angle (degrees)

! Are there aerosols to affect CRTM?

! Initialize radiative transfer

    crtm_coeffs_path = "./"

    sensorlist(1)=isis

    error_status = crtm_init(sensorlist,channelinfo,&
         Load_CloudCoeff=Load_CloudCoeff,&
         &Load_AerosolCoeff=Load_AerosolCoeff)

    IF (error_status /= success) THEN
       WRITE(6,*)TRIM(myname)//':  ***ERROR*** crtm_init error_status=',&
            &error_status, '   TERMINATE PROGRAM EXECUTION'
       STOP(71)
    ENDIF

    sensorindex = 0
    IF (channelinfo(1)%sensor_id == isis) THEN
       sensorindex = 1
    ENDIF
    IF (sensorindex == 0 ) THEN
       WRITE(6,*)TRIM(myname)//&
            &':  ***WARNING*** problem with sensorindex=',isis,&
            ' --> CAN NOT PROCESS isis=',isis,&
            &'   TERMINATE PROGRAM EXECUTION found ',&
            channelinfo(1)%sensor_id
       STOP(71)
    ENDIF

! Check for consistency between user specified number of channels (nchanl)
! and those defined by CRTM channelinfo structure.   Return to calling
! routine if there is a mismatch.

    IF (nchanl /= channelinfo(sensorindex)%n_channels) THEN
       WRITE(6,*)TRIM(myname)//&
            &':  ***WARNING*** mismatch between nchanl=',&
            nchanl,' and n_channels=',&
            &channelinfo(sensorindex)%n_channels,&
            ' --> CAN NOT PROCESS isis=',isis,&
            &'   TERMINATE PROGRAM EXECUTION'
       STOP(71)
    ENDIF

! Allocate structures for radiative transfer

    ALLOCATE(&
         rtsolution  (channelinfo(sensorindex)%n_channels,1),&
         rtsolution_k(channelinfo(sensorindex)%n_channels,1),&
         atmosphere_k(channelinfo(sensorindex)%n_channels,1),&
         surface_k   (channelinfo(sensorindex)%n_channels,1))

!  Check to ensure that number of levels requested does not exceed crtm max

    IF(msig > max_n_layers)THEN
       WRITE(6,*) TRIM(myname)//&
            &':  msig > max_n_layers - increase crtm max_n_layers ',&
            msig,max_n_layers
       STOP(36)
    END IF

!  Create structures for radiative transfer

    n_absorbers=min_n_absorbers

    CALL crtm_atmosphere_create(atmosphere(1),msig,n_absorbers,&
         &n_clouds,n_aerosols_crtm)

    CALL crtm_rtsolution_create(rtsolution,msig)
    CALL crtm_rtsolution_create(rtsolution_k,msig)
    CALL crtm_options_create(options,nchanl)

    IF (.NOT.(crtm_atmosphere_associated(atmosphere(1)))) &
         WRITE(6,*)TRIM(myname)//': ***ERROR** creating atmosphere.'
    IF (.NOT.(ANY(crtm_rtsolution_associated(rtsolution)))) &
         WRITE(6,*)TRIM(myname)//': ***ERROR** creating rtsolution.'
    IF (.NOT.(ANY(crtm_rtsolution_associated(rtsolution_k)))) &
         WRITE(6,*)TRIM(myname)//': ***ERROR** creating rtsolution_k.'
    IF (.NOT.(ANY(crtm_options_associated(options)))) &
         WRITE(6,*)TRIM(myname)//': ***ERROR** creating options.'

! Turn off antenna correction

    options(1)%use_antenna_correction = .FALSE. 

! Check for consistency with information in crtm for number of channels

    IF(nchanl /= channelinfo(sensorindex)%n_channels) &
         &WRITE(6,*)TRIM(myname)//': ***ERROR** nchanl,n_channels ', &
         nchanl,channelinfo(sensorindex)%n_channels

! Load surface sensor data structure

    surface(1)%sensordata%n_channels = channelinfo(sensorindex)%n_channels

    atmosphere(1)%n_layers = msig
    atmosphere(1)%absorber_id(1) = H2O_ID
    atmosphere(1)%absorber_id(2) = O3_ID
    atmosphere(1)%absorber_units(1) = MASS_MIXING_RATIO_UNITS
    atmosphere(1)%absorber_units(2) = VOLUME_MIXING_RATIO_UNITS
    atmosphere(1)%level_pressure(0) = TOA_PRESSURE

! Currently all considered trace gases affect CRTM. Load trace gases into CRTM atmosphere

!  Allocate structure for _k arrays (jacobians)

    DO ii=1,nchanl
       atmosphere_k(ii,1) = atmosphere(1)
       surface_k(ii,1)   = surface(1)
    END DO

    RETURN
  END SUBROUTINE init_crtm

  SUBROUTINE call_crtm(obstype,nchanl, &
       h,q,qs,prsl,prsi,aeros,error_status,&
       layer_od)  

!   input argument list:
!     obstype      - type of observations for which to get profile
!     data_s       - array containing input data information
!     nchanl       - number of channels

!
!   output argument list:
!     h            - interpolated temperature
!     q            - interpolated specific humidity (max(qsmall,q))
!     prsl         - interpolated layer pressure (nsig)
!     prsi         - interpolated level pressure (nsig+1)
!     error_status - error status from crtm
!     layer_od     - layer optical depth
!     jacobian_aero- nsigaerojac level jacobians for use in intaod
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
    USE kinds, ONLY: r_kind,i_kind
    USE constants, ONLY: zero,one,one_tenth,fv,r0_05,r10,r100,r1000,constoz,&
         &grav,rad2deg,deg2rad, &
         sqrt_tiny_r_kind,constoz, rd, rd_over_g, two, three, four,five,t0c
    USE constants, ONLY: max_varname_length,pi  
    USE set_crtm_aerosolmod, ONLY: set_crtm_aerosol
    USE crtm_module, ONLY: limit_exp,o3_id

    IMPLICIT NONE

! Declare passed variables
    INTEGER(i_kind)                       ,INTENT(in   ) :: nchanl
    REAL(r_kind),DIMENSION(nsig)          ,INTENT(in) :: h,q,prsl
    REAL(r_kind),DIMENSION(nsig+1)        ,INTENT(in) :: prsi
    CHARACTER(10)                         ,INTENT(in   ) :: obstype
    INTEGER(i_kind)                       ,INTENT(  out) :: error_status
    REAL(r_kind),DIMENSION(nsig,n_aerosols), INTENT(in) :: aeros
    REAL(r_kind),DIMENSION(nsig,nchanl)   ,INTENT(  out)  ,OPTIONAL :: layer_od

! Declare local parameters
    CHARACTER(len=*),PARAMETER::myname_=myname//'*call_crtm'
    REAL(r_kind),PARAMETER:: minsnow=one_tenth
    REAL(r_kind),PARAMETER:: qsmall  = 1.e-6_r_kind
    REAL(r_kind),PARAMETER:: ozsmall = 1.e-10_r_kind
    REAL(r_kind),PARAMETER:: jac_pert  = 1.0_r_kind
    REAL(r_kind),PARAMETER:: small_wind = 1.e-3_r_kind
    REAL(r_kind),PARAMETER:: windscale = 999999.0_r_kind
    REAL(r_kind),PARAMETER:: windlimit = 0.0001_r_kind
    REAL(r_kind),PARAMETER:: quadcof  (4, 2  ) =      &
         RESHAPE((/0.0_r_kind, 1.0_r_kind, 1.0_r_kind, 2.0_r_kind, 1.0_r_kind, &
         -1.0_r_kind, 1.0_r_kind, -1.0_r_kind/), (/4, 2/))

! Declare local variables  
    INTEGER(i_kind):: iquadrant  
    INTEGER(i_kind):: ier,ii,kk,kk2,i,itype,leap_day,day_of_year
    INTEGER(i_kind):: ig,istatus
    INTEGER(i_kind):: j,k,m1,ix,ix1,ixp,iy,iy1,iyp,m,iii
    INTEGER(i_kind):: itsig,itsigp,itsfc,itsfcp
    INTEGER(i_kind):: istyp00,istyp01,istyp10,istyp11
    INTEGER(i_kind):: iqs,iozs
    INTEGER(i_kind),DIMENSION(8)::obs_time,anal_time
    INTEGER(i_kind),DIMENSION(msig) :: klevel

! ****************************** 
! Constrained indexing for lai
! CRTM 2.1 implementation change
! ******************************
    INTEGER(i_kind):: lai_type


    REAL(r_kind):: delx,dely,delx1,dely1,dtsig,dtsigp,dtsfc,dtsfcp
    REAL(r_kind):: sst00,sst01,sst10,sst11,total_od,term,uu5,vv5, ps
    REAL(r_kind):: sno00,sno01,sno10,sno11,secant_term
    REAL(r_kind),DIMENSION(0:3):: wgtavg
    REAL(r_kind),DIMENSION(nsig,nchanl):: omix
    REAL(r_kind),DIMENSION(nsig,nchanl,n_aerosols_jac):: jaero
    REAL(r_kind),DIMENSION(nchanl) :: uwind_k,vwind_k
    REAL(r_kind),DIMENSION(msig+1) :: prsi_rtm
    REAL(r_kind),DIMENSION(msig)  :: prsl_rtm
    REAL(r_kind),DIMENSION(msig)  :: auxq,auxdp
    REAL(r_kind),DIMENSION(nsig)  :: poz
    REAL(r_kind),DIMENSION(nsig)  :: rh,qs,qclr
    REAL(r_kind),DIMENSION(5)     :: tmp_time
    REAL(r_kind),DIMENSION(0:3)   :: dtskin
    REAL(r_kind),DIMENSION(msig)  :: c6
    REAL(r_kind),DIMENSION(nsig)  :: c2,c3,c4,c5
    REAL(r_kind) cf
    REAL(r_kind),DIMENSION(nsig) :: ugkg_kgm2,cwj
    REAL(r_kind),ALLOCATABLE,DIMENSION(:,:) :: tgas1d
    REAL(r_kind),POINTER,DIMENSION(:,:,:)::qges_itsig =>NULL()
    REAL(r_kind),POINTER,DIMENSION(:,:,:)::qges_itsigp=>NULL()
    REAL(r_kind),POINTER,DIMENSION(:,:,:)::aeroges_itsig =>NULL()
    REAL(r_kind),POINTER,DIMENSION(:,:,:)::aeroges_itsigp=>NULL()

    LOGICAL :: sea,icmask,logaod

    INTEGER(i_kind),PARAMETER,DIMENSION(12):: mday=(/0,31,59,90,&
         120,151,181,212,243,273,304,334/)
    REAL(r_kind) ::   lai

    aero=aeros

    logaod=(INDEX(obstype,'aod') > 0)

    DO k=1,nsig
       IF(k == 1)THEN
!        Find delta Surface temperatures for all surface types

          DO i=1,nchanl
             rtsolution_k(i,1)%radiance = zero
             rtsolution_k(i,1)%brightness_temperature = one
          END DO

       END IF

       c2(k)=one/(one+fv*q(k))
       c3(k)=one/(one-q(k))
       c4(k)=fv*h(k)*c2(k)
       c5(k)=r1000*c3(k)*c3(k)

    END DO

! Interpolate level pressure to observation point for top interface

! Add additional crtm levels/layers to profile       

    CALL add_rtm_layers(prsi,prsl,prsi_rtm,prsl_rtm,klevel)

! Space-time interpolation of aerosol fields from sigma files

    DO k=1,nsig
       rh(k) = q(k)/qs(k)
    ENDDO


! Find tropopause height at observation

!  Zero atmosphere jacobian structures

    CALL crtm_atmosphere_zero(atmosphere_k(:,:))
    CALL crtm_surface_zero(surface_k(:,:))

    IF (n_aerosols>0) THEN
       DO k = 1, nsig
!       Convert mixing-ratio to concentration
          ugkg_kgm2(k)=1.0e-9_r_kind*(prsi(k)-prsi(k+1))*r1000/grav
          aero(k,:)=aero(k,:)*ugkg_kgm2(k)
       ENDDO
    ENDIF

    DO k = 1,msig

! Load profiles into extended RTM model layers

       kk = msig - k + 1
       atmosphere(1)%level_pressure(k) = r10*prsi_rtm(kk)
       atmosphere(1)%pressure(k)       = r10*prsl_rtm(kk)

       kk2 = klevel(kk)
       atmosphere(1)%temperature(k)    = h(kk2)
       atmosphere(1)%absorber(k,1)  = r1000*q(kk2)*c3(kk2)
       atmosphere(1)%absorber(k,2)  = O3_ID
       IF (n_aerosols>0) THEN
          aero_conc(k,:)=aero(kk2,:)
          auxrh(k)      =rh(kk2)
       ENDIF

! Include cloud guess profiles in mw radiance computation

    END DO

! Set aerosols for CRTM

    IF(n_aerosols>0) THEN
       CALL Set_CRTM_Aerosol ( msig, n_aerosols, n_aerosols_crtm, aero_names, aero_conc, auxrh, &
            atmosphere(1)%aerosol )
    ENDIF

! Call CRTM K Matrix model


    DO i=1,nchanl
       rtsolution_k(i,1)%layer_optical_depth(:) = jac_pert
    ENDDO

    error_status = 0
    error_status = crtm_aod_k(atmosphere,rtsolution_k,&
         channelinfo(sensorindex:sensorindex),rtsolution,atmosphere_k)

! If the CRTM returns an error flag, do not assimilate any channels for this ob
! and set the QC flag to 10 (done in setuprad).

    IF (error_status /=0) THEN
       WRITE(6,*)myname_,':  ***ERROR*** during crtm_k_matrix call ',&
            error_status
    END IF

! initialize intent(out) variables that are not available with modis_aod/viis_aod (logaod)
    jaero        = zero

    IF(PRESENT(layer_od)) layer_od = zero

    DO i=1,nchanl
       DO k=1,msig
          kk = klevel(msig-k+1)
          IF(PRESENT(layer_od)) THEN
             layer_od(kk,i) = layer_od(kk,i) + rtsolution(i,1)%layer_optical_depth(k)
          ENDIF
          DO ii=1,n_aerosols_jac
             IF ( n_aerosols_jac > n_aerosols_crtm .AND. ii == indx_p25 ) THEN
                jaero(kk,i,ii) = jaero(kk,i,ii) + &
                     (0.5_r_kind*(0.78_r_kind*atmosphere_k(i,1)%aerosol(indx_dust1)%concentration(k) + &
                     0.22_r_kind*atmosphere_k(i,1)%aerosol(indx_dust2)%concentration(k)) )
             ELSE
                jaero(kk,i,ii) = jaero(kk,i,ii) + atmosphere_k(i,1)%aerosol(ii)%concentration(k)
             ENDIF
          ENDDO
       ENDDO
    ENDDO
  END SUBROUTINE call_crtm


  SUBROUTINE destroy_crtm
    
    IMPLICIT NONE
    
    CHARACTER(len=20) :: myname='destroy_crtm'
    
    INTEGER(i_kind) error_status
    
    error_status = crtm_destroy(channelinfo)
    IF (error_status /= success) &
         WRITE(6,*)myname//':  ***ERROR*** error_status=',error_status
    CALL crtm_atmosphere_destroy(atmosphere(1))
    CALL crtm_surface_destroy(surface(1))
    CALL crtm_rtsolution_destroy(rtsolution)
    CALL crtm_rtsolution_destroy(rtsolution_k)
    CALL crtm_options_destroy(options)
    IF (crtm_atmosphere_associated(atmosphere(1))) &
         WRITE(6,*)myname//' ***ERROR** destroying atmosphere.'
    IF (crtm_surface_associated(surface(1))) &
         WRITE(6,*)myname//' ***ERROR** destroying surface.'
    IF (ANY(crtm_rtsolution_associated(rtsolution))) &
         WRITE(6,*)myname//' ***ERROR** destroying rtsolution.'
    IF (ANY(crtm_rtsolution_associated(rtsolution_k))) &
         WRITE(6,*)myname//' ***ERROR** destroying rtsolution_k.'
    IF (ANY(crtm_options_associated(options))) &
         WRITE(6,*)myname//' ***ERROR** destroying options.'
    DEALLOCATE(rtsolution,atmosphere_k,surface_k,rtsolution_k)
    IF(n_aerosols>0)THEN
       DEALLOCATE(aero_names)
       DEALLOCATE(aero,aero_conc,auxrh)
    ENDIF
    
    RETURN
  END SUBROUTINE destroy_crtm

END MODULE crtm_interface
