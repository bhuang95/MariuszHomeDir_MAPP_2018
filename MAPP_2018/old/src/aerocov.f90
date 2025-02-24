PROGRAM aerocov

!eliminate pm2_5 from the list and modify num_aeros-1
!to calculate covariances between aerosols for GSI corerctions
!to ratios

!original location of aero_mod.f90 
!/scratch1/portfolios/BMC/chem-var/pagowski/WRFDA3.5/var/gen_be

!be_for_aero.nl from
!/scratch1/portfolios/BMC/chem-var/pagowski/gen_be_aero/gen_be/working

!aerosols is
!/scratch1/portfolios/BMC/chem-var/pagowski/WRFDA3.5/var/scripts/gen_be
!wrapper_gen_be_gsi.ksh

  USE aero_mod

  REAL,PARAMETER :: nh4_mfac=1.375,oc_mfac=1.8

  INTEGER              :: nx,ny,nz,ni,nj,nk,dim1,dim2,dim3,ncat
  INTEGER              :: nml_unit,ncases,ncount,member
  REAL                 :: lat_bins_in_deg,aero_fact,vartmp
  INTEGER              :: less_levels_from_top, debug
  REAL                 :: ds, angle,ncount_inv,min_xlat,rnxy,rncas
  logical              :: fstat

  CHARACTER(len=250),allocatable  :: filen(:)
  CHARACTER(len=250)   :: stage1_gsi_dir, filename
  character(len=10)    :: date
  character(len=3)     :: ce

  REAL, DIMENSION(:,:,:,:), ALLOCATABLE    :: aero
  REAL, DIMENSION(:,:,:), ALLOCATABLE    :: covar
  REAL, DIMENSION(:), ALLOCATABLE    :: varratio

  INTEGER              :: i,j,n, ierror, npes, mype,k,l,m
!
  NAMELIST /gen_be_stage2_gsi_nl/debug,stage1_gsi_dir,nx,ny,nz, fstat,&
       less_levels_from_top, lat_bins_in_deg

  INTEGER              :: num_aeros
  INTEGER, PARAMETER   :: num_aeros_max = 200
  CHARACTER (len=40)   :: aeros_to_process(1:num_aeros_max)
  LOGICAL              :: process_aero

  mype=0

  deg2rad=ATAN(1.)/45.
  rad2deg=1.0/deg2rad

  nml_unit=5
  OPEN(unit=nml_unit, file='gen_be_stage2_gsi_nl.nl',form='formatted', &
       &status='old', action='read')
  READ(nml_unit, gen_be_stage2_gsi_nl)
  CLOSE ( nml_unit)
  IF(mype==0) WRITE(6,nml= gen_be_stage2_gsi_nl)

  filename=TRIM(stage1_gsi_dir)//'/pert_files'

  OPEN(unit=10,file=TRIM(filename),form='formatted', &
       status='old', action='read')

  l=0
  DO WHILE(.TRUE.)
     READ(10,'(a)',END=100)
     l=l+1
  ENDDO

100 CONTINUE

  REWIND(10)

  ncases=l
  rncas=1./REAL(ncases)

  ALLOCATE(filen(1:ncases))
  DO l = 1, ncases
     READ(10,'(a)')filen(l)
  END DO
  CLOSE (10)

  CALL get_aero_info(process_aero,aeros_to_process,num_aeros)

  ALLOCATE(aero(1:num_aeros,1:nx,1:ny,1:nz),&
       &covar(1:num_aeros,1:num_aeros,1:nz),&
       &varratio(1:nz))

  IF (process_aero) THEN

     rnxy=1./REAL(nx*ny)

     covar=0.
     
     DO l=1,10 !ncases
        CALL read_wrf_arw_aero(TRIM(filen(l)),nx,ny,nz,&
             num_aeros,aero)
        PRINT *,l

        DO n=1,num_aeros
           DO m=n,num_aeros
              DO k=1,nz
                 DO j=1,ny
                    DO i=1,nx
                       covar(n,m,k)=covar(n,m,k)+&
                            &aero(n,i,j,k)*aero(m,i,j,k)
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO

           DO m=n+1,num_aeros
              covar(m,n,:)=covar(n,m,:)
           ENDDO

        END DO
        
     ENDDO
        
     rncas=1./10.
     
     covar=covar*rnxy*rncas
     
     DO k=1,nz
        
        vartmp=0.

        DO m=1,num_aeros-2
           
           SELECT CASE (TRIM(aeros_to_process(m)))
           CASE('sulf')
              aero_factor=nh4_mfac
           CASE('OC1','OC2')
              aero_factor=oc_mfac-1.
           CASE default
              aero_factor=1.
           END SELECT
           
           vartmp=vartmp+covar(m,m,k)*aero_factor

        ENDDO

        varratio(k)=vartmp

     ENDDO

     DO k=1,nz
        PRINT *,SQRT(varratio(k)/&
             &covar(num_aeros-1,num_aeros-1,k))
     ENDDO

     k=1
     DO n=1,num_aeros
        DO m=n+1,num_aeros
           PRINT *,TRIM(aeros_to_process(n)),'  ',&
                &TRIM(aeros_to_process(m)),&
                &covar(n,m,k)/SQRT(covar(n,n,k)*covar(m,m,k))
        ENDDO
     ENDDO
     
     STOP

  ELSE
     PRINT *,'No aeros to process - rerun gs_be - stopping'
     STOP
  ENDIF


END PROGRAM aerocov
