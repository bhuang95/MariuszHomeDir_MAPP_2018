MODULE module_obs_thinning 

!to thin data for time window, distance, and negative values

  USE datetime_mod
  USE timedelta_mod

  USE m_unirnk

  USE kd_tree, ONLY: init_kd_tree, close_kd_tree, knn_search_ts, knn_search  

  USE module_misc, ONLY: max_name_length,unit_namelist,pi,d2r,small_value
  USE module_aod_nnr, ONLY: aod_nnr
  USE module_fv3, ONLY: read_fv3_grid, ntiles, ctiles

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: obs_thinning

CONTAINS

  SUBROUTINE obs_thinning(fcstdate,aod_nnr_record,nobs,aod_nnr_thin_record,nobs_thin)

    TYPE(datetime_type), INTENT(in) :: fcstdate
    TYPE(aod_nnr), DIMENSION(nobs), INTENT(in) :: aod_nnr_record
    INTEGER, INTENT(in) :: nobs

    TYPE(aod_nnr), DIMENSION(:), ALLOCATABLE, INTENT(out) :: aod_nnr_thin_record
    INTEGER, INTENT(out) :: nobs_thin

    CHARACTER(len = max_name_length) :: input_dir_grid_thinning
    REAL :: thinning_grid_ratio_min, thinning_grid_ratio_max
    INTEGER :: time_half_window
    LOGICAL :: thin_spatial
    TYPE(timedelta_type) :: dt_window,dt
    INTEGER :: nobs_twindow


    CHARACTER(len = max_name_length) :: input_file
    REAL, ALLOCATABLE, DIMENSION(:,:) :: griddata
    CHARACTER(len = max_name_length), DIMENSION(ntiles) :: grid_files
    INTEGER :: stderr
    INTEGER :: Open_Status,Read_Status
    INTEGER :: i,j,k,nx,nxy,nchan_nnr

    INTEGER :: num_nn,num_nn_found
    INTEGER, DIMENSION(:), ALLOCATABLE :: nn
    REAL, DIMENSION(3) :: hp1,hp2
    REAL :: dphi_max
    REAL, DIMENSION(:), ALLOCATABLE :: min_d
    INTEGER, DIMENSION(:), ALLOCATABLE :: obsgrid,obsgrid_unique

    REAL, ALLOCATABLE, DIMENSION(:,:) ::  grid1, grid2
    TYPE(aod_nnr), DIMENSION(:), ALLOCATABLE :: aod_nnr_tmp_record

    REAL :: start,finish

    NAMELIST /record_obs_thinning/ input_dir_grid_thinning, &
         &thinning_grid_ratio_min, thinning_grid_ratio_max,&
         &time_half_window,thin_spatial
    
    CONTINUE

    stderr = 0

    OPEN(unit=unit_namelist, file = "namelist.modis_nnr_correlation_4hl", &
         &status="old",action = "READ",iostat=open_status)
    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no NAMELIST file (NAMELIST.modis_nnr_correlation)"
       STOP
    END IF
    WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation_4hl record_obs_thinning'
    READ (unit_namelist, NML=record_obs_thinning, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

!screen if within time window
!calculate first nobs_twindow

    nchan_nnr=SIZE(aod_nnr_record(1)%channels)
    dt_window=timedelta(minutes=time_half_window)

    ALLOCATE(aod_nnr_tmp_record(nobs))

    j=0

    DO i=1,nobs
       dt=aod_nnr_record(i)%obsdate-fcstdate

       IF ( ANY(aod_nnr_record(i)%values(:) < small_value) ) CYCLE
 
       IF ( ABS(dt%total_minutes()) < dt_window%total_minutes() ) THEN

          j=j+1

          IF (ALLOCATED(aod_nnr_tmp_record(j)%channels)) &
               &DEALLOCATE(aod_nnr_tmp_record(j)%channels)
          
          IF (ALLOCATED(aod_nnr_tmp_record(j)%values)) &
               &DEALLOCATE(aod_nnr_tmp_record(j)%values)

             ALLOCATE(aod_nnr_tmp_record(j)%channels(nchan_nnr),&
                  &aod_nnr_tmp_record(j)%values(nchan_nnr))
          
          aod_nnr_tmp_record(j)%satellite=aod_nnr_record(i)%satellite
          aod_nnr_tmp_record(j)%obstype=aod_nnr_record(i)%obstype
          aod_nnr_tmp_record(j)%channels(:)=aod_nnr_record(i)%channels(:)
          aod_nnr_tmp_record(j)%values(:)=aod_nnr_record(i)%values(:)
          aod_nnr_tmp_record(j)%lat=aod_nnr_record(i)%lat
          aod_nnr_tmp_record(j)%lon=aod_nnr_record(i)%lon
          aod_nnr_tmp_record(j)%obsdate=aod_nnr_record(i)%obsdate
       ENDIF

    ENDDO

    nobs_twindow=j

    PRINT *,'There are ',nobs_twindow,' observations within half-time window ',time_half_window,' minutes'

    IF (nobs_twindow == 0) THEN
       PRINT *,'Model not matching observation times - Stopping'
       STOP
    ENDIF

    grid_files=TRIM(input_dir_grid_thinning)//'/'//"grid_spec.tile"//ctiles//".nc"

    CALL read_fv3_grid(griddata,grid_files)

    nxy=SIZE(griddata,2)
    nx=SQRT(REAL(nxy)/REAL(ntiles))

    ALLOCATE(grid1(nobs_twindow,2),grid2(nxy,2))

    grid1(:,1)=aod_nnr_tmp_record(:nobs_twindow)%lat*d2r
    grid1(:,2)=aod_nnr_tmp_record(:nobs_twindow)%lon*d2r
    WHERE ( (grid1(:,2) > pi) ) grid1(:,2)=grid1(:,2)-2.*pi

    grid2(:,1)=griddata(1,:)*d2r
    grid2(:,2)=griddata(2,:)*d2r
    WHERE ( (grid2(:,2) > pi) ) grid2(:,2)=grid2(:,2)-2.*pi

!    PRINT *,MINVAL(grid1(:,1)),MAXVAL(grid1(:,1))
!    PRINT *,MINVAL(grid1(:,2)),MAXVAL(grid1(:,2))

!    PRINT *,MINVAL(grid2(:,1)),MAXVAL(grid2(:,1))
!    PRINT *,MINVAL(grid2(:,2)),MAXVAL(grid2(:,2))

!    PRINT *,nobs_twindow 

    IF (.NOT. thin_spatial) THEN
       nobs_thin=nobs_twindow    
    ELSE

       dphi_max=2.*pi/(4.*REAL(nx))*thinning_grid_ratio_max

       hp1=0.
       hp2=0.
!number of closest neighbors
       num_nn=1

       CALL init_kd_tree(grid1, nobs_twindow, num_nn)    

       ALLOCATE(nn(num_nn),min_d(num_nn),obsgrid(nxy))

       obsgrid=0

       CALL CPU_TIME(start)

       j=0

!$OMP PARALLEL DO DEFAULT (NONE) &
!$OMP SHARED (grid2,dphi_max,hp1,hp2,obsgrid,num_nn,nxy,j) &
!$OMP PRIVATE (nn,min_d,num_nn_found,i) 

       DO i=1,nxy
!       CALL knn_search(grid2(i,:),nn,min_d,hp1,hp2,1.0,num_nn_found)
          CALL knn_search_ts(grid2(i,1:2),nn,min_d,hp1,hp2,1.0,num_nn,num_nn_found)
          IF ( num_nn_found > 0 .AND. MINVAL(min_d) < dphi_max ) THEN
             
!!$OMP ATOMIC UPDATE          
!$OMP CRITICAL
             j=j+1
             obsgrid(j)=nn(1)
!$OMP END CRITICAL

          ENDIF
       ENDDO
       
       DO i=1,j
          WRITE(105,*)obsgrid(i)
       ENDDO
       
       CALL CPU_TIME(finish)
       PRINT *,'Time spent in kd_search = ',finish-start
       
       CALL close_kd_tree()
       
       PRINT *,j
       
       ALLOCATE(obsgrid_unique(j))
       
       PRINT *,'@@1'
       
       CALL unirnk(obsgrid(1:j),obsgrid_unique,nobs_thin)
       
       DO i=1,nobs_thin
          WRITE(106,*)obsgrid(obsgrid_unique(i))
       ENDDO
       
       PRINT *,'@@2',nobs_thin
       
       STOP

    ENDIF

    ALLOCATE(aod_nnr_thin_record(nobs_thin))
 
    DO i=1,nobs_thin

       IF (ALLOCATED(aod_nnr_thin_record(i)%channels)) &
            &DEALLOCATE(aod_nnr_thin_record(i)%channels)

       IF (ALLOCATED(aod_nnr_thin_record(i)%values)) &
            &DEALLOCATE(aod_nnr_thin_record(i)%values)

       ALLOCATE(aod_nnr_thin_record(i)%channels(nchan_nnr),&
            &aod_nnr_thin_record(i)%values(nchan_nnr))
       
       IF (thin_spatial) THEN
          j=obsgrid(obsgrid_unique(i))          
       ELSE
          j=i
       ENDIF

       aod_nnr_thin_record(i)%satellite=aod_nnr_tmp_record(j)%satellite
       aod_nnr_thin_record(i)%obstype=aod_nnr_tmp_record(j)%obstype
       aod_nnr_thin_record(i)%channels(:)=aod_nnr_tmp_record(j)%channels(:)
       aod_nnr_thin_record(i)%values(:)=aod_nnr_tmp_record(j)%values(:)
       aod_nnr_thin_record(i)%lat=aod_nnr_tmp_record(j)%lat
       aod_nnr_thin_record(i)%lon=aod_nnr_tmp_record(j)%lon
       aod_nnr_thin_record(i)%obsdate=aod_nnr_tmp_record(j)%obsdate

    ENDDO

    DO i=1,nobs_twindow
       DEALLOCATE(aod_nnr_tmp_record(i)%channels,aod_nnr_tmp_record(i)%values)
    ENDDO

    DEALLOCATE(aod_nnr_tmp_record)

    DEALLOCATE(griddata,grid1,grid2)
    IF (ALLOCATED(obsgrid)) DEALLOCATE(obsgrid)
    IF (ALLOCATED(obsgrid_unique)) DEALLOCATE(obsgrid_unique)

    PRINT *,'Retained ',nobs_thin,' observations out of ',nobs

  END SUBROUTINE obs_thinning
  
END MODULE module_obs_thinning
