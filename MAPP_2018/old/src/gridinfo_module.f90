!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module gridinfo_module

   use constants_module
   use misc_definitions_module
   use module_debug
 
   ! Parameters
   integer, parameter :: MAX_DOMAINS = 1
 
   ! Variables
   integer :: iproj_type, n_domains, io_form_output, dyn_opt
   integer :: parent_grid_ratio, parent_id, ixdim, jydim
   real :: known_lat, known_lon, pole_lat, pole_lon, stand_lon, truelat1, truelat2, &
           known_x, known_y, dxkm, dykm, phi, lambda, ref_lat, ref_lon, ref_x, ref_y, &
           dlatdeg, dlondeg
   real :: parent_ll_x, parent_ll_y, parent_ur_x, parent_ur_y
   character (len=MAX_FILENAME_LEN) :: geog_data_path, opt_output_from_geogrid_path, opt_geogrid_tbl_path

   character (len=128) :: geog_data_res 
   character (len=1) :: gridtype
   logical :: do_tiled_output
   integer :: debug_level
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_grid_params
   !
   ! Purpose: This subroutine retrieves all parameters regarding the model domains
   !    to be processed by geogrid.exe. This includes map parameters, domain
   !    size and location, and nest information. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_grid_params()
 
      implicit none
  
      ! Local variables
      integer :: i, j, max_dom, funit, io_form_geogrid, interval_seconds
      real :: dx, dy, rparent_gridpts
      integer :: i_parent_start, j_parent_start, &
                           s_we, e_we, s_sn, e_sn, &
                           start_year, start_month, start_day, start_hour, start_minute, start_second, &
                           end_year,   end_month,   end_day,   end_hour,   end_minute,   end_second
      character (len=128) :: map_proj
      character (len=128) :: start_date, end_date
      character (len=3) :: wrf_core
      logical :: is_used

      namelist /share/ wrf_core, max_dom, start_date, end_date, &
                        start_year, end_year, start_month, end_month, &
                        start_day, end_day, start_hour, end_hour, &
                        start_minute, end_minute, start_second, end_second, &
                        interval_seconds, &
                        io_form_geogrid, opt_output_from_geogrid_path, debug_level
      namelist /geogrid/ parent_id, parent_grid_ratio, &
                         i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
                         map_proj, ref_x, ref_y, ref_lat, ref_lon, &
                         pole_lat, pole_lon, truelat1, truelat2, stand_lon, &
                         dx, dy, geog_data_res, geog_data_path, opt_geogrid_tbl_path
  
      ! Set defaults for namelist variables
      debug_level = 0
      io_form_geogrid = 2
      wrf_core = 'ARW'
      max_dom = 1
      geog_data_path = 'NOT_SPECIFIED'
      ref_x = NAN
      ref_y = NAN
      ref_lat = NAN
      ref_lon = NAN
      dx = NAN
      dy = NAN
      map_proj = 'Lambert'
      pole_lat = 90.0
      pole_lon = 0.0
      truelat1 = NAN
      truelat2 = NAN
      stand_lon = NAN
      do i=1,MAX_DOMAINS
         geog_data_res = 'default'
         parent_id = 1
         parent_grid_ratio = INVALID
         s_we = 1
         e_we = INVALID
         s_sn = 1
         e_sn = INVALID
         start_year = 0
         start_month = 0
         start_day = 0
         start_hour = 0
         start_minute = 0
         start_second = 0
         end_year = 0
         end_month = 0
         end_day = 0
         end_hour = 0
         end_minute = 0
         end_second = 0
         start_date = '0000-00-00_00:00:00'
         end_date = '0000-00-00_00:00:00'
      end do
      opt_output_from_geogrid_path = './'
      opt_geogrid_tbl_path = 'geogrid/'
      interval_seconds = INVALID
      
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
      read(funit,share)
      read(funit,geogrid)
      close(funit)

! BUG: More properly handle debug_level in module_debug
      if (debug_level.gt.100) then
         call set_debug_level(DEBUG)
      else
         call set_debug_level(WARN)
      end if

      call mprintf(.true.,LOGFILE,'Using the following namelist variables:')
      call mprintf(.true.,LOGFILE,'&SHARE')
      call mprintf(.true.,LOGFILE,'  WRF_CORE         = %s',s1=wrf_core)
      call mprintf(.true.,LOGFILE,'  MAX_DOM          = %i',i1=max_dom)
      call mprintf(.true.,LOGFILE,'  START_YEAR       = %i',i1=start_year)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_year)
      end do
      call mprintf(.true.,LOGFILE,'  START_MONTH      = %i',i1=start_month)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_month)
      end do
      call mprintf(.true.,LOGFILE,'  START_DAY        = %i',i1=start_day)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_day)
      end do
      call mprintf(.true.,LOGFILE,'  START_HOUR       = %i',i1=start_hour)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_hour)
      end do
      call mprintf(.true.,LOGFILE,'  START_MINUTE     = %i',i1=start_minute)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_minute)
      end do
      call mprintf(.true.,LOGFILE,'  START_SECOND     = %i',i1=start_second)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_second)
      end do
      call mprintf(.true.,LOGFILE,'  END_YEAR         = %i',i1=end_year)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_year)
      end do
      call mprintf(.true.,LOGFILE,'  END_MONTH        = %i',i1=end_month)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_month)
      end do
      call mprintf(.true.,LOGFILE,'  END_DAY          = %i',i1=end_day)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_day)
      end do
      call mprintf(.true.,LOGFILE,'  END_HOUR         = %i',i1=end_hour)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_hour)
      end do
      call mprintf(.true.,LOGFILE,'  END_MINUTE       = %i',i1=end_minute)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_minute)
      end do
      call mprintf(.true.,LOGFILE,'  END_SECOND       = %i',i1=end_second)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_second)
      end do
      call mprintf(.true.,LOGFILE,'  START_DATE       = %s',s1=start_date)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %s',s1=start_date)
      end do
      call mprintf(.true.,LOGFILE,'  END_DATE         = %s',s1=end_date)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %s',s1=end_date)
      end do
      call mprintf(.true.,LOGFILE,'  INTERVAL_SECONDS = %i',i1=interval_seconds)
      call mprintf(.true.,LOGFILE,'  IO_FORM_GEOGRID  = %i',i1=io_form_geogrid)
      call mprintf(.true.,LOGFILE,'  OPT_OUTPUT_FROM_GEOGRID_PATH = %s',s1=opt_output_from_geogrid_path)
      call mprintf(.true.,LOGFILE,'  DEBUG_LEVEL      = %i',i1=debug_level)
      call mprintf(.true.,LOGFILE,'/')

      call mprintf(.true.,LOGFILE,'&GEOGRID')
      call mprintf(.true.,LOGFILE,'  PARENT_ID         = %i',i1=parent_id)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=parent_id)
      end do
      call mprintf(.true.,LOGFILE,'  PARENT_GRID_RATIO = %i',i1=parent_grid_ratio)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=parent_grid_ratio)
      end do
      call mprintf(.true.,LOGFILE,'  I_PARENT_START    = %i',i1=i_parent_start)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=i_parent_start)
      end do
      call mprintf(.true.,LOGFILE,'  J_PARENT_START    = %i',i1=j_parent_start)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=j_parent_start)
      end do
      call mprintf(.true.,LOGFILE,'  S_WE              = %i',i1=s_we)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=s_we)
      end do
      call mprintf(.true.,LOGFILE,'  E_WE              = %i',i1=e_we)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=e_we)
      end do
      call mprintf(.true.,LOGFILE,'  S_SN              = %i',i1=s_sn)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=s_sn)
      end do
      call mprintf(.true.,LOGFILE,'  E_SN              = %i',i1=e_sn)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %i',i1=e_sn)
      end do
      call mprintf(.true.,LOGFILE,'  GEOG_DATA_RES     = %s',s1=geog_data_res)
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                    = %s',s1=geog_data_res)
      end do
      call mprintf(.true.,LOGFILE,'  DX                = %f',f1=dx)
      call mprintf(.true.,LOGFILE,'  DY                = %f',f1=dy)
      call mprintf(.true.,LOGFILE,'  MAP_PROJ          = %s',s1=map_proj)
      call mprintf(.true.,LOGFILE,'  POLE_LAT          = %f',f1=pole_lat)
      call mprintf(.true.,LOGFILE,'  POLE_LON          = %f',f1=pole_lon)
      call mprintf(.true.,LOGFILE,'  REF_LAT           = %f',f1=ref_lat)
      call mprintf(.true.,LOGFILE,'  REF_LON           = %f',f1=ref_lon)
      call mprintf(.true.,LOGFILE,'  REF_X             = %f',f1=ref_x)
      call mprintf(.true.,LOGFILE,'  REF_Y             = %f',f1=ref_y)
      call mprintf(.true.,LOGFILE,'  TRUELAT1          = %f',f1=truelat1)
      call mprintf(.true.,LOGFILE,'  TRUELAT2          = %f',f1=truelat2)
      call mprintf(.true.,LOGFILE,'  STAND_LON         = %f',f1=stand_lon)
      call mprintf(.true.,LOGFILE,'  GEOG_DATA_PATH    = %s',s1=geog_data_path)
      call mprintf(.true.,LOGFILE,'  OPT_GEOGRID_TBL_PATH = %s',s1=opt_geogrid_tbl_path)
      call mprintf(.true.,LOGFILE,'/')

      dxkm = dx
      dykm = dy

      known_lat = ref_lat
      known_lon = ref_lon
      known_x = ref_x
      known_y = ref_y

      ! Convert wrf_core to uppercase letters
      do i=1,3
         if (ichar(wrf_core(i:i)) >= 97) wrf_core(i:i) = char(ichar(wrf_core(i:i))-32)
      end do

      ! Before doing anything else, we must have a valid grid type 
      gridtype = ' '
      if (wrf_core == 'ARW') then
         gridtype = 'C'
         dyn_opt = 2
      else if (wrf_core == 'NMM') then
         gridtype = 'E'
         dyn_opt = 4
      end if

      ! Next, if this is NMM, we need to subtract 1 from the specified E_WE and E_SN;
      !    for some reason, these two variables need to be set to 1 larger than they 
      !    really ought to be in the WRF namelist, so, to be consistent, we will do 
      !    the same in the WPS namelist
      if (gridtype == 'E') then
         do i=1,max_dom
            e_we = e_we - 1 
            e_sn = e_sn - 1 
         end do 
      end if
  
      call mprintf(gridtype /= 'C' .and. gridtype /= 'E', ERROR, &
                   'A valid wrf_core must be specified in the namelist. '// &
                   'Currently, only "ARW" and "NMM" are supported.')

      call mprintf(max_dom > MAX_DOMAINS, ERROR, &
                   'In namelist, max_dom must be <= %i. To run with more'// &
                   ' than %i domains, increase the MAX_DOMAINS parameter.', &
                   i1=MAX_DOMAINS, i2=MAX_DOMAINS)

      ! Every domain must have a valid parent id
      do i=2,max_dom
         call mprintf(parent_id <= 0 .or. parent_id >= i, ERROR, &
                      'In namelist, the parent_id of domain %i must be in '// &
                      'the range 1 to %i.', i1=i, i2=i-1)
      end do
  
      ! Check for valid geog_data_path
      j=1
      do i=1,len(geog_data_path)
         geog_data_path(j:j) = geog_data_path(i:i)
         if (geog_data_path(i:i) /= ' ') j = j + 1
      end do
      if (geog_data_path(1:1) == ' ') then
         call mprintf(.true.,ERROR,'In namelist, geog_data_path must be specified.')
      end if
      j = len_trim(geog_data_path)
      if (j >= MAX_FILENAME_LEN) then
         call mprintf(.true.,ERROR, &
                      'In namelist, geog_data_path must be strictly less '// &
                      'than 128 characters in length.')
      else
         if (geog_data_path(j:j) /= '/') then
            geog_data_path(j+1:j+1) = '/'
         end if
      end if

      ! Paths need to end with a /
      j = len_trim(opt_geogrid_tbl_path)
      if (opt_geogrid_tbl_path(j:j) /= '/') then
         opt_geogrid_tbl_path(j+1:j+1) = '/'
      end if

      j = len_trim(opt_output_from_geogrid_path)
      if (opt_output_from_geogrid_path(j:j) /= '/') then
         opt_output_from_geogrid_path(j+1:j+1) = '/'
      end if
  
      ! Handle IOFORM+100 to do tiled IO
      if (io_form_geogrid > 100) then
         do_tiled_output = .true.
         io_form_geogrid = io_form_geogrid - 100
      else
         do_tiled_output = .false.
      end if
  
      ! Check for valid io_form_geogrid
      if ( &
           io_form_geogrid /= BINARY .AND. &
          .true. ) then
         call mprintf(.true.,WARN,'Valid io_form_geogrid values are:')
         call mprintf(.true.,WARN,'       %i (=BINARY)',i1=BINARY)
         call mprintf(.true.,ERROR,'No valid value for io_form_geogrid was specified in the namelist.')
      end if
      io_form_output = io_form_geogrid
  
      ! Convert map_proj to uppercase letters
      do i=1,len(map_proj)
         if (ichar(map_proj(i:i)) >= 97) map_proj(i:i) = char(ichar(map_proj(i:i))-32)
      end do
  
      ! Assign parameters to module variables
      if ((index(map_proj, 'LAMBERT') /= 0) .and. &
          (len_trim(map_proj) == len('LAMBERT'))) then
         iproj_type = PROJ_LC 
  
      else if ((index(map_proj, 'MERCATOR') /= 0) .and. &
               (len_trim(map_proj) == len('MERCATOR'))) then
         iproj_type = PROJ_MERC 
  
      else if ((index(map_proj, 'POLAR') /= 0) .and. &
               (len_trim(map_proj) == len('POLAR'))) then
         iproj_type = PROJ_PS 
  
      else if ((index(map_proj, 'ROTATED_LL') /= 0) .and. &
               (len_trim(map_proj) == len('ROTATED_LL'))) then
         iproj_type = PROJ_ROTLL 
  
      else if ((index(map_proj, 'LAT-LON') /= 0) .and. &
               (len_trim(map_proj) == len('LAT-LON'))) then
         iproj_type = PROJ_CASSINI 
  
      else
         call mprintf(.true.,ERROR,&
                      'In namelist, invalid map_proj specified. Valid '// &
                      'projections are "lambert", "mercator", "polar", '// &
                      '"lat-lon", and "rotated_ll".')
      end if

      ! For Cassini / lat-lon projections
      if (iproj_type == PROJ_CASSINI) then

         ! If no dx,dy specified, assume global grid
         if (dx == NAN .and. dy == NAN) then
            dlondeg = 360. / (e_we-s_we)   ! Here, we really do not want e_we-s_we+1
            dlatdeg = 180. / (e_sn-s_sn)   ! Here, we really do not want e_we-s_we+1
            known_x = 1.
            known_y = 1.
            known_lon = stand_lon + dlondeg/2.
            known_lat = -90. + dlatdeg/2.
            dxkm = EARTH_RADIUS_M * PI * 2.0 / (e_we-s_we)
            dykm = EARTH_RADIUS_M * PI       / (e_sn-s_sn)

         ! If dx,dy specified, however, assume regional grid
         else
            dlatdeg = dy
            dlondeg = dx
            dxkm = dlondeg * EARTH_RADIUS_M * PI * 2.0 / 360.0
            dykm = dlatdeg * EARTH_RADIUS_M * PI * 2.0 / 360.0
            if (known_lat == NAN .or. known_lon == NAN) then
               call mprintf(.true.,ERROR,'For lat-lon projection, if dx/dy are specified, '// &
                        'a regional domain is assumed, and a ref_lat,ref_lon must also be specified')
            end if
         end if
      end if

      ! Manually set truelat2 = truelat1 if truelat2 not specified for Lambert
      if (iproj_type == PROJ_LC .and. truelat2 == NAN) then
         call mprintf ((truelat1 == NAN), ERROR, "No TRUELAT1 specified for Lambert conformal projection.") 
         truelat2 = truelat1
      end if

  
      n_domains = max_dom
  
      ! For C grid, let ixdim and jydim be the number of mass points in 
      !    each direction; for E grid, we will put the row and column back
      !    later; maybe this should be changed to be more clear, though.
      do i=1,n_domains
         ixdim = e_we - s_we + 1
         jydim = e_sn - s_sn + 1
      end do
  
      if (gridtype == 'E') then
         phi = dykm*real(jydim-1)/2.
         lambda = dxkm*real(ixdim-1)
      end if

      ! If the user hasn't supplied a known_x and known_y, assume the center of domain 1
      if (gridtype == 'E' .and. (known_x /= NAN .or. known_y /= NAN)) then
         call mprintf(.true.,WARN, &
                      'Namelist variables ref_x and ref_y cannot be used for NMM grids.'// &
                      ' (ref_lat, ref_lon) will refer to the center of the coarse grid.')
      else if (gridtype == 'C') then
         if (known_x == NAN .and. known_y == NAN) then
            known_x = ixdim / 2.
            known_y = jydim / 2.
         else if (known_x == NAN .or. known_y == NAN) then
            call mprintf(.true.,ERROR, &
                      'In namelist.wps, neither or both of ref_x, ref_y must be specified.')
         end if 
      end if

      ! Checks specific to E grid
      if (gridtype == 'E') then
  
         ! E grid supports only the rotated lat/lon projection
         if (iproj_type /= PROJ_ROTLL) then
            call mprintf(.true., WARN, &
                         'For the NMM core, projection type must be rotated '// &
                         'lat/lon (map_proj=rotated_ll)')
            call mprintf(.true.,WARN,'Projection will be set to rotated_ll')
            iproj_type = PROJ_ROTLL
         end if
   
         ! In the following check, add back the 1 that we had to subtract above 
         !   for the sake of being consistent with WRF namelist
         call mprintf(mod(e_sn+1,2) /= 0, ERROR, &
                      'For the NMM core, E_SN must be an even number for grid %i.', i1=1)

         do i=2,n_domains
            call mprintf((parent_grid_ratio /= 3), WARN, &
                         'For the NMM core, the parent_grid_ratio must be 3 for '// &
                         'domain %i. A ratio of 3 will be assumed.', i1=i) 
            parent_grid_ratio = 3
         end do

         ! Check that nests have an acceptable number of grid points in each dimension
!         do i=2,n_domains
!            rparent_gridpts = real(ixdim+2)/real(parent_grid_ratio)
!            if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
!               call mprintf(.true.,ERROR,'For nest %i, e_we must be 3n-2 '// &
!                            'for some integer n > 1.', &
!                            i1=i)
!            end if
!            rparent_gridpts = real(jydim+2)/real(parent_grid_ratio)
!            if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
!               call mprintf(.true.,ERROR,'For nest %i, e_sn must be 3n-2 '// &
!                            'for some odd integer n > 1.', &
!                            i1=i)
!            end if
!         end do

         do i=2,n_domains
            parent_ll_x = 1.
            parent_ll_y = 1.
         end do
   
      ! Checks specific to C grid
      else if (gridtype == 'C') then
  
         ! C grid does not support the rotated lat/lon projection
         call mprintf((iproj_type == PROJ_ROTLL), ERROR, &
                      'Rotated lat/lon projection is not supported for the ARW core. '// &
                      'Valid projecitons are "lambert", "mercator", "polar", and "lat-lon".')

         ! Check that nests have an acceptable number of grid points in each dimension
         do i=2,n_domains
            rparent_gridpts = real(ixdim-1)/real(parent_grid_ratio)
            if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
               call mprintf(.true.,ERROR,'For nest %i, (e_we-s_we+1) must be one greater '// &
                            'than an interger multiple of the parent_grid_ratio of %i.', &
                            i1=i, i2=parent_grid_ratio)
            end if
            rparent_gridpts = real(jydim-1)/real(parent_grid_ratio)
            if (floor(rparent_gridpts) /= ceiling(rparent_gridpts)) then
               call mprintf(.true.,ERROR,'For nest %i, (e_sn-s_sn+1) must be one greater '// &
                            'than an interger multiple of the parent_grid_ratio of %i.', &
                            i1=i, i2=parent_grid_ratio)
            end if
         end do

         do i=1,n_domains
            parent_ll_x = real(i_parent_start)
            parent_ll_y = real(j_parent_start)
            parent_ur_x = real(i_parent_start)+real(ixdim)/real(parent_grid_ratio)-1.
            parent_ur_y = real(j_parent_start)+real(jydim)/real(parent_grid_ratio)-1.
         end do
  
      end if
  
      return
  
 1000 call mprintf(.true.,ERROR,'Error opening file namelist.wps')
 
   end subroutine get_grid_params
  
end module gridinfo_module
