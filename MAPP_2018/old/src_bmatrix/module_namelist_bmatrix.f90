MODULE module_namelist_bmatrix

!&fv_core_nml begin

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: npx,npy,npz,layout,ntiles,nlayout,namelist_bmatrix_sub

  PUBLIC :: nxblocks, nyblocks
  
  CHARACTER(len=80) :: input_nml_file='input_fv3.nml'
  INTEGER :: nml_unit=101,ios

  LOGICAL :: use_logp, do_schmidt, fv_debug, fv_land, nudge,do_sat_adj,&
       &do_f3d,external_ic,nggps_ic,use_new_ncep,read_increment,&
       &ncep_ic,use_ncep_phy,fv_diag_ic,adjust_dry_mass,&
       &non_ortho,warm_start,mountain,convert_ke,use_old_omega,&
       &do_Held_Suarez,do_reed_physics,reed_cond_only,fill,&
       &filter_phys,fill_wz,fill_dp,range_warn,z_tracer,&
       &reproduce_sum,adiabatic,inline_q,consv_am,dwind_2d,do_vort_damp,&
       &hydrostatic,no_dycore,breed_vortex_inline,hybrid_z,Make_NH,&
       &reset_eta,remap_t,phys_hydrostatic,use_hydro_pressure,&
       &make_hybrid_z,nested,twowaynest,old_divg_damp,check_negative,&
       &nudge_ic,agrid_vel_rst,gocart_esrl,gfs_phil
  
  INTEGER, PARAMETER :: nlayout=2

  INTEGER :: npx,npy,ntiles, npz, npz_rst, ncnst, nwat, &
       &k_split, n_split, m_split,q_split,&
       &hord_mt,hord_vt, hord_tm, hord_dp, hord_tr,&
       &kord_mt, kord_wz, kord_tm, kord_tr,&
       &print_freq,n_sponge,nord,nord_tr,&
       &grid_type,nf_omega,fv_sg_adj,na_init,n_zs_filter,&
       &dnats,a2b_ord,c2l_ord,nord_zs_filter,pnats,&
       &parent_grid_num,parent_tile,nestbctype,nsponge,&
       &nestupdate,refinement,ioffset,joffset,halo_update_type

  INTEGER, DIMENSION(nlayout) :: layout,io_layout

  REAL :: a_imp,p_fac,shift_fac,stretch_fac,target_lat,target_lon,&
       &scale_z,w_max,z_min,beta,dddmp,d2_bg, d4_bg,d_ext,&
       &vtdm4,trdm2,ke_bg,d_con,dry_mass,consv_te,tau,tau_h2o,&
       &rf_cutoff,dx_const,dy_const,p_ref,d2_bg_k1,d2_bg_k2,deglat,&
       &deglon_start, deglon_stop, deglat_start, deglat_stop,&
       &add_noise,umax,s_weight

  CHARACTER(len=128) :: res_latlon_dynamics
  CHARACTER(len=128) :: res_latlon_tracers

  LOGICAL :: surface_debug, dycore_only, debug, sync

  INTEGER :: nxblocks, nyblocks

CONTAINS 

  SUBROUTINE namelist_bmatrix_sub

    NAMELIST /atmos_model_nml/ nxblocks, nyblocks, &
         &surface_debug, dycore_only,debug, sync

    NAMELIST /fv_core_nml/npx, npy, ntiles, npz, npz_rst, &
         &layout, io_layout, ncnst, nwat,  &
         &use_logp, p_fac, a_imp, k_split, n_split, m_split, q_split, &
         &print_freq, do_schmidt,      &
         &hord_mt, hord_vt, hord_tm, hord_dp, hord_tr, &
         &shift_fac, stretch_fac, target_lat, target_lon, &
         &kord_mt, kord_wz, kord_tm, kord_tr, &
         &fv_debug, fv_land, nudge, do_sat_adj, do_f3d, &
         &external_ic, read_increment, ncep_ic, nggps_ic, use_new_ncep, &
         &use_ncep_phy, fv_diag_ic, &
         &res_latlon_dynamics, res_latlon_tracers, &
         &scale_z, w_max, z_min, &
         &dddmp, d2_bg, d4_bg, vtdm4, trdm2, d_ext, &
         &beta, non_ortho, n_sponge, &
         &warm_start, adjust_dry_mass, mountain, d_con, ke_bg, &
         &nord, nord_tr, convert_ke, use_old_omega, &
         &dry_mass, grid_type, do_Held_Suarez, do_reed_physics, &
         &reed_cond_only, &
         &consv_te, fill, filter_phys, fill_dp, fill_wz, &
         &consv_am, range_warn, dwind_2d, inline_q, z_tracer, &
         &reproduce_sum, adiabatic, do_vort_damp, no_dycore,   &
         &tau, tau_h2o, rf_cutoff, nf_omega, hydrostatic, fv_sg_adj, &
         &breed_vortex_inline,  &
         &na_init, hybrid_z, Make_NH, n_zs_filter, nord_zs_filter, &
         &reset_eta,         &
         &pnats, dnats, a2b_ord, remap_t, p_ref, &
         &d2_bg_k1, d2_bg_k2,  &
         &c2l_ord, dx_const, dy_const, umax, deglat,      &
         &deglon_start, deglon_stop, deglat_start, deglat_stop, &
         &phys_hydrostatic, use_hydro_pressure, make_hybrid_z, &
         &old_divg_damp, add_noise, &
         &nested, twowaynest, parent_grid_num, parent_tile, &
         &refinement, nestbctype, nestupdate, nsponge, s_weight, &
         &ioffset, joffset, check_negative, nudge_ic, halo_update_type, &
         &gfs_phil, agrid_vel_rst,gocart_esrl

    
    OPEN(unit=nml_unit,file='input.nml')
    READ (nml_unit,atmos_model_nml,iostat=ios)
    READ (nml_unit,fv_core_nml,iostat=ios)
    CLOSE(nml_unit)

  END SUBROUTINE namelist_bmatrix_sub

END MODULE module_namelist_bmatrix
