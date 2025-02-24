#!/bin/tcsh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS/qslogs
#PBS -d /scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS/scripts
#PBS -N c192test
##PBS -q batch
#PBS -q urgent
##PBS -q debug
#PBS -A chem-var
#PBS -l walltime=02:00:00
##PBS -l walltime=00:30:00
#PBS -l nodes=2:ppn=24
##PBS -l nodes=2:ppn=12 #Phil
##PBS -j oe #e and o outputs in the same file

source /etc/csh.login
source /apps/lmod/lmod/init/csh
module purge
module load intel
module load mvapich2/2.1a
module load netcdf
echo "MODULE LIST"

set echo

set BASEDIR    = "/scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS"
set INPUT_DATA = "/scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS/FV3_INPUT_DATA"
set BUILD_AREA = "/scratch3/BMC/chem-var/pagowski/tmp/fv3/fv3GFS/fv3_gfs_build"


# release number for the script
set RELEASE = "`cat ${BUILD_AREA}/release`"

# case specific details
set TYPE = "nh"         # choices:  nh, hydro
set MODE = "32bit"      # choices:  32bit, 64bit
set MONO = "non-mono"   # choices:  mono, non-mono
set CASE = "C192"
set NAME = "20150801.00Z"
set MEMO = ""
set HYPT = "off"        # choices:  on, off  (controls hyperthreading)
set COMP = "prod"       # choices:  debug, repro, prod

# directory structure
set WORKDIR    = ${BASEDIR}/workdir_C192
set executable = ${BUILD_AREA}/FV3GFS/BUILD/bin/fv3_gfs_${TYPE}.${COMP}.${MODE}.x


# prepare environment
source ${BUILD_AREA}/site/env.cshrc

# input filesets
set ICS  = ${INPUT_DATA}/${CASE}/${NAME}_IC/INPUT
set FIX  = ${INPUT_DATA}/fix
set GFS  = ${INPUT_DATA}/INPUT
set GRID = ${INPUT_DATA}/${CASE}/INPUT

# changeable parameters
    # dycore definitions
    set npx = "193"
    set npy = "193"
    set npz = "63"
    set layout_x = "2" 
    set layout_y = "2" 
    set io_layout = "1,1"
    set nthreads = "2"
    @ ncols = (${npx} - 1) * (${npy} - 1) * 6

    # blocking factor used for threading and general physics performance
    set nxblocks = "1"
    set nyblocks = "24"

    # run length
    set months = "0"
    set days = "2"
    set hours = "0"
    set dt_atmos = "450"

    # set the pre-conditioning of the solution
    # =0 implies no pre-conditioning
    # >0 means new adiabatic pre-conditioning
    # <0 means older adiabatic pre-conditioning
    set na_init = 0

    # variables for controlling initialization of NCEP/NGGPS ICs
    set filtered_terrain = ".true."
    set ncep_terrain = ".false."
    set ncep_plevels = ".false."
    set ncep_levs = "64"
    set gfs_dwinds = ".true."

    # variables for gfs diagnostic output intervals and time to zero out time-accumulated data
#    set fdiag = "6.,12.,18.,24.,30.,36.,42.,48.,54.,60.,66.,72.,78.,84.,90.,96.,102.,108.,114.,120.,126.,132.,138.,144.,150.,156.,162.,168.,174.,180.,186.,192.,198.,204.,210.,216.,222.,228.,234.,240."
    set fdiag = "6."
    set fhzer = "6."
    set fhcyc = "0."
#if set fhcyc > 0 then need grib files in FIX dir

    # determines whether FV3 or GFS physics calculate geopotential
    set gfs_phil = ".false."

    # determine whether ozone production occurs in GFS physics
    set ozcalc = ".true."

    # set various debug options
    set no_dycore = ".false."
    set dycore_only = ".false."
    set print_freq = "6"

    if (${TYPE} == "nh") then
      # non-hydrostatic options
      set make_nh = ".T."
      set hydrostatic = ".F."
      set phys_hydrostatic = ".F."     # can be tested
      set use_hydro_pressure = ".F."   # can be tested
      set consv_te = "1."
        # time step parameters in FV3
      set k_split = "1"
      set n_split = "10"
    else
      # hydrostatic options
      set make_nh = ".F."
      set hydrostatic = ".T."
      set phys_hydrostatic = ".F."     # will be ignored in hydro mode
      set use_hydro_pressure = ".T."   # have to be .T. in hydro mode
      set consv_te = "0."
        # time step parameters in FV3
      set k_split = "1"
      set n_split = "10"
    endif

    if (${MONO} == "mono" || ${MONO} == "monotonic") then
      # monotonic options
      set d_con = "0."
      set do_vort_damp = ".false."
      if (${TYPE} == "nh") then
        # non-hydrostatic
        set hord_mt = " 10"
        set hord_xx = "-10"
      else
        # hydrostatic
        set hord_mt = " 10"
        set hord_xx = "-10"
      endif
    else
      # non-monotonic options
      set d_con = "1."
      set do_vort_damp = ".true."
      if (${TYPE} == "nh") then
        # non-hydrostatic
        set hord_mt = " 6"
        set hord_xx = "-5"
      else
        # hydrostatic
        set hord_mt = " 10"
        set hord_xx = "-10"
      endif
    endif

    if (${MONO} == "non-mono" && ${TYPE} == "nh" ) then
      set vtdm4 = "0.04"
    else
      set vtdm4 = "0.04"
    endif

    # variables for hyperthreading
    set cores_per_node = "24"
    if (${HYPT} == "on") then
      set hyperthread = ".true."
      set j_opt = "-j2"
    else
      set hyperthread = ".false."
      set j_opt = "-j1"
    endif

# when running with threads, need to use the following command
    @ npes = ${layout_x} * ${layout_y} * 6
    setenv MPICH_ENV_DISPLAY
    setenv MPICH_MPIIO_CB_ALIGN 2

#    set run_cmd = "mpirun -n $npes -d $nthreads $j_opt ./$executable:t"
#    set nprocs = "$PBS_NP"
#    set run_cmd = "mpirun -np $nprocs ./$executable:t"
    set run_cmd = "mpirun -n $npes ./$executable:t"


    setenv MALLOC_MMAP_MAX_ 0
    setenv MALLOC_TRIM_THRESHOLD_ 536870912
    setenv NC_BLKSZ 1M
# necessary for OpenMP when using Intel
    setenv KMP_STACKSIZE 256m

\rm -rf $WORKDIR/rundir

mkdir -p $WORKDIR/rundir
cd $WORKDIR/rundir

mkdir -p RESTART

# build the date for curr_date and diag_table from NAME
unset echo
set y = `echo ${NAME} | cut -c1-4`
set m = `echo ${NAME} | cut -c5-6`
set d = `echo ${NAME} | cut -c7-8`
set h = `echo ${NAME} | cut -c10-11`
set echo
set curr_date = "${y},${m},${d},${h},0,0"

# build the diag_table with the experiment name and date stamp
cat > diag_table << EOF
${NAME}.${CASE}.${MODE}.${MONO}
$y $m $d $h 0 0 
EOF
cat ${BUILD_AREA}/FV3GFS/RUN/RETRO/diag_table >> diag_table

# copy over the other tables and executable
cp ${BUILD_AREA}/FV3GFS/RUN/RETRO/data_table data_table
cp ${BUILD_AREA}/FV3GFS/RUN/RETRO/field_table field_table
cp $executable .

# GFS standard input data
#tar xf ${GFS}
mkdir INPUT
ln -s ${GFS}/* ./INPUT


# Grid and orography data
#tar xf ${GRID}
ln -s ${GRID}/* ./INPUT

# Date specific ICs
#tar xf ${ICS}
ln -s ${ICS}/* ./INPUT



cat > input.nml <<EOF
 &amip_interp_nml
     interp_oi_sst = .true.
     use_ncep_sst = .true.
     use_ncep_ice = .false.
     no_anom_sst = .false.
     data_set = 'reynolds_oi',
     date_out_of_range = 'climo',
/

 &atmos_model_nml
     nxblocks = $nxblocks
     nyblocks = $nyblocks
     surface_debug = .false.
     dycore_only = $dycore_only
/

 &diag_manager_nml
     prepend_date = .true.
     long_date = .true.
/

 &fms_io_nml
       checksum_required   = .false.
       max_files_r = 100,
       max_files_w = 100,
/

 &fms_nml
       clock_grain = 'ROUTINE',
       domains_stack_size = 115200,
       print_memory_usage = .false.
/

 &fv_grid_nml
       grid_file = 'INPUT/grid_spec.nc'
/

 &fv_core_nml
       layout   = $layout_x,$layout_y
       io_layout = $io_layout
       npx      = $npx
       npy      = $npy
       ntiles   = 6,
       npz    = $npz
       grid_type = -1
       make_nh = $make_nh
       fv_debug = .F.
       range_warn = .F.
       reset_eta = .F.
       n_sponge = 4
       tau = 5.
       rf_cutoff = 8.e2
       d2_bg_k1 = 0.16
       d2_bg_k2 = 0.02
       kord_tm = -11
       kord_mt =  10
       kord_wz =  10
       kord_tr =  11
       hydrostatic = $hydrostatic
       phys_hydrostatic = $phys_hydrostatic
       use_hydro_pressure = $use_hydro_pressure
       beta = 0.
       a_imp = 1.
       p_fac = 0.1
       k_split  = $k_split
       n_split  = $n_split
       nwat = 2 
       na_init = $na_init
       d_ext = 0.0
       dnats = 0
       fv_sg_adj = 900
       d2_bg = 0.
       nord =  2
       dddmp = 0.0
       d4_bg = 0.15 
       vtdm4 = $vtdm4
       ke_bg = 0.
       do_vort_damp = $do_vort_damp
       external_ic = .F.
       gfs_phil = $gfs_phil
       nggps_ic = .F.
       mountain = .T.
       ncep_ic = .F.
       d_con = $d_con
       hord_mt = $hord_mt
       hord_vt = $hord_xx
       hord_tm = $hord_xx
       hord_dp = $hord_xx
       hord_tr = -8
       adjust_dry_mass = .F.
       consv_te = $consv_te
       consv_am = .F.
       fill = .T.
       dwind_2d = .F.
       print_freq = $print_freq
       warm_start = .T.
       no_dycore = $no_dycore
       z_tracer = .T.
/

 &coupler_nml
       months = $months
       days  = $days
       hours = $hours
       dt_atmos = $dt_atmos
       dt_ocean = $dt_atmos
       current_date =  $curr_date
       calendar = 'julian'
       memuse_verbose = .false.
       atmos_nthreads = $nthreads
       use_hyper_thread = $hyperthread
       ncores_per_node = $cores_per_node
/

 &external_ic_nml 
       filtered_terrain = $filtered_terrain
!       ncep_terrain = $ncep_terrain
       ncep_plevels = $ncep_plevels
       levp = $ncep_levs
       gfs_dwinds = $gfs_dwinds
       checker_tr = .F.
       nt_checker = 0
/

 &gfs_physics_nml
       debug = .false.
       ntoz = 3
       ntcw = 2
       fhswr = 3600.
       fhlwr = 3600.
       ozcalc = $ozcalc
       nocnv = .false.
       fdiag = $fdiag
       fhzero = $fhzer
       prslrd0 = 0.
       fhcyc = $fhcyc
       use_ufo = .F.
       nst_anl = .F.
       ncols = $ncols
       cdmbgwd=0.125,3.0  ! changed for DA resolution at direction of Jeff Whitaker
/

  &interpolator_nml
       interp_method = 'conserve_great_circle'
/

&namsfc
       FNGLAC = "$FIX/global_glacier.2x2.grb"
       FNMXIC = "$FIX/global_maxice.2x2.grb"
       FNTSFC = "$FIX/cfs_oi2sst1x1monclim19822001.grb"
       FNSNOC = "$FIX/global_snoclim.1.875.grb"
       FNZORC = "$FIX/global_zorclim.1x1.grb"
       FNALBC = "$FIX/global_albedo4.1x1.grb"
       FNAISC = "$FIX/cfs_ice1x1monclim19822001.grb"
       FNTG3C = "$FIX/global_tg3clim.2.6x1.5.grb"
       FNVEGC = "$FIX/global_vegfrac.0.144.decpercent.grb"
       FNVETC = "$FIX/global_vegtype.1x1.grb"
       FNSOTC = "$FIX/global_soiltype.1x1.grb"
       FNSMCC = "$FIX/global_soilmcpc.1x1.grb"
       FNMSKH = "$FIX/seaice_newland.grb"
       FNTSFA = " "
       FNACNA = " "
       FNSNOA = " "
       FNVMNC = "$FIX/global_shdmin.0.144x0.144.grb"
       FNVMXC = "$FIX/global_shdmax.0.144x0.144.grb"
       FNSLPC = "$FIX/global_slope.1x1.grb"
       FNABSC = "$FIX/global_snoalb.1x1.grb"
       LDEBUG=.false.,
       FSMCL(2) = 99999
       FSMCL(3) = 99999
       FSMCL(4) = 99999
       FTSFS = 90
       FAISS = 99999
       FSNOL = 99999
       FSICL = 99999
       FTSFL=99999,
       FAISL=99999,
       FVETL=99999,
       FSOTL=99999,
       FvmnL=99999,
       FvmxL=99999,
       FSLPL=99999,
       FABSL=99999,
       FSNOS=99999,
       FSICS=99999,
/
EOF

# run the executable
   ${run_cmd} | tee fms.out
   if ( $? != 0 ) then
	exit
   endif
