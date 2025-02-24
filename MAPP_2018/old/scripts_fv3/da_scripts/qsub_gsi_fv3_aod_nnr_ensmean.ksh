#!/bin/ksh
# Set the queueing options 
#PBS -l procs=96
#PBS -l walltime=00:30:00
##PBS -A chem-var
#PBS -A wrf-chem
##PBS -A gsd-fv3-dev
#PBS -N gsi_aod_nnr
##PBS -q debug
##PBS -q urgent
#PBS -q batch
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/da_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080106 qsub_gsi_fv3_aod_nnr_ensmean.ksh


GSIPROC=$PBS_NP

export OMP_NUM_THREADS=4
export OMP_STACKSIZE=256M

sim=DA_GSI

charnanal='ensmean'
analdate=$ident

set -x

ndate=~/bin/ndate
nln="/bin/ln -fs"

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

. ./environ.sh

#
# GSIPROC = processor number used for GSI analysis
#------------------------------------------------

  ARCH='LINUX_PBS'

# Supported configurations:
            # IBM_LSF,
            # LINUX, LINUX_LSF, LINUX_PBS,
            # DARWIN_PGI
#
#####################################################
# case set up (users should change this part)
#####################################################
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working directory, where GSI runs
# PREPBURF = path of PreBUFR conventional obs
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 
  ANAL_TIME=${analdate}
  GSI_ROOT=${GSI_BASEDIR}
  WORK_ROOT=${MAINDIR}/tmp_dirs/workdir_gsi
#TBD

  analday=`echo ${analdate} | cut -c1-8`
  analtime=`echo ${analdate} | cut -c9-10`

  echo $analtime 

  analctime=`printf %02i $analtime`

  if [[ ! -r $LOG_DATE_DIR ]]
  then
      LOG_DATE_DIR=${LOGDIR}/${analdate}
      mkdir -p $LOG_DATE_DIR
  fi

  OBS_ROOT=${OBS_DIR}/nnr_bufr

  NNRBUFR=${OBS_ROOT}/Aqua_Terra_AOD_nnr_BUFR:${analday}${analctime}

  analdatem=`$ndate -$ANALINC $analdate`

  BK_ROOT=${FV3_RUNS}/${sim}/${analdatem}/ensmean/OUTPUT_FV3_nemsio

  CRTM_ROOT=${GSI_ROOT}/crtm_coeffs
  FIX_ROOT=${GSI_ROOT}/fix
  GSI_EXE=${GSI_EXEC}
  GSI_NAMELIST=${SCRIPTDIR_DA}/namelist_gsi_fv3_aod_nnr.sh

#------------------------------------------------
# bk_core= which WRF core is used as background (NMM or ARW)
# bkcv_option= which background error covariance and parameter will be used 
#              (GLOBAL or NAM or NMMB)
# if_clean = clean  : delete temperal files in working directory (default)
#            no     : leave running directory as is (this is for debug only)
#  bk_core=ARW
  bkcv_option=GLOBAL
  bk_core=GLOBAL
  if_clean=clean
# if_observer = Yes  : only used as observation operater for enkf
# no_member     number of ensemble members
# BK_FILE_mem   path and base for ensemble members
  if_observer=No
  diag_aero=.false.
#
#
#####################################################
# Users should NOT change script after this point
#####################################################
#
BYTE_ORDER=Big_Endian
# BYTE_ORDER=Little_Endian

case $ARCH in
   'IBM_LSF')
      ###### IBM LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX')
      if [ $GSIPROC = 1 ]; then
         #### Linux workstation - single processor
         RUN_COMMAND=""
      else
         ###### Linux workstation -  mpi run
        RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   'LINUX_LSF')
      ###### LINUX LSF (Load Sharing Facility)
      RUN_COMMAND="mpirun.lsf " ;;

   'LINUX_PBS')
      #### Linux cluster PBS (Portable Batch System)
      RUN_COMMAND="mpirun -np ${GSIPROC} " ;;

   'DARWIN_PGI')
      ### Mac - mpi run
      if [ $GSIPROC = 1 ]; then
         #### Mac workstation - single processor
         RUN_COMMAND=""
      else
         ###### Mac workstation -  mpi run
         RUN_COMMAND="mpirun -np ${GSIPROC} -machinefile ~/mach "
      fi ;;

   * )
     print "error: $ARCH is not a supported platform configuration."
     exit 1 ;;
esac


##################################################################################
# Check GSI needed environment variables are defined and exist
#
 
# Make sure ANAL_TIME is defined and in the correct format
if [ ! "${ANAL_TIME}" ]; then
  echo "ERROR: \$ANAL_TIME is not defined!"
  exit 1
fi

# Make sure WORK_ROOT is defined and exists
if [ ! "${WORK_ROOT}" ]; then
  echo "ERROR: \$WORK_ROOT is not defined!"
  exit 1
fi

# Make sure OBS_ROOT is defined and exists
if [ ! "${OBS_ROOT}" ]; then
  echo "ERROR: \$OBS_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${OBS_ROOT}" ]; then
  echo "ERROR: OBS_ROOT directory '${OBS_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the GSI static files
if [ ! "${FIX_ROOT}" ]; then
  echo "ERROR: \$FIX_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${FIX_ROOT}" ]; then
  echo "ERROR: fix directory '${FIX_ROOT}' does not exist!"
  exit 1
fi

# Set the path to the CRTM coefficients 
if [ ! "${CRTM_ROOT}" ]; then
  echo "ERROR: \$CRTM_ROOT is not defined!"
  exit 1
fi
if [ ! -d "${CRTM_ROOT}" ]; then
  echo "ERROR: fix directory '${CRTM_ROOT}' does not exist!"
  exit 1
fi


# Make sure the GSI executable exists
if [ ! -x "${GSI_EXECDIR}/${GSI_EXE}" ]; then
  echo "ERROR: ${GSI_EXE} does not exist!"
  exit 1
fi

# Check to make sure the number of processors for running GSI was specified
if [ -z "${GSIPROC}" ]; then
  echo "ERROR: The variable $GSIPROC must be set to contain the number of processors to run GSI"
  exit 1
fi

#
##################################################################################
# Create the ram work directory and cd into it

workdir=${WORK_ROOT}
echo " Create working directory:" ${workdir}

if [ -d "${workdir}" ]; then
  rm -rf ${workdir}
fi
mkdir -p ${workdir}
cd ${workdir}

#
##################################################################################

echo " Copy GSI executable, background file, and link observation bufr to working directory"

# Save a copy of the GSI executable in the workdir
/bin/cp ${GSI_EXECDIR}/${GSI_EXEC} .

# Bring over background field (it's modified by GSI so we can't link to it)
# Link to the prepbufr data

$nln ${NNRBUFR} ./nnrbufr


# Link to the radiance data
# ln -s ${OBS_ROOT}/gdas1.t06z.1bamua.tm00.bufr_d amsuabufr
# ln -s ${OBS_ROOT}/nam.t18z.1bamub.tm00.bufr_d amsubbufr
# ln -s ${OBS_ROOT}/nam.t18z.1bhrs3.tm00.bufr_d hirs3bufr
# ln -s ${OBS_ROOT}/gdas1.t06z.1bhrs4.tm00.bufr_d hirs4bufr
# ln -s ${OBS_ROOT}/nam.t18z.1bmhs.tm00.bufr_d mhsbufr
# ln -s ${OBS_ROOT}/nam.t18z.gpsro.tm00.bufr_d gpsrobufr
#
##################################################################################

echo " Copy fixed files and link CRTM coefficient files to working directory"

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

if [ ${bkcv_option} = GLOBAL ] ; then
  echo ' Use global background error covariance'
  OBERROR=${FIX_ROOT}/prepobs_errtable.global
  ((NLAT_ens=LATB_ens+2))
  BERROR=${FIX_ROOT}/${BYTE_ORDER}/global_berror.l${LEVS}y${NLAT_ens}_aero_gocart.f77
  ANAVINFO=${FIX_ROOT}/anavinfo_fv3_gocart
  AEROINFO=${FIX_ROOT}/aeroinfo_aod.txt
fi


SATANGL=${FIX_ROOT}/global_satangbias.txt
SATINFO=${FIX_ROOT}/global_satinfo.txt
CONVINFO=${FIX_ROOT}/global_convinfo.txt
OZINFO=${FIX_ROOT}/global_ozinfo.txt
PCPINFO=${FIX_ROOT}/global_pcpinfo.txt

#  copy Fixed fields to working directory
 cp $ANAVINFO anavinfo
 cp $BERROR   berror_stats
 cp $SATANGL  satbias_angle
 cp $SATINFO  satinfo
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable
 cp $AEROINFO aeroinfo
#
#    # CRTM Spectral and Transmittance coefficients
CRTM_ROOT_ORDER=${CRTM_ROOT}/${BYTE_ORDER}
emiscoef_IRwater=${CRTM_ROOT_ORDER}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${CRTM_ROOT_ORDER}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${CRTM_ROOT_ORDER}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${CRTM_ROOT_ORDER}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${CRTM_ROOT_ORDER}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${CRTM_ROOT_ORDER}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${CRTM_ROOT_ORDER}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${CRTM_ROOT_ORDER}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${CRTM_ROOT_ORDER}/FASTEM5.MWwater.EmisCoeff.bin
aercoef=${CRTM_ROOT_ORDER}/AerosolCoeff.bin
cldcoef=${CRTM_ROOT_ORDER}/CloudCoeff.bin

ln -s $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
ln -s $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -s $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -s $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -s $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -s $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -s $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -s $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -s $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
ln -s $aercoef  ./AerosolCoeff.bin
ln -s $cldcoef  ./CloudCoeff.bin
# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   ln -s ${CRTM_ROOT_ORDER}/${file}.SpcCoeff.bin ./
   ln -s ${CRTM_ROOT_ORDER}/${file}.TauCoeff.bin ./
done

for file in `awk '{if($1!~"!"){print $1}}' ./aeroinfo | sort | uniq` ;do
   ln -s ${CRTM_ROOT_ORDER}/${file}.SpcCoeff.bin ./
   ln -s ${CRTM_ROOT_ORDER}/${file}.TauCoeff.bin ./
done

# Only need this file for single obs test
 bufrtable=${FIX_ROOT}/prepobs_prep.bufrtable
 cp $bufrtable ./prepobs_prep.bufrtable
 

# for satellite bias correction
#cp ${FIX_ROOT}/sample.satbias ./satbias_in

#
##################################################################################
# Set some parameters for use by the GSI executable and to build the namelist
echo " Build the namelist "

# default is NAM
#   as_op='1.0,1.0,0.5 ,0.7,0.7,0.5,1.0,1.0,'

vs_op='1.,'
hzscl_op='.373,.746,1.5,'

if [ ${bkcv_option} = GLOBAL ] ; then
#   as_op='0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0'
   vs_op='0.7,'
   hzscl_op='1.7,0.8,0.5,'
fi
if [ ${bk_core} = NMMB ] ; then
   vs_op='0.6,'
fi

# default is NMM
   bk_core_arw='.false.'
   bk_core_nmm='.false.'
   bk_core_nmmb='.false.'
   bk_if_netcdf='.false.'
if [ ${bk_core} = ARW ] ; then
   bk_core_arw='.true.'
   bk_core_nmm='.false.'
   bk_core_nmmb='.false.'
   bk_if_netcdf='.true.'
fi
if [ ${bk_core} = NMMB ] ; then
   bk_core_arw='.false.'
   bk_core_nmm='.false.'
   bk_core_nmmb='.true.'
   bk_if_netcdf='.false.'
fi

if [ ${if_observer} = Yes ] ; then
  nummiter=0
  if_read_obs_save='.true.'
  if_read_obs_skip='.false.'
else
  nummiter=2
  if_read_obs_save='.false.'
  if_read_obs_skip='.false.'
fi

# Build the GSI namelist on-the-fly
. $GSI_NAMELIST
cat << EOF > gsiparm.anl

 $gsi_namelist

EOF

if [ ${bk_core} = GLOBAL ] ; then
    fh=$FHMIN
    while [[ $fh -le $FHMAX ]]
    do
	cfh=`printf %02i $fh`
	$nln ${BK_ROOT}/bfg_${analdate}_fhr${cfh} ./sfcf${cfh}
	$nln ${BK_ROOT}/sfg_${analdate}_fhr${cfh} ./sigf${cfh}
	((fh=fh+1))
    done
    
    $nln ${GFS_DIR}/sfcanl_${analdate}_${charnanal} ./sfcanl
fi

if [ -s ./sfcf03 ] && [ -s ./sfcf06 ] && [ -s ./sfcf09 ] && [ -s ./sigf03 ] && [ -s ./sigf06 ] && [ -s ./sigf09 ] ; then
    cat gsiparm.anl
    echo "${RUN_COMMAND} ${GSI_EXEC} > ${LOG_DATE_DIR}/gsi_ensmean.out 2>&1"
    ${RUN_COMMAND} $GSI_EXEC > ${LOG_DATE_DIR}/gsi_ensmean.out 2>&1
    rc=$?
    if [[ $rc -ne 0 ]];then
        echo "GSI failed with exit code $rc"
        exit $rc
    fi

    OUTDIR=${FV3_RUNS}/${sim}/${analdate}/ensmean/OUTPUT_GSI_nemsio

    if [[ ! -r ${OUTDIR} ]]
    then
	mkdir -p ${OUTDIR}
    fi

    /bin/cp ${LOG_DATE_DIR}/gsi_ensmean.out ${OUTDIR}

    /bin/mv siganl ${OUTDIR}/siganl_${analdate}

else
    echo "Input files missing"
    exit
fi

cd $SCRIPTDIR_UTIL

echo "qsub -v ident=${ident} qsub_calc_increment_nemsio_ensmean.sh"

qsub -v ident=${ident} qsub_calc_increment_nemsio_ensmean.sh

exit 0
