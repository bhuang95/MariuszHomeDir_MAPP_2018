#!/bin/ksh

ulimit -s unlimited
export KMP_AFFINITY=disabled

export FHMIN=3
export FHMAX=9
export FHOUT=3
export ANALINC=6
export LEVS=63
export SMOOTHINF=35

export JCAP_ens=382
export LONB_ens=768
export LATB_ens=384
export RES=192

export NANALS=20

export MAINDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS

export BASEDIR=${MAINDIR}/SOURCES/global_shared.v15.0.0

export FIXGLOBAL=/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix/fix_am

export OBS_DIR=${MAINDIR}/OBS/viirs_bufr_concat_sampled

export GFS_DIR=${MAINDIR}/GFS/indata_spectral
export GFS_DIR_ENS=${MAINDIR}/GFS/indata_spectral_ensemble

export FV3_BASEDIR=${MAINDIR}/../fv3GFS_old/sorc/fv3gfs.fd
export FV3_EXECDIR=${FV3_BASEDIR}/BUILD/bin
export FV3_EXEC=fv3_gfs_nh.prod.32bit.x

#export FV3_BASEDIR=${MAINDIR}/fv3_phil/trunk/FV3
#export FV3_EXECDIR=${FV3_BASEDIR}
#export FV3_EXEC=fv3.exe

export FV3_DATA=${MAINDIR}/FV3_DATA
export FV3_FIXDIR=${MAINDIR}/FV3_FIX
export FV3_CLIM=${MAINDIR}/FV3_CLIM
export FV3_RUNS=${MAINDIR}/FV3_RUNS

export GSI_BASEDIR=${MAINDIR}/GSI
export GSI_EXECDIR=${GSI_BASEDIR}/src
export GSI_EXEC=global_gsi
export GSI_FIXDIR=${GSI_BASEDIR}/fix

export ENKF_BASEDIR=${GSI_EXECDIR}/enkf
export ENKF_EXECDIR=${ENKF_BASEDIR}
export ENKF_EXEC=global_enkf

export SCRIPTDIR_DRIVER=${HOME}/codes/scripts_fv3/driver_scripts

export SCRIPTDIR_DRIVER_ENSEMBLE=${HOME}/codes/scripts_fv3/driver_scripts_ensemble

export SCRIPTDIR_UTIL=${HOME}/codes/scripts_fv3/util_scripts

export SCRIPTDIR_DA=${HOME}/codes/scripts_fv3/da_scripts

export LOGDIR=${MAINDIR}/LOGS

