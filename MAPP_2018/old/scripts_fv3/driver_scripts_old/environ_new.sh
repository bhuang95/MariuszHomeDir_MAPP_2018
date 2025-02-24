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

export FIXGLOBAL=/scratch4/BMC/fim/Keren.Rosado/fv3gfs_gf_judy/global_shared.v15.0.0/fix/fix_am

export BASEDIR=/scratch3/BMC/chem-var/pagowski/tmp

export OBS_DIR=${BASEDIR}/viirs_bufr_concat_sampled

export GFS_DIR=${BASEDIR}/indata_gfs_spectral

export FV3_BASEDIR=${BASEDIR}/fv3GFS_old
export FV3_BUILD=${FV3_BASEDIR}/sorc/fv3gfs.fd/BUILD
export FV3_EXECDIR=${FV3_BUILD}/bin
export FV3_EXEC=fv3_gfs_nh.prod.32bit.x
export FV3_DATA=${FV3_BASEDIR}/FV3_DATA
export FV3_FIXDIR=${FV3_DATA}/fix
export FV3_CLIM=${FV3_FIXDIR}/fix_am
export FV3_RUNS=${FV3_BASEDIR}/FV3_RUNS

export GSI_BASEDIR=${BASEDIR}/gsi_new
export GSI_EXECDIR=${GSI_BASEDIR}/src
export GSI_EXEC=global_gsi
export GSI_FIXDIR=${GSI_BASEDIR}/fix


export ENKF_BASEDIR=${GSI_EXECDIR}/enkf
export ENKF_EXECDIR=${ENKF_BASEDIR}
export ENKF_EXEC=global_enkf

export SCRIPTDIR=${HOME}/codes/fv3/driver_scripts

export SCRIPTDIR_UTIL=${HOME}/codes/fv3/util_scripts

export SCRIPTDIR_DA=${HOME}/codes/fv3/da_scripts

export LOGDIR=${BASEDIR}/logs

