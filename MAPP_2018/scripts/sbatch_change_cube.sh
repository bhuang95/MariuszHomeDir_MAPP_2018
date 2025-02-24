#!/bin/ksh
#SBATCH -n 6
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J cube_res
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_change_cube.sh

. /apps/lmod/lmod/init/ksh

export HOMEufs=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/UFS_UTILS

module purge

. ${HOMEufs}/modulefiles/build.hera.intel


export INPUT_TYPE=restart

CRES_in=384
CRES_out=96

export CDATE=2021052500

yyyymmdd=`echo $CDATE | cut -c1-8`
hh=`echo $CDATE | cut -c9-10`0000

INDIR_RESTART=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/nrt/RESTART/inputs
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_change_cube

export COMIN=$INDIR_RESTART

FIXorog=${HOMEufs}/fix/fix_fv3_gmted2010

export MOSAIC_FILE_INPUT_GRID=${FIXorog}/C${CRES_in}/C${CRES_in}_mosaic.nc
export OROG_DIR_INPUT_GRID=${FIXorog}/C${CRES_in}

OROG_FILES_INPUT_GRID='C'${CRES_in}_oro_data.tile1.nc'","'C${CRES_in}_oro_data.tile2.nc'","'C${CRES_in}_oro_data.tile3.nc'","'C${CRES_in}_oro_data.tile4.nc'","'C${CRES_in}_oro_data.tile5.nc'","'C${CRES_in}_oro_data.tile6.nc

export OROG_FILES_INPUT_GRID

export ATM_CORE_FILES_INPUT=${yyyymmdd}.${hh}.fv_core.res.tile1.nc'","'${yyyymmdd}.${hh}.fv_core.res.tile2.nc'","'${yyyymmdd}.${hh}.fv_core.res.tile3.nc'","'${yyyymmdd}.${hh}.fv_core.res.tile4.nc'","'${yyyymmdd}.${hh}.fv_core.res.tile5.nc'","'${yyyymmdd}.${hh}.fv_core.res.tile6.nc'","'${yyyymmdd}.${hh}.fv_core.res.nc

export ATM_TRACER_FILES_INPUT=${yyyymmdd}.${hh}.fv_tracer.res.tile1.nc'","'${yyyymmdd}.${hh}.fv_tracer.res.tile2.nc'","'${yyyymmdd}.${hh}.fv_tracer.res.tile3.nc'","'${yyyymmdd}.${hh}.fv_tracer.res.tile4.nc'","'${yyyymmdd}.${hh}.fv_tracer.res.tile5.nc'","'${yyyymmdd}.${hh}.fv_tracer.res.tile6.nc

export TRACERS_INPUT='"sphum","liq_wat","rainwat","ice_wat","snowwat","graupel","o3mr","so2"'
export TRACERS_TARGET='"sphum","liq_wat","rainwat","ice_wat","snowwat","graupel","o3mr","so2"'

export CONVERT_ATM=.true.
export CONVERT_SFC=.false.
export CONVERT_NST=.false.

export DATA=$TMPDIR

export CRES=${CRES_out}

export APRUN='srun --export=ALL'

. ${HOMEufs}/ush/chgres_cube.sh
