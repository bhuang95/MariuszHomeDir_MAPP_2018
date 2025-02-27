#!/bin/ksh

set -x 

SDATE=2016060100
EDATE=2016060200
INC_H=6

. /etc/profile
. ~/mapp_2018/.environ_met.ksh

BASE=~/mapp_2018/scripts/met

GRID_NAME="G003"

INPUTBASE="/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL"
OUTPUTBASE="${INPUTBASE}/metstats"

MODELNAME="fv3"
FCSTDIR=$INPUTBASE/${MODELNAME}/pll
FCSTINPUTTMP=${MODELNAME}"_aeros_{init?fmt=%Y%m%d%H}_pll.nc"

OBSNAME="cams"
#OBSNAME="m2"
OBSDIR=$INPUTBASE/${OBSNAME}/pll
OBSINPUTTMP=${OBSNAME}"_aeros_{init?fmt=%Y%m%d}_pll.nc"

MSKLIST="FULL TROP"

export OBS_VAR="DUSTTOTAL"
export FCST_VAR="DUSTTOTAL"  

LINETYPELIST="SL1L2"

WRKD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_gridstat
NDATE=~/bin/ndate
CONFIG_DIR=$BASE/config
MAINCONF=$CONFIG_DIR/main.conf.IN
MASKS_DIR=${INPUTBASE}/masks/nc_mask
PY_DIR=$BASE/python
MASTER=$METPLUS_PATH/ush/master_metplus.py

if [[ ! -r $OUTPUTBASE ]]
then
    mkdir -p $OUTPUTBASE
fi

if [[ ! -r $WRKD ]]
then
    mkdir -p $WRKD
fi

cd $WRKD

cat $MAINCONF | sed s:_MET_PATH_:${MET_PATH}:g \
              | sed s:_INPUTBASE_:${INPUTBASE}:g \
              | sed s:_OUTPUTBASE_:${OUTPUTBASE}:g \
              > ./main.conf

#
echo "Grid_Stat for $MODELNAME vs. $OBSNAME from $SDATE to $EDATE"

for msk in $MSKLIST
do
  if [[ $msk == "FULL" ]]; then
     continue
  else
     MSKFILE=${MASKS_DIR}/${msk}_MSK.nc
     if [ -z "${AREA_MASK}" ] ; then
        AREA_MASK="${MSKFILE}"
     else
        AREA_MASK="${AREA_MASK},${MSKFILE}"
     fi
  fi
done

INCONFIG=${CONFIG_DIR}/GridStat.conf.IN

cat $INCONFIG | sed s:_SDATE_:${SDATE}:g \
    | sed s:_EDATE_:${EDATE}:g \
    | sed s:_INC_H_:${INC_H}:g \
    | sed s:_BASE_:${BASE}:g \
    | sed s:_GRID_NAME_:${GRID_NAME}:g \
    | sed s:_MODELNAME_:${MODELNAME}:g \
    | sed s:_OBSNAME_:${OBSNAME}:g \
    | sed s:_FCSTDIR_:${FCSTDIR}:g \
    | sed s:_OBSDIR_:${OBSDIR}:g \
    | sed s:_FCSTINPUTTMP_:"${FCSTINPUTTMP}":g \
    | sed s:_OBSINPUTTMP_:"${OBSINPUTTMP}":g \
    | sed s:_AREA_MASK_:${AREA_MASK}:g \
    | sed s:_FTIME_:{valid?fmt=%Y%m%d_%H%M%S}:g \
    | sed s:_OTIME_:{valid?fmt=%Y%m%d_%H%M%S}:g \
    > ./GridStat.conf

$MASTER -c ./GridStat.conf -c ./main.conf 

