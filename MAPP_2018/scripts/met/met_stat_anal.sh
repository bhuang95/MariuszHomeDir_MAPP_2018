#!/bin/ksh
set -x 


SDATE=2016060100
EDATE=2016060118
INC_H=6


cycle_hours=`seq 00 $INC_H 23`

echo $cycle_hours

list_cycle_hours=`printf " %02i," ${cycle_hours[*]}`

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
OBSVAR="DUSTTOTAL"
FCSTVAR="DUSTTOTAL"
LINETYPELIST="SL1L2"

WRKD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_statanal
NDATE=~/bin/ndate
CONFIG_DIR=$BASE/config
MAINCONF=$CONFIG_DIR/main.conf.IN
MASKS_DIR=$BASE/masks/nc_mask
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


echo "Stat_Analysis for $MODELNAME vs. $OBSNAME from $SDATE to $EDATE"

VALIDHLIST=$list_cycle_hours

for msk in $MSKLIST
do
  if [ -z "$AREA_MASK" ]; then
     AREA_MASK="${msk}"
  else
     AREA_MASK="$AREA_MASK,${msk}"
  fi
done

echo ${VALIDHLIST}

INCONFIG=${CONFIG_DIR}/StatAnalysis.conf.IN
cat $INCONFIG | sed s:_SDATE_:${SDATE}:g \
    | sed s:_EDATE_:${EDATE}:g \
    | sed s:_GRID_NAME_:${GRID_NAME}:g \
    | sed s:_MODELNAME_:${MODELNAME}:g \
    | sed s:_VALIDHLIST_:"${VALIDHLIST}":g \
    | sed s:_OBSNAME_:${OBSNAME}:g \
    | sed s:_INC_H_:${INC_H}H:g \
    | sed s:_FCSTVAR_:${FCSTVAR}:g \
    | sed s:_OBSVAR_:${OBSVAR}:g \
    | sed s:_AREA_MASK_:${AREA_MASK}:g \
    | sed s:_LINETYPELIST_:${LINETYPELIST}:g \
    > ./StatAnalysis.conf

$MASTER -c ./StatAnalysis.conf -c ./main.conf 
