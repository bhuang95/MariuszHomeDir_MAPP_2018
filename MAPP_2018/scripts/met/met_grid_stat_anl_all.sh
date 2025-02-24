#!/bin/ksh

set -x 

SDATE=2016060100
EDATE=2016060100
INC_H=6

. /etc/profile
. ~/mapp_2018/.environ_met.ksh

ident=$SDATE

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hr=`echo "${ident}" | cut -c9-10`

BASE=~/mapp_2018/scripts/met

GRID_NAME="G002"

MODELNAME="CAMS"
OBSNAME="M2"
MSKLIST="FULL TROP"

OBSVAR="DUSTTOTAL"
FCSTVAR="DUSTTOTAL"  

LINETYPELIST="SL1L2"

INPUTBASE="/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL"
OUTPUTBASE="/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/metstats"

FCSTDIR=$INPUTBASE/cams/pll
FCSTINPUTTMP="cams_aeros_${year}${month}${day}_sdtotals.nc"

OBSDIR=$INPUTBASE/m2/pll
OBSINPUTTMP="m2_aeros_${year}${month}${day}_pll.nc"

WRKD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/gridstat
NDATE=~/bin/ndate
CONFIG_DIR=$BASE/config
MAINCONF=$CONFIG_DIR/main.conf.IN
MASKS_DIR=${INPUTBASE}/masks/nc_mask
PY_DIR=$BASE/python
MASTER=$METPLUS_PATH/ush/master_metplus.py

if [[ ! -r $OUTPUT_BASE ]]
then
    mkdir -p $OUTPUT_BASE
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
echo "Step1: Grid_Stat for $MODELNAME vs. $OBSNAME from $SDATE to $EDATE"
for msk in $MSKLIST
do
  echo $msk
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
              | sed s:_FCSTVAR_:${FCSTVAR}:g \
              | sed s:_OBSVAR_:${OBSVAR}:g \
              | sed s:_FCSTDIR_:${FCSTDIR}:g \
              | sed s:_OBSDIR_:${OBSDIR}:g \
              | sed s:_FCSTINPUTTMP_:"${FCSTINPUTTMP}":g \
              | sed s:_OBSINPUTTMP_:"${OBSINPUTTMP}":g \
              | sed s:_AREA_MASK_:${AREA_MASK}:g \
              > ./GridStat.conf

$MASTER -c ./main.conf -c ./GridStat.conf


exit

#
# Stat_Analysis
#
case $STP2 in
Y|y|yes)
echo "Step2: Stat_Analysis for $MODELNAME vs. $OBSNAME from $SDATE to $EDATE"

# determine valid time list for stat_analysis
tmpedate=`$NDATE $SDATE 24`
CDATE=$SDATE
while [ $CDATE -lt $tmpedate ];
do
  HH=`echo $CDATE | cut -c9-10`
  if [ -z "$VALIDHLIST" ]; then
     VALIDHLIST="$HH"
  else
     VALIDHLIST="$VALIDHLIST, $HH"
  fi
  CDATE=`$NDATE $CDATE $INC_H`
done

for msk in $MSKLIST
do
  if [ -z "$AREAMASKNAME" ]; then
     AREAMASKNAME="${msk}"
  else
     AREAMASKNAME="$AREAMASKNAME,${msk}"
  fi
done

INCONFIG=${CONFIG_DIR}/StatAnalysis.conf.IN
cat $INCONFIG | sed s:_SDATE_:${SDATE}:g \
              | sed s:_EDATE_:${EDATE}:g \
              | sed s:_GRID_NAME_:${GRID_NAME}:g \
              | sed s:_MODELNAME_:${MODELNAME}:g \
              | sed s:_OBSNAME_:${OBSNAME}:g \
              | sed s:_VALIDHLIST_:"${VALIDHLIST}":g \
              | sed s:_FCSTVAR_:${FCSTVAR}:g \
              | sed s:_OBSVAR_:${OBSVAR}:g \
              | sed s:_AREAMASKNAME_:"${AREAMASKNAME}":g \
              | sed s:_LINETYPELIST_:${LINETYPELIST}:g \
              > ./StatAnalysis.conf

$MASTER -c ./main.conf -c ./StatAnalysis.conf
;;
*)
  echo "Skip Step2: Stat_Analysis"
;;
esac
#
# Plot vertical profile and time series
#
case $STP3 in
Y|y|yes)
OUTPUT_BASE=`grep OUTPUT_BASE ./main.conf | awk '{print $3}'`
echo $OUTPUT_BASE
TMPSAPATH=`grep "STAT_ANALYSIS_OUTPUT_DIR =" $CONFIG_DIR/StatAnalysis.conf.IN | awk '{print $3}' | sed -e 's/{OUTPUT_BASE}/${OUTPUT_BASE}/g'`
SAPATH=`eval echo $TMPSAPATH`
echo $SAPATH

if [ ! -s $OUTPUT_BASE/stat_images ]; then
   mkdir $OUTPUT_BASE/stat_images
fi

for LINETYPE in $LINETYPELIST
do
for AREAMSK in $MSKLIST
do

INPYSCRIPT=${PY_DIR}/plt_grid_stat_anl.py.IN
cat $INPYSCRIPT | sed s:_BASE_:${BASE}:g \
                | sed s:_SDATE_:${SDATE}:g \
                | sed s:_EDATE_:${EDATE}:g \
                | sed s:_INC_H_:${INC_H}:g \
                | sed s:_SAPATH_:${SAPATH}:g \
                | sed s:_MODELNAME_:${MODELNAME}:g \
                | sed s:_OBSNAME_:${OBSNAME}:g \
                | sed s:_AREAMSK_:${AREAMSK}:g \
                | sed s:_OBSVAR_:${OBSVAR}:g \
                | sed s:_LINETYPE_:${LINETYPE}:g \
                > ./plt_grid_stat_anl.py

if [ -s ./plt_grid_stat_anl.py ]; then
   python ./plt_grid_stat_anl.py > ./plt_grid_stat_anl.out 2>&1
   if [[ $? == 0 ]] ; then
      mv ./*.png $OUTPUT_BASE/stat_images
   fi
fi
done
done
;;
*)
  echo "Skip Step3: Generate Plots of "
;;
esac
