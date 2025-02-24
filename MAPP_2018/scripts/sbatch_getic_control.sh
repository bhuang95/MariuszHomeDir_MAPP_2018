#!/bin/sh
#---- Hera JOBCARD
#---- Submit as: sbatch $script
#SBATCH -J getic
#SBATCH -A gsd-fv3-dev
#SBATCH --open-mode=truncate
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j
#SBATCH --nodes=2
#SBATCH --nodes=2
#SBATCH -q debug
#SBATCH -t 0:30:00
export machine=HERA

#modified for control only from 
# /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/workflows/fv3gfs_chgres.sh

set -x

export PSLOT=fv3test
export CDUMP=gdas
export CASE_HIGH=C192
export CDATE=2015122000

export HOMEgfs=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/workflows/ccpp-chem
export PTMP=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES/tmp
mkdir -p ${PTMP}
export zero_bias_dir=/scratch1/NCEPDEV/global/glopara/bias_zero


#-------------------------------------------------------------------------------------------------
# Perform simple check to ensure user has HOMEgfs set correctly and has linked the 'fix'
# directories.
#-------------------------------------------------------------------------------------------------

if [ ! -d ${HOMEgfs}/fix/fix_fv3/$CASE_HIGH ]; then
  set +x
  echo FATAL ERROR: CANT FIND FIX DIRECTORIES. 
  echo ENSURE HOMEgfs PATH IS CORRECT.
  echo YOUR HOMEgfs SETTING IS: $HOMEgfs
  echo ALSO ENSURE sorc/link_fixdirs.sh WAS RUN.
  exit 1
fi

export NSTSMTH=NO                                  ##apply 9-point smoothing to nsst tref
export NST_TF_CHG=$HOMEgfs/exec/nst_tf_chg.x
export ZERO_BIAS=YES                                ##zeroed out all bias and radsat files 


export ymd=`echo $CDATE | cut -c 1-8`
export cyc=`echo $CDATE | cut -c 9-10`
export yy=`echo $CDATE | cut -c 1-4`
export mm=`echo $CDATE | cut -c 5-6`
export dd=`echo $CDATE | cut -c 7-8`

export ROTDIR=$PTMP/$PSLOT
export RUNDIR=$ROTDIR/chgres

export NODES=1
export APRUNC=""

. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status


export APRUNC=time
export APRUNTF=time
export OMP_NUM_THREADS_CH=24
export SUB=$HOMEgfs/util/sub_slurm
export ACCOUNT=gsd-fv3-dev
export QUEUE=batch
export QUEUE_TRANS=batch

#----------------------------
#----------------------------
#--for high-res
#----------------------------
#----------------------------

export CASE=$CASE_HIGH
export INIDIR=$RUNDIR/$CDUMP/$CASE
export COMROT=$ROTDIR/${CDUMP}.$ymd/$cyc
export OUTDIR=$COMROT/INPUT
export DATA=$INIDIR/stmp
rm -rf $INIDIR $OUTDIR $DATA
mkdir -p $INIDIR $OUTDIR  $DATA
cd $INIDIR ||exit 8

#--------------------------------------------------------------------------
# See if requested data is in the ops directory.  If not, pull from hpss.
#--------------------------------------------------------------------------

if [ -s $COMROOT/gfs/prod/${CDUMP}.${ymd}/$cyc ]; then

   if [ $CDUMP = gdas ]; then
     set +x
     echo GDAS CYCLE NOT SUPPORTED.
     echo USE NEW PARALLEL VERSION OF CHGRES.
     echo CONTACT REPOSITORY MANAGER FOR DETAILS.
     exit
   fi
   atm=./${CDUMP}.t${cyc}z.atmanl.nemsio
   sfc=./${CDUMP}.t${cyc}z.sfcanl.nemsio
   nst=$sfc  # for fv3gfs, nst fields stored in surface file
   for ff in $atm $sfc ; do
     cp $COMROOT/gfs/prod/${CDUMP}.${ymd}/$cyc/$ff .
   done

else   ##get data from HPSS archive

  if [ $CDATE -lt 2014050100 ]; then

    HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
    nst=" "
    if [ $CDUMP = gdas ]; then
      tarball_high=com_gfs_prod_${CDUMP}.${CDATE}.tar
      atm=./gdas1.t${cyc}z.sanl
      sfc=./gdas1.t${cyc}z.sfcanl
      biascr=./gdas1.t${cyc}z.abias
      biascr_pc=" "
      radstat=./gdas1.t${cyc}z.radstat
      aircraft_t_bias=" "
    else
      tarball_high=com_gfs_prod_${CDUMP}.${CDATE}.anl.tar
      atm=./gfs.t${cyc}z.sanl
      sfc=./gfs.t${cyc}z.sfcanl
    fi

  elif [ $CDATE -le 2017072012 ]; then
    if [ $CDATE -ge 2016110100 ]; then
      oldexp=prnemsrn
    elif [ $CDATE -ge 2016050100 ]; then
      oldexp=pr4rn_1605
    elif [ $CDATE -ge 2015121500 ]; then
      oldexp=pr4rn_1512
    elif [ $CDATE -ge 2015050200 ]; then
      oldexp=pr4rn_1505
    elif [ $CDATE -ge 2014073000 ]; then
      oldexp=pr4rn_1408
    elif [ $CDATE -ge 2014050100 ]; then
      oldexp=pr4rn_1405
    else
      echo "NEMS GSM retro ICs do not exit, exit"
      exit 1
    fi

    HPSSPATH=/5year/NCEPDEV/emc-global/emc.glopara/WCOSS_C/$oldexp    ##use q3fy17 nems gfs parallel ics
    tarball_high=${CDATE}${CDUMP}.tar
    atm=gfnanl.${CDUMP}.$CDATE
    sfc=sfnanl.${CDUMP}.$CDATE
    nst=nsnanl.${CDUMP}.$CDATE
    biascr=biascr.${CDUMP}.$CDATE
    biascr_pc=biascr_pc.${CDUMP}.$CDATE
    aircraft_t_bias=aircraft_t_bias.${CDUMP}.$CDATE
    radstat=radstat.${CDUMP}.$CDATE
  elif [[ $CDATE -le 2019061118 ]]; then
    HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
    if [ $CDUMP = gfs ]; then
      tarball_high=gpfs_hps_nco_ops_com_gfs_prod_${CDUMP}.${CDATE}.anl.tar
    else
      tarball_high=gpfs_hps_nco_ops_com_gfs_prod_${CDUMP}.${CDATE}.tar
    fi
    atm=./${CDUMP}.t${cyc}z.atmanl.nemsio
    sfc=./${CDUMP}.t${cyc}z.sfcanl.nemsio
    nst=./${CDUMP}.t${cyc}z.nstanl.nemsio
    biascr=./${CDUMP}.t${cyc}z.abias 
    biascr_pc=./${CDUMP}.t${cyc}z.abias_pc
    aircraft_t_bias=./${CDUMP}.t${cyc}z.abias_air  
    radstat=./${CDUMP}.t${cyc}z.radstat
  else  # FV3GFS starting 2019061200
    HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd
    if [ $CDUMP = gfs ]; then
      tarball_high=gpfs_dell1_nco_ops_com_gfs_prod_${CDUMP}.${yy}${mm}${dd}_${cyc}.${CDUMP}_nemsioa.tar
    else
      set +x
      echo GDAS CYCLE NOT SUPPORTED.
      echo USE NEW PARALLEL VERSION OF CHGRES.
      echo CONTACT REPOSITORY MANAGER FOR DETAILS.
      exit
    fi
    atm=./${CDUMP}.$yy$mm$dd/$cyc/${CDUMP}.t${cyc}z.atmanl.nemsio
    sfc=./${CDUMP}.$yy$mm$dd/$cyc/${CDUMP}.t${cyc}z.sfcanl.nemsio
  fi

#--extract ICs from hpss
cat > read_hpss.sh <<EOF1
   
   . $HOMEgfs/ush/load_fv3gfs_modules.sh   2>>/dev/null

   cd $INIDIR
   htar -xvf  $HPSSPATH/$tarball_high $atm $sfc $nst  
   if [[ $CDATE -ge 2019061200 ]]; then
     ln -fs $atm .
     ln -fs $sfc .
   fi
   if [ $CDUMP = gdas ]; then
      cd $COMROT 
      htar -xvf  $HPSSPATH/$tarball_high $biascr $biascr_pc $aircraft_t_bias $radstat

      [[ '$biascr' = ./gdas1.t${cyc}z.abias ]] && mv $biascr ./gdas.t${cyc}z.abias
      [[ '$radstat' = ./gdas1.t${cyc}z.radstat ]] && mv $radstat ./gdas.t${cyc}z.radstat

      [[ '$biascr' = biascr.${CDUMP}.$CDATE ]] && mv biascr.${CDUMP}.$CDATE  ${CDUMP}.t${cyc}z.abias 
      [[ '${biascr_pc}' = biascr_pc.${CDUMP}.$CDATE ]] &&  mv biascr_pc.${CDUMP}.$CDATE ${CDUMP}.t${cyc}z.abias_pc
      [[ '$aircraft_t_bias' = aircraft_t_bias.${CDUMP}.$CDATE ]] && mv aircraft_t_bias.${CDUMP}.$CDATE ${CDUMP}.t${cyc}z.abias_air
      [[ '$radstat' = radstat.${CDUMP}.$CDATE ]] &&  mv radstat.${CDUMP}.$CDATE  ${CDUMP}.t${cyc}z.radstat

   fi
EOF1
chmod u+x read_hpss.sh
$SUB -a $ACCOUNT -q $QUEUE_TRANS -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out $INIDIR/read_hpss.sh

#................................................
fi
#................................................

testfile=$INIDIR/$sfc                 
nsleep=0; tsleep=120;  msleep=50
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done
sleep 300

if [ ! -s $testfile ]; then 
  echo "$testfile does not exist, exit !"
  exit 1
fi

[[ $atm = ./gdas1.t${cyc}z.sanl ]] && mv $INIDIR/$atm $INIDIR/gdas.t${cyc}z.sanl
[[ $sfc = ./gdas1.t${cyc}z.sfcanl ]] && mv $INIDIR/$sfc $INIDIR/gdas.t${cyc}z.sfcanl

#------------------------------
if [ $NSTSMTH = "YES" ]; then
#------------------------------
mv $nst fnsti
rm -f tf_chg_parm.input
cat >tf_chg_parm.input <<EOF1
&config
nsmth=3,istyp=0,
/
EOF1
$APRUNTF $NST_TF_CHG <tf_chg_parm.input 
mv fnsto $nst
if [ $? -ne 0 ] ; then
  echo "NST_TF_CHG for $CDUMP $CASE failed. exit"
  exit 1
fi
#------------------------------
fi
#------------------------------

$HOMEgfs/ush/global_chgres_driver.sh
if [ $? -ne 0 ] ; then
 echo "chgres for $CDUMP $CASE failed. exit"
 exit 1
fi

if [ $CDUMP = "gdas" -a $ZERO_BIAS = "YES" ]; then
   cd $COMROT
   for ff in abias abias_air abias_pc radstat; do
    mv gdas.t${cyc}z.${ff}  gdas.t${cyc}z.${ff}_save
    cp -p ${zero_bias_dir}/gdas.t18z.${ff} gdas.t${cyc}z.${ff}
   done
fi 

[[ $CDUMP = gfs ]] && exit

exit
