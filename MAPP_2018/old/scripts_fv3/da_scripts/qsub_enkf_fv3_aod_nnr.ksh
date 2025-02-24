#!/bin/ksh
# Set the queueing options
##PBS -l procs=96
#PBS -l procs=24
#PBS -l walltime=00:30:00
#PBS -A wrf-chem
##PBS -A chem-var
##PBS -A gsd-fv3-dev
#PBS -N enkf_aod_nnr
##PBS -q debug
##PBS -q urgent
#PBS -q bigmem
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/da_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080106 qsub_enkf_fv3_aod_nnr.ksh

. ./environ.sh

nanals=$NANALS

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

set -x

analdate=$ident

export enkf_threads=1
export cores_enkf=$nanals
export mpitaskspernode=1
export OMP_NUM_THREADS=$enkf_threads
export OMP_STACKSIZE=256M
export nprocs=$PBS_NP

ndate=~/bin/ndate

analdatem=`$ndate -$ANALINC $analdate`

workdir=${MAINDIR}/tmp_dirs/workdir_enkf

/bin/rm -rf $workdir
mkdir -p $workdir

. ./enkf_nml.sh

sim=DA_ENKF

INDIR_BCKG=${FV3_RUNS}/${sim}/${analdatem}
INDIR_DIAG=${FV3_RUNS}/${sim}/${analdate}

cd $workdir

charnanal=ensmean

ln -sf ${INDIR_DIAG}/${charnanal}/OUTPUT_GSI_nemsio/diag_*_ges.${analdate}_${charnanal} .

indir_bckg=${INDIR_BCKG}/${charnanal}/OUTPUT_FV3_nemsio

bckgfile=${indir_bckg}/sfg_${analdate}_fhr06
sfcbckgfile=${indir_bckg}/bfg_${analdate}_fhr06
if [[ ! -s $bckgfile || ! -s $sfcbckgfile ]]
then
    echo "missing input analysis file `basename $bckgfile`  or `basename $sfcbckgfile`"  
    exit 1
fi

ln -sf $bckgfile ./sfg_${analdate}_fhr06_${charnanal}
ln -sf $sfcbckgfile ./bfg_${analdate}_fhr06_${charnanal}

nanal=1
while [[ $nanal -le $nanals ]]
do
    charnanal="mem"`printf %03i $nanal`

    indir_bckg=${INDIR_BCKG}/${charnanal}/OUTPUT_FV3_nemsio

    bckgfile=${indir_bckg}/sfg_${analdate}_fhr06
    sfcbckgfile=${indir_bckg}/bfg_${analdate}_fhr06
    if [[ ! -s $bckgfile || ! -s $sfcbckgfile ]]
    then
	echo "missing input analysis file `basename $bckgfile`  or `basename $sfcbckgfile`"  
	exit 1
    fi
    
    ln -sf $bckgfile ./sfg_${analdate}_fhr06_${charnanal}
    ln -sf $sfcbckgfile ./bfg_${analdate}_fhr06_${charnanal}

    ln -sf ${INDIR_DIAG}/${charnanal}/OUTPUT_GSI_nemsio/diag_*_ges.${analdate}_${charnanal} .

    ((nanal=nanal+1))
done

#if [[ $univartracers == '.true.' ]]
#then
#    echo "univartracers == '.true.' does not work properly"
#    exit 1
#fi


cat <<EOF > enkf.nml
&nam_enkf
datestring="$analdate",datapath="${workdir}/"
analpertwtnh=$analpertwtnh,analpertwtsh=$analpertwtsh,analpertwttr=$analpertwttr,
lupd_satbiasc=$lupd_satbiasc,zhuberleft=$zhuberleft,zhuberright=$zhuberright,huber=$huber,
varqc=$varqc,
covinflatemax=$covinflatemax,covinflatemin=$covinflatemin,pseudo_rh=$pseudo_rh,
corrlengthnh=$corrlengthnh,corrlengthsh=$corrlengthsh,corrlengthtr=$corrlengthtr,
obtimelnh=$obtimelnh,obtimelsh=$obtimelsh,obtimeltr=$obtimeltr,iassim_order=$iassim_order,
lnsigcutoffnh=$lnsigcutoffnh,lnsigcutoffsh=$lnsigcutoffsh,lnsigcutofftr=$lnsigcutofftr,
lnsigcutoffsatnh=$lnsigcutoffsatnh,lnsigcutoffsatsh=$lnsigcutoffsatsh,
lnsigcutoffsattr=$lnsigcutoffsattr,
lnsigcutoffpsnh=$lnsigcutoffpsnh,lnsigcutoffpssh=$lnsigcutoffpssh,
lnsigcutoffpstr=$lnsigcutoffpstr,
simple_partition=$simple_partition,nlons=$nlons,nlats=$nlats,smoothparm=$smoothparm,
readin_localization=$readin_localization,saterrfact=$saterrfact,numiter=$numiter,
sprd_tol=$sprd_tol,paoverpb_thresh=$paoverpb_thresh,letkf_flag=$letkf_flag,
use_qsatensmean=$use_qsatensmean,npefiles=$npefiles,lobsdiag_forenkf=$lobsdiag_forenkf,
reducedgrid=$reducedgrid,nlevs=$LEVS,nanals=$nanals,deterministic=$deterministic,
write_spread_diag=.true.,
sortinc=$sortinc,univaroz=$univaroz,univartracers=$univartracers,
massbal_adjust=$massbal_adjust,nhr_anal=$nhr_anal,
nhr_state=$nhr_state,
use_gfs_nemsio=$use_gfs_nemsio,adp_anglebc=$adp_anglebc,angord=$angord,
newpc4pred=$newpc4pred,use_edges=$use_edges,emiss_bc=$emiss_bc,
biasvar=$biasvar,write_spread_diag=$write_spread_diag
/
&satobs_enkf
/
&END
&ozobs_enkf
/
&END
&aodobs_enkf
sattypes_aod(1)='nnr'
/
&END
EOF

cat enkf.nml

LOG_DATE_DIR=${LOGDIR}/${analdate}

if [[ ! -r $LOG_DATE_DIR ]]
then
    mkdir -p $LOG_DATE_DIR
fi

/bin/cp ${GSI_FIXDIR}/aeroinfo_aod.txt ./aeroinfo 
/bin/cp ${GSI_FIXDIR}/anavinfo_fv3_gocart_enkf ./anavinfo

echo "mpirun -np $nanals ${ENKF_EXECDIR}/${ENKF_EXEC} > ${LOG_DATE_DIR}/enkf.out 2>&1"
mpirun -np $nanals ${ENKF_EXECDIR}/${ENKF_EXEC} > ${LOG_DATE_DIR}/enkf.out 2>&1

OUTDIR=${FV3_RUNS}/${sim}/${analdate}

grep 'all done' ${LOG_DATE_DIR}/enkf.out

if [[ $? -eq 0 ]]
then
    EXEC=${MY_EXECDIR}/calc_increment_nemsio.x
    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal="mem"`printf %03i $nanal`
	outdir=${OUTDIR}/${charnanal}/OUTPUT_ENKF
	if [[ ! -r $outdir ]] then
	    mkdir -p $outdir
	fi

	ln -sf sfg_${analdate}_fhr06_${charnanal} file_fcst 
	ln -sf sanl_${analdate}_${charnanal} file_anal
	
	file_increment=increment_${analdate}_${charnanal}.nc
	/bin/cp ${FV3_FIXDIR}/C${RES}/fv3_increment_zero.nc $file_increment
	${EXEC} file_fcst file_anal $file_increment

	if [[ ! -r sanl_${analdate}_$charnanal || ! -r $file_increment ]]
	then
	    echo "EnKF analysis and/or increment missing - Stopping"
	    exit 1
	fi

	/bin/mv sanl_${analdate}_$charnanal $file_increment $outdir

#	ncpdq -O -a '-lev' $file_increment ${file_increment}_lev_reversed
#	/bin/mv sanl_${analdate}_$charnanal $file_increment ${file_increment}_lev_reversed $outdir

	((nanal=nanal+1))
    done
else
    echo 'ENKF failed'
    exit 1
fi

cd $SCRIPTDIR_DRIVER

nanal=1
while [[ $nanal -le $nanals ]]
do
    echo "qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh"
    qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh
    ((nanal=nanal+1))
done


cd $SCRIPTDIR_UTIL
echo "qsub -v ident=$analdatem qsub_calc_aod_ensemble.sh"
#qsub -v ident=$analdatem qsub_calc_aod_ensemble.sh

exit 0

echo "$analdate starting ens mean analysis computation `date`"
csh ${enkfscripts}/compute_ensmean_enkf.csh >&!  ${current_logdir}/compute_ensmeans_enkf.out
echo "$analdate done computing ensemble mean analyses `date`"

# check output files again.
nanal=1
set filemissing='no'
while ($nanal <= $nanals)
   set charnanal="mem"`printf %03i $nanal`
   if ($IAU == ".true.") then
      set analfile="${datapath2}/sanl_${analdate}_${charfhr}_${charnanal}"
   else
      set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
   endif
   if ( ! -s $analfile) set filemissing='yes'
   @ nanal = $nanal + 1
end
if ( $satbiasc == ".true." &&  ! -s $ABIAS) set filemissing='yes'

if ($filemissing == 'yes') then
    echo "there are output files missing!"
    exit 1
else
    echo "all output files seem OK `date`"
endif

exit 0
