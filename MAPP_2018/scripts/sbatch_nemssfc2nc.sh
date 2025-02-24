#!/bin/ksh
#SBATCH -A gsd-fv3-dev
#SBATCH -J nemssfc2nc
#SBATCH -q batch
##SBATCH -q debug
#SBATCH -t 01:00:00
#SBATCH -n 120
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


#sbatch --export=ALL,start=2019072000,end=2019072018 sbatch_nemssfc2nc.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

HOMEgfs=/home/Mariusz.Pagowski/workflows/NRT2RETRO

CONTROL_ANAL=1
ENKF_FCST=0
ENKF_ANAL=1

CASE=C96
CASE_CNTL_GDAS=C768
CASE_ENKF_GDAS=C384

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata
INDIR=${MAINDIR}/rawdata/gdasAna
OUTDIR=${MAINDIR}/gdasAna/${CASE}

ndate=~/bin/ndate

cycle_frequency=6
nanals=20

DATA=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_change_cube.$$

. $HOMEgfs/ush/load_fv3gfs_modules.sh

CDUMP=gdas

NLN='/bin/ln -sf'
NRM='/bin/rm -rf'
NMV='/bin/mv'
NCP='/bin/cp'

export HOMEufs=${HOMEgfs}
export APRUN="srun --export=ALL -n $SLURM_NTASKS"
export CHGRESEXEC=${CHGRESEXEC:-"${HOMEgfs}/exec/chgres_cube"}
FIXOROG=${HOMEgfs}/fix/fix_fv3_gmted2010
export CRES=`echo ${CASE} | cut -c2-4`
export VCOORD_FILE=${HOMEgfs}/fix/fix_am/global_hyblev.l64.txt
export MOSAIC_FILE_INPUT_GRID=${FIXOROG}/${CASE_ENKF_GDAS}/${CASE_ENKF_GDAS}_mosaic.nc
export OROG_DIR_INPUT_GRID=${FIXOROG}/${CASE_ENKF_GDAS}/
export OROG_FILES_INPUT_GRID=${CASE_ENKF_GDAS}_oro_data.tile1.nc'","'${CASE_ENKF_GDAS}_oro_data.tile2.nc'","'${CASE_ENKF_GDAS}_oro_data.tile3.nc'","'${CASE_ENKF_GDAS}_oro_data.tile4.nc'","'${CASE_ENKF_GDAS}_oro_data.tile5.nc'","'${CASE_ENKF_GDAS}_oro_data.tile6.nc
export MOSAIC_FILE_TARGET_GRID=${FIXOROG}/${CASE}/${CASE}_mosaic.nc
export OROG_FILES_TARGET_GRID=${CASE}_oro_data.tile1.nc'","'${CASE}_oro_data.tile2.nc'","'${CASE}_oro_data.tile3.nc'","'${CASE}_oro_data.tile4.nc'","'${CASE}_oro_data.tile5.nc'","'${CASE}_oro_data.tile6.nc

export CONVERT_ATM=".false."
export CONVERT_SFC=".true."
export CONVERT_NST=".true."

start_date=$start
end_date=$end

ident=$start_date

while [[ $ident -le $end_date ]]
do

    CDATE=$ident
    export CDATE=${CDATE}
    
    CYY=`echo "${CDATE}" | cut -c1-4`
    CMM=`echo "${CDATE}" | cut -c5-6`
    CDD=`echo "${CDATE}" | cut -c7-8`
    CHH=`echo "${CDATE}" | cut -c9-10`
    CYMD=${CYY}${CMM}${CDD}

    
    if [[ ! -r $DATA ]]
    then
	mkdir -p $DATA
    fi
    
    export DATA
    cd $DATA

#control

    if [[ $CONTROL_ANAL == 1 ]]
    then
	indir=${INDIR}/${CDUMP}.${CYMD}/${CHH}
	export INPUT_TYPE=gaussian_nemsio
	export SFC_FILES_INPUT=${CDUMP}.t${CHH}z.sfcanl.nemsio
    
	export COMIN=./
    
	${NLN} ${indir}/${SFC_FILES_INPUT}  .
    
	${HOMEgfs}/ush/chgres_cube.sh
	ERR=$?
	
	if [[ ${ERR} -eq 0 ]]; then
	    echo "chgres_cube runs successful and move data."
	    
	    outdir=${OUTDIR}/${CDUMP}.${CYY}${CMM}${CDD}/${CHH}/RESTART
	    
	    echo $outdir
	    [[ ! -d ${outdir} ]] && mkdir -p ${outdir}
	    ${NMV} fort.41 ${outdir}/
	    for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
		${NMV} out.sfc.${tile}.nc ${outdir}/${CYMD}.${CHH}0000.sfc_data.${tile}.nc 
	    done
	    ${NRM} ${SFC_FILES_INPUT}
	else
	    echo "chgres_cube run  failed for control and exit."
	    exit ${ERR}
	fi
	
    fi

#ensemble

    if [[ $ENKF_FCST == 0 && $ENKF_ANAL == 0 ]] 
    then
	ident=`$ndate +${cycle_frequency} $ident`
	continue
    fi

    nanal=1

    while [[ $nanal -le $nanals ]]
    do

	charnanal=mem`printf %03i $nanal`
	
	if [[ ! -r $DATA ]]
	then
	    mkdir -p $DATA
	fi
	
	export COMIN=./
	
	if [[ $ENKF_ANAL == 1 ]]
	then
	    export INPUT_TYPE=restart
	    indir=${INDIR}/enkf${CDUMP}.${CYMD}/${CHH}/${charnanal}/RESTART
	    export SFC_FILES_INPUT=${CYMD}.${CHH}0000.sfcanl_data.tile1.nc'","'${CYMD}.${CHH}0000.sfcanl_data.tile2.nc'","'${CYMD}.${CHH}0000.sfcanl_data.tile3.nc'","'${CYMD}.${CHH}0000.sfcanl_data.tile4.nc'","'${CYMD}.${CHH}0000.sfcanl_data.tile5.nc'","'${CYMD}.${CHH}0000.sfcanl_data.tile6.nc
	    restartoutdir=RESTART
	    for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
		${NLN} ${indir}/${CYMD}.${CHH}0000.sfcanl_data.${tile}.nc .
	    done
	else
	    indir=${INDIR}/enkf${CDUMP}.${CYMD}/${CHH}/${charnanal}
	    export SFC_FILES_INPUT=${CDUMP}.t${CHH}z.sfcf006.nemsio
	    ${NLN} ${indir}/${SFC_FILES_INPUT} .
	    restartoutdir=RESTART_6hFcst
	fi
	

	${HOMEgfs}/ush/chgres_cube.sh
	ERR=$?
	
	if [[ ${ERR} -eq 0 ]]; then
	    echo "chgres_cube runs successful and move data."
	    
	    outdir=${OUTDIR}/enkf${CDUMP}.${CYY}${CMM}${CDD}/${CHH}/${charnanal}/${restartoutdir}
	    
	    echo $outdir
	    [[ ! -d ${outdir} ]] && mkdir -p ${outdir}
	    ${NMV} fort.41 ${outdir}/
	    for tile in tile1 tile2 tile3 tile4 tile5 tile6; do
		${NMV} out.sfc.${tile}.nc ${outdir}/${CYMD}.${CHH}0000.sfc_data.${tile}.nc 
	    done
	    ${NRM} ${SFC_FILES_INPUT}
	else
	    echo "chgres_cube run  failed for and exit."
	    exit ${ERR}
	fi

	((nanal=nanal+1))

    done

    ident=`$ndate +${cycle_frequency} $ident`

done

#${NRM} $DATA
