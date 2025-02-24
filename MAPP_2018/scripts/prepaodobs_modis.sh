#!/bin/bash 


##SBATCH --account=chem-var
##SBATCH --qos=debug
##SBATCH --ntasks=40
##SBATCH --cpus-per-task=10
##SBATCH --time=5
##SBATCH --job-name="bashtest"
##SBATCH --exclusive
##! /usr/bin/env bash

###############################################################
## Abstract:
## Create biomass burning emissions for FV3-CHEM
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################
# Source FV3GFS workflow modules
#. $HOMEgfs/ush/load_fv3gfs_modules.sh
#. /apps/lmod/lmod/init/bash
#module purge
#module load intel impi netcdf/4.6.1 nco # Modules required on NOAA Hera
set -x
export HOMEjedi=${HOMEjedi:-"/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/fv3-bundle/V20210701/build/"}
. ${HOMEjedi}/jedi_module_base.hera
. ${HOMEjedi}/hdf4_module.hera
#module load nco
module list
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOMEjedi}/lib/"
status=$?
[[ $status -ne 0 ]] && exit $status

export LD_LIBRARY_PATH="/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
export PATH="/scratch2/BMC/wrfruc/Samuel.Trahan/viirs-thinning/mpiserial/exec:$PATH"
# Make sure we have the required executables
for exe in mpiserial ncrcat ; do
    if ( ! which "$exe" ) ; then
         echo "Error: $exe is not in \$PATH. Go find it and rerun." 1>&2 
         #if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi
done
#export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK # Must match --cpus-per-task in job card
#export OMP_STACKSIZE=128M # Should be enough; increase it if you hit the stack limit.

STMP="/scratch2/BMC/gsd-fv3-dev/NCEPDEV/stmp3/$USER/"
export RUNDIR="$STMP/RUNDIRS/$PSLOT"
export DATA="$RUNDIR/$CDATE/$CDUMP/prepaodobs_modis"
#CURRDIR=`pwd`
#export RUNDIR="${CURRDIR}/stmp"
#export DATA="${RUNDIR}/prepaodobs_modis"
# OBSDIR_NASA

[[ ! -d $DATA ]] && mkdir -p $DATA
cd $DATA || exit 10
HOMEgfs=${HOMEgfs:-"/home/Bo.Huang/JEDI-2020/GSDChem_cycling/global-workflow-CCPP2-Chem-NRT-clean/"}
OBSDIR_MODIS_NASA=${OBSDIR_MODIS_NASA:-"/scratch2/BMC/public/data/sat/nasa/modis/aerosol/"}
OBSDIR_MODIS_NRT=${OBSDIR_MODIS_NRT:-"/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/NRTdata/aodObs"}
AODTYPE=${AODTYPE:-"MODIS-NRT"}
AODSAT=${AODSAT:-"MYD04_L2 MOD04_L2"}
CDATE=${CDATE:-"2021072000"}
CYCINTHR=${CYCINTHR:-"6"}
CASE=${CASE:-""}
NDATE=${NDATE:-"/scratch2/NCEPDEV/nwprod/NCEPLIBS/utils/prod_util.v1.1.0/exec/ndate"}

#VIIRS2IODAEXEC=/scratch2/BMC/wrfruc/Samuel.Trahan/viirs-thinning/mmapp_2018_src_omp/exec/viirs2ioda.x
MODIS2IODAEXEC=${HOMEgfs}/exec/modis2ioda.x
IODAUPGRADEREXEC=${HOMEjedi}/bin/ioda-upgrade.x
#FV3GRID=${HOMEgfs}/fix/fix_fv3/${CASE}
FV3GRID=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fix_fv3/${CASE}
AODOUTDIR=${OBSDIR_MODIS_NRT}/${AODTYPE}-${CASE}/${CDATE}/
#AODOUTDIR=${CURRDIR}/${AODTYPE}-${CASE}/${CDATE}/

[[ ! -d ${AODOUTDIR} ]] && mkdir -p ${AODOUTDIR}

RES=`echo $CASE | cut -c2-4`
YY=`echo "${CDATE}" | cut -c1-4`
MM=`echo "${CDATE}" | cut -c5-6`
DD=`echo "${CDATE}" | cut -c7-8`
HH=`echo "${CDATE}" | cut -c9-10`

HALFCYCLE=$(( CYCINTHR/2 ))
STARTOBS=$(${NDATE} -${HALFCYCLE} ${CDATE})
ENDOBS=$(${NDATE} ${HALFCYCLE} ${CDATE})

STARTYY=`echo "${STARTOBS}" | cut -c1-4`
STARTMM=`echo "${STARTOBS}" | cut -c5-6`
STARTDD=`echo "${STARTOBS}" | cut -c7-8`
STARTHH=`echo "${STARTOBS}" | cut -c9-10`
STARTYMD=${STARTYY}${STARTMM}${STARTDD}
STARTYMDH=${STARTYY}${STARTMM}${STARTDD}${STARTHH}
STARTYMDHMS=${STARTYY}${STARTMM}${STARTDD}${STARTHH}0000

ENDYY=`echo "${ENDOBS}" | cut -c1-4`
ENDMM=`echo "${ENDOBS}" | cut -c5-6`
ENDDD=`echo "${ENDOBS}" | cut -c7-8`
ENDHH=`echo "${ENDOBS}" | cut -c9-10`
ENDYMD=${ENDYY}${ENDMM}${ENDDD}
ENDYMDH=${ENDYY}${ENDMM}${ENDDD}${ENDHH}
ENDYMDHMS=${ENDYY}${ENDMM}${ENDDD}${ENDHH}0000

JULIANS=`date -d ${STARTYMD} +%j`
JULIANE=`date -d ${ENDYMD} +%j`

STARTYMD_JULIAN=${STARTYY}${JULIANS}
STARTYMDHM_JULIAN=${STARTYY}${JULIANS}.${STARTHH}00

ENDYMD_JULIAN=${ENDYY}${JULIANE}
ENDYMDHM_JULIAN=${ENDYY}${JULIANE}.${ENDHH}00


for sat in ${AODSAT}; do
    FINALFILEv1_tmp="${AODTYPE}_AOD_${sat}.${CDATE}.iodav1.tmp.nc"
    FINALFILEv1="${AODTYPE}_AOD_${sat}.${CDATE}.iodav1.nc"
    FINALFILEv2="${AODTYPE}_AOD_${sat}.${CDATE}.nc"
    [[ -f ${FINALFILEv1_tmp} ]] && /bin/rm -rf ${FINALFILEv1_tmp}
    [[ -f ${FINALFILEv1} ]] && /bin/rm -rf ${FINALFILEv1}
    [[ -f ${FINALFILEv2} ]] && /bin/rm -rf ${FINALFILEv2}
    declare -a usefiles # usefiles is now an array
    usefiles=() # clear the list of files
    allfiles=`ls -1 ${OBSDIR_MODIS_NASA}/${sat}.A${STARTYMD_JULIAN}.*.*.NRT.hdf ${OBSDIR_MODIS_NASA}/${sat}.A${ENDYMD_JULIAN}.*.*.NRT.hdf | sort -u`
    for f in ${allfiles}; do
	basef=`basename ${f}`
	((julian=${basef:14:3} -1))
	year=${basef:10:4}
	caldate=`date -d "$julian day ${year:0:4}0101" +"%Y%m%d"`
	newf=${basef:0:8}_s${caldate}${basef:18:4}.hdf
	ln -sf $f $newf
    done

    allfiles_new=`ls ${DATA}/*.hdf | sort -u`
    #MYD04_L2_s202107200005
    for f in ${allfiles_new}; do
        # Match the _s(number) start time and make sure it is after the time of interest
	if ! [[ $f =~ ^.*_s([0-9]{10}) ]] || ! (( BASH_REMATCH[1] >= STARTYMDH )) ; then
	    echo "${BASH_REMATCH[1]}"
	    echo ${STARTYMDH}
            echo "Skip; too early: $f"
        # Match the _e(number) end time and make sure it is after the time of interest
        elif ! [[ $f =~ ^.*_s([0-9]{10}) ]] || ! (( BASH_REMATCH[1] < ENDYMDH )) ; then
	    echo "${BASH_REMATCH[1]}"
	    echo ${ENDYMDH}
            echo "Skip; too late:  $f"
        else
	    echo "${BASH_REMATCH[1]}"
	    echo ${STARTYMDH}
	    echo ${ENDYMDH}
            echo "Using this file: $f"
            usefiles+=("$f") # Append the file to the usefiles array
        fi
    done
    echo "${usefiles[*]}" | tr ' ' '\n'
    
    # Make sure we found some files.
    echo "Found ${#usefiles[@]} files between $STARTOBS and $ENDOBS."
    if ! (( ${#usefiles[@]} > 0 )) ; then
        echo "Error: no files found for specified time range in ${OBSDIR_MODIS_NASA}" 1>&2
    exit 1
    fi

    # Prepare the list of commands to run.
    [[ -f cmdfile ]] && /bin/rm -rf cmdfile
    cat /dev/null > cmdfile
    file_count=0
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" )
        echo "${MODIS2IODAEXEC}" "${CDATE}" "$f" "$fout.nc" >> cmdfile
        file_count=$(( file_count + 1 ))
    done
    
    # Run many tasks in parallel via mpiserial.
    mpiserial_flags='-m '
    echo "Now running executable ${MODIS2IODAEXEC}"
    if ( ! srun  -l mpiserial $mpiserial_flags cmdfile ) ; then
        echo "At least one of the files failed. See prior logs for details." 1>&2
        exit 1
    fi
    
    # Make sure all files were created.
    no_output=0
    success=0
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" )

        if [[ -s "$fout.nc" ]] ; then
            success=$(( success + 1 ))
        else
            no_output=$(( no_output + 1 ))
            echo "Missing output file: $fout"
        fi
    done
    
    if [[ "$success" -eq 0 ]] ; then
        echo "Error: no files were output in this analysis cycle. Perhaps there are no obs at this time?" 1>&2
            exit 1
    fi
    if [[ "$success" -ne "${#usefiles[@]}" ]] ; then
        echo "In analysis cycle ${CDATE}, only $success of ${#usefiles[@]} files were output."
        echo "Usually this means some files had no valid obs. See prior messages for details."
    else
        echo "In analysis cycle ${CDATE}, all $success of ${#usefiles[@]} files were output."
    fi
    
    # Merge the files.
    echo Merging files now...
    if ( ! ncrcat -O ${sat}*.hdf.nc "${FINALFILEv1_tmp}" ) ; then
        echo "Error: ncrcat returned non-zero exit status" 1>&2
        exit 1
    fi
    
    # Make sure they really were merged.
    if [[ ! -s "$FINALFILEv1_tmp" ]] ; then
        echo "Error: ncrcat did not create $FINALFILEv1_tmp ." 1>&2
        exit 1
    fi
    #/bin/rm -rf JRR-AOD_v2r3_${sat}_*.nc
     
    ncks --fix_rec_dmn all ${FINALFILEv1_tmp} ${FINALFILEv1}

    echo "IODA_UPGRADE for ${FINALFILEv1}"
    ${IODAUPGRADEREXEC} ${FINALFILEv1} ${FINALFILEv2}
    err=$?
    if [[ $err -eq 0 ]]; then
        #/bin/mv ${FINALFILEv1}  ${AODOUTDIR}/
        /bin/mv ${FINALFILEv2}  ${AODOUTDIR}/
	/bin/rm -rf *.hdf *.hdf.nc
        err=$?
    else
        echo "IODA_UPGRADER failed for ${FINALFILEv1} and exit."
	exit 1
    fi

    #/bin/mv JRR-AOD_v2r3_${sat}_*.nc  ${AODOUTDIR}/
done

#Start prepare AERONET AOD processing
#aeronetdir=/home/Bo.Huang/JEDI-2020/miscScripts-home/JEDI-Support/aeronetScript/readAeronet/
#module load python/3.7.5
#module use -a /contrib/anaconda/modulefiles
#module load anaconda/latest
#
#AERODATE=${YY}:${MM}:${DD}T${HH}:00:00
#outfile_v1=${DATA}/MODIS-NRT_AOD_AERONET.${CDATE}.v1.nc
#outfile_v2=${DATA}/MODIS-NRT_AOD_AERONET.${CDATE}.nc
#python ${aeronetdir}/aeronet.py -i ${AERODATE} -o ${outfile_v1}
#err=$?
#if [ ${err} != '0' ]; then
#    echo "aeronet.py failed at ${CDATE} and exit!"
#    exit 1
#else
#    echo "aeronet.py succeeded at ${CDATE} and proceed to next step!"
#fi
#
#${JEDIdir}/bin/ioda-upgrade.x ${outfile_v1} ${outfile_v2}
#err=$?
#if [ ${err} != '0' ]; then
#    echo "ioda-upgrade.x failed at ${CDATE} and exit!"
#    exit 1
#else
#    echo "ioda-upgrade.x succeeded at ${CDATE} and move data!"
#    /bin/mv ${outfile_v2} ${AODOUTDIR}
#fi
    
if [[ $err -eq 0 ]]; then
    /bin/rm -rf $DATA
fi
    
echo $(date) EXITING $0 with return code $err >&2
exit $err
