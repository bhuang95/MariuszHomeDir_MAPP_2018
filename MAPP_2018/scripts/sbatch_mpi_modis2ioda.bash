#! /usr/bin/env bash

#SBATCH --account=chem-var
#SBATCH --qos=debug
#SBATCH --ntasks=40
#SBATCH --cpus-per-task=10
#SBATCH --time=29
#SBATCH --job-name="modis2ioda"
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j
#SBATCH --exclusive

#sbatch --export=ALL sbatch_mpi_modis2ioda.bash

# Synopsis: This replacement for run_modis2ioda.py is parallelized,
# and it has error checking, documentation, and logging. It uses
# mpiserial to run multiple invocations of modis2ioda simultaneously.
# There is per MPI rank (AKA task or PE), and any number of ranks from
# 1 onward will work; more ranks effect a shorter runtime.
#
# This script uses bash-specific features, so it must run in bash.
# (Last tested with GNU bash 4.2.46 on NOAA Hera.)
#
# Author: Samuel Trahan, NOAA, April 28, 2021
#
# Prerequisites: modis2ioda, mpiserial, ncrcat

script_start_time=$( date +%s )

# ----------------------------------------------------------------------

# Configuration options

set -xue # disabled later if verbose==NO

# How many hours between analysis cycles?
CycleHrs=6 # Must be an integer in the range [1,47]

# Input directory:
InRoot='/scratch2/BMC/public/data/sat/nasa/modis/aerosol'

# Output directory. Will receive one subdirectory per cycle:
OutRoot='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/MODIS/test'

verbose=NO # YES = very wordy; NO = normal

StartCycle="2021-10-10t00:00:00"
EndCycle="2021-10-10t18:00:00"

# The modis2ioda.x executable. Make sure the "Prepare the environment"
# section correctly prepares the environment (module load, LD_LIBRARY_PATH)
modis2ioda='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/modis2ioda.x'

ignore_errors=NO # if YES, keep going no matter what. If NO, exit on error.

# ----------------------------------------------------------------------

# Prepare the environment


export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK # Must match --cpus-per-task in job card
export OMP_STACKSIZE=128M # Should be enough; increase it if you hit the stack limit.

if [[ "$verbose" == NO ]] ; then
    set +xue
    mpiserial_flags='-m -q'
else
    mpiserial_flags='-m'
fi

if [[ "$ignore_errors" == YES ]] ; then
    set +ue
fi

. /apps/lmod/lmod/init/bash
. ~/.jedi
. ~/.hdf4

JEDIDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle/bin

module list

# The Fortran datetime library must be in LD_LIBRARY_PATH:
export LD_LIBRARY_PATH="/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# The mpiserial and ncrcat must be in $PATH:
export PATH="/scratch2/BMC/wrfruc/Samuel.Trahan/viirs-thinning/mpiserial/exec:$PATH"

# ----------------------------------------------------------------------




# Actual script begins here

# Make sure we have the required executables
for exe in mpiserial ncrcat ; do
    if ( ! which "$exe" ) ; then
        echo "Error: $exe is not in \$PATH. Go find it and rerun." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi
done

# Time calculations must be in UTC:
export TZ=UTC

# Make sure we have modis
if [[ ! -e "$modis2ioda" || ! -x "$modis2ioda" ]] ; then
    echo "Error: modis2ioda is not an executable file: $modis2ioda" 1>&2
    if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
fi

HalfCycleMinutes=$(( CycleHrs*60/2 ))
NowCycle=$StartCycle

echo
echo "Will process every ${CycleHrs}-hour cycles from $StartCycle to $EndCycle, inclusive."

declare -a usefiles # usefiles is now an array
file_count=0
no_output=0
# Loop over analysist times. The lexical comparison (< =) ignores the : t - chars
while [[ "$NowCycle" < "$EndCycle" || "$NowCycle" = "$EndCycle" ]] ; do
    echo
    echo "Processing analysis cycle: $NowCycle"
    echo

    StartObs=$( date -d "$NowCycle UTC - $HalfCycleMinutes minutes" +"%Y-%m-%dt%H:%M:%S" )
    EndObs=$( date -d "$NowCycle UTC + $HalfCycleMinutes minutes" +"%Y-%m-%dt%H:%M:%S" )

    # Get the start and end obs time ranges in YYYYMMDD and YYYYMMDDHHMMSS format:
    StartYMD=${StartObs:0:4}${StartObs:5:2}${StartObs:8:2}
    StartYMDH=$StartYMD${StartObs:11:2}
    EndYMD=${EndObs:0:4}${EndObs:5:2}${EndObs:8:2}
    EndYMDH=$EndYMD${EndObs:11:2}
    validtime=${NowCycle:0:4}${NowCycle:5:2}${NowCycle:8:2}${NowCycle:11:2}

    echo "Valid time: $validtime"

    julians=`date -d $StartYMD +%j`
    juliane=`date -d $EndYMD +%j`

    StartObs_modis=${StartObs:0:4}${julians}.${StartObs:11:2}${StartObs:14:2}
    EndObs_modis=${EndObs:0:4}${juliane}.${EndObs:11:2}${EndObs:14:2}

    # Output directory: subdirectory for each valid time.
    OutDir="$OutRoot/$validtime"
    echo "Output directory: $OutDir"

    # Delete the output directory if it exists.
    if [[ -e "$OutDir" || -L "$OutDir" ]] ; then
        rm -rf "$OutDir"
        if [[ -e "$OutDir" || -L "$OutDir" ]] ; then
            echo "Warning: could not delete $OutDir" 1>&2
        fi
    fi

    # Create the output directory and pushd into it.
    [[ -d "$OutDir" ]] || mkdir -p "$OutDir"
    if [[ ! -d "$OutDir" ]] ; then
        echo "Error: could not make $OutDir" 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi
    pushd "$OutDir"
    if [[ "$?" -ne 0 ]] ; then
        echo "Could not pushd into $OutDir" 1>&2
        exit 1 # cannot keep going after this failure
    fi

    for f in ${InRoot}/*${StartObs_modis:0:7}*.hdf ${InRoot}/*${EndObs_modis:0:7}*.hdf 
    do
	basef=`basename $f`
	((julian=${basef:14:3} -1))
	year=${basef:10:4}
	caldate=`date -d "$julian day ${year:0:4}0101" +"%Y%m%d"`
	newf=${basef:0:8}_s${caldate}${basef:18:4}.hdf
	ln -sf $f $newf
    done

    # Scan the files in the date directories for ones in the right time range.
    if [[ "$verbose" != NO ]] ; then
        set +x
        echo "Disabling set -x for the moment; the next region is too verbose for set -x."
    fi
    usefiles=() # clear the list of files

    for f in $( ls -1 "$OutDir/"*.hdf "$OutDir/"*.hdf | sort -u ) ; do
        # Match the _s(number) start time and make sure it is after the time of interest
        if ! [[ $f =~ ^.*_s([0-9]{10}) ]] || ! (( BASH_REMATCH[1] >= StartYMDH )) ; then
            echo "Skip; too early: $f"
        # Match the _e(number) end time and make sure it is after the time of interest
        elif ! [[ $f =~ ^.*_s([0-9]{10}) ]] || ! (( BASH_REMATCH[1] <= EndYMDH )) ; then
            echo "Skip; too late:  $f"
        else
            echo "Using this file: $f"
            usefiles+=("$f") # Append the file to the usefiles array
        fi
    done

    if [[ "$verbose" != NO ]] ; then
        echo "Turning set -x back on."
        set -x
    fi

    # Make sure we found some files.
    echo "Found ${#usefiles[@]} files between $StartObs and $EndObs."
    if ! (( ${#usefiles[@]} > 0 )) ; then
        echo "Error: no files found for specified time range in $InRoot" 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    # Output directory: subdirectory for each valid time.
#    OutDir="$OutRoot/$validtime"
#    echo "Output directory: $OutDir"

    # Delete the output directory if it exists.
#    if [[ -e "$OutDir" || -L "$OutDir" ]] ; then
#        rm -rf "$OutDir"
#        if [[ -e "$OutDir" || -L "$OutDir" ]] ; then
#            echo "Warning: could not delete $OutDir" 1>&2
#        fi
#    fi

    # Create the output directory and pushd into it.
#    [[ -d "$OutDir" ]] || mkdir -p "$OutDir"
#    if [[ ! -d "$OutDir" ]] ; then
#        echo "Error: could not make $OutDir" 1>&2
#        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
#    fi
#    pushd "$OutDir"
#    if [[ "$?" -ne 0 ]] ; then
#        echo "Could not pushd into $OutDir" 1>&2
#        exit 1 # cannot keep going after this failure
#    fi

    # Prepare the list of commands to run.
    cat /dev/null > cmdfile
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" ).nc
        echo "$modis2ioda" "$validtime" "$f" "$fout" >> cmdfile
        file_count=$(( file_count + 1 ))
    done

    # Run many tasks in parallel via mpiserial.
    echo "Now running executable $modis2ioda"
    if ( ! srun -l mpiserial $mpiserial_flags cmdfile ) ; then
        echo "At least one of the files failed. See prior logs for details." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    # Make sure all files were created.
    success=0
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" )
        if [[ -s "$fout" ]] ; then
            success=$(( success + 1 ))
        else
            no_output=$(( no_output + 1 ))
            echo "Missing output file: $fout"
        fi
    done
    if [[ "$success" -eq 0 ]] ; then
        echo "Error: no files were output in this analysis cycle. Perhaps there are no obs at this time?" 1>&2
        if [[ $ignore_errors == NO ]] ; then
            echo "       Rerun with ignore_errors=YES to continue processing anyway." 1>&2
            exit 1
        fi
    fi
    if [[ "$success" -ne "${#usefiles[@]}" ]] ; then
        echo "In analysis cycle $NowCycle, only $success of ${#usefiles[@]} files were output."
        echo "Usually this means some files had no valid obs. See prior messages for details."
    else
        echo "In analysis cycle $NowCycle, all $success of ${#usefiles[@]} files were output."
    fi

    # Merge the files.

    FinalFile_v1="modis_aod_aqua.${validtime}_v1.nc"
    FinalFile_v2="modis_aod_aqua.${validtime}.nc"

    echo Merging files now...
    if ( ! ncrcat -O *MYD*.nc tmp.nc ) ; then
        echo "Error: ncrcat returned non-zero exit status" 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    # Make sure they really were merged.
    if [[ ! -s tmp.nc ]] ; then
        echo "Error: ncrcat did not create ${FinalFile_v1}." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    ncks --fix_rec_dmn all tmp.nc "${FinalFile_v1}"
    /bin/rm tmp.nc

    ${JEDIDIR}/ioda-upgrade.x ${FinalFile_v1} ${FinalFile_v2}

    FinalFile_v1="modis_aod_terra.${validtime}_v1.nc"
    FinalFile_v2="modis_aod_terra.${validtime}.nc"
    echo Merging files now...
    if ( ! ncrcat -O *MOD*.nc tmp.nc ) ; then
        echo "Error: ncrcat returned non-zero exit status" 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    # Make sure they really were merged.
    if [[ ! -s tmp.nc ]] ; then
        echo "Error: ncrcat did not create ${FinalFile_v1}." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    ncks --fix_rec_dmn all tmp.nc "${FinalFile_v1}"
    ${JEDIDIR}/ioda-upgrade.x ${FinalFile_v1} ${FinalFile_v2}

    /bin/rm tmp.nc *.hdf *.hdf.nc

    # Go back to the prior directory:
    popd
    if [[ "$?" -ne 0 ]] ; then
        echo "Could not popd back to original directory." 1>&2
        exit 1 # cannot keep going after this failure
    fi

    echo "Completed analysis cycle $NowCycle"

    # Step to the next cycle
    NowCycle=$( date -d "$NowCycle UTC + $CycleHrs hours" +"%Y-%m-%dt%H:%M:%S" )
done

script_end_time=$( date +%s )

# Excitedly report success:
echo
echo "Done!"
echo
echo "Output files are here ==> $OutRoot"
echo
echo "Processed $file_count files in $(( script_end_time - script_start_time )) seconds."
if (( no_output > 0 )) ; then
    echo "Of those, $no_output input files had no thinned obs output files."
    echo "Usually this means some files had no valid obs. See prior messages for details."
fi
echo "Please enjoy your files and have a nice day."
