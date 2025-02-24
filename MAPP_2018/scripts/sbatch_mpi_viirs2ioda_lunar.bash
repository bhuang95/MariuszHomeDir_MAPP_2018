#! /usr/bin/env bash

##SBATCH --account=chem-var
#SBATCH --account=ap-fc
#SBATCH --qos=debug
##SBATCH --qos=batch
#SBATCH --ntasks=40
#SBATCH --cpus-per-task=10
##SBATCH --time=04:00:00
#SBATCH --time=00:30:00
#SBATCH --job-name="viirs2ioda"
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j
#SBATCH --exclusive

#sbatch --export=ALL sbatch_mpi_viirs2ioda_new.bash

# Synopsis: This replacement for run_viirs2ioda.py is parallelized,
# and it has error checking, documentation, and logging. It uses
# mpiserial to run multiple invocations of viirs2ioda simultaneously.
# There is per MPI rank (AKA task or PE), and any number of ranks from
# 1 onward will work; more ranks effect a shorter runtime.
#
# This script uses bash-specific features, so it must run in bash.
# (Last tested with GNU bash 4.2.46 on NOAA Hera.)
#
# Author: Samuel Trahan, NOAA, April 28, 2021
#
# Prerequisites: viirs2ioda, mpiserial, ncrcat

script_start_time=$( date +%s )

# ----------------------------------------------------------------------

# Configuration options

set -xue # disabled later if verbose==NO

StartCycle="2020-09-29t06:00:00"
EndCycle="2020-10-06t12:00:00"

grid=192 # FV3 grid size

# How many hours between analysis cycles?
CycleHrs=6 # Must be an integer in the range [1,47]

# Input directory:
InRoot='/scratch1/BMC/chem-var/pagowski/DATA/OBS/VIIRS_lunar'

# Output directory. Will receive one subdirectory per cycle:
OutRoot='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS_lunar/thinned_debiased_C'${grid}

# FV3 grid definition file:
FV3Grid='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fix_fv3/C'$grid

verbose=NO # YES = very wordy; NO = normal


# The viirs2ioda_lunar.x executable. Make sure the "Prepare the environment"
# section correctly prepares the environment (module load, LD_LIBRARY_PATH)
viirs2ioda='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/viirs2ioda_lunar.x'

ignore_errors=YES # if YES, keep going no matter what. If NO, exit on error.

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

#. ~/.nc
#module load impi/2020.2

#. ~/.jedi

#module unload eckit
#module load eckit/ecmwf-1.18.2
#module unload fckit
#module load fckit/ecmwf-0.9.5
#module unload atlas
#module load atlas/ecmwf-0.29.0
#module list

JEDIDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/fv3-bundle/V20210701/build/bin

#module list

# The Fortran datetime library must be in LD_LIBRARY_PATH:
export LD_LIBRARY_PATH="/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# The mpiserial and ncrcat must be in $PATH:
export PATH="/scratch2/BMC/wrfruc/Samuel.Trahan/viirs-thinning/mpiserial/exec:$PATH"

# ----------------------------------------------------------------------


# Actual script begins here

# Make sure we have the required executables
for exe in mpiserial ; do
    if ( ! which "$exe" ) ; then
        echo "Error: $exe is not in \$PATH. Go find it and rerun." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi
done

# Time calculations must be in UTC:
export TZ=UTC

# Make sure we have viirs
if [[ ! -e "$viirs2ioda" || ! -x "$viirs2ioda" ]] ; then
    echo "Error: viirs2ioda is not an executable file: $viirs2ioda" 1>&2
    if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
fi

HalfCycleMinutes=$(( CycleHrs*60/2 ))
NowCycle=$StartCycle

echo
echo "Will process every $CycleHrs cycles from $StartCycle to $EndCycle, inclusive."

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
    StartYMDHMS=$StartYMD${StartObs:11:2}0000
    EndYMD=${EndObs:0:4}${EndObs:5:2}${EndObs:8:2}
    EndYMDHMS=$EndYMD${EndObs:11:2}0000
    validtime=${NowCycle:0:4}${NowCycle:5:2}${NowCycle:8:2}${NowCycle:11:2}

    echo "Valid time: $validtime"

    # Scan the files in the date directories for ones in the right time range.
    if [[ "$verbose" != NO ]] ; then
        set +x
        echo "Disabling set -x for the moment; the next region is too verbose for set -x."
    fi
    usefiles=() # clear the list of files
    for f in $( ls -1 "$InRoot/"*"$StartYMD"*.nc "$InRoot/"*"$EndYMD"*.nc | sort -u ) ; do
        # Match the _s(number) start time and make sure it is after the time of interest
        if ! [[ $f =~ ^.*_s([0-9]{14}) ]] || ! (( BASH_REMATCH[1] >= StartYMDHMS )) ; then
            echo "Skip; too early: $f"
        # Match the _e(number) end time and make sure it is after the time of interest
        elif ! [[ $f =~ ^.*_e([0-9]{14}) ]] || ! (( BASH_REMATCH[1] <= EndYMDHMS )) ; then
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

    # Prepare the list of commands to run.
    cat /dev/null > cmdfile
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" )
        echo "$viirs2ioda" "$validtime" "$FV3Grid" "$f" "$fout" >> cmdfile
        file_count=$(( file_count + 1 ))
    done

    . /home/Mariusz.Pagowski/mapp_2018/.environ.ksh

    # Run many tasks in parallel via mpiserial.
    echo "Now running executable $viirs2ioda"
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

    FinalFile_v1="viirs_aod_lunar_snpp.${validtime}_v1.nc"
    echo Merging files now...

    . ~/.nc

    if ( ! ncrcat -O *npp*.nc tmp.nc ) ; then
        echo "Error: ncrcat returned non-zero exit status" 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    # Make sure they really were merged.
    if [[ ! -s tmp.nc ]] ; then
        echo "Error: ncrcat did not create ${FinalFile_v1} ." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    ncks -O --fix_rec_dmn all tmp.nc ../"${FinalFile_v1}"

    /bin/rm tmp.nc JRR-AOD*.nc 

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
