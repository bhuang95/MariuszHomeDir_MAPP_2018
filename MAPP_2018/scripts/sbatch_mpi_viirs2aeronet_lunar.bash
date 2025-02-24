#! /usr/bin/env bash
##SBATCH --account=chem-var
#SBATCH --account=ap-fc
#SBATCH --qos=debug
##SBATCH --qos=batch
#SBATCH --ntasks=40
#SBATCH --cpus-per-task=1
##SBATCH --time=08:00:00
#SBATCH --time=00:30:00
#SBATCH --job-name="viirs2aeronet_l"
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j
#SBATCH --exclusive

#sbatch --export=ALL sbatch_mpi_viirs2aeronet_lunar.bash

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
# MZP: for AERONET
#
# Prerequisites: viirs2aeronet, mpiserial, ncrcat

script_start_time=$( date +%s )

# ----------------------------------------------------------------------

# Configuration options

set -xue # disabled later if verbose==NO

StartCycle="2020-09-29t06:00:00"
EndCycle="2020-10-06t06:00:00"

#input params
radius=3 # in km
tdiff_aeronet_max=0.5
tdiff_v2a_max=1.
satellite=npp
#satellite=j01


# How many hours between analysis cycles?
CycleHrs=1 # Must be an integer in the range [1,47]

# Input directory:
InRoot='/scratch1/BMC/chem-var/pagowski/DATA/OBS/VIIRS_lunar'

# Output directory. Will receive one subdirectory per cycle:
OutRoot='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS/viirs2aeronet_lunar'

#AERONET file
AeronetRoot='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/AERONET_lunar_AOD15'

verbose=NO # YES = very wordy; NO = normal

# The viirs2aeronet.x executable. Make sure the "Prepare the environment"
# section correctly prepares the environment (module load, LD_LIBRARY_PATH)
viirs2aeronet='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/viirs2aeronet_lunar.x'

#@mzp
#ignore_errors=NO # if YES, keep going no matter what. If NO, exit on error.
ignore_errors=YES

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


# The Fortran datetime library must be in LD_LIBRARY_PATH:
#@mzp
#export LD_LIBRARY_PATH="/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime/lib:${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# The mpiserial and ncrcat must be in $PATH:
#export PATH="/scratch2/BMC/wrfruc/Samuel.Trahan/viirs-thinning/mpiserial/exec:$PATH"

export PATH="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec:$PATH"



# ----------------------------------------------------------------------


# Actual script begins here

# Make sure we have the required executables
#@mzp
#for exe in mpiserial ncrcat ; do
for exe in mpiserial ; do
    if ( ! which "$exe" ) ; then
        echo "Error: $exe is not in \$PATH. Go find it and rerun." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi
done

# Time calculations must be in UTC:
export TZ=UTC

# Make sure we have viirs
if [[ ! -e "$viirs2aeronet" || ! -x "$viirs2aeronet" ]] ; then
    echo "Error: viirs2aeronet is not an executable file: $viirs2aeronet" 1>&2
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

    aeronetfile=${AeronetRoot}/aeronet_aod.${validtime}.nc

    # Prepare the list of commands to run.
    cat /dev/null > cmdfile
    for f in "${usefiles[@]}" ; do
        fout=$( basename "$f" )
        echo "$viirs2aeronet" "$validtime" "$aeronetfile" "$f" "$fout" >> cmdfile
        file_count=$(( file_count + 1 ))
    done

    . ~/mapp_2018/.environ.ksh


cat > common.nl <<EOF
&common_params
radius=${radius}
tdiff_aeronet_max=${tdiff_aeronet_max}
tdiff_v2a_max=${tdiff_v2a_max}
satellite='${satellite}'
/
EOF

echo 'ABC'

    # Run many tasks in parallel via mpiserial.
    echo "Now running executable $viirs2aeronet"
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


    . ~/.nc

    FinalFile="viirs2aeronet_aod_${satellite}.${validtime}.nc"
    echo Merging files now...
    if ( ! ncrcat -O *${satellite}*.nc ${FinalFile} ) ; then
        echo "ncrcat returned non-zero exit status" 1>&2
        echo "ncrcat did not create ${FinalFile} ." 1>&2
        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
    fi

    /bin/rm -f VIIRS_lunar_${satellite}_s*.nc

    # Make sure they really were merged.
#    if [[ ! -s tmp.nc ]] ; then
#        echo "Error: ncrcat did not create ${FinalFile} ." 1>&2
#        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
#    fi

#    ncks -O --fix_rec_dmn all tmp.nc "${FinalFile}"

#    /bin/rm tmp.nc

#    FinalFile="viirs2aeronet_aod_j01.${validtime}.nc"
#    echo Merging files now...
#    if ( ! ncrcat -O *j01*.nc tmp.nc ) ; then
#        echo "Error: ncrcat returned non-zero exit status" 1>&2
#        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
#    fi

    # Make sure they really were merged.
#    if [[ ! -s tmp.nc ]] ; then
#        echo "Error: ncrcat did not create ${FinalFile}." 1>&2
#        if [[ $ignore_errors == NO ]] ; then exit 1 ; fi
#    fi

#    ncks -O --fix_rec_dmn all tmp.nc "${FinalFile}"

#    /bin/rm tmp.nc JRR-AOD*.nc 

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
