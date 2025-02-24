#!/bin/ksh --login
#PBS -l nodes=1:ppn=24
#PBS -N chgres
##PBS -A chem-var
##PBS -A gsd-fv3-dev
#PBS -A wrf-chem
#PBS -l walltime=08:00:00
##PBS -l walltime=00:10:00
##PBS -q debug
##PBS -q urgent
#PBS -q batch
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#-----------------------------------------------------------------
# Run chgres using input gfs data in old format (sigio/sfcio)
#-----------------------------------------------------------------
#to create FV3 inputs from gfs files 
#does not source environ to set its own values of GFS resolution


. /etc/profile
. /apps/lmod/lmod/init/sh

set -x

start_date=2015081918
enddate=2015090100

module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
module load netcdf/4.3.0
module load hdf5/1.8.14

. ./environ.sh

nanals=$NANALS

MAINDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS
export BASEDIR=${MAINDIR}/SOURCES/global_shared.v15.0.0
export CHGRESEXEC=$BASEDIR/exec/global_chgres
export CHGRESSH=$BASEDIR/ush/global_chgres.sh

export CRES=$RES
export INDIR=${MAINDIR}/GFS/indata_spectral_ensemble
OUTDIR_BASE=${MAINDIR}/FV3_RUNS/GFS_cold

export FIXgsm=$FIXGLOBAL

export VERBOSE=YES

export JCAP=574
export LONB=1152
export LATB=576
export LEVS=64
export LSOIL=4
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=2

# fixed fields describing fv3 grid
export FIXfv3=${MAINDIR}/FV3_FIX/C${RES}
export FV3GRID_TILE1=$FIXfv3/C${RES}_grid.tile1.nc
export FV3GRID_TILE2=$FIXfv3/C${RES}_grid.tile2.nc
export FV3GRID_TILE3=$FIXfv3/C${RES}_grid.tile3.nc
export FV3GRID_TILE4=$FIXfv3/C${RES}_grid.tile4.nc
export FV3GRID_TILE5=$FIXfv3/C${RES}_grid.tile5.nc
export FV3GRID_TILE6=$FIXfv3/C${RES}_grid.tile6.nc
export FV3OROG_TILE1=$FIXfv3/C${RES}_oro_data.tile1.nc
export FV3OROG_TILE2=$FIXfv3/C${RES}_oro_data.tile2.nc
export FV3OROG_TILE3=$FIXfv3/C${RES}_oro_data.tile3.nc
export FV3OROG_TILE4=$FIXfv3/C${RES}_oro_data.tile4.nc
export FV3OROG_TILE5=$FIXfv3/C${RES}_oro_data.tile5.nc
export FV3OROG_TILE6=$FIXfv3/C${RES}_oro_data.tile6.nc

export FV3GRID_TILE=${FIXfv3}/C${RES}_grid.tile
export FV3OROG_TILE=${FIXfv3}/C${RES}_oro_data.tile

# to use with old albedo, soil/veg type
#export SIGLEVEL=${FIXgsm}/global_hyblev3.l${LEVS}.txt
export IALB=0
export FNSMCC=${FIXgsm}/global_soilmcpc.1x1.grb

export OMP_NUM_THREADS_CH=24
export OMP_NUM_THREADS=24

. ~/bin/funcs.sh


year=`echo $start_date | cut -c1-4`
month=`echo $start_date | cut -c5-6`
day=`echo $start_date | cut -c7-8`
hr=`echo $start_date | cut -c9-10`

CDATE=$start_date

# input data - old sigio/sfcio format (with old albedo)

while [[ $CDATE -le $enddate ]]
do
    DATE8=$(echo $CDATE | cut -c1-8)
    HOUR=$(echo $CDATE | cut -c9-10)
    
# work directories
    
    export SAVDIR=${MAINDIR}/tmp_dirs/workdir_chgres_savdir
    if [[ ! -r $SAVDIR ]]
    then
        mkdir -p $SAVDIR
    fi

    export DATA=${MAINDIR}/tmp_dirs/workdir_chgres_workdir
    if [[ ! -r $DATA ]]
    then
        mkdir -p $DATA
    fi

    
# Set chgres executable and script
    
#-----------------------------------
# convert atmospheric (sigma) file.
#-----------------------------------

    export CHGRESVARS="use_ufo=.false.,nst_anl=.false.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=.true."
    
    nanal=1
    
    while [[ $nanal -le $nanals ]]
    do

	cd $DATA
	
	/bin/rm *
	
	for tile in '1' '2' '3' '4' '5' '6'
	do
            ln -sf ${FV3GRID_TILE}${tile}.nc chgres.fv3.grd.t${tile}
	done

	charnanal=mem`printf %03i $nanal`
	export SIGINP=$INDIR/sanl_${CDATE}_${charnanal}
	export SFCINP=/dev/null
	export NSTINP=NULL
	
	echo "charnanal=$charnanal"

	$CHGRESSH > sigma_chgres.out
	rc=$?
	if [[ $rc -ne 0 ]] ; then
	    echo "***ERROR*** rc= $rc"
	    exit 1
	fi

	outdir=${OUTDIR_BASE}/${CDATE}/${charnanal}
	
	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi
    
	
	for tile in '1' '2' '3' '4' '5' '6'
	do
	    mv ${DATA}/gfs_data.tile${tile}.nc ${outdir}/gfs_data.tile${tile}_${charnanal}.nc
	done	    


#------------------------------------------
# convert surface file one tile at a time.
#------------------------------------------
	    
	export CHGRESVARS="use_ufo=.false.,nst_anl=.false.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=.true."
	
	export SIGINP=NULL
	export SFCINP=$INDIR/sfcanl_${CDATE}_${charnanal}
	export NSTINP=NULL
	
	for tile in '1' '2' '3' '4' '5' '6'
	do

	    ln -sf ${FV3OROG_TILE}${tile}.nc chgres.fv3.orog.t${tile}
	    
	    export TILE_NUM=$tile

	    $CHGRESSH > sfc_chgres.out
	    
	    rc=$?
	    if [[ $rc -ne 0 ]] ; then
		echo "***ERROR*** rc= $rc"
		exit 1
	    fi
	    
	    /bin/mv ${DATA}/out.sfc.tile$tile.nc ${outdir}/sfc_data.tile${tile}_${charnanal}.nc

#only for tile 1
	    /bin/mv ${DATA}/gfs_ctrl.nc ${outdir}

	done
	    
	((nanal=nanal+1))

    done
    
    increment_date 6
    
    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}
    
    CDATE=${year}${month}${day}${hr}
    
    echo $CDATE

done
