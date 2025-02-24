#!/bin/ksh --login
#PBS -l nodes=1:ppn=24
#PBS -N chgres
#PBS -A chem-var
#PBS -l walltime=04:00:00
##PBS -l walltime=00:10:00
##PBS -q debug
#PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/qslogs

#-----------------------------------------------------------------
# Run chgres using input gfs data in old format (sigio/sfcio)
#-----------------------------------------------------------------
#to create FV3 inputs from gfs files 
#does not source environ to set its own values of GFS resolution


. /etc/profile
. /apps/lmod/lmod/init/sh

set -x

module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
module load netcdf/4.3.0
module load hdf5/1.8.14

MAINDIR=/scratch3/BMC/chem-var/pagowski/tmp
export BASEDIR=${MAINDIR}/chgres_fv3
export CHGRESEXEC=$BASEDIR/exec/global_chgres
export CHGRESSH=$BASEDIR/ush/global_chgres.sh

nanals=20

export CRES=192
export INDIR=${MAINDIR}/indata_gfs_spectral
FV3_BASE=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS
OUTDIR_BASE=${FV3_BASE}/FV3_RUNS/DA_ENKF

export FIXgsm=/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/trunk/global_shared.v15.0.0/fix/fix_am


export OMP_NUM_THREADS_CH=24
export OMP_NUM_THREADS=24

. ~/bin/funcs.sh

start_date=2015081006
enddate=2015081100

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
    
    export DATA=/scratch3/BMC/chem-var/pagowski/tmp/workdir_chgres_ensemble
    rm -fr $DATA
    mkdir -p $DATA

    
# Set chgres executable and script
    
    export VERBOSE=YES
    
    export JCAP=574  
    export LONB=1152 
    export LATB=576  
    export LEVS=64 
    export LSOIL=4
    export CLIMO_FIELDS_OPT=3
    export LANDICE_OPT=2
    
# fixed fields describing fv3 grid
    export FIXfv3=$BASEDIR/fix/C${CRES}
    export FV3GRID_TILE1=$FIXfv3/C${CRES}_grid.tile1.nc
    export FV3GRID_TILE2=$FIXfv3/C${CRES}_grid.tile2.nc
    export FV3GRID_TILE3=$FIXfv3/C${CRES}_grid.tile3.nc
    export FV3GRID_TILE4=$FIXfv3/C${CRES}_grid.tile4.nc
    export FV3GRID_TILE5=$FIXfv3/C${CRES}_grid.tile5.nc
    export FV3GRID_TILE6=$FIXfv3/C${CRES}_grid.tile6.nc
    export FV3OROG_TILE1=$FIXfv3/C${CRES}_oro_data.tile1.nc
    export FV3OROG_TILE2=$FIXfv3/C${CRES}_oro_data.tile2.nc
    export FV3OROG_TILE3=$FIXfv3/C${CRES}_oro_data.tile3.nc
    export FV3OROG_TILE4=$FIXfv3/C${CRES}_oro_data.tile4.nc
    export FV3OROG_TILE5=$FIXfv3/C${CRES}_oro_data.tile5.nc
    export FV3OROG_TILE6=$FIXfv3/C${CRES}_oro_data.tile6.nc
    
# to use with old albedo, soil/veg type
#export SIGLEVEL=${FIXgsm}/global_hyblev3.l${LEVS}.txt
    export IALB=0
    export FNSMCC=${FIXgsm}/global_soilmcpc.1x1.grb
    
# to use new albedo, soil/veg type
#export IALB=1
#export FIXgsm=/scratch4/NCEPDEV/da/save/George.Gayno/global_shared.v14.1.0_backup/fix/fix_am
#export FNSMCC=$FIXgsm/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb
#export FNSOTC=$FIXgsm/global_soiltype.statsgo.t${JCAP}.${LONB}.${LATB}.rg.grb
#export SOILTYPE_OUT=statsgo
#export FNVETC=$FIXgsm/global_vegtype.igbp.t${JCAP}.${LONB}.${LATB}.rg.grb
#export VEGTYPE_OUT=igbp
#export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t${JCAP}.${LONB}.${LATB}.rg.grb
#export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t${JCAP}.${LONB}.${LATB}.rg.grb
## needed for facsf and facwf
#export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
#export FNZORC=igbp


#-----------------------------------
# convert atmospheric (sigma) file.
#-----------------------------------

    export CHGRESVARS="use_ufo=.false.,nst_anl=.false.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=.true."
    
    nanal=1
    
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	export SIGINP=$INDIR/sanl_${CDATE}_${charnanal}
	export SFCINP=/dev/null
	export NSTINP=NULL
	
	echo "charnanal=$charnanal"

	$CHGRESSH
	
	rc=$?
	if [[ $rc -ne 0 ]] ; then
	    echo "***ERROR*** rc= $rc"
	    exit
	fi

	outdir=${OUTDIR_BASE}/${CDATE}/INPUT_cold/${charnanal}
	
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
	    
	    export TILE_NUM=$tile

	    $CHGRESSH
	    
	    rc=$?
	    if [[ $rc -ne 0 ]] ; then
		echo "***ERROR*** rc= $rc"
		exit
	    fi
	    

	    /bin/mv ${DATA}/out.sfc.tile$tile.nc ${outdir}/sfc_data.tile${tile}_${charnanal}.nc
	    
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
