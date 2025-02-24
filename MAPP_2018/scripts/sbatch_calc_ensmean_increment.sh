#!/bin/ksh 
#SBATCH -n 20
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J calc_ensmaenincr
#SBATCH -D /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/qslogs/%x.e%j

#sbatch --export=ALL,ident=2018041500 sbatch_calc_ensmean_increment.sh

. /etc/profile

. ~/MAPP_2018/.environ_ksh

set -x

nanals=20

MAINDIR=/scratch1/NCEPDEV/da/Cory.R.Martin/aero_c96_jedi3densvar
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/tmpdir
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/from_cory/ensmean
EXEC=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/calc_ensmean_fv3.x

cycle_frequency=6

if [[ $nanals -gt $SLURM_NTASKS ]]
then
    echo "SLURM_NTASKS=$SLURM_NTASKS < nanals=$nanals - Stopping"
    exit 1
fi

ndate=~/bin/ndate

analdate=$ident

yeara=`echo $analdate | cut -c1-4`
montha=`echo $analdate | cut -c5-6`
daya=`echo $analdate | cut -c7-8`
houra=`echo $analdate | cut -c9-10`
adatea=${yeara}${montha}${daya}

fcstdate=`$ndate -$cycle_frequency $ident`
yearf=`echo $fcstdate | cut -c1-4`
monthf=`echo $fcstdate | cut -c5-6`
dayf=`echo $fcstdate | cut -c7-8`
hourf=`echo $fcstdate | cut -c9-10`
adatef=${yearf}${monthf}${dayf}

INDIR=${MAINDIR}/enkfgdas.${adatef}/${hourf}

workdir=${TMPDIR}/workdir_calc_ensmean_increment/${analdate}

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > ensmean.nml <<EOF
&ensmean_nml
varnames = 'bc1','bc2','oc1','oc2','sulf','dust1','dust2','dust3','dust4','dust5','seas1','seas2','seas3','seas4'
/
EOF

itile=1
while [[ $itile -le 6 ]]
do

    echo $itile
    /bin/cp ${INDIR}/mem001/RESTART/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc restart.ensmean
    chmod u+w restart.ensmean

    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${INDIR}/${charnanal}/RESTART
	ln -sf ${indir}/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc restart.${charnanal}
	if [[ ! -s restart.${charnanal} ]]
	then
	    echo "Missing fv3 restart output for member ${charnanal} tile ${itile} - Stopping"
	    exit 1
	fi
	((nanal=nanal+1))
    done

    echo "mpirun -np $nanals ${EXEC} $nanals restart"
    mpirun -np $nanals ${EXEC} $nanals restart

    if [[ $? != 0 ]]
    then
        echo "calc_ensmean failed - Stopping"
        exit 1
    fi

    ncdiff -O restart.ensmean ${INDIR}/ensmean/RESTART/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc ${adatea}.${houra}0000.fv_tracer.incr.tile${itile}.nc

    ncpdq -O -a -zaxis_1  ${adatea}.${houra}0000.fv_tracer.incr.tile${itile}.nc ${adatea}.${houra}0000.fv_tracer.incr.tile${itile}_rev.nc

    /bin/mv *incr.tile${itile}.nc *incr.tile${itile}_rev.nc $OUTDIR

    ncpdq -O -a -zaxis_1  restart.ensmean restart.ensmean_rev

    /bin/mv restart.ensmean $OUTDIR/${adatea}.${houra}0000.fv_tracer.anal.tile${itile}.nc
    /bin/mv restart.ensmean_rev $OUTDIR/${adatea}.${houra}0000.fv_tracer.anal.tile${itile}_rev.nc

    ((itile=itile+1))
done

exit 0
