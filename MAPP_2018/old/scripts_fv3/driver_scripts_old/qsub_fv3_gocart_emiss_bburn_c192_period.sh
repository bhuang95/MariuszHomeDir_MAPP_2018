#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/fv3/driver_scripts
#PBS -N femiss_c192
##PBS -q batch
##PBS -q urgent
#PBS -q debug
#PBS -A chem-var
##PBS -l walltime=02:00:00
#PBS -l walltime=00:10:00
#PBS -l nodes=1:ppn=24
##PBS -l nodes=4:ppn=24
##PBS -l nodes=2:ppn=12 #Phil
##PBS -j oe #e and o outputs in the same file

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge

. ./environ.sh

. ${FV3_BUILD}/../site/env.kshrc

module load nco

echo "MODULE LIST"

resolution="C192"

GOCART_EMISS_DATA=${FV3_BASEDIR}/FIRE_DATA/FIRE_NCDF
GOCART_EMISS_DIR=${FV3_BASEDIR}/sorc/fv3gfs.fd/DA_gocart_emiss/inbound
GOCART_EMISS_EXEC_DIR=${GOCART_EMISS_DIR}/BUILD/bin
GOCART_EMISS_EXEC=fv3_gocart_emiss_bburn_32bit.x
OUTDIR=${FV3_BASEDIR}/FV3_DATA/${resolution}

grid=${FV3_DATA}/${resolution}/grid

WORKDIR=${FV3_BASEDIR}/workdir_${resolution}_gocart_emiss

. ~/bin/funcs.sh

startdate=2015080100
enddate=2015090100

increment=24

\rm -rf $WORKDIR/rundir

mkdir -p $WORKDIR/rundir
cd $WORKDIR/rundir

mkdir INPUT

ln -sf ${grid}/* ./INPUT

cores_per_node="24"

npes=$PBS_NP

io_layout="1,1"

# blocking factor used for threading and general physics performance
nxblocks="1"
nyblocks="24"

/bin/cp ${GOCART_EMISS_EXEC_DIR}/${GOCART_EMISS_EXEC} .

src_file=emiss_bburn_fv3.nc
dst_file=emiss_bburn.tile1.nc

cat > input.nml <<EOF
&test_horiz_interp_nml
src_file = "INPUT/${src_file}"
dst_grid = "INPUT/grid_spec.nc"
field_name="eb_bc"
dst_file = "${dst_file}"
new_missing_handle = F
missing_permit = 3
!interp_method = "conservative" - segfaults
interp_method = "bilinear"
/

&interpolator_nml
interp_method='conserve_great_circle'
/

EOF

npes=1

current_date=${startdate}

while [[ ${current_date} -le ${enddate} ]]
do

    echo $current_date

    year=`echo $current_date | cut -c1-4`
    month=`echo $current_date | cut -c5-6`
    day=`echo $current_date | cut -c7-8`
    hr=`echo $current_date | cut -c9-10`

    outdir=${OUTDIR}/${current_date}/emiss_bioburn_data
    mkdir -p $outdir

    ln -sf ${GOCART_EMISS_DATA}/emiss_bburn_fv3_${current_date}.nc ./INPUT/emiss_bburn_fv3.nc
    mpirun -n $npes ${GOCART_EMISS_EXEC} | tee fms.out

    ctile=1
    while [[ ctile -le 6 ]]
    do
	/bin/mv emiss_bburn.tile${ctile}.nc ${outdir}/emiss_bioburn.tile${ctile}_${current_date}.nc
	((ctile=ctile+1))
    done

    /bin/mv fms.out ${outdir}

    increment_date $increment

    current_date=${end_year}${end_month}${end_day}${end_hr}
    
done

exit



