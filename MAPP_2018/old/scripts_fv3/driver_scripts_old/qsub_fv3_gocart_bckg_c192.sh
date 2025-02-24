#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/fv3/driver_scripts
#PBS -N gbckg_c192
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

GOCART_BCKG_DATA=${FV3_BASEDIR}/GOCART_BCKG_DATA
GOCART_BCKG_DIR=${FV3_BASEDIR}/sorc/fv3gfs.fd/DA_gocart_bckg/inbound
GOCART_BCKG_EXEC_DIR=${GOCART_BCKG_DIR}/BUILD/bin
GOCART_BCKG_EXEC=fv3_gocart_bckg_32bit.x

grid=${FV3_DATA}/${resolution}/grid

WORKDIR=${FV3_BASEDIR}/workdir_${resolution}_gocart_bckg

\rm -rf $WORKDIR/rundir

mkdir -p $WORKDIR/rundir
cd $WORKDIR/rundir

mkdir INPUT

ln -sf ${grid}/* ./INPUT
ln -sf ${GOCART_BCKG_DATA}/* ./INPUT

((LEVP = $LEVS + 1))

cores_per_node="24"

npes=$PBS_NP

io_layout="1,1"

# blocking factor used for threading and general physics performance
nxblocks="1"
nyblocks="24"

/bin/cp ${GOCART_BCKG_EXEC_DIR}/${GOCART_BCKG_EXEC} .

month=1

while [[ $month -le 12 ]]
do
    cmonth=`printf %02i $month`
    src_file=gmi_2006${cmonth}.nc
    dst_file=gocart_bckg_${cmonth}.tile1.nc

cat > input.nml <<EOF
&test_horiz_interp_nml
src_file = "INPUT/${src_file}"
dst_grid = "INPUT/grid_spec.nc"
field_name="h2o2"
dst_file = "${dst_file}"
new_missing_handle = F
missing_permit = 3
interp_method = "bilinear"
/

&interpolator_nml
interp_method='conserve_great_circle'
/

EOF

    npes=1
    mpirun -n $npes ${GOCART_BCKG_EXEC} | tee fms.out

    for file in gocart_bckg_${cmonth}.tile?.nc
    do
	ncpdq -O -a '-zaxis_1' $file ${file}_z_reversed
    done

    mkdir $cmonth
    /bin/mv gocart_bckg_${cmonth}.tile?.nc* $cmonth

    ((month=month+1))

done

exit

if [[ $? != 0 ]]
then
    echo "FV3 failed"
    exit
fi

/bin/mv ${analdate}*.nc diag_table field_table fms.out input.nml logfile.000000.out time_stamp.out $OUTDIR




