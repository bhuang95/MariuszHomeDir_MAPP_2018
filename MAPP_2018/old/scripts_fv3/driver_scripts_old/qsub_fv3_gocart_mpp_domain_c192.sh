#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/fv3/driver_scripts
#PBS -N emiss_c192
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
GOCART_EMISS_EXEC=fv3_gocart_emiss_32bit.x

grid=${FV3_DATA}/${resolution}/grid

WORKDIR=${FV3_BASEDIR}/workdir_${resolution}_gocart_emiss

\rm -rf $WORKDIR/rundir

mkdir -p $WORKDIR/rundir
cd $WORKDIR/rundir

mkdir INPUT

ln -sf ${grid}/* ./INPUT
ln -sf ${GOCART_EMISS_DATA}/emiss_bburn_fv3_2015082600.nc ./INPUT
#ln -sf ${GOCART_EMISS_DATA}/*.nc ./INPUT

cores_per_node="24"

npes=$PBS_NP

io_layout="1,1"

# blocking factor used for threading and general physics performance
nxblocks="1"
nyblocks="24"

/bin/cp ${GOCART_EMISS_EXEC_DIR}/${GOCART_EMISS_EXEC} .

src_file=emiss_bburn_fv3_2015082600.nc
dst_file=emiss_bburn.tile1.nc

cat > input.nml <<EOF
&test_horiz_interp_nml
src_file = "INPUT/${src_file}"
dst_grid = "INPUT/grid_spec.nc"
field_name="eb_bc"
dst_file = "${dst_file}"
new_missing_handle = F
missing_permit = 3
!interp_method = "conservative" 
!- segfaults
interp_method = "bilinear"
/

&interpolator_nml
interp_method='conserve_great_circle'
/

EOF

npes=8
mpirun -n $npes ${GOCART_EMISS_EXEC} | tee fms.out


exit

month=1
while [[ $month -le 12 ]]
do
    cmonth=`printf %02i $month`
    mkdir $cmonth
    /bin/mv emiss_edgar_anthro_${cmonth}.tile?.nc $cmonth
    ((month=month+1))
done

exit

if [[ $? != 0 ]]
then
    echo "FV3 failed"
    exit
fi

/bin/mv ${analdate}*.nc diag_table field_table fms.out input.nml logfile.000000.out time_stamp.out $OUTDIR




