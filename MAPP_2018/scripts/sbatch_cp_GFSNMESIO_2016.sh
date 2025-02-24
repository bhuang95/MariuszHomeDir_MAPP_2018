#!/bin/ksh --login
#SBATCH -J getgfs_c
#SBATCH -A wrf-chem
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -p service
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

#GFS and EnKF gdas files in 2016 were distribute at different $hpssTop/$oldexp directory
#Copy gdas files from $hpssTop/$oldexp to ${mineTop}
oldexp=pr4rn_1605 
oldexp=pr4rn_1505 
oldexp=prnemsrn
cdump=gdas
hpssTop=/5year/NCEPDEV/emc-global/emc.glopara/WCOSS_C/${oldexp}
mineTop=/BMC/fim/5year/MAPP_2018/MET_ANALYSES

#Set up the starting and end cycles
dateS=2016010100
dateE=2016123118

dateInc=6
ndate=/home/Mariusz.Pagowski/bin

dateL=${dateS}

while [ ${dateL} -le ${dateE} ]; do
    echo ${dateL}
    YY=`echo ${dateL} | cut -c1-4`
    MM=`echo ${dateL} | cut -c5-6`
    DD=`echo ${dateL} | cut -c7-8`
    HH=`echo ${dateL} | cut -c9-10`

    YM=${YY}${MM}
    YMD=${YY}${MM}${DD}

    hpssDay=${hpssTop}/

    # Copy GFS gdas file
    fileName=${YMD}${HH}${cdump}.tar
    hpssFile=${hpssDay}/${fileName}
    mineFile=${mineTop}/${fileName}

    hsi "cp ${hpssFile} ${mineFile}"

    status=$?
    if [[ $status -eq 0 ]]; then
       echo "Yes" > ./cycle-cntl-${dateL}.txt
    else
       exit $status
    fi

    # Copy GFS gdas *.idx file
    fileName=${YMD}${HH}${cdump}.tar.idx
    hpssFile=${hpssDay}/${fileName}
    mineFile=${mineTop}/${fileName}

    hsi "cp ${hpssFile} ${mineFile}"

    status=$?
    if [[ $status -eq 0 ]]; then
       echo "Yes" > ./cycle-cntlidx-${dateL}.txt
    else
       exit $status
    fi

    # Copy EnKF gdas file
    fileName=${YMD}${HH}${cdump}.enkf.anl.tar
    hpssFile=${hpssDay}/${fileName}
    mineFile=${mineTop}/${fileName}

    hsi "cp ${hpssFile} ${mineFile}"

    status=$?
    if [[ $status -eq 0 ]]; then
       echo "Yes" > ./cycle-enkf-${dateL}.txt
    else
       exit $status
    fi

    # Copy EnKF gdas .idx file
    fileName=${YMD}${HH}${cdump}.enkf.anl.tar.idx
    hpssFile=${hpssDay}/${fileName}
    mineFile=${mineTop}/${fileName}

    hsi "cp ${hpssFile} ${mineFile}"

    status=$?
    if [[ $status -eq 0 ]]; then
       echo "Yes" > ./cycle-enkfidx-${dateL}.txt
    else
       exit $status
    fi


    dateL=`$ndate ${dateInc} ${dateL}`

done

exit 0;
