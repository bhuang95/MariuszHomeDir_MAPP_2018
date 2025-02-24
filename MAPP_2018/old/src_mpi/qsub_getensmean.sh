#!/bin/ksh --login
#PBS -N ensmean
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=12
#PBS -q debug
#PBS -l walltime=00:10:00
#PBS -d /scratch1/portfolios/BMC/chem-var/pagowski/codes/src_mpi
#PBS -o /scratch1/portfolios/BMC/chem-var/pagowski/codes/src_mpi/qslogs
#PBS -e /scratch1/portfolios/BMC/chem-var/pagowski/codes/src_mpi/qslogs

. ./getensmean.sh
