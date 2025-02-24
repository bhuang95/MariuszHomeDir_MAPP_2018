#!/bin/ksh 

. /etc/profile

. ~/.nc

sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201712
sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v14_0dz0dp_41M_C96_201712
sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201712
#sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v15_0dz0dp_1M_C96_202006
#sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v15_0dz0dp_41M_C96_202006
#sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202006

MAINDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/expRuns/UFS-Aerosols_RETcyc

INDIR=${MAINDIR}/${sim}/dr-data-longfcst-backup/"*"/00/diag/aod_grid
OUTDIR=${MAINDIR}/${sim}/clim_fcst

for fcst in `seq -f '%03.f' 0 6 120`
do
    indir="${INDIR}/FV3AOD_fv_tracer_*_fhr${fcst}"
    outdir=${OUTDIR}/fhr${fcst}

    echo fhr${fcst}

    if [[ ! -r ${outdir} ]]
    then
	mkdir -p ${outdir}
    fi

    itile=1
    while [[ $itile -le 6 ]]
    do
	ncea -O ${indir}/*fv_aod_LUTs.fv_tracer.res.tile${itile}.nc ${outdir}/fv_aod_fhr${fcst}_mean.res.tile${itile}.nc
	ncatted -O -a history,global,d,, ${outdir}/fv_aod_fhr${fcst}_mean.res.tile${itile}.nc
	((itile=itile+1))
    done
done

