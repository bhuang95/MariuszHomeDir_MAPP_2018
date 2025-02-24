#!/bin/ksh

expName=aero_c96_jedi3densvar
gridDir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/GSDChem_cycling/global-workflow/fix/fix_fv3/C96
expDir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expRuns/${expName}/dr-data/


stCyc=2018041400
edCyc=2018042312
inCyc=6

incDate=/home/Bo.Huang/JEDI-2020/usefulScripts/incdate.sh

nwCyc=${stCyc}

while [[ ${nwCyc} -le ${edCyc} ]]
do
    ntCyc=`${incDate} ${nwCyc} ${inCyc}`

    nwY=`echo $nwCyc | cut -c1-4`
    nwM=`echo $nwCyc | cut -c5-6`
    nwD=`echo $nwCyc | cut -c7-8`
    nwH=`echo $nwCyc | cut -c9-10`

    ntY=`echo $ntCyc | cut -c1-4`
    ntM=`echo $ntCyc | cut -c5-6`
    ntD=`echo $ntCyc | cut -c7-8`
    ntH=`echo $ntCyc | cut -c9-10`

    echo ${nwCyc}

    indir=${DATADIR}

    export DATADIR="${expDir}/gdas.${nwY}${nwM}${nwD}/${nwH}/RESTART"
    export NCPREFIX="${ntY}${ntM}${ntD}.${ntH}0000"
    export GRIDDIR=${gridDir}


    
    #export TITLE="Seasalt-increment-${ntCyc}"
    #export LIMITLOW=-50
    #export LIMITHIGH=100
    #export NCONS=10
    #export CMAPSTR="MPL_bwr"
    #export SPACING=10
    #export SPECIES="seas_total"

    ##export TITLE="Dust increment   ${ident}"
    #export TITLE="Dust-inc-${ntCyc}"
    #export LIMITLOW=-10
    #export LIMITHIGH=50
    #export NCONS=10
    #export CMAPSTR="MPL_bwr"
    #export SPACING=5
    #export SPECIES="dust_total"

    #export TITLE="Carbonaceus increment   ${ident}"
#    export TITLE="Carbonaceus-inc-${ntCyc}"
#    export LIMITLOW=-12
#    export LIMITHIGH=12
#    export NCONS=60
#    export CMAPSTR="BlueWhiteOrangeRed" #"BlAqGrWh2YeOrReVi22" #"MPL_bwr"
#    export SPACING=2
#    export SPECIES="carbon_total"
#
#    mkdir -p ${SPECIES}
#    ncl plotit_globe_increment_total_Bo.ncl
#    /bin/mv ${TITLE}.png ${SPECIES}

#    export TITLE="Dust-inc-${ntCyc}"
#    export LIMITLOW=-50
#    export LIMITHIGH=50
#    export NCONS=60
#    export CMAPSTR="BlueWhiteOrangeRed" #"BlAqGrWh2YeOrReVi22" #"MPL_bwr"
#    export SPACING=2
#    export SPECIES="dust_total"
#
#    mkdir -p ${SPECIES}
#    ncl plotit_globe_increment_total_Bo.ncl
#    /bin/mv ${TITLE}.png ${SPECIES}

    export TITLE="Seasalt-inc-${ntCyc}"
    export LIMITLOW=-100
    export LIMITHIGH=100
    export NCONS=60
    export CMAPSTR="BlueWhiteOrangeRed" #"BlAqGrWh2YeOrReVi22" #"MPL_bwr"
    export SPACING=2
    export SPECIES="seas_total"

    mkdir -p ${SPECIES}
    ncl plotit_globe_increment_total_Bo.ncl
    /bin/mv ${TITLE}.png ${SPECIES}


    nwCyc=`${incDate} ${nwCyc} ${inCyc}`

done
