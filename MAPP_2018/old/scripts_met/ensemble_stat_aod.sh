#!/bin/ksh

#test=test_301
#wrf=1
wrf=0
test=DA_ENKF

#network='surfrad'
network='aeronet'

if [[ $wrf == 1 ]]
then
    INFCSTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all/${test}/Output
    model="wrf"
    MODEL="WRF"
    prefix=aod
    fcst_length=24
    cycle_frequency=24
    output_increment=1
    start_ident=2015080318
    end_ident=2015083018
    nens=0
else
    INFCSTDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/${test}
    model="fv3"
    MODEL="FV3"
    prefix=aod
    fcst_length=9
    cycle_frequency=6
#    cycle_frequency=24
    output_increment=1
    start_ident=2015080118
    end_ident=2015081018
    nens=20
fi

EVALDIR=/scratch3/BMC/chem-var/pagowski/eval/gmac_2018
INOBSDIR=/scratch3/BMC/chem-var/pagowski/gmac_2018/${network}
RUNDIR=${EVALDIR}/tmp

STATSDIR=${EVALDIR}/${test}/ensemble_stats_aod_${network}

configfile=${RUNDIR}/EnsembleStatConfig_aod.tmp
prefix_stat="ensemble_stat"


#set -x

ndate=~/bin/ndate


if [ ! -r ${STATSDIR} ]
    then
    mkdir -p ${STATSDIR}
fi

if [ ! -r ${RUNDIR} ]
    then
    mkdir -p ${RUNDIR}
fi

cd ${RUNDIR}

var2d="AOD"
obsvar2d="AOD" 
lev2d="Z0"  

/bin/rm -f ${STATSDIR}/*_cnt.txt ${STATSDIR}/*_sl1l2.txt ${STATSDIR}/*.stat

ident=$start_ident

while [[ $ident -le $end_ident ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    if [[ $model == 'wrf' ]]
    then
	ident_dir=${year}_${month}_${day}_${hour}
	indir=${INFCSTDIR}/${ident_dir}
    else
	ident_dir=${year}${month}${day}${hour}
	indir=${INFCSTDIR}/${ident_dir}/GRIB
    fi

    fcst_hour=1

    i=0

    while [[ $fcst_hour -le $fcst_length ]]
    do
	
	cfcst_hour="`printf %02i $fcst_hour`"
	modelfiles=${indir}/${prefix}_mem???_${ident}.grib

	chour="`printf %02i $hour`"

	obsfile=${INOBSDIR}/aod550_4met.nc

#	echo @@@ $ident $fcst_hour $obsfile

	if [ ! -r $obsfile ]
	then
	    echo "Missing $obsfile" 
	    echo "Skipping ${end_year}${end_month}${end_day}${end_hour}"
	    continue
	fi


#	newdate=`$ndate $fcst_hour ${ident}`
#	newyear=`echo "${newdate}" | cut -c1-4`
#	newmonth=`echo "${newdate}" | cut -c5-6`
#	newday=`echo "${newdate}" | cut -c7-8`
#	newhour=`echo "${newdate}" | cut -c9-10`
#	echo $newdate $newyear $newmonth $newday $newhour

	
	
cat > $configfile << EOF

model = "${MODEL}";
	
desc = "NA";

regrid = {
   to_grid    = NONE;
   method     = NEAREST;
   width      = 1;
   vld_thresh = 0.5;
   shape      = SQUARE;
}

censor_thresh = [];
censor_val    = [];
cat_thresh    = [];
nc_var_str    = "";


ens = {
   ens_thresh = 1.0;
   vld_thresh = 1.0;

   field = [
      {
         name       = "${var2d}";
         level      = "${lev2d}";
         cat_thresh = [ < 5.0 ];
      }
   ];
}




fcst = {
   field = [
      {
         name       = "${var2d}";
         level      = "${lev2d}";
         cat_thresh = [ < 5.0 ];
      }
   ];
};


fcst = {
   init_time = "${year}${month}${day}_${hour}";
   lead_time = "$fcst_hour";
}


obs = fcst;
message_type = [ "ADPSFC" ];
sid_exc        = [];
obs_quality    = [];
duplicate_flag = NONE;
obs_summary    = NONE;
obs_perc_value = 50;

message_type_group_map = [
   { key = "SURFACE"; val = "ADPSFC,SFCSHP,MSONET";               },
   { key = "ANYAIR";  val = "AIRCAR,AIRCFT";                      },
   { key = "ANYSFC";  val = "ADPSFC,SFCSHP,ADPUPA,PROFLR,MSONET"; },
   { key = "ONLYSF";  val = "ADPSFC,SFCSHP";                      }
];

ens_ssvar_bin_size = 1.0;
ens_phist_bin_size = 0.05;


obs_window = {
   beg = -1800;
   end =  1800;
}

mask = {
   grid = [ "FULL" ];
   poly = [];
   sid  = "";
};

ci_alpha  = [ 0.05 ];

interp = {
   vld_thresh = 1.0;

   type = [
      {
         method = DW_MEAN;
         width  = 2;
      }
   ];
};

output_flag = {
   rhist = BOTH;
   phist = BOTH;
   orank = BOTH;
   ssvar = BOTH;
   relp  = BOTH;
};

tmp_dir        = "/tmp";
output_prefix  = "${var2d}";
version        = "V7.0";

EOF

        ensemble_stat $nens $modelfiles $configfile \
	    -point_obs $obsfile -outdir $STATSDIR -v 4

	((i=i+1))
	
	((fcst_hour=i*output_increment))

    done

    exit

    ident=`$ndate +$cycle_frequency $ident`

done
      
