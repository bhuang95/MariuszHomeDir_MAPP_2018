#!/bin/ksh

wrf=1
test=test_301
wrf=0
test=DA_ENKF

network="surfrad"
#network="aeronet"

if [[ $wrf == 1 ]]
then
    MODEL="WRF"
    fcst_length=24
    fcst_increment=1
    cycle_frequency=24
    output_increment=1
    mincount=4
    start_ident=2015080318
    end_ident=2015083018
else
    MODEL="FV3"
    fcst_length=9
    fcst_increment=1
    cycle_frequency=6
#    cycle_frequency=24
    output_increment=1
    mincount=0
    start_ident=2015080318
    end_ident=2015080718
fi

var2d="AOD"

EVALDIR=/scratch3/BMC/chem-var/pagowski/eval/gmac_2018
RUNDIR=${EVALDIR}/tmp

STATSDIR=${EVALDIR}/${test}/point_stats_aod_${network}

configfile=${RUNDIR}/STATAnalysisConfig_aod.tmp


#set -x

ndate=~/bin/ndate

if [ ! -r ${STATSDIR} ]
    then
    echo "Pointstat output not available"
    exit 1
fi

rm -f ${STATSDIR}/stats_analysis_${var2d}_all.txt

if [ ! -r ${RUNDIR} ]
    then
    mkdir -p ${RUNDIR}
fi

cd ${RUNDIR}

ident=$start_ident

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hour=`echo "${ident}" | cut -c9-10`

fcst_hour=0

i=0

while [[ $fcst_hour -le $fcst_length ]]
do
    
    cfcst_hour="`printf %02i $fcst_hour`"
    
cat > $configfile << EOF

model = "${MODEL}";
	
desc = "NA";

fcst_lead = ["$cfcst_hour"];
obs_lead  = [];

fcst_valid_beg  = "";
fcst_valid_end  = "";
fcst_valid_hour = [];

obs_valid_beg   = "";
obs_valid_end   = "";
obs_valid_hour  = [];

fcst_init_beg   = "";
fcst_init_end   = "";
fcst_init_hour  = [];

obs_init_beg    = "";
obs_init_end    = "";
obs_init_hour   = [];

fcst_var = ["$var2d"];
obs_var  = ["$var2d"];

fcst_lev = [];
obs_lev  = [];

obtype = [];

vx_mask = [];

interp_mthd = [];

interp_pnts = [];

fcst_thresh = [];
obs_thresh  = [];
cov_thresh  = [];

alpha = [];

line_type = [];

column = [];

weight = [];

////////////////////////////////////////////////////////////////////////////////

//
// Array of STAT-Analysis jobs to be performed on the filtered data
//
jobs = [
   "-job summary -column CNT:FBAR,CNT:OBAR,CNT:ME,CNT:BCMSE,CNT:RMSE,CNT:PR_CORR,CNT:FSTDEV,CNT:OSTDEV,CNT:ESTDEV -dump_row ${STATSDIR}/dump_stats_analysis_${var2d}_${cfcst_hour}.txt"
];

//jobs = [
//   "-job filter -dump_row ${STATSDIR}/stats_analysis_${var2d}.txt"
//];
////////////////////////////////////////////////////////////////////////////////

//
// Confidence interval settings
//
out_alpha = 0.05;

boot = {
   interval = PCTILE;
   rep_prop = 1.0;
   n_rep    = 0;
   rng      = "mt19937";
   seed     = "";
}

////////////////////////////////////////////////////////////////////////////////

//
// WMO mean computation logic
//
wmo_sqrt_stats   = [ "CNT:FSTDEV",  "CNT:OSTDEV",  "CNT:ESTDEV",
                     "CNT:RMSE",    "CNT:RMSFA",   "CNT:RMSOA",
                     "VCNT:FS_RMS", "VCNT:OS_RMS", "VCNT:RMSVE",
                     "VCNT:FSTDEV", "VCNT:OSTDEV" ];

wmo_fisher_stats = [ "CNT:PR_CORR", "CNT:SP_CORR",
                     "CNT:KT_CORR", "CNT:ANOM_CORR" ];



////////////////////////////////////////////////////////////////////////////////

rank_corr_flag = FALSE;
vif_flag       = FALSE;
tmp_dir        = "/tmp";
version        = "V7.0";

////////////////////////////////////////////////////////////////////////////////



EOF

    stat_analysis -lookin $STATSDIR -out ${STATSDIR}/stats_analysis_${var2d}_full_${cfcst_hour}.txt -config $configfile -v 4

    if [[ -s ${STATSDIR}/stats_analysis_${var2d}_full_${cfcst_hour}.txt ]]
	then
	count=`sed '1,2d' ${STATSDIR}/stats_analysis_${var2d}_full_${cfcst_hour}.txt \
            | head -n1 | cut -c32-33`

	if [[ ${count} -ge ${mincount} ]] then
	    echo ${cfcst_hour} >>  ${STATSDIR}/stats_analysis_${var2d}_all.txt
	    sed '1,2d' ${STATSDIR}/stats_analysis_${var2d}_full_${cfcst_hour}.txt \
		| cut -c 21-28,34-42 >>  ${STATSDIR}/stats_analysis_${var2d}_all.txt
	fi
    fi

    ((i=i+1))
	
    ((fcst_hour=i*output_increment))

done
      
