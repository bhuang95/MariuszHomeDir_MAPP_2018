import sys,os
sys.path.append('/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/METplus-diag/METplus_pkg//pyscripts/lib')
#from subprocess import check_output as chkop
import subprocess as sbps
import numpy as np
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.dates import (DAILY, DateFormatter,
                              rrulewrapper, RRuleLocator)
from ndate import ndate
from datetime import datetime
from datetime import timedelta

fsave=1

expdir=sys.argv[1]
modelname=sys.argv[2]
obsname=sys.argv[3]
obsname1=sys.argv[4]
modelvar=sys.argv[5]
obsvar=sys.argv[6]
obsvar1=sys.argv[7]
area=sys.argv[8]
sa_path="%s/wrk-%s-%s/%s-%s/met_tool_wrapper/stat_analysis/" %(expdir, modelname, obsname, modelvar, obsvar) 
sa_path1="%s/wrk-%s-%s/%s-%s/met_tool_wrapper/stat_analysis/" %(expdir, modelname, obsname1, modelvar, obsvar1) 
#sa_path="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/METplus-diag/METplus_pkg//wrk-CAMS-FV3/DUSTTOTAL-DUSTTOTAL/met_tool_wrapper/stat_analysis"
#sa_path1="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/METplus-diag/METplus_pkg//wrk-CAMS-MERRA2/DUSTTOTAL-DUSTTOTAL/met_tool_wrapper/stat_analysis"
#area="SOCEAN"
#area="FULL"
#obsvar="DUSTTOTAL"
#area=sys.argv[2]
#obsvar=sys.argv[1]
linetype="SL1L2"
plev=(100, 250, 400, 500, 600, 700, 850, 925, 1000)
#plev=(100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 850, 900, 925, 950, 1000)

sdate=2016060100
edate=2016063018
inc_h=6

cdate=sdate
tmpfile="%s/%s/%s/%s_%s_%s.stat" %(sa_path,modelname,cdate,area,obsvar,linetype)
f=open(tmpfile,'r')
nlev=-1
for line in f.readlines():
    nlev=nlev+1
f.close()

dlist=[]
ntime=0
cdate=sdate
while (cdate<edate):
    filename="%s/%s/%s/%s_%s_%s.stat" %(sa_path,modelname,cdate,area,obsvar,linetype)
    ntime=ntime+1
    dlist.append(str(cdate))
    cdate=ndate(cdate,inc_h)

p_matched=np.zeros((ntime,nlev),dtype='int')
fbar=np.zeros((ntime,nlev),dtype='float')
obar=np.zeros_like(fbar)
obar1=np.zeros_like(fbar)

ntime=0
cdate=sdate
while (cdate<edate):
    filename="%s/%s/%s/%s_%s_%s.stat" %(sa_path,modelname,cdate,area,obsvar,linetype)
    if (cdate!=2018061000):
        f=open(filename,'r')
        nlev=-1
        for line in f.readlines():
            if (nlev==-1):
               nlev=nlev+1
               continue
            else:
               p_matched[ntime,nlev]=int(line.split()[24])
               fbar[ntime,nlev]=float(line.split()[25])
               obar[ntime,nlev]=float(line.split()[26])
               nlev=nlev+1
        f.close()
    else:
        p_matched[ntime,:]=p_matched[ntime-1,:]
        fbar[ntime,:]=float("nan")
        obar[ntime,:]=float("nan")

    filename1="%s/%s/%s/%s_%s_%s.stat" %(sa_path1,modelname,cdate,area,obsvar1,linetype)
    f1=open(filename1,'r')
    nlev=-1
    for line in f1.readlines():
        if (nlev==-1):
           nlev=nlev+1
           continue
        else:
           #p_matched[ntime,nlev]=int(line.split()[24])
           #fbar[ntime,nlev]=float(line.split()[25])
           obar1[ntime,nlev]=float(line.split()[26])
           nlev=nlev+1
    f.close()

    cdate=ndate(cdate,inc_h)
    ntime=ntime+1
print('get data')
#
# Plot data
#
syy=int(str(sdate)[:4]); smm=int(str(sdate)[4:6])
sdd=int(str(sdate)[6:8]); shh=int(str(sdate)[8:10])
eyy=int(str(edate)[:4]); emm=int(str(edate)[4:6])
edd=int(str(edate)[6:8]); ehh=int(str(edate)[8:10])

date1 = datetime(syy,smm,sdd,shh)
date2 = datetime(eyy,emm,edd,ehh)
delta = timedelta(hours=inc_h)
dates = mdates.drange(date1, date2, delta)

rule = rrulewrapper(DAILY, byhour=0, interval=5)
loc = RRuleLocator(rule)
formatter = DateFormatter('%Y%h %n %d %Hz')

print('get dates')

leglist=[obsname1,modelname,obsname]
print(leglist)

#sys.exit()
#
#  Timeseries
#
for nlv in np.arange(nlev):
    pltdata=np.zeros((ntime,3),dtype='float')
    pltdata[:,1]=fbar[:,nlv]
    pltdata[:,2]=obar[:,nlv]
    pltdata[:,0]=obar1[:,nlv]

    fig=plt.figure(figsize=(12,6))
    ax=plt.subplot()
    ax.set_prop_cycle(color=['red','blue','green'])
    #ax.plot_date(dates,pltdata,'o-')
    ax.plot_date(dates,pltdata,'o-', lw=0.5)
    ax.xaxis.set_major_locator(loc)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_tick_params(labelsize=14)
    ax.legend(leglist,fontsize=16)
    #ax.set_ylabel('Mixing Ratio [kg/kg]',fontsize=14)
    ax.set_ylabel('AOD',fontsize=14)
    ax.grid()
    #ax.set_title('P%s %s %s' %(str(plev[nlv]),area,obsvar),loc='left',pad=15)
    #ax.set_title('P%s %s %s' %(str(plev[nlv]),area,obsvar),loc='center',fontsize=18)
    ax.set_title('%s AOD' %(area),loc='center',fontsize=18)

    if (fsave):
        #fig.savefig('./Time_P%s_%s.%s_%s_%s.%s_%s.png' %(str(plev[nlv]),area,obsvar,modelname,obsname,sdate,cdate))
        #fig.savefig('./TimeSeries_P%s_%s.%s_%s_%s_%s.%s_%s.eps' %(str(plev[nlv]),area,obsvar,obsname1,modelname,obsname,sdate,cdate), format='eps')
        fig.savefig('./TimeSeries_%s.%s_%s_%s_%s.%s_%s.png' %(area,obsvar,obsname1,modelname,obsname,sdate,cdate), format='png')


#
#  Vertical profile
#
#pltdata=np.zeros((nlev,3),dtype='float')
#pltdata[:,1]=fbar.mean(axis=0)
#pltdata[:,2]=obar.mean(axis=0)
#pltdata[:,0]=obar1.mean(axis=0)
#
#fig=plt.figure(figsize=(6,8))
#ax=plt.subplot()
#ax.invert_yaxis()
#ax.set_prop_cycle(color=['red','blue','green'])
#ax.plot(pltdata,plev,'o-',lw=2.0)
#ax.legend(leglist, fontsize=16)
#ax.set_xlabel('Mixing Ratio [kg/kg]',fontsize=16)
#ax.set_ylabel('Pressure[hPa]',fontsize=16)
#ax.grid()
#ax.set_title('%s %s' %(area,obsvar),loc='center', fontsize=18)
#
#if (fsave):
#    #fig.savefig('./Prof_%s.%s_%s_%s.%s_%s.png' %(area,obsvar,modelname,obsname,sdate,cdate))
#    fig.savefig('./Profile_%s.%s_%s_%s_%s.%s_%s.eps' %(area,obsvar,obsname1,modelname,obsname,sdate,cdate), format='eps')

