import sys,os
#sys.path.append('_BASE_/pyscripts/lib')
#import matplotlib
#matplotlib.use('agg')
import xarray as xa
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mpcrs
import cartopy.crs as ccrs
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter

proj=ccrs.PlateCarree()

outputbase='/scratch1/BMC/gsd-fv3-dev/Shih-wei.Wei/MAPP/METplus_pkg/output/met_series_anl'
savedir='sa_images'
sapath='%s/met_tool_wrapper/SeriesAnalysis'%(outputbase)

fcstname='FV3'
fcstvar='aod'

#obsname='MERRA2'
#obsvar='AODANA'
obsname='CAMS(EAC4)'
obsvar='aod550'

sdate=2016060100
edate=2016063018

safile='%s/%s_%s.%s_%s.%s_%s.nc' %(sapath,fcstname,fcstvar,obsname,obsvar,sdate,edate)

varlist=['RMSE','ME','PR_CORR','KT_CORR']

def setupax():
   fig=plt.figure(figsize=(8,5),constrained_layout=True)
   ax=plt.subplot(projection=proj)
   ax.coastlines(resolution='110m')
   ax.set_extent((-180,180,-90,90),crs=proj)
   ax.set_yticks([-90.,-60.,-30.,0.,30.,60.,90.], crs=proj)
   ax.set_xticks([-160.,-120.,-80.,-40.,0.,40.,80.,120.,160.],crs=proj)
   ax.xaxis.set_major_formatter(LongitudeFormatter())
   ax.yaxis.set_major_formatter(LatitudeFormatter())
   return fig,ax

def setup_cmap(name,selidx):
    nclcmap='/scratch1/BMC/gsd-fv3-dev/Shih-wei.Wei/common/colormaps'
    cmapname=name
    f=open(nclcmap+'/'+cmapname+'.rgb','r')
    a=[]
    for line in f.readlines():
        if ('ncolors' in line):
            clnum=int(line.split('=')[1])
        a.append(line)
    f.close()
    b=a[-clnum:]
    c=[]
    for i in selidx[:]:
        c.append(tuple(float(y)/255. for y in b[i].split()))

    d=mpcrs.LinearSegmentedColormap.from_list(name,c,selidx.size)
    return d
  
sa=xa.open_dataset(safile)
sa=sa.assign_coords(lon=((sa.lon+180)%360-180))
sa=sa.sortby(sa.lon)

for var in varlist:
   varname='series_cnt_%s'%(var)
   if (var == 'RMSE'):
      lvs=np.array((0, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5))
      clridx=np.array((2,3,4,5,6,7,8,9,10,11))
      nclmapname='sunshine_9lev'
   elif (var == 'ME'):
      lvs=np.array((-1.5,-1.2,-1.,-0.7,-0.5,-0.3,-0.1,-0.05,0.05,0.1,0.3,0.5,0.7,1.,1.2,1.5))
      clridx=np.array((4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
      nclmapname='temp_diff_18lev'
   elif (var == 'PR_CORR' or var == 'KT_CORR'):
      lvs=np.array((-1.,-0.9,-0.8,-0.7,-0.6,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.6,0.7,0.8,0.9,1.))
      clridx=np.array((4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
      nclmapname='temp_diff_18lev'

   norm = mpcrs.BoundaryNorm(lvs,len(lvs))
   cmpd=setup_cmap(nclmapname,clridx-2)

   f,a=setupax()
   im=a.contourf(sa.lon,sa.lat,sa[varname],levels=lvs,cmap=cmpd,norm=norm)
   f.colorbar(im,ax=a,orientation='horizontal',fraction=0.06,ticks=lvs)
   a.set_title('%s' %(var),loc='left')
   #f.savefig('%s/%s/%s_%s.%s_%s.%s_%s.png' %(outputbase,savedir,fcstname,fcstvar,obsname,obsvar,sdate,edate))
   f.savefig('./SeriesAnl_%s.%s_%s.%s_%s.%s_%s.png' %(var,fcstname,fcstvar,obsname,obsvar,sdate,edate))
   
