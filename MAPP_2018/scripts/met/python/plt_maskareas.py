import sys,os
#sys.path.append('/scratch1/BMC/gsd-fv3-dev/Shih-wei.Wei/MAPP/METplus_pkg/pyscripts/lib')
#import matplotlib
#matplotlib.use('agg')
import xarray as xa
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
from os.path import expanduser
import cartopy
cartopy.config['pre_existing_data_dir'] = expanduser('/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/masks/cartopy-data/')

proj=ccrs.PlateCarree()

masknum=6
masklist  =[ 'CONUS','EASIA','NAFRME',  'RUSC2S','SAFRTROP','SOCEAN']
colorlist =[   'red','green',  'blue',   'brown',  'orange','purple']


fig=plt.figure(figsize=(8,5))
ax=plt.subplot(projection=proj)
ax.coastlines(resolution='110m')
ax.set_extent((-180,180,-90,90),crs=proj)
ax.set_yticks([-90.,-60.,-30.,0.,30.,60.,90.], crs=proj)
ax.set_xticks([-160.,-120.,-80.,-40.,0.,40.,80.,120.,160.],crs=proj)
ax.xaxis.set_major_formatter(LongitudeFormatter())
ax.yaxis.set_major_formatter(LatitudeFormatter())

for idx in np.arange(masknum):
   areaname=masklist[idx]
   usecolor=colorlist[idx]
   maskfile='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/masks/nc_mask/%s_MSK.nc'%(areaname)
   mask=xa.open_dataset(maskfile)
   mask=mask.assign_coords(lon=((mask.lon+180)%360-180))
   mask=mask.sortby(mask.lon)
   mskarea=xa.where(mask[areaname],mask[areaname],np.nan)
   msklat=xa.where(mask[areaname],mask.lat,np.nan)
   msklon=xa.where(mask[areaname],mask.lon,np.nan)
   txtcenlat=np.nanmean(msklat)
   txtcenlon=np.nanmean(msklon)
   ax.contourf(mask.lon,mask.lat,mskarea,colors=usecolor)
   ax.text(txtcenlon,txtcenlat,areaname,c='white',
           horizontalalignment='center',verticalalignment='center')

ax.set_title('Mask areas',loc='left')
#tries to download 
#http://naciscdn.org/naturalearth/110m/physical/ne_110m_coastline.zip
#how to let it know that already downloaded
fig.savefig('./maskareas.png',dpi=200)
