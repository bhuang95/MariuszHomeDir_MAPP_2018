load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   title=getenv("TITLE")
   limitlow=tofloat(getenv("LIMITLOW"))
   limithigh=tofloat(getenv("LIMITHIGH"))
   ncons=toint(getenv("NCONS"))
   species=getenv("SPECIES")
   lon=getenv("LON")
   lat=getenv("LAT")
   time_index=toint(getenv("TIME_INDEX"))

;
;  Open netCDF files containing the grid and data from FV3
;
  fd1   = addfile("./indata_aod_ll/aod_ll.nc","r")  ; data

  lon  :=                                 fd1->$lon$(:)    
  lat  :=                                 fd1->$lat$(:)

  nlons=dimsizes(lon)
  nlats=dimsizes(lat)

  grid_lon = new((/nlats,nlons/),typeof(lon))

  do i=0,nlats-1  
     grid_lon(i,:) = lon 
  end do	      

  grid_lon := reshape(grid_lon, (nlats*nlons))

  grid_lat = new((/nlats,nlons/),typeof(lat))

  do i=0,nlats-1  
     grid_lat(i,0:(nlons-1)) =  lat(i) 
  end do	      

  grid_lat := reshape(grid_lat, (nlats*nlons))

  t = time_index    ; time index

  temp       :=                            fd1->$species$(t,:,:)  

  scale_factor = temp@scale_factor
  add_offset = temp@add_offset

  temp := temp*scale_factor + add_offset

  temp := reshape(temp, (nlats*nlons))

; Open a file to send output to. You can use "ps", "pdf", "x11",
; or "ncgm".
;

  wks = gsn_open_wks("png","aod")

;
; The first call below selects a colormap, and the second reverses it.
; The colors go from violet at the low values to blue at the high
; values.
;
; To see the various colormaps, go to:
;
; http://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml
;
; https://www.ncl.ucar.edu/Document/Graphics/ColorTables/Rainbow_cat.shtml

; Set the bundaries of the raw data [limitlow, limithigh]
;  grid_data = where(grid_data.gt.limithigh,limithigh,grid_data)
;  grid_data = where(grid_data.lt.limitlow,limitlow,grid_data)

  res                     = True
  res@gsnMaximize         = False         ; Maximize size of plot in frame

  res@mpProjection = "CylindricalEquidistant"
  res@mpDataBaseVersion     = "MediumRes"      ; Improve the resolution
                                               ; of the map outlines 
                                               ; Default is "LowRes".

  res@mpPerimOn             = False       ; Turn off map perimeter
  res@mpCenterLatF          =  0.         ; Rotate map.
  res@mpCenterLonF          =  0. ;180.
  res@pmTickMarkDisplayMode = "Always"    ; Turn on map tickmarks
;  res@mpGeophysicalLineColor = "Gray"


  res@sfXArray            = grid_lon        ; Required to tell NCL where to
  res@sfYArray            = grid_lat        ; overlay data on globe.



  res@cnMinLevelValF       = limitlow
  res@cnMaxLevelValF       = limithigh
  ;res@cnLevelSpacingF      = spacing
  res@cnLineLabelsOn       = False

  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnFillPalette = "wh-bl-gr-ye-re"
  nplevels = fspan(limitlow, limithigh, ncons+1)
;  print(nplevels)
  res@cnLevels = nplevels


  res@cnFillOn            = True         ; Turn on contour fill
  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels
  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.
  res@lbBoxLinesOn        = False        ; Turn of labelbar box lines.

  res@tiMainString        = title
  res@tiMainFontHeightF = 0.02

  res@tmXBLabelFontHeightF = 0.015
  res@tmYLLabelFontHeightF = 0.015
  res@pmLabelBarWidthF = 0.5
  res@pmLabelBarHeightF = 0.08
  res@pmLabelBarOrthogonalPosF = .08

  wks = gsn_open_wks("png","aod")
  ;wks = gsn_open_wks("eps",title)
  plot_c = gsn_csm_contour_map(wks,temp,res)


end
