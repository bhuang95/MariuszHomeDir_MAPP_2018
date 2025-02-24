load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   title=getenv("TITLE")	

;alternatively toint

;   limitlow=tofloat(getenv("LIMITLOW"))
;   limithigh=tofloat(getenv("LIMITHIGH"))
;   spacing=tofloat(getenv("SPACING"))
;   species=getenv("SPECIES")

   mwair=0.0289645
   mwco=0.0280101
   grav=9.80665

   factor=1.e-6/grav*mwco/mwair

   limitlow=toint(getenv("LIMITLOW"))*1.e-6
   limithigh=toint(getenv("LIMITHIGH"))*1.e-6
   spacing=toint(getenv("SPACING"))*1.e-6
   species=getenv("SPECIES")
   grid=getenv("GRID")

   nlevels=(limithigh-limitlow)/spacing

;
;  Open netCDF files containing the grid and data from FV3 ;
  fg1   = addfile("./indata_grid/"+grid+"_grid_spec.tile1.nc","r") ;latlon  
  fg2   = addfile("./indata_grid/"+grid+"_grid_spec.tile2.nc","r") ;latlon   
  fg3   = addfile("./indata_grid/"+grid+"_grid_spec.tile3.nc","r") ;latlon   
  fg4   = addfile("./indata_grid/"+grid+"_grid_spec.tile4.nc","r") ;latlon   
  fg5   = addfile("./indata_grid/"+grid+"_grid_spec.tile5.nc","r") ;latlon   
  fg6   = addfile("./indata_grid/"+grid+"_grid_spec.tile6.nc","r") ;latlon   

  fd1   = addfile("./indata_fv3_co/fv_tracer.res.tile1.nc","r")  ; data
  fd2   = addfile("./indata_fv3_co/fv_tracer.res.tile2.nc","r")  ; data
  fd3   = addfile("./indata_fv3_co/fv_tracer.res.tile3.nc","r")  ; data
  fd4   = addfile("./indata_fv3_co/fv_tracer.res.tile4.nc","r")  ; data
  fd5   = addfile("./indata_fv3_co/fv_tracer.res.tile5.nc","r")  ; data
  fd6   = addfile("./indata_fv3_co/fv_tracer.res.tile6.nc","r")  ; data

  fc1   = addfile("./indata_fv3_co/fv_core.res.tile1.nc","r")  ; DELP
  fc2   = addfile("./indata_fv3_co/fv_core.res.tile2.nc","r")  ; DELP
  fc3   = addfile("./indata_fv3_co/fv_core.res.tile3.nc","r")  ; DELP
  fc4   = addfile("./indata_fv3_co/fv_core.res.tile4.nc","r")  ; DELP
  fc5   = addfile("./indata_fv3_co/fv_core.res.tile5.nc","r")  ; DELP
  fc6   = addfile("./indata_fv3_co/fv_core.res.tile6.nc","r")  ; DELP



; Glue all 6 panels together
  grid_latt  =                                 ndtooned(fg1->grid_latt(:,:))    ; Pull off lat/lon variables
  grid_latt := array_append_record (grid_latt, ndtooned(fg2->grid_latt(:,:)), 0)
  grid_latt := array_append_record (grid_latt, ndtooned(fg3->grid_latt(:,:)), 0)
  grid_latt := array_append_record (grid_latt, ndtooned(fg4->grid_latt(:,:)), 0)
  grid_latt := array_append_record (grid_latt, ndtooned(fg5->grid_latt(:,:)), 0)
  grid_latt := array_append_record (grid_latt, ndtooned(fg6->grid_latt(:,:)), 0)

; Glue all 6 panels together
  grid_lont  =                                 ndtooned(fg1->grid_lont(:,:))    ; Pull off lat/lon variables
  grid_lont := array_append_record (grid_lont, ndtooned(fg2->grid_lont(:,:)), 0)
  grid_lont := array_append_record (grid_lont, ndtooned(fg3->grid_lont(:,:)), 0)
  grid_lont := array_append_record (grid_lont, ndtooned(fg4->grid_lont(:,:)), 0)
  grid_lont := array_append_record (grid_lont, ndtooned(fg5->grid_lont(:,:)), 0)
  grid_lont := array_append_record (grid_lont, ndtooned(fg6->grid_lont(:,:)), 0)

  k = 63   ; model level nearest the ground - counting starts from 0
  t = 0    ; time index

; Glue all 6 panels together
;  temp2      := array_append_record (temp, fd2->$species$(t,:,:,:),0)    ; Read some data

  temp1      := fd1->$species$(t,:,:,:)        
  temp2      := fd2->$species$(t,:,:,:)
  temp3      := fd3->$species$(t,:,:,:)
  temp4      := fd4->$species$(t,:,:,:)
  temp5      := fd5->$species$(t,:,:,:)
  temp6      := fd6->$species$(t,:,:,:)

  dz1      := fc1->DELP(t,:,:,:)        
  dz2      := fc2->DELP(t,:,:,:)
  dz3      := fc3->DELP(t,:,:,:)
  dz4      := fc4->DELP(t,:,:,:)
  dz5      := fc5->DELP(t,:,:,:)
  dz6      := fc6->DELP(t,:,:,:)

col1 =  ndtooned(dim_sum_n(dz1 * temp1 * factor,0))
col2 =  ndtooned(dim_sum_n(dz2 * temp2 * factor,0))
col3 =  ndtooned(dim_sum_n(dz3 * temp3 * factor,0))
col4 =  ndtooned(dim_sum_n(dz4 * temp4 * factor,0))
col5 =  ndtooned(dim_sum_n(dz5 * temp5 * factor,0))
col6 =  ndtooned(dim_sum_n(dz6 * temp6 * factor,0))

temp  := col1
temp  := array_append_record (temp, col2,0)     
temp  := array_append_record (temp, col3,0)
temp  := array_append_record (temp, col4,0)
temp  := array_append_record (temp, col5,0)
temp  := array_append_record (temp, col6,0)

print(min(temp))
print(max(temp))
;print(dimsizes(temp))

;lat_factor_low = 40.
;lat_factor_high = 80.

;print(dimsizes(grid_latt))

;factor := where((grid_latt.gt.lat_factor_low).and.(grid_latt.lt.lat_factor_high),1+0.5*(grid_latt-lat_factor_low)/(lat_factor_high-lat_factor_low),1)

;factor := where(grid_latt.gt.lat_factor_high,1.5,factor)

factor = 1

temp  := temp*factor

;
; Open a file to send output to. You can use "ps", "pdf", "x11",
; or "ncgm".
;


  wks = gsn_open_wks("png","co_column")

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

;  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")
;  gsn_define_colormap(wks,"example")	
;  gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")	
;  gsn_define_colormap(wks,"WhBlGrYeRe")	
  gsn_define_colormap(wks,"wh-bl-gr-ye-re")	
;  gsn_reverse_colormap(wks)

temp = where(temp.gt.limithigh,limithigh,temp)
temp = where(temp.lt.limitlow,limitlow,temp)

;
; Set some plot resources.  Order doesn't matter here.
;
; Any time you don't want a plot drawn, just set the resources:
;
;  res@gsnDraw  = False
;  res@gsnFrame = False
;
; Of course, you'll have to remember to delete these resource
; settings or change them to True if you want later plots to
; be drawn.
;
  res                     = True
  res@gsnMaximize         = False         ; Maximize size of plot in frame
  res@gsnSpreadColors     = True         ; Use full colormap, but start
  res@gsnSpreadColorStart = 24           ; at color index 24.

  res@sfXArray            = grid_lont        ; Required to tell NCL where to
  res@sfYArray            = grid_latt        ; overlay data on globe.

  res@cnFillOn            = True         ; Turn on contour fill
  res@cnFillMode          = "AreaFill"   ; Style of fill. You can also
                                         ; use "CellFill" and "RasterFill"
  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels

  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.
  res@lbBoxLinesOn        = False        ; Turn of labelbar box lines.
  res@lbTitleString       = "kg/m^2"
  res@lbTitlePosition  = "bottom"
  res@lbTitleFontHeightF= .015

  res@tiMainString        = title
  res@tiMainFontHeightF = 0.02

  res@mpProjection = "CylindricalEquidistant"
  res@mpCenterLatF          =  0.         ; Rotate map.
  res@mpCenterLonF          =  0. ;180.

  res@mpDataBaseVersion     = "MediumRes"      ; Improve the resolution
                                               ; of the map outlines 
                                               ; Default is "LowRes".

  res@mpPerimOn             = False       ; Turn off map perimeter
  res@pmTickMarkDisplayMode = "Always"    ; Turn on map tickmarks

  res@cnMaxLevelCount = 255
  res@cnFillMode          = "AreaFill"
;  res@cnLevelSelectionMode = "AutomaticLevels"
   res@cnLevelSelectionMode = "ManualLevels"
   res@cnMinLevelValF       = limitlow+spacing
   res@cnMaxLevelValF       = limithigh-spacing
   res@cnLevelSpacingF      = spacing
   res@cnLineLabelsOn       = False
;   res@cnLabelBarEndStyle    = "IncludeMinMaxLabels"


  map = gsn_csm_contour_map(wks,temp,res)   ; Draw contours over a map.

;
; Try some other contour resource settings to show how the
; contour fill can be adjusted.
;
  res@cnFillMode      = "RasterFill"
  res@cnMaxLevelCount = 255 

;  map = gsn_csm_contour_map(wks,temp,res)

;
; Change the map projection.
;
  res@mpProjection = "CylindricalEquidistant"
;  map = gsn_csm_contour_map_ce(wks,temp,res)

;
; Turn on smoothing. - NO!!!
;

;  res@cnRasterSmoothingOn = True

;  map = gsn_csm_contour_map_ce(wks,temp,res)

;
; Change the map projection again.
;
  res@mpProjection = "LambertEqualArea"
  res@mpCenterLatF = 0
;  res@mpCenterLonF = 180
;  map = gsn_csm_contour_map(wks,temp,res)
end
