load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   title=getenv("TITLE")
   limitlow=tofloat(getenv("LIMITLOW"))
   limithigh=tofloat(getenv("LIMITHIGH"))
   ncons=toint(getenv("NCONS"))
   species=getenv("SPECIES")

;
;  Open netCDF files containing the grid and data from FV3
;
  fg1   = addfile("./indata_grid/C96_grid_spec.tile1.nc","r")        ; lons and lats
  fg2   = addfile("./indata_grid/C96_grid_spec.tile2.nc","r")        ; lons and lats
  fg3   = addfile("./indata_grid/C96_grid_spec.tile3.nc","r")        ; lons and lats
  fg4   = addfile("./indata_grid/C96_grid_spec.tile4.nc","r")        ; lons and lats
  fg5   = addfile("./indata_grid/C96_grid_spec.tile5.nc","r")        ; lons and lats
  fg6   = addfile("./indata_grid/C96_grid_spec.tile6.nc","r")        ; lons and lats

  fd1   = addfile("./indata_fv3_aod/fv_aod.tile1.nc","r")  ; data
  fd2   = addfile("./indata_fv3_aod/fv_aod.tile2.nc","r")  ; data
  fd3   = addfile("./indata_fv3_aod/fv_aod.tile3.nc","r")  ; data
  fd4   = addfile("./indata_fv3_aod/fv_aod.tile4.nc","r")  ; data
  fd5   = addfile("./indata_fv3_aod/fv_aod.tile5.nc","r")  ; data
  fd6   = addfile("./indata_fv3_aod/fv_aod.tile6.nc","r")  ; data

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

  k = 0   ; model level nearest the ground - counting starts from 0
  t = 0    ; time index

; Glue all 6 panels together


  temp       =                            ndtooned(fd1->$species$(t,k,:,:))        ; Read some data; convert to 1D.
  temp      := array_append_record (temp, ndtooned(fd2->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
  temp      := array_append_record (temp, ndtooned(fd3->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
  temp      := array_append_record (temp, ndtooned(fd4->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
  temp      := array_append_record (temp, ndtooned(fd5->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
  temp      := array_append_record (temp, ndtooned(fd6->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.

;
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


  res@sfXArray            = grid_lont        ; Required to tell NCL where to
  res@sfYArray            = grid_latt        ; overlay data on globe.

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
