load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   title=getenv("TITLE")	
   limitlow=toint(getenv("LIMITLOW"))
   limithigh=toint(getenv("LIMITHIGH"))
   ncons=toint(getenv("NCONS"))
;   cmapstr=getenv("CMAPSTR")
;   spacing=toint(getenv("SPACING"))
   species=getenv("SPECIES")	
;   datadir=getenv("DATADIR")
;   griddir=getenv("GRIDDIR")
;   ncprefix=getenv("NCPREFIX")
;
;  Open netCDF files containing the grid and data from FV3

  k = 63   ; model level nearest the ground - counting starts from 0
  t = 0    ; time index

  do itile = 1,6
     gfile ="./indata_grid/C96_grid_spec.tile" + itile + ".nc"
     incfile = "./indata_fv3/fv_tracer.total.tile" + itile + ".nc"
     gread = addfile(gfile, "r")
     incread = addfile(incfile, "r")
     if (itile .eq. 1) then
        grid_latt = ndtooned(gread->grid_latt(:,:))    ; Pull off lat/lon variables
        grid_lont = ndtooned(gread->grid_lont(:,:))    ; Pull off lat/lon variables
        grid_data = ndtooned(incread->$species$(t,k,:,:))        ; Read some data; convert to 1D.

     else
        grid_latt := array_append_record (grid_latt, ndtooned(gread->grid_latt(:,:)), 0)
        grid_lont := array_append_record (grid_lont, ndtooned(gread->grid_lont(:,:)), 0)
        grid_data := array_append_record (grid_data, ndtooned(incread->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
     end if

  end do

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

  wks = gsn_open_wks("png","tracer")
  plot_c = gsn_csm_contour_map(wks,grid_data,res)

end
