load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   title=getenv("TITLE")	
   limitlow=toint(getenv("LIMITLOW"))
   limithigh=toint(getenv("LIMITHIGH"))
   ncons=toint(getenv("NCONS"))
   cmapstr=getenv("CMAPSTR")
   spacing=toint(getenv("SPACING"))
   species=getenv("SPECIES")	
   datadir=getenv("DATADIR")
   griddir=getenv("GRIDDIR")
   ncprefix=getenv("NCPREFIX")
;
;  Open netCDF files containing the grid and data from FV3

  k = 63   ; model level nearest the ground - counting starts from 0
  t = 0    ; time index

  do itile = 1,6
     gfile = griddir + "/C96_grid_spec.tile" + itile + ".nc"
     incfile = datadir + "/" + ncprefix + ".fv_tracer.res.tile" + itile + ".nc_inc_total"
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
;  fg1   = addfile(griddir + "/indata_grid/C96_grid_spec.tile1.nc","r")        ; lons and lats
;  fg2   = addfile(griddir + "/indata_grid/C96_grid_spec.tile2.nc","r")        ; lons and lats
;  fg3   = addfile(griddir + "/indata_grid/C96_grid_spec.tile3.nc","r")        ; lons and lats
;  fg4   = addfile(griddir + "/indata_grid/C96_grid_spec.tile4.nc","r")        ; lons and lats
;  fg5   = addfile(griddir + "/indata_grid/C96_grid_spec.tile5.nc","r")        ; lons and lats
;  fg6   = addfile(griddir + "/indata_grid/C96_grid_spec.tile6.nc","r")        ; lons and lats
;
;  fd1   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile1.nc","r")  ; data
;  fd2   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile2.nc","r")  ; data
;  fd3   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile3.nc","r")  ; data
;  fd4   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile4.nc","r")  ; data
;  fd5   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile5.nc","r")  ; data
;  fd6   = addfile(datadir + "/indata_fv3/fv_tracer.incr.total.tile6.nc","r")  ; data

;; Glue all 6 panels together
;  grid_latt  =                                 ndtooned(fg1->grid_latt(:,:))    ; Pull off lat/lon variables
;  grid_latt := array_append_record (grid_latt, ndtooned(fg2->grid_latt(:,:)), 0)
;  grid_latt := array_append_record (grid_latt, ndtooned(fg3->grid_latt(:,:)), 0)
;  grid_latt := array_append_record (grid_latt, ndtooned(fg4->grid_latt(:,:)), 0)
;  grid_latt := array_append_record (grid_latt, ndtooned(fg5->grid_latt(:,:)), 0)
;  grid_latt := array_append_record (grid_latt, ndtooned(fg6->grid_latt(:,:)), 0)
;
;; Glue all 6 panels together
;  grid_lont  =                                 ndtooned(fg1->grid_lont(:,:))    ; Pull off lat/lon variables
;  grid_lont := array_append_record (grid_lont, ndtooned(fg2->grid_lont(:,:)), 0)
;  grid_lont := array_append_record (grid_lont, ndtooned(fg3->grid_lont(:,:)), 0)
;  grid_lont := array_append_record (grid_lont, ndtooned(fg4->grid_lont(:,:)), 0)
;  grid_lont := array_append_record (grid_lont, ndtooned(fg5->grid_lont(:,:)), 0)
;  grid_lont := array_append_record (grid_lont, ndtooned(fg6->grid_lont(:,:)), 0)
;
;
;; Glue all 6 panels together
;
;
;  temp       =                            ndtooned(fd1->$species$(t,k,:,:))        ; Read some data; convert to 1D.
;  temp      := array_append_record (temp, ndtooned(fd2->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
;  temp      := array_append_record (temp, ndtooned(fd3->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
;  temp      := array_append_record (temp, ndtooned(fd4->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
;  temp      := array_append_record (temp, ndtooned(fd5->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.
;  temp      := array_append_record (temp, ndtooned(fd6->$species$(t,k,:,:)),0)     ; Read some data; convert to 1D.

; Set the bundaries of the raw data [limitlow, limithigh]
  grid_data = where(grid_data.gt.limithigh,limithigh,grid_data)
  grid_data = where(grid_data.lt.limitlow,limitlow,grid_data)

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
  res@mpGeophysicalLineColor = "Gray"


  res@sfXArray            = grid_lont        ; Required to tell NCL where to
  res@sfYArray            = grid_latt        ; overlay data on globe.

  res@cnMinLevelValF       = limitlow
  res@cnMaxLevelValF       = limithigh
  ;res@cnLevelSpacingF      = spacing
  res@cnLineLabelsOn       = False

  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnFillPalette = cmapstr
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

  wks = gsn_open_wks("png",title)
  plot_c = gsn_csm_contour_map(wks,grid_data,res)

end
