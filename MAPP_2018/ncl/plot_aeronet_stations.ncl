load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  freq_min = 0
  freq_max = 500

  bias_min = -.3
  bias_max = .3
  bias_minm = bias_min+0.01
  bias_maxm = bias_max-0.01

  rmse_min = .0
  rmse_max = .4

  r_min = -1.
  r_max = 1.

;  nconsf = freq_max/10
  nconsf = freq_max/50
  nconsr = 10
  nconsb = 12
  nconsrm = 16

  dfreq = (freq_max-freq_min)/nconsf
  freq_minm = freq_min + dfreq
  freq_maxm = freq_max - dfreq

  drmse = (rmse_max-rmse_min)/nconsrm
  rmse_minm = rmse_min + drmse
  rmse_maxm = rmse_max - drmse

  dbias = (bias_max-bias_min)/nconsb
  bias_minm = bias_min + dbias
  bias_maxm = bias_max - dbias


  fd  = addfile("./indata_aeronet/aeronet_stats.nc","r")

  lons  :=  fd->longitude(:)
  lats  :=  fd->latitude(:)
  freqs :=  fd->frequency(:)
  rcoeff :=  fd->rcoeff(:)
  bias := fd->bias(:)
  rmse := fd->rmse(:)

  freqs_max = max(freqs)
  print("max numbers of obs")
  print(freqs_max)

  freqs := where(freqs.gt.freq_maxm,freq_maxm,freqs)
  freqs := where(freqs.lt.freq_minm,freq_minm,freqs)

  bias := where(bias.gt.bias_maxm,bias_maxm,bias)
  bias := where(bias.lt.bias_minm,bias_minm,bias)

  rmse := where(rmse.gt.rmse_maxm,rmse_maxm,rmse)
  rmse := where(rmse.lt.rmse_minm,rmse_minm,rmse)

  res                     = True
  res@gsnMaximize         = False         ; Maximize size of plot in frame

  res@tiMainFontHeightF = 0.02


  res@mpProjection = "CylindricalEquidistant"
  res@mpDataBaseVersion     = "MediumRes"      ; Improve the resolution
                                               ; of the map outlines 
                                               ; Default is "LowRes".

  res@mpOutlineOn = True ; (True - turn off continental outlines)
  res@mpFillOn = False ; (True - turn on map fill)


  res@mpPerimOn             = False       ; Turn off map perimeter
  res@mpCenterLatF          =  0.         ; Rotate map.
  res@mpCenterLonF          =  0.  ;180.

  res@pmTickMarkDisplayMode = "Always"    ; Turn on map tickmarks

  res@gsnDraw  = False
  res@gsnFrame = False

  font_height = 0.02

  lbres                    = True      ; Set up a resource list for the labelbar.
  lbres@vpWidthF           = 0.5
  lbres@vpHeightF          = 0.08

  lbres@lbBoxLinesOn       = False
  lbres@lbMonoFillPattern  = True

  lbres@lbOrientation      = "Horizontal"
  lbres@lbPerimOn          = False

  lbres@lbLabelFontHeightF = 0.0135
  lbres@lbLabelAutoStride  = True
  lbres@lbLabelAlignment   = "InteriorEdges"
  lbres@lbTitlePosition  = "bottom"
  lbres@lbTitleFontHeightF= .0135
  lbres@lbTitleJust = "CenterCenter"

  mkres = True
  mkres@gsMarkerIndex    = 16          ; Filled dots
  mkres@gsMarkerSizeF    = 0.005

  amres                  = True
  amres@amZone           = 2
  amres@amParallelPosF   =  0.5
  amres@amOrthogonalPosF =  0.15

  palette="wh-bl-gr-ye-re"

; plot freqs

  wksf = gsn_open_wks("png","./pics_aeronet/aeronet_aod_freqs")

  title = "AERONET stations"
  res@tiMainString = title

  map = gsn_csm_map(wksf,res)

  gsn_define_colormap(wksf,palette)	

  nlevels = nconsf+1
  nlevelsm1 = nlevels-1

  levels = fspan(0,freq_max,nlevels)

  icols = span_color_indexes(palette,nlevels)
  rcols = span_color_rgba(palette,nlevels)

  lbres@lbFillColors       = rcols ;(2::2,:)

  dum = new(nlevelsm1,graphic)

  do i=0,nlevels-2

     ii := ind(levels(i).le.freqs.and.freqs.lt.levels(i+1))   

     if (ismissing(ii(0))) then
     	continue
     end if	

     mkres@gsMarkerColor    = rcols(i,:)

     dum(i) = gsn_add_polymarker(wksf,map,lons(ii),lats(ii),mkres)

  end do

  lbres@lbTitleString = "          Number of retrievals"
  labels = "" + levels ;(2::2)

  lbid = gsn_create_labelbar(wksf,dimsizes(levels),labels,lbres)

  annoid = gsn_add_annotation(map,lbid,amres)

  draw(map)

  frame(wksf)

; plot rmse

  wksrm = gsn_open_wks("png","./pics_aeronet/aeronet_aod_rmse")

  title = "RMSE"
  res@tiMainString        = title

  map = gsn_csm_map(wksrm,res)

  gsn_define_colormap(wksrm,palette)	

  nlevels = nconsrm+1
  nlevelsm1 = nlevels-1
  
  levels := fspan(rmse_min,rmse_max,nlevels)

  icols := span_color_indexes(palette,nlevels)
  rcols := span_color_rgba(palette,nlevels)

  lbres@lbFillColors       := rcols ;(2::2,:)
;  lbres@lbFillColors       := rcols(2::2,:)

  dum := new(nlevels,graphic)

  do i=0,nlevels-2

     ii := ind(levels(i).le.rmse.and.rmse.lt.levels(i+1))   

     if (ismissing(ii(0))) then
     	continue
     end if	

     mkres@gsMarkerColor    = rcols(i,:)

     dum(i) = gsn_add_polymarker(wksrm,map,lons(ii),lats(ii),mkres)

  end do

  lbres@lbTitleString = ""
  labels := "" + levels ;(2::2)

  lbid = gsn_create_labelbar(wksrm,dimsizes(levels),labels,lbres)

  annoid = gsn_add_annotation(map,lbid,amres)

  draw(map)

  frame(wksrm)


; plot rcoeff

  wksr = gsn_open_wks("png","./pics_aeronet/aeronet_aod_rcoeff")

  title = "Correlation Coefficient"
  res@tiMainString        = title

  map = gsn_csm_map(wksr,res)

  gsn_define_colormap(wksr,palette)	

  nlevels = nconsr+1
  nlevelsm1 = nlevels-1

  levels := fspan(r_min,r_max,nlevels)

  icols := span_color_indexes(palette,nlevels)
  rcols := span_color_rgba(palette,nlevels)

  lbres@lbFillColors       := rcols ;(2::2,:)

  dum := new(nlevelsm1,graphic)

  do i=0,nlevels-2

     ii := ind(levels(i).le.rcoeff.and.rcoeff.lt.levels(i+1))   

     if (ismissing(ii(0))) then
     	continue
     end if	

     mkres@gsMarkerColor    = rcols(i,:)

     dum(i) = gsn_add_polymarker(wksr,map,lons(ii),lats(ii),mkres)

  end do

  lbres@lbTitleString = ""
  labels := "" + levels ;(2::2)

  lbid = gsn_create_labelbar(wksr,dimsizes(levels),labels,lbres)

  annoid = gsn_add_annotation(map,lbid,amres)

  draw(map)

  frame(wksr)

; plot bias

  wksb = gsn_open_wks("png","./pics_aeronet/aeronet_aod_bias")

  title = "Bias"
  res@tiMainString        = title

  map = gsn_csm_map(wksb,res)

  gsn_define_colormap(wksb,palette)	

  nlevels = nconsb+1
  nlevelsm1 = nlevels-1

  levels := fspan(bias_min,bias_max,nlevels)

  icols := span_color_indexes(palette,nlevels)
  rcols := span_color_rgba(palette,nlevels)

  lbres@lbFillColors       := rcols ;(2::2,:)

  dum := new(nlevelsm1,graphic)

  do i=0,nlevels-2

     ii := ind(levels(i).le.bias.and.bias.lt.levels(i+1))   

     if (ismissing(ii(0))) then
     	continue
     end if	

     mkres@gsMarkerColor    = rcols(i,:)

     dum(i) = gsn_add_polymarker(wksb,map,lons(ii),lats(ii),mkres)

  end do

  lbres@lbTitleString = ""
  labels := "" + levels ;(2::2)

  lbid = gsn_create_labelbar(wksb,dimsizes(levels),labels,lbres)

  annoid = gsn_add_annotation(map,lbid,amres)

  draw(map)

  frame(wksb)

end
