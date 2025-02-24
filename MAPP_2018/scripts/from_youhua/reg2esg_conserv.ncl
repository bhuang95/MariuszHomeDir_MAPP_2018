load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin
;Written by Patrick C. Campbell (Patrick.C.Campbell@noaa.gov)
;12/01/2021
;*****************************************************************
; Read source files and lat/lon for regridding
;*****************************************************************

;---Read the source and destination lat/lon grids
    srcFileName = getenv("srcfile")
    dstFileName = getenv("dstfile")

;---Read the weight file name
    wgtFileName = getenv("wgtfile")
    wgtFileName2 = getenv("wgtfile2")
    
;---Read input and write to  the regridded output filename
    inFileName  = getenv("infile")
    outFileName = getenv("outfile")

    sfile = addfile(srcFileName,"r")
    dfile = addfile(dstFileName,"r")
    ifile = addfile(inFileName,"r")
    ofile = addfile(outFileName,"c")

    src_lat = sfile->Latitude
    src_lon = sfile->Longitude
;    src_area= sfile->area
    dst_lat = dfile->grid_latt
    dst_lon = dfile->grid_lont
    dst_area= dfile->area

;*****************************************************************
; Read data for regridding
;*****************************************************************

;---Get names of all variables on file
    vNames = getfilevarnames (ifile)
    nNames = dimsizes (vNames)
;---Loop through and read the data you want to regrid
    do n=0,nNames-1
        v = ifile->$vNames(n)$
        rank=(dimsizes(dimsizes(v)))
     if (rank .ge. 3) then  ;must be three-dimensional emission variables
        if(typeof(v).eq."short") then
         vfloat=short2flt(v)
         delete(v)
         v=vfloat
        end if
;        do t=0,dimsizes(v(:,0,0)) -1
;           v(t,:,:)=v(t,:,:)/(src_area*1e6)  ;convert to flux, i.e., remove grid cell area before regridding (units=kg m-2)
;        end do

;*****************************************************************
; Set options for regridding
;*****************************************************************

        Opt = True
        ;---source grid information
        Opt@SrcRegional = False
        Opt@SrcGridLat = src_lat
        Opt@SrcGridLon = src_lon
        ;---destination grid information
        Opt@DstRegional = True
        Opt@DstGridLat = dst_lat
        Opt@DstGridLon = dst_lon

        Opt@ForceOverwrite = True
        Opt@CopyVarCoords = True ; Whether to copy coordinate information
                                 ; to regridded variable
        Opt@InterpMethod = "conserve"; "neareststod" ; (default) "patch" or "conserve"
        Opt@WgtFileName    = wgtFileName

;*****************************************************************
; Check / create weight files for regridding, and then regrid
;*****************************************************************
        if (vNames(n).eq. "MeanFRP") then
	 Opt@InterpMethod ="neareststod"
	 Opt@WgtFileName    = wgtFileName2 
         exists=isfilepresent(wgtFileName2)
	 cwgt=wgtFileName2
	 print ("Process MeanFRP") 
	else
	 exists=isfilepresent(wgtFileName)
	 cwgt=wgtFileName
	end if  
        if( .not. exists ) then
                print("creating weight file and regridding..."+vNames(n))
                vregrid = ESMF_regrid(v,Opt)
        else
                print("weight file exists. Regridding..."+vNames(n))
                vregrid = ESMF_regrid_with_weights(v,cwgt,Opt)
        end if

;*****************************************************************
; Write regridded variables to output file
;*****************************************************************
        dimNames=getvardims(v)
        if (rank .eq. 2) then
        vregrid!0=dimNames(0)
        vregrid!1=dimNames(1)
        end if
        if (rank .eq. 3) then
        vregrid!1=dimNames(1)
        vregrid!2=dimNames(2)
        end if
        if (rank .eq. 4) then
        vregrid!2=dimNames(2)
        vregrid!3=dimNames(3)
        end if
        delete_VarAtts(vregrid,(/"lat2d","lon2d"/))
	if(vregrid@units .eq. "kg s-1 m-2") then
	  print("convert unit for "+vNames(n))
          do t=0,dimsizes(v(:,0,0)) -1
             vregrid(t,:,:) = vregrid(t,:,:)*dst_area(:,:)  ;multiplying dst area back after regridding (units=kg/grid cell area)
          end do 
          vregrid@units="kg s-1"
	end if  
        ofile->$vNames(n)$=vregrid
        delete(dimNames)
        delete(vregrid)
     end if
        delete(v)
     end do


;*****************************************************************
; Write global attributes to output file
;*****************************************************************

     global_attnames = getvaratts(dfile) ;-- retrieve the destination grid global attributes from input file
   do i=0,dimsizes(global_attnames)-1
      ofile@$global_attnames(i)$ = dfile@$global_attnames(i)$ ;-- write global attributes to new file
   end do

end
                                                                                      
