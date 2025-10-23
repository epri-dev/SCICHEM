load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/ut_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "SHAPEUTIL"

begin

f = addfile("INF","r")
g = addfile("GRID","r")
shp_filename1 = "SHP"
d = "IREGXREF"

data1 = asciiread(d,-1,"string")
region  = str_get_field(data1(0:),2,",")

;1st tstep, surface layer conc (2-D) array
conc0 = f->O3(0,0,:,:)     ; dims(tstep,lay,row,col)
conc0@lat2d = g->LAT(0,0,:,:)     ; dims(tstep,lay,row,col)
conc0@lon2d = g->LON(0,0,:,:)     ; dims(tstep,lay,row,col)

;avg conc in each region (hour,level,region)
regn_avg = 0.

do r=0,(NREG-1)
  print(region(r))

;---Set all hgt values to missing except for those over Ohio.
  opt             = True
  opt@debug       = True
  opt@shape_var   = "Domain"
  opt@shape_names = region(r)
  opt@return_mask = True
  conc0_mask  = shapefile_mask_data(conc0,shp_filename1,opt)
  ncell_mask  = sum(conc0_mask)

;generate cell mask netCDF file here
  system("/bin/rm -f NCDF/cell_mask.GRIDNAME."+region(r)+".nc")   ; remove any pre-existing file
  ncdf_out = addfile("NCDF/cell_mask.GRIDNAME."+region(r)+".nc" ,"c")  ; open output netCDF file
 ;===================================================================
 ; create global attributes of the file (optional)
 ;===================================================================
  fAtt               = True            ; assign file attributes
  fAtt@title         = "grid cell mask for "+region(r)
  fAtt@source_file   = "GRID"
  fAtt@Conventions   = "None"
  fAtt@creation_date = systemfunc ("date")
  fileattdef( ncdf_out, fAtt )            ; copy file attributes

 ;===================================================================
 ; make time an UNLIMITED dimension; recommended  for most applications
 ;===================================================================
  filedimdef(ncdf_out,"time",-1,True) 

  ncdf_out->REG_MASK = conc0_mask

end do;region

end
