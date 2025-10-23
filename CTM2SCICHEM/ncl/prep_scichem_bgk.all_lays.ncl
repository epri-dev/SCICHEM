; ************************ Load libraries **************************
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/ut_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "SHAPEUTIL"
; *******************************************************************
begin

f = addfile("INF","r")
shp_filename1 = "SHP"
d = "IREGXREF"

data1 = asciiread(d,-1,"string")
region  = str_get_field(data1(0:),2,",")

;full 4-D conc array 
conc  = f->VAR(:,:,:,:)     ; dims(tstep,lay,row,col)

header = "Hour,Layer,Region,VAR"
write_table("OUTF","w",[/header/],"%s")

do r=0,(NREG-1)
  print(region(r))
  ;open netCDF file with grid cell mask
  h = addfile("NCDF/cell_mask.GRIDNAME."+region(r)+".nc","r")

  conc0_mask = h->REG_MASK(:,:) ;dims(row,col)
  ncell_mask  = sum(conc0_mask)

;multiply mask by conc array to get masked conc averages
;then write out to csv file
regn_avg = 0.
do n=0,23
do z=0,17
  regn_avg = sum(conc(n,z,:,:)*conc0_mask(:,:))/ncell_mask
  write_table("OUTF","a",[/n+1,z+1,region(r),regn_avg/], \
      "%4i,%4i,%s,%15.10f")
end do;z
end do;n
end do;region

end
