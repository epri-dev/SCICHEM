#!/bin/csh

## Tool to create SCICHEM inputs from 3D CTM Model Outputs (CAMx or CMAQ)
# -----------------------------------------------------------------------

### Steps:
## 1. CAMx2IOAPI (only required for CAMx output files)
#     - Converts CAMx 3D output to I/O-API format
#     - Only required for CAMX files as CMAQ output files are already in I/O-API format
#     - Sigma levels of CAMx output are specified under the CAMx2IOAPI section
## 2. diurnal_prof 
#     - Calculate diurnal averages across each month
#     - Calculated for all months in the date range provided
#     - If full months data not provided, diurnal average calculated for available days
## 3. spcmap (only required for CAMx files, skipped if CMAQ specified as input model)
#     - Map CTM model species to SCICHEM CB6r2 species
## 4. NCL
#     - Calculate regional average using shapefile masking (e.g. states)
## 5. reformat
#     - Reformat NCL ascii output to SCICHEM input format

########## User Inputs ##############
#-----------------------------------#
echo "\n----- READING USER INPUT -----\n "
## Specify location of CTM2SCICHEM Directory
 set DIR_CTM2SCICHEM = /disk7/EPRI_SCICHEM32/CTM2SCICHEM/dev

## Specify whether CAMx or CMAQ model were used to generated hourly 3D Input files (not case sensitive)
 set MODEL = CAMx
 
## Specify year of model run
 set YYYY = 2011

## Provide unique runname for intermediate output directory and file labels
 set RUNNAME = CONUS12km_CAMx2011_cb6r4

##Provide directory containing 3D CAMx or CMAQ outputs
 # assumes all output files are in the same directory (if not create symbolic links)
 set DIR_CTM_3DOUT = /disk7/EPRI_SCICHEM32/CTM2SCICHEM/input/

## Specify daily CTM (CMAQ or CAMx) output filenames format using $JDATE for the julian date
 # MUST USE SINGLE QUOTES ' '
 set CTM_3DOUT_FILE = 'camx64_cb6r4.$JDATE.CONUS12km.avrg.grd01'

## Specify NCARG_ROOT environment variable for NCL and m3 tools directory that contains latlon
 # NCARG_ROOT - specify if not already included in .cshrc 
  setenv NCARG_ROOT /usr/local/ncarg

  # directory containing m3tools latlon
  set DIR_M3LATLON = /models/CMAQ/lib64/ioapi-3.1/Linux2_x86_64pg

## Region list and shapefile
 # Full name must match Domains field in shapefile specified below 
 set REGLIST = $DIR_CTM2SCICHEM/inputs/reg_list.txt

 # Region Shapefile, used to create regional mask
 set REGION_SHP = "$DIR_CTM2SCICHEM/ncl/input/shp/SCICHEM_Regions.shp"

## Variable list
 # list of variables that be included in created SCICHEM background files 
 set VARLIST = $DIR_CTM2SCICHEM/inputs/var_list.txt

## SCICHEM layer heights (top)
 set TOP_HT = $DIR_CTM2SCICHEM/inputs/vertlayer_height.txt

## Directory Locations associated with steps listed above, associated output directories, and option to skip steps
# 1. CAMx2IOAPI (only required for CAMx output files)
  set DIR_CAMx2IOAPI = $DIR_CTM2SCICHEM/camx2ioapi_v6
  set DIR_CAMx2IOAPI_OUT = $DIR_CTM_3DOUT
  set SIGMA_LEVELS = '0.9975,0.995,0.99,0.98,0.97,0.96,0.94,0.92,0.9,0.88,0.86,0.84,0.82,0.8,0.77,0.74,0.7,0.65,0.6,0.5,0.45,0.3,0.2,0.1,0'
  # Option to skip step (if already completed). If True, IOAPI files are assumed to be in DIR_CAMx2IOAPI_OUT
  set SKIP_CAMx2IOAPI = 'False'

# 2. diurnal_prof
  set DIR_DIURNAL_PROF = "$DIR_CTM2SCICHEM/diurnal_prof" # main diurnal_prof directory
  set DIR_DIURNAL_PROF_OUT = "/dhex7c/EPRI_SCICHEM32/CTM2SCICHEM/diurnal_prof/output/${RUNNAME}" # output directory
  set SKIP_DIURNAL_PROF = 'False' # option to skip step (if already completed)

# 3. spcmap
  set DIR_SPCMAP = "$DIR_CTM2SCICHEM/spcmap" # main spcmap directory
  set DIR_SPCMAP_OUT = "$DIR_SPCMAP/output/$RUNNAME" # spcmap output directory
  # mapping tables from CTM chemical mechanism to SCICHEM species
  set SPCMAP_MAPTBL = "$DIR_SPCMAP/Species_Mapping_Tables/MAPTBL.CAMx-CB6-CF_to_CB6r4.scichem"
  # option to skip (if already completed), if True assumes ioapi files for spatial averaging are in $DIR_specmap_out
  set SKIP_SPCMAP = 'False'

# 5. NCL (and getlatlon)
  set DIR_NCL = "$DIR_CTM2SCICHEM/ncl"
  set DIR_NCL_INPUT = $DIR_NCL/input
  set DIR_NCL_OUT = "$DIR_NCL/output/$RUNNAME"
  set DIR_NCL_NCDF_OUT = "${DIR_NCL}/ncf"

  # GRIDDESC file containing grid definition. 
  set CTM_GRIDDESC = "$DIR_NCL_INPUT/GRIDDESC.CONUS12km"
  set CTM_GRIDNAME = "CONUS12km"
  set DIR_GETLATLON_OUT = "$DIR_NCL_INPUT"
  ## Name of output file for LATLON tool
  # For CMAQ runs, a GRIDCRO2D file can be provided instead.
  # If specifying an exiting GRIDCRO2D file LATLON_OUTFILE, set SKIP_GETLATLON to 'True'
  set LATLON_OUTFILE = "$DIR_GETLATLON_OUT/LATLON.CONUS12km.nc"
  set SKIP_GETLATLON = 'False' # If True, assumes latlon outfile above exists
  set SKIP_NCL = 'False'

# 6. reformat
  set DIR_REFORMAT = "$DIR_CTM2SCICHEM/reformat"
  set DIR_REFORMAT_OUT = "$DIR_REFORMAT/output/$RUNNAME/"
  set REFORMAT_VARLIST = "$VARLIST"
  set REFORMAT_REGIONLIST = "$REGLIST"
  set SKIP_REFORMAT = 'False'
#-----------------------------------#
#####################################

## Alias to provide 3D CTM output file name within JDATE loop
# Used in CAMx2IOAPI for CAMx files, and in diurnal_prof for CMAQ files
alias CTM_DAILYFILE echo "$CTM_3DOUT_FILE"

# Beginning and ending julian dates based on user specied Year.
set BEGJDATE = ${YYYY}001
set ENDJDATE = `date -d "1/1/$YYYY + 1 year - 1 day" +%Y%j`
# Months 
set MONTHS = '01 02 03 04 05 06 07 08 09 10 11 12'

echo "\nUSER INPUT: "

echo "    CTM MODEL:         $MODEL"
echo "    CTM Grid Name:     $CTM_GRIDNAME"
echo "    CTM GRIDDESC File: $CTM_GRIDDESC"
echo "    RUN YEAR:          $YYYY"
echo "    RUN NAME:          $RUNNAME"
echo "    Variable List:     $VARLIST"
echo "    Regions List:      $REGLIST"
echo "    Regions Shapefile: $REGION_SHP"
echo "    Species Mapping:   $SPCMAP_MAPTBL"
echo "    Final SCICHEM Out: $DIR_REFORMAT_OUT\n"

####################################
######## Begin CTM2SCICHEM #########
####################################

echo "----- BEGINNING PROCESSING -----\n "

#################
# CAMx2IOAPI
#################
echo "# START CAMx2IOAPI #\n"
# covert user specified model to lower case
set MODEL_LC = `echo $MODEL | tr "[:upper:]" "[:lower:]"`

# If CMAQ skip CAMx2IOAPI
if($MODEL_LC == "cmaq") then 
  echo "User specified CMAQ model as source of hourly 3D input files"
  echo "Skipping CAMx2IOAPI (not required as CMAQ files are already I/O-API format)\n"

else if($MODEL_LC == "camx") then
  # Convert SKIP option to lower case
  set SKIP_CAMx2IOAPI_LC = `echo $SKIP_CAMx2IOAPI | tr "[:upper:]" "[:lower:]"`
 
  # SKIP_CAMX2IOAPI set to True, skip.
  if($SKIP_CAMx2IOAPI_LC == "true" || $SKIP_CAMx2IOAPI_LC == "t") then
    echo "Skipping CAMx2IOAPI even though CAMx was selected as user specified SKIP_CAMx2IOAPI as True"
    echo "Setting IOAPI directory (Hourly 3D IO-API model files) to $DIR_CAMx2IOAPI_OUT"
    set DIR_IOAPI = $DIR_CAMx2IOAPI_OUT

  # SKIP_CAMx2IOAPI set to False, continue.
  else
    echo "User specified CAMx model as source of hourly 3D input files, running CAMx2IOAPI"
    # create output directory
    mkdir -p $DIR_CAMx2IOAPI_OUT
    set DIR_IOAPI = $DIR_CAMx2IOAPI_OUT

    set JDATE = $BEGJDATE
    echo "CAMx2IOAPI - Beginning JDATE: $JDATE; End JDATE: $ENDJDATE"
    while ($JDATE <= $ENDJDATE)
      ##!! SPECIFY CAMx daily output file names using $JDATE
      set INFILE_3DCAMxOUT = $DIR_CTM_3DOUT/`CTM_DAILYFILE`
      set INBASE = `basename $INFILE_3DCAMxOUT`
      # IOAPI output file name specified using CAMx output file names
      set OUTFILE_IOAPI = ${INBASE}.ioapi.nc
      setenv IOAPI_OUT $DIR_IOAPI/$OUTFILE_IOAPI
      rm -f $IOAPI_OUT
      $DIR_CAMx2IOAPI/src/camx2ioapi << EOF
Input CAMx         |$INFILE_3DCAMxOUT
Data Type          |AVRG
Sigma Levels       |$SIGMA_LEVELS
EOF
      echo "finished running CAMx2IOAPI for JDATE: $JDATE" 
      @ JDATE++
    end # jdate loop
  endif # SKIP_CAMx2IOAPI option
else
  echo "Selected CTM model '$MODEL' not recognized. Please choose CAMx or CMAQ."
  echo "Exiting"
  exit
endif # Model selection

################
# diurnal_prof
#################
echo "# START diurnal_prof #\n"

# Convert user specified SKIP_DIURNAL_PROF option to lowercase
set SKIP_DIURNAL_PROF_LC = `echo $SKIP_DIURNAL_PROF | tr "[:upper:]" "[:lower:]"`

# SKIP_DIURNAL_PROF set to True, skip.
if($SKIP_DIURNAL_PROF_LC == "true" || $SKIP_DIURNAL_PROF_LC == "t") then
  echo "Skipping diurnal_prof (monthly diurnal average) as user specified SKIP_DIURNAL_PROF as True"
  echo "Assuming monthly diurnal average files are in $DIR_DIURNAL_PROF_OUT\n"

# SKIP_DIURNAL_PROF set to False, continue.
else
  echo "Starting diurnal_prof to calculate monthly diurnal averages."
  setenv ALL_LAYERS T
  setenv GET_SUM F
  # make output directory
  mkdir -p ${DIR_DIURNAL_PROF_OUT}

  foreach MM ($MONTHS)
    set ndays = `cal $MM $YYYY | awk 'FNR>2{d+=NF}END{print d}'`
    setenv NFILE $ndays
    set JDATE = `date -d "$MM/1/$YYYY" +%Y%j`
    set i = 1
    while($i <= $NFILE) # read input files for each month
      set INUM = `echo $i | awk '{printf("%3.3d",$1)}'`
      if($MODEL_LC == "camx") then
        set IOAPI_FILE = `CTM_DAILYFILE`.ioapi.nc
        setenv INFILE${INUM} "${DIR_IOAPI}/${IOAPI_FILE}"
      else if($MODEL_LC == "cmaq") then
        setenv INFILE${INUM} "${DIR_CTM_3DOUT}/`CTM_DAILYFILE`"
      else
        echo "Selected CTM model '$MODEL' not recognized. Please choose CAMx or CMAQ."
        echo "Exiting"
        exit
      endif 
      @ i++
      @ JDATE++
    end # read input files for each month
    setenv OUTFILE ${DIR_DIURNAL_PROF_OUT}/${RUNNAME}.${YYYY}${MM}.hrly_prof.ioapi.nc
    rm -f $OUTFILE
    echo "Running diurnal_prof for month $MM of $YYYY with $ndays days of input"
    $DIR_DIURNAL_PROF/src/diurnal_prof  
  end # foreach MM
endif # SKIP_DIURNAL_PROF

#################
# spcmap
#################
echo "# START spcmap #\n"
echo test

# If CMAQ skip spcmap
if($MODEL_LC == "cmaq") then 
  echo "User specified CMAQ model as source of hourly 3D input files"
  echo "Skipping spcmap\n"
  echo "Setting ncl input as diurnal prof output files: $DIR_DIURNAL_PROF_OUT"
  set DIR_SPCMAP_OUT = $DIR_DIURNAL_PROF_OUT

else if($MODEL_LC == "camx") then

# Convert user specified SKIP_SPCMAP option to lowercase
  set SKIP_SPCMAP_LC = `echo $SKIP_SPCMAP | tr "[:upper:]" "[:lower:]"`

  if($SKIP_SPCMAP_LC == "true" || $SKIP_SPCMAP_LC == "t") then
    echo "Skipping spcmap (mapping model species to SCICHEM species) as user specified SKIP_SPCMAP as True"
    echo "Assuming monthly files with SCICHEM species are in $DIR_SPCMAP_OUT\n"
  else
    setenv MAPTBL $SPCMAP_MAPTBL
    echo "Mapping CTM species to SCICHEM species using $MAPTBL."
    mkdir -p $DIR_SPCMAP_OUT
    foreach MM ($MONTHS)
    
      setenv INFILE ${DIR_DIURNAL_PROF_OUT}/${RUNNAME}.${YYYY}${MM}.hrly_prof.ioapi.nc
      setenv OUTFILE ${DIR_SPCMAP_OUT}/${RUNNAME}.${YYYY}${MM}.hrly_prof.ioapi.nc
  
      rm -f $OUTFILE
      echo "Running spcmap on diurnal average file of month: $MM"
      $DIR_SPCMAP/src/spcmap
    end # foreach MM
  endif # skip spcmap
endif # CAMx or CMAQ
###################
# NCL and getlatlon
###################
echo "# START NCL FOR REGION MAPPING #\n"

##  getlatlon
## ------------
# Use latlon tool of IO-API to generate a 1-layer time-indepedent gridded lat, lon file
# latlon uses the grid defifition in the $CTM_GRIDDESC file
# The grid name used in GRIDDESC and in the latlon program should be exactly the same (specified between laton <<EOF  and EOF) 
## ------------

# Convert user specified SKIP_DIURNAL_PROF option to lowercase
set SKIP_GETLATLON_LC = `echo $SKIP_GETLATLON | tr "[:upper:]" "[:lower:]"`

if($SKIP_GETLATLON_LC == "true" || $SKIP_GETLATLON_LC == "t") then
  echo "Skipping getlatlon as user specified SKIP_GETLATLON as True"
  echo "Assuming that the latlon netcdf file for the domain, $LATLON_OUTFILE, available in ncl dir\n"
else
  setenv IOAPI_ISPH 20
  setenv GRIDDESC $CTM_GRIDDESC
  setenv GRDFILE $LATLON_OUTFILE

  echo "Creating a time-indepedent gridded latlon file based on $CTM_GRIDDESC for grid: $CTM_GRIDNAME"

$DIR_M3LATLON/latlon <<EOF
Y

NONE

$CTM_GRIDNAME


EOF
endif

## NCL 
## ----------
# Convert user specified SKIP_NCL option to lowercase
set SKIP_NCL_LC = `echo $SKIP_NCL | tr "[:upper:]" "[:lower:]"`

if($SKIP_NCL_LC == "true" || $SKIP_NCL_LC == "t") then
  echo "Skipping ncl as user specified SKIP_NCL as True"
  echo "Assuming monthly, regional output files for SCICHEM species are in $DIR_NCL_OUT\n"
else
  ## First ncl script generates netcdf files with region mask 
  ##Second ncl script applies the region mask and calculates the conc values for each variable 
  #*********** ncl script 1 **************

  set NCLMASK_INFILE = `echo "${DIR_SPCMAP_OUT}/${RUNNAME}.${YYYY}01.hrly_prof.ioapi.nc" | sed 's:/:\\\/:g'`
  set GRIDCRO_FILE = `echo "$LATLON_OUTFILE" | sed 's:/:\\\/:g'`
  set SHAPEFILE = `echo "$REGION_SHP" | sed 's:/:\\\/:g'`
  set OUT_NCF_PATH = `echo "$DIR_NCL_NCDF_OUT" | sed 's:/:\\\/:g'`
  set SHPFILE_UTILS = `echo "${DIR_NCL}/shapefile_utils.ncl" | sed 's:/:\\\/:g'`

  mkdir -p $DIR_NCL_OUT
  mkdir -p $DIR_NCL_NCDF_OUT

 #create region csv file for NCL from user specified $REGLIST
  echo "Create region csv for NCL from $REGLIST"
  # remove existing region list
  rm -f $DIR_NCL_INPUT/${RUNNAME}.ireg_xref.csv
  set k = 1
  foreach line("`cat $REGLIST`")
    set reg_long = `echo $line | awk -F, '{print $2}'`
    printf {{"%6s,$reg_long\n"}} {{$k}} >> $DIR_NCL_INPUT/${RUNNAME}.ireg_xref.csv
    @ k++
  end

  set NUM_REGIONS = `wc -l < $REGLIST`
  echo "Number of regions: $NUM_REGIONS"
  set NCLREGION_CSV = `echo "$DIR_NCL_INPUT/${RUNNAME}.ireg_xref.csv" | sed 's:/:\\\/:g'`

  cat ${DIR_NCL}/mk_regn_nc.ncl \
  | sed "s/SHPUTIL/${SHPFILE_UTILS}/g" \
  | sed "s/INF/${NCLMASK_INFILE}/g" \
  | sed "s/GRIDNAME/${CTM_GRIDNAME}/g" \
  | sed "s/GRID/${GRIDCRO_FILE}/g" \
  | sed "s/SHP/${SHAPEFILE}/g" \
  | sed "s/SHAPEUTIL/${SHPFILE_UTILS}/g" \
  | sed "s/IREGXREF/${NCLREGION_CSV}/g" \
  | sed "s/NREG/${NUM_REGIONS}/g" \
  | sed "s/NCDF/${OUT_NCF_PATH}/g" \
  >! ${DIR_NCL}/tmp_ncl1.ncl

  ncl ${DIR_NCL}/tmp_ncl1.ncl
  rm -f ${DIR_NCL}/tmp_ncl1.ncl

#******* ncl script 2 **********************

  set VARS = `cat $VARLIST | awk -F, '{print $1}'`

  foreach MM ($MONTHS)
    foreach VAR ($VARS)

    echo "$MM --- $VAR"

    set INFILE = `echo "${DIR_SPCMAP_OUT}/${RUNNAME}.${YYYY}${MM}.hrly_prof.ioapi.nc" | sed 's:/:\\\/:g'`
    set OUTFILE = `echo "${DIR_NCL_OUT}/${RUNNAME}.${YYYY}${MM}.${VAR}.no_laymap.txt" | sed 's:/:\\\/:g'`

    cat ${DIR_NCL}/prep_scichem_bgk.all_lays.ncl \
    | sed s/INF/$INFILE/g \
    | sed s/OUTF/$OUTFILE/g \
    | sed s/VAR/$VAR/g \
    | sed s/SHP/$SHAPEFILE/g \
    | sed s/SHAPEUTIL/$SHPFILE_UTILS/g \
    | sed s/GRIDNAME/$CTM_GRIDNAME/g \
    | sed s/IREGXREF/$NCLREGION_CSV/g \
    | sed s/NREG/$NUM_REGIONS/g \
    | sed s/NCDF/$OUT_NCF_PATH/g \
    >! ${DIR_NCL}/tmp_ncl2.ncl

    ncl ${DIR_NCL}/tmp_ncl2.ncl
    rm -f ${DIR_NCL}/tmp_ncl2.ncl

    end  # End foreach VAR
  end  # foreach MM
endif # skip_ncl


#################
# reformat
#################

echo "# START reformat #\n"

# Convert user specified SKIP_DIURNAL_PROF option to lowercase
set SKIP_REFORMAT_LC = `echo $SKIP_REFORMAT | tr "[:upper:]" "[:lower:]"`

if($SKIP_REFORMAT_LC == "true" || $SKIP_REFORMAT_LC == "t") then
  echo "Skipping reformat as user specified SKIP_REFORMAT as True\n"
else
  mkdir -p ${DIR_REFORMAT_OUT}
  perl $DIR_REFORMAT/reformat.pl $DIR_NCL_OUT $DIR_REFORMAT_OUT $REFORMAT_VARLIST $REFORMAT_REGIONLIST $YYYY $RUNNAME $TOP_HT
endif #skip_reformat 
