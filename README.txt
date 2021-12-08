
**SCICHEM 3.3**  

SCICHEM 3.3 is a reactive puff model that can be used to calculate  
single or multi-source impacts of emissions at downwind locations.  The  
model can be used for both short-range calculations (for example,  
1-hour SO2, 1-hour NO2, 24-hour secondary PM2.5, or 8-hour ozone  
concentrations at fenceline receptors, or long-range calculations for  
primary  and secondary pollutant impacts. For 1-hour NO2 applications,  
the model  uses an optimized near-source NO-NO2-O3 chemistry scheme.  
For  long-range applications or near-field PM2.5/ozone, the full  
chemistry  option can be used to calculate downwind ozone and PM2.5  
concentrations.  

The full chemistry modules includes a gas-phase chemistry module based  
on the latest version of the Carbon Bond mechanism (CB6r2), while the  
aerosol and aqueous-phase chemistry  modules are based on those found  
in the Community Multiscale Air  Quality (CMAQ) Model version 4.7.1. A  
user-provided input file  determines which chemistry option is used.  
Sample input files for both  1-hour NO2 concentrations and full  
chemistry options are provided with  the case studies in the SCICHEM  
distribution. In addition to the  source code and executable files, the  
package includes the following:  

-	User's Guide,
-	Technical documentation,
-   Support document for alternative model PSD compliance demonstration,
-	4 case studies and accompanying tutorials, and  
-	3 Readme files  

This distribution includes a limited version of a Graphical User  
Interface (GUI), named "SCIPUFFgui, which is provided for the 64-bit  
Windows 8 or higher operating system as an aid to the user for  
visualization of model results. The GUI can plot concentration contour  
plots for surface, horizontal, or vertical slices for all source types.  
Note that SCIPUFFgui can also be used to create and run SCICHEM   
namelist-type projects. Note that it does not generate keyword-type
projects (introduced in SCICHEM 3.x) and cannot be used to define  area
sources. It is recommended to use the GUI primarily for viewing simulation
results or modifying input from existing projects. 

This distribution consists of three readme files, namely "README.txt"  
(this file),README-Examples.txt and "READMe-Build-Instructions.txt", 
three documents, "3002022845 User's Guide.pdf" and "3002022845 Technical
Documentation.pdf" and "3002022845 Support Document.pdf" and the following
four zipped files (to limit the size of the individual zipped files):

-	SCICHEM-3.3-Binary.tar  
-	SCICHEM-3.3-Examples.tar  
-	SCICHEM-3.3-FC_MEDOC.tgz
-   CTM2SCICHEM.tar

All the files should be unzipped in the same directory. These can be
unzipped on Windows using the free software 7-Zip. For running  
SCICHEM, the appropriate scipuff.ini file (in the bin/windows/x64 or  
bin/linux directory) should be edited so that the paths for the sciData  
directory and landuse.dat file point to the correct directory on the  
User's system. Details for running the SCIPUFFgui on Windows is  
provided in the User's Guide.  

Building downwash in SCICHEM 3.3 is based on PRIME (Schulman et al., 2000). 
PRIME has not been updated in over 15 years, and has been shown to overpredict 
concentrations by factors of 2 to 8 for certain building types (Petersen et al., 2017). 
New treatments for building downwash are being developed (Petersen et al., 2017)  
and it is anticipated that future releases of SCICHEM will include these improvements. 
Thus, the current default in SCICHEM is to ignore building downwash effects (RUNPRIME = N). 
However, in case the user wishes to activate the building downwash option, 
the user can do so by setting RUNPRIME to Y and providing the building dimensions 
from the BPIPPRM output in the SO section. See the User's guide for details.

The Mesoscale Model Interface Program (MMIF) on the U.S. EPA SCRAM web  
site can be used to convert prognostic meteorological model (MM5 and/or  
WRF) outputs to SCICHEM ready meteorological  inputs. SCICHEM 3.2 or later  
requires MMIF version 3.4 or later for compatibility.  

This is a full release that has been tested for a number of conditions.  
Windows and Linux versions of the executables are provided with the  
distribution. Both the Windows and Linux builds were created using the  
Intel compiler. For users interested in building the executable files  
on Linux or Windows machines, build scripts and Visual Fortran project  
files for the Intel compiler are provided. Users can create builds  
using other compilers but builds with non-Intel compilers have not been  
tested.  

Additional details and user instructions are provided in the documents  
bundled with the package. Users are requested to offer feedback to EPRI  
and the model developers on the model, including bug reports, and  
additional features that would make the model more useful to the air  
quality modeling community.  
 
**BEST PRACTICES**  

Some guidelines for creating good project input files for SCICHEM are  
provided below:  

1) With observed meteorology, if a terrain file is being provided  
separately then the terrain grid dimensions should be limited to a grid  
of less than 100x100 cells. Using more grid cells will result in  
significant increases in run time, because SCICHEM conducts mass  
consistent wind field calculations. For high resolution runs, mass  
consistent wind fields should be generated using a meteorological model  
(e.g., WRF) and provided to SCICHEM as gridded meteorological fields.  
Note that WRF fields can be directly read by SCICHEM, or the MMIF  
processor mentioned above can be used to create gridded meteorological  
files in SCICHEM format.  

2) The model can output time-averaged concentrations at selected  
receptor locations at runtime. However CPU requirements increase  
significantly with the number of samplers/receptors specified ahead
of time. Hence, for surface samplers/receptors, it is recommended
that the user calculate these concentrations as a post-processing
step using the provided postprocessor sciDOSpost. SciDOSpost can read
the surface deposition and dosage files, and calculate output (averaged
concentrations, deposition, visibility obscuration) at arbitrary
receptors (i.e. not specified before the SCICHEM run). For samplers
that are not at the surface (e.g., at locations corresponding to
aircraft measurements), the user can define a maximum of 5000
sampling locations.  

3) The large scale variance type (ENSM_TYPE) should be set to none for  
small domains (~<150 kms)  
