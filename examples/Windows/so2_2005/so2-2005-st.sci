**
****************************************
**
** EPRI SO2 Example for SCICHEM 3.4
** Created by Ramboll & Parsons
** Tracer simulation with no chemistry
**
****************************************
**
**
****************************************
** SCICHEM Control Pathway
****************************************
**
**
CO STARTING
CO TITLEONE EPRI SO2 Example/Tutorial
CO MODELOPT CONC 
%% CO POLLUTID TRACER
***************
**Building downwash in SCICHEM 3.4 is based on PRIME (Schulman et al., 2000). 
**PRIME has not been updated in over 15 years, and has been shown to overpredict 
**concentrations by factors of 2 to 8 for certain building types (Petersen et al., 2017). 
**New treatments for building downwash are being developed (Petersen et al., 2017) and 
**it is anticipated that future releases of SCICHEM will include these improvements. 
**Thus, the current default in SCICHEM is to ignore building downwash effects (RUNPRIME = N). 
**However, in case the user wishes to activate the building downwash option, 
**the user can do so by setting RUNPRIME to Y and providing the building dimensions from the 
**BPIPPRM output in the SO section. See the User's guide for details.
CO RUNPRIME N
****************
CO PROJECTN UTM WGS84 11  N KM
** xMin xMax yMin yMax zMax
CO DOMAIN  560.38 560.78 4824.7 4825.1 2000.
CO TIMEZONE -7
CO STARTEND 2005 1 1 0  2005 12 31 24 
CO FINISHED

MA STARTING
** MA MATCLASS TRACER Gas
** MA DENSITY  TRACER 1.2
** MA GASDEPOS TRACER 0.0
** Imc files with dummy reaction for tracer SO2
MA IMCFILE  so2_norxn.imc
MA FINISHED

**
****************************************
** SCICHEM Source Pathway
****************************************
**
**
SO STARTING
SO ELEVUNIT METERS
**          Name  Type    X        Y     Elev
SO LOCATION Boiler POINT 560.58 4824.85  848
** Multicomponent species ** must come before SRCPARAM and after LOCATION
   MULTCOMP  SO2
**POINTS: Name  emis(g/s)  Ht(m)  Temp(K)  Vel(m/s) Diam(m) species_emissions(g/s)
SO SRCPARAM Boiler 1.0 21.3 333.15 12.72 1.07 3.36
** Hourly emissions file
SO HOUREMIS so2-2005-st.emi
** Source Groups
SO SRCGROUP ALL
SO FINISHED

**
****************************************
** D Receptor Pathway
****************************************
**
**
** Receptors
** USE sciDOSpost for surface receptors
**RE STARTING
**RE ELEVUNIT METERS
**   DISCCART     560.530   4824.800     0.0    0.0 
**RE FINISHED
  
** Meteorology
ME STARTING
ME SCISFC KBOI2005.SFC
ME SCIPRF KBOI2005.PRF
ME ASOS1MIN '64050KBOI2005.dat' '64060KBOI2005.dat' 43.567 -116.241
ME FINISHED

** Terrain
TE STARTING
   TELUFILE terrain.utm.ter
TE FINISHED
