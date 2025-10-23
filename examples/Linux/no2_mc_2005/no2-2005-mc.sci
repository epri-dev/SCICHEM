**
****************************************
**
** EPRI NO2 Example for SCICHEM 3.0(Beta3)
** Created by Ramboll-ENVIRON & Sage-Xator
** Multicomponent simulation with near-source NO-NO2-O3 chemistry
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
   TITLEONE EPRI NO2 Example/Tutorial with Short Range Chemistry
   TITLETWO hourly O3 data
   PROJECTN UTM WGS84 11 N KM
** xMin xMax yMin yMax zMax
   DOMAIN  560.38 560.78 4824.7 4825.1 2000.
   TIMEZONE -7   
   STARTEND 2005 1 1 0  2005 12 31 24
CO FINISHED

MA STARTING
MA MATCLASS trac Gas
MA DESCRPTN This is ignored
MA DENSITY  trac 1.2
MA GASDEPOS trac 0.0
MA IMCFILE  '../../Common/no2_mc_2005/no2.imc'
MA FINISHED

**
****************************************
** SCICHEM Source Pathway
****************************************
**
**
SO STARTING
** Source Location **
** Source ID -              Type       X Coord. Y Coord. Z Coord.**
   LOCATION FIREPUMP        POINT      560.58  4824.85  848.000
** Multicomponent species ** must come before SRCPARAM and after LOCATION
   MULTCOMP  NO2 NO
** Source Parameters **
**                          emis   hgt    tmp    vel    dia     species emissions (corresponding to MULTCOMP list)
   SRCPARAM FIREPUMP        1.0e3  8.53   622.0  57.4   0.127  0.375525 1.387583
** Hourly emissions file
   HOUREMIS no2-2005-mc.emi
SO FINISHED
**
****************************************
** D Receptor Pathway
****************************************
**
**
** Receptors
RE STARTING
** Species to be output in sampler file
RE MULTLIST (NO2,NO,O3)
RE ELEVUNIT METERS
** From file ../aermap/prop_bndy.rec
   DISCCART     560.530   4824.800     847.44     847.44
   DISCCART     560.540   4824.800     847.53     847.53
   DISCCART     560.550   4824.800     847.56     847.56
   DISCCART     560.560   4824.800     847.56     847.56
   DISCCART     560.570   4824.800     847.63     847.63
   DISCCART     560.580   4824.800     847.65     847.65
   DISCCART     560.590   4824.800     847.68     847.68
   DISCCART     560.600   4824.800     847.76     847.76
   DISCCART     560.610   4824.800     847.78     847.78
   DISCCART     560.620   4824.800     847.78     847.78
   DISCCART     560.630   4824.800     847.79     847.79
   DISCCART     560.630   4824.810     847.75     847.75
   DISCCART     560.630   4824.820     847.73     847.73
   DISCCART     560.630   4824.830     847.68     847.68
   DISCCART     560.630   4824.840     847.66     847.66
   DISCCART     560.630   4824.850     847.70     847.70
   DISCCART     560.630   4824.860     847.75     847.75
   DISCCART     560.630   4824.870     847.82     847.82
   DISCCART     560.630   4824.880     847.91     847.91
   DISCCART     560.630   4824.890     847.95     847.95
   DISCCART     560.630   4824.900     848.02     848.02
   DISCCART     560.620   4824.900     847.99     847.99
   DISCCART     560.610   4824.900     847.89     847.89
   DISCCART     560.600   4824.900     847.79     847.79
   DISCCART     560.590   4824.900     847.69     847.69
   DISCCART     560.580   4824.900     847.60     847.60
   DISCCART     560.570   4824.900     847.49     847.49
   DISCCART     560.560   4824.900     847.38     847.38
   DISCCART     560.550   4824.900     847.22     847.22
   DISCCART     560.540   4824.900     847.02     847.02
   DISCCART     560.530   4824.900     846.89     846.89
   DISCCART     560.530   4824.890     846.94     846.94
   DISCCART     560.530   4824.880     846.99     846.99
   DISCCART     560.530   4824.870     847.05     847.05
   DISCCART     560.530   4824.860     847.08     847.08
   DISCCART     560.530   4824.850     847.10     847.10
   DISCCART     560.530   4824.840     847.14     847.14
   DISCCART     560.530   4824.830     847.19     847.19
   DISCCART     560.530   4824.820     847.25     847.25
   DISCCART     560.530   4824.810     847.34     847.34
   DISCCART     560.530   4824.800     847.44     847.44
** From file ../aermap/50m.rec
   DISCCART     560.380   4824.650     850.25     850.25
   DISCCART     560.380   4824.700     848.94     848.94
   DISCCART     560.380   4824.750     847.92     847.92
   DISCCART     560.380   4824.800     847.30     847.30
   DISCCART     560.380   4824.850     846.76     846.76
   DISCCART     560.380   4824.900     846.31     846.31
   DISCCART     560.380   4824.950     845.81     845.81
   DISCCART     560.380   4825.000     845.29     845.29
   DISCCART     560.380   4825.050     845.10     845.10
   DISCCART     560.430   4824.650     850.24     850.24
   DISCCART     560.430   4824.700     848.84     848.84
   DISCCART     560.430   4824.750     847.94     847.94
   DISCCART     560.430   4824.800     847.32     847.32
   DISCCART     560.430   4824.850     846.82     846.82
   DISCCART     560.430   4824.900     846.41     846.41
   DISCCART     560.430   4824.950     845.87     845.87
   DISCCART     560.430   4825.000     845.41     845.41
   DISCCART     560.430   4825.050     845.40     845.40
   DISCCART     560.480   4824.650     850.79     854.23
   DISCCART     560.480   4824.700     848.98     848.98
   DISCCART     560.480   4824.750     848.02     848.02
   DISCCART     560.480   4824.800     847.50     847.50
   DISCCART     560.480   4824.850     847.05     847.05
   DISCCART     560.480   4824.900     846.58     846.58
   DISCCART     560.480   4824.950     846.15     846.15
   DISCCART     560.480   4825.000     845.89     845.89
   DISCCART     560.480   4825.050     846.38     846.38
   DISCCART     560.530   4824.650     853.91     853.91
   DISCCART     560.530   4824.700     849.92     849.92
   DISCCART     560.530   4824.750     847.99     847.99
   DISCCART     560.530   4824.800     847.44     847.44
   DISCCART     560.530   4824.850     847.10     847.10
   DISCCART     560.530   4824.900     846.89     846.89
   DISCCART     560.530   4824.950     847.07     847.07
   DISCCART     560.530   4825.000     847.13     847.13
   DISCCART     560.530   4825.050     847.88     847.88
   DISCCART     560.580   4824.650     853.61     853.61
   DISCCART     560.580   4824.700     850.29     850.29
   DISCCART     560.580   4824.750     848.09     848.09
   DISCCART     560.580   4824.800     847.65     847.65
   DISCCART     560.580   4824.900     847.60     847.60
   DISCCART     560.580   4824.950     848.17     848.17
   DISCCART     560.580   4825.000     848.22     848.22
   DISCCART     560.580   4825.050     848.38     848.38
   DISCCART     560.630   4824.650     853.24     853.24
   DISCCART     560.630   4824.700     850.31     850.31
   DISCCART     560.630   4824.750     848.08     848.08
   DISCCART     560.630   4824.800     847.79     847.79
   DISCCART     560.630   4824.850     847.70     847.70
   DISCCART     560.630   4824.900     848.02     848.02
   DISCCART     560.630   4824.950     848.41     848.41
   DISCCART     560.630   4825.000     848.56     848.56
   DISCCART     560.630   4825.050     848.38     848.38
   DISCCART     560.680   4824.650     853.08     853.08
   DISCCART     560.680   4824.700     850.09     850.09
   DISCCART     560.680   4824.750     848.13     848.13
   DISCCART     560.680   4824.800     847.89     847.89
   DISCCART     560.680   4824.850     848.02     848.02
   DISCCART     560.680   4824.900     848.23     848.23
   DISCCART     560.680   4824.950     848.46     848.46
   DISCCART     560.680   4825.000     848.65     848.65
   DISCCART     560.680   4825.050     848.64     848.64
   DISCCART     560.730   4824.650     853.05     853.05
   DISCCART     560.730   4824.700     850.69     850.69
   DISCCART     560.730   4824.750     848.68     848.68
   DISCCART     560.730   4824.800     848.09     848.09
   DISCCART     560.730   4824.850     848.27     848.27
   DISCCART     560.730   4824.900     848.54     848.54
   DISCCART     560.730   4824.950     848.72     848.72
   DISCCART     560.730   4825.000     848.91     848.91
   DISCCART     560.730   4825.050     848.89     848.89
   DISCCART     560.780   4824.650     853.42     853.42
   DISCCART     560.780   4824.700     851.71     851.71
   DISCCART     560.780   4824.750     849.59     849.59
   DISCCART     560.780   4824.800     848.47     848.47
   DISCCART     560.780   4824.850     848.40     848.40
   DISCCART     560.780   4824.900     848.73     848.73
   DISCCART     560.780   4824.950     848.97     848.97
   DISCCART     560.780   4825.000     849.11     849.11
   DISCCART     560.780   4825.050     848.89     848.89
RE FINISHED

** Meteorology
ME STARTING
ME SCISFC ../../Common/metsci/KBOI2005.SFC
ME SCIPRF ../../Common/metsci/KBOI2005.PRF
ME ASOS1MIN '../../Common/ASOS1Min/64050KBOI2005.dat' '../../Common/ASOS1Min/64060KBOI2005.dat' 43.567 -116.241
ME FINISHED

** Terrain
TE STARTING
   TELUFILE ../../Common/tersci/terrain.utm.ter
TE FINISHED
