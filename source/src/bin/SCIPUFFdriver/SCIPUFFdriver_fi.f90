MODULE SCIPUFFdriver_fi

  USE tooluser_fd
  USE default_fd
  USE param_fd
  USE SWIMparam_fd
  USE AERMODreceptor_fd
  USE AERMODmet_fd

  IMPLICIT NONE

  INTEGER, PARAMETER :: SUCCESS =  1
  INTEGER, PARAMETER :: FAILURE = -1

  INTEGER, PARAMETER :: MET_AERSRF = 0  !AERMOD srf met file
  INTEGER, PARAMETER :: MET_AERPRF = 1  !AERMOD prf met file
  INTEGER, PARAMETER :: MET_SCISRF = 2  !SCIPUFF sfc met file
  INTEGER, PARAMETER :: MET_SCIPRF = 3  !SCIPUFF upper air file
  INTEGER, PARAMETER :: MET_SCIGRD = 4  !SCIPUFF gridded (MEDOC) file
  INTEGER, PARAMETER :: MET_SCITER = 5  !SCIPUFF terrain file
  INTEGER, PARAMETER :: MET_SCILIS = 6  !SCIPUFF met list file (supersedes other met)
  INTEGER, PARAMETER :: MET_SCIFIX = 7  !SCIPUFF fixed met input
  INTEGER, PARAMETER :: MET_MEDLIS = 8  !List of gridded MEDOC files
  INTEGER, PARAMETER :: MET_ASOS1M = 9  !ASOS 1 Minute (DS6405) met file

!  INTEGER, PARAMETER :: HR_AREA  = -1  !These must be distinct from SCIPUFF HR parameters
  INTEGER, PARAMETER :: HR_AREAC = -2
  INTEGER, PARAMETER :: HR_VOL   = -3

  INTEGER :: lun     = 1                !Unit number for SCIPUFF input file
  INTEGER :: lun_sam = 2                !Unit number for sampler input file
  INTEGER :: lun_met = 3                !Unit number for met list file
  INTEGER :: lun_emi = 4                !Unit number for hourly emission file
  INTEGER :: lun_pri = 98               !Unit number for PRIME input file
  INTEGER :: lun_tmp = 99

!------ SCIPUFF structures

  TYPE( createNewT ) new
  TYPE( materialT  ),DIMENSION(:), ALLOCATABLE :: mtlList
  TYPE( releaseT   ),DIMENSION(:), ALLOCATABLE :: relList
  TYPE( releaseMCT ), DIMENSION(:), ALLOCATABLE :: relMCList
  TYPE( pendT      ) run
  TYPE( limitT     ) limit

!------ Various local release structures (for reading input)

  TYPE( relInstT      ) relInstData
  TYPE( relStackT     ) relStackData
  TYPE( relStackFileT ) relStackFileData
  TYPE( relContT      ) relContData
  TYPE( relContFileT  ) relContFileData
  TYPE( relAreaT      ) relAreaData
  TYPE( relAreaFileT  ) relAreaFileData

!------ Source info

  CHARACTER(512) emiFile        !Hourly emissions file

  INTEGER jul0, year0           !For reading times on file
  REAL    hour0, tEmi, tNextEmi

  LOGICAL :: lEmissionFile  = .FALSE.
  LOGICAL :: lReadEmission  = .FALSE.
  LOGICAL :: lIntrpEmission = .FALSE.
  LOGICAL :: lPRIME = .FALSE.   ! PRIME default is set to FALSE for SCICHEM 3.1

  REAL, DIMENSION(:), ALLOCATABLE :: emiRate1, emiTemp1, emiVel1
  REAL, DIMENSION(:), ALLOCATABLE :: emiRate2, emiTemp2, emiVel2
  REAL, DIMENSION(:), ALLOCATABLE :: SrcFlag

  LOGICAL, DIMENSION(:), ALLOCATABLE :: lRelPrime

  INTEGER nAreaSrc, nrel0

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: iAreaSrc

!------ Local gas material

  TYPE( matGasT ), DIMENSION(:), ALLOCATABLE :: gasMatl
  TYPE( matGasT ) :: gasMatl0

!------ Local gas material

  TYPE( matParticleT ), DIMENSION(:), ALLOCATABLE :: partMatl
  TYPE( matParticleT ) :: partMatl2p5
  TYPE( matParticleT ) :: partMatl10

  INTEGER nmat
  LOGICAL lMApathway
  LOGICAL :: isProj = .FALSE.  ! Mandatory projection keyword in CO

!------ Multicomponent

  INTEGER nMC, nMCrel

  CHARACTER(1000)                               :: MCList = ''
  CHARACTER(16),    DIMENSION(:),   ALLOCATABLE :: MCname
  REAL,             DIMENSION(:,:), ALLOCATABLE :: MCrate
  REAL,             DIMENSION(:,:), ALLOCATABLE :: emiMC1, emiMC2

!------ Saved info

  INTEGER n_re, i_re, nsamp, ndisc_re
  INTEGER nMet, isrf, iprf, nvertlev
  REAL    vdep, zruf
  REAL    elevCnv, xfac, yoff
  REAL    xmin, xmax, ymin, ymax
  LOGICAL lTime, lDomRef, lDomLimits, lDomRecptr, lSetupOnly, lZruf, lPrimeFile, lformat
  LOGICAL lSetPrjTcnv, lSetTzone, lSetMetTcnv, lSetDT, lSetDTout, lSetTimeAvg, lIncluded
  LOGICAL lSetZmax, lSetLSV, lSetUUcalm, lSetSLcalm, lSetSrfParam, lSetTimeBin
  LOGICAL lDeletePrj

  CHARACTER(1)   hemiUTM
  CHARACTER(64)  landuse
  CHARACTER(64)  sensorClass
  CHARACTER(128) path_in
  CHARACTER(512) fname, prjname, primeFile

  TYPE( AERMODmet ), DIMENSION(MAXMETINP) :: metInp

  TYPE( AERMODreceptorInfo ) :: receptor

  TYPE( NetRE ), DIMENSION(MAXNUMRE) :: receptor_net

  TYPE( DiscRE ), POINTER :: First_Disc, Disc_re

!------ Output file names

  CHARACTER(512) metFile, postFile, puffFile, prjFile, concFile, depFile, dosFile, smpFile

!------ get_next_data variables

  INTEGER, PARAMETER :: MAXN = 40     !Max. no. of arguments on a line

  INTEGER nch, narg, ikwrd
  LOGICAL lerr

  CHARACTER(16)   kwrd
  CHARACTER(1000) line

  CHARACTER(128), DIMENSION(MAXN) :: carg

END MODULE SCIPUFFdriver_fi
