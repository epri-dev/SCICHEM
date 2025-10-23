INTEGER FUNCTION InitAERMOD()

!------ Initialize AERMOD data

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER i, alloc_stat

receptor%lAvg      = .FALSE.
receptor%lConc     = .FALSE.
receptor%lDepTot   = .TRUE.
receptor%lDepDry   = .TRUE.
receptor%lDepWet   = .TRUE.
receptor%lFlagPole = .FALSE.
receptor%Flagdf    = 0.
receptor%elevCnv   = 1.
receptor%dtSampler = DEF_VAL_R

n_re  = 0

DO i = 1,MAXNUMRE
  receptor_net(i)%id = 'Not_Set'
END DO

NULLIFY( First_Disc )

vdep    = NOT_SET_R
elevCnv = 1.
yoff    = 0.

DO i = 1,MAXMETINP
  metInp(i)%file     = 'NOT_SET'
  metInp(i)%type     = NOT_SET_I
  metInp(i)%staNum   = NOT_SET_I
  metInp(i)%year     = NOT_SET_I
  metInp(i)%x        = NOT_SET_R
  metInp(i)%Y        = NOT_SET_R
  metInp(i)%baseElev = NOT_SET_R
  metInp(i)%name     = 'NOT_SET'
END DO

lTime      = .FALSE.
lDomRef    = .FALSE.
lDomLimits = .FALSE.
lDomRecptr = .FALSE.
lZruf      = .FALSE.
lPrimeFile = .FALSE.
lDeletePrj = .FALSE.
lIncluded  = .FALSE.

!------ Set flags for reporting use of default values

lSetPrjTcnv  = .FALSE.
lSetTzone    = .FALSE.
lSetMetTcnv  = .FALSE.
lSetDT       = .FALSE.
lSetDTout    = .FALSE.
lSetTimeAvg  = .FALSE.
lSetZmax     = .FALSE.
lSetLSV      = .FALSE.
lSetUUcalm   = .FALSE.
lSetSLcalm   = .FALSE.
lSetSrfParam = .FALSE.
lSetTimeBin  = .FALSE.

primeFile = ''
path_in   = ''

nvertlev = 0

nmat = 1  !Default gas material
lMApathway = .FALSE.

nMC = 0
ALLOCATE( MCrate(1,1),STAT=alloc_stat )

emiFile = ''

new%input%domain%domain%coord = I_CARTESIAN !Default coordinates (previously I_METERS)
new%input%option%samplerFile = ''

xfac = 1. !AERMOD-style Cartesian or UTM input is kilometers by default

nAreaSrc = 1

metFile  = ''
postFile = ''
smpFile  = ''
puffFile = ''
prjFile  = ''
concFile = ''
depFile  = ''
dosFile  = ''

InitAERMOD = SUCCESS

RETURN
END
