!*******************************************************************************
!            Check Project End Input
!*******************************************************************************
INTEGER FUNCTION Check_End( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( endT ), INTENT( IN ) :: check

INTEGER irv

INTEGER, EXTERNAL :: CheckTime
INTEGER, EXTERNAL :: CheckStep

!==== Initialize

Check_End = SCIPfailure

!==== Check the time

irv = CheckTime( check%time,SPECIAL_NOTSET )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the step

irv = CheckStep( check%step )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Return

Check_End = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Start Input
!*******************************************************************************
INTEGER FUNCTION Check_Start( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( startT ), INTENT( IN ) :: check

INTEGER irv

INTEGER, EXTERNAL :: CheckTime

REAL rMin,rMax

!==== Initialize

Check_Start = SCIPfailure

!==== Check the time

irv = CheckTime( check%time,SPECIAL_NONE )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the time zone

IF( check%time%reference == HT_LOCAL )THEN
  rMin = -24.
  rMax =  24.
  IF( BadValue('CheckStart','time zone',SPECIAL_DEFAULT+SPECIAL_NOTSET, &
                rMin,rMax,check%zone) )GOTO 9999
END IF

!==== Return

Check_Start = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Time Input
!*******************************************************************************
INTEGER FUNCTION Check_Time( check )

USE checkErr
USE timstruct_fd
USE SCIPresults_fd

IMPLICIT NONE

TYPE( temporalT ), INTENT( IN ) :: check

INTEGER irv

CHARACTER(40),PARAMETER :: ROUTINE = 'Check_Time'
CHARACTER(80)           :: string


INTEGER, EXTERNAL :: Check_Start
INTEGER, EXTERNAL :: Check_End
INTEGER, EXTERNAL :: ComputeDuration

REAL rMin
REAL rMax
REAL runtime

!==== Initialize

Check_Time = SCIPfailure

!==== Check the start time

irv = Check_Start( check%start )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the end time

irv = Check_End( check%end )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check for consistency in end time

string = 'runtime'

irv = ComputeDuration( check%start%time,check%end%time,runtime )
IF( irv /= SCIPsuccess )THEN
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE, &
               rMin,rMax,check%end%time%runtime) )GOTO 9999
  runtime = check%end%time%runtime
END IF

!==== Check for end time > start time

rMin = check%start%time%runtime + SPACING(check%start%time%runtime)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,runtime) )GOTO 9999

!==== Return

Check_Time = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Options Input
!*******************************************************************************
INTEGER FUNCTION Check_Options( check )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( optionsT ), INTENT( IN ) :: check

INTEGER irv

INTEGER, EXTERNAL :: CheckOptions

!==== Initialize

Check_Options = SCIPfailure

!==== Check the time

irv = CheckOptions( check )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Return

Check_Options = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Domain Input
!*******************************************************************************
INTEGER FUNCTION Check_Domain( check )

USE checkErr
USE domain_fd
USE SCIPresults_fd

IMPLICIT NONE

TYPE( spatialT ), INTENT( INOUT ) :: check

INTEGER irv


INTEGER, EXTERNAL :: CheckDomain
INTEGER, EXTERNAL :: CheckReference
INTEGER, EXTERNAL :: CheckReferenceUTM

!==== Initialize

Check_Domain = SCIPfailure

!==== Check the Domain

irv = CheckDomain( check%domain )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Reference

IF( check%domain%coord == HD_UTM )THEN
  irv = CheckReferenceUTM( check%domain,check%reference )
  IF( irv /= SCIPsuccess )GOTO 9999
ELSE IF( check%domain%coord /= HD_LATLON )THEN
  irv = CheckReference( check%reference )
  IF( irv /= SCIPsuccess )GOTO 9999
END IF

!==== Return

Check_Domain = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Weather Input
!*******************************************************************************
INTEGER FUNCTION Check_Weather( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( weatherT ), INTENT( IN ) :: check

INTEGER irv

INTEGER, EXTERNAL :: CheckWeatherFlags
INTEGER, EXTERNAL :: CheckWeatherMet
INTEGER, EXTERNAL :: CheckWeatherBL
INTEGER, EXTERNAL :: CheckWeatherLSV
INTEGER, EXTERNAL :: CheckWeatherPrecip
INTEGER, EXTERNAL :: CheckWeatherTerrain

!==== Initialize

Check_Weather = SCIPfailure

!==== Check the Weather flags

irv = CheckWeatherFlags( check%flags,check%met%type )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Weather met

irv = CheckWeatherMet( check%met )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Weather BL

irv = CheckWeatherBL( check%BL )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Weather LSV

irv = CheckWeatherLSV( check%LSV )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Weather precip

irv = CheckWeatherPrecip( check%precip )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Weather terrain

IF( check%flags%doMC == SCIPon )THEN
  irv = CheckWeatherTerrain( check%terrain )
  IF( irv /= SCIPsuccess )GOTO 9999
END IF

!==== Return

Check_Weather = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Input
!*******************************************************************************
INTEGER FUNCTION Check_Inp( check,mtlList )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

TYPE( inputT ),                 INTENT( INOUT ) :: check
TYPE( materialT ),DIMENSION(*), INTENT( IN    ) :: mtlList

INTEGER irv
INTEGER iMtl
INTEGER jMtl

LOGICAL checkSubstrate

INTEGER, EXTERNAL :: Check_Time
INTEGER, EXTERNAL :: Check_Domain
INTEGER, EXTERNAL :: Check_Options
INTEGER, EXTERNAL :: Check_Material

!==== Initialize

Check_Inp = SCIPfailure

!==== Check the time

irv = Check_Time( check%time )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the domain

irv = Check_Domain( check%domain )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the options

irv = Check_Options( check%option )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the materials

checkSubstrate = check%option%substrate > 0

DO iMtl = 1,check%mtlHead%number
  irv = Check_Material( mtlList(iMtl),checkSubstrate )
  IF( irv /= SCIPsuccess )THEN
    eRoutine = TRIM(mtlList(iMtl)%name)//':'//TRIM(eRoutine)
    GOTO 9999
  END IF
END DO

IF( check%mtlHead%number > 1 )THEN
  DO iMtl = 1,check%mtlHead%number-1
    DO jMtl = iMtl+1,check%mtlHead%number
      IF( TRIM(mtlList(iMtl)%name) == TRIM(mtlList(jMtl)%name) )THEN
        nError   = IV_ERROR
        eRoutine = 'CheckInput'
        eMessage = 'Multiple material with the same name'
        eInform  = 'Name = "'//TRIM(mtlList(iMtl)%name)//'"'
        GOTO 9999
      END IF
    END DO
  END DO
END IF

!==== Return

Check_Inp = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Material Input
!*******************************************************************************
INTEGER FUNCTION Check_Material( check,substrate )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

TYPE( materialT ), INTENT( IN ) :: check
LOGICAL,           INTENT( IN ) :: substrate

INTEGER irv

INTEGER, EXTERNAL :: CheckMaterialGas
INTEGER, EXTERNAL :: CheckMaterialParticle
INTEGER, EXTERNAL :: CheckMaterialLiquid
INTEGER, EXTERNAL :: CheckMaterialAerosol

INTEGER mtlType
LOGICAL secondEvap

!==== Initialize

Check_Material = SCIPfailure

!==== Check the material name

IF( LEN_TRIM(check%name) <= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'Check_Material'
  eMessage = 'Missing material name'
  eInform  = 'All materials must be identified by a unique string'
  GOTO 9999
END IF

!==== Check the material type/properties

mtlType = IBITS(check%type,0,HMB_BASIC)         !Basic types only


SELECT CASE( mtlType )
  CASE( HM_GAS )
    irv = CheckMaterialGas( check%matData )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HM_PARTICLE )
    irv = CheckMaterialParticle( check%matData )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HM_LIQUID )
    secondEvap = BTEST(check%type,HMB_2NDEVAP)
    irv = CheckMaterialLiquid( check%matData,secondEvap,substrate )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HM_WETPARTICLE )
    irv = CheckMaterialParticle( check%matData )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HM_AEROSOL )
    irv = CheckMaterialAerosol( check%matData )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'Check_Material'
    eMessage = 'Invalid material type'
    eInform  = 'valid material types are HM_GAS,HM_PARTICLE'
    eInform  = TRIM(eInform)//',HM_LIQUID,HM_WETPARTICLE'
    eInform  = TRIM(eInform)//',HM_AEROSOL'
    GOTO 9999
END SELECT

!==== Return

Check_Material = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Project Release Input
!*******************************************************************************
INTEGER FUNCTION Check_Release( check,matl )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

TYPE( releaseT ),  INTENT( IN ) :: check
TYPE( materialT ), INTENT( IN ) :: matl

CHARACTER(40),PARAMETER :: ROUTINE = 'Check_Release'

INTEGER irv

INTEGER special
INTEGER specialZ
INTEGER specialU

INTEGER distMin
INTEGER distMax
LOGICAL distMMD
INTEGER mtlType

REAL    rMin,rMax
REAL(8) dMin,dMax

CHARACTER(80)  string
CHARACTER(80)  errMessage
CHARACTER(32)  mtlName, relName

LOGICAL, EXTERNAL :: IsValidData       !Function used by SCIPUFF to check release
INTEGER, EXTERNAL :: GetMaterialDistribution
INTEGER, EXTERNAL :: CheckReleaseCont
INTEGER, EXTERNAL :: CheckReleaseInst
INTEGER, EXTERNAL :: CheckReleaseXInst
INTEGER, EXTERNAL :: CheckReleaseXInst3
INTEGER, EXTERNAL :: CheckReleaseMove
INTEGER, EXTERNAL :: CheckReleaseStack
INTEGER, EXTERNAL :: CheckReleaseStack3
INTEGER, EXTERNAL :: CheckReleaseFile
INTEGER, EXTERNAL :: CheckReleasePuff
INTEGER, EXTERNAL :: CheckReleasePool
INTEGER, EXTERNAL :: CheckReleaseFlash
INTEGER, EXTERNAL :: CheckReleaseArea

!==== Initialize

Check_Release = SCIPfailure

!==== Check the Release time

string = 'release time'
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%tRel) )GOTO 9999

!==== Check the Release status

IF( IsValidData(check%status) )THEN
  special = SPECIAL_NONE
ELSE
  special = SPECIAL_DEFER
END IF

specialZ = special
IF( check%type == HR_POOL )specialZ = SPECIAL_VALUE

specialU = special + SPECIAL_DEFAULT + SPECIAL_NOTSET

!==== Check the Release location

rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
dMin = -HUGE(dMin)
dMax =  HUGE(dMax)
string = 'release X location'
IF( BadValue(ROUTINE,string,special,dMin,dMax,check%xRel) )GOTO 9999
string = 'release Y location'
IF( BadValue(ROUTINE,string,special,dMin,dMax,check%yRel) )GOTO 9999
string = 'release Z location'
IF( BadValue(ROUTINE,string,specialZ,rMin,rMax,check%zRel) )GOTO 9999

!==== Check the Release material

IF( check%type /= HR_FILE )THEN
  relName = check%material
  mtlName = matl%name
  CALL CUPPER( relName )
  CALL CUPPER( mtlName )
  IF( TRIM(relName) /= TRIM(mtlName) )THEN
    string = 'release material name'
    errMessage = 'release/material mismatch : '//TRIM(check%material)//' : '//TRIM(matl%name)
    CALL SetCheckError(ROUTINE,string,errMessage)
    GOTO 9999
  END IF
END IF

irv = GetMaterialDistribution( matl,distMin,distMax,distMMD,mtlType )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Check the Release type/properties

SELECT CASE (ABS(check%type))
  CASE( HR_CONT,HR_CONTF )
    irv = CheckReleaseCont( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_INST )
    irv = CheckReleaseInst( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_XINST )
    irv = CheckReleaseXInst( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_XINST3 )
    irv = CheckReleaseXInst3( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_MOVE )
    irv = CheckReleaseMove( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_FILE )
    irv = CheckReleaseFile( check%relData,special )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_STACK,HR_STACKF,HR_PRIME,HR_STACKFP,-HR_STACK  )
    irv = CheckReleaseStack( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_STACK3,HR_STACK3F )
    irv = CheckReleaseStack3( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_AREA )
    irv = CheckReleaseArea( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_AREAF )
    irv = CheckReleaseArea( check%relData,special,distMin,distMax,distMMD,mtlType )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_PUFF )
    irv = CheckReleasePuff( check%relData,special,distMin,distMax )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_POOL )
    IF( distMin /= 0 )THEN
      string = 'release material type'
      errMessage = 'Pool release requires liquid material'
      CALL SetCheckError( ROUTINE,string,errMessage )
      GOTO 9999
    END IF
    irv = CheckReleasePool( check%relData,special )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE( HR_FINST )
    irv = CheckReleaseFlash( check%relData,special,matl%type )
    IF( irv /= SCIPsuccess )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'Check_Release'
    eMessage = 'Invalid Release type'
    eInform  = 'valid Release types are HR_CONT,HR_INST,HR_XINST,HR_MOVE,HR_FILE,HR_STACK,HR_PUFF'
    eInform  = TRIM(eInform)//',HR_CONTF,HR_STACKF'
    eInform  = TRIM(eInform)//',HR_AREA'
    eInform  = TRIM(eInform)//',HR_AREAF'
    eInform  = TRIM(eInform)//',HR_POOL'
    eInform  = TRIM(eInform)//',HR_FINST'
    eInform  = TRIM(eInform)//',HR_PRIME'
    eInform  = TRIM(eInform)//',HR_STACKFP'
    GOTO 9999
END SELECT

!==== Return

Check_Release = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Stack Releases
!*******************************************************************************
INTEGER FUNCTION Check_StackReleases( nRel,rel,nMat,matl )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

INTEGER,                            INTENT( IN ) :: nRel
TYPE( releaseT ),  DIMENSION(nRel), INTENT( IN ) :: rel
INTEGER,                            INTENT( IN ) :: nMat
TYPE( materialT ), DIMENSION(nMat), INTENT( IN ) :: matl

INTEGER, PARAMETER :: NON_STACK      = 0
INTEGER, PARAMETER :: MOMENTUM_STACK = 1
INTEGER, PARAMETER :: PASSIVE_STACK  = 2

INTEGER, DIMENSION(:), ALLOCATABLE :: StackType

INTEGER i, j, mtlType, alloc_stat, stack_distribution
LOGICAL checkDynamics, checkStack
REAL    stack_dryFrac

TYPE( relStackT ) stack
TYPE( relStack3T ) stack3

LOGICAL, EXTERNAL :: ColocatedStack

Check_StackReleases = SCIPfailure

!------  Allocate type array

ALLOCATE( StackType(nRel), STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError = IV_ERROR
  eRoutine = 'Check_StackReleases'
  eMessage = 'Error allocating array for release types'
  GOTO 9999
END IF

!--- Scan release list for stack releases

DO i = 1,nRel
  checkStack = .FALSE.
  IF( rel(i)%type == HR_STACK .OR. rel(i)%type == HR_STACKF )THEN
    stack = TRANSFER(rel(i)%relData,stack)
    stack_dryFrac = stack%dryFrac
    stack_distribution = stack%distribution
    checkStack = .TRUE.
  ELSE IF( rel(i)%type == HR_STACK3 )THEN
    stack3 = TRANSFER(rel(i)%relData,stack3)
    stack_dryFrac = stack3%dryFrac
    stack_distribution = stack3%distribution
    checkStack = .TRUE.
  END IF
  IF( checkStack )THEN
    DO j = 1,nMat
      IF( TRIM(rel(i)%material) == TRIM(matl(j)%name) )THEN
        mtlType = IBITS(matl(j)%type,0,HMB_BASIC)
        EXIT
      END IF
    END DO
    checkDynamics = (mtlType == HM_GAS)
    checkDynamics = checkDynamics .OR. (mtlType == HM_AEROSOL)
    IF( mtlType == HM_LIQUID )THEN
      IF( stack_distribution == 0 )THEN
        checkDynamics = .TRUE.
      ELSE
        checkDynamics = checkDynamics .OR. (stack_dryFrac < 1.0 .AND. stack_dryFrac >= 0.0)  !Two-phase release
      END IF
    END IF
    IF( checkDynamics )THEN
      StackType(i) = MOMENTUM_STACK
    ELSE
      StackType(i) = PASSIVE_STACK
    END IF
  ELSE
    StackType(i) = NON_STACK
  END IF
END DO

!---- Check any PASSIVE stacks for colocated MOMENTUM stacks

DO i = 1,nRel
  IF( StackType(i) == PASSIVE_STACK )THEN

    checkDynamics = .FALSE.

CheckLoop: DO j = 1,nRel
      IF( StackType(j) == MOMENTUM_STACK )THEN
        checkDynamics = ColocatedStack( rel(i),rel(j) )
        IF( checkDynamics )EXIT CheckLoop
      END IF
    END DO CheckLoop

    IF( .NOT.checkDynamics )THEN
      nError = IV_ERROR
      eRoutine = 'Check_StackReleases'
      eMessage = 'Illegal stack release'
      eInform  = 'Stack release of non-vapor material must be colocated with a vapor stack'
      eACtion  = 'Non-vapor material = '//TRIM(rel(i)%material)
      GOTO 9999
    END IF

  END IF
END DO

!==== Return

Check_StackReleases = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check If stacks are collocated
!*******************************************************************************
LOGICAL FUNCTION ColocatedStack( relP,relM )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

TYPE( releaseT ),  INTENT( IN ) :: relP     !Passive stack
TYPE( releaseT ),  INTENT( IN ) :: relM     !Momentum stack

REAL del

REAL diameterP, diameterM
REAL durationP, durationM

TYPE( relStackT ) stack
TYPE( relStack3T ) stack3

del = SNGL(relP%xRel - relM%xRel)**2 &
    + SNGL(relP%yRel - relM%yRel)**2 &
    + (relP%zRel - relM%zRel)**2

IF( del /= 0.0 )THEN
  ColocatedStack = .FALSE.
  GOTO 9999
END IF

IF( relP%type == HR_STACK )THEN
  stack = TRANSFER(relP%relData,stack)
  diameterP = stack%diameter
  durationP = stack%duration
ELSE
  stack3 = TRANSFER(relP%relData,stack3)
  diameterP = stack3%diameter
  durationP = stack3%duration
END IF

IF( relM%type == HR_STACK )THEN
  stack = TRANSFER(relM%relData,stack)
  diameterM = stack%diameter
  durationM = stack%duration
ELSE
  stack3 = TRANSFER(relM%relData,stack3)
  diameterM = stack3%diameter
  durationM = stack3%duration
END IF

IF( diameterP /= diameterM )THEN
  ColocatedStack = .FALSE.
  GOTO 9999
END IF

IF( relP%tRel < relM%tRel )THEN
  ColocatedStack = .FALSE.
  GOTO 9999
END IF

IF( relP%tRel+durationP > relM%tRel+durationM )THEN
  ColocatedStack = .FALSE.
  GOTO 9999
END IF

ColocatedStack = .TRUE.

9999 CONTINUE

RETURN
END
