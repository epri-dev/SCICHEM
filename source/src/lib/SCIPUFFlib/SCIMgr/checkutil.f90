!*******************************************************************************
!            Check a timeT structure
!*******************************************************************************
INTEGER FUNCTION CheckTime( check,special )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( timeT ), INTENT( IN ) :: check
INTEGER,       INTENT( IN ) :: special

CHARACTER(40),          PARAMETER :: ROUTINE = 'CheckTime'
INTEGER, DIMENSION(12), PARAMETER :: NDAY = (/31,28,31,30,31,30, &
                                              31,31,30,31,30,31/)

INTEGER       iMin,iMax
REAL          rMin,rMax
LOGICAL       ymd
CHARACTER(80) string, errMessage

LOGICAL, EXTERNAL :: leap_year

!==== Initialize

CheckTime = SCIPfailure

!==== Check the time reference

string = 'time reference'
SELECT CASE( check%reference )
  CASE( HT_UTC,HT_LOCAL )

  CASE DEFAULT
    errMessage = TRIM(string)//' must be either HT_UTC or HT_LOCAL'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Check the time runtime

string = 'runtime'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%runtime) )GOTO 9999

!==== Check the time hour

string = 'time(hour)'
rMin = 0.  - SPACING(0.)
rMax = 24. + SPACING(24.)
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%hour) )GOTO 9999

!==== Check for YMD specification

ymd = .NOT.SpecialValue(SPECIAL_VALUE,check%year) .OR. &
      .NOT.SpecialValue(SPECIAL_VALUE,check%month)

!==== Check time specification

IF( ymd )THEN

  string = 'time(year)'
  IF( check%year < 100 )THEN
    iMin = 0
    iMax = 99
  ELSE
    iMin = 1800
    iMax = 2200
  END IF
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%year) )GOTO 9999

  string = 'time(month)'
  iMin = 1
  iMax = 12
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%month) )GOTO 9999

  string = 'time(day)'
  iMin = 1
  iMax = NDAY(check%month)
  IF( check%month == 2 .AND. leap_year(check%year) )iMax = iMax + 1
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%day) )GOTO 9999

ELSE

  string = 'time(day)'
  iMin = 0
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,iMin,iMax,check%day) )GOTO 9999

END IF

!==== Return

CheckTime = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a stepT structure
!*******************************************************************************
INTEGER FUNCTION CheckStep( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( stepT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckStep'

REAL          rMin,rMax
CHARACTER(80) string

!==== Initialize

CheckStep = SCIPfailure

!==== Check the max time step

string = 'maximum time step'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%max) )GOTO 9999

!==== Check the output interval

string = 'output interval'
rMin = check%max/3600.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%output) )GOTO 9999

!==== Return

CheckStep = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a optionT structure
!*******************************************************************************
INTEGER FUNCTION CheckOptions( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( optionsT ), INTENT( IN ) :: check

CHARACTER(40),PARAMETER :: ROUTINE = 'CheckOptions'

INTEGER       iMin,iMax
REAL          rMin,rMax
CHARACTER(80) string

!==== Initialize

CheckOptions = SCIPfailure

!==== Check the Boundary Layer Points (NZBL)

string = 'number of boundary layer points'
iMin = 3
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%nzbl) )GOTO 9999

!==== Check the Puff split grid level (MGRD)

string = 'number of puff split grid levels'
iMax = 9
iMin = -iMax
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%mgrd) )GOTO 9999

!==== Check the Substrate Index       (SUBSTRATE)

string = 'surface substrate index'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%substrate) )GOTO 9999

!==== Check the Conditional Averaging (TAVG)

string = 'conditional averaging time'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%timeavg) )GOTO 9999

!==== Check the Minimum Mass          (CMIN)

string = 'minimum puff mass'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%massmin) )GOTO 9999

!==== Check the Surface Resolution    (DELMIN)

string = 'adaptive grid resolution'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%delmin) )GOTO 9999

!==== Check the Turbulence(Stable)    (WWTROP)

string = 'stable atmosphere turbulence'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%wwtrop) )GOTO 9999

!==== Check the Scale(Stable)         (SLTROP)

string = 'stable atmosphere turbulence scale'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%sltrop) )GOTO 9999

!==== Check the Dissipation(Stable)   (EPSTROP)

string = 'stable atmosphere turbulence dissipation'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%epstrop) )GOTO 9999

!==== Check the Turbulence(Calm)      (UUCALM)

string = 'calm atmosphere turbulence'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%uucalm) )GOTO 9999

!==== Check the Scale(Calm)           (SLCALM)

string = 'calm atmosphere turbulence scale'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%slcalm) )GOTO 9999

!==== Check the surface dosage height (ZDOSAGE)

string = 'surface dosage height'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%zdosage) )GOTO 9999

!==== Check sampler file

IF( LEN_TRIM(check%samplerfile) > 0 )THEN

!==== Check the Sampler output interval (DTSMP)

  string = 'sampler output interval'
  rMin = 0.
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%dtsampler) )GOTO 9999

!==== Check the Sampler input file      (FILE_SMP)

END IF

!==== Return

CheckOptions = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a domainT structure
!*******************************************************************************
INTEGER FUNCTION CheckDomain( check )

USE SCIMgr_fd
USE SCIMgr_fi
USE checkErr
USE default_fd

IMPLICIT NONE

TYPE( domainT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckDomain'

INTEGER       special
INTEGER       iMin,iMax
REAL          rMin,rMax
CHARACTER(80) string,errMessage

!==== Initialize

CheckDomain = SCIPfailure

!==== Check the Map type

string = 'domain mapping'
SELECT CASE( check%coord )
  CASE( HD_LATLON,HD_CARTESIAN,HD_METERS )

  CASE( HD_UTM )
    IF( .NOT.AvailUTM )THEN
      errMessage = 'Cannot use UTM coordinates. Transforms not available'
      CALL SetCheckError( ROUTINE,string,errMessage )
      GOTO 9999
    END IF

  CASE DEFAULT
    errMessage = TRIM(string)//' must be either HD_LATLON,HD_UTM or HD_CARTESIAN'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Check the UTMzone

IF( check%coord == HD_UTM )THEN
  IF( check%xmin /= DEF_VAL_R .AND. &
      check%xmax /= DEF_VAL_R .AND. &
      check%ymin /= DEF_VAL_R .AND. &
      check%ymax /= DEF_VAL_R       )THEN
    special = SPECIAL_NONE
  ELSE
    special = SPECIAL_NOTSET
  END IF
  string = 'UTM zone'
  iMin = -60
  iMax = 60
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%zoneUTM) )GOTO 9999
END IF

!==== Check the horizontal X Domain

SELECT CASE( check%coord )
  CASE( HD_LATLON )
    rMin = -360.
    rMax =  360.

  CASE( HD_UTM )
    rMin = -501.
    rMax =  1501.

  CASE DEFAULT
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)

END SELECT

string = 'horizontal X minimum'
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%xmin) )GOTO 9999

string = 'horizontal X maximum'
IF( check%xmin /= DEF_VAL_R ) rMin = check%xmin
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%xmax) )GOTO 9999

!==== Check the horizontal Y Domain

SELECT CASE( check%coord )
  CASE( HD_LATLON )
    rMin = -90.
    rMax =  90.

  CASE( HD_UTM )
    rMin = -10000.
    rMax =  10000.

  CASE DEFAULT
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)

END SELECT

string = 'horizontal Y minimum'
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%ymin) )GOTO 9999

string = 'horizontal Y maximum'
IF( check%ymin /= DEF_VAL_R ) rMin = check%ymin
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%ymax) )GOTO 9999

!==== Check the vertical Domain

string = 'vertical domain'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%zmax) )GOTO 9999

!==== Check the Horizontal resolution

string = 'horizontal resolution'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%hres) )GOTO 9999

!==== Check the Vertical resolution

string = 'vertical resolution'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%vres) )GOTO 9999

!==== Return

CheckDomain = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a referenceT structure
!*******************************************************************************
INTEGER FUNCTION CheckReference( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( referenceT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReference'

REAL          rMin,rMax
CHARACTER(80) string

!==== Initialize

CheckReference = SCIPfailure

!==== Check the reference origin

string = 'reference X origin'
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%x) )GOTO 9999
string = 'reference Y origin'
IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%y) )GOTO 9999

!==== Check the reference latitude

string = 'reference latitude'
rMin = -90.
rMax =  90.
IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%lat) )GOTO 9999

!==== Check the reference latitude

string = 'reference longitude'
rMin = -360.
rMax =  360.
IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%lon) )GOTO 9999

!==== Return

CheckReference = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a referenceT structure for UTM project
!*******************************************************************************
INTEGER FUNCTION CheckReferenceUTM( checkdom,checkref )

USE SCIMgr_fd
USE checkErr
USE default_fd
USE error_fi
USE datums

IMPLICIT NONE

TYPE( domainT    ), INTENT( IN  ) :: checkdom
TYPE( referenceT ), INTENT( OUT ) :: checkref

INTEGER irv

!==== Initialize

CheckReferenceUTM = SCIPfailure

!==== Set reference if domain is set

IF( checkdom%xmin /= DEF_VAL_R .AND. &
    checkdom%xmax /= DEF_VAL_R .AND. &
    checkdom%ymin /= DEF_VAL_R .AND. &
    checkdom%ymax /= DEF_VAL_R       )THEN

  checkref%x = 0.5*(checkdom%xmin+checkdom%xmax)
  checkref%y = 0.5*(checkdom%ymin+checkdom%ymax)

  irv = UTM2LL( checkdom%zoneUTM,checkref%x,checkref%y,checkref%lat,checkref%lon )
  IF( irv /= 0 )THEN
    CALL setUTMerror( 'UTM2LL',irv )
    GOTO 9999
  END IF

END IF

CheckReferenceUTM = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a metFlagsT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherFlags( check,metType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( metFlagsT ), INTENT( IN ) :: check
INTEGER,           INTENT( IN ) :: metType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherFlags'

REAL          rMin,rMax
CHARACTER(80) string,errMessage

!==== Initialize

CheckWeatherFlags = SCIPfailure

!==== Check the weather time reference (unless fixed or climo)

string = 'weather time reference'
SELECT CASE( metType )
  CASE( HW_METFIX )

  CASE DEFAULT
    SELECT CASE( check%reference )
      CASE( HT_UTC,HT_LOCAL )

      CASE DEFAULT
        errMessage = TRIM(string)//' must be either HT_UTC or HT_LOCAL'
        CALL SetCheckError( ROUTINE,string,errMessage )
        GOTO 9999

    END SELECT

END SELECT

!==== Check the weather MC flag

string = 'weather mass-consistency flag'
SELECT CASE( check%doMC )
  CASE( SCIPon,SCIPoff )

  CASE DEFAULT
    errMessage = TRIM(string)//' must be either SCIPon or SCIPoff'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Check the weather output flag

string = 'weather output flag'
IF( BTEST(check%doOutput,HOB_OUTPUT) )THEN
  IF( BTEST(check%doOutput,HOB_OUTMET) )THEN
    string = 'weather output interval'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,rMin,rMax,check%tOutput) )GOTO 9999
  END IF
  IF( BTEST(check%doOutput,HOB_2D) .OR. BTEST(check%doOutput,HOB_3D))THEN
  ELSE
    errMessage = TRIM(string)//' must include HO_2D and/or HO_3D'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
END IF

!==== Return

CheckWeatherFlags = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a metMetT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherMet( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( metMetT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherMet'

INTEGER       iMin,iMax,metType
REAL          rMin,rMax
CHARACTER(80) string,errMessage
LOGICAL       metOper

!==== Initialize

CheckWeatherMet = SCIPfailure

!==== Check the met type

metOper = BTEST(check%type,HWB_METOPER)
IF( metOper )THEN
  metType = IBCLR(check%type,HWB_METOPER)
  metType = IBCLR(metType,HWB_METFCST)
  metType = IBCLR(metType,HWB_METCURR)
ELSE
  metType = check%type
END IF

string = 'weather met type'
SELECT CASE( metType )
  CASE( HW_METFIX )
    string = 'fixed wind direction'
    rMin = -360.
    rMax =  360.
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%direction) )GOTO 9999
    string = 'fixed wind speed'
    rMin = 0.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%speed) )GOTO 9999
    string = 'fixed wind direction units'
    SELECT CASE( check%unitDir )
      CASE( HU_DEG )

      CASE DEFAULT
        errMessage = TRIM(string)//' must be HU_DEG'
        CALL SetCheckError( ROUTINE,string,errMessage )
        GOTO 9999

    END SELECT
    string = 'fixed wind speed units'
    SELECT CASE( check%unitSpd )
      CASE( HU_MPS,HU_KTS,HU_MPH,HU_KPH,HU_FPS )

      CASE DEFAULT
        errMessage = TRIM(string)//' must be HU_MPS,HU_KTS,HU_MPH,HU_KPH or HU_FPS'
        CALL SetCheckError( ROUTINE,string,errMessage )
        GOTO 9999

    END SELECT
  CASE( HW_METMRF )

  CASE( HW_METMED )

  CASE( HW_METWRF )

  CASE( HW_METASSIM )

  CASE( HW_METMEDLS )

  CASE( HW_METSRF,HW_METPRF,HW_METSRF+HW_METPRF )
    IF( BTEST( metType,HWB_METSRF) )THEN
      string = 'number surface obs interpolation stations'
      iMin = 1
      iMax = HUGE(iMax)
      IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,iMin,iMax,check%nnsfc) )GOTO 9999
    END IF
    IF( BTEST( metType,HWB_METPRF) )THEN
      string = 'number profile obs interpolation stations'
      iMin = 1
      iMax = HUGE(iMax)
      IF( BadValue(ROUTINE,string,SPECIAL_DEFAULT,iMin,iMax,check%nnprf) )GOTO 9999
    END IF

  CASE DEFAULT
    errMessage = TRIM(string)//' must be a valid met type'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Return

CheckWeatherMet = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a metBLT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherBL( check )

USE SCIMgr_fd
USE checkErr
USE files_fi
USE default_fd

IMPLICIT NONE

TYPE( metBLT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherBL'

REAL          rMin, rMax
CHARACTER(80) string, errMessage

!==== Initialize

CheckWeatherBL = SCIPfailure

!==== Check data based on type

string = 'weather boundary layer type'
SELECT CASE( check%type )
  CASE( HB_NONE,HB_BLOBS,HB_BLPRF,HB_BLMED )

  CASE( HB_BLCALC,HB_BLOPER )
    string = 'Bowen ratio'
    rMin = 0.0
    rMax = HUGE(rMax)
    IF( TRIM(check%landuse) == NOT_SET_C )THEN
      IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Bowen) )GOTO 9999
    ELSE
      IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%Bowen) )GOTO 9999
    END IF

    string = 'surface albedo'
    rMin = 0.0
    rMax = 1.0
    IF( TRIM(check%landuse) == NOT_SET_C )THEN
      IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%albedo) )GOTO 9999
    ELSE
      IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%albedo) )GOTO 9999
    END IF

    string = 'cloud cover'
    rMin = 0.0
    rMax = 1.0
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%cloud) )GOTO 9999

  CASE( HB_BLSIMPLE )
    string = 'minimum inversion height'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%ziNight) )GOTO 9999
    string = 'maximum inversion height'
    rMin = check%ziNight
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%ziDay) )GOTO 9999
    string = 'minimum surface heat flux'
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%hfluxNight) )GOTO 9999
    string = 'maximum surface heat flux'
    rMin = check%hfluxNight
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%hfluxDay) )GOTO 9999

  CASE DEFAULT
    errMessage = TRIM(string)//' must be a valid boundary layer type'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Check surface wetness

string = 'surface wetness index'
SELECT CASE( check%wetness )
  CASE( HM_MSTDRY,HM_MSTNORM,HM_MSTWET )

  CASE DEFAULT
    errMessage = TRIM(string)//' must be HM_MSTDRY,HM_MSTNORM,HM_MSTWET'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Check surface roughness/canopy

IF( SpecialValue(SPECIAL_NOTSET,check%canopy) )THEN
  string = 'surface roughness'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( TRIM(check%landuse) == NOT_SET_C )THEN
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%roughness) )GOTO 9999
  ELSE
    IF( BadValue(ROUTINE,string,SPECIAL_NOTSET,rMin,rMax,check%roughness) )GOTO 9999
  END IF
ELSE
  string = 'canopy height'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%canopy) )GOTO 9999
  string = 'canopy parameter'
  rMin = 0.0
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%canopyParam) )GOTO 9999
END IF

!==== Return

CheckWeatherBL = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a metLSVT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherLSV( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( metLSVT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherLSV'

REAL          rMin,rMax
CHARACTER(80) string,errMessage

!==== Initialize

CheckWeatherLSV = SCIPfailure

!==== Check input based on type

string = 'weather large-scale variability type'
SELECT CASE( check%type )
  CASE( HL_NONE,HL_LSVOPER,HL_LSVMOD )

  CASE( HL_LSVOBS )
    string = 'LSV length scale'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%sl) )GOTO 9999

  CASE( HL_LSVINP )
    string = 'LSV variance'
    rMin = 0.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%uu) )GOTO 9999
    string = 'LSV length scale'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%sl) )GOTO 9999

  CASE DEFAULT
    errMessage = TRIM(string)//' must be a valid boundary layer type'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Return

CheckWeatherLSV = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a PrecipPrecipT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherPrecip( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( metPrecipT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherPrecip'

CHARACTER(80) string,errMessage

!==== Initialize

CheckWeatherPrecip = SCIPfailure

!==== Check input based on type

string = 'weather precipitation type'
SELECT CASE( check%type )
  CASE( HP_PRCPMET )

  CASE( HP_PRCPINP )
    string = 'precipitation class'
    SELECT CASE( check%class )
      CASE (HP_NONE)
      CASE (HP_RAIN + HP_LIGHT)
      CASE (HP_RAIN + HP_MODERATE)
      CASE (HP_RAIN + HP_HEAVY)
      CASE (HP_SNOW + HP_MODERATE)
      CASE (HP_SNOW + HP_HEAVY)
      CASE (HP_SNOW + HP_LIGHT)
      CASE DEFAULT
        errMessage = 'must be HP_NONE or HP_RAIN,HP_SNOW modified by HP_LIGHT,HP_MODERATE or HP_HEAVY'
        CALL SetCheckError( ROUTINE,string,errMessage )
        GOTO 9999
    END SELECT

  CASE DEFAULT
    errMessage = TRIM(string)//' must be HP_NONE,HP_PRCPINP or HP_PRCPMET'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999

END SELECT

!==== Return

CheckWeatherPrecip = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a TerrainTerrainT structure
!*******************************************************************************
INTEGER FUNCTION CheckWeatherTerrain( check )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( terrainT ), INTENT( IN ) :: check

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckWeatherTerrain'

INTEGER        iMin,iMax,testTerrain,iz,ios
REAL           rMin,rMax
CHARACTER(80)  string
CHARACTER(120) errMessage

!==== Initialize

CheckWeatherTerrain = SCIPfailure

!==== Check terrain type


string = 'mass-consistency flag'
testTerrain = IBCLR(check%type,HTB_SCIPUFF)
testTerrain = IBCLR(testTerrain,HTB_AVAIL_T)
testTerrain = IBCLR(testTerrain,HTB_USE_T)
testTerrain = IBCLR(testTerrain,HTB_AVAIL_L)
testTerrain = IBCLR(testTerrain,HTB_USE_L)
testTerrain = IBCLR(testTerrain,HTB_CATEGORY)
IF( testTerrain /= 0 )THEN
  errMessage = TRIM(string)//' must be a valid combination of parameters'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
END IF

!==== Check Z grid

string = 'No. grid points for internal SCIPUFF M/C'
iMin = 3
iMax = 50
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%mc%nz) )GOTO 9999

string = 'Z(  ) for internal SCIPUFF M/C'
DO iz = 1,check%mc%nz
  WRITE(string(3:4),'(I2.2)',iostat=ios)iz
  IF( iz <= 1 )THEN
    rMin = TINY(rMin)
  ELSE
    rMin = check%mc%Z(iz-1)+2.*SPACING(check%mc%Z(iz-1))
  END IF
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%mc%Z(iz)) )GOTO 9999
END DO

!==== Check max iterations

string = 'Max. iterations for internal SCIPUFF M/C (FFT)'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%mc%maxiter(1)) )GOTO 9999

string = 'Max. iterations for internal SCIPUFF M/C (PRM)'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%mc%maxiter(2)) )GOTO 9999

!==== Check iteration tolerance

string = 'iteration tolerance for internal SCIPUFF M/C (FFT)'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%mc%eps(1)) )GOTO 9999

string = 'iteration tolerance for internal SCIPUFF M/C (PRM)'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%mc%eps(2)) )GOTO 9999

!==== Check vertical wind adjustment

string = 'min vertical wind adjustment for internal SCIPUFF M/C'
rMin = 0.0
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%mc%alpha(1)) )GOTO 9999

string = 'max vertical wind adjustment for internal SCIPUFF M/C'
rMin = check%mc%alpha(1)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%mc%alpha(2)) )GOTO 9999

!==== Return

CheckWeatherTerrain = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a matGasT structure
!*******************************************************************************
INTEGER FUNCTION CheckMaterialGas( checkIn )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: checkIn

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckMaterialGas'

TYPE( matGasT ) check
REAL            rMin,rMax
CHARACTER(80)   string

!==== Initialize

CheckMaterialGas = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the Minimum concentration

string = 'minimum concentration'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%minConcentration) )GOTO 9999

!==== Check the decay parameters

string = 'minimum activity decay rate'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayMin) )GOTO 9999

string = 'maximum activity decay rate'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayAmp) )GOTO 9999

!==== Check the gas density

string = 'gas density'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasDensity) )GOTO 9999

!==== Check the gas deposition velocity

string = 'gas deposition velocity'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasDeposition) )GOTO 9999

!==== Return

CheckMaterialGas = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a matLiquidT structure
!*******************************************************************************
INTEGER FUNCTION CheckMaterialLiquid( checkIn,secondEvap,substrate )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: checkIn
LOGICAL,         INTENT( IN ) :: secondEvap
LOGICAL,         INTENT( IN ) :: substrate

CHARACTER(40),PARAMETER    :: ROUTINE = 'CheckMaterialLiquid'

TYPE( matLiquidT ) check
INTEGER            iMin,iMax,iBin,ios
REAL               rMin,rMax
CHARACTER(80)      string,errMessage

!==== Initialize

CheckMaterialLiquid = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the Minimum concentration

string = 'minimum concentration'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%minConcentration) )GOTO 9999

!==== Check the decay parameters

string = 'minimum activity decay rate'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayMin) )GOTO 9999

string = 'maximum activity decay rate'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayAmp) )GOTO 9999

!==== Check the gas density

string = 'gas density'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasDensity) )GOTO 9999

!==== Check the gas deposition velocity

string = 'gas deposition velocity'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasDeposition) )GOTO 9999

!==== Check the liquid density

string = 'liquid density'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%liquidDensity(1)) )GOTO 9999

string = 'liquid density temperature dependence'
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%liquidDensity(2)) )GOTO 9999

!==== Check the Antoine coefficients

rMin =  TINY(rMin)
rMax =  HUGE(rMax)
string = 'Antoine coefficient(1)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(1)) )GOTO 9999
rMin =  0.
string = 'Antoine coefficient(2)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(2)) )GOTO 9999
string = 'Antoine coefficient(3)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(3)) )GOTO 9999

!==== Check the Molecular Weight

string = 'molecular weight'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%molWeight) )GOTO 9999

!==== Check the surface tension

string = 'surface tension'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%srfTension) )GOTO 9999

!==== Check the liquid specific heat

string = 'liquid specific heat'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%liqSpecificHeat) )GOTO 9999

!==== Check the gas specific heat

string = 'gas specific heat'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasSpecificHeat) )GOTO 9999

!==== Check the secondary evaporation parameter

IF( secondEvap )THEN
  string = 'droplet spread factor'
  rMin = 1.
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%spreadFactor) )GOTO 9999
END IF

!==== Check the liquid viscosity

IF( substrate )THEN
  string = 'liquid viscosity'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%viscosity) )GOTO 9999
END IF

!==== Check the material bins

string = 'no. droplet bins'
iMin = 1
iMax = HS_MAXMTLBINSIZE
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%nSizeBins) )GOTO 9999

string = 'droplet bin boundary 01'
IF( check%binBounds(1) /= 0.0 )THEN
  errMessage = TRIM(string)//' must be 0.0'
  CALL SetCheckError(ROUTINE,string,errMessage)
  GOTO 9999
END IF

rMax = HUGE(rMax)
DO iBin = 2,check%nSizeBins+1
  WRITE(string(22:23),'(I2.2)',iostat=ios)iBin
  rMin = check%binBounds(iBin-1) + 2.*SPACING(check%binBounds(iBin-1))
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%binBounds(iBin)) )GOTO 9999
END DO

string = 'droplet bin size '
DO iBin = 1,check%nSizeBins
  WRITE(string(18:19),'(I2.2)',iostat=ios)iBin
  rMin = check%binBounds(iBin)
  rMax = check%binBounds(iBin+1)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%binSize(iBin)) )GOTO 9999
END DO

!==== Return

CheckMaterialLiquid = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a matAerosolT structure
!*******************************************************************************
INTEGER FUNCTION CheckMaterialAerosol( checkIn )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: checkIn

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckMaterialAerosol'

TYPE( matAerosolT ) check
REAL                rMin,rMax
CHARACTER(80)       string

!==== Initialize

CheckMaterialAerosol = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the Minimum concentration

string = 'minimum concentration'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%minConcentration) )GOTO 9999

!==== Check the decay parameters

string = 'minimum activity decay rate'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayMin) )GOTO 9999

string = 'maximum activity decay rate'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayAmp) )GOTO 9999

!==== Check the gas deposition velocity

string = 'gas deposition velocity'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasDeposition) )GOTO 9999

!==== Check the liquid density

string = 'liquid density'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%liquidDensity) )GOTO 9999

!==== Check the Antoine coefficients

rMin =  TINY(rMin)
rMax =  HUGE(rMax)
string = 'Antoine coefficient(1)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(1)) )GOTO 9999
rMin =  -HUGE(rMax)
string = 'Antoine coefficient(2)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(2)) )GOTO 9999
string = 'Antoine coefficient(3)'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%Antoine(3)) )GOTO 9999

!==== Check the Molecular Weight

string = 'molecular weight'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%molWeight) )GOTO 9999

!==== Check the liquid specific heat

string = 'liquid specific heat'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%liqSpecificHeat) )GOTO 9999

!==== Check the gas specific heat

string = 'gas specific heat'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%gasSpecificHeat) )GOTO 9999

!==== Return

CheckMaterialAerosol = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a matParticleT structure
!*******************************************************************************
INTEGER FUNCTION CheckMaterialParticle( checkIn )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: checkIn

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckMaterialParticle'

TYPE( matParticleT ) check
INTEGER              iMin,iMax,iBin,ios
REAL                 rMin,rMax
CHARACTER(80)        string

!==== Initialize

CheckMaterialParticle = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the Minimum concentration

string = 'minimum concentration'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%minConcentration) )GOTO 9999

!==== Check the decay parameters

string = 'minimum activity decay rate'
rMin = 0.
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayMin) )GOTO 9999

string = 'maximum activity decay rate'
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%decayAmp) )GOTO 9999

!==== Check the density

string = 'particle density'
rMin = TINY(rMin)
rMax = HUGE(rMax)
IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%density) )GOTO 9999

!==== Check the material bins

string = 'no. particle bins'
iMin = 1
iMax = HS_MAXMTLBINSIZE
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,check%nSizeBins) )GOTO 9999

string = 'particle bin boundary '
rMax = HUGE(rMax)
DO iBin = 1,check%nSizeBins+1
  WRITE(string(23:24),'(I2.2)',IOSTAT=ios)iBin
  IF( iBin == 1 )THEN
    rMin = TINY(rMin)
  ELSE
    rMin = check%binBounds(iBin-1) + 2.*SPACING(check%binBounds(iBin-1))
  END IF
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%binBounds(iBin)) )GOTO 9999
END DO

string = 'particle bin size '
DO iBin = 1,check%nSizeBins
  WRITE(string(19:20),'(I2.2)',iostat=ios)iBin
  rMin = check%binBounds(iBin)
  rMax = check%binBounds(iBin+1)
  IF( BadValue(ROUTINE,string,SPECIAL_NONE,rMin,rMax,check%binSize(iBin)) )GOTO 9999
END DO

!==== Return

CheckMaterialParticle = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Get valid Material Distribution
!*******************************************************************************
INTEGER FUNCTION GetMaterialDistribution( matl,distMin,distMax,distMMD,mtlType )

USE SCIMgr_fd
USE checkErr
USE error_fi

IMPLICIT NONE

TYPE( materialT ), INTENT( IN  ) :: matl
INTEGER,           INTENT( OUT ) :: distMin
INTEGER,           INTENT( OUT ) :: distMax
LOGICAL,           INTENT( OUT ) :: distMMD
INTEGER,           INTENT( OUT ) :: mtlType

INTEGER, EXTERNAL :: SubgroupsParticle
INTEGER, EXTERNAL :: SubgroupsLiquid

!==== Initialize

GetMaterialDistribution = SCIPfailure

distMin = 1
distMax = 1
distMMD = .FALSE.

!==== Check the material type/properties

mtlType = IBITS(matl%type,0,HMB_BASIC)         !Basic types only

SELECT CASE( mtlType )

  CASE( HM_GAS )

  CASE( HM_PARTICLE )
    distMax = SubgroupsParticle( matl%matData )
    IF( distMax <= 0 )GOTO 9999
    distMMD = distMax >= 1

  CASE( HM_LIQUID )
    distMin = 0
    distMax = SubgroupsLiquid( matl%matData )
    IF( distMax <= 0 )GOTO 9999
    distMMD = distMax >= 1

  CASE( HM_WETPARTICLE )
    distMax = SubgroupsParticle( matl%matData )
    IF( distMax <= 0 )GOTO 9999
    distMMD = distMax >= 1

  CASE( HM_AEROSOL )
    distMin = 0

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'GetMaterialDistribution'
    eMessage = 'Invalid material type'
    eInform  = 'valid material types are HM_GAS,HM_PARTICLE'
    eInform  = TRIM(eInform)//',HM_LIQUID,HM_WETPARTICLE'
    eInform  = TRIM(eInform)//',HM_AEROSOL'
    GOTO 9999

END SELECT

!==== Return

GetMaterialDistribution = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            particle subgroups
!*******************************************************************************
INTEGER FUNCTION SubgroupsParticle( matlIn )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: matlIn

CHARACTER(40), PARAMETER :: ROUTINE = 'SubgroupsParticle'

TYPE( matParticleT ) matl
INTEGER              iMin,iMax
CHARACTER(80)        string

!==== Initialize

SubgroupsParticle = 0

matl = TRANSFER(matlIn,matl)

!==== check data validity

string = 'no. particle bins'
iMin = 1
iMax = HS_MAXMTLBINSIZE
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,matl%nSizeBins) )GOTO 9999

!==== Return

SubgroupsParticle = matl%nSizeBins

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            liquid subgroups
!*******************************************************************************
INTEGER FUNCTION SubgroupsLiquid( matlIn )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( matGenT ), INTENT( IN ) :: matlIn

CHARACTER(40), PARAMETER :: ROUTINE = 'SubgroupsLiquid'

TYPE( matLiquidT ) matl
INTEGER            iMin,iMax
CHARACTER(80)      string

!==== Initialize

SubgroupsLiquid = 0

matl = TRANSFER(matlIn,matl)

!==== check data validity

string = 'no. particle bins'
iMin = 1
iMax = HS_MAXMTLBINSIZE
IF( BadValue(ROUTINE,string,SPECIAL_NONE,iMin,iMax,matl%nSizeBins) )GOTO 9999

!==== Return

SubgroupsLiquid = matl%nSizeBins

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relContT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseCont( checkIn,special,distMin,distmax,distMMD, &
                                   mtlType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseCont'

TYPE( relContT ) check
INTEGER          iMin,iMax,specialX
REAL             rMin,rMax
CHARACTER(80)    string,errMessage

!==== Initialize

CheckReleaseCont = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check Dry/Liquid Fraction if Wet-Particle/Liquid release

IF( mtlType == HM_LIQUID .OR. mtlType == HM_WETPARTICLE )THEN
  specialX = IBSET(special,DEFAULT_BIT)
  specialX = IBSET(specialX,NOTSET_BIT)
  IF( mtlType == HM_LIQUID )THEN
    string = 'liquid fraction'
  ELSE
    string = 'slurry dry fraction'
  END IF
  rMin = 0.0
  rMax = 1.0
  IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%dryFrac) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release rate,duration

rMax = HUGE(rMax)
rMin = 0.0
string = 'rate'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%rate) )GOTO 9999
rMin = TINY(rMin)
string = 'duration'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%duration) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigY) )GOTO 9999
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigZ) )GOTO 9999

!==== Return

CheckReleaseCont = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relMoveT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseMove( checkIn,special,distMin,distmax,distMMD, &
                                   mtlType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseMove'

TYPE( relMoveT ) check
INTEGER          iMin, iMax, specialX
REAL             rMin, rMax
CHARACTER(80)    string, errMessage

!==== Initialize

CheckReleaseMove = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check Dry/Liquid Fraction if Wet-Particle/Liquid release

IF( mtlType == HM_LIQUID .OR. mtlType == HM_WETPARTICLE )THEN
  specialX = IBSET(special,DEFAULT_BIT)
  specialX = IBSET(specialX,NOTSET_BIT)
  IF( mtlType == HM_LIQUID )THEN
    string = 'liquid fraction'
  ELSE
    string = 'slurry dry fraction'
  END IF
  rMin = 0.0
  rMax = 1.0
  IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%dryFrac) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release rate,duration

rMax = HUGE(rMax)
rMin = 0.0
string = 'rate'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%rate) )GOTO 9999
rMin = TINY(rMin)
string = 'duration'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%duration) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigY) )GOTO 9999
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigZ) )GOTO 9999

!==== Check the release velocity

rMin = -HUGE(rMax)
rMax =  HUGE(rMax)
string = 'U-velocity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%velX) )GOTO 9999
string = 'V-velocity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%velY) )GOTO 9999
string = 'Z-velocity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%velZ) )GOTO 9999

!==== Return

CheckReleaseMove = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relInstT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseInst( checkIn,special,distMin,distmax,distMMD, &
                                   mtlType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseInst'

TYPE( relInstT ) check
INTEGER          iMin,iMax,specialX
REAL             rMin,rMax
CHARACTER(80)    string,errMessage

!==== Initialize

CheckReleaseInst = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check Dry/Liquid Fraction if Wet-Particle/Liquid release

IF( mtlType == HM_LIQUID .OR. mtlType == HM_WETPARTICLE )THEN
  specialX = IBSET(special,DEFAULT_BIT)
  specialX = IBSET(specialX,NOTSET_BIT)
  IF( mtlType == HM_LIQUID )THEN
    string = 'liquid fraction'
  ELSE
    string = 'slurry dry fraction'
  END IF
  rMin = 0.0
  rMax = 1.0
  IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%dryFrac) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-X'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigX) )GOTO 9999
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigY) )GOTO 9999
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigZ) )GOTO 9999

!==== Check the release random instances parameters

specialX = IBSET(special,NOTSET_BIT)
string = 'no. random instances'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,specialX,iMin,iMax,check%nRandom) )GOTO 9999

IF( check%nRandom > 0 )THEN
  string = 'random seed'
  iMin = 100000
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%ranSeed) )GOTO 9999
  string = 'random spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpread) )GOTO 9999
END IF

!==== Return

CheckReleaseInst = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relXInstT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseXInst( checkIn,special,distMin,distmax,distMMD, &
                                    mtlType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseInst'

TYPE( relXInstT ) check
INTEGER           iMin, iMax, specialX
REAL              rMin, rMax
CHARACTER(80)     string, errMessage

!==== Initialize

CheckReleaseXInst = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check Dry/Liquid Fraction if Wet-Particle/Liquid release

IF( mtlType == HM_LIQUID .OR. mtlType == HM_WETPARTICLE )THEN
  specialX = IBSET(special,DEFAULT_BIT)
  specialX = IBSET(specialX,NOTSET_BIT)
  IF( mtlType == HM_LIQUID )THEN
    string = 'liquid fraction'
  ELSE
    string = 'slurry dry fraction'
  END IF
  rMin = 0.0
  rMax = 1.0
  IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%dryFrac) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-X'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigX) )GOTO 9999
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigY) )GOTO 9999
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigZ) )GOTO 9999

!==== Check the release random instances parameters

specialX = IBSET(special,NOTSET_BIT)
string = 'no. random instances'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,specialX,iMin,iMax,check%nRandom) )GOTO 9999

IF( check%nRandom > 0 )THEN
  string = 'random seed'
  iMin = 100000
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%ranSeed) )GOTO 9999
  string = 'random spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpread) )GOTO 9999
END IF

!==== Check the off-diagonal correlation coefficients

string = 'correlation coefficients'
rMin = -0.99999
rMax =  0.99999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRxy) )GOTO 9999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRxz) )GOTO 9999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRyz) )GOTO 9999

!==== Return

CheckReleaseXInst = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relXInstT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseXInst3( checkIn,special,distMin,distmax,distMMD, &
                                    mtlType )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseInst'

TYPE( relXInst3T ) check
INTEGER            iMin, iMax, specialX
REAL               rMin, rMax
CHARACTER(80)      string, errMessage

!==== Initialize

CheckReleaseXInst3 = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check Dry/Liquid Fraction if Wet-Particle/Liquid release

IF( mtlType == HM_LIQUID .OR. mtlType == HM_WETPARTICLE )THEN
  specialX = IBSET(special,DEFAULT_BIT)
  specialX = IBSET(specialX,NOTSET_BIT)
  IF( mtlType == HM_LIQUID )THEN
    string = 'liquid fraction'
  ELSE
    string = 'slurry dry fraction'
  END IF
  rMin = 0.0
  rMax = 1.0
  IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%dryFrac) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-X'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigX) )GOTO 9999
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigY) )GOTO 9999
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigZ) )GOTO 9999

!==== Check the release random instances parameters

specialX = IBSET(special,NOTSET_BIT)
string = 'no. random instances'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,specialX,iMin,iMax,check%nRandom) )GOTO 9999

IF( check%nRandom > 0 )THEN
  string = 'random seed'
  iMin = 100000
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%ranSeed) )GOTO 9999
  string = 'random parallel spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpreadA) )GOTO 9999
  string = 'random transverse spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpreadT) )GOTO 9999
  string = 'random vertical spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpreadV) )GOTO 9999
  string = 'random spread direction'
  rMin = 0.0
  rMax = 360.0
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranDir) )GOTO 9999
END IF

!==== Check the off-diagonal correlation coefficients

string = 'correlation coefficients'
rMin = -0.99999
rMax =  0.99999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRxy) )GOTO 9999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRxz) )GOTO 9999
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRyz) )GOTO 9999

!==== Return

CheckReleaseXInst3 = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relStackT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseStack( checkIn,special,distMin,distmax,distMMD,mtlType )

USE SCIMgr_fd
USE checkErr
USE default_fd

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseStack'

TYPE( relStackT ) check
INTEGER           iMin, iMax, specialX
REAL              rMin, rMax
LOGICAL           checkDynamics
CHARACTER(80)     string, errMessage

!==== Initialize

CheckReleaseStack = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Skip checks if mass rate is zero

IF( check%rate <= 0. )THEN
  CheckReleaseStack = SCIPsuccess
  GOTO 9999
END IF

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

checkDynamics = (mtlType == HM_GAS)
checkDynamics = checkDynamics .OR. (mtlType == HM_AEROSOL)
IF( mtlType == HM_LIQUID )THEN
  IF( check%distribution == 0 )THEN
    checkDynamics = .TRUE.
  ELSE
    checkDynamics = checkDynamics .OR. (check%dryFrac < 1.0 .AND. check%dryFrac >= 0.0)  !Two-phase release
  END IF
END IF
IF( checkDynamics )THEN

  string = 'exit velocity'
  IF( check%exitVel == DEF_VAL_R )THEN
    errMessage = 'Stack exit velocity must not be Default'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,0,rMin,rMax,check%exitVel) )GOTO 9999

  IF( check%exitTemp /= DEF_VAL_R )THEN
    string = 'exit temperature'
    specialX = IBSET(special,NOTSET_BIT)
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitTemp) )GOTO 9999
  END IF

END IF

!==== Check the release rate, duration

rMax = HUGE(rMax)
rMin = 0.0
string = 'rate'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%rate) )GOTO 9999
rMin = TINY(rMin)
string = 'duration'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%duration) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'diameter'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%diameter) )GOTO 9999

!==== Return

CheckReleaseStack = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relStack3T structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseStack3( checkIn,special,distMin,distmax,distMMD,mtlType )

USE SCIMgr_fd
USE checkErr
USE default_fd

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseStack'

TYPE( relStack3T ) check
INTEGER           iMin, iMax, specialX
INTEGER           i
REAL              vel
REAL              rMin, rMax
LOGICAL           checkDynamics
LOGICAL           checkVel
CHARACTER(80)     string, errMessage

!==== Initialize

CheckReleaseStack3 = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Skip checks if mass rate is zero

IF( check%rate <= 0. )THEN
  CheckReleaseStack3 = SCIPsuccess
  GOTO 9999
END IF

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

checkDynamics = (mtlType == HM_GAS)
checkDynamics = checkDynamics .OR. (mtlType == HM_AEROSOL)
IF( mtlType == HM_LIQUID )THEN
  IF( check%distribution == 0 )THEN
    checkDynamics = .TRUE.
  ELSE
    checkDynamics = checkDynamics .OR. (check%dryFrac < 1.0 .AND. check%dryFrac >= 0.0)  !Two-phase release
  END IF
END IF
IF( checkDynamics )THEN

  string = 'exit velocity'
  IF( check%exitVel(1) == DEF_VAL_R )THEN
    IF( check%exitVel(2) /= DEF_VAL_R )THEN
      errMessage = 'If one horizontal exit velocity is Default then both horizontal exit velocities must be Default'
      CALL SetCheckError( ROUTINE,string,errMessage )
      GOTO 9999
    END IF
!----- Default U,V - should be same as non 3D tack
    specialX = IBSET(special,NOTSET_BIT)
    rMin =  TINY(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitVel(3)) )GOTO 9999
  ELSE
    IF( check%exitVel(2) == DEF_VAL_R )THEN
      errMessage = 'If one horizontal exit velocity is Default then both horizontal exit velocities must be Default'
      CALL SetCheckError( ROUTINE,string,errMessage )
      GOTO 9999
    END IF
    IF( check%exitVel(3) == DEF_VAL_R )THEN
      errMessage = 'Vertical exit velocity must not be Default'
      CALL SetCheckError( ROUTINE,string,errMessage )
      GOTO 9999
    END IF
!--- 3D jet
    specialX = IBSET(special,NOTSET_BIT)
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitVel(1)) )GOTO 9999
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitVel(2)) )GOTO 9999
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitVel(3)) )GOTO 9999
    checkVel = .TRUE.
    DO i = 1,3
      IF( check%exitVel(i) == DEFERRED_R )checkVel = .FALSE.
    END DO
    IF( checkVel )THEN
      vel = 0.0
      DO i = 1,3
        IF( check%exitVel(i) /= NOT_SET_R )vel = vel + check%exitVel(i)*check%exitVel(i)
      END DO
    END IF
    vel  = SQRT(vel)
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,0,rMin,rMax,vel) )GOTO 9999
  END IF

  IF( check%exitTemp /= DEF_VAL_R )THEN
    string = 'exit temperature'
    specialX = IBSET(special,NOTSET_BIT)
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitTemp) )GOTO 9999
  END IF

END IF

!==== Check the release rate, duration

rMax = HUGE(rMax)
rMin = 0.0
string = 'rate'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%rate) )GOTO 9999
rMin = TINY(rMin)
string = 'duration'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%duration) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'diameter'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%diameter) )GOTO 9999

!==== Return

CheckReleaseStack3 = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relAreaT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseArea( checkIn,special,distMin,distmax,distMMD,mtlType )

USE SCIMgr_fd
USE checkErr
USE default_fd

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax
LOGICAL        , INTENT( IN ) :: distMMD
INTEGER        , INTENT( IN ) :: mtlType

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseArea'

TYPE( relAreaT ) check
INTEGER          iMin, iMax, specialX
REAL             rMin, rMax
LOGICAL          checkDynamics
CHARACTER(80)    string, errMessage

!==== Initialize

CheckReleaseArea = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Skip checks if mass rate is zero

IF( check%rate <= 0. )THEN
  CheckReleaseArea = SCIPsuccess
  GOTO 9999
END IF

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  IF( distMMD )THEN
    string = 'MMD for log-normal distribution'
    rMin = TINY(rMin)
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%MMD) )GOTO 9999
    string = 'sigma for log-normal distribution'
    rMin = 1.0
    rMax = HUGE(rMax)
    IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigma) )GOTO 9999
  ELSE
    errMessage = 'log-normal distribution not valid for the specified release material'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
ELSE
  iMin = distMin
  iMax = distMax
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

checkDynamics = (mtlType == HM_GAS)
checkDynamics = checkDynamics .OR. (mtlType == HM_AEROSOL)
IF( mtlType == HM_LIQUID )THEN
  IF( check%distribution == 0 )THEN
    checkDynamics = .TRUE.
  ELSE
    checkDynamics = checkDynamics .OR. (check%dryFrac < 1.0 .AND. check%dryFrac >= 0.0)  !Two-phase release
  END IF
END IF
IF( checkDynamics )THEN

  string = 'exit velocity'
  IF( check%exitVel == DEF_VAL_R )THEN
    errMessage = 'Area exit velocity must not be Default'
    CALL SetCheckError( ROUTINE,string,errMessage )
    GOTO 9999
  END IF
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,0,rMin,rMax,check%exitVel) )GOTO 9999

  IF( check%exitTemp /= DEF_VAL_R )THEN
    string = 'exit temperature'
    specialX = IBSET(special,NOTSET_BIT)
    rMin = -HUGE(rMin)
    rMax =  HUGE(rMax)
    IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%exitTemp) )GOTO 9999
  END IF

END IF

!==== Check the release rate, duration

rMax = HUGE(rMax)
rMin = 0.0
string = 'rate'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%rate) )GOTO 9999
rMin = TINY(rMin)
string = 'duration'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%duration) )GOTO 9999

!==== Check the area dimensions and rotation angle

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'xlen'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%xlen) )GOTO 9999

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'ylen'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ylen) )GOTO 9999

rMin = 0.
rMax = 360.
string = 'ang'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ang) )GOTO 9999

!==== Return

CheckReleaseArea = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relFileT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseFile( checkIn,special )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseFile'

TYPE( relFileT ) check
INTEGER          iMin,iMax,specialX
REAL             rMin,rMax
CHARACTER(80)    string

!==== Initialize

CheckReleaseFile = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release random instances parameters

specialX = IBSET(special,NOTSET_BIT)
string = 'no. random instances'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,specialX,iMin,iMax,check%nRandom) )GOTO 9999

IF( check%nRandom > 0 )THEN
  string = 'random seed'
  iMin = 100000
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%ranSeed) )GOTO 9999
  string = 'random spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpread) )GOTO 9999
END IF

!==== Return

CheckReleaseFile = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relPoolT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleasePool( checkIn,special )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleasePool'

TYPE( relPoolT ) check
REAL             rMin,rMax
CHARACTER(80)    string

!==== Initialize

CheckReleasePool = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'size-X'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sizeX) )GOTO 9999
string = 'size-Y'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sizeY) )GOTO 9999

!==== Return

CheckReleasePool = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a relPuffT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleasePuff( checkIn,special,distMin,distmax )

USE SCIMgr_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: distMin
INTEGER        , INTENT( IN ) :: distMax

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleasePuff'

TYPE( relPuffT ) check
INTEGER          iMin,iMax
REAL             rMin,rMax
CHARACTER(80)    string,errMessage

LOGICAL, EXTERNAL :: PuffRealizable

!==== Initialize

CheckReleasePuff = SCIPfailure

check = TRANSFER(checkIn,check)

!==== Check the release subgroup

string = 'subgroup'
iMin = distMin
iMax = distMax
IF( BadValue(ROUTINE,string,special,iMin,iMax,check%subgroup) )GOTO 9999

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release variance

rMin = 0.0
rMax = HUGE(rMax)
string = 'sigma-C/Cbar'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sigRatio) )GOTO 9999

!==== Check the release active fraction

rMin = 0.0
rMax = 1.0
string = 'active fraction'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%activeFrac) )GOTO 9999

!==== Check the release sigma

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'Puff moment - Sxx'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sxx) )GOTO 9999
string = 'Puff moment - Syy'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%syy) )GOTO 9999
string = 'Puff moment - Szz'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%szz) )GOTO 9999
rMin = -HUGE(rMin)
string = 'Puff moment - Sxy'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sxy) )GOTO 9999
string = 'Puff moment - Sxz'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%sxz) )GOTO 9999
string = 'Puff moment - Syz'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%syz) )GOTO 9999

IF( .NOT.PuffRealizable(check%sxx,check%sxy,check%sxz,check%syy,check%syz,check%szz) )THEN
  string = 'Puff moments'
  errMessage = 'moments not realizable'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
END IF

!==== Check the release diffusivities

rMin = 0.0
rMax = HUGE(rMax)
string = 'Puff small scale diffusivity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difhShear) )GOTO 9999
string = 'Puff BL scale diffusivity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difhBuoy) )GOTO 9999
string = 'Puff large scale diffusivity (XX)'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difhLSVxx) )GOTO 9999
string = 'Puff large scale diffusivity (XY)'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difhLSVxy) )GOTO 9999
string = 'Puff large scale diffusivity (YY)'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difhLSVyy) )GOTO 9999
string = 'Puff vertical diffusivity'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%difVert) )GOTO 9999

!==== Check the release scales

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'lateral scale'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%scaleLateral) )GOTO 9999
string = 'streamwise scale'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%scaleStream) )GOTO 9999
string = 'vertical scale'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%scaleVert) )GOTO 9999

!==== Return

CheckReleasePuff = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check a Flashing relInstT structure
!*******************************************************************************
INTEGER FUNCTION CheckReleaseFlash( checkIn,special,itype )

USE SCIMgr_fd
USE default_fd
USE checkErr

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: checkIn
INTEGER        , INTENT( IN ) :: special
INTEGER        , INTENT( IN ) :: itype

CHARACTER(40), PARAMETER :: ROUTINE = 'CheckReleaseFlash'

TYPE( relInstT ) check
INTEGER          iMin,iMax,specialX
REAL             rMin,rMax
LOGICAL          default
CHARACTER(80)    string,errMessage

LOGICAL, EXTERNAL :: IsAerosol

!==== Initialize

CheckReleaseFlash = SCIPfailure

!==== Check the release material type

string = 'material'
IF( .NOT.BTEST(itype,HMB_AEROSOL) )THEN
  errMessage = 'Flashing release must specify an aerosol material'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
END IF

check = TRANSFER(checkIn,check)

!==== Check the release distribution

string = 'distribution'
IF( check%distribution == HD_LOGNORM )THEN
  errMessage = 'log-normal distribution not valid for the specified release material'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
ELSE
  iMin = 1
  iMax = 1
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%distribution) )GOTO 9999
END IF

!==== Check the release momentum, buoyancy

specialX = IBSET(special,NOTSET_BIT)
rMin = -HUGE(rMin)
rMax =  HUGE(rMax)
string = 'momentum'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%momentum) )GOTO 9999
string = 'buoyancy'
specialX = IBSET(specialX,DEFAULT_BIT)
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%buoyancy) )GOTO 9999

!==== Check the release mass

rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'mass'
IF( BadValue(ROUTINE,string,special,rMin,rMax,check%mass) )GOTO 9999

!==== Check the release sigma

specialX = IBSET(special,DEFAULT_BIT)
specialX = IBSET(specialX,NOTSET_BIT)
rMin = TINY(rMin)
rMax = HUGE(rMax)
string = 'sigma-X'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%sigX) )GOTO 9999
default = ( check%sigX == DEF_VAL_R .OR. check%sigX == NOT_SET_R )
IF (.NOT.default )specialX = special
string = 'sigma-Y'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%sigY) )GOTO 9999
IF( default .AND. check%sigY /= DEF_VAL_R .AND. check%sigY /= NOT_SET_R )THEN
  errMessage = 'Cannot mix DEFAULT and user-specified sigmas in flashing release'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
END IF
string = 'sigma-Z'
IF( BadValue(ROUTINE,string,specialX,rMin,rMax,check%sigZ) )GOTO 9999
IF( default .AND. check%sigZ /= DEF_VAL_R .AND. check%sigZ /= NOT_SET_R )THEN
  errMessage = 'Cannot mix DEFAULT and user-specified sigmas in flashing release'
  CALL SetCheckError( ROUTINE,string,errMessage )
  GOTO 9999
END IF

!==== Check the release random instances parameters

specialX = IBSET(special,NOTSET_BIT)
string = 'no. random instances'
iMin = 0
iMax = HUGE(iMax)
IF( BadValue(ROUTINE,string,specialX,iMin,iMax,check%nRandom) )GOTO 9999

IF( check%nRandom > 0 )THEN
  string = 'random seed'
  iMin = 100000
  iMax = HUGE(iMax)
  IF( BadValue(ROUTINE,string,special,iMin,iMax,check%ranSeed) )GOTO 9999
  string = 'random spread'
  rMin = TINY(rMin)
  rMax = HUGE(rMax)
  IF( BadValue(ROUTINE,string,special,rMin,rMax,check%ranSpread) )GOTO 9999
END IF

!==== Return

CheckReleaseFlash = SCIPsuccess

9999 CONTINUE

RETURN
END
