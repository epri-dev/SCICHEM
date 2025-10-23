!===============================================================================
!     ReadWeather
!===============================================================================
SUBROUTINE ReadWeather( file,lunit,list,maxList,nList )

USE scipuff_fi
USE met_fi

!     Reads the SCIPUFF weather file (*.MSC)
!     maxList = 0 -> read namelist and additional input for OBS,FIXED and all GRIDDED
!     maxList > 0 -> read namelist and additional input for all types
!     maxList < 0 -> read only the additional input for CLIMO and CLI3D

IMPLICIT NONE

CHARACTER(*),               INTENT( IN    ) :: file
INTEGER     ,               INTENT( IN    ) :: lunit
CHARACTER(*), DIMENSION(*), INTENT( INOUT ) :: list
INTEGER     ,               INTENT( IN    ) :: maxList
INTEGER     ,               INTENT( INOUT ) :: nList

INTEGER ios

LOGICAL readFix
LOGICAL readObs
LOGICAL read3D
LOGICAL readGrd

LOGICAL lReadNamelist
LOGICAL lReadInput

CHARACTER(80) UseMetType, UseBLType, UseLSVType, UsePrecipType

!==== Initialize

lReadNamelist = maxList >= 0
lReadInput    = maxList /= 0

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION='READ',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadWeather'
  eMessage = 'Error opening SCIPUFF met scenario input file'
  GOTO 9998
END IF

!==== Read the namelist

IF( lReadNamelist )THEN
  CALL ReadNamelistMet( lunit )
  IF( nError /= NO_ERROR )GOTO 9998
  CALL CheckNamelistMet( UseMetType,UseBLType,UseLSVType,UsePrecipType )
  IF( nError /= NO_ERROR )GOTO 9999
  lsv_oper = ( ensm_type == 'OPER3.1' )
ELSE
  CALL SkipNamelistMet( lunit )
  UseMetType    = met_type
  UseBLType     = bl_type
  UseLSVType    = ensm_type
  UsePrecipType = pr_type
END IF

!==== Read the additional input lines

readFix = .FALSE.
readObs = .FALSE.
read3D  = .FALSE.
readGrd = .FALSE.
SELECT CASE( TRIM(UseMetType) )
  CASE( 'FIXED' )
    readFix = .TRUE.

  CASE( 'OBS' )
    readObs = .TRUE.

  CASE( 'CLI3D','CLI3DMC' )

  CASE( 'GRIDDED' )
    readGrd = .TRUE.

  CASE( 'MEDOC' )
    readGrd = .TRUE.

  CASE( 'WRF' )
    readGrd = .TRUE.

  CASE( 'ASSIM','SCIPUFF_LIST' )
    readGrd = .TRUE.

  CASE( 'MEDOC_LIST','MEDOCLIST' )
    readGrd = .TRUE.

  CASE( 'MRF','GRIDMC' )
    readGrd = .TRUE.

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadWeather'
    eMessage = 'Invalid meteorology (MET_TYPE)'
    eInform  = 'MET_TYPE='//TRIM(met_type)

END SELECT
IF( nError /= NO_ERROR )GOTO 9999

!==== Fixed winds

IF( readFix )THEN

  IF( lReadNamelist )THEN
    CALL ReadFixedInput( lunit )
    IF( nError /= NO_ERROR )GOTO 9998
  END IF

!==== Observations

ELSE IF( readObs )THEN

  IF( lReadNamelist )THEN
    CALL ReadObsInput( lunit,lReadNamelist )
    IF( nError /= NO_ERROR )GOTO 9998
  END IF

!==== Gridded

ELSE IF( readGrd )THEN

  IF( lReadNamelist )THEN
    CALL ReadGriddedInput( lunit,UseMetType )
    IF( nError /= NO_ERROR )GOTO 9998
  END IF

END IF

!==== Check Combinations

CALL CheckMetCombinations( UseMetType,UseBLType,UseLSVType,UsePrecipType )
IF( nError /= NO_ERROR )GOTO 9999

!==== Additional SCIPUFF run actions

IF( .NOT.lReadInput )THEN

  IF( lbl )THEN
    nzbl = MAX(3,nzbl)
  ELSE
    nzbl = 3
  END IF

END IF

met_type  = UseMetType
bl_type   = UseBLType
ensm_type = UseLSVType
pr_type   = UsePrecipType

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteWeather
!===============================================================================
SUBROUTINE WriteWeather( file,lunit,list,nList )

USE scipuff_fi
USE met_fi

!     Writes the SCIPUFF weather file (*.MSC)

IMPLICIT NONE

CHARACTER(*),               INTENT( IN ) :: file
INTEGER     ,               INTENT( IN ) :: lunit
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: list
INTEGER     ,               INTENT( IN ) :: nList

INTEGER ios

LOGICAL writeFix
LOGICAL writeObs
LOGICAL writeGrd

CHARACTER(80) UseMetType, UseBLType, UseLSVType, UsePrecipType

!==== Open the file

OPEN(UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE')
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteWeather'
  eMessage = 'Error opening SCIPUFF met scenario input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistMet( lunit )
IF( nError /= NO_ERROR )GOTO 9998
CALL CheckNamelistMet( UseMetType,UseBLType,UseLSVType,UsePrecipType )
IF( nError /= NO_ERROR )GOTO 9999

!==== Check Combinations

CALL CheckMetCombinations( UseMetType,UseBLType,UseLSVType,UsePrecipType )
IF( nError /= NO_ERROR )GOTO 9999

!==== Write the additional input lines

writeFix = .FALSE.
writeObs = .FALSE.
writeGrd = .FALSE.
SELECT CASE( TRIM(UseMetType) )
  CASE( 'FIXED' )
    writeFix = .TRUE.

  CASE( 'OBS' )
    writeObs = .TRUE.

  CASE( 'GRIDDED' )
    writeGrd = .TRUE.

  CASE( 'GRIDMC' )
    writeGrd = .TRUE.

  CASE( 'MEDOC' )
    writeGrd = .TRUE.

  CASE( 'WRF' )
    writeGrd = .TRUE.

  CASE( 'ASSIM','SCIPUFF_LIST' )
    writeGrd = .TRUE.

  CASE( 'MEDOC_LIST','MEDOCLIST' )
    writeGrd = .TRUE.

  CASE( 'MRF' )
    writeGrd = .TRUE.

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'WriteWeather'
    eMessage = 'Invalid meteorology (MET_TYPE)'
    eInform  = 'MET_TYPE='//TRIM(met_type)

END SELECT
IF( nError /= NO_ERROR )GOTO 9999

!==== Fixed winds

IF( writeFix )THEN

  CALL WriteFixedInput( lunit )
  IF( nError /= NO_ERROR )GOTO 9998

!==== Observations

ELSE IF( writeObs )THEN

  CALL WriteObsInput( lunit )
  IF( nError /= NO_ERROR )GOTO 9998

!==== Gridded

ELSE IF( writeGrd )THEN

  CALL WriteGriddedInput( lunit )
  IF( nError /= NO_ERROR )GOTO 9998

END IF

!==== Close the file and return

9999 CONTINUE

CLOSE(UNIT=lunit,IOSTAT=ios)

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
