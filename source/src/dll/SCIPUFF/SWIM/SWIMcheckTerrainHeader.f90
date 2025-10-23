INTEGER FUNCTION SWIMcheckTerrainHeader( lmap_prj,filename,lerr,ter_map,nskip,luse,lcat )

!------ Check terrain file header for project compatibility

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: lmap_prj
LOGICAL,      INTENT( OUT ) :: lerr
CHARACTER(*), INTENT( IN  ) :: filename
CHARACTER(*), INTENT( OUT ) :: ter_map
LOGICAL,      INTENT( OUT ) :: luse
LOGICAL,      INTENT( OUT ) :: lcat
INTEGER,      INTENT( OUT ) :: nskip

CHARACTER(80) line

INTEGER lun, ios, lmap_met

LOGICAL, EXTERNAL :: CheckRef, CheckForLatLonTerr

SWIMcheckTerrainHeader = SWIMfailure

ter_map = 'NOT_SET'
lerr  = .FALSE.
luse  = .FALSE.
lcat  = .FALSE.
nskip = 0

!------ Use first file unit number

lun = SWIMunit

!------ Open terrain file

OPEN(lun,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios )
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMcheckTerrainHeader'
  error%Message = 'Error opening terrain file'
  CALL ReportFileName( error%Inform,'File=',filename )
  GOTO 9999
END IF

READ(lun,'(A80)',IOSTAT=ios) line
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMcheckTerrainHeader'
  error%Message = 'Error reading terrain file (1st record)'
  CALL ReportFileName( error%Inform,'File=',filename )
  GOTO 9999
END IF

CALL cupper( line )

IF( INDEX(line,'LAND_USE') /= 0 )THEN
  READ(lun,'(A8)',IOSTAT=ios) ter_map
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'Error reading terrain file (2nd record)'
    CALL ReportFileName( error%Inform,'File=',filename )
    GOTO 9999
  END IF
  CALL cupper( ter_map )
  luse = .TRUE.
  lcat = INDEX(line,'CATEGORY') /= 0
ELSE
  ter_map = line(1:8)
  luse = .FALSE.
  lcat = .FALSE.
END IF

IF( TRIM(ter_map) == 'SCALED' )THEN
  ter_map ='METERS'
END IF

!------ Set map parameters

SELECT CASE( TRIM(ter_map) )
  CASE( 'LATLON' )
    lmap_met = I_LATLON
  CASE( 'M','KM','METERS' )
    lmap_met = I_CARTESIAN
  CASE( 'UTM' )
    lmap_met = I_UTM
  CASE DEFAULT
    error%Number  = IV_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'Invalid map type on terrain file (1st record)'
    error%Inform  = 'Valid types are LATLON, M, METERS, KM or UTM'
    CALL ReportFileName( error%Action,'File=',filename )
    GOTO 9999
END SELECT

!------ Checks project / terrain coorindate combinations

nskip = 0

IF( lmap_prj == I_UTM .AND. lmap_met /= I_UTM )THEN
  lerr = .TRUE.
  error%Number  = IV_ERROR
  error%Routine = 'SWIMcheckTerrainHeader'
  error%Message = 'UTM project requires UTM terrain'
  CALL ReportFileName( error%Action,'File=',filename )
  GOTO 9999

ELSE IF( ANY(lmap_prj==(/I_CARTESIAN,I_METERS/)) .AND. lmap_met == I_UTM )THEN
  IF( .NOT.CheckForLatLonTerr(lun,nskip) )THEN
    lerr = .TRUE.
    error%Number  = IV_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'CARTESIAN project cannot use UTM terrain'
    error%Message = 'LATLON terrain is acceptable but none found'
    CALL ReportFileName( error%Action,'File=',filename )
    GOTO 9999
  ELSE
   lmap_met = I_LATLON
  END IF

ELSE IF( ANY(lmap_prj==(/I_CARTESIAN,I_METERS/)) .AND. lmap_met == I_LATLON )THEN
  !OK

ELSE IF( lmap_prj == I_LATLON .AND. lmap_met == I_UTM )THEN
  IF( .NOT.CheckForLatLonTerr(lun,nskip) )THEN
    lerr = .TRUE.
    error%Number  = IV_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'LATLON project cannot use UTM terrain'
    error%Message = 'No LATLON terrain found'
    CALL ReportFileName( error%Action,'File=',filename )
    GOTO 9999
  ELSE
   lmap_met = I_LATLON
  END IF

ELSE IF( lmap_prj == I_LATLON .AND. ANY(lmap_met==(/I_CARTESIAN,I_METERS/)) )THEN
  IF( .NOT.CheckForLatLonTerr(lun,nskip) )THEN
    lerr = .TRUE.
    error%Number  = IV_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'LATLON project cannot use CARTESIAN terrain'
    error%Message = 'No LATLON terrain found'
    CALL ReportFileName( error%Action,'File=',filename )
    GOTO 9999
  ELSE
   lmap_met = I_LATLON
  END IF

END IF

!------ Check for reference location for LL terrain w/ cartesian project

IF( ANY(lmap_prj==(/I_CARTESIAN,I_METERS/)) .AND. lmap_met == I_LATLON  )THEN
  IF( .NOT.CheckRef(PrjCoord%reference) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMcheckTerrainHeader'
    error%Message = 'Cartesian project requires reference location'
    lerr = .TRUE.
  END IF
END IF

SWIMcheckTerrainHeader = SWIMresult

9999 CONTINUE

CLOSE(lun,IOSTAT=ios)

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckForLatLonTerr( lun,nskip )

!------ Check for second (Lat/lon) section on terrain file

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: lun
INTEGER, INTENT( OUT ) :: nskip

INTEGER ios, imax, jmax, i
REAL    dum

CHARACTER(8) cmap

CheckForLatLonTerr = .FALSE.

nskip = 0

READ(lun,*,IOSTAT=ios) dum,dum,dum,dum,imax,jmax  !Skip to LL section (if it exists)
IF( ios /= 0 )GOTO 9999

nskip = (imax*jmax+11)/12

DO i = 1,nskip
  READ(lun,*,IOSTAT=ios)
  IF(ios /= 0 )GOTO 9999
END DO

READ(lun,'(A8)',IOSTAT=ios) cmap
IF(ios /= 0 )GOTO 9999

CALL cupper( cmap )

CheckForLatLonTerr = TRIM(cmap) =='LATLON'

9999 CONTINUE

IF( .NOT.CheckForLatLonTerr )nskip = 0

RETURN
END
