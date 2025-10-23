INTEGER FUNCTION SWIMreadTerrainNest( TerFile,grid )

!------ Read terrain/landuse file for nests.
!       N.B. Does not convert to project coordinates
!            Assumes that Prj%Xmin, etc. have been set to DEF_VAL_R in the calling routine

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE TerHead_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid
CHARACTER(*),    INTENT( IN    ) :: TerFile

TYPE( TerHeaderStr ) :: TerHead

INTEGER irv, ios, lun, nrec, mcType
LOGICAL lerr, luse, lcat
REAL    lat0, lon0

CHARACTER(8) ter_map

INTEGER, EXTERNAL :: SWIMcheckTerrainHeader, ReadTerrainHeader
INTEGER, EXTERNAL :: CheckTerrainCoord, TransGridCoord, ReadTerrainFields
INTEGER, EXTERNAL :: SetTerrainDomain
INTEGER, EXTERNAL :: PostProgressMessage

SWIMreadTerrainNest = SWIMfailure

message%bString  = 'Reading terrain file'
irv = PostProgressMessage( message )

lun = SWIMunit

CALL ReportFileName( error%Inform,'File=',TerFile )

!------ Clear header structure

CALL ClearTerrainHeader( TerHead )

!------ Check header for map compatibility; determine if skipping is required
!       N.B. Ignore error for UTM/LL combinates

irv = SWIMcheckTerrainHeader( Prj%coord,TerFile,lerr,ter_map,nrec,luse,lcat )
IF( irv /= SWIMsuccess )THEN
  IF( ANY(Prj%coord==(/I_LATLON,I_UTM/)) .AND. &
      (TRIM(ter_map) == 'LATLON' .OR. TRIM(ter_map) == 'UTM') )THEN
    nrec = 0
    CALL SWIMclearError()
  ELSE
    error%Number  = IV_ERROR
    error%Routine = 'SWIMreadTerrainNest'
    error%Message = 'Incompatible project and nested terrain map types'
    GOTO 9999
  END IF
END IF

!------ Open terrain file

OPEN( lun,FILE=TerFile,STATUS='OLD',ACTION="READ",IOSTAT=ios )
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMreadTerrainNest'
  error%Message = 'Error opening terrain file'
  GOTO 9999
END IF

error%Action = TRIM(error%Inform)
error%Inform = ''

!------ Read header records

TerHead%nskip = nrec

mcType = Prj%MC%type
Prj%MC%type = IBSET(0,MCB_TER); Prj%MC%type = IBSET(Prj%MC%type,MCB_LC)

irv = ReadTerrainHeader( lun,TerHead )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Check terrain coordinates
!       N.B. Make sure project lat/lon reference is not redefined

lat0 = Prj%Lat0; lon0 = Prj%Lon0; Prj%Lon0 = 0.; Prj%Lat0 = 0.

irv = CheckTerrainCoord( TerHead )
IF( irv /= SWIMsuccess )GOTO 9999

Prj%Lon0 = lon0; Prj%Lat0 = lat0

!------ Set sub-domain limits
!       N.B. Prj%Xmin, etc. must equal DEF_VAL_R

irv = SetTerrainDomain( TerHead,grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Allocate and fill terrain and landuse arrays

irv = ReadTerrainFields( lun,TerHead,grid )
IF( irv /= SWIMsuccess )GOTO 9999

error%Action  = '' !Clear since no error occurred

Prj%MC%type = mcType

SWIMreadTerrainNest = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(TerHead%FieldName) )DEALLOCATE( TerHead%FieldName,STAT=ios )

CLOSE( UNIT=lun,IOSTAT=ios )

RETURN
END

