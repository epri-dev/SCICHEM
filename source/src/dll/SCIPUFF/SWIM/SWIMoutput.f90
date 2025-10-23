INTEGER FUNCTION SWIMoutput()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMoutput

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER irv, i

INTEGER, EXTERNAL :: OutputMEDOC

SWIMoutput = SWIMfailure

DO i = 1,numField

  IF( field(i)%unitOut > 0 )THEN
    irv = OutputMEDOC( i )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

END DO

SWIMoutput = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMinitOutput()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMinitOutput

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER irv, i, unit

INTEGER, EXTERNAL :: AddOutput

SWIMinitOutput = SWIMfailure

IF( Prj%lOut3D .OR. Prj%lOut2D )THEN

  unit = unitMet

  DO i = 1,numField
    IF( BTEST(field(i)%type,FTB_NPOLE) .OR. BTEST(field(i)%type,FTB_SPOLE) )CYCLE
    irv = AddOutput( i,unit )
    IF( irv /= SWIMsuccess )GOTO 9999
  END DO

  lInitOutput = .TRUE.
  unitMet     = unit

END IF

SWIMinitOutput = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION AddOutput( ifld,unit )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ifld
INTEGER, INTENT( INOUT ) :: unit

INTEGER irv, ios, ip

CHARACTER(PATH_MAXLENGTH) :: base, file

CHARACTER(16)  :: ext

INTEGER, EXTERNAL :: OpenOutputFile
INTEGER, EXTERNAL :: OutputModelHeader

AddOutput = SWIMfailure

!------ Open file associated with ifld-th met field

unit = unit + 1

IF( ifld > 1 )THEN
  base = TRIM(Prj%outFile(1:LEN_TRIM(Prj%outFile)-1))
  WRITE(ext,*,IOSTAT=ios) ifld
  IF( ios /= 0 )GOTO 9999
  file = TRIM(base)//TRIM(ADJUSTL(ext))
ELSE
  file = TRIM(Prj%outFile)
END IF

field(ifld)%unitOut = unit
field(ifld)%fileOut = TRIM(file)

irv = OpenOutputFile( unit,TRIM(file) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Write special header for non-Lat/lon or UTM coordinates or non-standard vertical coord

IF( Prj%lCreateOut .AND. ANY(BTEST(field(ifld)%grid%type, &
                                   (/GTB_LAMBERT,GTB_POLAR,GTB_RPOLAR,GTB_MERCATOR, &
                                     GTB_ROTLL,GTB_SIGMA,GTB_Z3D,GTB_UTM/))) )THEN
  ip = ifld
  DO WHILE( BTEST(field(ip)%type,FTB_SMOOTH) )  !Use original parent for smooth fields
    ip = field(ip)%gridSource%unit
  END DO

  irv = OutputModelHeader( unit,Prj%lFormat,field(ip)%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

AddOutput = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION OpenOutputFile( unit,file )

USE SWIM_FI
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: unit
CHARACTER(*), INTENT( IN ) :: file

INTEGER ios

OpenOutputFile = SWIMfailure

!------ If creating new file, open with 'replace'

IF( Prj%lCreateOut )THEN

  IF( Prj%lFormat )THEN
    OPEN(unit,FILE=TRIM(file),STATUS='REPLACE',FORM='FORMATTED',IOSTAT=ios)
  ELSE
    OPEN(unit,FILE=TRIM(file),STATUS='REPLACE',FORM='UNFORMATTED',IOSTAT=ios)
  END IF

ELSE !-- Otherwise, open with 'append'

  IF( Prj%lFormat )THEN
    OPEN(unit,FILE=TRIM(file),STATUS='UNKNOWN',FORM='FORMATTED', &
                                    POSITION='APPEND',IOSTAT=ios)
  ELSE
    OPEN(unit,FILE=TRIM(file),STATUS='UNKNOWN',FORM='UNFORMATTED', &
                                    POSITION='APPEND',IOSTAT=ios)
  END IF

END IF

IF( ios /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'OpenOutputFile'
  error%Message = 'Error opening met output file'
  CALL ReportFileName( error%Inform,'File=',file )
  GOTO 9999
END IF

OpenOutputFile = SWIMresult

9999 CONTINUE

RETURN
END
