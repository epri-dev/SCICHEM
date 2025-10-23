INTEGER FUNCTION SWIMinitMEDOClist( unit )

!----- Setup appropriate number of met fields
!      Build MEDOC file names and store in GridSource structure

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: unit

CHARACTER(PATH_MAXLENGTH) :: path, root, string, listFile

INTEGER irv, alloc_stat, lun, iskip, ios
INTEGER i, j, nrd, mettype
REAL    MMIFver
LOGICAL lexist, lOldMMIF

TYPE ( messageT ) caution

CHARACTER(PATH_MAXLENGTH) :: NestSrc

INTEGER, EXTERNAL :: SWIMreallocMetField, SWIMaddLogMessage, PostProgressMessage
INTEGER, EXTERNAL :: SetGriddedType, SWIMinitMEDOC
INTEGER, EXTERNAL :: PostCautionMessage

!------ Initialize failure

SWIMinitMEDOClist  = SWIMfailure

listFile = TRIM(field(numField)%gridSource%Source(1))

error%Routine = 'SWIMinitMEDOClist'
error%Number  = RD_ERROR
CALL ReportFileName( error%Inform,'File=',listFile )

IF( Reverse )THEN
  error%Number  = IV_ERROR
  error%Message = 'MEDOC list input cannot be used with reverse mode'
  GOTO 9999
END IF

!------ Setup messaging

message%bString = 'Reading MEDOC list file '//TRIM(listFile)

irv = PostProgressMessage( message )

irv = SWIMaddLogMessage( message%bString )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Open list file

lun = unit
OPEN(UNIT=lun,FILE=TRIM(listFile),STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=irv )
IF( irv /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening MEDOC list file'
  GOTO 9999
END IF

!------ Initialize path and grid specs

path   = ''
iskip  = 1

!------ Read first line: should be 'MEDOC'

READ(lun,FMT='(A)',IOSTAT=irv) string
IF( irv /= 0 )THEN
  error%Message = 'Error reading first line in MEDOC list file'
  GOTO 9999
END IF

string = TRIM(ADJUSTL(string))
CALL cupper( string )

IF( TRIM(string(1:5)) /= 'MEDOC' )THEN
  error%Message = 'First line in MEDOC list file must be "MEDOC"'
  GOTO 9999
END IF

lOldMMIF = .FALSE.

i = INDEX(string,'MMIF')
IF( i > 0 )THEN
  READ(string(i+4:i+6),*,IOSTAT=ios) MMIFver
  IF( ios == 0 )THEN
    lOldMMIF = MMIFver < 3.4
  END IF
END IF

!------ Read second line: check for path; otherwise it's the file name

READ(lun,FMT='(A)',IOSTAT=irv) root
IF( irv /= 0 )THEN
  error%Message = 'Error reading first line in MEDOC list file'
  GOTO 9999
END IF

string = root !Copy and make case insensitive
CALL cupper( string )

i = INDEX(string,'PATH=')
IF( i > 0 )THEN
  path = TRIM(root(i+5:))
  iskip = iskip + 1
  READ(lun,FMT='(A)',IOSTAT=irv) root !Read first file name (if previous line was special input)
  IF( irv /= 0 )THEN
    error%Message = 'Error reading file name in MEDOC list file'
    GOTO 9999
  END IF

END IF

CALL AddPath( root,path )

INQUIRE( FILE=TRIM(root),EXIST=lexist )    !Make sure files exist
IF( .NOT.lexist )THEN
  error%Number  = IV_ERROR
  error%Message = 'MEDOC file does not exist'
  CALL ReportFileName( error%Inform,'File=',root )
END IF

!------ Read list file names and save

REWIND(lun,IOSTAT=irv)
IF( iskip > 0 )THEN
  DO i = 1,iskip
    READ(lun,FMT=*,IOSTAT=irv) !Skip path name
  END DO
END IF

nrd = 0
DO
  READ(lun,FMT='(A)',IOSTAT=irv) string
  IF( irv < 0 )THEN
    EXIT  !End-of-file
  ELSE IF( irv > 0 )THEN
    error%Message = 'Error reading MEDOC list file'
    GOTO 9999
  ELSE
    nrd = nrd + 1
  END IF
END DO

REWIND(lun,IOSTAT=irv)
IF( iskip > 0 )THEN
  DO i = 1,iskip
    READ(lun,FMT=*,IOSTAT=irv) !Skip path name
  END DO
END IF

i = numField

field(i)%gridSource%nSource = 0
ALLOCATE( field(i)%gridSource%Source(nrd),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating space for input file names'
  GOTO 9999
END IF

DO J = 1,nrd

  READ(lun,FMT='(A)',IOSTAT=irv) root
  IF( irv > 0 )THEN
    error%Message = 'Error reading MEDOC list file'
    GOTO 9999
  END IF
  CALL ADDPATH( root,path )
  INQUIRE( FILE=TRIM(root),EXIST=lexist )
  IF( lexist )THEN
    field(i)%gridSource%nSource = field(i)%gridSource%nSource + 1
    field(i)%gridSource%Source(field(i)%gridSource%nSource) = TRIM(root)
  END IF

END DO

CLOSE(lun,IOSTAT=irv )

field(i)%gridSource%type = 0                                       !Resets first field from GSB_INITMEDOC
field(i)%gridSource%type = IBSET(field(i)%gridSource%type,GSB_MEDOCLIST)
field(i)%gridSource%type = IBSET(field(i)%gridSource%type,GSB_MEDOC)

IF( lOldMMIF )field(i)%gridSource%type = IBSET(field(i)%gridSource%type,GSB_OLDMMIF)

CALL setFieldIndex( field(i)%gridSource%unit,unit )
CALL setSmoothIndex( field(i)%gridSource%unit,1 )

irv = SetGriddedType( field(i)%gridSource,metType )
IF( irv /= SWIMsuccess )GOTO 9999

IF( metType /= SWIMMEDOC )THEN
  error%Number  = IV_ERROR
  error%Message = 'File specified in list is not MEDOC type'
  CALL ReportFileName( error%Inform,'File=',root )
END IF

!------ Define source parameters

irv = SWIMinitMEDOC( field(i)%gridSource,NestSrc )
IF( irv /= SWIMsuccess )GOTO 9999

IF( LEN_TRIM(NestSrc) > 0 )THEN
  caution%aString = 'Nested MEDOC file ignored with MEDOC list input'
  caution%bString = 'File name = '//TRIM(root)
  caution%cString = 'Nest name = '//TRIM(NestSrc)
  irv = PostCautionMessage( caution )
END IF

SWIMinitMEDOClist = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

