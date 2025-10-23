PROGRAM met2sci

!------ Generate SCIPUFF weather observation files
!       Based on AERMET (dated 12345)

!       Written by D. Henn, Sage / Xator Corp., Princeton, NJ

USE met2sci_fi

IMPLICIT NONE

INTEGER irv
INTEGER iPathway

INTEGER, EXTERNAL :: ParseJobInput, ParseUpperAirInput, ParseSurfaceInput, ParseOnsiteInput
INTEGER, EXTERNAL :: ExtractUAdata, ExtractSFCdata, ExtractOSdata

!------ Initialization

lun        =  1      !Basic input/control file
lunUA      =  2
lunSfc     =  3
lunOS      =  4
lunOutSFC  = 130
lunOutUA   = 131
lunOutOS   = 132
lunTmp     = 199
lunScratch = 198
lunErr     = 197

iPathway  = NoPathway
lJob      = .FALSE.
lUpperAir = .FALSE.
lSurface  = .FALSE.
lOnsite   = .FALSE.
lUAQ      = .TRUE.
lSFQ      = .TRUE.
lRandUA   = .FALSE.
lRandSFC  = .FALSE.
dRan0     = 10.
dRanUA    = 1.
dRanSFC   = 1.

nOSrecord = 0
nOSformat = 0
nOSrange  = 0
nOSheight = 0
iPress    = 0
iPressSL  = 0

UAlist%nFile  = 0; NULLIFY( UAlist%first )
SFClist%nFile = 0; NULLIFY( SFClist%first )
OSlist%nFile  = 0; NULLIFY( OSlist%first )

UAfile = ''; SFCfile = ''; OSfile = ''
reportFile = '';  messageFile = ''
!QAfileUA = ''; QAfileSFC = ''

OSdataLine = ''; OSqaoutLine = ''; OSxdatesLine = ''; OSlocationLine = ''; OSheightLine = ''
OSauditLine = ''; OSobsperhourLine = ''; OSnomissingLine = ''; OSthresholdLine = ''

nvars = 0; nvarp = 0

NULLIFY( firstOSread%first )
NULLIFY( firstOSformat%first )
NULLIFY( firstOSrange%first )

error%Number  = NO_ERROR
error%Routine = ''
error%aString = ''
error%bString = ''
error%cString = ''

!------ Define some AERMET data

CALL SetAERMETdata()

!------ Open file name from command line

CALL getarg(1,inputFile)

IF( LEN_TRIM(inputFile) == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'met2sci'
  error%aString = 'No input file specifed on command line'
  GOTO 9999
END IF

OPEN(UNIT=lun,FILE=TRIM(inputFile),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'met2sci'
  error%aString = 'Error opening input file '//TRIM(inputFile)
  GOTO 9999
END IF

!----- Read down file looking for pathway sections and data
!      N.B. blank lines are skipped and all strings are
!      converted to upper case

DO

  line = ' '
  CALL get_next_data( lun,line,nch,kwrd,narg,carg,-MAXN,lerr )  !-MAXN so '=' does not denot keyword input
  IF( lerr )EXIT !Assumed to be EOF error

  CALL cupper( carg(1) )

!----- Skip comments

  IF( carg(1)(1:1) == '*' )CYCLE

!----- Check for one of 3 pathways
!      JOB, UPPERAIR, or SURFACE

  SELECT CASE( TRIM(carg(1)) )

    CASE( 'JOB' )
      iPathway = JOB;
      lJob     = .TRUE.
      CYCLE

    CASE( 'UPPERAIR' )
      iPathway  = UPPERAIR
      lUpperAir = .TRUE.
      NULLIFY( currentFile )
      CYCLE

    CASE( 'SURFACE' )
      iPathway = SURFACE
      lSurface = .TRUE.
      NULLIFY( currentFile )
      CYCLE

    CASE( 'ONSITE' )
      iPathway = ONSITE
      lOnsite  = .TRUE.
      NULLIFY( currentFile )
      CYCLE

  END SELECT

!------ Parse input for current pathway

  SELECT CASE( iPathway )

    CASE( JOB )
      irv = ParseJobInput()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( UPPERAIR )
      irv = ParseUpperAirInput()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( SURFACE )
      irv = ParseSurfaceInput()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( ONSITE )
      irv = ParseOnsiteInput()
      IF( irv /= SUCCESS )GOTO 9999

  END SELECT

END DO

!------ Open report and message files
!       for AERMET routines 

CALL SetDeviceUnits( lunRpt,lunMsg )

IF( LEN_TRIM(reportFile) > 1 )THEN
  OPEN(UNIT=lunRpt,FILE=TRIM(reportFile),STATUS='UNKNOWN',ACTION='READWRITE',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'met2sci'
    error%aString = 'Error opening report file'
    error%bString = 'File='//TRIM(reportFile)
    GOTO 9999
  END IF
END IF

IF( LEN_TRIM(messageFile) > 1 )THEN
  OPEN(UNIT=lunMsg,FILE=TRIM(messageFile),STATUS='UNKNOWN',ACTION='READWRITE',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'met2sci'
    error%aString = 'Error opening message file'
    error%bString = 'File='//TRIM(messageFile)
    GOTO 9999
  END IF
END IF

!------ Read UA and SFC files and create corresponding SCICHEM files
!       N.B. Call AREMET subroutines

IF( UAlist%nFile > 0 )THEN
  irv = ExtractUAdata()
  IF( irv /= SUCCESS )GOTO 9999
END IF

IF( SFClist%nFile > 0 )THEN
  irv = ExtractSFCdata()
  IF( irv /= SUCCESS )GOTO 9999
END IF

IF( OSlist%nFile > 0 )THEN
  irv = ExtractOSdata()
  IF( irv /= SUCCESS )GOTO 9999
END IF

9999 CONTINUE

!------ Write error messages to met2sci.err

IF( error%Number /= NO_ERROR )THEN
  CALL ReportError()
END IF

!------ Create (partial) Stage 1 AERMET report

IF( UAlist%nFile + SFClist%nFile > 0  )THEN  !N.B. ONSITE does not generate any report info
  CALL SUMRY2( 1 )
END IF

CLOSE(lun,   IOSTAT=ios)
CLOSE(lunMsg,IOSTAT=ios)
CLOSE(lunRpt,IOSTAT=ios)

STOP

END PROGRAM met2sci

