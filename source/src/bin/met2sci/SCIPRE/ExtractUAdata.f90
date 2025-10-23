INTEGER FUNCTION ExtractUAdata() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER istat, NSDGS

INTEGER, EXTERNAL :: WriteUAheader, WriteUAdata

!------ Initialize

irv = FAILURE

IF( UAlist%nFile == 0 )THEN
  error%Number = IV_ERROR
  error%Routine = 'GetUA'
  error%aString = 'No UA files specified'
  GOTO 9999
END IF

NSDGS = 0

!------ Open scratch file for GetFSL output

OPEN(UNIT=lunScratch,STATUS='SCRATCH',ACTION='READWRITE',IOSTAT=ios)
IF( ios /= 0)THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractUAdata'
  error%aString = 'Error opening UA scratch file'
  GOTO 9999
END IF

!------ Loop over UA input files

currentFile => UAlist%First

DO WHILE( ASSOCIATED(currentFile) )

  OPEN(UNIT=lunUA,FILE=TRIM(currentFile%name),ACTION='READ',IOSTAT=istat)
  IF( istat /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'ExtractUAdata'
    error%aString = 'Error opening UA file'
    error%bString = 'File='//TRIM(currentFile%name)
    GOTO 9999
  END IF

!------ Set AERMET common

  CALL SetUA( lunUA,lunScratch,currentFile%tAdj, &
              yearStartUA,monthStartUA,dayStartUA, &
              yearEndUA,  monthEndUA,  dayEndUA,  currentFile%ID )

!------ Read with AREMET subroutine

  istat = 0
  CALL UAEXT( istat )
  IF( istat /= 0 )THEN
    error%Number = RD_ERROR
    error%Routine = 'GETFSL'
    error%aString = 'Error reading FSL file'
    GOTO 9999
  END IF

  IF( TRIM(currentFile%ID) == 'NotSet' )THEN
    CALL GetUAloc( istat,currentFile%lat,currentFile%lon,currentFile%ID )
    IF( istat /= 0 )THEN
      error%Number = RD_ERROR
      error%Routine = 'GetUAloc'
      error%aString = 'Error reading ID and location from FSL file'
      GOTO 9999
    END IF
  END IF
   
  IF( lUAQ )THEN

    CALL SetUAQA( lunUAQ )

    OPEN(UNIT=lunUAQ,STATUS='SCRATCH',ACTION='READWRITE',IOSTAT=ios)
    IF( ios /= 0 )THEN
      error%Number  = OP_ERROR
      error%Routine = 'ExtractUAdata'
      error%aString = 'Error opening UA QA file'
      GOTO 9999
    END IF

    WRITE(lunUAQ,'(A)',IOSTAT=ios) '***' !End-of-header section (needed for QA processing)

    CALL UAQASM( istat )
    IF( istat /= 0 )THEN
      GOTO 9999
      error%Number  = UK_ERROR
      error%Routine = 'UAQASM'
      error%aString = 'Error in UA QA'
    END IF

    CLOSE( lunUAQ,IOSTAT=ios )

  END IF

  CLOSE( lunUA,IOSTAT=ios )

!------ Point to next file

  EXIT  !Limit to one file
  currentFile => currentFile%next

END DO

!------ Rewind scratch file

REWIND( lunScratch,IOSTAT=ios )

OPEN(UNIT=lunOutUA,FILE=TRIM(UAfile),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
IF( ios /= 0)THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractUAdata'
  error%aString = 'Error opening UA outout file'
  error%bString = 'File='//TRIM(UAfile)
  GOTO 9999
END IF

!------ Write header

irv = WriteUAheader()
IF( irv /= SUCCESS )THEN
  error%Number  = WR_ERROR
  error%Routine = 'ExtractUAdata'
  error%aString = 'Error writing UA output file header'
  GOTO 9999
END IF

!------ Read temporary file and output SCICHEM variables

irv = WriteUAdata()
IF( irv /= SUCCESS )GOTO 9999

CLOSE( lunOutUA,  IOSTAT=ios )
CLOSE( lunScratch,IOSTAT=ios )

irv = SUCCESS

9999 CONTINUE

RETURN
END
