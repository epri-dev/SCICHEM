INTEGER FUNCTION ExtractSFCdata() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER istat, kount
REAL    zref, elev, lat, lon

CHARACTER(16) landuse

INTEGER, EXTERNAL :: WriteSFCheader, WriteSFCdata

!------ Initialize

irv = FAILURE

IF( SFClist%nFile == 0 )THEN
  error%Number = IV_ERROR
  error%Routine = 'ExtractSFCdata'
  error%aString = 'No SFC files specified'
  GOTO 9999
END IF

!------ Open scratch file for RDISHD output

OPEN(UNIT=lunScratch,STATUS='SCRATCH',ACTION='READWRITE',IOSTAT=ios)
IF( ios /= 0)THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractSFCdata'
  error%aString = 'Error opening SFC scratch file'
  GOTO 9999
END IF

!------ Loop over SFC input files
!       N.B. Currently limited to single file

currentFile => SFClist%First

DO WHILE( ASSOCIATED(currentFile) )

  OPEN(UNIT=lunSFC,FILE=TRIM(currentFile%name),ACTION='READ',IOSTAT=istat)
  IF( istat /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'ExtractSFCdata'
    error%aString = 'Error opening SFC file'
    error%bString = 'File='//TRIM(currentFile%name)
    GOTO 9999
  END IF

!------ Set AERMET common

  CALL SetSFC( lunSFC,lunScratch,lunTmp,currentFile%tAdj, &
               yearStartSFC,monthStartSFC,dayStartSFC, &
               yearEndSFC,  monthEndSFC,  dayEndSFC, &
               currentFile%ID,currentFile%clat,currentFile%clon, &
               currentFile%elev )

!------ Read with AREMET subroutine
!       N.B. Only read ISHD format  *** N.B. time is currently UTC

  istat = 0
  kount = 0
  CALL RDISHD( kount,istat )  
  IF( istat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'RDISHD'
    error%aString = 'Error reading SFC file'
    error%bString = 'File='//TRIM(currentFile%name)
    GOTO 9999
  END IF

  IF( TRIM(currentFile%ID) == 'NotSet' )THEN
    CALL GetSFCloc( istat,currentFile%lat,currentFile%lon,currentFile%ID )
    IF( istat /= 0 )THEN
      error%Number = RD_ERROR
      error%Routine = 'GetSFCloc'
      error%aString = 'Error reading ID and location from ISHD file'
      GOTO 9999
    END IF
  END IF

  IF( currentFile%elev == -9999. )THEN
    CALL GetSFCelev( istat,currentFile%elev )
    IF( istat /= 0 )THEN
      error%Number = RD_ERROR
      error%Routine = 'GetSFCelev'
      error%aString = 'Error reading elevation from ISHD file'
      GOTO 9999
    END IF
  END IF

  IF( lSFQ )THEN

    CALL SetSFQA( lunSFQ )

    OPEN(UNIT=lunSFQ,STATUS='SCRATCH',ACTION='READWRITE',IOSTAT=ios)
    IF( ios /= 0 )THEN
      error%Number  = OP_ERROR
      error%Routine = 'ExtractSFCdata'
      error%aString = 'Error opening SFC QA file'
      GOTO 9999
    END IF

    CALL SFQASM( istat )
    IF( istat /= 0 )THEN
      GOTO 9999
      error%Number  = UK_ERROR
      error%Routine = 'SFQASM'
      error%aString = 'Error in SFC QA'
    END IF

    CLOSE( lunSFQ,IOSTAT=ios )

  END IF
    
  CALL GetAnemHgt( currentFile%zref )  !Set anemometer height if read in RDISHD

  landuse = TRIM(currentFile%landuse)
  elev    = currentFile%elev
  lat     = currentFile%lat
  lon     = currentFile%lon

  CLOSE( lunSFC,IOSTAT=ios )

!------ Point to next file

  EXIT  !Limit to one file
  currentFile => currentFile%next

END DO

!------ Close temporary file and rewind scratch file

CLOSE(  lunTmp,    IOSTAT=ios )
REWIND( lunScratch,IOSTAT=ios )

OPEN(UNIT=lunOutSFC,FILE=TRIM(SFCfile),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
IF( ios /= 0)THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractSFCdata'
  error%aString = 'Error opening SFC outout file'
  error%bString = 'File='//TRIM(SFCfile)
  GOTO 9999
END IF

!------ Write header

irv = WriteSFCheader( elev,lat,lon,landuse )
IF( irv /= SUCCESS )THEN
  error%Number  = WR_ERROR
  error%Routine = 'ExtractSFCdata'
  error%aString = 'Error writing SFC output file header'
  GOTO 9999
END IF

!------ Read temporary file and output SCICHEM variables

irv = WriteSFCdata()
IF( irv /= SUCCESS )GOTO 9999

CLOSE( lunOutSFC, IOSTAT=ios )
CLOSE( lunScratch,IOSTAT=ios )

irv = SUCCESS

9999 CONTINUE

RETURN
END
