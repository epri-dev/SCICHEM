INTEGER FUNCTION ExtractOSdata() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER istat
LOGICAL lFirst

INTEGER KOUNT, KEYID, OSCNTR, HOUR, MINUTE
CHARACTER(132), DIMENSION(10) :: BUF80

INTEGER, EXTERNAL :: ParseOnsiteVar, WriteOSheader, WriteOSdata
LOGICAL, EXTERNAL :: IsErr

!------ Initialize and basic checks.

irv = FAILURE

IF( OSlist%nFile == 0 )THEN
  error%Number = IV_ERROR
  error%Routine = 'ExtractOSdata'
  error%aString = 'No ONSTITE files specified'
  GOTO 9999
END IF

IF( nOSrecord /= nOSformat )THEN
  error%Number = IV_ERROR
  error%Routine = 'ExtractOSdata'
  error%aString = 'ONSITE FORMAT records do not match READ records'
  GOTO 9999
END IF

!------ Setup for read errors

error%Number  = RD_ERROR
error%Routine = 'ExtractOSdata'
error%aString = 'Error parsing ONSTITE input'

!------ Setup some AERMET common values

CALL SetOS()

KOUNT = 0

!------ Input file

error%aString = 'data record'

KOUNT = KOUNT + 1
BUF80(1) = TRIM(OSdataLine)

CALL DEFINE( 132,BUF80(1) )
CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 4 )GOTO 9999
CALL SetKEYID( KEYID )

CALL OSCARD( KOUNT,BUF80(1),1 )
IF( IsErr() )GOTO 9999

!------ Loop through READ records

error%aString = 'READ records'
lFirst = .TRUE.
OSread => firstOSread%first

DO WHILE( ASSOCIATED(OSread) )
  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSread%line)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 18 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

  IF( lFirst )THEN

    irv = ParseOnsiteVar( BUF80(1) )
    IF( irv /= SUCCESS )GOTO 9999
    CALL get_next_data( 0,BUF80(1),nch,kwrd,narg,carg,-MAXN,lerr )
    IF( lerr )GOTO 9999
    lFirst = .FALSE.

  END IF

  CALL cupper( carg(1) )

  OSread => OSread%next
END DO

!------ Loop through FORMAT records

error%aString = 'FORMAT records'

OSread => firstOSformat%first

DO WHILE( ASSOCIATED(OSread) )
  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSread%line)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 6 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

  OSread => OSread%next
END DO

!------ Loop through RANGE records

error%aString = 'RANGE records'

OSread => firstOSrange%first

DO WHILE( ASSOCIATED(OSread) )
  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSread%line)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 13 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

  OSread => OSread%next
END DO

!------ Setup heights

IF( nOSheight > 0 )THEN

  error%aString = 'height record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSheightLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 16 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

!------ Other optional input

IF( LEN_TRIM(OSqaoutLine) > 0 )THEN

  error%aString = 'QAOUT record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSqaoutLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 8 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSxdatesLine) > 0 )THEN

  error%aString = 'XDATES record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSxdatesLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 9 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSlocationLine) > 0 )THEN

  error%aString = 'LOCATION record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSlocationLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 10 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSauditLine) > 0 )THEN

  error%aString = 'AUDIT record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSauditLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 14 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSobsperhourLine) > 0 )THEN

  error%aString = 'OBS/HOUR record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSobsperhourLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 28 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )  !N.B. Data is NOT averaged
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSnomissingLine) > 0 )THEN

  error%aString = 'MISSING record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSnomissingLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 5 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

IF( LEN_TRIM(OSthresholdLine) > 0 )THEN

  error%aString = 'THRESHOLD record'

  KOUNT = KOUNT + 1
  BUF80(1) = TRIM(OSthresholdLine)

  CALL DEFINE( 132,BUF80(1) )
  CALL FDKEY( KOUNT,BUF80(1),KEYID ); IF( KEYID /= 17 )GOTO 9999
  CALL SetKEYID( KEYID )

  CALL OSCARD( KOUNT,BUF80(1),1 )
  IF( IsErr() )GOTO 9999

END IF

!------ Perform AERMET checks

CALL OSTEST( 1,istat )
IF( istat /= 0 )GOTO 9999

!------ Open output file

OPEN(UNIT=lunOutOS,FILE=TRIM(OSfile),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
IF( ios /= 0)THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractOSdata'
  error%aString = 'Error opening OS outout file'
  error%bString = 'File='//TRIM(osfile)
  GOTO 9999
END IF

!------ Write header

irv = WriteOSheader()
IF( irv /= SUCCESS )THEN
  error%Number  = WR_ERROR
  error%Routine = 'ExtractOSdata'
  error%aString = 'Error writing OS output file header'
  GOTO 9999
END IF

!------ Check levels are defined properly

CALL GetNlev( istat )
IF( .NOT.lOSHGT .AND. istat > 1 )THEN
  error%Number  = IV_ERROR
  error%bString = 'Multiple levels without heights defined'
  GOTO 9999
END IF

!------ Open ONSITE data file

currentFile => OSlist%First

!DO WHILE( ASSOCIATED(currentFile) )

OPEN(UNIT=lunOS,FILE=TRIM(currentFile%name),ACTION='READ',IOSTAT=istat)
IF( istat /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'ExtractOSdata'
  error%aString = 'Error opening OS file'
  error%bString = 'File='//TRIM(currentFile%name)
  GOTO 9999
END IF

CALL SetDateRange()

!------ Read all data using AERMET routines 

OSCNTR = 0 
DO 
  CALL OSFILL2( 5,OSCNTR,lunOS,istat )
  IF( istat == 4 )EXIT
  IF( istat /= 0 )THEN
    error%bString = 'Reading data'
    GOTO 9999
  END IF
  CALL GetHour( HOUR,MINUTE )
  CALL OSSWAP( 1 )
  irv = WriteOSdata()
  IF( irv /= SUCCESS )GOTO 9999
END DO

CONTINUE

irv = SUCCESS

CALL ClearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseOnsiteVar( string ) RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

INTEGER i, j, k
LOGICAL lHT, lTT, lWD, lWS, lRH

irv = FAILURE
 
error%Number  = IV_ERROR
error%Routine = 'ParseOnsiteVar'
error%aString = 'Error parsing ONSITE input'

!------ Parse string
!       N.B. Assumes arguments are space-delimited (no commas)

line = TRIM(string)

CALL get_next_data( 0,line,nch,kwrd,narg,carg,-MAXN,lerr )
IF( lerr )THEN
  GOTO 9999
END IF

!------ Loop over arguments

DO i = 1,narg

!------ Check date/time input

  SELECT CASE( TRIM(carg(i)) )
    CASE( 'OSDY' )
      iOSDY = i
    CASE( 'OSMO' )
      iOSMO = i
    CASE( 'OSYR' )
      iOSYR = i
    CASE( 'OSHR' )
      iOSHR = i
    CASE( 'OSMN' )
      iOSMN = i
  END SELECT

!------ Look for surface variables

  SELECT CASE( TRIM(carg(i)) )
    CASE( 'HFLX','USTR','MHGT','ZOHT','TSKC' )
      nvars = nvars + 1
    CASE( 'PRES' )
      iPress = i
    CASE( 'SLVP' )
      iPressSL = i
  END SELECT

!------ Look for profile variables denoted by the first 2 characters
!       N.B. Check only first occurent 

lHT = .FALSE.; lTT = .FALSE.; lWD = .FALSE.; lWS = .FALSE.; lRH = .FALSE.

  SELECT CASE( carg(i)(1:2) )
    CASE( 'HT' )
      IF( .NOT.lHT )nvarp = nvarp + 1
      lHT = .TRUE.
    CASE( 'TT' )
      IF( .NOT.lTT )nvarp = nvarp + 1
      lTT = .TRUE.
    CASE( 'WD' )
      IF( .NOT.lWD )nvarp = nvarp + 1
      lWD = .TRUE.
    CASE( 'WS' )
      IF( .NOT.lWS )nvarp = nvarp + 1
      lWS = .TRUE.
    CASE( 'RH' )
      IF( .NOT.lRH )nvarp = nvarp + 1
      lRH = .TRUE.
  END SELECT

END DO

!------ Basic error checks

IF( nvars+nvarp == 0 )GOTO 9999  !Error if no variables recognized

IF( ANY((/iOSDY,iOSMO,iOSYR,iOSHR/) == 0) )GOTO 9999 !Incomplete date/time

IF( nvarp == 0 )THEN  !Output pressure only if other "profile" quantities are available.
  iPress   = 0
  iPressSL = 0
END IF

!------ Allocate arrays for extracting data

IF( nvars > 0 )THEN
  ALLOCATE( OSstype(nvars),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( nvarp > 0 )THEN
  ALLOCATE( OSptype(nvarp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

!------ Loop again; set IDs

j = 0
k = 0

lHT = .FALSE.; lTT = .FALSE.; lWD = .FALSE.; lWS = .FALSE.; lRH = .FALSE.

DO i = 1,narg

  SELECT CASE( TRIM(carg(i)) )
    CASE( 'HFLX')
      j = j + 1
      OSstype(j) = OSHFLX; CYCLE
    CASE( 'USTR')
      j = j + 1
      OSstype(j) = OSUSTR; CYCLE
    CASE( 'MHGT')
      j = j + 1
      OSstype(j) = OSMHGT; CYCLE
    CASE( 'ZOHT')
      j = j + 1
      OSstype(j) = OSZ0HT; CYCLE
    CASE( 'TSKC')
      j = j + 1
      OSstype(j) = OSSKY; CYCLE
  END SELECT

  SELECT CASE( carg(i)(1:2) )
    CASE( 'HT' )
      IF( lHT )CYCLE
      k = k + 1
      OSptype(k) = OSHGT
      lHT = .TRUE.
    CASE( 'TT' )
      IF( lTT )CYCLE
      k = k + 1
      OSptype(k) = OSTEMP
      lTT = .TRUE.
    CASE( 'WD' )
      IF( lWD )CYCLE
      k = k + 1
      OSptype(k) = OSWDIR
      lWD = .TRUE.
    CASE( 'WS' )
      IF( lWS )CYCLE
      k = k + 1
      OSptype(k) = OSWSPD
      lWS = .TRUE.
    CASE( 'RH' )
      IF( lRH )CYCLE
      k = k + 1
      OSptype(k) = OSRH
      lRH = .TRUE.
  END SELECT
    
END DO

irv = SUCCESS

CALL ClearError()

9999 CONTINUE

RETURN
END
      