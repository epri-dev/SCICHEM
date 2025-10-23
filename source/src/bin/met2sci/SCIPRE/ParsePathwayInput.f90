INTEGER FUNCTION ParseJobInput() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

irv = FAILURE

SELECT CASE( TRIM(carg(1)) )

  CASE( 'REPORT' )
    IF( narg > 1 )THEN
      reportFile = TRIM(carg(2))
    END IF
          
  CASE( 'MESSAGES' )
    IF( narg > 1 )THEN
      messageFile = TRIM(carg(2))
    END IF

END SELECT

irv = SUCCESS

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseUpperAirInput() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER, EXTERNAL :: ParseDate, ParseLocation

irv = FAILURE

error%Routine = 'ParseUpperAirInput'

SELECT CASE( TRIM(carg(1)) )

  CASE( 'DATA' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for DATA'
      error%bString = 'UA file name and type required'
      GOTO 9999
    END IF

    INQUIRE( FILE=TRIM(carg(2)),EXIST=lexist )
    IF( .NOT.lexist )THEN
      error%Number  = NF_ERROR
      error%aString = 'UA file not found'
      error%bString = TRIM(carg(2))
      GOTO 9999
    END IF

    UAlist%nFile = UAlist%nFile + 1
      
    IF( UAlist%nFile == 1 )THEN
      ALLOCATE( currentFile,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error initializing link-list of UA files'
        GOTO 9999
      END IF
      UAlist%First => currentFile
    ELSE 
      ALLOCATE( currentFile%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error adding to link-list of UA files'
        GOTO 9999
      END IF
      currentFile => currentFile%next 
    END IF

    CALL InitFileList()

    currentFile%name = TRIM(carg(2))
    currentFile%type = TRIM(carg(3))

  CASE( 'EXTRACT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for EXTRACT'
      error%bString = 'UA file name required'
      GOTO 9999
    END IF

    UAfile = TRIM(carg(2))
    
  CASE( 'QAOUT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for QAOUT'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lUAQ = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'
    
  CASE( 'RANDOM' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for RANDOM'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lRandUA = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'
    
    IF( narg > 2 )THEN
      READ(carg(3),*,IOSTAT=ios ) dRanUA
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading angle precision for adding random component'
        error%bString = 'Must specify a positive real number'
        GOTO 9999
      END IF
      IF( dRanUA < 1. )THEN
        error%Number  = IV_ERROR
        error%aString = 'Invalid value for random angle precision'
        error%bString = 'Must specify be 1 or greater'
        GOTO 9999
      END IF
      dRanUA = dRanUA / dRan0
    END IF

  CASE( 'XDATES' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for UA XDATES'
      error%bString = 'Start and end dates required'
      GOTO 9999
    END IF

    ios = ParseDate( carg(2),yearStartUA,monthStartUA,dayStartUA )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading start date from UA XDATES'
      GOTO 9999
    END IF

    ios = ParseDate( carg(narg),yearEndUA,monthEndUA,dayEndUA )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading end date from UA XDATES'
      GOTO 9999
    END IF

  CASE( 'LOCATION' )

    IF( .NOT.ASSOCIATED(currentFile) )THEN
      error%Number  = IV_ERROR
      error%aString = 'LOCATION must follow DATA'
      GOTO 9999
    END IF

    IF( narg < 4 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for UA LOCATION'
      error%bString = 'ID, Lat, Lon required'
      GOTO 9999
    END IF

    currentFile%ID = TRIM(carg(2))

    ios = ParseLocation( carg(3),carg(4),currentFile%lat,currentFile%lon, &
                                        currentFile%clat,currentFile%clon)
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading UA LOCATION lat/lon'
      GOTO 9999
    END IF

    IF( narg > 4 )THEN
      READ(carg(5),*,IOSTAT=ios) currentFile%tAdj
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading UA LOCATION time adjustment'
        GOTO 9999
      END IF
    END IF
 
    IF( narg > 5 )THEN
      READ(carg(6),*,IOSTAT=ios) currentFile%elev
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading UA LOCATION elevation'
        GOTO 9999
      END IF
    END IF
 
END SELECT

irv = SUCCESS
error%Routine = ''

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseSurfaceInput() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER i

INTEGER, EXTERNAL :: ParseDate, ParseLocation

irv = FAILURE

SELECT CASE( TRIM(carg(1)) )

  CASE( 'DATA' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for DATA'
      error%bString = 'SFC file name and type required'
      GOTO 9999
    END IF

    INQUIRE( FILE=TRIM(carg(2)),EXIST=lexist )
    IF( .NOT.lexist )THEN
      error%Number  = NF_ERROR
      error%aString = 'SFC File not found'
      error%bString = TRIM(carg(2))
      GOTO 9999
    END IF

    SFClist%nFile = SFClist%nFile + 1
      
    IF( SFClist%nFile == 1 )THEN
      ALLOCATE( currentFile,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error initializing link-list of SFC files'
        GOTO 9999
      END IF
      SFClist%First => currentFile
    ELSE 
      ALLOCATE( currentFile%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error adding to link-list of SFC files'
        GOTO 9999
      END IF
      currentFile => currentFile%next 
    END IF

    CALL InitFileList()

    currentFile%name = TRIM(carg(2))
    currentFile%type = TRIM(carg(3))

  CASE( 'EXTRACT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for EXTRACT'
      error%bString = 'SFC file name required'
      GOTO 9999
    END IF

    SFCfile = TRIM(carg(2))

  CASE( 'QAOUT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for QAOUT'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lSFQ = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'
    
  CASE( 'RANDOM' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for RANDOM'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lRandSFC = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'

    IF( narg > 2 )THEN
      READ(carg(3),*,IOSTAT=ios ) dRanSFC
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading angle precision for adding random component'
        error%bString = 'Must specify a positive real number'
        GOTO 9999
      END IF
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading angle precision for adding random component'
        error%bString = 'Must specify a positive real number'
        GOTO 9999
      END IF
      IF( dRanSFC < 1. )THEN
        error%Number  = IV_ERROR
        error%aString = 'Invalid value for random angle precision'
        error%bString = 'Must specify be 1 or greater'
        GOTO 9999
      END IF
      dRanSFC = dRanSFC / dRan0

    END IF

  CASE( 'XDATES' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for SFC XDATES'
      error%bString = 'Start and end dates required'
      GOTO 9999
    END IF

    ios = ParseDate( carg(2),yearStartSFC,monthStartSFC,dayStartSFC )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading start date from SFC XDATES'
      GOTO 9999
    END IF

    ios = ParseDate( carg(narg),yearEndSFC, monthEndSFC,dayEndSFC )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading end date from SFC XDATES'
      GOTO 9999
    END IF

  CASE( 'LOCATION' )

    IF( .NOT.ASSOCIATED(currentFile) )THEN
      error%Number  = IV_ERROR
      error%aString = 'LOCATION must follow DATA'
      GOTO 9999
    END IF

    IF( narg < 4 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for SFC LOCATION'
      error%bString = 'ID, Lat, Lon required'
      GOTO 9999
    END IF

    currentFile%ID = TRIM(carg(2))

    ios = ParseLocation( carg(3),carg(4),currentFile%lat,currentFile%lon, &
                                        currentFile%clat,currentFile%clon)
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading SFC LOCATION lat/lon'
      GOTO 9999
    END IF

    IF( narg > 4 )THEN
      READ(carg(5),*,IOSTAT=ios) currentFile%tAdj
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading SFC LOCATION time adjustment'
        GOTO 9999
      END IF
    END IF
 
    IF( narg > 5 )THEN
      READ(carg(6),*,IOSTAT=ios) currentFile%elev
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading SFC LOCATION elevation'
        GOTO 9999
      END IF
    END IF
 
!  CASE( 'AUDIT' )
!    Always audit wind speed & direction if QA

  CASE( 'LANDUSE','LANDCOVER' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for LANDUSE'
      error%bString = 'Landuse type must be specified'
      GOTO 9999
    END IF

    SELECT CASE( TRIM(carg(2)) )
      CASE( 'GRASSLAND','URBAN','FOREST','DESERT','WATER','CULTIVATED' )
      CASE DEFAULT
        error%Number  = WN_ERROR
        error%aString = 'Landuse category may not be recognized by SCICHEM'
        error%bString = 'Standard categories are:'
        error%cString = 'GRASSLAND, URBAN, FOREST, DESERT, WATER, or CULTIVATED'
        GOTO 9999
    END SELECT

    currentFile%landuse = TRIM(carg(2))

END SELECT

irv = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseOnsiteInput() RESULT( irv )

USE met2sci_fi

IMPLICIT NONE

INTEGER nrec

INTEGER, EXTERNAL :: ParseDate, ParseLocation

irv = FAILURE

error%Routine = 'ParseOnsiteInput'

SELECT CASE( TRIM(carg(1)) )

  CASE( 'DATA' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for DATA'
      error%bString = 'ONSITE file name'
      GOTO 9999
    END IF

    INQUIRE( FILE=TRIM(carg(2)),EXIST=lexist )
    IF( .NOT.lexist )THEN
      error%Number  = NF_ERROR
      error%aString = 'ONSITE data file not found'
      error%bString = TRIM(carg(2))
      GOTO 9999
    END IF

    OSlist%nFile = OSlist%nFile + 1
      
    IF( OSlist%nFile == 1 )THEN
      ALLOCATE( currentFile,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error initializing link-list of ONSITE files'
        GOTO 9999
      END IF
      OSlist%First => currentFile
    ELSE 
      ALLOCATE( currentFile%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number = UK_ERROR
        error%aString = 'Error adding to link-list of ONSITE files'
        GOTO 9999
      END IF
      currentFile => currentFile%next 
    END IF

    CALL InitFileList()

    currentFile%name = TRIM(carg(2))
    currentFile%type = TRIM(carg(3))

    OSdataLine = TRIM(line)

  CASE( 'EXTRACT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for EXTRACT'
      error%bString = 'ONSITE output file name required'
      GOTO 9999
    END IF

    OSfile = TRIM(carg(2))
    
  CASE( 'QAOUT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for QAOUT'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lUAQ = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'

    OSqaoutLine = TRIM(line)
    
  CASE( 'RANDOM' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for RANDOM'
      error%bString = 'Must specify true or false'
      GOTO 9999
    END IF

    lRandUA = carg(2)(1:1) == 'T' .OR. carg(2)(1:1) == 'Y'
    
    IF( narg > 2 )THEN
      READ(carg(3),*,IOSTAT=ios ) dRanUA
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading angle precision for adding random component'
        error%bString = 'Must specify a positive real number'
        GOTO 9999
      END IF
      IF( dRanUA < 1. )THEN
        error%Number  = IV_ERROR
        error%aString = 'Invalid value for random angle precision'
        error%bString = 'Must specify be 1 or greater'
        GOTO 9999
      END IF
      dRanOS = dRanOS / dRan0
    END IF

  CASE( 'XDATES' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE XDATES'
      error%bString = 'Start and end dates required'
      GOTO 9999
    END IF

    ios = ParseDate( carg(2),yearStartOS,monthStartOS,dayStartOS )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading start date from ONSITE XDATES'
      GOTO 9999
    END IF

    ios = ParseDate( carg(narg),yearEndOS,monthEndOS,dayEndOS )
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading end date from ONSITE XDATES'
      GOTO 9999
    END IF

    OSxdatesLine = TRIM(line)

  CASE( 'LOCATION' )

    IF( .NOT.ASSOCIATED(currentFile) )THEN
      error%Number  = IV_ERROR
      error%aString = 'LOCATION must follow DATA'
      GOTO 9999
    END IF

    IF( narg < 4 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE LOCATION'
      error%bString = 'ID, Lat, Lon required'
      GOTO 9999
    END IF

    currentFile%ID = TRIM(carg(2))

    ios = ParseLocation( carg(3),carg(4),currentFile%lat,currentFile%lon, &
                                        currentFile%clat,currentFile%clon)
    IF( ios /= SUCCESS )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading ONSITE LOCATION lat/lon'
      GOTO 9999
    END IF

    IF( narg > 4 )THEN
      READ(carg(5),*,IOSTAT=ios) currentFile%tAdj
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading ONSITE LOCATION time adjustment'
        GOTO 9999
      END IF
    END IF
 
    IF( narg > 5 )THEN
      READ(carg(6),*,IOSTAT=ios) currentFile%elev
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%aString = 'Error reading ONSITE LOCATION elevation'
        GOTO 9999
      END IF
    END IF
 
    OSlocationLine = TRIM(line)

  CASE( 'READ' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE READ'
      error%bString = 'record index and at least one variable required'
      GOTO 9999
    END IF

    nOSrecord = nOSrecord + 1
    READ(carg(2),*,IOSTAT=ios) nrec
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading ONSITE variable list'
      GOTO 9999
    END IF
    IF( nOSrecord /= nrec )THEN
      error%Number  = IV_ERROR
      error%aString = 'Invalid ONSITE READ record number'
      GOTO 9999
    END IF

    IF( nOSrecord == 1 )THEN
        ALLOCATE( firstOSread%first,STAT=alloc_stat )
        firstOSread%first%line = TRIM(line)
        NULLIFY( firstOSread%first%next )
        OSread => firstOSread%first
        OSread%rec = nOSrecord
    ELSE
        ALLOCATE( OSread%next,STAT=alloc_stat )
        OSread => OSread%next
        NULLIFY( OSread%next )
        OSread%line = TRIM(line)
        OSread%rec = nOSrecord
    END IF

  CASE( 'FORMAT' )

    IF( narg < 3 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE FORMAT'
      error%bString = 'record index and format spec required'
      GOTO 9999
    END IF

    nOSformat = nOSformat + 1
    READ(carg(2),*,IOSTAT=ios) nrec
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%aString = 'Error reading ONSITE FORMAT'
      GOTO 9999
    END IF
    IF( nOSformat /= nrec )THEN
      error%Number  = IV_ERROR
      error%aString = 'Invalid ONSITE format record number'
      GOTO 9999
    END IF

    IF( nOSformat == 1 )THEN
        ALLOCATE( firstOSformat%first,STAT=alloc_stat )
        firstOSformat%first%line = TRIM(line)
        NULLIFY( firstOSformat%first%next )
        OSread => firstOSformat%first
        OSread%rec = nOSformat
    ELSE
        ALLOCATE( OSread%next,STAT=alloc_stat )
        OSread => OSread%next
        NULLIFY( OSread%next )
        OSread%line = TRIM(line)
        OSread%rec = nOSformat
    END IF

  CASE( 'RANGE' )

    IF( narg < 5 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE RANGE'
      error%bString = 'variable name, min / max and missing values required'
      GOTO 9999
    END IF

    nOSrange = nOSrange + 1

    IF( nOSrange == 1 )THEN
        ALLOCATE( firstOSrange%first,STAT=alloc_stat )
        firstOSrange%first%line = TRIM(line)
        NULLIFY( firstOSrange%first%next )
        OSread => firstOSrange%first
        OSread%rec = nOSrange
    ELSE
        ALLOCATE( OSread%next,STAT=alloc_stat )
        OSread => OSread%next
        NULLIFY( OSread%next )
        OSread%line = TRIM(line)
        OSread%rec = nOSrange
    END IF

  CASE( 'OSHEIGHTS' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE OSHEIGHTS'
      error%bString = 'At least one height must be specified'
      GOTO 9999
    END IF

    nOSheight = narg - 1
    OSheightLine = TRIM(line)

  CASE( 'AUDIT' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE AUDIT'
      error%bString = 'At least one variable must be specified'
      GOTO 9999
    END IF

    OSauditLine = TRIM(line)

  CASE( 'NO_MISSING' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE NO_MISSING'
      error%bString = 'At least one variable must be specified'
      GOTO 9999
    END IF

    OSnomissingLine  = TRIM(line)

  CASE( 'OBS/HOUR' )

!**********************************************************
!    error%Number  = IV_ERROR
!    error%aString = 'ONSITE OBS/HOUR keyword not supported'
!    error%bString = 'Data must be reported hourly'
!    GOTO 9999
!**********************************************************

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE OBS/HOUR'
      error%bString = 'Times per hour must be specified'
      GOTO 9999
    END IF

    OSobsperhourLine = TRIM(line)

  CASE( 'THRESHOLD' )

    IF( narg < 2 )THEN
      error%Number  = IV_ERROR
      error%aString = 'Insufficient input specified for ONSITE THRESHOLD'
      error%bString = 'Threshold wind speed must be specified'
      GOTO 9999
    END IF

    OSthresholdLine = TRIM(line)

  CASE DEFAULT
    !Ignore **** needs a message? ****

END SELECT

irv = SUCCESS
error%Routine = ''

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE InitFileList()

!------ Initialize the current file list element

USE met2sci_fi

currentFile%name = 'NotSet'
currentFile%type = 'NotSet'
currentFile%ID   = 'NotSet'
currentFile%landuse  = 'CULTIVATED'  !Default landuse type
currentFile%lat  = -999.
currentFile%lon  = -999.
currentFile%clat = '-999.'
currentFile%clon = '-999.'
currentFile%zref = 10.
currentFile%tAdj = 0
currentFile%elev = -9999.

NULLIFY( currentFile%next )

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseDate( string,year,month,day ) RESULT( irv )

!------ Extract year, month, day from YY/MM/DD string
!       N.B MM and DD may not include leading zero

USE MetSCIparam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: string
INTEGER,      INTENT( OUT ) :: year, month, day

INTEGER i, j

irv = FAILURE

IF( LEN_TRIM(string) < 5 )GOTO 9999

i = INDEX(string,'/')
IF( i < 2 )GOTO 9999

READ(string(1:i-1),*,ERR=9999) year

j = INDEX(string(i+1:),'/')
IF( j < 2 )GOTO 9999

READ(string(i+1:i+j-1),*,ERR=9999) month

i = i + j + 1

READ(string(i:),*,ERR=9999) day

!------ Set two digit year for AERMET compatibility

IF( year > 2000 )THEN
  year = year - 2000
ELSE IF( year > 1900 )THEN
  year = year - 1900
END IF

irv = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseLocation( string1,string2,lat,lon,clat,clon ) RESULT( irv )

USE MetSCIparam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: string1, string2
REAL,         INTENT( OUT ) :: lat, lon
CHARACTER(*), INTENT( OUT ) :: clat, clon

INTEGER i, ios
REAL    x

CHARACTER(1) s

irv = FAILURE

lat  = -999.;   lon  = -999.
clat = '-999.'; clon = '-999.'

i = LEN_TRIM(string1)

READ(string1(1:i-1),*,IOSTAT=ios) x
IF( ios /= 0 )GOTO 9999

s = string1(i:i)

SELECT CASE( s )
  CASE( 'N' )
    lat  = x
    clat = TRIM(string1)
  CASE( 'S' )
    lat  = -x
    clat = TRIM(string1)
  CASE( 'E' )
    lon  = x
    clon = TRIM(string1)
  CASE( 'W' )
    lon  = -x
    clon = TRIM(string1)
  CASE DEFAULT
    GOTO 9999
END SELECT

i = LEN_TRIM(string2)

READ(string2(1:i-1),*,IOSTAT=ios) x
IF( ios /= 0 )GOTO 9999

s = string2(i:i)

SELECT CASE( s )
  CASE( 'N' )
    lat  = x
    clat = TRIM(string2)
  CASE( 'S' )
    lat  = -x
    clat = TRIM(string2)
  CASE( 'E' )
    lon  = x
    clon = TRIM(string2)
  CASE( 'W' )
    lon  = -x
    clon = TRIM(string2)
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( lat == -999. .OR. lon == -999. )GOTO 9999

irv = SUCCESS

9999 CONTINUE

RETURN
END





