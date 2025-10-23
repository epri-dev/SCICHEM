!===============================================================================
!     ReadDomain
!===============================================================================
SUBROUTINE ReadDomain( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( In ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadDomain'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistDomain( lunit )
IF( nError /= NO_ERROR )GOTO 9998

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteDomain
!===============================================================================
SUBROUTINE WriteDomain( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( In ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteDomain'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistDomain( lunit )
IF( nError /= NO_ERROR )GOTO 9998

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     ReadNamelistDomain
!===============================================================================
SUBROUTINE ReadNamelistDomain( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios
CHARACTER(80) cmap

INTEGER, EXTERNAL :: FindNML

INTEGER, DIMENSION(DOMAIN_STATUS_ARRAY_SIZE)  :: domain_status   !Compatibility with OPVIEW

NAMELIST / domain  / cmap,xmin,xmax,ymin,ymax,zmax,vres,hres &
                    ,utm_zone,xref,yref,lon0,lat0, domain_status

SELECT CASE( lmap )
  CASE( I_LATLON )
    cmap = 'LATLON'

  CASE( I_UTM )
    cmap = 'UTM'

  CASE( I_CARTESIAN )
    cmap  ='CARTESIAN'

  CASE( I_METERS )
    cmap = 'METERS'

  CASE DEFAULT
    cmap = 'LATLON'

END SELECT

ios = FindNML( iunit,'domain' )

IF( ios == 0 )READ(UNIT=iunit,NML=domain,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistDomain'
  eMessage = 'Error reading DOMAIN namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistDomain'
  eMessage = 'EOF reading DOMAIN namelist'
  GOTO 9999
END IF

CALL cupper( cmap )

SELECT CASE( TRIM(cmap) )
  CASE( 'LATLON' )
    lmap = I_LATLON

  CASE('UTM' )
    lmap = I_UTM

  CASE( 'CARTESIAN' )
    lmap = I_CARTESIAN

  CASE( 'METERS' )
    lmap = I_METERS

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'ReadNamelistDomain'
    eMessage = 'Unknown map function CMAP'
    eAction  = 'Must be CARTESIAN, METERS, LATLON, or UTM'
    GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistDomain
!===============================================================================
SUBROUTINE WriteNamelistDomain( iunit)

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

CHARACTER(80) cmap

NAMELIST / domain  / cmap,xmin,xmax,ymin,ymax,zmax,vres,hres &
                    ,utm_zone,xref,yref,lon0,lat0

SELECT CASE( lmap )
  CASE( I_LATLON )
    cmap = 'LATLON'

  CASE( I_UTM )
    cmap = 'UTM'

  CASE( I_CARTESIAN )
    cmap = 'CARTESIAN'

  CASE( I_METERS )
    cmap = 'METERS'

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'WriteNamelistDomain'
    eMessage = 'Unknown map function CMAP'
    eAction  = 'Must be CARTESIAN, METERS, LATLON, or UTM'
    GOTO 9999

END SELECT

WRITE(UNIT=iunit,NML=domain,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine ='WriteNamelistDomain'
  eMessage ='Error writing DOMAIN namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
