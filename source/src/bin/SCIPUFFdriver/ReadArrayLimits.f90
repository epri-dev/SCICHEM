INTEGER FUNCTION ReadArrayLimits()

!----- Read SCIPUFF array limits (if given)

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: ReadPathwaySC

ReadArrayLimits = FAILURE

!----- Read down file looking for CO pathway

DO

  line = ' '
  CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
  IF( lerr )EXIT !ASSUMED to be EOF error

!----- Skip comments (get_next_data skips blank lines)

  IF( carg(1)(1:1) == '*' )CYCLE  !Skip comments

  IF( TRIM(carg(1)) == 'CO' )THEN
    irv = ReadPathwaySC()
    IF( irv /= SUCCESS )GOTO 9999
    EXIT

  ELSE
    CYCLE

  END IF

END DO

ReadArrayLimits = SUCCESS

REWIND(lun,IOSTAT=irv)

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadPathwaySC()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: ParseDataSC !Special SCIPUFF array limits pathway

ReadPathwaySC = FAILURE

!----- Read until FINISHED

DO
  line = ' '
  CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
  IF( lerr )THEN
    WRITE(*,'(A)') 'Error reading CO Pathway'
    GOTO 9999
  END IF

  IF( carg(1)(1:1) == '*' )CYCLE !Skip comments

  ikwrd = 1  !Index into carg for keyword

!----- Check for pathway as first argument; set keyword index accordingly

  CALL CUPPER( carg(1) )

  IF( TRIM(carg(1)) == 'CO' )THEN
    ikwrd = 2
  ELSE
    ikwrd = 1
  END IF

  irv = ParseDataSC()
  IF( irv /= SUCCESS )GOTO 9999

  IF( TRIM(carg(2)) == 'FINISHED' )EXIT

END DO

ReadPathwaySC = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseDataSC()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios

ParseDataSC = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'MAXPUFFS' )

    READ(carg(ikwrd+1),*,IOSTAT=ios )limit%puffs
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO MAXPUFFS'
      GOTO 9999
    END IF

  CASE( 'MAXMET1D' )

    READ(carg(ikwrd+1),*,IOSTAT=ios )limit%met1D
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO MAXMET1D'
      GOTO 9999
    END IF

  CASE( 'MAXSURFG' )

    READ(carg(ikwrd+1),*,IOSTAT=ios )limit%surfaceGrid
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO MAXSURFG'
      GOTO 9999
    END IF

  CASE( 'FINISHED' )

  CASE DEFAULT
    !Ignore other keywords

END SELECT

ParseDataSC = SUCCESS

9999 CONTINUE

RETURN
END


