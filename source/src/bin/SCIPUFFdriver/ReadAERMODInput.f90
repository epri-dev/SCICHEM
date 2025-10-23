INTEGER FUNCTION ReadAERMODInput()

!----- Read AERMOD input file; set SCIPUFF defaults as appropriate

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER irv, ios

CHARACTER(2) pathway

INTEGER, EXTERNAL :: ReadPathway

ReadAERMODInput = FAILURE

!----- Read down file looking for new pathway sections

DO

  line = ' '
  CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
  IF( lerr )EXIT !ASSUMED to be EOF error

!----- Skip comments (get_next_data skips blank lines)

  IF( carg(1)(1:1) == '*' )CYCLE

!----- Check pathway

  IF( narg /= 2 )THEN
    WRITE(*,'(A)') 'Error reading input file'
    WRITE(*,'(A)') 'Not a proper pathway starting record'
    GOTO 9999
  ELSE
    CALL cupper( carg(2) )
    IF( TRIM(carg(2)) /= 'STARTING' )THEN
      WRITE(*,'(A)') 'Error reading input file'
      WRITE(*,'(A)') 'Not a proper starting record for pathway '//TRIM(carg(1))
      GOTO 9999
    END IF
  END IF

  CALL cupper( carg(1) )

  pathway = carg(1)(1:2)

  irv = ReadPathway( pathway )
  IF( irv /= SUCCESS )GOTO 9999

END DO

ReadAERMODInput = SUCCESS

9999 CONTINUE

CLOSE(lun,IOSTAT=ios)
IF( lPRIME .AND. lPrimeFile )CLOSE(lun_pri,IOSTAT=ios)

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadPathway( MyPathway )

USE SCIPUFFdriver_fi

IMPLICIT NONE

CHARACTER(2), INTENT( IN ) :: MyPathway

INTEGER irv

INTEGER, EXTERNAL :: ParseDataCO
INTEGER, EXTERNAL :: ParseDataMA !Special SCIPUFF material input
INTEGER, EXTERNAL :: ParseDataSO
INTEGER, EXTERNAL :: ParseDataME
INTEGER, EXTERNAL :: ParseDataTE
INTEGER, EXTERNAL :: ParseDataRE
INTEGER, EXTERNAL :: ParseDataEV
INTEGER, EXTERNAL :: ParseDataOU

ReadPathway = FAILURE

!----- Read until FINISHED

DO
  line = ' '

  IF( lIncluded )THEN
    CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,MAXN,lerr )
    IF( lerr )THEN    !Assume error is from EOF
      CLOSE(UNIT=lun_tmp,IOSTAT=irv)
      lIncluded = .FALSE.
      CYCLE
    END IF
  ELSE
    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
  END IF
  IF( lerr )THEN
    WRITE(*,'(A)') 'Error reading '//MyPathway
    GOTO 9999
  END IF

!----- Skip comments (get_next_data already skips blank lines)

  IF( carg(1)(1:1) == '*' )CYCLE

!----- Check for pathway as first argument; set keyword index accordingly

  CALL CUPPER( carg(1) )

  IF( TRIM(carg(1)) == MyPathway )THEN
    ikwrd = 2
  ELSE
    ikwrd = 1
  END IF

  SELECT CASE( MyPathway )
    CASE( 'CO' )
      irv = ParseDataCO()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'MA' )
      irv = ParseDataMA()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'SO' )
      irv = ParseDataSO()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'ME' )
      irv = ParseDataME()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'TE' )
      irv = ParseDataTE()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'RE' )
      irv = ParseDataRE()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'EV' )
      irv = ParseDataEV()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( 'OU' )
      irv = ParseDataOU()
      IF( irv /= SUCCESS )GOTO 9999

  END SELECT

  IF( TRIM(carg(2)) == 'FINISHED' )EXIT

END DO

ReadPathway = SUCCESS

9999 CONTINUE

RETURN
END

