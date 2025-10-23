!======================================================================
! ExtractContours
!======================================================================
SUBROUTINE ExtractContours()

USE Extract_fi

IMPLICIT NONE

CALL SelectContours()
IF( nError/= NO_ERROR )GOTO 9999

CALL SelectContourExportMode()
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContours
!======================================================================
SUBROUTINE SelectContours()

USE Extract_fi

IMPLICIT NONE

SELECT CASE( ContourType )
  CASE( AUTO )
    CALL SelectAutoContours()
  CASE( CUSTOM )
    CALL SelectCustomContours()
  CASE( UNIFORM )
    CALL SelectUniformContours()
  CASE DEFAULT
    nError   = UK_ERROR
    eMessage = 'Unrecognized Contour Extraction Type'
END SELECT
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
! SelectAutoContours
!======================================================================
SUBROUTINE SelectAutoContours()

USE Extract_fi

IMPLICIT NONE

contourInput%Cmax = FldMax(1)
contourInput%Cmin = FldMin(1)
contourInput%Del = DEF_VAL_R

contourHead%scale = 1.0

CALL SelectContourGenerationNumber()
IF( nError /= NO_ERROR )GOTO 9999

CALL SelectContourGenerationMode()
IF( nError /= NO_ERROR )GOTO 9999

CALL GenerateContours( Fields(1) )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
! SelectCustomContours
!======================================================================
SUBROUTINE SelectCustomContours()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER irv, ios, itry, i
REAL, DIMENSION(:), ALLOCATABLE :: x
CHARACTER(128) string

INTEGER, EXTERNAL :: parseString
INTEGER, EXTERNAL :: getInput

CALL SelectContourGenerationNumber()
IF( nError /= NO_ERROR )GOTO 9999

contourHead%number = contourInput%number

ALLOCATE( contourList(contourHead%number),STAT=irv )
IF( irv /= 0 )THEN
  nError   = irv
  eroutine = 'SelectCustomContours'
  eMessage = 'Error allocating contour list array'
  GOTO 9999
END IF

CALL SelectContourGenerationScale()
IF( nError /= NO_ERROR )GOTO 9999

ALLOCATE( x(contourHead%number),STAT=irv )
IF( irv /= 0 )THEN
  nError   = irv
  eroutine = 'SelectCustomContours'
  eMessage = 'Error allocating work array'
  GOTO 9999
END IF

IF( iFld > 0 )THEN

  IF( iCnt <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectCustomContours'
    eMessage = 'No contour values specified on command line'
    GOTO 9999
  END IF
  ios = parseString( cmds(iCnt)%cargs,contourHead%number,x )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectCustomContours'
    eMessage = 'Error reading contour values'
    eInform  = 'from argument '//TRIM(cmds(iCnt)%cargs)
    GOTO 9999
  END IF
  DO i = 1,contourHead%number
    contourList(i)%contour = x(i)
    IF( iFld > 0 )&
      contourList(i)%contour = contourList(i)%contour/fScl
  END DO

ELSE

  ios  = 1
  itry = 0
  string = 'contour values'
  DO WHILE( ios > 0 )
    itry = itry + 1
    ios = getInput( string,contourHead%number,x )
    IF( ios > 0 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'Error reading input, try again'
      ELSE
        nError   = UK_ERROR
        eRoutine = 'SelectCustomContours'
        eMessage = 'Error reading '//TRIM(string)
        GOTO 9999
      END IF
    ELSE IF( ios < 0 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'Must specify values, try again'
      ELSE
        nError   = UK_ERROR
        eRoutine = 'SelectCustomContours'
        eMessage = 'No contour values specified'
        GOTO 9999
      END IF
    ELSE
      DO i = 1,contourHead%number
        contourList(i)%contour = x(i)
      END DO
    END IF
  END DO

END IF

9999 CONTINUE

IF( ALLOCATED(x) )DEALLOCATE(x,STAT=irv)

RETURN
END
!======================================================================
! SelectUniformContours
!======================================================================
SUBROUTINE SelectUniformContours()

USE Extract_fi

IMPLICIT NONE

CALL SelectContourGenerationNumber()
IF( nError /= NO_ERROR )GOTO 9999

CALL SelectContourGenerationRange()
IF( nError /= NO_ERROR )GOTO 9999

CALL SelectContourGenerationScale()
IF( nError /= NO_ERROR )GOTO 9999

CALL SelectContourGenerationMode()
IF( nError /= NO_ERROR )GOTO 9999

CALL GenerateContours( Fields(1) )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourGenerationMode
!======================================================================
SUBROUTINE SelectContourGenerationMode()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER itry, ios, i, input

CHARACTER(4) string

WRITE(6,'(A)')'Available Contour Generation Modes'
WRITE(6,'(A)')'===================================='
DO i = 1,NUM_GEN_MODE
  WRITE(6,'(I3,1X,A)')i,TRIM(GEN_MODES(i))
END DO

itry = 0

loop: DO
  itry = itry + 1

  IF( UseKey )THEN
    WRITE(6,'(/,"Contour generation mode Keyword? : ",$)')
  ELSE
    WRITE(6,'(/,"Contour generation mode Number? : ",$)')
  END IF

  IF( iFld > 0 )THEN
    string = 'AU'
  ELSE
    READ(lun_in,'(A)',IOSTAT=ios)string
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationMode'
      eMessage = 'Error reading Contour generation mode'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    CALL cupper( string )
  END IF

  IF( UseKey )THEN
    string = ADJUSTL(string)
    SELECT CASE( string(1:2) )
      CASE( 'AU' )
        input = 1
        EXIT loop
      CASE( 'LO' )
        input = 2
        EXIT loop
      CASE( 'LI' )
        input = 3
        EXIT loop
      CASE DEFAULT
        IF( itry < maxTry )THEN
          WRITE(6,'(/,(A))')'Invalid Contour generation mode : '//TRIM(string)//'. Try again'
          CYCLE
        ELSE
          nError   = UK_ERROR
          eRoutine = 'SelectContourGenerationMode'
          eMessage = 'Invalid Contour generation mode method : '//TRIM(string)
          GOTO 9999
        END IF
    END SELECT
  ELSE
    string = ADJUSTR(string)
    READ(string,*,IOSTAT=ios)input
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationMode'
      eMessage = 'Error getting Contour generation mode number from input'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    IF( input < 1 .OR. input > NUM_GEN_MODE )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(/,(A))')'Invalid Contour generation mode number : '//TRIM(string)//'. Try again'
        CYCLE
      ELSE
        nError   = UK_ERROR
        eRoutine = 'SelectContourGenerationMode'
        eMessage = 'Invalid Contour generation mode number : '//TRIM(string)
        GOTO 9999
      END IF
    ELSE
      EXIT loop
    END IF
  END IF
END DO loop

WRITE(6,'(/,A)')'Using Contour generation mode '//TRIM(GEN_MODES(input))

SELECT CASE( input )
  CASE( 1 )
    contourInput%mode = PLOT_DEF
  CASE( 2 )
    contourInput%mode = PLOT_LOG
  CASE( 3 )
    contourInput%mode = PLOT_LIN
  CASE DEFAULT
    contourInput%mode = PLOT_DEF
END SELECT

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourGenerationNumber
!======================================================================
SUBROUTINE SelectContourGenerationNumber()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER itry, ios, input

CHARACTER(8) string

itry = 0

loop: DO
  WRITE(6,'(/,A,$)')'Desired number of contours? '
  IF( iFld > 0 )THEN
    IF( iCnt > 0 )THEN
      WRITE(string,'(I3)')cmds(iCnt)%narg
    ELSE IF ( TRIM(fld%fclass) == 'Terrain' )THEN
      string = '10'
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationNumber'
      eMessage = 'Error reading desired number of contours'
      eInform  = 'Must provide contours levels as command line argument'
      EXIT loop
    END IF
  ELSE
    READ(lun_in,'(A)',IOSTAT=ios)string
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationNumber'
      eMessage = 'Error reading Contour generation mode'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
  END IF

  itry = itry + 1
  string = ADJUSTR(string)
  READ(string,*,IOSTAT=ios)input
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectContourGenerationNumber'
    eMessage = 'Error getting Contour generation mode number from input'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF
  IF( input < 1 .OR. input > 25 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(/,(A))')'Invalid Contour generation mode number (n=[1,25]): '//TRIM(string)//'. Try again'
      CYCLE
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationNumber'
      eMessage = 'Invalid Contour generation mode number : '//TRIM(string)
      GOTO 9999
    END IF
  ELSE
    EXIT loop
  END IF
END DO loop

IF( nError == NO_ERROR )&
  WRITE(6,'(/,A,I0)')'Using Contour generation number ',input

contourInput%number = input

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourGenerationRange
!======================================================================
SUBROUTINE SelectContourGenerationRange()

USE Extract_fi

IMPLICIT NONE

INTEGER itry, ios
REAL    c(2)
CHARACTER(128) string

INTEGER, EXTERNAL :: getInput

ios  = 1
itry = 0
string = 'contour range (min max)'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,2,c )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationRange'
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    contourInput%Cmax = FldMax(1)
    contourInput%Cmin = FldMin(1)
  ELSE
    contourInput%Cmax = c(2)
    contourInput%Cmin = c(1)
  END IF
END DO

WRITE(6,'(/,A,2F0.5)')'Using contour range ',contourInput%Cmin,contourInput%Cmax

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourGenerationIncrement
!======================================================================
SUBROUTINE SelectContourGenerationIncrement()

USE Extract_fi

IMPLICIT NONE

INTEGER itry, ios
REAL    c(2)
CHARACTER(128) string

INTEGER, EXTERNAL :: getInput

ios  = 1
itry = 0
string = 'contour increment'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,c )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationIncrement'
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    contourInput%Del = DEF_VAL_R
  ELSE
    contourInput%Del = c(1)
  END IF
END DO

WRITE(6,'(/,A,2F0.5)')'Using contour increment ',contourInput%Del

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourGenerationScale
!======================================================================
SUBROUTINE SelectContourGenerationScale()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER itry, ios
REAL    c(2)
CHARACTER(128) string

INTEGER, EXTERNAL :: getInput

ios  = 1
itry = 0
IF( iFld > 0 )THEN
  IF( iScl > 0 )THEN
    narg = 0
    CALL SplitString( cmds(iScl)%cargs,',',narg,cargs,lerr )
    IF( narg > 1 )THEN
      READ(cargs(1),*,IOSTAT=ios)fScl
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'SelectContourGenerationScale'
        eMessage = 'Error reading scale factor from command line arguments '//TRIM(cargs(1))
        GOTO 9999
      END IF
      IF( narg == 2 )contourHead%unit = TRIM(cargs(2))
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectContourGenerationScale'
      eInform  = 'Scale factor must be provided on command line'
      eMessage = 'Error reading scale factor from command line arguments'//TRIM(cmds(iScl)%cargs)
      GOTO 9999
    END IF
  END IF
  contourHead%scale = fScl
ELSE
  string = 'contour scale factor'
  DO WHILE( ios > 0 )
    itry = itry + 1
    ios = getInput( string,1,c )
    IF( ios > 0 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'Error reading input, try again'
      ELSE
        nError   = UK_ERROR
        eRoutine = 'SelectContourGenerationScale'
        eMessage = 'Error reading '//TRIM(string)
        GOTO 9999
      END IF
    ELSE IF( ios < 0 )THEN
      contourHead%scale = 1.0
    ELSE
      contourHead%scale = c(1)
    END IF
  END DO

  WRITE(6,'(/,A,2F0.5)')'Using contour Scale ',contourHead%scale
END IF

9999 CONTINUE

RETURN
END
!======================================================================
! SelectContourExportMode
!======================================================================
SUBROUTINE SelectContourExportMode()

USE tooluser_fd
USE write_fd
USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER itry, ios, i, input

CHARACTER(4) string

WRITE(6,'(A)')'Available Contour Export Modes'
WRITE(6,'(A)')'===================================='
DO i = 1,NUM_EXPORT
  WRITE(6,'(I3,1x,A)')i,TRIM(EXPORTS(i))
END DO

itry = 0

loop: DO
  itry = itry + 1

  IF( UseKey )THEN
    WRITE(6,'(/,"Contour export mode Keyword? : ",$)')
  ELSE
    WRITE(6,'(/,"Contour export mode Number? : ",$)')
  END IF

  IF( iFld > 0 )THEN
    string = 'OVL'
  ELSE
    READ(lun_in,'(A)',IOSTAT=ios)string
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SelectContourExportMode'
      eMessage = 'Error reading Contour export mode'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    CALL cupper( string )
  END IF

  IF( UseKey )THEN
    string = ADJUSTL(string)
    SELECT CASE( string(1:3) )
      CASE( 'OVL' )
        input = 1
        EXIT loop
      CASE( 'USA' )
        input = 2
        EXIT loop
      CASE( 'OIL' )
        input = 3
        EXIT loop
      CASE( 'EIS' )
        input = 4
        EXIT loop
      CASE( 'CTS' )
        input = 5
        EXIT loop
      CASE DEFAULT
        IF( itry < maxTry )THEN
          WRITE(6,'(/,(A))')'Invalid Contour export mode : '//TRIM(string)//'. Try again'
          CYCLE
        ELSE
          nError   = UK_ERROR
          eRoutine = 'SelectContourExportMode'
          eMessage = 'Invalid Contour export mode method : '//TRIM(string)
          GOTO 9999
        END IF
    END SELECT
  ELSE
    string = ADJUSTR(string)
    READ(string,*,IOSTAT=ios)input
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SelectContourExportMode'
      eMessage = 'Error getting Contour export mode number from input'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    IF( input < 1 .OR. input > NUM_GEN_MODE )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(/,(A))')'Invalid Contour export mode number : '//TRIM(string)//'. Try again'
        CYCLE
      ELSE
        nError   = UK_ERROR
        eRoutine = 'SelectContourExportMode'
        eMessage = 'Invalid Contour export mode number : '//TRIM(string)
        GOTO 9999
      END IF
    ELSE
      EXIT loop
    END IF
  END IF
END DO loop

WRITE(6,'(/,A)')'Using Contour export mode '//TRIM(EXPORTS(input))

SELECT CASE( input )
  CASE( 1 )
    ContourWrite%mode = OVL_FILE
    contourExt = '.ovl'
  CASE( 2 )
    ContourWrite%mode = USA_FILE
    contourExt = '.usa'
  CASE( 3 )
    ContourWrite%mode = OIL_FILE
    contourExt = '.oil'
  CASE( 4 )
    ContourWrite%mode = EIS_FILE
    contourExt = '.eis'
  CASE( 5 )
    ContourWrite%mode = CTS_FILE
    contourExt = '.cts'
  CASE DEFAULT
    ContourWrite%mode = OVL_FILE
    contourExt = '.ovl'
END SELECT

9999 CONTINUE

RETURN
END
!******************************************************************************
!******************************************************************************
!  GenerateContours - Modified from pltchoice.f90 (ARAPgui)
!******************************************************************************
!******************************************************************************
SUBROUTINE GenerateContours( thisField )

USE Extract_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ) :: thisField

REAL,    PARAMETER :: SMALL         = 1.E-30
INTEGER, PARAMETER :: SUGGESTED_MAX = 25

INTEGER ios

REAL cMin
REAL cMax
REAL cInc
REAL p
REAL rInc

INTEGER nc
INTEGER i
INTEGER mode
LOGICAL useNC

mode = contourInput%Mode
IF( mode == PLOT_DEF )THEN
  IF( CatClassArray(thisField%Category,thisField%Class)%type == SCIPtrue )THEN
    IF( PlotType%type == HP_PROB )THEN
       mode = PLOT_LIN
    ELSE
      IF( thisField%interpType == SCIPon )THEN
         mode = PLOT_LIN
      ELSE
         mode = PLOT_LOG
      END IF
    END IF
  ELSE
    IF( thisField%interpType == SCIPon )THEN
       mode = PLOT_LIN
    ELSE
       mode = PLOT_LOG
    END IF
  END IF
END IF

!==============================================================================
!Set Approximate number of contours
!==============================================================================

nc = MAX(1,ABS(contourInput%Number))

useNC = contourInput%Number < 0 .AND. mode == PLOT_LOG

!==============================================================================
!Set Min/Max
!==============================================================================

cMin = contourInput%Cmin
cMax = contourInput%Cmax

!==============================================================================
!Check Min/Max - Error if not set
!==============================================================================

IF( cMax == DEF_VAL_R  .OR. cMax == NOT_SET_R )THEN
  nError   = UK_ERROR
  eRoutine = 'GenerateContours'
  eMessage = 'In Contour range maximum value not set'
  GOTO 1000
END IF

IF( cMin == DEF_VAL_R  .OR. cMin == NOT_SET_R )THEN
  nError   = UK_ERROR
  eRoutine = 'GenerateContours'
  eMessage = 'In Contour range minimum value not set'
  GOTO 1000
END IF

!==============================================================================
!Swap Min/Max if necessary
!==============================================================================

IF( cMin > cMax )THEN
  cMin = contourInput%Cmax
  cMax = contourInput%Cmin
END IF

!==============================================================================
!Check for constant value
!==============================================================================

IF( ABS(cMax-cMin)/MAX(cMax,cMin,SMALL) <= EPSILON(SMALL) )GOTO 3000

!==============================================================================
!Take log if log spacing
!==============================================================================

IF( mode == PLOT_LOG )THEN
  cMax = LOG10( MAX( cMax,SMALL ) )
  cMin = LOG10( MAX( cMin,SMALL ) )
END IF

!==============================================================================
!Set Inc - Based on approximate number of contours
!==============================================================================

IF( contourInput%Del == DEF_VAL_R  .OR. contourInput%Del == NOT_SET_R .OR. useNC )THEN

!==============================================================================
! Limit cMin for log spacing
!==============================================================================

  IF( mode == PLOT_LOG )THEN
    IF( contourInput%Del == DEF_VAL_R  .OR. contourInput%Del == NOT_SET_R )THEN
      rInc = 1.0
    ELSE
      rInc = LOG10(contourInput%Del)
    END IF
    cMin = MAX( cMin,cMax-rInc*FLOAT(nc) )
  END IF

!==============================================================================
! Compute Inc
!==============================================================================

  cInc = (cMax - cMin)/FLOAT(nc)
  IF( ABS(cInc) < SMALL )GOTO 3000

!==============================================================================
! Make Inc "nice"
!==============================================================================

  IF( mode == PLOT_LOG )THEN
    IF( cInc >= 1.0 )THEN
      cInc = ANINT(cInc)
    ELSE
      cInc = 1./ANINT(1./cInc)
    END IF
  ELSE
    p = 10.**(IFIX(LOG10(cInc)+5000.)-5000)
    cInc = MAX(1.,AINT(cInc/p))*p
  END IF

!==============================================================================
!Set Inc - User specified
!==============================================================================

ELSE

  cInc = ABS(contourInput%Del)
  IF( mode == PLOT_LOG )THEN
    cInc = LOG10( MAX( cInc, SMALL ) )
  ELSE
    cInc = cInc
  END IF

END IF

!==============================================================================
!Adjust Min/Max based on Inc
!==============================================================================

IF( cMax < 0.0 )THEN
  cMax = (AINT((cMax+0.1*cInc)/cInc)-1.)*cInc
ELSE IF( cMax > 0.0 )THEN
  cMax = (AINT((cMax-0.1*cInc)/cInc)+1.)*cInc
END IF
cMax = cMax*(1.+SIGN(1.e-6,cMax))

IF( cMin < 0.0 )THEN
  cMin = (AINT((cMin+0.1*cInc)/cInc)-1.)*cInc
ELSE IF( cMin > 0.0 )THEN
  cMin = (AINT((cMin-0.1*cInc)/cInc)+1.)*cInc
END IF

!==============================================================================
!Compute number of contours
!==============================================================================

nc = NINT( (cMax - cMin - 0.1*cInc)/cInc ) + 1

!==============================================================================
!Warn if nc > SUGGESTED_MAX
!==============================================================================

IF( nc >= SUGGESTED_MAX )THEN
  nError   = SZ_ERROR
  eRoutine = 'GenerateContours'
  eMessage = 'Geneated too many contours. Current max=25'
  WRITE(eInform,'(A,I0)')'Number requested =',nc
  GOTO 1000
END IF

!==============================================================================
!Allocate space and Compute contours
!==============================================================================

ALLOCATE( contourList(nc),STAT=ios )
IF( ios /= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'GenerateContours'
  eMessage = 'Error allocation contour list'
  WRITE(eInform,'(A,I0,A,I0)')'Number requested =',nc,' : Error=',ios
  GOTO 1000
END IF

contourHead%number = nc
DO i = 1,contourHead%number
  contourList(i)%Contour = (cMin + FLOAT(i-1)*cInc)
END DO

!==============================================================================
!Set return value to SUCCESS
!==============================================================================

2000 CONTINUE

!==============================================================================
!Log spacing - transform back to real numbers
!==============================================================================

IF( mode == PLOT_LOG )THEN
  DO i = 1,contourHead%number
    contourList(i)%contour = (10**contourList(i)%contour)
  END DO
ELSE
  DO i = 1,contourHead%number
    contourList(i)%contour = contourList(i)%contour
  END DO
END IF

1000 CONTINUE

RETURN

!==============================================================================
!Single contour
!==============================================================================

3000 CONTINUE

ALLOCATE( contourList(1),STAT=ios )
IF(ios /= 0)THEN
  nError   = SZ_ERROR
  eRoutine = 'GenerateContours'
  eMessage = 'Error allocation contour list'
  WRITE(eInform,'(A,I0)')'Number requested = 1 : Error=',ios
  GOTO 1000
END IF
contourHead%number     = 1
contourList(1)%contour = contourInput%Cmax
IF( mode == PLOT_LOG )THEN
  contourList(1)%contour = LOG10( MAX( contourList(1)%contour, SMALL ) )
END IF

GOTO 2000

END
