!==============================================================================
! setPlotClassData
!==============================================================================
SUBROUTINE setPlotClassData( doMet,doInput )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

LOGICAL :: doMet
LOGICAL :: doInput

INTEGER ios
INTEGER mClassData, nClassData

INTEGER, EXTERNAL :: NumClassData

IF( doMet )THEN
  IF ( MetField%class > 0 )THEN
    mClassData = NumClassData( MetField )

    IF( ALLOCATED(MetClassData) )DEALLOCATE( MetClassData,STAT=ios )
    ALLOCATE( MetClassData(MAX(mClassData,1)),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'setPlotClassData'
      eMessage = 'Error  : allocating MetClassData array'
      WRITE(eInform,'(A,I0,A,I0)')'Request=',mClassData,' : error =',ios
      GOTO 9999
    END IF

    CALL FillClassData( MetField,mClassData,MetClassData )
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    nError   = UK_ERROR
    eRoutine = 'setPlotClassData'
    eMessage = 'Error setting MetClassData array. MetField%class not set'
    GOTO 9999
  END IF
END IF

IF( Field%class > 0 )THEN
  nClassData = NumClassData( Field )

  IF( ALLOCATED(ClassData) )DEALLOCATE( ClassData,STAT=ios )
  ALLOCATE( ClassData(MAX(nClassData,1)),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'setPlotClassData'
    eMessage = 'Error  : allocating classData array'
    WRITE(eInform,'(A,I0,A,I0)')'Request=',nClassData,' : error =',ios
    GOTO 9999
  END IF

  IF( doInput .AND. nClassData > 0 )THEN
    CALL inputClassData( Field )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  CALL FillClassData( Field,nClassData,ClassData )
  IF( nError /= NO_ERROR )GOTO 9999
ELSE
  nError   = UK_ERROR
  eRoutine = 'setPlotClassData'
  eMessage = 'Error setting ClassData array. Field%class not set'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
! NumClassData
!==============================================================================
!==============================================================================
INTEGER FUNCTION NumClassData( inField )

USE Extract_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT (IN ) :: inField

INTEGER nClassData
TYPE( char64T ) CurClass,CurKind,CurChoice
INTEGER         CurCat

!==============================================================================
! Initialize
!==============================================================================
nClassData = 0

!==============================================================================
! Category data
!==============================================================================
SELECT CASE( inField%Category )
  CASE( HP_HSLICE )
    nClassData = nClassData + CD_NUM_HSLICE
  CASE( HP_VSLICE,HP_HINT )
    nClassData = nClassData + CD_NUM_VSLICE
  CASE( HP_VINT,HP_SSLICE )
    nClassData = nClassData + CD_NUM_VINT
  CASE( HP_TABLE )
    nClassData = nClassData + CD_NUM_TABLE
  CASE DEFAULT
  END SELECT

!==============================================================================
! Source estimation data
!==============================================================================
CurClass  = ClassStr(inField%Class)
IF( INDEX(CurClass%string,'Source Estimation') /= 0 )THEN
  nClassData = nClassData + CD_NUM_ADJ_DOMAIN
  CurChoice = ChoiceStr(inField%Choice)
  IF( INDEX(CurChoice%string,'Max') /= 0 )nClassData = nClassData + CD_NUM_ADJ_TIME
END IF

!==============================================================================
! Return
!==============================================================================

NumClassData = nClassData

RETURN
END
!==============================================================================
!==============================================================================
! FillClassData
!==============================================================================
!==============================================================================
SUBROUTINE FillClassData( inField,mClassData,xClassData )

USE Extract_fi
USE cmd_fi


IMPLICIT NONE

TYPE( SCIPPlotFieldT ),                INTENT (IN ) :: inField
INTEGER               ,                INTENT (IN ) :: mClassData
REAL, DIMENSION( MAX(mClassData,1) ) , INTENT (OUT) :: xClassData

INTEGER nClassData
INTEGER i
TYPE( char64T ) CurClass, CurChoice
REAL    rounding, risk

!==============================================================================
! Initialize
!==============================================================================
nClassData = 0
IF( sliceXmin == DEF_VAL_R .OR. sliceXmin == NOT_SET_R )THEN
  xMin = DEF_VAL_R
ELSE
  xMin = sliceXmin
END IF

IF( sliceXmax == DEF_VAL_R .OR. sliceXmax == NOT_SET_R )THEN
  xMax = DEF_VAL_R
ELSE
  xMax = sliceXmax
END IF

IF( sliceYmin == DEF_VAL_R .OR. sliceYmin == NOT_SET_R )THEN
  yMin = DEF_VAL_R
ELSE
  yMin = sliceYmin
END IF

IF( sliceYmax == DEF_VAL_R .OR. sliceYmax == NOT_SET_R )THEN
  yMax = DEF_VAL_R
ELSE
  yMax = sliceYmax
END IF

IF( sliceZmin == DEF_VAL_R .OR. sliceZmin == NOT_SET_R )THEN
  zMin = 0.
ELSE
  zMin = sliceZmin
END IF

IF( sliceZmax == DEF_VAL_R .OR. sliceZmax == NOT_SET_R )THEN
  zMax = 2500.
ELSE
  zMax = sliceZmax
END IF

IF( riskLevel == DEF_VAL_R .OR. riskLevel == NOT_SET_R )THEN
  risk = 10.0
ELSE
  risk = riskLevel
END IF

rounding = FLOAT(iRound)

!==============================================================================
! Check for empty array
!==============================================================================
IF( mClassData == 0 )THEN
  xClassData = 0.0

!==============================================================================
! Fill data array
!==============================================================================
ELSE

!==============================================================================
! Category data
!==============================================================================
  SELECT CASE( inField%Category )
    CASE( HP_VSLICE,HP_HINT )
      nClassData = nClassData + CD_NUM_VSLICE
      xClassData(CD_XMIN) = xMin
      xClassData(CD_XMAX) = xMax
      xClassData(CD_YMIN) = yMin
      xClassData(CD_YMAX) = yMax
      xClassData(CD_ZMIN) = zMin
      xClassData(CD_ZMAX) = zMax
      xClassData(CD_VRES) = sliceNz
    CASE( HP_HSLICE )
      nClassData = nClassData + CD_NUM_HSLICE
      xClassData(CD_XMIN) = xMin
      xClassData(CD_XMAX) = xMax
      xClassData(CD_YMIN) = yMin
      xClassData(CD_YMAX) = yMax
      xClassData(CD_ZMIN) = sliceHeight
    CASE( HP_VINT,HP_SSLICE )
      nClassData = nClassData + CD_NUM_VINT
      xClassData(CD_XMIN) = xMin
      xClassData(CD_XMAX) = xMax
      xClassData(CD_YMIN) = yMin
      xClassData(CD_YMAX) = yMax
    CASE( HP_TABLE )
      nClassData = nClassData + CD_NUM_TABLE
      xClassData(CD_RISK)  = risk
      xClassData(CD_ROUND) = rounding
    CASE DEFAULT
  END SELECT

!==============================================================================
! Source estimation data
!==============================================================================
  CurClass  = ClassStr(inField%Class)
  IF( INDEX(CurClass%string,'Source Estimation') /= 0 )THEN
    DO i = 1,CD_NUM_ADJ_DOMAIN
      xClassData(nClassData+i) = DEF_VAL_R
    END DO
!    ClassData(nClassData+1) = -2.38   !XminMask
!    ClassData(nClassData+2) = -2.32   !XmaxMask
!    ClassData(nClassData+3) = 48.12   !YminMask
!    ClassData(nClassData+4) = 48.18   !YmaxMask
    nClassData = nClassData + CD_NUM_ADJ_DOMAIN
    CurChoice = ChoiceStr(inField%Choice)
    IF( INDEX(CurChoice%string,'Max') /= 0 )THEN
      DO i = 1,CD_NUM_ADJ_TIME
        xClassData(nClassData+i) = DEF_VAL_R
      END DO
      nClassData = nClassData + CD_NUM_ADJ_TIME
    END IF
  END IF

END IF

!==============================================================================
! Return
!==============================================================================
IF( nClassData /= mClassData )THEN
  WRITE(eInform,*)'Requested number =',mClassData,'  :  Number set =',nClassData
  WRITE(eMessage,*)'Error setting class data array', &
                 TRIM(eInform),' ','SetClassData'
  nError = UK_ERROR
END IF

RETURN
END
!==============================================================================
!==============================================================================
! inputClassData
!==============================================================================
!==============================================================================
SUBROUTINE inputClassData( inField )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT (IN ) :: inField

INTEGER, EXTERNAL :: parseString
INTEGER, EXTERNAL :: getInput

INTEGER itry, ios
REAL    x(2)
CHARACTER(120) string

!==============================================================================
! Category data
!==============================================================================
WRITE(6,*)
SELECT CASE( inField%Category )
  CASE( HP_VSLICE,HP_HINT )
   IF( iFld > 0 )THEN
      IF( Vslc > 0 .OR. Hpro > 0 )THEN
        IF( Vslc > 0 )THEN
          cmd = TRIM(cmds(Vslc)%cargs)
        ELSE
          cmd = TRIM(cmds(Hpro)%cargs)
        END IF
        sliceZmin = DEF_VAL_R
        sliceZmax = DEF_VAL_R
        sliceNz   = 100
        narg = 0
        IF( LEN_TRIM(cmd) > 0 )CALL SplitString( cmd,',',narg,cargs,lerr )
        IF( narg >= 4 .AND. .NOT.lerr )THEN
          ! Slice lower, left point
          READ( cargs(1),*,IOSTAT=ios )sliceXmin
          IF( ios == 0 )READ( cargs(2),*,IOSTAT=ios )sliceYmin
          ! Slice upper, right point
          IF( ios == 0 )READ( cargs(3),*,IOSTAT=ios )sliceXmax
          IF( ios == 0 )READ( cargs(4),*,IOSTAT=ios )sliceYmax
          IF( narg >= 6 )THEN
            IF( ios == 0 )READ( cargs(5),*,IOSTAT=ios )sliceZmin
            IF( ios == 0 )READ( cargs(6),*,IOSTAT=ios )sliceZmax
            IF( narg > 6 )THEN
              IF( ios == 0 )READ( cargs(7),*,IOSTAT=ios )sliceNZ
            END IF
          END IF
        ELSE
          sliceXmin = DEF_VAL_R
          sliceYmin = DEF_VAL_R
          sliceXmax = DEF_VAL_R
          sliceYmax = DEF_VAL_R
        END IF
      ELSE
        nError = UK_ERROR
      END IF
      IF( nError == UK_ERROR .OR. ios /= 0 )THEN
        eMessage = 'Error reading horizontal slice arguments [xMin,yMin,Xmax,yMax,]ZSlice'
        GOTO 9999
      END IF
   ELSE
      !== Slice Start Point
      ios  = 1
      itry = 0
      string = 'Slice starting point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmin = x(1)
          sliceYmin = x(2)
        END IF
      END DO
      !== Slice End Point
      ios  = 1
      itry = 0
      string = 'Slice ending point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmax = x(1)
          sliceYmax = x(2)
        END IF
      END DO
      !== Slice vertical Range
      ios  = 1
      itry = 0
      string = 'Slice vertical range'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceZmin = x(1)
          sliceZmax = x(2)
        END IF
      END DO
      !== Slice number vertical of Points
      ios  = 1
      itry = 0
      string = 'Slice no. of vertical pts'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,1,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceNz = MAX(0,NINT(x(1))-5) !Create field adds 5
        END IF
      END DO
  END IF
  CASE( HP_HSLICE )
    IF( iFld > 0 )THEN
      IF( Hslc > 0 )THEN
        cmd  = TRIM(cmds(Hslc)%cargs)
        narg = 0
        CALL SplitString( cmd,',',narg,cargs,lerr )
        IF( narg > 1 .AND. .NOT.lerr )THEN
          ! Slice lower, left point
          READ( cargs(1),*,IOSTAT=ios )sliceXmin
          IF( ios == 0 )READ( cargs(2),*,IOSTAT=ios )sliceYmin
          ! Slice upper, right point
          IF( ios == 0 )READ( cargs(3),*,IOSTAT=ios )sliceXmax
          IF( ios == 0 )READ( cargs(4),*,IOSTAT=ios )sliceYmax
          ! Slice height
          IF( ios == 0 )READ( cargs(5),*,IOSTAT=ios )sliceHeight
        ELSE IF ( narg == 1 .AND. .NOT.lerr )THEN
          ! Slice height
          READ( cargs(1),*,IOSTAT=ios )sliceHeight
          sliceXmin = DEF_VAL_R
          sliceYmin = DEF_VAL_R
          sliceXmax = DEF_VAL_R
          sliceYmax = DEF_VAL_R
        ELSE
          nError = UK_ERROR
        END IF
      ELSE
        nError = UK_ERROR
      END IF
      IF( nError == UK_ERROR .OR. ios /= 0 )THEN
        eMessage = 'Error reading horizontal slice arguments [xMin,yMin,Xmax,yMax,]ZSlice'
        GOTO 9999
      END IF
    ELSE
      !== Slice lower, left point
      ios  = 1
      itry = 0
      string = 'Slice lower, left point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmin = x(1)
          sliceYmin = x(2)
        END IF
      END DO
      !== Slice upper, right point
      ios  = 1
      itry = 0
      string = 'Slice upper, right point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmax = x(1)
          sliceYmax = x(2)
        END IF
      END DO
      IF( .NOT. output3D )THEN
      !== Slice height
        ios  = 1
        itry = 0
        string = 'Slice height'
        DO WHILE( ios > 0 )
          itry = itry + 1
          ios = getInput( string,1,x )
          IF( ios > 0 )THEN
            IF( itry < maxTry )THEN
              WRITE(6,'(A)')'Error reading input, try again'
            ELSE
              nError   = UK_ERROR
              eMessage = 'Error reading '//TRIM(string)
              GOTO 9999
            END IF
          ELSE IF( ios == 0 )THEN
            sliceHeight = x(1)
          END IF
        END DO
      END IF
    END IF
  CASE( HP_VINT,HP_SSLICE )
    IF( iFld > 0 )THEN
      IF( Vint > 0 .OR. Sslc > 0 )THEN
        IF( Sslc > 0 )THEN
          cmd = TRIM(cmds(Sslc)%cargs)
        ELSE
          cmd = TRIM(cmds(Vint)%cargs)
        END IF
        narg = 0
        CALL SplitString( cmd,',',narg,cargs,lerr )
        IF( narg == 4 .AND. .NOT.lerr )THEN
          ! Slice lower, left point
          READ( cargs(1),*,IOSTAT=ios )sliceXmin
          IF( ios == 0)READ( cargs(2),*,IOSTAT=ios )sliceYmin
          ! Slice upper, right point
          IF( ios == 0)READ( cargs(3),*,IOSTAT=ios )sliceXmax
          IF( ios == 0)READ( cargs(4),*,IOSTAT=ios )sliceYmax
        ELSE
          sliceXmin = DEF_VAL_R
          sliceYmin = DEF_VAL_R
          sliceXmax = DEF_VAL_R
          sliceYmax = DEF_VAL_R
        END IF
      ELSE
        nError   = UK_ERROR
      END IF
      IF( nError == UK_ERROR .OR. ios /= 0 )THEN
        IF( Sslc > 0 )THEN
          eMessage = 'Error reading surface slice arguments xMin,yMin,Xmax,yMax'
        ELSE
          eMessage = 'Error reading vertical integrated slice arguments xMin,yMin,Xmax,yMax'
        END IF
        GOTO 9999
      END IF
    ELSE
      !== Slice lower, left point
      ios  = 1
      itry = 0
      string = 'Slice lower, left point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmin = x(1)
          sliceYmin = x(2)
        END IF
      END DO
      !== Slice upper, right point
      ios  = 1
      itry = 0
      string = 'Slice upper, right point'
      DO WHILE( ios > 0 )
        itry = itry + 1
        ios = getInput( string,2,x )
        IF( ios > 0 )THEN
          IF( itry < maxTry )THEN
            WRITE(6,'(A)')'Error reading input, try again'
          ELSE
            nError   = UK_ERROR
            eMessage = 'Error reading '//TRIM(string)
            GOTO 9999
          END IF
        ELSE IF( ios == 0 )THEN
          sliceXmax = x(1)
          sliceYmax = x(2)
        END IF
      END DO
    END IF

  CASE( HP_TABLE )
!== Risk Level
    ios  = 1
    itry = 0
    string = 'Risk level'
    DO WHILE( ios > 0 )
      itry = itry + 1
      ios = getInput( string,1,x )
      IF( ios > 0 )THEN
        IF( itry < maxTry )THEN
          WRITE(6,'(A)')'Error reading input, try again'
        ELSE
          nError   = UK_ERROR
          eMessage = 'Error reading '//TRIM(string)
          GOTO 9999
        END IF
      ELSE IF( ios == 0 )THEN
        riskLevel = x(1)
      END IF
    END DO
!===Rounding mode
    WRITE(6,'(A)')'Rounding options'
    WRITE(6,'(A)')'0 = default'
    WRITE(6,'(A)')'1 = none'
    WRITE(6,'(A)')'2 = nearest hundred'
    WRITE(6,'(A)')'3 = nearest thousand'
    WRITE(6,'(A)')'4 = two digits'
    WRITE(6,'(A)')'5 = three digits'
    WRITE(6,'(A)')'Enter Rounding option'
    READ(lun_in,*)iround
    iround = MIN(MAX(0,iround),5)
  CASE DEFAULT
END SELECT

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
! getInput
!==============================================================================
!==============================================================================
INTEGER FUNCTION getInput( header,n,x )

USE Extract_fi

IMPLICIT NONE

CHARACTER(*), INTENT (IN )       :: header
INTEGER,      INTENT (IN )       :: n
REAL, DIMENSION(n), INTENT (OUT) :: x

CHARACTER(128) string

INTEGER, EXTERNAL :: parseString

getInput = -1

WRITE(6,'(A,$)')'Enter '//TRIM(header)//' (CR=default):'
READ(lun_in,'(A)') string

IF( LEN_TRIM(string) > 0 )THEN
  getInput = parseString( string,n,x )
END IF

RETURN
END
!==============================================================================
!==============================================================================
! parseString
!==============================================================================
!==============================================================================
INTEGER FUNCTION parseString( string,n,x )

IMPLICIT NONE

CHARACTER(*), INTENT (IN )       :: string
INTEGER,      INTENT (IN )       :: n
REAL, DIMENSION(n), INTENT (OUT) :: x

INTEGER ipt, ios, i
CHARACTER(120)worker

worker = TRIM(ADJUSTL(string))
ios    = 0

ipt   = INDEX(worker,',')
IF( ipt > 0 )THEN !comma delimited
  DO i = 1,n
    READ(worker(1:ipt),*,IOSTAT=ios)x(i)
    IF( ios /= 0 )EXIT
    worker = TRIM(ADJUSTL(worker(ipt+1:)))
    ipt    = INDEX(worker,',')
    IF( ipt <= 0 )ipt = LEN_TRIM(worker)
  END DO
ELSE
  ipt = INDEX(worker,' ')
  IF( ipt > 0 )THEN !space delimited
    DO i = 1,n
      READ(worker(1:ipt),*,IOSTAT=ios)x(i)
      IF( ios /= 0 )EXIT
      worker = TRIM(ADJUSTL(worker(ipt+1:)))
      ipt    = INDEX(worker,' ')
      IF( ipt <= 0 )ipt = LEN_TRIM(worker)
    END DO
  ELSE
    ios = 999
  END IF
END IF

IF( ios < 0 )ios = 100000 - ios

parseString = ios

RETURN
END
