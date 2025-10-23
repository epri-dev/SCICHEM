!==============================================================================
! SelectField
!==============================================================================
SUBROUTINE SelectField()

USE Extract_fi

IMPLICIT NONE

WRITE(6,'(/,A)')'Select Plot Field'

CALL pickPlotClass()
IF( nError /= NO_ERROR )GOTO 9999

CALL pickPlotChoice( Field%class,Field%choice,'Field' )
IF( nError /= NO_ERROR )GOTO 9999

IF( .NOT. output3D )mxclass = 0

IF( output3D .AND. mxclass > 1 )THEN
  CALL pickPlotChoice( MetField%class,MetField%choice,'MetField' )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

CALL pickPlotKind( Field%class,Field%choice,Field%kind,'Field' )
IF( nError /= NO_ERROR )GOTO 9999

IF( output3D .AND. mxclass > 1 )THEN
  CALL pickPlotKind( MetField%class,MetField%choice,MetField%kind,'MetField' )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( output3D )THEN
  Field%category = HP_HSLICE
  IF( mxclass > 1)MetField%category = HP_SURF
  PlotType%type = HP_MEAN
  doFld = .TRUE.
ELSE
  CALL pickPlotCategory( Field%class,Field%category,'Field' )
  IF( nError /= NO_ERROR )GOTO 9999
  doFld = Field%category /= HP_TABLE
  CALL pickPlotType( Field%class,Field%category,PlotType%type,PlotType%data,'Field Plot' )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

CALL pickPlotTime( Field%class,Field%choice,Field%timeID,Field%userTime,'Field' )
IF( nError /= NO_ERROR )GOTO 9999


IF( output3D .AND. mxclass > 1 )THEN
  MetField%timeID   = Field%timeID
  MetField%userTime = Field%userTime
END IF

CALL setPlotClassData( (output3D .AND. mxclass > 1),.TRUE. )

hasVariance = PlotType%type == HP_MEAN
hasVariance = hasVariance .AND. CatClassArray(Field%category,Field%class)%type == SCIPtrue

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotClass
!==============================================================================
SUBROUTINE pickPlotClass()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER i,j,maxClass, itry
CHARACTER(128) string, string1
LOGICAL lFld

maxClass = nClass

WRITE(6,'(A)')'Available Field Classes'
WRITE(6,'(A)')'===================================='
DO i = 1,maxClass
  lFld = .TRUE.
  IF( noTable )THEN
    lFld = CatClassArray(HP_TABLE,i)%available == SCIPfalse
  END IF
  IF( concProfile )THEN
    lFld = lFld .AND. (CatClassArray(HP_VSLICE,i)%available == SCIPtrue .OR. CatClassArray(HP_HINT,i)%available == SCIPtrue)
  END IF
  IF( lFld)WRITE(6,'(I3,1X,A)')i,TRIM(ClassStr(i)%string)
END DO
IF( has3D )THEN
  maxClass = maxClass + 1
  WRITE(6,'(I3,1X,A)')maxClass,TRIM(CONCENTRATION_3D)
END IF

itry = 0
100 CONTINUE

Field%class = 0

IF( UseKey )THEN
  WRITE(6,'(/,A,$)')'Class keyword? '
  IF( iFld > 0 )THEN
    narg          = 0
    fld%fcategory = ''
    fld%fkind     = 'Total'
    cmd           = TRIM(cmds(iFld)%cargs)
    CALL SplitString( cmd,',',narg,cargs,lerr )
    SELECT CASE( TRIM(cargs(1)) )
    CASE( 'dos' )
      fld%fclass = 'Surface Dosage'
    CASE( 'dep' )
      fld%fclass = 'Surface Deposition'
    CASE( 'con' )
      fld%fclass = 'Concentration'
    CASE( 'ico' )
      fld%fclass = 'Integrated Concentration'
    CASE( 'dur' )
      fld%fclass = 'Dosage Duration'
    CASE( 'txd' )
      fld%fclass = 'Toxic Dosage'
    CASE( 'agl' )
      fld%fclass = 'AEGLs'
      fld%fkind  = 'AEGLs  '! Need space to skip 10% exceedance
    CASE( 'inh' )
      fld%fclass = 'Inhalable Dosage'
    CASE( 'ter' )
      fld%fclass = 'Terrain'
    CASE( 'pbt' )
      fld%fclass = 'Probit Function'
    CASE DEFAULT
      WRITE(6,'(A)')'Unable to set valid Field class selection, exiting'
      nError = UK_ERROR
      eMessage = 'Unable to set valid Field class selection, exiting'
      WRITE(eInform,'("Input Field class value is ",A)')TRIM(cargs(1))
      GOTO 9999
    END SELECT
    IF( narg > 1 )fld%fkind = cargs(2)
    fld%fchoice = matName
    string      = fld%fclass
  ELSE
    READ(lun_in,'(A)')string
  END IF
  string = ADJUSTL(string)
  CALL CUPPER( string )
  j = LEN_TRIM(string)
  DO i = 1,nClass
    string1 = ClassStr(i)%string
    CALL CUPPER( string1 )
    IF( INDEX(string1,string(1:j)) > 0 )THEN
      Field%class = i
      EXIT
    END IF
  END DO
  IF( maxClass > nClass .AND. Field%class == 0 )THEN
    string1 = CONCENTRATION_3D
    CALL CUPPER( string1 )
    IF( INDEX(string1,string(1:j)) > 0 )THEN
      Field%class = maxClass
      output3D = .TRUE.
    END IF
  END IF
ELSE
  WRITE(6,'(/,A,$)')'Class number ? '
  READ(lun_in,'(I3)')Field%class
  output3D = (has3D .AND. Field%class == maxClass)
END IF

itry = itry + 1
IF( Field%class <= 0 .OR. Field%class > maxClass )THEN
  IF( itry < maxTry )THEN
    WRITE(6,'(A)')'Invalid Field selection, try again'
    GOTO 100
  ELSE
    WRITE(6,'(A)')'Unable to set valid Field class selection, exiting'
    nError = UK_ERROR
    eMessage = 'Unable to set valid Field class selection, exiting'
    GOTO 9999
  END IF
END IF

IF( output3D )THEN
  WRITE(6,'(/,"Field class : ",A,/)')CONCENTRATION_3D
  CALL setPlotClass3D()
  IF( nError /= NO_ERROR )GOTO 9999
ELSE
  WRITE(6,'(/,"Field class : ",I3,A/)')Field%class,' ('//TRIM(ClassStr(Field%class)%string)//')'
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotChoice
!==============================================================================
SUBROUTINE pickPlotChoice( iClass,iChoice,header )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (OUT) :: iChoice
CHARACTER(*), INTENT (IN ) :: header

INTEGER i, j, maxChoice, itry, count, oneChoice
LOGICAL lFld
CHARACTER(128) string, string1

count     = 0
oneChoice = 1
! User Input : Plot Field Choice
WRITE(6,'(A)')'Available '//TRIM(header)//' Choices'
WRITE(6,'(A)')'===================================='
DO i = 1,nChoice
  IF( LEN_TRIM(ChoiceStr(i)%string) > 0 )THEN
    lFld = BTEST(ClassChoiceArray( iClass, i )%available,HPB_AVAILABLE)
    IF( lFld )THEN
      WRITE(6,'(I3,1x,A)')i,TRIM(ChoiceStr(i)%string)
      count = count + 1
      oneChoice = i
    END IF
  END IF
END DO
maxChoice = count

IF( lScript .OR. maxChoice > 1 )THEN

  itry = 0
100 CONTINUE

  iChoice = 0
  IF( UseKey )THEN
    WRITE(6,'(/,A,$)')'Choice keyword? '
    IF( iFld > 0 )THEN
      IF( LEN_TRIM(matName) > 0 )THEN
        string = matName
      ELSE
        string = TRIM(ChoiceStr(oneChoice)%string)
      END IF
    ELSE
      READ(lun_in,'(A)')string
    END IF
    string = ADJUSTL(string)
    CALL CUPPER( string )
    j = LEN_TRIM(string)
    DO i = 1,nchoice
      lFld = BTEST(ClassChoiceArray(iClass,i)%available,HPB_AVAILABLE)
      IF( lFld )THEN
        string1 = ChoiceStr(i)%string
        CALL CUPPER( string1 )
        IF( INDEX(string1,string(1:j)) > 0 )THEN
          iChoice = i
          EXIT
        END IF
      END IF
    END DO
  ELSE
    WRITE(6,'(/,A,$)')'Choice number ? '
    READ(lun_in,'(I3)')iChoice
  END IF

  itry = itry + 1
  IF( iChoice <= 0 .OR. iChoice > nChoice )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Invalid '//TRIM(header)//' choice selection, try again'
      GOTO 100
    ELSE
      WRITE(6,'(A)')'Unable to set valid '//TRIM(header)//' choice selection, exiting'
      nError = UK_ERROR
      eMessage = 'Unable to set valid '//TRIM(header)//' choice selection, exiting'
      GOTO 9999
    END IF
  END IF
ELSE
  iChoice = oneChoice
END IF

WRITE(6,'(/,"Field choice : ",I3,A/)')iChoice,' ('//TRIM(ChoiceStr(iChoice)%string)//')'

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotKind
!==============================================================================
SUBROUTINE pickPlotKind( iClass,iChoice,iKind,header )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (IN ) :: iChoice
INTEGER,      INTENT (OUT) :: iKind
CHARACTER(*), INTENT (IN ) :: header

LOGICAL lFld
INTEGER i, j, itry, jkind
CHARACTER(128) string, string1

iKind = 0

lFld = BTEST(ClassChoiceArray(iClass,iChoice)%kind,HPB_AVAILABLE)
IF( lFld )THEN
  WRITE(6,'(/,A)')'Available '//TRIM(header)//' Kinds '
  WRITE(6,'(A)')'===================================='
  DO i = 1,ClassChoiceArray( iClass, iChoice )%nkind
    WRITE(6,'(I3,1x,A)',ADVANCE='NO')i,TRIM(KindStr(i+ClassChoiceArray(iClass,iChoice)%ikind-1)%string)
    WRITE(6,*)
  END DO

  IF( lScript .OR. ClassChoiceArray(iClass,iChoice)%nkind > 1 )THEN
    itry = 0
100 CONTINUE

    ikind = 0
    jkind = 0
    IF( UseKey )THEN
      WRITE(6,'(/,A,$)')'Kind keyword? '
      IF( iFld > 0 )THEN
        IF( TRIM(fld%fkind) == 'AEGLs' )THEN
          jkind = 2
        ELSE IF( INDEX('Probit Function',fld%fclass) > 0 )THEN
          jkind = 1
        ELSE
          String = fld%fkind
        END IF
      ELSE
        READ(lun_in,'(A)')String
      END IF
      IF( jkind == 0 )THEN
        String = ADJUSTL(String)
        CALL CUPPER( String )
        j = LEN_TRIM(String)
        i = 1
        DO i = 1,ClassChoiceArray(iClass,iChoice)%nkind
          string1 = KindStr(i+ClassChoiceArray(iClass,iChoice)%ikind-1)%string
          CALL CUPPER( string1 )
          IF( INDEX(string1,String(1:j)) > 0 )THEN
            jkind = i
            EXIT
          END IF
        END DO
      ENDIF
    ELSE
      WRITE(6,'(/,A,$)')'Kind  number? '
      READ(lun_in,'(I3)')jkind
    END IF

    itry = itry + 1
    IF( jkind <= 0 .OR. jkind > ClassChoiceArray(iClass,iChoice)%nkind )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'Invalid '//TRIM(header)//' kind selection, try again'
        GOTO 100
      ELSE
        WRITE(6,'(A)')'Unable to set valid '//TRIM(header)//' kind selection, exiting'
        nError = UK_ERROR
        eMessage = 'Unable to set valid '//TRIM(header)//' kind selection, exiting'
        GOTO 9999
      END IF
    END IF
  ELSE
    jkind = 1
  END IF

  ikind = jkind + ClassChoiceArray(iClass,iChoice )%ikind-1
  WRITE(6,'(/,A," Kind : ",I3,A)')TRIM(header),jkind,' ('//TRIM(KindStr(iKind)%string)//')'

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotCategory
!==============================================================================
SUBROUTINE pickPlotCategory( iClass,iCategory,header )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (OUT) :: iCategory
CHARACTER(*), INTENT (IN ) :: header

INTEGER i, j, itry, icat, maxCat
LOGICAL lFld
CHARACTER(128) string, string1

!
!CATEGORY_STRING: Surface(HP_SURF),Surface Slice(HP_SSLICE),Horizontal Slice(HP_HSLICE)
!        Vertically Integrated Slice(HP_VINT),Vertical Slice(HP_VSLICE),Table(HP_TABLE)
!        Horizontal Projection (HP_HINT)

maxCat = 0
DO i = 1,HP_NUMCAT
  lFld = CatClassArray(i,iClass)%available == SCIPtrue
  IF( noTable )THEN
    lFld = lFld .AND. i /= HP_TABLE
  END IF
  IF( concProfile )THEN
    lFld = lFld .AND. ( i == HP_VSLICE .OR. i == HP_HINT )
  END IF
  IF( lFld )THEN
    maxCat = maxCat + 1
    iCat = i
  END IF
END DO

IF( lScript .OR. maxCat > 1 )THEN
  WRITE(6,'(/,A)')'Available '//TRIM(header)//' Categories'
  WRITE(6,'(A)')'===================================='
  DO i = 1,HP_NUMCAT
    lFld = CatClassArray(i,iClass)%available == SCIPtrue
    IF( concProfile )THEN
      lFld = lFld .AND. ( i == HP_VSLICE .OR. i == HP_HINT )
    END IF
    IF( lFld )WRITE(6,'(I3,1x,A)')i,TRIM(CATEGORY_STRING(i))
  END DO

  itry = 0

100 CONTINUE
  iCat = 0

  IF( UseKey )THEN
    WRITE(6,'(/,A,$)')'Category keyword? '
    IF( iFld  > 0 )THEN
      IF( Hslc  > 0 )THEN
        String = 'Horizontal Slice'
      ELSE IF( Sslc  > 0 )THEN
        String = 'Surface Slice'
      ELSE IF( Vslc > 0 )THEN
        String = 'Vertical Slice'
      ELSE IF( Vint > 0 )THEN
        String = 'Vertically Integrated Slice'
      ELSE IF( Hpro > 0 )THEN
        String = 'Horizontal Projection'
      ELSE
        String = 'Unknown Category'
      END IF
    ELSE
      READ(lun_in,'(A)')String
    END IF
    String = ADJUSTL(String)
    CALL CUPPER( String )
    j = LEN_TRIM(String)
    DO i = 1,HP_NUMCAT
      lFld = CatClassArray(i,Field%class)%available == SCIPtrue
      IF( lFld )THEN
        string1 = CATEGORY_STRING(i)
        CALL CUPPER( string1 )
        IF( INDEX(string1,String(1:j)) > 0 )THEN
          iCat = i
          EXIT
        END IF
      END IF
    END DO
  ELSE
    WRITE(6,'(/,A,$)')'Category number? '
    READ(lun_in,'(I3)')iCat
  END IF

  itry = itry + 1
  IF( iCat <= 0 .OR. iCat > HP_NUMCAT )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Invalid '//TRIM(header)//' category selection, try again'
      GOTO 100
    ELSE
      WRITE(6,'(A)')'Unable to set valid '//TRIM(header)//' category selection, exiting'
      nError   = UK_ERROR
      eMessage = 'Unable to set valid '//TRIM(header)//' category selection, exiting'
      GOTO 9999
    END IF
  END IF

END IF

iCategory = iCat

WRITE(6,'(/,A," category : ",I3,A)')TRIM(header),iCategory,' ('//TRIM(CATEGORY_STRING(iCategory))//')'
IF(iFld > 0 )fld%fcategory = TRIM(CATEGORY_STRING(iCategory))

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotType
!==============================================================================
SUBROUTINE pickPlotType( iClass,iCategory,iType,dType,header )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (IN ) :: iCategory
INTEGER,      INTENT (OUT) :: iType
REAL,         INTENT (OUT) :: dType
CHARACTER(*), INTENT (IN ) :: header

INTEGER i, j, itry, iTyp, maxType
LOGICAL lFld
CHARACTER(128) string, string1

!TYPE_STRING = (/'Mean Value  ( M )' ,'Probability ( P[v>E] )' ,
!                'Exceedance  ( v[Pc>P] )' ,'Variance    ( V )/)

iType   = 0
dType   = 0.0

IF( CatClassArray(iCategory,iClass)%type == SCIPtrue )THEN
  IF( pickedType > 0 )THEN
    iType = pickedType
    dType = pickedTypeData
    GOTO 9999
  END IF

  maxType = 0
  DO i = 1,HP_NUMTYP
    lFld = CatClassArray(Field%Category,Field%Class)%type == SCIPtrue
    IF( lFld )THEN
      maxType = maxType + 1
      iTyp = i
      IF( iFld > 0 )THEN
        IF( maxType == 1 )String = TRIM(TYPE_STRING(i))
      END IF
    END IF
  END DO

  IF( lScript .OR. maxType > 1 )THEN
    WRITE(6,'(/,A)')'Available '//TRIM(header)//' types'
    WRITE(6,'(A)')'===================================='
    DO i = 1,HP_NUMTYP
      lFld = CatClassArray(Field%Category,Field%Class)%type == SCIPtrue
      IF( lFld )THEN
        WRITE(6,'(I3,1x,A)')i,TRIM(TYPE_STRING(i))
      END IF
    END DO
    iTry = 0
100 CONTINUE

    iTyp = 0
    IF( UseKey )THEN
      WRITE(6,'(/,A,$)')'Type keyword? '
      IF( iFld > 0 )THEN
        IF( LEN_TRIM(fld%ftype) > 0 )THEN
          String = TRIM(fld%ftype)
        ELSE
          String = TRIM(TYPE_STRING(1))
        END IF
      ELSE
        READ(lun_in,'(A)')String
      END IF
      String = ADJUSTL(String)
      CALL CUPPER(String)
      j = LEN_TRIM(String)
      DO i = 1,HP_NUMTYP
        lFld = CatClassArray(Field%Category,Field%Class)%type == SCIPtrue
        IF( lFld )THEN
          string1 = TYPE_STRING(i)
          CALL CUPPER( string1 )
          IF( INDEX(string1,String(1:j)) > 0 )THEN
            iTyp = i
            EXIT
          END IF
        END IF
      END DO
    ELSE
      WRITE(6,'(/,A,$)')'Type number? '
      READ(lun_in,'(I3)')iTyp
    END IF

    itry = itry + 1
    IF( iTyp <= 0 .OR. iTyp > HP_NUMTYP )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'Invalid '//TRIM(header)//' type selection, try again'
        GOTO 100
      ELSE
        WRITE(6,'(A)')'Unable to set valid '//TRIM(header)//' type selection, exiting'
        nError = UK_ERROR
        eMessage = 'Unable to set valid '//TRIM(header)//' type selection, exiting'
        GOTO 9999
      END IF
    END IF

  END IF

  iType = iTyp

  WRITE(string,'(A,I2,A)')TRIM(header)//' Type : ',iType,' ('//TRIM(TYPE_STRING(iType))//')'

  CALL setPlotTypeData( iType,dType,string )
  IF( nError /= NO_ERROR )GOTO 9999

  WRITE(6,'(/,A)')TRIM(string)
ELSE
  iType = HP_MEAN
  dType = 0.0
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! setPlotTypeData
!==============================================================================
SUBROUTINE setPlotTypeData( typeID,typeData,line )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER      :: typeID
REAL         :: typeData
CHARACTER(*) :: line

INTEGER ios, itry
CHARACTER(32) input

itry = 0

IF( typeID == HP_PROB )THEN

  DO
    WRITE(6,'(/,"Probability of exceeding ? (CR=default): ",$)')
    IF( iFld > 0 )THEN
      typeData = vexcd
    ELSE
      READ(lun_in,'(A)')input
      IF( LEN_TRIM(input) <= 0 )input = '0.0'
      input = ADJUSTR(input)

      itry = itry + 1

      READ(input,*,IOSTAT=ios)typeData
      IF( ios /= 0 )THEN
        IF( itry < maxTry )THEN
          WRITE(6,'(A)')'Error reading value from '//TRIM(ADJUSTL(input))//'. try again'
          CYCLE
        ELSE
          nError   = UK_ERROR
          eMessage = 'Error reading value from '//TRIM(ADJUSTL(input))
          GOTO 9999
        END IF
      END IF
    END IF
    EXIT
  END DO

  line = TRIM(line)//'('//TRIM(input)//')'

ELSE IF( typeID == HP_EXCEED )THEN

  DO
    IF( iFld > 0 )THEN
      typeData = pexcd
    ELSE
      WRITE(6,'(/,"Exceedance level [0.-100.]? (CR=default): ",$)')
      READ(lun_in,'(A)')input
      IF( LEN_TRIM(input) <= 0 )input = '10.0'
      input = ADJUSTR(input)

      itry = itry + 1

      READ(input,*,IOSTAT=ios)typeData
      IF( ios /= 0 )THEN
        IF( itry < maxTry )THEN
          WRITE(6,'(A)')'Error reading value from '//TRIM(ADJUSTL(input))//'. try again'
          CYCLE
        ELSE
          nError   = UK_ERROR
          eMessage = 'Error reading value from '//TRIM(ADJUSTL(input))
          GOTO 9999
        END IF
      ELSE IF( typeData >= 100.0 .OR. typeData <=0. )THEN
        IF( itry < maxTry )THEN
          WRITE(6,'(A)')'Invalid probability. 0.< input <100. : '//TRIM(ADJUSTL(input))//'. try again'
        ELSE
          nError   = UK_ERROR
          eMessage = 'Invalid probability. 0.< input <100. : '//TRIM(ADJUSTL(input))
          GOTO 9999
        END IF
      END IF
    END IF
    EXIT
  END DO

  line = TRIM(line)//'('//TRIM(input)//')'

ELSE

  typeData = 0.0

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotTime
!==============================================================================
SUBROUTINE pickPlotTime( iClass,iChoice,iTime,rTime,header )

USE Extract_fi
USE GetTimes_fi
USE cmd_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (IN ) :: iChoice
INTEGER,      INTENT (OUT) :: iTime
REAL,         INTENT (OUT) :: rTime
CHARACTER(*), INTENT (IN ) :: header

INTEGER i, j, itry, jtry

LOGICAL lUserTime
REAL    pTime, maxTime
REAL    minTime
REAL    dt, dtx
CHARACTER(120) string, string1, usertime

LOGICAL, EXTERNAL :: LoadPlotTimes

lUserTime = loadPlotTimes( ClassChoiceArray(iClass,iChoice)%itime, &
                           ClassChoiceArray(iClass,iChoice)%usertime,maxTime )

iTime = 0
rTime = 0.0
minTime = HUGE(1.0)

IF( nTimeOut > 0 )THEN

  WRITE(6,'(/,A)')'Available '//TRIM(header)//' Times '
  WRITE(6,'(A)')'===================================='
  DO i = 1,nTimeOut
    WRITE(6,'(F10.4,7X,"(",A,")")')TimeOut(i)%time%runTime,TimeOut(i)%string
  END DO
  IF( iTim > 0 )THEN
    DO i = 1,nTimeOut
      IF( minTime > ABS(TimeOut(i)%time%runTime - tOut) )THEN
        minTime = AMIN1(minTime,ABS(TimeOut(i)%time%runTime - tOut))
        iTime   = i
        rTime   = TimeOut(i)%time%runTime
      ENDIF
    END DO
  END IF

  IF( lUserTime )THEN
    IF( maxTime < HUGE(1.0) )THEN
      WRITE(usertime,'(F13.5)')maxTime
      usertime = 'User time <'//ADJUSTL(TRIM(usertime))
    ELSE
      usertime = 'User time'
    END IF
    WRITE(6,'(A)')usertime
  END IF

  IF( iTim > 0 )THEN
    WRITE(6,'(/,"Using output time of ",F10.4,7X,"(",A,")")')TimeOut(iTime)%time%runTime,TimeOut(iTime)%string
  ELSE
    IF( UseKey )THEN
      itry = 0
100   CONTINUE
      itry = itry + 1
      WRITE(6,'(/,A,$)')'Plot time keyword ? '
      READ(lun_in,'(A)')string
      string = ADJUSTL(string)
      CALL CUPPER( string )
      j = LEN_TRIM(string)

      iTime = -1
      DO i = 1,nTimeOut
        string1 = TimeOut(i)%string
        CALL CUPPER( string1 )
        IF( INDEX(string1,string(1:j)) > 0 )THEN
          iTime = i
          rTime = TimeOut(i)%time%runTime
          EXIT
        END IF
      END DO
      IF( lUserTime .AND. iTime < 1 )THEN
        string1 = usertime
        CALL CUPPER( string1 )
        IF( INDEX(string1,string(1:j)) > 0 )THEN
          jtry = 0
200       CONTINUE
          WRITE(6,'(/,"User Time : ",$)')
          READ(lun_in,*)pTime
          jtry = jtry + 1
          IF( pTime <= TimeOut(nTimeOut)%time%runTime )THEN
            IF( jtry < maxTry )THEN
              WRITE(6,'(A,F10.4)')'User time must be greater than',TimeOut(nTimeOut)%time%runTime
              GOTO 200
            ELSE
              nError = UK_ERROR
              WRITE(eMessage,'(A,F10.4)')'User time must be greater than',TimeOut(nTimeOut)%time%runTime
            END IF
          ELSE
            iTime = nTimeOut+1
            rTime = MIN(pTime,maxTime)
          END IF
        END IF
      END IF

      IF( iTime < 1 )THEN
        IF( itry < maxTry )THEN
          WRITE(6,'(/,"Cannot find keyword ",(A)," in time strings. Please try again")')TRIM(string)
          GOTO 100
        ELSE
          nError   = UK_ERROR
          eMessage = 'Cannot find keyword '//TRIM(string)//' in time strings.'
          GOTO 9999
        END IF
      ELSE
      END IF

    ELSE
      WRITE(6,'(/,"Plot Time : ",$)')
      READ(lun_in,*)pTime

      iTime = 0
      IF( pTime <= TimeOut(1)%time%runTime )THEN
        iTime = 1
        rTime = TimeOut(1)%time%runTime
      ELSE IF( pTime > TimeOut(nTimeOut)%time%runTime )THEN
        IF( lUserTime )THEN
          iTime = nTimeOut+1
          rTime = MIN(pTime,maxTime)
        ELSE
          iTime = nTimeOut
          rTime = TimeOut(nTimeOut)%time%runTime
        END IF
      ELSE
        dtx = HUGE(1.0)
        DO i = 1,nTimeOut
          dt = ABS(pTime - TimeOut(i)%time%runTime)
          IF( dt < dtx )THEN
            dtx = dt
            iTime = i
            rTime = TimeOut(i)%time%runTime
          END IF
        END DO
      END IF
    END IF
  END IF

  IF( lUsertime .AND. iTime > nTimeOut )THEN
    WRITE(6,'("Using usertime: ",I7,1x,F10.4)')nTimeOut+1,rTime
  ELSE
    WRITE(6,'(F10.4,7X,"(",A,")")')TimeOut(iTime)%time%runTime,TimeOut(iTime)%string
  END IF

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! pickPlotTime
!==============================================================================
SUBROUTINE pickAnotherPlotTime( iClass,iChoice,lastID,lastTime,iTime,rTime,header )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER,      INTENT (IN ) :: iClass
INTEGER,      INTENT (IN ) :: iChoice
INTEGER,      INTENT (IN ) :: lastID
REAL,         INTENT (IN ) :: lastTime
INTEGER,      INTENT (OUT) :: iTime
REAL,         INTENT (OUT) :: rTime
CHARACTER(*), INTENT (IN ) :: header

INTEGER i, j, itry, jtry
LOGICAL lUserTime, moreTimes
REAL    pTime, maxTime, minTime
REAL    dt, dtx
CHARACTER(120) string, string1, usertime

LOGICAL, EXTERNAL :: LoadPlotTimes

lUserTime = loadPlotTimes( ClassChoiceArray(iClass,iChoice)%itime, &
                           ClassChoiceArray(iClass,iChoice)%usertime,maxTime )

iTime = 0
rTime = 0.0

IF( nTimeOut > 0 )THEN

  minTime = TimeOut(nTimeOut)%time%runTime
  moreTimes = lastID < ntimeOut
  IF( .NOT.moreTimes .AND. lUserTime )THEN
    minTime = MAX(minTime,lastTime)
    moreTimes = minTime < maxTime
  END IF

  IF( .NOT.moreTimes )THEN
    iTime = -1
    GOTO 9999
  END IF

  WRITE(6,'(/,A)')'Available '//TRIM(header)//' Times '
  WRITE(6,'(A)')'===================================='
  DO i = lastID+1,nTimeOut
    WRITE(6,'(F10.4,7X,"(",A,")")')TimeOut(i)%time%runTime,TimeOut(i)%string
  END DO
  IF( lUserTime )THEN
    string = " "
    string1 = " "
    IF( maxTime < HUGE(1.0) )THEN
      WRITE(string,'(F13.5)')maxTime
      string = '<'//ADJUSTL(TRIM(string))
    END IF
    IF( minTime > 0.0 )THEN
      WRITE(string1,'(F13.5)')minTime
      string1 = ADJUSTL(TRIM(string))//'<'
    END IF
    usertime = TRIM(string1)//'User time'//TRIM(string)
    WRITE(6,'(A)')usertime
  END IF


  IF( UseKey )THEN
    itry = 0
100 CONTINUE
    itry = itry + 1
    WRITE(6,'(/,A,$)')'Plot time keyword ? '
    READ(lun_in,'(A)')string
    string = ADJUSTL(string)
    CALL CUPPER( string )
    j = LEN_TRIM(string)

    iTime = 0
    DO i = 1,nTimeOut
      string1 = TimeOut(i)%string
      CALL CUPPER( string1 )
      IF( INDEX(string1,string(1:j)) > 0 )THEN
        iTime = i
        rTime = TimeOut(i)%time%runTime
        EXIT
      END IF
    END DO
    IF( lUserTime .AND. iTime < 1 )THEN
      string1 = usertime
      CALL CUPPER( string1 )
      IF( INDEX(string1,string(1:j)) > 0 )THEN
        jtry = 0
200     CONTINUE
        WRITE(6,'(/,"User Time : ",$)')
        READ(lun_in,*)pTime
        jtry = jtry + 1
        IF( pTime <= minTime )THEN
          IF( jtry < maxTry )THEN
            WRITE(6,'(A,F10.4)')'User time must be greater than',minTime
            GOTO 200
          ELSE
            nError = UK_ERROR
            WRITE(eMessage,'(A,F10.4)')'User time must be greater than',minTime
          END IF
        ELSE
          iTime = nTimeOut+1
          rTime = MIN(pTime,maxTime)
        END IF
      END IF
    END IF

    IF( iTime < 1 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(/,"Cannot find keyword ",(A)," in time strings. Please try again")')TRIM(string)
        GOTO 100
      ELSE
        nError   = UK_ERROR
        eMessage = 'Cannot find keyword '//TRIM(string)//' in time strings.'
        GOTO 9999
      END IF
    ELSE
    END IF

  ELSE
    WRITE(6,'(/,"Plot Time : ",$)')
    READ(lun_in,*)pTime

    iTime = 0
    IF( pTime <= TimeOut(1)%time%runTime )THEN
      iTime = 1
      rTime = TimeOut(1)%time%runTime
    ELSE IF( pTime >= TimeOut(nTimeOut)%time%runTime )THEN
      IF( lUserTime )THEN
        iTime = nTimeOut+1
        rTime = MAX(minTime+1./3600.,MIN(pTime,maxTime))
      ELSE
        iTime = nTimeOut
        rTime = TimeOut(nTimeOut)%time%runTime
      END IF
    ELSE
      dtx = HUGE(1.0)
      DO i = lastID+1,nTimeOut
        dt = ABS(pTime - TimeOut(i)%time%runTime)
        IF( dt < dtx )THEN
          dtx = dt
          iTime = i
          rTime = TimeOut(i)%time%runTime
        END IF
      END DO
    END IF
  END IF
  IF( iTime <= lastTime )GOTO 9999
  IF( iTime < 1 )GOTO 9999

  IF( lUsertime .AND. iTime > nTimeOut )THEN
    WRITE(6,'("Using usertime: ",I7,1x,F10.4)')nTimeOut+1,rTime
  ELSE
    WRITE(6,'(F10.4,7X,"(",A,")")')TimeOut(iTime)%time%runTime,TimeOut(iTime)%string
  END IF

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! loadPlotTimes
!==============================================================================
LOGICAL FUNCTION loadPlotTimes( timeID,usertime,maxtime )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER, INTENT (IN ) :: timeID
INTEGER, INTENT (IN ) :: usertime
REAL,    INTENT (OUT) :: maxtime

maxtime = HUGE(1.0)
SELECT CASE( timeID )
  CASE( HP_SRFTIME )
    TimeOut  => TimeSrf
    nTimeOut = nTimeSrf
  CASE( HP_PUFFTIME )
    TimeOut  => TimePuff
    nTimeOut = nTimePuff
  CASE( HP_METTIME )
    TimeOut  => TimeMet
    nTimeOut = nTimeMet
  CASE DEFAULT
    nTimeOut = 0
END SELECT

loadPlotTimes = (usertime == SCIPtrue)

RETURN
END
