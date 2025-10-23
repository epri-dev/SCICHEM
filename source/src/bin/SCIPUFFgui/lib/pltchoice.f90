SUBROUTINE init_plot_choice( inputFile,SyncMode )

USE pltchoice_fi
USE errorParam_fd
USE SCIPtool

IMPLICIT NONE

CHARACTER(*) inputFile

TYPE( projectIDT ) :: Project

TYPE( SCIPFieldCoordinateT ) :: PrjCoord

INTEGER irv,ios,i,userID
INTEGER n
LOGICAL SyncMode

CHARACTER(32) who,what

LOGICAL, EXTERNAL :: hasError

!==============================================================================
!Deallocate plot choice arrays
!==============================================================================

CALL DeallocatePlotChoice()
IF( hasError() )GOTO 9999

!==============================================================================
!Set Project name/path
!==============================================================================

CALL SplitName( inputFile,Project%name,Project%path )
CALL RemoveExtension( Project%name )

!==============================================================================
!Get the number of plot types
!==============================================================================

userID = 8642
irv = SCIPNumPlotClasses( userID,Project,nPltClass,nPltChoice,nPltKind )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'AllocatePlot' )
  GOTO 9999
END IF

!==============================================================================
!Allocate plot choice arrays
!==============================================================================

who = 'Class strings'
ALLOCATE( ClassStr(nPltClass),STAT=ios )

IF( ios == 0 )THEN
  who = 'Choice strings'
  ALLOCATE( ChoiceStr(nPltChoice),STAT=ios )
END IF

IF( ios == 0 )THEN
  who = 'Kind strings'
  ALLOCATE( KindStr(nPltKind),STAT=ios )
END IF

IF( ios == 0 )THEN
  who = 'Category/Class array'
  ALLOCATE( CatClassArray(HP_NUMCAT,nPltClass),STAT=ios )
END IF

IF( ios == 0 )THEN
  who = 'Class/Choice array'
  ALLOCATE( ClassChoiceArray(nPltClass,nPltChoice),STAT=ios )
END IF

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR, &
                'Error allocating memory for '//TRIM(who), &
                'ALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'AllocatePlotChoice' )
  GOTO 9999
END IF

!==============================================================================
!Get Plot choice strings and arrays
!==============================================================================

irv = SCIPGetPlotClasses( userID,Project,ClassStr,ChoiceStr,KindStr, &
                          CatClassArray,ClassChoiceArray,PrjCoord )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'AllocatePlotChoice' )
  GOTO 9999
END IF

IF( SyncMode )GOTO 9999

!==============================================================================
!Deallocate plot time arrays
!==============================================================================

CALL DeallocatePlotTime( 0 )
IF( hasError() )GOTO 9999

!==============================================================================
!Get number of plot times
!==============================================================================

irv = SCIPNumPlotTimes( userID,Project,nTimePuff,nTimeSrf,nTimeMet,n )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'AllocatePlotTime' )
  GOTO 9999
END IF

!==============================================================================
!Allocate plot time arrays
!==============================================================================

who = 'Puff times'
ALLOCATE( TimePuff(nTimePuff),STAT=ios )

IF( ios == 0 )THEN
  who = 'Surface times'
  ALLOCATE( TimeSrf(nTimeSrf),STAT=ios )
END IF

IF( ios == 0 )THEN
  who = 'Meteorology times'
  ALLOCATE( TimeMet(nTimeMet),STAT=ios )
END IF

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR, &
                'Error allocating memory for '//TRIM(who), &
                'ALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'AllocatePlotTime' )
  GOTO 9999
END IF

!==============================================================================
!Get plot times
!==============================================================================

irv = SCIPGetPlotTimes( userID,Project,TimePuff,TimeSrf,TimeMet )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'AllocatePlotTime' )
  GOTO 9999
END IF

IF( nTimePuff > 0 )THEN
  DO i = 1,nTimePuff
    CALL format_time( TimePuff(i)%time%runTime,TimePuff(i)%string,0 )
  END DO
END IF

IF( nTimeSrf > 0 )THEN
  DO i = 1,nTimeSrf
    CALL format_time( TimeSrf(i)%time%runTime,TimeSrf(i)%string,0 )
  END DO
END IF

IF( nTimeMet > 0 )THEN
  DO i = 1,nTimeMet
    CALL format_time( TimeMet(i)%time%runTime,TimeMet(i)%string,0 )
  END DO
END IF

nTimeRad = 0
IF( nTimeSrf > 0 )THEN
  DO i = 1,nTimeSrf
    IF( TimeSrf(i)%time%runTime <= RadTimeMax )nTimeRad = nTimeRad + 1
  END DO
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE DeallocatePlotChoice()

USE pltchoice_fi
USE errorParam_fd

IMPLICIT NONE

CHARACTER(32) who,what
INTEGER       ios

!==============================================================================
!Deallocate plot choice
!==============================================================================

ios = 0

IF( ALLOCATED(ClassStr) )THEN
  who = 'Class strings'
  DEALLOCATE( ClassStr,STAT=ios )
END IF

IF( ALLOCATED(ChoiceStr) )THEN
  who = 'Choice strings'
  DEALLOCATE( ChoiceStr ,STAT=ios )
END IF

IF( ALLOCATED(KindStr) )THEN
  who = 'Kind strings'
  DEALLOCATE( KindStr ,STAT=ios )
END IF

IF( ALLOCATED(CatClassArray) )THEN
  who = 'Category/Class array'
  DEALLOCATE( CatClassArray,STAT=ios )
END IF

IF( ALLOCATED(ClassChoiceArray) )THEN
  who = 'Class/Choice array'
  DEALLOCATE( ClassChoiceArray,STAT=ios )
END IF

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR, &
                'Error deallocating memory for '//TRIM(who), &
                'DEALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'DeallocatePlotChoice' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE DeallocatePlotTime( iflag )

USE pltchoice_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER iflag

CHARACTER(32) who,what
INTEGER       ios

LOGICAL doPuff
LOGICAL doSrf
LOGICAL doMet
LOGICAL doRad
LOGICAL doPlot

!==============================================================================
!Set Deallocation flags
!==============================================================================

doPuff = .FALSE.
doSrf  = .FALSE.
doMet  = .FALSE.
doRad  = .FALSE.
doPlot = .FALSE.

SELECT CASE( iflag )
  CASE( HP_PUFFTIME )
    doPuff = .TRUE.

  CASE( HP_SRFTIME )
    doSrf = .TRUE.

  CASE( HP_METTIME )
    doMet = .TRUE.

  CASE( - 1)
    doPlot = .TRUE.

  CASE DEFAULT
    doPuff = .TRUE.
    doSrf  = .TRUE.
    doMet  = .TRUE.
    doRad  = .TRUE.
    doPlot = .FALSE.

END SELECT

ios = 0

!==============================================================================
!Deallocate plot times
!==============================================================================

IF( doPuff )THEN
  IF( ALLOCATED(TimePuff) )THEN
    who = 'Puff times'
    DEALLOCATE( TimePuff,STAT=ios )
  END IF
END IF

IF( doSrf )THEN
  IF( ALLOCATED(TimeSrf) )THEN
    who = 'Surface times'
    DEALLOCATE( TimeSrf,STAT=ios )
  END IF
END IF

IF( doMet )THEN
  IF( ALLOCATED(TimeMet) )THEN
    who = 'Meteorology times'
    DEALLOCATE( TimeMet,STAT=ios )
  END IF
END IF

IF( doRad )THEN
  IF( ALLOCATED(TimeRad) )THEN
    who = 'SCIP Radiation times'
    DEALLOCATE( TimeRad,STAT=ios )
  END IF
END IF

IF( doPlot )THEN
  IF( ALLOCATED(TimePlot) )THEN
    who = 'Meteorology times'
    DEALLOCATE( TimePlot,STAT=ios )
  END IF
END IF

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR, &
                'Error deallocating memory for '//TRIM(who), &
                'DEALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'DeallocatePlotTime' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE ExitPlot()

USE pltchoice_fi

IMPLICIT NONE

INTEGER ios

!==============================================================================
!Deallocate plot choice arrays
!==============================================================================

CALL DeallocatePlotChoice()

!==============================================================================
!Deallocate plot time arrays
!==============================================================================

CALL DeallocatePlotTime(  0 )
CALL DeallocatePlotTime( -1 )

!==============================================================================
!Deallocate plot fields
!==============================================================================

CALL FreePlotField( 0 )

!==============================================================================
!Deallocate contour arrays
!==============================================================================

CALL DeallocateContours( 0,0 )

!==============================================================================
!Deallocate plot contour arrays
!==============================================================================

IF( ASSOCIATED(ContourPlot%ListPtr) )DEALLOCATE( ContourPlot%ListPtr,STAT=ios )
IF( ASSOCIATED(ContourTer%ListPtr ) )DEALLOCATE( ContourTer%ListPtr, STAT=ios )

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE DeallocateContours( iflag,indx )

USE pltchoice_fi
USE errorParam_fd
USE GUIparam_fd

IMPLICIT NONE

INTEGER iflag
INTEGER indx

CHARACTER(32) who,what
INTEGER       ios

LOGICAL doUser
LOGICAL doTer
LOGICAL doNullify

INTEGER Sindx
INTEGER Eindx
INTEGER i
INTEGER j

!==============================================================================
!Set Deallocation flags
!==============================================================================

doUser = .FALSE.
doTer  = .FALSE.

SELECT CASE( iflag )
  CASE( USER_CONTOUR )
    doUser = .TRUE.

  CASE( TER_CONTOUR )
    doTer = .TRUE.

  CASE DEFAULT
    doUser = .TRUE.
    doTer  = .TRUE.

END SELECT

IF( indx == DEFAULT_LEVEL )THEN
    Sindx = indx
    Eindx = indx

ELSE IF( indx == BASE_LEVEL )THEN
    Sindx = indx
    Eindx = indx

ELSE IF( indx == EDIT_LEVEL )THEN
    Sindx = indx
    Eindx = indx

ELSE
    Sindx = DEFAULT_LEVEL
    Eindx = EDIT_LEVEL

ENDIF

ios = 0

!==============================================================================
!Deallocate contour arrays
!==============================================================================

DO i = Sindx,Eindx

  IF( doUSER .AND. ios == 0 )THEN
    IF( ASSOCIATED(ContourList(USER_CONTOUR,i)%ListPtr) )THEN
	  doNullify = .FALSE.
      DO j = DEFAULT_LEVEL,EDIT_LEVEL
	      IF( j /= i )THEN
		      doNullify = doNullify .OR. &
		                  ASSOCIATED(ContourList(USER_CONTOUR,i)%ListPtr, &
					                       ContourList(USER_CONTOUR,j)%ListPtr)
		    END IF
	    END DO
	    IF( doNullify )THEN
	      NULLIFY(ContourList(USER_CONTOUR,i)%ListPtr)
		    ContourList(USER_CONTOUR,i)%ListHdr%Number = 0
	    ELSE
        who = 'USER Plot contour array'
        DEALLOCATE( ContourList(USER_CONTOUR,i)%ListPtr,STAT=ios )
	      IF( ios == 0 )ContourList(USER_CONTOUR,i)%ListHdr%Number = 0
	    END IF
    END IF
  END IF

  IF( doTer .AND. ios == 0 )THEN
    IF( ASSOCIATED(ContourList(TER_CONTOUR,i)%ListPtr) )THEN
	    doNullify = .FALSE.
      DO j = DEFAULT_LEVEL,EDIT_LEVEL
	      IF( j /= i )THEN
		      doNullify = doNullify .OR. &
		                  ASSOCIATED(ContourList(TER_CONTOUR,i)%ListPtr, &
					                       ContourList(TER_CONTOUR,j)%ListPtr)
		    END IF
	    END DO
	    IF( doNullify )THEN
	      NULLIFY(ContourList(TER_CONTOUR,i)%ListPtr)
		    ContourList(TER_CONTOUR,i)%ListHdr%Number = 0
	    ELSE
        who = 'Terrain Overlay contour array'
        DEALLOCATE( ContourList(TER_CONTOUR,i)%ListPtr,STAT=ios )
	      IF( ios == 0 )ContourList(TER_CONTOUR,i)%ListHdr%Number = 0
	    END IF
    END IF
  END IF

END DO

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR, &
                'Error deallocating memory for '//TRIM(who), &
                'DEALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'DeallocateContour' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE FreePlotField( iflag )

USE pltchoice_fi
USE errorParam_fd
USE GUIparam_fd
USE SCIPtool

IMPLICIT NONE

INTEGER iflag

INTEGER irv
INTEGER i
INTEGER is
INTEGER ie
INTEGER userID

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

!==============================================================================
!See loop parameters
!==============================================================================

userID = 1111

SELECT CASE( iflag )
  CASE( FLD_INDEX )
    is = iflag
	  ie = iflag

  CASE( TER_INDEX )
    is = iflag
	  ie = iflag

  CASE DEFAULT
    is = 1
    ie = NUM_FIELDS

END SELECT

!==============================================================================
!ask SCIPtool to deallocate field
!==============================================================================

DO i = is,ie

  IF( FieldID(i) /= 0 .AND. FieldID(i) /= -1 )THEN

    irv = SCIPDeleteField( userID,FieldID(i) )
    IF( irv /= SCIPsuccess .OR. FieldID(i) /= 0 )THEN

      nError   = UK_ERROR
      eRoutine = 'DeallocatePlotField'
      eInform  = 'SCIPDeleteField failure'

      SELECT CASE( i )

        CASE( FLD_INDEX )
          eMessage = 'Error deallocating memory for Plot field'

        CASE( TER_INDEX )
          eMessage = 'Error deallocating memory for Terrain field'

        CASE DEFAULT
          eMessage = 'Error deallocating memory for Unknown field'

      END SELECT

      eAction = ' '
      CALL SetError( nError,eMessage,eInform,eAction,eRoutine )

    END IF

    SELECT CASE( i )

      CASE( FLD_INDEX )
        PlotDef(BASE_LEVEL)%Created = .FALSE.
        PlotDef(EDIT_LEVEL)%Created = .FALSE.

      CASE( TER_INDEX )
        PlotTer%Created = .FALSE.

      CASE DEFAULT

    END SELECT

  ELSE

    FieldID(i) = 0

  END IF

END DO

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE ResetContours( icont,indx )

USE pltchoice_fi
USE GUIparam_fd

IMPLICIT NONE

INTEGER indx
INTEGER icont
INTEGER mode

IF( indx >= DEFAULT_LEVEL .AND. indx <= EDIT_LEVEL )THEN

  IF( indx /= DEFAULT_LEVEL )THEN

    SELECT CASE( icont )

      CASE( USER_CONTOUR )
        CALL DeallocateContours( USER_CONTOUR,indx )
        ContourList(USER_CONTOUR,indx)  = ContourList(USER_CONTOUR,DEFAULT_LEVEL)
        ContourBuild(USER_CONTOUR,indx) = ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)

      CASE( TER_CONTOUR )
        CALL DeallocateContours( TER_CONTOUR,indx )
        mode = ContourList(TER_CONTOUR,indx)%ListHdr%DrawMode
        ContourList(TER_CONTOUR,indx) = ContourList(TER_CONTOUR,DEFAULT_LEVEL)
        IF( mode == PLOT_NULL )ContourList(TER_CONTOUR,indx)%ListHdr%DrawMode = mode
        ContourBuild(TER_CONTOUR,indx) = ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)

      CASE DEFAULT

    END SELECT

  END IF

END IF

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE DefaultContours( iflag,indx )

USE pltchoice_fi

IMPLICIT NONE

INTEGER iflag
INTEGER indx

INTEGER i

LOGICAL DoReset

DoReset = .FALSE.

SELECT CASE( iflag )

  CASE( USER_CONTOUR )
    CALL ResetContours( USER_CONTOUR,indx )

  CASE( TER_CONTOUR )
    CALL ResetContours( TER_CONTOUR,indx )

  CASE DEFAULT
    DO i = 1,NUM_CONTOUR
      CALL ResetContours( i,indx)
	  END DO
	  DoReset = .TRUE.

END SELECT

IF( DoReset )PlotDef(indx)%ContourIndex = USER_CONTOUR

RETURN
END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
SUBROUTINE SetContours( iflag,fID,cList )

USE pltchoice_fi
USE errorParam_fd
USE tooluser_fd
USE GUIparam_fd
USE SCIPtool

IMPLICIT NONE

INTEGER iflag
INTEGER fID

INTEGER ios
INTEGER i

TYPE( SCIPContourElementList ) :: cList
TYPE( ContourGenerateT )       :: cGen

LOGICAL NeedField

REAL dmin,dmax,fmin,fmax,dmn,dmx

INTEGER, EXTERNAL :: GenerateContours

INTEGER UserID

CHARACTER(32) what

!==============================================================================
!DEALLOCATE Existing list
!==============================================================================

UserID = 1357

IF( ASSOCIATED(cList%ListPtr) )THEN

  DEALLOCATE( cList%ListPtr,STAT=ios )
  IF( ios /= 0 )GOTO 9999

  cList%ListHdr%Number = 0

END IF

!==============================================================================
!Check to see if DrawMode is ON
!==============================================================================

cList%ListHdr%DrawMode = PLOT_NULL

IF( ContourList(iflag,BASE_LEVEL)%ListHdr%DrawMode == PLOT_ON )THEN

!==============================================================================
!If DrawMode = ON : Check for USER Defined contour list
!==============================================================================

  IF( ContourList(iflag,BASE_LEVEL)%ListHdr%Number > 0 )THEN

!==============================================================================
!If Mode = USER : Check for USER Defined contour list
!==============================================================================

    IF( ASSOCIATED(ContourList(iflag,BASE_LEVEL)%ListPtr) )THEN

!==============================================================================
!Valid List : Copy to contour list
!==============================================================================

      cList%ListHdr%Number = ContourList(iflag,BASE_LEVEL)%ListHdr%Number
	    ALLOCATE( cList%ListPtr(cList%ListHdr%Number),STAT=ios )
	    IF( ios /= 0 )GOTO 9997
	    DO i = 1,cList%ListHdr%Number
	      cList%ListPtr(i) = ContourList(iflag,BASE_LEVEL)%ListPtr(i)
	    END DO

    ELSE

!==============================================================================
!Invalid List : Check for default list
!==============================================================================

      IF( ASSOCIATED(ContourList(iflag,DEFAULT_LEVEL)%ListPtr) )THEN

!==============================================================================
!Valid Default List : Copy to contour list
!==============================================================================

        cList%ListHdr%Number = ContourList(iflag,DEFAULT_LEVEL)%ListHdr%Number
	      ALLOCATE( cList%ListPtr(cList%ListHdr%Number),STAT=ios )
	      IF( ios /= 0 )GOTO 9997
	      DO i = 1,cList%ListHdr%Number
	        cList%ListPtr(i) = ContourList(iflag,DEFAULT_LEVEL)%ListPtr(i)
	      END DO

      ELSE

!==============================================================================
!Invalid Default List : Error
!==============================================================================

        GOTO 9998

	    END IF

    END IF

    cList%ListHdr%DrawMode = PLOT_USER

!==============================================================================
!If Mode /= USER : Generate contours
!==============================================================================

  ELSE

!==============================================================================
!Setup Generation structure
!==============================================================================

    cGen = ContourBuild(iflag,BASE_LEVEL)

!==============================================================================
!Determine spacing type if current value = DEFAULT
!==============================================================================

    IF( cGen%Mode == PLOT_DEF )THEN

!==============================================================================
!Spacing Type = Default : Set based on plot type
!                         Terrain Overlay = Linear
!                         Probability     = Linear
!                         Terrain/Met     = Linear
!                         Hazard Areas    = Linear (Should be USER but just in case)
!                         All others      = Log
!==============================================================================

	    SELECT CASE( iflag )

	      CASE( TER_CONTOUR )
          cGen%Mode = PLOT_LIN

		    CASE DEFAULT
          IF( CatClassArray(PlotDef(BASE_LEVEL)%Field%Category,PlotDef(BASE_LEVEL)%Field%Class)%type == SCIPtrue )THEN
            IF( PlotDef(BASE_LEVEL)%Type == HP_PROB )THEN
   	          cGen%Mode = PLOT_LIN
            ELSE
              IF( PlotDef(BASE_LEVEL)%Field%interpType == SCIPon )THEN
   	            cGen%Mode = PLOT_LIN
              ELSE
   	            cGen%Mode = PLOT_LOG
              END IF
		        END IF
		      ELSE
            IF( PlotDef(BASE_LEVEL)%Field%interpType == SCIPon )THEN
 	            cGen%Mode = PLOT_LIN
            ELSE
 	            cGen%Mode = PLOT_LOG
            END IF
		      END IF

      END SELECT

	  END IF

!==============================================================================
!Determine contour limits if current value = DEFAULT
!==============================================================================

	  NeedField = ( cGen%Cmin == DEF_VAL_R ) .OR. ( cGen%Cmin == NOT_SET_R ) .OR. &
	              ( cGen%Cmax == DEF_VAL_R ) .OR. ( cGen%Cmax == NOT_SET_R )

    IF( NeedField )THEN

!==============================================================================
!Need Field to determine limits - Check for Valid Field
!==============================================================================

      IF( fID /= 0 )THEN

!==============================================================================
!Valid Field - Get limits
!==============================================================================

        IF( iflag == TER_CONTOUR )THEN
          i = SCIPGetFieldMinMax( UserID,fID,TerType,dmin,dmax,fmin,fmax,dmn,dmx)
        ELSE
          i = SCIPGetFieldMinMax( UserID,fID,DrawType,dmin,dmax,fmin,fmax,dmn,dmx)
        END IF
		    IF( i /= SCIPsuccess )GOTO 9994

!==============================================================================
!Reser Cmin/Cmax based on field limits
!==============================================================================

		    IF(( cGen%Cmin == DEF_VAL_R ) .OR. ( cGen%Cmin == NOT_SET_R ))THEN
		      cGen%Cmin = dmn !*ContourList(iflag,BASE_LEVEL)%ListHdr%Scale
          cGen%number = -cGen%number
		    END IF
		    IF(( cGen%Cmax == DEF_VAL_R ) .OR. ( cGen%Cmax == NOT_SET_R ))THEN
		      cGen%Cmax = dmx !*ContourList(iflag,BASE_LEVEL)%ListHdr%Scale
		    END IF

	    ELSE

!==============================================================================
!Invalid Field - Error
!==============================================================================

	      GOTO 9996

	    END IF

	  END IF

!==============================================================================
!Generate contours
!==============================================================================

    ios = GenerateContours( cGen,cList,ContourList(iflag,BASE_LEVEL)%ListHdr%Scale )
	  IF( ios /= 0 )GOTO 9995

	  cList%ListHdr%DrawMode = cGen%Mode

  END IF

!==============================================================================
!Set proper scaling/units
!==============================================================================

  cList%ListHdr%Unit      = ContourList(iflag,BASE_LEVEL)%ListHdr%Unit
  cList%ListHdr%Scale     = ContourList(iflag,BASE_LEVEL)%ListHdr%Scale
  cList%ListHdr%LabelMode = ContourList(iflag,BASE_LEVEL)%ListHdr%LabelMode

END IF

1000 CONTINUE

RETURN

!==============================================================================
!Deallocation error
!==============================================================================

9999 CONTINUE

WRITE(what,*)ios
CALL SetError( UK_ERROR,'Error Deallocating local contour list memory', &
               'DEALLOCATE error = '//TRIM(ADJUSTL(what)), &
               'No contours generated','SetContours' )

GOTO 1000

!==============================================================================
!Invalid list error
!==============================================================================

9998 CONTINUE

CALL SetError( UK_ERROR,'USER specifed contour list with invalid list', &
               'No contours generated',' ','SetContours' )

GOTO 1000

!==============================================================================
!Allocation error
!==============================================================================

9997 CONTINUE

WRITE(what,*)ios
CALL SetError( UK_ERROR,'Error Allocating local contour list memory', &
               'ALLOCATE error = '//TRIM(ADJUSTL(what)), &
               'No contours generated','SetContours' )

GOTO 1000

!==============================================================================
!InValid Field error
!==============================================================================

9996 CONTINUE

CALL SetError( UK_ERROR, &
              'Error - Invalid SAG Field ID', &
              'Field needed to determine limits for contour generation', &
              'No contours generated', &
              'SetContours' )

GOTO 1000

!==============================================================================
!Contour Generation Error
!==============================================================================

9995 CONTINUE

SELECT CASE( ios )

  CASE( -100,-101 )
    CALL SetError( UK_ERROR, &
                  'Error generating contour', &
                  'No real field values available', &
                  'Check field request is above ground level', &
                  'SetContours' )

  CASE DEFAULT
    WRITE(what,*)ios
    CALL SetError( UK_ERROR, &
                  'Error generating contour', &
                  'Contour generation error = '//TRIM(ADJUSTL(what)), &
                  'No contours generated', &
                  'SetContours' )
END SELECT
GOTO 1000

!==============================================================================
!SCIPtool Error
!==============================================================================

9994 CONTINUE

CALL GetToolError( 'SetContour' )

GOTO 1000

END
!******************************************************************************
!******************************************************************************
!
!******************************************************************************
!******************************************************************************
INTEGER FUNCTION GenerateContours( cGen,cList,Scale )

USE pltchoice_fi
USE errorParam_fd

IMPLICIT NONE

REAL,    PARAMETER :: SMALL         = 1.E-30
INTEGER, PARAMETER :: SUGGESTED_MAX = 20

TYPE(SCIPContourElementList)  :: cList
TYPE(ContourGenerateT)        :: cGen
REAL                          :: Scale

INTEGER ios

REAL cMin
REAL cMax
REAL cInc
REAL p
REAL rInc

INTEGER nc
INTEGER i
LOGICAL useNC

CHARACTER(16) string1,string2

LOGICAL, EXTERNAL :: hasError

!==============================================================================
!Set Approximate number of contours
!==============================================================================

nc = MAX(1,ABS(cGen%Number))

useNC = cGen%Number < 0 .AND. cGen%Mode == PLOT_LOG

!==============================================================================
!Set Min/Max
!==============================================================================

cMin = cGen%Cmin*Scale
cMax = cGen%Cmax*Scale

!==============================================================================
!Check Min/Max - Error if not set
!==============================================================================

IF( cMax == DEF_VAL_R  .OR. cMax == NOT_SET_R )THEN
  GenerateContours = -100
  GOTO 1000
END IF

IF( cMin == DEF_VAL_R  .OR. cMin == NOT_SET_R )THEN
  GenerateContours = -101
  GOTO 1000
END IF

!==============================================================================
!Swap Min/Max if necessary
!==============================================================================

IF( cMin > cMax )THEN
  cMin = cGen%Cmax
  cMax = cGen%Cmin
END IF

!==============================================================================
!Check for constant value
!==============================================================================

IF( ABS(cMax-cMin)/MAX(cMax,cMin,SMALL) <= EPSILON(SMALL) )GOTO 3000

!==============================================================================
!Take log if log spacing
!==============================================================================

IF( cGen%Mode == PLOT_LOG )THEN
  cMax = LOG10( MAX( cMax,SMALL ) )
  cMin = LOG10( MAX( cMin,SMALL ) )
END IF

!==============================================================================
!Set Inc - Based on approximate number of contours
!==============================================================================

IF( cGen%Del == DEF_VAL_R  .OR. cGen%Del == NOT_SET_R .OR. useNC )THEN

!==============================================================================
! Limit cMin for log spacing
!==============================================================================

  IF( cGen%Mode == PLOT_LOG )THEN
    IF( cGen%Del == DEF_VAL_R  .OR. cGen%Del == NOT_SET_R )THEN
      rInc = 1.0
    ELSE
      rInc = LOG10(cGen%Del)
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

  IF( cGen%Mode == PLOT_LOG )THEN
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

  cInc = ABS(cGen%Del)
  IF( cGen%Mode == PLOT_LOG )THEN
    cInc = LOG10( MAX( cInc, SMALL ) )
  ELSE
    cInc = cInc*Scale
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
  WRITE(string1,*)SUGGESTED_MAX
  WRITE(string2,*)nc
  CALL SetError( WN_ERROR, &
                 'WARNING - Contour generator is requesting a large number of contours', &
                 'Requested = '//TRIM(ADJUSTL(string2))//' : Suggested MAX = '//TRIM(ADJUSTL(string1)), &
                 'Do you want to continue with the requested number?', &
                 'GenerateContours' )
  CALL ShowWarningMessage( 0,.TRUE. )
  IF( hasError() )THEN
    CALL InitError()
	  nc   = SUGGESTED_MAX
	  cMin = cMax - FLOAT(nc-1)*cInc
  END IF
END IF

!==============================================================================
!Allocate space and Compute contours
!==============================================================================

ALLOCATE( cList%ListPtr(nc),STAT=ios )
IF( ios /= 0 )THEN
  GenerateContours = ios
  GOTO 1000
END IF

cList%ListHdr%Number = nc
DO i = 1,cList%ListHdr%Number
  cList%ListPtr(i)%Contour = (cMin + FLOAT(i-1)*cInc)
END DO

!==============================================================================
!Set return value to SUCCESS
!==============================================================================

2000 CONTINUE

GenerateContours = 0

!==============================================================================
!Log spacing - transform back to real numbers
!==============================================================================

IF( cGen%Mode == PLOT_LOG )THEN
  DO i = 1,cList%ListHdr%Number
    cList%ListPtr(i)%Contour = (10**cList%ListPtr(i)%Contour)/Scale
  END DO
ELSE
  DO i = 1,cList%ListHdr%Number
    cList%ListPtr(i)%Contour = cList%ListPtr(i)%Contour/Scale
  END DO
END IF

1000 CONTINUE

RETURN

!==============================================================================
!Single contour
!==============================================================================

3000 CONTINUE

ALLOCATE( cList%ListPtr(1),STAT=ios )
IF(ios /= 0)THEN
  GenerateContours = ios
  GOTO 1000
END IF
cList%ListHdr%Number     = 1
cList%ListPtr(1)%Contour = cGen%Cmax
IF( cGen%Mode == PLOT_LOG )THEN
  cList%ListPtr(1)%Contour = LOG10( MAX( cList%ListPtr(1)%Contour, SMALL ) )
END IF

GOTO 2000

END
