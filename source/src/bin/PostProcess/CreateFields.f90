!==============================================================================
! createFields
!==============================================================================
SUBROUTINE createFields()

USE Extract_fi

IMPLICIT NONE

INTEGER, EXTERNAL :: PostCreateField

IF( output3D )THEN

  CALL CreateOutput3D()
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  CALL allocateFields( 1 )
  IF( nError /= NO_ERROR )GOTO 9999

  Fields(1) = Field
  FieldIds(1) = PostCreateField( Fields(1),ClassData,.TRUE.,'Plot' )
  IF( FieldIds(1) < 0 )GOTO 9999

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! createFields
!==============================================================================
INTEGER FUNCTION PostCreateField( thisField,thisData,doMax,header )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

TYPE( SCIPPlotFieldT ) :: thisField
REAL, DIMENSION(*)     :: thisData
LOGICAL                :: doMax
CHARACTER(*)           :: header

INTEGER fID, irv
REAL    dmin, dmax, fmin, fmax, dmn, dmx

PostCreateField = -1

fID = SCIPCreateField( callerID,thisField,thisData )
IF( fID <= 0 )THEN
  CALL toolError( 'Error retrieving '//TRIM(header)//' field from SCIPtool' )
  GOTO 9999
END IF

IF( doMax )THEN
  irv = SCIPGetFieldMinMax( callerID,fID,plotType,dmin,dmax,fmin,fmax,dmn,dmx)
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error in SCIPGetFieldMinMax' )
    GOTO 9999
  END IF
  FldMin(1) = dmn
  FldMax(1) = dmx

  IF( BTEST(ClassChoiceArray(thisField%class,thisField%choice)%available,HPB_ASSOC_LINE) )THEN
    CALL getAssociatedData( fID )
    IF( nError /= 0 )GOTO 9999
  END IF

END IF

PostCreateField = fID


9999 CONTINUE

RETURN
END
!==============================================================================
! getAssociatedData
!==============================================================================
SUBROUTINE getAssociatedData( fID )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER fID

INTEGER irv, j, k, ios
INTEGER nPlots, nLines, nPoints

TYPE( SCIPPointT ), DIMENSION(:), ALLOCATABLE :: AssocPoints
TYPE( SCIPLineT  ), DIMENSION(:), ALLOCATABLE :: AssocLines
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocTitles
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocAxes
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocLineIDs

irv = SCIPGetFieldAssocSize( callerID,fID,nPlots,nLines,nPoints )
IF( irv /= SCIPsuccess )THEN
  CALL toolError('Error getting assocaied field sizes')
  GOTO 9999
END IF

ALLOCATE( AssocPoints(nPoints),STAT=ios )
IF( ios == 0 )ALLOCATE( AssocLines(nLines),STAT=ios )
IF( ios == 0 )ALLOCATE( AssocTitles(nPlots),STAT=ios )
IF( ios == 0 )ALLOCATE( AssocAxes(nPlots),STAT=ios )
IF( ios == 0 )ALLOCATE( AssocLineIDs(nLines),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSrcFuncAssociatedData'
  eMessage = 'Error : associated data, Fields arrays'
  WRITE(eInform,'(A,I0,I0,I0,A,I0)')'Requests=',nplots,nLines,nPoints,' : error =',ios
  GOTO 9999
END IF

irv = SCIPGetFieldAssocData( callerID,fID,AssocLines,AssocPoints, &
                             AssocTitles,AssocAxes,AssocLineIDs )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error getting assocaied fields' )
  GOTO 9999
END IF

DO k = 1,nPlots
  WRITE(99,*) TRIM(AssocTitles(k)%string)
  WRITE(99,*) TRIM(AssocAxes(k)%string)
  DO j = AssocLines(k)%start,AssocLines(k)%start + AssocLines(k)%number - 1
    WRITE(99,*) AssocPoints(j)%x,AssocPoints(j)%y
  END DO
END DO

9999 CONTINUE

IF( ALLOCATED(AssocPoints ) )DEALLOCATE( AssocPoints, STAT=irv )
IF( ALLOCATED(AssocLines  ) )DEALLOCATE( AssocLines,  STAT=irv )
IF( ALLOCATED(AssocTitles ) )DEALLOCATE( AssocTitles, STAT=irv )
IF( ALLOCATED(AssocAxes   ) )DEALLOCATE( AssocAxes,   STAT=irv )
IF( ALLOCATED(AssocLineIDs) )DEALLOCATE( AssocLineIDs,STAT=irv )

RETURN
END
