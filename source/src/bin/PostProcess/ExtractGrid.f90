!==============================================================================
! ExtractData
!==============================================================================
SUBROUTINE ExtractGridData()

USE Extract_fi

IMPLICIT NONE

IF( output3D )THEN

  CALL CreateGrid()
  IF( nError/= NO_ERROR )GOTO 9999

  CALL ExtractOutput3D()
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  IF( GridType == NATIVE )THEN

    CALL ExtractNative( FieldIds(1),Fields(1) )
    IF( nError/= NO_ERROR )GOTO 9999

  ELSE

    CALL CreateGrid()
    IF( nError/= NO_ERROR )GOTO 9999

    CALL ExtractGrid( FieldIds(1),Fields(1),1,hasVariance )
    IF( nError /= NO_ERROR )GOTO 9999

    FldMax(2) = MAXVAL(mFldGrd)
    FldMin(2) = MINVAL(mFldGrd)

  END IF

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! ExtractRegular
!==============================================================================
SUBROUTINE ExtractGrid( fID,thisField,index,doVariance )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER                :: fID
TYPE( SCIPPlotFieldT ) :: thisField
INTEGER                :: index
LOGICAL                :: doVariance

INTEGER irv

irv = SCIPGetFieldValues( callerID,fID,thisField,plotType,nxy,xGrd,yGrd,mFldGrd )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in SCIPGetFieldValues' )
  GOTO 9999
END IF
dFldGrd(1:nxy,index,1) = mFldGrd(1:nxy)

IF( doVariance .AND. plotType%type == HP_MEAN )THEN
  plotType%type = HP_VARIANCE
  irv = SCIPGetFieldValues( callerID,fID,thisField,plotType,nxy,xGrd,yGrd,vFldGrd )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error in SCIPGetFieldValues' )
    GOTO 9999
  END IF
  dFldGrd(1:nxy,index,2) = vFldGrd(1:nxy)
  plotType%type = HP_MEAN
END IF

9999 CONTINUE

RETURN
END
