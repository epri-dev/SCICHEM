!==============================================================================
! CreateContours
!==============================================================================
SUBROUTINE CreateContours( fID,thisField,dmx,dmn )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER                :: fID
TYPE( SCIPPlotFieldT ) :: thisField
REAL                   :: dmx
REAL                   :: dmn

INTEGER irv

irv = SCIPContourCount( callerID,fID,thisField,plotType,contourHead,contourList,contourMode,nLine,nPoint )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in SCIPContourField' )
  GOTO 9999
END IF

IF( nPoint <= 0 )THEN
  nError   = SZ_ERROR
  eMessage = 'No contour at requested level'
  WRITE(eInform,'(A,F0.5,F0.5)')'Field range ('//TRIM(Field%units)//'):',dmn,dmx
  GOTO 9999
ELSE
  CALL allocateContour( nPoint,nLine )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

irv = SCIPContourField( callerID,fID,thisField,plotType,contourHead,contourList,contourMode,cLines,cPoints )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in SCIPContourField' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!======================================================================
! WriteContours
!======================================================================
SUBROUTINE WriteContours( file )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

TYPE( fileNameT ) file

INTEGER irv,ncom
TYPE( char128T ), DIMENSION(25) :: comment

ContourWrite%filename = file%string

CALL GetWriteHeader( ncom,comment )

irv = SCIPWriteField( callerID,FieldIds(1),Fields(1),PlotType, &
                      contourHead,contourList,ContourWrite,ncom,comment )

IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in WriteContours' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
