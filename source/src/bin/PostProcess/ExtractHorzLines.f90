!======================================================================
! ExtractHorzLines
!======================================================================
SUBROUTINE ExtractHorzLines()

USE Extract_fi

IMPLICIT NONE

CALL SelectHorzLines()
IF( nError/= NO_ERROR )GOTO 9999

CALL GetHorzLines()
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectHorzLines()

USE Extract_fi

IMPLICIT NONE

CALL SelectNumberHorzLines()
IF( nError/= NO_ERROR )GOTO 9999

CALL SelectNumberLinePoints()
IF( nError/= NO_ERROR )GOTO 9999

CALL SelectHorzLineLocations()
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectNumberHorzLines()

USE Extract_fi

IMPLICIT NONE

INTEGER itry, ios, input

CHARACTER(8) string

itry = 0

loop: DO
  WRITE(6,'(/,A,$)')'Desired number of Horizontal Lines [1-256]? '
  READ(lun_in,'(A)',IOSTAT=ios)string
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberHorzLines'
    eMessage = 'Error reading number of horizontal lines'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF

  itry = itry + 1
  string = ADJUSTR(string)
  READ(string,*,IOSTAT=ios)input
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberHorzLines'
    eMessage = 'Error getting number of horizontal lines from input'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF
  IF( input < 1 .OR. input > 256 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(/,(A))')'Invalid number of horizontal lines (n=[1,256]): '//TRIM(string)//'. Try again'
      CYCLE
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectNumberHorzLines'
      eMessage = 'Invalid number of horizontal lines : '//TRIM(string)
      GOTO 9999
    END IF
  ELSE
    EXIT loop
  END IF
END DO loop

WRITE(6,'(/,A,I0)')'Using number of horizontal lines: ',input

nHorzLines = input

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectNumberLinePoints()

USE Extract_fi

IMPLICIT NONE

INTEGER itry, ios, input

CHARACTER(8) string

itry = 0

loop: DO
  WRITE(6,'(/,A,$)')'Desired number of points per line [1-1024]? '
  READ(lun_in,'(A)',IOSTAT=ios)string
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberLinePoints'
    eMessage = 'Error reading number of pointe per line'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF

  itry = itry + 1
  string = ADJUSTR(string)
  READ(string,*,IOSTAT=ios)input
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberLinePoints'
    eMessage = 'Error getting number of points per line from input'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF
  IF( input < 1 .OR. input > 1024 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(/,(A))')'Invalid number of points per line (n=[1,1024]): '//TRIM(string)//'. Try again'
      CYCLE
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectNumberLinePoints'
      eMessage = 'Invalid number of points per line : '//TRIM(string)
      GOTO 9999
    END IF
  ELSE
    EXIT loop
  END IF
END DO loop

WRITE(6,'(/,A,I0)')'Using number of points per line: ',input

nLinePoints = input

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectHorzLineLocations()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER irv, ios, itry, i, j, nn

REAL, DIMENSION(2) ::  x

CHARACTER(128) stringa, stringb, stringc, stringd

INTEGER, EXTERNAL :: getInput

irv = SCIPGetFieldDomain( callerID,FieldIDs(1),nx,nz,xMin,yMin,dx,dy )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error from SCIPGetFieldDomain in SelectHorzLinesLocations' )
  GOTO 9999
END IF

xMax = xMin + nx*dx
yMax = yMin + ny*dy

nxy = nHorzLines*nLinePoints

ALLOCATE( xGrd(nxy), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SelectProfileLocations'
  eMessage = 'Error allocating xGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxy,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( yGrd(nxy), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SelectProfileLocations'
  eMessage = 'Error allocating yGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxy,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( horzLineType(nHorzLines), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SelectProfileLocations'
  eMessage = 'Error allocating line types'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nHorzLines,' : Error=',irv
  GO TO 9999
END IF

WRITE(stringa,*)xMin
WRITE(stringb,*)xMax
WRITE(stringc,*)yMin
WRITE(stringd,*)yMax
WRITE(6,'(/,A)')'Line locations should be in X=['//TRIM(ADJUSTL(stringa))//','//TRIM(ADJUSTL(stringb))//']'// &
                                         ' Y=['//TRIM(ADJUSTL(stringc))//','//TRIM(ADJUSTL(stringd))//'].'

nn = (nLinePoints-1)*nHorzLines
DO i = 1,nHorzLines
  ios  = 1
  itry = 0
  WRITE(stringa,'(A,I3,A)')'line ',i,' starting point'
  DO WHILE( ios > 0 )
    itry = itry + 1
    ios = getInput( stringa,2,x )
    IF( ios > 0 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'No default available, try again'
      ELSE
        nError   = UK_ERROR
        eMessage = 'Error reading '//TRIM(stringa)
        GOTO 9999
      END IF
    ELSE IF( ios == 0 )THEN
      xGrd(i) = x(1)
      yGrd(i) = x(2)
    END IF
  END DO

  ios  = 1
  itry = 0
  WRITE(stringa,'(A,I3,A)')'line ',i,' ending point'
  DO WHILE( ios > 0 )
    itry = itry + 1
    ios = getInput( stringa,2,x )
    IF( ios > 0 )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(A)')'No default available, try again'
      ELSE
        nError   = UK_ERROR
        eMessage = 'Error reading '//TRIM(stringa)
        GOTO 9999
      END IF
    ELSE IF( ios == 0 )THEN
      xGrd(nn+i) = x(1)
      yGrd(nn+i) = x(2)
    END IF
  END DO
END DO

DO i = 1,nHorzLines
  x(1) = (xGrd(nn+i) - xGrd(i))/FLOAT(nLinePoints-1)
  x(2) = (yGrd(nn+i) - yGrd(i))/FLOAT(nLinePoints-1)
  IF( nLinePoints > 1 )THEN
    IF( ABS(x(1)) < EPSILON(xGRD(nn+i)))THEN
      horzLineType(i) = VERT_LINE
    ELSE IF( ABS(x(2)) < EPSILON(yGRD(nn+i)) )THEN
      horzLineType(i) = HORZ_LINE
    ELSE
      horzLineType(i) = 0
    END IF
  ELSE
    horzLineType(i) = 0
  END IF
  DO j = 2,nLinePoints-1
    xGrd(i+(j-1)*nHorzLines) = xGrd(i) + x(1)*(j-1)
    yGrd(i+(j-1)*nHorzLines) = yGrd(i) + x(2)*(j-1)
  END DO
END DO

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE GetHorzLines()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER irv

nxy = nHorzLines*nLinePoints

ALLOCATE( mFldGrd(nxy), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'GetHorzLines'
  eMessage = 'Error allocating Mean'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxy,' : Error=',irv
  GO TO 9999
END IF

irv = SCIPGetFieldValues( callerID,FieldIDs(1),Fields(1),PlotType,nxy,xGrd,yGrd,mFldGrd )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in SCIPGetFieldValues' )
  GOTO 9999
END IF

IF( hasVariance )THEN
  ALLOCATE( vFldGrd(nxy), STAT=irv )
  IF( irv /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'GetHorzLines'
    eMessage = 'Error allocating Variance'
    WRITE(eInform,'(A,I0,A,I0)')'Request =',nxy,' : Error=',irv
    GO TO 9999
  END IF

  PlotType%type = HP_VARIANCE
  irv = SCIPGetFieldValues( callerID,FieldIDs(1),Fields(1),PlotType,nxy,xGrd,yGrd,vFldGrd )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error in SCIPGetFieldValues' )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
