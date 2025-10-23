!======================================================================
! ExtractProfiles
!======================================================================
SUBROUTINE ExtractProfiles()

USE Extract_fi

IMPLICIT NONE

CALL SelectProfiles()
IF( nError/= NO_ERROR )GOTO 9999

CALL GetProfiles()
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectProfiles()

USE Extract_fi

IMPLICIT NONE

CALL SelectNumberProfiles()
IF( nError/= NO_ERROR )GOTO 9999

CALL SelectProfileLocations()
IF( nError/= NO_ERROR )GOTO 9999


9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectNumberProfiles()

USE Extract_fi

IMPLICIT NONE

INTEGER itry, ios, input

CHARACTER(8) string

itry = 0

loop: DO
  WRITE(6,'(/,A,$)')'Desired number of Profiles [1-256]? '
  READ(lun_in,'(A)',IOSTAT=ios)string
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberProfiles'
    eMessage = 'Error reading number of profiles'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF

  itry = itry + 1
  string = ADJUSTR(string)
  READ(string,*,IOSTAT=ios)input
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectNumberProfiles'
    eMessage = 'Error getting number of profiles from input'
    WRITE(eInform,'(A,I0)')'error =',ios
  END IF
  IF( input < 1 .OR. input > 256 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(/,(A))')'Invalid number of profiles (n=[1,256]): '//TRIM(string)//'. Try again'
      CYCLE
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectNumberProfiles'
      eMessage = 'Invalid number of profiles : '//TRIM(string)
      GOTO 9999
    END IF
  ELSE
    EXIT loop
  END IF
END DO loop

WRITE(6,'(/,A,I0)')'Using number of profiles: ',input

nProfiles = input

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE SelectProfileLocations()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER irv, ios, itry, i, j, nxz

CHARACTER(128) stringa, stringb

INTEGER, EXTERNAL :: getInput

irv = SCIPGetFieldDomain( callerID,FieldIDs(1),nx,nz,xMin,zMin,dx,dz )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error from SCIPGetFieldDomain in SelectProfileLocations' )
  GOTO 9999
END IF

xMax = xMin + nx*dx

nxz = nProfiles*nz

ALLOCATE( xGrd(nxz), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SelectProfileLocations'
  eMessage = 'Error allocating xGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxz,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( zGrd(nxz), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SelectProfileLocations'
  eMessage = 'Error allocating zGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxz,' : Error=',irv
  GO TO 9999
END IF

DO j = 1,nz
  DO i = 1,nProfiles
    zGrd((j-1)*nProfiles+i) = zMin + 0.5*dz + (j-1)*dz
  END DO
END DO

WRITE(stringa,*)xMin
WRITE(stringb,*)xMax
WRITE(6,'(/,A)')'Profile locations should be ordered and in ['//TRIM(ADJUSTL(stringa))//','//TRIM(ADJUSTL(stringb))//'].'

ios  = 1
itry = 0
stringa = 'Profile locations'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( stringa,nProfiles,xGrd(1:nProfiles) )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectProfileLocations'
      eMessage = 'Error reading '//TRIM(stringa)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'No default available, try again'
    ELSE
      nError   = UK_ERROR
      eRoutine = 'SelectProfileLocations'
      eMessage = 'No '//TRIM(stringa)//' specified'
      GOTO 9999
    END IF
  END IF
END DO

DO i = 2,nz
  j = (i-1)*nProfiles
  xGrd(j+1:j+nProfiles) = xGrd(1:nProfiles)
END DO

9999 CONTINUE

RETURN
END
!======================================================================
!======================================================================
SUBROUTINE GetProfiles()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER irv, nxz

nxz = nProfiles*nz

ALLOCATE( mFldGrd(nxz), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'GetProfiles'
  eMessage = 'Error allocating Mean'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxz,' : Error=',irv
  GO TO 9999
END IF

irv = SCIPGetFieldValues( callerID,FieldIDs(1),Fields(1),PlotType,nxz,xGrd,zGrd,mFldGrd )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in SCIPGetFieldValues' )
  GOTO 9999
END IF

IF( hasVariance )THEN
  ALLOCATE( vFldGrd(nxz), STAT=irv )
  IF( irv /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'GetProfiles'
    eMessage = 'Error allocating Variance'
    WRITE(eInform,'(A,I0,A,I0)')'Request =',nxz,' : Error=',irv
    GO TO 9999
  END IF

  PlotType%type = HP_VARIANCE
  irv = SCIPGetFieldValues( callerID,FieldIDs(1),Fields(1),PlotType,nxz,xGrd,zGrd,vFldGrd )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error in SCIPGetFieldValues' )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
