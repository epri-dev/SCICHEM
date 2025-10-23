PROGRAM ScipuffPostProcess

USE tooluser_fd
USE Extract_fi
USE SCIPtool
USE cmd_fi
USE scipuff_fi, ONLY: create
USE MPI_fi, ONLY: useMPI,myid

IMPLICIT NONE

INTEGER irv, ios
REAL startTime, endTime

CHARACTER(8) maxPath

INTEGER, EXTERNAL :: readCommandLine
REAL,    EXTERNAL :: currentTime

startTime = currentTime()

useMPI = .FALSE.
myid   = 0
create = .FALSE.

!--- Start up : Initialize common

CALL DataInitialization()

!--- Start up : Write Screen Header

WRITE(maxPath,'(I0)')PATH_MAXLENGTH

WRITE(6,'(/,A)')'Program : SCIPUFFPostProcess v'//TRIM(CODE_VERSION)//' (maxPath='//ADJUSTL(TRIM(maxPath))//')'

irv = SCIPGetVersionString(USER_ID,toolString)
IF( irv == SCIPsuccess )THEN
  irv = SCIPGetPathMaxLength()
  IF( irv == PATH_MAXLENGTH )THEN
    WRITE(6,'(A)')'   uses : SCIPtool v'//TRIM(toolString%string)
  ELSE
    nError = UK_ERROR
    eMessage = 'Error:SCIPtool PATH_MAXLENGTH mismatch'
    eInform ='SCIPUFFPostProcess='//ADJUSTL(TRIM(maxPath))//' : SCIPtool='
    WRITE(maxPath,'(I0)')irv
    eInform =TRIM(eInform)//ADJUSTL(TRIM(maxPath))
    GO TO 9999
  END IF
ELSE
  nError = UK_ERROR
  eMessage = 'Error checking SCIPtool version'
  GO TO 9999
END IF

!--- Start up : Read Command line anmd set inputmode

irv = readCommandLine()
IF( irv /= 0 )THEN
  IF( irv == -999 )GO TO 9999
  WRITE(6,*)'readCommandLine error',irv
  GO TO 9999
END IF

IF( LEN_TRIM(script_file) > 0 )THEN
  lun_in  = 105
  maxTry  = 1
  lScript = .TRUE.
  OPEN(lun_in,FILE=TRIM(script_file),IOSTAT=ios)
ELSE
  lun_in = 5
END IF

!--- Start up : Initialize SCIPtool library

CALL ToolInitialization()
IF( nError /= NO_ERROR )GOTO 9999

!---- Start up : Initialize run mode

CALL ModeInitialization()
IF( nError /= NO_ERROR )GOTO 9999

!---- Run based on mode

WRITE(6,'(/,A)')'====================================================================='
SELECT CASE( RunMode(1:2) )
  CASE( 'KE' )
    UseKey = .TRUE.
    WRITE(6,'("Running ExtractOutput using Keyword prompts")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'RP' )
    !-- READPUF Utility
    WRITE(6,'("Running readpuf utility")')
    CALL Readpuf( callerID )
    GO TO 9999
  CASE( 'PM' )
    !-- PUFMOM Utility
    WRITE(6,'("Running pufmom utility")')
    CALL PufMom( callerID )
    GO TO 9999
  CASE( 'BM' )
    !-- BUBBLEMOM Utility
    WRITE(6,'("Running bubblemom utility")')
    CALL BubbleMom( callerID )
    GO TO 9999
  CASE( 'TM' )
    !-- TotalMass Utility
    WRITE(6,'("Running TotalMass utility")')
    CALL TotalMass()
    GO TO 9999
  CASE( 'TG' )
    !-- TotalMassByGroup Utility
    WRITE(6,'("Running TotalMass by Group utility")')
    CALL TotalMassByGroup()
    GOTO 9999
  CASE( 'NK' )
    UseKey = .TRUE.
    allow3D = .FALSE.
    nativeSAG = .TRUE.
    pickedtype = HP_MEAN
    noTable = .TRUE.
    WRITE(6,'("Running ExtractOutput(keyword) for native SAG output")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'NS' )
    UseKey = .FALSE.
    allow3D = .FALSE.
    nativeSAG = .TRUE.
    pickedtype = HP_MEAN
    noTable = .TRUE.
    WRITE(6,'("Running ExtractOutput(index) for native SAG output")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'CK' )
    UseKey = .TRUE.
    allow3D = .FALSE.
    concProfile = .TRUE.
    noTable = .TRUE.
    pickedtype = HP_MEAN
    pickedTypeData = 0.0
    WRITE(6,'("Running ExtractOutput(keyword) for concentration profiles")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'CP' )
    UseKey = .FALSE.
    allow3D = .FALSE.
    concProfile = .TRUE.
    noTable = .TRUE.
    pickedtype = HP_MEAN
    pickedTypeData = 0.0
    WRITE(6,'("Running ExtractOutput(index) for concentration profiles")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'HK' )
    UseKey = .TRUE.
    allow3D = .FALSE.
    horzLines = .TRUE.
    noTable = .TRUE.
    pickedtype = HP_MEAN
    pickedTypeData = 0.0
    WRITE(6,'("Running ExtractOutput(keyword) for horizontal lines")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'HL' )
    UseKey = .FALSE.
    allow3D = .FALSE.
    horzLines = .TRUE.
    noTable = .TRUE.
    pickedtype = HP_MEAN
    pickedTypeData = 0.0
    WRITE(6,'("Running ExtractOutput(index) for horizontal lines")')
    CALL ExtractOutput()
    GOTO 9999
  CASE( 'QU' )
    WRITE(6,'("Quitting ...")')
    GO TO 9999
  CASE DEFAULT
    UseKey = .FALSE.
    WRITE(6,'("Running ExtractOutput using default number prompt")')
    CALL ExtractOutput()
    GOTO 9999
END SELECT

!---- Finished

9999 CONTINUE

IF( nError /= NO_ERROR )THEN
  WRITE(6,'(A)',IOSTAT=ios)'***********ERROR*****************'
  irv = LEN_TRIM(eRoutine)
  IF( irv > 1 )THEN
    WRITE(6,'(A)',IOSTAT=ios)TRIM(eRoutine)
  END IF
  WRITE(6,'(A)',IOSTAT=ios)TRIM(eMessage)
  WRITE(6,'(A)',IOSTAT=ios)TRIM(eInform)
  irv = LEN_TRIM(eAction)
  IF( irv > 1 )THEN
    WRITE(6,'(A)',IOSTAT=ios)TRIM(eAction)
  END IF
  WRITE(6,'(A)',IOSTAT=ios)'*********************************'
END IF


!---- Shut down : Exit SCIPtool library

IF( lInit )irv = SCIPExitTool()

endTime = currentTime()

!WRITE(77,*)'Total runtime = ',endTime - startTime

STOP
END
