!            Run SCIPUFF
!*******************************************************************************
SUBROUTINE RunScipuff( StartFlag )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIMgr_fi
USE SCIMgrState
USE scipuff_fi
USE metparam_fd
USE surface_fi

IMPLICIT NONE

CHARACTER(1), PARAMETER :: HEADER_CHAR = '#'

INTEGER StartFlag

INTEGER irv
INTEGER ios
INTEGER nch, nch3

CHARACTER(128) ctem
CHARACTER(128) string(11)
TYPE( char128T) VersionString

INTEGER, EXTERNAL :: GetVersionString

INTEGER, EXTERNAL :: PostButtonStateMessage
INTEGER, EXTERNAL :: PostButtonTagMessage
INTEGER, EXTERNAL :: PostButtonLabelMessage

INTEGER currentState
INTEGER waitState

!==== Open Log File

IF( StartFlag >= -1 )THEN
  OPEN(UNIT=lun_log,FILE=file_log,STATUS='UNKNOWN',DELIM='APOSTROPHE',POSITION='APPEND',IOSTAT=ios)
ELSE
  OPEN(UNIT=lun_log,FILE=file_log,STATUS='UNKNOWN',DELIM='APOSTROPHE',IOSTAT=ios)
END IF

IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'RunScipuff'
  eMessage = 'Error opening SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  WRITE(eAction,'(A,I6)')'FORTRAN ios=',ios
  LastError = FORTRAN_IOS_ERROR + ios
  GOTO 9999
END IF

!==== Setup Abort/Halt/Stop files

CALL SetupStopFiles()

irv = GetVersionString( 0,VersionString )
nch = INDEX(TRIM(VersionString%string),';')
string(3) = HEADER_CHAR//' SCIPtool Version ' &
          //VersionString%string(1:nch-1)//'  '//HEADER_CHAR
string(2) = HEADER_CHAR//' '//TRIM(VersionString%string(nch+1:))//&
            REPEAT(' ',LEN_TRIM(string(3))-LEN_TRIM(VersionString%string(nch+1:))-3)//HEADER_CHAR
string(1) = REPEAT(HEADER_CHAR,LEN_TRIM(string(3)))
string(4) = HEADER_CHAR//REPEAT(' ',LEN_TRIM(string(3))-2)//HEADER_CHAR
nch3 = LEN_TRIM(string(3))
CALL i_format( MAXSG,nch,ctem )
string(5) = HEADER_CHAR//'  MaxGrid  = '//TRIM(ctem)
nch = LEN_TRIM(string(5))
string(5) = TRIM(string(5))//REPEAT(' ',nch3-nch-1)//HEADER_CHAR
CALL i_format( MAXPUF,nch,ctem )
string(6) = HEADER_CHAR//'  MaxPuff  = '//TRIM(ctem)
nch = LEN_TRIM(string(6))
string(6) = TRIM(string(6))//REPEAT(' ',nch3-nch-1)//HEADER_CHAR
CALL i_format( MAX1D_MET,nch,ctem )
string(7) = HEADER_CHAR//'  MaxMet1D = '//TRIM(ctem)
nch = LEN_TRIM(string(7))
string(7) = TRIM(string(7))//REPEAT(' ',nch3-nch-1)//HEADER_CHAR
string(8) = string(4)
CALL i_format( nthreads,nch,ctem )
string(9) = HEADER_CHAR//'  OMP Threads = '//TRIM(ctem)
nch = LEN_TRIM(string(9))
string(9)  = TRIM(string(9))//REPEAT(' ',nch3-nch-1)//HEADER_CHAR
string(10) = string(4)
string(11) = string(1)

DO irv = 1,11
  WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(string(irv))
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'RunScipuff'
    eMessage = 'Error writing SCIPUFF log file header'
    CALL ReportFileName( eInform,'File=',file_log )
    WRITE(eAction,'(A,I6)')'FORTRAN ios=',ios
    LastError = FORTRAN_IOS_ERROR + ios
    GOTO 9999
  END IF
END DO

!==== Initialize Buttons

message%iParm   = 7 !All three buttons - bits 0,1,2 set
message%jParm   = TRUE !Button labels
message%routine = CHAR(0)
message%aString = 'Next Output'
message%bString = 'Next Step'
message%cString = 'Immediate'

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostButtonLabelMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )THEN
  CALL SetMessageHandlerError( 'RunScipuff' )
  GOTO 9998
END IF

message%iParm   = 7 !All three buttons - bits 0,1,2 set
message%jParm   = FALSE !Button text
message%routine = CHAR(0)
message%aString = '&Stop'
message%bString = '&Halt'
message%cString = '&Abort'

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostButtonTagMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )THEN
  CALL SetMessageHandlerError( 'RunScipuff' )
  GOTO 9998
END IF

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostButtonStateMessage( 0 )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )THEN
  CALL SetMessageHandlerError( 'RunScipuff' )
  GOTO 9998
END IF

!==== Call scipuff

IF( StartFlag == 0 )THEN
  CALL InitScipuff()
ELSE
  CALL scipuff()
END IF

!==== Inform GUI that SCIPUFF is done

9998 CONTINUE

IF( nError == S0_ERROR .OR. nError == AB_ERROR )CALL InfoMessage( )

message%iParm   = nError
message%jParm   = FALSE
message%routine = eRoutine
message%aString = eMessage
message%bString = eInform
message%cString = eAction

9999 CONTINUE

RETURN
END
!===============================================================================

LOGICAL FUNCTION IsValidData( data )

USE SCIMgrparam_fd

IMPLICIT NONE

INTEGER data

IsValidData = IAND( data,HS_VALID ) == HS_VALID

RETURN
END

!===============================================================================

SUBROUTINE SetEnvironment( opMet,x,y )

USE SCIMgr_fd
USE scipuff_fi
USE met_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( environmentT ), INTENT( INOUT ) :: opMet
REAL,                 INTENT( IN    ) :: x, y

REAL, DIMENSION(HS_MAXENVIRO) :: ze

INTEGER irv, ns, i

INTEGER, EXTERNAL :: SWIMgenZgrid

!------ Define vertical grid based on met fields

irv = SWIMgenZgrid( x,y,zmax,HS_MAXENVIRO,ze,ns )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMgenZgrid' )
  GOTO 9999
END IF

!------ Loop over grid to get met

DO i = 1,ns
  CALL get_met( x,y,ze(i),0.0,0.0,1 )
  IF( nError /= NO_ERROR )GOTO 9999
  opMet%samples(i)%z = ze(i)
  opMet%samples(i)%pressure = pb
  opMet%samples(i)%potentialTemp = thb * (1000./PSURF)**KAPPA
  opMet%samples(i)%humidity = hb
  opMet%samples(i)%windUComp = ub
  opMet%samples(i)%windVComp = vb
  opMet%samples(i)%windWComp = wb
END DO

DO i = ns+1,HS_MAXENVIRO
  opMet%samples(i)%z = NOT_SET_R
  opMet%samples(i)%pressure = NOT_SET_R
  opMet%samples(i)%potentialTemp = NOT_SET_R
  opMet%samples(i)%humidity = NOT_SET_R
  opMet%samples(i)%windUComp = NOT_SET_R
  opMet%samples(i)%windVComp = NOT_SET_R
  opMet%samples(i)%windWComp = NOT_SET_R
END DO

!------ Set surface values

CALL get_met( x,y,0.0,0.0,0.0,1 )
IF( nError /= NO_ERROR )GOTO 9999

opMet%sfcElevation      = TerElev+hmin
opMet%sfcPressure       = pb
opMet%mixingLayerHeight = zinv - TerElev
opMet%nsamp             = ns

9999 CONTINUE

RETURN
END

!===============================================================================

REAL FUNCTION GetSCIPUFFtime()

USE scipuff_fi

IMPLICIT NONE

GetSCIPUFFtime = t/3600.

RETURN
END

!===============================================================================

INTEGER FUNCTION SCIPUFFNumMtl()

USE scipuff_fi

IMPLICIT NONE

SCIPUFFNumMtl = ntypm

RETURN
END
