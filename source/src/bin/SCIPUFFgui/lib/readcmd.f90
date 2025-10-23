!***********************************************************************
!                ReadCommandLine
!***********************************************************************
SUBROUTINE read_command_line( hwnd,line )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE script_fi
USE SwinAPI

INTEGER(POINTER_LEN) hwnd
CHARACTER(*)         line

CHARACTER(128) Myline
CHARACTER(4)   Switch
CHARACTER(128) Value,Command

INTEGER nch,nlen,i,ios,n
LOGICAL lerr

!---- Current Command Switches -----------------------------------------
!
!     A:analyst        - Default analyst
!     B:batch_file     - File of script commands
!     C:classification - Default classification
!     D:default_file   - File of default plot values
!     G:grid_file      - File of default met grid values
!     I:ini_file       - File of initialization strings
!     P:               - PCSCIPUF Mode
!     T:MaxTable       - Maximum Table size (List Size)
!     U:[y,n]          - Unattended Mode
!                          [y] - Yes - turn on
!                          [n] - No  - turn off
!     V:[y,n]          - Alternate (VsrfEvap) Mode
!                          [y] - Yes - turn on VsrfEvap
!                          [n] - No  - turn off VsrfEvap - default
!     W:               - Welcome Mode
!-----------------------------------------------------------------------

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

CHARACTER(128), EXTERNAL :: AddNull

LOGICAL, EXTERNAL :: hasError
LOGICAL, EXTERNAL :: CheckFile

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'ReadCommandLine'

!---- Check for blank line

nlen = LEN(TRIM(line))

IF( nlen <= 0 )GOTO 9999

!---- Loop over line Getting each switch

Myline = line

DO

  CALL get_c( Myline,nlen,' ',Command,nch,lerr )
  IF( lerr )THEN
    nError = RD_ERROR
    eMessage = 'Error reading command line'
    eInform  = 'Line='//TRIM(Myline)
    GOTO 9998
  END IF

!------ Check for valid Command format

  IF( Command(1:1) /= '/' )THEN
    nError = IV_ERROR
    eMessage = 'Invalid command line parameter'
    eInform  = 'Parameter='//TRIM(Command)
    GOTO 9998
  END IF

!------ Get Command value

  i = INDEX( Command,':' )

  IF( i <= 0 )THEN

    Value = ' '

  ELSE IF( i >= nch )THEN

    Value = ' '

  ELSE IF( i < 3 )THEN

    nError = IV_ERROR
    eMessage = 'Invalid command line parameter'
    eInform  = 'Parameter='//TRIM(Command)
    GOTO 9998

  ELSE

    Value = Command(i+1:nch)

  END IF

!-----  Determine Switch

  Switch = Command(2:2)
  CALL cupper( Switch )

  SELECT CASE( Switch )

    CASE( 'W' )
      Welcome = .TRUE.
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        Welcome = Value(1:1) == 'Y'
      END IF

    CASE( 'U' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        IF( Value(1:1) == 'Y' )THEN
          pcscipuf_mode = IBSET(pcscipuf_mode,IPMB_SCRIPT)
        ELSE
          pcscipuf_mode = IBCLR(pcscipuf_mode,IPMB_SCRIPT)
        END IF
      ELSE
        pcscipuf_mode = IBSET(pcscipuf_mode,IPMB_SCRIPT)
      END IF

    CASE( 'P' )

    CASE( 'A' )
      IF( Value /= ' ' )project(DEFAULT_LEVEL)%audit%Analyst = TRIM(Value)

    CASE( 'T' )
      IF( Value /= ' ' )THEN
        READ(Value,*,IOSTAT=ios)n
        IF( ios /= 0 )n = MAX_MCX
        n = MAX(n,MAX_MCX)
        n = MIN(n,325000)
        maxList = n
      END IF

    CASE( 'C' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        project(DEFAULT_LEVEL)%audit%Classification = TRIM(Value)
      END IF

    CASE( 'B' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        IF( .NOT.CheckFile(Value) )THEN
          eMessage = 'Script file not found'
          CALL ReportFileName( eInform,'File=',Value )
          eAction  = 'Check Path/File name'
          GOTO 9998
        END IF
        pcscipuf_mode = IPM_SCRIPT
        script_file   = TRIM(Value)
      ELSE
        IF( nError /= NO_ERROR )THEN
          eMessage = 'Script file not specified'
          eAction  = 'Please specify the name of the desired script file'
          GOTO 9998
        END IF
      END IF

    CASE( 'D' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        IF( .NOT.CheckFile(Value) )THEN
          eMessage = 'Default definitions file not found'
          CALL ReportFileName( eInform,'File=',Value )
          eAction  = 'Check Path/File name'
          GOTO 9998
        END IF
        file_def = TRIM(Value)
      END IF

    CASE( 'G' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        IF( .NOT.CheckFile(Value) )THEN
          eMessage = 'Default grid file not found'
          CALL ReportFileName( eInform,'File=',Value )
          eAction  = 'Check Path/File name'
          GOTO 9998
        END IF
        file_met = TRIM(Value)
      END IF

    CASE( 'I' )
      IF( Value /= ' ' )THEN
        CALL cupper( Value )
        IF( .NOT.CheckFile(Value) )THEN
          eMessage = 'Specified INI file not found'
          CALL ReportFileName( eInform,'File=',Value )
          eAction  = 'Check Path/File name'
          GOTO 9998
        END IF
        ini_file = AddNull( TRIM(Value) )
      END IF

    CASE DEFAULT
      nError = IV_ERROR
      eMessage = 'Invalid command line switch : '//TRIM(Switch)
      eInform  = 'Valid switches are A,B,C,D,I,G,P,S,T,U,W'
      GOTO 9998

  END SELECT

  IF( nlen <= 1 )EXIT

END DO

9999 CONTINUE

RETURN

9998 CONTINUE

CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowInfoMessage( hwnd )

pcscipuf_mode = IPM_EXIT

nch = PostMessage( hwnd,WM_DESTROY,0,0 )
GOTO 9999

END

