!==============================================================================
! readCommandLine
!==============================================================================
INTEGER FUNCTION readCommandLine()

USE Extract_fi
USE cmd_fi
USE SCIPversion_fd, ONLY: SCIPUFF_TAG

IMPLICIT NONE

INTEGER    numCommand
INTEGER    ios2
CHARACTER(128) Command
CHARACTER(128) Value

INTEGER i, irv, ios
INTEGER nch, cNo

INTEGER, EXTERNAL :: SYSCheckFile

INTEGER iArg
LOGICAL lExist, isChar

readCommandLine = 0
ncmd = 0

numCommand = COMMAND_ARGUMENT_COUNT()

DO iArg = 1,numCommand

  CALL GET_COMMAND_ARGUMENT( iArg, Command, ios2 )

  IF( ios2 > 0 )THEN

    cNo = ICHAR(Command(2:2))

    !-- Check that arguments are characters and not -tive numbers
    IF( (cNo>64 .AND. cNo <91) .OR. (cNo>96 .AND. cNo<123) .OR. cNo == 45 )THEN
      isChar = .TRUE.
    ELSE
      isChar = .FALSE.
    END IF

#if defined(LINUX)
    IF( Command(1:1) == '-' .AND. isChar )THEN
#else
    IF( ( Command(1:1) == '/' .OR. Command(1:1) == '-' ) .AND. isChar )THEN
#endif
      ncmd = ncmd + 1
      IF( ALLOCATED(cmds) )THEN
        ncmd = SIZE(cmds)
        ALLOCATE(tcmd(ncmd),STAT=alloc_stat)
        tcmd = cmds
        DEALLOCATE(cmds,STAT=alloc_stat)
        ncmd = ncmd + 1
        ALLOCATE(cmds(ncmd),STAT=alloc_stat)
        cmds(1:ncmd-1) = tcmd
        DEALLOCATE(tcmd,STAT=alloc_stat)
      ELSE
        ncmd = 1
        ALLOCATE(cmds(1),STAT=alloc_stat)
      END IF
      narg = 0
      nch = LEN_TRIM(Command)
      i   = INDEX(Command,':')
      IF( i <= 0 )THEN
        Value = ' '
      ELSE IF( i >= nch )THEN
        Value = ' '
      ELSE IF( i < 3 )THEN
        WRITE(6,*)'Invalid command line parameter'
        WRITE(6,*)'Parameter='//TRIM(Command)
#if defined(LINUX)
        WRITE(6,'(" Valid parameter format is -SWITCH:Value ")')
#else
        WRITE(6,'(" Valid parameter format is /SWITCH:Value or -SWITCH:Value ")')
#endif
        readCommandLine = -3
        GOTO 9999
      ELSE
        SELECT CASE( Command(1:2) )
        CASE('-I','-i')
        CASE('-P','-p')
        CASE('-B')
        CASE( 'V','v')
          WRITE(6,'(A)')TRIM(SCIPUFF_TAG)
          readCommandLine = -999
          GO TO 9999
        CASE DEFAULT
          CALL CUPPER(Command)
        END SELECT
      END IF

      CALL SplitString( Command,':',narg,cargs,lerr )

      !--- Undo split for ini file on Windows if similar to C:\SCIPUFF etc.
      IF( cargs(1)(2:2) == 'I' .OR. cargs(1)(2:2) == 'i' )THEN
        IF( cargs(1)(1:1) == '/' .OR. cargs(1)(1:1) == '-' )THEN
          IF( narg == 3 )THEN
            narg = 2
            cargs(2) = TRIM(cargs(2))//':'//TRIM(cargs(3))
          END IF
        END IF
      END IF

      IF( narg > 1 )THEN
        cmds(ncmd)%name  = cargs(1)(2:)
        cmds(ncmd)%cargs = cargs(2)
        cmds(ncmd)%narg  = 1
      ELSE
        cmds(ncmd)%name  = cargs(1)(2:) !TRIM(Command(2:))
        cmds(ncmd)%cargs = ''
        cmds(ncmd)%narg  = 0
      END IF

    ELSE

      IF( cmds(ncmd)%narg ==  0 )THEN
        cmds(ncmd)%cargs = TRIM(Command)
      ELSE
        cmds(ncmd)%cargs = TRIM(cmds(ncmd)%cargs)//','//TRIM(Command)
      END IF
      cmds(ncmd)%narg  = cmds(ncmd)%narg + 1

    END IF

  ELSE

    WRITE(6,*)'Error reading command line'
    WRITE(6,*)'Processing command number ',iArg
    readCommandLine = -1
    GOTO 9999

  END IF

END DO

ini_file = ""
prjName  = ''
matName  = ''
RunMode  = 'NOTSET'
effType  = ''
iOut     = -1
iTim     = -1
iCnt     = -1
iFld     = -1
iScl     = -1
extType  = 'sag'
Hslc     = -1
Sslc     = -1
Vslc     = -1
Vint     = -1
Hpro     = -1
fScl     = 1.0 ! Scaling factor
fld%ftype = ''

IF( ALLOCATED(cmds) )THEN
  ncmd = SIZE(cmds)
ELSE
  ncmd = 0
END IF

DO i = 1,ncmd

  SELECT CASE( TRIM(cmds(i)%name) )

  CASE( 'B' )
    IF( LEN_TRIM(cmds(i)%cargs) > 0 )THEN
      script_file = ADJUSTL(cmds(i)%cargs)
      irv = sysCheckFile( script_file )
      IF( irv /= SCIPsuccess )THEN
        WRITE(6,*)'Script file not found'
        WRITE(6,*)'File='//TRIM(script_file)
        WRITE(6,*)'Check Path/File name'
        GOTO 9999
      END IF
    ELSE
      WRITE(6,*)'Script file not specified'
      WRITE(6,*)'Please specify the name of the desired script file'
      GOTO 9999
    END IF
    RunMode = ''

  CASE( 'P','p','-prj' )
    IF( cmds(i)%narg == 1 )THEN
      PrjName = TRIM(cmds(i)%cargs)
    ELSE
      WRITE(6,*)'Project name not specified'
      WRITE(6,*)'Please specify the name of the project'
      GOTO 9999
    END IF
  CASE( 'R','r','-mode' )
    IF( cmds(i)%narg == 1 )THEN
      CALL CUPPER( cmds(i)%cargs )
      RunMode = ADJUSTL(cmds(i)%cargs)
      IF( TRIM(RunMode) == 'PUFFDATA' )RunMode = 'RP'
    ELSE
      WRITE(6,*)'Run Mode not specified'
      WRITE(6,*)'Please specify the run mode'
      GOTO 9999
    END IF
  CASE( 'I','i','-ini' )
    IF( cmds(i)%narg == 1 )THEN
      Value = TRIM(cmds(i)%cargs)
      INQUIRE( FILE=Value,EXIST=lExist )
      IF( .NOT.lExist )THEN
        WRITE(6,*)'Specified INI file not found'
        WRITE(6,*)'File='//TRIM(Value)
        WRITE(6,*)'Check Path/File name'
        readCommandLine = -4
        GOTO 9999
      END IF
      ini_file = TRIM(Value)
    ELSE
      ini_file = "DEFAULT"
    END IF
  CASE( 'a','-scal' )
    iScl = i  ! Scaling factor
  CASE( 'c','-cntr' )
    iCnt = i  ! Contours
  CASE( 'e','-ext' )
    extType = TRIM(cmds(i)%cargs)  ! Extraction type
  CASE( 'f','-fld' )
    iFld = i  ! Fields
    IF( cmds(i)%cargs(1:3) == 'agl' )WRITE(effType,'(I3)')i
  CASE( 'h','-hslc' )
    Hslc = i  ! Horizontal slice
  CASE( 'j','-hpro' )
    Hpro = i  ! Horizontal projection
  CASE( 'l','-sslc' )
    Sslc = i  ! Surface Slice
  CASE( 'm','-mat' )
    IF( cmds(i)%narg == 1 )matName = TRIM(cmds(i)%cargs)
  CASE( 'M' )
    IF( LEN_TRIM(cmds(i)%cargs) > 0 )THEN
      READ(cmds(i)%cargs,*,IOSTAT=ios)nch
      IF( ios /= 0 .OR. nch <= 0 )THEN
        nch = maxGrd
      END IF
      maxGrd = nch
    END IF
  CASE( 'n','-vint' )
    Vint = i  ! Vertically integrated slice
  CASE( 'o','-out' )
    IF( cmds(i)%narg == 1 )THEN
      outFile = TRIM(cmds(i)%cargs)
      iOut  = i
    END IF
  CASE( 'O' )
    Value = cmds(i)%cargs
    IF( LEN_TRIM(Value) > 0 )THEN
      CALL cupper( Value )
      SELECT CASE( Value(1:1) )
        CASE ('Y')
          overWrite = 1
        CASE ('N')
          overWrite = -1
        CASE ('P')
          overWrite = 0
        CASE DEFAULT
          WRITE(6,*)'Invalid /O command value : '//TRIM(Value)
          WRITE(6,*)'Valid values are Y(es), N(o), P(rompt)'
          WRITE(6,*)'Using default value : P'
      END SELECT
    END IF
  CASE( '-prb' )
    fld%fType = 'Probability'
    vexcd = 0.
    IF( cmds(i)%narg == 1 )READ(cmds(i)%cargs,*,IOSTAT=ios)vexcd
  CASE( 't','-time' )
    iTim = i  ! Times
    READ(cmds(i)%cargs,*,IOSTAT=ios)tOut
    IF( ios /= 0 )THEN
      WRITE(6,*)'Error reading time argument'
      WRITE(6,*)'Check time value in ',TRIM(cmds(i)%cargs)
      GOTO 9999
    END IF
  CASE( '-var' )
    fld%fType = 'Variance'
  CASE( 'v','-vslc' )
    Vslc = i  ! Vertical slice
  CASE( '-excd' )
    fld%fType = 'Exceedance'
    pexcd = 1.e-3
    IF( cmds(i)%narg == 1 )READ(cmds(i)%cargs,*,IOSTAT=ios)pexcd
  CASE DEFAULT
    WRITE(6,*)'Invalid command line switch : '//TRIM(cmds(i)%name)
    WRITE(6,'(" Valid switches for Ini file, Max grid, Overwrite ")',ADVANCE='NO')
    WRITE(6,'(" Batch file and Run mode are I,M,O, B and R respt. ")')
    WRITE(6,'()')
    readCommandLine = -5
    GOTO 9999
  END SELECT

END DO
IF( iFld > 0 )THEN
  ! Must set runmode and ini_file if not specified for Fld mode
  IF( TRIM(RunMode) == 'NOTSET' )RunMode  = 'KE'
  IF( LEN_TRIM(ini_file) == 0   )ini_file = "DEFAULT"
ELSEIF( TRIM(RunMode) == 'NOTSET' )THEN
  RunMode = ''
ENDIF

9999 CONTINUE

RETURN
END

!==============================================================================
! CurrentTime
!==============================================================================
REAL FUNCTION CurrentTime()

IMPLICIT NONE

INTEGER, DIMENSION(8) :: values
INTEGER, EXTERNAL :: julian_day

INTEGER jday

CALL DATE_AND_TIME(VALUES = values)

!Value 1: 4 digit yr, 2: month, 3: day, local -> 5: hr, 6: min, 7: sec, 8: millisec

jday = julian_day( values(2),values(3),values(1) )

CurrentTime = ((jday*24. + values(5))*60. + values(6))*60. + values(7)

RETURN
END
