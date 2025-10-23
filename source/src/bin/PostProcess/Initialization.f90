!======================================================================
! DataInitialization
!======================================================================
SUBROUTINE DataInitialization()

! Intializes all common variables

USE contour_fd
USE AreaMode_fd
USE tooluser_fd
USE Extract_fi
USE GetSrcFunc_fi
USE GetTimes_fi
USE localpuf
USE scipuff_fi, ONLY : mat_mc

IMPLICIT NONE

!--- Initialize

callerID = USER_ID
maxTry   = 5
nFields  = 0

!Error handling

nError      = NO_ERROR
eMessage    = ' '
eInform     = ' '
eAction     = ' '
eRoutine    = ' '

!Flags

noTable        = .FALSE.
append         = .FALSE.
nativeSAG      = .FALSE.
concProfile    = .FALSE.
horzLines      = .FALSE.
allow3D        = .TRUE.
pickedType     = -1
nProfiles      = 0
nHorzLines     = 0
nLinePoints    = 0

UseKey        = .FALSE.
doFld         = .FALSE.
lInit         = .FALSE.
has3D         = .FALSE.
output3D      = .FALSE.
lScript       = .FALSE.
hasVariance   = .FALSE.
overWrite     = 0
extractMethod = GRID

!Reverse

lReverse    = .FALSE.

Project%name = ' '
Project%path = ' '

!Plot type

plotType%type     = HP_MEAN
plotType%data     = 0.0
plotType%areaMode = HP_OFF
typeProb          = 0.0                    !Exceed/Prob value for field type
typeExceed        = 10.0                   !Exceed/Prob value for field type

!Contour

contourHead%number    = 1
contourHead%DrawMode  = PLOT_OFF
contourHead%LabelMode = PLOT_OFF
contourHead%Scale     = 1.0
contourHead%Unit      = 'default'
contourMode           = CLOSE_CONTOUR

!Plot category data

sliceHeight    = 0.0                    !Horizontal slice height
sliceXmin      = DEF_VAL_R              !Minimum X slice value - vertical slice
sliceXmax      = DEF_VAL_R              !Maximum X slice value - vertical slice
sliceYmin      = DEF_VAL_R              !Minimum Y slice value - vertical slice
sliceYmax      = DEF_VAL_R              !Maximum Y slice value - vertical slice
sliceZmin      = 0.0                    !Minimum Z slice value - vertical slice
sliceZmax      = 2500.                  !Maximum Z slice value - vertical slice
sliceNz        = 21                     !Slice Z points - Vertical slice

iround         = -999
riskLevel      = 10.0

!Plot data sizes
nClass    = 0
nChoice   = 0
nKind     = 0
nTimePuff = 0
nTimeSrf  = 0
nTimeMet  = 0
nTimeOut  = 0
nTable    = 0
nCol      = 0
nRow      = 0
nTriangle = 0
nNode     = 0
nPoint    = 0
nLine     = 0

!Field max values

FldMax = -HUGE(1.0)
FldMin = HUGE(1.0)

!Command line strings

script_file = ''
ini_file    = ''
RunMode     = ''
PrjName     = ''

!Grid

maxGrd         = 0                      !Maximum grid cells
GridType       = NATIVE
ContourType    = AUTO
extractMethod  = GRID

xMin =  HUGE(1.0)
xMax = -HUGE(1.0)
yMin = xMin
yMax = xMax
zMin = sliceZmin
zMax = SliceZmax
dx   = 0.0
dy   = 0.0
dz   = 0.0

nx = 0
ny = 0
nz = 0

!Unit numbers
lun_in  = 5
lun_out = 6
lun_grd = 0
lun_ter = 0

Field%category   = HP_SSLICE              !Field category index
Field%class      = -1
Field%choice     = 0
Field%kind       = 0
Field%timeID     = 0
Field%userTime   = 0.0
Field%iNotUsed   = SCIPoff
Field%maxCells   = 25000
Field%maxLev     = 99
Field%fldLev     = 99
Field%resolution = 1.0
Field%interpType = 0.0
Field%units      = 'XX'
Field%project    = ' '
Field%path       = ' '

! Fill up SCIPFieldCoordinateT structure
Field%coordinate%mode                 = HD_LATLON
Field%coordinate%UTMZone              = NOT_SET_I
Field%coordinate%reference%x          = NOT_SET_R
Field%coordinate%reference%y          = NOT_SET_R
Field%coordinate%reference%lat        = NOT_SET_R
Field%coordinate%reference%lon        = NOT_SET_R
Field%coordinate%vertSlice%resolution = NOT_SET_I
Field%coordinate%vertSlice%startPt%x  = NOT_SET_R
Field%coordinate%vertSlice%startPt%y  = NOT_SET_R
Field%coordinate%vertSlice%endPt      = Field%coordinate%vertSlice%startPt
Field%coordinate%horzSlice%height     = NOT_SET_R
Field%coordinate%horzSlice%mode       = NOT_SET_I

NULLIFY( TimePuff )
NULLIFY( TimeSrf )
NULLIFY( TimeMet )
NULLIFY( TimeOut )

NULLIFY(mcList%type)
NULLIFY(mcList%ID)

contourExt = '.txt'

RETURN
END
!======================================================================
! ToolInitialization
!======================================================================
SUBROUTINE ToolInitialization()

#if !defined(LINUX)
USE SwinAPI, ONLY : GetModuleFileName, GetWindowsDirectory
#endif
USE basic_fd
USE tooluser_fd
USE SCIPtool
USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER irv
#if defined(LINUX)
INTEGER nch
#endif
INTEGER request
INTEGER(LEN_ADDRESS) CallBackAddress
INTEGER idefault
LOGICAL lExist

CHARACTER(32)  mode
CHARACTER(128) string1,string2,string3,string4

TYPE( fileNameT ) :: file

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull, AddNull

INTEGER,              EXTERNAL :: sysGetProfileString
INTEGER,              EXTERNAL :: sysGetProfileInt
INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
INTEGER,              EXTERNAL :: sppCallback

CallBackAddress = ADDRESSOF( sppCallback )

request =  0
#if !defined(LINUX)
request = request + HI_UTM
#endif

!--- Start up : Initialize

#if defined(LINUX)
IF( LEN_TRIM(ini_file) <= 0 )THEN
  WRITE(6,*)
  WRITE(6,'(''PATH for scipuff.ini file(press Enter for default): '',$)')
  READ(lun_in,'(A)')string2
  IF ( LEN_TRIM(string2) == 0 ) THEN
    ini_file = 'scipuff.ini'
    INQUIRE( file=ini_file,EXIST=lexist )
    IF( .NOT.lexist )THEN
      ini_file = 'DEFAULT'
    END IF
  ELSE
    ini_file = TRIM(string2)
    nch      = LEN_TRIM(ini_file)
    WRITE(*,*)'User specified ini file/path = ',ini_file(nch-3:nch)
    ! Check if argument is the ini file with full path
    IF( ini_file(nch-3:nch) == '.ini' )THEN
      INQUIRE( file=TRIM(ini_file),EXIST=lexist )
      IF( .NOT.lexist )THEN
        WRITE(6,*)'Cannot find ini file ',TRIM(ini_file)
        ini_file = 'DEFAULT'
      ENDIF
    ELSE
      ! Try adding scipuff.ini to full path
      ini_file = TRIM(ADJUSTL(string2))//'/scipuff.ini'
      INQUIRE( file=ini_file,EXIST=lexist )
      IF( .NOT.lexist )THEN
        WRITE(6,*)'Cannot find scipuff.ini file in directory ',TRIM(string2)
        ini_file = 'DEFAULT'
      END IF
    END IF
  END IF
ELSE
  ini_file = ADJUSTL(ini_file)
END IF

IF( TRIM(ini_file) == 'DEFAULT' )THEN
  WRITE(6,*)'Using scipuff.ini from current directory'
  ini_file = 'scipuff.ini'
END IF

#else
IF( LEN_TRIM(ini_file) <= 0 )THEN
  WRITE(6,*)
  WRITE(6,'(''INI file (press Enter for default):'',$)')
  READ(5,'(A)')ini_file
  ini_file = ADJUSTL(ini_file)
  INQUIRE( file=ini_file,EXIST=lExist )
  IF( .NOT.lexist )THEN
    irv = GetModuleFileName( 0,string1,LEN(string1) )
    CALL SplitName( string1(1:irv),string2,string3 )
    ini_file = 'scipuff.ini'
    CALL AddPath( ini_file,TRIM(string3) )
  ENDIF
  INQUIRE( file=ini_file,EXIST=lexist )
  IF( .NOT.lexist )ini_file = 'DEFAULT'
ELSE
  ini_file = ADJUSTL(ini_file)
END IF

IF( TRIM(ini_file) == 'DEFAULT' )THEN
  WRITE(6,*)'Using default SCIP.ini'
  irv = GetWindowsDirectory( string2,LEN(string2) )
  string1 = TRIM(StripNull(string2))//'\'
  ini_file = 'scipuff.ini'
  CALL AddPath( ini_file,TRIM(string1) )
END IF
#endif

INQUIRE(file=ini_file,EXIST=lexist)
IF( .NOT.lexist )THEN
  nError      = UK_ERROR
  eMessage    = 'Unable to initialize the SCIPtool library'
  eInform     = 'Specified initialization file not found'
  eAction     = 'INI file='//TRIM(ini_file)
  eRoutine    = 'ToolInitialzaion'
  GOTO 9999
END IF

string4 = AddNull( TRIM(ini_file) )
string1 = AddNull( 'SCIPMode' )
string2 = AddNull( 'GUIMode' )
string3 = 'Standard'
irv = sysGetProfileString( string1,string2,string3,mode,string4 )

CALL cupper( mode )

SELECT CASE( TRIM(mode) )
  CASE( 'STANDARD' )
    limit%met1D       = HUGE(1)
    limit%puffs       = 20000
    limit%surfaceGrid = 25000

  CASE( 'OPERATIONAL' )
    limit%met1D       = HUGE(1)
    limit%puffs       = 20000
    limit%surfaceGrid = 25000

  CASE( 'EXTENDED' )
    limit%met1D       = HUGE(1)
    limit%puffs       = 40000
    limit%surfaceGrid = 85000

  CASE( 'ULTIMATE' )
    limit%met1D       = HUGE(1)
    limit%puffs       = 60000
    limit%surfaceGrid = 100000

  CASE DEFAULT
    limit%met1D       = HUGE(1)
    limit%puffs       = 20000
    limit%surfaceGrid = 25000
    string2 = AddNull( 'MaxMet1D' )
    idefault = limit%met1D
    irv = sysGetProfileInt( string1,string2,idefault,limit%met1D,string4 )
    string2 = AddNull( 'MaxPuff' )
    idefault = limit%puffs
    irv = sysGetProfileInt( string1,string2,idefault,limit%puffs,string4 )
    string2 = AddNull( 'MaxGrid' )
    idefault = limit%surfaceGrid
    irv = sysGetProfileInt( string1,string2,idefault,limit%surfaceGrid,string4 )
END SELECT

!==== Initialize PlotStub

WRITE(6,*)
WRITE(6,*)'Initializing SCIPtool from '//TRIM(ini_file)
WRITE(6,*)
WRITE(6,'("Running ScipuffPostProcess in ",A," mode")')TRIM(mode)
WRITE(6,'("  MaxPuff = ",I15)')limit%puffs
WRITE(6,'("  MaxGrid = ",I15)')limit%surfaceGrid

file%string = TRIM(ini_file)

irv = SCIPInitTool( callerID,CallBackAddress,request,limit,file )
IF( irv == SCIPfailure )THEN
  CALL toolError ('Failed to initialize the SCIPtool library' )
  GOTO 9999
END IF

lInit = .TRUE.

9999 CONTINUE

RETURN
END
!======================================================================
! ModeInitialization
!======================================================================
SUBROUTINE ModeInitialization()

USE Extract_fi

IMPLICIT NONE

INTEGER i
CHARACTER(128) string

IF( LEN_TRIM(RunMode) <= 0 )THEN
  DO
    WRITE(6,'(/,"Valid running modes are : ")')
    DO i = 1,NUM_RUNMODE
      WRITE(6,'(A)')TRIM(RUNMODES(i))
    END DO

    WRITE(6,'(/,"Run Mode : ",$)')
    READ(lun_in,'(A)') string
    IF( LEN_TRIM(string) <= 0 )THEN
      RunMode='NU'
      EXIT
    ELSE
      RunMode = ADJUSTL(string)
      CALL CUPPER( RunMode )
      SELECT CASE( RunMode(1:2) )
        CASE( 'NU','KE','RP','PM','BM','TM','TG','NK','NS','CK','CP','HK','HL','QU' )
          EXIT
        CASE DEFAULT
          string = 'Valid entries are'
          DO i = 1,NUM_RUNMODE
            string = TRIM(string)//' '//RUNMODES(i)(46:47)
          END DO
          string = TRIM(string)//'. Please try again.'
          WRITE(6,'(A)')TRIM(string)
          CYCLE
      END SELECT
    END IF
  END DO
END IF

9999 CONTINUE

RETURN
END
