MODULE Extract_fi

  USE tooluser_fd
  USE classData_fd
  USE default_fd

  IMPLICIT NONE

  SAVE

  INTEGER,       PARAMETER :: USER_ID          = 5852
  CHARACTER(16), PARAMETER :: CODE_VERSION     = '5.0'
  CHARACTER(16), PARAMETER :: CODE_NAME        = 'SCIPP'
  CHARACTER(32), PARAMETER :: CONCENTRATION_3D = '3D Concentration'
  CHARACTER(32), PARAMETER :: SRC_LOC_PROB     = 'Source Location Prob'

  INTEGER,       PARAMETER :: NUM_RUNMODE = 14
  CHARACTER(48), DIMENSION(NUM_RUNMODE), PARAMETER :: RUNMODES =(/ 'Extract Fields w keyword prompts            (KE)', &
                                                                   'Extract Fields w index prompts  (DEFAULT)   (NU)', &
                                                                   'Extract/View Puff data          [readpuf]   (RP)', &
                                                                   'Time history of Puff momments   [puffmom]   (PM)', &
                                                                   'Time history of Plume rise      [bubblemom] (BM)', &
                                                                   'Time history of total mass      [totalmass] (TM)', &
                                                                   'Time history of mass by group   [totalmass] (TG)', &
                                                                   'Extract Field w native SAG format(keyword)  (NK)', &
                                                                   'Extract Field w native SAG format(index)    (NS)', &
                                                                   'Concentration vertical profiles  (keyword)  (CK)', &
                                                                   'Concentration vertical profiles  (index)    (CP)', &
                                                                   'Horizontal lines                 (keyword)  (HK)', &
                                                                   'Horizontal lines                 (index)    (HL)', &
                                                                   'Exit application                            (QU)' /)

  INTEGER,       PARAMETER :: NUM_EXTRACT_GRID    = 4
  INTEGER,       PARAMETER :: NUM_EXTRACT_CONTOUR = 3
  INTEGER,       PARAMETER :: NUM_EXTRACT = NUM_EXTRACT_GRID + NUM_EXTRACT_CONTOUR
  CHARACTER(32), DIMENSION(0:NUM_EXTRACT), PARAMETER :: EX_METHODS =(/ 'Uniform Line       (LI)', &
                                                                       'Uniform Grid       (UG)', &
                                                                       'Custom Grid        (CG)', &
                                                                       'Automatic Grid     (AG)', &
                                                                       'Native SAG Grid    (NG)', &
                                                                       'Uniform Contours   (UC)', &
                                                                       'Custom Contours    (CC)', &
                                                                       'Automatic Contours (AC)' /)

  INTEGER,       PARAMETER :: NUM_GEN_MODE    = 3
  CHARACTER(32), DIMENSION(NUM_GEN_MODE), PARAMETER :: GEN_MODES =(/ 'Automatic   (AU)', &
                                                                     'Logarithmic (LO)', &
                                                                     'Linear      (LI)' /)
  INTEGER,       PARAMETER :: NUM_EXPORT    = 5
  CHARACTER(32), DIMENSION(NUM_EXPORT), PARAMETER :: EXPORTS =(/ 'SCIPUFF overlay (*.ovl) (OVL)', &
                                                                 'Street Atlas USA(*.usa) (USA)', &
                                                                 'OILSTOCK  format(*.oil) (OIL)', &
                                                                 'EIS Plume format(*.eis) (EIS)', &
                                                                 'CTS overlay file(*.cts) (CTS)' /)
! Error values

  INTEGER, PARAMETER :: EOF_ERROR =-1    !END-OF-FILE
  INTEGER, PARAMETER :: NO_ERROR  = 0    !No error
  INTEGER, PARAMETER :: LI_ERROR  = 1    !List Index error
  INTEGER, PARAMETER :: HT_ERROR  = 2    !SCIPtool error
  INTEGER, PARAMETER :: FD_ERROR  = 3    !Field error
  INTEGER, PARAMETER :: NI_ERROR  = 4    !Request not implemented
  INTEGER, PARAMETER :: UI_ERROR  = 5    !User input error
  INTEGER, PARAMETER :: SZ_ERROR  = 6    !Size error
  INTEGER, PARAMETER :: WR_ERROR  = 7    !Write error
  INTEGER, PARAMETER :: OP_ERROR  = 8    !Write error
  INTEGER, PARAMETER :: UK_ERROR  = 99   !Unknown error
  INTEGER, PARAMETER :: EX_ERROR  = -999 !Simple Exit

! Extraction methods

  INTEGER, PARAMETER :: GRID     = 0  !Grids
  INTEGER, PARAMETER :: CONTOURS = 1  !Contours

! Grid/Contour Types

  INTEGER, PARAMETER :: LINE1D  = 0  !Uniform line - user specified
  INTEGER, PARAMETER :: UNIFORM = 1  !Uniform grid/contours - user specified
  INTEGER, PARAMETER :: CUSTOM  = 2  !User defined locations/contours
  INTEGER, PARAMETER :: AUTO    = 3  !Uniform grid/contours - automatic
  INTEGER, PARAMETER :: NATIVE  = 4  !SAG nodes

! Line Types

  INTEGER, PARAMETER :: HORZ_LINE = 1  !Horizontal line
  INTEGER, PARAMETER :: VERT_LINE = 2  !Vertical profile

! Contour Generation

  INTEGER, PARAMETER :: PLOT_LOG  =  1
  INTEGER, PARAMETER :: PLOT_LIN  =  2
  INTEGER, PARAMETER :: PLOT_USER =  0
  INTEGER, PARAMETER :: PLOT_DEF  = -1

  TYPE ContourGenerateT
    SEQUENCE
    INTEGER       Mode     !PLOT_USER/PLOT_LOG/PLOT_LIN/PLOT_DEF
    INTEGER       Number   !Approx. number to generate
    REAL          Cmin     !Minimum contour
    REAL          Cmax     !Maximum contour
    REAL          Del      !Increment
  END TYPE ContourGenerateT

  TYPE( ARAPWriteT ) :: ContourWrite

  TYPE( char128T ) :: toolString
  INTEGER          :: callerID                !SCIPtool caller ID

! Error variables

  INTEGER        :: nError
  CHARACTER(32)  :: eRoutine
  CHARACTER(256) :: eMessage
  CHARACTER(256) :: eInform
  CHARACTER(256) :: eAction

  LOGICAL doFld, lInit, UseKey , output3D, has3D, lScript, hasVariance
  LOGICAL ldynamic, lstatic
  LOGICAL ldense
  LOGICAL lReverse
  INTEGER GridType, ContourType, LineType
  INTEGER overWrite, extractMethod

  INTEGER maxGrd, maxTry

  INTEGER lun_out, lun_grd, lun_in, lun_ter

  CHARACTER(PATH_MAXLENGTH) ini_file, script_file, RunMode, PrjName

  CHARACTER(4) contourExt

  TYPE( projectIDT ):: Project

  INTEGER nClass, nChoice, nKind

  INTEGER nTable, nCol, nRow  !Plot table sizes

  TYPE( char64T ),                DIMENSION(:),   ALLOCATABLE  :: ClassStr          !Class strings
  TYPE( char64T ),                DIMENSION(:),   ALLOCATABLE  :: ChoiceStr         !Choice strings
  TYPE( char64T ),                DIMENSION(:),   ALLOCATABLE  :: KindStr           !Kind strings
  TYPE( char32T ),                DIMENSION(:),   ALLOCATABLE  :: TableTitle        !Plot table labels
  TYPE( char32T ),                DIMENSION(:),   ALLOCATABLE  :: ColTitle
  TYPE( char32T ),                DIMENSION(:),   ALLOCATABLE  :: RowTitle
  TYPE( SCIPCategoryClassT ),     DIMENSION(:,:), ALLOCATABLE  :: CatClassArray     !Class/Category use array
  TYPE( SCIPClassChoiceT ),       DIMENSION(:,:), ALLOCATABLE  :: ClassChoiceArray  !Class/Choice use array
  TYPE( SCIPFieldCoordinateT )                                 :: ProjectCoordinate !Project coordinate descriptor
  TYPE( SCIPPlotFieldT )                                       :: Field
  TYPE( SCIPPlotFieldT )                                       :: MetField
  TYPE( SCIPPlotTypeT  )                                       :: plotType
  TYPE( SCIPPlotFieldT ),         DIMENSION(:),   ALLOCATABLE  :: Fields
  INTEGER,                        DIMENSION(:),   ALLOCATABLE  :: FieldIDs
  INTEGER                                                      :: nFields

  REAL, DIMENSION(:),                              ALLOCATABLE :: ClassData
  REAL, DIMENSION(:),                              ALLOCATABLE :: MetClassData

  INTEGER contourMode
  INTEGER nPoint, nLine
  TYPE( ContourGenerateT )                                     :: contourInput
  TYPE( SCIPContourElementList )                               :: contourExtract
  TYPE( SCIPContourHeaderT )                                   :: contourHead
  TYPE( SCIPContourElementT ),    DIMENSION(:),   ALLOCATABLE  :: contourList
  TYPE( SCIPLineT ),              DIMENSION(:),   ALLOCATABLE  :: cLines
  TYPE( SCIPPointT ),             DIMENSION(:),   ALLOCATABLE  :: cPoints

  INTEGER nNode, nTriangle
  TYPE( SCIPPlotFieldNodeT ),     DIMENSION(:),   ALLOCATABLE  :: fNodes        !field nodes
  TYPE( SCIPPlotFieldTriangleT ), DIMENSION(:),   ALLOCATABLE  :: fTriangles    !field triangles

  INTEGER,                        DIMENSION(:,:,:), ALLOCATABLE :: Table  !Plot table entries

  INTEGER nx, ny, nxy, nz
  REAL    xMin, xMax, yMin, yMax, zMin, zMax, dx, dy, dz

  REAL,                           DIMENSION(:),     ALLOCATABLE :: xGrd
  REAL,                           DIMENSION(:),     ALLOCATABLE :: yGrd
  REAL,                           DIMENSION(:),     ALLOCATABLE :: zGrd
  REAL,                           DIMENSION(:),     ALLOCATABLE :: mFldGrd
  REAL,                           DIMENSION(:),     ALLOCATABLE :: vFldGrd
  REAL,                           DIMENSION(:,:,:), ALLOCATABLE :: dFldGrd
  REAL,                           DIMENSION(2)                  :: FldMin, FldMax

  INTEGER :: sliceNz                    !Slice Z points - Vertical slice
  INTEGER :: iRound                     !Table rounding

  REAL    :: typeProb                   !Exceed/Prob value for field type
  REAL    :: typeExceed                 !Exceed/Prob value for field type
  REAL    :: minValue                   !Minimum value of interest - auto grid
  REAL    :: gridXmin                   !Minimum X grid value - uniform grid
  REAL    :: gridXmax                   !Maximum X grid value - uniform grid
  REAL    :: gridYmin                   !Minimum Y grid value - uniform grid
  REAL    :: gridYmax                   !Maximum Y grid value - uniform grid
  REAL    :: sliceHeight                !Horizontal slice height
  REAL    :: sliceXmin                  !Minimum X slice value - vertical slice
  REAL    :: sliceXmax                  !Maximum X slice value - vertical slice
  REAL    :: sliceYmin                  !Minimum Y slice value - vertical slice
  REAL    :: sliceYmax                  !Maximum Y slice value - vertical slice
  REAL    :: sliceZmin                  !Minimum Z slice value - vertical slice
  REAL    :: sliceZmax                  !Maximum Z slice value - vertical slice
  REAL    :: riskLevel                  !Table risk level

  INTEGER mxclass

  REAL    linePos

  LOGICAL                                 :: noTable
  LOGICAL                                 :: allow3D
  INTEGER                                 :: pickedType
  REAL                                    :: pickedTypeData

  LOGICAL :: nativeSAG
  LOGICAL :: append

  LOGICAL                                 :: concProfile
  INTEGER                                 :: nProfiles
  REAL, DIMENSION(:), ALLOCATABLE         :: profiles
  CHARACTER(4), DIMENSION(:), ALLOCATABLE :: profileNames

  LOGICAL                                 :: horzLines
  INTEGER                                 :: nHorzLines
  INTEGER                                 :: nLinePoints
  INTEGER, DIMENSION(:), ALLOCATABLE      :: horzLineType

END MODULE Extract_fi

MODULE cmd_fi

  INTEGER, PARAMETER        :: lun_ext = 201
  INTEGER ncmd, narg, alloc_stat
  INTEGER iTim, iMat, iFld, iCnt, iOut, iScl
  INTEGER Hslc, Sslc, Vslc, Vint, Hpro
  REAL tOut, fScl, vExcd, pExcd
  LOGICAL useArgs, lerr
  CHARACTER(16)  fUnit
  CHARACTER(256) matName, cmd, extType, outFile, effType
  CHARACTER(256), DIMENSION(:), POINTER :: cargs

  TYPE cmd_str
    CHARACTER(8)   :: name
    INTEGER        :: narg
    CHARACTER(256) :: cargs
  END TYPE cmd_str

  TYPE( cmd_str ), DIMENSION(:), ALLOCATABLE :: cmds, tcmd

  TYPE fld_str
    CHARACTER(16)   :: fclass     ! dos,dep,con,ico,dur,c3d
    CHARACTER(16)   :: fchoice    ! Material name
    CHARACTER(16)   :: fcategory  ! Surface
    CHARACTER(16)   :: fkind      ! Vapor, Liquid, Particle Bin,Total
    CHARACTER(16)   :: ftype      ! Mean, Prob, Exceed, Var
  END TYPE fld_str

  TYPE( fld_str ):: fld

  INTERFACE
    SUBROUTINE SplitString( InString,Separator,nSubStr,OutStrings,lerr )
      IMPLICIT NONE
      CHARACTER(*),   INTENT( IN    )       :: InString
      CHARACTER(1),   INTENT( IN    )       :: Separator
      INTEGER,        INTENT( INOUT )       :: nSubStr
      CHARACTER(256), DIMENSION(:), POINTER :: OutStrings
      LOGICAL,        INTENT( OUT   )       :: lerr
    END SUBROUTINE
  END INTERFACE

END MODULE cmd_fi

