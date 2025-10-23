MODULE pltchoice_fd

  USE tooluser_fd
  USE resource_fd
  USE type_fd
  USE contourlist_fd

  INTEGER, PARAMETER :: OPTION_MAGIC    = 520508
  INTEGER, PARAMETER :: OPTION_LEVEL    = 0

  INTEGER, PARAMETER :: CLASS_COMBO    = IDB_COMBO1
  INTEGER, PARAMETER :: CHOICE_COMBO   = IDB_COMBO2
  INTEGER, PARAMETER :: KIND_COMBO     = IDB_COMBO3
  INTEGER, PARAMETER :: TIME_COMBO     = IDB_COMBO4
  INTEGER, PARAMETER :: CATEGORY_COMBO = IDB_COMBO5
  INTEGER, PARAMETER :: TYPE_COMBO     = IDB_COMBO6

  INTEGER, PARAMETER :: CLASS_COMBO_ID    = CLASS_COMBO    - COMBO_BASE
  INTEGER, PARAMETER :: CHOICE_COMBO_ID   = CHOICE_COMBO   - COMBO_BASE
  INTEGER, PARAMETER :: KIND_COMBO_ID     = KIND_COMBO     - COMBO_BASE
  INTEGER, PARAMETER :: TIME_COMBO_ID     = TIME_COMBO     - COMBO_BASE
  INTEGER, PARAMETER :: CATEGORY_COMBO_ID = CATEGORY_COMBO - COMBO_BASE
  INTEGER, PARAMETER :: TYPE_COMBO_ID     = TYPE_COMBO     - COMBO_BASE

  INTEGER, PARAMETER :: TIME_COMBO_LABEL  = IDB_STATIC14
  INTEGER, PARAMETER :: USER_TIME_LABEL   = IDB_STATIC35
  INTEGER, PARAMETER :: USER_TIME_EDIT    = IDB_REAL3

  INTEGER, PARAMETER :: PROB_EDIT  = IDB_REAL4
  INTEGER, PARAMETER :: EXCD_EDIT  = IDB_REAL1
  INTEGER, PARAMETER :: RISK_EDIT  = IDB_REAL5
  INTEGER, PARAMETER :: TYPE_LABEL = IDB_STATIC34
  INTEGER, PARAMETER :: EXCD_TEXT_ID = 1
  INTEGER, PARAMETER :: EXCD_EDIT_ID = IDB_REAL1 - REAL_BASE

  INTEGER, PARAMETER :: HSLICE_LABEL   = IDB_STATIC17
  INTEGER, PARAMETER :: HSLICE_EDIT    = IDB_REAL2
  INTEGER, PARAMETER :: VSLICE_LABEL   = IDB_STATIC19
  INTEGER, PARAMETER :: VSLICE_BUTTON  = IDB_BUTTON6

  INTEGER, PARAMETER :: PLOT_LOG  =  1
  INTEGER, PARAMETER :: PLOT_LIN  =  2
  INTEGER, PARAMETER :: PLOT_USER =  0
  INTEGER, PARAMETER :: PLOT_DEF  = -1

  INTEGER, PARAMETER :: BPLT_FILL =  0
  INTEGER, PARAMETER :: BPLT_DRAW =  1
  INTEGER, PARAMETER :: PLOT_FILL =  2**BPLT_FILL
  INTEGER, PARAMETER :: PLOT_DRAW =  2**BPLT_DRAW
  INTEGER, PARAMETER :: PLOT_BOTH =  PLOT_DRAW + PLOT_FILL

  INTEGER, PARAMETER :: NUM_FIELDS =  2
  INTEGER, PARAMETER :: FLD_INDEX  =  1
  INTEGER, PARAMETER :: TER_INDEX  =  2

  INTEGER, PARAMETER :: USER_CONTOUR  =  1
  INTEGER, PARAMETER :: TER_CONTOUR   =  USER_CONTOUR + 1
  INTEGER, PARAMETER :: NUM_CONTOUR =  TER_CONTOUR

  INTEGER, PARAMETER :: NUM_CLASSDATA =  2
  INTEGER, PARAMETER :: HSLICE_INDEX  =  1
  INTEGER, PARAMETER :: VSLICE_INDEX  =  2

  INTEGER, PARAMETER :: AXESBUTTON    = IDB_BUTTON11
  INTEGER, PARAMETER :: ANIMATEBUTTON = IDB_BUTTON12
  INTEGER, PARAMETER :: CONTOURBUTTON = IDB_BUTTON9
  INTEGER, PARAMETER :: OPTIONBUTTON  = IDB_BUTTON5
  INTEGER, PARAMETER :: MAPSBUTTON    = IDB_BUTTON8
  INTEGER, PARAMETER :: TITLEBUTTON   = IDB_BUTTON7

  TYPE ContourGenerateT

    SEQUENCE

    INTEGER       Mode     !PLOT_USER/PLOT_LOG/PLOT_LIN/PLOT_DEF
    INTEGER       Number   !Approx. number to generate
    REAL          Cmin     !Minimum contour
    REAL          Cmax     !Maximum contour
    REAL          Del      !Increment

  END TYPE ContourGenerateT

  TYPE PlotField

    SEQUENCE

    LOGICAL                                            Created      !true/false
    INTEGER                                            Mode         !PLOT_DRAW/PLOT_FILL
    INTEGER                                            Type         !HP_MEAN/HP_PROB/HP_EXCEED
    REAL, DIMENSION(HP_NUMTYP)                      :: TypeData     !RiskLevel/Probability/Exceedance
    TYPE( SCIPPlotFieldT )                             Field        !Field definition
    TYPE( SCIPPlotData  ), DIMENSION(NUM_CLASSDATA) :: ClassData    !Class specific data
    INTEGER                                            ContourIndex !SCIP_CONTOUR/USER_CONTOUR etc

  END TYPE PlotField

  TYPE PlotChoice
    CHARACTER(64)  ClassStr
    CHARACTER(64)  ChoiceStr
    CHARACTER(64)  KindStr
    INTEGER        Type
    INTEGER        Category
  END TYPE PlotChoice

END MODULE pltchoice_fd

MODULE pltchoice_fi

  USE pltchoice_fd

! Plot choice sizes

  SAVE

  INTEGER nPltClass
  INTEGER nPltChoice
  INTEGER nPltKind

!Plot choice availability information

  TYPE( SCIPCategoryClassT  ), DIMENSION(:,:), ALLOCATABLE :: CatClassArray
  TYPE( SCIPClassChoiceT    ), DIMENSION(:,:), ALLOCATABLE :: ClassChoiceArray

!Plot choice strings

  TYPE( char64T ), DIMENSION(:), ALLOCATABLE :: ClassStr
  TYPE( char64T ), DIMENSION(:), ALLOCATABLE :: ChoiceStr
  TYPE( char64T ), DIMENSION(:), ALLOCATABLE :: KindStr

!Plot times sizes

  INTEGER nTimePuff
  INTEGER nTimeSrf
  INTEGER nTimeMet
  INTEGER nTimeRad
  INTEGER nTimePlot
  REAL    RadTimeMax
  REAL    InputTimeMax

!Plot times

  TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: TimePuff
  TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: TimeSrf
  TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: TimeMet
  TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: TimeRad
  TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: TimePlot

!Plot table sizes

  INTEGER nTable
  INTEGER nCol
  INTEGER nRow

!Plot table labels

  TYPE( char32T ), DIMENSION(:), ALLOCATABLE :: TableTitle
  TYPE( char32T ), DIMENSION(:), ALLOCATABLE :: ColTitle
  TYPE( char32T ), DIMENSION(:), ALLOCATABLE :: RowTitle

!Plot table entries

  INTEGER, DIMENSION(:,:,:), ALLOCATABLE   :: Table

!Current selection

  TYPE( PlotField ), DIMENSION(-1:1) :: PlotDef
  TYPE( PlotField )                  :: PlotTer

!Field ID

  INTEGER, DIMENSION(NUM_FIELDS) :: FieldID

!Contour Data

  TYPE( SCIPContourElementList ), DIMENSION(NUM_CONTOUR,-1:1) :: ContourList
  TYPE( ContourGenerateT ),       DIMENSION(NUM_CONTOUR,-1:1) :: ContourBuild
  TYPE( SCIPContourElementList )                              :: ContourPlot
  TYPE( SCIPContourElementList )                              :: ContourTer

!Drawing structures

  TYPE( SCIPPlotTypeT ) :: DrawType
  TYPE( ARAPDrawT )     :: PlotDraw
  TYPE( SCIPPlotTypeT ) :: TerType
  TYPE( ARAPDrawT )     :: TerDraw

!Export Structure

  TYPE( ARAPWriteT )    :: PlotWrite

!Default Class/Choice/Kind strings

  TYPE( PlotChoice ) :: DefaultPlot

END MODULE pltchoice_fi
