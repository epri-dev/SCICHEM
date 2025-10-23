!==============================================================================
!  Plot Field definition module
!==============================================================================
MODULE field_fd

  USE domain_fd
  USE DefSize_fd

!------ Plot CLASS ID's

  INTEGER, PARAMETER :: HP_CONC     =  0       !Concentration
  INTEGER, PARAMETER :: HP_MET      =  1       !Sfc Meteorology/terrain
  INTEGER, PARAMETER :: HP_DEP      =  2       !Surface deposition
  INTEGER, PARAMETER :: HP_DOS      =  3       !Surface dosage
  INTEGER, PARAMETER :: HP_INTCONC  =  7       !Integrated Concentration
  INTEGER, PARAMETER :: HP_ADJOINT  =  8       !Source estimation fields
  INTEGER, PARAMETER :: HP_ADJ_SFC  =  9       !Source estimation sfc fields
  INTEGER, PARAMETER :: HP_CMAX     =  10      !Max ave conc for AEGL's
  INTEGER, PARAMETER :: HP_EMAX     =  11      !Max execeedance for AEGL's

  INTEGER, PARAMETER :: HP_NUMADJ   = 3       !No. of Source estimation fields

  INTEGER, PARAMETER :: ADJ_MAXLOC  = 1     !Adjoint field choices
  INTEGER, PARAMETER :: ADJ_LOC     = 2
  INTEGER, PARAMETER :: ADJ_MASS    = 3


!------ Plot TIME LIST ID's

  INTEGER, PARAMETER :: HP_NOTIME   = 0      !No times
  INTEGER, PARAMETER :: HP_PUFFTIME = 1      !Puff output times
  INTEGER, PARAMETER :: HP_SRFTIME  = 2      !Surface integral output times
  INTEGER, PARAMETER :: HP_METTIME  = 3      !Meteorology times

!------ Plot CATEGORY indices

  INTEGER, PARAMETER :: HP_NUMCAT  = 7       !Number of types
  INTEGER, PARAMETER :: HP_SURF    = 1       !Surface integral field
  INTEGER, PARAMETER :: HP_SSLICE  = 2       !Surface slice
  INTEGER, PARAMETER :: HP_HSLICE  = 3       !Horizontal slice
  INTEGER, PARAMETER :: HP_VINT    = 4       !Vertically-integrated slice
  INTEGER, PARAMETER :: HP_VSLICE  = 5       !Vertical slice
  INTEGER, PARAMETER :: HP_TABLE   = 6       !Casualty table
  INTEGER, PARAMETER :: HP_HINT    = 7       !Horizontally-integrated slice

  INTEGER, PARAMETER :: HP_CATTYPE = 0       !Special for communicating with
                                             !effects module - encode categories
                                             !as bits and this is a TYPE bit

!------ Plot CONTOUR output types

  INTEGER, PARAMETER :: HP_RIGHTHAND =  1    !Right-handed
  INTEGER, PARAMETER :: HP_OPEN      =  0    !Open, i.e.,not closed contour
  INTEGER, PARAMETER :: HP_LEFTHAND  = -1    !Left-handed

  REAL, PARAMETER    :: HP_SPV = -1.E+36

  INTEGER, PARAMETER :: PLOT_ON   =  1
  INTEGER, PARAMETER :: PLOT_OFF  = -1
  INTEGER, PARAMETER :: PLOT_NULL =  0

!------ ClassChoice "available" bit types

  INTEGER, PARAMETER :: HPB_AVAILABLE  =  0    !Choice is available
  INTEGER, PARAMETER :: HPB_ASSOC_LINE =  1    !Additional "line' plot(s) available

  INTEGER, PARAMETER :: HP_AVAILABLE  =  2**HPB_AVAILABLE
  INTEGER, PARAMETER :: HP_ASSOC_LINE =  2**HPB_ASSOC_LINE

!==============================================================================
!  PlotType Strings
!==============================================================================

  CHARACTER(32), DIMENSION(HP_NUMCAT), PARAMETER :: CATEGORY_STRING = (/&
         'Surface                    ' ,& !HP_SURF
         'Surface Slice              ' ,& !HP_SSLICE
         'Horizontal Slice           ' ,& !HP_HSLICE
         'Vertically Integrated Slice' ,& !HP_VINT
         'Vertical Slice             ' ,& !HP_VSLICE
         'Table                      ' ,& !HP_TABLE
         'Horizontal Projection      '  & !HP_HINT
  /)

!==============================================================================
!  PlotField Point definition structure
!==============================================================================

  TYPE SCIPPointT
    SEQUENCE
    REAL             x            !X coordinate
    REAL             y            !Y coordinate
  END TYPE SCIPPointT

!==============================================================================
!  PlotField Contour Line definition structure
!==============================================================================

  TYPE SCIPLineT
    SEQUENCE
    INTEGER          index        !Contour index
    INTEGER          start        !Start point
    INTEGER          number       !Number of points
    INTEGER          mode         !LeftHand/RightHand
  END TYPE SCIPLineT

!==============================================================================
!  PlotField Vertical slice definition structure
!==============================================================================

  TYPE VerticalSliceT
    SEQUENCE
    INTEGER             resolution      !Vertical slice resolution
    TYPE ( SCIPPointT ) startPt         !Vertical slice Start Point
    TYPE ( SCIPPointT ) endPt           !Vertical slice End Point
  END TYPE VerticalSliceT

!==============================================================================
!  PlotField Horizontal slice definition structure
!==============================================================================

  TYPE HorizontalSliceT
    SEQUENCE
    INTEGER   mode          ! MSL/AGL 0=AGL 1=MSL
    REAL      height        ! Slice height
  END TYPE HorizontalSliceT

!==============================================================================
!  PlotField Coordinate definition structure
!==============================================================================

  TYPE SCIPFieldCoordinateT
    SEQUENCE
    INTEGER                   mode            !HD_LATLON,HD_UTM,HD_CARTESIAN
    INTEGER                   UTMZone         !UTM refernce zone if Mode=HD_UTM
    TYPE ( referenceT )       reference       !Cartesian Refernce point if Mode=HD_CARTESIAN
    TYPE ( VerticalSliceT )   vertSlice       !Vertical Slice data if Mode < 0
    TYPE ( HorizontalSliceT ) horzSlice       !Horizontal Slice data if Mode > 0
  END TYPE SCIPFieldCoordinateT

!==============================================================================
!  PlotField definition structure
!==============================================================================

  TYPE SCIPPlotFieldT
    SEQUENCE
    INTEGER                       category    !Plot Category ID
    INTEGER                       class       !Index into ClassStr array
    INTEGER                       choice      !Index into ChoiceStr array
    INTEGER                       kind        !Index into KindStr array
    INTEGER                       timeID      !Time list ID (Puff,Srf,Met,Rad)
    REAL                          userTime    !Run Time
    INTEGER                       iNotUsed    !reserved
    INTEGER                       maxCells    !Maximum number of cells to allocate
    INTEGER                       maxLev      !Maximun number of levels of refinememnt
    INTEGER                       fldLev      !Actual number of levels of refinememnt - Filled in by SCIPCreateField
    REAL                          resolution  !Minimum grid cell size - Filled in by SCIPCreateField
    INTEGER                       interpType  !Interpolation type - Filled in by SCIPCreateField
    TYPE( SCIPFieldCoordinateT )  coordinate  !Field coordinate structure - Filled in by SCIPCreateField
    CHARACTER(16)                 units       !Field Units - Filled in by SCIPCreateField
    CHARACTER(PATH_MAXLENGTH)     project     !
    CHARACTER(PATH_MAXLENGTH)     path        !
  END TYPE SCIPPlotFieldT

!==============================================================================
!  PlotField Category data structure
!==============================================================================

  TYPE SCIPPlotData
    SEQUENCE
    REAL xmin    !Vert. Slice - X coordinate LH point
    REAL xmax    !Vert. Slice - X coordinate RH point
    REAL ymin    !Vert. Slice - Y coordinate LH point
    REAL ymax    !Vert. Slice - Y coordinate RH point
    REAL zmin    !Vert. Slice - Min Z value : Horz. Slice - Slice value
    REAL zmax    !Vert. Slice - Max Z value
    REAL zres    !Vert. Slice - Vertical resolution (Number of points)
  END TYPE SCIPPlotData

!==============================================================================
!  PlotType definition structure
!==============================================================================

  TYPE SCIPPlotTypeT
    SEQUENCE
    INTEGER type         !PlotType Mean,Prob,Exceed,Hazard Area
    REAL    data         !Type data : Prob->exceed level :
    INTEGER areaMode
  END TYPE SCIPPlotTypeT

!==============================================================================
!  DrawField drawing instruction structure
!==============================================================================

  TYPE ARAPDrawT
    SEQUENCE
    INTEGER fillContour
    INTEGER drawContour
    INTEGER fill_Lo
    INTEGER fill_Hi
  END TYPE ARAPDrawT

!==============================================================================
!  WriteField drawing instruction structure
!==============================================================================

  TYPE ARAPWriteT
    SEQUENCE
    INTEGER  mode
    CHARACTER(PATH_MAXLENGTH) filename
  END TYPE ARAPWriteT

!==============================================================================
!  PlotField Node definition structure
!==============================================================================

  TYPE SCIPPlotFieldNodeT
    SEQUENCE
    INTEGER         id            !Node ID value
    REAL             x            !X coordinate
    REAL             y            !Y coordinate
    REAL             z            !Z coordinate
    REAL            hx            !X grid resolution
    REAL            hy            !Y grid resolution
    REAL             v            !Field value
  END TYPE SCIPPlotFieldNodeT

!==============================================================================
!  PlotField Triangle definition structure
!==============================================================================

  TYPE SCIPPlotFieldTriangleT
    SEQUENCE
    INTEGER         id            !Triangle ID value
    INTEGER       nidA            !Node ID value
    INTEGER       nidB            !Node ID value
    INTEGER       nidC            !Node ID value
  END TYPE SCIPPlotFieldTriangleT

END MODULE field_fd
