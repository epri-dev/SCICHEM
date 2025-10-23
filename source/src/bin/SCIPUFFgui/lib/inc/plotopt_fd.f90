!***********************************************************************
!               OptionsModule
!***********************************************************************
MODULE PlotOpt_fd

  USE pltchoice_fi

  INTEGER, PARAMETER :: PLOT_OPTION_MAGIC   = 4522950
  INTEGER, PARAMETER :: PLOT_OPTION_START   = 0
  INTEGER, PARAMETER :: PLOT_OPTION_CURRENT = 4
  INTEGER, PARAMETER :: CURRENT_OPTION      = 0
  INTEGER, PARAMETER :: DEFAULT_OPTION      = 1
  INTEGER, PARAMETER :: INTERACTIVE_OPTION  = 2

  TYPE SCIPPlotFieldT3
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
    CHARACTER(128)                project     !
    CHARACTER(128)                path        !
  END TYPE SCIPPlotFieldT3

  TYPE PlotField3
    SEQUENCE
    LOGICAL                                            Created      !true/false
    INTEGER                                            Mode         !PLOT_DRAW/PLOT_FILL
    INTEGER                                            Type         !HP_MEAN/HP_PROB/HP_EXCEED
    REAL, DIMENSION(HP_NUMTYP)                      :: TypeData     !RiskLevel/Probability/Exceedance
    TYPE( SCIPPlotFieldT3 )                            Field        !Field definition
    TYPE( SCIPPlotData  ), DIMENSION(NUM_CLASSDATA) :: ClassData    !Class specific data
    INTEGER                                            ContourIndex !SCIP_CONTOUR/USER_CONTOUR etc
  END TYPE PlotField3

  CONTAINS

  SUBROUTINE TransferOldField( ReadFld3,ReadFld )

    IMPLICIT NONE

    TYPE( PlotField3 ), INTENT( IN  ) :: ReadFld3
    TYPE( PlotField  ), INTENT( OUT ) :: ReadFld

    ReadFld%Created      = ReadFld3%Created
    ReadFld%Mode         = ReadFld3%Mode
    ReadFld%Type         = ReadFld3%Type
    ReadFld%TypeData     = ReadFld3%TypeData
    ReadFld%ClassData    = ReadFld3%ClassData
    ReadFld%ContourIndex = ReadFld3%ContourIndex

    ReadFld%Field            = ReadFld%Field
    ReadFld%Field%category   = ReadFld3%Field%category
    ReadFld%Field%class      = ReadFld3%Field%class
    ReadFld%Field%choice     = ReadFld3%Field%choice
    ReadFld%Field%kind       = ReadFld3%Field%kind
    ReadFld%Field%timeID     = ReadFld3%Field%timeID
    ReadFld%Field%userTime   = ReadFld3%Field%userTime
    ReadFld%Field%iNotUsed   = ReadFld3%Field%iNotUsed
    ReadFld%Field%maxCells   = ReadFld3%Field%maxCells
    ReadFld%Field%maxLev     = ReadFld3%Field%maxLev
    ReadFld%Field%fldLev     = ReadFld3%Field%fldLev
    ReadFld%Field%resolution = ReadFld3%Field%resolution
    ReadFld%Field%interpType = ReadFld3%Field%interpType
    ReadFld%Field%coordinate = ReadFld3%Field%coordinate
    ReadFld%Field%units      = ReadFld3%Field%units
    ReadFld%Field%project    = TRIM(ReadFld3%Field%project)
    ReadFld%Field%path       = TRIM(ReadFld3%Field%path)

    RETURN

   END SUBROUTINE TransferOldField


END MODULE PlotOpt_fd
