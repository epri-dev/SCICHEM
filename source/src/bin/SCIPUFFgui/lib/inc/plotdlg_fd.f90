MODULE plotdlg_fd

  USE DefSize_fd

  INTEGER,PARAMETER :: MAXCNTG = 40

  INTEGER,PARAMETER :: PLOT_MEAN    = 1
  INTEGER,PARAMETER :: PLOT_PROB    = 2
  INTEGER,PARAMETER :: PLOT_ICPROB  = 3
  INTEGER,PARAMETER :: PLOT_CPROB   = 5

  TYPE  EXCEED_LAB
    SEQUENCE
    INTEGER     nc
    REAL        scale
    REAL        value(MAXCNTG)
    CHARACTER(16)  label(MAXCNTG)
    CHARACTER(16)  units
  END TYPE  EXCEED_LAB

  TYPE  MAP_DLG
    SEQUENCE
    REAL    popden
    LOGICAL MapsOn
    LOGICAL HiRes
    LOGICAL PPTextLo
    LOGICAL PPText
    LOGICAL PPSymbol
    LOGICAL PPArea
    LOGICAL Roads
    LOGICAL RRs
    LOGICAL Airport
    LOGICAL NucFac
    LOGICAL AirportText
    LOGICAL NucFacText
  END TYPE  MAP_DLG

  TYPE  POPT2_DLG
    SEQUENCE
    UNION
      MAP
        REAL    AR
        REAL    Size
        REAL    X
        REAL    Y
        REAL    Font
      END MAP
      MAP
        REAL    dbreal(5)
      END MAP
    END UNION
  END TYPE  POPT2_DLG

  TYPE  POPT_DLG
    SEQUENCE
    INTEGER Maxlev
    LOGICAL Max
    LOGICAL Cell
    LOGICAL Contour
    LOGICAL Source
    LOGICAL SrcText
    LOGICAL ColorBox
    LOGICAL FillLo
    LOGICAL FillHi
    LOGICAL Terrain
    LOGICAL Weather
    LOGICAL KmScale
    LOGICAL Overlay
    CHARACTER(128) OverlayFile
  END TYPE  POPT_DLG

  TYPE  SLICE_DLG
    SEQUENCE
    INTEGER nres(2)
    REAL    hslice(2,2)
    REAL    vext(2)
  END TYPE  SLICE_DLG

  TYPE  TITLE_DLG
    SEQUENCE
    UNION
      MAP
        REAL    X(3)
        REAL    Y(3)
      END MAP
      MAP
        REAL    dbreal(6)
      END MAP
    END UNION
    LOGICAL Show(3)
    LOGICAL ShowDate
    LOGICAL ShowTime
    CHARACTER(128) string(3)
  END TYPE  TITLE_DLG

  TYPE  AXES_DLG
    SEQUENCE
    UNION
      MAP
        INTEGER TicX
        INTEGER TicY
        INTEGER TicH
        INTEGER TicV
        INTEGER MapCoord
      END MAP
      MAP
        INTEGER dbint(5)
      END MAP
    END UNION
    UNION
      MAP
        REAL    Lat0
        REAL    Lon0
        REAL    X0
        REAL    Y0
        REAL    Xmin
        REAL    Xmax
        REAL    Xscale
        REAL    Ymin
        REAL    Ymax
        REAL    Yscale
        REAL    Hmin
        REAL    Hmax
        REAL    Hscale
        REAL    Vmin
        REAL    Vmax
        REAL    Vscale
        REAL    XminP
        REAL    XmaxP
        REAL    XscaleP
        REAL    YminP
        REAL    YmaxP
        REAL    YscaleP
      END MAP
      MAP
        REAL    dbreal(22)
      END MAP
    END UNION
    LOGICAL ShowX
    LOGICAL ShowY
    LOGICAL Cartesian
    CHARACTER(128) Xlab
    CHARACTER(128) Ylab
    CHARACTER(32)  Xformat
    CHARACTER(32)  Yformat
  END TYPE  AXES_DLG
END MODULE plotdlg_fd
