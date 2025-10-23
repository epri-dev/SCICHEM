MODULE FXgui

  INTERFACE

!==============================================================================
! FXGuiInit
!==============================================================================
    INTEGER FUNCTION FXGuiInit( INIfile )
      USE charT_fd
      TYPE( fileNameT ), INTENT ( IN ) :: INIfile
    END FUNCTION FXGuiInit

!==============================================================================
! FXGuiExit
!==============================================================================
    INTEGER FUNCTION FXGuiExit( )
    END FUNCTION FXGuiExit

!==============================================================================
! FXGuiDialog
!==============================================================================
    INTEGER FUNCTION FXGuiDialog( hWind, iCat, cClass, cChoice, cKind )
      USE charT_fd
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN), INTENT ( IN ) :: hWind
      INTEGER,              INTENT ( IN ) :: iCat
      TYPE( char64T ),      INTENT ( IN ) :: cClass
      TYPE( char64T ),      INTENT ( IN ) :: cChoice
      TYPE( char64T ),      INTENT ( IN ) :: cKind
    END FUNCTION FXGuiDialog

!==============================================================================
! FXGuiHasDialog
!==============================================================================
    INTEGER FUNCTION FXGuiHasDialog( iCat, cClass, cKind )
      USE charT_fd
      INTEGER,         INTENT ( IN ) :: iCat
      TYPE( char64T ), INTENT ( IN ) :: cClass
      TYPE( char64T ), INTENT ( IN ) :: cKind
    END FUNCTION FXGuiHasDialog

!==============================================================================
! FXGuiDialogLabel
!==============================================================================
    INTEGER FUNCTION FXGuiDialogLabel( iCat, cClass, cKind, cLabel )
      USE charT_fd
      INTEGER,         INTENT ( IN  ) :: iCat
      TYPE( char64T ), INTENT ( IN  ) :: cClass
      TYPE( char64T ), INTENT ( IN  ) :: cKind
      TYPE( char64T ), INTENT ( OUT ) :: cLabel
    END FUNCTION FXGuiDialogLabel

!==============================================================================
! FXGuiNumClassData
!==============================================================================
    INTEGER FUNCTION FXGuiNumClassData( cChoice, iCat, cClass, cKind )
      USE charT_fd
      TYPE( char64T ), INTENT ( IN  ) :: cChoice
      INTEGER,         INTENT ( IN  ) :: iCat
      TYPE( char64T ), INTENT ( IN  ) :: cClass
      TYPE( char64T ), INTENT ( IN  ) :: cKind
    END FUNCTION FXGuiNumClassData

!==============================================================================
! FXGuiClassData
!==============================================================================
    INTEGER FUNCTION FXGuiClassData( iCat, cClass, cChoice, cKind, pTime, classData )
      USE charT_fd
      INTEGER,            INTENT ( IN  ) :: iCat
      TYPE( char64T ),    INTENT ( IN  ) :: cClass
      TYPE( char64T ),    INTENT ( IN  ) :: cChoice
      TYPE( char64T ),    INTENT ( IN  ) :: cKind
      REAL,               INTENT ( IN  ) :: pTime
      REAL, DIMENSION(*), INTENT ( OUT ) :: classData
    END FUNCTION FXGuiClassData

!==============================================================================
! FXGuiSetClassData
!==============================================================================
    INTEGER FUNCTION FXGuiSetClassData( iCat, cClass, cChoice, cKind, pTime, classData )
      USE charT_fd
      INTEGER,            INTENT ( IN  ) :: iCat
      TYPE( char64T ),    INTENT ( IN  ) :: cClass
      TYPE( char64T ),    INTENT ( IN  ) :: cChoice
      TYPE( char64T ),    INTENT ( IN  ) :: cKind
      REAL,               INTENT ( IN  ) :: pTime
      REAL, DIMENSION(*), INTENT ( OUT ) :: classData
    END FUNCTION FXGuiSetClassData

!==============================================================================
! FXGuiPlotTitle
!==============================================================================
    INTEGER FUNCTION FXGuiPlotTitle( iCat, cClass, cChoice, cKind, cTitle )
      USE charT_fd
      INTEGER,            INTENT ( IN  ) :: iCat
      TYPE( char64T ),    INTENT ( IN  ) :: cClass
      TYPE( char64T ),    INTENT ( IN  ) :: cChoice
      TYPE( char64T ),    INTENT ( IN  ) :: cKind
      TYPE( char128T ),   INTENT ( OUT ) :: cTitle
    END FUNCTION FXGuiPlotTitle

  END INTERFACE

END MODULE FXgui
