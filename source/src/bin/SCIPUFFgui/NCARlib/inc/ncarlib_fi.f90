MODULE ncarlib_fi

  USE winAPI_fd

  SAVE

!==============================================================================
!NCAR Parameters
!==============================================================================
  INTEGER, PARAMETER :: MAXCOLOR =  235
  INTEGER, PARAMETER :: MAXPNT   = 2000
  INTEGER, PARAMETER :: SPECIAL  =    3

!==============================================================================
!NCAR Structures
!==============================================================================
  TYPE MY_LOGPALETTE
    SEQUENCE
    INTEGER(2) palVersion
    INTEGER(2) palNumEntries
    TYPE( T_PALETTEENTRY ), DIMENSION(MAXCOLOR+1) :: palPalEntry
  END TYPE MY_LOGPALETTE

!==============================================================================
!Structures
!==============================================================================
  TYPE( T_LOGBRUSH ) brushnc

  TYPE( T_LOGPALETTE  ) MSWIN$ncpal     ! These allow the API interface statement
  TYPE( MY_LOGPALETTE ) ncpal           ! to work.  It calls for a T_LOGPALETTE
  EQUIVALENCE( ncpal, MSWIN$ncpal)      ! but a T_LOGPALETTE has only 1 entry

!==============================================================================
!Strings
!==============================================================================
  CHARACTER(20) cfmty
  CHARACTER(20) cfmtx
  CHARACTER(32) NCARfont

!==============================================================================
!Reals
!==============================================================================
  REAL xorg
  REAL xend
  REAL yorg
  REAL yend
  REAL ratx
  REAL raty
  REAL fontnc
  REAL sszhnc
  REAL sszwnc

!==============================================================================
!Logicals
!==============================================================================
  LOGICAL newpen
  LOGICAL newbrush
  LOGICAL lprt

!==============================================================================
!Integers
!==============================================================================
  INTEGER pstylenc
  INTEGER(POINTER_LEN) hrgn
  INTEGER ixorg
  INTEGER iyorg
  INTEGER(POINTER_LEN) ihwndnc
  INTEGER(POINTER_LEN) ihdcnc
  INTEGER(POINTER_LEN) ihdcget
  INTEGER idevnc
  INTEGER npxx
  INTEGER npxy
  INTEGER ncres
  INTEGER(POINTER_LEN) instnc
  INTEGER nchfx
  INTEGER nchfy
  INTEGER(POINTER_LEN) ihpalnc
  INTEGER npalnc
  INTEGER(POINTER_LEN) ihpalprt
  INTEGER ibstyle
  INTEGER ibstylec
  INTEGER ibmode
  INTEGER ibmodec
  INTEGER ibcolor
  INTEGER ibcolorc
  INTEGER ipstyle
  INTEGER ipstylec
  INTEGER ipcolor
  INTEGER ipcolorc
  INTEGER ipwidth
  INTEGER ipwidthc
  INTEGER(POINTER_LEN) ihpnc
  INTEGER(POINTER_LEN) ihbnc
  INTEGER(POINTER_LEN) ihbitnc
  INTEGER itstyle
  INTEGER itcolor
  INTEGER itbmode
  INTEGER itbcolor

END MODULE ncarlib_fi
