MODULE winAPI_fd
  USE, INTRINSIC :: ISO_C_BINDING
  USE basic_fd, ONLY: TRUE, FALSE, NULL
#if defined(__INTEL_COMPILER) && !defined(LINUX)
!==============================================================================
!==============================================================================
!Use Intel WINDOWs function defintions (Intel and not Linux)
!==============================================================================
!==============================================================================
  USE ifwinty, HIDE_TRUE => TRUE, HIDE_FALSE => FALSE, HIDE_NULL=>NULL, HIDE_NO_ERROR => NO_ERROR &
             , HIDEX_ERROR => ERROR
  INTEGER(C_INTPTR_T), EXTERNAL :: ADDRESSOF
#else
!==============================================================================
!==============================================================================
!Use WINDOWs parameter and structure definitions (Linux or not Intel)
!==============================================================================
!==============================================================================
!
! PARAMETERS
!
  INTEGER, PARAMETER :: LONG_PTR = C_INTPTR_T
  INTEGER, PARAMETER :: UINT_PTR = C_INTPTR_T
  INTEGER, PARAMETER :: ULONG_PTR= C_INTPTR_T
  INTEGER, PARAMETER :: LPVOID   = C_INTPTR_T
  INTEGER, PARAMETER :: PVOID    = C_INTPTR_T
  INTEGER, PARAMETER :: LPCSTR   = C_INTPTR_T
  INTEGER, PARAMETER :: LPSTR    = C_INTPTR_T
  INTEGER, PARAMETER :: LPBYTE   = C_INTPTR_T
  INTEGER, PARAMETER :: LPDWORD  = C_INTPTR_T
  INTEGER, PARAMETER :: HANDLE   = C_INTPTR_T

  INTEGER, PARAMETER :: HFILESZ  = 4         ! REAL(4)  WHY?
  INTEGER, PARAMETER :: SFLOAT   = 4         ! REAL(4)
  INTEGER, PARAMETER :: DWORD    = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: ULONG    = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: LONG     = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: UINT     = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: SINT     = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: BOOL     = 4         ! INTEGER(4)
  INTEGER, PARAMETER :: WORD     = 2         ! INTEGER(2)
  INTEGER, PARAMETER :: SHORT    = 2         ! INTEGER(2)
  INTEGER, PARAMETER :: BYTE     = 1         ! INTEGER(1)

  INTEGER, PARAMETER :: FLPARAM  = LONG_PTR
  INTEGER, PARAMETER :: FWPARAM  = UINT_PTR

!==============================================================================
! Basic parameters
!==============================================================================
  INTEGER, PARAMETER :: IDOK  = 1

#if !defined(LINUX)
!==============================================================================
! Message Box parameters
!==============================================================================
  INTEGER, PARAMETER :: MB_OK                 =  #00000000
  INTEGER, PARAMETER :: MB_YESNO              =  #00000004
  INTEGER, PARAMETER :: MB_ICONHAND           =  #00000010
  INTEGER, PARAMETER :: MB_ICONQUESTION       =  #00000020
  INTEGER, PARAMETER :: MB_TASKMODAL          =  #00002000
  INTEGER, PARAMETER :: MB_SETFOREGROUND      =  #00010000

  INTEGER, PARAMETER :: IDYES                 = 6

  INTEGER, PARAMETER :: IDI_EXCLAMATION     =  #00007F03
  INTEGER, PARAMETER :: IDI_QUESTION        =  #00007F02
  INTEGER, PARAMETER :: IDI_HAND            =  #00007F01

!==============================================================================
! Cursor parameters
!==============================================================================
  INTEGER, PARAMETER :: IDC_ARROW             =  #00007F00
  INTEGER, PARAMETER :: IDC_CROSS             =  #00007F03
  INTEGER, PARAMETER :: IDC_WAIT              =  #00007F02

!==============================================================================
! Brush/Pen parameters
!==============================================================================
  INTEGER, PARAMETER :: WHITE_BRUSH           = 0
  INTEGER, PARAMETER :: NULL_BRUSH            = 5
  INTEGER, PARAMETER :: BLACK_PEN             = 7
  INTEGER, PARAMETER :: NULL_PEN              = 8

  INTEGER, PARAMETER :: R2_MASKPEN            =  9   !  DPA
  INTEGER, PARAMETER :: R2_COPYPEN            = 13   !  P

  INTEGER, PARAMETER :: PS_GEOMETRIC      =  #00010000
  INTEGER, PARAMETER :: PS_ENDCAP_SQUARE  =  #00000100
  INTEGER, PARAMETER :: PS_JOIN_BEVEL     =  #00001000
  INTEGER, PARAMETER :: PC_NOCOLLAPSE     =  #04        !  DO NOT MATCH COLOR TO SYSTEM PALETTE

  INTEGER, PARAMETER :: HS_DIAGCROSS      = 5

  INTEGER, PARAMETER :: BS_SOLID          = 0

!==============================================================================
! Text parameters
!==============================================================================
  INTEGER, PARAMETER :: TA_LEFT           = 0
  INTEGER, PARAMETER :: TA_TOP            = 0
  INTEGER, PARAMETER :: TA_UPDATECP       = 1
  INTEGER, PARAMETER :: TA_RIGHT          = 2
  INTEGER, PARAMETER :: TA_CENTER         = 6
  INTEGER, PARAMETER :: TA_BOTTOM         = 8
  INTEGER, PARAMETER :: TA_BASELINE       = 24


!==============================================================================
! Window Style parameters
!==============================================================================
  INTEGER, PARAMETER :: WS_POPUP       =  #80000000     !-2147483647
  INTEGER, PARAMETER :: WS_MAXIMIZE    =  #01000000
  INTEGER, PARAMETER :: WS_MINIMIZEBOX =  #00020000
  INTEGER, PARAMETER :: WS_CAPTION     =  #00C00000     !  WS_BORDER | WS_DLGFRAME
  INTEGER, PARAMETER :: WS_SYSMENU     =  #00080000
  INTEGER, PARAMETER :: WS_CHILD       =  #40000000
  INTEGER, PARAMETER :: WS_VISIBLE     =  #10000000
  INTEGER, PARAMETER :: WS_DLGFRAME    =  #00400000

!==============================================================================
! Show Window parameters
!==============================================================================
  INTEGER, PARAMETER :: SW_HIDE               = 0
  INTEGER, PARAMETER :: SW_SHOWNORMAL         = 1
  INTEGER, PARAMETER :: SW_SHOWMINIMIZED      = 2
  INTEGER, PARAMETER :: SW_SHOW               = 5

!==============================================================================
! Window Message parameters
!==============================================================================
  INTEGER, PARAMETER :: WM_CREATE             =  #0001
  INTEGER, PARAMETER :: WM_DESTROY            =  #0002
  INTEGER, PARAMETER :: WM_MOVE               =  #0003
  INTEGER, PARAMETER :: WM_SETTEXT            =  #000C
  INTEGER, PARAMETER :: WM_GETTEXT            =  #000D
  INTEGER, PARAMETER :: WM_PAINT              =  #000F
  INTEGER, PARAMETER :: WM_QUERYOPEN          =  #0013
  INTEGER, PARAMETER :: WM_SETCURSOR          =  #0020
  INTEGER, PARAMETER :: WM_DRAWITEM           =  #002B
  INTEGER, PARAMETER :: WM_SETFONT            =  #0030
  INTEGER, PARAMETER :: WM_GETFONT            =  #0031
  INTEGER, PARAMETER :: WM_WINDOWPOSCHANGING  =  #0046
  INTEGER, PARAMETER :: WM_KEYFIRST           =  #0100
  INTEGER, PARAMETER :: WM_KEYDOWN            =  #0100
  INTEGER, PARAMETER :: WM_SYSKEYDOWN         =  #0104
  INTEGER, PARAMETER :: WM_KEYLAST            =  #0108
  INTEGER, PARAMETER :: WM_INITDIALOG         =  #0110
  INTEGER, PARAMETER :: WM_COMMAND            =  #0111
  INTEGER, PARAMETER :: WM_TIMER              =  #0113
  INTEGER, PARAMETER :: WM_HSCROLL            =  #0114
  INTEGER, PARAMETER :: WM_CTLCOLORMSGBOX     =  #0132
  INTEGER, PARAMETER :: WM_CTLCOLOREDIT       =  #0133
  INTEGER, PARAMETER :: WM_CTLCOLORLISTBOX    =  #0134
  INTEGER, PARAMETER :: WM_CTLCOLORBTN        =  #0135
  INTEGER, PARAMETER :: WM_CTLCOLORDLG        =  #0136
  INTEGER, PARAMETER :: WM_CTLCOLORSTATIC     =  #0138
  INTEGER, PARAMETER :: WM_MOUSEMOVE          =  #0200
  INTEGER, PARAMETER :: WM_LBUTTONDOWN        =  #0201
  INTEGER, PARAMETER :: WM_LBUTTONUP          =  #0202
  INTEGER, PARAMETER :: WM_RBUTTONDOWN        =  #0204
  INTEGER, PARAMETER :: WM_RBUTTONUP          =  #0205
  INTEGER, PARAMETER :: WM_MBUTTONDOWN        =  #0207
  INTEGER, PARAMETER :: WM_PALETTECHANGED     =  #0311

  INTEGER, PARAMETER :: STM_SETICON           =  #0170
  INTEGER, PARAMETER :: STM_SETIMAGE          =  #0172
  INTEGER, PARAMETER :: STM_GETIMAGE          =  #0173

  INTEGER, PARAMETER :: EN_KILLFOCUS   = #0200
  INTEGER, PARAMETER :: CBN_SELCHANGE  = 1
  INTEGER, PARAMETER :: LBN_SELCHANGE  = 1
  INTEGER, PARAMETER :: LBN_KILLFOCUS  = 5

!==============================================================================
! System Metric parameters
!==============================================================================
  INTEGER, PARAMETER :: SM_CXSCREEN           =  0
  INTEGER, PARAMETER :: SM_CYSCREEN           =  1
  INTEGER, PARAMETER :: SM_CXFRAME            = 32
  INTEGER, PARAMETER :: SM_CYFRAME            = 33

!==============================================================================
! Path parameters
!==============================================================================
  INTEGER,  PARAMETER :: MAX_PATH = 260

!==============================================================================
! COMBO/LIST Box parameters
!==============================================================================
  INTEGER, PARAMETER :: CB_ADDSTRING    =  #0143
  INTEGER, PARAMETER :: CB_DELETESTRING =  #0144
  INTEGER, PARAMETER :: CB_GETCOUNT     =  #0146
  INTEGER, PARAMETER :: CB_GETCURSEL    =  #0147
  INTEGER, PARAMETER :: CB_GETLBTEXT    =  #0148
  INTEGER, PARAMETER :: CB_GETLBTEXTLEN =  #0149
  INTEGER, PARAMETER :: CB_INSERTSTRING =  #014A
  INTEGER, PARAMETER :: CB_RESETCONTENT =  #014B
  INTEGER, PARAMETER :: CB_SELECTSTRING =  #014D
  INTEGER, PARAMETER :: CB_SETCURSEL    =  #014E
  INTEGER, PARAMETER :: CB_GETITEMDATA  =  #0150
  INTEGER, PARAMETER :: CB_SETITEMDATA  =  #0151
  INTEGER, PARAMETER :: CB_FINDSTRINGEXACT =  #0158

  INTEGER, PARAMETER :: LB_ADDSTRING    =  #0180
  INTEGER, PARAMETER :: LB_INSERTSTRING =  #0181
  INTEGER, PARAMETER :: LB_DELETESTRING =  #0182
  INTEGER, PARAMETER :: LB_RESETCONTENT =  #0184
  INTEGER, PARAMETER :: LB_SETCURSEL    =  #0186
  INTEGER, PARAMETER :: LB_GETCURSEL    =  #0188
  INTEGER, PARAMETER :: LB_GETTEXT      =  #0189
  INTEGER, PARAMETER :: LB_GETTEXTLEN   =  #018A
  INTEGER, PARAMETER :: LB_GETCOUNT     =  #018B
  INTEGER, PARAMETER :: LB_SELECTSTRING =  #018C
  INTEGER, PARAMETER :: LB_GETSELCOUNT  =  #0190
  INTEGER, PARAMETER :: LB_GETSELITEMS  =  #0191
  INTEGER, PARAMETER :: LB_GETITEMDATA  =  #0199
  INTEGER, PARAMETER :: LB_SETITEMDATA  =  #019A
  INTEGER, PARAMETER :: LB_SELITEMRANGE =  #019B
  INTEGER, PARAMETER :: LB_FINDSTRINGEXACT =  #01A2

  INTEGER, PARAMETER :: LB_ERR      = (-1)
  INTEGER, PARAMETER :: LB_ERRSPACE = (-2)

!==============================================================================
! HELP parameters
!==============================================================================
  INTEGER, PARAMETER :: HELP_CONTEXT  =  #0001  !  DISPLAY TOPIC IN ULTOPIC
  INTEGER, PARAMETER :: HELP_CONTENTS =  #0003

!==============================================================================
! Window Position/Size parameters
!==============================================================================
  INTEGER, PARAMETER :: SWP_NOSIZE   =  #0001
  INTEGER, PARAMETER :: SWP_NOMOVE   =  #0002
  INTEGER, PARAMETER :: SWP_NOZORDER =  #0004

  INTEGER, PARAMETER :: HWND_TOPMOST    = -1

!==============================================================================
! ScrollBar parameters
!==============================================================================
  INTEGER, PARAMETER :: SB_LINEUP         = 0
  INTEGER, PARAMETER :: SB_LINEDOWN       = 1
  INTEGER, PARAMETER :: SB_PAGEUP         = 2
  INTEGER, PARAMETER :: SB_PAGEDOWN       = 3
  INTEGER, PARAMETER :: SB_THUMBPOSITION  = 4
  INTEGER, PARAMETER :: SB_TOP            = 6
  INTEGER, PARAMETER :: SB_BOTTOM         = 7
  INTEGER, PARAMETER :: SB_ENDSCROLL      = 8

  INTEGER, PARAMETER :: SB_CTL = 2

!==============================================================================
! OpenFile parameters
!==============================================================================
  INTEGER, PARAMETER :: OFN_OVERWRITEPROMPT              = #00000002
  INTEGER, PARAMETER :: OFN_HIDEREADONLY                 = #00000004
  INTEGER, PARAMETER :: OFN_ENABLEHOOK                   = #00000020
  INTEGER, PARAMETER :: OFN_ENABLETEMPLATE               = #00000040
  INTEGER, PARAMETER :: OFN_PATHMUSTEXIST                = #00000800
  INTEGER, PARAMETER :: OFN_LONGNAMES                    = #00200000

!==============================================================================
! Print parameters
!==============================================================================
  INTEGER, PARAMETER :: PD_NOSELECTION                = #00000004
  INTEGER, PARAMETER :: PD_RETURNDC                   = #00000100
  INTEGER, PARAMETER :: PD_RETURNDEFAULT              = #00000400
  INTEGER, PARAMETER :: PD_ENABLEPRINTHOOK            = #00001000
  INTEGER, PARAMETER :: PD_ENABLESETUPHOOK            = #00002000
  INTEGER, PARAMETER :: PD_ENABLEPRINTTEMPLATE        = #00004000
  INTEGER, PARAMETER :: PD_ENABLESETUPTEMPLATE        = #00008000

!==============================================================================
! PeekMessage parameters
!==============================================================================
  INTEGER, PARAMETER :: PM_REMOVE     =  #0001

!==============================================================================
! Error parameters
!==============================================================================
  INTEGER, PARAMETER :: ERROR_CALL_NOT_IMPLEMENTED = 120

  INTEGER, PARAMETER :: INVALID_HANDLE_VALUE       =  -1

  INTEGER, PARAMETER :: SP_ERROR                   = (-1)

!==============================================================================
! OBJECT parameters
!==============================================================================
  INTEGER, PARAMETER :: OBJ_BITMAP        = 7

!==============================================================================
! OwnerDraw parameters
!==============================================================================
  INTEGER, PARAMETER :: ODA_DRAWENTIRE    =  #0001
  INTEGER, PARAMETER :: ODA_SELECT        =  #0002
  INTEGER, PARAMETER :: ODA_FOCUS         =  #0004

  INTEGER, PARAMETER :: ODS_SELECTED      =  #0001
  INTEGER, PARAMETER :: ODS_FOCUS         =  #0010

!==============================================================================
! File parameters
!==============================================================================
  INTEGER, PARAMETER :: GENERIC_READ      = #80000000
  INTEGER, PARAMETER :: OF_READ           = #00000000
  INTEGER, PARAMETER :: OPEN_EXISTING     = 3

  INTEGER, PARAMETER :: HFILE_ERROR       = -1

  INTEGER, PARAMETER :: FILE_ATTRIBUTE_NORMAL         =  #00000080
  INTEGER, PARAMETER :: MOVEFILE_REPLACE_EXISTING     =  #00000001
  INTEGER, PARAMETER :: MOVEFILE_COPY_ALLOWED         =  #00000002
#endif

!==============================================================================
! Drive parameters
!==============================================================================
  INTEGER, PARAMETER :: DRIVE_REMOVABLE = 3
  INTEGER, PARAMETER :: DRIVE_REMOTE    = 4
  INTEGER, PARAMETER :: DRIVE_CDROM     = 5
#if !defined(LINUX)

!==============================================================================
! Memory parameters
!==============================================================================
  INTEGER, PARAMETER :: GMEM_FIXED           =  #0000
  INTEGER, PARAMETER :: GMEM_ZEROINIT        =  #0040

!==============================================================================
! Bitmap. parameters
!==============================================================================
  INTEGER, PARAMETER :: CBM_INIT          = #04       !  INITIALIZE BITMAP

  INTEGER, PARAMETER :: DIB_RGB_COLORS    = 0         !  COLOR TABLE IN RGBS

  INTEGER, PARAMETER :: SRCCOPY           = #00CC0020 !   DEST = SOURCE

  INTEGER, PARAMETER :: CF_BITMAP         = 2

  INTEGER, PARAMETER :: BI_RGB            = 0

!==============================================================================
! Misc. parameters
!==============================================================================
  INTEGER, PARAMETER :: GM_ADVANCED    = 2
  INTEGER, PARAMETER :: TRANSPARENT    = 1
  INTEGER, PARAMETER :: OPAQUE         = 2

  INTEGER, PARAMETER :: MM_ISOTROPIC   = 7
  INTEGER, PARAMETER :: HORZSIZE       = 4     !  HORIZONTAL SIZE IN MILLIMETERS
  INTEGER, PARAMETER :: VERTSIZE       = 6     !  VERTICAL SIZE IN MILLIMETERS
  INTEGER, PARAMETER :: HORZRES        = 8     !  HORIZONTAL WIDTH IN PIXELS
  INTEGER, PARAMETER :: VERTRES        = 10    !  VERTICAL WIDTH IN PIXELS

  INTEGER, PARAMETER :: LOGPIXELSX = 88
  INTEGER, PARAMETER :: LOGPIXELSY = 90
  INTEGER, PARAMETER :: PS_SOLID   = 0
  INTEGER, PARAMETER :: R2_NOT     = 6

  INTEGER, PARAMETER :: PBM_SETRANGE = (#0400+1)
  INTEGER, PARAMETER :: PBM_SETSTEP  = (#0400+4)
  INTEGER, PARAMETER :: PBM_SETPOS   = (#0400+2)
  INTEGER, PARAMETER :: BM_SETCHECK  = #00F1
  INTEGER, PARAMETER :: BM_SETSTATE  = #00F3

!==============================================================================
! WINDOWS API parameter and structure definitions
!==============================================================================
!
! STRUCTURES
!
!==============================================================================
! RECT
!==============================================================================
  TYPE  T_RECT
    SEQUENCE
    INTEGER(LONG)   :: left
    INTEGER(LONG)   :: top
    INTEGER(LONG)   :: right
    INTEGER(LONG)   :: bottom
  END TYPE  T_RECT

!==============================================================================
! POINT
!==============================================================================
  TYPE  T_POINT
    SEQUENCE
    INTEGER(LONG)    :: x
    INTEGER(LONG)    :: y
  END TYPE  T_POINT

!==============================================================================
! SIZE
!==============================================================================
  TYPE  T_SIZE
    SEQUENCE
    INTEGER(LONG)    :: cx
    INTEGER(LONG)    :: cy
  END TYPE  T_SIZE

!==============================================================================
! MSG
!==============================================================================
  TYPE  T_MSG
    SEQUENCE
    INTEGER(HANDLE)  :: hwnd
    INTEGER(UINT)    :: message
    INTEGER(fWPARAM) :: wParam
    INTEGER(fLPARAM) :: lParam
    INTEGER(DWORD)   :: time
  END TYPE  T_MSG

!==============================================================================
! WNDCLASS
!==============================================================================
  TYPE  T_WNDCLASSEX
    SEQUENCE
    INTEGER(UINT)   :: cbSize
    INTEGER(UINT)   :: style
    INTEGER(LPVOID) :: lpfnWndProc
    INTEGER(SINT)   :: cbClsExtra
    INTEGER(SINT)   :: cbWndExtra
    INTEGER(HANDLE) :: hInstance
    INTEGER(HANDLE) :: hIcon
    INTEGER(HANDLE) :: hCursor
    INTEGER(HANDLE) :: hbrBackground
    INTEGER(LPCSTR) :: lpszMenuName
    INTEGER(LPCSTR) :: lpszClassName
    INTEGER(HANDLE) :: hIconSm
  END TYPE  T_WNDCLASSEX

!==============================================================================
! FILETIME
!==============================================================================
  TYPE  T_FILETIME
    SEQUENCE
    INTEGER(DWORD)   :: dwLowDateTime
    INTEGER(DWORD)   :: dwHighDateTime
  END TYPE  T_FILETIME

!==============================================================================
! WIN32_FIND_DATA
!==============================================================================
  TYPE  T_WIN32_FIND_DATA
    SEQUENCE
    INTEGER(DWORD)      :: dwFileAttributes
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    TYPE (T_FILETIME)   :: ftCreationTime
    TYPE (T_FILETIME)   :: ftLastAccessTime
    TYPE (T_FILETIME)   :: ftLastWriteTime
    INTEGER(DWORD)      :: nFileSizeHigh
    INTEGER(DWORD)      :: nFileSizeLow
    INTEGER(DWORD)      :: dwReserved0
    INTEGER(DWORD)      :: dwReserved1
    CHARACTER(MAX_PATH) :: cFileName
    CHARACTER(14)       :: cAlternateFileName
    INTEGER(BYTE)       :: Pad2(2)
  END TYPE  T_WIN32_FIND_DATA

!==============================================================================
! OPENFILENAME
!==============================================================================
  TYPE  T_OPENFILENAME
    SEQUENCE
    INTEGER(DWORD)    :: lStructSize
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    INTEGER(HANDLE)   :: hwndOwner
    INTEGER(HANDLE)   :: hInstance
    INTEGER(LPCSTR)   :: lpstrFilter
    INTEGER(LPSTR)    :: lpstrCustomFilter
    INTEGER(DWORD)    :: nMaxCustFilter
    INTEGER(DWORD)    :: nFilterIndex
    INTEGER(LPSTR)    :: lpstrFile
    INTEGER(DWORD)    :: nMaxFile
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad2
#endif
    INTEGER(LPSTR)    :: lpstrFileTitle
    INTEGER(DWORD)    :: nMaxFileTitle
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad3
#endif
    INTEGER(LPCSTR)   :: lpstrInitialDir
    INTEGER(LPCSTR)   :: lpstrTitle
    INTEGER(DWORD)    :: Flags
    INTEGER(WORD)     :: nFileOffset
    INTEGER(WORD)     :: nFileExtension
    INTEGER(LPCSTR)   :: lpstrDefExt
    INTEGER(fLPARAM)  :: lCustData
    INTEGER(LPVOID)   :: lpfnHook
    INTEGER(LPSTR)    :: lpTemplateName
    INTEGER(LPVOID)   :: pvReserved
    INTEGER(DWORD)    :: dwReserved
    INTEGER(DWORD)    :: FlagsEx
  END TYPE  T_OPENFILENAME

!==============================================================================
! PAINTSTRUCT
!==============================================================================
  TYPE  T_PAINTSTRUCT
    SEQUENCE
    INTEGER(HANDLE)   :: hdc
    INTEGER(BOOL)     :: fErase
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    TYPE (T_RECT)     :: rcPaint
    INTEGER(BOOL)     :: fRestore
    INTEGER(BOOL)     :: fIncUpdate
    INTEGER(BYTE)     :: rgbReserved (32 )
  END TYPE  T_PAINTSTRUCT

!==============================================================================
! BITMAPINFOHEADER
!==============================================================================
  TYPE  T_BITMAPINFOHEADER
    SEQUENCE
    INTEGER(DWORD)    :: biSize
    INTEGER(LONG)     :: biWidth
    INTEGER(LONG)     :: biHeight
    INTEGER(WORD)     :: biPlanes
    INTEGER(WORD)     :: biBitCount
    INTEGER(DWORD)    :: biCompression
    INTEGER(DWORD)    :: biSizeImage
    INTEGER(LONG)     :: biXPelsPerMeter
    INTEGER(LONG)     :: biYPelsPerMeter
    INTEGER(DWORD)    :: biClrUsed
    INTEGER(DWORD)    :: biClrImportant
  END TYPE  T_BITMAPINFOHEADER

!==============================================================================
! RGBQUAD
!==============================================================================
  TYPE  T_RGBQUAD
    SEQUENCE
    INTEGER(BYTE)     :: rgbBlue
    INTEGER(BYTE)     :: rgbGreen
    INTEGER(BYTE)     :: rgbRed
    INTEGER(BYTE)     :: rgbReserved
  END TYPE  T_RGBQUAD

!==============================================================================
! BITMAPINFO
!==============================================================================
  TYPE  T_BITMAPINFO
    SEQUENCE
    TYPE (T_BITMAPINFOHEADER) :: bmiHeader
    TYPE (T_RGBQUAD)          :: bmiColors(1)
  END TYPE  T_BITMAPINFO

!==============================================================================
! BITMAPFILEHEADER
!==============================================================================
!DEC$ OPTIONS /ALIGN=(RECORDS=PACKED) /WARN=NOALIGN
  TYPE  T_BITMAPFILEHEADER
    SEQUENCE
    INTEGER(WORD)     :: bfTYPE
    INTEGER(DWORD)    :: bfSize
    INTEGER(WORD)     :: bfReserved1
    INTEGER(WORD)     :: bfReserved2
    INTEGER(DWORD)    :: bfOffBits
  END TYPE  T_BITMAPFILEHEADER
!DEC$ END OPTIONS

#endif
!==============================================================================
! SECURITY_ATTRIBUTES
!==============================================================================
  TYPE  T_SECURITY_ATTRIBUTES
    SEQUENCE
    INTEGER(DWORD)    :: nLength
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    INTEGER(LPVOID)   :: lpSecurityDescriptor
    INTEGER(BOOL)     :: bInheritHandle
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad2
#endif
  END TYPE  T_SECURITY_ATTRIBUTES

!==============================================================================
! SYSTEMTIME
!==============================================================================
!DEC$ PACK:1
  TYPE  T_SYSTEMTIME
    SEQUENCE
    INTEGER(WORD)     :: wYear
    INTEGER(WORD)     :: wMonth
    INTEGER(WORD)     :: wDayOfWeek
    INTEGER(WORD)     :: wDay
    INTEGER(WORD)     :: wHour
    INTEGER(WORD)     :: wMinute
    INTEGER(WORD)     :: wSecond
    INTEGER(WORD)     :: wMilliseconds
  END TYPE  T_SYSTEMTIME
!DEC$ PACK:

#if !defined(LINUX)
!==============================================================================
! OVERLAPPED
!==============================================================================
  TYPE  T_OVERLAPPED
    SEQUENCE
    INTEGER(ULONG_PTR):: Internal
    INTEGER(ULONG_PTR):: InternalHigh
    UNION
      MAP
        INTEGER(DWORD)    :: Offset
        INTEGER(DWORD)    :: OffsetHigh
      END MAP
      MAP
        INTEGER(PVOID)    :: ointer
      END MAP
    END UNION
    INTEGER(HANDLE)   :: hEvent
  END TYPE  T_OVERLAPPED

!==============================================================================
! XFORM
!==============================================================================
  TYPE  T_XFORM
    SEQUENCE
    REAL(SFLOAT)      :: eM11
    REAL(SFLOAT)      :: eM12
    REAL(SFLOAT)      :: eM21
    REAL(SFLOAT)      :: eM22
    REAL(SFLOAT)      :: eDx
    REAL(SFLOAT)      :: eDy
  END TYPE  T_XFORM

!==============================================================================
! PRINTDLG
!==============================================================================
!DEC$ OPTIONS /ALIGN=RECORDS=PACKED /WARN=NOALIGN
  TYPE  T_PRINTDLG
    SEQUENCE
    INTEGER(DWORD)    :: lStructSize
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    INTEGER(HANDLE)   :: hwndOwner
    INTEGER(HANDLE)   :: hDevMode
    INTEGER(HANDLE)   :: hDevNames
    INTEGER(HANDLE)   :: hDC
    INTEGER(DWORD)    :: Flags
    INTEGER(WORD)     :: nFromPage
    INTEGER(WORD)     :: nToPage
    INTEGER(WORD)     :: nMinPage
    INTEGER(WORD)     :: nMaxPage
    INTEGER(WORD)     :: nCopies
#if defined(_WIN64)
    INTEGER(WORD)     :: Pad2
#endif
    INTEGER(HANDLE)   :: hInstance
    INTEGER(fLPARAM)  :: lCustData
    INTEGER(LPVOID)   :: lpfnPrintHook
    INTEGER(LPVOID)   :: lpfnSetupHook
    INTEGER(LPCSTR)   :: lpPrintTemplateName
    INTEGER(LPCSTR)   :: lpSetupTemplateName
    INTEGER(HANDLE)   :: hPrintTemplate
    INTEGER(HANDLE)   :: hSetupTemplate
  END TYPE  T_PRINTDLG
!DEC$ END OPTIONS

!==============================================================================
! WINDOWPOS
!==============================================================================
  TYPE  T_WINDOWPOS
    SEQUENCE
    INTEGER(HANDLE)   :: hwnd
    INTEGER(HANDLE)   :: hwndInsertAfter
    INTEGER(SINT)     :: x
    INTEGER(SINT)     :: y
    INTEGER(SINT)     :: cx
    INTEGER(SINT)     ::  cy
    INTEGER(UINT)     :: flags
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
  END TYPE  T_WINDOWPOS

!==============================================================================
! DRAWITEMSTRUCT
!==============================================================================
  TYPE  T_DRAWITEMSTRUCT
    SEQUENCE
    INTEGER(UINT)     :: CtlType
    INTEGER(UINT)     :: CtlID
    INTEGER(UINT)     :: itemID
    INTEGER(UINT)     :: itemAction
    INTEGER(UINT)     :: itemState
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    INTEGER(HANDLE)   :: hwndItem
    INTEGER(HANDLE)   :: hDC
    TYPE (T_RECT)     :: rcItem
    INTEGER(ULONG_PTR)::itemData
  END TYPE  T_DRAWITEMSTRUCT

!==============================================================================
! DEVMODE
!==============================================================================
  INTEGER, PARAMETER :: CCHDEVICENAME = 32
  INTEGER, PARAMETER :: CCHFORMNAME   = 32
  TYPE T_DEVMODE
    SEQUENCE
    CHARACTER(CCHDEVICENAME) :: dmDeviceName
    INTEGER(WORD)     :: dmSpecVersion
    INTEGER(WORD)     :: dmDriverVersion
    INTEGER(WORD)     :: dmSize
    INTEGER(WORD)     :: dmDriverExtra
    INTEGER(DWORD)    :: dmFields
    INTEGER(SHORT)    :: dmOrientation
    INTEGER(SHORT)    :: dmPaperSize
    INTEGER(SHORT)    :: dmPaperLength
    INTEGER(SHORT)    :: dmPaperWidth
    INTEGER(SHORT)    :: dmScale
    INTEGER(SHORT)    :: dmCopies
    INTEGER(SHORT)    :: dmDefaultSource
    INTEGER(SHORT)    :: dmPrintQuality
    INTEGER(SHORT)    :: dmColor
    INTEGER(SHORT)    :: dmDuplex
    INTEGER(SHORT)    :: dmYResolution
    INTEGER(SHORT)    :: dmTTOption
    INTEGER(SHORT)    :: dmCollate
    CHARACTER(CCHFORMNAME) :: dmFormName
    INTEGER(WORD)     :: dmLogPixels
    INTEGER(DWORD)    :: dmBitsPerPel
    INTEGER(DWORD)    :: dmPelsWidth
    INTEGER(DWORD)    :: dmPelsHeight
    INTEGER(DWORD)    :: dmDisplayFlags
    INTEGER(DWORD)    :: dmDisplayFrequency
    INTEGER(DWORD)    :: dmICMMethod
    INTEGER(DWORD)    :: dmICMIntent
    INTEGER(DWORD)    :: dmMediaType
    INTEGER(DWORD)    :: dmDitherType
    INTEGER(DWORD)    :: dmICCManufacturer
    INTEGER(DWORD)    :: dmICCModel
    INTEGER(DWORD)    :: dmPanningWidth
    INTEGER(DWORD)    :: dmPanningHeight
  END TYPE T_DEVMODE

!==============================================================================
! DEVNAMES
!==============================================================================
  TYPE  T_DEVNAMES
    SEQUENCE
    INTEGER(WORD)     :: wDriverOffset
    INTEGER(WORD)     :: wDeviceOffset
    INTEGER(WORD)     :: wOutputOffset
    INTEGER(WORD)     :: wDefault
  END TYPE  T_DEVNAMES

!==============================================================================
! DOCINFO
!==============================================================================
  TYPE  T_DOCINFO
    SEQUENCE
    INTEGER(SINT)     :: cbSize
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad1
#endif
    INTEGER(LPCSTR)   :: lpszDocName
    INTEGER(LPCSTR)   :: lpszOutput
!#if (WINVER >= 0x0400)
    INTEGER(LPCSTR)   :: lpszDatatype;
    INTEGER(DWORD)    :: fwType;
#if defined(_WIN64)
    INTEGER(DWORD)    :: Pad2
#endif
!!DEC$ ENDIF /* WINVER */
   END TYPE  T_DOCINFO

!==============================================================================
! PALETTEENTRY
!==============================================================================
  TYPE  T_PALETTEENTRY
    SEQUENCE
    INTEGER(BYTE)     ::  peRed
    INTEGER(BYTE)     ::  peGreen
    INTEGER(BYTE)     ::  peBlue
    INTEGER(BYTE)     ::  peFlags
  END TYPE  T_PALETTEENTRY

!==============================================================================
! LOGBRUSH
!==============================================================================
  TYPE  T_LOGBRUSH
    SEQUENCE
    INTEGER(UINT)     :: lbStyle
    INTEGER(ULONG)    :: lbColor
#if defined(_M_IA64) || defined(_M_AMD64) || defined(__x86_64__)
    INTEGER(ULONG_PTR):: lbHatch ! TYPEDEFS  ULONG_PTR
#endif
#if defined( _M_IX86)
    INTEGER(ULONG)    :: lbHatch ! TYPEDEFS  ULONG_PTR
#endif
  END TYPE  T_LOGBRUSH

!==============================================================================
! LOGPALETTE
!==============================================================================
  TYPE  T_LOGPALETTE
    SEQUENCE
    INTEGER(WORD)     :: palVersion
    INTEGER(WORD)     :: palNumEntries
    TYPE (T_PALETTEENTRY) :: palPalEntry(1)
  END TYPE  T_LOGPALETTE

#endif
#endif
END MODULE winAPI_fd

MODULE MaxLength_fd

  USE DefSize_fd

  INTEGER, PARAMETER :: SIZE_CNULL = 1
  INTEGER, PARAMETER :: MAX_PATHF  = PATH_MAXLENGTH
  INTEGER, PARAMETER :: MAX_PATHC  = MAX_PATHF + SIZE_CNULL
  INTEGER, PARAMETER :: MAX_FILEF  = PATH_MAXLENGTH
  INTEGER, PARAMETER :: MAX_FILEC  = MAX_FILEF + SIZE_CNULL
  INTEGER, PARAMETER :: MAX_LARGEF = 2*PATH_MAXLENGTH + 12

END MODULE maxLength_fd
