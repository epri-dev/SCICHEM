MODULE dib

  INTERFACE

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    FUNCTION OpenDIB(pString)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'OpenDIB' :: OpenDIB
!DEC$ ATTRIBUTES REFERENCE                   :: pString
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: OpenDib
      CHARACTER(*) pString
    END FUNCTION OpENDIB

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    INTEGER FUNCTION DibInfo(hFile,hdr)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'DibInfo' :: DibInfo
!DEC$ ATTRIBUTES VALUE                       :: hFile
!DEC$ ATTRIBUTES REFERENCE                   :: hdr
      USE winAPI_fd
      INTEGER(POINTER_LEN)        hFile
      TYPE ( T_BITMAPINFOHEADER ) hdr
    END FUNCTION DibInfo

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    FUNCTION CreateDibPalette(hDIB)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'CreateDibPalette' :: CreateDibPalette
!DEC$ ATTRIBUTES VALUE                                :: hDIB
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: CreateDibPalette
      INTEGER(POINTER_LEN)    hDIB
    END FUNCTION CreateDibPalette

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    FUNCTION BitmapFromDib(hDIB,hPal)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'BitmapFromDib' :: BitmapFromDib
!DEC$ ATTRIBUTES VALUE                    :: hDIB
!DEC$ ATTRIBUTES VALUE                    :: hPal
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: BitmapFromDib
      INTEGER(POINTER_LEN)    hDIB
      INTEGER(POINTER_LEN)    hPal
    END FUNCTION BitmapFromDib

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    FUNCTION DibFromBitmap(hBit,biStyle,biBits,hPal)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'DibFromBitmap' :: DibFromBitmap
!DEC$ ATTRIBUTES VALUE                    :: hBit
!DEC$ ATTRIBUTES VALUE                    :: biStyle
!DEC$ ATTRIBUTES VALUE                    :: biBits
!DEC$ ATTRIBUTES VALUE                    :: hPal
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: DibFromBitmap
      INTEGER(POINTER_LEN)    hBit
      INTEGER                 biStyle
      INTEGER                 biBits
      INTEGER(POINTER_LEN)    hPal
    END FUNCTION DibFromBitmap

!==============================================================================
! CreateCompatibleBitmap
!==============================================================================
    INTEGER FUNCTION WriteDIB(pString,hDIB)
!DEC$ ATTRIBUTES DECORATE, ALIAS : 'WriteDIB' :: WriteDIB
!DEC$ ATTRIBUTES VALUE               :: pString
!DEC$ ATTRIBUTES VALUE               :: hDIB
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) Pstring
      INTEGER(POINTER_LEN) hDIB
    END FUNCTION WriteDIB

  END INTERFACE

END MODULE dib

MODULE Pcx

  INTEGER,PARAMETER :: BTYPE_BMP = 0
  INTEGER,PARAMETER :: BTYPE_PCX = 1

  INTERFACE

!==============================================================================
! pcxSize
!==============================================================================
    INTEGER FUNCTION  pcxSize (dummy0,dummy1,dummy2)
!DEC$ ATTRIBUTES DEFAULT                              :: pcxSize
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'pcxSize' :: pcxSize
!DEC$ ATTRIBUTES REFERENCE                            :: dummy0
!DEC$ ATTRIBUTES REFERENCE                            :: dummy1
!DEC$ ATTRIBUTES REFERENCE                            :: dummy2
      CHARACTER(*) dummy0
      INTEGER      dummy1
      INTEGER      dummy2
    END FUNCTION pcxSize

!==============================================================================
! pcxLoad
!==============================================================================
    INTEGER FUNCTION  pcxLoad (dummy0,dummy1,dummy2)
!DEC$ ATTRIBUTES DEFAULT                              :: pcxLoad
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'pcxLoad' :: pcxLoad
!DEC$ ATTRIBUTES REFERENCE                            :: dummy0
      USE winAPI_fd, ONLY: POINTER_LEN
      CHARACTER(*)              dummy0
      INTEGER(POINTER_LEN)      dummy1
      INTEGER(POINTER_LEN)      dummy2
    END FUNCTION pcxLoad

!==============================================================================
! pcxSave
!==============================================================================
    INTEGER FUNCTION  pcxSave (dummy0,dummy1,dummy2)
!DEC$ ATTRIBUTES DEFAULT                              :: pcxSave
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'pcxSave' :: pcxSave
!DEC$ ATTRIBUTES REFERENCE                            :: dummy0
      USE winAPI_fd, ONLY: POINTER_LEN
      CHARACTER(*)              dummy0
      INTEGER(POINTER_LEN)      dummy1
      INTEGER(POINTER_LEN)      dummy2
    END FUNCTION pcxSave

  END INTERFACE

END MODULE Pcx
