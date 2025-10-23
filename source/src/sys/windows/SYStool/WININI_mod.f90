!==============================================================================
!Use Windows functions
!==============================================================================
MODULE WININI
  INTERFACE

!==============================================================================
! GetPrivateProfileInt
!==============================================================================
    INTEGER(UINT) FUNCTION GetPrivateProfileInt( lpAppName, lpKeyName, nDefault, lpFileName ) BIND(C, NAME="GetPrivateProfileIntA")
      USE winAPI_fd
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpAppName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpKeyName(*)
      INTEGER(C_INT),         VALUE, INTENT(IN)  :: nDefault
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpFileName(*)
    END FUNCTION GetPrivateProfileInt

!==============================================================================
! GetPrivateProfileString
!==============================================================================
    INTEGER(BOOL) FUNCTION GetPrivateProfileString( lpAppName, lpKeyName, lpDefault, lpReturnedString, nSize, lpFileName ) BIND(C, NAME="GetPrivateProfileStringA")
      USE winAPI_fd
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpAppName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpKeyName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpDefault(*)
      CHARACTER(KIND=C_CHAR),        INTENT(OUT) :: lpReturnedString(*)
      INTEGER(C_INT),         VALUE, INTENT(IN)  :: nSize
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpFileName(*)
    END FUNCTION GetPrivateProfileString

!==============================================================================
! WritePrivateProfileString
!==============================================================================
    INTEGER(BOOL) FUNCTION WritePrivateProfileString( lpAppName, lpKeyName, lpString, lpFileName ) BIND(C, NAME="WritePrivateProfileStringA")
      USE winAPI_fd
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpAppName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpKeyName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpString(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpFileName(*)
    END FUNCTION WritePrivateProfileString

    INTEGER(BOOL) FUNCTION DeletePrivateProfileString( lpAppName, lpKeyName, lpString, lpFileName ) BIND(C, NAME="WritePrivateProfileStringA")
      USE winAPI_fd
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_INTPTR_T
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpAppName(*)
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpKeyName(*)
      INTEGER(C_INTPTR_T),    VALUE, INTENT(IN)  :: lpString
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpFileName(*)
    END FUNCTION DeletePrivateProfileString

    INTEGER(BOOL) FUNCTION DeletePrivateProfileSection( lpAppName, lpKeyName, lpString, lpFileName ) BIND(C, NAME="WritePrivateProfileStringA")
      USE winAPI_fd
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_INTPTR_T
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpAppName(*)
      INTEGER(C_INTPTR_T),    VALUE, INTENT(IN)  :: lpKeyName
      INTEGER(C_INTPTR_T),    VALUE, INTENT(IN)  :: lpString
      CHARACTER(KIND=C_CHAR),        INTENT(IN)  :: lpFileName(*)
    END FUNCTION DeletePrivateProfileSection

  END INTERFACE
END MODULE WININI
