MODULE guiAPI

  INTERFACE

!==============================================================================
! ABORTDLG
!==============================================================================
    FUNCTION ABORTDLG(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: ABORTDLG
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'ABORTDLG'     :: ABORTDLG
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: ABORTDLG
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION ABORTDLG

!==============================================================================
! ABORTPROC
!==============================================================================
    INTEGER FUNCTION ABORTPROC(hdc,nCode)
!DEC$ ATTRIBUTES DEFAULT                                    :: ABORTPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'ABORTPROC'     :: ABORTPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hdc
      INTEGER              nCode
    END FUNCTION ABORTPROC

!==============================================================================
! DIALPROC
!==============================================================================
    FUNCTION DIALPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: DIALPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DIALPROC'     :: DIALPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: DIALPROC
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION DIALPROC

!==============================================================================
! MAINWNDPROC
!==============================================================================
    FUNCTION MAINWNDPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: MAINWNDPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'MAINWNDPROC'  :: MAINWNDPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: MAINWNDPROC
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION MAINWNDPROC

!==============================================================================
! OpenHook
!==============================================================================
    FUNCTION OpenHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: OpenHook
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'OpenHook'     :: OpenHook
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: OpenHook
      INTEGER(POINTER_LEN) HDLG
      INTEGER              MESSAGE
      INTEGER(POINTER_LEN) WPARAM
      INTEGER(POINTER_LEN) LPARAM
    END FUNCTION OpenHook

!==============================================================================
! PLOTWNDPROC
!==============================================================================
    FUNCTION PLOTWNDPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: PLOTWNDPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PLOTWNDPROC'  :: PLOTWNDPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: PLOTWNDPROC
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION PLOTWNDPROC

!==============================================================================
! PrintHook
!==============================================================================
    FUNCTION PrintHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: PrintHook
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PrintHook'    :: PrintHook
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: PrintHook
      INTEGER(POINTER_LEN) HDLG
      INTEGER              MESSAGE
      INTEGER(POINTER_LEN) WPARAM
      INTEGER(POINTER_LEN) LPARAM
    END FUNCTION PrintHook

!==============================================================================
! PROGPROC
!==============================================================================
    FUNCTION PROGPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: PROGPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PROGPROC'     :: PROGPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: PROGPROC
      INTEGER(POINTER_LEN) HDLG
      INTEGER              MESSAGE
      INTEGER(POINTER_LEN) WPARAM
      INTEGER(POINTER_LEN) LPARAM
    END FUNCTION PROGPROC

!==============================================================================
! STATWNDPROC
!==============================================================================
    FUNCTION STATWNDPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: STATWNDPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'STATWNDPROC'  :: STATWNDPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: STATWNDPROC
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION STATWNDPROC

!==============================================================================
! SaveAsHook
!==============================================================================
    FUNCTION SaveAsHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: SaveAsHook
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'SaveAsHook'   :: SaveAsHook
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: SaveAsHook
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION SaveAsHook

!==============================================================================
! ToolCallBack
!==============================================================================
    INTEGER FUNCTION  ToolCallBack(iCall,iMessage,iParm)
!DEC$ ATTRIBUTES DEFAULT                                             :: ToolCallBack
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: iCall
      INTEGER(POINTER_LEN) :: iMessage
      INTEGER,DIMENSION(*) :: iParm
    END FUNCTION ToolCallBack

!==============================================================================
! TOOLPROC
!==============================================================================
    FUNCTION TOOLPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES DEFAULT                                   :: TOOLPROC
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'TOOLPROC'     :: TOOLPROC
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) :: TOOLPROC
      INTEGER(POINTER_LEN)    HDLG
      INTEGER                 MESSAGE
      INTEGER(POINTER_LEN)    WPARAM
      INTEGER(POINTER_LEN)    LPARAM
    END FUNCTION TOOLPROC

  END INTERFACE
END MODULE guiAPI
