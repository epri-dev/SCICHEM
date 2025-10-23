MODULE mouse_fi
USE mouse_fd
USE SwinAPI

TYPE( T_POINT ) MousePnt

TYPE( T_RECT ) MouseBox,MouseBoxS,Box,BoxS

LOGICAL MouseOn,MouseInBox,TimerOn,LBDown,BoxVisible,Zooming
LOGICAL MouseSave(MAXMOUSE),ZoomOK,ZoomDraw

INTEGER(POINTER_LEN) HcurCross,HcurArrow,HpenNull,HPwnd,MouseDC,MouseDB
INTEGER MouseBoxHeight,MouseBoxWidth
INTEGER(POINTER_LEN) HcurZoom,HcurSlice,HcurPick
INTEGER MouseBoxHeightS,MouseBoxWidthS
INTEGER BoxHeight,BoxWidth
INTEGER(POINTER_LEN) HDB,HCntrl
INTEGER TimerID,SliceSet,SliceNull,MouseLevel
Real    AspectRatio

END MODULE mouse_fi
