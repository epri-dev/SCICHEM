!******************************************************************************
!     MouseInit - Initializes the Mouse tracking flags
!******************************************************************************

SUBROUTINE MouseInit

USE mouse_fi

IMPLICIT NONE

MouseOn    = .FALSE.
TimerOn    = .FALSE.
MouseInBox = .FALSE.
LBDown     = .FALSE.
BoxVisible = .FALSE.
Zooming    = .FALSE.
ZoomOK     = .FALSE.

MouseLevel = 1

RETURN
END

!******************************************************************************
!     MouseStart - Starts the Mouse tracking
!******************************************************************************

SUBROUTINE MouseStart(idc,ihwnd,ihdb)

USE resource_fd
USE mouse_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) idc !Device Context Handle
INTEGER(POINTER_LEN) ihwnd !Window Handle
INTEGER(POINTER_LEN) ihdb !Dialog Box handle

INTEGER irv

type  (T_POINT) pts(2)

!---- Save Window Handles

HPwnd  = ihwnd
HDB    = ihdb

!---- Initialize flags

MouseOn    = .TRUE.
TimerOn    = .FALSE.
MouseInBox = .FALSE.
LBDown     = .FALSE.
BoxVisible = .FALSE.
Zooming    = .FALSE.
ZoomOK     = .FALSE.

MouseLevel = 1

!---- Initialize MouseBox - Logical Units

CALL SetMouseBox(MouseBoxS)
MouseBoxHeightS = MouseBoxS%bottom - MouseBoxS%top
MouseBoxWidthS  = MouseBoxS%right  - MouseBoxS%left

!---- Initialize MouseBox - Device Units

MouseBox = MouseBoxS
pts(1)%y = MouseBox%bottom
pts(1)%x = MouseBox%left
pts(2)%y = MouseBox%top
pts(2)%x = MouseBox%right
irv = LPtoDP(idc,pts(1),2)
MouseBox%bottom = pts(1)%y
MouseBox%left   = pts(1)%x
MouseBox%top    = pts(2)%y
MouseBox%right  = pts(2)%x

MouseBoxHeight  = MouseBox%bottom - MouseBox%top
MouseBoxWidth   = MouseBox%right  - MouseBox%left

!---- Initialize ZoomBox

Box       = MouseBox
BoxWidth  = MouseBoxWidth
BoxHeight = MouseBoxHeight

IF( BoxHeight > 0 )THEN
  AspectRatio = FLOAT(BoxWidth)/FLOAT(BoxHeight)
ELSE
  AspectRatio = 1.0
END IF

RETURN

END

!******************************************************************************
!     MouseDown - Starts the Zoom Process
!******************************************************************************

LOGICAL FUNCTION MouseDown(ix,iy,flag)
USE resource_fd
USE mouse_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER ix !Mouse X Position
INTEGER iy !Mouse Y Position
LOGICAL   flag !True=Right False=Left

INTEGER irv,ilev
INTEGER(POINTER_LEN)iwnd

!---- Initialize Return Value

MouseDown = .FALSE.

IF( flag )THEN
  IF( SliceSet == IDB_SLICE)RETURN !Ignore Right Button on Slice
  IF( SliceSet == IDB_ZOOM)RETURN !Ignore Right Button on Zoom
  IF( SliceSet == NOT_SET_I)RETURN !Ignore Right Button all together
ELSE
  IF( SliceSet == IDB_PICK)RETURN !Ignore Left Button on Pick
END IF

!---- Check for Mouse Tracking

IF( .NOT.MouseOn )THEN

!------ OFF - Check for possible ZOOMing

  IF( .NOT. Zooming )THEN !OFF - do nothing
    RETURN !
  ELSE !ON - prepare to restart ZOOM
    CALL MouseClear(.FALSE.) !  Stop ZOOMing
    irv = ReleaseCapture() !  Release Mouse
  END IF !
  Zooming = .TRUE. !  Set ZOOM flag
END IF

!---- Check Mouse location

MousePnt%x = ix
MousePnt%y = iy
MouseInBox = PtInRect( MouseBox, MousePnt ) /= FALSE

!---- Is Mouse in MouseBox

IF( MouseInBox )THEN !YES
  IF( Zooming )THEN !  Check ZOOM flag
    CALL ZoomStart !    YES - Restart ZOOM
  ELSE !
    Zooming = .TRUE. !    NO - Set ZOOM flag
    irv = SetCapture(HPwnd) !         Capture mouse
    IF( SliceSet == IDB_SLICE )THEN
      CALL ZoomStart
      irv = SetCursor(HcurCross) !    Set CrossHairs
      CALL FindHwndListId(IDB_SLICE,iwnd,ilev)
      irv = SetFocus(iwnd)
    ELSE
      IF( flag )THEN
        CALL CallButton(HDB,IDB_BUTTON18,irv) !         Start Pick Dialog
      ELSE
        CALL CallButton(HDB,IDB_BUTTON15,irv) !         Start ZOOM Dialog
      END IF
    END IF
  END IF !
ELSE !NO
  IF( Zooming )THEN !  Check ZOOM flag
    Zooming = .FALSE. !    YES - Reset ZOOM Flag
    IF( SliceSet == IDB_SLICE )THEN
      CALL MouseClear(.FALSE.)
    ELSE IF( SliceSet == IDB_PICK )THEN
      CALL CallButton(MouseDB,ID_CANCEL,irv) !          Cancel DialogBox
    ELSE
      CALL CallButton(MouseDB,ID_CANCEL,irv) !          Cancel DialogBox
    END IF
  END IF !
END IF !

!---- Set Return value

MouseDown = .TRUE.
RETURN

END
!*******************************************************************************
!     MouseUp - End Active Zoom
!*******************************************************************************
LOGICAL FUNCTION MouseUp()
USE mouse_fi

IMPLICIT NONE

!integer ix !Mouse X Position
!integer iy !Mouse Y Position

INTEGER irv

!---- Initialize Return Value

MouseUp = .FALSE.

!---- Check for Mouse Tracking - Return if not ON

IF( .NOT.MouseOn)RETURN

!---- Was LEFT Button DOWN - Was I Zooming

IF( LBDown )THEN
  LBDown  = .FALSE. !Rest Button flag
  Zooming = .TRUE. !Set ZOOM flag
  ZoomOK  = .TRUE. !Set ZOOMOK flag
  MouseOn = .FALSE. !Stop Mouse Tracking
  irv = ReleaseCapture() !Release Mouse
END IF

!---- Set return value

MouseUp = .TRUE.

RETURN
END
!******************************************************************************
!     MouseMove - Tracks the mouse movement
!******************************************************************************
LOGICAL FUNCTION MouseMove(ix,iy)
USE resource_fd
USE mouse_fi

IMPLICIT NONE

INTEGER  PICK_SIZE
PARAMETER (PICK_SIZE = 8)

INTEGER ix !Mouse X Position
INTEGER iy !Mouse Y Position

!---- Initialize Return Value

MouseMove = .FALSE.

!---- Check for Mouse Tracking - Return if not ON

IF( .NOT.MouseOn)RETURN

!---- Is Left Buttom Down - DRAG movement

IF( LBDown )THEN
  IF( BoxVisible)CALL FlashBox !Flash ZOOM Box
  IF( SliceSet == IDB_PICK )THEN
    Box%right   = MAX(ix        ,MouseBox%left) !Set new ZOOM Box location
    Box%right   = MIN(Box%right ,MouseBox%right) !
    Box%bottom  = MAX(iy        ,MouseBox%top) !
    Box%bottom  = MIN(Box%bottom,MouseBox%bottom) !
    Box%left    = Box%right  - PICK_SIZE
    Box%top     = Box%bottom - PICK_SIZE
    Box%right   = Box%right  + PICK_SIZE
    Box%bottom  = Box%bottom + PICK_SIZE
  ELSE
    Box%right   = MAX(ix        ,MouseBox%left) !Set new ZOOM Box location
    Box%right   = MIN(Box%right ,MouseBox%right) !
    Box%bottom  = MAX(iy        ,MouseBox%top) !
    Box%bottom  = MIN(Box%bottom,MouseBox%bottom) !
  END IF
  BoxWidth    = Box%right-Box%left !
  BoxHeight   = Box%bottom-Box%top !
!        if(BoxHeight .ne. 0)then                                       !Draw new BOX if visible
    CALL DrawBox !
    BoxVisible = .TRUE. !
!        else                                                           !Else set to Zero Box
!          Box%right = Box%left                                         !
!          BoxWidth = 0                                                 !
!        end if                                                         !
  CALL ZoomReset !Reset ZOOM Dialog Box parameters
END IF !

!---- Check Mouse location

MousePnt%x = ix
MousePnt%y = iy
MouseInBox = PtInRect( MouseBox, MousePnt ) /= FALSE

!---- Set return value

MouseMove = .TRUE.

RETURN

END

!*******************************************************************************
!     MousePause - Pause Mouse
!*******************************************************************************
SUBROUTINE MousePause

USE mouse_fi

IMPLICIT NONE

MouseSave(MouseLevel) = MouseOn
MouseLevel = MIN(MouseLevel + 1,MAXMOUSE)
MouseOn = .FALSE.

RETURN
END
!*******************************************************************************
!     EnablePick - EnablePick
!*******************************************************************************
SUBROUTINE EnablePick

USE mouse_fi

IMPLICIT NONE

SliceNull = NULL
SliceSet  = SliceNull

RETURN
END
!*******************************************************************************
!     DisablePick - DisablePick
!*******************************************************************************
SUBROUTINE DisablePick

USE default_fd
USE mouse_fi

IMPLICIT NONE

SliceNull = NOT_SET_I
SliceSet  = SliceNull

RETURN
END
!*******************************************************************************
!     MouseResume - Resume Mouse
!*******************************************************************************
SUBROUTINE MouseResume

USE mouse_fi

IMPLICIT NONE

MouseLevel = MAX(MouseLevel - 1,1)
MouseOn = MouseSave(MouseLevel)

RETURN
END
!*******************************************************************************
!     MouseClear - Clear Mouse
!*******************************************************************************
SUBROUTINE MouseClear(flag)

USE resource_fd
USE mouse_fi
USE SwinAPI, ONLY: POINTER_LEN

IMPLICIT NONE

LOGICAL flag

INTEGER irv
INTEGER(POINTER_LEN) id

!---- reset flags

Zooming = .FALSE.
ZoomOK  = .FALSE.
MouseOn = .TRUE.

!---- If TIMER is set - Cancel

If(TimerOn )THEN
  id = TimerID
  irv = KillTimer(HPwnd,id)
  TimerOn = .FALSE.
END IF

!---- if ZOOM Box has been drawn - erase

IF( BoxVisible )THEN
  CALL DrawBox
  BoxVisible = .FALSE.
END IF

IF( flag )THEN
!        SliceSet = NOT_SET_I !IDB_ZOOM
  SliceSet = SliceNull !IDB_ZOOM
END IF

RETURN
END
!******************************************************************************
!     MouseCheck - Checks for Mouse usage
!******************************************************************************
LOGICAL FUNCTION MouseCheck()
USE mouse_fi

IMPLICIT NONE

MouseCheck = MouseOn .AND. Zooming .AND. ZoomOK

RETURN

END
!******************************************************************************
!     MouseCursor - Changes the cursor
!******************************************************************************
SUBROUTINE MouseCursor

USE resource_fd
USE mouse_fi

IMPLICIT NONE

INTEGER ii

IF( MouseInBox )THEN !In MouseBox
  IF( SliceSet == IDB_SLICE )THEN
    IF( Zooming )THEN !  Zooming
      ii = SetCursor(HcurCross) !    Set CrossHairs
    ELSE !  Not Zooming
      ii = SetCursor(HcurSlice) !    Set MagnifyingGlass
    END IF
!          ii = SetCursor(HcurSlice)                               !    Set Scissors
  ELSE IF( SliceSet == IDB_PICK )THEN
    ii = SetCursor(HcurPick) !    Set Scissors
  ELSE
    IF( Zooming )THEN !  Zooming
      ii = SetCursor(HcurCross) !    Set CrossHairs
    ELSE !  Not Zooming
      ii = SetCursor(HcurZoom) !    Set MagnifyingGlass
    END IF
  END IF !
ELSE !Out of MouseBox
  ii = SetCursor(HcurArrow) !  Set Arrow
END IF !

RETURN
END
!******************************************************************************
!     MouseStop - Turns off Mouse Routines
!******************************************************************************
SUBROUTINE MouseStop

USE mouse_fi

IMPLICIT NONE

MouseOn = .FALSE.

RETURN

END
!******************************************************************************
!     MouseMap - Sets NCAR Mapping functionc
!******************************************************************************
SUBROUTINE MouseMap(ihwnd,ihdc)

USE winAPI_fd, ONLY: POINTER_LEN

IMPLICIT NONE

INTEGER(POINTER_LEN) ihwnd,ihdc

CALL SetNCARWindow(ihwnd) !NCAR Window handle
CALL SetNCARDC(ihdc,ihdc) !NCAR Device context
CALL SetMapNCAR(1) !NCAR Mapping
RETURN

END
!*******************************************************************************
!     MoveMouse - Move Mouse to a new location
!*******************************************************************************
LOGICAL FUNCTION MoveMouse(x1,y1,x2,y2)
USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE mouse_fi
USE pltchoice_fi

IMPLICIT NONE

REAL x1,y1 !New position in real coordinates
REAL x2,y2 !New position in real coordinates

LOGICAL MouseDown,MouseUp,MouseMove

REAL          BoxR(4)
TYPE( T_RECT ) BoxI
INTEGER BI(4)

LOGICAL lOK

INTEGER irv

type  (T_POINT) pts(2)

MoveMouse = .FALSE.

IF( MouseOn )THEN
  MoveMouse = SliceSet == IDB_SLICE
  RETURN
ELSE IF( SliceSet == IDB_SLICE )THEN
  IF( .NOT.lplotOK .OR. (PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE) .OR. &
                        (PlotDef(BASE_LEVEL)%Field%Category == HP_HINT) )THEN
    MoveMouse = .TRUE.
    RETURN
  END IF
END IF

lOK = x1 /= DEF_VAL_R .AND. x1 /= NOT_SET_R
lOK = lOK .AND. x2 /= DEF_VAL_R .AND. x2 /= NOT_SET_R
lOK = lOK .AND. y1 /= DEF_VAL_R .AND. y1 /= NOT_SET_R
lOK = lOK .AND. y2 /= DEF_VAL_R .AND. y2 /= NOT_SET_R

IF( .NOT.lOK )THEN
  MoveMouse = .TRUE.
  RETURN
END IF

MouseDC = GetDC(HPwnd)
CALL MouseMap(HPwnd,MouseDC) !Set Mapping

!---- Get limits in Real coordinates

BI(1) = MouseBoxS%left
BI(2) = MouseBoxS%top
BI(3) = MouseBoxS%right
BI(4) = MouseBoxS%bottom
CALL LPtoReal(BI,BoxR)

!---- Limit Position

x1 = MAX(x1,BoxR(1))
x1 = MIN(x1,BoxR(3))
y1 = MAX(y1,BoxR(4))
y1 = MIN(y1,BoxR(2))

x2 = MAX(x2,BoxR(1))
x2 = MIN(x2,BoxR(3))
y2 = MAX(y2,BoxR(4))
y2 = MIN(y2,BoxR(2))

IF( SliceSet == IDB_ZOOM )THEN
  IF( x2 < x1 )THEN
    BoxR(1) = x1
    x1 = x2
    x2 = BoxR(1)
  END IF

  IF( y2 < y1 )THEN
    BoxR(1) = y1
    y1 = y2
    y2 = BoxR(1)
  END IF
END IF

!---- Convert to Logical units

BoxR(1) = x1
BoxR(2) = y1
BoxR(3) = x2
BoxR(4) = y2
CALL RealtoLP(BoxR,BI,2)
BoxI%left   = BI(1)
BoxI%top    = BI(2)
BoxI%right  = BI(3)
BoxI%bottom = BI(4)

!---- Convert to Device units

pts(1)%y = BoxI%bottom
pts(1)%x = BoxI%left
pts(2)%y = BoxI%top
pts(2)%x = BoxI%right
irv = LPtoDP(MouseDC,pts(1),2) !Convert to Device units
BoxI%bottom = pts(1)%y
BoxI%left   = pts(1)%x
BoxI%top    = pts(2)%y
BoxI%right  = pts(2)%x

irv = ReleaseDC(HPwnd,MouseDC)

BoxI%left   = MIN(BoxI%left  ,MouseBox%right-1)
BoxI%top    = MIN(BoxI%top   ,MouseBox%bottom-1)
BoxI%right  = MIN(BoxI%right ,MouseBox%right-1)
BoxI%bottom = MIN(BoxI%bottom,MouseBox%bottom-1)

!---- Move Mouse by simulating RightClick Down/Up sequence

MoveMouse = MouseDown(BoxI%left,BoxI%top,(SliceSet==IDB_PICK))
MoveMouse = MoveMouse .AND. MouseMove(BoxI%right,BoxI%bottom)
MoveMouse = MoveMouse .AND. MouseUp()

RETURN

END
!******************************************************************************
!     LPtoReal - Maps from logical units to real units
!******************************************************************************
SUBROUTINE LPtoReal(BI,BR)

IMPLICIT NONE

INTEGER BI(4) !Logical units
REAL    BR(4) !Real Units

INTEGER ixorg,iyorg
REAL xorg,yorg,xend,yend,ratx,raty

!---- Get NCAR Transfor parameters

CALL GetNCARTransformX(xorg,xend,ratx,ixorg)
CALL GetNCARTransformY(yorg,yend,raty,iyorg)

!---- Transform first point

BR(1) = FLOAT(BI(1)-ixorg)/ratx + xorg
BR(2) = FLOAT(BI(2)-iyorg)/raty + yorg

!---- Transform second point

BR(3) = FLOAT(BI(3)-ixorg)/ratx + xorg
BR(4) = FLOAT(BI(4)-iyorg)/raty + yorg

RETURN
END
!******************************************************************************
!     RealtoLP - Maps from real units to logical units
!******************************************************************************
SUBROUTINE RealtoLP(BR,BI,n)

IMPLICIT NONE

INTEGER n !number of point to convert
INTEGER BI(4) !Logical units
REAL    BR(4) !Real Units

INTEGER ixorg,iyorg,i,j
REAL xorg,yorg,xend,yend,ratx,raty

!---- Get NCAR Transfor parameters

CALL GetNCARTransformX(xorg,xend,ratx,ixorg)
CALL GetNCARTransformY(yorg,yend,raty,iyorg)

!---- Transform points

DO i = 1,n
  j = (i-1)*2 + 1
  BI(j  ) = ixorg + NINT(ratx*(BR(j  ) - xorg))
  BI(j+1) = iyorg + NINT(raty*(BR(j+1) - yorg))
END DO

RETURN
END
!******************************************************************************
!     MouseLoad - Loads Mouse resources
!******************************************************************************
SUBROUTINE MouseLoad(inst)

USE resource_fd
USE mouse_fi
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN) inst !Instance handle

INTEGER(POINTER_LEN) icur, jnst

CHARACTER(12) Load

INTEGER icol,isiz

!---- Load Cursors

jnst = 0
icur = IDC_CROSS
HcurCross = LoadCursor( jnst, icur )
icur = IDC_ARROW
HcurArrow = LoadCursor( jnst, icur )

Load = 'ZoomCursor'//CHAR(0)
HcurZoom  = LoadCursor( inst, Load )

Load = 'SLICECursor'//CHAR(0)
HcurSlice  = LoadCursor( inst, Load )

Load = 'PICKCursor'//CHAR(0)
HcurPick  = LoadCursor( inst, Load )

!---- Load pen - ZOOM Box drawing

icol = RGB(0,0,0)
isiz = 2
HpenNull = CreatePen( PS_SOLID, isiz, icol)

RETURN

END
!******************************************************************************
!     MouseExit - Deletes Mouse resources
!******************************************************************************
SUBROUTINE MouseExit

USE mouse_fi

IMPLICIT NONE

INTEGER irv

irv = DeleteObject(HpenNull)

RETURN

END
!******************************************************************************
!     SetMouseBox - Initializes the MouseBox location
!******************************************************************************
SUBROUTINE SetMouseBox(MouseBox)

USE SwinAPI

IMPLICIT NONE

TYPE( T_RECT ) MouseBox !MouseBox

INTEGER ix,jx,iy,jy,ixorg,iyorg
REAL xorg,yorg,xend,yend,ratx,raty

!---- Get NCAR Transform parameters

CALL GetNCARTransformX(xorg,xend,ratx,ixorg)
CALL GetNCARTransformY(yorg,yend,raty,iyorg)

!---- Compute Mouse limits

ix = ixorg
iy = iyorg
jx = ixorg + NINT(ratx*(xend-xorg))
jy = iyorg + NINT(raty*(yend-yorg))

!---- Set MouseBox

MouseBox%left   = ix
MouseBox%top    = jy
MouseBox%right  = jx
MouseBox%bottom = iy

RETURN

END
!*******************************************************************************
!     Flash Zoom Box - Change Visibility of Temporary Zoom Box Outline
!*******************************************************************************
SUBROUTINE FlashBox

USE mouse_fi

IMPLICIT NONE

!---- Toggle visibility parameter

BoxVisible = .NOT.BoxVisible

!---- Redraw Box

Call DrawBox

RETURN
END
!*******************************************************************************
!     Draw Zoom Box - Draw Temporary Zoom Box Outline
!*******************************************************************************
SUBROUTINE DrawBox

USE pcscipuf_fi
USE mouse_fi

IMPLICIT NONE

INTEGER irv

irv = InvalidateRect(HPwnd, MouseBox, FALSE) !Invalidate MouseBox
lflashit = .TRUE. !Set WM_PAINT type
irv = UpdateWindow(HPwnd) !Redraw Window

RETURN
END
!*******************************************************************************
!     Redraw Mouse Box in response to WM_PAINT Command
!*******************************************************************************
SUBROUTINE FlashPaint

USE mouse_fi

IMPLICIT NONE

TYPE( T_PAINTSTRUCT) MousePaint
type  (T_POINT) pts(2)

INTEGER iROP2,irv

MouseDC = BeginPaint(HPwnd, MousePaint) !Start Painting
CALL MouseMap(HPwnd,MouseDC) !Set Mapping
iROP2 = SetROP2(MouseDC,R2_NOT) !Set Draw mode to Screen Inverse
irv   = SelectObject(MouseDC,HpenNull) !Get the Null Pen
BoxS  = Box !Box to Draw (Device Units)
pts(1)%y = BoxS%bottom
pts(1)%x = BoxS%left
pts(2)%y = BoxS%top
pts(2)%x = BoxS%right
irv   = DPtoLP(MouseDC,pts(1),2) !Convert to Logical units
BoxS%bottom = pts(1)%y
BoxS%left   = pts(1)%x
BoxS%top    = pts(2)%y
BoxS%right  = pts(2)%x
CALL DrawRectangle(MouseDC,BoxS,SliceSet) !Draw the Box
irv   = SetROP2(MouseDC,iROP2) !Reset Draw mode
irv   = EndPaint(HPwnd, MousePaint) !Finish Painting

RETURN
END
!*******************************************************************************
!     DrawRectangle - Draw Rectangle using current pen
!*******************************************************************************
SUBROUTINE DrawRectangle(idc,Box,flag)

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) idc !Device Context
TYPE( T_RECT  )      Box !Rectangle to draw
INTEGER              flag

TYPE( T_POINT ) p(5)

INTEGER n,irv

IF( flag == IDB_SLICE )THEN
  n = 2
  p(1)%x = Box%left
  p(1)%y = Box%top
  p(2)%x = Box%right
  p(2)%y = Box%bottom
ELSE IF( flag == IDB_PICK )THEN
  n = 2
  p(1)%x = Box%left
  p(1)%y = Box%top
  p(2)%x = Box%right
  p(2)%y = Box%bottom
  irv = Polyline(idc,p(1),n)
  p(1)%x = Box%right
  p(1)%y = Box%top
  p(2)%x = Box%left
  p(2)%y = Box%bottom
  irv = Polyline(idc,p(1),n)
  n = 5
  p(1)%x = Box%left
  p(1)%y = Box%top
  p(2)%x = Box%left
  p(2)%y = Box%bottom
  p(3)%x = Box%right
  p(3)%y = Box%bottom
  p(4)%x = Box%right
  p(4)%y = Box%top
  p(5)%x = Box%left
  p(5)%y = Box%top
ELSE
  n = 5
  p(1)%x = Box%left
  p(1)%y = Box%top
  p(2)%x = Box%left
  p(2)%y = Box%bottom
  p(3)%x = Box%right
  p(3)%y = Box%bottom
  p(4)%x = Box%right
  p(4)%y = Box%top
  p(5)%x = Box%left
  p(5)%y = Box%top
END IF

irv = Polyline(idc,p(1),n)

RETURN
END
!*******************************************************************************
!     ZoomInit - Initializes Mouse paramters for Zoom Dialog Box
!*******************************************************************************
SUBROUTINE ZoomInit(idb,lflag)

USE mouse_fi
USE resource_fd

IMPLICIT NONE

INTEGER idb
INTEGER lflag

MouseDB   = idb
ZoomDraw  = .FALSE.
SliceSet  = lflag
IF( SliceSet /= IDB_SLICE )THEN
  CALL ZoomStart
END IF

RETURN

END
!*******************************************************************************
!     ZoomStart - Start Zooming
!*******************************************************************************

SUBROUTINE ZoomStart

USE resource_fd
USE mouse_fi

IMPLICIT NONE

INTEGER ix,iy

!---- Get Mouse Position

ix         = MousePnt%x
iy         = MousePnt%y

!---- Set ZOOMing flags

ZoomOK     = .FALSE.
LBDown     = .TRUE.

!---- Initialize ZOOM Box location

IF( SliceSet == IDB_PICK )THEN
  Box%left   = ix-8
  Box%top    = iy-8
  Box%right  = ix+8
  Box%bottom = iy+8
  BoxWidth   = 16
  BoxHeight  = 16
  BoxVisible = .FALSE.
ELSE
  Box%left   = ix
  Box%top    = iy
  Box%right  = ix
  Box%bottom = iy
  BoxWidth   = 0
  BoxHeight  = 0
  BoxVisible = .FALSE.
END IF

!---- Start the Timer

TimerID    = SetTimer(HPwnd,1,100,0)
TimerOn    = .TRUE.

!---- Reset ZOOM Dialog Box parameters

CALL ZoomReset

RETURN

END
!*******************************************************************************
!     ZoomOut - Restore Zoom Box to default size
!*******************************************************************************
SUBROUTINE ZoomOut

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi

IMPLICIT NONE

INTEGER id_level,iset
INTEGER(POINTER_LEN) jwnd

CALL FindHwndListId(IDB_ZOOM,jwnd,id_level)

IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_HINT )THEN
  iset = 11
ELSE
  iset = 5
END IF

dbreal(1,id_level) = axesdef(DEFAULT_LEVEL)%dbreal(iset  )
dbreal(2,id_level) = axesdef(DEFAULT_LEVEL)%dbreal(iset+1)
dbreal(3,id_level) = axesdef(DEFAULT_LEVEL)%dbreal(iset+3)
dbreal(4,id_level) = axesdef(DEFAULT_LEVEL)%dbreal(iset+4)

RETURN

END
!*******************************************************************************
!     ZoomDone - Zoom Dialog Box Draw Button routine
!*******************************************************************************
SUBROUTINE ZoomDone

USE resource_fd
USE mouse_fi

IMPLICIT NONE

INTEGER irv

!---- Send ZOOM Dialog Box a OK Message with ZoomDraw flag set to draw

CALL CallButton(MouseDB,ID_OK,irv)
ZoomDraw = .TRUE.

RETURN

END
!*******************************************************************************
!     ZoomIt - Draw Zoomed Area
!*******************************************************************************
SUBROUTINE ZoomIt

USE resource_fd
USE pcscipuf_fi
USE mouse_fi

IMPLICIT NONE

INTEGER irv

!---- Send PLOT Dialog a DRAW Button Message
IF( ZoomDraw )THEN
  CALL CallButton(hwnd_db,IDB_BUTTON10,irv)
END IF

RETURN

END
!*******************************************************************************
!     ZoomReset - Resets Zoom Dialog Box numbers
!*******************************************************************************
SUBROUTINE ZoomReset

USE resource_fd
USE pcscipuf_fi
USE mouse_fi

IMPLICIT NONE

INTEGER id_level,n,irv
REAL BoxR(4)
REAL x1,x2,y1,y2
INTEGER BI(4)
INTEGER(POINTER_LEN) jwnd

type  (T_POINT) pts(2)

!---- Find ZOOM Dialog Bix ID

IF( SliceSet == IDB_SLICE )THEN
  CALL FindHwndListId(IDB_SLICE,jwnd,id_level)
ELSE IF( SliceSet == IDB_PICK )THEN
  CALL FindHwndListId(IDB_PICK,jwnd,id_level)
ELSE
  CALL FindHwndListId(IDB_ZOOM,jwnd,id_level)
END IF

!---- Set NCAR parameters

MouseDC = GetDC(HPwnd)
CALL MouseMap(HPwnd,MouseDC) !Set Mapping

!---- Convert Device ->logical->real

BoxS = Box
pts(1)%y = BoxS%bottom
pts(1)%x = BoxS%left
pts(2)%y = BoxS%top
pts(2)%x = BoxS%right
irv = DPtoLP(MouseDC,pts(1),2)
BoxS%bottom = pts(1)%y
BoxS%left   = pts(1)%x
BoxS%top    = pts(2)%y
BoxS%right  = pts(2)%x
irv = ReleaseDC(HPwnd,MouseDC)
BI(1) = BoxS%left
BI(2) = BoxS%top
BI(3) = BoxS%right
BI(4) = BoxS%bottom
CALL LPtoReal(BI,BoxR)

!---- Save ZOOM coordinates

IF( SliceSet == IDB_SLICE )THEN
  x1 = BoxR(1)
  y1 = BoxR(2)
  x2 = BoxR(3)
  y2 = BoxR(4)
  n  = 4
ELSE IF( SliceSet == IDB_PICK )THEN
  x1 = 0.5*(BoxR(1) + BoxR(3))
  y1 = 0.5*(BoxR(2) + BoxR(4))
  x2 = NOT_SET_R
  CALL get_plot_point_data(x1,y1,x2)
  y2 = NOT_SET_R
  n  = 3
  CALL EnableControl(MouseDB,IDB_BUTTON2,TRUE)
ELSE
  x1 = MIN(BoxR(1),BoxR(3))
  y1 = MIN(BoxR(2),BoxR(4))
  x2 = MAX(BoxR(1),BoxR(3))
  y2 = MAX(BoxR(2),BoxR(4))
  n  = 4
END IF
dbreal(1,id_level)  = x1
dbreal(2,id_level)  = y1
dbreal(3,id_level)  = x2
dbreal(4,id_level)  = y2

!---- Post Coordinates to ZOOM Box

CALL SetEditRs(MouseDB,dbreal(1,id_level),1,n)

!---- This seems to fix th eneed to double click ZOOM and SLICE buttons
irv = SetFocus(GetDlgItem(MouseDB,IDB_BUTTON1))

RETURN
END
!*******************************************************************************
!     IsZooming - Zoom Query
!*******************************************************************************
LOGICAL FUNCTION IsZooming
USE mouse_fi

IMPLICIT NONE

IsZooming = Zooming

RETURN
END
