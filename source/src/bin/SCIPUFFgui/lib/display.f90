!***********************************************************************
!                DisplayIt
!***********************************************************************
SUBROUTINE DisplayIt( ihwnd )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE animate_fi
USE pltchoice_fi
USE SwinAPI
USE testprt

IMPLICIT NONE

INTEGER(POINTER_LEN) ihwnd !PLOT Window handle

TYPE( T_PAINTSTRUCT)     PS

LOGICAL lprt,lavs,lcts,lovl,lmap,startZoom

INTEGER irv
INTEGER(POINTER_LEN)ifocus,idc,iirv

LOGICAL, EXTERNAL :: hasError

!---- Check Plot flag

IF( .NOT.lplotOK )RETURN
IF( hasError() )RETURN

!---- Get the Device Context

idc = BeginPaint( ihwnd,PS )

!---- Set the Mouse Mapping and NCAR parameters

CALL MouseMap( ihwnd,idc )

!---- Stop Zooming

CALL MouseStop()

!---- Draw Plot (Printing=false)

nxpage = 1
nypage = 1
ixpage = 1
iypage = 1
lprt = .FALSE.
lavs = .FALSE.
lcts = .FALSE.
lovl = .FALSE.
lmap = .FALSE.
string1 = ' '
ifocus = GetFocus()
irv    = SetFocus( hwnd_pw )
map_scale = DEF_VAL_R
CALL plottri( lprt,lavs,lcts,lovl,lmap,string1 )
IF( hasError() )CALL ShowErrorMessage( hwnd_mw )

iirv = SetFocus( ifocus )

!---- Start Zooming

startZoom = PlotDef(BASE_LEVEL)%Field%Category /= HP_TABLE
startZoom = startZoom .AND. .NOT.lanimate .AND. .NOT.SyncMode

IF( startZoom )CALL MouseStart( idc,ihwnd,hwnd_db )

!---- Release Device Context

9999 CONTINUE

irv = EndPaint( ihwnd,PS )

!---- Copy PLOT Window to Bitmap

IF( .NOT.lanimate )CALL copy_bmp( ihwnd,hbitmap,0,0,1 )

IF( hasError() )CALL ShowErrorMessage( hwnd_mw )

!---- Enable Printing

lprintOK = .NOT.hasError()

RETURN
END
!***********************************************************************
!                DisplayStat
!***********************************************************************
SUBROUTINE DisplayStat( ihwnd )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) ihwnd !STATS Window handle

TYPE( T_PAINTSTRUCT) PS

LOGICAL lprt

INTEGER irv
INTEGER(POINTER_LEN)idc
REAL    yoff

LOGICAL, EXTERNAL :: hasError

!---- Get Device Context

idc = BeginPaint( ihwnd,PS )

!---- Set Mouse Mapping and NCAR Parameters

CALL MouseMap( ihwnd,idc )

!---- Write STATS at top of window (yoff=0)

yoff = 0.0
lprt = .FALSE.
CALL plotstat( yoff,lprt )
IF( hasError() )GOTO 9999

!---- Release Device Context

9999 CONTINUE

irv = EndPaint( ihwnd,PS )

IF( hasError() )CALL ShowErrorMessage( hwnd_mw )

RETURN
END
