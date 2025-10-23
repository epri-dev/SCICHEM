!***********************************************************************
!                owner_draw
!***********************************************************************
SUBROUTINE owner_draw(iwnd,draw)

USE resource_fd
USE reldef_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd

TYPE( T_DRAWITEMSTRUCT) draw

INTEGER id_dialog,id_level

!==== Find Dialog ID

CALL FindHwndList(iwnd,id_dialog,id_level)

!==== Call Dialog dependent routines

SELECT CASE (id_dialog)
  CASE (IDB_SETUP,IDB_PRJDEF)
    CALL owner_draw_setup(draw,id_level)
  CASE DEFAULT
END SELECT

RETURN
END

!***********************************************************************
!                owner_draw_setup
!***********************************************************************
SUBROUTINE owner_draw_setup(draw,id_level)

USE resource_fd
USE reldef_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER id_level

TYPE( T_DRAWITEMSTRUCT) draw

INTEGER irv,ictl
INTEGER(POINTER_LEN)ipn

INTEGER focus_color,focus_style,focus_width
INTEGER select_color,select_style,select_width

!==== Initialize

focus_style  = 2
focus_color  = dlgBkColor !RGB(64,64,64)
focus_width  = 0
select_style = 0
select_color = RGB(0,0,0)
select_width = 2

!==== Select based on Action

ictl = draw%CtlID - 430

SELECT CASE (draw%itemAction)
  CASE (ODA_DRAWENTIRE)
    SELECT CASE (draw%CtlID)
      CASE (IDB_RADIO31,IDB_RADIO32,IDB_RADIO33)
        IF( ictl == ichoice(4,id_level) )THEN
          ipn = CreatePen(select_style,select_width,select_color)
        ELSE
          ipn = CreatePen(0,select_width,dlgBkColor)
        END IF
        CALL draw_button_rect(draw,ipn,4)
        irv = DeleteObject(ipn)
        IF( IAND(draw%itemState,ODS_FOCUS) == ODS_FOCUS .AND. &
          ictl == ichoice(4,id_level) )THEN
          ipn = CreatePen(focus_style,focus_width,focus_color)
        ELSE
          ipn = CreatePen(0,focus_width,dlgBkColor)
        END IF
        CALL draw_button_rect(draw,ipn,0)
        irv = DeleteObject(ipn)
      CASE DEFAULT
    END SELECT
  CASE (ODA_SELECT)
    SELECT CASE (draw%CtlID)
      CASE (IDB_RADIO31,IDB_RADIO32,IDB_RADIO33)
        IF( IAND(draw%itemState,ODS_FOCUS) == ODS_FOCUS .AND. &
     IAND(draw%itemState,ODS_SELECTED) == ODS_SELECTED )THEN
          ipn = CreatePen(focus_style,focus_width,focus_color)
        ELSE
          ipn = CreatePen(0,focus_width,dlgBkColor)
        END IF
        CALL draw_button_rect(draw,ipn,0)
        irv = DeleteObject(ipn)
      CASE DEFAULT
    END SELECT
  CASE (ODA_FOCUS)
    SELECT CASE (draw%CtlID)
      CASE (IDB_RADIO31,IDB_RADIO32,IDB_RADIO33)
        IF( IAND(draw%itemState,ODS_FOCUS) == ODS_FOCUS .AND. &
          ictl == ichoice(4,id_level) )THEN
          ipn = CreatePen(focus_style,focus_width,focus_color)
        ELSE
          ipn = CreatePen(0,focus_width,dlgBkColor)
        END IF
        CALL draw_button_rect(draw,ipn,0)
        irv = DeleteObject(ipn)
      CASE DEFAULT
    END SELECT
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!                draw_button_rect
!***********************************************************************
SUBROUTINE draw_button_rect(draw,ipn,ioff)

USE resource_fd
USE reldef_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) ipn
INTEGER ioff

TYPE( T_DRAWITEMSTRUCT) draw

TYPE( T_POINT)     pts(5)

INTEGER irv,joff
INTEGER(POINTER_LEN)jpn

joff = ioff + 2

jpn = SelectObject(draw%hDC,ipn)

pts(1)%x = draw%rcItem%left   + ioff
pts(1)%y = draw%rcItem%bottom - joff
pts(2)%x = draw%rcItem%right  - joff
pts(2)%y = draw%rcItem%bottom - joff
pts(3)%x = draw%rcItem%right  - joff
pts(3)%y = draw%rcItem%top    + ioff
pts(4)%x = draw%rcItem%left   + ioff
pts(4)%y = draw%rcItem%top    + ioff
pts(5)%x = draw%rcItem%left   + ioff
pts(5)%y = draw%rcItem%bottom - joff

irv = Polyline(draw%hDC,pts(1),5)

jpn = SelectObject(draw%hDC,jpn)

RETURN
END
!***********************************************************************
!                owner_redraw
!***********************************************************************
SUBROUTINE owner_redraw(iwnd,id_dialog,igroup,id_level)

USE resource_fd
USE reldef_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              id_dialog
INTEGER              igroup
INTEGER              id_level

TYPE( T_RECT) , POINTER :: pRect

INTEGER ibase,irv,i
INTEGER(POINTER_LEN)ictrl

NULLIFY (pRect)

!==== Perform Dialog dependent actions

SELECT CASE (id_dialog)
  CASE (IDB_SETUP,IDB_PRJDEF)
    ibase = RADIO_BASE+RADIO_GROUP*(igroup)
    DO i = 1,nradio(igroup+1,id_level)
      ictrl = GetDlgItem(iwnd,ibase+i)
      irv = InvalidateRect(ictrl,pRect,FALSE)
      irv = UpdateWindow(ictrl)
    END DO
  CASE DEFAULT
END SELECT

RETURN
END
