!***********************************************************************
!                ProcessScroll
!***********************************************************************
RECURSIVE SUBROUTINE process_scroll(iwnd_db,MyCmd)
USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE animate_fi

!     This routine processes RADIOBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
TYPE( CMD )          MyCmd !Command Structure

INTEGER(POINTER_LEN)id_cntrl, iwnd
INTEGER id_dialog,id_level
INTEGER irv,ipos,inc,line,page,imin,imax
INTEGER iscroll !Command
INTEGER ivalue !Command value

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
ivalue    = MyCmd%button
iscroll   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

SELECT CASE (id_dialog)
  CASE (IDB_SLICE)
    line = 5
    page = 10
  CASE (IDB_PLAY)
    line = LINE_PAUSE
    page = PAGE_PAUSE
  CASE DEFAULT
    line = 1
    page = 5
END SELECT

iwnd = 0

CALL GetScrollControlPos(iwnd,id_cntrl,ipos)

CALL GetScrollControlRange(iwnd,id_cntrl,imin,imax,irv)

SELECT CASE  (iscroll)
  CASE (SB_TOP) !,SB_LEFT)
    inc = imin - ipos
  CASE (SB_BOTTOM) !,SB_RIGHT)
    inc = imax - ipos
  CASE (SB_LINEUP) !,SB_LINELEFT)
    inc = -line
  CASE (SB_LINEDOWN) !,SB_LINERIGHT)
    inc =  line
  CASE (SB_PAGEUP) !,SB_PAGELEFT)
    inc = -page
  CASE (SB_PAGEDOWN) !,SB_PAGERIGHT)
    inc =  page
  CASE (SB_THUMBPOSITION)
    inc = ivalue - ipos
  CASE DEFAULT
END SELECT

IF( ipos+inc >= imin .AND. ipos+inc <= imax )THEN
  ipos = ipos + inc
  CALL SetScrollControlPos(iwnd,id_cntrl,ipos,irv)
  SELECT CASE (id_dialog)
    CASE (IDB_SLICE)
      dbint(2,id_level) = ipos
      CALL SetEditIs(iwnd_db,dbint(2,id_level),2,1)
    CASE (IDB_PLAY)
      dbint(1,id_level) = ipos
    CASE DEFAULT
  END SELECT
END IF

RETURN

END

!***********************************************************************
!               Set Scroll Range
!***********************************************************************
SUBROUTINE SetScrollControlRange(iwnd,iscroll,imin,imax,irv)

USE SwinAPI
USE pcscipuf_fi, ONLY: NULL_POINTER

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog window handle
INTEGER(POINTER_LEN) iscroll !Scroll ID
INTEGER              imin !Minimum value
INTEGER              imax !Maximum value
INTEGER              irv !return value

INTEGER              jscroll
INTEGER(POINTER_LEN) ictrl

TYPE (T_SCROLLINFO) info

IF( iwnd == NULL_POINTER )THEN
  ictrl = iscroll
ELSE
  jscroll = iscroll
  ictrl = GetDlgItem(iwnd,jscroll)
END IF

info%Size    = SIZEOF(info)
info%Mask     = SIF_RANGE
info%Min      = imin
info%Max      = imax
info%Page     = 0
info%Pos      = 0
info%TrackPos = 0
irv = SetScrollInfo(ictrl,SB_CTL,info,FALSE)


RETURN
END

!***********************************************************************
!               Get Scroll Range
!***********************************************************************
SUBROUTINE GetScrollControlRange(iwnd,iscroll,imin,imax,irv)

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog window handle
INTEGER(POINTER_LEN) iscroll !Scroll ID
INTEGER              imin !Minimum value
INTEGER              imax !Maximum value
INTEGER              irv !return value

INTEGER              jscroll
INTEGER(POINTER_LEN) ictrl

TYPE (T_SCROLLINFO) info

IF( iwnd == 0 )THEN
  ictrl = iscroll
ELSE
  jscroll = iscroll
  ictrl = GetDlgItem(iwnd,jscroll)
END IF

info%Size    = SIZEOF(info)
info%Mask     = SIF_RANGE
info%Min      = 0
info%Max      = 0
info%Page     = 0
info%Pos      = 0
info%TrackPos = 0

irv = GetScrollInfo(ictrl,SB_CTL,info)
imin = info%Min
imax = info%Max

RETURN
END

!***********************************************************************
!               Set Scroll Position
!***********************************************************************
SUBROUTINE SetScrollControlPos(iwnd,iscroll,ipos,irv)

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog window handle
INTEGER(POINTER_LEN) iscroll !Scroll ID
INTEGER              ipos !Current value
INTEGER              irv !Old value

INTEGER              jscroll
INTEGER(POINTER_LEN) ictrl

TYPE (T_SCROLLINFO) info

IF( iwnd == 0 )THEN
  ictrl = iscroll
ELSE
  jscroll = iscroll
  ictrl = GetDlgItem(iwnd,jscroll)
END IF

info%Size    = SIZEOF(info)
info%Mask     = SIF_POS
info%Min      = 0
info%Max      = 0
info%Page     = 0
info%Pos      = iPos
info%TrackPos = 0

irv = SetScrollInfo(ictrl,SB_CTL,info,TRUE)

RETURN
END

!***********************************************************************
!               Get Scroll Position
!***********************************************************************
SUBROUTINE GetScrollControlPos(iwnd,iscroll,ipos)

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog window handle
INTEGER(POINTER_LEN) iscroll !Scroll ID
INTEGER              ipos !Current value

INTEGER              jscroll,irv
INTEGER(POINTER_LEN) ictrl

TYPE (T_SCROLLINFO) info


IF( iwnd == 0 )THEN
  ictrl = iscroll
ELSE
  jscroll = iscroll
  ictrl = GetDlgItem(iwnd,jscroll)
END IF

info%Size    = SIZEOF(info)
info%Mask     = SIF_POS +SIF_TRACKPOS
info%Min      = 0
info%Max      = 0
info%Page     = 0
info%Pos      = 0
info%TrackPos = 0

irv = GetScrollInfo(ictrl,SB_CTL,info)
ipos = info%Pos

RETURN
END

