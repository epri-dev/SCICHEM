!***********************************************************************
!                SetHwndList
!***********************************************************************
SUBROUTINE SetHwndList(hwnd,id_dialog,id_level)

USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Dialog Window Handle
INTEGER              id_dialog !Dialog ID
INTEGER              id_level !Dialog Data level

!---- Look for an unused level

id_level = 1
DO WHILE( hwnd_list(1,id_level) > 0)
  id_level = id_level + 1
  IF( id_level > MAX_DLEV )GOTO 9999
END DO

!---- Set List data

hwnd_list(1,id_level) = hwnd
hwnd_list(2,id_level) = id_dialog

RETURN

!---- Error Section

9999  id_level = 0
RETURN
END
!***********************************************************************
!                DeleteHwndList
!***********************************************************************
SUBROUTINE DeleteHwndList(hwnd)

USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Dialog handle

INTEGER id_level

!---- Search list for specified handle

id_level = 1
DO WHILE( hwnd_list(1,id_level) /= hwnd)
  id_level = id_level + 1
  IF( id_level > MAX_DLEV )GOTO 9999
END DO

!---- Clear list data

hwnd_list(1,id_level) = 0
hwnd_list(2,id_level) = 0

9999  RETURN

END
!***********************************************************************
!                FindHwndList
!***********************************************************************
SUBROUTINE FindHwndList(hwnd,id_dialog,id_level)

USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Dialog Window Handle
INTEGER              id_dialog !Dialog ID
INTEGER              id_level !Dialog Data level

!---- Search List for specified handle

id_level = 1
DO WHILE( hwnd_list(1,id_level) /= hwnd)
  id_level = id_level + 1
  IF( id_level > MAX_DLEV )GOTO 9999
END DO

!---- Extract ID from list

id_dialog = hwnd_list(2,id_level)

RETURN

!---- Error Section

9999  id_dialog = 0
id_level = 0
RETURN

END
!***********************************************************************
!                FindHwndListId
!***********************************************************************
SUBROUTINE FindHwndListId(id_dialog,hwnd,id_level)

USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Dialog Window Handle
INTEGER              id_dialog !Dialog ID
INTEGER              id_level !Dialog Data level

!---- Search List for specified ID

id_level = 1
DO WHILE( hwnd_list(2,id_level) /= id_dialog)
  id_level = id_level + 1
  IF( id_level > MAX_DLEV )GOTO 9999
END DO

!---- Extract handle from list

hwnd = hwnd_list(1,id_level)

RETURN

!---- Error Section

9999  hwnd = 0
id_level = 0
RETURN

END
!***********************************************************************
!                FindHwndListTop
!***********************************************************************
SUBROUTINE FindHwndListTop(hwnd,id_dialog,id_level)

USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Dialog Window Handle
INTEGER              id_dialog !Dialog ID
INTEGER              id_level !Dialog Data level

!---- Search List for first unused position

id_level = 1
DO WHILE( hwnd_list(1,id_level) /= 0)
  id_level = id_level + 1
  IF( id_level > MAX_DLEV )GOTO 9999
END DO

id_level = id_level - 1

!---- Extract handle from list

IF( id_level > 0 )THEN
  hwnd      = hwnd_list(1,id_level)
  id_dialog = hwnd_list(2,id_level)
ELSE
  hwnd      = 0
  id_dialog = 0
END IF

RETURN

!---- Error Section

9999  hwnd = 0
id_level = 0
id_dialog = 0
RETURN

END
