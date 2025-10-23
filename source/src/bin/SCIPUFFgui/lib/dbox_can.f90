!***********************************************************************
!                cancel_dialog
!***********************************************************************
SUBROUTINE cancel_dialog(iwnd_db,id)

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUItool_fi
USE create_fi
USE files_fi
USE pltchoice_fi

!
!     This routine initializes Dialog Boxes
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id !Dialog Box id

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level

!---- Initialize flags

ldestroy   = .FALSE. !Clear Mouse Flag OFF
ltool      = .FALSE. !TOOLBAR Flag OFF
lokbutton  = .FALSE. !RELDEF,MATDEF flag

SELECT CASE (id)
  CASE (IDB_PLOT)
    ltool    = .TRUE.
    CALL DeallocateContours(0,EDIT_LEVEL)
  CASE (IDB_EDTPRJ)
    ltool = .TRUE.
    DefinedOK = DefinedSaved
  CASE (IDB_RELDEF)
  CASE (IDB_SLICE)
    ldestroy = .TRUE.
  CASE (IDB_PICK)
    ldestroy = .TRUE.
  CASE (IDB_ZOOM)
    ldestroy = .TRUE.
  CASE (IDB_XYTABLE)
  CASE (IDB_MATNEW)
    CALL FindHwndListId(IDB_MATDEF,jwnd_db,jd_level) !  Parent = MATDEF
    CALL init_material_edit(jwnd_db,jd_level)
  CASE (IDB_LIQPARM)
  CASE (IDB_RNDPARM)
  CASE (IDB_UNCPARM)
  CASE (IDB_RELNEW)
    CALL FindHwndListId(IDB_RELDEF,jwnd_db,jd_level) !  Parent = RELDEF
    CALL init_release_edit(jwnd_db,jd_level)
  CASE (IDB_SCIPUF)
    ltool = .TRUE.
  CASE DEFAULT
END SELECT

RETURN
END
