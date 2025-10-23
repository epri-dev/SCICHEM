!===============================================================================

SUBROUTINE SetNCARWindow( iwind )

USE ncarlib_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwind

ihwndnc = iwind

RETURN
END

!===============================================================================

SUBROUTINE SetNCARDC( ihdc,jhdc )

USE ncarlib_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: ihdc,JHDC

ihdcnc  = ihdc
ihdcget = jhdc

RETURN
END

!===============================================================================

SUBROUTINE SetNCARPrint( lprint )

USE ncarlib_fi

IMPLICIT NONE

logical, INTENT( IN ) :: lprint

lprt = lprint

RETURN
END
