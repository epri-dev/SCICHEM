SUBROUTINE labmod( cfx,cfy )

USE ncarlib_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: cfx,cfy

cfmtx = cfx
cfmty = cfy

nchfx = MIN(LEN_TRIM(cfx),LEN(cfmtx))
nchfy = MIN(LEN_TRIM(cfy),LEN(cfmty))

RETURN
END
