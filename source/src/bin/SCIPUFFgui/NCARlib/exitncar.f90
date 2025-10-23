SUBROUTINE ExitNCARGraphics()

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER irv

irv = DeleteObject( ihpnc )
irv = DeleteObject( ihbnc )
irv = DeleteObject( ihpalnc )
irv = DeleteObject( ihbitnc )

RETURN
END
