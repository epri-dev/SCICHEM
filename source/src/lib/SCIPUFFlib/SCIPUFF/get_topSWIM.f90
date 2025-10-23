SUBROUTINE get_topog( x,y,h,hx,hy )

!------ find topography height, slope and stretch factor at (x,y)

IMPLICIT NONE

REAL, INTENT( IN )  :: x, y
REAL, INTENT( OUT ) :: h, hx, hy

INTERFACE
  SUBROUTINE SWIMpuffTopog( X,Y,H,Hx,Hy,inField,Shh,outField )
    REAL,              INTENT( IN  ) :: X, Y
    REAL,              INTENT( OUT ) :: H, Hx, Hy
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField
  END SUBROUTINE SWIMpuffTopog
END INTERFACE

CALL SWIMpuffTopog( x,y,h,hx,hy )

RETURN
END

!==============================================================================

SUBROUTINE get_canopyIn( x,y,hc,ifld )

!------ find canopy height at (x,y)

IMPLICIT NONE

REAL,    INTENT( IN )  :: x, y
REAL,    INTENT( OUT ) :: hc
INTEGER, INTENT( IN )  :: ifld

INTERFACE
  SUBROUTINE SWIMpuffCanopy( X,Y,Hcnp,inField,Shh,outField )
    REAL,              INTENT( IN  ) :: X, Y
    REAL,              INTENT( OUT ) :: Hcnp
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField
  END SUBROUTINE SWIMpuffCanopy
END INTERFACE

CALL SWIMpuffCanopy( x,y,hc,inField=ifld )

RETURN
END

!==============================================================================

SUBROUTINE get_topogInSWIM( x,y,h,hx,hy,inField )

!------ find topography height, slope and stretch factor at (x,y)

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x, y
REAL,    INTENT( OUT ) :: h, hx, hy
INTEGER, INTENT( IN  ) :: inField

INTERFACE
  SUBROUTINE SWIMpuffTopog( X,Y,H,Hx,Hy,inField,Shh,outField )
    REAL,              INTENT( IN  ) :: X, Y
    REAL,              INTENT( OUT ) :: H, Hx, Hy
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField
  END SUBROUTINE SWIMpuffTopog
END INTERFACE

CALL SWIMpuffTopog( x,y,h,hx,hy,inField=inField )

RETURN
END

!==============================================================================

SUBROUTINE get_topogOut( x,y,h,hx,hy,Shh,outField )

!------ find topography height, slope and stretch factor at (x,y)

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x, y
REAL,    INTENT( OUT ) :: h, hx, hy
REAL,    INTENT( IN  ) :: Shh
INTEGER, INTENT( OUT ) :: outField

INTERFACE
  SUBROUTINE SWIMpuffTopog( X,Y,H,Hx,Hy,inField,Shh,outField )
    REAL,              INTENT( IN  ) :: X, Y
    REAL,              INTENT( OUT ) :: H, Hx, Hy
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField
  END SUBROUTINE SWIMpuffTopog
END INTERFACE

CALL SWIMpuffTopog( x,y,h,hx,hy,outField=outField,Shh=Shh )

RETURN
END

!================================================================================

SUBROUTINE get_metFieldID( x,y,z,shh,ifld )

USE met_fi
USE SCIPresults_fd

REAL,    INTENT( IN  ) :: x, y    !Horizontal location (project coord)
REAL,    INTENT( IN  ) :: z       !Height AGL
REAL,    INTENT( IN  ) :: shh     !Horizontal puff size
INTEGER, INTENT( OUT ) :: ifld

LOGICAL lAddMet
INTEGER irv, jfld

INTEGER, EXTERNAL :: SWIMgetMetFieldID, SetMetGrid

ifld = -1

jfld = SWIMgetMetFieldID( x,y,z,shh,lAddMet )

IF( lAddMet )THEN
!------ Implicitly assuming this routine cannot be called in parallel
!       Otherwise SetMetGrid can cause conflicts
!$OMP CRITICAL (UpdateMetGrid)
  irv = SetMetGrid()
!$OMP END CRITICAL (UpdateMetGrid)
  IF( irv /= SCIPsuccess )GOTO 9999
END IF

ifld = jfld

9999 CONTINUE

RETURN
END

