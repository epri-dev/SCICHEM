INTEGER FUNCTION SWIMcurrentTopog( X,Y,H,Hx,Hy,inField )

USE SWIMparam_fd

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMcurrentTopog

IMPLICIT NONE

REAL,    INTENT( IN  ) :: X, Y
REAL,    INTENT( OUT ) :: H, Hx, Hy
INTEGER, INTENT( IN  ) :: inField

INTEGER irv

INTERFACE
  SUBROUTINE SWIMpuffTopog( X,Y,H,Hx,Hy,inField,Shh,outField )
    REAL,              INTENT( IN  ) :: X, Y
    REAL,              INTENT( OUT ) :: H, Hx, Hy
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField
  END SUBROUTINE SWIMpuffTopog
END INTERFACE

SWIMcurrentTopog = SWIMfailure

IF( inField > 0 )THEN
  CALL SWIMpuffTopog( x,y,h,hx,hy,inField=inField,outField=irv )
ELSE
  CALL SWIMpuffTopog( x,y,h,hx,hy,outField=irv )
END IF

IF( irv > 0 )SWIMcurrentTopog = SWIMsuccess

RETURN
END

!==============================================================================

SUBROUTINE SWIMpuffTopog( X,Y,H,Hx,Hy,inField,Shh,outField )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMpuffTopog

USE SWIM_fi
USE SWIMgetTopog_fi
USE SWIMpuff_fd
USE SWIMcurrentMet_fi, ONLY: lSaveCoord, thisThread

IMPLICIT NONE

REAL,              INTENT( IN  ) :: X, Y
REAL,              INTENT( OUT ) :: H, Hx, Hy
INTEGER, OPTIONAL, INTENT( IN  ) :: inField
REAL,    OPTIONAL, INTENT( IN  ) :: Shh
INTEGER, OPTIONAL, INTENT( OUT ) :: outField

TYPE( PuffMetRequest ) :: Request
TYPE( Topog          ) :: TerStr

INTEGER irv, i
REAL    xp, yp

INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: SWIMlimit, SelectMetField

i = -1

IF( PRESENT(inField) )THEN
  IF( inField > 0 .AND. inField <= numField )i = inField
END IF

IF( i == -1 )THEN

  Request%type   = 0
  Request%iField = -1
  Request%X      = X
  Request%Y      = Y

  IF( PRESENT(Shh) )THEN
    Request%Shh = Shh
  ELSE
    Request%Shh = 0.
  END IF

  Request%type = IBSET(Request%type,RTB_AGL)
  Request%z    = 0.
  Request%SigZ = 0.

  lSaveCoord = .FALSE.

  i = SelectMetField( Request )
  i = SWIMlimit( i,1,numField )

END IF

!------ Set met field index if requested

IF( PRESENT(outField) )outField = i

IF( i > 0 )THEN

  IF( PrjCoord%type /= field(i)%grid%coord%type )THEN
    irv = SWIMcnvCoord( X,Y,PrjCoord,xp,yp,field(i)%grid%coord )
    IF( irv /= SWIMsuccess )THEN
      xp = field(i)%grid%Xmin
      yp = field(i)%grid%Ymin
    END IF
  ELSE
    xp = X; yp = Y
  END IF

  TerStr = SWIMGetTopog( field(i)%grid,xp,yp )

  H  = TerStr%H + field(i)%grid%Hmin - Prj%Hmin
  Hx = TerStr%Hx
  Hy = TerStr%Hy

ELSE

  H  = 0.0
  Hx = 0.0
  Hy = 0.0

END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMpuffCanopy( X,Y,Hcnp,inField,Shh,outField )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMpuffCanopy

USE SWIM_fi
USE SWIMgetTopog_fi
USE SWIMpuff_fd
USE SWIMcurrentMet_fi, ONLY: lSaveCoord, thisThread

IMPLICIT NONE

REAL,              INTENT( IN  ) :: X, Y
REAL,              INTENT( OUT ) :: Hcnp
INTEGER, OPTIONAL, INTENT( IN  ) :: inField
REAL,    OPTIONAL, INTENT( IN  ) :: Shh
INTEGER, OPTIONAL, INTENT( OUT ) :: outField

TYPE( met1dh )         :: mx, my
TYPE( meth   )         :: mxy
TYPE( PuffMetRequest ) :: Request

TYPE( MetGrid ), POINTER  :: grid

REAL    xfac, yfac
INTEGER i, nxb, nyb, nxyb

INTEGER, EXTERNAL :: SWIMlimit, SelectMetField

i = -1

IF( PRESENT(inField) )THEN
  IF( inField > 0 .AND. inField <= numField )i = inField
END IF

IF( i == -1 )THEN

  Request%type   = 0
  Request%iField = -1
  Request%X      = X
  Request%Y      = Y

  IF( PRESENT(Shh) )THEN
    Request%Shh = Shh
  ELSE
    Request%Shh = 0.
  END IF

  Request%type = IBSET(Request%type,RTB_AGL)
  Request%z    = 0.
  Request%SigZ = 0.

  lSaveCoord = .FALSE.

  i = SelectMetField( Request )
  i = SWIMlimit( i,1,numField )

END IF

!------ Set met field index if requested

IF( PRESENT(outField) )outField = i

IF( i > 0 )THEN

  grid => field(i)%grid

  IF( BTEST(grid%type,GTB_HCNP) )THEN

!------ Define locals

    nxb  = grid%nX
    nyb  = grid%nY
    nxyb = grid%nXY

    xfac = 1.; yfac = 1. !Not used

    CALL get_1dfac( x,grid%Xmin,grid%dX,nxb,xfac,mx )
    CALL get_1dfac( y,grid%Ymin,grid%dY,nyb,yfac,my )

    CALL SetMXY( mx,my,mxy,nxb,nxyb,.TRUE. )
    CALL IntXY( mxy,grid%landcover%canopyHt,Hcnp)

  ELSE

    Hcnp = Prj%BL%hc

  END IF

ELSE

  Hcnp = 0.0

END IF

RETURN
END
