RECURSIVE REAL FUNCTION SWIMgetSunFac( ifld,x,y ) RESULT( fac )

!------ Get solar factor for activity decay

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetSunFac

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMinterpPointer
USE SWIMcurrentMet_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: ifld
REAL,    INTENT( IN    ) :: x, y

TYPE( PuffMetRequest )   :: Request
TYPE( MetGrid ), POINTER :: grid

INTEGER, EXTERNAL :: SetMetField

!----- Default return

fac = 0.0

IF( .NOT.Prj%decay )GOTO 9999

!------ Build request

Request%X = x; Request%Y = y; Request%Z = 0.

Request%SigZ = 0.; Request%Zcap = 0.; Request%Shh = 0.

Request%iField = ifld

Request%type = IBSET(0,RTB_AGL)

lSaveCoord = .TRUE.

ifld = SetMetField( Request )
IF( ifld < 1 )GOTO 9999

grid => field(ifld)%grid

Request%X = xFld(ifld); Request%Y = yFld(ifld)

CALL SetXYfac( Request,field(ifld)%grid,mx,my,mxu,myv )
CALL SetMXY( mx,my,mxy,grid%nX,grid%nXY,.FALSE. )

CALL IntXY( mxy,grid%sunfac,fac )

9999 CONTINUE

lSaveCoord = .FALSE.

RETURN
END

