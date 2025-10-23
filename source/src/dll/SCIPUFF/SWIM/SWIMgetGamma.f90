RECURSIVE REAL FUNCTION SWIMgetGamma( ifld,x,y,zi ) RESULT( tz )

!------ Get temperature gradient above inversion height zi

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetGamma

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMinterpPointer
USE SWIMcurrentMet_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifld
REAL,    INTENT( IN ) :: x, y, zi  !Zi referenced to Hmin

TYPE( PuffMetRequest ) :: Request

INTEGER km, kp
INTEGER alloc_stat, i, k, irv
REAL    dum1, dum2, tdum, ziMSL

TYPE( MetGrid ), POINTER :: grid

REAL, DIMENSION(:), POINTER :: Z
REAL, DIMENSION(:), POINTER :: zb

INTERFACE

  SUBROUTINE SetTopog( Request,grid,Met )
    USE SWIMmetField_fd
    USE SWIMpuff_fd
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ),    INTENT( INOUT ) :: Request
    TYPE( MetGrid        ),    INTENT( IN    ) :: grid
    TYPE( PuffMet ), OPTIONAL, INTENT( INOUT ) :: Met
  END SUBROUTINE SetTopog

END INTERFACE

INTEGER, EXTERNAL :: SWIMcnvCoord

grid => field(ifld)%grid

IF( BTEST(field(ifld)%type,FTB_T) )THEN

  Request%Z      = zi
  Request%Zcap   = 0.
  Request%SigZ   = 0.
  Request%Shh    = 0.
  Request%type   = 0
  Request%iField = 0

  IF( PrjCoord%type /= grid%coord%type )THEN
    irv = SWIMcnvCoord( x,y,PrjCoord,Request%X,Request%Y,grid%coord )
    IF( irv /= SWIMsuccess )GOTO 9999
  ELSE
    Request%X = x; Request%Y = y
  END IF

  CALL SetTopog( Request,field(ifld)%grid )

  IF( BTEST(grid%type,GTB_Z3D) )THEN

    CALL SetXYfac( Request,grid,mx,my,mxu,myv )
    CALL SetMXY( mx,my,mxy,grid%nX,grid%nXY,.FALSE. )

    ALLOCATE( zb(grid%nZ),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMgetGamma'
      error%Message = 'Error allocating height arrays for sigma coordinates'
      tz = 0. !NOT_SET_R
      GOTO 9999
    END IF

    DO k = 1,grid%nZ
      i = (k-1)*grid%nXY + 1
      Z  => grid%sigma%Z(i:)
      CALL IntXY( mxy,Z,zb(k) )
    END DO
    Z => zb

  ELSE

    Z => grid%Z

  END IF

  CALL SetZfac( zm,Z,grid%nZ,mz,0.,0. )

!------ Reset index to give gradient above zi

  km = ABS(mz%km) + 1
  kp = km + 1

!------ Get interpolation factors (only if height is below zb(nzb))

  IF( km < grid%nZ )THEN

    zm = 0.5*(Z(km)+Z(kp))
    CALL SetZfac( zm,Z,grid%nZ,mz,0.,0. )

    IF( .NOT.BTEST(grid%type,GTB_Z3D) )THEN
      CALL SetXYfac( Request,grid,mx,my,mxu,myv )
      CALL SetMXY( mx,my,mxy,grid%nX,grid%nXY,.FALSE. )
    END IF

    CALL IntXYZ( mxy,mz,field(ifld)%Field%Tpot, &
               tdum,dum1,dum2,tz, &
               dp,gxp,gyp,0,BTEST(grid%type,GTB_STAGGER) )

  ELSE

    ziMSL = zi + Prj%Hmin
    CALL stnd_atmos( ziMSL,dum1,dum2,tz,SA_TA )  !N.B. tz is potential temp grad

  END IF

  IF( BTEST(grid%type,GTB_Z3D) )DEALLOCATE( zb,STAT=alloc_stat )

ELSE !------ Else use standard atmosphere

  ziMSL = zi + grid%Hmin
  CALL stnd_atmos( ziMSL,dum1,dum2,tz,SA_TA )

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMgetZi( ifld,x,y,zi,wts )

!------ Get inversion height zi

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetZi

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMinterpPointer
USE SWIMcurrentMet_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ifld
REAL,    INTENT( IN  ) :: x, y
REAL,    INTENT( OUT ) :: zi, wts

INTEGER irv
REAL    h

TYPE( PuffMetRequest ) :: Request

INTEGER, EXTERNAL :: SWIMcnvCoord

IF( field(ifld)%BLType /= BLP_NONE )THEN

  IF( PrjCoord%type /= field(ifld)%grid%coord%type )THEN
    irv = SWIMcnvCoord( x,y,PrjCoord,Request%X,Request%Y,field(ifld)%grid%coord )
    IF( irv /= SWIMsuccess )THEN
      zi = 0.; wts = 0.
      GOTO 9999
    END IF
  ELSE
    Request%X = x; Request%Y = y
  END IF
  Request%type = 0

  CALL SetXYfac( Request,field(ifld)%grid,mx,my,mxu,myv )
  CALL SetMXY( mx,my,mxy,field(ifld)%grid%nX,field(ifld)%grid%nXY,.FALSE. )

  CALL IntXY( mxy,field(ifld)%BL%zi,zi )
  CALL IntXY( mxy,field(ifld)%BL%HeatFlux,wts )
  IF( BTEST(field(ifld)%grid%type,GTB_TERRAIN) )THEN
    CALL IntXY( mxy,field(ifld)%grid%terrain%H,h )
    h = h + field(ifld)%grid%Hmin - Prj%Hmin
  ELSE
    h = 0.
  END IF
  zi = zi + h

ELSE

  zi  = 0.
  wts = 0.

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE LOGICAL FUNCTION SWIMcappedBL( ifld,x,y )

!------ Determine if boundary layer is capped (convective & dtdz > 0.)

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMcappedBL

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMinterpPointer
USE SWIMcurrentMet_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifld
REAL,    INTENT( IN ) :: x, y

INTEGER irv
REAL    h, zi, hflux

TYPE( PuffMetRequest ) :: Request

REAL,    EXTERNAL :: SWIMgetGamma
INTEGER, EXTERNAL :: SWIMcnvCoord

IF( field(ifld)%BLType /= BLP_NONE )THEN

  IF( PrjCoord%type /= field(ifld)%grid%coord%type )THEN
    irv = SWIMcnvCoord( x,y,PrjCoord,Request%X,Request%Y,field(ifld)%grid%coord )
    IF( irv /= SWIMsuccess )THEN
      SWIMcappedBL = .FALSE.
      GOTO 9999
    END IF
  ELSE
    Request%X = x; Request%Y = y
  END IF
  Request%type = 0

  CALL SetXYfac( Request,field(ifld)%grid,mx,my,mxu,myv )
  CALL SetMXY( mx,my,mxy,field(ifld)%grid%nX,field(ifld)%grid%nXY,.FALSE. )

  CALL IntXY( mxy,field(ifld)%BL%HeatFlux,hflux )

  IF( hflux >= 0. )THEN
    CALL IntXY( mxy,field(ifld)%BL%zi,zi )
    CALL IntXY( mxy,field(ifld)%grid%terrain%H,h )
    h = h + field(ifld)%grid%Hmin - Prj%Hmin
    SWIMcappedBL = SWIMgetGamma( ifld,x,y,zi+h ) > 0.
  ELSE
    SWIMcappedBL = .FALSE.
  END IF

ELSE

  SWIMcappedBL = .FALSE.

END IF

9999 CONTINUE

RETURN
END

