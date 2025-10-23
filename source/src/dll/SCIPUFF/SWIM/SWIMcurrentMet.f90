!==============================================================================
!  SWIM met interpolation given a puff request
!==============================================================================

RECURSIVE INTEGER FUNCTION SWIMcurrentMet( Request,Met )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMcurrentMet

USE SWIMpuff_fd
USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( INOUT ) :: Request
TYPE( PuffMet        ), INTENT( OUT   ) :: Met

INTEGER i

TYPE( CurrentMetZ ) :: zzz

INTERFACE
  RECURSIVE SUBROUTINE InitCurrentMet( Request,fld,Met,zzz )
    USE SWIMpuff_fd
    USE SWIM_fi
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ), INTENT( INOUT ) :: Request
    TYPE( MetField       ), INTENT( INOUT ) :: fld
    TYPE( PuffMet        ), INTENT( INOUT ) :: Met
    TYPE( CurrentMetZ )   , INTENT( INOUT ) :: zzz
  END SUBROUTINE InitCurrentMet

  RECURSIVE SUBROUTINE GetMeanMet( Request,fld,Met,zzz )
    USE SWIM_fi
    USE SWIMpuff_fd
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ), INTENT( IN    ) :: Request
    TYPE( MetField ),       INTENT( IN    ) :: fld
    TYPE( PuffMet  ),       INTENT( INOUT ) :: Met
    TYPE( CurrentMetZ )   , INTENT( INOUT ) :: zzz
  END SUBROUTINE GetMeanMet

  RECURSIVE SUBROUTINE GetEnsemTurb( Request,fld,Met,zzz )
    USE SWIM_fi
    USE SWIMpuff_fd
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ), INTENT( IN    ) :: Request
    TYPE( MetField ),       INTENT( INOUT ) :: fld
    TYPE( PuffMet  ),       INTENT( INOUT ) :: Met
    TYPE( CurrentMetZ )   , INTENT( INOUT ) :: zzz
  END SUBROUTINE GetEnsemTurb

END INTERFACE

INTEGER, EXTERNAL :: SetMetField

SWIMcurrentMet = SWIMfailure

!------ Save request location in coordinates of met fields

lSaveCoord = .TRUE.


NULLIFY( zzz%zb,zzz%zbw )

!------ Set met field index

i = SetMetField( Request )
IF( i < 1 )GOTO 9999

!------ Save field id

Met%iField = i

!------ Initialization: set interpolation factors, terrain, etc.
!       N.B. Request%X,Y are transformed into met grid coordinates

CALL InitCurrentMet( Request,field(i),Met,zzz )
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Interpolate 2d boundary layer variables

CALL GetBLparam( field(i),Met%BL )

!------ Get mean velocities, temperature and gradients

CALL GetMeanMet( Request,field(i),Met,zzz )

!------ Get boundary layer turbulence

CALL GetBLTurb( field(i),Met )

!------ Get ensemble turbulence (large-scale variance & wind uncertainty)
!       N.B. Must be last routine called since wind uncertainty interpolation
!            may overwrite variables (in module SWIMcurrentMet_fi) used for
!            mean wind, etc.

CALL GetEnsemTurb( Request,field(i),Met,zzz )
IF( error%Number /= NO_ERROR )GOTO 9999

SWIMcurrentMet = SWIMresult

9999 CONTINUE

IF( i > 0 .AND. i <= numField )THEN
  IF( BTEST(field(i)%grid%type,GTB_Z3D) )THEN
    IF( ASSOCIATED(zzz%zb ) )DEALLOCATE(zzz%zb, STAT=i)
    IF( ASSOCIATED(zzz%zbw) )DEALLOCATE(zzz%zbw,STAT=i)
  END IF
END IF


lSaveCoord = .FALSE.

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE InitCurrentMet( Request,fld,Met,zzz )

!------ Initialization for met request

USE SWIMpuff_fd
USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( INOUT ) :: Request
TYPE( MetField       ), INTENT( INOUT ) :: fld
TYPE( PuffMet        ), INTENT( INOUT ) :: Met
TYPE( CurrentMetZ    ), INTENT( INOUT ) :: zzz

INTEGER irv, nz, nxy, nx, ny, i, k, alloc_stat
REAL    xp, yp, ztem, tem

REAL, DIMENSION(:), POINTER :: Z

TYPE( PuffMetRequest ) :: zRequest

INTERFACE

  RECURSIVE SUBROUTINE SetTopog( Request,grid,Met )
    USE SWIMmetField_fd
    USE SWIMpuff_fd
    TYPE( PuffMetRequest ),    INTENT( INOUT ) :: Request
    TYPE( MetGrid        ),    INTENT( IN    ) :: grid
    TYPE( PuffMet ), OPTIONAL, INTENT( INOUT ) :: Met
  END SUBROUTINE SetTopog

  RECURSIVE SUBROUTINE SetUAfac( Request,grid,zzz )
    USE SWIM_fi
    USE SWIMpuff_fd
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ), INTENT( IN ) :: Request
    TYPE( MetGrid        ), INTENT( IN ) :: grid
    TYPE( CurrentMetZ    ), INTENT( IN ) :: zzz
  END SUBROUTINE SetUAfac

END INTERFACE

INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: getFieldIndex

!------ Set basic grid parameters

lter     = BTEST( fld%grid%type,GTB_TERRAIN )
lstagger = BTEST( fld%grid%type,GTB_STAGGERZ ) !Used in IntXYZ

!------ Save requested puff size and latitude (for lat/lon gradient corrections)

sigh = SQRT( Request%Shh )
sigv = Request%SigZ

xbar = Request%X
ybar = Request%Y

!------ Convert to met grid coordinates

IF( PrjCoord%type /= fld%grid%coord%type )THEN
  IF( lSaveCoord )THEN
    i = getFieldIndex( fld%index )
    Request%X = xFld(i); Request%Y = yFld(i)
  ELSE
    irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xp,yp,fld%grid%coord )
    IF( irv /= SWIMsuccess )GOTO 9999
    Request%X = xp; Request%Y = yp
  END IF
END IF

!------ Set horizontal interpolation factors

CALL SetXYfac( Request,fld%grid,mx,my,mxu,myv )
CALL SetMXY( mx,my,mxy,fld%grid%nX,fld%grid%nXY,lter )

!------ Set terrain elevation, slopes and terrain-following coordinate

CALL SetTopog( Request,fld%grid,Met )

!------ Interpolate vertical grid for sigma coordinate

klev0  = 1
klev0w = 1

IF( BTEST(fld%grid%type,GTB_Z3D) )THEN

  nx  = fld%grid%nX
  ny  = fld%grid%nY
  nz  = fld%grid%nZ
  nxy = fld%grid%nXY

  NULLIFY( zzz%zb,zzz%zbw )

  ALLOCATE( zzz%zb(nz),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'InitCurrentMet'
    error%Message = 'Error allocating height array for sigma coordinates'
    met%ifield = -1
    GOTO 9999
  END IF

  DO k = 1,nz
    i =  (k-1)*nxy + 1
    Z => fld%grid%sigma%Z(i:); CALL IntXY( mxy,Z,zzz%zb(k) )
    IF( zzz%zb(k) < 0. )klev0 = k+1
  END DO

  klev0w = klev0

  IF( lstagger )THEN

    ALLOCATE( zzz%zbw(nz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'InitCurrentMet'
      error%Message = 'Error allocating height arrays for sigma coordinates on w-levels'
      GOTO 9999
    END IF

    DO k = 1,nz
      i =  (k-1)*nxy + 1
      Z => fld%grid%sigma%Zw(i:); CALL IntXY( mxy,Z,zzz%zbw(k) )
      IF( zzz%zbw(k) < 0. )klev0w = k+1
    END DO

  END IF

ELSE !---- Or just point to vertical grids

  zzz%zb  => fld%grid%Z
  zzz%zbw => fld%grid%Zw

END IF

!------ Compute horizontal gradient of height for sigma coordinate
!       N.B. sigma%Z is height AGL

IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
  zRequest = Request; zRequest%Shh = 0.; zRequest%SigZ = 0.; zRequest%Zcap = 0.
  CALL SetUAfac( zRequest,fld%grid,zzz ) !Use mz temporarily
  CALL IntXYZ( mxy,mz,fld%grid%sigma%Z,ztem,gxp,gyp,tem,1.,0.,0.,1,.FALSE. )
  gxp = -(gxp+hx); gyp = -(gyp+hy)
END IF

!------ Set vertical interpolation factors

CALL SetUAfac( Request,fld%grid,zzz )

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE GetBLparam( fld,BL )

USE SWIM_fi
USE SWIMpuff_fd
USE SWIMcurrentMet_fi
USE SWIMparam_fd
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( MetField       ), INTENT( IN  ) :: fld
TYPE( PuffMetBLparam ), INTENT( OUT ) :: BL

REAL invL, prate, rainprb, tbl, VHlim
REAL praten

REAL,    EXTERNAL :: UstarDep, LfromInvL
INTEGER, EXTERNAL :: PrecipRate2Type

SELECT CASE( fld%BLtype )

  CASE( BLP_NONE ) !No boundary layer

    BL%MixingHt   = 0.
    BL%HeatFlux   = 0.
    BL%zruf       = 0.
    BL%CanopyHt   = 0.
    BL%Alpha      = 0.
    BL%L          = 0.
    BL%Ustar2     = 0.
    BL%Wstar2     = 0.
    BL%PrecipType = 0
    BL%Zsl        = 0.
    BL%prate      = 0.
    BL%cc         = 0.

    lsl   = .FALSE.
    lzinv = .FALSE.

  CASE DEFAULT

    CALL IntXY( mxy,fld%BL%zi,BL%MixingHt )
    CALL IntXY( mxy,fld%BL%HeatFlux,BL%HeatFlux )
    CALL IntXY( mxy,fld%BLaux%wstr2,BL%Wstar2 )
    CALL IntXY( mxy,fld%BLaux%ustr2,BL%Ustar2 )
    CALL IntXY( mxy,fld%BL%cc,BL%cc )
    CALL IntXY( mxy,fld%BL%LAI,BL%LAI )

    IF( BL%HeatFlux < 0. )THEN
      VHlim = 0.09*(1.-0.5*BL%cc**2)
      BL%HeatFlux = MAX(BL%HeatFlux,-VHlim*SQRT(BL%Ustar2))
      BL%Wstar2   = 0.
    END IF

    CALL IntXY( mxy,fld%BLaux%tbl,tbl )
    invL = -VONK*G0/tbl * BL%HeatFlux/BL%Ustar2**1.5
    BL%L = LfromInvL( invL )

    CALL IntXY( mxy,fld%grid%landcover%roughness,BL%zruf )
    CALL IntXY( mxy,fld%grid%landcover%canopyHt,BL%CanopyHt )
    CALL IntXY( mxy,fld%grid%landcover%alpha,BL%Alpha )

    CALL IntXY( mxy,fld%BLaux%zsl,BL%Zsl )

    IF( BTEST(fld%type,FTB_PRCP) )THEN
      CALL nIntXY( mxy,fld%BL%prcp,BL%PrecipType )
      BL%prate = NOT_SET_R

    ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
      IF( BTEST(fld%type,FTB_ACCPR) )THEN
        IF( fld%tNext-fld%t > 1. )THEN
          CALL IntXY( mxy,fld%NextBL%prcp,praten )
          CALL IntXY( mxy,fld%BL%prcp,prate )
          prate = (praten-prate)/(fld%tNext-fld%t) * 3600. !mm/hr
        ELSE
          prate = 0.
        END IF
      ELSE
        CALL IntXY( mxy,fld%BL%prcp,prate )
      END IF

      BL%prate = prate

      IF( ASSOCIATED(fld%BL%rainprb) )THEN
        CALL IntXY( mxy,fld%BL%rainprb,rainprb )
        IF( BTEST(fld%type,FTB_T) )THEN
          CALL IntXY( mxy,fld%BLaux%tbl,tbl )
        ELSE
          rainprb = 100.
          tbl     = 300.
        END IF
      ELSE
        rainprb = 100.
        tbl     = 300.
      END IF
      BL%PrecipType = PrecipRate2Type( prate,rainprb,tbl )
      BL%prate = prate

    ELSE
      BL%PrecipType = NINT(fld%BL%prcp(1))
      BL%prate = NOT_SET_R

    END IF

    IF( fld%BLtype == BLP_PROF )THEN
      lzinv = .FALSE.
      lsl   = zh < BL%Zsl
    ELSE
      lzinv = (BL%MixingHt > 0.) .AND. (zh <= BL%MixingHt)
      lsl   = (zh <= BL%Zsl) .AND. lzinv
    END IF

END SELECT

!------ Get deposition flux velocity

BL%UstarDep = UstarDep( BL )

!------ Save terrain elevation (relative to Hmin reference)

BL%TerElev = hp
BL%Hmin    = Prj%Hmin

zsl = BL%Zsl

!------ Solar angle factor for activity decay

IF( Prj%decay )THEN
  CALL IntXY( mxy,fld%grid%sunfac,BL%sunfac )
ELSE
  BL%sunfac =0.0
END IF

BL%dTdz_mh = -999.

RETURN
END

!===============================================================================

REAL FUNCTION UstarDep( BL  )

USE SWIMpuff_fd

IMPLICIT NONE

TYPE( PuffMetBLparam ), INTENT( IN  ) :: BL

REAL fac

IF( BL%Wstar2 > 0. )THEN
  fac      = 0.46*(BL%zruf/BL%MixingHt)**0.16
  UstarDep = SQRT(BL%Ustar2 + fac*fac*BL%Wstar2)
ELSE
  UstarDep = SQRT(BL%Ustar2)
END IF

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION PrecipRate2Type( prate,rainprb,tbl ) RESULT( pType )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: prate    !rate (mm/hr)
REAL, INTENT( IN ) :: rainprb  !Probability of rain (%)
REAL, INTENT( IN ) :: tbl      !Temperature (K) near surface

!------ Precipitation group rate (mm/hr) boundaries (geometric means)

REAL, DIMENSION(2), PARAMETER :: PRR = (/ 1.3,13.3 /)  !Rain
REAL, DIMENSION(2), PARAMETER :: PRS = (/ 10.,44.7 /)  !Snow

REAL tmf, sfrat

IF( prate < 0.2 )THEN        !No precip
  pType = 0
  RETURN
END IF

IF( rainprb >= 50. )THEN    !Rain probability 50% or greater

  IF( prate <= PRR(1) )THEN
    pType = 1
  ELSE IF( prate <= PRR(2) )THEN
    pType = 2
  ELSE
    pType = 3
  END IF

ELSE                        !Otherwise, snow.  Increase rate by water equivalent factor

  tmf = (tbl+ABSZERO)*9./5. + 32.

  IF( tmf >= 27.5 )THEN
    sfrat=10.0
  ELSE IF( tmf < 27.5 .AND. tmf >=  19.5 )THEN
    sfrat=15.0
  ELSE IF( tmf < 19.5 .AND. tmf >=  14.5 )THEN
    sfrat=20.0
  ELSE IF( tmf < 14.5 .AND. tmf >=   9.5 )THEN
    sfrat=30.0
  ELSE IF( tmf <  9.5 .AND. tmf >=  -0.5 )THEN
    sfrat=40.0
  ELSE IF( tmf < -0.5 .AND. tmf >= -20.5 )THEN
    sfrat=50.0
  ELSE
    sfrat=100.0
  END IF

  sfrat = prate * sfrat

  IF( sfrat <= PRS(1) )THEN
    pType = 4
  ELSE IF( sfrat <= PRS(2) )THEN
    pType = 5
  ELSE
    pType = 6
  END IF

END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE GetMeanMet( Request,fld,Met,zzz )

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer
USE constants_fd
USE MapCoord_fd

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( IN    ) :: Request
TYPE( MetField ),       INTENT( IN    ) :: fld
TYPE( PuffMet  ),       INTENT( INOUT ) :: Met
TYPE( CurrentMetZ    ), INTENT( INOUT ) :: zzz

TYPE( meth ) mxyu, mxyv
TYPE( metv ) mv

TYPE( MapCoord ) coordO

INTEGER irv
INTEGER nx, nxy, nz
INTEGER k, k0, i, k1, k2
REAL    hc, alpha, zruf, L, zinv, zt
REAL    dum, dum1, dum2, t0, ztmp, rh, Ptop, Ttop, hsx
REAL    pr, pr1, pr2, thb, dtdz, dtdzs, divc
REAL    fz, fsl, dfdz, fac
REAL    lat, lon
REAL    qcld, sumqz, delz
REAL, DIMENSION(:), POINTER :: Qcloud

INTEGER, EXTERNAL :: SWIMcnvCoord
REAL,    EXTERNAL :: uzlog, dtdz_fun, StndHumid
REAL,    EXTERNAL :: SWIMrlimit

!------ Define local variables

zruf  = Met%BL%zruf
hc    = Met%BL%CanopyHt
alpha = Met%BL%Alpha
L     = Met%BL%L
zinv  = Met%BL%MixingHt

nx  = fld%grid%nX
nxy = fld%grid%nXY
nz  = fld%grid%nZ

!------ Setup interpolation factors accounting for surface layer

CALL set_mv( zzz%zb,fld%grid%nZ,mz,mv,zh,dp,zsl,hc,alpha,zruf,L )

!------ Interpolate velocity (set appropriately for staggered grid)

IF( BTEST(fld%grid%type,GTB_STAGGERB) )THEN
  CALL SetMXY( mxu,myv,mxyu,nx,nxy,lter )
  mxyv = mxyu
ELSE IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
  CALL SetMXY( mxu,my,mxyu,nx,nxy,lter )
  CALL SetMXY( mx,myv,mxyv,nx,nxy,lter )
ELSE
  mxyu = mxy
  mxyv = mxy
END IF

CALL IntXYZ( mxyu,mv,fld%Field%U, &
             Met%Mean%U, &
             Met%Mean%Ugrad%Ux,Met%Mean%Ugrad%Uy,Met%Mean%Ugrad%Uz, &
             dp,gxp,gyp,1,lstagger )

CALL IntXYZ( mxyv,mv,fld%Field%V, &
             Met%Mean%V, &
             Met%Mean%Ugrad%Vx,Met%Mean%Ugrad%Vy,Met%Mean%Ugrad%Vz, &
             dp,gxp,gyp,2,lstagger )

IF( BTEST(fld%type,FTB_W) )THEN

  IF( mzw%km == -fld%grid%nz )THEN
    Met%Mean%W = 0.
    Met%Mean%Ugrad%Wx = 0.; Met%Mean%Ugrad%Wy = 0.; Met%Mean%Ugrad%Wz = 0.

  ELSE
    CALL IntXYZ( mxy,mzw,fld%Field%W, &
                 Met%Mean%W, &
                 Met%Mean%Ugrad%Wx,Met%Mean%Ugrad%Wy,Met%Mean%Ugrad%Wz, &
                 dp,gxp,gyp,3,lstagger )


    fac = -1.              !Set W to approach parallel flow near the ground
    IF( klev0w > 1 )THEN
      IF( mzw%km < 0 )THEN
        IF( lstagger )THEN
          fac = zh*dp/zzz%zbw(-mzw%km)
        ELSE
          fac = zh*dp/zzz%zb(-mzw%km)
        END IF
      END IF
    ELSE
      IF( lstagger )THEN
        IF( zzz%zbw(1) <= 0. )THEN
          IF( mzw%km == -2 )fac = zh*dp/zzz%zbw(2)
        ELSE
          IF( mzw%km == -1 )fac = zh*dp/zzz%zbw(1)
        END IF
      ELSE
        IF( mzw%km == -1 )fac = zh*dp/zzz%zb(1)
      END IF
    END IF
    fac = MIN(fac,1.0)
    IF( fac > 0. )Met%Mean%W = fac*Met%Mean%W + (1.-fac)*(hx*Met%Mean%U + hy*Met%Mean%V)

  END IF

ELSE

  Met%Mean%W = 0.
  Met%Mean%Ugrad%Wx = 0.; Met%Mean%Ugrad%Wy = 0.; Met%Mean%Ugrad%Wz = 0.

END IF

IF( BTEST(fld%type,FTB_DU2) )THEN
    CALL IntXYZ( mxy,mz,fld%Field%dU2, &
                 Met%Mean%dU2,dum,dum1,dum2, &
                 dp,gxp,gyp,0,lstagger )
ELSE
  Met%Mean%dU2 = 0.
END IF

!------ Correct Ux & Uy for lat/lon coordinates (but not polar fields)

SELECT CASE( fld%grid%coord%type )

  CASE( I_LATLON )

    IF( .NOT.(BTEST(fld%grid%type,GTB_NPOLE) .OR. BTEST(fld%grid%type,GTB_SPOLE)) )THEN
      fac = TAN(PI180*ybar)/Rearth
      Met%Mean%Ugrad%Ux = Met%Mean%Ugrad%Ux - Met%Mean%V*fac
      Met%Mean%Ugrad%Uy = Met%Mean%Ugrad%Uy + Met%Mean%U*fac
    END IF

  CASE( I_LAMBERT,I_POLAR,I_RPOLAR,I_ROTLL )

    IF( PrjCoord%type == I_LATLON )THEN
      lon = xbar; lat = ybar
    ELSE IF( fld%grid%coord%type == I_ROTLL )THEN
      lon = Request%X; lat = Request%Y !Use rotated lat/lon for wind rotation
    ELSE
      coordO%type = I_LATLON
      irv = SWIMcnvCoord( xbar,ybar,PrjCoord,lon,lat,coordO )
    END IF

    CALL SWIMrotMet2LL( lat,lon,Met%Mean,fld%grid%coord )

END SELECT

!------ Ensure zero divergence

divc = -0.5*(Met%Mean%Ugrad%Ux + Met%Mean%Ugrad%Vy + Met%Mean%Ugrad%Wz)

Met%Mean%Ugrad%Ux = Met%Mean%Ugrad%Ux + divc
Met%Mean%Ugrad%Vy = Met%Mean%Ugrad%Vy + divc

!------ Velocity gradients below lowest grid level (if in surface layer)

IF( lsl .AND. mv%km <= 0 )THEN

  fsl = -1.
  CALL set_fsl( zh,zsl,hc,alpha,zruf,L,fz,fsl )
  dfdz = uzlog( hc,alpha,zh,zruf,L ) / fz
  Met%Mean%Ugrad%Uz = dfdz*Met%Mean%U
  Met%Mean%Ugrad%Vz = dfdz*Met%Mean%V

END IF

!------ Pressure

  IF( BTEST(fld%type,FTB_P) )THEN
    CALL IntXYZ( mxy,mz,fld%Field%Press, &
                 pr,dum,dum1,dum2, &
                 dp,gxp,gyp,0,lstagger )
  pr = EXP(pr)
  IF( zm < zzz%zb(klev0)*dp )THEN
    ztmp = zzz%zb(klev0)
  ELSE IF( zm > zzz%zb(nz)*dp )THEN
    ztmp = zzz%zb(nz)
    IF( BTEST(fld%type,FTB_H) )Ptop = pr * PSURF !Save for humidity extrapolation
  ELSE
    ztmp = NOT_SET_R
  END IF
  IF( ztmp /= NOT_SET_R )THEN              !Extrapolate with standard atmosphere
    ztmp = ztmp*dp + hp + Prj%Hmin
    CALL stnd_atmos( ztmp,pr1,t0,dtdz,SA_TA )
    IF( BTEST(fld%type,FTB_H) .AND. .NOT.BTEST(fld%type,FTB_T))Ttop = t0 !Save for humidity extrapolation (actual temp)
    ztmp = zRef + Prj%Hmin
    CALL stnd_atmos( ztmp,pr2,t0,dtdz,SA_TA )
    pr = pr * (pr2/pr1)
  END IF
ELSE
  ztmp = zRef + Prj%hmin
  CALL stnd_atmos( ztmp,pr,thb,dtdz,SA_TPOT )
END IF
Met%Mean%Press = pr * PSURF

!------ Temperature

IF( BTEST(fld%type,FTB_T) )THEN
  CALL IntXYZ( mxy,mz,fld%Field%Tpot, &
               Met%Mean%Tpot,dum,dum1,dtdz, &
               dp,gxp,gyp,0,lstagger )
  IF( zm < zzz%zb(klev0)*dp )THEN
    ztmp = zzz%zb(klev0)
  ELSE IF( zm > zzz%zb(nz)*dp )THEN
    ztmp = zzz%zb(nz)
  ELSE
    ztmp = NOT_SET_R
  END IF
  IF( ztmp /= NOT_SET_R )THEN              !Extrapolate with standard atmosphere
    ztmp = ztmp*dp + hp + Prj%Hmin
    CALL stnd_atmos( ztmp,pr1,t0,dtdz,SA_TPOT )
    IF( zm > zzz%zb(nz)*dp .AND. BTEST(fld%type,FTB_H) )THEN !Save for humidity extrapolation
      IF( BTEST(fld%type,FTB_P) )THEN
        Ttop = Met%Mean%Tpot * (Ptop/PSURF)**KAPPA  !Actual temp
      ELSE
        Ptop = pr1 * PSURF
        Ttop = Met%Mean%Tpot * pr1**KAPPA
      END IF
    END IF
    Met%Mean%Tpot = Met%Mean%Tpot - t0
    ztmp = zRef + Prj%Hmin
    CALL stnd_atmos( ztmp,dum,t0,dtdz,SA_TPOT )
    Met%Mean%Tpot = Met%Mean%Tpot + t0
  END IF
ELSE IF( BTEST(fld%type,FTB_P) )THEN
  ztmp = zRef + Prj%Hmin
  CALL stnd_atmos( ztmp,pr1,Met%Mean%Tpot,dtdz,SA_TPOT )
ELSE
  Met%Mean%Tpot = thb     !Already called standard atmosphere for pressure
END IF

Met%Mean%T = Met%Mean%Tpot * pr**KAPPA !Actual temperature

!------ Check temperature gradient

IF( lzinv )THEN
  IF( Met%BL%L > 0. )THEN
    dtdz  = MAX(dtdz,0.001) !Must be stable
    zt    = MAX(0.5*zinv,zsl)
    dtdzs = dtdz_fun( Met%BL%HeatFlux,Met%BL%Ustar2,zt,L,zruf,.FALSE. )
    zt    = MIN(0.9*zinv,Met%BL%L*0.5)
    IF( zh < zt )THEN
      dtdz = dtdzs
    ELSE
      fac = (zinv-zh)/(zinv-zt) * EXP(-2.*(zh-zt)/Met%BL%L)
      dtdz = fac*dtdzs + (1.-fac)*dtdz
    END IF
    IF( zh < hc )dtdz = dtdz * EXP(alpha*(zh/hc-1.))
  ELSE
    dtdz = 0.
  END IF
END IF

Met%Mean%TpotZ = MAX(dtdz,0.)


!------ Humidity

IF( BTEST(fld%type,FTB_H) )THEN

  CALL IntXYZ( mxy,mz,fld%Field%Humid, &
               Met%Mean%Humid,dum,dum1,dum2, &
               dp,gxp,gyp,0,lstagger )

!------ Extrapolate if necessary

  IF( zm > zzz%zb(nz)*dp .AND. Met%Mean%Press > 10. )THEN

    IF( .NOT.(BTEST(fld%type,FTB_P) .OR. BTEST(fld%type,FTB_T)) )THEN
      ztmp = zzz%zb(nz)*dp + hp + Prj%Hmin
      CALL stnd_atmos( ztmp,pr,Ttop,dtdz,SA_TA )
      Ptop = pr*PSURF
    END IF

    IF( Ptop > PSTRAT )THEN         !Only extrapolate if top met level is below stratosphere
      rh = Met%Mean%Humid
      Met%Mean%Humid = rh + (RHSTRAT-rh)/(PSTRAT-Ptop) * (Met%Mean%Press-Ptop)
    END IF
  END IF

  CALL HumidMixRatio( Met%Mean%Humid,Met%Mean%T,Met%Mean%Press,hsx )
  Met%Mean%Humid = MAX(hsx,HSTRAT)

  IF( Met%Mean%Press <= 10. )Met%Mean%Humid = MIN(Met%Mean%Humid,HSTRAT*Met%Mean%Press/10.)

ELSE

  Met%Mean%Humid = StndHumid( Met%Mean%Press,Met%Mean%T )

END IF

!------ Cloud liquid water

Met%Mean%cldBot = 0.
Met%Mean%cldTop = 0.
Met%Mean%cldAvg = 0.

IF( BTEST(fld%type,FTB_QCLD) )THEN

  CALL IntXYZ( mxy,mz,fld%Field%Qcloud, &
               Met%Mean%Qcloud,dum,dum1,dum2, &
               dp,gxp,gyp,0,lstagger )

  IF( zh > zzz%zb(nz)*dp + 10. )Met%Mean%Qcloud = Met%Mean%Qcloud * EXP(-0.1*(zm-zzz%zb(nz)*dp-10.))


!------ Cloud top & bottom

  IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
    k0 = nxy
  ELSE
    k0 = 0
  END IF

  k1 = nz+1; k2 = 0; sumqz = 0.

  DO k = 1,nz
    i =  k0 + (k-1)*nxy + 1
    Qcloud => fld%Field%Qcloud(i:); CALL IntXY( mxy,Qcloud,qcld )
    IF( qcld > 0. )THEN
      k1 = MIN(k,k1)
      k2 = MAX(k,k2)
      IF( k == 1 )THEN
        delz = 0.5*(zzz%zb(1) + zzz%zb(2))
      ELSE IF( k == nz )THEN
        delz = zzz%zb(nz) - zzz%zb(nz-1)
      ELSE
        delz = 0.5*(zzz%zb(k+1) + zzz%zb(k-1))
      END IF
      sumqz = sumqz + qcld*delz*dp
    END IF
  END DO

  IF( k1 == 1 )THEN
    Met%Mean%cldBot = 0.
  ELSE IF( k1 < nz-1 )THEN
    Met%Mean%cldBot = 0.5*(zzz%zb(k1) + zzz%zb(k1-1)) * dp
  END IF
  IF( k2 == nz )THEN
    Met%Mean%cldTop = (1.5*zzz%zb(nz) - 0.5*zzz%zb(nz-1)) * dp
  ELSE IF ( k2 > 0 )THEN
    Met%Mean%cldTop = 0.5*(zzz%zb(k2) + zzz%zb(k2+1)) * dp
  END IF

  IF( sumqz > 0. .AND. Met%Mean%cldTop-Met%Mean%cldBot > 0. )THEN
    Met%Mean%cldAvg = sumqz/(Met%Mean%cldTop-Met%Mean%cldBot)
  END IF

ELSE

  Met%Mean%Qcloud = 0.

END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE GetBLTurb( fld,Met )

!------ Get small-scale turbulence

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( IN    ) :: fld
TYPE( PuffMet  ), INTENT( INOUT ) :: Met

TYPE( metv ) :: mt

REAL    frac, dum, dum1, dum2, amet, bmet, zarg, btem
REAL    zs, zinv, dzbl, us2, ws2, wtbl, dtdz
REAL    zruf, hc, alpha, L, zp
REAL    difb, dddz
INTEGER nzbl
LOGICAL lblcap

REAL, EXTERNAL :: phi, phiz

Met%Turb%WWgrad    = 0.
Met%Turb%DiffGradX = 0.
Met%Turb%DiffGradY = 0.

IF( lzinv )THEN !From 'standard' SCIPUFF BL profiles

  zinv  = Met%BL%MixingHt
  nzbl  = fld%BLaux%nz
  us2   = Met%BL%Ustar2
  ws2   = Met%BL%Wstar2
  wtbl  = Met%BL%HeatFlux
  zruf  = Met%BL%zruf
  L     = Met%BL%L
  hc    = MAX(Met%BL%CanopyHt,0.)
  alpha = Met%BL%Alpha
  dtdz  = Met%Mean%TpotZ

  dzbl   = 1.0/FLOAT(nzbl-1)
  mt%dzr = 1.0/(dzbl*zinv)
  frac   = zh*mt%dzr

  zarg = frac*dzbl

  CALL BLturb( zarg,zinv,us2,ws2,wtbl, &
               Met%Turb%UUshear,Met%Turb%UUbuoy,Met%Turb%WW,Met%Turb%QQshear, &
               Met%Turb%WWgrad,Met%Turb%LscaleBuoy,Met%Turb%LscaleShear, &
               amet,bmet,hc,alpha,zruf )

  btem = bmet*dtdz
  difb = amet / (1. + btem)
  wtbl = MAX(wtbl*(1.0-zarg),0.)

  IF( frac >= 1. )THEN
    frac    = MIN(frac,FLOAT(nzbl-2))
    mt%km   = MIN(INT(frac)+1,nzbl-2)
    mt%kp   = mt%km + 1
    mt%ratz = frac - FLOAT(mt%km-1)
    mt%rzm1 = 1.0 - mt%ratz
    mt%facm = 0.0
    mt%facp = 0.0
    CALL IntdXYZ( mxy,mt,dtdz,fld%BLaux%aDiff,fld%BLaux%bDiff,dum,dddz,1.,.FALSE. )
  ELSE
    dddz = difb / MAX(zh,zruf)
    dddz = dddz * ( (amet+bmet*wtbl)/amet - 2.*btem/(1.+btem) )
  END IF

  Met%Turb%WT       = wtbl
  Met%Turb%Diff     = difb
  Met%Turb%DiffGrad = dddz

!------ Compute horizontal gradients in shear-driven diffusivity

  !Don't call HorizDiffGrad since its output is ultimately unused
  !If that changes, must point to appropriate BL, BL aux
  Met%Turb%DiffGradX = 0.
  Met%Turb%DiffGradY = 0.
!  IF( lsl .AND. fld%grid%nXY > 1 )CALL HorizDiffGrad( fld,Met )

ELSE IF( BTEST(fld%type,FTB_UU) )THEN  !Profile BL (lzinv = .FALSE.)

  zruf = Met%BL%zruf
  L    = Met%BL%L
  dtdz = Met%Mean%TpotZ

  CALL IntXYZ( mxy,mz,fld%BLprof%UU,Met%Turb%UUshear,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLprof%VV,Met%Turb%UUbuoy,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLprof%WW,Met%Turb%WW,dum,dum1,Met%Turb%WWgrad, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLprof%WT,Met%Turb%WT,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLprof%SL,Met%Turb%LscaleBuoy,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLprof%SZ,Met%Turb%LscaleShear,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntXYZ( mxy,mz,fld%BLaux%QQ,Met%Turb%QQshear,dum,dum1,dum2, &
                                                dp,gxp,gyp,1,lstagger )
  CALL IntdXYZ( mxy,mz,dtdz,fld%BLaux%aDiff,fld%BLaux%bDiff,difb,dddz,1.,lstagger )

  IF( lsl )THEN
    zs   = fld%grid%Z(1)*dp
    zp   = MAX(zh,zruf)
    difb = phi( zs,L ) * zp / (phi( zp,L )*zs) * difb
    dddz = difb*(1.-phiz( zp,L ))/zp
    Met%Turb%LscaleShear = 0.65*(zp+zruf)
  END IF

  Met%Turb%Diff     = difb
  Met%Turb%DiffGrad = dddz

ELSE !------ Stable region above boundary layer

  Met%Turb%UUshear     = Prj%BL%WWtrop
  Met%Turb%UUbuoy      = Prj%BL%UUcalm
  Met%Turb%WW          = Prj%BL%WWtrop
  Met%Turb%LscaleShear = Prj%BL%SLtrop
  Met%Turb%LscaleBuoy  = Prj%BL%SLcalm
  Met%Turb%QQshear     = 3.*Prj%BL%WWtrop
  Met%Turb%Diff        = Prj%BL%Atrop/(1.0+Prj%BL%Btrop*Met%Mean%TpotZ)
  Met%Turb%DiffGrad    = 0.0
  Met%Turb%WT          = 0.0

!----- Check for "close" to uncapped BL to balance splitting from below

  lblcap = (Met%Mean%TpotZ > 0.) .AND. (Met%BL%HeatFlux >= 0.)

  IF( zh-sigv < Met%BL%MixingHt .AND. .NOT.lblcap )THEN
    nzbl   = fld%BLaux%nz
    mt%dzr = FLOAT(nzbl-1)/Met%BL%MixingHt
    frac   = (zh-sigv)*mt%dzr
    IF( frac > 1.0 )THEN
      frac    = MIN(frac,FLOAT(nzbl-2))
      mt%km   = MIN(INT(frac)+1,nzbl-2)
      mt%kp   = mt%km + 1
      mt%ratz = frac - FLOAT(mt%km-1)
      mt%rzm1 = 1.0 - mt%ratz
      mt%facm = 0.0
      mt%facp = 0.0
      CALL IntdXYZ( mxy,mt,Met%Mean%TpotZ,fld%BLaux%aDiff,fld%BLaux%bDiff, &
                    difb,dddz,1.,.FALSE. )
      Met%Turb%DiffGrad = MIN(dddz,(Met%Turb%Diff-difb)/sigv)
    END IF
  END IF

END IF


RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE HorizDiffGrad( fld,Met )

!------ Get horizontal gradients of shear-driven diffusivity (drift velocity)

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( MetField ),       INTENT( IN    ) :: fld
TYPE( PuffMet  ),       INTENT( INOUT ) :: Met

INTEGER i1, i2, i3, i4
REAL    Dif1, Dif2, Dif3, Dif4, vm, vp

i1 = mxy%ij; i2 = mxy%ij+mxy%nxi; i3 = mxy%ij+mxy%nxi+1; i4 = mxy%ij+1

CALL GetShearDiffPoint( i1,zh,fld,Dif1 )
CALL GetShearDiffPoint( i2,zh,fld,Dif2 )
CALL GetShearDiffPoint( i3,zh,fld,Dif3 )
CALL GetShearDiffPoint( i4,zh,fld,Dif4 )

vm = mxy%rym1*Dif1 + mxy%raty*Dif2
vp = mxy%raty*Dif3 + mxy%rym1*Dif4

Met%Turb%DiffGradX = (vp-vm)*mxy%dxr

vm = mxy%rxm1*Dif1 + mxy%ratx*Dif4
vp = mxy%rxm1*Dif2 + mxy%ratx*Dif3

Met%Turb%DiffGradY = (vp-vm)*mxy%dyr

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE GetShearDiffPoint( i,zh,fld,Dif )

USE SWIM_fi
USE constants_fd

IMPLICIT NONE

INTEGER ,         INTENT( IN  ) :: i
REAL,             INTENT( IN  ) :: zh
TYPE( MetField ), INTENT( IN  ) :: fld
REAL,             INTENT( OUT ) :: Dif

REAL hc, zarg
REAL wwz
REAL uu_shear, uu_buoy, ww, qq, LscaleBuoy, LscaleShear, amet, bmet

hc   = MAX(fld%grid%landcover%canopyHt(i),0.)
zarg = zh/fld%BL%zi(i)

CALL BLturb( zarg,fld%BL%zi(i),fld%BLaux%ustr2(i),fld%BLaux%wstr2(i),fld%BL%HeatFlux(i), &
             uu_shear,uu_buoy,ww,qq,wwz,LscaleBuoy,LscaleShear,amet,bmet, &
             hc,fld%grid%landcover%alpha(i),fld%grid%landcover%roughness(i) )

Dif = uu_shear*LscaleShear/(A*SQRT(2.*uu_shear + ww))

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE GetEnsemTurb( Request,fld,Met,zzz )

!------ Get ensemble turbulence

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMinterp_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( IN    ) :: Request
TYPE( MetField ),       INTENT( INOUT ) :: fld
TYPE( PuffMet  ),       INTENT( INOUT ) :: Met
TYPE( CurrentMetZ )    , INTENT( INOUT ) :: zzz

REAL    fac, hc, alpha, zruf, L, z
REAL    dum, dum1

INTERFACE
  RECURSIVE SUBROUTINE SetTopog( Request,grid,Met )
    USE SWIMmetField_fd
    USE SWIMpuff_fd
    TYPE( PuffMetRequest ),    INTENT( INOUT ) :: Request
    TYPE( MetGrid        ),    INTENT( IN    ) :: grid
    TYPE( PuffMet ), OPTIONAL, INTENT( INOUT ) :: Met
  END SUBROUTINE SetTopog

  RECURSIVE SUBROUTINE SetUAfac( Request,grid,zzz )
    USE SWIM_fi
    USE SWIMpuff_fd
    USE SWIMcurrentMet_fi
    TYPE( PuffMetRequest ), INTENT( IN ) :: Request
    TYPE( MetGrid        ), INTENT( IN ) :: grid
    TYPE( CurrentMetZ    ), INTENT( IN ) :: zzz
  END SUBROUTINE SetUAfac

END INTERFACE

REAL, DIMENSION(:), POINTER :: uuLSV, slLSV
REAL, EXTERNAL :: ulog, sind, cosd

!------ Define a surface layer reduction factor for LSV

fac = 1.0

IF( lsl .AND. (fld%LSVtype /= LVP_OFF) )THEN

  z = -1.
  IF( fld%LSVtype == LVP_MET )THEN
    IF( mz%km < 0 .AND. mz%km /= -fld%grid%nZ )z = MIN(zsl,zzz%zb(-mz%km)*dp)
  ELSE
    z = zsl
  END IF

  IF( z /= -1. )THEN
    hc    = Met%BL%CanopyHt
    alpha = Met%BL%Alpha
    zruf  = Met%BL%zruf
    L     = Met%BL%L
    fac   = ulog( hc,alpha,zh,zruf,L ) / ulog( hc,alpha,z,zruf,L )
    fac   = MIN(fac*fac,1.0)
  END IF

END IF

!------ Large-scale variance

SELECT CASE( fld%LSVtype )

  CASE( LVP_INPUT )

    Met%LSV%UU     = fld%LSV%UU(1)*fac
    Met%LSV%VV     = fld%LSV%VV(1)*fac
    Met%LSV%Lscale = fld%LSV%SL(1)
    Met%LSV%UV     = 0.
    Met%LSV%UUz    = 0.
    Met%LSV%VVz    = 0.
    Met%LSV%UVz    = 0.

  CASE( LVP_MODEL )

    IF( fld%grid%coord%type == I_LATLON )THEN
      uuLSV => fld%LSV%UU(1:fld%grid%nXY-fld%grid%nX+1:fld%grid%nX)
      slLSV => fld%LSV%SL(1:fld%grid%nXY-fld%grid%nX+1:fld%grid%nX)
      CALL IntX( my,uuLSV,Met%LSV%UU )
      CALL IntX( my,slLSV,Met%LSV%Lscale )
    ELSE
      CALL IntXY( mxy,fld%LSV%UU,Met%LSV%UU )
      CALL IntXY( mxy,fld%LSV%SL,Met%LSV%Lscale )
    END IF

    Met%LSV%UU  = Met%LSV%UU*fac
    Met%LSV%VV  = Met%LSV%UU
    Met%LSV%UV  = 0.
    Met%LSV%UUz = 0.
    Met%LSV%VVz = 0.
    Met%LSV%UVz = 0.

  CASE( LVP_MET )

    CALL IntXYZ( mxy,mz,fld%LSV%UU,Met%LSV%UU,dum,dum1,Met%LSV%UUz, &
                                                  dp,gxp,gyp,0,lstagger )
    CALL IntXYZ( mxy,mz,fld%LSV%VV,Met%LSV%VV,dum,dum1,Met%LSV%VVz, &
                                                  dp,gxp,gyp,0,lstagger )
    CALL IntXYZ( mxy,mz,fld%LSV%UV,Met%LSV%UV,dum,dum1,Met%LSV%UVz, &
                                                  dp,gxp,gyp,0,lstagger )

    Met%LSV%UU  = Met%LSV%UU*fac
    Met%LSV%VV  = Met%LSV%VV*fac
    Met%LSV%UV  = Met%LSV%UV*fac
    Met%LSV%UUz = Met%LSV%UUz*fac
    Met%LSV%VVz = Met%LSV%VVz*fac
    Met%LSV%UVz = Met%LSV%UVz*fac

    Met%LSV%Lscale = fld%LSV%SL(1)

  CASE DEFAULT

    Met%LSV%UU     = 0.
    Met%LSV%VV     = 0.
    Met%LSV%UV     = 0.
    Met%LSV%UUz    = 0.
    Met%LSV%VVz    = 0.
    Met%LSV%UVz    = 0.
    Met%LSV%Lscale = 1000.

END SELECT

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SetXYfac( Request,grid,mx,my,mxu,myv )

USE SWIMparam_fd
USE SWIMmetField_fd
USE SWIMpuff_fd
USE SWIMinterp_fd

USE swim_fi, ONLY: error

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( IN  ) :: Request
TYPE( MetGrid        ), INTENT( IN  ) :: grid
TYPE( met1dh ),         INTENT( OUT ) :: mx, mxu, my, myv

REAL xfac, yfac

CALL SWIMmapfac( grid%coord,Request%X,Request%Y,xfac,yfac )

CALL get_1dfac( Request%X,grid%Xmin,grid%dX,grid%nX,xfac,mx )
CALL get_1dfac( Request%Y,grid%Ymin,grid%dY,grid%nY,yfac,my )

IF( BTEST(grid%type,GTB_STAGGER) )THEN

  mxu = mx
  IF( mx%rat > 0.5 )THEN
    mxu%rat = mx%rat - 0.5
  ELSE
    mxu%rat = 0.5 + mx%rat
    mxu%i   = mx%i - 1
    IF( mxu%i == 0 )THEN
      mxu%i   = 1
      mxu%rat = 0.
    END IF
  END IF
  mxu%rm1 = 1. - mxu%rat

  myv = my
  IF( my%rat > 0.5 )THEN
    myv%rat = my%rat - 0.5
  ELSE
    myv%rat = 0.5 + my%rat
    myv%i   = my%i - 1
    IF( myv%i == 0 )THEN
      myv%i   = 1
      myv%rat = 0.
    END IF
  END IF
  myv%rm1 = 1. - myv%rat

END IF

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE get_1dfac( xp,x1,dx,nx,xfac,mx )

USE SWIMinterp_fd

IMPLICIT NONE

REAL,           INTENT( IN  ) :: xp, x1, dx
INTEGER,        INTENT( IN  ) :: nx
REAL,           INTENT( IN  ) :: xfac
TYPE( met1dh ), INTENT( OUT ) :: mx

INTEGER ig
REAL    ratx, rxm1, frac

IF( nx > 1 )THEN

  frac = (xp-x1)/dx
  frac = MAX(frac,0.0)
  frac = MIN(frac,FLOAT(nx-1)-1.E-3)
  ig   = INT(frac) + 1
  ratx = frac - FLOAT(ig-1)
  rxm1 = 1. - ratx
  mx%dxr = xfac/dx

ELSE

  ig   = 1
  ratx = 0.
  rxm1 = 1.
  mx%dxr = 0.

END IF

mx%rm1 = rxm1
mx%rat = ratx
mx%i   = ig

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetMXY( m1,m2,m,nx,nxy,lter )

USE SWIMinterp_fd

IMPLICIT NONE

TYPE( met1dh ), INTENT( IN  ) :: m1, m2
TYPE( meth ),   INTENT( OUT ) :: m
INTEGER,        INTENT( IN  ) :: nx, nxy
LOGICAL,        INTENT( IN  ) :: lter

m%cc1 = m1%rm1*m2%rm1
m%cc2 = m1%rm1*m2%rat
m%cc3 = m1%rat*m2%rat
m%cc4 = m1%rat*m2%rm1

m%ratx = m1%rat
m%rxm1 = m1%rm1
m%raty = m2%rat
m%rym1 = m2%rm1

m%nxyi = nxy
m%nxi  = nx
m%ij   = (m2%i-1)*nx + m1%i

m%dxr  = m1%dxr
m%dyr  = m2%dxr

m%lter = lter

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetUAfac( Request,grid,zzz )

!------ Get vertical interpolation factors for upper air fields

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMinterpPointer
USE SWIMcurrentMet_fi

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( IN ) :: Request
TYPE( MetGrid        ), INTENT( IN ) :: grid
TYPE( CurrentMetZ    ), INTENT( IN ) :: zzz

REAL szx, zcx

IF( Request%SigZ > 0. )THEN
  szx = Request%SigZ/dp
  zcx = (Request%Zcap-hp)/dp
ELSE
  szx = 0.
  zcx = 0.
END IF

CALL SetZfac( zm,zzz%zb,grid%nZ,mz,szx,zcx )

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
  CALL SetZfac( zm,zzz%zbw,grid%nZ,mzw,szx,zcx )
ELSE
  mzw = mz
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetTopog( Request,grid,Met )

!------ Set topography-related variables

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMcurrentMet_fi
USE SWIMgetTopog_fi

IMPLICIT NONE

TYPE( PuffMetRequest ),    INTENT( INOUT ) :: Request
TYPE( MetGrid        ),    INTENT( IN    ) :: grid
TYPE( PuffMet ), OPTIONAL, INTENT( INOUT ) :: Met

TYPE( Topog ) :: TerStr, TerStrOld

TYPE( MetGrid ), POINTER :: gridi

INTEGER irv
REAL    xi, yi, zzm

INTEGER, EXTERNAL :: SWIMcnvCoord

zRef = Request%Z  !Height above project Hmin (if not AGL)

IF( BTEST(grid%type,GTB_TERRAIN) )THEN

  TerStr = SWIMGetTopog( grid,Request%X,Request%Y )

  hp = TerStr%H
  hx = TerStr%Hx
  hy = TerStr%Hy
  dp = 1. - hp/grid%Ztop

!------ Set 'delta elevation' if input/output fields differ

  IF( PRESENT(Met) )THEN
    IF( Met%iField /= Request%iField .AND.  &
        Request%iField > 0 .AND. Request%iField <= numField )THEN
      gridi => field(Request%iField)%grid
      IF( BTEST(gridi%type,GTB_TERRAIN) )THEN
        IF( gridi%coord%type /= grid%coord%type )THEN
          irv = SWIMcnvCoord( Request%X,Request%Y,grid%coord,xi,yi,gridi%coord )
        ELSE
          xi = Request%X; yi = Request%Y
        END IF
        TerStrOld = SWIMGetTopog( field(Request%iField)%grid,xi,yi )
        Met%dElev = hp + grid%Hmin - TerStrOld%H - gridi%Hmin
      ELSE
        Met%dElev = hp + grid%Hmin
      END IF
      zRef = zRef + Met%dElev
      IF( Request%Zcap > 0. )Request%Zcap = Request%Zcap + Met%dElev
    ELSE
      Met%dElev = 0.
    END IF
  END IF

!------ Reset terrain to be relative to project Hmin

  hp = hp + grid%Hmin - Prj%Hmin

!------ Add terrain height if AGL

  IF( BTEST(Request%type,RTB_AGL) )zRef = zRef + hp

!------ Use height based on SigZ for near- (or below-)surface requests

  CALL SetZref( zRef,Request%SigZ,Request%Zcap,hp )

  zh = zRef - hp

  IF( BTEST(grid%type,GTB_Z3D) )THEN

    zm  = zh
    dp  = 1.
    gxp = 1.
    gyp = 1.

  ELSE

    zm  = zh/dp
    zzm = MIN(zm/grid%Ztop,1.) - 1.

    gxp = zzm*hx
    gyp = zzm*hy

  END IF

ELSE

  hp = 0.
  hx = 0.
  hy = 0.
  dp = 1.

!------ Set 'delta elevation' if input/output fields differ

  IF( PRESENT(Met) )THEN
    IF( Met%iField /= Request%iField .AND.  &
        Request%iField > 0 .AND. Request%iField <= numField )THEN
      IF( BTEST(field(Request%iField)%grid%type,GTB_TERRAIN) )THEN
        TerStrOld = SWIMGetTopog( field(Request%iField)%grid,Request%X,Request%Y )
        Met%dElev = hp - TerStrOld%H - field(Request%iField)%grid%Hmin
        zRef = zRef + Met%dElev
        IF( Request%Zcap > 0. )Request%Zcap = Request%Zcap + Met%dElev
      ELSE
        Met%dElev = 0.
      END IF
    ELSE
      Met%dElev = 0.
    END IF
  END IF

  CALL SetZref( zRef,Request%SigZ,Request%Zcap,hp )

  zh  = zRef
  zm  = zRef
  gxp = 0.
  gyp = 0.

END IF

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SetZref( zRef,SigZ,Zcap,hp )

IMPLICIT NONE

REAL, INTENT( INOUT ) :: zRef
REAL, INTENT( IN    ) :: SigZ
REAL, INTENT( IN    ) :: Zcap
REAL, INTENT( IN    ) :: hp

LOGICAL lGrdRfl, lCapRfl
REAL    zzm

lGrdRfl = zRef-hp < SigZ
lCapRfl = Zcap > 0.0 .AND. Zcap-zRef < SigZ

IF( lGrdRfl .OR. lCapRfl )THEN
  zzm = 0.5*(Zcap+hp)
  IF( lGrdRfl .AND. lCapRfl )THEN
    zRef = zzm
  ELSE IF( lGrdRfl )THEN
    zRef = hp + SigZ
    IF( Zcap > 0.0 )zRef = MIN(zRef,zzm)
  ELSE
    zRef = MAX(zzm,Zcap-SigZ)
  END IF
END IF

RETURN
END

!==============================================================================

RECURSIVE INTEGER FUNCTION SetMetField( Request ) RESULT( ifld )

!------ Select met field based on puff request

USE SWIMpuff_fd
USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( INOUT ) :: Request

INTEGER irv

INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: SelectMetField

!------ Allocate and intialize arrays for transformed locations

IF( lSaveCoord )THEN
  xFld = NOT_SET_R
  yFld = NOT_SET_R
END IF

ifld = Request%iField

IF( ifld < 1 .OR. .NOT.BTEST(Request%type,RTB_FLD) )THEN

  IF( numField == 1 .AND. Request%Shh == 0. )THEN
    ifld = 1
    IF( lSaveCoord )THEN
      IF( PrjCoord%type /= field(1)%grid%coord%type )THEN
        irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xFld(1),yFld(1),field(1)%grid%coord )
      ELSE
        xFld(1) = Request%X; yFld(1) = Request%Y
      END IF
    END IF
  ELSE
    ifld = SelectMetField( Request )
    IF( ifld < 1 )GOTO 9999
  END IF

ELSE IF( ifld > numField )THEN

  ifld = -1
  error%Number  = IV_ERROR
  error%Routine = 'SetMetField'
  error%Message = 'Invalid met field number'

ELSE IF( lSaveCoord )THEN

  IF( PrjCoord%type /= field(ifld)%grid%coord%type )THEN
    irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xFld(ifld),yFld(ifld),field(ifld)%grid%coord )
  ELSE
    xFld(ifld) = Request%X; yFld(ifld) = Request%Y
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE INTEGER FUNCTION SelectMetField( Request ) RESULT( ifld )

!------ Select met field based on puff request

USE SWIMpuff_fd
USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi
USE constants_fd

IMPLICIT NONE

TYPE( PuffMetRequest ), INTENT( INOUT ) :: Request

REAL, PARAMETER :: SIGDX   = 1.0         !Sig/dx of "perfect" field
REAL, PARAMETER :: SIGDXR2 = 1./SIGDX**2
INTEGER alloc_stat, i, irv
REAL    dx, dy, xfac, yfac, fac, facBest
REAL    xp, yp
LOGICAL lAGL

TYPE MetList
  INTEGER                    :: ifld
  TYPE( MetList  ), POINTER  :: Next
END TYPE MetList

TYPE( MetList ), POINTER :: FirstHoriz
TYPE( MetList ), POINTER :: FirstDomain
TYPE( MetList ), POINTER :: Fld, HorizFld

INTEGER, EXTERNAL :: SWIMcnvCoord
LOGICAL, EXTERNAL :: CheckHorizDomain, CheckVertDomain
INTEGER, EXTERNAL :: SWIMaddSmoothField

!------ Initialize failure

ifld = -1
error%Number  = IV_ERROR
error%Routine = 'SelectMetField'
error%Message = 'Error allocating list'

IF( numField < 1 )GOTO 9999


!------ First check if North/South pole field is appropriate

IF( Prj%coord == I_LATLON .AND. numField > 1 )THEN
  IF( Request%Y > POLARCAP_LAT )THEN
    DO i = 2,numField
      IF( BTEST(field(i)%type,FTB_NPOLE) )THEN
        ifld = i
        CALL SWIMclearError(); EXIT
      END IF
    END DO
  ELSE IF( Request%Y < -POLARCAP_LAT )THEN
    DO i = 2,numField
      IF( BTEST(field(i)%type,FTB_SPOLE) )THEN
        ifld = i
        CALL SWIMclearError(); EXIT
      END IF
    END DO
  END IF
END IF

NULLIFY( FirstHoriz,FirstDomain )

!------ Build linked-lists of fields containing request location

!------ First check if puff is within horizontal limits

IF( lSaveCoord )THEN
  IF( ifld > MAXFLD )THEN
    ifld = -1
    error%Number  = IV_ERROR
    error%Routine = 'SelectMetField'
    error%Message = 'Too many met fields'
    WRITE(error%Inform,'(A,I0)',IOSTAT=i) 'Max is ',MAXFLD
    GOTO 9999
  END IF
END IF

DO i = 1,numField

  IF( PrjCoord%type /= field(i)%grid%coord%type )THEN
    irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xp,yp,field(i)%grid%coord )
  ELSE
    xp = Request%X; yp = Request%Y
  END IF
  IF( lSaveCoord )THEN
    xFld(i) = xp; yFld(i) = yp
  END IF

  IF( CheckHorizDomain( field(i)%grid,xp,yp) )THEN

    IF( .NOT.ASSOCIATED(FirstHoriz) )THEN
      ALLOCATE( FirstHoriz,STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      Fld => FirstHoriz
    ELSE
      ALLOCATE( Fld%Next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      Fld => Fld%Next
    END IF

    Fld%ifld = i
    NULLIFY(Fld%Next)

  END IF

END DO

IF( ASSOCIATED(FirstHoriz) )THEN

!------ Check if puff is below top of domain (if within horizontal limits)

  lAGL     = BTEST(Request%type,RTB_AGL)
  HorizFld => FirstHoriz

  DO WHILE( ASSOCIATED(HorizFld) )

    i = HorizFld%ifld

    IF( lSaveCoord )THEN
      xp = xFld(i); yp = yFld(i)
    ELSE
      IF( PrjCoord%type /= field(i)%grid%coord%type )THEN
        irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xp,yp,field(i)%grid%coord )
      ELSE
        xp = Request%X; yp = Request%Y
      END IF
    END IF

    IF( CheckVertDomain( field(i)%grid,xp,yp,Request%Z,Request%SigZ,lAGL) )THEN

      IF( .NOT.ASSOCIATED(FirstDomain) )THEN
        ALLOCATE( FirstDomain,STAT=alloc_stat )
        IF( alloc_stat /= 0 )GOTO 9999
        Fld => FirstDomain
      ELSE
        ALLOCATE( Fld%Next,STAT=alloc_stat )
        IF( alloc_stat /= 0 )GOTO 9999
        Fld => Fld%Next
      END IF

      Fld%ifld = i
      NULLIFY(Fld%Next)

    END IF

    HorizFld => HorizFld%Next

  END DO

END IF

CALL SWIMclearError()

!------ Select field based on horizontal puff size (if within domain)

IF( .NOT.(ASSOCIATED(FirstDomain) .OR. ASSOCIATED(FirstHoriz)) )THEN

  IF( Request%iField > 0 .AND. Request%iField <= numField )THEN
    ifld = Request%iField  !Use previous puff field if not within any horizontal domain
  ELSE
    ifld = 1               !Default field if none associated with request
  END IF

ELSE !------ Move down appropriate list

  IF( ASSOCIATED(FirstDomain) )THEN
    Fld => FirstDomain
  ELSE
    Fld => FirstHoriz
  END IF

  ifld = -1

  DO WHILE( ASSOCIATED(Fld) )

    i = Fld%ifld

    IF( lSaveCoord )THEN
      xp = xFld(i); yp = yFld(i)
    ELSE
      IF( PrjCoord%type /= field(i)%grid%coord%type )THEN
        irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xp,yp,field(i)%grid%coord )
      ELSE
        xp = Request%X; yp = Request%Y
      END IF
    END IF

    CALL SWIMmapfac( field(i)%grid%coord,xp,yp,xfac,yfac )
    dx = field(i)%grid%dX/xfac; dy = field(i)%grid%dY/yfac

    IF( Request%Shh > 0. )THEN

      fac = LOG(2.*SIGDXR2*Request%Shh/(dx*dx+dy*dy))

      IF( ifld == -1 )THEN
        facBest = fac
        ifld = i
      ELSE
        CALL BestGridRes( i,fac,ifld,facBest )
      END IF

    ELSE

      fac = 0.5*(dx*dx+dy*dy)

      IF( ifld == -1 )THEN
        facBest = fac
        ifld = i
      ELSE IF( fac < facBest )THEN
        facBest = fac
        ifld = i
      END IF

    END IF

    Fld => Fld%Next

  END DO

!------ Check if smoothed field should be added (if requested)
!       Request if SIGDX*sig/dx > sqrt(2) (roughly speaking)

  IF( BTEST(Request%type,RTB_SMOOTH) .AND. Request%Shh > 0. )THEN
    IF( facBest > LOG2 )THEN
!$OMP CRITICAL (ADDSMTHFLD)
      irv = SWIMaddSmoothField( ifld )
      IF( irv == SWIMsuccess )THEN
        IF( ifld > 0 )THEN
          Request%type = IBSET(Request%type,RTB_ADDMET)
        ELSE
          ifld = -ifld
        END IF
        IF( lSaveCoord )THEN
          IF( ifld < MAXFLD )THEN
          IF( PrjCoord%type /= field(ifld)%grid%coord%type )THEN
            irv = SWIMcnvCoord( Request%X,Request%Y,PrjCoord,xp,yp,field(ifld)%grid%coord )
          ELSE
            xp = Request%X; yp = Request%Y
          END IF
          xFld(ifld) = xp; yFld(ifld) = yp
          ELSE
            lSaveCoord = .FALSE.
          END IF
        END IF
      END IF
!$OMP END CRITICAL (ADDSMTHFLD)
      IF( irv == SWIMfailure )GOTO  9999
    END IF
  END IF

!------ Cleanup lists

  fld => FirstHoriz
  DO WHILE( ASSOCIATED(fld) )
    FirstHoriz => fld
    fld => FirstHoriz%Next
    DEALLOCATE( FirstHoriz,STAT=alloc_stat )
  END DO
  NULLIFY( FirstHoriz )

  fld => FirstDomain
  DO WHILE( ASSOCIATED(fld) )
    FirstDomain => fld
    fld => FirstDomain%Next
    DEALLOCATE( FirstDomain,STAT=alloc_stat )
  END DO
  NULLIFY( FirstDomain )

END IF

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE LOGICAL FUNCTION CheckHorizDomain( grid,x,y )

USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL,            INTENT( IN ) :: x, y


CheckHorizDomain = x >= grid%Xmin .AND. x <= grid%Xmax .AND. &
                   y >= grid%Ymin .AND. y <= grid%Ymax

RETURN
END

!==============================================================================

RECURSIVE LOGICAL FUNCTION CheckVertDomain( grid,x,y,z,sz,lAGL )

USE SWIM_fi
USE SWIMgetTopog_fi

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL,            INTENT( IN ) :: x, y
REAL,            INTENT( IN ) :: z, sz
LOGICAL,         INTENT( IN ) :: lAGL

TYPE( Topog ) :: TerStr

REAL zp

TerStr = SWIMGetTopog( grid,x,y )

IF( lAGL )THEN
  zp = MAX(z,sz) + TerStr%H
ELSE
  IF( BTEST(grid%type,GTB_TERRAIN) )THEN
    zp = MAX(z,sz+TerStr%H+grid%Hmin-Prj%Hmin)
  ELSE
    zp = MAX(z,sz+TerStr%H)
  END IF
END IF

CheckVertDomain = (zp <= grid%Ztop)

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE BestGridRes( i,fac,iBest,facBest )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

!------ Select best resolution based on fac (ratio of puff size to grid res.)
!       N.B. Do not consider coarser smooth fields if finer one is acceptable

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: i
REAL,    INTENT( IN    ) :: fac
INTEGER, INTENT( INOUT ) :: iBest
REAL,    INTENT( INOUT ) :: facBest

IF( facBest < LOG2 )THEN
  IF( BTEST(field(i)%gridSource%type,GSB_SMOOTH) )THEN
    IF( field(i)%gridSource%unit == iBest )RETURN
  END IF
END IF

IF( ABS(fac) < ABS(facBest) )THEN
  iBest   = i
  facBest = fac
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SWIMrotMet2LL( lat,lon,Mean,coordI )

!------ Rotate wind vector in met coordinates to ENU coordinates
!       N.B. No rotation for lat/lon, UTM or Cartesian met grid
!
!       N.B. Not valid for latitudes > 89

USE SWIMpuff_fd
USE SWIMparam_fd
USE MapCoord_fd
USE coordinate_fd
USE constants_fd

IMPLICIT NONE

REAL,                INTENT( IN    ) :: lat, lon
TYPE( PuffMetMean ), INTENT( INOUT ) :: Mean
TYPE( MapCoord    ), INTENT( IN    ) :: coordI

REAL rot, s, c, ue, vn

REAL, DIMENSION(3,3) :: ar, at, ux, utem

REAL, EXTERNAL       :: sind, cosd

IF( ABS(lat) > 89. )GOTO 9999

!------ Set rotation based for "special" map types; only proceed if rotation is not very small

SELECT CASE( coordI%type )
  CASE( I_LAMBERT,I_POLAR,I_RPOLAR,I_ROTLL )

    IF( coordI%type == I_RPOLAR )THEN
      rot = SIND(lat)
      c   = 1. + rot**2
      s   = (c*coordI%sp0 + coordI%cp0**2*rot)*SIND(lon-coordI%Lon0)
      c   = c*COSD(lon-coordI%Lon0) + coordI%cp0*COSD(lat)
      rot = ATAN2(s,c)
    ELSE IF( coordI%type == I_ROTLL )THEN
      c   = coordI%cp0*COSD(lat) - coordI%sp0*SIND(lat)*COSD(lon) !N.B. lat,lon on rotated sphere
      s   = coordI%sp0*SIND(lon)
      rot = ATAN2(s,c)
    ELSE
      rot = (lon - coordI%Lon0)*PI180 * coordI%n
    END IF

    IF( ABS(rot) < 0.001 )GOTO 9999

    c  = COS(rot)
    s  = SIN(rot)

!------ Rotate horizontal velocity components

    ue =  c*Mean%U + s*Mean%V
    vn = -s*Mean%U + c*Mean%V

!------ Rotate velocity gradient tensor

    ar = RESHAPE( (/ c, s, 0.,   &
                    -s, c, 0.,   &
                     0.,0.,1. /),&
           (/ 3,3 /) )

    CALL trnsps( ar,at )

    ux = RESHAPE( (/ Mean%Ugrad%Ux, Mean%Ugrad%Uy, Mean%Ugrad%Uz,   &
                     Mean%Ugrad%Vx, Mean%Ugrad%Vy, Mean%Ugrad%Vz,   &
                     Mean%Ugrad%Wx, Mean%Ugrad%Wy, Mean%Ugrad%Wz /),&
           (/ 3,3 /) )

    CALL xmatmul( at,ux,utem )
    CALL xmatmul( utem,ar,ux )

!------ Copy back to puff met structure

    Mean%U = ue; Mean%V = vn

    Mean%Ugrad%Ux = ux(1,1); Mean%Ugrad%Uy = ux(2,1); Mean%Ugrad%Uz = ux(3,1)
    Mean%Ugrad%Vx = ux(1,2); Mean%Ugrad%Vy = ux(2,2); Mean%Ugrad%Vz = ux(3,2)
    Mean%Ugrad%Wx = ux(1,3); Mean%Ugrad%Wy = ux(2,3); Mean%Ugrad%Wz = ux(3,3)

END SELECT

9999 CONTINUE

RETURN
END
