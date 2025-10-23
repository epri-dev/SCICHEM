RECURSIVE SUBROUTINE get_met( xp,yp,zp,szz,zc,iAGL,inField,Shh,outField,dElev )

!------ Define met variables at location (xp,yp,zp)

USE SCIPresults_fd
USE SWIMparam_fd
USE SWIMpuff_fd
USE scipuff_fi
USE met_fi, except_self => get_met ! Don't use interface to current function

IMPLICIT NONE

REAL,    INTENT( IN ) :: xp, yp              !Horizontal location
REAL,    INTENT( IN ) :: zp                  !Puff height above Hmin if iAGL=0; AGL if iAGL=1
REAL,    INTENT( IN ) :: szz                 !Puff vertical variance
REAL,    INTENT( IN ) :: zc                  !Puff cap
INTEGER, INTENT( IN ) :: iAGL                !Height flag: 1 for AGL, 0 for relative to Hmin
INTEGER, OPTIONAL, INTENT( IN  ) :: inField  !Met field index (from previous call)
REAL,    OPTIONAL, INTENT( IN  ) :: Shh      !Puff spread scale (m**2)
INTEGER, OPTIONAL, INTENT( OUT ) :: outField !Met field index (from this call)
REAL,    OPTIONAL, INTENT( OUT ) :: dElev    !Change in terrain from different met fields

TYPE( PuffMetRequest ) :: Request
TYPE( PuffMet        ) :: Met

INTEGER irv
REAL    xpp

INTEGER, EXTERNAL :: SWIMcurrentMet
INTEGER, EXTERNAL :: AddMetGrid

!------ Build request structure

IF ( global_lon )THEN
  xpp = xp
  DO WHILE( xpp < MetGrid(1)%xmin )
    xpp = xpp + 360.
  END DO
  DO WHILE( xpp > MetGrid(1)%xmin + 360. )
    xpp = xpp - 360.
  END DO
  Request%X = xpp
ELSE
  Request%X = xp
END IF
Request%Y    = yp
Request%Z    = zp
Request%SigZ = SQRT(szz)
Request%Zcap = zc

!------ Set request type

Request%type = 0
IF( iAGL == 1 )Request%type = IBSET(Request%type,RTB_AGL)

IF( .NOT.PRESENT(outField) )Request%type = IBSET(Request%type,RTB_FLD)

!------ Set optional input for selecting met field

IF( PRESENT(inField) )THEN
  Request%iField = inField
ELSE
  Request%iField = -1
END IF

IF( PRESENT(Shh) )THEN
  Request%Shh = Shh
  IF( mgrd >= 0 )Request%type = IBSET(Request%type,RTB_SMOOTH)
ELSE
  Request%Shh = 0.
END IF

!------ Get current met

irv = SWIMcurrentMet( Request,Met )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMcurrentMet' )
  GOTO 9999
END IF

!------ Set met field index if requested

IF( PRESENT(outField) )outField = Met%iField

!------ Set change in elevation due to different met fields if requested

IF( PRESENT(dElev) )dElev = Met%dElev

!------ Reset met grids if one has been added

IF( BTEST(Request%type,RTB_ADDMET) )THEN
  irv = AddMetGrid( Met%iField )
  IF( irv /= SCIPsuccess )GOTO 9999
ELSE
  DO WHILE( .NOT.lMetGridComplete(Met%iField) )
!$OMP FLUSH( lMetGridComplete )
    CONTINUE
  END DO
END IF

!------ Move met into common

CALL SetSWIMmet( Met )

9999 CONTINUE

END

!===============================================================================

RECURSIVE SUBROUTINE SetSWIMmet( Met )

USE scipuff_fi
USE SWIMpuff_fd
USE met_fi
USE constants_fd

IMPLICIT NONE

TYPE( PuffMet), INTENT( IN ) :: Met

IF( BTEST(run_mode,REVERSE_MODE) )THEN

  ub = -Met%Mean%U
  vb = -Met%Mean%V
  wb = -Met%Mean%W

  dudx = -Met%Mean%Ugrad%Ux
  dudy = -Met%Mean%Ugrad%Uy
  dudz = -Met%Mean%Ugrad%Uz
  dvdx = -Met%Mean%Ugrad%Vx
  dvdy = -Met%Mean%Ugrad%Vy
  dvdz = -Met%Mean%Ugrad%Vz
  dwdx = -Met%Mean%Ugrad%Wx
  dwdy = -Met%Mean%Ugrad%Wy
  dwdz = -Met%Mean%Ugrad%Wz

ELSE

  ub = Met%Mean%U
  vb = Met%Mean%V
  wb = Met%Mean%W

  dudx = Met%Mean%Ugrad%Ux
  dudy = Met%Mean%Ugrad%Uy
  dudz = Met%Mean%Ugrad%Uz
  dvdx = Met%Mean%Ugrad%Vx
  dvdy = Met%Mean%Ugrad%Vy
  dvdz = Met%Mean%Ugrad%Vz
  dwdx = Met%Mean%Ugrad%Wx
  dwdy = Met%Mean%Ugrad%Wy
  dwdz = Met%Mean%Ugrad%Wz

END IF

su2 = Met%Mean%dU2

thb  = Met%Mean%Tpot
tb   = Met%Mean%T

hb = Met%Mean%Humid
pb = Met%Mean%Press
cw     = Met%Mean%Qcloud
cldtop = Met%Mean%cldTop
cldbot = Met%Mean%cldBot
cw_avg = Met%Mean%cldAvg

zinv = Met%BL%MixingHt + Met%BL%TerElev

zruf    = Met%BL%zruf
hc      = Met%BL%CanopyHt
alphac  = Met%BL%Alpha
ustdep  = Met%BL%UstarDep
us2     = Met%BL%Ustar2
ws2     = Met%BL%Wstar2
wts     = Met%BL%HeatFlux
prbl    = FLOAT(Met%BL%PrecipType)
sun_fac = Met%BL%sunfac
prate   = Met%BL%prate
cldcvr  = Met%BL%cc
LAI     = Met%BL%LAI

wwz  = Met%Turb%WWgrad

uubl = Met%Turb%UUshear
vvbl = Met%Turb%UUbuoy
wwbl = Met%Turb%WW
wtbl = Met%Turb%WT
qqs  = Met%Turb%QQshear

sbz = Met%Turb%LscaleShear
sbl = Met%Turb%LscaleBuoy

difb = Met%Turb%Diff
dddz = Met%Turb%DiffGrad

dddx = Met%Turb%DiffGradX
dddy = Met%Turb%DiffGradY

xml = Met%BL%L
zsl = Met%BL%Zsl

IF( wts > 0. )THEN
  dtdzs = 0.
ELSE
  dtdzs = 0.001
END IF

wwbh = wwbl
sbls = sbz

uub = Met%LSV%UU
vvb = Met%LSV%VV
uvb = Met%LSV%UV
sby = Met%LSV%Lscale

sb_lsv = sby

uubz = Met%LSV%UUz
vvbz = Met%LSV%VVz
uvbz = Met%LSV%UVz

TerElev = Met%BL%TerElev

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetSkewTurb()

USE param_fd
USE constants_fd
USE met_fi
USE step_p_fi

REAL, PARAMETER :: SKEW_MIN = 0.1

REAL m, r, m2, m2p1, lambda1, lambda2, zagl, f33, g33, fac

wb_skew = 0.0

IF( iSkew == SKEW_NONE .OR. .NOT.lzinv )GOTO 9999

!----- Set skew turbulence parameters for convective conditions

skewness = 0.6*(ws2/(ws2+4.*us2))**1.5

IF( skewness < SKEW_MIN )THEN

  iSkew = SKEW_NONE
  GOTO 9999

ELSE

  zagl = MAX(zsav-hp,sz)
  IF( zagl < ABS(xml) )THEN
    fac = zagl/ABS(xml)
    zagl = zagl/(zinv-hp)
    f33 = us2*1.5*(1.0-zagl)
    g33 = ws2*1.1*(1.05-zagl)*zagl**0.666667
    skewness = skewness*fac + (1.-fac)*0.6*(g33/(g33+f33))**1.5
  END IF

  m = TwoThirds * skewness**0.3333333

  m2 = m*m

  m2p1 = 1.0 + m2

  r = m2p1**3 * skewness**2 / (m2 * (3.0+m2)**2)

  lambda2 = 0.5*(1.0 - SQRT(r/(4.0+r)))
  lambda1 = 1.0 - lambda2

  IF( iSkew == SKEW_DOWN )THEN

    ww_skew = wwbl * lambda2 / (lambda1*m2p1)
    wb_skew = -m*SQRT(ww_skew)

  ELSE

    ww_skew = wwbl * lambda1 / (lambda2*m2p1)
    wb_skew = m*SQRT(ww_skew)

  END IF

  difb = difb * ww_skew/wwbl
  dddz = dddz * ww_skew/wwbl
  wwbl = ww_skew
  wwbh = ww_skew

END IF

9999 CONTINUE

RETURN
END
