INTEGER FUNCTION SWIMcurrentSrfMet( x,y,Met )

!------ Get met parameters at (or near) the surface

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMcurrentSrfMet

USE SWIMpuff_fd
USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi
USE constants_fd

IMPLICIT NONE

REAL,            INTENT( IN  ) :: x, y
TYPE( PuffMet ), INTENT( OUT ) :: Met

TYPE( PuffMetRequest ) :: Request

INTEGER ifld, i
REAL    L, tref, zp, pr, dum, tdum, thstar
REAL    pr1, pr2, hsx

TYPE( MetField ), POINTER :: fld

INTEGER, EXTERNAL :: SetMetField
REAL,    EXTERNAL :: UstarDep, tlog, LfromInvL
REAL,    EXTERNAL :: StndHumid

SWIMcurrentSrfMet = SWIMfailure

!------ Build request

Request%X = x; Request%Y = y; Request%Z = 0.

Request%SigZ = 0.; Request%Zcap = 0.; Request%Shh = 0.

Request%type   = IBSET(0,RTB_AGL)
Request%iField = 0 !DSH - may be associated with a size, but find 'best' now

!------ Set met field index

lSaveCoord = .TRUE.

ifld = SetMetField( Request )
IF( ifld < 1 )GOTO 9999

!------ Save field id

Met%iField = ifld

fld => Field(ifld)

Request%X = xFld(ifld); Request%Y = yFld(ifld)

!------ Find closest grid location (don't do full interpolation)

CALL SetXYfac( Request,fld%grid,mx,my,mxu,myv )

IF( mx%rat > 0.5 )mx%i = mx%i + 1
IF( my%rat > 0.5 )my%i = my%i + 1

i = (my%i-1)*fld%grid%nX + mx%i

!------ Set boundary layer variables

Met%BL%MixingHt = fld%BL%zi(i)
Met%BL%Zsl      = fld%BLaux%Zsl(i)
Met%BL%Ustar2   = fld%BLaux%ustr2(i)
Met%BL%Wstar2   = fld%BLaux%wstr2(i)
Met%BL%HeatFlux = fld%BL%HeatFlux(i)
Met%BL%zruf     = fld%grid%landcover%roughness(i)

Met%BL%UstarDep = UstarDep( Met%BL )

IF( Met%BL%UstarDep == 0.0 )THEN
  thstar = 0.
  L      = 1.E4
ELSE
  thstar = -Met%BL%HeatFlux / Met%BL%UstarDep
  thstar = MIN(thstar,0.09)  !Venkatram limit
  L      = LfromInvL( fld%BL%invMOL(i) )
END IF

! DSH - proposed effective reduction in evaporation due to canopy
!IF( fld%grid%landcover%canopyHt(i) > 0.0 )THEN
!  Met%BL%UstarDep = Met%BL%UstarDep * EXP(-fld%grid%landcover%alpha(i))
!END IF

!------ Set mean velocity to surface layer values

Met%Mean%U = fld%BLaux%usl(i)
Met%Mean%V = fld%BLaux%vsl(i)

!------ Find surface temperature using surface layer profile

IF( BTEST(fld%type,FTB_T) )THEN
  tref = fld%BLaux%tbl(i)
ELSE IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
  zp = fld%grid%terrain%H(i)+fld%grid%Hmin
  CALL stnd_atmos( zp,pr,tref,dum,SA_TA )
ELSE
  tref = TSURF !Stnd. Atmos
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  pr = fld%BLaux%pbl(i)/PSURF
ELSE
  zp = fld%grid%terrain%H(i)+fld%grid%Hmin
  CALL stnd_atmos( zp,pr,tdum,dum,SA_TA )
END IF

thstar = thstar * pr**KAPPA !Convert to actual temperature

Met%Mean%T = tref - thstar/VONK*tlog( fld%BLaux%zsl(i),Met%BL%zruf,L )

IF( BTEST(fld%type,FTB_P) )THEN
  zp = fld%grid%terrain%H(i) + Met%BL%Zsl + Prj%Hmin
  CALL stnd_atmos( zp,pr1,tdum,dum,SA_TA )
  zp = fld%grid%terrain%H(i) + Prj%Hmin
  CALL stnd_atmos( zp,pr2,tdum,dum,SA_TA )
  pr = pr * (pr2/pr1)
END IF

Met%Mean%Press = pr * PSURF

IF( BTEST(fld%type,FTB_H) )THEN
  CALL HumidMixRatio( fld%Field%Humid(i),Met%Mean%T,Met%Mean%Press,hsx)
  Met%Mean%Humid = MAX(hsx,HSTRAT)
ELSE
  Met%Mean%Humid = StndHumid( Met%Mean%Press,Met%Mean%T )
END IF

SWIMcurrentSrfMet = SWIMresult

9999 CONTINUE

lSaveCoord = .FALSE.

RETURN
END
