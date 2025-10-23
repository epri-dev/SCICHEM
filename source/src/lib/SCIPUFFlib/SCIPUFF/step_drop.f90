RECURSIVE SUBROUTINE step_drop( p,ipuf,pt,pd,drop,matlIn,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( INOUT ) :: p
INTEGER,               INTENT( IN    ) :: ipuf
TYPE( puff_liquid ),   INTENT( INOUT ) :: drop
TYPE( puff_material ), INTENT( IN    ) :: matlIn
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
TYPE( puff_totalcc ),  INTENT( INOUT ) :: pt
REAL,                  INTENT( IN    ) :: dt

REAL,    PARAMETER :: PR    = 0.71
REAL,    PARAMETER :: TEPS  = 1.0E-4    !Convergence test
REAL,    PARAMETER :: DELTA = 2.16E-7
REAL,    PARAMETER :: THIRD = 1.0/3.0
INTEGER, PARAMETER :: MAXI  = 10

TYPE( puff_material )   matp
TYPE( liquid_material ) matl

REAL tamb, tinf, KA
REAL mass, xmass, nu, rfac, ddrop, difd, dmass, hvd
REAL rhoa, ret, sc, sh, cs, cvhat, clhat, cc_save
REAL dpower, onemdp, pamb, psat, rlqd, plqd, psatprime
REAL alpha, gama, pdenom, denom
REAL termf1, termf2, termd1, termd2
REAL xxx, xp1, ps, deltax, fac_iter, fac_iter_m1
REAL gasctl, gascta, dift, dfunct, lambda, radius
REAL dtx, rhov, tdrop, tboil, mv, rhocpm, tvap, dlimit, delv
REAL efac, r

INTEGER iter, iloop
LOGICAL checkT

REAL, EXTERNAL :: sherwd, kincof, fun_rhoa

matl = TRANSFER(matlIn,matl)

!----- Save droplet mass before evaporation for interaction calculation

drop%ccs = p%c

!----- Ambient agent liquid/vapor concentration

cvhat = MAX(0.0,(MAX(pt%cctb,pt%cct)-MAX(p%cc,p%ccb))/p%c)
clhat = MAX(0.0,p%cc/p%c)

!----- Ambient temperature

IF( dynamic )THEN
  tamb = tb + tdyn
ELSE
  tamb = tb
END IF

tboil = matl%b/(matl%a - LOG10(pb*0.7500616)) - matl%c - ABSZERO

tinf = MIN(tamb,tboil)

KA = 0.023815 + 7.1E-5*(tamb+ABSZERO) ! Air thermal conductivity (W/m-K)

!----- Calculate drop Reynolds number

ddrop  = drop%d
radius = 0.5*ddrop

rlqd   = 8.31436E3/matl%w   ! gas constant for liquid
gasctl = rlqd*tamb

rhoa   = fun_rhoa( tamb,pb )
lambda = 3.101E-10*tamb**2/((pb/PSURF)*(tamb+110.4)) !---uses temp-dependent air viscosity
ret    = rhoa*vfall*ddrop*aspect/rmuair
nu     = sherwd( ret,PR )

gascta = RGAS*tamb
dift   = KA/(rhoa*CP)   !Thermal diffusivity (m**2/s)
nu     = nu*kincof( radius,gascta,dift,0.7,DELTA )

!----- Calculate agent properties

dpower = 0.94     ! temperature exponent of liquid minus 1.0
onemdp = 1.0 - dpower
pamb   = 100.*pb  !1.0E5*pb  *** units of kPa?
CALL lqd( matl,tinf,rhod,cs,hvd,difd )

sc = rmuair/(rhoa*difd)
sh = sherwd( ret,sc )

!----- Adjust the diffusivity into air for ambient pressure

difd = difd * PSURF/pb

psat = MIN(0.9999*pamb,cs*tinf*rlqd)

!-----  Ambient agent vapor pressure

plqd = MIN(psat,cvhat*tamb*rlqd)

!----- Calculate agent vapor pressures

psatprime = MIN(0.9999*pamb,psat*EXP(2.0*matl%st/(gasctl*radius*rhod)))

pdenom = pamb - psatprime   !0.5*(plqd+psatprime)
dfunct = difd*pamb/pdenom
sh     = sh*kincof( radius,gasctl,dfunct,0.036,1.3*lambda )

alpha = hvd/gasctl                      !  L/RT
gama  = alpha*pamb*difd*sh/(KA*nu*tinf) !  PLD/KRT**2

termf2 = gama/pdenom
denom  = 1.0 + termf2*alpha*psatprime*(pamb-plqd)/pdenom

!----- Calculate initial scaled temperature departure, xxx

xxx = termf2*(plqd-psatprime)/denom

!----- Iteration loop

iter = 0

IterationLoop: DO

  iter = iter + 1

  xp1  = xxx + 1.0

  ps     = MIN(0.9999*pamb,psatprime*EXP(alpha*xxx/xp1))  !  effective surface pressure
  pdenom = pamb - ps   !0.5*(plqd+ps)

  termf1 = (xp1**onemdp-1.0)/onemdp      !  1st term of function
  termd1 = xp1**(-dpower)                !  1st term of deriv
  termf2 = gama/pdenom
  termd2 = termf2*alpha/pdenom*(pamb-plqd)*ps/(xp1*xp1) ! 2nd term of deriv
  termf2 = termf2*(plqd-ps)                             ! 2nd term of function

!----- Calculate increment to xxx

  deltax = (termf2-termf1)/(termd1+termd2)
  xxx    = xxx + deltax

!----- Check for convergence

  IF( ABS(deltax) <= TEPS .OR. iter >= MAXI )EXIT

END DO IterationLoop

xp1    = xxx + 1.0
drop%t = xp1*tinf                      ! surface temperature of the drop
ps     = psatprime*EXP(alpha*xxx/xp1)  ! effective saturated pressure at the surface

!----- Calculate mass increment of water component

IF( ABS(xxx) > 1.0E-4 )THEN
  denom  = xp1**onemdp - 1.0
  termf1 = onemdp*xxx/denom
ELSE
  termf1 = 1.0
END IF
termd1 = pamb*difd*sh/gasctl*(ps-plqd)/(pamb-ps)     !0.5*(plqd+ps))
dmass  = PI*ddrop*termf1*termd1*dt

!----- Calculate liquid drop mass

rfac   = (PI/6.)*rhod
mass   = rfac*(ddrop**3)
dlimit = MIN(dmass,mass)

!----- Limit based on final saturation and number density

cs    = cs*ps*tinf/(psatprime*drop%t)
dmass = (cs-cvhat)*mass/clhat
IF( dmass > dlimit )THEN
  dmass    = dlimit
  fac_iter = 0.75
ELSE
  fac_iter = 0.95
END IF

fac_iter_m1 = 1.0 - fac_iter

checkT = .TRUE.

IF( dmass <= 0.0 )THEN
  drop%tevap = 0.0
  GOTO 9999
END IF

IF( dynamic )THEN

  rhov   = (matl%w/MWAIR)*fun_rhoa( drop%t,pb )
  mv     = cs/rhov
  delv   = clhat*dmass/mass
  rhocpm = (mv*matl%cpv + (1.0-mv)*CP*MWAIR/matl%w)*(rhov+delv)
  tvap   = tb + tdyn - delv*(matl%cpv*(tamb - drop%t)+ hvd)/rhocpm

  IF( tvap < drop%t .AND. checkT )THEN
    iter = 0
    DO
      iter  = iter + 1
      tdrop = drop%t
      iloop = 0
      CALL lqd( matl,drop%t,rhod,cs,hvd,difd )
      DO
        iloop = iloop + 1
        drop%t = fac_iter*tdrop + fac_iter_m1*MAX(0.,tvap)
        CALL lqd( matl,drop%t,rhod,cs,hvd,difd )
        IF( cs > cvhat )EXIT
        tvap = drop%t
        IF( iloop > 20 )EXIT
      END DO
      dmass = (cs-cvhat)*mass/clhat
      IF( dmass > dlimit )THEN
        dmass    = dlimit
        fac_iter = 0.75
      ELSE
        fac_iter = 0.95
      END IF
      fac_iter_m1 = 1.0 - fac_iter
      rhov   = (matl%w/MWAIR)*fun_rhoa( drop%t,pb )
      mv     = cs/rhov
      delv   = clhat*dmass/mass
      rhocpm = (mv*matl%cpv + (1.0-mv)*CP*MWAIR/matl%w)*(rhov+delv)
      tvap   = tb + tdyn - delv*(matl%cpv*(tamb - drop%t)+ hvd)/rhocpm
      IF( ABS(tvap-drop%t)/tamb < 0.01 )EXIT
      IF( iter > 20 )GOTO 9999
    END DO

  END IF

  tdrop = (matl%cpv*(drop%t - tamb) - hvd)/rhocpm

ELSE

  rhov  = (matl%w/MWAIR)*fun_rhoa( drop%t,pb )
  tdrop = 0.0

END IF

!-----  Generate gas phase puff

xmass = mass - dmass
IF( xmass/rfac <= 2.*TINY(mass) )THEN
  drop%d = 0.
  xmass  = 0.
ELSE
  drop%d = (xmass/rfac)**THIRD
END IF

rfac = xmass/mass

!----- Expand puff volume to account for vapor formation

IF( rfac < 1.0 )THEN
  efac  = 1.0 + (1.0-rfac)*clhat/rhov
  r     = efac**TwoThirds
  p%sxx = r*p%sxx
  p%syy = r*p%syy
  p%szz = r*p%szz
  p%cc  = p%cc/efac
  drop%tevap = (1.0-rfac)*p%c*tdrop
ELSE
  drop%tevap = 0.0
END IF

CALL evap_puff( p,ipuf,pt,pd,rfac,vfall,dt,tdrop,clhat )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Update puff mass and mean square concentration

cc_save = p%cc

CALL scale_psum( p,rfac )

p%cc   = cc_save            !Reset <cc>, since it is updated fully in inter
pt%cct = rfac*pt%cct

IF( dynamic )THEN
  pd%wcp = rfac*pd%wcp
  pd%wcb = rfac*pd%wcb
  pd%ucp = rfac*pd%ucp
  pd%ucb = rfac*pd%ucb
  pd%vcp = rfac*pd%vcp
  pd%vcb = rfac*pd%vcb
  pd%ctp = rfac*pd%ctp
  pd%ctb = rfac*pd%ctb
  IF( buoy_gas )THEN
    pd%bcp = rfac*pd%bcp
    pd%bcb = rfac*pd%bcb
  END IF
END IF

!-----  Check for change of size bin

DO WHILE( drop%d < matl%dmin )
  p%ityp = p%ityp - 1
  CALL get_puff_material( p%ityp,matp )
  matl = TRANSFER(matp,matl)
END DO

!********** Modify time step limit ************* RIS 10/4/96***********
!    Final evaporation can be extremely rapid, but dynamic step limits
!    ensure puff dose evolves significantly. Hence, overestimate of
!    evaporation time is not significant - don't need to track progress
!    through individual size bin
!**********************************************************************

!-----  Set droplet time step limit

dtx = MAX(0.5*(matl%dmax-matl%dmin),0.1*ddrop)
dtx = dt*dtx/MAX(ddrop-drop%d,0.1*dtx)
dtx = MAX(dtx,0.1*delt)
dts = MIN(dts,dtx)

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

RECURSIVE SUBROUTINE lqd( m,tkdrop,rhod,cs,hv,dif )

USE scipuff_fi

IMPLICIT NONE

TYPE( liquid_material ), INTENT( IN  ) :: m
REAL,                    INTENT( IN  ) :: tkdrop
REAL,                    INTENT( OUT ) :: cs, dif, hv, rhod

REAL, PARAMETER :: DIFFAC = 3.1038
REAL, PARAMETER :: WA     = 0.03448
REAL, PARAMETER :: HVFAC  = 1.9148e4

REAL difa, tc

!-----  Convert droplet temperature from Kelvin to Celsius

tc = tkdrop + ABSZERO

!-----  Calculate liquid properties as a function of temperature
!-----  Units are MKS
!-----  In cgs units 1.6e-5 = 1.0e+6/760/R

rhod = MAX(m%rho - m%rhob*tc,1.0E-6)
tc   = tc + m%c

IF( tc < 1.0E-6 )THEN
  cs  = 0.0
  hv  = 0.0
  dif = 0.0
ELSE
  cs = MIN(20.,MAX(m%a-m%b/tc,-20.0))
  cs = 1.6E-2*m%w*(10.**cs)/tkdrop !kg/m3
  hv   = HVFAC*m%b*(tkdrop/tc)**2/m%w
  difa = 4.3E-7*SQRT(tkdrop*(WA+1./m%w))*tkdrop
  dif  = difa/(DIFFAC+(1000.*m%w/rhod)**0.3333)**2
END IF

RETURN
END

!==============================================================================

REAL FUNCTION sherwd( re,sc )

IMPLICIT NONE

REAL, INTENT( IN ) :: re, sc

!  Sherwood number for falling droplet. Original NUSSE model
!          sherwd = 2. + 0.552*SQRT(re)*sc**0.33333333
!  Modified 03/21/00 to use
!     Beard & Pruppacher, (1971), JAS, 28, 1455-1464

REAL xxx

xxx = SQRT(re)*sc**0.33333333

IF( xxx < 1.4259 )THEN
  sherwd = 2.000000 + 0.216*xxx**2
ELSE
  sherwd = 1.560815 + 0.616*xxx
END IF

RETURN
END

!-----------------------------------------------------------------------

RECURSIVE SUBROUTINE evap_puff( p,ipuf,pt,pd,rfac,vfall,dt,tdrop,clhat )

USE scipuff_fi
USE met_fi
USE step_p_fi, ONLY:lblcap, hp, myTId, puffSplit, puffTemp, npufTemp, nDimTemp, &
                    useTemp

!  Creates a new gas-phase puff for evaporating drop

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN ) :: p
INTEGER,               INTENT( IN ) :: ipuf
TYPE( puff_dynamics ), INTENT( IN ) :: pd
TYPE( puff_totalcc  ), INTENT( IN ) :: pt
REAL,                  INTENT( IN ) :: rfac, vfall, dt
REAL,                  INTENT( IN ) :: tdrop, clhat

TYPE( puff_dynamics )pdn
TYPE( puff_totalcc  )ptn

REAL    frac, delz
INTEGER naux, jtyp, ios, next

TYPE( puff_str ), DIMENSION(:), POINTER :: ptem

INTEGER, EXTERNAL :: allocatePuffAux
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: next_puff

frac = 1.0 - rfac

jtyp = p%ityp - typeID(p%ityp)%igrp + 1
naux = typeID(jtyp)%npaux

IF( useTemp )THEN
  IF( puffSplit(ipuf,2) == 0 )puffSplit(ipuf,3) = npufTemp(myTId)
  puffSplit(ipuf,2) = puffSplit(ipuf,2) + 1
  npufTemp(myTId)   = npufTemp(myTId)   + 1
  IF( npufTemp(myTId) > nDimTemp(myTId) )THEN
   CALL ExpandPuffTemp( myTId )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  ptem => puffTemp(myTId)%p
  next =  npufTemp(myTId)
  NULLIFY( ptem(next)%aux )

  CALL copy_puff_noaux( p,ptem(next) )

  ptem(next)%naux = naux
  ios = allocatePuffAux( ptem(next) )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'evap_puff'
    eMessage = 'Error allocating scipuff puff auxiliary array'
    WRITE(eInform,'(A,I0,A,I0)')'Request=',ptem(next)%naux,'  : Error = ',ios
    GOTO 9999
  END IF

  delz = 0.5*vfall*dt

  ptem(next)%zbar = p%zbar + delz
  ptem(next)%szz  = p%szz  + delz*delz

  CALL scale_psum( ptem(next),frac )

  ptem(next)%cc  = frac*(pt%cct-p%cc)

  ptem(next)%ityp = jtyp
  ptem(next)%inxt = 0
  ptem(next)%iprv = 0
  ptem(next)%idtl = p%idtl
  ptem(next)%idtn = 0

!----- Reset zcap

  IF( lblcap .AND. ptem(next)%zbar <= zinv )THEN

    IF( ptem(next)%zbar + 3.0*SQRT(ptem(next)%szz) < zinv )THEN
      ptem(next)%zc = zinv
    ELSE
      IF( ptem(next)%zc > 0. .AND. ptem(next)%zc < zinv + MAX(0.3*(zinv-hp),dzg) )THEN
        ptem(next)%zc = MAX(ptem(next)%zc,zinv)
      END IF
    END IF

  ELSE

    IF( ptem(next)%zc > 0. )THEN
      ptem(next)%zc = MAX(ptem(next)%zc,ptem(next)%zbar+0.01)
    END IF

  END IF

ELSE

CALL check_newpuff()
IF( nError /= NO_ERROR )THEN
  eRoutine = 'evap_puff'
  GOTO 9999
END IF

next = next_puff()
CALL copy_puff_noaux( p,puff(next) )

puff(next)%naux = naux
ios = allocatePuffAux( puff(next) )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'evap_puff'
  eMessage = 'Error allocating scipuff puff auxiliary array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',puff(next)%naux,'  : Error = ',ios
  GOTO 9999
END IF

delz = 0.5*vfall*dt

puff(next)%zbar = p%zbar + delz
puff(next)%szz  = p%szz  + delz*delz

CALL scale_psum( puff(next),frac )

puff(next)%cc  = frac*(pt%cct-p%cc)

puff(next)%ityp = jtyp
puff(next)%inxt = 0
puff(next)%iprv = 0
puff(next)%idtl = p%idtl
puff(next)%idtn = 0

!----- Reset zcap

IF( lblcap .AND. puff(next)%zbar <= zinv )THEN

  IF( puff(next)%zbar + 3.0*SQRT(puff(next)%szz) < zinv )THEN
    puff(next)%zc = zinv
  ELSE
    IF( puff(next)%zc > 0. .AND. puff(next)%zc < zinv + MAX(0.3*(zinv-hp),dzg) )THEN
      puff(next)%zc = MAX(puff(next)%zc,zinv)
    END IF
  END IF

ELSE

  IF( puff(next)%zc > 0. )THEN
    puff(next)%zc = MAX(puff(next)%zc,puff(next)%zbar+0.01)
  END IF

END IF

END IF

!----- Auxiliary arrays

ptn%cct  = frac*pt%cct
ptn%cctb = frac*pt%cctb
IF( useTemp )THEN
  CALL put_totalcc( ptem(next),ptn )
ELSE
CALL put_totalcc( puff(next),ptn )
END IF

IF( dynamic )THEN

  pdn%w   = 0.0
  IF( useTemp )THEN
    pdn%t   = tdrop*ptem(next)%c
  ELSE
  pdn%t   = tdrop*puff(next)%c
  END IF
  pdn%wcp = frac*pd%wcp
  pdn%wcb = frac*pd%wcb
  pdn%ctp = frac*pd%ctp
  pdn%ctb = frac*pd%ctb + frac*clhat*pdn%t

  pdn%un  = 0.0
  pdn%vn  = 0.0
  pdn%ucp = frac*pd%ucp
  pdn%ucb = frac*pd%ucb
  pdn%vcp = frac*pd%vcp
  pdn%vcb = frac*pd%vcb

  IF( buoy_gas )THEN
    pdn%bcp = frac*pd%bcp
    pdn%bcb = frac*pd%bcb
  END IF

  IF( dense_gas )THEN
    pdn%u    = pd%u
    pdn%v    = pd%v
    pdn%dudx = pd%dudx
    pdn%dudy = pd%dudy
    pdn%dvdx = pd%dvdx
    pdn%dvdy = pd%dvdy
    pdn%u0   = 0.0
    pdn%X    = 0.0
    pdn%Y    = 0.0
    pdn%sn   = 0.0
    pdn%cs   = 0.0
  END IF

  IF( useTemp )THEN
    CALL put_dynamics( ptem(next),pdn )
  ELSE
  CALL put_dynamics( puff(next),pdn )
  END IF

END IF

IF( IsMulti(typeID(jtyp)%icls) )THEN
  IF( useTemp )THEN
    CALL InitMCvapor( ptem(next) )
  ELSE
  CALL InitMCvapor( puff(next) )
  END IF
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE drop_deform( rhod,rhoa,d,st,aspect )

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( IN  ) :: rhod          !Liquid density  (kg/m3)
REAL, INTENT( IN  ) :: rhoa          !Air density     (kg/m3)
REAL, INTENT( IN  ) :: d             !Drop diameter   (m)
REAL, INTENT( IN  ) :: st            !Surface tension (N/m)
REAL, INTENT( OUT ) :: aspect        !Aspect ratio

REAL eo, tem

!----- Calculate liquid drop aspect ratio

IF( st <= 0.0 )THEN

  aspect = 1.0

ELSE

  eo = g*(rhod-rhoa)*d*d/st

  IF( eo <= 0.4 )THEN
    aspect = 1.
  ELSE
    tem    = 1.+0.7*((eo-0.4)**0.8)
    aspect = tem**0.33333333
  END IF

END IF

RETURN
END

!==============================================================================

SUBROUTINE step_wetpart( drop,pmatlIn,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_liquid ),   INTENT( INOUT ) :: drop
TYPE( puff_material ), INTENT( IN )    :: pmatlIn
REAL,                  INTENT( IN )    :: dt

REAL,    PARAMETER :: PR    = 0.71
REAL,    PARAMETER :: DELTA = 2.16E-7
REAL,    PARAMETER :: TEPS  = 1.0E-4    !Convergence test
REAL,    PARAMETER :: THIRD = 1.0/3.0
INTEGER, PARAMETER :: MAXI  = 10

REAL tamb, KA
REAL mass, nu, rfac, ddrop, difd, dmass, hvd
REAL rhoa, ret, sc, sh
REAL apower, dpower, onemdp, pamb, psat, rlqd, plqd, psatprime
REAL alpha, gama, pdenom, denom
REAL termf1, termf2, termd1, termd2
REAL xxx, xp1, ps, deltax
REAL gasctl, gascta, dift, dfunct, lambda, radius
REAL wvfrac, wvlim, mwl, stl

INTEGER iter, ID
TYPE( part_material ) pmatl

REAL, EXTERNAL :: sherwd, kincof, fun_rhoa

pmatl = TRANSFER(pmatlIn,pmatl)

!----- Set up liquid parameters

ID = material(imat)%AuxMatID
IF( ID /= NOT_SET_I )THEN
  mwl = WetPartLiquidMat(ID)%w
  stl = WetPartLiquidMat(ID)%st
ELSE
  mwl = MW_WATER
  stl = ST_WATER
END IF

!----- Set ambient temp

tamb = tb

KA = 0.023815 + 7.1E-5*(tamb+ABSZERO) ! Air thermal conductivity (W/m-K)

!-----  Calculate drop Reynolds number

ddrop   = drop%d
wvfrac  = 1.0 - (pmatl%dbar/ddrop)**3
radius  = 0.5*ddrop

rlqd    = 8.31436E3/mwl    ! gas constant for liquid
gasctl  = rlqd*tamb

rhoa   = fun_rhoa( tb,pb )
lambda = 3.101E-10*tamb**2/((pb/PSURF)*(tamb+110.4)) !---uses temp-dependent air viscosity

ret  = rhoa*vfall*ddrop*aspect/rmuair
nu   = sherwd( ret,PR )

gascta = RGAS*tamb
dift   = KA/(rhoa*CP)   !Thermal diffusivity (m**2/s)
nu     = nu*kincof( radius,gascta,dift,0.7,DELTA )

!----- Calculate agent properties

apower = 1.0      ! activity reduction exponent (due to water dilution)
dpower = 0.94     ! temperature exponent of water minus 1.0
onemdp = 1.0 - dpower
pamb   = 100.*pb  !1.0E5*pb
IF( ID > 0 )THEN
  CALL lqd( WetPartLiquidMat(ID),tamb,rhod,psat,hvd,difd )
  difd = difd * PSURF/pb
  psat = psat*tamb*rlqd
ELSE
  CALL set_water_props( tamb,rhod,pb/PSURF,psat,hvd,difd )
END IF

sc = rmuair/(rhoa*difd)
sh = sherwd( ret,sc )

!-----  Ambient water vapor pressure

IF( ID > 0 )THEN
  plqd = 0.0
ELSE
  plqd = MIN(psat,hb*rhoa*gasctl)
END IF

!----- Calculate water vapor pressures

psatprime = psat*(wvfrac**apower) ! reduced effective saturated pressure in impure drop
psatprime = psatprime*EXP(2.0*stl/(gasctl*radius*rhod))

pdenom = pamb - 0.5*(plqd+psatprime)
dfunct = difd*pamb/pdenom
sh     = sh*kincof( radius,gasctl,dfunct,0.036,1.3*lambda )

alpha = hvd/gasctl                      !  L/RT
gama  = alpha*pamb*difd*sh/(KA*nu*tamb) !  PLD/KRT**2

termf2 = gama/pdenom
denom  = 1.0 + termf2*alpha*psatprime*(pamb-plqd)/pdenom

!----- Calculate initial scaled temperature departure, xxx

xxx = termf2*(plqd-psatprime)/denom

!----- Iteration loop

iter = 0

IterationLoop: DO

  iter = iter + 1

  xp1   = xxx + 1.0

  ps     = psatprime*EXP(alpha*xxx/xp1)  !  effective surface pressure
  pdenom = pamb - 0.5*(plqd+ps)

  termf1 = (xp1**onemdp-1.0)/onemdp      !  1st term of function
  termd1 = xp1**(-dpower)                !  1st term of deriv
  termf2 = gama/pdenom
  termd2 = termf2*alpha/pdenom*(pamb-plqd)*ps/(xp1*xp1) ! 2nd term of deriv
  termf2 = termf2*(plqd-ps)                             ! 2nd term of function

!----- Calculate increment to xxx

  deltax = (termf2-termf1)/(termd1+termd2)
  xxx    = xxx + deltax

!----- Check for convergence

  IF( ABS(deltax) <= TEPS .OR. iter >= MAXI )EXIT

END DO IterationLoop

xp1    = xxx + 1.0
drop%t = xp1*tamb      ! surface temperature of the drop
ps     = psatprime*EXP(alpha*xxx/xp1)  ! effective saturated pressure at the surface

drop%tevap = 0.0

!----- Calculate mass increment of water component

IF( ABS(xxx) > 1.0E-4 )THEN
  denom  = xp1**onemdp - 1.0
  termf1 = onemdp*xxx/denom
ELSE
  termf1 = 1.0
END IF
termd1 = pamb*difd*sh/gasctl*(ps-plqd)/(pamb-0.5*(plqd+ps))
dmass  = PI*ddrop*termf1*termd1*dt

!----- Calculate water drop mass

rfac = (PI/6.)*rhod
mass = rfac*(ddrop**3-pmatl%dbar**3)

IF( dmass > 0. )THEN
  wvlim = MAX(MIN(plqd/psat,wvfrac),0.01)
  xxx   = 1. - wvlim*(1.-wvfrac)/(wvfrac*(1.-wvlim))
  dmass = MIN(dmass,mass*xxx)
ELSE
  wvlim = MAX(plqd/psat,wvfrac)
  xxx   = 1. - wvlim*(1.-wvfrac)/(wvfrac*(1.-wvlim))
  dmass = MAX(dmass,mass*xxx)
END IF

!----- Expand puff volume to account for vapor formation

drop%d = (drop%d**3-dmass/rfac)**THIRD

RETURN
END

!==============================================================================

SUBROUTINE set_water_props( tamb,rhod,pb,psat,hv,dif )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: tamb, pb
REAL, INTENT( OUT ) :: psat, dif, hv, rhod

REAL tc

!-----  Convert droplet temperature from Kelvin to Celsius

tc = tamb + ABSZERO

!-----  Calculate liquid properties as a function of temperature
!-----  Units are MKS
!-----  In cgs units 1.6e-5 = 1.0e+6/760/R

rhod = MAX(1000.78 - 0.197*tc,1.0E-6)
psat = 1.0E+2 *10.**((0.7859+0.03477*tc)/(1.0+0.00412*tc)) ! Pa
dif  = 2.11E-5*(tamb/273.2)**1.94/pb ! (m2/s)
hv   = 2.5008E6 - 2.3E3*tc ! (j/Kg)

RETURN
END

!-----------------------------------------------------------------------

REAL FUNCTION kincof( radius,gasct,dfunct,alphc,delta )

USE constants_fd

IMPLICIT NONE

REAL radius, gasct, dfunct, alphc, delta

!----- Note that the "radius" cannot be zero in the following expression!

kincof = 1.0/(radius/(radius+delta)+dfunct/(radius*alphc)*SQRT(PI2/gasct))

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE set_liquid_buoy( p,pt,imat,bfac )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_str ),     INTENT( IN  ) :: p
TYPE( puff_totalcc ), INTENT( IN  ) :: pt
INTEGER,              INTENT( IN  ) :: imat
REAL,                 INTENT( OUT ) :: bfac

REAL mcv, rhov, rhob

TYPE( liquid_material ) matl

CALL GetLiquidParam( matl,material(imat)%iaux,2,mat_aux )

IF( p%cc > 0.0 )THEN
  mcv  = MIN(1.0,p%cc/pt%cct)
  rhov = rhoair*matl%w/MWAIR
  rhob = (1.0-mcv)/matl%rho + mcv/rhov
  rhob = 1.0/rhob
  bfac = (rhob-rhoair)/(rhob*rhoair*mcv)
ELSE
  bfac = buoy_fac(p%ityp)
END IF

RETURN
END

