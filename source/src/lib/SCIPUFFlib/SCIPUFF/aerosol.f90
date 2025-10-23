!------ Two-phase aerosol routines using Homogeneous Equilibrium Approximation

SUBROUTINE set_aerosol( p,pa,pd,pmatIn )

USE scipuff_fi
USE met_fi
USE aerosol_fi

!  Initialize aerosol thermodynamic variables and adjust puff
!  temperatures to account for specific heat variation

IMPLICIT NONE

TYPE( puff_str      ), INTENT( INOUT ) :: p
TYPE( puff_aerosol  ), INTENT( INOUT ) :: pa
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
TYPE( puff_material ), INTENT( IN    ) :: pmatIn

TYPE( aerosol_material ) pmataer
TYPE( liquid_material  ) pmat

REAL efac, r, cn, rhoa, rhob, rhov, denf

REAL,    EXTERNAL :: fun_rhoa
LOGICAL, EXTERNAL :: IsLiquid

IF( IsLiquid(material(typeID(p%ityp)%imat)%icls) )THEN
  pmat = TRANSFER(pmatIn,pmat)
ELSE
  pmataer = TRANSFER(pmatIn,pmataer)
  pmat%rho = pmataer%rho                         !Liquid density
  pmat%a   = pmataer%a                           !Antoine Coefficient a
  pmat%b   = pmataer%b                           !Antoine Coefficient b
  pmat%c   = pmataer%c                           !Antoine Coefficient c
  pmat%w   = pmataer%w                           !Molecular Weight
  pmat%cpL = pmataer%cpL                         !Specific heat (liquid)
  pmat%cpV = pmataer%cpV                         !Specific heat (vapor)
END IF

!------ Set ambient and cloud temperatures, and ambient pressure

temp = tb
pamb = pb / PSURF
IF( dynamic )temp = MAX(100.,temp + pd%ctp/p%c)

rhoa = fun_rhoa( temp,pb )! rhoair*(pb/PSURF)*(TZERO/temp) !(pb**KAPPAC)*TZERO/temp
rhov = rhoa*pmat%w/MWAIR

!------ Entrainment factor (= old conc / new conc)

cn   = p%cc/p%c
efac = pa%co/cn

!---- Reset mass fraction if concentration increases

IF( efac < 1.0 )THEN
  rhob = (1.0-pa%fl)/rhov + pa%fl/pmat%rho
  mc   = 1.0/(1.0+rhoa*rhob*(1.0/MIN(1.0,(cn*rhob))-1.0))
  r    = 0.0
ELSE
  rhob = (1.0-pa%fl)/rhov + pa%fl/pmat%rho
  mc   = 1.0/(1.0+rhoa*rhob*(1.0/MIN(1.0,(pa%co*rhob))-1.0))
  r    = efac - 1.0
END IF

!------ Set initial contaminant mass fractions

mc  = MIN(1.0,mc)
mcl = mc*pa%fl
mcv = mc*(1.0 - pa%fl)

!------ Set initial water mass fractions (assume ambient mixing ratio)

mw  = hb*(1.0 - mc)
IF( mc > 0.9999 )THEN
  mwl = 1.0E-10
ELSE
  mwl = mw*pa%fw
END IF
mwv = mw*(1.0 - pa%fw)

!------ Set dry air mass fraction as remainder

mda = 1.0 - mc - mw

!------ Set specific heats

cpl = pmat%cpL
cpv = pmat%cpV

cbar = mcl*cpl  + mcv*cpv  + mwl*CPWL  + mwv*CPWV  + mda*CPAIR

!------ Set initial density, assuming standard for air/water compoment

rhob = mcl/pmat%rho + mcv/rhov + (1.0-mc)/rhoa

!---- Set entrainment reduction factor

denf = MAX( -0.9,r*rhoa*rhob )
mw   = MAX(  0.0,mw+denf*hb )     !Add entrained water
denf = MAX( mc+mw,1.0+denf )
denf = 1.0/denf

!---- New mass fractions due to entrainment

mc  = mc *denf
mcl = mcl*denf
mcv = mcv*denf

mw  = mw *denf
mwl = MIN(mw,mwl*denf)
mwv = mw - mwl

mda = 1.0 - mc - mw

!---- New specific heat

cbarn = mcl*cpl  + mcv*cpv  + mwl*CPWL  + mwv*CPWV  + mda*CPAIR

!---- Adjust temperatures due to entrainment

IF( dynamic )THEN
  denf = denf*cbar/cbarn
  pd%t = pd%t*denf*efac
END IF

!---- Save values for contaminant mass fraction and old concentration

pa%co = cn

IF( mcl > SMALL .OR. mwl > SMALL )THEN

!---- Update initial value for equilibrium phase adjustment

  cbar = cbarn

!---- Calculate new equilibrium state

  CALL init_aerosol_params( pmat )
  CALL update_aerosol_equilib( pmat,p,pd,pa )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

!----- Clear aerosol liquid fractions

  pa%fl = 0.0
  pa%fw = 0.0
  pa%tevap = 0.0

END IF

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE init_aerosol_params( pmat )

USE scipuff_fi
USE aerosol_fi

!  Set aerosol thermodynamic parameters

IMPLICIT NONE

REAL, PARAMETER :: HVFAC  = 1.9148E4
REAL, PARAMETER :: LOG10  = 2.302585
REAL, PARAMETER :: LOG760 = 6.633318

TYPE( liquid_material ), INTENT( IN ) :: pmat

MWc = pmat%w
Ac  = pmat%a*LOG10 - LOG760
Bc  = pmat%b*LOG10
Cc  = pmat%c - TZERO
Tcb = Bc/Ac - Cc
Twb = TB_WATER

Tc_min = Bc/(20.0+Ac) - Cc
Tw_min = B_WATER/(20.0+A_WATER) - C_WATER

Lc = HVFAC*pmat%b*(Tcb/(Tcb+Cc))**2/MWc
Lw = LWATER

Lct = Lc + Tcb*(cpl-cpv)
Lwt = Lw + Twb*(CPWL-CPWV)

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE update_aerosol_equilib( pmat,p,pd,pa )

USE scipuff_fi
USE met_fi
USE aerosol_fi

!  Calculate new  aerosol thermodynamic equilibrium state

IMPLICIT NONE

TYPE( liquid_material ),  INTENT( IN    ) :: pmat
TYPE( puff_str      ),    INTENT( INOUT ) :: p
TYPE( puff_dynamics ),    INTENT( INOUT ) :: pd
TYPE( puff_aerosol  ),    INTENT( INOUT ) :: pa

REAL mclo, mwlo, delh
REAL rhoa, rhov, rhob, Lcx, Lwx
REAL efac, r

REAL, EXTERNAL :: fun_rhoa

mclo = mcl
mwlo = mwl

CALL aerosol_equilib()
IF( nError /= NO_ERROR )GOTO 9999

!------ Update aerosol variables

pa%fl = mcl/mc
IF( mw > 0.0 )THEN
  pa%fw = mwl/mw
ELSE
  pa%fw = 0.0
END IF

!------ Update dynamic variables

IF( dynamic )THEN
  rhoa = fun_rhoa( temp,pb )
  rhov = rhoa*pmat%w/MWAIR
!--- Expand puff volume to account for vapor formation and adjust concentration
  IF( mclo > mcl )THEN
    efac  = 1.0 + (mclo-mcl)*p%cc/(mc*rhov*p%c)
    r     = efac**TwoThirds
    p%sxx = r*p%sxx
    p%syy = r*p%syy
    p%szz = r*p%szz
    p%cc  = p%cc/efac
    pa%co = pa%co/efac
  ELSE
    efac = 1.0
  END IF
  rhob = mcl/pmat%rho + (mc-mcl)/rhov + (1.0-mc)/rhoa
  Lcx  = Lct + MIN(Tcb,temp)*(cpv-cpl)
  Lwx  = Lwt + MIN(Twb,temp)*(CPWV-CPWL)
  delh   = rhob*((mcl-mclo)*Lcx + (mwl-mwlo)*Lwx)/mc
  pd%t   = (cbar*pd%t   + delh*p%c  )/cbarn
  pd%ctb   = (cbar*pd%ctb + delh*p%ccb/efac)/cbarn
  pa%tevap = delh*p%c/cbarn      !Save temperature change due to evaporation
END IF

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE aerosol_equilib()

USE aerosol_fi
USE error_fi

!  Calculate new aerosol thermodynamic equilibrium state

IMPLICIT NONE

REAL mclo, mwlo
REAL Lcx, Lwx

REAL, DIMENSION(3) :: vec, tol

EXTERNAL aerosol_fun_c, aerosol_fun_w, aerosol_fun_cw

mclo = mcl
mwlo = mwl

tol(1) = 1.0E-4 ! Liquid mass fraction
tol(2) = 1.0E-4 ! Liquid mass fraction
tol(3) = 0.1    ! Temperature

DO

  IF( mcl > 0.0 .AND. mwl > 0.0 )THEN

    vec(1) = mcl
    vec(2) = mwl
    vec(3) = MIN(Tcb,temp)
    CALL eqsolve( 10,vec,3,aerosol_fun_cw,tol )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( vec(3) > Tcb )THEN
      Lcx    = Lct + Tcb*(cpv-cpl)
      cbarn  = mc*cpv + mw*CPWV + mda*CPAIR
      vec(1) = vec(1) - (vec(3)-Tcb)*cbarn/Lcx
      vec(3) = Tcb
    END IF
    IF( vec(3) > Twb )THEN
      Lwx    = Lwt + Twb*(CPWV-CPWL)
      cbarn  = mc*cpv + mw*CPWV + mda*CPAIR
      vec(2) = vec(2) - (vec(3)-Twb)*cbarn/Lwx
      vec(3) = Twb
    END IF
    IF( vec(1) <= 0.0 .AND. vec(2) <= 0.0 )THEN
      Lcx   = Lct + MIN(Tcb,temp)*(cpv-cpl)
      Lwx   = Lwt + MIN(Twb,temp)*(CPWV-CPWL)
      cbarn = mc*cpv + mw*CPWV + mda*CPAIR
      temp  = temp - (Lcx*mcl + Lwx*mwl)/cbarn
      mcl   = 0.0
      mwl   = 0.0
      EXIT
    ELSE IF( vec(1) <= 0.0 )THEN
      mwl   = vec(2)
      Lcx   = Lct + MIN(Tcb,temp)*(cpv-cpl)
      Lwx   = Lwt + MIN(Twb,temp)*(CPWV-CPWL)
      cbarn = mc*cpv + mwl*CPWL + (mw-mwl)*CPWV + mda*CPAIR
      temp  = temp - (Lcx*mcl+Lwx*(mwlo-mwl))/cbarn
      mcl   = 0.0
    ELSE IF( vec(2) <= 0.0 )THEN
      mcl   = vec(1)
      Lcx   = Lct + MIN(Tcb,temp)*(cpv-cpl)
      Lwx   = Lwt + MIN(Twb,temp)*(CPWV-CPWL)
      cbarn = mcl*cpl + (mc-mcl)*cpv + mw*CPWV + mda*CPAIR
      temp  = temp - (Lwx*mwl+Lcx*(mclo-mcl))/cbarn
      mwl   = 0.0
    ELSE
      mcl  = vec(1)
      mwl  = vec(2)
      temp = vec(3)
    END IF
    IF( mcl > 0.0 .AND. mwl > 0.0 )EXIT

  ELSE IF( mcl > 0.0 )THEN

    vec(1) = mcl
    vec(2) = MIN(Tcb,temp)
    tol(2) = tol(3)
    CALL eqsolve( 10,vec,2,aerosol_fun_c,tol )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( vec(2) > Tcb )THEN
      Lcx    = Lct + Tcb*(cpv-cpl)
      cbarn  = mc*cpv + mw*CPWV + mda*CPAIR
      vec(1) = vec(1) - (vec(2)-Tcb)*cbarn/Lcx
      vec(2) = Tcb
    END IF
    IF( vec(1) <= 0.0 )THEN
      Lcx   = Lct + MIN(Tcb,temp)*(cpv-cpl)
      cbarn = mc*cpv + mw*CPWV + mda*CPAIR
      temp  = temp - Lcx*mcl/cbarn
      mcl   = 0.0
    ELSE
      mcl  = vec(1)
      temp = vec(2)
    END IF
    EXIT

  ELSE IF( mwl > 0.0 )THEN

    vec(1) = mwl
    vec(2) = temp
    tol(2) = tol(3)
    CALL eqsolve( 10,vec,2,aerosol_fun_w,tol )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( vec(2) > Twb )THEN
      Lwx    = Lwt + Twb*(CPWV-CPWL)
      cbarn  = mc*cpv + mw*CPWV + mda*CPAIR
      vec(1) = vec(1) - (vec(2)-Twb)*cbarn/Lwx
      vec(2) = Twb
    END IF
    IF( vec(1) <= 0.0 )THEN
      Lwx   = Lwt + MIN(Twb,temp)*(CPWV-CPWL)
      cbarn = mc*cpv + mw*CPWV + mda*CPAIR
      temp  = temp - Lwx*mwl/cbarn
      mwl   = 0.0
    ELSE
      mwl  = vec(1)
      temp = vec(2)
    END IF
    EXIT

  ELSE ! mcl & mwl = 0

    EXIT

  END IF

END DO

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE eqsolve( nmax,x,n,fun,tol )

USE error_fi

!  Calculate new  aerosol thermodynamic equilibrium state

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: nmax, n
REAL, DIMENSION(n), INTENT( INOUT ) :: x
REAL, DIMENSION(n), INTENT( IN    ) :: tol

EXTERNAL fun

REAL, DIMENSION(3)   :: p, vwrk
REAL, DIMENSION(3,3) :: fjac

INTEGER, DIMENSION(3) :: indx

INTEGER i, k
REAL    d
LOGICAL converge

converge = .FALSE.

k = 0

DO WHILE( k < nmax .AND. .NOT.converge )

  k = k + 1

!---- Compute residual and Jacobian

  CALL fun( x,n,3,p,fjac )

!---- Solve matrix system

  CALL ludcmp( fjac,n,3,indx,d,vwrk )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL lubksb( fjac,n,3,indx,p )

!---- Check convergence

  converge = .TRUE.
  DO i = 1,n
    converge = converge .AND. ( ABS(p(i)) < tol(i) )
    x(i) = x(i) + p(i)
  END DO

END DO

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE aerosol_fun_cw( x,n,ndim,fvec,fjac )

USE constants_fd
USE aerosol_fi

!  Aerosol thermodynamic equilibrium functions for full component mix

IMPLICIT NONE

INTEGER,                 INTENT( IN  ) :: n, ndim
REAL, DIMENSION(n),      INTENT( IN  ) :: x
REAL, DIMENSION(n),      INTENT( OUT ) :: fvec
REAL, DIMENSION(ndim,n), INTENT( OUT ) :: fjac

REAL Psw, Psc, Pvw, Pvc, num

mcv = mc - x(1)
mwv = mw - x(2)

IF( x(3) < Tw_min )THEN
  Psw = 0.0
ELSE
  Psw = EXP( A_WATER - B_WATER/(x(3)+C_WATER) ) / pamb
END IF
IF( x(3) < Tc_min )THEN
  Psc = 0.0
ELSE
  Psc = EXP( Ac - Bc/(x(3)+Cc) ) / pamb
END IF

num = (mda/MWAIR + mwv/MW_WATER + mcv/MWc)

Pvw = Psw*MW_WATER*num
Pvc = Psc*MWc*num

cbarn = x(1)*cpl + mcv*cpv + x(2)*CPWL + mwv*CPWV + mda*CPAIR

fvec(1) = x(3)*cbarn - temp*cbar + (mcl-x(1))*Lct + (mwl-x(2))*Lwt
fvec(2) = x(2) - (mw - Pvw)
fvec(3) = x(1) - (mc - Pvc)

fjac(1,1) =  Lct - x(3)*(cpl-cpv)
fjac(1,2) =  Lwt - x(3)*(CPWL-CPWV)
fjac(1,3) = -cbarn

fjac(2,1) =  Psw*MW_WATER/MWc
fjac(2,2) = -1.0 + Psw
fjac(2,3) = -Pvw*B_WATER/( (x(3)+C_WATER)**2 )

fjac(3,1) = -1.0 + Psc
fjac(3,2) =  Psc*MWc/MW_WATER
fjac(3,3) = -Pvc*Bc/( (x(3)+Cc)**2 )

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE aerosol_fun_c( x,n,ndim,fvec,fjac )

USE aerosol_fi

!  Aerosol thermodynamic equilibrium functions for no liquid water

IMPLICIT NONE

INTEGER,                 INTENT( IN  ) :: n, ndim
REAL, DIMENSION(n),      INTENT( IN  ) :: x
REAL, DIMENSION(n),      INTENT( OUT ) :: fvec
REAL, DIMENSION(ndim,n), INTENT( OUT ) :: fjac

REAL Psc, Pvc, num

mcv = mc - x(1)

IF( x(2) < Tc_min )THEN
  Psc = 0.0
ELSE
  Psc = EXP( Ac - Bc/(x(2)+Cc) ) / pamb
END IF

num = (mda/MWAIR + mw/MW_WATER + mcv/MWc)

Pvc = Psc*MWc*num

cbarn = x(1)*cpl + mcv*cpv + mw*CPWV + mda*CPAIR

fvec(1) = x(2)*cbarn - temp*cbar + (mcl-x(1))*Lct
fvec(2) = x(1) - (mc - Pvc)

fjac(1,1) =  Lct - x(2)*(cpl-cpv)
fjac(1,2) = -cbarn

fjac(2,1) = -1.0 + Psc
fjac(2,2) = -Pvc*Bc/( (x(2)+Cc)**2 )

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE aerosol_fun_w( x,n,ndim,fvec,fjac )

USE constants_fd
USE aerosol_fi

!  Aerosol thermodynamic equilibrium functions for no liquid contaminant

IMPLICIT NONE

INTEGER,                 INTENT( IN  ) :: n, ndim
REAL, DIMENSION(n),      INTENT( IN  ) :: x
REAL, DIMENSION(n),      INTENT( OUT ) :: fvec
REAL, DIMENSION(ndim,n), INTENT( OUT ) :: fjac

REAL Psw, Pvw, num

mwv = mw - x(1)

IF( x(2) < Tw_min )THEN
  Psw = 0.0
ELSE
  Psw = EXP( A_WATER - B_WATER/(x(2)+C_WATER) ) / pamb
END IF

num = (mda/MWAIR + mwv/MW_WATER + mc/MWc)

Pvw = Psw*MW_WATER*num

cbarn = mc*cpv + x(1)*CPWL + mwv*CPWV + mda*CPAIR

fvec(1) = x(2)*cbarn - temp*cbar + (mwl-x(1))*Lwt
fvec(2) = x(1) - (mw - Pvw)

fjac(1,1) =  Lwt - x(2)*(CPWL-CPWV)
fjac(1,2) = -cbarn

fjac(2,1) = -1.0 + Psw
fjac(2,2) = -Pvw*B_WATER/( (x(2)+C_WATER)**2 )

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE set_aerosol_inst( release,p,pd,pmat )

USE scipuff_fi
USE met_fi
USE aerosol_fi

!  Set initial variables for aerosol puff
!  Flash calculation + entrainment without phase change

IMPLICIT NONE

TYPE( releaseT ),         INTENT( IN    ) :: release
TYPE( puff_str      ),    INTENT( INOUT ) :: p
TYPE( puff_dynamics ),    INTENT( INOUT ) :: pd
TYPE( aerosol_material ), INTENT( IN    ) :: pmat

TYPE( puff_aerosol     ) pa
TYPE( liquid_material  ) pmatliq

REAL    rhoi, rhov, rad, vol, efac, rhoa, denf
REAL    xbar, ybar, zbar, hp, hx, hy, sx, sy
INTEGER imat, ifld

INTEGER subgroup
REAL mmd,sigma,buoy,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz
REAL, DIMENSION(3) :: mom

REAL,    EXTERNAL :: fun_rhoa
INTEGER, EXTERNAL :: getPuffifld

!------ Get ambient conditions

xbar = SNGL(p%xbar)
ybar = SNGL(p%ybar)

ifld = getPuffifld( p )

IF( lter )THEN
  CALL get_topogIn( xbar,ybar,hp,hx,hy,ifld )
ELSE
  hp = 0.
END IF

zbar = MAX(p%zbar,SQRT(p%szz)+hp)
CALL get_met( xbar,ybar,zbar,p%szz,p%zc,0,inField=ifld )

IF( release%type == HR_INST )THEN    !Simple vapor release

  CALL getReleaseDistribution( release,subgroup,mmd,sigma )
  IF( subgroup == 2 )THEN

    pa%co = p%c/(PI3*SQRT(8.*p%sxx*p%syy*p%szz))

    temp = tb
    IF( dynamic )temp = temp + pd%ctp/p%c
    rhoa = fun_rhoa( temp,pb )

    pa%tevap = 0.0
    pa%fl    = 0.0
    pa%fw    = 0.0

  ELSE    !Old style flashing liquid with expansion factors

!------ Initialize aerosol material properties

    cpl = pmat%cpL
    cpv = pmat%cpV

    pmatliq%rho = pmat%rho                         !Liquid density
    pmatliq%a   = pmat%a                           !Antoine Coefficient a
    pmatliq%b   = pmat%b                           !Antoine Coefficient b
    pmatliq%c   = pmat%c                           !Antoine Coefficient c
    pmatliq%w   = pmat%w                           !Molecular Weight
    pmatliq%cpL = pmat%cpL                         !Specific heat (liquid)
    pmatliq%cpV = pmat%cpV                         !Specific heat (vapor)

    CALL init_aerosol_params( pmatliq )

!------ Initial liquid fraction

    mcv = (cpl/Lc)*(tb-Tcb)
    mcv = MIN(mcv,1.0)
    mcl = 1.0 - mcv

!------ Check for sufficient superheat

    IF( mcv <= 0. )THEN
      imat     = typeID(p%ityp)%imat
      nError   = IV_ERROR
      eRoutine = 'set_aerosol_inst'
      eMessage = 'Aerosol material '//TRIM(material(imat)%cmat)//' will not flash'
      eInform  = 'Boiling point is above ambient temperature'
      GOTO 9999
    END IF

!------ Initial density (pure contaminant)

    rhoa = fun_rhoa( Tcb,pb )
    rhov = rhoa*MWc/MWAIR
    rhoi = mcv/rhov + mcl/pmat%rho

!------ Initialize auxiliary aerosol puff data and store

    pa%tevap = 0.0
    pa%fl    = mcl/(mcl+mcv)
    pa%fw    = 1.0E-10
    pa%co    = 1.0/rhoi

!------ Initial cloud sigmas (rad is half-radius squared)

    vol = p%c*rhoi
    rad = 0.25*(vol*0.75/PI)**TwoThirds

!------ Set sigmas using entrainment ratios

    p%sxx = rad*p%sxx
    p%syy = rad*p%syy
    p%szz = rad*p%szz

!------ Set initial length scales

    sx    = SQRT(p%sxx)
    sy    = SQRT(p%syy)
    p%si  = MIN(sx,sy)
    p%si2 = MAX(sx,sy)
    p%sv  = SQRT(p%szz)

!------ Thermodynamic adjustment for entrainment

    efac = SQRT(p%sxx*p%syy*p%szz/rad**3)

    IF( efac <= 1.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'set_aerosol_inst'
      eMessage = 'Invalid release volume'
      WRITE(eInform,*)'Input size must be larger than pure agent flash size: ',SQRT(rad)
    END IF

    cbar = mcl*cpl + mcv*cpv

    denf = 1.0/(1.0 + (efac-1.0)*rhoa*rhoi)

    mcl = mcl*denf
    mcv = mcv*denf
    mwv = (1.-denf)*hb
    mda = 1.0 - mcl - mcv - mwv

!------ Initialize temperature to contaminant boiling point

    IF( dynamic )THEN

      cbarn = mcl*cpl  + mcv*cpv  + mwv*CPWV  + mda*CPAIR
      pd%t  = vol*(Tcb-tb)*cbar/cbarn

      CALL put_dynamics( p,pd )

    END IF

  END IF

ELSE IF( release%type == HR_FINST )THEN   !Flashing release

  CALL getReleaseSigmas( release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )
  CALL getReleaseDynamics( release,buoy,mom )

!------ Initialize aerosol material properties

  cpl = pmat%cpL
  cpv = pmat%cpV

  pmatliq%rho = pmat%rho                         !Liquid density
  pmatliq%a   = pmat%a                           !Antoine Coefficient a
  pmatliq%b   = pmat%b                           !Antoine Coefficient b
  pmatliq%c   = pmat%c                           !Antoine Coefficient c
  pmatliq%w   = pmat%w                           !Molecular Weight
  pmatliq%cpL = pmat%cpL                         !Specific heat (liquid)
  pmatliq%cpV = pmat%cpV                         !Specific heat (vapor)

  CALL init_aerosol_params( pmatliq )

  IF( buoy == DEF_VAL_R )THEN
    temp = tb
  ELSE
    temp = buoy + TZERO
  END IF

!------ Initial liquid fraction

  mcv = (cpl/Lc)*(temp-Tcb)
  mcv = MIN(mcv,1.0)
  mcl = 1.0 - mcv

!------ Check for sufficient superheat

  IF( mcv <= 0. )THEN
    imat     = typeID(p%ityp)%imat
    nError   = IV_ERROR
    eRoutine = 'set_aerosol_inst'
    eMessage = 'Aerosol material '//TRIM(material(imat)%cmat)//' will not flash'
    eInform  = 'Boiling point is above ambient temperature'
    GOTO 9999
  END IF

!------ Initial density (pure contaminant)

  rhoa = fun_rhoa( Tcb,pb )
  rhov = rhoa*MWc/MWAIR
  rhoi = mcv/rhov + mcl/pmat%rho

!------ Initialize auxiliary aerosol puff data and store

  pa%tevap = 0.0
  pa%fl    = mcl/(mcl+mcv)
  pa%fw    = 1.0E-10
  pa%co    = 1.0/rhoi

!------ Initial cloud sigmas (rad is half-radius squared)

  vol = p%c*rhoi
  rad = 0.25*(vol*0.75/PI)**TwoThirds

  IF( sigx == DEF_VAL_R .OR. sigx == NOT_SET_R )THEN

!------ Calculate entrainment

    CALL FlashEvap( p%c,vol,efac )

!------ Set sigmas using entrainment ratios

    p%sxx = rad*(efac**2)
    p%syy = p%sxx
    p%szz = p%sxx

  END IF

!------ Set initial length scales

  sx    = SQRT(p%sxx)
  sy    = SQRT(p%syy)
  p%si  = MIN(sx,sy)
  p%si2 = MAX(sx,sy)
  p%sv  = SQRT(p%szz)

!------ Thermodynamic adjustment for entrainment

  efac = SQRT(p%sxx*p%syy*p%szz/rad**3)

  IF( efac <= 1.0 )THEN
    nError   = IV_ERROR
    eRoutine = 'set_aerosol_inst'
    eMessage = 'Invalid release volume'
    WRITE(eInform,*)'Input size must be larger than pure agent flash size: ',SQRT(rad)
  END IF

  cbar = mcl*cpl + mcv*cpv

  denf = 1.0/(1.0 + (efac-1.0)*rhoa*rhoi)

  mcl = mcl*denf
  mcv = mcv*denf
  mwv = (1.-denf)*hb
  mda = 1.0 - mcl - mcv - mwv

!------ Initialize temperature to contaminant boiling point

  IF( dynamic )THEN

    cbarn = mcl*cpl  + mcv*cpv  + mwv*CPWV  + mda*CPAIR
    pd%t  = vol*(Tcb-tb)*cbar/cbarn

    CALL put_dynamics( p,pd )

  END IF

ELSE

    nError = IV_ERROR
    eRoutine = 'set_aerosol_inst'
    WRITE(eMessage,'(A,I0)')'Invalid release type:',release%type
    WRITE(eInform,'(A,I0,A,I0)')'Must be', HR_INST,' or',HR_FINST
    GOTO 9999

END IF

CALL put_aerosol( p,pa )

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE FlashEvap( mass,voli,efac )

USE aerosol_fi
USE met_fi

IMPLICIT NONE

REAL, INTENT( IN  ) :: mass
REAL, INTENT( IN  ) :: voli
REAL, INTENT( OUT ) :: efac

REAL, PARAMETER :: GAMMA  = 2.361111

INTEGER :: iter
REAL    :: rhoa, rhosv, rhov
REAL    :: volc, volf, r0, rf, uL, ke0, kef
REAL    :: tau0, bexp, fac, tf

REAL, EXTERNAL :: Fun_rhoa

!------ Initial density (pure contaminant)

rhoa  = fun_rhoa( Tcb,pb )
rhosv = rhoa*MWc/MWAIR
rhoa  = fun_rhoa( tb,pb )
rhov  = rhoa*MWc/MWAIR

!------ Initial cloud volume

volc = 19.5*2.*voli
uL   = 40.*(mcv*rhoa*rhoa/(rhosv*rhov))**0.1111

ke0  = 1.5*uL*uL
r0   = (1.5*volc/PI)**0.3333
tau0 = r0/SQRT(ke0)
bexp = -0.8588   ! 1/(3*gamma) - 1

volf = volc
volc = 0.5*volf
iter = 0

DO WHILE( ABS(volf-volc)/volc > 0.1 .AND. iter < 10 )
  kef  = 1.5*G0*mass/(rhoa*volf**TwoThirds)
  tf   = (EXP(LOG(SQRT(kef)*tau0/r0)/bexp) - 1.)*tau0/GAMMA
  fac  = 1.+ GAMMA*tf/tau0
  rf   = r0*fac**0.141176
  volc = volf
  volf = TwoThirds*PI*rf**3.
  iter = iter + 1
END DO

efac = (volf/voli)**0.3333333

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE set_aerosol_cont( p,pd,pa,pmat )

USE scipuff_fi
USE aerosol_fi
USE met_fi

!  Set initial variables for aerosol puff
!  Continuous release

IMPLICIT NONE

TYPE( puff_str      ), INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
TYPE( puff_aerosol  ), INTENT( INOUT ) :: pa
TYPE( puff_material ), INTENT( IN    ) :: pmat

INTEGER icls
REAL    rhov, rhoi, rhoa

TYPE( liquid_material  ) pmatliq

LOGICAL, EXTERNAL :: IsLiquid
REAL,    EXTERNAL :: fun_rhoa

icls = material(typeID(p%ityp)%imat)%icls

IF( IsLiquid(icls) )THEN
  pmatliq = TRANSFER( pmat,pmatliq)
  pa%co = p%cc/p%c
  temp = tb
  IF( dynamic )temp = temp + pd%ctp/p%c
  rhoa = fun_rhoa( temp,pb )
  rhov = rhoa*pmatliq%w/MWAIR
  rhoi = (1.0-pa%fl)/rhov + pa%fl/pmatliq%rho
  pa%tevap = 0.0
ELSE
  pa%fl    = 0.0
  pa%fw    = 0.0
  pa%tevap = 0.0
  pa%co    = 0.0
END IF

CALL put_aerosol( p,pa )

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE init_aerosol_equilibrium( p,pd,pmat )

USE scipuff_fi

!  Set initial equilibrium state for aerosol puff

IMPLICIT NONE

TYPE( puff_str      ), INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
TYPE( puff_material ), INTENT( IN )    :: pmat

TYPE( puff_aerosol ) pa

!----- Load puff auxiliary structures

CALL get_aerosol( p,pa )

IF( dynamic )CALL get_dynamics( p,pd )

!----- Calculate thermodynamic equilibrium state

CALL set_aerosol( p,pa,pd,pmat )
IF( nError /= NO_ERROR )GOTO 9999

!----- Store puff auxiliary structures

CALL put_aerosol( p,pa )

IF( dynamic )CALL put_dynamics( p,pd )

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------------

SUBROUTINE set_aerosol_buoy( pa,pmatIn,bfac,lliqm )

USE scipuff_fi
USE aerosol_fi

!----- Set buoyancy factor for aerosol 2-phase puff

IMPLICIT NONE

TYPE( puff_aerosol  ), INTENT( IN  ) :: pa
TYPE( puff_material ), INTENT( IN  ) :: pmatIn
REAL,                  INTENT( OUT ) :: bfac
LOGICAL,               INTENT( IN  ) :: lliqm

REAL rhob, rhov, mwx, rhol

TYPE( aerosol_material ) pmat
TYPE( liquid_material  ) pmatliq

IF( lliqm )THEN
  pmatliq = TRANSFER(pmatIn,pmatliq)
  mwx  = pmatliq%w
  rhol = pmatliq%rho
ELSE
  pmat = TRANSFER(pmatIn,pmat)
  mwx  = pmat%w
  rhol = pmat%rho
END IF

!----- Set liquid/vapor contaminant mass fractions

mcl = pa%fl
mcv = 1.0 - mcl

!----- Set 2-phase contaminant density
!      NB only accounts for contaminant since air density doesn't change
!      due to phase change of water

rhov = rhoair*mwx/MWAIR
rhob = mcl/rhol + mcv/rhov
rhob = 1.0/rhob

!----- Set buoyancy factor

bfac = (rhob-rhoair)/(rhob*rhoair)

RETURN
END
