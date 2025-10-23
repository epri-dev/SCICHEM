!===============================================================================
!
!  SWIM Boundary Layer Utility Routines
!
!===============================================================================

!------ Module for Monin-Obukhov iterations

MODULE molfunc_fi

  SAVE

  REAL, PARAMETER :: TOL  = 0.001  !Relative tolerance on L iteration
  REAL, PARAMETER :: ATOL = 0.1    !Absolute tolerance on L iteration
  REAL, PARAMETER :: BETA = 4.9    !Stable vel. profile factor (as z/L -> 0)

  TYPE mol_struct
    SEQUENCE
    REAL zref
    REAL uref
    REAL tref
    REAL zruf
    REAL hc
    REAL alphac
    REAL bflx
    REAL vonk
    REAL Lmin
    REAL VHlim
    REAL ustar
    REAL lat
    REAL cc
    REAL zi
    LOGICAL lAdvZi
  END TYPE mol_struct

  TYPE( mol_struct ) :: f

  COMMON / molfuncCommon / f
!$OMP THREADPRIVATE(/molfuncCommon/)

END MODULE molfunc_fi

!==============================================================================

RECURSIVE SUBROUTINE hv( t,p,hvn,bowen,hvs )

!------ Computes the sensible heat flux from the net radiation.

IMPLICIT NONE

REAL, INTENT( IN  ) :: t, p, hvn, bowen
REAL, INTENT( OUT ) :: hvs

REAL, PARAMETER :: XLV   = 2.501E6
REAL, PARAMETER :: RV    = 461.51
REAL, PARAMETER :: EPI   = 0.622
REAL, PARAMETER :: GAMMA = 4.01808e-4
REAL, PARAMETER :: CG    = 0.1
REAL, PARAMETER :: BETAP = 20.

REAL es, desdt, s, z, qup, alpha, beta, press

press = p*100. !Convert from mb to N/m/m
es    = 100.*10.**(-2937.4/t-4.9283*LOG10(t)+23.5518)
desdt = XLV*es/RV/t**2
s     = (press*EPI*desdt)/(press-es)**2
z     = 1.0 + GAMMA/s
qup   = (1.0 - CG) * hvn
alpha = (z*qup)/((1.0+bowen)*(qup + BETAP*z))
beta  = alpha*BETAP
hvs   = (z-alpha)/z*qup - beta

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE hvnet( sun_ang,albedo,cc,qr,t,hvn )

!       Computes the net radiation from total incoming solar radiation.

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: sun_ang, albedo, cc, qr, t
REAL, INTENT( OUT ) :: hvn

REAL, PARAMETER :: C1 = 5.31E-13
REAL, PARAMETER :: C2 = 60.
REAL, PARAMETER :: C3 = 0.12

REAL c, bb, r

c   = 1.0 - albedo
bb  = -0.5*c*c

IF( sun_ang <= 0.0 )THEN
  r = 1.0
ELSE
  r = albedo + c*EXP(-0.1*sun_ang + bb)
END IF

hvn = ((1.-r)*qr+C1*t**6-SIG_SB*t**4+C2*cc)/(1.+C3)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE sun( lat,jul,t_local,sun_ang )

!       Calculates the solar elevation angle for a given hour of the day from
!       the date, latitude

USE constants_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: jul
REAL,    INTENT( IN  ) :: lat, t_local
REAL,    INTENT( OUT ) :: sun_ang

REAL, PARAMETER :: YRFAC = 2.*PI/365.242
REAL, PARAMETER :: DEGHR = 15.
REAL, PARAMETER :: HFAC  = DEGHR*PI180

REAL day, snd, sigma, capd, solha

REAL, EXTERNAL :: cosd

day     = (FLOAT(jul)-1.)*YRFAC
snd     = SIN(day)
sigma   = 4.871 + day + 0.033*snd
capd    = ASIN(0.398*SIN(sigma))

solha   = (t_local-12.)*HFAC + 0.043*SIN(2.*sigma) - 0.033*snd

sun_ang = ASIN(SIN(lat*PI180)*SIN(capd)+cosd(lat)*COS(capd)*COS(solha))/PI180

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE total( sun_ang,cc,qr )

!       Calculates the total incoming solar radiation from cloud cover and
!       solar elevation angle using the Holtslag-Van Ulden Technique.

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: sun_ang, cc
REAL, INTENT( OUT ) :: qr

REAL, PARAMETER :: R0 = 990.
REAL, PARAMETER :: C0 = 30.
REAL, PARAMETER :: B1 = 0.75
REAL, PARAMETER :: B2 = 3.4

!------ sun_ang assumed .ge. 0 (in degrees)

qr = R0*SIN(sun_ang*PI180) - C0

IF( cc > 0. )qr = qr*(1.0-B1*(cc**B2))

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE hdayus( uref,tref,zref,zo,cc,hc,alphac,lhvs,hvs,ustar,L )

USE constants_fd
USE molfunc_fi

!------ Calculates ustar and L for the unstable cases (L<0) using the
!       Holtslag-Van Ulden technique.

!------ Stable and neutral cases added for lhvs=T cases

IMPLICIT NONE

REAL,    INTENT( IN    ) :: uref, tref, zref
REAL,    INTENT( IN    ) :: zo, cc, hc, alphac
LOGICAL, INTENT( IN    ) :: lhvs
REAL,    INTENT( INOUT ) :: hvs
REAL,    INTENT( INOUT ) :: ustar, L

INTEGER ierr
REAL    L1, Lmin, Li, L1i, Tstar

REAL, EXTERNAL :: Lfunc1, Lfunc3, ulog

!------ Special case for zero speed

IF( uref == 0. )THEN
  ustar = 1.E-6
  IF( .NOT.lhvs )hvs = 0.
  L = SIGN( 10.*zo,hvs )
  GOTO 9999
END IF

IF( hvs > 0. .OR. lhvs )THEN

!------ Unstable or fixed heat flux: iterate on L and ustar
!       (except for neutral conditions)

  IF( hvs == 0.0 )THEN
    L = 0.0
    ustar = VONK*uref/ulog( hc,alphac,zref,zo,L )
    L = 1.0E4
    GOTO 9999
  END IF

!------ Initial bracketing values

  Lmin = 2.*MAX(10.*zo,hc)
  IF( hvs > 0. )THEN
    L1   = -1.E+4
    Lmin = -2.*MAX(10.*zo,hc)
  ELSE
    L1   =  1.E+4
    Lmin = 2.*MAX(10.*zo,hc)
  END IF

!------ Setup structure for MOL function

  f%zref   = zref
  f%uref   = uref
  f%tref   = tref
  f%zruf   = zo
  f%hc     = hc
  f%alphac = alphac
  f%bflx   = G0*hvs/tref
  f%vonk   = VONK
  f%Lmin   = Lmin
  f%cc     = cc
  f%VHlim  = 0.09*(1.-0.5*cc**2)*G0/tref

!------ Iterate

  IF( ustar > 0. )THEN
    Li  = -VONK*f%bflx/ustar**3
    L1i = 1./ SIGN(1.E-3,-hvs)
    IF( Lfunc1(Li)*Lfunc1(L1i) > 0. )L1i = 0.
  ELSE
    Li  = 0.
    L1i = 1./ SIGN(1.E-3,-hvs)
  END IF

  CALL fzero( Lfunc1,Li,L1i,TOL,2.E-5,ierr )
  IF( ierr == 4 )THEN
    Lmin = 2.*MAX(10.*zo,hc)
    L = SIGN(Lmin,-hvs)
    ustar = VONK*uref/ulog( hc,alphac,zref,zo,L )
  ELSE
    ustar = f%ustar
    Li = SIGN(MAX(ABS(Li),TINY(0.)),Li)  !Check for near-neutral
    L  = 1./Li
  END IF

ELSE

!------ Stable: compute heat flux along with ustar and L

  L = 1.E+4 !Initial guess

  CALL wnus( uref,tref,zref,zo,cc,hc,alphac,ustar,L,hvs )

END IF

!------ Compute T* (for stable conditions) and re-iterate if exceeds VH limit

IF( L > 0. )THEN
  Tstar = -hvs/ustar
  f%VHlim = 0.09*(1.-0.5*cc**2)
  IF( Tstar > f%VHlim )THEN
    f%VHlim = f%VHlim*G0/tref
    L  = 1.E-3
    L1 = 1.E+10
    CALL fzero( Lfunc3,L,L1,TOL,2.E-5,ierr )
    IF( ierr == 4 )THEN
      Lmin = 2.*MAX(10.*zo,hc)
      L = Lmin
      ustar = VONK*uref/ulog( hc,alphac,zref,zo,L )
    ELSE
      ustar = f%ustar
    END IF
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE wnus( uref,tref,zref,zo,cc,hc,alphac,ustar,L,qs )

USE default_fd
USE constants_fd
USE molfunc_fi

!------ Calculates ustar and Monin-Obukov length for the stable cases

IMPLICIT NONE

REAL, INTENT( IN    ) :: uref, tref, zref
REAL, INTENT( IN    ) :: zo, cc, hc, alphac
REAL, INTENT( OUT   ) :: ustar, qs
REAL, INTENT( INOUT ) :: L

INTEGER ierr
REAL    L1

REAL, EXTERNAL :: Lfunc2, ulog

!------ Setup structure for MOL function

f%zref   = zref
f%uref   = uref
f%tref   = tref
f%zruf   = zo
f%hc     = hc
f%alphac = alphac
f%bflx   = NOT_SET_R
f%vonk   = VONK
f%VHlim  = 0.09*(1.-0.5*cc**2)*G0/tref
f%Lmin   = 1.E-6

!------ Iterate

L  = f%Lmin
L1 = 1.E+10

CALL fzero( Lfunc2,L,L1,TOL,ATOL,ierr )
IF( ierr == 4 )THEN
  L = 1.E+4
  ustar = VONK*uref/ulog( hc,alphac,zref,zo,L )
ELSE
  ustar = f%ustar
END IF

qs = -tref*ustar**3/(VONK*G0*L)

!------ Set minimum length

L = MAX(L,20.*zo,2.*hc)

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION Lfunc1( Li )

!------ Lfunc1 = 1/L - f(1/L), where 1/f = ustar^3/(Vonk*Bflux)

USE molfunc_fi

IMPLICIT NONE

REAL, INTENT( INOUT ) :: Li

REAL L

REAL, EXTERNAL :: ulog

!------ Compute ustar based on uref and log profile

IF( ABS(Li) < TINY(0.) )THEN
  L = 0.
ELSE
  L = 1./Li
END IF
f%ustar = f%vonk*f%uref / ulog( f%hc,f%alphac,f%zref,f%zruf,L )

Lfunc1 = -f%vonk*f%bflx / f%ustar**3 - Li

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION Lfunc2( L )

!------ Lfunc2 = L - f(L), where f = ustar^3/(Vonk*Bflux) subject to limits on Bflux
!       Used for stable conditions

USE molfunc_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: L

REAL, PARAMETER :: CONST = 0.05 * G0 ! Factor for limit on [ustar*tstar]

REAL fac

REAL, EXTERNAL :: ulog

f%ustar = f%vonk*f%uref/ulog( f%hc,f%alphac,f%zref,f%zruf,L )
fac     = f%ustar**3 / f%vonk
f%bflx  = -MIN(f%VHlim*f%ustar,CONST/f%tref,fac/f%Lmin)
Lfunc2  = L + fac/f%bflx

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION Lfunc3( L )

!------ Lfunc3 = L - f(L), where f = ustar^2/(Vonk*VHlim)
!       Used for stable conditions limited by Venkatram limit

USE molfunc_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: L

REAL, EXTERNAL :: ulog

f%ustar = f%vonk*f%uref/ulog( f%hc,f%alphac,f%zref,f%zruf,L )
Lfunc3  = L - f%ustar**2 / (f%vonk*f%VHlim)

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION fun_rhoa( t,p )

!DEC$ ATTRIBUTES DLLEXPORT :: fun_rhoa

!------ Computes air density using the ideal gas law.
!       t = temperature (K), p = pressure (mb)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t, p

fun_rhoa = p*100./(Rgas*t)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE zill( lat,ustar,L,zil )

!       Calculates the nocturnal boundary layer height.

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat, ustar, L
REAL, INTENT( OUT ) :: zil

REAL xlat, f

REAL, EXTERNAL :: sind

xlat = MAX(ABS(lat),10.)

f = FCOR0*sind(xlat)
IF( ustar >= 0. .AND. L > 0. )THEN
  zil = (-L + SQRT(L*L + 2.28*ustar*L/f))/3.8
ELSE
  zil = 0.3*ustar/f
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_kbl( grid )

!------ set level for ubl arrays above canopy

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

REAL, PARAMETER :: RFAC = 4. / AREA_DENSITY_TO_ALPHA

INTEGER i,j, ii, jj, i0, ii0, is, iis, i1, i2, j1, j2, kbl, alloc_stat
INTEGER nxy, ntot
REAL    x, y, xx, yy, fac, xmap, ymap

REAL, DIMENSION(:), POINTER :: Z

REAL, DIMENSION(:), ALLOCATABLE :: Heff, Reff

REAL, EXTERNAL :: SWIMrlimit

IF( .NOT.ASSOCIATED(grid%landcover%kbl) )THEN
  ALLOCATE( grid%landcover%kbl(grid%nXY),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'set_kbl'
    error%Message = 'Error allocating array'
    GOTO 9999
  END IF
END IF

!------ Define effective canopy height based on influence region

ALLOCATE( Heff(grid%nXY),Reff(grid%nXY),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'set_kbl'
  error%Message = 'Error allocating effective height arrays'
  GOTO 9999
END IF

!$OMP PARALLEL DO PRIVATE(i) SHARED(grid,Heff,Reff)
DO i = 1,grid%nXY
  IF( grid%landcover%canopyHt(i)*grid%landcover%alpha(i) > 0. )THEN
    Heff(i) = grid%landcover%canopyHt(i)
  ELSE
    Heff(i) = 0.
  END IF
  Reff(i) = 10.*grid%landcover%canopyHt(i) &
               *MIN(grid%landcover%alpha(i)*RFAC,1.0)
END DO
!$OMP END PARALLEL DO

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  y  = grid%Ymin + FLOAT(j-1)*grid%dY

  DO i= 1,grid%nX
    is = i0 + i
    x  = grid%Xmin + FLOAT(i-1)*grid%dX

    IF( Reff(is) <= 0. )CYCLE

    CALL SWIMmapfac( grid%coord,x,y,xmap,ymap )
    ii = INT(Reff(is)*xmap/grid%dX+1.)
    jj = INT(Reff(is)*ymap/grid%dY+1.)
    i1 = MAX(i-ii,1); i2 = MIN(i+ii,grid%nX)
    j1 = MAX(j-jj,1); j2 = MIN(j+jj,grid%nY)

    DO jj = j1,j2
      ii0 = (jj-1)*grid%nX
      yy  = (FLOAT(jj-j)*grid%dY/ymap)**2

      DO ii = i1,i2
        iis = ii0 + ii
        xx  = (FLOAT(ii-i)*grid%dX/xmap)**2

        fac = SWIMrlimit( 1.5*(1.-SQRT(xx+yy)/Reff(is)),0.,1. )
        Heff(iis) = MAX(Heff(iis),fac*grid%landcover%canopyHt(is))

      END DO
    END DO

  END DO
END DO

!------ Set reference level to be at least twice effective canopy height

nxy  = grid%nXY
ntot = nxy*grid%nZ

!$OMP PARALLEL DO PRIVATE(i,kbl,Z) SHARED(grid,Heff)
DO i = 1,grid%nXY

  IF( BTEST(grid%type,GTB_Z3D) )THEN
   Z => grid%sigma%Z(i:ntot:nxy)
  ELSE
    Z => grid%Z
  END IF

  kbl = 1

  IF( grid%nZ > 1 )THEN  !Check level wrt canopy height

    DO WHILE( Z(kbl)*grid%terrain%D(i) < Heff(i) .AND. kbl < grid%nZ ) !Increment until
      kbl = kbl + 1                                                    !above canopy height
    END DO

    IF( Z(kbl)*grid%terrain%D(i) < 2.*Heff(i) .AND. kbl < grid%nZ )THEN  !Increment if level is
                                                                         !less than twice canopy height
      DO WHILE( Z(kbl)*grid%terrain%D(i) < 2.*Heff(i) .AND. kbl < grid%nZ )
        kbl = kbl + 1
      END DO

      IF( Z(kbl)*grid%terrain%D(i) > 4.*Heff(i) )THEN
        kbl = kbl - 1  !But we don't want level more than four times canopy height
      END IF

    END IF

  END IF

  grid%landcover%kbl(i) = kbl

END DO
!$OMP END PARALLEL DO

9999 CONTINUE

IF( ALLOCATED(Heff) )DEALLOCATE( Heff,STAT=alloc_stat )
IF( ALLOCATED(Reff) )DEALLOCATE( Reff,STAT=alloc_stat )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_kbl_Z3D( grid,fld,kbls )

!------ Set level for ubl arrays above canopy
!       Version to use with time-varying 3d height field
!       N.B. Does not change fld%grid%landcover%kbl

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetGrid   ),     INTENT( IN  ) :: grid
TYPE( MetMean3D ),     INTENT( IN  ) :: fld
INTEGER, DIMENSION(*), INTENT( OUT ) :: kbls

REAL, PARAMETER :: RFAC = 4. / AREA_DENSITY_TO_ALPHA

INTEGER i, j, ii, jj, i0, ii0, is, iis, i1, i2, j1, j2, kbl, alloc_stat
INTEGER nxy, ntot
REAL    x, y, xx, yy, fac, xmap, ymap

REAL, DIMENSION(:), POINTER :: Z

REAL, DIMENSION(:), ALLOCATABLE :: Heff, Reff

REAL, EXTERNAL :: SWIMrlimit

!------ Set to standard kbl if there is no 3d height field

IF( .NOT.BTEST(grid%type,GTB_Z3D)  )THEN
  DO i = 1,grid%nXY
    kbls(i) = grid%landcover%kbl(i)
  END DO
  GOTO 9999
END IF

!------ Define effective canopy height based on influence region

ALLOCATE( Heff(grid%nXY),Reff(grid%nXY),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'set_kbl'
  error%Message = 'Error allocating effective height arrays'
  GOTO 9999
END IF

!$OMP PARALLEL DO PRIVATE(i) SHARED(grid,Heff,Reff)
DO i = 1,grid%nXY
  IF( grid%landcover%canopyHt(i)*grid%landcover%alpha(i) > 0. )THEN
    Heff(i) = grid%landcover%canopyHt(i)
  ELSE
    Heff(i) = 0.
  END IF
  Reff(i) = 10.*grid%landcover%canopyHt(i) &
               *MIN(grid%landcover%alpha(i)*RFAC,1.0)
END DO
!$OMP END PARALLEL DO

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  y  = grid%Ymin + FLOAT(j-1)*grid%dY

  DO i= 1,grid%nX
    is = i0 + i
    x  = grid%Xmin + FLOAT(i-1)*grid%dX

    IF( Reff(is) <= 0. )CYCLE

    CALL SWIMmapfac( grid%coord,x,y,xmap,ymap )
    ii = INT(Reff(is)*xmap/grid%dX+1.)
    jj = INT(Reff(is)*ymap/grid%dY+1.)
    i1 = MAX(i-ii,1); i2 = MIN(i+ii,grid%nX)
    j1 = MAX(j-jj,1); j2 = MIN(j+jj,grid%nY)

    DO jj = j1,j2
      ii0 = (jj-1)*grid%nX
      yy  = (FLOAT(jj-j)*grid%dY/ymap)**2

      DO ii = i1,i2
        iis = ii0 + ii
        xx  = (FLOAT(ii-i)*grid%dX/xmap)**2

        fac = SWIMrlimit( 1.5*(1.-SQRT(xx+yy)/Reff(is)),0.,1. )
        Heff(iis) = MAX(Heff(iis),fac*grid%landcover%canopyHt(is))

      END DO
    END DO

  END DO
END DO

!------ Set reference level to be at least twice effective canopy height

nxy  = grid%nXY
ntot = nxy*grid%nZ

!$OMP PARALLEL DO PRIVATE(i,Z,kbl) SHARED(grid,Heff,kbls)
DO i = 1,grid%nXY

  Z => grid%sigma%Z(i:ntot:nxy)

  kbl = 1

  IF( grid%nZ > 1 )THEN  !Check level wrt canopy height

    DO WHILE( Z(kbl)*grid%terrain%D(i) < Heff(i) .AND. kbl < grid%nZ ) !Increment until
      kbl = kbl + 1                                                    !above canopy height
    END DO

    IF( Z(kbl)*grid%terrain%D(i) < 2.*Heff(i) .AND. kbl < grid%nZ )THEN  !Increment if level is
                                                                         !less than twice canopy height
      DO WHILE( Z(kbl)*grid%terrain%D(i) < 2.*Heff(i) .AND. kbl < grid%nZ )
        kbl = kbl + 1
      END DO

      IF( Z(kbl)*grid%terrain%D(i) > 4.*Heff(i) )THEN
        kbl = kbl - 1  !But we don't want level more than four times canopy height
      END IF

    END IF

  END IF

  kbls(i) = kbl

END DO
!$OMP END PARALLEL DO

9999 CONTINUE

IF( ALLOCATED(Heff) )DEALLOCATE( Heff,STAT=alloc_stat )
IF( ALLOCATED(Reff) )DEALLOCATE( Reff,STAT=alloc_stat )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetSL( fld )

!------ Set velocity at surface layer height

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, ik
REAL    ufac, urfac, zbl, L

REAL, EXTERNAL :: ulog, LfromInvL

IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                       BTEST(fld%type,FTB_NEST)) )THEN

  DO i = 1,fld%grid%nXY

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
      zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
    ELSE
      zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%d(i)
    END IF

    IF( fld%BLaux%zsl(i) > zbl )THEN

      L = LfromInvL( fld%BL%invMOL(i) )

      ufac = ulog( fld%grid%landcover%canopyHt(i), &
                   fld%grid%landcover%alpha(i), &
                   fld%BLaux%zsl(i), &
                   fld%grid%landcover%roughness(i), &
                   L )

      urfac = ulog( fld%grid%landcover%canopyHt(i), &
                 fld%grid%landcover%alpha(i), &
                 zbl, &
                 fld%grid%landcover%roughness(i), &
                 L )

      fld%BLaux%usl(i) = fld%BLaux%ubl(i)*ufac/urfac
      fld%BLaux%vsl(i) = fld%BLaux%vbl(i)*ufac/urfac

    ELSE

      fld%BLaux%zsl(i) = zbl

      fld%BLaux%usl(i) = fld%BLaux%ubl(i)
      fld%BLaux%vsl(i) = fld%BLaux%vbl(i)

    END IF

  END DO

ELSE

  DO i = 1,fld%grid%nXY

    L = LfromInvL( fld%BL%invMOL(i) )

    ufac = ulog( fld%grid%landcover%canopyHt(i), &
                 fld%grid%landcover%alpha(i), &
                 fld%BLaux%zsl(i), &
                 fld%grid%landcover%roughness(i), &
                 L )

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
      zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
    ELSE
      zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%d(i)
    END IF

    IF( fld%BLaux%zsl(i) > zbl )THEN

      urfac = ulog( fld%grid%landcover%canopyHt(i), &
                 fld%grid%landcover%alpha(i), &
                 zbl, &
                 fld%grid%landcover%roughness(i), &
                 L )

      fld%BLaux%usl(i) = fld%BLaux%ubl(i)*ufac/urfac
      fld%BLaux%vsl(i) = fld%BLaux%vbl(i)*ufac/urfac

    ELSE

      fld%BLaux%usl(i) = fld%BLaux%ubl(i)
      fld%BLaux%vsl(i) = fld%BLaux%vbl(i)

    END IF

  END DO

END IF

RETURN
END

!===============================================================================

SUBROUTINE CheckZi( fld )

!------ Set maximum boundary layer depth (for calculated BL) based on latitude

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i
REAL    lat, ziMax

DO i = 1,fld%grid%nXY
  lat   = MIN(ABS(fld%grid%lat(i)),HILAT)
  ziMax = MAXZI_MIDLAT + MAX(lat-MIDLAT,0.)*DZIDLAT
  fld%BL%zi(i) = MIN(fld%BL%zi(i),ziMax)
END DO

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SurfIter( zref,wspd,tref,pref,zruf,hc,alphac,bowen, &
                               cloud_cover,rn,rlat,ladv_zi,lhflx, &
                               hflx,zi,z_sl,L,ustar,lustar )

USE SWIMparam_fd
USE constants_fd
USE default_fd

IMPLICIT NONE

REAL,    INTENT( IN    ) :: zref, wspd, tref, pref, zruf
REAL,    INTENT( IN    ) :: hc, alphac, bowen, cloud_cover, rn, rlat
REAL,    INTENT( INOUT ) :: hflx, zi
REAL,    INTENT( OUT   ) :: z_sl, L
REAL,    INTENT( INOUT ) :: ustar
LOGICAL, INTENT( IN    ) :: lustar
LOGICAL, INTENT( IN    ) :: ladv_zi, lhflx

REAL rho, zr, hflx0, VHlim

INTEGER, EXTERNAL :: SWIMaddLogMessage
REAL,    EXTERNAL :: fun_rhoa

!------ Set air density

rho = fun_rhoa( tref,pref )

!------ Initialize heat flux (if not fixed)

IF( .NOT.lhflx )THEN
  IF( rn > 0. )THEN
    CALL hv( tref,pref,rn,bowen,hflx )
    hflx = hflx / (rho*CP)  !Fixed for iteration if hflx > 0.
  ELSE
    hflx = -1.              !Set negative: will be reset during iteration
  END IF
END IF

!------ Initialize based on stability

IF( lustar .AND. lhflx )THEN  !Don't go through iteration for ustar
                              !N.B. lustar=T only if lhflx=T
  IF( ABS(hflx) > 1.E-6 )THEN
    L = -ustar**3/(VONK*G0*hflx/tref)
  ELSE
    L = -SIGN(1.E10,hflx)
  END IF

ELSE

  IF( hflx > 0. )THEN
    L = -1000.
    CALL set_zsl( z_sl,zi,L,zruf,hc )
  ELSE
    L    = 1000.
    z_sl = zi
  END IF

  zr  = MIN(zref,z_sl)

!------ Compute L & ustar for fixed (input) or unstable heat flux

  IF( lhflx .OR. hflx > 0. )THEN

    CALL hdayus( wspd,tref,zr,zruf,cloud_cover,hc,alphac,.TRUE.,hflx,ustar,L )

  !------ Otherwse, compute heat flux, L & ustar for for stable conditions

  ELSE

    CALL wnus( wspd,tref,zr,zruf,cloud_cover,hc,alphac,ustar,L,hflx )

  END IF

END IF

!------ Compute BL depth for stable conditions if "advancing"

IF( hflx <= 0. .AND. ladv_zi )THEN
  CALL zill( rlat,ustar,L,zi )
  zi = MAX(zi,20.*zruf,2.*hc)
END IF

!------ Set surface layer depth

CALL set_zsl( z_sl,zi,L,zruf,hc )

!------ Check if iteration required for stable conditions (only if ustar is not
!       input), i.e., when surface layer depth is below reference height

IF( hflx < 0. .AND. .NOT.lustar )THEN
  IF( z_sl < zr )THEN
    IF( .NOT.ladv_zi )zi   = -zi
    IF( lhflx )THEN
      VHlim = 0.09*(1.-0.5*cloud_cover**2)
      hflx0 = MAX(hflx,-VHlim*ustar)
    ELSE
      hflx0 = NOT_SET_R
    END IF
    CALL ZrefIter( wspd,tref,zref,z_sl,zi,zruf,rlat,cloud_cover,hc,alphac,ustar,L,hflx0 )
    IF( .NOT.lhflx )hflx = hflx0
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE ZrefIter( uref,tref,zref,zsl,zi,zo,rlat,cc,hc,alphac,ustar,L,hvs )

USE constants_fd
USE molfunc_fi
USE default_fd

!------ Calculates ustar and Monin-Obukov length for the stable cases

IMPLICIT NONE

REAL, INTENT( IN    ) :: uref, tref, zref
REAL, INTENT( IN    ) :: rlat, zo, cc, hc, alphac
REAL, INTENT( OUT   ) :: ustar
REAL, INTENT( INOUT ) :: L, zsl, zi, hvs

INTEGER ierr
REAL    z1, z2, L1, Tstar

REAL, EXTERNAL :: ZslFunc, ZslFunc2, Lfunc3, ulog

!------ Setup structure for MOL function

f%zref   = zref
f%uref   = uref
f%tref   = tref
f%zruf   = zo
f%hc     = hc
f%alphac = alphac
IF( hvs == NOT_SET_R )THEN
  f%bflx = NOT_SET_R
ELSE
  f%bflx = G0*hvs/tref
END IF
f%vonk   = VONK
f%VHlim  = 0.09*(1.-0.5*cc**2)*G0/tref
f%Lmin   = 1.E-6
f%lat    = rlat
f%cc     = cc

IF( zi < 0. )THEN
  f%lAdvZi = .FALSE.
  zi = -zi
ELSE
  f%lAdvZi = .TRUE.
END IF
f%zi = zi

!------ Bracketing values: zsl & zref

z2 = zsl * 0.5
z1 = zref

IF( hvs == NOT_SET_R )THEN
  CALL fzero( ZslFunc,z1,z2,TOL,ATOL,ierr )
ELSE
  CALL fzero( ZslFunc2,z1,z2,TOL,ATOL,ierr )
END IF

ustar = f%ustar
L     = -ustar**3 / (f%vonk*f%bflx)

!------ Compute T* (for stable conditions) and re-iterate if exceeds VH limit

IF( L > 0. )THEN
  Tstar = -f%bflx*tref/G0/f%ustar
  f%VHlim = 0.09*(1.-0.5*cc**2)
  IF( Tstar > f%VHlim )THEN
    f%VHlim = f%VHlim*G0/tref
    L  = 0.001
    L1 = 1.E+10
    CALL fzero( Lfunc3,L,L1,TOL,2.E-5,ierr )
    IF( ierr == 4 )THEN
      L = 2.*MAX(10.*zo,hc)
      ustar  = VONK*uref/ulog( hc,alphac,z1,zo,L )
    ELSE
      ustar = f%ustar
    END IF
  END IF
END IF

IF( hvs == NOT_SET_R )hvs = f%bflx*tref/G0

IF( f%lAdvZi )CALL zill( rlat,ustar,L,zi )
CALL set_zsl( zsl,zi,L,zo,hc )

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION ZslFunc2( zref )

USE constants_fd
USE molfunc_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: zref

INTEGER ierr
REAL    Li, L1i, Lmin, L, zsl

REAL, EXTERNAL :: Lfunc1, ulog

f%zref  = zref
f%VHlim = 0.09*(1.-0.5*f%cc**2)*G0/f%tref

Lmin = 2.*MAX(10.*f%zruf,f%hc)
Li   = 0.
L1i  = 1./Lmin

CALL fzero( Lfunc1,Li,L1i,TOL,2.E-5,ierr )
IF( ierr == 4 )THEN
  L = SIGN(Lmin,-f%bflx)
  f%ustar  = VONK*f%uref/ulog( f%hc,f%alphac,f%zref,f%zruf,L )
ELSE
  Li = SIGN(MAX(ABS(Li),TINY(0.)),Li)  !Check for near-neutral
  L  = 1./Li
END IF

IF( f%lAdvZi )CALL zill( f%lat,f%ustar,L,f%zi )

CALL set_zsl( zsl,f%zi,L,f%zruf,f%hc )

ZslFunc2 = zsl - zref

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION ZslFunc( zref )

USE constants_fd
USE molfunc_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: zref

INTEGER ierr
REAL    L1, L, zsl

REAL, EXTERNAL :: Lfunc2

f%zref  = zref
f%VHlim = 0.09*(1.-0.5*f%cc**2)*G0/f%tref

L1 = 1.E+10
L  = f%Lmin
CALL fzero( Lfunc2,L,L1,TOL,ATOL,ierr )

IF( f%lAdvZi )CALL zill( f%lat,f%ustar,L,f%zi )

CALL set_zsl( zsl,f%zi,L,f%zruf,f%hc )

ZslFunc = zsl - zref

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SetSurfRef( T_flag,P_flag,Tin,Pin,hp,Tref,Pref )

USE constants_fd
USE SWIMparam_fd

IMPLICIT NONE

REAL pr, tr, tzdum

LOGICAL, INTENT( IN  ) :: T_flag, P_flag
REAL,    INTENT( IN  ) :: Tin,    Pin, hp
REAL,    INTENT( OUT ) :: Tref,   Pref

IF( .NOT.(T_flag .AND. P_flag) )CALL stnd_atmos( hp,pr,tr,tzdum,SA_TA )

IF( T_flag )THEN
 Tref = Tin
ELSE
  Tref = tr
END IF

IF( P_flag )THEN
  Pref = Pin
ELSE
  Pref = pr*PSURF
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetBLfromMOL( invL,zruf,hc,alpha,lSetZi,L,Zi,Zsl,Zref,us2 )

!------ Set BL parameters from inverse Monin-Obukhov length

USE constants_fd

IMPLICIT NONE

REAL,    INTENT( IN    ) :: invL       !1/L
REAL,    INTENT( IN    ) :: zruf       !Roughness
REAL,    INTENT( IN    ) :: hc, alpha  !Canopy height and velocity parameter
LOGICAL, INTENT( IN    ) :: lSetZi     !Output Zi flag
REAL,    INTENT( OUT   ) :: L
REAL,    INTENT( INOUT ) :: Zi
REAL,    INTENT( OUT   ) :: Zsl        !Surface layer depth
REAL,    INTENT( IN    ) :: Zref       !Reference height for wind speed
REAL,    INTENT( INOUT ) :: us2        !In wind speed squared; Out ustar**2

REAL, EXTERNAL :: LfromInvL, ulog

L = LfromInvL( invL )

IF( lSetZi )THEN
  IF( L <= 0. )THEN
    Zi = 1000.
  ELSE
    Zi = MAX(MIN(5.*L,1000.),hc+10.*zruf)
  END IF
END IF

CALL set_zsl( Zsl,Zi,L,zruf,hc )

us2 = us2*(vonk/ulog( hc,alpha,MIN(Zsl,Zref),zruf,L ))**2

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION LfromInvL( invL ) RESULT( L )

!------ Set L from inverse Monin-Obukhov length

IMPLICIT NONE

REAL,    INTENT( IN ) :: invL       !1/L

L = 1./SIGN(MAX(ABS(invL),1.E-4),invL)

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION InvLfromL( L ) RESULT( invL )

!------ Set inverse Monin-Obukhov from L

IMPLICIT NONE

REAL,    INTENT( IN ) :: L

invL = 1./SIGN(MIN(ABS(L),1.E+4),L)

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION ComputeNetRadiation( tLocal,jul,lat,tref,cc,albedo ) RESULT( rn )

!------ Compute neat radiation based on heat balance

USE SWIMparam_fd

IMPLICIT NONE

REAL,    INTENT( IN ) :: tLocal   !Solar time (in hours)
INTEGER, INTENT( IN ) :: jul      !Julian day
REAL,    INTENT( IN ) :: lat      !Latitude
REAL,    INTENT( IN ) :: tref     !Reference temperature
REAL,    INTENT( IN ) :: cc       !Cloud cover
REAL,    INTENT( IN ) :: albedo   !Albedo

REAL sun_ang, qr

INTERFACE
  SUBROUTINE SunRiseSet( jul,lat,n,tsr,tss )
    INTEGER,            INTENT( IN  ) :: jul, n
    REAL, DIMENSION(:), POINTER       :: lat
    REAL, DIMENSION(:), POINTER       :: tsr, tss
  END SUBROUTINE SunRiseSet
END INTERFACE

REAL, DIMENSION(1), TARGET  :: rlat, sr, ss
REAL, DIMENSION(:), POINTER :: plat, psr, pss

plat => rlat; psr => sr; pss => ss

rn = 0.

IF( jul < 1 .OR. ABS(lat) > 90. )RETURN

rlat(1) = lat

CALL SunRiseSet( jul,plat,1,psr,pss )

IF( tLocal > sr(1) .AND. tLocal < ss(1) )THEN
  CALL sun( rlat(1),jul,tLocal,sun_ang )
  CALL total( sun_ang,cc,qr )
  CALL hvnet( sun_ang,albedo,cc,qr,tref,rn )
END IF

RETURN
END

!==============================================================================

SUBROUTINE SunRiseSet( jul,lat,n,tsr,tss )

!------ Calculate time of sunrise and sunset on the prime meridian

USE constants_fd

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: jul, n
REAL, DIMENSION(:), POINTER       :: lat
REAL, DIMENSION(:), POINTER       :: tsr, tss

REAL, PARAMETER :: YRFAC = 2.*PI/365.242
REAL, PARAMETER :: DEGHR = 15.
REAL, PARAMETER :: HFAC  = 1./DEGHR/PI180

INTEGER i
REAL    day, snd, sigma, capd, tcapd, tem, hsr, arg

day   = (FLOAT(jul)-1.)*YRFAC
snd   = SIN(day)
sigma = 4.871 + day + 0.033*snd
capd  = ASIN(0.398*SIN(sigma))
tcapd = TAN(capd)
tem   = 0.043*SIN(2.*sigma) - 0.033*snd

DO i = 1,n
  arg    = -TAN(lat(i)*PI180)*tcapd
  IF( arg <= -1. )THEN
    tss(i) =  25.
    tsr(i) = -1.
  ELSE IF( arg >= 1. )THEN
    tss(i) = -1.
    tsr(i) =  25.
  ELSE
    hsr    = ACOS(arg)
    tss(i) = 12. + ( hsr - tem)*HFAC
    tsr(i) = 12. + (-hsr - tem)*HFAC
  END IF
END DO

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_fsl( z,zsl,hc,alphac,zruf,xml,f,fsl )

!----- Set surface layer shape function

IMPLICIT NONE

REAL, INTENT( IN    ) :: z
REAL, INTENT( IN    ) :: zsl, hc, alphac, zruf, xml
REAL, INTENT( OUT   ) :: f
REAL, INTENT( INOUT ) :: fsl

REAL, EXTERNAL :: ulog

IF( z < zsl )THEN
  f = ulog( hc,alphac,z,zruf,xml )
ELSE
  IF( fsl > 0. )THEN
    f = fsl
  ELSE
    f   = ulog( hc,alphac,zsl,zruf,xml )
    fsl = f
  END IF
END IF

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE vel_prof( z,u,mol,zruf,hc,alphac,zsl,usl )

!------ compute velocity based on surface layer profile

IMPLICIT NONE

REAL, INTENT( IN    ) :: z, mol, zruf, hc, alphac, zsl, usl
REAL, INTENT( INOUT ) :: u

IF( z < zsl )THEN
  CALL get_ubl( zsl,usl,mol,zruf,hc,alphac,z,u )
ELSE
  u = usl
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE get_ubl( zsl,usl,xml,zruf,hc,alphac,z,ubl )

!------ scale velocity u(zsl) to ubl=u(z) using surface layer profile

IMPLICIT NONE

REAL, INTENT( IN  ) :: zsl, usl, xml, zruf, hc, alphac, z
REAL, INTENT( OUT ) :: ubl

REAL, EXTERNAL :: ulog

ubl = usl * ulog( hc,alphac,z,zruf,xml ) / ulog( hc,alphac,zsl,zruf,xml )

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION ulog( hc,alpha,z,zruf,L )

!------ surface layer velocity profile shape function (log term + correction)

IMPLICIT NONE

REAL, INTENT( IN ) :: hc, alpha, z, zruf, L

REAL d, fac, ufac, efac, zr, Lmag, MOL

REAL, EXTERNAL :: canopy_fac, psi

zr   = MAX( z,zruf )
IF( L == 0. )THEN
  MOL = 0.
ELSE
  Lmag = MAX( ABS(L),10.*zruf )
  MOL  = SIGN( Lmag,L )
END IF

IF( hc <= zruf .OR. alpha <= 0. )THEN
  ulog = LOG(zr/zruf+1.) - psi( zr,MOL )
ELSE
  fac = canopy_fac( alpha )
  d   = 0.7*fac*hc
  IF( zr >= hc )THEN
    ulog = LOG((zr-d)/zruf+1.) - psi( zr-d,MOL )
  ELSE
    ulog = LOG((hc-d)/zruf+1.) - psi( hc-d,MOL )
    ufac = LOG(zr/zruf+1.) - psi( zr,MOL )
    ufac = ufac/(LOG(hc/zruf) - psi( hc,MOL ))
    efac = EXP(alpha*(z/hc - 1.))
    ulog = ((1.-fac)*ufac + fac*efac) * ulog
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION canopy_fac( alp )

IMPLICIT NONE

REAL, PARAMETER :: ALP0 = 0.5

REAL, INTENT( IN ) :: alp

canopy_fac = 1. - EXP(-alp*alp/(ALP0*ALP0+ALP0*alp))

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION uzlog( hc,alpha,z,zruf,L )

!------ surface layer velocity gradient shape function

IMPLICIT NONE

REAL, INTENT( IN ) :: hc, alpha, z, zruf, L

REAL d, fac, ulog, ufac, efac, Lmag, MOL

REAL, EXTERNAL :: canopy_fac, psi, phi

Lmag = MAX( ABS(L),10.*zruf )
MOL  = SIGN( Lmag,L )

IF( hc <= 0. )THEN
  uzlog = phi( z,MOL )/(z+zruf)
ELSE
  fac = canopy_fac( alpha )
  d   = 0.7*fac*hc
  IF( z >= hc )THEN
    uzlog = phi( z-d,MOL )/(z - d)
  ELSE
    ulog  = LOG((hc-d)/zruf+1.) - psi( hc-d,MOL )
    ufac  = phi( z,MOL )/((z+zruf)*(LOG(hc/zruf)-psi( hc,MOL )))
    efac  = EXP(alpha*(z/hc - 1.)) * alpha/hc
    uzlog = ((1.-fac)*ufac + fac*efac) * ulog
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION psi( z,L )

!------ correction term to log profile

IMPLICIT NONE

REAL, PARAMETER :: HPI = 3.141593/2.

REAL, INTENT( IN ) :: z, L

REAL zeta, X

IF( L == 0. )THEN
  psi = 0.
  RETURN
END IF

zeta = z/L

!------ neutral

IF( ABS(zeta) < 1.E-4 )THEN

  psi = 0.

!------ unstable

ELSE IF( zeta < 0. )THEN

  X   = (1. - 15.*zeta)**0.25
  psi = 2.*LOG((1.+X)/2.) + LOG((1.+X*X)/2.) - 2.*ATAN(X) + HPI

!------ stable

ELSE

  psi = -17.*(1.-EXP(-0.29*zeta))

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION phi( z,L )

!------ non-dimensional velocity gradient

IMPLICIT NONE

REAL, PARAMETER :: HPI = 3.141593/2.

REAL, INTENT( IN ) :: z, L

REAL zeta

IF( L == 0. )THEN
  phi = 1.
  RETURN
END IF

zeta = z/L

!------ neutral

IF( ABS(zeta) < 1.E-4 )THEN

  phi = 1.

!------ unstable

ELSE IF( zeta < 0. )THEN

  phi = 1./(1. - 15.*zeta)**0.25

!------ stable

ELSE

  phi = 1. + 4.93*zeta*EXP(-0.29*zeta)

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION phiz( z,L )

!------ non-dimensional gradient of non-dimensional velocity gradient
!           phiz = zeta * d(phi)/d(zeta) / phi

IMPLICIT NONE

REAL, PARAMETER :: HPI = 3.141593/2.

REAL, INTENT( IN ) :: z, L

REAL zeta

REAL, EXTERNAL :: phi

IF( L == 0. )THEN
  phiz = 0.
  RETURN
END IF

zeta = z/L

!------ neutral

IF( ABS(zeta) < 1.E-4 )THEN

  phiz = 0.

!------ unstable

ELSE IF( zeta < 0. )THEN

  phiz = 3.75*zeta/(1. - 15.*zeta)

!------ stable

ELSE

  phiz = (1.0-1.0/phi(z,L))*(1.0-0.29*zeta)

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION tlog( zref,zruf,L )

!------ surface layer temperature profile shape function (log term + correction)

IMPLICIT NONE

REAL, PARAMETER :: Pr = 0.74

REAL, INTENT( IN ) :: zref, zruf, L

REAL, EXTERNAL :: psi_h

tlog = Pr*(LOG(zref/zruf+1.) - psi_h( zref,L) )

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION psi_h( z,L )

!------ correction to log temperature profile

IMPLICIT NONE

REAL, INTENT( IN ) :: z, L

REAL zeta

!------ L = 0 (neutral)

IF( L == 0. )THEN
  psi_h = 0.
  RETURN
END IF

zeta = z/L

!------ neutral

IF( ABS(zeta) < 1.E-4 )THEN

  psi_h = 0.

!------ unstable

ELSE IF( zeta < 0. )THEN

  psi_h = 2.*LOG(0.5*(1.+SQRT(1.-9.*zeta)))

!------ stable

ELSE

  psi_h = -4.7*MIN(zeta,1.)

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION dtdz_fun( hflux,ustar2,zref,L,zruf,lSetHeatFlux )

USE constants_fd

IMPLICIT NONE

REAL,    INTENT( IN    ) :: ustar2, zref, L, zruf
REAL,    INTENT( INOUT ) :: hflux
LOGICAL, INTENT( IN    ) :: lSetHeatFlux

REAL thstar, zlim

REAL, EXTERNAL :: phi_h

!------ neutral

IF( L == 0. )THEN

  dtdz_fun = 0.

  IF( lSetHeatFlux )hflux = 0.

!------ diabatic

ELSE

  IF( lSetHeatFlux )hflux = -SQRT(ustar2)**3 / (vonk*G0/300.*L)

  thstar =  ustar2 / (vonk*G0/300.*L)

  zlim = MAX(zref,zruf)

  dtdz_fun = phi_h( zlim,L ) * thstar/(vonk*zlim)

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION phi_h( z,L )

!------ non-dimensional temperature gradient

IMPLICIT NONE

REAL, PARAMETER :: Pr = 0.74

REAL, INTENT( IN ) :: z, L

REAL zeta

!------ L = 0 (neutral)

IF( L == 0. )THEN
  phi_h = Pr
  RETURN
END IF

zeta = z/L

!------ neutral

IF( ABS(zeta) < 1.E-4 )THEN

  phi_h = Pr

!------ unstable

ELSE IF( zeta < 0. )THEN

  phi_h = Pr/SQRT(1.-9.*zeta)

!------ stable

ELSE

  phi_h = Pr + 4.7*zeta

END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_zsl( z_sl,zi,L,zruf,hc )

!------ Set surface layer height based on L and Zi

!DEC$ ATTRIBUTES DLLEXPORT :: set_zsl

IMPLICIT NONE

REAL, PARAMETER :: ALP0 = 0.1
REAL, PARAMETER :: DALP = 0.9

REAL, INTENT( IN  ) :: zi, L, zruf, hc
REAL, INTENT( OUT ) :: z_sl

REAL alp, r

IF( L <= 0. )THEN

!------ unstable or neutral

  z_sl = ALP0*zi

ELSE

!------ stable

  r    = zi/L
  alp  = ALP0 + DALP*(1.0-EXP(-r))
  z_sl = alp*zi

END IF

z_sl = MAX(z_sl,MAX(hc,0.)+10.*zruf)
z_sl = MIN(z_sl,zi)

RETURN
END

!==============================================================================

INTEGER FUNCTION SetBLType( fld )

!------- Set and/or check BL type based on available met input

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv
REAL    lon, lat

CHARACTER(128) string

INTEGER, EXTERNAL :: SWIMaddLogMessage, BLoperLogMsg, SWIMgetLL
LOGICAL, EXTERNAL :: HasPrjReference

SetBLType = SWIMfailure

fld%BLtype = Prj%BL%type

!------ Set operational BL

IF( fld%BLtype == BLP_OPER )THEN

  IF( (BTEST(fld%type,FTB_UU) ) )THEN
    fld%BLtype = BLP_PROF
  ELSE IF( (BTEST(fld%type,FTB_ZI) .AND. BTEST(fld%type,FTB_HFLX)) .OR. &
       BTEST(fld%type,FTB_MOL) )THEN
    fld%BLtype = BLP_MET
  ELSE
    fld%BLtype = BLP_CALC
  END IF

  irv = BLoperLogMsg( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------- Check if BL input is available for 'met' BL

IF( fld%BLtype == BLP_MET )THEN

  IF( .NOT.((BTEST(fld%type,FTB_ZI) .AND. BTEST(fld%type,FTB_HFLX)) .OR. &
       BTEST(fld%type,FTB_MOL)) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetBLType'
    error%Message = 'Boundary layer fields not available from met input'
    GOTO 9999
  END IF

!------- Check if BL lat/lon & julian day input for calculated BL

ELSE IF( fld%BLtype == BLP_CALC )THEN

  IF( SWIMgetLL( fld%grid%Xmin,fld%grid%Ymin,lon,lat ) == SWIMfailure )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetBLType'
    error%Message = 'Cannot do calculated boundary layer'
    error%Inform  = 'Unable to set lat/lon'
  END IF

  IF( Prj%julStart == NOT_SET_I .OR. Prj%julStart <= 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetBLType'
    error%Message = 'Cannot do calculated boundary layer'
    error%Inform  = 'Julian day input required'
  END IF

!------ Try to set 'Simple BL' for operational input if can't do calculated BL

  IF( error%Number /= NO_ERROR )THEN

    IF( Prj%BL%type /= BLP_OPER )GOTO 9999

!------ Check if zi and heat flux parameters are set

    IF( Prj%BL%ZImin == NOT_SET_R .OR. Prj%BL%ZImax == NOT_SET_R .OR. &
        Prj%BL%ZImin <= 0.        .OR. Prj%BL%ZImax <= 0.            )GOTO 9999
    IF( Prj%BL%HFLXmin == NOT_SET_R .OR. Prj%BL%HFLXmax == NOT_SET_R )GOTO 9999

    fld%BLtype = BLP_SBL

    WRITE(string,"(12X,' *** Cannot set BL Type to CALC')")
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

    WRITE(string,"(12X,' *** Operational BL Type = SIMPLE')")
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

    CALL SWIMclearError()

  END IF

ELSE IF( fld%BLtype == BLP_SBL )THEN

  IF( Prj%BL%ZImin == NOT_SET_R .OR. Prj%BL%ZImax == NOT_SET_R .OR. &
      Prj%BL%ZImin <= 0.        .OR. Prj%BL%ZImax <= 0.            )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetBLType'
    error%Message = 'Invalid BL depth parameters for SIMPLE BL'
    GOTO 9999
  END IF

  IF( Prj%BL%HFLXmin == NOT_SET_R .OR. Prj%BL%HFLXmax == NOT_SET_R )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetBLType'
    error%Message = 'Invalid heat flux parameters for SIMPLE BL'
    GOTO 9999
  END IF

END IF

!------ Check time zone for calculated BL

IF( fld%BLtype == BLP_CALC .AND. Prj%localMet )THEN
  IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
    error%Number   = IV_ERROR
    error%Routine  = 'SetBLType'
    error%Message  = 'Cannot do calculated boundary layer'
    error%Inform   = 'Time zone must be set'
    GOTO 9999
  END IF
END IF

!------ Check time zone for SBL with UTC project

IF( fld%BLtype == BLP_SBL .AND. .NOT.Prj%localMet )THEN
  IF( SWIMgetLL( fld%grid%Xmin,fld%grid%Ymin,lon,lat ) == SWIMfailure )THEN
    IF( Prj%TimeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
      error%Number   = IV_ERROR
      error%Routine  = 'SetBLType'
      error%Message  = 'Cannot do simple boundary layer in UTC project'
      error%Inform   = 'Time zone must be set'
      GOTO 9999
    END IF
  END IF
END IF

SetBLType = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION BLoperLogMsg( fld )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( IN ) :: fld

CHARACTER(128) string

INTEGER irv

INTEGER, EXTERNAL :: SWIMaddLogMessage, getFieldIndex

BLoperLogMsg = SWIMfailure

IF( fld%BLtype == BLP_CALC )THEN
  WRITE(string,100) getFieldIndex( fld%index ),'CALC'
ELSE IF( fld%BLtype == BLP_PROF )THEN
  WRITE(string,100) getFieldIndex( fld%index ),'PROF'
ELSE
  WRITE(string,100) getFieldIndex( fld%index ),'MET INPUT'
END IF
100 FORMAT( 'Met Field ',I2,' *** Operational BL Type = ',A )
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%type,FTB_ZI) )THEN
  string = '                 Using observed mixing heights'
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF
IF( BTEST(fld%type,FTB_HFLX) )THEN
  string = '                 Using observed surface heat flux'
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF
IF( BTEST(fld%type,FTB_MOL) )THEN
  string = '                 Using observed Monin-Obukov length'
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

BLoperLogMsg = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetPrjBL()

!------ Set "not set" surface parameters based on landuse category

USE SWIM_fi
USE SWIMparam_fd
USE landuse_fd
USE checkErr_fd

IMPLICIT NONE

INTEGER irv, season, mode
REAL    x, y, lon, lat
REAL    zruf, hc, alphac, albedo, bowen

CHARACTER(80) string

TYPE( landuse_init ) file_landuse
TYPE( MapCoord     ) coord
INTEGER, EXTERNAL :: InitLandUse, SetLandUse, ExitLandUse, GetSeason
INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: SWIMaddLogMessage
LOGICAL, EXTERNAL :: SpecialValueReal

!------ Skip during Create

IF( Prj%create )GOTO 9999

!------ Check that domain is defined

mode = IBSET(0,DEFAULT_BIT); mode = IBSET(mode,NOTSET_BIT);mode = IBSET(mode,DEFER_BIT);
IF( SpecialValueReal(mode,prj%Xmin) .OR. SpecialValueReal(mode,prj%Xmax) .OR. &
    SpecialValueReal(mode,prj%Ymin) .OR. SpecialValueReal(mode,prj%Ymax) )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetPrjBL'
  error%Message = 'Error checking/setting operational roughness'
  error%Inform  = 'Computational domain must be set'
  GOTO 9999
END IF

IF(  Prj%BL%zruf   == NOT_SET_R .OR. &
     Prj%BL%Bowen  == NOT_SET_R .OR. &
     Prj%BL%albedo == NOT_SET_R )THEN

  IF( Prj%BL%i_cat == NOT_SET_I .OR. Prj%BL%i_cat == 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetPrjBL'
    error%Message = 'Surface roughness/Bowen/albedo and landuse category cannot both be NOT SET'
    GOTO 9999
  END IF

END IF

!------ Initialize landuse dll

file_landuse%lun  = 0
file_landuse%file = TRIM(Prj%MC%LandUseFile)

irv = InitLandUse( file_landuse )
IF( irv /= 1 )THEN
  CALL SWIMgetLandUseError()
  GOTO 9999
END IF

!------ Get season (need latitude)

CALL SWIMinitCoord( coord )
coord%type = I_LATLON

x = 0.5*(prj%Xmin + prj%Xmax)
y = 0.5*(prj%Ymin + prj%Ymax)

lon = 0.; lat = 0.
irv = SWIMcnvCoord( x,y,PrjCoord,lon,lat,coord )

season = GetSeason( lat,prj%julStart )
IF( season < 1 )THEN
  error%Number   = IV_ERROR
  error%Routine = 'SetPrjBL'
  error%Message = 'Error setting season'
  error%Inform  = 'Check domain coordinates'
  GOTO 9999
END IF

!------ Get surface parameters for project landuse category; set those not set

irv = SetLandUse( Prj%BL%i_cat,season,prj%BL%i_wet,zruf,hc,alphac,albedo,bowen )

IF( Prj%BL%zruf == NOT_SET_R .AND. Prj%BL%hc <= 0. )THEN
  Prj%BL%zruf = zruf
  WRITE(string,'(A,ES10.3)') 'Project roughness set to',zruf
  irv = SWIMaddLogMessage( string )
END IF
IF( Prj%BL%Bowen  == NOT_SET_R )Prj%BL%Bowen  = bowen
IF( Prj%BL%albedo == NOT_SET_R )Prj%BL%albedo = albedo

9999 CONTINUE

RETURN
END
