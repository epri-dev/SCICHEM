SUBROUTINE set_stack_rel_prise( release )
!******************************************************************************
!
! FUNCTION:  Set true release parameters for STACK release
!            Use plume rise formula
!            STACK release contains EXIT DIAMETER (m) in size
!                                   EXIT TEMPERATURE (C) in buoy
!                                   EXIT VELOCITY in wmom
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!******************************************************************************

! --- MODULES

use scipuff_fi
use met_fi
use files_fi

implicit none

TYPE( releaseT ), INTENT( IN ) :: release

! --- LOCALS

real tstack, tab_stk
real ddz, thv1, thv2
real hp, dstack
real zbar

integer ipbl, istk, k, nzp, invflg, alloc_stat

REAL, ALLOCATABLE, DIMENSION(:) ::  dthdz, ta, wspd
REAL, ALLOCATABLE, DIMENSION(:) ::  zh, zf, zstk

real  wstk, zplm, sigz0
real  hflx

logical lprcap

REAL                      :: sigx,sigy,sigz,sigRxy,sigRxz,sigRyz,buoy,cmass
REAL, DIMENSION(3)        :: mom

!------ Set vertical grid (use existing one, if exists)
nzp = 0
IF( nzMC > 0 )nzp = MAXLOC(zMC,1)
IF( nzp < 34 )nzp = 34

ALLOCATE( zh(nzp), zf(0:nzp), zstk(nzp), STAT = alloc_stat )
IF( alloc_stat /= 0 ) THEN
  nError = UK_ERROR
  eRoutine='set_stack_rel_prise'
  eMessage='Error allocating dthdz array'
  WRITE(eInform,*)'nzp = ', nzp
  eAction  = char(0)
  IF( nError /= NO_ERROR ) GOTO 9999
END IF

zf(0)	= 0.0
IF( nzp == 34 )THEN
  zf = (/0.,10.,20.,30.,40.,60.,80.,100.,120.,160.,200.,240.,280.,360.,440.,520.,600.,760.,920.,1080.,1240.,1560.,&
        1880.,2200.,2520.,3160.,3800.,4440.,5080.,6000.,7000.,8500.,11000.,14000.,20000./)
ELSE
  DO k = 1,nzp
    zf(k) = zMC(k)
  END DO
END IF

ALLOCATE( dthdz(nzp), ta(nzp), wspd(nzp), STAT = alloc_stat )
IF( alloc_stat /= 0 ) THEN
  nError = UK_ERROR
  eRoutine='set_stack_rel_prise'
  eMessage='Error allocating dthdz array'
  WRITE(eInform,*)'nzp = ', nzp
  eAction  = char(0)
  IF( nError /= NO_ERROR ) GOTO 9999
END IF

CALL set_plmrs_grid(zf,zh,zstk,nzp,ipbl,istk,hp,release%xrel,release%yrel,release%zrel)

! ---- Set stack parameters

CALL get_met(SNGL(release%xrel),SNGL(release%yrel),release%zrel,0.,0.,1 )

CALL getReleaseSigmas( release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )
CALL getReleaseDynamics( release,buoy,mom )
CALL getReleaseMass( release,cmass )

pb      = pb/1013.25                        ! press(atm)
tab_stk = thb
tstack  = buoy + 273.15
wstk    = sqrt(ub*ub + vb*vb)

! --- Set meteorological arrays for plume rise calculation

CALL get_met(SNGL(release%xrel),SNGL(release%yrel),0.,0.,0.,1 )
thv1 = (1. + 0.608*hb)*thb

CALL get_met(SNGL(release%xrel),SNGL(release%yrel),zh(1),0.,0.,1 )
pb       = pb/1013.25                        ! press(atm)
wspd(1)  = sqrt(ub*ub + vb*vb)
ta(1)    = thb
thv2     = (1. + 0.608*hb)*thb
dthdz(1) = MAX((thv2-thv1)/zh(1),0.)

do k = 2, nzp
  ddz = 1./(zh(k) - zh(k-1))
  thv1 = thv2
  call get_met(SNGL(release%xrel),SNGL(release%yrel),zh(k),0.,0.,1 )
  pb       = pb/1013.25                        ! press(atm)
  wspd(k)  = sqrt(ub*ub + vb*vb)
  ta(k)    = thb
  thv2     = (1. + 0.608*hb)*thb
  dthdz(k) = MAX(ddz*(thv2-thv1),0.)
end do

!------ interpolate 2d boundary layer variables: zinv, u* and hflx

hflx   = wts ! wtbl?
dstack = 2.*sigx  ! stack diameter (m) - See comments in set_stack_rel_prime.f90(179)

call plmris( nzp, ipbl, istk, hflx, zinv-hp, dstack, release%zrel,&
                  tstack, mom(3), tab_stk, dthdz, ta, wspd,&
                          zf, zh, zstk, wstk, zplm, invflg )

! Compute plume spread
call plsprd( dthdz, zf, nzp, zplm, sigz0 )

zbar = zplm + hp

if (invflg == 1 .or. invflg == 2) then   !capped
  if (zbar > zinv) then
    write(lun_log,'("Limiting plume rise to inversion height: ",2F12.3)',IOSTAT=alloc_stat)zbar,zinv
    zbar = zinv
  end if
end if

if (zbar < release%zrel + hp) then

  nError = WN_ERROR
  eRoutine='set_stack_rel_prise'
  eMessage='Plume rise calculation failed, zbar < zrel'
  write(eInform,*)'Eff stack ht set equal to stack ht', zbar, release%zrel, release%zrel + hp, hp
  eAction  = char(0)

  call WarningMessage(.true.)
  if (nError /= NO_ERROR) go to 9999

  zbar = release%zrel + hp

end if

!write(lun_log,FMT="('Plume rise calculation (xrel,yrel,zrel,zeff,hp,texit(K),wexit,tamb,vel,nzp): ',1p,9E12.3,0p,1x,I3)",IOSTAT=alloc_stat) &
!                     xrel, yrel, zrel, zbar, hp, tstack, wmom, tab_stk, wstk, nzp

CALL set_prise_rel( sigz0,zbar )

9999 CONTINUE

IF( ALLOCATED(dthdz) )THEN
  DEALLOCATE( dthdz, ta, wspd, STAT = alloc_stat )
  DEALLOCATE( zh, zstk, STAT = alloc_stat )
END IF

RETURN
END

!==============================================================================

SUBROUTINE set_prise_rel( sigz0,zbar )

USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

REAL :: sigz0, zbar

sigy_prm(1) = sigz0
sigz_prm(1) = sigz0
zrel_prm(1) = zbar

RETURN
END

!==============================================================================
subroutine set_plmrs_grid(zf,zh,zstk,nzp,ipbl,istk,hp,xrel,yrel,zrel)
!******************************************************************************
!
! FUNCTION:  Set the vertical grid to be used in the plume rise calculation
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!******************************************************************************

! --- MODULES

use scipuff_fi
use met_fi

implicit none

! --- ARGUMENTS

real    zh(*)         !Layer center height
real    zf(0:nzp)     !Layer surface height
real    zstk(*)       !zf(k) - stack height
integer nzp           !Number of layers
integer ipbl          !Layer of inversion height
integer istk          !Layer of stack height
real    hp            !Terrain height
REAL(8) xrel
REAL(8) yrel
REAL    zrel
! --- LOCALS

integer k
real    hx, hy, delz

!------ Get topography

if (lter) then
  CALL get_topogIn( SNGL(xrel),SNGL(yrel),hp,hx,hy,1 )
else
  hp = 0.
end if

!------ Set vertical grid  (to actual height and not terrain-following, but exclude terrain height)

zstk(1)= zf(1) - zrel
do k = 1, nzp
  zh(k)    = 0.5*(zf(k-1) + zf(k))
  zstk(k)  = zf(k) - zrel
end do

!------ Set vertical levels of the inversion height and the stack height

ipbl = 1
do while (zf(ipbl) .lt. zinv - hp .and. ipbl .lt. nzp)
  ipbl = ipbl + 1
end do

istk = 1
do while (zf(istk) .lt. zrel .and. istk .lt. nzp)
  istk = istk + 1
end do

return
end

SUBROUTINE  plmris( EMLAYS, LPBL, LSTK, HFX, HMIX, STKDM, STKHT, &
                    STKTK, STKVE, TSTK, DTHDZ, TA, WSPD, ZF, ZH, &
                    ZSTK, WSTK, ZPLM, IFLAG )

!***********************************************************************
!
!  DESCRIPTION:
! computes plume rise
!
!  PRECONDITIONS REQUIRED:
! meteorology and stack parameters
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!
!  REVISION  HISTORY:
! Adapted from Sparse Matrix Operator Kernel Emissions (SMOKE)
! Modeling System, File: @(#)plmris.f  1.1
!***********************************************************************

IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

  INTEGER, INTENT (IN) :: EMLAYS    ! no. of emission layers
  INTEGER, INTENT (IN) :: LPBL! lyr of height of PBL, = RADM's KMIX
  INTEGER, INTENT (IN) :: LSTK! lyr of top of stack, = RADM's KSTK
  REAL   , INTENT (IN) :: HFX! sensible heat flux (M K / S )
  REAL   , INTENT (IN) :: HMIX     ! mixing height (m)
  REAL   , INTENT (IN) :: STKDM    ! stack diameter (m)
  REAL   , INTENT (IN) :: STKHT    ! stack height (m)
  REAL   , INTENT (IN) :: STKTK    ! exhaust temperature (deg K)
  REAL   , INTENT (IN) :: STKVE    ! exhaust velocity (m/s)
  REAL   , INTENT (IN) :: TSTK     ! tmptr at top of stack (deg K)
  REAL   , INTENT (IN) :: DTHDZ( EMLAYS )! gradient of THETV
  REAL   , INTENT (IN) :: TA   ( EMLAYS )! temperature (deg K)
  REAL   , INTENT (IN) :: WSPD ( EMLAYS )! wind speed (m/s)
  REAL   , INTENT (IN) :: ZF ( 0:EMLAYS )! layer surface height (m)
  REAL   , INTENT (IN) :: ZH   ( EMLAYS )! layer center height (m)
  REAL   , INTENT (IN) :: ZSTK ( EMLAYS )! zf( l )   - stkht   (m)
  REAL, INTENT(IN OUT) :: WSTK     ! wind speed @ top of stack (m/s)
  REAL, INTENT(OUT)    :: ZPLM     ! effective plume height (m)

  INTEGER, INTENT(OUT) :: IFLAG! inversion penetration flag
                               ! Above cap = 0; Above and below = 1, All below = 2

!...........   PARAMETERS and their descriptions:

  REAL, PARAMETER :: GRAV    = 9.8   ! gravity (m/s2)
  REAL, PARAMETER :: HCRIT   =  1.0E-4 * 0.03  ! hfx min * tolerance
  REAL, PARAMETER :: SMALL   =  1.0E-5   ! Criterion for stability
  REAL, PARAMETER :: D3=  1.0 /  3.0     ! 1/ 3
  REAL, PARAMETER :: D30     =  1.0 / 30.0     ! 1/30
  REAL, PARAMETER :: D1355   =  1.0 /  1.355   ! 1/ 1.355
  REAL, PARAMETER :: D17576  =  1.0 / 17.576   ! 1/17.576
  REAL, PARAMETER :: TWOTHD  =  2.0 /  3.0     ! 2/ 3
  REAL, PARAMETER :: FIVETHD =  5.0 /  3.0     ! 5/ 3S
  REAL, PARAMETER :: NODIV0  =  1.0E-10  ! Prevent divide by zero

!...........   SCRATCH LOCAL VARIABLES and their descriptions:

  INTEGER IQ  !  stab class:  1-unstbl,2-neut,3-stbl
  INTEGER LPLM!  first L: ZH(L) > Plume height ! same as RADM's KPR

  REAL    BFLX      !  buoyancy flux (m**4/s**3)
  REAL    DH        !  plume rise increment to center of the plume
  REAL    DHM       !  plume rise from momentum
  REAL    DHN       !  plume rise for neutral case
  REAL    DHT       !  plume rise increment to the top of the plume
  REAL    HSTAR     !  convective scale at stack (m**2/s**3)
  REAL    P, R, S   !  scratch coefficients
  REAL    RBFLX     !  residual buoyancy flux (m**4/s**3)
  REAL    TPLM      !  temperature at top of plume (K)
  REAL    WPLM      !  wind speed  at top of plume (m/s)
  REAL    ZMIX      !  hmix - hs

!...........   STATEMENT FUNCTIONS:

  REAL    B, H, U     !  arguments

  REAL    NEUTRL        !  neutral-stability plume rise function
  REAL    STABLE        !  stableplume rise function
  REAL    UNSTBL        !  unstable    plume rise function

  NEUTRL( H, B, U ) = &
     MIN( 10.0 * H, &
          1.2 * ( (    144.*B/( U*WSPD(1)*WSPD(1) )   )**0.6 * &  ! pwr 3 * 0.2
          ( H + 1.3 * (144.*B/( U*WSPD(1)*WSPD(1) ) ) )**0.4   )) ! pwr 2 * 0.2

  STABLE( B, U, S ) =  2.6 * ( B / ( U * S ) )**D3

  UNSTBL( B, U )    = 30.0 * ( B / U )**0.6

!***********************************************************************
!   begin body of subroutine  PLMRIS

IFLAG = -1

!.......   Compute buoyancy flux, convective scale.

HSTAR = GRAV * HFX / TA( 1 )   ! Using surface temperature is correct
BFLX  = 0.25*GRAV * ( STKTK-TSTK ) * STKVE * STKDM**2 / STKTK

!.......   Initialize layer of plume
LPLM  = LSTK

!.......   Compute momentum rise
DHM   = 3.0 * STKDM * STKVE / WSTK

!.......   When BFLX <= zero, use momentum rise only
!.......   NOTE: This part of algorithm added based on Models-3 plume rise

IF( BFLX .LE. 0.0 ) THEN

  ZPLM = STKHT + MAX( DHM, +2. )

  RETURN

ENDIF

!.......   Compute initial plume rise from stack top to next level surface:

IF( HSTAR .GT. HCRIT ) THEN     !  unstable case:

  ZMIX = HMIX - STKHT

  IF ( ZMIX .LE. 0.0 ) THEN     !  Stack above mixing height:

    LPLM = MIN( EMLAYS-1, LPBL+1 )
    S    = MAX( GRAV * DTHDZ( LPLM ) / TSTK, SMALL )

!.................  Reset the wind speed at stack to the wind speed at plume
!.................  when the layer of the plume is not equal to the layer of
!.................  the stack.  This is from Models-3, and we have asked
!.................  EPA 10/8/97 why this is done and haven't gotten a response.
    IF( LPLM .NE. LSTK ) THEN
      WSTK = WSPD( LPLM )
      IF( WSTK .EQ. 0. ) WSTK = NODIV0
    ENDIF

    DHN = NEUTRL( STKHT, BFLX, WSTK )
    DH  = STABLE( BFLX, WSTK, S )

!.............  NOTE: The Models-3 version of plume rise recalculates the
!.............  momentum plume rise here with the new WSTK.  We have
!.............  asked EPA on 10/8/97 if this is a bug but have not heard.
!.............  DHM = 3.0 * STKDM * STKVE / WSTK

    IF( DHN .LT. DH ) THEN  ! Take the minimum of neutral and stable
      IQ = 2
      DH = DHN
    ELSE
      IQ = 3
    ENDIF

    IF( DHM .GT. DH ) THEN
      IQ = 4
      DH = DHM
    ENDIF

    DHT = DH

    IFLAG = 0

  ELSE      !  unstable case:

    DH  = UNSTBL( BFLX, WSTK )
    DHN = NEUTRL( STKHT, BFLX, WSTK )

    IF ( DHN .LT. DH ) THEN
      DH = DHN
      IQ = 2
    ELSE
      IQ = 1
    END IF

    IF( DHM .GT. DH ) THEN
      DH = DHM
      IQ = 4
    ENDIF

    DHT = DH

    IFLAG = 2

  END IF

ELSE IF( HSTAR .LT. -HCRIT ) THEN!  stable case:

  S   = MAX( GRAV * DTHDZ( LSTK ) / TSTK, SMALL )
  DHT = STABLE( BFLX, WSTK, S )
  DHN = NEUTRL( STKHT, BFLX, WSTK )

  IF ( DHN .LT. DHT ) THEN
    DHT = DHN
    IQ = 2
  ELSE
    IQ = 3
  END IF

ELSE    !  neutral case:

  DHT = NEUTRL( STKHT, BFLX, WSTK )
  IQ  = 2

END IF  !  hstar ==> unstable, stable, or neutral

!.......   Compute further plume rise from between level surfaces:

RBFLX = BFLX
ZPLM  = DHT

!.......   End calculations if the momentum rise was used in the calculation

IF( IQ .EQ. 4 ) GO TO 199  ! to point past iterative bouyancy loop

!.......   NOTE: LPLM has been initialized at line 145, and may have been
!    reset at line 169
DO !  loop computing further plume rise

  R = ZPLM - ZSTK( LPLM )
  IF( R .LE. 0.0 ) THEN
    EXIT  ! exit plume rise loop

  ELSE IF ( LPLM .LT. EMLAYS ) THEN
    LPLM = LPLM + 1

  ELSE
    ZPLM = MIN( ZPLM, ZF( EMLAYS ) - STKHT )
    EXIT  ! exit plume rise loop

  END IF

!...........   Re-set met data. NOTE: the original RADM code submitted the
!...........   WSPD and TA to a interpolator, but then requested the a height of
!...........   interpolation identical to ZH( LPLM ).

  WPLM = WSPD( LPLM )
  TPLM = TA  ( LPLM )

!...........   Compute residual bflx by stability case IQ:

  IF( IQ .EQ. 1 ) THEN   !  now compute resid bflx by stab case:
    R     = D30 * R
    RBFLX = WPLM * R**FIVETHD
  ELSE IF ( IQ .EQ. 2 ) THEN
    P = STKHT + TWOTHD * ZPLM
    RBFLX = D1355 * R * WPLM * ( ( WSPD( 1 )**2. ) / 144. )  &
          * ( R / P )**TWOTHD
  ELSE  !  else iq = 3:
    RBFLX = D17576 * WPLM * S * R**3
  END IF        !  if stability flag iq is 1, 2, or 3

!...........   Prevent divide-by-zero by WPLM

  IF( WPLM .EQ. 0. ) WPLM = NODIV0

!...........   Process according to stability cases:

  S    = GRAV * DTHDZ( LPLM ) / TPLM
  IF( S .GT. SMALL ) THEN   ! stable case:

    DHT = STABLE( RBFLX, WPLM, S )
    DHN = NEUTRL( STKHT, RBFLX, WPLM )
    IF ( DHN .LT. DHT ) THEN
      DHT = DHN
      IQ  = 2
    ELSE
      IQ  = 3
    END IF

  ELSE    ! if upper layer is not stable, use neutral formula

    DHN =  NEUTRL( STKHT, RBFLX, WPLM )
    DH  = UNSTBL( BFLX, WSTK )
    IQ = 1
    IF ( DHN .LT. DH ) THEN

       DH = DHN
       IQ  = 2

    ENDIF

    DHT = DH

  END IF

  ZPLM = ZSTK( LPLM-1 ) + DHT

END DO   !  end loop computing further plume rise

199     CONTINUE


!.........   Determine actual height of plume centerline after rise
ZPLM = ZPLM + STKHT

RETURN

END SUBROUTINE PLMRIS

SUBROUTINE plsprd( DTHDZ, ZF, KZ, CEFSTK, SIGZ0 )
!***********************************************************************
!  subroutine body starts at line 70
!
!  DESCRIPTION:
!       Calculates the initial vertical spread of a plume; modified
!       from Gillani's model.
!
!  PRECONDITIONS REQUIRED:
!
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!
!  REVISION  HISTORY:
!       Adapted from SMOKE 2.0 code
!
!***********************************************************************

       IMPLICIT NONE

!...........   ARGUMENTS and their descriptions:

       INTEGER, INTENT  (IN) :: KZ             ! number of emissions layers
       REAL,    INTENT  (IN) :: DTHDZ( KZ )    ! potential temperature lapsed rate
       REAL,    INTENT  (IN) :: ZF( 0:KZ )     ! elevation by layer
       REAL,    INTENT  (IN) :: CEFSTK         ! effective stack height
       REAL,    INTENT (OUT) :: SIGZ0          ! Initial plume sigmaz

!...........   PARAMETERS and their descriptions:
       REAL, PARAMETER :: SZ0FAC = 3.545    ! factor used to derive plume depth
       REAL, PARAMETER :: SPRFAC = 15.      ! empirical coefficient for vertical spread
       REAL, PARAMETER :: GAMA   = -0.0098

!...........   Local variables
       INTEGER    K

       REAL :: PLTOP          ! plume top
       REAL :: PLBOT          ! plume bottom

       REAL       DTDZ
       REAL       DPTH

!***********************************************************************
!   begin body of subroutine  PLSPRD

!........  Get ambient temperature above plume rise height (effective stack height)
       K = 0
       DO
           K = K + 1
           IF( K == KZ .OR. CEFSTK <= ZF( K ) ) EXIT
       END DO
       DTDZ  = DTHDZ( K ) + GAMA

!........  Compute initial vertical spread
       SIGZ0 = MAX( 10.0, SPRFAC * EXP( -117. * DTDZ ) )
       DPTH  = SZ0FAC * SIGZ0

!........  Compute plume top and bottom heights; plume is either completely
!          within or outside mixing layer
       PLTOP = CEFSTK + DPTH/2.
       PLBOT = CEFSTK - DPTH/2.

!........  Make sure plume bottom is at least zero
       PLBOT = MAX( 0.0, PLBOT )

       PLTOP = MIN( ZF( KZ ), PLTOP )

       RETURN

       END SUBROUTINE PLSPRD
