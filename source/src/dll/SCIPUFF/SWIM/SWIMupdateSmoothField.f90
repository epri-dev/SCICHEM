INTEGER FUNCTION SWIMupdateSmoothField( fldi,iflag )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), TARGET, INTENT( INOUT ) :: fldi
INTEGER,                  INTENT( IN    ) :: iflag !0=non-velocity only; 1=velocity only; 2=all

INTEGER irv
TYPE( MetField ),   POINTER :: fld, fldp
TYPE( MetGrid ),    POINTER :: grid, gridp
TYPE( MetMean3D),   POINTER :: Fld3d, Fld3dp
TYPE( MetVariance), POINTER :: LSV, LSVp
TYPE( MetBLparam ), POINTER :: BL, BLp
TYPE( MetQLprof),   POINTER :: QL, QLp

INTERFACE

  INTEGER FUNCTION Avg3dVelField( gridp,fldp,grid,fld )
    USE SWIM_fi
    TYPE( MetGrid ),   INTENT( IN ) :: gridp
    TYPE( MetGrid ),   INTENT( IN ) :: grid
    TYPE( MetMean3D ), POINTER      :: fldp
    TYPE( MetMean3D ), POINTER      :: fld
  END FUNCTION Avg3dVelField

  SUBROUTINE Avg3dField( gridp,fldp3d,grid,fld3d,UorV )
    USE SWIM_fi
    TYPE( MetGrid ),        INTENT( IN ) :: gridp
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    REAL, DIMENSION(:),     POINTER      :: fldp3d, fld3d
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Avg3dField

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

  SUBROUTINE SmoothPrcpType( fldp,grid,fld )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),  POINTER      :: fldp, fld
    TYPE( MetGrid ),     INTENT( IN ) :: grid
  END SUBROUTINE SmoothPrcpType

END INTERFACE

INTEGER, EXTERNAL :: getFieldIndex

SWIMupdateSmoothField = SWIMfailure

!------ Point to parent field

fld  => fldi                         !Done to enable pointing to grid
fldp => field(fld%gridSource%unit)

!------ Point to grids

grid  => fld%grid
gridp => fldp%grid

!------ Point to fields; update time and status

IF( BTEST(fld%status,FSB_DOINIT) )THEN
  Fld3d  => fld%Field
  Fld3dp => fldp%Field
  LSV    => fld%LSV
  LSVp   => fldp%LSV
  BL     => fld%BL
  BLp    => fldp%BL
  QL    => fld%QLprof
  QLp   => fldp%QLprof
  fld%t =  fldp%t
ELSE
  IF( BTEST(fldp%status,FSB_UPDATE) )THEN
    fld%status = IBSET(fld%status,FSB_UPDATE)
  ELSE
    fld%status = IBCLR(fld%status,FSB_UPDATE)
  END IF
  fld%tNext = fldp%tNext
  IF( .NOT.BTEST(fldp%status,FSB_UPDATE) )THEN
    SWIMupdateSmoothField = SWIMresult !Return if parent field is not updating
    GOTO 9999
  END IF
  Fld3d  => fld%NextField
  Fld3dp => fldp%NextField
  LSV    => fld%NextLSV
  LSVp   => fldp%NextLSV
  BL     => fld%NextBL
  BLp    => fldp%NextBL
  QL    => fld%NextQLprof
  QLp   => fldp%NextQLprof
END IF

!------ Average 3d fields

IF( iflag >= 1 )THEN

  irv = Avg3dVelField( fldp%grid,Fld3dp,fld%grid,Fld3d )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

IF( iflag /= 1 )THEN

  IF( BTEST(fldp%type,FTB_T) ) &
    CALL Avg3dField( fldp%grid,Fld3dp%Tpot,fld%grid,Fld3d%Tpot )

  IF( BTEST(fldp%type,FTB_P) ) &
    CALL Avg3dField( fldp%grid,Fld3dp%Press,fld%grid,Fld3d%Press )

  IF( BTEST(fldp%type,FTB_H) ) &
    CALL Avg3dField( fldp%grid,Fld3dp%Humid,fld%grid,Fld3d%Humid )

  IF( BTEST(fldp%type,FTB_QCLD) ) &
    CALL Avg3dField( fldp%grid,Fld3dp%Qcloud,fld%grid,Fld3d%Qcloud )

  IF( BTEST(fldp%type,FTB_Z) ) &
    CALL Avg3dField( fldp%grid,Fld3dp%Z,fld%grid,Fld3d%Z )

  IF( BTEST(fldp%type,FTB_LSV) )THEN               !Large-scale variability
    CALL Avg3dField( fldp%grid,LSVp%UU,fld%grid,LSV%UU )
    CALL Avg3dField( fldp%grid,LSVp%VV,fld%grid,LSV%VV )
    CALL Avg3dField( fldp%grid,LSVp%UV,fld%grid,LSV%UV )
    IF( BTEST(fldp%type,FTB_LSVL) )THEN
      CALL Avg3dField( fldp%grid,LSVp%SL,fld%grid,LSV%SL )
    END IF
  END IF

  IF( ASSOCIATED(fldp%QLprof%QQ) )THEN            !Model QQ, etc.
    CALL Avg3dField( fldp%grid,QLp%QQ,   fld%grid,QL%QQ )
    CALL Avg3dField( fldp%grid,QLp%SL,   fld%grid,QL%SL )
    CALL Avg3dField( fldp%grid,QLp%Tpot, fld%grid,QL%Tpot )
    CALL Avg3dField( fldp%grid,QLp%Press,fld%grid,QL%Press )
  END IF

  IF( BTEST(fldp%type,FTB_ZRUF) ) &                !Time-dependent roughness
    CALL Smooth2d( BLp%zruf,grid,BL%zruf )

  IF( BTEST(fldp%type,FTB_PRATE) )THEN             !(2d) Precipitation
    CALL Smooth2d( BLp%prcp,grid,BL%prcp )
    IF( ASSOCIATED(BLp%rainprb) )CALL Smooth2d( BLp%rainprb,fld%grid,BL%rainprb )
  ELSE IF( BTEST(fldp%type,FTB_PRCP) )THEN
    CALL SmoothPrcpType( BLp%prcp,fld%grid,BL%prcp )
  END IF

END IF

SWIMupdateSmoothField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION Avg3dVelField( gridp,fldp,grid,fld )

!------ Horizontal average of 3d velocity fields U, V, W

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ),   INTENT( IN ) :: gridp
TYPE( MetGrid ),   INTENT( IN ) :: grid
TYPE( MetMean3D ), POINTER      :: fldp
TYPE( MetMean3D ), POINTER      :: fld

INTEGER alloc_stat, k, i, ip
LOGICAL inverse

REAL, DIMENSION(:), POINTER :: U, V, WW, pU, pV, pWW

INTERFACE

  SUBROUTINE Avg3dField( gridp,fldp3d,grid,fld3d,UorV )
    USE SWIMmetField_fd
    IMPLICIT NONE
    TYPE( MetGrid ),        INTENT( IN ) :: gridp
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    REAL, DIMENSION(:),     POINTER      :: fldp3d, fld3d
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Avg3dField

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

  SUBROUTINE SetOmega( grid,u,v,w,ww,inverse )
    USE SWIMmetField_fd
    TYPE( MetGrid ),   INTENT( IN ) :: grid
    REAL, DIMENSION(:), POINTER     :: u, v, w, ww
    LOGICAL, OPTIONAL, INTENT( IN ) :: inverse
  END SUBROUTINE SetOmega

END INTERFACE

Avg3dVelField = SWIMfailure

IF( BTEST(gridp%type,GTB_STAGGER) )THEN

!------ Average U & V along (smooth) cell faces to conserve flux

!$OMP PARALLEL SHARED( grid,fld,fldp )
!$OMP DO PRIVATE(k,ip,i,pU,pV,U,V)
  DO k = 1,grid%nZ
    ip = (k-1)*gridp%nXY + 1 ; pU => fldp%U(ip:); pV => fldp%V(ip:)
    i  = (k-1)*grid%nXY + 1  ;  U => fld %U(i:) ;  V => fld %V(i:)
    CALL Smooth2d( pU,grid,U,'U' )
    CALL Smooth2d( pV,grid,V,'V' )
  END DO
!$OMP END DO
!$OMP END PARALLEL

  k = grid%nZ*grid%nXY  !Set top slice
  DO i = 1,grid%nXY
    ip = k+i
    fld%U(ip) = fld%U(ip-grid%nXY)
    fld%V(ip) = fld%V(ip-grid%nXY)
  END DO

  IF( ASSOCIATED(fld%W) )THEN

!------ Convert W to Omega and average; convert back to W

    ALLOCATE( WW(SIZE(fld%U)),pWW(SIZE(fldp%U)),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'Avg3dVelField'
      error%Message = 'Error allocating arrays for velocity smoothing'
      GOTO 9999
    END IF

    inverse = .FALSE.
    CALL SetOmega( gridp,fldp%U,fldp%V,fldp%W,pWW,inverse )

    CALL Avg3dField( gridp,pWW,grid,WW,'W' )

    inverse = .TRUE.
    CALL SetOmega( grid,fld%U,fld%V,fld%W,WW,inverse )

    DEALLOCATE( WW,pWW,STAT=alloc_stat )

    k = grid%nZ*grid%nXY  !Set top slice
    DO i = 1,grid%nXY
      ip = k+i
      fld%W(ip) = 0.
    END DO

  END IF

ELSE !- Standard averaging for non-staggered grid

  CALL Avg3dField( gridp,fldp%U,grid,fld%U,'nU' )
  CALL Avg3dField( gridp,fldp%V,grid,fld%V,'nV' )
  IF( ASSOCIATED(fld%W) )CALL Avg3dField( gridp,fldp%W,grid,fld%W,'W' )

END IF

Avg3dVelField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE Avg3dField( gridp,fldp3d,grid,fld3d,UorV )

!------ Horizontal average of 3d fields e.g., temperature, unstaggered velocity, etc.

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ),        INTENT( IN ) :: gridp
TYPE( MetGrid ),        INTENT( IN ) :: grid
REAL, DIMENSION(:),     POINTER      :: fldp3d, fld3d
CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV

INTEGER k, i, ip

REAL, DIMENSION(:), POINTER :: fldp, fld

INTERFACE
  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d
END INTERFACE

INTEGER, EXTERNAL :: SWIMlimit

!$OMP PARALLEL SHARED( grid,fld3d,fldp3d,UorV )
!$OMP DO PRIVATE(k,ip,i,fld,fldp)
DO k = 1,grid%nZ
  ip = (k-1)*gridp%nXY ; fldp => fldp3d(ip+1:ip+gridp%nXY)
  i  = (k-1)*grid%nXY  ; fld  => fld3d(i+1:i+grid%nXY)
  IF( PRESENT( UorV ) )THEN
    CALL Smooth2d( fldp,grid,fld,UorV )
  ELSE
    CALL Smooth2d( fldp,grid,fld )
  END IF
END DO
!$OMP END DO
!$OMP END PARALLEL

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
  k = grid%nZ*grid%nXY
  DO i = 1,grid%nXY
    fld3d(k+i) = fld3d(k+1-grid%nXY)
  END DO
END IF

RETURN
END

!==============================================================================

SUBROUTINE Smooth2d( fldp,grid,fld,UorV )

!------ Perform horizontal smoothing

USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

REAL, DIMENSION(:),     POINTER      :: fldp, fld
TYPE( MetGrid ),        INTENT( IN ) :: grid
CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

INTEGER nxs, nys, nxp
INTEGER i, j, i0, ij, ii, jj, i0p, ijp
REAL    fmin, fmax
LOGICAL lMinMax

REAL, EXTERNAL :: SWIMrlimit

nxs = grid%Smooth%xNsmth; xSmooth => grid%Smooth%xSmooth
nys = grid%Smooth%yNsmth; ySmooth => grid%Smooth%ySmooth
nxp = grid%Smooth%nXp

IF( PRESENT(UorV) )THEN
  IF( BTEST(grid%type,GTB_STAGGERB) )THEN
    nxs = grid%Smooth%xNsmthU; xSmooth => grid%Smooth%xSmoothU
    nys = grid%Smooth%yNsmthV; ySmooth => grid%Smooth%ySmoothV
  ELSE
    SELECT CASE( UorV(1:1) )
      CASE( 'U','u' )
        nxs = grid%Smooth%xNsmthU; xSmooth => grid%Smooth%xSmoothU
      CASE( 'V','v' )
        nys = grid%Smooth%yNsmthV; ySmooth => grid%Smooth%ySmoothV
    END SELECT
  END IF
  lMinMax = .FALSE.
ELSE
  lMinMax = .TRUE.
END IF

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  DO i = 1,grid%nX
    ij = i0 + i
    fmin =  HUGE(fmin)
    fmax = -fmin

    fld(ij) = 0.
    DO jj = 1,nys
      i0p  = (ySmooth(j)%ip(jj)-1)*nxp
      DO ii = 1,nxs
        ijp  = i0p + xSmooth(i)%ip(ii)
        fld(ij) = fld(ij) + ySmooth(j)%wt(jj)*xSmooth(i)%wt(ii)*fldp(ijp)
        IF( lMinMax )THEN
          fmin = MIN(fmin,fldp(ijp))
          fmax = MAX(fmax,fldp(ijp))
        END IF
      END DO
    END DO
    IF( lMinMax )fld(ij) = SWIMrlimit( fld(ij),fmin,fmax )

  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE SmoothLandUse( fldp,grid,fld )

!------ Perform horizontal smoothing

USE SWIMparam_fd
USE SWIM_fi
USE landuse_fd

IMPLICIT NONE

INTEGER, DIMENSION(:),  POINTER      :: fldp, fld
TYPE( MetGrid ),        INTENT( IN ) :: grid

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

INTEGER nxs, nys, nxp
INTEGER irv, i, j, i0, ij, ii, jj, i0p, ijp
REAL    xtol, ytol

INTEGER, EXTERNAL :: InitLandUse, InitLandUseCat, ExitLandUse
INTEGER, EXTERNAL :: AddLandUseCat, OutLandUseCat, ClearLandUseCat

nxs = grid%Smooth%xNsmth; xSmooth => grid%Smooth%xSmooth
nys = grid%Smooth%yNsmth; ySmooth => grid%Smooth%ySmooth
nxp = grid%Smooth%nXp

xtol = 0.9/FLOAT(nxs)
ytol = 0.9/FLOAT(nys)

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  DO i = 1,grid%nX
    ij = i0 + i

    irv = InitLandUseCat( 1 )         !category cells
    IF( irv /= 1 )THEN
      CALL SWIMgetLandUseError()
      GOTO 9999
    END IF

    DO jj = 1,nys
      IF( ySmooth(j)%wt(jj) > ytol )THEN
        i0p  = (ySmooth(j)%ip(jj)-1)*nxp
        DO ii = 1,nxs
          IF( xSmooth(i)%wt(ii) > xtol )THEN
            ijp = i0p + xSmooth(i)%ip(ii)
            irv = AddLandUseCat( 1,fldp(ijp) )
            IF( irv /= 1 )THEN
              CALL SWIMgetLandUseError()
              GOTO 9999
            END IF
          END IF
        END DO
      END IF
    END DO

    fld(ij) = OutLandUseCat( 1 )
    IF( fld(ij) == 0 )THEN
      CALL SWIMgetLandUseError()
      GOTO 9999
    END IF

    irv = ClearLandUseCat()
    IF( irv /= 1 )THEN
      CALL SWIMgetLandUseError()
      GOTO 9999
    END IF

  END DO
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE Hcnp2d( nxp,nyp,fldp,nx,ny,fld,shift )

!------ Perform special horizontal averaging for canopy height

USE SWIM_fi

IMPLICIT NONE

INTEGER,       INTENT( IN ) :: nxp, nyp
INTEGER,       INTENT( IN ) :: nx, ny, shift
REAL, DIMENSION(:), POINTER :: fldp, fld

REAL, DIMENSION(-1:2), PARAMETER :: AVGWT  = (/ 1.,3.,3.,1. /)
REAL, PARAMETER                  :: AVGFAC = 1./64.

INTEGER i, j, i0, ij, ip, jp, ijp, ii, jj, iip, jjp, i0p, nxy

INTEGER, EXTERNAL :: SWIMlimit

nxy        = nx*ny
fld(1:nxy) = 0.

DO j = 1,ny
  i0  = (j-1)*nx
  jp  = 2*j-shift

  DO i = 1,nx
    ij = i0 + i
    ip = 2*i-shift

    fld(ij) = 0.
    DO jj = -1,2 !0,1
      jjp  = SWIMlimit( jp+jj,1,nyp )
      i0p = (jjp-1)*nxp
      DO ii = -1,2 !0,1
        iip  = SWIMlimit( ip+ii,1,nxp )
        ijp = i0p + iip
        fld(ij) = MAX(fld(ij),fldp(ijp))
      END DO
    END DO

  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE SmoothPrcpType( fldp,grid,fld )

!------ Perform special horizontal smoothing for precipitation type

USE SWIMparam_fd
USE SWIM_fi
USE landuse_fd

IMPLICIT NONE

REAL, DIMENSION(:),  POINTER      :: fldp, fld  !Parent, child precipitation types
TYPE( MetGrid ),     INTENT( IN ) :: grid

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

INTEGER, DIMENSION(0:6) :: cat

INTEGER nxs, nys, nxp
INTEGER i, j, i0, ij, ii, jj, i0p, ijp, icat
REAL    xtol, ytol

INTEGER, EXTERNAL :: OutPrcpCat

nxs = grid%Smooth%xNsmth; xSmooth => grid%Smooth%xSmooth
nys = grid%Smooth%yNsmth; ySmooth => grid%Smooth%ySmooth
nxp = grid%Smooth%nXp

xtol = 0.9/FLOAT(nxs)
ytol = 0.9/FLOAT(nys)

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  DO i = 1,grid%nX
    ij  = i0 + i
    cat = 0

    DO jj = 1,nys
      IF( ySmooth(j)%wt(jj) > ytol )THEN
        i0p  = (ySmooth(j)%ip(jj)-1)*nxp
        DO ii = 1,nxs
          IF( xSmooth(i)%wt(ii) > xtol )THEN
            ijp = i0p + xSmooth(i)%ip(ii)
            icat = INT(fldp(ijp))
            cat(icat) = cat(icat) + 1
          END IF
        END DO
      END IF
    END DO

    fld(ij) = FLOAT(OutPrcpCat( cat ))

  END DO
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION OutPrcpCat( cat ) RESULT( pType )

IMPLICIT NONE

INTEGER, DIMENSION(0:6) :: cat

INTEGER ntot, i, nr, ns

ntot = SUM(cat)

IF( ntot == 0 )THEN

  pType = 0

ELSE

  DO i = 0,6
    IF( cat(i) > ntot/2 )THEN
      pType = i
      GOTO 9999
    END IF
  END DO

  nr = SUM(cat(1:3))
  ns = SUM(cat(4:6))

  IF( nr >= ns )THEN
    IF( cat(0) > nr )THEN
      pType = 1
    ELSE
      pType = 2
    END IF

  ELSE
    IF( cat(0) > ns )THEN
      pType = 4
    ELSE
      pType = 5
    END IF

  END IF

END IF

9999 CONTINUE

RETURN
END
