RECURSIVE INTEGER FUNCTION SWIMaddSmoothField( ifldp )

!------ Add smoothed field based on parent field number ifldp

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMaddSmoothField

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr
USE constants_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: ifldp   !Parent field input; smooth field index output

TYPE( MetField), POINTER :: fld, fldp

INTEGER i, irv, n, nxy, nxyz

TYPE( MetGrid ), POINTER :: grid, gridp

INTERFACE

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

  SUBROUTINE SmoothLandUse( fldp,grid,fld )
    USE SWIMmetField_fd
    INTEGER, DIMENSION(:),  POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
  END SUBROUTINE SmoothLandUse

  INTEGER FUNCTION SWIMupdateSmoothField( fldi,iflag )
    USE SWIMmetField_fd
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fldi
    INTEGER,                  INTENT( IN    ) :: iflag
  END FUNCTION SWIMupdateSmoothField

END INTERFACE

INTEGER, EXTERNAL :: AddOutput
INTEGER, EXTERNAL :: SWIMalloc3dField, SWIMallocBLParam, GridLogMsg
INTEGER, EXTERNAL :: SWIMallocBLaux, SetGridLL, SetTerrainGrad, SetSmooth2D
INTEGER, EXTERNAL :: SWIMupdateBL, SWIMshearGrad, InitLSV
INTEGER, EXTERNAL :: SWIMallocQLprof, getSmoothIndex

!------ Initialize failure

SWIMaddSmoothField = SWIMfailure

!------ Make sure potential parent has the right values

IF( ifldp > numField .OR. ifldp < 1 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMaddSmoothField'
  error%Message = 'Requested field does not exist'
  GOTO 9999
END IF

!------ Do not build smooth field if one has already been built, or
!       parent grid is very small.
!       Also make sure field has been pre-allocated.

IF( BTEST(field(ifldp)%status,FSB_DONESMOOTH) )THEN
  SWIMaddSmoothField = SWIMsuccess
  ifldp = -getSmoothIndex( field(ifldp)%index )  !Point to already-created smooth field
  GOTO 9999
ELSE IF( field(ifldp)%grid%nX <= NMIN_SMTH   .OR. &
         field(ifldp)%grid%nY <= NMIN_SMTH   .OR. &
         numField + 1 > numFieldMax )THEN
  SWIMaddSmoothField = SWIMnull
  GOTO 9999
END IF

!------ Add a met field

n = numField + 1

!------ Point to smooth met field; set index of 'parent' field

fld  => field(n)
fldp => field(ifldp)

grid  => fld%grid
gridp => fldp%grid

fld%index = 0
CALL setFieldIndex( fld%index,n )

!------ Set grid source type and save parent field number

fld%gridSource%type = IBSET(fld%gridSource%type,GSB_SMOOTH)
fld%gridSource%unit = ifldp
IF( BTEST(fldp%gridSource%type,GSB_PERIODIC) )fld%gridSource%type = &
                                              IBSET(fld%gridSource%type,GSB_PERIODIC)

fld%status = 0

!------ Set field type

fld%type = IBSET(0,FTB_SMOOTH)
IF( BTEST(fldp%type,FTB_WRF) )fld%type = IBSET(fld%type,FTB_WRF)
IF( BTEST(fldp%type,FTB_W) )fld%type = IBSET(fld%type,FTB_W)
IF( BTEST(fldp%type,FTB_T) )fld%type = IBSET(fld%type,FTB_T)
IF( BTEST(fldp%type,FTB_P) )fld%type = IBSET(fld%type,FTB_P)
IF( BTEST(fldp%type,FTB_H) )fld%type = IBSET(fld%type,FTB_H)
IF( BTEST(fldp%type,FTB_Z) )fld%type = IBSET(fld%type,FTB_Z)
IF( BTEST(fldp%type,FTB_QCLD) )fld%type = IBSET(fld%type,FTB_QCLD)

IF( BTEST(fldp%type,FTB_LSV ) )fld%type = IBSET(fld%type,FTB_LSV)
IF( BTEST(fldp%type,FTB_LSVL) )fld%type = IBSET(fld%type,FTB_LSVL)

IF( BTEST(fldp%type,FTB_PRCP)  )fld%type = IBSET(fld%type,FTB_PRCP)
IF( BTEST(fldp%type,FTB_PRATE) )fld%type = IBSET(fld%type,FTB_PRATE)
IF( BTEST(fldp%type,FTB_ACCPR) )fld%type = IBSET(fld%type,FTB_ACCPR)
IF( BTEST(fldp%type,FTB_ZRUF)  )fld%type = IBSET(fld%type,FTB_ZRUF)

!------ Set BL type

SELECT CASE( fldp%BLtype )

  CASE( BLP_SBL,BLP_NONE )
    fld%BLtype = fldp%BLtype

  CASE DEFAULT
    fld%BLtype = BLP_NEST

END SELECT

!------ Set horizontal grid

CALL SetSmoothHorizGrid( gridp,grid )

!------ Set grid

CALL SetSmoothGrid( fld%grid,fldp%grid )

IF( grid%nxy >= 6 )fld%type = IBSET(fld%type,FTB_DU2)

nxy  = grid%nXY

!------ Allocate met fields

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
  nxyz = nxy*(grid%nZ+1)
ELSE
  nxyz = nxy*grid%nZ
END IF

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,nxyz,fld%NextField )
IF( irv /= SWIMsuccess )GOTO 9999

IF( ASSOCIATED(fldp%QLprof%QQ) )THEN
  irv = SWIMallocQLprof( nxyz,fld%QLprof )
  IF( irv /= SWIMsuccess )GOTO 9999
  irv = SWIMallocQLprof( nxyz,fld%NextQLprof )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

irv = SWIMallocBLParam( fld%type,nxy,fld%BL,fld%NextBL )
IF( irv /= SWIMsuccess )GOTO 9999

fld%BLaux%nz = Prj%BL%nzbl

irv = SWIMallocBLaux( nxy,fld%BLaux )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Some final things:

!------ Compute lat/lon arrays if possible

irv = SetGridLL( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize solar zenith angle if required

IF( Prj%decay )CALL Smooth2d( gridp%sunfac,grid,grid%sunfac )

!------ Initialize large-scale variability

irv = InitLSV( field(n) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Log messages

irv = GridLogMsg( field(n) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set to avoid output

fld%unitOut = 0

!------ Average 3d fields

fld%status = IBSET(fld%status,FSB_DOINIT)
irv = SWIMupdateSmoothField( field(n),2 ) !Does Field (w/ FSB_DOINIT)
IF( irv /= SWIMsuccess )GOTO 9999

fld%status = IBCLR(fld%status,FSB_DOINIT)
irv = SWIMupdateSmoothField( field(n),2 ) !Does NextField
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set 3d height in grid structure

IF( BTEST(grid%type,GTB_Z3D) .AND. .NOT.BTEST(grid%type,GTB_SIGMA) )THEN

  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
    IF( BTEST(grid%type,GTB_Z3DW) )THEN
      grid%sigma%Zw => fld%field%Z
      CALL SetSigmaZ( grid )
    ELSE
      grid%sigma%Z => fld%field%Z(grid%nXY+1:)
      CALL SetSigmaZw( grid )
    END IF
  ELSE
    grid%sigma%Z => fld%field%Z
  END IF

END IF

!------ Set grid level for setting surface layer profile (above canopy)
!       N.B. Must be called after setting grid%sigma%Z

CALL set_kbl( field(n)%grid )
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Compute velocity gradient inhomogeneity measure

irv = SWIMshearGrad( fld%Field,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%status,FSB_UPDATE) )THEN
  irv = SWIMshearGrad( fld%NextField,fld%grid )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Set BL

fld%tBL = fldp%tBL

IF( fld%BLtype /= BLP_NONE )THEN
  IF( Prj%decay )fld%status = IBSET(fld%status,FSB_DOINITBL) !To set sunrise/set
  CALL Smooth2d( fldp%BLaux%zsl,fld%grid,fld%BLaux%zsl )     !Estimate zsl and invMOL (for SetSqVel)
  CALL Smooth2d( fldp%BL%invMOL,fld%grid,fld%BL%invMOL )
  irv = SWIMupdateBL( fld%t,field(n) )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Set status indicating first parent time is after current time
!       to avoid redundant time interpolation

IF( BTEST(fldp%status,FSB_FIRST) )fld%status = IBSET(fld%status,FSB_FIRST)

!------ Show that parent field has a smooth ancestor

fldp%status = IBSET(fldp%status,FSB_DONESMOOTH)
CALL setSmoothIndex( fldp%index,n )

!$OMP ATOMIC WRITE
numField = n
ifldp    = n

!------ Another SWIMming success

SWIMaddSmoothField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSmoothGrid( grid,gridp )

!------ Set smooth grid parameters, horizontal and vertical grids, and other fields

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr
USE constants_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid
TYPE( MetGrid ), INTENT( IN    ) :: gridp

INTEGER i, irv, alloc_stat, nxy, nxyp, nzw
INTEGER ntot, k, ip
REAL    a1, a2, pt, zs, r, ps, riso, aiso, ziso, sigma
REAL    alphaMax

REAL, DIMENSION(:), POINTER :: alphaHcnp, alphaHcnp2

INTERFACE

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

  SUBROUTINE SmoothLandUse( fldp,grid,fld )
    USE SWIMmetField_fd
    INTEGER, DIMENSION(:),  POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
  END SUBROUTINE SmoothLandUse

END INTERFACE

INTEGER, EXTERNAL :: SetSmooth2D, SetTerrainGrad, SWIMallocTerrain

!------ Copy type bits

grid%type = gridp%type

!------ Copy map coordinate structure

grid%coord = gridp%coord

!------ Copy vertical grid

grid%nZ = gridp%nZ

ALLOCATE( grid%Z(grid%nZ),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetSmoothGrid'
  error%Message = 'Error allocating vertical grid for smoothed field'
  GOTO 9999
END IF

CALL CopyArray( grid%Z,gridp%Z,grid%nZ )

IF( ASSOCIATED(gridp%Zw) )THEN
  nzw = SIZE(gridp%Zw)
  ALLOCATE( grid%Zw(nzw),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetSmoothGrid'
    error%Message = 'Error allocating vertical w-grid for smoothed field'
    GOTO 9999
  END IF
  CALL CopyArray( grid%Zw,gridp%Zw,nzw )
END IF

grid%Ztop = gridp%Ztop

nxy  = grid%nXY
nxyp = gridp%nXY

!------ Setup 2d smoothing factors

irv = SetSmooth2D( gridp,grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Build smoothed terrain and landuse fields

irv = SWIMallocTerrain( grid )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(gridp%type,GTB_TERRAIN) )THEN
  CALL Smooth2d( gridp%terrain%H,grid,grid%terrain%H )
ELSE
  DO i = 1,nxy
    grid%terrain%H(i)  = 0.
    grid%terrain%Hx(i) = 0.
    grid%terrain%Hy(i) = 0.
    grid%terrain%D(i)  = 1.
    grid%terrain%Du(i) = 1.
    grid%terrain%Dv(i) = 1.
  END DO
END IF

grid%Hmin = gridp%Hmin !Copy from parent for same reference level
grid%Hmax = gridp%Hmax !Not used

!------ Setup for sigma coordinate

IF( BTEST(gridp%type,GTB_SIGMA)  )THEN

  ntot = grid%nXY*grid%nZ
  ALLOCATE( grid%sigma%Z(ntot), &
            grid%sigma%Psrf(grid%nXY), &
            grid%sigma%Px(grid%nXY),grid%sigma%Py(grid%nXY),STAT=alloc_stat)
  IF( alloc_stat /= 0 )THEN
    error%Number   = IV_ERROR
    error%Routine = 'SetSmoothGrid'
    error%Message = 'Error allocating arrays for sigma height levels'
    GOTO 9999
  END IF
  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
    ntot = grid%nXY*(grid%nZ+1)
    ALLOCATE( grid%sigma%Zw(ntot),STAT=alloc_stat)
    IF( alloc_stat /= 0 )THEN
      error%Number   = IV_ERROR
      error%Routine = 'SetSmoothGrid'
      error%Message = 'Error allocating arrays for sigma height w-levels'
      GOTO 9999
    END IF
  END IF

  a1   = gridp%Sigma%a1
  a2   = gridp%Sigma%a2
  pt   = gridp%sigma%Ptop
  aiso = gridp%Sigma%aiso

  grid%Ztop       = -HUGE(0.)
  grid%sigma%Ptop = pt
  grid%Sigma%a1   = a1
  grid%Sigma%a2   = a2
  grid%sigma%P00  = gridp%sigma%P00
  grid%sigma%aiso = aiso

  IF( aiso > 0. ) THEN
    riso = -(aiso + a1)/a2
    ziso = a2*riso*riso + a1*riso - grid%Hmin
  ELSE
    riso = -HUGE(0.)
  END IF

  DO i = 1,grid%nXY

    zs = grid%terrain%H(i) + grid%Hmin
    r  = -(a1 + SQRT(a1*a1 + 4.*a2*zs))/ (2.*a2)
    ps = EXP(r)
    grid%sigma%Psrf(i) = ps - pt

    DO k = 1,grid%nZ
      ip = (k-1)*grid%nXY + i
      sigma = 1. - grid%Z(k)  !Z=1-sigma from child field
      r = LOG(sigma*ps + (1.-sigma)*pt)
      IF( r > riso )THEN
        grid%sigma%Z(ip) = a2*r*r + a1*r - grid%Hmin  !Relative to field Hmin
      ELSE
        grid%sigma%Z(ip) = ziso + aiso*(riso-r)
      END IF
      grid%Ztop        = MAX(grid%sigma%Z(ip),grid%Ztop)
      grid%sigma%Z(ip) = grid%sigma%Z(ip) - grid%terrain%H(i)
    END DO

    IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
      DO k = 1,grid%nZ+1
        ip = (k-1)*grid%nXY + i
        sigma = 1. - grid%Zw(k)  !1-sigma from child field
        r = LOG(sigma*ps + (1.-sigma)*pt)
        IF( r > riso )THEN
          grid%sigma%Zw(ip) = a2*r*r + a1*r - grid%Hmin - grid%terrain%H(i)
        ELSE
          grid%sigma%Zw(ip) = ziso + aiso*(riso-r) - grid%terrain%H(i)
        END IF
      END DO
    END IF

  END DO

ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN

  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
    IF( BTEST(grid%type,GTB_Z3DW) )THEN
      ntot = grid%nXY*grid%nZ
      ALLOCATE( grid%sigma%Z(ntot),STAT=alloc_stat)
      IF( alloc_stat /= 0 )THEN
        error%Number   = IV_ERROR
        error%Routine = 'SetSmoothGrid'
        error%Message = 'Error allocating arrays for sigma height levels'
 !       CALL ReportFileName( error%Inform,'File=',src%Source(1) )
        GOTO 9999
      END IF
    ELSE
      ntot = grid%nXY*(grid%nZ+1)
      ALLOCATE( grid%sigma%Zw(ntot),STAT=alloc_stat)
      IF( alloc_stat /= 0 )THEN
        error%Number   = IV_ERROR
        error%Routine = 'SetSmoothGrid'
        error%Message = 'Error allocating arrays for sigma height w-levels'
        GOTO 9999
      END IF
    END IF
  END IF

END IF

!------ Compute terrain gradients (and surface pressure gradients for sigma coordinates)

IF( BTEST(gridp%type,GTB_TERRAIN) )THEN
  irv = SetTerrainGrad( grid )
  IF( irv /= SWIMsuccess )GOTO 9999
  grid%type = IBSET(grid%type,GTB_TERRAIN)
END IF

!------ Roughness
!       N.B. Always smooth even though may be a constant

CALL Smooth2d( gridp%landcover%roughness,grid,grid%landcover%roughness )
IF( BTEST(gridp%type,GTB_ZRUF) )grid%type = IBSET(grid%type,GTB_ZRUF)

!------ Canopy height and velocity factor:
!       Hbar = SUM(A*H^2)/SUM(A*H) & Abar = SUM(A*H)^2/SUM(A*H^2) (if both available)

IF( BTEST(gridp%type,GTB_HCNP) .AND. BTEST(gridp%type,GTB_ALPHA) )THEN

  ALLOCATE( alphaHcnp(nxyp),alphaHcnp2(nxyp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetSmoothGrid'
    error%Message = 'Error allocating arrays for smoothing canopy height'
    GOTO 9999
  END IF
  CALL MultArray( nxyp,alphaHcnp,gridp%landcover%canopyHt,gridp%landcover%alpha )
  CALL MultArray( nxyp,alphaHcnp2,alphaHcnp,gridp%landcover%canopyHt )
  CALL Smooth2d( alphaHcnp,grid,grid%landcover%alpha )
  CALL Smooth2d( alphaHcnp2,grid,grid%landcover%canopyHt )
  alphaMax = 0.0
  DO i = 1,nxyp
    alphaMax = MAX(alphaMax,gridp%landcover%alpha(i))
  END DO
  DO i = 1,nxy
    IF( grid%landcover%alpha(i) > 0. )THEN
      grid%landcover%canopyHt(i) = grid%landcover%canopyHt(i) / grid%landcover%alpha(i)
      grid%landcover%alpha(i)    = MIN( alphaMax, &
                                grid%landcover%alpha(i) / grid%landcover%canopyHt(i) )
    ELSE
      grid%landcover%canopyHt(i) = 0.
    END IF
  END DO
  DEALLOCATE( alphaHcnp,alphaHcnp2,STAT=alloc_stat )
  grid%type = IBSET(grid%type,GTB_ALPHA)
  grid%type = IBSET(grid%type,GTB_HCNP)

ELSE

  IF( BTEST(gridp%type,GTB_ALPHA) )THEN
    CALL Smooth2d( gridp%landcover%alpha,grid,grid%landcover%alpha )
    grid%type = IBSET(grid%type,GTB_ALPHA)
  ELSE IF( BTEST(gridp%type,GTB_ZRUF) )THEN
    DO i = 1,nxy
      grid%landcover%alpha(i) = 0.
    END DO
  ELSE
    DO i = 1,nxy
      grid%landcover%alpha(i) = Prj%BL%alpha
    END DO
  END IF

  IF( BTEST(gridp%type,GTB_HCNP) )THEN
    CALL Smooth2d( gridp%landcover%canopyHt,grid,grid%landcover%canopyHt )
    grid%type = IBSET(grid%type,GTB_HCNP)
  ELSE IF( BTEST(gridp%type,GTB_ZRUF) )THEN
    DO i = 1,nxy
      grid%landcover%canopyHt(i) = -1.
    END DO
  ELSE
    DO i = 1,nxy
      grid%landcover%canopyHt(i) = Prj%BL%hc
    END DO
  END IF

END IF

!------ Albedo
!       N.B. Albedo and Bowen not used for smooth fields
!       but set for completeness

IF( BTEST(gridp%type,GTB_ALBEDO) )THEN
  CALL Smooth2d( gridp%landcover%albedo,grid,grid%landcover%albedo )
ELSE
  DO i = 1,nxy
    grid%landcover%albedo(i) = Prj%BL%albedo
  END DO
END IF

!------ Bowen ratio

IF( BTEST(gridp%type,GTB_BOWEN) )THEN
  CALL Smooth2d( gridp%landcover%Bowen,grid,grid%landcover%Bowen )
ELSE
  DO i = 1,nxy
    grid%landcover%Bowen(i) = Prj%BL%Bowen
  END DO
END IF

IF( BTEST(gridp%type,GTB_LANDUSE) )THEN
  CALL SmoothLandUse( gridp%landcover%LandUse,grid,grid%landcover%LandUse )
  grid%type = IBSET(grid%type,GTB_LANDUSE)
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSmoothHorizGrid( gridp,grid )

USE SWIMmetField_fd
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN    ) :: gridp
TYPE( MetGrid ), INTENT( INOUT ) :: grid

grid%Xmin = gridp%Xmin         ; grid%Ymin = gridp%Ymin
grid%Xmax = gridp%Xmax         ; grid%Ymax = gridp%Ymax
grid%nX   = INT((gridp%nX+2)/2); grid%nY   = INT((gridp%nY+2)/2)

grid%dX  = (grid%Xmax-grid%Xmin)/FLOAT(grid%nX-1)
grid%dY  = (grid%Ymax-grid%Ymin)/FLOAT(grid%nY-1)
grid%nXY = grid%nX*grid%nY

IF( BTEST(gridp%type,GTB_PERIODIC) )grid%type = IBSET(grid%type,GTB_PERIODIC)
IF( BTEST(gridp%type,GTB_STAGGER)  )grid%type = IBSET(grid%type,GTB_STAGGER)
IF( BTEST(gridp%type,GTB_STAGGERB) )grid%type = IBSET(grid%type,GTB_STAGGERB)
IF( BTEST(gridp%type,GTB_STAGGERZ) )grid%type = IBSET(grid%type,GTB_STAGGERZ)

RETURN
END

!==============================================================================

INTEGER FUNCTION SetSmooth2D( gridp,grid )

!------ Setup arrays for horizontal smoothing

USE SWIMparam_fd
USE SWIM_fi
USE FilterDefinition

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN    ) :: gridp
TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER alloc_stat, nFil, ns
INTEGER nx, ny, nxp, nyp
INTEGER i, j, ic, im, ip, jc, jm, jp, ii, jj
REAL    rx, ry, xp, yp
LOGICAL xOdd, yOdd, xPeriodic

REAL, DIMENSION(:), ALLOCATABLE :: wtFil, wtFilStg

INTEGER, EXTERNAL :: SWIMlimit, LimitX

SetSmooth2D = SWIMfailure

nx = grid%nX; nxp = gridp%nX; xOdd = (MOD(nxp,2) /= 0)
ny = grid%nY; nyp = gridp%nY; yOdd = (MOD(nyp,2) /= 0)

xPeriodic = BTEST(gridp%type,GTB_PERIODIC)

grid%Smooth%nXp = nxp

!------ Setup arrays for unstaggered grid or cell center locations for staggered grid

ALLOCATE( grid%Smooth%xSmooth(nx),grid%Smooth%ySmooth(ny),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetSmooth2D'
  error%Message = 'Error allocating interpolation arrays for smooth met fields'
  GOTO 9999
END IF

!------ Get filter parameters

CALL GetFilterLength( nFil )
ALLOCATE( wtFil(nFil),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetSmooth2D'
  error%Message = 'Error allocating filter weights array'
  GOTO 9999
END IF

CALL GetFilterWeights( wtFil )

ns = (nFil-1)/2

!------ X-grid

IF( xOdd )THEN
  grid%Smooth%xNsmth = nFil
ELSE
  grid%Smooth%xNsmth = nFil + 1
END IF

DO i = 1,nx

  ALLOCATE( grid%Smooth%xSmooth(i)%ip(grid%Smooth%xNsmth), &
            grid%Smooth%xSmooth(i)%wt(grid%Smooth%xNsmth),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetSmooth2D'
    error%Message = 'Error allocating interpolation point arrays for smooth met fields'
    GOTO 9999
  END IF

  IF( xOdd )THEN
    ic = 2*i - 1
    DO ii = 1,nFil
      grid%Smooth%xSmooth(i)%ip(ii) = LimitX( ic-ns+ii-1,nxp,xPeriodic )
      grid%Smooth%xSmooth(i)%wt(ii) = wtFil(ii)
    END DO
  ELSE
    rx = grid%Xmin - gridp%Xmin
    rx = (rx + FLOAT(i-1)*grid%dX)/gridp%dX
    ic = INT(rx) + 1
    im = ic - ns
    ip = ic + ns + 1
    j  = 0
    DO ii = im,ip
      j = j + 1
      xp = FLOAT(ii) - 1.
      ic = LimitX( ii,nxp,xPeriodic )
      grid%Smooth%xSmooth(i)%ip(j) = ic
      grid%Smooth%xSmooth(i)%wt(j) = FilterFac( rx,xp )
    END DO
  END IF

END DO

!------ Y-grid

IF( yOdd )THEN
  grid%Smooth%yNsmth = nFil
ELSE
  grid%Smooth%yNsmth = nFil + 1
END IF

DO j = 1,ny

  ALLOCATE( grid%Smooth%ySmooth(j)%ip(grid%Smooth%yNsmth), &
            grid%Smooth%ySmooth(j)%wt(grid%Smooth%yNsmth),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetSmooth2D'
    error%Message = 'Error allocating interpolation point arrays for smooth met fields'
    GOTO 9999
  END IF

  IF( yOdd )THEN
    jc = 2*j - 1
    DO jj = 1,nFil
      grid%Smooth%ySmooth(j)%ip(jj) = SWIMlimit( jc-ns+jj-1,1,nyp )
      grid%Smooth%ySmooth(j)%wt(jj) = wtFil(jj)
    END DO
  ELSE
    ry = grid%Ymin - gridp%Ymin
    ry = (ry + FLOAT(j-1)*grid%dY)/gridp%dY
    jc = INT(ry) + 1
    jm = jc - ns
    jp = jc + ns + 1
    i  = 0
    DO jj = jm,jp
      i = i + 1
      yp = FLOAT(jj) - 1.
      grid%Smooth%ySmooth(j)%ip(i) = SWIMlimit( jj,1,nyp )
      grid%Smooth%ySmooth(j)%wt(i) = FilterFac( ry,yp )
    END DO
  END IF

END DO

Staggered : IF( BTEST(gridp%type,GTB_STAGGER) )THEN

!------ Setup arrays for staggered grid

  ALLOCATE( grid%Smooth%xSmoothU(nx),grid%Smooth%ySmoothV(ny),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetSmooth2D'
    error%Message = 'Error allocating interpolation arrays for staggered smooth met fields'
    GOTO 9999
  END IF

ALLOCATE( wtFilStg(nFil+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetSmooth2D'
  error%Message = 'Error allocating filter weights array for staggered grid'
  GOTO 9999
END IF

CALL GetFilterWeightsStg( wtFilStg )

!------ X-grid ("U" location)

  IF( xOdd )THEN
    grid%Smooth%xNsmthU = nfil+1
  ELSE
    grid%Smooth%xNsmthU = nfil+2
  END IF

  DO i = 1,nx

    ALLOCATE( grid%Smooth%xSmoothU(i)%ip(grid%Smooth%xNsmthU), &
              grid%Smooth%xSmoothU(i)%wt(grid%Smooth%xNsmthU),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetSmooth2D'
      error%Message = 'Error allocating interpolation point arrays for x-staggered smooth met fields'
      GOTO 9999
    END IF

    IF( xOdd )THEN
      DO ii = 1,nFil+1
        grid%Smooth%xSmoothU(i)%ip(ii) = LimitX( 2*i-2-ns+ii,nxp,xPeriodic )
        grid%Smooth%xSmoothU(i)%wt(ii) = wtFilStg(ii)
      END DO
    ELSE
      rx = grid%Xmin - gridp%Xmin
      rx = (rx + (FLOAT(i)-0.5)*grid%dX - 0.5*gridp%dX)/gridp%dX
      ic = INT(rx) + 1
      ii = INT(rx-FLOAT(ic-1)+0.5)
      im = ic - (ns+1) + ii
      ip = ic + (ns+1) + ii
      j  = 0
      DO ii = im,ip
        j = j + 1
        xp = FLOAT(ii) - 1.
        ic = LimitX( ii,nxp,xPeriodic )
        grid%Smooth%xSmoothU(i)%ip(j) = ic
        grid%Smooth%xSmoothU(i)%wt(j) = FilterFacStg( rx,xp )
      END DO

    END IF

  END DO

!------ Y-grid ("V" location)

  IF( yOdd )THEN
    grid%Smooth%yNsmthV = nfil + 1
  ELSE
    grid%Smooth%yNsmthV = nfil + 2
  END IF

  DO j = 1,ny

    ALLOCATE( grid%Smooth%ySmoothV(j)%ip(grid%Smooth%yNsmthV), &
              grid%Smooth%ySmoothV(j)%wt(grid%Smooth%yNsmthV),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetSmooth2D'
      error%Message = 'Error allocating interpolation point arrays for y-staggered smooth met fields'
      GOTO 9999
    END IF

    IF( yOdd )THEN
      DO ii = 1,nFil+1
        grid%Smooth%ySmoothV(j)%ip(ii) = SWIMlimit(2*j-2-ns+ii,1,nyp)
        grid%Smooth%ySmoothV(j)%wt(ii) = wtFilStg(ii)
      END DO
    ELSE
      ry = grid%Ymin - gridp%Ymin
      ry = (ry + (FLOAT(j)-0.5)*grid%dY - 0.5*gridp%dY)/gridp%dY
      jc = INT(ry) + 1
      jm = jc - (ns+1)
      jp = jc + (ns+1)
      i  = 0
      DO jj = jm,jp
        i = i + 1
        yp = FLOAT(jj) - 1.
        jc = SWIMlimit( jj,1,nyp )
        grid%Smooth%ySmoothV(j)%ip(i) = jc
        grid%Smooth%ySmoothV(j)%wt(i) = FilterFacStg( ry,yp )
      END DO

    END IF

  END DO

END IF Staggered

SetSmooth2D = SWIMresult

9999 CONTINUE

IF( ALLOCATED(wtFil)    )DEALLOCATE( wtFil,STAT=alloc_stat )
IF( ALLOCATED(wtFilStg) )DEALLOCATE( wtFilStg,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION LimitX( i,nx,lPeriodic ) RESULT( ii )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i, nx
LOGICAL, INTENT( IN ) :: lPeriodic

INTEGER, EXTERNAL :: SWIMlimit

IF( lPeriodic )THEN

  IF( i < 1 )THEN
    ii = i + (nx-1)
  ELSE IF( i > nx )THEN
    ii = i - (nx-1)
  ELSE
    ii = i
  END IF

ELSE

  ii = SWIMlimit( i,1,nx )

END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMaddSmoothPotential( ifldp )

!------ Add potential smoothed fields for parent field ifldp

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMaddSmoothPotential

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr
USE constants_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifldp

INTEGER irv, nadd
REAL    x

INTEGER, EXTERNAL :: SWIMreallocMetField

!------ Initialize failure

SWIMaddSmoothPotential = SWIMfailure

!------ Make sure potential parent has the right values

IF( ifldp > numField .OR. ifldp < 1 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMaddSmoothPotential'
  error%Message = 'Parent field does not exist'
  GOTO 9999
END IF

!------ Do not build smooth fields if parent grid is very small
!       or if it's already been requested

IF( field(ifldp)%grid%nX < NMIN_SMTH  .OR. &
    field(ifldp)%grid%nY < NMIN_SMTH  .OR. &
    BTEST(field(ifldp)%status,FSB_INITSMOOTH) )THEN
  SWIMaddSmoothPotential = SWIMsuccess
  GOTO 9999
END IF

!------ Number of potential smooth fields based on MAX(nx,ny)

x    = FLOAT(MAX(field(ifldp)%grid%nX,field(ifldp)%grid%nY))
nadd = MAX(CEILING( (LOG(x)-LOG(FLOAT(NMIN_SMTH)))/LOG2 ),1)

!------ Add met fields

irv = SWIMreallocMetField( -nadd )
IF( irv /= SWIMsuccess )GOTO 9999

field(ifldp)%status = IBSET(field(ifldp)%status,FSB_INITSMOOTH)

!------ Another SWIMming success

SWIMaddSmoothPotential = SWIMsuccess

9999 CONTINUE

RETURN
END
