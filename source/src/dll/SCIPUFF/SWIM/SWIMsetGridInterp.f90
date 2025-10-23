INTEGER FUNCTION SetGridInterp( type,src,grid )

!------ Setup factors for grid interpolation: FROM src TO grid
!       N.B. This routine only called from SWIMinitGridded for climo or SCIP
!       gridded w/terrain and InitObsUncertainty

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),      INTENT( IN    ) :: type
TYPE( GridSrc ), INTENT( INOUT ) :: src
TYPE( MetGrid ), INTENT( IN    ) :: grid

INTEGER irv

INTERFACE
  INTEGER FUNCTION SetGridInterpZ( src,grid,height3d )
    USE SWIMmetField_fd
    TYPE( GridSrc ), TARGET,     INTENT( INOUT ) :: src      !Input
    TYPE( MetGrid ),             INTENT( IN    ) :: grid     !Output grid
    REAL, DIMENSION(:), POINTER, OPTIONAL        :: height3d !Time-varying 3d height field
  END FUNCTION SetGridInterpZ
END INTERFACE

INTEGER, EXTERNAL :: SetGridInterp2d, AllocGridInterp

SetGridInterp = SWIMfailure

!------ Set horizontal factors

irv = SetGridInterp2d( src,grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set vertical factors

irv = SetGridInterpZ( src,grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set stagger interpolation flag

src%IntrpFac%Stagger = BTEST(src%type,GTB_STAGGERZ)

!------ Allocate field arrays for reading (and input for interpolation)

irv = AllocGridInterp( type,src )
IF( irv /= SWIMsuccess )GOTO 9999

SetGridInterp = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetGridInterp2d( src,grid )

!------ Setup factors for horizontal grid interpolation
!       FROM src TO grid

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src
TYPE( MetGrid ), INTENT( IN    ) :: grid

TYPE( MetGrid  ), POINTER :: grids
TYPE( MapCoord )          :: coords

TYPE( met1dh ), DIMENSION(:), ALLOCATABLE :: mx, mxu, mxv, my, myu, myv

INTEGER alloc_stat, i, j, i0, is, irv
INTEGER nx, ny, nxy
INTEGER nxs, nys, nxys
REAL    xmin, ymin, dx, dy, x, y, mapfac, sX0, sY0
REAL    dxs, dys
INTEGER gType
REAL    x1, y1, xu, yv, xp, yp
LOGICAL lter

INTEGER, EXTERNAL :: SWIMcnvCoord

CHARACTER(128), EXTERNAL :: ArraySizeStr

SetGridInterp2d = SWIMfailure

!------ Set locals from output grid

nx  = grid%nx; xmin = grid%Xmin; dx = grid%dX !Output grid
ny  = grid%ny; ymin = grid%Ymin; dy = grid%dY
nxy = nx*ny

!------ Set locals for source grid

IF( BTEST(src%type,GSB_NEST) )THEN

  grids => field(src%unit)%grid

  nxs = grids%nX; sX0 = grids%Xmin; dxs = grids%dX
  nys = grids%nY; sY0 = grids%Ymin; dys = grids%dY

  coords = grids%coord
  gType  = grids%type

ELSE

  nxs = src%nX; sX0 = src%X0 + FLOAT(src%iStart-1)*src%dX; dxs = src%dX
  nys = src%nY; sY0 = src%Y0 + FLOAT(src%jStart-1)*src%dY; dys = src%dY

  IF( BTEST(src%type,GSB_LATLON) )THEN
    coords%type = I_LATLON
  ELSE IF( BTEST(src%type,GSB_UTM) )THEN
    coords%type = I_UTM
    coords%zone = src%UTMzone
  ELSE  !Assume that source matches coord of grid
    coords%type = grid%coord%type
  END IF

  gType = 0

  IF( src%nStg3d > 1 )  THEN

    gType = IBSET(gType,GTB_STAGGER)   !Arakawa C is default horizontal stagger

    DO i = 1,src%nStg3D
      IF( src%iStg3D(3,i) /= 0 )THEN
        gType = IBSET(gType,GTB_STAGGERZ)
      END if
      IF( src%iStg3D(1,i) /= 0 .AND. src%iStg3D(2,i) /= 0 )THEN
        gType = IBSET(gType,GTB_STAGGERB)  !Arakawa B (assumed if any variable is shifted in x and y)
      END IF
    END DO

  END IF

END IF

nxys = nxs*nys
lter = BTEST(grid%type,GTB_TERRAIN)

!------ Allocate grid factor 2d arrays

ALLOCATE( mx(nxy),my(nxy),mxu(nxy),myu(nxy),mxv(nxy),myv(nxy),STAT=alloc_stat )
IF( alloc_stat == 0 ) &
  ALLOCATE( src%IntrpFac%mh(nxy),src%IntrpFac%mhu(nxy),src%IntrpFac%mhv(nxy),STAT=alloc_stat )
IF( alloc_stat == 0 ) &
  ALLOCATE( src%IntrpFac%DiffLandU(nxy),src%IntrpFac%DiffLandV(nxy),STAT=alloc_stat )
IF( alloc_stat == 0 ) &
  ALLOCATE( src%IntrpFac%zblu(nxy),src%IntrpFac%zblv(nxy),STAT=alloc_stat )
IF( alloc_stat == 0 ) &
  ALLOCATE( src%IntrpFac%zbluS(nxy),src%IntrpFac%zblvS(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMsetGridInterp'
  error%Message = 'Error allocating grid interpolation arrays'
  error%Inform  =  ArraySizeStr( 1,(/nxy/) )
  GOTO 9999
END IF

mapfac = 1. !Not used

!------ Loop over output grid

DO j = 1,ny
  y  = ymin + FLOAT(j-1)*dy
  yv = y + 0.5*dy
  i0 = (j-1)*nx

  DO i = 1,nx
    x  = xmin + FLOAT(i-1)*dx
    xu = x + 0.5*dx
    is = i0 + i

!------ Convert to source grid coordinates

    irv = SWIMcnvCoord( x,y,grid%coord,xp,yp,coords )
    IF( irv /= SWIMsuccess )GOTO 9999

!----- Set 1D horizontal grid factors for grid centers

    CALL get_1dfac( xp,sx0,dxs,nxs,mapfac,mx(is) )
    CALL get_1dfac( yp,sy0,dys,nys,mapfac,my(is) )

!------ Set for staggered grids

    IF( BTEST(grid%type,GTB_STAGGER) )THEN

!------ First do u-point

      irv = SWIMcnvCoord( xu,y,grid%coord,xp,yp,coords )
      IF( irv /= SWIMsuccess )GOTO 9999

      x1 = sx0
      y1 = sy0
      IF( BTEST(gType,GTB_STAGGER) )THEN !Check stagger of source grid
        x1 = x1 + 0.5*dxs
        IF( BTEST(gType,GTB_STAGGERB) )y1 = y1 + 0.5*dys
      END IF
      CALL get_1dfac( xp,x1,dxs,nxs,mapfac,mxu(is) )
      CALL get_1dfac( yp,y1,dys,nys,mapfac,myu(is) )

!------ Then v-point

      irv = SWIMcnvCoord( x,yv,grid%coord,xp,yp,coords )
      IF( irv /= SWIMsuccess )GOTO 9999

      x1 = sx0
      y1 = sy0
      IF( BTEST(gType,GTB_STAGGER) )THEN !Check stagger of source grid
        y1 = y1 + 0.5*dys
        IF( BTEST(gType,GTB_STAGGERB) )x1 = x1 + 0.5*dxs
      END IF
      CALL get_1dfac( xp,x1,dxs,nxs,mapfac,mxv(is) )
      CALL get_1dfac( yp,y1,dys,nys,mapfac,myv(is) )

    ELSE IF( BTEST(gType,GTB_STAGGER) )THEN

!------ Set for staggered input, unstaggered output (should never actually occur)

      x1 = sx0 + 0.5*dxs
      CALL get_1dfac( xp,x1,dxs,nxs,mapfac,mxu(is) )
      IF( BTEST(gType,GTB_STAGGERB) )THEN
        y1 = sy0 + 0.5*dys
        CALL get_1dfac( yp,y1,dys,nys,mapfac,myu(is) )
      ELSE
        myu = my
      END IF

      y1 = sy0 + 0.5*dys
      CALL get_1dfac( yp,y1,dys,nys,mapfac,myv(is) )
      IF( BTEST(gType,GTB_STAGGERB) )THEN
        x1 = sx0 + 0.5*dxs
        CALL get_1dfac( xp,x1,dxs,nxs,mapfac,mxv(is) )
      ELSE
        mxv(is) = mx(is)
      END IF

    ELSE

      mxu(is) = mx(is); myu(is) = my(is)
      mxv(is) = mx(is); myv(is) = my(is)

    END IF

  END DO
END DO

!------ Set 2D factors

DO i = 1,nxy
  CALL SetMXY( mx(i), my(i), src%IntrpFac%mh(i), nxs,nxys,lter )
  CALL SetMXY( mxu(i),myu(i),src%IntrpFac%mhu(i),nxs,nxys,lter )
  CALL SetMXY( mxv(i),myv(i),src%IntrpFac%mhv(i),nxs,nxys,lter )
END DO

SetGridInterp2d = SWIMresult

9999 CONTINUE

IF( ALLOCATED(mx)  )DEALLOCATE( mx,STAT=alloc_stat )
IF( ALLOCATED(my)  )DEALLOCATE( my,STAT=alloc_stat )
IF( ALLOCATED(mxu) )DEALLOCATE( mxu,STAT=alloc_stat )
IF( ALLOCATED(myv) )DEALLOCATE( myv,STAT=alloc_stat )
IF( ALLOCATED(mxv) )DEALLOCATE( mxu,STAT=alloc_stat )
IF( ALLOCATED(myu) )DEALLOCATE( mxu,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION SetGridInterpZ( src,grid,pZ3d )

!------ Setup factors for vertical grid interpolation
!       FROM src TO grid

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( GridSrc ), TARGET,     INTENT( INOUT ) :: src  !Input
TYPE( MetGrid ),             INTENT( IN    ) :: grid !Output grid
REAL, DIMENSION(:), POINTER, OPTIONAL        :: pZ3d !Time-varying 3d height field

INTEGER alloc_stat, i0, is, k, ip, i, j, ii, jj, iu, iv, i0p, ifld
INTEGER ij0u, ij0v, iju, ijv
INTEGER j0, jp0, k0, ipu, ipv, gType
INTEGER nx, ny, nz, nxy, nxyz
INTEGER nxs, nys
INTEGER nzs, nxys
REAL    z, zu, zv, zw, d, du, dv, zru, zrv, hcu, hcv, alpu, alpv, h
REAL    zblu, zblv, zbluS, zblvS, zmin
REAL    zlim
INTEGER km
LOGICAL lVertVel, lVertStg, lDiffLandCharU, lDiffLandCharV

TYPE( MetGrid ), POINTER :: grids

REAL,    DIMENSION(:), POINTER :: dSrc, duSrc, dvSrc, hcSrc, alpSrc, zrfSrc
INTEGER, DIMENSION(:), POINTER :: kblSrc
REAL,    DIMENSION(:), POINTER :: zblSrc
REAL,    DIMENSION(:), POINTER :: zr, hc, alpha, pZw
REAL,    DIMENSION(:), POINTER :: zIntrp, zuIntrp, zvIntrp, zwIntrp
REAL,    DIMENSION(:), POINTER :: zVelU, zVelV
REAL,    DIMENSION(:), POINTER :: hs, zk, z3d

CHARACTER(128), EXTERNAL :: ArraySizeStr

SetGridInterpZ = SWIMfailure

!------ Set locals

nx  = grid%nx; ny = grid%ny; nz  = grid%nz; nxy = nx*ny; nxyz = nxy*nz !Output grid
nxys = src%nx*src%ny;   nzs = src%nz                                   !Input  grid

lVertVel = .FALSE.

NULLIFY( zVelU,zVelV,grids )
NULLIFY( zIntrp,zuIntrp,zvIntrp,zwIntrp )

!------ Allocate grid factor arrys
!       N.B. Do not allocate for subsequent calls for 3d height fields

IF( .NOT.ASSOCIATED(src%IntrpFac%mz) )THEN

  ALLOCATE( src%IntrpFac%mz(nxyz),src%IntrpFac%mzu(nxyz),src%IntrpFac%mzv(nxyz),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    error%Routine = 'SWIMsetGridInterpZ'
    error%Message = 'Error allocating grid interpolation arrays'
    error%Inform  =  ArraySizeStr( 1,(/nxyz/) )
    GOTO 9999
  END IF

END IF

!------ Point to parent grid for nested field

IF( BTEST(src%type,GSB_NEST) )THEN
  ifld  =  src%unit
  grids => field(ifld)%grid
  lVertVel = BTEST(field(ifld)%type,FTB_W)
  lVertStg = BTEST(field(ifld)%grid%type,GTB_STAGGERZ)

  dSrc   => grids%terrain%D
  duSrc  => grids%terrain%Du
  dvSrc  => grids%terrain%Dv
  hcSrc  => grids%landcover%canopyHt
  alpSrc => grids%landcover%alpha
  zrfSrc => grids%landcover%roughness
  kblSrc => grids%landcover%kbl
  hs     => grids%terrain%h

  nxs = grids%nX
  nys = grids%nY

  gType = grids%type

  IF( lVertVel )THEN
    IF( .NOT.ASSOCIATED(src%IntrpFac%mzw) )THEN
      ALLOCATE( src%IntrpFac%mzw(nxyz),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMsetGridInterpZ'
        error%Message = 'Error allocating w-grid interpolation arrays'
        error%Inform  =  ArraySizeStr( 1,(/nxyz/) )
        GOTO 9999
      END IF
    END IF
    lVertVel = .TRUE.
    IF( lVertStg )THEN
      pZw => grid%Zw
    ELSE
      pZw => grid%Z
    END IF
  END IF

!------ Setup vertical arrays for interpolation from 3d height field

  IF( BTEST(grids%type,GTB_Z3D) )THEN

    ALLOCATE( zIntrp(nzs),zwIntrp(nzs),zuIntrp(nzs),zvIntrp(nzs),STAT=alloc_stat )  !at interpolation point
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMsetGridInterpZ'
      error%Message = 'Error allocating z-grid for sigma interpolation arrays'
      error%Inform  =  ArraySizeStr( 1,(/nzs/) )
      GOTO 9999
    END IF

    IF( BTEST(grids%type,GTB_SIGMA) .OR. BTEST(grids%type,GTB_Z3DW) )THEN             !N.B. Zw in sigma structure (if needed)
      z3d => grids%sigma%Z
    ELSE IF( BTEST(grids%type,GTB_STAGGERZ) )THEN
      z3d => pZ3d(nxys+1:)
    ELSE
      z3d => pZ3d
    END IF

    IF( .NOT.ASSOCIATED(z3d) )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMsetGridInterpZ'
      error%Message = 'Error pointing to 3d grid height field interpolation arrays'
      GOTO 9999
    END IF

    IF( BTEST(grids%type,GTB_STAGGER) )THEN

      ALLOCATE( zVelU(nxys*nzs),zVelV(nxys*nzs),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMsetGridInterpZ'
        error%Message = 'Error allocating z-grid for height interpolation arrays'
        error%Inform  =  ArraySizeStr( 1,(/nxys*nzs/) )
        GOTO 9999
      END IF

      IF( BTEST(grids%type,GTB_STAGGERB) )THEN          !Construct 3d arrays at staggered locations for velocity components

        DO j = 1,nys
          j0  = (j-1)*nxs
          jp0 = (MIN(j+1,nys)-1)*nxs

          DO i = 1,nxs
            ip = MIN(i+1,nxs)
            is = j0 + i
            DO k = 1,nzs
              k0 = (k-1)*nxys
              zk => z3d(k0+1:k0+nxys)
              zVelU(k0+is) = 0.25*(zk(is) + zk(j0+ip) + zk(jp0+i) + zk(jp0+ip))
              zVelV(k0+is) = zVelU(k0+is)
            END DO

          END DO
        END DO

      ELSE

        DO j = 1,nys
          j0 = (j-1)*nxs
          jp0 = (MIN(j+1,nys)-1)*nxs

          DO i = 1,nxs
            ip = MIN(i+1,nxs)
            is = j0 + i

            DO k = 1,nzs
              k0 = (k-1)*nxys
              zk => z3d(k0+1:k0+nxys)
              zVelU(k0+is) = 0.5*(zk(is) + zk(j0+ip))
              zVelV(k0+is) = 0.5*(zk(is) + zk(jp0+i))
            END DO

          END DO
        END DO

      END IF

    END IF

  END IF

!------ Set surface layer reference heights for source grid

  ALLOCATE( zblSrc(nxys),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    error%Routine = 'SWIMsetGridInterpZ'
    error%Message = 'Error allocating zbl interpolation array'
    error%Inform  =  ArraySizeStr( 1,(/nxys/) )
    GOTO 9999
  END IF

  IF( BTEST(grids%type,GTB_Z3D) )THEN
    DO is = 1,nxys
      i = (kblSrc(is)-1)*nxys + is
      zblSrc(is) = MAX(12.5*zrfSrc(is),1.25*hcSrc(is))
      zblSrc(is) = MIN(z3d(i),zblSrc(is))
    END DO
  ELSE
    DO is = 1,nxys
      zblSrc(is) = MAX(12.5*zrfSrc(is),1.25*hcSrc(is))
      zblSrc(is) = MIN(grids%Z(kblSrc(is))*dSrc(is),zblSrc(is))
    END DO
  END IF

ELSE  !---- Src is not a nest

  gType = 0

  IF( src%nStg3d > 0 )  THEN

    gType = IBSET(gType,GTB_STAGGER)   !Arakawa C is default horizontal stagger

    DO i = 1,src%nStg3D
      IF( src%iStg3D(3,i) /= 0 )THEN
        gType = IBSET(gType,GTB_STAGGERZ)
      END if
      IF( src%iStg3D(1,i) /= 0 .AND. src%iStg3D(2,i) /= 0 )THEN
        gType = IBSET(gType,GTB_STAGGERB)  !Arakawa B (assumed if any variable is shifted in x and y)
      END IF
    END DO

  END IF

  zbluS = MAX( 12.5*Prj%BL%zruf,1.25*Prj%BL%hc)
  zblvS = zbluS

END IF

zr    => grid%landcover%roughness
hc    => grid%landcover%canopyHt
alpha => grid%landcover%alpha

!------ Initialize vertical interpolation grid (but defined later for sigma coordinates)

IF( .NOT.BTEST(gType,GTB_Z3D) )THEN
  zIntrp  => src%Z
  zuIntrp => src%Z
  zvIntrp => src%Z
  zwIntrp => src%Zw
  zmin    =  grid%Z(1)
END IF

!------ Loop over output horizontal grid
!       N.B. There are checks for staggered grids but note that this routine is currently only
!       used for C-grids (for use in mass-consistent ajdustment)

DO j = 1,ny
  i0  = (j-1)*nx
  IF( BTEST(grid%type,GTB_STAGGER) )i0p = (MIN(j+1,ny)-1)*nx

  DO i = 1,nx
    is = i0 + i

    IF( BTEST(grid%type,GTB_STAGGER) )THEN
      iu   = i0 + MIN(i+1,nx)          ; iv   = i0p + i
      zru  = 0.5*(zr(is)+zr(iu))       ; zrv  = 0.5*(zr(is)+zr(iv))
      hcu  = 0.5*(hc(is)+hc(iu))       ; hcv  = 0.5*(hc(is)+hc(iv))
      alpu = 0.5*(alpha(is)+alpha(iu)) ; alpv = 0.5*(alpha(is)+alpha(iv))
      zblu = MAX(20.*zru,2.*hcu)       ; zblv = MAX(20.*zrv,2.*hcv)
    ELSE
      iu   = is                  ; iv   = is
      zru  = zr(is)              ; zrv  = zru
      hcu  = hc(is)              ; hcv  = hcu
      alpu = alpha(is)           ; alpv = alpu
      zblu = MAX(20.*zru,2.*hcu) ; zblv = zblu
    END IF

!------ Set "D" and reference height from source grid at interpolation point; check if
!       land characteristics differ significantly between input & output grids

    IF( BTEST(src%type,GSB_NEST) )THEN

      IF( BTEST(grids%type,GTB_Z3D) )THEN
        zmin = z3d(src%IntrpFac%mh(is)%ij) !Don't bother to interpolate
        zmin = MAX(zmin,10.)
      END IF

      CALL IntXY( src%IntrpFac%mh(is) ,dSrc, d )
      CALL IntXY( src%IntrpFac%mhu(is),duSrc,du )
      CALL IntXY( src%IntrpFac%mhv(is),dvSrc,dv )

      IF( src%IntrpFac%mh(is)%nxyi == 1 )THEN

        lDiffLandCharU = ABS(hcSrc(1)-hcu)   / MAX(hcSrc(1),hcu,zmin) > 0.2 .OR. &
                         ABS(alpSrc(1)-alpu) / MAX(alpSrc(1),alpu,1.) > 0.2 .OR. &
                         ABS(zrfSrc(1)-zru)  / MAX(zrfSrc(1),zru,0.1) > 0.2
        lDiffLandCharV = ABS(hcSrc(1)-hcv)   / MAX(hcSrc(1),hcv,zmin) > 0.2 .OR. &
                         ABS(alpSrc(1)-alpv) / MAX(alpSrc(1),alpv,1.) > 0.2 .OR. &
                         ABS(zrfSrc(1)-zrv)  / MAX(zrfSrc(1),zrv,0.1) > 0.2

        zbluS = zblSrc(1)
        zblvS = zblSrc(1)
        zblu  = MAX( zblu,20.*zrfSrc(1),2.*hcSrc(1))
        zblv  = MAX( zblv,20.*zrfSrc(1),2.*hcSrc(1))

      ELSE

        lDiffLandCharU = .FALSE.
        lDiffLandCharV = .FALSE.
        ij0u = src%IntrpFac%mhu(is)%ij
        ij0v = src%IntrpFac%mhv(is)%ij
        zbluS = 0.; zblvS = 0.
        Search : DO jj = 0,1
          iju = ij0u + jj*src%IntrpFac%mh(is)%nxi
          ijv = ij0v + jj*src%IntrpFac%mh(is)%nxi
          DO ii = 0,1
            iju = iju + ii
            ijv = ijv + ii
            lDiffLandCharU = lDiffLandCharU .OR. &
                             ABS(hcSrc(iju)-hcu)   / MAX(hcSrc(iju),hcu,zmin) > 0.2 .OR. &
                             ABS(alpSrc(iju)-alpu) / MAX(alpSrc(iju),alpu,1.) > 0.2 .OR. &
                             ABS(zrfSrc(iju)-zru)  / MAX(zrfSrc(iju),zru,0.1) > 0.2
            lDiffLandCharV = lDiffLandCharv .OR. &
                             ABS(hcSrc(ijv)-hcv)   / MAX(hcSrc(ijv),hcv,zmin) > 0.2 .OR. &
                             ABS(alpSrc(ijv)-alpv) / MAX(alpSrc(ijv),alpv,1.) > 0.2 .OR. &
                             ABS(zrfSrc(ijv)-zrv)  / MAX(zrfSrc(ijv),zrv,0.1) > 0.2
            zbluS = MAX(zbluS,zblSrc(iju))
            zblvS = MAX(zblvS,zblSrc(ijv))
            zblu  = MAX( zblu,20.*zrfSrc(iju),2.*hcSrc(iju))
            zblv  = MAX( zblv,20.*zrfSrc(ijv),2.*hcSrc(ijv))
          END DO
        END DO Search

      END IF

!------ Setup interpolating vertical grid

      IF( BTEST(gType,GTB_Z3D) )THEN

        DO k = 1,nzs
          ip =  (k-1)*nxys + 1
          zk => z3d(ip:); CALL IntXY( src%IntrpFac%mh(is),zk,zIntrp(k)  )
          IF( lVertVel )THEN
            IF( BTEST(gType,GTB_STAGGERZ) )THEN
              zk => grids%sigma%Zw(ip:); CALL IntXY( src%IntrpFac%mh(is),zk,zwIntrp(k) )
            ELSE
              zwIntrp(k) = zIntrp(k)
            END IF
          END IF
          IF( BTEST(gType,GTB_STAGGER) )THEN
            zk => zVelU(ip:); CALL IntXY( src%IntrpFac%mhu(is),zk,zuIntrp(k) )
            zk => zVelV(ip:); CALL IntXY( src%IntrpFac%mhv(is),zk,zvIntrp(k) )
          ELSE
            zuIntrp(k) = zIntrp(k)
            zvIntrp(k) = zIntrp(k)
          END IF
        END DO

      END IF

    ELSE

      d = 1.; du = 1.; dv = 1.

      IF( BTEST(grid%type,GTB_STAGGER) )THEN
        lDiffLandCharU = ABS(Prj%BL%hc-hcu)     / MAX(Prj%BL%hc,hcu,zmin)   > 0.2 .OR. &
                         ABS(Prj%BL%alpha-alpu) / MAX(Prj%BL%alpha,alpu,1.) > 0.2
        lDiffLandCharV = ABS(Prj%BL%hc-hcv)     / MAX(Prj%BL%hc,hcv,zmin)   > 0.2 .OR. &
                         ABS(Prj%BL%alpha-alpv) / MAX(Prj%BL%alpha,alpv,1.) > 0.2
      ELSE
        lDiffLandCharU = ABS(Prj%BL%hc-hc(is))       / MAX(Prj%BL%hc,hc(is),zmin)     > 0.2 .OR. &
                         ABS(Prj%BL%alpha-alpha(is)) / MAX(Prj%BL%alpha,alpha(is),1.) > 0.2
        lDiffLandCharV = lDiffLandCharU
      END IF

      zblu  = MAX( zblu,20.*Prj%BL%zruf,2.*Prj%BL%hc)
      zblv  = MAX( zblv,20.*Prj%BL%zruf,2.*Prj%BL%hc)

    END IF

!------ Save heights for surface layer profile

    src%IntrpFac%DiffLandU(is) = lDiffLandCharU; src%IntrpFac%DiffLandV(is) = lDiffLandCharV
    src%IntrpFac%zbluS(is)     = zbluS         ; src%IntrpFac%zblvS(is)     = zblvS
    src%IntrpFac%zblu(is)      = zblu          ; src%IntrpFac%zblv(is)      = zblv

    h = grid%hmin + grid%terrain%H(is)  !Elevation above MSL

    zlim = 2.                           !Limit for lowest interpolation level

!------ Loop over vertical levels

    DO k = 1,nz
      ip  = (k-1)*nxy + is
      ipu = (k-1)*nxy + iu
      ipv = (k-1)*nxy + iv

!------ Set transformed height (in source coord) of interpolation point

      IF( BTEST(grid%type,GTB_Z3D) )THEN
        z  = z3d(ip)
        zu = 0.5*(grid%sigma%Z(ip) + grid%sigma%Z(ipu))
        zv = 0.5*(grid%sigma%Z(ip) + grid%sigma%Z(ipv))
      ELSE
        z  = grid%Z(k)*grid%terrain%D(is)  / d
        zu = grid%Z(k)*grid%terrain%Du(is) / du
        zv = grid%Z(k)*grid%terrain%Dv(is) / dv
        zlim = zlim*grid%terrain%D(is)  / d
      END IF

!------ Set vertical location and interpolation factors

      IF( k == 1 )zlim = MIN(z,zu,zv,zlim)
      CALL SetZfacSL( z,zIntrp,nzs,src%IntrpFac%mz(ip),0.,0.,zlim )
      IF( BTEST(gType,GTB_STAGGER) )THEN
        CALL SetZfacSL( zu,zuIntrp,nzs,src%IntrpFac%mzu(ip),0.,0.,zlim )
        CALL SetZfacSL( zv,zvIntrp,nzs,src%IntrpFac%mzv(ip),0.,0.,zlim )
      ELSE
        src%IntrpFac%mzu(ip) = src%IntrpFac%mz(ip)
        src%IntrpFac%mzv(ip) = src%IntrpFac%mz(ip)
      END IF

      IF( lVertVel )THEN
        zw = pZw(k)*grid%terrain%D(is) / d
        CALL SetZfacSL( zw,zwIntrp,nzs,src%IntrpFac%mzw(ip),0.,0.,zlim )
      END IF

!------ Save heights for Temp/Press extrapolation

      km = ABS(src%IntrpFac%mz(ip)%km)
      IF( z < zIntrp(km) )THEN
        src%IntrpFac%mz(ip)%zp = zIntrp(km)*d + h
        src%IntrpFac%mz(ip)%zm = z*d + h
      ELSE IF( z > zIntrp(nzs) )THEN
        src%IntrpFac%mz(ip)%zm = z*d + h
        src%IntrpFac%mz(ip)%zp = zIntrp(nzs)*d + h
      END IF

    END DO

  END DO
END DO

SetGridInterpZ = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(zblSrc) )DEALLOCATE( zblSrc,STAT=alloc_stat )

IF( ASSOCIATED(grids) )THEN
  IF( BTEST(grids%type,GTB_Z3D) )THEN
    IF( ASSOCIATED(zIntrp)  )DEALLOCATE( zIntrp, STAT=alloc_stat )
    IF( ASSOCIATED(zuIntrp) )DEALLOCATE( zuIntrp,STAT=alloc_stat )
    IF( ASSOCIATED(zvIntrp) )DEALLOCATE( zvIntrp,STAT=alloc_stat )
    IF( ASSOCIATED(zwIntrp) )DEALLOCATE( zwIntrp,STAT=alloc_stat )
    IF( ASSOCIATED(zVelU)   )DEALLOCATE( zVelU,STAT=alloc_stat )
    IF( ASSOCIATED(zVelV)   )DEALLOCATE( zVelV,STAT=alloc_stat )
  END IF
  NULLIFY(grids)
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION AllocGridInterp( type,src )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),       INTENT( IN    ) :: type
TYPE( GridSrc  ), INTENT( INOUT ) :: src

INTEGER alloc_stat, irv, nxy, nxyz

INTEGER, EXTERNAL :: SWIMalloc3dField, SWIMallocVariance

CHARACTER(128), EXTERNAL :: ArraySizeStr

AllocGridInterp = SWIMfailure

!------ Define locals

nxy  = src%nXY
nxyz = src%nXY*src%nZ

!------ Allocate mean field arrays

irv = SWIMalloc3dField( type,nxyz,src%ReadField )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Large-scale variance

IF( BTEST(type,FTB_LSV) )THEN
  irv = SWIMallocVariance( nxyz,src%ReadLSV )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Boundary layer fields

alloc_stat = 0
IF( BTEST(type,FTB_ZI  )                      )ALLOCATE( src%ReadBL%zi(nxy)      ,STAT=alloc_stat )
IF( BTEST(type,FTB_HFLX).AND. alloc_stat == 0 )ALLOCATE( src%ReadBL%HeatFlux(nxy),STAT=alloc_stat )
IF( BTEST(type,FTB_MOL) .AND. alloc_stat == 0 )ALLOCATE( src%ReadBL%invMOL(nxy)  ,STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'AllocGridInterp'
  error%Message = 'Error allocating BL arrays'
  error%Inform  =  ArraySizeStr( 1,(/nxy/) )
  GOTO 9999
END IF

AllocGridInterp = SWIMresult

9999 CONTINUE

RETURN
END
