MODULE SWIMintrpGrid_fi

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer
USE SWIMinterp_fd
USE constants_fd

CONTAINS

!==============================================================================

SUBROUTINE SWIMinterpGridded( fieldType,BLType,time,grid,IntrpFac, &
                              nxi,nyi,nzi,inpFld,inpBL,inpLSV, &
                              inpGrid,inpZ, &
                              nxo,nyo,nzo,outFld,outBL,outLSV )

!------ Interpolate gridded fields (3d & 2d)

IMPLICIT NONE

INTEGER(8),         INTENT( IN ) :: fieldType
INTEGER,            INTENT( IN ) :: BLType
REAL,               INTENT( IN ) :: time
TYPE( MetGrid ),    INTENT( IN ) :: grid
TYPE( GridIntrp ),  INTENT( IN ) :: IntrpFac
INTEGER,            INTENT( IN ) :: nxi, nyi, nzi
INTEGER,            INTENT( IN ) :: nxo, nyo, nzo

TYPE( MetMean3D   ), OPTIONAL, INTENT( IN  ) :: inpFld
TYPE( MetBLparam  ), OPTIONAL, INTENT( IN  ) :: inpBL
TYPE( MetVariance ), OPTIONAL, INTENT( IN  ) :: inpLSV
TYPE( MetGrid     ), OPTIONAL, INTENT( IN  ) :: inpGrid
REAL, DIMENSION(:),  OPTIONAL, INTENT( IN  ) :: inpZ

TYPE( MetMean3D   ), OPTIONAL, INTENT( OUT ) :: outFld
TYPE( MetBLparam  ), OPTIONAL, INTENT( OUT ) :: outBL
TYPE( MetVariance ), OPTIONAL, INTENT( OUT ) :: outLSV

TYPE( metv ) :: mv

TYPE( metv ), DIMENSION(:), POINTER :: mz, mzu, mzv, mzw
TYPE( meth ), DIMENSION(:), POINTER :: mh, mhu, mhv

REAL, DIMENSION(:), POINTER :: u,  v,  w,  t,  h,  p
REAL, DIMENSION(:), POINTER :: us, vs,     ts, hs, ps
REAL, DIMENSION(:), POINTER :: q, qs
REAL, DIMENSION(:), POINTER :: ws
REAL, DIMENSION(:), POINTER :: zi,  hflx,  invMOL,  ustr,  prcp,  rainprb,  cc
REAL, DIMENSION(:), POINTER :: zis, hflxs, invMOLs, ustrs, prcps, rainprbs, ccs
REAL, DIMENSION(:), POINTER :: uul,  vvl,  uvl,  sbl,  zsl,  usl,  vsl,  invL
REAL, DIMENSION(:), POINTER :: uuls, vvls, uvls, sbls, zsls, usls, vsls, invLs
REAL, DIMENSION(:), POINTER :: zruf,  hc,  alpha,  bowen,  albedo
REAL, DIMENSION(:), POINTER :: zrufs, hcs, alphas, bowens, albedos, zrefs, elevs
REAL, DIMENSION(:), POINTER :: zrufp, hcp, alp, invLp, zslp
REAL, DIMENSION(:), POINTER :: lats, lons
REAL, DIMENSION(:), POINTER :: zs
REAL, DIMENSION(:), POINTER :: zs_ptr
REAL, DIMENSION(:), POINTER :: zblu, zblv, zbluP, zblvP
REAL, DIMENSION(:), POINTER :: Z

LOGICAL, DIMENSION(:), POINTER :: DiffLandU, DiffLandV

INTEGER, DIMENSION(:), POINTER :: kbls, kbl

REAL,    DIMENSION(:), ALLOCATABLE, TARGET :: zuIntrp, zvIntrp
REAL,    DIMENSION(:), POINTER :: zVelU, zVelV
REAL,    DIMENSION(:), POINTER :: zk, z3d

INTEGER nxs, nys, nzs, nxys, j0, jp0
INTEGER alloc_stat, is, k, iz, ip, k0, i, j, i0, i0p, ipp, iup, ivp
INTEGER nxyi, nxyo, iflag, kOffset, iprcp
REAL    gx, gy, dp, dum1, dum2, dum3, dum
REAL    zref, uref, vref, Tref, pref, zp
REAL    inpZi, inpHflx, inpL, inpLat, inpLon, zh, L
REAL    inpUstr
REAL    inpLat0, inpLon0
REAL    zslu,  zrufu,  hcu,  alpu,  Lu, du
REAL    zslv,  zrufv,  hcv,  alpv,  Lv, dv
REAL    zslpu, zrfpu, hcpu, alppu, Lpu
REAL    zslpv, zrfpv, hcpv, alppv, Lpv
REAL    zInfl, zInflp, fsl, fslp, f, fp, facp
REAL    Pgrd, Px, dRH
LOGICAL lMean, lBL, lSL, lLSV, lLSVL
LOGICAL lGrdI
LOGICAL lVel, lVertVel
LOGICAL lstaggerS, lstagger

INTERFACE
  SUBROUTINE SetOmega( grid,u,v,w,ww,inverse )
    USE SWIM_fi
    TYPE( MetGrid ),   INTENT( IN ) :: grid
    REAL, DIMENSION(:),   POINTER   :: u, v, w, ww
    LOGICAL, OPTIONAL, INTENT( IN ) :: inverse
  END SUBROUTINE SetOmega
END INTERFACE

REAL, EXTERNAL :: StndTpot, StndLogP, InvLfromL, LfromInvL, SWIMrlimit

!----- Define locals; determine which fields are to be interpolated

lMean = PRESENT(inpFld)
lBL   = PRESENT(inpBL)
lSL   = lMean .AND. BLType /= BLP_NONE
lLSV  = PRESENT(inpLSV) .AND. BTEST(fieldType,FTB_LSV)
lLSVL = lLSV .AND. BTEST(fieldType,FTB_LSVL)
lGrdI = PRESENT(inpGrid)

IF( lMean )THEN
  lVel     = .TRUE.
  lVertVel = ASSOCIATED(inpFld%W) .AND. BTEST(fieldType,FTB_NEST) & !Interpolate W only
                                  .AND. BTEST(fieldType,FTB_W)      !for nested fields
ELSE
  lVel     = .FALSE.
  lVertVel = .FALSE.
END IF

NULLIFY( zVelU,zVelV,z3d )

nxyi = nxi*nyi
nxyo = nxo*nyo

mz  => IntrpFac%mz
mzu => IntrpFac%mzu
mzv => IntrpFac%mzv
mzw => IntrpFac%mzw
mh  => IntrpFac%mh
mhu => IntrpFac%mhu
mhv => IntrpFac%mhv

IF( lMean )THEN

  us => inpFld%U ; u => outFld%U
  vs => inpFld%V ; v => outFld%V

  IF( lVertVel )THEN
    IF( BTEST(inpGrid%type,GTB_TERRAIN) )THEN
      IF( BTEST(inpGrid%type,GTB_STAGGER) )THEN
        k = nxyi*(nzi+1)
      ELSE
        k = nxyi*nzi
      END IF
      ALLOCATE( ws(k),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMinterpGridded'
        error%Message = 'Error allocating vertical velocity array'
        GOTO 9999
      END IF
      DO i = 1,nxyi
        ws(i) = 0.
      END DO
      CALL SetOmega( inpGrid,us,vs,inpFld%W,ws )
    ELSE
      ws => inpFld%W
    END IF
  END IF

  w => outFld%W

  IF( BTEST(fieldType,FTB_T) )THEN
    ts => inpFld%Tpot
    t  => outFld%Tpot
  END IF
  IF( BTEST(fieldType,FTB_P) )THEN
    ps => inpFld%Press
    p  => outFld%Press
  END IF
  IF( BTEST(fieldType,FTB_H) )THEN
    hs => inpFld%Humid
    h  => outFld%Humid
  END IF
  IF( BTEST(fieldType,FTB_QCLD) )THEN
    qs => inpFld%Qcloud
    q  => outFld%Qcloud
  END IF

END IF

IF( lBL )THEN

  lBL = .FALSE.
  IF( BTEST(fieldType,FTB_ZI) .AND. ASSOCIATED(inpBL%zi)  )THEN
    zis => inpBL%zi
    zi  => outBL%zi
    lBL = .TRUE.
  END IF
  IF( BTEST(fieldType,FTB_HFLX) .AND. ASSOCIATED(inpBL%HeatFlux) )THEN
    hflxs => inpBL%HeatFlux
    hflx  => outBL%HeatFlux
    lBL = .TRUE.
  END IF
  IF( BTEST(fieldType,FTB_MOL) .AND. ASSOCIATED(inpBL%invMol)  )THEN
    invMOLs => inpBL%invMOL
    invMOL  => outBL%invMOL
    lBL = .TRUE.
  END IF
  IF( lBL )THEN
    IF( BTEST(fieldType,FTB_UST) .AND. ASSOCIATED(inpBL%ustr) )THEN
      ustrs => inpBL%ustr  !Assumes FTB_UST set only if in parent field
      ustr  => outBL%ustr
    ELSE IF( ASSOCIATED(inpBL%ustr) )THEN
      ustrs => inpBL%ustr  !Used for setting surface layer of parent field but not interpolation
    END IF
  END IF
  IF( BTEST(fieldType,FTB_PRATE) .OR. BTEST(fieldType,FTB_PRCP) )THEN
    prcps => inpBL%prcp
    prcp  => outBL%prcp
    IF( ASSOCIATED(inpBL%rainprb) )THEN
      rainprbs => inpBL%rainprb
      rainprb  => outBL%rainprb
    ELSE
      NULLIFY( rainprbs )
      NULLIFY( rainprb  )
    END IF
  END IF
  IF( BTEST(fieldType,FTB_CLDCV)  )THEN
    ccs => inpBL%CC
    cc  => outBL%CC
  END IF

END IF

IF( lLSV )THEN

  uuls => inpLSV%UU ; uul => outLSV%UU
  vvls => inpLSV%VV ; vvl => outLSV%VV
  uvls => inpLSV%UV ; uvl => outLSV%UV
  IF( lLSVL )THEN
    sbls => inpLSV%SL ; sbl => outLSV%SL
  END IF

ELSE

  NULLIFY(uuls)

END IF


!------ Set fixed/dummy interpolation parameters

dp = 1.0; gx = 0.; gy = 0.; iflag = 0

lstaggerS = IntrpFac%Stagger

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
  kOffset  = nxyo
  lstagger = .TRUE.
ELSE
  kOffset = 0
  lstagger = .FALSE.
END IF

!------ First interpolate BL fields if requested

DoBL : IF( lBL )THEN

!------ BL depth

  IF( BTEST(fieldType,FTB_ZI) .AND. ASSOCIATED(inpBL%zi) )THEN
    DO is = 1,nxyo
      CALL IntXY( mh(is),zis,zi(is) )
    END DO
  END IF

!------ BL heat flux

  IF( BTEST(fieldType,FTB_HFLX) .AND. ASSOCIATED(inpBL%HeatFlux) )THEN
    DO is = 1,nxyo
      CALL IntXY( mh(is),hflxs,hflx(is) )
    END DO
  END IF

!------ Inverse Monin-Obukhov length

  IF( BTEST(fieldType,FTB_MOL) .AND. ASSOCIATED(inpBL%invMOL) )THEN
    DO is = 1,nxyo
      CALL IntXY( mh(is),invMOLs,invMOL(is) )
    END DO
  END IF

!------ Friction Velocity

  IF( BTEST(fieldType,FTB_UST) .AND. ASSOCIATED(inpBL%ustr) )THEN
    DO is = 1,nxyo
      CALL IntXY( mh(is),ustrs,ustr(is) )
    END DO
  END IF

END IF DoBL

!------ Precipitation rate

IF( BTEST(fieldType,FTB_PRATE) )THEN
  DO is = 1,nxyo
      CALL IntXY( mh(is),prcps,prcp(is) )
  END DO
  IF( ASSOCIATED(rainprb) )THEN
      DO is = 1,nxyo
      CALL IntXY( mh(is),rainprbs,rainprb(is) )
      END DO
  END IF
ELSE IF( BTEST(fieldType,FTB_PRCP) )THEN
  DO is = 1,nxyo
      CALL nIntXY( mh(is),prcps,iprcp )
      prcp(is) = REAL(iprcp)
  END DO
END IF

!------ Cloud Cover

IF( BTEST(fieldType,FTB_CLDCV) )THEN
  DO is = 1,nxyo
    CALL IntXY( mh(is),ccs,cc(is) )
  END DO
END IF

!------ Mean field, if requested

DoMeanField : IF( lMean )THEN

!------ Temperature

  IF( BTEST(fieldType,FTB_T) )THEN

!$OMP PARALLEL SHARED(mh,mz,ts,t,nxyo,nzo,kOffset,dp,gx,gy,iflag,lstaggerS)
!$OMP DO PRIVATE(is,k,iz,ip,dum1,dum2,dum3)
    DO is = 1,nxyo
      DO k = 1,nzo
        iz = (k-1)*nxyo + is
        ip = kOffset + iz

        CALL IntXYZ( mh(is),mz(iz),ts,t(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )

        IF( mz(iz)%km <= 0 )THEN               !Extrapolate using US Atmosphere
          t(ip) = t(ip) - StndTpot( mz(iz)%zp ) + StndTpot( mz(iz)%zm )
        END IF

      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

    IF( lstagger )THEN !Set bottom slice for staggered grid
      DO is = 1,nxyo
        t(is) = t(is+nxyo)
      END DO
    END IF

  END IF

!------ Pressure

  IF( BTEST(fieldType,FTB_P) )THEN

!$OMP PARALLEL SHARED(mh,mz,ps,p,nxyo,nzo,kOffset,dp,gx,gy,iflag,lstaggerS)
!$OMP DO PRIVATE(is,k,iz,ip,dum1,dum2,dum3)
    DO is = 1,nxyo
      DO k = 1,nzo
        iz = (k-1)*nxyo + is
        ip = kOffset + iz

        CALL IntXYZ( mh(is),mz(iz),ps,p(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )

        IF( mz(iz)%km < 0 )THEN               !Extrapolate using US Atmosphere
          p(ip) = p(ip) - StndLogP( mz(iz)%zp ) + StndLogP( mz(iz)%zm )
        END IF

      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

    IF( lstagger )THEN !Set bottom slice for staggered grid
      DO is = 1,nxyo
        p(is) = p(is+nxyo)
      END DO
    END IF

  END IF

!------ Humidity

  IF( BTEST(fieldType,FTB_H) )THEN

!$OMP PARALLEL SHARED(mh,mz,hs,h,p,nxyo,nzo,kOffset,dp,gx,gy,iflag,lstaggerS,fieldType)
!$OMP DO PRIVATE(is,k,iz,ip,dum1,dum2,dum3,Pgrd,Px,dRH)
    DO is = 1,nxyo
      DO k = 1,nzo
        iz = (k-1)*nxyo + is
        ip = kOffset + iz

        CALL IntXYZ( mh(is),mz(iz),hs,h(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )

        IF( mz(iz)%km < 0 )THEN               !Extrapolate based on linear gradient to stratosphere
          IF( BTEST(fieldType,FTB_P) )THEN
            Pgrd = p(ip) + StndLogP( mz(iz)%zp ) - StndLogP( mz(iz)%zm )
            Pgrd = EXP(Pgrd)  * PSURF
            Px   = EXP(p(ip)) * PSURF
          ELSE
            Pgrd = EXP(StndLogP( mz(iz)%zp )) * PSURF
            Px   = EXP(StndLogP( mz(iz)%zm )) * PSURF
          END IF
          IF( mz(iz)%zp < mz(iz)%zm )THEN     !Above top level
            dRH = (RHSTRAT-h(ip))/(PSTRAT-Pgrd) * (Px-Pgrd)
          ELSE                                !Below bottom level
            dRH = RHSLOPE*(Px-Pgrd)
          END IF
          h(ip) = h(ip) + dRH
          h(ip) = SWIMrlimit( h(ip),1.,100.)
        END IF

      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

    IF( lstagger )THEN !Set bottom slice for staggered grid
      DO is = 1,nxyo
        h(is) = h(is+nxyo)
      END DO
    END IF

  END IF

!------ Cloud liquid water

  IF( BTEST(fieldType,FTB_QCLD) )THEN

!$OMP PARALLEL SHARED(mh,mz,qs,q,nxyo,nzo,kOffset,dp,gx,gy,iflag,lstaggerS)
!$OMP DO PRIVATE(is,k,iz,ip,dum1,dum2,dum3)
    DO is = 1,nxyo
      DO k = 1,nzo
        iz = (k-1)*nxyo + is
        ip = kOffset + iz

        CALL IntXYZ( mh(is),mz(iz),qs,q(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )

        IF( mz(iz)%km < 0 )q(ip) = 0.

      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

    IF( lstagger )THEN !Set bottom slice for staggered grid
      DO is = 1,nxyo
        q(is) = q(is+nxyo)
      END DO
    END IF

  END IF

!------ Setup vertical arrays for interpolation from 3d height field

    IF( lGrdI )THEN
    IF( BTEST(inpGrid%type,GTB_Z3D) )THEN

      nxs = inpGrid%nX; nys = inpGrid%nY; nzs = inpGrid%nZ; nxys = inpGrid%nXY

      ALLOCATE( zuIntrp(nzs),zvIntrp(nzs),STAT=alloc_stat )  !at interpolation point
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMinterpGridded'
        error%Message = 'Error allocating z-grids for 3d height interpolation arrays'
        GOTO 9999
      END IF

      IF( BTEST(inpGrid%type,GTB_SIGMA) .OR. BTEST(inpGrid%type,GTB_Z3DW) )THEN
        z3d => inpGrid%sigma%Z
      ELSE
        z3d => inpFld%Z
      END IF

      IF( .NOT.ASSOCIATED(z3d) )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMinterpGridded'
        error%Message = 'Error pointing to 3d grid height field interpolation arrays'
        GOTO 9999
      END IF

      IF( BTEST(inpGrid%type,GTB_STAGGER) )THEN

        ALLOCATE( zVelU(nxys*nzs),zVelV(nxys*nzs),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinterpGridded'
          error%Message = 'Error allocating z-grid for height interpolation arrays'
          GOTO 9999
        END IF

        IF( BTEST(inpGrid%type,GTB_STAGGERB) )THEN   !Construct 3d arrays at staggered locations for velocity components

!$OMP PARALLEL SHARED(nys,nxs,nzs,z3d,zVelU,zVelV)
!$OMP DO PRIVATE(j,j0,jp0,i,ip,is,k,k0,zk)
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
!$OMP END DO
!$OMP END PARALLEL

        ELSE

!$OMP PARALLEL SHARED(nys,nxs,nzs,z3d,zVelU,zVelV)
!$OMP DO PRIVATE(j,j0,jp0,i,ip,is,k,k0,zk)
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

!$OMP END DO
!$OMP END PARALLEL

        END IF

      END IF

    END IF
    END IF

!------ Estimate surface layer parameters for 'parent' grid

    IF( lSL )THEN

      ALLOCATE( usl(nxyo),vsl(nxyo),zsl(nxyo),invL(nxyo),usls(nxyi), &
                vsls(nxyi),zsls(nxyi),invLs(nxyi),zrefs(nxyi),elevs(nxyi), &
                zslp(nxyo),zrufp(nxyo),hcp(nxyo), alp(nxyo), invLp(nxyo), &
                STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMinterpGridded'
        error%Message = 'Error allocating surface layer arrays'
        GOTO 9999
      END IF

      IF( lGrdI )THEN

        zrufs   => inpGrid%landcover%roughness
        hcs     => inpGrid%landcover%canopyHt
        alphas  => inpGrid%landcover%alpha
        bowens  => inpGrid%landcover%bowen
        albedos => inpGrid%landcover%albedo
        kbls    => inpGrid%landcover%kbl
        lats    => inpGrid%lat
        lons    => inpGrid%lon

        IF( BTEST(inpGrid%type,GTB_Z3D) )THEN
          IF( BTEST(inpGrid%type,GTB_SIGMA) .OR. BTEST(inpGrid%type,GTB_Z3DW) )THEN
            zs => inpGrid%sigma%Z
          ELSE
            ALLOCATE( kbls(nxyi),STAT=alloc_stat )
            IF( alloc_stat /= 0 )THEN
              error%Number  = UK_ERROR
              error%Routine = 'SWIMinterpGridded'
              error%Message = 'Error allocating surface layer index array'
              GOTO 9999
            END IF
            CALL set_kbl_Z3D( inpGrid,inpFld,kbls )
            IF( error%Number /= NO_ERROR )GOTO 9999
            zs => inpFld%Z
          END IF
        ELSE
          zs => inpGrid%Z
        END IF

        DO i = 1,nxyi
          IF( BTEST(inpGrid%type,GTB_Z3D) )THEN
            iz = (kbls(i)-1)*nxyi + i
            zrefs(i) = zs(iz)*inpGrid%terrain%D(i)
          ELSE
            zrefs(i) = zs(kbls(i))*inpGrid%terrain%D(i)
          END IF
          elevs(i) = inpGrid%terrain%H(i) + inpGrid%hmin
        END DO

      ELSE IF( PRESENT(inpZ) )THEN

        ALLOCATE( zrufs(nxyi),hcs(nxyi),alphas(nxyi),bowens(nxyi),albedos(nxyi), &
                  kbls(nxyi),zs(nzi),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinterpGridded'
          error%Message = 'Error allocating surface layer arrays'
          GOTO 9999
        END IF

        DO i = 1,nzi
          zs(i) = inpZ(i) + Prj%BL%hc
        END DO

        DO i = 1,nxyi
          zrufs(i)   = Prj%BL%zruf
          bowens(i)  = Prj%BL%Bowen
          albedos(i) = Prj%BL%albedo
          hcs(i)     = Prj%BL%hc
          alphas(i)  = Prj%BL%alpha
          zrefs(i)   = zs(1)
          elevs(i)   = Prj%Hmin
          kbls(i)    = 1
        END DO

        NULLIFY(lats,lons)

      ELSE

        error%Number = IV_ERROR
        error%Routine = 'SWIMinterpGridded'
        error%Message = 'Invalid surface layer interpolation'
        GOTO 9999

      END IF

      IF( Prj%Lat0 /= NOT_SET_R )THEN
        inpLat0 = Prj%Lat0
        inpLon0 = Prj%Lon0
      ELSE
        inpLat0 = NOT_SET_R
        inpLon0 = NOT_SET_R
      END IF

      DiffLandU => IntrpFac%DiffLandU
      DiffLandV => IntrpFac%DiffLandV
      zblu      => IntrpFac%zblu
      zblv      => IntrpFac%zblv
      zbluP     => IntrpFac%zbluS
      zblvP     => IntrpFac%zblvS

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j,i0,i0p,i,is,k0,ip,ipp,uref,vref,zp,tref,pref,dum,inpUstr,L,inpLat,inpLon,inpZi,inpL,inpHflx)
      DO j = 1,nyi
        i0 = (j-1)*nxi
        IF( lGrdI )THEN
          IF( BTEST(inpGrid%type,GTB_STAGGER) )i0p = (MIN(j+1,nyi)-1)*nxi
        END IF

        DO i = 1,nxi
          is = i0 + i
          k0 = (kbls(is)-1)*nxyi
          ip = k0 + is
          IF( ASSOCIATED(lons) )THEN
            inpLon = lons(is)
            inpLat = lats(is)
          ELSE
            inpLon = inpLon0
            inpLat = inpLat0
          END IF

          IF( lstaggerS )THEN
            k0 = k0 + nxyi
            ip = ip + nxyi
          END IF

          IF( lGrdI )THEN
            IF( BTEST(inpGrid%type,GTB_STAGGER) )THEN
              ipp  = k0 + i0 + MIN(i+1,nxi)
              IF( BTEST(inpGrid%type,GTB_STAGGERB) )THEN
                uref = us(ip)+us(ipp)
                vref = vs(ip)+vs(ipp)
                ipp  = k0 + i0p + i
                uref = uref+us(ipp)
                vref = vref+vs(ipp)
                ipp  = k0 + i0p + MIN(i+1,nxi)
                uref = 0.25*(uref+us(ipp))
                vref = 0.25*(vref+vs(ipp))
              ELSE
                uref = 0.5*(us(ip)+us(ipp))
                ipp  = k0 + i0p + i
                vref = 0.5*(vs(ip)+vs(ipp))
              END IF
            ELSE
              uref = us(ip)
              vref = vs(ip)
            END IF
          ELSE
            uref = us(ip)
            vref = vs(ip)
          END IF

          zp = zrefs(is) + elevs(is)
          CALL stnd_atmos( zp,pref,tref,dum,SA_TPOT )
          IF( BTEST(fieldType,FTB_T) )tref = ts(ip)
          IF( BTEST(fieldType,FTB_P) )pref = EXP(ps(ip))
          tref = tref*pref**KAPPA
          pref = pref*PSURF

          IF( BTEST(fieldType,FTB_ZI) )THEN
            inpZi = zis(is)
          ELSE
            inpZi = NOT_SET_R
          END IF
          IF( BTEST(fieldType,FTB_MOL) )THEN
            inpL = LfromInvL( invMOLs(is) )
          ELSE
            inpL = NOT_SET_R
          END IF
          IF( BTEST(fieldType,FTB_HFLX) )THEN
            inpHflx = hflxs(is)
          ELSE
            inpHflx = NOT_SET_R
          END IF
          IF( BTEST(fieldType,FTB_UST) )THEN
            inpUstr = ustrs(is)
          ELSE
            inpUstr = NOT_SET_R
          END IF

          CALL SWIMPointSrfLayer( BLtype,time,zrefs(is),uref,vref, &
                                  zrufs(is),hcs(is),alphas(is),Bowens(is),albedos(is),tref,pref, &
                                  inpLat,inpLon,inpL,inpZi,inpHflx,inpUstr, &
                                  zsls(is),usls(is),vsls(is),L )
          invLs(is) = InvLfromL( L )

        END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Interpolate surface layer height and velocity onto output grid (as initial conditions);
!       also interpolate landuse and L for use in surface layer profile

      DO is = 1,nxyo
        CALL IntXY( mh(is),zsls,zsl(is) ); zslp(is) = zsl(is)
        CALL IntXY( mh(is),usls,usl(is) )
        CALL IntXY( mh(is),vsls,vsl(is) )
        CALL IntXY( mh(is),zrufs,zrufp(is) )
        CALL IntXY( mh(is),hcs,hcp(is) )
        CALL IntXY( mh(is),alphas,alp(is) )
        CALL IntXY( mh(is),invLs,invLp(is) )
      END DO

!------ Setup input for output grid surface layer

      zruf   => grid%landcover%roughness
      hc     => grid%landcover%canopyHt
      alpha  => grid%landcover%alpha
      bowen  => grid%landcover%bowen
      albedo => grid%landcover%albedo
      kbl    => grid%landcover%kbl

      IF( Prj%Lat0 /= NOT_SET_R )THEN
        inpLat0 = Prj%Lat0
        inpLon0 = Prj%Lon0
      ELSE
        inpLat0 = NOT_SET_R
        inpLon0 = NOT_SET_R
      END IF

!------ Recompute surface layer

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j,i0,i,is,inpLat,inpLon,iz,zref,uref,vref,ip,zp,pref,tref,dum,inpZi,inpL,inpHflx,inpUstr,L)
      DO j = 1,nyo
        i0 = (j-1)*nxo

        DO i = 1,nxo
          is = i0 + i
          IF( ASSOCIATED(grid%lat) )THEN
            inpLat = grid%lat(is)
            inpLon = grid%lon(is)
          ELSE
            inpLat = inpLat0
            inpLon = inpLon0
          END IF

          IF( BTEST(grid%type,GTB_Z3D) )THEN
            iz  = (kbl(is)-1)*nxyo + is
            zref = grid%sigma%Z(iz)
          ELSE
            zref = grid%Z(kbl(is))*grid%terrain%D(is)
          END IF
          zref = MAX(zsl(is),zref)
          uref = usl(is)
          vref = vsl(is)

          ip = (kbl(is)-1)*nxyo + is
          IF( BTEST(grid%type,GTB_STAGGERZ) )ip = ip + nxyo

          zp = zref + grid%terrain%H(is) + grid%Hmin
          CALL stnd_atmos( zp,pref,tref,dum,SA_TPOT )
          IF( BTEST(fieldType,FTB_T) )tref = t(ip)
          IF( BTEST(fieldType,FTB_P) )pref = EXP(p(ip))
          tref = tref*pref**KAPPA
          pref = pref*PSURF

          IF( BTEST(fieldType,FTB_MOL) )THEN
            inpL = LfromInvL( invMOL(is) )
          ELSE
            inpL = NOT_SET_R
          END IF
          IF( BTEST(fieldType,FTB_ZI) )THEN
            inpZi = zi(is)
          ELSE
            inpZi = NOT_SET_R
          END IF
          IF( BTEST(fieldType,FTB_HFLX) )THEN
            inpHflx = hflx(is)
          ELSE
            inpHflx = NOT_SET_R
          END IF

          zref = 1.E6  !Set large so that zsl is used as reference height

          IF( ASSOCIATED(inpBL%ustr) .AND. lbl )THEN
            inpUstr = ustr(is)
          ELSE
            inpUstr = NOT_SET_R
          END IF

          CALL SWIMPointSrfLayer( BLtype,time,zref,uref,vref, &
                                  zruf(is),hc(is),alpha(is),Bowen(is),albedo(is),tref,pref, &
                                  inpLat,inpLon,inpL,inpZi,inpHflx,inpUstr, &
                                  zsl(is),usl(is),vsl(is),L )
          invL(is) = InvLfromL( L )

        END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL

    END IF

    zs_ptr => zs

!------ Interpolate horizontal velocity components

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j,i0,i0p,i,is,iup,zslu,hcu,alpu,zrufu,Lu,du,zslpu,zrfpu,hcpu,alppu,Lpu, &
!$OMP                          ivp,zslv,hcv,alpv,zrufv,Lv,dv,zslpv,zrfpv,hcpv,alppv,Lpv, &
!$OMP k,iz,ip,zh,dum,mv,dum1,dum2,dum3,zInfl,zInflp,fslp,fsl,fp,f,facp,zref,uref,vref,Z,zs,zk,zuIntrp,zvIntrp)
    DO j = 1,nyo
      i0  = (j-1)*nxo
      IF( BTEST(grid%type,GTB_STAGGER) )i0p = (MIN(j+1,nyo)-1)*nxo

      DO i = 1,nxo
        is = i0 + i

        IF( lSL )THEN

!------ Set landcover parameters

          IF( BTEST(grid%type,GTB_STAGGER) )THEN
            iup   = i0 + MIN(i+1,nxo)          ; ivp   = i0p + i
            zslu  = 0.5*(zsl(is)+zsl(iup))     ; zslv  = 0.5*(zsl(is)+zsl(ivp))
            hcu   = 0.5*(hc(is)+hc(iup))       ; hcv   = 0.5*(hc(is)+hc(ivp))
            alpu  = 0.5*(alpha(is)+alpha(iup)) ; alpv  = 0.5*(alpha(is)+alpha(ivp))
            zrufu = 0.5*(zruf(is)+zruf(iup))   ; zrufv = 0.5*(zruf(is)+zruf(ivp))
            Lu    = 0.5*(invL(is)+invL(iup))   ; Lv    = 0.5*(invL(is)+invL(ivp))
            du    = grid%terrain%Du(is)        ; dv    = grid%terrain%Dv(is)
            zslpu = 0.5*(zslp(is)+zslp(iup))   ; zslpv = 0.5*(zslp(is)+zslp(ivp))
            zrfpu = 0.5*(zrufp(is)+zrufp(iup)) ; zrfpv = 0.5*(zrufp(is)+zrufp(ivp))
            hcpu  = 0.5*(hcp(is)+hcp(iup))     ; hcpv  = 0.5*(hcp(is)+hcp(ivp))
            alppu = 0.5*(alp(is)+alp(iup))     ; alppv = 0.5*(alp(is)+alp(ivp))
            Lpu   = 0.5*(invLp(is)+invLp(iup)) ; Lpv   = 0.5*(invLp(is)+invLp(ivp))
          ELSE
            zslu  = zsl(is)            ; zslv  = zslu
            hcu   = hc(is)             ; hcv   = hcu
            alpu  = alpha(is)          ; alpv  = alpu
            zrufu = zruf(is)           ; zrufv = zrufu
            Lu    = invL(is)           ; Lv    = Lu
            du    = grid%terrain%D(is) ; dv    = du
            zslpu = zslp(is)           ; zslpv = zslpu
            zrfpu = zrufp(is)          ; zrfpv = zrfpu
            hcpu  = hcp(is)            ; hcpv  = hcpu
            alppu = alp(is)            ; alppv = alppu
            Lpu   = invLp(is)          ; Lpv   = Lpu
          END IF

          dum = Lu;  Lu  = LfromInvL( dum )
          dum = Lv;  Lv  = LfromInvL( dum )
          dum = Lpu; Lpu = LfromInvL( dum )
          dum = Lpv; Lpv = LfromInvL( dum )

!------ Point to vertical grid
!       N.B. should be interpolated to U & V locations for sigma coordinates

          IF( BTEST(grid%type,GTB_Z3D) )THEN
            Z => grid%sigma%Z(is:nxyo*nzo:nxyo)
          ELSE
            Z => grid%Z
          END IF

          zs => zs_ptr

          IF( lGrdI )THEN
            IF( BTEST(inpGrid%type,GTB_Z3D) )THEN
              DO k = 1,nzs
                ip =  (k-1)*nxys + 1
                IF( BTEST(inpGrid%type,GTB_STAGGER) )THEN
                  zk => zVelU(ip:); CALL IntXY( mhu(is),zk,zuIntrp(k) )
                  zk => zVelV(ip:); CALL IntXY( mhv(is),zk,zvIntrp(k) )
                ELSE
                  zk => z3d(ip:); CALL IntXY( mh(is),zk,zuIntrp(k)  )
                  zvIntrp(k) = zuIntrp(k)
                END IF
              END DO
              zs => zuIntrp
            END IF
          END IF

!------ Compute U-component:

!------ Linear interpolation, modified for surface layer profile (based on input parameters)

          DO k = 1,nzo
            iz = (k-1)*nxyo + is
            ip = kOffset + iz
            zh = Z(k)*du
            CALL set_mv( zs,nzi,mzu(iz),mv,zh,du,zslpu,hcpu,alppu,zrfpu,Lpu )
            CALL IntXYZ( mhu(is),mv,us,u(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
          END DO

!------ Check if different land characteristics require surface layer profiles

          IF( DiffLandU(is) )THEN

            zInfl  = zblu(is)
            zInflp = zbluP(is)

            fslp = -1.
            fsl  = -1.

            CALL set_fsl( zInfl,zslpu,hcpu,alppu,zrfpu,Lpu,fp,fslp )
            CALL set_fsl( zInfl,zslu, hcu ,alpu, zrufu,Lu, f, fsl  )
            facp = fp/f

            IF( Z(1)*du <= zInflp )THEN
              k = 1
              DO
                IF( Z(k)*du >= zInflp )EXIT
                IF( k == grid%nZ )EXIT
                k = k + 1
              END DO
              zref = Z(k)*du
              CALL set_fsl( zref,zslpu,hcpu,alppu,zrfpu,Lpu,fp,fslp )
              CALL set_fsl( zref,zslu, hcu ,alpu, zrufu,Lu, f, fsl  )
              iz = (k-1)*nxyo + is
              ip = kOffset + iz
              uref = u(ip) * facp*f/fp
            END IF

            DO k = 1,grid%nZ
              zh = Z(k)*du
              iz = (k-1)*nxyo + is
              ip = kOffset + iz
              IF( zh <= zInflp )THEN
                CALL get_ubl( zref,uref,Lu,zrufu,hcu,alpu,zh,u(ip) )
              ELSE IF( zh <= zInfl )THEN
                CALL set_fsl( zh,zslpu,hcpu,alppu,zrfpu,Lpu,fp,fslp )
                CALL set_fsl( zh,zslu, hcu ,alpu, zrufu,Lu, f, fsl  )
                u(ip) = u(ip) * facp*f/fp
              ELSE
                EXIT
              END IF
            END DO

          END IF

!------ Compute V-component:

          IF( lGrdI )THEN
            IF( BTEST(inpGrid%type,GTB_Z3D) )THEN
              zs => zvIntrp
            ELSE
              zs => zs_ptr
            END IF
          END IF

!------ Linear interpolation, modified for surface layer profile (based on input parameters)

          DO k = 1,nzo
            iz = (k-1)*nxyo + is
            ip = kOffset + iz
            zh = Z(k)*dv
            CALL set_mv( zs,nzi,mzv(iz),mv,zh,dv,zslpv,hcpv,alppv,zrfpv,Lpv )
            CALL IntXYZ( mhv(is),mv,vs,v(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
          END DO

!------ Check if different land characteristics require surface layer profiles

          IF( DiffLandV(is) )THEN

            zInfl  = zblv(is)
            zInflp = zblvP(is)

            fslp = -1.
            fsl  = -1.

            CALL set_fsl( zInfl,zslpv,hcpv,alppv,zrfpv,Lpv,fp,fslp )
            CALL set_fsl( zInfl,zslv, hcv ,alpv, zrufv,Lv, f, fsl  )
            facp = fp/f

            IF( Z(1)*dv <= zInflp )THEN
              k = 1
              DO
                IF( Z(k)*dv >= zInflp )EXIT
                IF( grid%Z(k)*dv >= zInflp )EXIT
                IF( k == grid%nZ )EXIT
                k = k + 1
              END DO
              zref = Z(k)*dv
              CALL set_fsl( zref,zslpv,hcpv,alppv,zrfpv,Lpv,fp,fslp )
              CALL set_fsl( zref,zslv, hcv ,alpv, zrufv,Lv, f, fsl  )
              iz = (k-1)*nxyo + is
              ip = kOffset + iz
              vref = v(ip) * facp*f/fp
            END IF

            DO k = 1,grid%nZ
              zh = Z(k)*dv
              iz = (k-1)*nxyo + is
              ip = kOffset + iz
              IF( zh <= zInflp )THEN
                CALL get_ubl( zref,vref,Lv,zrufv,hcv,alpv,zh,v(ip) )
              ELSE IF( zh <= zInfl )THEN
                CALL set_fsl( zh,zslpv,hcpv,alppv,zrfpv,Lpv,fp,fslp )
                CALL set_fsl( zh,zslv, hcv ,alpv, zrufv,Lv, f, fsl  )
                v(ip) = v(ip) * facp*f/fp
              ELSE
                EXIT
              END IF
            END DO

          END IF

        ELSE !.NOT.lSL

          DO k = 1,nzo
            iz = (k-1)*nxyo + is
            ip = kOffset + iz
            CALL IntXYZ( mhu(is),mzu(iz),us,u(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
            CALL IntXYZ( mhv(is),mzv(iz),vs,v(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
          END DO

        END IF

      END DO
    END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

!------ Set bottom slice for staggered grid

    IF( lstagger )THEN
      DO is = 1,nxyo
        u(is) = u(is+nxyo)
        v(is) = v(is+nxyo)
      END DO
    END IF

!------ Vertical velocity component

    IF( lVertVel )THEN

      DO is = 1,nxyo
        DO k = 1,nzo-1
          iz = (k-1)*nxyo + is
          ip = kOffset + iz
          CALL IntXYZ( mh(is),mzw(iz),ws,w(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
        END DO
      END DO

      DO is = 1,nxyo
        w(is) = 0.     !Bottom BC
      END DO

      IF( BTEST(inpGrid%type,GTB_TERRAIN) )DEALLOCATE( ws,STAT=alloc_stat )

    ELSE IF( BTEST(fieldType,FTB_W) )THEN

      DO i = 1,SIZE(w)
        w(i) = 0.
      END DO

    END IF

!------ Deallocate surface layer arrays/pointers

    IF( lSL )THEN


      IF( lGrdI )THEN
        NULLIFY ( zrufs,hcs,alphas,bowens,albedos,lats,lons,zs )
        IF( BTEST(inpGrid%type,GTB_Z3D) .AND. .NOT.(BTEST(inpGrid%type,GTB_SIGMA) .OR. BTEST(inpGrid%type,GTB_Z3DW)) )THEN
          DEALLOCATE( kbls,STAT=alloc_stat )
        ELSE
          NULLIFY( kbls )
        END IF
      ELSE
        DEALLOCATE( zrefs,zrufs,hcs,alphas,bowens,albedos,kbls,zs,STAT=alloc_stat )
      END IF

      DEALLOCATE( usl,vsl,zsl,invL,usls,vsls,zsls,invLs,zrefs, &
                  zslp,zrufp,hcp,alp,invLp,STAT=alloc_stat )

    END IF

END IF DoMeanField

!------ Large-scale variance, if requested

DoLSV : IF( ASSOCIATED(uuls) )THEN

  DO is = 1,nxyo
    DO k = 1,nzo
      iz = (k-1)*nxyo + is
      ip = kOffset + iz

      CALL IntXYZ( mh(is),mz(iz),uuls,uul(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
      CALL IntXYZ( mh(is),mz(iz),vvls,vvl(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
      CALL IntXYZ( mh(is),mz(iz),uvls,uvl(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
      IF( lLSVL )THEN
        CALL IntXYZ( mh(is),mz(iz),sbls,sbl(ip),dum1,dum2,dum3,dp,gx,gy,iflag,lstaggerS )
      END IF

    END DO
  END DO

  IF( lstagger )THEN !Set bottom slice for staggered grid
    DO is = 1,nxyo
      uul(is) = uul(is+nxyo)
      vvl(is) = vvl(is+nxyo)
      uvl(is) = uvl(is+nxyo)
    END DO
    IF( lLSVL )THEN
      DO is = 1,nxyo
        sbl(is) = sbl(is+nxyo)
      END DO
    END IF
  END IF

END IF DoLSV



9999 CONTINUE

!------ Deallocate & nullify

IF( lVertVel )THEN
  IF( ASSOCIATED(ws) .AND. BTEST(inpGrid%type,GTB_TERRAIN) )DEALLOCATE( ws,STAT=alloc_stat )
END IF

IF( lSL )THEN

  IF( ASSOCIATED(usls)  )DEALLOCATE( usls, STAT=alloc_stat )
  IF( ASSOCIATED(vsls)  )DEALLOCATE( vsls, STAT=alloc_stat )
  IF( ASSOCIATED(zsls)  )DEALLOCATE( zsls, STAT=alloc_stat )
  IF( ASSOCIATED(usl)   )DEALLOCATE( usl,  STAT=alloc_stat )
  IF( ASSOCIATED(vsl)   )DEALLOCATE( vsl,  STAT=alloc_stat )
  IF( ASSOCIATED(zsl)   )DEALLOCATE( zsl,  STAT=alloc_stat )
  IF( ASSOCIATED(invL)  )DEALLOCATE( invL, STAT=alloc_stat )
  IF( ASSOCIATED(invLs) )DEALLOCATE( invLs,STAT=alloc_stat )
  IF( ASSOCIATED(zslp)  )DEALLOCATE( zslp, STAT=alloc_stat )
  IF( ASSOCIATED(zrufp) )DEALLOCATE( zrufp,STAT=alloc_stat )
  IF( ASSOCIATED(hcp)   )DEALLOCATE( hcp,  STAT=alloc_stat )
  IF( ASSOCIATED(alp)   )DEALLOCATE( alp,  STAT=alloc_stat )
  IF( ASSOCIATED(invLp) )DEALLOCATE( invLp,STAT=alloc_stat )

  IF( .NOT.PRESENT(inpGrid) )THEN
    IF( ASSOCIATED(zs)      )DEALLOCATE( zs_ptr, STAT=alloc_stat )
    IF( ASSOCIATED(zrufs)   )DEALLOCATE( zrufs,  STAT=alloc_stat )
    IF( ASSOCIATED(zrefs)   )DEALLOCATE( zrefs,  STAT=alloc_stat )
    IF( ASSOCIATED(hcs)     )DEALLOCATE( hcs,    STAT=alloc_stat )
    IF( ASSOCIATED(alphas)  )DEALLOCATE( alphas, STAT=alloc_stat )
    IF( ASSOCIATED(bowens)  )DEALLOCATE( bowens, STAT=alloc_stat )
    IF( ASSOCIATED(albedos) )DEALLOCATE( albedos,STAT=alloc_stat )
    IF( ASSOCIATED(kbls)    )DEALLOCATE( kbls,   STAT=alloc_stat )
  ELSE IF( BTEST(inpGrid%type,GTB_Z3D) )THEN
    IF( ALLOCATED(zuIntrp)  )DEALLOCATE( zuIntrp,STAT=alloc_stat )
    IF( ALLOCATED(zvIntrp)  )DEALLOCATE( zvIntrp,STAT=alloc_stat )
    IF( ASSOCIATED(zVelU)   )DEALLOCATE( zVelU,STAT=alloc_stat )
    IF( ASSOCIATED(zVelV)   )DEALLOCATE( zVelV,STAT=alloc_stat )
  END IF

END IF

RETURN

END SUBROUTINE SWIMinterpGridded

!==============================================================================

RECURSIVE SUBROUTINE SWIMPointSrfLayer( BLtype,time,zref,uref,vref, &
                              zruf,hc,alpha,Bowen,albedo,tref,pref, &
                              lat,lon,inpL,Zi,inpHflx,inpUstr,      &
                              zsl,usl,vsl,outL )

!------ Set or compute surface layer parameters for grid interpolation

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: BLtype
REAL,    INTENT( IN    ) :: time
REAL,    INTENT( IN    ) :: zref, uref, vref
REAL,    INTENT( IN    ) :: zruf, hc, alpha, Bowen, albedo, tref, pref
REAL,    INTENT( IN    ) :: lat, lon, inpL
REAL,    INTENT( INOUT ) :: inpUstr
REAL,    INTENT( INOUT ) :: Zi, inpHflx
REAL,    INTENT( OUT   ) :: zsl, usl, vsl, outL


INTEGER jul
REAL    tLocal, tSolar, xlon
REAL    hflux, ziSBL, hfluxSBL, unit_vel
REAL    cc, rn, ufac
REAL    L, wspd, ustar
LOGICAL lSetZi, FixedHeatFlux, FixedZi, GotLL, lUstr

INTEGER, EXTERNAL :: JulianPrj
REAL,    EXTERNAL :: ulog, LocalTime, UTCtime, SolarTime, ComputeNetRadiation, InvLfromL

!------ Set Julian day

jul = JulianPrj( time )

!------ Set depth and heat flux for Simple BL

IF( BLtype == BLP_SBL )THEN
  tLocal = LocalTime( time )
  IF( tLocal == NOT_SET_R )THEN
    xlon = lon
    IF( xlon == NOT_SET_R )xlon = 0.
    tLocal = SolarTime( UTCtime(time),xlon )
  END IF
  CALL SetSimpleBL( tLocal,ziSBL,hfluxSBL )
END IF

!------ Copy cloud cover; set reference wind speed

cc   = Prj%BL%cc
wspd = SQRT(uref**2 + vref**2)

!------ Surface layer is completely defined if given M-O Length

IF( inpL /= NOT_SET_R )THEN

  lSetZI = Zi == NOT_SET_R  !Set Zi based on L if no input

  ustar = wspd**2
  CALL SetBLfromMOL( InvLfromL(inpL),zruf,hc,alpha,lSetZi,L,Zi,zsl,zref,ustar )

  ustar = SQRT(ustar)
  hflux = -ustar**3*tref/(vonk*L*G0)

ELSE

!------ Otherwise, set parameters based on input or calculation

  GotLL = lat /= NOT_SET_R

!------ Set heat flux

  IF( inpHflx /= NOT_SET_R )THEN
    hflux = inpHflx
    FixedHeatFlux = .TRUE.
  ELSE
    IF( BLtype == BLP_SBL )THEN
      hflux = hfluxSBL
      FixedHeatFlux = .TRUE.
    ELSE IF( GotLL )THEN
      IF( Prj%local .AND.  (Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R) )THEN
        tSolar = LocalTime( time )
      ELSE
        tSolar = SolarTime( UTCtime(time),lon )
      END IF
      rn = ComputeNetRadiation( tSolar,jul,lat,tref,cc,albedo )
      hflux = rn              !Set initial value for Zi estimate, but reset in SurfIter
      FixedHeatFlux = .FALSE.
    ELSE
      hflux = 0.
      FixedHeatFlux = .TRUE.
    END IF
  END IF

!------ Set BL depth

  IF( Zi /= NOT_SET_R )THEN
    FixedZi = .TRUE.
  ELSE
    IF( BLtype == BLP_SBL )THEN
      Zi      = ziSBL
      FixedZi = .TRUE.
    ELSE
     IF( hflux > 0. )THEN
        Zi = 1000.
      ELSE
        Zi = 100.
      END IF
      FixedZi = .NOT.GotLL  !Compute zi only if latitude given
    END IF
  END IF

!------ Compute surface layer depth (and heat flux & zi if appropriate)

  lUstr = inpUstr /= NOT_SET_R
  CALL SurfIter( zref,wspd,tref,pref,zruf,hc,alpha,bowen, &
                 cc,rn,lat,.NOT.FixedZi,FixedHeatFlux, &
                 hflux,Zi,zsl,L,inpUstr,lUstr )

END IF

!------ Extrapolate velocity to zsl if greater than zref

IF( zsl > zref )THEN
  unit_vel = 1.
  CALL get_ubl( zref,unit_vel,L,zruf,hc,alpha,zsl,ufac )
  usl = uref*ufac
  vsl = vref*ufac
ELSE
  usl = uref
  vsl = vref
END IF

outL = L

IF( inpHflx == NOT_SET_R )inpHflx = hflux

RETURN
END SUBROUTINE SWIMpointSrfLayer

END MODULE SWIMintrpGrid_fi


