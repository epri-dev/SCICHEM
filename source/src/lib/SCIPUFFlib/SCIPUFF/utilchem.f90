RECURSIVE SUBROUTINE GetChemAux( ID,p )

USE scipuff_fi
USE step_p_fi, ONLY: myTid
USE chem_fi

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: ID
TYPE( puff_str ), INTENT( IN ) :: p

INTEGER i, nskp, nmc

nskp = typeID(p%ityp)%ipmc - 1
chem => chemMC(ID,myTid)

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,nmc
  nskp = nskp + 1
  chem%species(i)%mass = p%aux(nskp)
  chem%species(i)%conc = p%aux(nskp)
END DO

IF( p%c > SMALL )THEN

DO i = 1,chem%nStar
  nskp = nskp + 1
  chem%star(i)%s%conc = p%aux(nskp)/p%c
END DO

vol   = p%aux(nskp+1)/p%c
pres  = p%aux(nskp+2)/p%c
temp  = p%aux(nskp+3)/p%c
vself = p%aux(nskp+4)/p%c

ELSE

DO i = 1,chem%nStar
  nskp = nskp + 1
  chem%star(i)%s%conc = 0.
END DO

vol   = 0.
pres  = 0.
temp  = 0.
vself = 0.
END IF

RETURN
END

!===============================================================================
RECURSIVE SUBROUTINE PutChemAux( ID,p )

USE step_p_fi, ONLY: myTId
USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ID
TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER i, nskp, nmc

nskp = typeID(p%ityp)%ipmc - 1
chem => chemMC(ID,myTid)

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,nmc
  nskp = nskp + 1
  p%aux(nskp) = chem%species(i)%mass
END DO

DO i = 1,chem%nStar
  nskp = nskp + 1
  p%aux(nskp) = chem%star(i)%s%conc * p%c
END DO

p%aux(nskp+1) = vol*p%c
p%aux(nskp+2) = pres*p%c
p%aux(nskp+3) = temp*p%c
p%aux(nskp+4) = vself*p%c

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE WriteChemMC()

USE error_fi
USE files_fi
USE default_fd
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER i, ii, j, n, ndat, np, ios

DO ii = 1,mat_mc%nMCtype
  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n,1)

    WRITE(lun_prj,IOSTAT=ios)chem%flag,chem%nSpecies,chem%nReactions,chem%nZenith, &
                             chem%rtol,chem%ambFile,chem%FileNotUsed,chem%sFlxFile, &
                             chem%lBalance,chem%lStepAmb,chem%lStage, &
                             chem%pTypeN,chem%cUnits,chem%oUnits,chem%eUnits,chem%kUnits, &
                             chem%tConv,chem%kConv,chem%eConv,chem%eConvType
    IF( ios /= 0 )GOTO 9998

    WRITE(lun_prj,IOSTAT=ios) (chem%zenith(i),i=1,chem%nZenith)
    IF( ios /= 0 )GOTO 9998

    !-- write the particle types and particle emission units
    IF( chem%pTypeN > 0 )THEN
      WRITE(lun_prj,IOSTAT=ios) (chem%pTypes(i),chem%pUnits(i),i=1,chem%pTypeN)
      IF( ios /= 0 )GOTO 9998
    END IF

    DO i = 1,chem%nSpecies
       WRITE(lun_prj,IOSTAT=ios)chem%species(i)%class,chem%species(i)%classAux,chem%species(i)%name, &
                                chem%species(i)%ambient,chem%species(i)%tol,&
                                chem%species(i)%ldos,chem%species(i)%ldep, &
                                chem%species(i)%scav,chem%species(i)%vdep,&
                                chem%species(i)%Henry0,chem%species(i)%TpFac,&
                                chem%species(i)%RxFac,chem%species(i)%SrfRFac,chem%species(i)%ldrydep,&
                                chem%species(i)%mw,chem%species(i)%nit,chem%species(i)%lim
      IF( ios /= 0 )GOTO 9998
    END DO

    DO i = 1,chem%nReactions
      ndat = SIZE(chem%reaction(i)%data)
      np   = chem%reaction(i)%nP
      WRITE(lun_prj,IOSTAT=ios) chem%reaction(i)%class,chem%reaction(i)%type, &
                                chem%reaction(i)%A,chem%reaction(i)%B,&
                                chem%reaction(i)%fB,chem%reaction(i)%nP, &
                            (chem%reaction(i)%P(j),chem%reaction(i)%fP(j),j=1,np),&
                                ndat,(chem%reaction(i)%data(j),j=1,ndat)
      IF( ios /= 0 )GOTO 9998
    END DO
    WRITE(lun_prj,IOSTAT=ios) chem%nOutGroup
    IF( ios /= 0 )GOTO 9998
    IF( chem%nOutGroup > 0 )THEN
      DO i = 1,chem%nOutGroup
        np = chem%OutGroup(i)%nComp
        WRITE(lun_prj,IOSTAT=ios) chem%OutGroup(i)%Name,np,(chem%OutGroup(i)%iComp(j),chem%OutGroup(i)%fComp(j),j=1,np)
      END DO
    END IF
  END IF
END DO

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error writing multicomponent arrays'
eRoutine = 'WriteChemMC'

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE SetChemSrfDosMC( p,ID,cfac,ng,ig,n0 )

USE struct_fd
USE chem_fi
USE constants_fd
USE scipuff_fi, ONLY: z_dosage, dt_save

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
INTEGER,               INTENT( IN    ) :: ID
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac
INTEGER,               INTENT( INOUT ) :: ng
INTEGER, DIMENSION(*), INTENT( INOUT ) :: ig
INTEGER,               INTENT( IN    ) :: n0

INTEGER nmc, i
INTEGER igrp, icomp
REAL    f

CALL GetChemAux( ID,p )

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,nmc
  IF( chem%species(i)%ldos )THEN
    ng = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = cfac(n0)*chem%species(i)%mass/p%c
    cfac(ng) = cfac(ng)/dt_save  ! Convert to Avg. concentrations
    IF( chem%oUnits == UNIT_UGM3 .AND. &
        chem%species(i)%class /= ID_SPECIES_PARTICLE )&
        cfac(ng) = cfac(ng)*PPM2UGM3*chem%species(i)%MW
  END IF
END DO

IF( chem%nOutGroup > 0 )THEN
  DO igrp = 1,chem%nOutGroup
    ng       = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = 0.
    DO icomp = 1,chem%OutGroup(igrp)%nComp
      i = chem%OutGroup(igrp)%iComp(icomp)
      f = chem%OutGroup(igrp)%fComp(icomp)
      IF( chem%oUnits == UNIT_UGM3 .AND. &
          chem%species(i)%class /= ID_SPECIES_PARTICLE )&
          f = f*PPM2UGM3*chem%species(i)%MW
      cfac(ng) = cfac(ng) + f*cfac(n0)*chem%species(i)%mass/p%c
    END DO
    cfac(ng) = cfac(ng)/dt_save  ! Convert to Avg. concentrations
  END DO
END IF

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE GetChemValMC( p,cID,csmp,fac,facr,x_sensor )

USE chem_fi
USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),    INTENT( IN  )   :: p
INTEGER, DIMENSION(:), POINTER       :: cID
REAL,  DIMENSION(*), INTENT( INOUT ) :: csmp
REAL,                INTENT( IN    ) :: fac
REAL,                INTENT( IN    ) :: facr
REAL,  DIMENSION(3), INTENT( IN    ) :: x_sensor

INTEGER i, mcID, ID, isp
REAL    xp, yp, zp, pvol
REAL    r, facspec, xmap, ymap, f1, f2
REAL(8) xn, yn, zn, dy0, y1, y2
REAL(8) argxz, argy0, argym, argyp

REAL(8), DIMENSION(6) :: asig
REAL(8), DIMENSION(6) :: tsig

i    = typeID(p%ityp)%imat
mcID = material(i)%mcID
ID   = mat_mc%ID(mcID)

species => chem%species

CALL GetChemAux( mcID,p )

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

xp = (x_sensor(1)-p%xbar)/xmap
yp = (x_sensor(2)-p%ybar)/ymap
zp = (x_sensor(3)-p%zbar)

asig(1) = DBLE(p%axx)
asig(2) = DBLE(p%axy)
asig(3) = DBLE(p%axz)
asig(4) = DBLE(p%ayy)
asig(5) = DBLE(p%ayz)
asig(6) = DBLE(p%azz)

CALL puff_rot( xp,yp,zp,p%uo,p%vo,asig,tsig,xn,yn,zn,dy0 )

pvol = PI3*SQRT(p%det)

DO i = 1,SIZE(cID)

  isp = cID(i)

  facspec = fac

  IF( species(isp)%lstar .AND. species(isp)%amb /= 0. )THEN
    !-- Set r between 0.(conc = -amb) and 1.(conc = 0.)
    r = MIN(1.0,MAX(0., 1. + species(isp)%conc/species(isp)%amb))
    r = r**1.2 !Too much tophat so try only r**1.2 instead of r**2.
  ELSE
    r = 1.0
  END IF

  IF( r < 1.0 )THEN
    argxz  = tsig(1)*xn*xn + 2.D0*tsig(3)*xn*zn + tsig(6)*zn*zn
    argy0  = 2.D0*tsig(2)*xn*yn + tsig(4)*yn*yn + 2.D0*tsig(5)*yn*zn + argxz
    !-- Separation between 3 Gaussians(1.8*sigma)
    dy0    = 1.8/SQRT(2.*tsig(4))
    y1     = yn - dy0
    argym  = 2.D0*tsig(2)*xn*y1 + tsig(4)*y1*y1 + 2.D0*tsig(5)*y1*zn + argxz
    y2     = yn + dy0
    argyp  = 2.D0*tsig(2)*xn*y2 + tsig(4)*y2*y2 + 2.D0*tsig(5)*y2*zn + argxz
    !-- Interpolate between Gaussion(r=1.0) and TopHat(r=0.)
    f1     = (2.*r + 1.)/3.
    f2     = (1. - r)/3.
    !-- Sum of three Gaussians
    facspec = (f1*EXP(-argy0) + f2*(EXP(-argym)+EXP(-argyp))) * facr
  END IF

  csmp(i) = csmp(i) + facspec*species(isp)%mass/pvol
END DO

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE GetChemMassMC( p,cID,comp )

USE chem_fi
USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),    INTENT( IN  )   :: p
INTEGER, DIMENSION(:), POINTER       :: cID
REAL,  DIMENSION(*), INTENT( INOUT ) :: comp

INTEGER i, mcID, ID

i    = typeID(p%ityp)%imat
mcID = material(i)%mcID
ID   = mat_mc%ID(mcID)

CALL GetChemAux( mcID,p )

DO i = 1,SIZE(cID)
  comp(i) = chem%species(cID(i))%mass
END DO

RETURN
END

!==============================================================================

SUBROUTINE puff_rot( xp,yp,zp,uo,vo,asig,tsig,xn,yn,zn,dy0 )

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: xp, yp, zp, uo, vo
REAL(8),               INTENT( OUT ) :: xn, yn, zn, dy0
REAL(8), DIMENSION(6), INTENT( IN  ) :: asig
REAL(8), DIMENSION(6), INTENT( OUT ) :: tsig

REAL(8), DIMENSION(3,3) :: a, b, c, at
REAL(8), DIMENSION(3)   :: x, xr

REAL(8) sp, cn, sn, ud, vd

b(1,:) = asig(1:3)
b(2,2) = asig(4)
b(2,3) = asig(5)
b(3,3) = asig(6)
b(2,1) = b(1,2)
b(3,1) = b(1,3)
b(3,2) = b(2,3)

ud = DBLE(uo)
vd = DBLE(vo)

sp = SQRT(ud*ud +vd*vd)
IF( sp /= 0.D0 )THEN
  cn = ud/sp
  sn = vd/sp
ELSE
  cn = 1.0D0
  sn = 0.0D0
END IF

a = 0.0D0
a(1,1) = cn
a(1,2) = sn
a(2,1) = -sn
a(2,2) = cn
a(3,3) = 1.D0

at = TRANSPOSE( a )
c = MATMUL( a,b )
b = MATMUL( c,at )

x(1) = DBLE(xp)
x(2) = DBLE(yp)
x(3) = DBLE(zp)
xr = MATMUL( a,x )
xn = xr(1)
yn = xr(2)
zn = xr(3)

IF( b(2,2) /= 0.D0 )THEN
  dy0 = -((b(1,2)*xn + b(2,3)*zn)/b(2,2))
ELSE
  dy0 = 0.0D0
END IF

tsig(1:3) = b(1,:)
tsig(4)   = b(2,2)
tsig(5)   = b(2,3)
tsig(6)   = b(3,3)

RETURN
END

!-------------------------------------------------------------------------
RECURSIVE SUBROUTINE InitChemInst( relSpec,p,ID )
USE chem_fi
USE step_p_fi, ONLY: myTid
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( IN    ) :: relSpec
TYPE( puff_str ),     INTENT( INOUT ) :: p
INTEGER,              INTENT( IN    ) :: ID

INTEGER i, j
REAL    frac

TYPE( MCrelData ), POINTER :: rel

chem => chemMC(ID,1)

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,nspectot
  chem%species(i)%mass = 0.
  chem%species(i)%conc = 0.
END DO

SELECT CASE( relSpec%release%type )
  CASE( HR_PRIME,HR_STACKFP )
    i = relSpec%release%padding
    frac= frac_prm(i)         !Fraction due to downwash
  CASE DEFAULT
    frac = 1.
END SELECT

rel => relSpec%MClist%firstMCrel

DO i = 1,relSpec%MClist%nList
  DO j = 1,nspectot
    IF( TRIM(rel%MCname) == TRIM(chem%species(j)%name) )THEN
      IF( chem%species(j)%class == ID_SPECIES_PARTICLE )THEN
        IF( LEN_TRIM(chem%species(j)%classAux) == 1 )THEN
          ! emission units always assumed to be g/s for particles (P type only)
          chem%species(j)%mass = frac*rel%MCmass*1.e+6 ! Must be ug/s for aerosol particles
        ENDIF
      ELSE
        chem%species(j)%mass = rel%MCmass * chem%eConv
        IF( chem%eConvType >= G_PPM )THEN
          chem%species(j)%mass = chem%species(j)%mass/chem%species(j)%mw
          chem%species(j)%mass = frac*chem%species(j)%mass
        END IF
      END IF
      EXIT
    END IF
  END DO
  rel => rel%next
END DO

vol   = 0.0
vself = 0.0
pres  = 0.0
temp  = 0.0

CALL PutChemAux( ID,p )

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

RECURSIVE SUBROUTINE InitChemCont( p,ID,dt,velp )
USE chem_fi
USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ID
REAL,             INTENT( IN    ) :: dt
REAL,             INTENT( IN    ) :: velp

INTEGER i

chem => chemMC(ID,1)

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

CALL GetChemAux( ID,p )

DO i = 1,nspectot
  chem%species(i)%mass = chem%species(i)%mass * dt
END DO

!----- Adjust overlap concentrations since puff mass has been scaled by dt

DO i = 1,chem%nStar
  chem%star(i)%s%conc = chem%star(i)%s%conc * dt
END DO

!----- Volume already has the dt factor

vol   = vol/velp
vself = vself/velp
pres  = 0.0
temp  = 0.0

CALL PutChemAux( ID,p )

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE ScaleStaticChem( p,ID,fac )

USE chem_fi
USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ID
REAL,             INTENT( IN    ) :: fac

INTEGER i

chem => chemMC(ID,1)

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

CALL GetChemAux( ID,p )

!----- Adjust overlap concentrations for static puff interactions

DO i = 1,chem%nStar
  chem%star(i)%s%conc = chem%star(i)%s%conc * fac
END DO

vol = vol*fac

CALL PutChemAux( ID,p )

9999 CONTINUE

RETURN
END

!========================================================================
RECURSIVE SUBROUTINE SetChemPointers()

USE scipuff_fi
USE multcomp_fd
USE chem_fi
USE chemReactions_fd
USE reallocate

!====   Set up pointers for fast/slow species and reactions

IMPLICIT NONE

INTEGER i, alloc_stat, nspec, nreact, nphoto

nspec = chem%nSpecies

!------ Count fast and slow species

nfast  = 0
nslow  = 0
nparticle  = 0
nequil = 0

DO i = 1,nspec
  chem%species(i)%eqID = 0
  SELECT CASE( chem%species(i)%class )
    CASE( ID_SPECIES_FAST )
      nfast = nfast + 1

    CASE( ID_SPECIES_SLOW )
      nslow = nslow + 1

    CASE( ID_SPECIES_PARTICLE )
      nparticle = nparticle + 1

    CASE( ID_SPECIES_EQUILIBRIUM )
      nequil = nequil + 1
      chem%species(i)%eqID = nequil

    CASE DEFAULT

  END SELECT
END DO

chem%nFast        = nfast
chem%nSlow        = nslow
chem%nGaseous     = ngaseous
chem%nParticle    = nparticle
chem%nEquilibrium = nequil

IF( ASSOCIATED(chem%fast)  )DEALLOCATE( chem%fast, STAT=alloc_stat )
IF( ASSOCIATED(chem%slow)  )DEALLOCATE( chem%slow, STAT=alloc_stat )
IF( ASSOCIATED(chem%gaseous)   )DEALLOCATE( chem%gaseous,  STAT=alloc_stat )
IF( ASSOCIATED(chem%particle)  )DEALLOCATE( chem%particle, STAT=alloc_stat )
IF( ASSOCIATED(chem%equil) )DEALLOCATE( chem%equil,STAT=alloc_stat )

IF( nfast > 0 )THEN
  ALLOCATE( chem%fast(nfast),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

IF( nslow > 0 )THEN
  ALLOCATE( chem%slow(nslow),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF
ngaseous = nfast + nslow
ALLOCATE( chem%gaseous(ngaseous),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating multicomponent gaseous arrays'
  eRoutine = 'SetChemPointers'
  GOTO 9999
END IF
IF( nparticle > 0 )THEN
  ALLOCATE( chem%particle(nparticle),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent particle arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

IF( nequil > 0 )THEN
  ALLOCATE( chem%equil(nequil),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

!---- Initialize pointers

nfast     = 0
nslow     = 0
ngaseous  = 0
nparticle = 0
nequil    = 0

DO i = 1,nspec
  SELECT CASE( chem%species(i)%class )
    CASE( ID_SPECIES_FAST )
      nfast = nfast + 1
      chem%fast(nfast)%s => chem%species(i)
      ngaseous = ngaseous + 1
      chem%gaseous(ngaseous)%s => chem%species(i)

    CASE( ID_SPECIES_SLOW )
      nslow = nslow + 1
      chem%slow(nslow)%s => chem%species(i)
      ngaseous = ngaseous + 1
      chem%gaseous(ngaseous)%s => chem%species(i)

    CASE( ID_SPECIES_PARTICLE )
      nparticle = nparticle + 1
      chem%particle(nparticle)%s => chem%species(i)

    CASE( ID_SPECIES_EQUILIBRIUM )
      nequil = nequil + 1
      chem%equil(nequil)%s => chem%species(i)

    CASE DEFAULT

  END SELECT
END DO

nreact = chem%nReactions

!------ Count fast and slow reactions

nfast  = 0
nslow  = 0
nequil = 0
nphoto = 0

DO i = 1,nreact

  IF( BTEST(chem%reaction(i)%class,ID_REACT_FAST)        )nfast  = nfast  + 1
  IF( BTEST(chem%reaction(i)%class,ID_REACT_SLOW)        )nslow  = nslow  + 1
  IF( BTEST(chem%reaction(i)%class,ID_REACT_EQUILIBRIUM) )nequil = nequil + 1

  IF( chem%reaction(i)%type == ID_K_RAD )nphoto = nphoto + 1

END DO

chem%nFastReact  = nfast
chem%nSlowReact  = nslow
chem%nEquilReact = nequil
chem%nPhotoReact = nphoto

IF( ASSOCIATED(chem%fReact) )DEALLOCATE( chem%fReact,STAT=alloc_stat )
IF( ASSOCIATED(chem%sReact) )DEALLOCATE( chem%sReact,STAT=alloc_stat )
IF( ASSOCIATED(chem%eReact) )DEALLOCATE( chem%eReact,STAT=alloc_stat )
IF( ASSOCIATED(chem%pReact) )DEALLOCATE( chem%pReact,STAT=alloc_stat )

IF( nfast > 0 )THEN
  ALLOCATE( chem%fReact(nfast),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

IF( nslow > 0 )THEN
  ALLOCATE( chem%sReact(nslow),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

IF( nequil > 0 )THEN
  ALLOCATE( chem%eReact(nequil),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

IF( nphoto > 0 )THEN
  ALLOCATE( chem%pReact(nphoto),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating multicomponent arrays'
    eRoutine = 'SetChemPointers'
    GOTO 9999
  END IF
END IF

!---- Initialize pointers

nfast  = 0
nslow  = 0
nequil = 0
nphoto = 0

DO i = 1,nreact

  IF( BTEST(chem%reaction(i)%class,ID_REACT_FAST) )THEN
    nfast = nfast + 1
    chem%fReact(nfast)%r => chem%reaction(i)
  END IF
  IF( BTEST(chem%reaction(i)%class,ID_REACT_SLOW) )THEN
    nslow = nslow + 1
    chem%sReact(nslow)%r => chem%reaction(i)
  END IF
  IF( BTEST(chem%reaction(i)%class,ID_REACT_EQUILIBRIUM) )THEN
    nequil = nequil + 1
    chem%eReact(nequil)%r => chem%reaction(i)
  END IF
  IF( chem%reaction(i)%type == ID_K_RAD )THEN
    nphoto = nphoto + 1
    chem%pReact(nphoto)%r => chem%reaction(i)
  END IF

END DO

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE SetChemAqaer( )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER i,alloc_stat

IF( .NOT. ASSOCIATED(chem_aqaer%species) )THEN

  chem_aqaer%cUnits   = chem%cUnits
  chem_aqaer%nSpecies = chem%nSpecies

  ALLOCATE( chem_aqaer%species(chem%nSpecies),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'SetChemAqaer'
    eMessage = 'Error allocating chem_aqaer%species array'
    GOTO 9999
  END IF
  DO i = 1,chem%nSpecies
    chem_aqaer%species(i)%name   = chem%species(i)%name
    chem_aqaer%species(i)%class  = chem%species(i)%class
  END DO

END IF

DO i = 1,chem%nSpecies
  chem_aqaer%species(i)%taudry = chem%species(i)%taudry
  chem_aqaer%species(i)%tauwet = chem%species(i)%tauwet
  chem_aqaer%species(i)%conc   = chem%species(i)%conc
  chem_aqaer%species(i)%amb    = chem%species(i)%amb
  chem_aqaer%species(i)%csav   = chem%species(i)%csav
END DO

9999 CONTINUE
RETURN
END

!=======================================================================

SUBROUTINE GetChemAqaer( )

USE chem_fi

IMPLICIT NONE

INTEGER i

DO i = 1,chem%nSpecies
  chem%species(i)%taudry = chem_aqaer%species(i)%taudry
  chem%species(i)%tauwet = chem_aqaer%species(i)%tauwet
  chem%species(i)%conc   = chem_aqaer%species(i)%conc
  chem%species(i)%amb    = chem_aqaer%species(i)%amb
  chem%species(i)%csav   = chem_aqaer%species(i)%csav
END DO

RETURN
END
!=======================================================================

SUBROUTINE InitMCUnits( imat,mcKind,units )

USE scipuff_fi
USE error_fi
USE chem_fi
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

INTEGER      , INTENT( IN    ) :: imat
INTEGER      , INTENT( IN    ) :: mcKind
CHARACTER(16), INTENT( INOUT ) :: units

INTEGER i, mcID

mcID  = material(imat)%mcID
units = NOT_SET_C
chem  => chemMC(mcID,1)
IF( chem%species(mcKind)%class == ID_SPECIES_PARTICLE )THEN
  DO i = 1,chem%pTypeN
    IF( TRIM(chem%species(mcKind)%classAux) == TRIM(chem%pTypes(i)) )THEN
      units = chem%pUnits(i)
      EXIT
    END IF
  END DO
  IF( TRIM(units) == TRIM(NOT_SET_C) )units = 'ug/m3'
END IF
RETURN
END

!=======================================================================

SUBROUTINE ReadChemDiagnostics()

USE diagnostics_fi
USE scipuff_fi
USE error_fi
USE files_fi
USE chem_fi

IMPLICIT NONE

INTEGER       i
INTEGER       ios
REAL          tem
LOGICAL       isOpen
CHARACTER(8)  fflag
CHARACTER(80) cmsg,cmsg2,cmsg3

cmsg  = 'Reading diagnostics file'
cmsg2 = CHAR(0)
cmsg3 = CHAR(0)

INQUIRE( UNIT=lun_dgn,OPENED=isOpen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadChemDiagnostics'
  WRITE(eMessage,'(A,I0)') 'Error checking on open status of dgn file: IOS=',ios
  CALL ReportFileName( eInform,'File=',file_dgn )
  lDiagno = .FALSE.
END IF

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

IF( lDiagno )THEN

  CALL AllocChemDiagnostics()

  !==== Check status for writing

  IF( isOpen )CLOSE( UNIT=lun_dgn )

  IF( lDepBin )THEN
    lDepBin = .TRUE.
    OPEN( UNIT=lun_dgn,FILE=file_dgn,FORM='UNFORMATTED',ACTION='READ',IOSTAT=ios )
    READ(lun_dgn,IOSTAT=ios)fflag
    IF( ios /= 0 .OR. fflag /= 'BBBBBBBB' )THEN
      CLOSE(lun_dgn,IOSTAT=ios)
      OPEN( UNIT=lun_dgn,FILE=file_dgn,ACTION='READ',IOSTAT=ios )
      READ(lun_dgn,'(A8)',IOSTAT=ios)fflag
      IF( ios == 0 .AND. fflag == 'FFFFFFFF' )THEN
        lDepBin = .FALSE.
      ELSE
        nError   = RD_ERROR
        eRoutine = 'ReadChemDiagnostics'
        eMessage = 'Error reading flag from diagnostic file'
        WRITE(eInform,*) 'File =',TRIM(file_dgn)
        GO TO 9999
      END IF
    END IF
  ELSE
    OPEN( UNIT=lun_dgn,FILE=file_dgn,ACTION='READ',IOSTAT=ios )
    READ(lun_dgn,'(A8)',IOSTAT=ios)fflag
    IF( ios /= 0 .OR. fflag /= 'FFFFFFFF' )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadChemDiagnostics'
      eMessage = 'Error reading flag from diagnostic file'
      WRITE(eInform,*) 'File =',TRIM(file_dgn)
      GO TO 9999
    END IF
  END IF
  IF( lDepBin )THEN
    READ(lun_dgn,IOSTAT=ios)i
  ELSE
    READ(lun_dgn,'(I5)',IOSTAT=ios)i
  END IF
  IF( ndvar /= i )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemDiagnostics'
    eMessage = 'ndvar on file does not match calculated value'
    WRITE(eInform,*) 'i,ndvar,File = ',i,ndvar,TRIM(file_dgn)
    GO TO 9999
  END IF
  IF( lDepBin )THEN
    READ(lun_dgn,IOSTAT=ios)(dgname(i),i=1,ndvar)
  ELSE
    READ(lun_dgn,*,IOSTAT=ios)(dgname(i),i=1,ndvar)
  ENDIF
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemDiagnostics'
    eMessage = 'Error reading dgnames'
    WRITE(eInform,*) 'File = ',TRIM(file_dgn)
    GO TO 9999
  END IF

  tem = -999.
  DO WHILE ( .TRUE. )
    IF( lDepBin )THEN
      READ(lun_dgn,IOSTAT=ios) tem,ndump,(emission(i),statics(i),boundary(i),transport(i),&
                               ddeposition(i),wdeposition(i),chemistry(i),active(i),removed(i),&
                               ppmfac(i),i=1,nspectot+1)
    ELSE

      READ(lun_dgn,*,IOSTAT=ios) tem,ndump,(emission(i),statics(i),boundary(i),transport(i),&
                                 ddeposition(i),wdeposition(i),chemistry(i),active(i),removed(i),&
                                 ppmfac(i),i=1,nspectot+1)
    END IF
    IF( ios /= 0 )THEN
      IF( ios < 0 )THEN
        EXIT
      ELSE
        WRITE(eMessage,'(A,I0)') 'Error reading time from dgn file: IOS=',ios
      END IF
      nError   = RD_ERROR
      eRoutine = 'ReadChemDiagnostics'
      WRITE(eInform,"('Last valid time = ',F8.1)")tem
      eAction = 'Restart Dgn File='//TRIM(file_dgn)
      GOTO 9999
    END IF

  END DO
  CLOSE( UNIT=lun_dgn )
  IF( lDepBin )THEN
    OPEN( UNIT=lun_dgn,FILE=file_dgn,FORM='UNFORMATTED',ACTION='READWRITE',POSITION='APPEND',IOSTAT=ios )
  ELSE
    OPEN( UNIT=lun_dgn,FILE=file_dgn,ACTION='READWRITE',POSITION='APPEND',IOSTAT=ios )
  END IF
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemDiagnostics'
    eMessage = 'Error opening diagnostic file in append mode'
    WRITE(eInform,*) 'File =',TRIM(file_dgn)
    GO TO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE CreateChemOutput()

USE error_fi

IMPLICIT NONE

CALL WriteChemDiagnostics()
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteChemStepAmb()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!=======================================================================

SUBROUTINE WriteChemDiagnostics( )

USE diagnostics_fi
USE scipuff_fi
USE error_fi
USE files_fi
USE chem_fi
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

INTEGER mcID, ID, i, isp, ios

CHARACTER(80) cmsg,cmsg2,cmsg3

ID = 0

DO mcID = 1,mat_mc%nMCtype
  IF( mat_mc%type(mcID) == MC_CHEM )THEN
    ID   = mat_mc%ID(mcID)
    chem => chemMC(ID,1)
    EXIT
  END IF
END DO

IF( ID == 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteChemDiagnostics'
  WRITE(eMessage,'(A,I0)') 'Cannot find multicomponent id'
  WRITE(eInform,"('Output time =',F8.1)") t
  eAction = ""
  GOTO 9999
END IF

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

IF( lDiagno )THEN

  cmsg  = 'Writing diagnostics file'
  cmsg2 = CHAR(0)
  cmsg3 = CHAR(0)
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999

  DO i = 1, nspectot+1
    active(i) = 0.
  END DO

  DO i = 1, npuf
    IF ( puff(i)%idtl >= 0 ) THEN
      CALL GetChemAux( ID,puff(i) )
      DO isp = 1, nspectot
        active(isp) = active(isp) + chem%species(isp)%mass
      END DO
      active(nspectot+1) = active(nspectot+1) + puff(i)%c
    END IF
  END DO

  IF( lDepBin )THEN
    WRITE( lun_dgn,IOSTAT=ios ) t,ndump,(emission(i),statics(i),boundary(i),transport(i),&
                   ddeposition(i),wdeposition(i),chemistry(i),active(i),removed(i),&
                   ppmfac(i),i=1,nspectot+1)
  ELSE
    WRITE( lun_dgn,*,IOSTAT=ios ) t,ndump,(emission(i),statics(i),boundary(i),transport(i),&
                   ddeposition(i),wdeposition(i),chemistry(i),active(i),removed(i),&
                   ppmfac(i),i=1,nspectot+1)
  END IF
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'WriteChemDiagnostics'
    WRITE(eMessage,'(A,I0)') 'Error writing to dgn file with IOSTAT = ',ios
    WRITE(eInform,"('Output time =',F8.1)") t
    eAction = 'Check Dgn File='//TRIM(file_dgn)
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END
