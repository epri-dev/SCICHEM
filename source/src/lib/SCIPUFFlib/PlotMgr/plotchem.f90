SUBROUTINE CountChemKindMC( ID,nkind )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ID
INTEGER, INTENT( OUT ) :: nkind

INTEGER nmc, ndos, ndep, i

chem => chemMC(ID,1)

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium
ndos = 0
ndep = 0

DO i = 1,nmc
  IF( chem%species(i)%ldos )ndos = ndos + 1
  IF( chem%species(i)%ldep )ndep = ndep + 2 ! Dry and Wet dep
END DO

nkind = nmc   !CONCENTRATION plots
IF( chem%nOutGroup > 0 )THEN
  nkind = nkind + ndep + 2*chem%nOutGroup  !Group dep output + individual species
ELSE
  IF( ndep < nmc )nkind = nkind + ndep           !Otherwise, point to concentration kinds
END IF
IF( chem%nOutGroup > 0 )THEN
  nkind = nkind + ndos + chem%nOutGroup  !Group dose output + individual species
ELSE
  IF( ndos < nmc )nkind = nkind + ndos         !Otherwise, point to concentration kinds
END IF

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE AddChemKindMC( ID,ikind,KindString,is,nkind )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER,                        INTENT( IN    ) :: ID
INTEGER,                        INTENT( INOUT ) :: ikind
CHARACTER(64), DIMENSION(*),    INTENT( INOUT ) :: KindString
INTEGER,       DIMENSION(4),    INTENT( OUT   ) :: is
INTEGER,       DIMENSION(4),    INTENT( OUT   ) :: nkind

INTEGER nmc, ndos, ndep, i

chem => chemMC(ID,1)

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

!---- Concentration plots

is(1)    = ikind + 1
nkind(1) = nmc

!---- Integrated Concentration plots

is(2)    = 0
nkind(2) = 0

!---- Add component names to KindString and check for surface plots

ndos = 0
ndep = 0

DO i = 1,nmc

  ikind = ikind + 1
  KindString(ikind) = TRIM(chem%species(i)%name)

  IF( chem%species(i)%ldos )ndos = ndos + 1
  IF( chem%species(i)%ldep )ndep = ndep + 1

END DO

!---- Deposition plots

nkind(3) = 0
IF( ndep == 0 )THEN
  is(3) = 0
ELSE IF( ndep == nmc .AND. chem%nOutGroup == 0 )THEN
  is(3)    = is(1)
  nkind(3) = nmc
ELSE
  is(3) = ikind + 1
  DO i = 1,nmc
    IF( chem%species(i)%ldep )THEN
      nkind(3) = nkind(3) + 1
      ikind    = ikind + 1
      KindString(ikind) = TRIM(chem%species(i)%Name)//" (DRY)"
      nkind(3) = nkind(3) + 1
      ikind    = ikind + 1
      KindString(ikind) = TRIM(chem%species(i)%Name)//" (WET)"
    END IF
  END DO
END IF

IF( chem%nOutGroup > 0 )THEN
  IF( ndep == 0 )is(3) = ikind + 1
  DO i = 1,chem%nOutGroup
    nkind(3) = nkind(3) + 1
    ikind    = ikind + 1
    KindString(ikind) = TRIM(chem%OutGroup(i)%Name)//" (DRY)"
    nkind(3) = nkind(3) + 1
    ikind    = ikind + 1
    KindString(ikind) = TRIM(chem%OutGroup(i)%Name)//" (WET)"
  END DO
END IF

!---- Dosage plots

nkind(4) = 0
IF( ndos == 0 )THEN
  is(4) = 0
ELSE IF( ndos == nmc .AND. chem%nOutGroup == 0 )THEN
  is(4)    = is(1)
  nkind(4) = nmc
ELSE
  is(4) = ikind + 1
  DO i = 1,nmc
    IF( chem%species(i)%ldos )THEN
      nkind(4) = nkind(4) + 1
      ikind    = ikind + 1
      KindString(ikind) = TRIM(chem%species(i)%name)
    END IF
  END DO
END IF

IF( chem%nOutGroup > 0 )THEN
  IF( ndos == 0 )is(4) = ikind + 1
  DO i = 1,chem%nOutGroup
    nkind(4) = nkind(4) + 1
    ikind    = ikind + 1
    KindString(ikind) = TRIM(chem%OutGroup(i)%Name)
  END DO
END IF

RETURN
    END

!-------------------------------------------------------------------------
SUBROUTINE InitChemSliceSurfMC( ID,icomp,grdI )

USE chem_fi
USE error_fi
USE sagstr_fd
USE scipuff_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID    ! Multicomponent ID
INTEGER, INTENT( IN ) :: grdI  ! index of slice SAG grid structure
INTEGER, INTENT( IN ) :: icomp ! Component number

INTEGER :: i, ix, iy
REAL    :: x, y, z

TYPE( SAGgrid_str ), POINTER :: srf

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'InitChemSliceSurfMC'
  GOTO 9999
END IF

!--- Initialize top level grid to ambient value

chem => chemMC(ID,1)
z     = 0.

!--- Initialize top level grid to ambient value
IF( chem%lAmbFile )THEN
  DO ix = 1,srf%nx
    x = srf%xmin + (FLOAT(ix-1)+0.5)*srf%dx
    DO iy = 1,srf%ny
      i = (iy-1)*srf%nx + ix
      y = srf%ymin + (FLOAT(iy-1)+0.5)*srf%dy
      IF( chem%lAddAmb )THEN
        CALL SetChemAmbient( x,y,z,t,.FALSE.,.FALSE.,.TRUE. )
      ELSE
        chem%species(icomp)%amb = 0.0
      ENDIF
      IF( nError /= NO_ERROR )GOTO 9999
      srf%ipdat(i) = chem%species(icomp)%amb
    END DO
  END DO
ELSE IF( chem%lStepAmb )THEN  !use stepped ambient for single point
  srf%ipdat(:) = chem%Ambient%stepAmb(1,1,icomp)
ELSE
  IF( chem%lAddAmb )THEN
    DO i = 1,srf%nx*srf%ny
      srf%ipdat(i) = chem%species(icomp)%ambient
    END DO
  ELSE
    DO i = 1,srf%nx*srf%ny
      srf%ipdat(i) = 0.0
    END DO
  ENDIF
END IF

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE InitChemSliceHorizMC( ID,icomp,grdI,slice )

USE met_fi
USE chem_fi
USE error_fi
USE slice_fd
USE sagstr_fd
USE scipuff_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: ID    ! Multicomponent ID
INTEGER,           INTENT( IN ) :: grdI  ! index of slice SAG grid structure
INTEGER,           INTENT( IN ) :: icomp ! Component number
TYPE( slice_str ), INTENT( IN ) :: slice ! slice definition structure

INTEGER :: i, ix, iy
REAL    :: x, y, z
REAL    :: hx, hy, hz

TYPE( SAGgrid_str ), POINTER :: srf

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'InitChemSliceHorizMC'
  GOTO 9999
END IF

chem => chemMC(ID,1)
z      = slice%data(SD_HEIGHT) - hmin

!--- Initialize top level grid to ambient value
IF( chem%lAmbFile )THEN
  DO iy = 1,srf%ny
    y = srf%ymin + (FLOAT(iy-1)+0.5)*srf%dy
    DO ix = 1,srf%nx
      i = (iy-1)*srf%nx + ix
      x = srf%xmin + (FLOAT(ix-1)+0.5)*srf%dx
      IF( lter )THEN
        CALL get_topogPrj( x,y,hz,hx,hy )
      ELSE
        hz = 0.0
      END IF
      IF( chem%lAddAmb )THEN
        CALL SetChemAmbient( x,y,z-hz,t,.FALSE.,.FALSE.,.TRUE. )
      ELSE
        chem%species(icomp)%amb = 0.0
      ENDIF
      IF( nError /= NO_ERROR )GOTO 9999
      srf%ipdat(i) = chem%species(icomp)%amb
    END DO
  END DO
ELSE IF( chem%lStepAmb )THEN !use stepped ambient for single point
  srf%ipdat(:) = chem%Ambient%stepAmb(1,1,icomp)
ELSE
  IF( chem%lAddAmb )THEN
    DO i = 1,srf%nx*srf%ny
      srf%ipdat(i) = chem%species(icomp)%ambient
    END DO
  ELSE
    DO i = 1,srf%nx*srf%ny
      srf%ipdat(i) = 0.0
    END DO
  ENDIF
END IF

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE InitChemSliceVertMC( ID,icomp,grdI,slice )

USE met_fi
USE chem_fi
USE error_fi
USE slice_fd
USE sagstr_fd
USE scipuff_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: ID    ! Multicomponent ID
INTEGER,           INTENT( IN ) :: grdI  ! index of slice SAG grid structure
INTEGER,           INTENT( IN ) :: icomp ! Component number
TYPE( slice_str ), INTENT( IN ) :: slice ! slice definition structure

INTEGER :: i, ix, iy
REAL    :: x, y, z
REAL    :: xt, dxt, dyt
REAL    :: hx, hy, hz

TYPE( SAGgrid_str ), POINTER :: srf

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'InitChemSliceVertMC'
  GOTO 9999
END IF

chem => chemMC(ID,1)
dxt   = slice%xmax - slice%xmin
dyt   = slice%ymax - slice%ymin

!--- Initialize top level grid to ambient value

IF( chem%lAmbFile )THEN
  DO ix = 1,srf%nx
    xt = srf%xmin + (FLOAT(ix-1)+0.5)*srf%dx
    x  = slice%xmin + xt*dxt
    y  = slice%ymin + xt*dyt
    DO iy = 1,srf%ny
      i = (iy-1)*srf%nx + ix
      IF( lter )THEN
        CALL get_topogPrj( x,y,hz,hx,hy )
      ELSE
        hz = 0.0
      END IF
      z = srf%ymin + (FLOAT(iy-1)+0.5)*srf%dy - hmin - hz
      IF( chem%lAddAmb )THEN
        CALL SetChemAmbient( x,y,z-hz,t,.FALSE.,.FALSE.,.TRUE. )
      ELSE
        chem%species(icomp)%amb = 0.0
      ENDIF
      IF( nError /= NO_ERROR )GOTO 9999
      srf%ipdat(i) = chem%species(icomp)%amb
    END DO
  END DO
ELSE IF( chem%lStepAmb )THEN !use stepped ambient for single point
  srf%ipdat(:) = chem%Ambient%stepAmb(1,1,icomp)
ELSE
  IF( chem%lAddAmb )THEN
  DO i = 1,srf%nx*srf%ny
    srf%ipdat(i) = chem%species(icomp)%ambient
  END DO
  ELSE
    DO i = 1,srf%nx*srf%ny
      srf%ipdat(i) = 0.0
    END DO
  END IF
END IF
9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE SetChemAmbDos( imat,ifld0,tfac )

USE scipuff_fi
USE error_fi
USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imat
INTEGER, INTENT( IN ) :: ifld0
REAL,    INTENT( IN ) :: tfac

INTEGER mcID, ID, nmc, nsd, i, alloc_stat

INTEGER, DIMENSION(:), ALLOCATABLE :: ispecie

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    ID  = mat_mc%ID(mcID)
    chem => chemMC(ID,1)
    nmc = chem%nFast + chem%nSlow +chem%nParticle + chem%nEquilibrium
    nsd = 0

    DO i = 1,nmc
      IF( chem%species(i)%ldos )THEN
        nsd = nsd + 1
      END IF
    END DO

    IF( nsd > 0 )THEN

      ALLOCATE( ispecie(nsd),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError = UK_ERROR
        eRoutine = 'SetChemAmbDos'
        eMessage = 'Error allocating list of multi-component species for ambient dose contribution'
        GOTO 9999
      END IF
      nsd = 0
      DO i = 1,nmc
        IF( chem%species(i)%ldos )THEN
          nsd = nsd + 1
          ispecie(nsd) = i
        END IF
      END DO

    ELSE

      ALLOCATE( ispecie(1),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError = UK_ERROR
        eRoutine = 'SetChemAmbDos'
        eMessage = 'Error allocating list of multi-component species for ambient dose contribution'
        GOTO 9999
      END IF
      ispecie(1) = 0

    END IF

    IF( chem%nOutGroup > 0 .OR. nsd > 0 )THEN
      CALL AddChemAmbientDoseMC(ID,ifld0,tfac,nsd,ispecie )
    END IF

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetChemAmbDosRes'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

IF( ALLOCATED(ispecie) )DEALLOCATE(ispecie,STAT=alloc_stat)

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE AddChemAmbientDoseMC( ID,ifld0,tfac,nsd,ispecie )

!------ Add ambient contribution to top level of dose field

USE chem_fi
USE met_fi
USE error_fi
USE sagdef_fd
USE sagstr_fd
USE scipuff_fi
USE surface_fi
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID      !Multicomponent ID
INTEGER, INTENT( IN ) :: ifld0   !Output field offset
REAL,    INTENT( IN ) :: tfac    !Time factor (s)
INTEGER, INTENT( IN ) :: nsd     !No. of individual specie doses
INTEGER, DIMENSION(*), &
         INTENT( IN ) :: ispecie !Indices of species

INTEGER :: i, ix, iy, i0, ig, j, k, l, n, isp
REAL    :: x, y, z, f

TYPE( SAGgrid_str ), POINTER :: srf, srfa

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

INTEGER, EXTERNAL :: SAG_CopyGridID

!----- Set SAG grid structure

srf => SAG_PtrGrdStr( srfdos )
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'AddChemAmbientDoseMC'
  GOTO 9999
END IF

srfa => SAG_PtrGrdStr( srfados )
IF( .NOT.ASSOCIATED( srfa ))THEN
  nError = UK_ERROR
  eRoutine = 'AddChemAmbientDoseMC'
  GOTO 9999
END IF

!----- Loop over top level grid

chem => chemMC(ID,myTid)

z = z_dosage

IF( chem%lAmbFile )THEN

  DO ix = 1,srf%nx
    x = srf%xmin + (FLOAT(ix-1)+0.5)*srf%dx

    DO iy = 1,srf%ny
      i = (iy-1)*srf%nx + ix
      y = srf%ymin + (FLOAT(iy-1)+0.5)*srf%dy

      CALL get_met(x,y,z,0.,0.,1 )

      CALL SetChemAmbient( x,y,z,t,.FALSE.,.FALSE.,.TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999

      srfa%ipdat(i)             = srfa%ipdat(i)             + tfac/dt_save*tb           ! temp(k)
      srfa%ipdat(srf%mxgrd+i)   = srfa%ipdat(srf%mxgrd+i)   + tfac/dt_save*pb/1013.25   ! press(atm)
      srfa%ipdat(2*srf%mxgrd+i) = srfa%ipdat(2*srf%mxgrd+i) + tfac/dt_save*hb           ! humidity(g H2O/g dry air)

      ig = ifld0

      IF( nsd > 0 )THEN
        DO j = 1,nsd
          ig = ig + 1
          i0 = (ig-1)*srf%mxgrd
          isp = ispecie(j)
          ! Assume ambient non particle species concentrations are in ppm. Also, convert dosage to concentration.
          f = chem%species(isp)%amb*tfac/dt_save
          IF( chem%oUnits == UNIT_UGM3 .AND. chem%species(isp)%class /= ID_SPECIES_PARTICLE )&
            f = f*PPM2UGM3*chem%species(isp)%MW
          srf%ipdat(i0+i)  = srf%ipdat(i0+i)  + f
          srfa%ipdat(i0+i) = srfa%ipdat(i0+i) + f
        END DO
      END IF

      DO k = 1,chem%nOutGroup
        ig = ig + 1
        i0 = (ig-1)*srf%mxgrd
        DO j = 1,chem%OutGroup(k)%nComp
          l = chem%OutGroup(k)%iComp(j)
          f = chem%OutGroup(k)%fComp(j)*chem%species(l)%amb*tfac/dt_save
          ! Assume ambient non particle species concentrations are in ppm. Also, convert dosage to concentration.
          IF( chem%oUnits == UNIT_UGM3 .AND. chem%species(l)%class /= ID_SPECIES_PARTICLE )&
            f = f*PPM2UGM3*chem%species(l)%MW
          srf%ipdat(i0+i)  = srf%ipdat(i0+i)  + f
          srfa%ipdat(i0+i) = srfa%ipdat(i0+i) + f
        END DO
      END DO

    END DO
  END DO

ELSE IF( .FALSE. )THEN  !use stepped ambient for single point. Currently chem%lStepAmb disabled

  n  = srf%nx*srf%ny
  ig = ifld0

  IF( nsd > 0 )THEN
    DO j = 1,nsd
      ig = ig + 1
      i0 = (ig-1)*srf%mxgrd
      srf%ipdat(i0+1:i0+n) = srf%ipdat(i0+1:i0+n) + chem%Ambient%stepAmb(1,1,ispecie(j)) * tfac
    END DO
  END IF

  DO k = 1,chem%nOutGroup
    ig = ig + 1
    i0 = (ig-1)*srf%mxgrd
    DO j = 1,chem%OutGroup(k)%nComp
      l = chem%OutGroup(k)%iComp(j)
      f = chem%OutGroup(k)%fComp(j)
      srf%ipdat(i0+1:i0+n) = srf%ipdat(i0+1:i0+n) + f*chem%Ambient%stepAmb(1,1,l) * tfac
    END DO
  END DO

ELSE

  x =  srf%xmin + 0.5*FLOAT(srf%nx)*srf%dx
  y =  srf%ymin + 0.5*FLOAT(srf%ny)*srf%dy
  z =  z_dosage
  CALL get_met(x,y,z,0.,0.,1 )  !met at domain center

  n  = srf%nx*srf%ny

  srfa%ipdat(1:n)                         = srfa%ipdat(1:n)                         + tb          ! temp(k)
  srfa%ipdat(srf%mxgrd+1:srf%mxgrd+n)     = srfa%ipdat(srf%mxgrd+1:srf%mxgrd+n)     + pb/1013.25  ! press(atm)
  srfa%ipdat(2*srf%mxgrd+1:2*srf%mxgrd+n) = srfa%ipdat(2*srf%mxgrd+1:2*srf%mxgrd+n) + hb          ! humidity(g H2O/g dry air)

  ig = ifld0

  IF( nsd > 0 )THEN
    DO j = 1,nsd
      ig  = ig + 1
      i0  = (ig-1)*srf%mxgrd
      isp = ispecie(j)
      f   = chem%species(isp)%ambient*tfac/dt_save
      ! Assume ambient non particle species concentrations are in ppm. Also, convert dosage to concentration.
      IF( chem%oUnits == UNIT_UGM3 .AND. chem%species(isp)%class /= ID_SPECIES_PARTICLE )&
        f = f*PPM2UGM3*chem%species(isp)%MW
      srf%ipdat(i0+1:i0+n)  = srf%ipdat(i0+1:i0+n)  + f
      srfa%ipdat(i0+1:i0+n) = srfa%ipdat(i0+1:i0+n) + f
    END DO
  END IF

  DO k = 1,chem%nOutGroup
    ig = ig + 1
    i0 = (ig-1)*srf%mxgrd
    DO j = 1,chem%OutGroup(k)%nComp
      l = chem%OutGroup(k)%iComp(j)
      f = chem%OutGroup(k)%fComp(j)*chem%species(l)%ambient*tfac/dt_save
      ! Assume ambient non particle species concentrations are in ppm. Also, convert dosage to concentration.
      IF( chem%oUnits == UNIT_UGM3 .AND. chem%species(l)%class /= ID_SPECIES_PARTICLE )&
        f = f*PPM2UGM3*chem%species(l)%MW
      srf%ipdat(i0+1:i0+n)  = srf%ipdat(i0+1:i0+n)  + f
      srfa%ipdat(i0+1:i0+n) = srfa%ipdat(i0+1:i0+n) + f
    END DO
  END DO

END IF

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE StepMCDoseEq()

!------ Step equilibrium or MC total dose from srcdos after pushing to bottom grid

USE met_fi
USE chem_fi
USE error_fi
USE scipuff_fi
USE surface_fi
USE sagstr_fd
USE sagcel_fd
USE sagdef_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

INTEGER irv, astat
INTEGER i, ix, iy, icell
INTEGER nfld

LOGICAL lStepEq

INTEGER, DIMENSION(:), ALLOCATABLE :: ifld
TYPE( SAGcell_str )                :: p0

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
  RECURSIVE INTEGER FUNCTION SAG_BottomWalk( grdC,p0,UserFunction )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grdC
    TYPE( SAGcell_str ), INTENT( IN ) :: p0
    INTEGER, EXTERNAL                 :: UserFunction
  END FUNCTION SAG_BottomWalk
END INTERFACE

INTEGER, EXTERNAL :: SAG_CopyGridID, SAG_BottomValueID, StepMCTotDose

lStepEq = .FALSE.

IF( .NOT. lStepEq ) RETURN
!----- Set SAG grid structure

grdC => SAG_PtrGrdStr( srfdos )

!imat = typeID(p%ityp)%imat)
!mcID  = material(imat)%mcID

chem => chemMC(1,myTid) ! Hard coded mcID for EPRI

nfld = grdC%mxfld

IF( nfld <= 3 )THEN
  nError = UK_ERROR
  eRoutine = 'StepMCDoseEq'
  GOTO 9999
END IF

IF( grdC%ipnam(3) == 'Scl' )THEN
  nMult = grdC%nvart - 3
ELSE
  nMult = grdC%nvart - 2
END IF
iMult = grdC%nvart - nMult


!==== Push to bottom grid
ALLOCATE(ifld(nfld),STAT=astat)

DO i = 1,nfld
  ifld(i) = i
END DO

irv = SAG_BottomValueID( srfdos,nfld,ifld )

!==== Walk to the bottom grid

DO ix = 1,grdC%nx
  DO iy = 1,grdC%ny

    icell = (iy-1)*grdC%nx + ix

    p0%id  = icell
    p0%x   = FLOAT(ix) - 0.5
    p0%y   = FLOAT(iy) - 0.5
    p0%hx  = 1.0
    p0%hy  = 1.0
    p0%lev = 0

    i = SAG_BottomWalk( grdC,p0,StepMCTotDose )
    IF( i /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'StepMCDoseEq'
      eMessage = 'Error setting ambient with StepMCTotDose'
      GOTO 9999
    END IF

  END DO
END DO

9999 CONTINUE

IF( ALLOCATED(ifld) )DEALLOCATE(ifld,STAT=astat)

RETURN
END

!-------------------------------------------------------------------------

INTEGER FUNCTION StepMCTotDose( dat,mx,p )

USE sagdef_fd
USE sagcel_fd
USE chem_fi
USE met_fi
USE error_fi
USE scipuff_fi, ONLY:t,z_dosage

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: mx
REAL, DIMENSION(:),  POINTER      :: dat
TYPE( SAGcell_str ), INTENT( IN ) :: p

INTEGER astat,ifld,i
REAL xx, yy, zz

REAL, DIMENSION(:), ALLOCATABLE :: spConc

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

xx = grdC%xmin+p%x*grdC%dx
yy = grdC%ymin+p%y*grdC%dy
zz = z_dosage

! Get met as tb=temp(k),pb/1013.25=press(atm),hb=humidity(g H2O/g dry air)
CALL get_met(xx,yy,zz,0.,0.,1 )
IF( nError /= NO_ERROR )GOTO 9999

!--- Get ambient in chem%species(i)%amb
CALL SetChemAmbient( xx,yy,zz,t,.FALSE.,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

ALLOCATE(spConc(nMult),STAT=astat)

! Get spConc
ifld = iMult
DO i = 1,chem%nSpecies
  IF( chem%species(i)%ldos )THEN
    ifld  = ifld + 1
    spConc(ifld-iMult) = dat(mx*(ifld-1)+p%ID)
  END IF
END DO

! Put spConc
ifld = iMult
DO i = 1,chem%nSpecies
  IF( chem%species(i)%ldos )THEN
    ifld  = ifld + 1
    dat(mx*(ifld-1)+p%ID) = spConc(ifld-iMult)
  END IF
END DO

StepMCTotDose = SAG_OK

9999 CONTINUE

IF( ALLOCATED(spConc) )DEALLOCATE(spConc,STAT=astat)

RETURN
END
