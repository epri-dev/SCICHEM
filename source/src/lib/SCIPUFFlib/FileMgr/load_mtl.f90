!*******************************************************************************
!            UnloadMaterial
!*******************************************************************************
SUBROUTINE UnloadMaterial( mtlHead,mtlList )

USE list_fd
USE material_fd
USE scipuff_fi
USE class_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( listHeadT )              , INTENT( IN ) :: mtlHead
TYPE( materialT ), DIMENSION(*), INTENT( IN ) :: mtlList

INTEGER i
INTEGER icls, alloc_stat

CHARACTER(16) class

INTEGER, EXTERNAL :: SetClass
INTEGER, EXTERNAL :: SetClassEvap
LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: AddClass

!==== Unload

!==== Initialize

ntypm = 0
nmaux = 0

!==== Loop over materials

AnyMaterials: IF( mtlHead%number > 0 )THEN

  IF( ALLOCATED(material) )DEALLOCATE( material,STAT=alloc_stat )
  ALLOCATE( material(mtlHead%number),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'UnloadMaterial'
    eMessage = 'Error allocating material array'
    WRITE(eInform,'(A,I4)')'Request size=',mtlHead%number
    GOTO 9999
  END IF

  MaterialLoop: DO i = 1,mtlHead%number

    ntypm = ntypm + 1
    material(ntypm)%cmat = mtlList(i)%name
    material(ntypm)%unit = mtlList(i)%units
    material(ntypm)%file = mtlList(i)%file
    material(ntypm)%path = mtlList(i)%path

    material(ntypm)%effClass = NOT_SET_I
    material(ntypm)%effAvail = NOT_SET_I

    IF( .FALSE. )THEN
    ELSE IF( BTEST(mtlList(i)%type,HMB_NULLSENSOR) )THEN
      class = MAT_NULL
    ELSE IF( BTEST(mtlList(i)%type,HMB_SATSENSOR) )THEN
      class = MAT_SSAT
    ELSE IF( BTEST(mtlList(i)%type,HMB_AEROSOL) )THEN
      class = MAT_AER
    ELSE IF( BTEST(mtlList(i)%type,HMB_LIQUID) )THEN
      class = MAT_LIQ
    ELSE IF( BTEST(mtlList(i)%type,HMB_WETPARTICLE) )THEN
      class = MAT_WET
    ELSE IF( BTEST(mtlList(i)%type,HMB_PARTICLE) )THEN
      class = MAT_PRT
    ELSE IF( BTEST(mtlList(i)%type,HMB_GAS) )THEN
      class = MAT_GAS
    ELSE
      nError = IV_ERROR
      eRoutine = 'UnloadMaterial'
      eMessage = 'Unable to determine material class'
      WRITE(eInform,'(A,I3)')'Class =',mtlList(i)%type
      GOTO 9999
    END IF
    material(ntypm)%ccls = class

    icls = SetClass( class )

    IF( BTEST(mtlList(i)%type,HMB_2NDEVAP) )icls = SetClassEvap( icls )

    material(ntypm)%icls = icls
    material(ntypm)%iaux = nmaux + 1
    material(ntypm)%ioffp    = mtlList(i)%puffIndex
    material(ntypm)%ioffs    = NOT_SET_I
    material(ntypm)%ioffd    = NOT_SET_I

    IF( IsAerosol(icls) )THEN                                !NB - Must check AEROSOL
      CALL UnloadMaterialAerosol( ntypm,mtlList(i)%matData ) !before GAS since aerosol
      IF( nError /= NO_ERROR )GOTO 9999                      !materials have both the GAS
    ELSE IF( IsGas(icls) )THEN                               !and AEROSOL bits set (though not here)
      CALL UnloadMaterialGas( ntypm,mtlList(i)%matData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE IF( IsParticle(icls) )THEN
      CALL UnloadMaterialParticle( ntypm,mtlList(i)%matData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE IF( IsLiquid(icls) )THEN
      CALL UnloadMaterialLiquid( ntypm,mtlList(i)%matData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE IF( IsWetParticle(icls) )THEN
      CALL UnloadMaterialParticle( ntypm,mtlList(i)%matData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      nError = IV_ERROR
      eRoutine = 'UnloadMaterial'
      eMessage = 'Invalid material class'
      WRITE(eInform,'(A,I3)')'Class =',icls
      GOTO 9999
    END IF

  END DO MaterialLoop

END IF AnyMaterials

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            LoadMaterial
!*******************************************************************************
SUBROUTINE LoadMaterial( mtlHead,mtlList )

USE list_fd
USE material_fd
USE scipuff_fi
USE SCIPresults_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( listHeadT ),               INTENT( INOUT ) :: mtlHead
TYPE( materialT ), DIMENSION(*), INTENT(   OUT ) :: mtlList

INTEGER i
INTEGER mtl

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle

LOGICAL, EXTERNAL :: IsEvap
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsNullSensor, IsSatSensor

!==== Load

!==== Initialize

mtlHead%number = 0

!==== Loop over materials

IF( ntypm > mtlHead%max )THEN
  nError = SZ_ERROR
  eRoutine = 'LoadMaterial'
  eMessage = 'Too many material types'
  WRITE(eInform,'(A,I3,A,I3)')'No. materials = ',ntypm, &
                              ' : Max. allowed = ',mtlHead%max
  GOTO 9999
END IF

mtl = 0
DO i = 1,ntypm

  mtl = mtl + 1

  mtlList(mtl)%name  = material(i)%cmat
  mtlList(mtl)%units = material(i)%unit
  mtlList(mtl)%file  = material(i)%file
  mtlList(mtl)%path  = material(i)%path

  mtlList(mtl)%type = SCIPnull

  IF( IsGas(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_GAS)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_GAS)
  END IF

  IF( IsLiquid(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_LIQUID)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_LIQUID)
  END IF

  IF( IsAerosol(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_AEROSOL)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_AEROSOL)
  END IF

  IF( IsParticle(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_PARTICLE)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_PARTICLE)
  END IF

  IF( IsWetParticle(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_WETPARTICLE)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_WETPARTICLE)
  END IF

  IF( IsEvap(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_2NDEVAP)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_2NDEVAP)
  END IF

  IF( IsMulti(material(i)%icls) .AND. LEN_TRIM(material(i)%file) > 0 &
      .AND. INDEX(material(i)%file,CHAR(0)) == 0 &
      .AND. material(i)%file /= 'intentionally bl' )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_MULTICOMP)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_MULTICOMP)
  END IF

  IF( IsNullSensor(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_NULLSENSOR)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_NULLSENSOR)
  END IF

  IF( IsSatSensor(material(i)%icls) )THEN
    mtlList(mtl)%type = IBSET(mtlList(mtl)%type,HMB_SATSENSOR)
  ELSE
    mtlList(mtl)%type = IBCLR(mtlList(mtl)%type,HMB_SATSENSOR)
  END IF

  IF( IsAerosol(material(i)%icls) )THEN                !NB - Must check AEROSOL
    CALL LoadMaterialAerosol( i,mtlList(mtl)%matData ) !     before GAS since
    IF( nError /= NO_ERROR )GOTO 9999                  !     aerosol materials
  ELSE IF( IsGas(material(i)%icls) )THEN               !     have both the GAS
    CALL LoadMaterialGas( i,mtlList(mtl)%matData )     !     and AEROSOL bits
    IF( nError /= NO_ERROR )GOTO 9999 !     set
  ELSE IF( IsLiquid(material(i)%icls) )THEN
    CALL LoadMaterialLiquid( i,mtlList(mtl)%matData )
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE IF( IsParticle(material(i)%icls )  &
            .OR. IsWetParticle(material(i)%icls) )THEN
    CALL LoadMaterialParticle( i,mtlList(mtl)%matData )
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    nError = IV_ERROR
    eRoutine = 'LoadMaterial'
    eMessage = 'Invalid material class'
    WRITE(eInform,'(A,I3)')'Class =',material(i)%icls
    GOTO 9999
  END IF

  mtlList(mtl)%puffIndex = material(i)%ioffp

END DO

mtlHead%number = mtl

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadMaterialGas
!*******************************************************************************
SUBROUTINE UnloadMaterialGas( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE UtilMtlAux
USE reallocate

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: mtl
TYPE( matGenT ), INTENT( IN ) :: mtlDataIn

TYPE( matGasT )      mtlData
TYPE( gas_material ) pmatgas

INTEGER alloc_stat, aux_sz

!==== Unload

mtlData = TRANSFER(mtlDataIn,mtlData)

!==== Properties

material(mtl)%prop(1) = mtlData%decayAmp
material(mtl)%prop(2) = mtlData%decayMin
material(mtl)%prop(3) = mtlData%minConcentration
material(mtl)%prop(4) = NOT_SET_R

!==== Surface flags

material(mtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP) .OR. BTEST(mtlData%save,HSB_TOTALDEP)
material(mtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS) .OR. BTEST(mtlData%save,HSB_TOTALDOS)
material(mtl)%lsrft = .FALSE.
material(mtl)%ldost = .FALSE.

!==== Auxiliary data

IF( ASSOCIATED(mat_aux) )THEN
  aux_sz = SIZE(mat_aux)
  IF( nmaux+MAXGMAUX > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,nmaux+MAXGMAUX-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'UnloadMaterialGas'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF
ELSE
  ALLOCATE( mat_aux(nmaux+MAXGMAUX),STAT=alloc_stat )
END IF

pmatgas%rho = mtlData%gasDensity
pmatgas%vd  = mtlData%gasDeposition

CALL PutGasParam( pmatgas,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

nmaux = nmaux + MAXGMAUX

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadMaterialLiquid
!*******************************************************************************
SUBROUTINE UnloadMaterialLiquid( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE UtilMtlAux
USE reallocate

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: mtl
TYPE( matGenT ), INTENT( IN ) :: mtlDataIn

TYPE( matLiquidT )      mtlData
TYPE( liquid_material ) pmatliq
TYPE( gas_material    ) pmatgas

INTEGER iaux
INTEGER nsg
INTEGER i
INTEGER alloc_stat, aux_sz

LOGICAL, EXTERNAL :: IsEvap

!==== Unload

mtlData = TRANSFER(mtlDataIn,mtlData)

!==== Properties

material(mtl)%prop(1) = mtlData%decayAmp
material(mtl)%prop(2) = mtlData%decayMin
material(mtl)%prop(3) = mtlData%minConcentration
material(mtl)%prop(4) = NOT_SET_R

!==== Surface flags

material(mtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
material(mtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
material(mtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
material(mtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

nsg  = mtlData%nSizeBins
iaux = MAXLMAUXX+MAXLMAUX*nsg+MAXLMAUXP+MAXGMAUX

IF( ASSOCIATED(mat_aux) )THEN
  aux_sz = SIZE(mat_aux)
  IF( nmaux+iaux > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,nmaux+iaux-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'UnloadMaterialGas'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF
ELSE
  ALLOCATE( mat_aux(nmaux+iaux),STAT=alloc_stat )
END IF

pmatgas%rho = mtlData%gasDensity
pmatgas%vd  = mtlData%gasDeposition

CALL PutGasParam( pmatgas,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

pmatliq%nsg  = nsg
pmatliq%rho  = mtlData%liquidDensity(1)
DO i = 1,MAXLMAUXP-10
  pmatliq%dum(i) = NOT_SET_R
END DO
pmatliq%sigvd = 0.0
pmatliq%diff  = 0.0
pmatliq%rhob  = mtlData%liquidDensity(2)
pmatliq%a     = mtlData%antoine(1)
pmatliq%b     = mtlData%antoine(2)
pmatliq%c     = mtlData%antoine(3)
pmatliq%w     = mtlData%molWeight
pmatliq%st    = mtlData%srfTension
IF( IsEvap(material(mtl)%icls) )THEN
  pmatliq%sf = ABS(mtlData%spreadFactor)
ELSE
  pmatliq%sf = -ABS(mtlData%spreadFactor)
END IF
pmatliq%viscosity = mtlData%viscosity

pmatliq%cpL = mtlData%liqSpecificHeat
pmatliq%cpV = mtlData%gasSpecificHeat

DO i = 1,nsg
  pmatliq%dmin = mtlData%binBounds(i)
  pmatliq%dbar = mtlData%binSize(i)
  pmatliq%dmax = mtlData%binBounds(i+1)
  CALL PutLiquidParam( pmatliq,material(mtl)%iaux,i+1,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

nmaux = nmaux + iaux

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadMaterialParticle
!*******************************************************************************
SUBROUTINE UnloadMaterialParticle( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE UtilMtlAux
USE reallocate

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: mtl
TYPE( matGenT ), INTENT( IN ) :: mtlDataIn

TYPE( matParticleT )  mtlData
TYPE( part_material ) pmatpart

INTEGER iaux
INTEGER nsg
INTEGER i
INTEGER alloc_stat, aux_sz

!==== Unload

mtlData = TRANSFER(mtlDataIn,mtlData)

!==== Properties

material(mtl)%prop(1) = mtlData%decayAmp
material(mtl)%prop(2) = mtlData%decayMin
material(mtl)%prop(3) = mtlData%minConcentration
material(mtl)%prop(4) = NOT_SET_R

!==== Surface flags

material(mtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
material(mtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
material(mtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
material(mtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

nsg  = mtlData%nSizeBins
iaux = MAXPMAUXX+MAXPMAUX*nsg

IF( ASSOCIATED(mat_aux) )THEN
  aux_sz = SIZE(mat_aux)
  IF( nmaux+iaux > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,nmaux+iaux-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'UnloadMaterialParticle'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF
ELSE
  ALLOCATE( mat_aux(nmaux+iaux),STAT=alloc_stat )
END IF

pmatpart%nsg   = nsg
pmatpart%rho   = mtlData%density
pmatpart%vd    = 0.0
pmatpart%sigvd = 0.0
pmatpart%diff  = 0.0

DO i = 1,nsg
  pmatpart%dmin = mtlData%binBounds(i)
  pmatpart%dbar = mtlData%binSize(i)
  pmatpart%dmax = mtlData%binBounds(i+1)
  CALL PutParticleParam( pmatpart,material(mtl)%iaux,i,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

nmaux = nmaux + iaux

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadMaterialAerosol
!*******************************************************************************
SUBROUTINE UnloadMaterialAerosol( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE UtilMtlAux
USE reallocate

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: mtl
TYPE( matGenT ), INTENT( IN ) :: mtlDataIn

TYPE( matAerosolT )      mtlData
TYPE( aerosol_material ) pmataer

INTEGER i
INTEGER alloc_stat, aux_sz

!==== Unload

mtlData = TRANSFER(mtlDataIn,mtlData)

!==== Properties

material(mtl)%prop(1) = mtlData%decayAmp
material(mtl)%prop(2) = mtlData%decayMin
material(mtl)%prop(3) = mtlData%minConcentration
material(mtl)%prop(4) = NOT_SET_R

!==== Surface flags

material(mtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
material(mtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
material(mtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
material(mtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

IF( ASSOCIATED(mat_aux) )THEN
  aux_sz = SIZE(mat_aux)
  IF( nmaux+MAXAMAUXP > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,nmaux+MAXAMAUXP-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'UnloadMaterialAerosol'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF
ELSE
  ALLOCATE( mat_aux(nmaux+MAXAMAUXP),STAT=alloc_stat )
END IF

DO i = 1,MAXAMAUXP-8
  pmataer%dum(i) = NOT_SET_R
END DO
pmataer%vd  = mtlData%gasDeposition
pmataer%rho = mtlData%liquidDensity
pmataer%a   = mtlData%antoine(1)
pmataer%b   = mtlData%antoine(2)
pmataer%c   = mtlData%antoine(3)
pmataer%w   = mtlData%molWeight
pmataer%cpL = mtlData%liqSpecificHeat
pmataer%cpV = mtlData%gasSpecificHeat

CALL PutAerosolParam( pmataer,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

nmaux = nmaux + MAXAMAUXP

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        LoadMaterialGas
!*******************************************************************************
SUBROUTINE LoadMaterialGas( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE SCIPresults_fd
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER,         INTENT( IN  ) :: mtl
TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matGasT )      mtlData
TYPE( gas_material ) pmatgas

!==== Load

!==== Properties

mtlData%decayAmp         = material(mtl)%prop(1)
mtlData%decayMin         = material(mtl)%prop(2)
mtlData%minConcentration = material(mtl)%prop(3)
mtlData%rNotUsed         = NOT_SET_R

!==== Surface flags

mtlData%save = SCIPnull

IF( material(mtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF

IF( material(mtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF

mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

CALL GetGasParam( pmatgas,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999
mtlData%gasDensity    = pmatgas%rho
mtlData%gasDeposition = pmatgas%vd
mtlData%padding       = NOT_SET_I

9999 CONTINUE

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

RETURN
END
!*******************************************************************************
!        LoadMaterialLiquid
!*******************************************************************************
SUBROUTINE LoadMaterialLiquid( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE UtilMtlAux
USE SCIPresults_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER,         INTENT( IN  ) :: mtl
TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matLiquidT )      mtlData
TYPE( gas_material    ) pmatgas
TYPE( liquid_material ) pmatliq

INTEGER i

!==== Load

!==== Properties

mtlData%decayAmp         = material(mtl)%prop(1)
mtlData%decayMin         = material(mtl)%prop(2)
mtlData%minConcentration = material(mtl)%prop(3)
mtlData%rNotUsed         = NOT_SET_R

!==== Surface flags

mtlData%save = SCIPnull

IF( material(mtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF

IF( material(mtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF

IF( material(mtl)%lsrft )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
END IF

IF( material(mtl)%ldost )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)
END IF

!==== Auxiliary data

CALL GetGasParam( pmatgas,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999
mtlData%gasDensity    = pmatgas%rho
mtlData%gasDeposition = pmatgas%vd

CALL GetLiquidParam( pmatliq,material(mtl)%iaux,2,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

mtlData%nSizeBins        = pmatliq%nsg
mtlData%liquidDensity(1) = pmatliq%rho
mtlData%liquidDensity(2) = pmatliq%rhob
mtlData%antoine(1)       = pmatliq%a
mtlData%antoine(2)       = pmatliq%b
mtlData%antoine(3)       = pmatliq%c
mtlData%molWeight        = pmatliq%w
mtlData%liqSpecificHeat  = pmatliq%cpL
mtlData%gasSpecificHeat  = pmatliq%cpV
mtlData%srfTension       = pmatliq%st
mtlData%spreadFactor     = ABS(pmatliq%sf)
mtlData%viscosity        = pmatliq%viscosity

IF( mtlData%nSizeBins <= HS_MAXMTLBINSIZE )THEN
  mtlData%binBounds(1) = pmatliq%dmin
  mtlData%binSize(1)   = pmatliq%dbar
  DO i = 2,mtlData%nSizeBins
    CALL GetLiquidParam( pmatliq,material(mtl)%iaux,i+1,mat_aux )
    IF( nError /= NO_ERROR )GOTO 9999
    mtlData%binBounds(i) = pmatliq%dmin
    mtlData%binSize(i)   = pmatliq%dbar
  END DO
  mtlData%binBounds(mtlData%nSizeBins+1) = pmatliq%dmax
  IF( mtlData%nSizeBins < HS_MAXMTLBINSIZE )THEN
    DO i = mtlData%nSizeBins+1,HS_MAXMTLBINSIZE
      mtlData%binBounds(i+1) = NOT_SET_R
      mtlData%binSize(i)     = NOT_SET_R
    END DO
  END IF
ELSE
  nError = SZ_ERROR
  eRoutine = 'LoadMaterialLiquid'
  eMessage = 'Too many droplet size bins'
  WRITE(eInform,'(A,I4,A,I4)')'No. size bins =',mtlData%nSizeBins, &
                              ' : Max. allowed =',HS_MAXMTLBINSIZE
END IF

9999 CONTINUE

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

RETURN
END
!*******************************************************************************
!        LoadMaterialParticle
!*******************************************************************************
SUBROUTINE LoadMaterialParticle( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE SCIPresults_fd
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER,         INTENT( IN  ) :: mtl
TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matParticleT )  mtlData
TYPE( part_material ) pmatpart

INTEGER i

!==== Load

!==== Properties

mtlData%decayAmp         = material(mtl)%prop(1)
mtlData%decayMin         = material(mtl)%prop(2)
mtlData%minConcentration = material(mtl)%prop(3)
mtlData%rNotUsed         = NOT_SET_R

!==== Surface flags

mtlData%save = SCIPnull

IF( material(mtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF

IF( material(mtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF

IF( material(mtl)%lsrft )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
END IF

IF( material(mtl)%ldost )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)
END IF

!==== Auxiliary data

CALL GetParticleParam( pmatpart,material(mtl)%iaux,1,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

mtlData%nSizeBins = pmatpart%nsg
mtlData%density   = pmatpart%rho

IF( mtlData%nSizeBins <= HS_MAXMTLBINSIZE )THEN
  mtlData%binBounds(1) = pmatpart%dmin
  mtlData%binSize(1)   = pmatpart%dbar
  DO i = 2,mtlData%nSizeBins
    CALL GetParticleParam( pmatpart,material(mtl)%iaux,i,mat_aux )
    IF( nError /= NO_ERROR )GOTO 9999
    mtlData%binBounds(i) = pmatpart%dmin
    mtlData%binSize(i)   = pmatpart%dbar
  END DO
  mtlData%binBounds(mtlData%nSizeBins+1) = pmatpart%dmax
  IF( mtlData%nSizeBins < HS_MAXMTLBINSIZE )THEN
    DO i = mtlData%nSizeBins+1,HS_MAXMTLBINSIZE
      mtlData%binBounds(i+1) = NOT_SET_R
      mtlData%binSize(i)     = NOT_SET_R
    END DO
  END IF
ELSE
  nError = SZ_ERROR
  eRoutine = 'LoadMaterialParticle'
  eMessage = 'Too many particle size bins'
  WRITE(eInform,'(A,I4,A,I4)')'No. size bins =',mtlData%nSizeBins, &
                              ' : Max. allowed =',HS_MAXMTLBINSIZE
END IF

mtlData%padding = NOT_SET_I

9999 CONTINUE

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

RETURN
END
!*******************************************************************************
!        LoadMaterialAerosol
!*******************************************************************************
SUBROUTINE LoadMaterialAerosol( mtl,mtlDataIn )

USE material_fd
USE scipuff_fi
USE SCIPresults_fd
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER,         INTENT( IN  ) :: mtl
TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matAerosolT )      mtlData
TYPE( aerosol_material ) pmataer

!==== Load

!==== Properties

mtlData%decayAmp         = material(mtl)%prop(1)
mtlData%decayMin         = material(mtl)%prop(2)
mtlData%minConcentration = material(mtl)%prop(3)
mtlData%rNotUsed         = NOT_SET_R

!==== Surface flags

mtlData%save = SCIPnull

IF( material(mtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF

IF( material(mtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF

mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

CALL GetAerosolParam( pmataer,material(mtl)%iaux,mat_aux )
IF( nError /= NO_ERROR )GOTO 9999

mtlData%gasDeposition   = pmataer%vd
mtlData%liquidDensity   = pmataer%rho
mtlData%antoine(1)      = pmataer%a
mtlData%antoine(2)      = pmataer%b
mtlData%antoine(3)      = pmataer%c
mtlData%molWeight       = pmataer%w
mtlData%liqSpecificHeat = pmataer%cpL
mtlData%gasSpecificHeat = pmataer%cpV

mtlData%padding = NOT_SET_I

9999 CONTINUE

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

RETURN
END
