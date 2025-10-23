!===============================================================================
!     InitMatdef
!===============================================================================
SUBROUTINE InitMatdef( psize,pbounds )

USE default_fd
USE class_fd
USE matdef_fi

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( OUT ) :: psize
REAL, DIMENSION(*), INTENT( OUT ) :: pbounds

INTEGER i

!==== Integers

nsg = 1

!==== Logicals

group_dose       = .TRUE.
total_dose       = .TRUE.
group_deposition = .TRUE.
total_deposition = .TRUE.
multi_comp       = .FALSE.

!==== Reals

antoine(1)        = NOT_SET_R
antoine(2)        = 0.
antoine(3)        = 0.
conc_min          = NOT_SET_R
decay_amp         = 0.
decay_min         = 0.
density           = 1.2
evap_min          = NOT_SET_R
gas_deposition    = 0.
liquid_density(1) = NOT_SET_R
liquid_density(2) = 0.
mweight           = NOT_SET_R
specific_heat_liq = NOT_SET_R
specific_heat_vap = NOT_SET_R
spread_factor     = NOT_SET_R
surf_tension      = NOT_SET_R
viscosity         = NOT_SET_R

DO i = 1,MAXSGP
  binSize(i)   = 0.
  binBounds(i) = 0.
  psize(i)     = 0.
  pbounds(i)   = 0.
END DO
binBounds(MAXSGP+1) = 0.
pbounds(MAXSGP+1)   = 0.

!==== Character strings

class     = MAT_GAS
mname     = ' '
units     = ' '
file_name = ' '
file_path = ' '

RETURN
END
!===============================================================================
!     PutMaterial
!===============================================================================
SUBROUTINE PutMaterial()

USE default_fd
USE struct_fd
USE error_fi
USE matl_fi
USE matdef_fi
USE UtilMtlAux
USE reallocate
USE class_fd

IMPLICIT NONE

TYPE( gas_material      ) pmatgas
TYPE( liquid_material   ) pmatliq
TYPE( aerosol_material  ) pmataer
TYPE( part_material     ) pmatpart

INTEGER imtl
INTEGER iaux
INTEGER maux

INTEGER i, aux_sz, alloc_stat
INTEGER icls

INTEGER, EXTERNAL :: SetClass
LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
INTEGER, EXTERNAL :: SetClassEvap
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
INTEGER, EXTERNAL :: AddClass

!==== Some preliminary error checking

IF( mname == ' ' )THEN
  nError = IV_ERROR
  eRoutine = 'PutMaterial'
  eMessage = 'A material name must be specified as part of the material definition'
  GOTO 9999
END IF

DO i = 1,ntypm
  IF( TRIM(mname) == TRIM(material(i)%cmat) )THEN
    nError = NF_ERROR
    eRoutine = 'PutMaterial'
    eMessage = 'Duplicate material name encountered'
    GOTO 9999
  END IF
END DO

!==== initialize

imtl = ntypm + 1

IF( ALLOCATED( material ) )THEN
  IF( imtl > SIZE(material) )THEN
    CALL realloc_matl( ntypm,1 )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
ELSE
  ALLOCATE( material(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'PutMaterial'
    eMessage = 'Error reallocating material array'
    GOTO 9999
  END IF
END IF

iaux = nmaux

IF( ASSOCIATED( mat_aux ) )THEN
  aux_sz = SIZE(mat_aux)
  IF( iaux+1 > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,iaux+1-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF
ELSE
  ALLOCATE( mat_aux(iaux+1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'PutMaterial'
    eMessage = 'Error allocating mat auxiliary array'
    GOTO 9999
  END IF
END IF

icls = SetClass( class )

IF( icls == 0 )THEN
  nError = UK_ERROR
  eRoutine = 'PutMaterial'
  eMessage = 'Unrecognized material class'
  eInform  = 'Class ='//TRIM(class)
  GOTO 9999
END IF

!==== Material pointers

material(imtl)%icls     = icls
material(imtl)%iaux     = iaux + 1
material(imtl)%mcID     = 0
material(imtl)%nmc      = 0
material(imtl)%AuxMatID = NOT_SET_I
material(imtl)%jNotUsed = NOT_SET_I
material(imtl)%lNotUsed = .FALSE.
material(imtl)%ioffp    = NOT_SET_I
material(imtl)%ioffs    = NOT_SET_I
material(imtl)%ioffd    = NOT_SET_I

material(imtl)%effClass = NOT_SET_I
material(imtl)%effAvail = NOT_SET_I

CALL CheckSubgroups( nsg,material(imtl) )
IF( nError /= NO_ERROR )GOTO 9999

!==== Material properties

material(imtl)%prop = NOT_SET_R

material(imtl)%prop(1) = decay_amp
material(imtl)%prop(2) = decay_min
material(imtl)%prop(3) = conc_min
material(imtl)%prop(4) = NOT_SET_R

!==== Material class dependent error checking

IF( IsLiquid(icls) )THEN

  CALL check_liquid_params( icls )
  IF( nError /= NO_ERROR )THEN
    eAction = 'Check material definition'
    GOTO 9999
  END IF

  IF( spread_factor > 0.0 )THEN
    IF( .NOT.group_deposition )THEN
      nError   = WN_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error specifying secondary evaporation for ' //TRIM(mname)
      eInform  = 'Group deposition must be enabled'
      eAction  = 'Do you want to enable it and continue?'
      CALL WarningMessage( .TRUE. )
      IF( nError /= NO_ERROR )THEN
        eAction = 'User cancelled read of material definition'
        GOTO 9999
      END IF
      group_deposition = .TRUE.
    END IF
    material(imtl)%icls = SetClassEvap( material(imtl)%icls )
  END IF

END IF

aux_sz = SIZE(mat_aux)

!==== Material class dependent data

IF( IsAerosol(icls) )THEN

  CALL check_liquid_params( icls )
  IF( nError /= NO_ERROR )THEN
    eAction = 'Check material definition'
    GOTO 9999
  END IF

  IF( iaux+MAXAMAUXP > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,iaux+MAXAMAUXP-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF

  DO i = 1,MAXAMAUXP-8
    pmataer%dum(i) = NOT_SET_R
  END DO
  pmataer%vd  = gas_deposition
  pmataer%rho = liquid_density(1)
  pmataer%a   = antoine(1)
  pmataer%b   = antoine(2)
  pmataer%c   = antoine(3)
  pmataer%w   = mweight
  pmataer%cpL = specific_heat_liq
  pmataer%cpV = specific_heat_vap

  CALL PutAerosolParam( pmataer,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  iaux = iaux + MAXAMAUXP

ELSE IF( IsGas(icls) )THEN

  IF( iaux+MAXGMAUX > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,iaux+MAXGMAUX-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF

  pmatgas%rho = density
  pmatgas%vd  = gas_deposition
  CALL PutGasParam( pmatgas,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999
  iaux = iaux + MAXGMAUX

ELSE IF( IsLiquid(icls) )THEN

  maux = MAXLMAUXX+MAXLMAUX*nsg+MAXLMAUXP+MAXGMAUX
  IF( iaux+maux > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,iaux+maux-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF

  pmatgas%rho = density
  pmatgas%vd  = gas_deposition
  CALL PutGasParam( pmatgas,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  pmatliq%nsg  = nsg
  pmatliq%rho  = liquid_density(1)
  pmatliq%diff = 0.0
  DO i = 1,MAXLMAUXP-10
    pmatliq%dum(i) = NOT_SET_R
  END DO
  pmatliq%rhob      = liquid_density(2)
  pmatliq%a         = antoine(1)
  pmatliq%b         = antoine(2)
  pmatliq%c         = antoine(3)
  pmatliq%w         = mweight
  pmatliq%st        = surf_tension
  pmatliq%sf        = spread_factor
  pmatliq%viscosity = viscosity
  pmatliq%cpL       = specific_heat_liq
  pmatliq%cpV       = specific_heat_vap
  pmatliq%sigvd     = 0.0
  pmatliq%diff      = 0.0

  binBounds(1) = 0.0
  DO i = 1,nsg
    pmatliq%dmin = binBounds(i)
    pmatliq%dbar = binSize(i)
    pmatliq%dmax = binBounds(i+1)
    CALL PutLiquidParam( pmatliq,material(imtl)%iaux,i+1,mat_aux )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

  iaux = iaux + maux

  nsg = 2

ELSE IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN

  maux = MAXPMAUXX+MAXPMAUX*nsg
  IF( iaux+maux > aux_sz )THEN
    alloc_stat = reallocate_real1d( mat_aux,iaux+maux-aux_sz )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PutMaterial'
      eMessage = 'Error reallocating mat auxiliary array'
      GOTO 9999
    END IF
  END IF

  pmatpart%nsg   = nsg
  pmatpart%rho   = density
  pmatpart%vd    = 0.0
  pmatpart%sigvd = 0.0
  pmatpart%diff  = 0.0

  DO i = 1,nsg
    pmatpart%dmin = binBounds(i)
    pmatpart%dbar = binSize(i)
    pmatpart%dmax = binBounds(i+1)
    CALL PutParticleParam( pmatpart,material(imtl)%iaux,i,mat_aux )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

  iaux = iaux + maux

ELSE

  nError = IV_ERROR
  eRoutine = 'PutMaterial'
  eMessage = 'Invalid material class'
  WRITE(eInform,'(A,I3)')'Class =',material(imtl)%icls
  GOTO 9999

END IF

!==== Logicals

IF( group_deposition .OR. total_deposition )THEN
  material(imtl)%lsrfg = group_deposition .OR. &
                          (total_deposition .AND. (nsg==1) )
  material(imtl)%lsrft = total_deposition .AND. (nsg>1)
ELSE
  material(imtl)%lsrfg = .FALSE.
  material(imtl)%lsrft = .FALSE.
END IF
IF( group_dose .OR. total_dose )THEN
  material(imtl)%ldosg = group_dose .OR. &
                         (total_dose .AND. (nsg==1) )
  material(imtl)%ldost = total_dose .AND. (nsg>1)
ELSE
  material(imtl)%ldosg = .FALSE.
  material(imtl)%ldost = .FALSE.
END IF

!==== Character strings

material(imtl)%cmat = mname
material(imtl)%unit = units
material(imtl)%file = ' '
material(imtl)%path = ' '
IF( LEN_TRIM(file_name) > 0  .AND. file_name /= 'intentionally bl' &
      .AND. INDEX(file_name,CHAR(0)) == 0 )THEN
  material(imtl)%file = file_name
  material(imtl)%path = file_path
  IF( IsGas(icls) )material(imtl)%icls = AddClass( material(imtl)%icls,MATID_MULTI )
END IF
material(imtl)%ccls = class

ntypm = imtl
nmaux = iaux

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE check_liquid_params( icls )

USE scipuff_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icls

REAL TB1100, TB10

LOGICAL, EXTERNAL :: IsLiquid

nError   = IV_ERROR
eRoutine = 'PutMaterial'

IF( liquid_density(1) == NOT_SET_R )THEN
  eMessage = 'Must specify liquid density'
  GOTO 9999
END IF
IF( antoine(1) == NOT_SET_R )THEN
  eMessage = 'Must specify liquid vapor pressure'
  GOTO 9999
END IF
IF( antoine(2) == NOT_SET_R )THEN
  antoine(2)  = 0.0
END IF
IF( antoine(3) == NOT_SET_R )THEN
  antoine(3)  = 0.0
END IF
IF( mweight == NOT_SET_R )THEN
  eMessage = 'Must specify liquid molecular weight'
  GOTO 9999
END IF

!==== Check Antoine coefficients for "reasonableness". Make sure Boiling point (Deg K) at
!     1100mb and 10mb are positive 0.7500616 = mb to mmHg

TB1100 = antoine(2)/(antoine(1) - LOG10(1100*0.7500616)) - antoine(3) - ABSZERO
TB10   = antoine(2)/(antoine(1) - LOG10(  10*0.7500616)) - antoine(3) - ABSZERO
IF( TB1100 < 0.0 .OR. TB10 < 0.0 )THEN
  eMessage = 'Antoine coefficents generate negative boiling points (Kelvin)'
  GOTO 9999
END IF

IF( IsLiquid(icls) )THEN
  IF( liquid_density(2) == NOT_SET_R )THEN
    liquid_density(2)  = 0.0
  END IF
  IF( surf_tension == NOT_SET_R )THEN
    eMessage = 'Must specify liquid surface tension'
    GOTO 9999
  END IF
END IF

CALL init_error()

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE realloc_matl( n,inc )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n, inc

TYPE( material_str ), DIMENSION(:), ALLOCATABLE :: tem_mat

INTEGER alloc_stat

ALLOCATE( tem_mat(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'realloc_matl'
  eMessage = 'Error allocating temporary array'
  GOTO 9999
END IF
tem_mat = material; DEALLOCATE( material,STAT=alloc_stat )

ALLOCATE( material(n+inc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'realloc_matl'
  eMessage = 'Error allocating temporary array'
  GOTO 9999
END IF
material(1:n) = tem_mat; DEALLOCATE( tem_mat,STAT=alloc_stat )

9999 CONTINUE

IF( ALLOCATED(tem_mat) )DEALLOCATE( tem_mat,STAT=alloc_stat )

RETURN
END
!===============================================================================
!     GetMaterial
!===============================================================================
SUBROUTINE GetMaterial( imtl )

USE scipuff_fi
USE matdef_fi
USE UtilMtlAux

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imtl

TYPE( gas_material      ) pmatgas
TYPE( liquid_material   ) pmatliq
TYPE( aerosol_material  ) pmataer
TYPE( part_material ) pmatpart

INTEGER i
LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

!==== Initialize

CALL InitMatdef( binSize,binBounds )

!==== Character strings

mname     = material(imtl)%cmat
units     = material(imtl)%unit
file_name = material(imtl)%file
file_path = material(imtl)%path
class     = material(imtl)%ccls

!==== Logicals

group_deposition = material(imtl)%lsrfg
group_dose       = material(imtl)%ldosg
total_deposition = material(imtl)%lsrft
total_dose       = material(imtl)%ldost

!==== Material properties

decay_amp  = material(imtl)%prop(1)
decay_min  = material(imtl)%prop(2)
conc_min   = material(imtl)%prop(3)

!==== Material class dependent data

IF( IsAerosol(material(imtl)%icls) )THEN

  CALL GetAerosolParam( pmataer,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  gas_deposition    = pmataer%vd
  liquid_density(1) = pmataer%rho
  liquid_density(2) = NOT_SET_R
  antoine(1)        = pmataer%a
  antoine(2)        = pmataer%b
  antoine(3)        = pmataer%c
  mweight           = pmataer%w
  specific_heat_liq = pmataer%cpL
  specific_heat_vap = pmataer%cpV

ELSE IF( IsGas(material(imtl)%icls) )THEN

  CALL GetGasParam( pmatgas,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  gas_deposition = pmatgas%vd
  density        = pmatgas%rho

ELSE IF( IsLiquid(material(imtl)%icls) )THEN

  CALL GetGasParam( pmatgas,material(imtl)%iaux,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  gas_deposition = pmatgas%vd
  density        = pmatgas%rho

  CALL GetLiquidParam( pmatliq,material(imtl)%iaux,2,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  nsg               = pmatliq%nsg
  liquid_density(1) = pmatliq%rho
  liquid_density(2) = pmatliq%rhob
  antoine(1)        = pmatliq%a
  antoine(2)        = pmatliq%b
  antoine(3)        = pmatliq%c
  mweight           = pmatliq%w
  surf_tension      = pmatliq%st
  spread_factor     = pmatliq%sf
  viscosity         = pmatliq%viscosity
  specific_heat_liq = pmatliq%cpL
  specific_heat_vap = pmatliq%cpV

  binBounds(1) = pmatliq%dmin
  binSize(1)   = pmatliq%dbar
  DO i = 2,nsg
    CALL GetLiquidParam( pmatliq,material(imtl)%iaux,i+1,mat_aux )
    IF( nError /= NO_ERROR )GOTO 9999
    binBounds(i) = pmatliq%dmin
    binSize(i)   = pmatliq%dbar
  END DO
  binBounds(nsg+1) = pmatliq%dmax

ELSE IF( IsParticle(material(imtl)%icls) &
          .OR. IsWetParticle(material(imtl)%icls) )THEN

  CALL GetParticleParam( pmatpart,material(imtl)%iaux,1,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999

  nsg     = pmatpart%nsg
  density = pmatpart%rho

  binBounds(1) = pmatpart%dmin
  binSize(1)   = pmatpart%dbar
  DO i = 2,nsg
    CALL GetParticleParam(pmatpart,material(imtl)%iaux,i,mat_aux)
    IF( nError /= NO_ERROR )GOTO 9999
    binBounds(i) = pmatpart%dmin
    binSize(i)   = pmatpart%dbar
  END DO
  binBounds(nsg+1) = pmatpart%dmax

ELSE

  nError   = IV_ERROR
  eRoutine = 'GetMaterial'
  eMessage = 'Invalid material class'
  WRITE(eInform,'(A,I3)')'Class =',material(imtl)%icls
  GOTO 9999

END IF

9999 CONTINUE

RETURN
END
