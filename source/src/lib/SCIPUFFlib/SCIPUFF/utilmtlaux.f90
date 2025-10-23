MODULE UtilMtlAux

IMPLICIT NONE

CONTAINS

!===============================================================================
!     GetSubgroups
!===============================================================================
INTEGER FUNCTION GetSubgroups( mat,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( material_str ), INTENT( INOUT ) :: mat
REAL, DIMENSION(:),   POINTER         :: mataux

TYPE( liquid_material ) pmatliq
TYPE( part_material ) pmatpart

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( IsGas(mat%icls) )THEN

  GetSubgroups = 0

ELSE IF( IsParticle(mat%icls) )THEN

  CALL GetParticleParam( pmatpart,mat%iaux,1,mataux )
  GetSubgroups = pmatpart%nsg

ELSE IF( IsWetParticle(mat%icls) )THEN

  CALL GetParticleParam( pmatpart,mat%iaux,1,mataux )
  GetSubgroups = pmatpart%nsg

ELSE IF( IsLiquid(mat%icls) )THEN

  CALL GetLiquidParam( pmatliq,mat%iaux,2,mataux )
  GetSubgroups = pmatliq%nsg

ELSE IF( IsAerosol(mat%icls) )THEN

  GetSubgroups = 0

END IF

RETURN

END FUNCTION GetSubgroups
!===============================================================================
!     GetAerosolParam
!===============================================================================
SUBROUTINE GetAerosolParam( pmatl,i0,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( aerosol_material ), INTENT( OUT ) :: pmatl
INTEGER,                  INTENT( IN  ) :: i0
REAL, DIMENSION(:),       POINTER       :: mataux

pmatl = TRANSFER(mataux(i0:i0+MAXAMAUXP-1),pmatl)

RETURN
END SUBROUTINE GetAerosolParam
!===============================================================================
!     GetGasParam
!===============================================================================
SUBROUTINE GetGasParam( pmatl,i0,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( gas_material ), INTENT( OUT ) :: pmatl
INTEGER,              INTENT( IN  ) :: i0
REAL, DIMENSION(:),   POINTER       :: mataux

pmatl%rho = mataux(i0)
pmatl%vd  = mataux(i0+1)

RETURN
END SUBROUTINE GetGasParam
!===============================================================================
!     GetParticleParam
!===============================================================================
SUBROUTINE GetParticleParam( pmatl,i0,igrp,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( part_material ), INTENT( OUT ) :: pmatl
INTEGER,               INTENT( IN  ) :: i0, igrp
REAL, DIMENSION(:),    POINTER       :: mataux

INTEGER iaux

iaux = i0

pmatl%nsg = NINT(mataux(iaux))

iaux = iaux + 1
pmatl%rho = mataux(iaux)

iaux = iaux + (igrp-1)*MAXPMAUX

pmatl%dmin  = mataux(iaux+1)
pmatl%dbar  = mataux(iaux+2)
pmatl%vd    = mataux(iaux+3)
pmatl%sigvd = mataux(iaux+4)
pmatl%diff  = mataux(iaux+5)
pmatl%dmax  = mataux(iaux+6)

RETURN
END SUBROUTINE GetParticleParam
!===============================================================================
!     GetLiquidParam
!===============================================================================
SUBROUTINE GetLiquidParam( pmatl,i0,j0,mataux )

USE struct_fd
USE default_fd

IMPLICIT NONE

TYPE( liquid_material ), INTENT( OUT ) :: pmatl
INTEGER,                 INTENT( IN  ) :: i0, j0
REAL, DIMENSION(:),      POINTER       :: mataux

INTEGER iaux, igrp

iaux = i0 + MAXGMAUX - 1
igrp = j0 - 1

pmatl%rhob      = mataux(iaux+1)
pmatl%a         = mataux(iaux+2)
pmatl%b         = mataux(iaux+3)
pmatl%c         = mataux(iaux+4)
pmatl%w         = mataux(iaux+5)
pmatl%st        = mataux(iaux+6)
pmatl%sf        = mataux(iaux+7)
pmatl%viscosity = mataux(iaux+8)
pmatl%cpL       = mataux(iaux+9)
pmatl%cpV       = mataux(iaux+10)

iaux = iaux + MAXLMAUXP + 1
pmatl%nsg = NINT(mataux(iaux))

iaux = iaux + 1
pmatl%rho = mataux(iaux)

iaux = iaux + (MIN(igrp,pmatl%nsg)-1)*MAXLMAUX

pmatl%dmin  = mataux(iaux+1)
pmatl%dbar  = mataux(iaux+2)
pmatl%sigvd = mataux(iaux+3)
pmatl%diff  = mataux(iaux+4)
pmatl%dmax  = mataux(iaux+5)

pmatl%dum = NOT_SET_R

RETURN
END SUBROUTINE GetLiquidParam
!===============================================================================
!     PutAerosolParam
!===============================================================================
SUBROUTINE PutAerosolParam( pmatl,i0,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( aerosol_material ), INTENT( IN ) :: pmatl
INTEGER,                  INTENT( IN ) :: i0
REAL, DIMENSION(:),       POINTER      :: mataux

mataux(i0:i0+MAXAMAUXP-1) = TRANSFER(pmatl,mataux)

RETURN
END SUBROUTINE PutAerosolParam
!===============================================================================
!     PutGasParam
!===============================================================================
SUBROUTINE PutGasParam( pmatl,i0,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( gas_material ), INTENT( IN ) :: pmatl
INTEGER,              INTENT( IN ) :: i0
REAL, DIMENSION(:),   POINTER      :: mataux

mataux(i0)   = pmatl%rho
mataux(i0+1) = pmatl%vd

RETURN
END SUBROUTINE PutGasParam
!===============================================================================
!     PutParticleParam
!===============================================================================
SUBROUTINE PutParticleParam( pmatl,i0,igrp,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( part_material ), INTENT( IN ) :: pmatl
INTEGER,               INTENT( IN ) :: i0, igrp
REAL, DIMENSION(:),    POINTER      :: mataux

INTEGER iaux

iaux = i0

IF( igrp == 1 )THEN
  mataux(iaux)   = FLOAT(pmatl%nsg)
  mataux(iaux+1) = pmatl%rho
END IF

iaux = iaux + (igrp-1)*MAXPMAUX + 1

mataux(iaux+1) = pmatl%dmin
mataux(iaux+2) = pmatl%dbar
mataux(iaux+3) = pmatl%vd
mataux(iaux+4) = pmatl%sigvd
mataux(iaux+5) = pmatl%diff
mataux(iaux+6) = pmatl%dmax

RETURN
END SUBROUTINE PutParticleParam
!===============================================================================
!     PutLiquidParam
!===============================================================================
SUBROUTINE PutLiquidParam( pmatl,i0,j0,mataux )

USE struct_fd

IMPLICIT NONE

TYPE( liquid_material ), INTENT( IN ) :: pmatl
INTEGER,                 INTENT( IN ) :: i0, j0
REAL, DIMENSION(:),      POINTER      :: mataux

INTEGER iaux, igrp

iaux = i0 + MAXGMAUX
igrp = j0 - 1

IF( igrp == 1 )THEN

  mataux(iaux  ) = pmatl%rhob
  mataux(iaux+1) = pmatl%a
  mataux(iaux+2) = pmatl%b
  mataux(iaux+3) = pmatl%c
  mataux(iaux+4) = pmatl%w
  mataux(iaux+5) = pmatl%st
  mataux(iaux+6) = pmatl%sf
  mataux(iaux+7) = pmatl%viscosity
  mataux(iaux+8) = pmatl%cpL
  mataux(iaux+9) = pmatl%cpV

  iaux = iaux + MAXLMAUXP
  mataux(iaux)   = FLOAT(pmatl%nsg)
  mataux(iaux+1) = pmatl%rho

ELSE

  iaux = iaux + MAXLMAUXP

END IF

iaux = iaux + (igrp-1)*MAXLMAUX + 1

mataux(iaux+1) = pmatl%dmin
mataux(iaux+2) = pmatl%dbar
mataux(iaux+3) = pmatl%sigvd
mataux(iaux+4) = pmatl%diff
mataux(iaux+5) = pmatl%dmax

RETURN
END SUBROUTINE PutLiquidParam

!===============================================================================

SUBROUTINE check_units( nmat,mat,mataux )

USE scipuff_fi

IMPLICIT NONE

INTEGER,                            INTENT( IN ) :: nmat
TYPE( material_str ), DIMENSION(*), INTENT( IN ) :: mat
REAL, DIMENSION(:),                 POINTER      :: mataux

REAL    denrat
INTEGER i, j

CHARACTER(80) string1,string2
CHARACTER(16) units

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid

DO i = 1,nmat

  string1 = 'Material='//TRIM(mat(i)%cmat)
  units   = TRIM(mat(i)%unit)
  CALL clower( units )
  string2 = 'Material='//TRIM(mat(i)%cmat)//' : units='//TRIM(units)

  j = mat(i)%icls

  IF( IsGas(j) )THEN
    denrat = mataux(mat(i)%iaux)/rhoair
  ELSE IF( IsLiquid(j) )THEN
    denrat = 0.0      ! Liquids require kg units (for pools and thermodynamics)
  ELSE IF( IsAerosol(j) )THEN
    denrat = 0.0      ! Aerosols require kg units (for pools and thermodynamics)
  ELSE
    CYCLE
  END IF

  CALL check_kg( units,denrat,dynamic )
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'CheckUnits'
    IF( nError == UK_ERROR )THEN
      eInform = TRIM(string1)
    ELSE
      eInform = TRIM(string2)
    END IF
    GOTO 9999
  END IF

END DO

9999 CONTINUE

RETURN
END SUBROUTINE check_units

!===============================================================================

SUBROUTINE check_kg( units,denrat,dynamic )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: units
REAL,         INTENT( IN ) :: denrat
LOGICAL,      INTENT( IN ) :: dynamic

CHARACTER(16) munit

IF( denrat == 0.0 )THEN

  munit = units
  CALL clower( munit )

  IF( munit /= 'kg' )THEN
    nError   = IV_ERROR
    eMessage = 'Mass release units must be ''kg'' for liquids'
  END IF

ELSE

  IF( dynamic )THEN
    munit = units
    CALL clower( munit )

    IF( denrat /= 1.0 )THEN

      IF( munit /= 'kg' )THEN
        nError = IV_ERROR
        eMessage ='Mass release units must be ''kg'' for '// &
                         'buoyant gases and liquid vapors'
      END IF

    END IF
  END IF

END IF

RETURN
END SUBROUTINE check_kg

END MODULE UtilMtlAux

