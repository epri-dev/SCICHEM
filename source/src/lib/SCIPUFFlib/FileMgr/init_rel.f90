!===============================================================================
!     InitRelease
!===============================================================================
SUBROUTINE InitRelease()

USE nextRel_fi
USE default_fd
USE release_fi

IMPLICIT NONE

INTEGER i

!==== Integers

relStatus  = NOT_SET_I

relName    = '<empty>'
relDisplay = '<empty>'

subgroup = 1

!==== Logicals

!==== Reals

trel = NOT_SET_R
xrel = NOT_SET_D
yrel = NOT_SET_D
zrel = NOT_SET_R

sigx = NOT_SET_R
sigy = NOT_SET_R
sigz = NOT_SET_R
size_rel = NOT_SET_R

sigRxy = NOT_SET_R
sigRxz = NOT_SET_R
sigRyz = NOT_SET_R

xlenA = NOT_SET_R
ylenA = NOT_SET_R
angA  = NOT_SET_R

urel = 0.0
vrel = 0.0
wrel = 0.0

umom = DEF_VAL_R
vmom = DEF_VAL_R
wmom = 0.0
buoy = 0.0

cmass = NOT_SET_R
tdur  = NOT_SET_R

DO i = 1,SCIPUFF_MAXRELPARAM
  rel_param(i) = NOT_SET_R
END DO

!==== Character strings

reltyp   = ' '
relmat   = 'UNKNOWN'
name_rel = ' '

RETURN
END
!===============================================================================
!     InitReleaseSpec
!===============================================================================
SUBROUTINE InitReleaseSpec( relSpec )

USE release_fd
USE default_fd

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec

CALL InitReleaseT( relSpec%release )

relSpec%ityp = NOT_SET_I
relSpec%distrib = NOT_SET_I
relSpec%release%trel = NOT_SET_R
CALL InitMCrelList( relSpec%MClist )

RETURN
END
!===============================================================================
!     InitReleaseSpec
!===============================================================================
SUBROUTINE ClearReleaseSpec( relSpec )

USE release_fd
USE default_fd

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec

relSpec%ityp = NOT_SET_I
relSpec%distrib = NOT_SET_I
relSpec%release%trel = NOT_SET_R
CALL ClearMCrelList( relSpec%MClist )

RETURN
END
!===============================================================================
!     InitMCrelList
!===============================================================================
SUBROUTINE InitMCrelList( MClist )

USE release_fd

IMPLICIT NONE

TYPE( MCrelList ), INTENT( INOUT ) :: MClist

MClist%nList = 0
NULLIFY( MClist%firstMCRel )

RETURN
END
!===============================================================================
!     ClearMCrelList
!===============================================================================
SUBROUTINE ClearMCrelList( MClist )

USE release_fd

IMPLICIT NONE

TYPE( MCrelList ), INTENT( INOUT ) :: MClist

TYPE( MCrelData ), POINTER :: MCrel, MCnext

MCrel => MClist%firstMCRel

DO WHILE( ASSOCIATED(MCrel) )
  MCnext => MCrel%next
  DEALLOCATE( MCrel )
  NULLIFY( MCrel )
  MCrel => MCnext
END DO

NULLIFY( MClist%firstMCRel )
MClist%nList = 0

RETURN
END
!===============================================================================
!     InitReleaseT
!===============================================================================
SUBROUTINE InitReleaseT( rel )

USE release_fd
USE default_fd

IMPLICIT NONE

TYPE( releaseT ), INTENT( INOUT ) :: rel

INTEGER i

rel%padding = NOT_SET_I
rel%type    = NOT_SET_I
rel%status = NOT_SET_I
rel%tRel = NOT_SET_R
rel%xRel = NOT_SET_D
rel%yRel = NOT_SET_D
rel%zRel = NOT_SET_R
rel%notUsedA = NOT_SET_R
rel%notUsedB = NOT_SET_R
rel%notUsed = NOT_SET_R
DO i = 1,HS_PADRELGEN
  rel%relData%padding(i) = NOT_SET_I
END DO
rel%material = ' '
rel%relName = ' '
rel%relDisplay = ' '

RETURN
END



