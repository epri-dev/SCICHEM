!==============================================================================
! SizeSubstrateList
!==============================================================================
INTEGER FUNCTION SizeSubstrateList( mode )

USE files_fi
USE SCIMgr_fd
USE landuse_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode

INTEGER nsub
INTEGER ruok

TYPE( landuse_init ) :: file_landuse

INTEGER, EXTERNAL :: GetNumSubstrates

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available

!==============================================================================
! Account for default - impermeable surface
!==============================================================================
  SizeSubstrateList = 1

!==============================================================================
! Check landuse dll for more
!==============================================================================
  file_landuse%lun  = lun_tmp
  file_landuse%file = TRIM(file_lus)
  ruok = GetNumSubstrates( file_landuse,nsub )

  IF( ruok == 1 )SizeSubstrateList = SizeSubstrateList + nsub

END IF

RETURN
END
!==============================================================================
! GetSubstrateList
!==============================================================================
INTEGER FUNCTION GetSubstrateList( mode,cSubstrate )

USE files_fi
USE SCIMgr_fd
USE landuse_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,                       INTENT( IN  ) :: mode
TYPE( char16T ), DIMENSION(*), INTENT( OUT ) :: cSubstrate

INTEGER nsub, ruok, nList, ios, i

CHARACTER(16), DIMENSION(:), ALLOCATABLE :: substrate

TYPE( landuse_init ) :: file_landuse

INTEGER, EXTERNAL :: SizeSubstrateList
INTEGER, EXTERNAL :: GetNumSubstrates, GetSubstrates

GetSubstrateList = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available

!==============================================================================
!Add default - impermeable surface
!==============================================================================
  nList = 1
  cSubstrate(nList)%string = 'Impermeable'

!==============================================================================
!Check landuse data file for more
!==============================================================================
  file_landuse%lun  = lun_tmp
  file_landuse%file = TRIM(file_lus)
  ruok = GetNumSubstrates( file_landuse,nsub )
  IF( ruok == 1 )THEN
    ALLOCATE( substrate(nsub),STAT=ios )
    IF( ios /= 0 )GOTO 9999
    ruok = GetSubstrates( file_landuse,nsub,substrate )
    IF( ruok == 1 )THEN
      DO i = 1,nsub
        nList = nList + 1
        cSubstrate(nList)%string = TRIM(substrate(i))
      END DO
    END IF
  END IF

  IF( nList /= SizeSubstrateList(mode) )GOTO 9999

  GetSubstrateList = SCIPsuccess

END IF

9999 CONTINUE

IF( ALLOCATED(substrate) )DEALLOCATE( substrate,STAT=ios )

RETURN
END

