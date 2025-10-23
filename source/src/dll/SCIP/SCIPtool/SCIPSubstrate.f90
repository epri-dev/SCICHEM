!==============================================================================
! SCIPNumSubstrates
!==============================================================================
INTEGER FUNCTION SCIPNumSubstrates( mode )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumSubstrates

INTEGER, INTENT( IN ) :: mode

INTEGER, EXTERNAL :: SizeSubstrateList

SCIPNumSubstrates = SizeSubstrateList( mode )

RETURN
END
!==============================================================================
! SCIPGetSubstrates
!==============================================================================
INTEGER FUNCTION SCIPGetSubstrates( mode,cSubstrate )

USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetSubstrates

INTEGER,                       INTENT( IN  ) :: mode
TYPE( char16T ), DIMENSION(*), INTENT( OUT ) :: cSubstrate

INTEGER, EXTERNAL :: GetSubstrateList

SCIPGetSubstrates = GetSubstrateList( mode,cSubstrate )

RETURN
END

