MODULE SAGSkip

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

IMPLICIT NONE

CONTAINS

!*******************************************************************************
!               SAG_SkipHeader
!*******************************************************************************
INTEGER FUNCTION SAG_SkipHeader( grd )

!     Skips a surface grid file time break header

TYPE( SAGgrid_str ), POINTER :: grd

SAG_SkipHeader = SAG_ERROR

!==== Check Position record pointer

IF( grd%status /= SAG_HEAD_REC )THEN
  LastError = SAG_ERR_SKIP
  LastParm  = SAG_HEAD_REC
  GOTO 9999
END IF

!==== Skip header

grd%record = grd%record + 1
grd%status = SAG_GRID_REC

!==== Set reurn value

SAG_SkipHeader = SAG_OK

9999 CONTINUE

RETURN
END FUNCTION SAG_SkipHeader
!*******************************************************************************
!               SAG_SkipGrid
!*******************************************************************************
INTEGER FUNCTION SAG_SkipGrid( grd )

!     Skips a surface grid file time break Grid

TYPE( SAGgrid_str ), POINTER :: grd

SAG_SkipGrid = SAG_ERROR

!==== Check Position record pointer

IF( grd%status /= SAG_GRID_REC )THEN
  LastError = SAG_ERR_SKIP
  LastParm  = SAG_GRID_REC
  GOTO 9999
END IF

!==== Skip Grid

grd%record = grd%record + (grd%ncells-1)/SAG_RECL + 1
grd%status = SAG_DATA_REC

!==== Set reurn value

SAG_SkipGrid = SAG_OK

9999 CONTINUE

RETURN
END FUNCTION SAG_SkipGrid
!*******************************************************************************
!               SAG_SkipData
!*******************************************************************************
INTEGER FUNCTION SAG_SkipData( grd )

!     Skips a surface grid file time break data

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER irv

SAG_SkipData = SAG_ERROR

!==== Check Position record pointer

IF( grd%status /= SAG_DATA_REC )THEN
  LastError = SAG_ERR_SKIP
  LastParm  = SAG_DATA_REC
  GOTO 9999
END IF

!==== Skip Data

grd%record = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart

IF( grd%naux > 0 )THEN
  grd%status = SAG_AUX_REC
  irv = SAG_SkipAuxData( grd )
  IF( irv /= SAG_OK )GOTO 9999
END IF

grd%status = SAG_HEAD_REC

!==== Set return value

SAG_SkipData = SAG_OK

9999 CONTINUE

RETURN
END FUNCTION SAG_SkipData
!*******************************************************************************
!               SAG_SkipAuxData
!*******************************************************************************
INTEGER FUNCTION SAG_SkipAuxData( grd )

!     Skips a surface grid file time break data

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i, ir, nc, nd, ios, nskip

SAG_SkipAuxData = SAG_ERROR

!==== Check Position record pointer

IF( grd%status /= SAG_AUX_REC )THEN
  LastError = SAG_ERR_SKIP
  LastParm  = SAG_AUX_REC
  GOTO 9999
END IF

!==== Skip Auxiliary Data

ir = grd%record
DO i = 1,grd%naux
  READ(grd%nunit,REC=ir,IOSTAT=ios)nc,nd
  ir = ir + 1
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF
  IF( nc > 0 )THEN
    nskip = (2*nc-1)/SAG_RECL + 1 + (nd-1)/SAG_RECL + 1
    ir    = ir + nskip !Update pointer to skip data
  END IF
END DO

grd%record = ir
grd%status = SAG_HEAD_REC

SAG_SkipAuxData = SAG_OK

9999 CONTINUE

RETURN
END FUNCTION SAG_SkipAuxData

END MODULE SAGSkip
