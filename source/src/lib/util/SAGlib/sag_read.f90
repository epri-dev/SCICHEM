!*******************************************************************************
!   SAG_ReadHeader
!*******************************************************************************
INTEGER FUNCTION SAG_ReadHeaderID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file time break header

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       !Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadHeader( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_ReadHeader
END INTERFACE

SAG_ReadHeaderID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReadHeaderID = SAG_ReadHeader( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ReadHeader( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Reads a surface grid file time break header

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios,nvs,i,mlev,iver

REAL, DIMENSION(SAG_HWRDS) :: dath

CHARACTER(4) cdum

CHARACTER(4),       DIMENSION(:), POINTER :: names  !variable name data
TYPE(SAGblock_str), DIMENSION(:), POINTER :: blocks !internal block data for block versions

INTERFACE
  INTEGER FUNCTION SAG_FindHeader( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindHeader
  INTEGER FUNCTION SAG_ReadNames( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_ReadNames
END INTERFACE

SAG_ReadHeader = SAG_ERROR

!==== Position record pointer

i = SAG_FindHeader( grd )
IF( i /= SAG_OK )THEN
  SAG_ReadHeader = i
  GOTO 9999
END IF

!==== Read header

IF( grd%ftype == 0 )THEN
  READ(grd%nunit,REC=grd%record,IOSTAT=ios) &
                          (dath(i),i=1,SAG_HWRDS),nvs,(cdum,i=1,nvs),iver
ELSE
  READ(grd%nunit,REC=grd%record,IOSTAT=ios) &
                          (dath(i),i=1,SAG_HWRDS),mlev
END IF
IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

!====Set structure values

grd%time   = dath(SAG_HEAD_TIME)
grd%ncells = TRANSFER(dath(SAG_HEAD_CELL),grd%ncells)
grd%nx     = TRANSFER(dath(SAG_HEAD_NX),grd%nx)
grd%ny     = TRANSFER(dath(SAG_HEAD_NY),grd%ny)
grd%xmin   = dath(SAG_HEAD_XMIN)
grd%ymin   = dath(SAG_HEAD_YMIN)
grd%dx     = dath(SAG_HEAD_DX)
grd%dy     = dath(SAG_HEAD_DY)

!====Set grid type

grd%type = SAG_GRID_BOTH
IF( grd%nx < 0 .OR. grd%ny < 0 )THEN
  IF ( grd%nx < 0 )THEN
    grd%nx = ABS(grd%nx)
    IF( grd%ny < 0 )THEN
      grd%ny = ABS(grd%ny)
      grd%type = SAG_GRID_NONE
    ELSE
      grd%type = SAG_GRID_VERT
    END IF
  ELSE
    grd%ny = ABS(grd%ny)
    grd%type = SAG_GRID_HORZ
  END IF
END IF

IF( grd%ftype == 0 )THEN
  grd%nvart   = nvs
  grd%version = iver
  IF( iver >= VERSION_DELMIN )THEN
    READ(grd%nunit,rec=grd%record,IOSTAT=ios) &
                  (dath(i),i=1,SAG_HWRDS),nvs,(cdum,i=1,nvs),iver,mlev
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_READ
      LastParm  = ios
      GOTO 9999
    END IF
  ELSE
    mlev = -99
  END IF
END IF

grd%maxlev = mlev
grd%delmin = 0.

!------ Allocate grid if necessary

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  IF( grd%mxgrd > 0 )THEN
    ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%mxgrd
      GOTO 9999
    END IF
  END IF
END IF

!------ Allocate names if necessary

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  IF( grd%mxnam <= 0 )grd%mxnam = grd%nvart
  ALLOCATE( names(grd%mxnam),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = grd%mxnam
    GOTO 9999
  END IF
  grd%ipnam => names
END IF

IF( grd%nvart > grd%mxnam )THEN
  LastError = SAG_ERR_MXNAM
  LastParm  = grd%nvart
  GOTO 9999
END IF

!------ Allocate blocks if necessary

IF( grd%nblk /= 0 )THEN
  IF( .NOT.ASSOCIATED(grd%ipblk) )THEN
    ALLOCATE( blocks(grd%nblk),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%ftype
      GOTO 9998
    END IF
    grd%ipblk => blocks
  END IF
END IF

!------ Allocate data fields if necessary

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  IF( grd%mxfld <= 0 )grd%mxfld = MAX( grd%nvart,ABS(grd%mxfld) )
  IF( grd%mxgrd > 0 )THEN
    ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%mxgrd*grd%mxfld
      GOTO 9999
    END IF
  END IF
END IF

IF( .NOT.ASSOCIATED(grd%ipflg) )THEN
  ALLOCATE( grd%ipflg(grd%mxfld),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = grd%mxfld
    GOTO 9999
  END IF
  DO i = 1,grd%mxfld
    grd%ipflg(i) = SAG_GRID_FULL
  END DO
END IF

!------ Read field names

i = SAG_ReadNames( grd )
IF( i /= SAG_OK )THEN
  SAG_ReadHeader = i
  GOTO 9999
END IF

!==== Set return value

IF( grd%ncells > 0 )THEN
  grd%record = grd%record + 1
  grd%status = SAG_GRID_REC
  SAG_ReadHeader = SAG_OK
ELSE
  grd%status = SAG_EOF_REC
  SAG_ReadHeader = SAG_EOF
END IF

9999 CONTINUE

RETURN

9998 CONTINUE
IF( ASSOCIATED(grd%ipblk) )THEN
  DO i = 1,SIZE(grd%ipblk)
    IF( ASSOCIATED( grd%ipblk(i)%fldnam ))THEN
      DEALLOCATE( grd%ipblk(i)%fldnam,STAT=ios )
    END IF
  END DO
  DEALLOCATE( grd%ipblk,STAT=ios )
  NULLIFY( grd%ipblk )
END IF
GOTO 9999

END
!*******************************************************************************
!   SAG_ReadNames
!*******************************************************************************
INTEGER FUNCTION SAG_ReadNames( grd )

USE sagstr_fd

!     Reads a surface grid file time break header

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadOldNames( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_ReadOldNames

  INTEGER FUNCTION SAG_ReadBlockNames( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_ReadBlockNames
END INTERFACE

IF( grd%ftype == 0 )THEN
  SAG_ReadNames = SAG_ReadOldNames( grd )
ELSE
  SAG_ReadNames = SAG_ReadBlockNames( grd )
END IF

RETURN
END
!*******************************************************************************
!   SAG_ReadOldNames
!*******************************************************************************
INTEGER FUNCTION SAG_ReadOldNames( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Reads a surface grid file time break header

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios,nvs,i
REAL    dum

INTERFACE
  INTEGER FUNCTION SAG_FindHeader( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindHeader
END INTERFACE

SAG_ReadOldNames = SAG_ERROR

!==== Check file type

IF( grd%ftype /= 0 )THEN
  LastError = SAG_ERR_FTYPE
  LastParm  = grd%ftype
  GOTO 9999
END IF

!==== Check pointer

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  LastError = SAG_ERR_IPNAM
  LastParm  = 0
  GOTO 9999
END IF

!==== Position record pointer

i = SAG_FindHeader( grd )
IF( i /= SAG_OK )THEN
  SAG_ReadOldNames = i
  GOTO 9999
END IF

!==== Read names

READ(grd%nunit,REC=grd%record,IOSTAT=ios) &
              (dum,i=1,SAG_HWRDS),nvs,(grd%ipnam(i),i=1,nvs)
IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

!==== Set reurn values

SAG_ReadOldNames = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadBlockNames
!*******************************************************************************
INTEGER FUNCTION SAG_ReadBlockNames( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Reads a surface grid file time break header

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios,nvs,i,irec,j

CHARACTER(4),       DIMENSION(:), POINTER :: name
TYPE(SAGblock_str), DIMENSION(:), POINTER :: blocks

SAG_ReadBlockNames = SAG_ERROR

!==== Check file type

IF( grd%ftype == 0 )THEN
  LastError = SAG_ERR_FTYPE
  LastParm  = grd%ftype
  GOTO 9999
END IF

!==== Check pointer

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  LastError = SAG_ERR_IPNAM
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipblk) )THEN
  LastError = SAG_ERR_IPBLK
  LastParm  = 0
  GOTO 9999
END IF

!==== Position record pointer

irec = 1

!==== Read block names

blocks => grd%ipblk

DO i = 1,grd%nblk

  irec = irec + 1
  READ(grd%nunit,REC=irec,IOSTAT=ios) blocks(i)%name,blocks(i)%ifld, &
                                      blocks(i)%nfld,blocks(i)%iaux
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF
  IF( i > 1 )THEN
    blocks(i-1)%nfld = blocks(i)%ifld - blocks(i-1)%ifld
  END IF

END DO

!==== Read field names

name => grd%ipnam

irec = irec + 1
READ(grd%nunit,REC=irec,IOSTAT=ios) nvs,(name(i),i=1,MIN(nvs,SAG_RECL-1))
IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF
IF( nvs >= SAG_RECL )THEN
  j = SAG_RECL - 1
  DO WHILE( j < nvs )
    irec = irec + 1
    READ(grd%nunit,REC=irec,IOSTAT=ios) (name(i),i=j+1,MIN(nvs,j+SAG_RECL))
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_READ
      LastParm  = ios
      GOTO 9999
    END IF
    j = j + SAG_RECL
  END DO
END IF

blocks(grd%ftype)%nfld = nvs - blocks(grd%ftype)%ifld + 1

!==== copy names to block structure

DO i = 1,grd%nblk

  IF( .NOT.ASSOCIATED(blocks(i)%fldnam) )THEN
    ALLOCATE( blocks(i)%fldnam(blocks(i)%nfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = ios
      GOTO 9999
    END IF
  END IF

  DO j = 1,blocks(i)%nfld
    blocks(i)%fldnam(j) = name(j+blocks(i)%ifld-1)
  END DO

END DO

!==== Set return values

SAG_ReadBlockNames = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadGrid
!*******************************************************************************
INTEGER FUNCTION SAG_ReadGridID( grdI )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file grid

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_ReadGrid
END INTERFACE

SAG_ReadGridID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReadGridID = SAG_ReadGrid( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ReadGrid( grd )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi

!     Reads a surface grid file grid

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ii,irec,j,j1,ios

INTEGER, DIMENSION(:), POINTER :: grid

INTERFACE
  INTEGER FUNCTION SAG_FindGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindGrid
END INTERFACE

SAG_ReadGrid = SAG_ERROR

!------ Allocate grid if necessary

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  IF( grd%mxgrd <= 0 )grd%mxgrd = grd%ncells
  IF( grd%mxgrd > 0 )THEN
    ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%mxgrd
      GOTO 9999
    END IF
  END IF
END IF

IF( grd%ncells > grd%mxgrd )THEN
  LastError = SAG_ERR_MXGRD
  LastParm  = grd%ncells
  GOTO 9999
END IF

!------ Check that grid is allocated

IF( .NOT.ASSOCIATED( grd%ipgrd ))THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

!==== Position record pointer

ii = SAG_FindGrid( grd )
IF( ii /= SAG_OK )THEN
  SAG_ReadGrid = ii
  GOTO 9999
END IF

!==== Read Grid

irec = grd%record

grid => grd%ipgrd

DO ii = 1,grd%ncells,SAG_RECL
  j1 = MIN(ii+SAG_RECL-1,grd%ncells)
  READ(grd%nunit,REC=irec,IOSTAT=ios) (grid(j), j=ii,j1)
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
END DO

!==== Set return values

SAG_ReadGrid = SAG_OK
grd%record   = irec
grd%status   = SAG_DATA_REC

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadData
!*******************************************************************************
INTEGER FUNCTION SAG_ReadDataID( grdI,ivar,ifld )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ivar       ! read field ivar
INTEGER, INTENT( IN ) :: ifld       ! into field ifld

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadData( grd,ivar,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ivar
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_ReadData
END INTERFACE

SAG_ReadDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReadDataID = SAG_ReadData( grd,ivar,ifld )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ReadData( grd,ivar,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Reads a surface grid file data

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ivar ! read field ivar
INTEGER,             INTENT( IN ) :: ifld ! into field ifld

INTEGER ii,irec,j,j1,ios

REAL, DIMENSION(:), POINTER :: datg

INTERFACE
  INTEGER FUNCTION SAG_FindData( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindData
END INTERFACE

SAG_ReadData = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  IF( grd%mxfld <= 0 )grd%mxfld = MAX( grd%nvart,ABS(grd%mxfld) )
  IF( grd%mxgrd > 0 )THEN
    ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%mxgrd*grd%mxfld
      GOTO 9999
    END IF
  END IF
END IF

!==== Check file

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF
IF( ivar <= 0 .OR. ivar > grd%nvart )THEN
  LastError = SAG_ERR_IVAR
  LastParm  = 0
  GOTO 9999
END IF
IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

!==== Position record pointer

ii = SAG_FindData( grd )
IF( ii /= SAG_OK )THEN
  SAG_ReadData = ii
  GOTO 9999
END IF

!==== Set data pointers

irec = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*(ivar-1)

grd%ipflg(ifld) = SAG_GRID_FULL

!==== Read Data

datg => grd%ipdat

DO ii = 1,grd%ncells,SAG_RECL
  j1 = MIN(ii+SAG_RECL-1,grd%ncells)
  READ(grd%nunit,REC=irec,IOSTAT=ios) (datg(j+(ifld-1)*grd%mxgrd),j=ii,j1)
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
END DO

!==== Set return values

SAG_ReadData = SAG_OK

9999 CONTINUE

RETURN
END

!*******************************************************************************
!   SAG_ReadMCData
!*******************************************************************************
INTEGER FUNCTION SAG_ReadMCDataID( grdI )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadMCData( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_ReadMCData
END INTERFACE

SAG_ReadMCDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReadMCDataID = SAG_ReadMCData( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ReadMCData( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Reads a surface grid file data

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd

INTEGER ii,irec,j,j1,ios,ivar

REAL, DIMENSION(:), POINTER :: datg

INTERFACE
  INTEGER FUNCTION SAG_FindData( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindData
END INTERFACE

SAG_ReadMCData = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  IF( grd%mxfld <= 0 )grd%mxfld = MAX( grd%nvart,ABS(grd%mxfld) )
  IF( grd%mxgrd > 0 )THEN
    ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%mxgrd*grd%mxfld
      GOTO 9999
    END IF
  END IF
END IF

!==== Check file

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

!==== Position record pointer

ii = SAG_FindData( grd )
IF( ii /= SAG_OK )THEN
  SAG_ReadMCData = ii
  GOTO 9999
END IF

!==== Set data pointers

DO ivar = 1,grd%mxfld

  irec = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*(ivar-1)

  grd%ipflg(ivar) = SAG_GRID_FULL

  !==== Read Data

  datg => grd%ipdat

  DO ii = 1,grd%ncells,SAG_RECL
    j1 = MIN(ii+SAG_RECL-1,grd%ncells)
    READ(grd%nunit,REC=irec,IOSTAT=ios) (datg(j+(ivar-1)*grd%mxgrd),j=ii,j1)
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_READ
      LastParm  = ios
      GOTO 9999
    END IF
    irec = irec + 1
  END DO

END DO

!==== Set return values

SAG_ReadMCData = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadBreakID
!*******************************************************************************
INTEGER FUNCTION SAG_ReadBreakID( grdI,time,nvar,names )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

REAL,                       INTENT( IN ) :: time   !Timebreak to read
INTEGER,                    INTENT( IN ) :: nvar   !No. of variables to read
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: names  !Var names to read
INTEGER,                    INTENT( IN ) :: grdI   !Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i,j,ivar

INTEGER, EXTERNAL :: SAG_OpenID,SAG_FindTime,SAG_ReadAuxDataID
INTEGER, EXTERNAL :: SAG_FindVariableID,SAG_ReadHeaderID,SAG_ReadGridID,SAG_ReadDataID

!==== Initialize

SAG_ReadBreakID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Open File

i = SAG_OpenID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadBreakID = i
  GOTO 9999
END IF

!==== Find the requested time

i = SAG_FindTime( grdI,time )
IF( i /= SAG_OK )THEN
  SAG_ReadBreakID = i
  GOTO 9999
END IF

!==== Read the header

i = SAG_ReadHeaderID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadBreakID = i
  GOTO 9999
END IF

!==== Read the grid

i = SAG_ReadGridID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadBreakID = i
  GOTO 9999
END IF

!==== Read data as requested
! nvar < 0 -> read all
! nvar = 0 -> read none
! nvar > 0 -> read nvar names

IF( nvar > 0 )THEN

!====== NVAR > 0 - Read nvar names

  DO j = 1,nvar

!======== Find variable

    i = SAG_FindVariableID( grdI,names(j),ivar )
    IF( i /= SAG_OK )THEN
      SAG_ReadBreakID = i
      GOTO 9999
    END IF

!======== Read data

    i = SAG_ReadDataID( grdI,ivar,j )
    IF( i /= SAG_OK )THEN
      SAG_ReadBreakID = i
      GOTO 9999
    END IF

  END DO

ELSE IF( nvar < 0 )THEN

!====== NVAR < 0 - Read all names

  DO j = 1,grd%nvart

!======== Read data
    i = SAG_ReadDataID( grdI,j,j )
    IF( i /= SAG_OK )THEN
      SAG_ReadBreakID = i
      GOTO 9999
    END IF

  END DO

END IF

!====== move record to next timebreak

grd%record = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart
grd%status = SAG_AUX_REC

!======== Read auxiliary data

i = SAG_ReadAuxDataID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadBreakID = i
  GOTO 9999
END IF

SAG_ReadBreakID = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadNextBreakID
!*******************************************************************************
INTEGER FUNCTION SAG_ReadNextBreakID( grdI,nvar,names )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER,                    INTENT( IN ) :: nvar   !No. of variables to read
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: names  !Var names to read
INTEGER,                    INTENT( IN ) :: grdI   !Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i,j,ivar

INTEGER, EXTERNAL :: SAG_ReadAuxDataID,SAG_ReadHeaderID,SAG_ReadGridID
INTEGER, EXTERNAL :: SAG_ReadDataID,SAG_FindVariableID

!==== Initialize

SAG_ReadNextBreakID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Read the header

i = SAG_ReadHeaderID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadNextBreakID = i
  GOTO 9999
END IF

!==== Read the grid

i = SAG_ReadGridID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadNextBreakID = i
  GOTO 9999
END IF

!==== Read data as requested
! nvar < 0 -> read all
! nvar = 0 -> read none
! nvar > 0 -> read nvar names

IF( nvar > 0 )THEN

!====== NVAR > 0 - Read nvar names

  DO j = 1,nvar

!======== Find variable

    i = SAG_FindVariableID( grdI,names(j),ivar )
    IF( i /= SAG_OK )THEN
      SAG_ReadNextBreakID = i
      GOTO 9999
    END IF

!======== Read data

    i = SAG_ReadDataID( grdI,ivar,j )
    IF( i /= SAG_OK )THEN
      SAG_ReadNextBreakID = i
      GOTO 9999
    END IF

  END DO

ELSE IF( nvar < 0 )THEN

!====== NVAR < 0 - Read all names

  DO j = 1,grd%nvart

!======== Read data
    i = SAG_ReadDataID( grdI,j,j )
    IF( i /= SAG_OK )THEN
      SAG_ReadNextBreakID = i
      GOTO 9999
    END IF

  END DO

END IF

!====== move record to next timebreak

grd%record = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart
grd%status = SAG_AUX_REC

!======== Read auxiliary data

i = SAG_ReadAuxDataID( grdI )
IF( i /= SAG_OK )THEN
  SAG_ReadNextBreakID = i
  GOTO 9999
END IF

SAG_ReadNextBreakID = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_ReadAuxDataID
!*******************************************************************************
INTEGER FUNCTION SAG_ReadAuxDataID( grdI )

!     Read a surface grid file aux data

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI   !Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER irec,i,irv

INTERFACE
  INTEGER FUNCTION SAG_FindHeader( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_FindHeader

  INTEGER FUNCTION SAG_ReadAuxVar( grd,iaux )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: iaux
  END FUNCTION SAG_ReadAuxVar
END INTERFACE

SAG_ReadAuxDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check file

IF( grd%naux > 0 )THEN
  IF( .NOT.ASSOCIATED( grd%aux ) )THEN
    LastError = SAG_ERR_IPDAT
    LastParm  = 0
    GOTO 9999
  END IF
ELSE
  grd%status = SAG_HEAD_REC
  SAG_ReadAuxDataID = SAG_OK
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status == SAG_AUX_REC )THEN
  irec = grd%record
ELSE IF( grd%status == SAG_DATA_REC )THEN
  irec = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart
ELSE
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_AUX_REC
  GOTO 9999
END IF

!==== Read Auxiliary Data

IF( grd%naux > 0 )THEN
  DO i = 1,grd%naux
    irv = SAG_ReadAuxVar( grd,i )
    IF( irv /= SAG_OK )GOTO 9999
  END DO
  irv = SAG_FindHeader( grd )
  IF( irv /= SAG_OK )GOTO 9999
ELSE
  grd%record = irec
END IF

SAG_ReadAuxDataID = SAG_OK

grd%status = SAG_HEAD_REC

9999 CONTINUE

RETURN
END
!*******************************************************************************
!   SAG_ReadAuxVarID
!*******************************************************************************
INTEGER FUNCTION SAG_ReadAuxVarID( grdI,iaux )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: iaux

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReadAuxVar( grd,iaux )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: iaux
  END FUNCTION SAG_ReadAuxVar
END INTERFACE

SAG_ReadAuxVarID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReadAuxVarID = SAG_ReadAuxVar( grd,iaux )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_ReadAuxVar
!*******************************************************************************
INTEGER FUNCTION SAG_ReadAuxVar( grd,iaux )

!     Read a surface grid file - single block of aux data

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: iaux

INTEGER ii,irec,i,j,j1,ios,nc,nd,naux,ic,nskip

INTEGER, DIMENSION(:), ALLOCATABLE :: auxID
REAL   , DIMENSION(:), ALLOCATABLE :: auxdata

SAG_ReadAuxVar = SAG_ERROR

!==== Check file

IF( grd%naux > 0 )THEN
  IF( .NOT.ASSOCIATED( grd%aux ) )THEN
    LastError = SAG_ERR_IPDAT
    LastParm  = 0
    GOTO 9999
  END IF
ELSE
  SAG_ReadAuxVar = SAG_OK
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status == SAG_AUX_REC )THEN
  irec = grd%record
ELSE IF( grd%status == SAG_DATA_REC )THEN
  irec = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart
ELSE
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_AUX_REC
  GOTO 9999
END IF

!Skip earlier aux data

DO i = 1,iaux-1
  READ(grd%nunit,REC=irec,IOSTAT=ios) nc,naux
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_WRITE
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
  IF( nc > 0 )THEN
    nskip = (2*nc-1)/SAG_RECL + 1 + (naux-1)/SAG_RECL + 1
    irec  = irec + nskip !Update pointer to skip data
  END IF
END DO

READ(grd%nunit,REC=irec,IOSTAT=ios) nc,naux
IF( ios /= 0 )THEN
  LastError = SAG_ERR_WRITE
  LastParm  = ios
  GOTO 9999
END IF

irec = irec + 1

IF( nc > 0 )THEN
  ALLOCATE( grd%aux(iaux)%srf_data(grd%mxgrd),STAT=ios )
  IF( ios == 0 )ALLOCATE( auxID(2*nc),auxdata(naux),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = ios
    GOTO 9999
  END IF
  DO ii = 1,grd%mxgrd
    NULLIFY( grd%aux(iaux)%srf_data(ii)%data )
  END DO
  grd%aux(iaux)%alloc = .TRUE.

  DO ii = 1,2*nc,SAG_RECL
    j1 = MIN(ii+SAG_RECL-1,2*nc)
    READ(grd%nunit,REC=irec,IOSTAT=ios) (auxID(j),j=ii,j1)
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_WRITE
      LastParm  = ios
      GOTO 9999
    END IF
    irec = irec + 1
  END DO
  DO ii = 1,naux,SAG_RECL
    j1 = MIN(ii+SAG_RECL-1,naux)
    READ(grd%nunit,REC=irec,IOSTAT=ios) (auxdata(j),j=ii,j1)
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_WRITE
      LastParm  = ios
      GOTO 9999
    END IF
    irec = irec + 1
  END DO

  naux = 0
  DO ic = 1,nc
    ii = auxID(2*ic-1)
    nd = auxID(2*ic  )
    ALLOCATE( grd%aux(iaux)%srf_data(ii)%data(nd),STAT=ios )
    DO j = 1,nd
      grd%aux(iaux)%srf_data(ii)%data(j) = auxdata(naux+j)
    END DO
    naux = naux + nd
  END DO
END IF
DEALLOCATE( auxID,auxdata,STAT=ios )

SAG_ReadAuxVar = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_WriteHeaderID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteHeaderID( grdI )

!     Writes a surface grid file time break header

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios,nvs,i,mlev,iver
LOGICAL lOpen

REAL, DIMENSION(SAG_HWRDS) :: data

CHARACTER(4), DIMENSION(:), POINTER :: cnam

SAG_WriteHeaderID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE(FILE=grd%file,OPENED=lOpen,NUMBER=i)
IF( .NOT.lOpen )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit number is correct

IF( i /= grd%nunit )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = i
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status /= SAG_HEAD_REC )THEN
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_HEAD_REC
  GOTO 9999
END IF

!==== Prepare header data

data(SAG_HEAD_TIME) = grd%time
data(SAG_HEAD_CELL) = TRANSFER(grd%ncells,data(1))
data(SAG_HEAD_XMIN) = grd%xmin
data(SAG_HEAD_YMIN) = grd%ymin
data(SAG_HEAD_DX)   = grd%dx
data(SAG_HEAD_DY)   = grd%dy

IF( grd%type == SAG_GRID_HORZ )THEN
  nvs = -grd%ny
ELSE
  nvs = grd%ny
END IF
data(SAG_HEAD_NY) = TRANSFER(nvs,data(1))

IF( grd%type == SAG_GRID_VERT )THEN
  nvs = -grd%nx
ELSE
  nvs = grd%nx
END IF
data(SAG_HEAD_NX) = TRANSFER(nvs,data(1))

iver = grd%version
nvs  = grd%nvart
mlev = grd%maxlev

IF( grd%ftype == 0 )THEN
  IF( .NOT.ASSOCIATED( grd%ipnam ) )THEN
    LastError = SAG_ERR_IPNAM
    LastParm  = 0
    GOTO 9999
  END IF
  cnam => grd%ipnam
END IF

!==== Write header

IF( grd%ftype == 0 )THEN
  WRITE(grd%nunit,REC=grd%record,IOSTAT=ios) &
                (data(i),i=1,SAG_HWRDS),nvs,(cnam(i),i=1,nvs),iver,mlev
ELSE
  WRITE(grd%nunit,REC=grd%record,IOSTAT=ios) (data(i),i=1,SAG_HWRDS),mlev
END IF
IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

!==== Set return value

grd%record = grd%record + 1
grd%status = SAG_GRID_REC

SAG_WriteHeaderID = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_WriteGridID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteGridID( grdI )

!     Writes a surface grid file grid

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ii,irec,j,j1,ios,i
LOGICAL lOpen

SAG_WriteGridID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check file

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE(FILE=grd%file,OPENED=lOpen,NUMBER=i)
IF( .NOT.lOpen )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit number is correct

IF( i /= grd%nunit )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = i
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status /= SAG_GRID_REC )THEN
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_GRID_REC
  GOTO 9999
END IF

!==== Write Grid

irec = grd%record

DO ii = 1,grd%ncells,SAG_RECL
  j1 = MIN(ii+SAG_RECL-1,grd%ncells)
  WRITE(grd%nunit,REC=irec,IOSTAT=ios) (grd%ipgrd(j), j=ii,j1)
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_WRITE
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
END DO

!==== Set return values

grd%record = irec
grd%status = SAG_DATA_REC

SAG_WriteGridID = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_WriteDataID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteDataID( grdI,jvar,ifld )

!     Writes a surface grid file data

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI   !Grid ID
INTEGER, INTENT( IN ) :: jvar   !Variable number in grid structure
INTEGER, INTENT( IN ) :: ifld   !Output field number

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ii, i0, irec, j, j1, ios, i, ivar
LOGICAL lOpen, lUpdate

SAG_WriteDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

ivar    = ABS(jvar)
lUpdate = jvar < 0

!==== Check file

IF( .NOT.ASSOCIATED( grd%ipdat ) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF
IF( ivar <= 0 .OR. ivar > grd%nvart )THEN
  LastError = SAG_ERR_IVAR
  LastParm  = 0
  GOTO 9999
END IF
IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE( FILE=grd%file,OPENED=lOpen,NUMBER=i )
IF( .NOT.lOpen )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit number is correct

IF( i /= grd%nunit )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = i
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status /= SAG_DATA_REC )THEN
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_DATA_REC
  GOTO 9999
END IF

!==== Set data pointers

irec = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*(ivar-1)
i0   = grd%mxgrd*(ifld-1)

!==== Write Data

DO ii = 1,grd%ncells,SAG_RECL
  j1 = MIN(ii+SAG_RECL-1,grd%ncells)
  WRITE(grd%nunit,REC=irec,IOSTAT=ios) (grd%ipdat(j+i0),j=ii,j1)
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_WRITE
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
END DO

!==== Set return values

SAG_WriteDataID = SAG_OK
IF( lUpdate )THEN
  grd%record = grd%record + ((grd%ncells-1)/SAG_RECL + 1)*grd%nvart
  IF( grd%naux == 0 )THEN
    grd%status = SAG_HEAD_REC
  ELSE
    grd%status = SAG_AUX_REC
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_WriteAuxDataID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteAuxDataID( grdI )
!DIR$ OPTIMIZE:0
!     Writes a surface grid file data

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI   !Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ii,irec,j,j1,ios,i,nc,nd,naux
LOGICAL lOpen

INTEGER, DIMENSION(:), ALLOCATABLE :: auxID
REAL   , DIMENSION(:), ALLOCATABLE :: auxdata

SAG_WriteAuxDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check file

IF( grd%naux > 0 )THEN
  IF( .NOT.ASSOCIATED( grd%aux ) )THEN
    LastError = SAG_ERR_IPDAT
    LastParm  = 0
    GOTO 9999
  END IF
ELSE
  grd%status = SAG_HEAD_REC
  SAG_WriteAuxDataID = SAG_OK
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE(FILE=grd%file,OPENED=lOpen,NUMBER=i)
IF( .NOT.lOpen )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit number is correct

IF( i /= grd%nunit )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = i
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status /= SAG_AUX_REC )THEN
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_AUX_REC
  GOTO 9999
END IF

!==== Write Auxiliary Data

irec = grd%record

IF( grd%naux > 0 )THEN
  DO i = 1,grd%naux
    nc   = 0
    naux = 0
    IF( grd%aux(i)%alloc )THEN
      DO ii = 1,grd%ncells
        IF( ASSOCIATED(grd%aux(i)%srf_data(ii)%data) )THEN
          nc   = nc + 1
          naux = naux + SIZE(grd%aux(i)%srf_data(ii)%data)
        END IF
      END DO
      IF( nc > 0 )THEN
        ALLOCATE( auxID(2*nc),STAT=ios )
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_MALLOC
          LastParm  = ios
          GOTO 9999
        END IF
        ALLOCATE(auxdata(naux),STAT=ios )
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_MALLOC
          LastParm  = ios
          GOTO 9999
        END IF
        nc   = 0
        naux = 0
        DO ii = 1,grd%ncells
          IF( ASSOCIATED(grd%aux(i)%srf_data(ii)%data) )THEN
            nc = nc + 1
            nd = SIZE(grd%aux(i)%srf_data(ii)%data)
            auxID(2*nc-1) = ii
            auxID(2*nc  ) = nd
            DO j = 1,nd
              auxdata(naux+j) = grd%aux(i)%srf_data(ii)%data(j)
            END DO
            naux = naux + nd
          END IF
        END DO
        WRITE(grd%nunit,REC=irec,IOSTAT=ios) nc,naux
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_WRITE
          LastParm  = ios
          GOTO 9999
        END IF
        irec = irec + 1
        DO ii = 1,2*nc,SAG_RECL
          j1 = MIN(ii+SAG_RECL-1,2*nc)
          WRITE(grd%nunit,REC=irec,IOSTAT=ios) (auxID(j),j=ii,j1)
          IF( ios /= 0 )THEN
            LastError = SAG_ERR_WRITE
            LastParm  = ios
            GOTO 9999
          END IF
          irec = irec + 1
        END DO
        DO ii = 1,naux,SAG_RECL
          j1 = MIN(ii+SAG_RECL-1,naux)
          WRITE(grd%nunit,REC=irec,IOSTAT=ios) (auxdata(j),j=ii,j1)
          IF( ios /= 0 )THEN
            LastError = SAG_ERR_WRITE
            LastParm  = ios
            GOTO 9999
          END IF
          irec = irec + 1
        END DO
      ELSE                                           !alloc but not nc > 0
        WRITE(grd%nunit,REC=irec,IOSTAT=ios) nc,naux
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_WRITE
          LastParm  = ios
          GOTO 9999
        END IF
        irec = irec + 1
      END IF
    ELSE                                             !not alloc
        WRITE(grd%nunit,REC=irec,IOSTAT=ios) nc,naux
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_WRITE
          LastParm  = ios
          GOTO 9999
        END IF
        irec = irec + 1
    END IF
    IF( ALLOCATED(auxID)   )DEALLOCATE( auxID,STAT=ios )
    IF( ALLOCATED(auxdata) )DEALLOCATE( auxdata,STAT=ios )
  END DO
END IF

SAG_WriteAuxDataID = SAG_OK

grd%record = irec
grd%status = SAG_HEAD_REC

9999 CONTINUE
IF( ALLOCATED(auxID)   )DEALLOCATE( auxID,STAT=ios )
IF( ALLOCATED(auxdata) )DEALLOCATE( auxdata,STAT=ios )

RETURN
END
!*******************************************************************************
!               SAG_WriteBreakID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteBreakID( grdI,time )

!     Reads a surface grid file data

USE sagstr_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
REAL,    INTENT( IN ) :: time

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ivar,i

INTEGER, EXTERNAL :: SAG_WriteHeaderID,SAG_WriteDataID
INTEGER, EXTERNAL :: SAG_WriteGridID,SAG_WriteAuxDataID,SAG_WriteEOFID

!==== Initialize

SAG_WriteBreakID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Set timebreak

grd%time = time

!==== Write Header

i = SAG_WriteHeaderID( grdI )
IF( i /= SAG_OK )THEN
  SAG_WriteBreakID = i
  GOTO 9999
END IF

!==== Write Grid

i = SAG_WriteGridID( grdI )
IF( i /= SAG_OK )THEN
  SAG_WriteBreakID = i
  GOTO 9999
END IF

!==== Write All Data

DO ivar = 1,grd%nvart-1
  i = SAG_WriteDataID( grdI,ivar,ivar )
  IF( i /= SAG_OK )THEN
    SAG_WriteBreakID = i
    GOTO 9999
  END IF
END DO

ivar = grd%nvart
i = SAG_WriteDataID( grdI,-ivar,ivar )
IF( i /= SAG_OK )THEN
  SAG_WriteBreakID = i
  GOTO 9999
END IF

!==== Write Auxiliary Data

i = SAG_WriteAuxDataID( grdI )
IF( i /= SAG_OK )THEN
  SAG_WriteBreakID = i
  GOTO 9999
END IF

!==== Write EOF

i = SAG_WriteEOFID( grdI )
IF( i /= SAG_OK )THEN
  SAG_WriteBreakID = i
  GOTO 9999
END IF

!==== return

SAG_WriteBreakID = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_WriteEOFID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteEOFID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Writes a surface grid file EOF header

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios,nvs,i,mlev,iver
LOGICAL lOpen

REAL, DIMENSION(SAG_HWRDS) :: dath

CHARACTER(4), DIMENSION(:), POINTER :: cnam

SAG_WriteEOFID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE(FILE=grd%file,OPENED=lOpen,NUMBER=i)
IF( .NOT.lOpen )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit number is correct

IF( i /= grd%nunit )THEN
  LastError = SAG_ERR_CLOSED
  LastParm  = i
  GOTO 9999
END IF

!==== Check position record pointer

IF( grd%status /= SAG_HEAD_REC )THEN
  LastError = SAG_ERR_WRTPOS
  LastParm  = SAG_HEAD_REC
  GOTO 9999
END IF

!==== Prepare header data

dath(SAG_HEAD_TIME) = grd%time
dath(SAG_HEAD_CELL) = TRANSFER(0,dath(1))
dath(SAG_HEAD_NX)   = TRANSFER(grd%nx,dath(1))
dath(SAG_HEAD_NY)   = TRANSFER(grd%ny,dath(1))
dath(SAG_HEAD_XMIN) = grd%xmin
dath(SAG_HEAD_YMIN) = grd%ymin
dath(SAG_HEAD_DX)   = grd%dx
dath(SAG_HEAD_DY)   = grd%dy

iver = grd%version
nvs  = grd%nvart
mlev = grd%maxlev

IF( grd%ftype == 0 )THEN
  IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
    LastError = SAG_ERR_IPNAM
    LastParm  = 0
    GOTO 9999
  END IF
  cnam => grd%ipnam
END IF

!==== Write header

IF( grd%ftype == 0 )THEN
  WRITE(grd%nunit,REC=grd%record,IOSTAT=ios) &
               (dath(i),i=1,SAG_HWRDS),nvs,(cnam(i),i=1,nvs),iver,mlev
ELSE
  WRITE(grd%nunit,REC=grd%record,IOSTAT=ios) (dath(i),i=1,SAG_HWRDS),mlev
END IF
IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

!==== Set return value

SAG_WriteEOFID = SAG_OK

9999 CONTINUE

RETURN
END

