!*******************************************************************************
!                SAG_FindTime
!*******************************************************************************
INTEGER FUNCTION SAG_FindTime( grdI,Tin )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Point to record with the neareset time

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
REAL,    INTENT( IN ) :: Tin !Desired Time

TYPE ( SAGgrid_str ), POINTER  :: grd

INTEGER nunit !Logical Unit
INTEGER irec  !record pointer
INTEGER ios
INTEGER ir,iro,ncell,idum,nvar,nskip,i,nc,nd
REAL    dum,tx,txo

INTEGER, EXTERNAL :: SAG_OpenID

SAG_FindTime = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check file

IF( grd%status == SAG_CLOSED )THEN
  ir = SAG_OpenID( grdI )
  IF( ir /= SAG_OK )THEN
    SAG_FindTime = ir
    GOTO 9999
  END IF
END IF

nunit = grd%nunit

!---- Initialize return values

irec = 0

!---- Initialize record pointer

IF( grd%ftype == 0 )THEN
  ir = 1
ELSE
  ir = grd%ftype + 3 + grd%nvart/SAG_RECL
END IF

!---- Read first header

IF( grd%ftype == 0 )THEN
  READ(nunit,rec=ir,IOSTAT=ios) tx,ncell,idum,idum,dum,dum,dum,dum,nvar
ELSE
  READ(nunit,rec=ir,IOSTAT=ios) tx, ncell
  nvar = grd%nvart
END IF

IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

txo = tx
iro = ir

!---- Loop over additional time breaks until find one greater than desired time

DO WHILE( ncell > 0 .AND. tx < Tin )

  nskip = ((ncell-1)/SAG_RECL + 1)*(nvar+1) + 1 !No. of data records
  iro   = ir                                    !Save current pointer
  txo   = tx                                    !Save current time
  ir    = ir + nskip                            !Update pointer to skip data
  IF( grd%naux > 0 )THEN
    DO i = 1,grd%naux
      READ(nunit,REC=ir,IOSTAT=ios)nc,nd
      ir = ir + 1
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_READ
        LastParm  = ios
        GOTO 9999
      END IF
      IF( nc > 0 )THEN
        nskip = (2*nc-1)/SAG_RECL + 1 + (nd-1)/SAG_RECL + 1
        ir    = ir + nskip                      !Update pointer to skip data
      END IF
    END DO
  END IF
  READ(nunit,REC=ir,IOSTAT=ios)tx, ncell
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF

END DO

!---- Choose Closest time

IF( ABS(tx-Tin) < ABS(txo-Tin) .AND. ncell > 0 )THEN
  irec = ir
ELSE
  irec = iro
END IF

SAG_FindTime = SAG_OK
grd%record   = irec
grd%status   = SAG_HEAD_REC

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_LastTime
!*******************************************************************************
INTEGER FUNCTION SAG_LastTime( grdI,Tout,append )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Point to record with the last time

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: grdI
REAL,    INTENT( OUT ) :: Tout !Last Time
LOGICAL, INTENT(  IN ) :: append

TYPE ( SAGgrid_str ), POINTER :: grd

INTEGER nunit !Logical Unit
INTEGER irec  !record pointer
INTEGER ios,i,nc,nd
INTEGER ir,iro,ncell,idum,nvar,nskip
REAL    dum,tx,txo

INTEGER, EXTERNAL :: SAG_OpenID

SAG_LastTime = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check file

IF( grd%status == SAG_CLOSED )THEN
  ir = SAG_OpenID( grdI )
  IF( ir /= SAG_OK )THEN
    SAG_LastTime = ir
    GOTO 9999
  END IF
END IF

nunit = grd%nunit

!---- Initialize return values

irec = 0

!---- Initialize record pointer

IF( grd%ftype == 0 )THEN
  ir = 1
ELSE
  ir = grd%ftype + 3 + grd%nvart/SAG_RECL
END IF

!---- Read first header

IF( grd%ftype == 0 )THEN
  READ(nunit,REC=ir,IOSTAT=ios) tx,ncell,idum,idum,dum,dum,dum,dum,nvar
ELSE
  READ(nunit,REC=ir,IOSTAT=ios) tx, ncell
  nvar = grd%nvart
END IF

IF( ios /= 0 )THEN
  LastError = SAG_ERR_READ
  LastParm  = ios
  GOTO 9999
END IF

txo = tx
iro = ir

!---- Loop over time breaks until eof

DO WHILE( ncell > 0 )

  nskip = ((ncell-1)/SAG_RECL + 1)*(nvar+1) + 1 !No. of data records
  iro   = ir                                    !Save current pointer
  txo   = tx                                    !Save current time
  ir    = ir + nskip                            !Update pointer to skip data
  IF( grd%naux > 0 )THEN
    DO i = 1,grd%naux
      READ(nunit,REC=ir,IOSTAT=ios)nc,nd
      ir = ir + 1
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_READ
        LastParm  = ios
        GOTO 9999
      END IF
      IF( nc > 0 )THEN
        nskip = (2*nc-1)/SAG_RECL + 1 + (nd-1)/SAG_RECL + 1
        ir    = ir + nskip                      !Update pointer to skip data
      END IF
    END DO
  END IF
  READ(nunit,rec=ir,IOSTAT=ios)tx,ncell
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_READ
    LastParm  = ios
    GOTO 9999
  END IF

END DO

Tout = txo

SAG_LastTime = SAG_OK
IF( append )THEN
  grd%record = ir
ELSE
  grd%record = iro
END IF
grd%status   = SAG_HEAD_REC

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FindVariableID
!*******************************************************************************
INTEGER FUNCTION SAG_FindVariableID( grdI,name,ivar )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

! Find field for variable name (or block name:variable name)

IMPLICIT NONE

INTEGER,      INTENT(  IN ) :: grdI
CHARACTER(*), INTENT(  IN ) :: name
INTEGER,      INTENT( OUT ) :: ivar

TYPE ( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_FindVariable( grd,name,ivar )
    USE sagstr_fd
    TYPE ( SAGgrid_str ), POINTER       :: grd
    CHARACTER(*),         INTENT(  IN ) :: name
    INTEGER,              INTENT( OUT ) :: ivar
  END FUNCTION SAG_FindVariable
END INTERFACE

SAG_FindVariableID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_FindVariableID = SAG_FindVariable( grd,name,ivar )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_FindVariable( grd,name,ivar )

USE sagstr_fd

!     Point to next grid record

IMPLICIT NONE

TYPE ( SAGgrid_str ), POINTER       :: grd
INTEGER,              INTENT( OUT ) :: ivar
CHARACTER(*),         INTENT( IN  ) :: name

INTERFACE
  INTEGER FUNCTION SAG_FindOldVar( grd,name,ivar )
    USE sagstr_fd
    TYPE ( SAGgrid_str ), POINTER       :: grd
    CHARACTER(*),         INTENT( IN )  :: name
    INTEGER,              INTENT( OUT ) :: ivar
  END FUNCTION SAG_FindOldVar

  INTEGER FUNCTION SAG_FindBlockVar( grd,name,ivar )
    USE sagstr_fd
    TYPE ( SAGgrid_str ), POINTER       :: grd
    CHARACTER(*),         INTENT( IN )  :: name
    INTEGER,              INTENT( OUT ) :: ivar
  END FUNCTION SAG_FindBlockVar
END INTERFACE

IF( grd%ftype == 0 )THEN
  SAG_FindVariable = SAG_FindOldVar( grd,name,ivar )
ELSE
  SAG_FindVariable = SAG_FindBlockVar( grd,name,ivar )
END IF

RETURN
END
!*******************************************************************************
!                SAG_FindOldVar
!*******************************************************************************
INTEGER FUNCTION SAG_FindOldVar( grd,name,ivar )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Point to next grid record

IMPLICIT NONE

TYPE ( SAGgrid_str ), POINTER       :: grd
CHARACTER(*),         INTENT(  IN ) :: name
INTEGER,              INTENT( OUT ) :: ivar

INTEGER i,ir
CHARACTER(4) search_name,file_name

SAG_FindOldVar = SAG_ERROR

IF( grd%ftype /= 0 )THEN
  LastError = SAG_ERR_FTYPE
  LastParm  = grd%ftype
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  LastError = SAG_ERR_IPNAM
  LastParm  = 0
  GOTO 9999
END IF
IF( grd%nvart <= 0 )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = 0
  GOTO 9999
END IF

search_name = TRIM(ADJUSTL(name))
CALL SAG_cupper( search_name )

ivar = 0
i    = 0
ir   = SAG_OK
DO WHILE( ivar == 0 .AND. i < grd%nvart .AND. ir == SAG_OK )
  i = i + 1
  file_name = TRIM(ADJUSTL(grd%ipnam(i)))
  CALL SAG_cupper( file_name )
  IF( TRIM(search_name) == TRIM(file_name) )ivar = i
END DO
IF( ivar <= 0 )THEN
  LastError = SAG_ERR_NOTFOUND
  LastParm  = 0
  GOTO 9999
END IF

SAG_FindOldVar = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FindBlockVar
!*******************************************************************************
INTEGER FUNCTION SAG_FindBlockVar( grd,name,ivar )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Point to next grid record

IMPLICIT NONE

TYPE ( SAGgrid_str ), POINTER       :: grd
CHARACTER(*),         INTENT(  IN ) :: name
INTEGER,              INTENT( OUT ) :: ivar

INTEGER i,jvar,jblk
CHARACTER(4)  search_name,file_name
CHARACTER(64) search_block,file_block

SAG_FindBlockVar = SAG_ERROR

IF( grd%ftype <= 0 )THEN
  LastError = SAG_ERR_FTYPE
  LastParm  = grd%ftype
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  LastError = SAG_ERR_IPNAM
  LastParm  = 0
  GOTO 9999
END IF
IF( grd%nvart <= 0 )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = 0
  GOTO 9999
END IF
IF( .NOT.ASSOCIATED( grd%ipblk ) )THEN
  LastError = SAG_ERR_IPBLK
  LastParm  = 0
  GOTO 9999
END IF

i = INDEX(name,':',BACK=.TRUE.)
IF( i <= 1 .OR. i >= LEN_TRIM(name) )THEN
  LastError = SAG_ERR_BLKNAM
  LastParm  = 0
  GOTO 9999
END IF

search_block = TRIM(ADJUSTL(name(1:i-1)))
search_name  = TRIM(ADJUSTL(name(i+1:)))
CALL SAG_cupper( search_name )
CALL SAG_cupper( search_block )

jblk = 0
i    = 0
DO WHILE( jblk == 0 .AND. i < grd%ftype )
  i = i + 1
  file_block = TRIM(ADJUSTL(grd%ipblk(i)%name))
  CALL SAG_cupper( file_block )
  IF( TRIM(search_block) == TRIM(file_block) )jblk = i
END DO

IF( jblk <= 0 )THEN
  LastError = SAG_ERR_NOTFOUND
  LastParm  = 0
  GOTO 9999
END IF

jvar = 0
i    = 0
DO WHILE( jvar == 0 .AND. i < grd%ipblk(jblk)%nfld )
  i = i + 1
  file_name = TRIM(ADJUSTL(grd%ipnam(grd%ipblk(jblk)%ifld+i-1)))
  CALL SAG_cupper( file_name )
  IF( TRIM(search_name) == TRIM(file_name) )jvar = i
END DO

IF( jvar <= 0 )THEN
  LastError = SAG_ERR_NOTFOUND
  LastParm  = 0
  GOTO 9999
END IF

ivar = grd%ipblk(jblk)%ifld + jvar - 1

SAG_FindBlockVar = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FindHeader
!*******************************************************************************
INTEGER FUNCTION SAG_FindHeader( grd )

USE SAGSkip

!     Point to next header record

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

INTERFACE
  INTEGER FUNCTION SAG_Open( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Open
END INTERFACE

SAG_FindHeader = SAG_ERROR

SELECT CASE( grd%status )
  CASE( SAG_CLOSED )
    i = SAG_Open( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindHeader = i
      GOTO 9999
    END IF

  CASE( SAG_HEAD_REC )

  CASE( SAG_GRID_REC )
    i = SAG_SkipGrid( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindHeader = i
      GOTO 9999
    END IF
    i = SAG_SkipData( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindHeader = i
      GOTO 9999
    END IF

  CASE( SAG_DATA_REC )
    i = SAG_SkipData( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindHeader = i
      GOTO 9999
    END IF

  CASE( SAG_AUX_REC )
    i = SAG_SkipAuxData( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindHeader = i
      GOTO 9999
    END IF

  CASE DEFAULT
    LastError = SAG_ERR_STATUS
    LastParm  = grd%status
    GOTO 9999

END SELECT

IF( grd%status /= SAG_HEAD_REC )THEN
  LastError = SAG_ERR_FIND
  LastParm  = SAG_HEAD_REC
  GOTO 9999
END IF

SAG_FindHeader = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FindGrid
!*******************************************************************************
INTEGER FUNCTION SAG_FindGrid( grd )

USE SAGSkip

!     Point to next grid record

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

INTERFACE
  INTEGER FUNCTION SAG_Open( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Open
END INTERFACE

SAG_FindGrid = SAG_ERROR

SELECT CASE( grd%status )
  CASE( SAG_CLOSED )
    i = SAG_Open( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindGrid = i
      GOTO 9999
    END IF
    i = SAG_SkipHeader(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindGrid = i
      GOTO 9999
    END IF

  CASE( SAG_HEAD_REC )
    i = SAG_SkipHeader(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindGrid = i
      GOTO 9999
    END IF

  CASE( SAG_GRID_REC )

  CASE( SAG_DATA_REC )
    i = SAG_SkipData(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindGrid = i
      GOTO 9999
    END IF

  CASE DEFAULT
    LastError = SAG_ERR_STATUS
    LastParm  = grd%status
    GOTO 9999

END SELECT

IF( grd%status /= SAG_GRID_REC )THEN
  LastError = SAG_ERR_FIND
  LastParm  = SAG_GRID_REC
  GOTO 9999
END IF

SAG_FindGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FindData
!***************************FindData****************************************************
INTEGER FUNCTION SAG_FindData( grd )

USE SAGSkip

!     Point to next data record

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

INTERFACE
  INTEGER FUNCTION SAG_Open( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Open
END INTERFACE

SAG_FindData = SAG_ERROR

SELECT CASE( grd%status )
  CASE( SAG_CLOSED )
    i = SAG_Open( grd )
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF
    i = SAG_SkipHeader(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF
    i = SAG_SkipGrid(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF

  CASE( SAG_HEAD_REC )
    i = SAG_SkipHeader(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF
    i = SAG_SkipGrid(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF

  CASE( SAG_GRID_REC )
    i = SAG_SkipGrid(grd)
    IF( i /= SAG_OK )THEN
      SAG_FindData = i
      GOTO 9999
    END IF

  CASE( SAG_DATA_REC )

  CASE DEFAULT
    LastError = SAG_ERR_STATUS
    LastParm  = grd%status
    GOTO 9999

END SELECT

IF( grd%status /= SAG_DATA_REC )THEN
  LastError = SAG_ERR_FIND
  LastParm  = SAG_DATA_REC
  GOTO 9999
END IF

SAG_FindData = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_cupper
!*******************************************************************************
SUBROUTINE SAG_cupper( line )

IMPLICIT NONE

CHARACTER(*) line

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) >= 'a'.AND. line(i:i) <= 'z' )THEN
    line(i:i) = CHAR(ICHAR(line(i:i)) - 32)
  END IF
END DO

RETURN
END
