!*******************************************************************************
!                SAG_InitDef
!*******************************************************************************
INTEGER FUNCTION SAG_InitDef()

USE sagdef_fd

!     Initialize a SAG common areas

IMPLICIT NONE

INTEGER i

INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_SetDefaultValue

i = SAG_InitError()
IF( i /= SAG_OK )THEN
  SAG_InitDef = i
  GOTO 9999
END IF

i = SAG_SetDefaultValue( 1.E+36 )
IF( i /= SAG_OK )THEN
  SAG_InitDef = i
  GOTO 9999
END IF

i = SAG_SetSpecialValue( .FALSE.,0.0 )
IF( i /= SAG_OK )THEN
  SAG_InitDef = i
  GOTO 9999
END IF

SAG_InitDef = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_Init
!*******************************************************************************
INTEGER FUNCTION SAG_Init( Dvalue,flag,Svalue )

USE sagdef_fd

!     Initialize a SAG common areas

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: flag
REAL,    INTENT( IN ) :: Svalue
REAL,    INTENT( IN ) :: Dvalue

INTEGER i

INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_SetDefaultValue

i = SAG_InitError()
IF( i /= SAG_OK )THEN
  SAG_Init = i
  GOTO 9999
END IF

i = SAG_SetDefaultValue( Dvalue )
IF( i /= SAG_OK )THEN
  SAG_Init = i
  GOTO 9999
END IF

i = SAG_SetSpecialValue( flag,Svalue )
IF( i /= SAG_OK )THEN
  SAG_Init = i
  GOTO 9999
END IF

SAG_Init = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_InitList
!*******************************************************************************
INTEGER FUNCTION SAG_InitList()

USE sagdef_fd
USE saglst_fd

!  Initialize a SAG linked list of pointers to grid structures

IMPLICIT NONE

NULLIFY( GrdList%First,GrdList%Last )

SAG_InitList = SAG_OK

RETURN
END
!*******************************************************************************
!                SAG_InitGrid
!*******************************************************************************
INTEGER FUNCTION SAG_InitGridID( filename,lun,type,mxgrd,mxfld,mxvar,grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Initialize a SAG grid structure

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: filename
INTEGER,      INTENT( IN ) :: lun
INTEGER,      INTENT( IN ) :: type
INTEGER,      INTENT( IN ) :: mxgrd
INTEGER,      INTENT( IN ) :: mxfld
INTEGER,      INTENT( IN ) :: mxvar
INTEGER,      INTENT( IN ) :: grdI

TYPE ( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_InitGrid( filename,lun,type,mxgrd,mxfld,mxvar,grd )
    USE sagstr_fd
    CHARACTER(*),        INTENT( IN ) :: filename
    INTEGER,             INTENT( IN ) :: lun
    INTEGER,             INTENT( IN ) :: type
    INTEGER,             INTENT( IN ) :: mxgrd
    INTEGER,             INTENT( IN ) :: mxfld
    INTEGER,             INTENT( IN ) :: mxvar
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_InitGrid
END INTERFACE

SAG_InitGridID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = lun
  GOTO 9999
END IF

SAG_InitGridID = SAG_InitGrid( filename,lun,type,mxgrd,mxfld,mxvar,grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_InitGrid( filename,lun,type,mxgrd,mxfld,mxvar,grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG grid structure

IMPLICIT NONE

CHARACTER(*),        INTENT( IN ) :: filename
INTEGER,             INTENT( IN ) :: lun
INTEGER,             INTENT( IN ) :: type
INTEGER,             INTENT( IN ) :: mxgrd
INTEGER,             INTENT( IN ) :: mxfld
INTEGER,             INTENT( IN ) :: mxvar
TYPE( SAGgrid_str ), POINTER      :: grd

INTEGER(8) testValue

INTEGER i, j, ios

SAG_InitGrid = SAG_ERROR

SELECT CASE( type )
  CASE( SAG_GRID_BOTH,SAG_GRID_HORZ,SAG_GRID_VERT,SAG_GRID_NONE )
    grd%type = type
  CASE DEFAULT
    LastError = SAG_ERR_TYPE
    LastParm  = type
    GOTO 9999
END SELECT

IF( LEN_TRIM(filename) > 0 )THEN
  IF( lun <= 0 )THEN
    LastError = SAG_ERR_LUN
    LastParm  = lun
    GOTO 9999
  END IF
  IF( LEN_TRIM(filename) > PATH_MAXLENGTH )THEN
    LastError = SAG_ERR_FSIZE
    LastParm  = LEN_TRIM(filename)
    GOTO 9999
  END IF
END IF

IF( mxgrd > 0 )THEN

  ALLOCATE( grd%ipgrd(mxgrd),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxgrd
    GOTO 9999
  END IF

  IF( mxfld > 0 )THEN
    testValue = KIDINT(DBLE(mxgrd)*DBLE(mxfld))
    IF( testValue .GT. MAX_ALLOWED_I4 )THEN
      LastError = SAG_ERR_INT4
      LastParm  = mxfld
      GOTO 9998
    END IF

    ALLOCATE( grd%ipdat(mxgrd*mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxgrd*mxfld
      GOTO 9998
    END IF

    ALLOCATE( grd%ipflg(mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxfld
      GOTO 9997
    END IF

  ELSE

    NULLIFY( grd%ipdat, grd%ipflg )

  END IF

ELSE

    NULLIFY( grd%ipgrd, grd%ipdat, grd%ipflg )

END IF

IF( mxvar > 0 )THEN
  ALLOCATE( grd%ipnam(mxvar),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxvar
    GOTO 9996
  END IF
ELSE
  IF( ASSOCIATED(grd%ipnam) )NULLIFY( grd%ipnam )
END IF

grd%nunit = lun
grd%file  = TRIM(filename)

grd%mxgrd = mxgrd
grd%mxfld = mxfld
grd%mxnam = mxvar

grd%record = 0
grd%ncells = 0
grd%naux   = 0
grd%nvart  = 0
grd%time   = -1.e+36

IF( LEN_TRIM(filename) <= 0 )THEN
  grd%status = SAG_TEMP
ELSE
  grd%status = SAG_CLOSED
END IF

IF( mxgrd > 0 )THEN
  DO i = 1,mxgrd
    grd%ipgrd(i) = 0
  END DO
  IF( mxfld > 0 )THEN
    DO i = 1,mxfld
      grd%ipflg(i) = SAG_GRID_FULL
      DO j = 1,mxgrd
        grd%ipdat((i-1)*mxgrd+j) = 0.0
      END DO
    END DO
  END IF
END IF

NULLIFY( grd%ipblk )
NULLIFY( grd%aux )

SAG_InitGrid = SAG_OK

9999 CONTINUE

RETURN

9996 IF( ASSOCIATED(grd%ipflg) )DEALLOCATE( grd%ipflg,STAT=ios )
     NULLIFY( grd%ipflg )
9997 IF( ASSOCIATED(grd%ipdat) )DEALLOCATE( grd%ipdat,STAT=ios )
     NULLIFY( grd%ipdat )
9998 IF( ASSOCIATED(grd%ipgrd) )DEALLOCATE( grd%ipgrd,STAT=ios )
     NULLIFY( grd%ipgrd )
GOTO 9999
END
!*******************************************************************************
!                SAG_ReinitGrid
!*******************************************************************************
INTEGER FUNCTION SAG_ReinitGridID( mxgrd,mxfld,mxvar,grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Initialize a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mxgrd
INTEGER, INTENT( IN ) :: mxfld
INTEGER, INTENT( IN ) :: mxvar
INTEGER, INTENT( IN ) :: grdI

TYPE ( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_ReinitGrid( mxgrd,mxfld,mxvar,grd )
    USE sagstr_fd
    INTEGER,             INTENT( IN ) :: mxgrd
    INTEGER,             INTENT( IN ) :: mxfld
    INTEGER,             INTENT( IN ) :: mxvar
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_ReinitGrid
END INTERFACE

SAG_ReinitGridID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReinitGridID = SAG_ReinitGrid( mxgrd,mxfld,mxvar,grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ReinitGrid( mxgrd,mxfld,mxvar,grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG grid structure

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: mxgrd
INTEGER,             INTENT( IN ) :: mxfld
INTEGER,             INTENT( IN ) :: mxvar
TYPE( SAGgrid_str ), POINTER      :: grd

CHARACTER(PATH_MAXLENGTH) filename

INTEGER lun
INTEGER type

INTEGER last_record, last_status

INTEGER irv

INTERFACE
  INTEGER FUNCTION SAG_InitGrid( filename,lun,type,mxgrd,mxfld,mxvar,grd )
    USE sagstr_fd
    CHARACTER(*),        INTENT( IN ) :: filename
    INTEGER,             INTENT( IN ) :: lun
    INTEGER,             INTENT( IN ) :: type
    INTEGER,             INTENT( IN ) :: mxgrd
    INTEGER,             INTENT( IN ) :: mxfld
    INTEGER,             INTENT( IN ) :: mxvar
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_InitGrid

  INTEGER FUNCTION SAG_FreeGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_FreeGrid

  INTEGER FUNCTION SAG_Open( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Open
END INTERFACE

SAG_ReinitGrid = SAG_ERROR

type = grd%type
lun  = grd%nunit
filename = grd%file

last_record = grd%record
last_status = grd%status

irv = SAG_FreeGrid( grd )
IF( irv /= SAG_OK )GOTO 9999

irv = SAG_InitGrid( filename,lun,type,mxgrd,mxfld,mxvar,grd )
IF( irv /= SAG_OK )GOTO 9999

IF( last_status /= SAG_TEMP )THEN
  irv = SAG_Open( grd )
  IF( irv /= SAG_OK )GOTO 9999
  grd%record = last_record
  grd%status = last_status
END IF

SAG_ReinitGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FreeGrid
!*******************************************************************************
INTEGER FUNCTION SAG_FreeGridID( grdI )

USE sagdef_fd
USE sagerr_fd
USE saglst_fd
USE sagerr_fi
USE PtrGrdStrItf

!  Deallocate memory for a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_FreeGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_FreeGrid
END INTERFACE

SAG_FreeGridID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_FreeGridID = SAG_FreeGrid( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_FreeGrid( grd )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi

!  Deallocate memory for a SAG grid structure

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i, j, alloc_stat, mx

INTERFACE
  INTEGER FUNCTION SAG_Close( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Close
END INTERFACE

IF( ASSOCIATED(grd%ipgrd) )THEN
  DEALLOCATE( grd%ipgrd,STAT=alloc_stat )
  NULLIFY( grd%ipgrd )
END IF

IF( ASSOCIATED(grd%ipdat) )THEN
  DEALLOCATE( grd%ipdat,STAT=alloc_stat )
  NULLIFY( grd%ipdat )
END IF

IF( ASSOCIATED(grd%ipnam) )THEN
  DEALLOCATE( grd%ipnam,STAT=alloc_stat )
  NULLIFY( grd%ipnam )
END IF

IF( ASSOCIATED(grd%ipblk) )THEN
  DO i = 1,SIZE(grd%ipblk)
    IF( ASSOCIATED(grd%ipblk(i)%fldnam) )THEN
      DEALLOCATE( grd%ipblk(i)%fldnam,STAT=alloc_stat )
      NULLIFY( grd%ipblk(i)%fldnam )
    END IF
  END DO
  DEALLOCATE( grd%ipblk,STAT=alloc_stat )
  NULLIFY( grd%ipblk )
END IF

IF( ASSOCIATED(grd%ipflg) )THEN
  DEALLOCATE( grd%ipflg,STAT=alloc_stat )
  NULLIFY( grd%ipflg )
END IF

IF( grd%naux > 0 )THEN
  IF( ASSOCIATED(grd%aux) )THEN
    DO i = 1,SIZE(grd%aux)
      IF( grd%aux(i)%alloc )THEN
        IF( ASSOCIATED(grd%aux(i)%srf_data) )THEN
          mx = SIZE(grd%aux(i)%srf_data)
          DO j = 1,mx
            IF( ASSOCIATED(grd%aux(i)%srf_data(j)%data) )THEN
              DEALLOCATE( grd%aux(i)%srf_data(j)%data,STAT=alloc_stat )
              NULLIFY( grd%aux(i)%srf_data(j)%data )
            END IF
          END DO
          DEALLOCATE( grd%aux(i)%srf_data,STAT=alloc_stat )
          NULLIFY( grd%aux(i)%srf_data )
        END IF
      END IF
    END DO
    DEALLOCATE( grd%aux,STAT=alloc_stat )
    NULLIFY( grd%aux )
  END IF
  grd%naux = 0
END IF

!------ Close associated file, if open

IF( grd%status /= SAG_TEMP )THEN
  SAG_FreeGrid =  SAG_Close( grd )
ELSE
  SAG_FreeGrid =  SAG_OK
END IF

RETURN
END
!*******************************************************************************
!                SAG_ClearGridID
!*******************************************************************************
INTEGER FUNCTION SAG_ClearGridID( grdI )

USE sagdef_fd
USE sagerr_fd
USE saglst_fd
USE sagerr_fi
USE PtrGrdStrItf

!  Clear grids for a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_ClearGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_ClearGrid
END INTERFACE

SAG_ClearGridID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ClearGridID = SAG_ClearGrid( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_ClearGrid( grd )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi

!  Clear grids for a SAG grid structure

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i, j, mx

IF( ASSOCIATED(grd%ipgrd) )THEN
  DO i = 1,SIZE(grd%ipgrd)
    grd%ipgrd(i) = 0
  END DO
END IF

IF( ASSOCIATED(grd%ipdat) )THEN
  DO i = 1,SIZE(grd%ipdat)
    grd%ipdat(i) = 0.
  END DO
END IF

grd%ncells = grd%nx*grd%ny

IF( grd%naux > 0 )THEN
  IF( ASSOCIATED(grd%aux) )THEN
    DO i = 1,SIZE(grd%aux)
      IF( grd%aux(i)%alloc )THEN
        IF( ASSOCIATED(grd%aux(i)%srf_data) )THEN
          mx = SIZE(grd%aux(i)%srf_data)
          DO j = 1,mx
            IF( ASSOCIATED(grd%aux(i)%srf_data(j)%data) )THEN
              grd%aux(i)%srf_data(j)%data = 0.
            END IF
          END DO
        END IF
      END IF
    END DO
  END IF
END IF

grd%maxlev = -99

SAG_ClearGrid =  SAG_OK

RETURN
END
