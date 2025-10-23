!*******************************************************************************
!               SAG_BackUpDataID
!*******************************************************************************
INTEGER FUNCTION SAG_BackUpDataID( grdI,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagbck_fi
USE PtrGrdStrItf

!     Dumps bottom level data

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: grdI
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_BackUpData( grd,nfld,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
    INTEGER,               INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
  END FUNCTION SAG_BackUpData
END INTERFACE

!==== Initialize result

SAG_BackUpDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_BackUpDataID = SAG_BackUpData( grd,nfld,ifld )

9999 CONTINUE

RETURN
END

!*******************************************************************************
!               SAG_BackUpData
!*******************************************************************************
INTEGER FUNCTION SAG_BackUpData( grd,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagbck_fi

!     Dumps bottom level data

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER      :: grd
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld

REAL,    DIMENSION(:), POINTER :: dat  !surface data
INTEGER, DIMENSION(:), POINTER :: flag !internal flags array

INTEGER i, i0, j, jfld, ios

SAG_BackUpData = SAG_ERROR

!==== Initial Error checking

IF( nBackUp /= 0 )THEN
  LastError = SAG_ERR_BACK
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED( grd%ipdat ) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

!==== Allocate flag space

ALLOCATE( Bflag(2,nfld),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = 2*nfld
  GOTO 9998
END IF

!==== Loop Over fields

flag => grd%ipflg
dat  => grd%ipdat

!==== Allocate backup data space

ALLOCATE( Bdata(grd%ncells,nfld),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = grd%ncells*nfld
  GOTO 9998
END IF

DO i = 1,nfld

  jfld = ifld(i)
  IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
    LastError = SAG_ERR_IFLD
    LastParm  = jfld
    GOTO 9998
  END IF

  Bflag(1,i) = jfld
  Bflag(2,i) = flag(jfld)

!==== Back up data

  i0 = (jfld-1)*grd%mxgrd
  DO j = 1,grd%ncells
    Bdata(j,i) = dat(i0+j)
  END DO

END DO

!==== Return

nBackUp = nfld
SAG_BackUpData = SAG_OK

9999 CONTINUE

RETURN

9998  CONTINUE
IF( ALLOCATED(Bflag) )DEALLOCATE( Bflag,STAT=ios )
IF( ALLOCATED(Bdata) )DEALLOCATE( Bdata,STAT=ios )
GOTO 9999

END
!*******************************************************************************
!               SAG_RestoreDataID
!*******************************************************************************
INTEGER FUNCTION SAG_RestoreDataID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagbck_fi
USE PtrGrdStrItf

!     Restores bottom level data

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: grdI

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_RestoreData( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
  END FUNCTION SAG_RestoreData
END INTERFACE

!==== Initialize result

SAG_RestoreDataID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_RestoreDataID = SAG_RestoreData( grd )

9999 CONTINUE

RETURN
END

!*******************************************************************************
!               SAG_RestoreData
!*******************************************************************************
INTEGER FUNCTION SAG_RestoreData( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagbck_fi

!     Restores bottom level data

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

REAL,    DIMENSION(:), POINTER :: dat  !surface data
INTEGER, DIMENSION(:), POINTER :: flag !internal flags array

INTEGER i,i0,j,jfld,nRestore,ios

SAG_RestoreData = SAG_ERROR

!==== Initial Error checking

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipflg) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( nBackUp == 0 .AND. .NOT.ALLOCATED(Bdata) )THEN
  GOTO 9998
END IF

IF( .NOT.ALLOCATED(Bdata) )THEN
  LastError = SAG_ERR_BACK
  LastParm  = -1
  GOTO 9999
END IF

!==== Loop Over fields

nRestore = nBackUp

dat  => grd%ipdat
flag => grd%ipflg

DO i = 1,nBackUp

  jfld = Bflag(1,i)

  IF( jfld > 0 .AND. jfld <= grd%mxfld )THEN

    flag(jfld) = Bflag(2,i)

!==== Restore data

    i0 = (jfld-1)*grd%mxgrd
    DO j = 1,grd%ncells
      dat(i0+j) = Bdata(j,i)
    END DO

    nRestore = nRestore - 1

  END IF

END DO

!==== DeAllocate space

DEALLOCATE( Bdata,Bflag,STAT=ios )

nBackUp = 0

!==== Error check

IF( nRestore /= 0 )THEN
  LastError = SAG_ERR_BACK
  LastParm  = nRestore
  GOTO 9999
END IF

!==== Return

9998 CONTINUE

SAG_RestoreData = SAG_OK

9999 CONTINUE

RETURN
END
