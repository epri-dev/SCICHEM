!*******************************************************************************
!               SAG_CellCountID
!*******************************************************************************
INTEGER FUNCTION SAG_CellCountID( grdI )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellCount( grd  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_CellCount
END INTERFACE

SAG_CellCountID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellCountID = SAG_CellCount( grd )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellCount
!*******************************************************************************
INTEGER FUNCTION SAG_CellCount( grd )

USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

SAG_CellCount = grd%ncells

RETURN
END
!*******************************************************************************
!               SAG_CellMaxID
!*******************************************************************************
INTEGER FUNCTION SAG_CellMaxID( grdI,ifld,fldmx )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: grdI       ! Grid ID
INTEGER, INTENT( IN  ) :: ifld
REAL,    INTENT( OUT ) :: fldmx

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellMax( grd,ifld,fldmx  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER       :: grd
    INTEGER,             INTENT( IN  ) :: ifld
    REAL,                INTENT( OUT ) :: fldmx
  END FUNCTION SAG_CellMax
END INTERFACE

SAG_CellMaxID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellMaxID = SAG_CellMax( grd,ifld,fldmx )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellMax
!*******************************************************************************
INTEGER FUNCTION SAG_CellMax( grd,ifld,fldmx )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagmaxval_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER       :: grd
INTEGER,             INTENT( IN  ) :: ifld
REAL,                INTENT( OUT ) :: fldmx

INTEGER, EXTERNAL :: SAG_MaxValue

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellMax = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

ifld_maxval = ifld
out_maxval  = -1.E+36
out_minval  =  1.E+36

SAG_CellMax = SAG_CellFunction( grd,SAG_MaxValue )
IF( SAG_CellMax == SAG_OK )THEN
  fldmx = out_maxval
ELSE
  fldmx = 0.
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddID
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddID( grdI,ifld,jfld,mult )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ifld
INTEGER, INTENT( IN ) :: jfld
REAL,    INTENT( IN ) :: mult

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellAdd( grd,ifld,jfld,mult  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: jfld
    REAL,                INTENT( IN ) :: mult
  END FUNCTION SAG_CellAdd
END INTERFACE

SAG_CellAddID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellAddID = SAG_CellAdd( grd,ifld,jfld,mult )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAdd
!*******************************************************************************
INTEGER FUNCTION SAG_CellAdd( grd,ifld,jfld,mult )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagadd_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: jfld
REAL,                INTENT( IN ) :: mult

INTEGER, EXTERNAL :: SAG_Add

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellAdd = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = jfld
  GOTO 9999
END IF

ifld_add  = ifld
jfld_add  = jfld
scale_add = mult

SAG_CellAdd = SAG_CellFunction( grd,SAG_Add )

9999 CONTINUE

RETURN
  END
!*******************************************************************************
!               SAG_CellAddVarianceID
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddVarianceID( grdI,ifld,jfld,mult )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ifld
INTEGER, INTENT( IN ) :: jfld
REAL,    INTENT( IN ) :: mult

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellAddVariance( grd,ifld,jfld,mult  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: jfld
    REAL,                INTENT( IN ) :: mult
  END FUNCTION SAG_CellAddVariance
END INTERFACE

SAG_CellAddVarianceID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellAddVarianceID = SAG_CellAddVariance( grd,ifld,jfld,mult )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddVariance
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddVariance( grd,ifld,jfld,mult )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagadd_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: jfld
REAL,                INTENT( IN ) :: mult

INTEGER, EXTERNAL :: SAG_AddVariance

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellAddVariance = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = jfld
  GOTO 9999
END IF

ifld_add  = ifld
jfld_add  = jfld
scale_add = mult

SAG_CellAddVariance = SAG_CellFunction( grd,SAG_AddVariance )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddLimitID
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddLimitID( grdI,ifld,jfld,mult,minvalue,maxvalue )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ifld
INTEGER, INTENT( IN ) :: jfld
REAL,    INTENT( IN ) :: mult
REAL,    INTENT( IN ) :: minvalue
REAL,    INTENT( IN ) :: maxvalue

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellAddLimit( grd,ifld,jfld,mult,minvalue,maxvalue  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: jfld
    REAL,                INTENT( IN ) :: mult
    REAL,                INTENT( IN ) :: minvalue
    REAL,                INTENT( IN ) :: maxvalue
  END FUNCTION SAG_CellAddLimit
END INTERFACE

SAG_CellAddLimitID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellAddLimitID = SAG_CellAddLimit( grd,ifld,jfld,mult,minvalue,maxvalue )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddLimit
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddLimit( grd,ifld,jfld,mult,minvalue,maxvalue )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagadd_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: jfld
REAL,                INTENT( IN ) :: mult
REAL,                INTENT( IN ) :: minvalue
REAL,                INTENT( IN ) :: maxvalue

INTEGER, EXTERNAL :: SAG_AddLimit

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellAddLimit = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = jfld
  GOTO 9999
END IF

ifld_add  = ifld
jfld_add  = jfld
scale_add = mult
min_value = minvalue
max_value = maxvalue

SAG_CellAddLimit = SAG_CellFunction( grd,SAG_AddLimit )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddVarianceLimitID
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddVarianceLimitID( grdI,ifld,jfld,mult,minvalue,maxvalue )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ifld
INTEGER, INTENT( IN ) :: jfld
REAL,    INTENT( IN ) :: mult
REAL,    INTENT( IN ) :: minvalue
REAL,    INTENT( IN ) :: maxvalue

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellAddVarianceLimit( grd,ifld,jfld,mult,minvalue,maxvalue  )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: jfld
    REAL,                INTENT( IN ) :: mult
    REAL,                INTENT( IN ) :: minvalue
    REAL,                INTENT( IN ) :: maxvalue
  END FUNCTION SAG_CellAddVarianceLimit
END INTERFACE

SAG_CellAddVarianceLimitID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellAddVarianceLimitID = SAG_CellAddVarianceLimit( grd,ifld,jfld,mult,minvalue,maxvalue )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellAddVarianceLimit
!*******************************************************************************
INTEGER FUNCTION SAG_CellAddVarianceLimit( grd,ifld,jfld,mult,minvalue,maxvalue )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagadd_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: jfld
REAL,                INTENT( IN ) :: mult
REAL,                INTENT( IN ) :: minvalue
REAL,                INTENT( IN ) :: maxvalue

INTEGER, EXTERNAL :: SAG_AddVarianceLimit

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellAddVarianceLimit = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = jfld
  GOTO 9999
END IF

ifld_add  = ifld
jfld_add  = jfld
scale_add = mult
min_value = minvalue
max_value = maxvalue

SAG_CellAddVarianceLimit = SAG_CellFunction( grd,SAG_AddVarianceLimit )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellCopyID
!*******************************************************************************
INTEGER FUNCTION SAG_CellCopyID( grdI,ifld,jfld )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Reads a surface grid file data

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI       ! Grid ID
INTEGER, INTENT( IN ) :: ifld
INTEGER, INTENT( IN ) :: jfld

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellCopy( grd,ifld,jfld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: jfld
  END FUNCTION SAG_CellCopy
END INTERFACE

SAG_CellCopyID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CellCopyID = SAG_CellCopy( grd,ifld,jfld )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellCopy
!*******************************************************************************
INTEGER FUNCTION SAG_CellCopy( grd,ifld,jfld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagadd_usr

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: jfld

INTEGER, EXTERNAL :: SAG_Copy

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_CellCopy = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( jfld <= 0 .OR. jfld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = jfld
  GOTO 9999
END IF

ifld_add  = ifld
jfld_add  = jfld

SAG_CellCopy = SAG_CellFunction( grd,SAG_Copy )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_SimpleCell
!*******************************************************************************
INTEGER FUNCTION SAG_SimpleCell( grdI,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
INTEGER, EXTERNAL     :: UserFunction

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTERFACE
      INTEGER FUNCTION UserFunction( dat,mx,p )
        USE sagcel_fd
        INTEGER,             INTENT( IN ) :: mx
        REAL, DIMENSION(:),  POINTER      :: dat
        TYPE( SAGcell_str ), INTENT( IN ) :: p
      END FUNCTION UserFunction
    END INTERFACE
  END FUNCTION SAG_CellFunction
END INTERFACE

SAG_SimpleCell = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipdat) )THEN
  LastError = SAG_ERR_IPDAT
  LastParm  = 0
  GOTO 9999
END IF

SAG_SimpleCell = SAG_CellFunction( grd,UserFunction )

9999 CONTINUE

RETURN
END
