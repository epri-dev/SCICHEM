!*******************************************************************************
!               SAG_GetPointVal
!*******************************************************************************
INTEGER FUNCTION SAG_GetPointValID( grdI,x,y,val,UserAddFunction,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: grdI
REAL,               INTENT( IN  ) :: x
REAL,               INTENT( IN  ) :: y
REAL, DIMENSION(*), INTENT( OUT ) :: val
INTEGER,            EXTERNAL      :: UserAddFunction
INTEGER,            EXTERNAL      :: UserFunction

TYPE( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_GetPointVal( grd,x,y,val,UserAddFunction,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER       :: grd
    REAL,                INTENT( IN  ) :: x
    REAL,                INTENT( IN  ) :: y
    REAL, DIMENSION(*),  INTENT( OUT ) :: val
    INTEGER,             EXTERNAL      :: UserAddFunction
    INTEGER,             EXTERNAL      :: UserFunction
  END FUNCTION SAG_GetPointVal
END INTERFACE

!==== Initialize result

SAG_GetPointValID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_GetPointValID = SAG_GetPointVal( grd,x,y,val,UserAddFunction, &
                                                    UserFunction )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_GetPointVal( grd,x,y,val,UserAddFunction,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER       :: grd
REAL,                INTENT( IN  ) :: x
REAL,                INTENT( IN  ) :: y
REAL, DIMENSION(*),  INTENT( OUT ) :: val
INTEGER,             EXTERNAL      :: UserFunction
INTEGER,             EXTERNAL      :: UserAddFunction

REAL, DIMENSION(:), ALLOCATABLE :: field_sum

INTEGER i, ios, i0
REAL    xx, yy

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_SumPointVal( grd,i0,x,y,field_sum,val, &
                                              UserAddFunction,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: grd
    INTEGER,             INTENT( IN    ) :: i0
    REAL,                INTENT( IN    ) :: x
    REAL,                INTENT( IN    ) :: y
    REAL, DIMENSION(*),  INTENT( INOUT ) :: field_sum
    REAL, DIMENSION(*),  INTENT( OUT   ) :: val
    INTEGER,             EXTERNAL        :: UserFunction
    INTEGER,             EXTERNAL        :: UserAddFunction
  END FUNCTION SAG_SumPointVal
END INTERFACE

!------ Initialize result

SAG_GetPointVal = SAG_ERROR

!------ Allocate fields for sums

ALLOCATE( field_sum(grd%nvart),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = grd%nvart
  GOTO 9999
END IF

DO i = 1,grd%nvart
  field_sum(i) = 0.
END DO

xx = (x-grd%xmin)/grd%dx
yy = (y-grd%ymin)/grd%dy

i0 = INT(yy)*grd%nx + INT(xx) + 1

SAG_GetPointVal = SAG_SumPointVal( grd,i0,xx,yy,field_sum,val &
                                  ,UserAddFunction,UserFunction )

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE INTEGER FUNCTION SAG_SumPointVal( grd,i0,x,y,field_sum,val, &
                                            UserAddFunction,UserFunction ) &
                                             RESULT( irv )
USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER         :: grd
INTEGER,             INTENT( IN    ) :: i0
REAL,                INTENT( IN    ) :: x
REAL,                INTENT( IN    ) :: y
REAL, DIMENSION(*),  INTENT( INOUT ) :: field_sum
REAL, DIMENSION(*),  INTENT( OUT   ) :: val
INTEGER,             EXTERNAL        :: UserFunction
INTERFACE
  INTEGER FUNCTION UserAddFunction( grd,icell,field )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: grd
    INTEGER,             INTENT( IN    ) :: icell
    REAL, DIMENSION(*),  INTENT( INOUT ) :: field
  END FUNCTION UserAddFunction
END INTERFACE

INTEGER i1
REAL    xx, yy

irv = UserAddFunction( grd,i0,field_sum )

IF( grd%ipgrd(i0) > 0 )THEN

  xx = 2.*( x - AINT(x) )
  yy = 2.*( y - AINT(y) )

  i1 = grd%ipgrd(i0) + 2*INT(yy) + INT(xx)

  irv = SAG_SumPointVal( grd,i1,xx,yy,field_sum,val,UserAddFunction, &
                                                        UserFunction )

ELSE

  irv = UserFunction( field_sum,grd%nvart,val )
  IF( irv /= SAG_OK )LastError = SAG_ERR_USER

END IF

RETURN
END
