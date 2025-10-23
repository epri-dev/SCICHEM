MODULE getcellval_itrf
  INTERFACE
    SUBROUTINE GetCellValue( p,xc,yc,grd,nfld,ifld )
      USE sagstr_fd
      USE cellstr_fd
      TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
      REAL,                  INTENT( OUT   ) :: xc,yc        !Location
      TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
      INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
      INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
    END SUBROUTINE GetCellValue

    SUBROUTINE GetBottomCellValue( p,xc,yc,grd,nfld,ifld )
      USE sagstr_fd
      USE cellstr_fd
      TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
      REAL,                  INTENT( OUT   ) :: xc,yc        !Location
      TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
      INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
      INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
    END SUBROUTINE GetBottomCellValue
  END INTERFACE
END MODULE getcellval_itrf

!============================================================================

MODULE setpoints_itrf

INTERFACE

  SUBROUTINE SetBottomPoints( p,grd,nfld,ifld,GetCell )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
    TYPE( SAGgrid_str ),                 POINTER         :: grd
    INTEGER,                             INTENT( IN    ) :: nfld !Number of fields
    INTEGER, DIMENSION(*),               INTENT( IN    ) :: ifld !Field numbers
    EXTERNAL                                             :: GetCell
  END SUBROUTINE SetBottomPoints

  SUBROUTINE SetTopPoints( p,grd,nfld,ifld,GetCell )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
    TYPE( SAGgrid_str ),                 POINTER         :: grd
    INTEGER,                             INTENT( IN    ) :: nfld      !Number of fields
    INTEGER, DIMENSION(*),               INTENT( IN    ) :: ifld      !Field numbers
    EXTERNAL                                             :: GetCell
  END SUBROUTINE SetTopPoints

  SUBROUTINE SetRightPoints( p,grd,nfld,ifld,GetCell )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
    TYPE( SAGgrid_str ),                 POINTER         :: grd
    INTEGER,                             INTENT( IN    ) :: nfld       !Number of fields
    INTEGER, DIMENSION(*),               INTENT( IN    ) :: ifld       !Field numbers
    EXTERNAL                                             :: GetCell
  END SUBROUTINE SetRightPoints

  SUBROUTINE SetLeftPoints( p,grd,nfld,ifld,GetCell )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
    TYPE( SAGgrid_str ),                 POINTER         :: grd
    INTEGER,                             INTENT( IN    ) :: nfld       !Number of fields
    INTEGER, DIMENSION(*),               INTENT( IN    ) :: ifld       !Field numbers
    EXTERNAL                                             :: GetCell
  END SUBROUTINE SetLeftPoints

END INTERFACE

END MODULE setpoints_itrf

!============================================================================

SUBROUTINE GetPointVal( xpnt,ypnt,fval,grdI,nfld,ifld,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: xpnt,ypnt     !Location (project coords)
REAL, DIMENSION(*),    INTENT( OUT ) :: fval          !Field value
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
INTEGER,               INTENT( IN  ) :: nfld          !Number of fields
INTEGER, DIMENSION(*), INTENT( IN  ) :: ifld          !Field numbers
LOGICAL,               INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( SAGgrid_str ), POINTER  :: grd

REAL    xgc, ygc
INTEGER i

!==== Initialize return

DO i = 1,nfld
  fval(i) = 0.0
END DO

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetPointVal'
  GOTO 9999
END IF

!==== Convert real coordinates to Grid coordinates

xgc = (xpnt-grd%xmin)/grd%dx
ygc = (ypnt-grd%ymin)/grd%dy

CALL GetPointValGC( xgc,ygc,fval,grdI,nfld,ifld,log_interp )

9999 CONTINUE

RETURN
END

!============================================================================

SUBROUTINE GetPointValGC( xpnt,ypnt,fval,grdI,nfld,ifld,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd
USE cellstr_fd
USE sagdef_fd
USE getcellval_itrf
USE setpoints_itrf
USE pointval_fi

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: xpnt,ypnt     !Location (grid coords)
REAL, DIMENSION(*),    INTENT( OUT ) :: fval          !Field value
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
INTEGER,               INTENT( IN  ) :: nfld          !Number of fields
INTEGER, DIMENSION(*), INTENT( IN  ) :: ifld          !Field numbers
LOGICAL,               INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( getp_cell_str ), DIMENSION(4) :: p
TYPE( SAGgrid_str   ), POINTER      :: grd

REAL    xmax, ymax, xx, yy, ratx, raty, c1, c2, c3, c4
INTEGER i, alloc_stat, npts

!==== Initialize return

DO i = 1,nfld
  fval(i) = 0.0
END DO

npts = 0

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetPointValGC'
  GOTO 9999
END IF

!==== Make sure point is within data domain

IF( (xpnt < 0) .OR. (ypnt < 0) )GOTO 9999

xmax = FLOAT(grd%nx)
ymax = FLOAT(grd%ny)

IF( (xpnt > xmax) .OR. (ypnt > ymax) )GOTO 9999

!==== Push location into pointval_fi

xp = xpnt
yp = ypnt

!==== Refined grids - use triangulation
!     Fixed   grids - use bilinear interpolation

IF( grd%type == SAG_GRID_NONE )THEN

!==== Allocate cell structure field pointers

  npts = 4

  DO i = 1,npts
    ALLOCATE( p(i)%f(nfld),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error allocating field pointers'
      eRoutine = 'GetPointVal'
      GOTO 9999
    END IF
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    DO i = 1,nfld
      fval(i) = p(1)%f(i)
    END DO

  ELSE

    IF( p(1)%x >= 0. )THEN
      p(2)%x = xp + 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp + 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp + 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    ELSE
      p(2)%x = xp - 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp - 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp - 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    END IF

    ratx = ABS(p(1)%x)
    raty = ABS(p(1)%y)

    c1 = (1.-ratx)*(1.-raty)
    c2 = (   ratx)*(1.-raty)
    c3 = (   ratx)*(   raty)
    c4 = (1.-ratx)*(   raty)

    DO i = 1,nfld
      fval(i) = c1*p(1)%f(i) + c2*p(2)%f(i) + c3*p(3)%f(i) + c4*p(4)%f(i)
    END DO

  END IF

ELSE

!==== Find the bottom level cell that contains the point
!       Note: GetCellValue - Point location
!                            in  = Grid coordinates (x=0-m0, y=0-n0)
!                            out = Cell coordinates (x=0=1 , y=0-1 )

!==== Allocate cell structure field pointers

  npts = 3

  DO i = 1,npts
    ALLOCATE( p(i)%f(nfld),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Erro allocating field pointers'
      eRoutine = 'GetPointVal'
      GOTO 9999
    END IF
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

!==== Set cell boundaries in Grid coordinates

  xR = x0 + 0.5*p(1)%hx
  xL = x0 - 0.5*p(1)%hx
  yT = y0 + 0.5*p(1)%hy
  yB = y0 - 0.5*p(1)%hy

!==== Reference to cell center

  xp = xp - x0
  yp = yp - y0

!==== Check to see if need to triangulate
!       If point is at (0,0) in cell-center coordinates justr return cell value

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    DO i = 1,nfld
      fval(i) = p(1)%f(i)
    END DO

  ELSE

!==== Triangulate

    IF( p(1)%y >= p(1)%x )THEN !Top/left

      IF( p(1)%y >= -p(1)%x )THEN !Top
        CALL SetTopPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Left
        CALL SetLeftPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    ELSE !Bottom/right

      IF( p(1)%y <= -p(1)%x )THEN !Bottom
        CALL SetBottomPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Right
        CALL SetRightPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    END IF

    CALL find_value( xp,yp,p(1),p(2),p(3),fval,nfld,log_interp )

  END IF

END IF

9999 CONTINUE

!==== Deallocate cell structure field pointers

IF( npts > 0 )THEN
  DO i = 1,npts
    IF( ASSOCIATED(p(i)%f) )DEALLOCATE( p(i)%f,STAT=alloc_stat )
  END DO
END IF

RETURN
END
!***********************************************************************
!               find_value
!***********************************************************************
SUBROUTINE find_value( x,y,p0,p1,p2,fval,nfld,log_interp )

USE cellstr_fd

!====  Performs the triangular logarithmic interpolation

IMPLICIT NONE

REAL,                 INTENT( IN  ) :: x,y         !Point location
TYPE( getp_cell_str ),INTENT( IN  ) :: p0, p1, p2  !Surrounding cells
REAL, DIMENSION(*),   INTENT( OUT ) :: fval        !Field value
INTEGER,              INTENT( IN  ) :: nfld        !Number of field values
LOGICAL,              INTENT( IN  ) :: log_interp  !Interpolation flag true=log false=linear

REAL    dd0, dd1, dd2, d1, d2, den
REAL    r1, r2, x1, x2, y1, y2, xx, yy
INTEGER i

IF( p0%id == p1%id .OR. p0%id == p2%id .OR. p1%id == p2%id )THEN
  DO i = 1,nfld
    fval(i) = MAX(p0%f(i),1.E-30)
  END DO
ELSE
  DO i = 1,nfld
    dd0 = MAX(p0%f(i),1.E-30)
    dd1 = MAX(p1%f(i),1.E-30)
    dd2 = MAX(p2%f(i),1.E-30)

    IF( dd2 > MAX( dd0,dd1 ) )THEN
      dd0 = dd2
      dd2 = MAX(p0%f(i),1.E-30)

      xx = x - p2%x
      yy = y - p2%y
      x1 = p1%x - p2%x
      y1 = p1%y - p2%y
      x2 = -p2%x
      y2 = -p2%y

    ELSE IF ( dd1 > MAX( dd0,dd2 ) )THEN
      dd0 = dd1
      dd1 = MAX(p0%f(i),1.E-30)

      xx = x - p1%x
      yy = y - p1%y
      x1 = -p1%x
      y1 = -p1%y
      x2 = p2%x - p1%x
      y2 = p2%y - p1%y

    ELSE
      xx = x
      yy = y
      x1 = p1%x
      y1 = p1%y
      x2 = p2%x
      y2 = p2%y

    END IF

    IF( log_interp )THEN
      d1 = LOG(MAX(1.e-30,dd1/MAX(dd0,1.E-30*dd1)))
      d2 = LOG(MAX(1.e-30,dd2/MAX(dd0,1.E-30*dd2)))
    ELSE
      d1 = dd1 - dd0
      d2 = dd2 - dd0
    END IF

    den = (x1*y2-x2*y1)

    r1 = (y2*d1-y1*d2)/den
    r2 = (x1*d2-x2*d1)/den

    IF( log_interp )THEN
      fval(i) = dd0*EXP(r1*xx + r2*yy)
    ELSE
      fval(i) = dd0 + r1*xx + r2*yy
    END IF

  END DO
END IF

RETURN
END
!***********************************************************************
!               GetCellValue
!***********************************************************************
SUBROUTINE GetCellValue( p,xc,yc,grd,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE cellstr_fd

! Gets the cell value and location for the specified location

IMPLICIT NONE

TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
REAL,                  INTENT( OUT   ) :: xc,yc        !Location
TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers

INTEGER ix, iy, i

INTERFACE

  RECURSIVE SUBROUTINE sum_point_val_xy( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p
    REAL,                  INTENT( INOUT ) :: xc, yc
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE sum_point_val_xy

  RECURSIVE SUBROUTINE sum_point_val_x( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p
    REAL,                  INTENT( INOUT ) :: xc, yc
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE sum_point_val_x

  RECURSIVE SUBROUTINE sum_point_val_y( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p
    REAL,                  INTENT( INOUT ) :: xc, yc
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE sum_point_val_y

END INTERFACE

p%x = MAX(EPS,MIN(p%x,FLOAT(grd%nx)-EPS))
p%y = MAX(EPS,MIN(p%y,FLOAT(grd%ny)-EPS))

ix  = MIN(INT(p%x)+1,grd%nx)
iy  = MIN(INT(p%y)+1,grd%ny)

xc  = FLOAT(ix) - 0.5
yc  = FLOAT(iy) - 0.5

p%id  = (iy-1)*grd%nx + ix
p%hx  = 1.0
p%hy  = 1.0

DO i = 1,nfld
 p%f(i) = grd%ipdat((ifld(i)-1)*grd%mxgrd+p%id)
END DO

SELECT CASE( grd%type )

  CASE( SAG_GRID_BOTH )
    CALL sum_point_val_xy( p,xc,yc,grd,nfld,ifld )

  CASE( SAG_GRID_HORZ )
    CALL sum_point_val_x( p,xc,yc,grd,nfld,ifld )

  CASE DEFAULT
    CALL sum_point_val_y( p,xc,yc,grd,nfld,ifld )

END SELECT

RETURN
END
!***********************************************************************
!               sum_point_val_xy
!***********************************************************************
RECURSIVE SUBROUTINE sum_point_val_xy( p,xc,yc,grd,nfld,ifld )

USE sagstr_fd
USE cellstr_fd

!==== Finds the value and bottom cell location of the point - refined both directions

IMPLICIT NONE

TYPE( getp_cell_str ), INTENT( INOUT ) :: p
REAL,                  INTENT( INOUT ) :: xc, yc
TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers

INTEGER ix,iy,ipt, i

p%x = p%x - FLOAT(INT(p%x))
p%y = p%y - FLOAT(INT(p%y))

IF( grd%ipgrd(p%id) /= 0 )THEN

  p%x  = p%x + p%x
  p%y  = p%y + p%y

  p%hx = 0.5*p%hx
  p%hy = 0.5*p%hy

  ix   = INT(p%x)
  iy   = INT(p%y)
  ipt  = iy + iy + ix
  p%id = grd%ipgrd(p%id) + ipt

  xc = xc + (FLOAT(ix) - 0.5)*p%hx
  yc = yc + (FLOAT(iy) - 0.5)*p%hy

  DO i = 1,nfld
    p%f(i) = p%f(i) + grd%ipdat((ifld(i)-1)*grd%mxgrd+p%id)
  END DO

  CALL sum_point_val_xy( p,xc,yc,grd,nfld,ifld )

END IF

RETURN
END
!***********************************************************************
!               sum_point_val_x
!***********************************************************************
RECURSIVE SUBROUTINE sum_point_val_x( p,xc,yc,grd,nfld,ifld )

USE sagstr_fd
USE cellstr_fd

!==== Finds the value and bottom cell location of the point - refined X direction only

IMPLICIT NONE

TYPE( getp_cell_str ), INTENT( INOUT ) :: p
REAL,                  INTENT( INOUT ) :: xc,yc
TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers

INTEGER ix, i

p%x = p%x - FLOAT(INT(p%x))
p%y = p%y - FLOAT(INT(p%y))

IF( grd%ipgrd(p%id) /= 0 )THEN

  p%x  = p%x + p%x

  p%hx = 0.5*p%hx

  ix   = INT(p%x)
  p%id = grd%ipgrd(p%id) + ix

  xc = xc + (FLOAT(ix) - 0.5)*p%hx

  DO i = 1,nfld
    p%f(i) = p%f(i) + grd%ipdat((ifld(i)-1)*grd%mxgrd+p%id)
  END DO

  CALL sum_point_val_x( p,xc,yc,grd,nfld,ifld )

END IF

RETURN
END
!***********************************************************************
!               sum_point_val_y
!***********************************************************************
RECURSIVE SUBROUTINE sum_point_val_y( p,xc,yc,grd,nfld,ifld )

USE sagstr_fd
USE cellstr_fd

!==== Finds the value and bottom cell location of the point - refined Y direction only

IMPLICIT NONE

TYPE( getp_cell_str ), INTENT( INOUT ) :: p
REAL,                  INTENT( INOUT ) :: xc,yc
TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers

INTEGER iy, i

p%x = p%x - FLOAT(INT(p%x))
p%y = p%y - FLOAT(INT(p%y))

IF( grd%ipgrd(p%id) /= 0 )THEN

  p%y  = p%y + p%y

  p%hy = 0.5*p%hy

  iy   = INT(p%y)
  p%id = grd%ipgrd(p%id) + iy

  yc = yc + (FLOAT(iy) - 0.5)*p%hy

  DO i = 1,nfld
    p%f(i) = p%f(i) + grd%ipdat((ifld(i)-1)*grd%mxgrd+p%id)
  END DO

  CALL sum_point_val_y( p,xc,yc,grd,nfld,ifld )

END IF

RETURN
END
!***********************************************************************
!               SetTopPoints
!***********************************************************************
SUBROUTINE SetTopPoints( p,grd,nfld,ifld,GetCell )

USE sagstr_fd
USE cellstr_fd
USE pointval_fi

IMPLICIT NONE

TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
TYPE( SAGgrid_str ),                 POINTER         :: grd
INTEGER,                             INTENT( IN    ) :: nfld      !Number of fields
INTEGER,DIMENSION(*),                INTENT( IN    ) :: ifld      !Field numbers

INTERFACE
  SUBROUTINE GetCell( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
    REAL,                  INTENT( OUT   ) :: xc,yc        !Location
    TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE GetCell
END INTERFACE

REAL xc, yc, xL2, xR2, dx

!==== Get second point - projection to edge

dx = 0.5*(p(1)%x/p(1)%y)*p(1)%hx
p(2)%x = x0 + dx
IF( dx /= 0.0 )p(2)%x = NEAREST(p(2)%x,-dx)

p(2)%y = NEAREST(yT,1.0)

CALL GetCell( p(2),xc,yc,grd,nfld,ifld )

xR2 = xc + 0.5*p(2)%hx
xL2 = xc - 0.5*p(2)%hx

p(2)%x = xc - x0
p(2)%y = yc - y0

!==== Get Third point - triangulate

IF( p(2)%hx > (1.+EPS)*p(1)%hx )THEN             !Cell2 Less refined

  IF( p(2)%x*p(1)%y >= p(2)%y*p(1)%x )THEN       !Point West of P1-P2

    p(3)%x = NEAREST(xL,-1.0)
    p(3)%y = NEAREST(yT,-1.0)

  ELSE                                           !Point East of P1-P2

    p(3)%x = NEAREST(xR,1.0)

    IF( xL2 <= p(3)%x )THEN                      !Cell East boundaries match
      p(3)%y = NEAREST(yT, 1.0)
    ELSE                                         !Cell2 boundary beyond Cell1
      p(3)%y = NEAREST(yT,-1.0)
    END IF

  END IF

ELSE IF( p(1)%hx > (1.+EPS)*p(2)%hx )THEN        !Cell2 more refined

  IF( p(2)%x*p(1)%y >= p(2)%y*p(1)%x )THEN       !Point West of P1-P2

    IF( xL2 <= NEAREST(xL,-1.0) )THEN            !Cell West boundaries match
      p(3)%x = NEAREST(xL,-1.0)
      p(3)%y = NEAREST(yT,-1.0)
    ELSE                                         !Need West neighbor
      p(3)%x = NEAREST(xL2,-1.0)
      p(3)%y = NEAREST(yT , 1.0)
    END IF

  ELSE                                           !Point East of P1-P2

    p(3)%y = NEAREST(yT,1.0)

    IF( xR2 >= NEAREST(xR,-1.0) )THEN            !Cell East boundaries match
      p(3)%x = NEAREST(xR ,1.0)
    ELSE                                         !Need East neighbor
      p(3)%x = NEAREST(xR2,1.0)
    END IF

  END IF

ELSE                                             !Cell2 same refinement

  IF( p(1)%x <= 0.0 )THEN                        !Point West of P1-P2
    p(3)%x = NEAREST(xL,-1.0)
    p(3)%y = NEAREST(yT,-1.0)
  ELSE                                           !Point East of P1-P2
    p(3)%x = NEAREST(xR,1.0)
    p(3)%y = NEAREST(yT,1.0)
  END IF

END IF

CALL GetCell( p(3),xc,yc,grd,nfld,ifld )

p(3)%x = xc - x0
p(3)%y = yc - y0

RETURN
END
!***********************************************************************
!               SetBottomPoints
!***********************************************************************
SUBROUTINE SetBottomPoints( p,grd,nfld,ifld,GetCell )

USE sagstr_fd
USE cellstr_fd
USE pointval_fi

IMPLICIT NONE

TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
TYPE( SAGgrid_str ),                 POINTER         :: grd
INTEGER,                             INTENT( IN    ) :: nfld !Number of fields
INTEGER, DIMENSION(*),               INTENT( IN    ) :: ifld !Field numbers

INTERFACE
  SUBROUTINE GetCell( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
    REAL,                  INTENT( OUT   ) :: xc,yc        !Location
    TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE GetCell
END INTERFACE

REAL xc, yc, xR2, xL2, dx

!==== Get second point - projection to edge

dx = -0.5*(p(1)%x/p(1)%y)*p(1)%hx
p(2)%x = x0 + dx
IF( dx /= 0.0 )p(2)%x = NEAREST(p(2)%x,-dx)

p(2)%y = NEAREST(yB,-1.0)

CALL GetCell( p(2),xc,yc,grd,nfld,ifld )

xR2 = xc + 0.5*p(2)%hx
xL2 = xc - 0.5*p(2)%hx

p(2)%x = xc - x0
p(2)%y = yc - y0

!==== Get Third point - triangulate

IF( p(2)%hx > (1.+EPS)*p(1)%hx )THEN             !Cell2 Less refined

  IF( p(2)%x*p(1)%y >= p(2)%y*p(1)%x )THEN       !Point East of P1-P2

    p(3)%x = NEAREST(xR,1.0)
    p(3)%y = NEAREST(yB,1.0)

  ELSE                                           !Point West of P1-P2

    p(3)%x = NEAREST(xL,-1.0)

    IF( xL2 >= p(3)%x )THEN                      !Cell West boundaries match
      p(3)%y = NEAREST(yB,-1.0)
    ELSE                                         !Cell2 boundary beyond Cell1
      p(3)%y = NEAREST(yB, 1.0)
    END IF

  END IF

ELSE IF( p(1)%hx > (1.+EPS)*p(2)%hx )THEN        !Cell2 more refined

  IF( p(2)%x*p(1)%y >= p(2)%y*p(1)%x )THEN       !Point East of P1-P2

    IF( xR2 >= NEAREST(xR,-1.0) )THEN            !Cell East boundaries match
      p(3)%x = NEAREST(xR,1.0)
      p(3)%y = NEAREST(yB,1.0)
    ELSE                                         !Need East neighbor
      p(3)%x = NEAREST(xR2, 1.0)
      p(3)%y = NEAREST(yB ,-1.0)
    END IF

  ELSE                                           !Point West of P1-P2

    p(3)%y = NEAREST(yB,-1.0)

    IF( xL2 <= NEAREST(xL,1.0) )THEN             !Cell West boundaries match
      p(3)%x = NEAREST(xL ,-1.0)
    ELSE                                         !Need West neighbor
      p(3)%x = NEAREST(xL2,-1.0)
    END IF

  END IF

ELSE                                             !Cell2 same refinement

  IF( p(1)%x <= 0.0 )THEN                        !Point West of P1-P2
    p(3)%x = NEAREST(xL,-1.0)
    p(3)%y = NEAREST(yB,-1.0)
  ELSE                                           !Point East of P1-P2
    p(3)%x = NEAREST(xR,1.0)
    p(3)%y = NEAREST(yB,1.0)
  END IF

END IF

CALL GetCell( p(3),xc,yc,grd,nfld,ifld )

p(3)%x = xc - x0
p(3)%y = yc - y0

RETURN
END
!***********************************************************************
!               SetRightPoints
!***********************************************************************
SUBROUTINE SetRightPoints( p,grd,nfld,ifld,GetCell )

USE sagstr_fd
USE cellstr_fd
USE pointval_fi

IMPLICIT NONE

TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
TYPE( SAGgrid_str ),                 POINTER         :: grd
INTEGER,                             INTENT( IN    ) :: nfld       !Number of fields
INTEGER,DIMENSION(*),                INTENT( IN    ) :: ifld       !Field numbers

INTERFACE
  SUBROUTINE GetCell( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
    REAL,                  INTENT( OUT   ) :: xc,yc        !Location
    TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE GetCell
END INTERFACE

REAL xc, yc, yT2, yB2, dy

!==== Get second point - projection to edge

p(2)%x = NEAREST(xR,1.0)

dy = 0.5*(p(1)%y/p(1)%x)*p(1)%hy
p(2)%y = y0 + dy
IF( dy /= 0.0 )p(2)%y = NEAREST(p(2)%y,-dy)

CALL GetCell( p(2),xc,yc,grd,nfld,ifld )

yT2 = yc + 0.5*p(2)%hy
yB2 = yc - 0.5*p(2)%hy

p(2)%x = xc - x0
p(2)%y = yc - y0

!==== Get Third point - triangulate

IF( p(2)%hy > (1.+EPS)*p(1)%hy )THEN             !Cell2 Less refined

  IF( p(2)%y*p(1)%x >= p(2)%x*p(1)%y )THEN       !Point South of P1-P2

    p(3)%x = NEAREST(xR,-1.0)
    p(3)%y = NEAREST(yB,-1.0)

  ELSE                                           !Point North of P1-P2

    p(3)%y = NEAREST(yT,1.0)

    IF( yT2 <= p(3)%y )THEN                      !Cell North boundaries match
      p(3)%x = NEAREST(xR, 1.0)
    ELSE                                         !Cell2 boundary beyond Cell1
      p(3)%x = NEAREST(xR,-1.0)
    END IF

  END IF

ELSE IF( p(1)%hy > (1.+EPS)*p(2)%hy )THEN        !Cell2 more refined

  IF( p(2)%y*p(1)%x >= p(2)%x*p(1)%y )THEN       !Point South of P1-P2

    IF( yB2 <= NEAREST(yB,1.0) )THEN             !Cell South boundaries match
      p(3)%x = NEAREST(xR,-1.0)
      p(3)%y = NEAREST(yB,-1.0)
    ELSE                                         !Need South neighbor
      p(3)%x = NEAREST(xR , 1.0)
      p(3)%y = NEAREST(yB2,-1.0)
    END IF

  ELSE                                           !Point North of P1-P2

    p(3)%x = NEAREST(xR,1.0)

    IF( yT2 >= NEAREST(yT,-1.0) )THEN            !Cell North boundaries match
      p(3)%y = NEAREST(yT ,1.0)
    ELSE                                         !Need North neighbor
      p(3)%y = NEAREST(yT2,1.0)
    END IF

  END IF

ELSE                                             !Cell2 same refinement

  IF( p(1)%y <= 0.0 )THEN                        !Point South of P1-P2
    p(3)%x = NEAREST(xR,-1.0)
    p(3)%y = NEAREST(yB,-1.0)
  ELSE                                           !Point North of P1-P2
    p(3)%x = NEAREST(xR,1.0)
    p(3)%y = NEAREST(yT,1.0)
  END IF

END IF

CALL GetCell( p(3),xc,yc,grd,nfld,ifld )

p(3)%x = xc - x0
p(3)%y = yc - y0

RETURN
END
!***********************************************************************
!               SetLeftPoints
!***********************************************************************
SUBROUTINE SetLeftPoints( p,grd,nfld,ifld,GetCell )

USE sagstr_fd
USE cellstr_fd
USE pointval_fi

IMPLICIT NONE

TYPE( getp_cell_str ), DIMENSION(3), INTENT( INOUT ) :: p
TYPE( SAGgrid_str ),                 POINTER         :: grd
INTEGER,                             INTENT( IN    ) :: nfld       !Number of fields
INTEGER,DIMENSION(*),                INTENT( IN    ) :: ifld       !Field numbers

INTERFACE
  SUBROUTINE GetCell( p,xc,yc,grd,nfld,ifld )
    USE sagstr_fd
    USE cellstr_fd
    TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
    REAL,                  INTENT( OUT   ) :: xc,yc        !Location
    TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
    INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
    INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers
  END SUBROUTINE GetCell
END INTERFACE

REAL xc, yc, yT2, yB2, dy

!==== Get second point - projection to edge

p(2)%x = NEAREST(xL,-1.0)

dy = -0.5*(p(1)%y/p(1)%x)*p(1)%hy
p(2)%y = y0 + dy
IF( dy /= 0.0 )p(2)%y = NEAREST(p(2)%y,-dy)

CALL GetCell( p(2),xc,yc,grd,nfld,ifld )

yT2 = yc + 0.5*p(2)%hy
yB2 = yc - 0.5*p(2)%hy

p(2)%x = xc - x0
p(2)%y = yc - y0

!==== Get Third point - triangulate

IF( p(2)%hy > (1.+EPS)*p(1)%hy )THEN             !Cell2 Less refined

  IF( p(2)%y*p(1)%x >= p(2)%x*p(1)%y )THEN       !Point North of P1-P2

    p(3)%x = NEAREST(xL,1.0)
    p(3)%y = NEAREST(yT,1.0)

  ELSE                                           !Point South of P1-P2

    p(3)%y = NEAREST(yB,-1.0)

    IF( yB2 >= p(3)%y )THEN                      !Cell South boundaries match
      p(3)%x = NEAREST(xL,-1.0)
    ELSE                                         !Cell2 boundary beyond Cell1
      p(3)%x = NEAREST(xL,1.0)
    END IF

  END IF

ELSE IF( p(1)%hy > (1.+EPS)*p(2)%hy )THEN        !Cell2 more refined

  IF( p(2)%y*p(1)%x >= p(2)%x*p(1)%y )THEN       !Point North of P1-P2

    IF( yT2 >= NEAREST(yT,-1.0) )THEN            !Cell North boundaries match
      p(3)%x = NEAREST(xL,1.0)
      p(3)%y = NEAREST(yT,1.0)
    ELSE                                         !Need North neighbor
      p(3)%x = NEAREST(xL ,-1.0)
      p(3)%y = NEAREST(yT2, 1.0)
    END IF

  ELSE                                           !Point South of P1-P2

    p(3)%x = NEAREST(xL,-1.0)

    IF( yB2 <= NEAREST(yB,1.0) )THEN             !Cell South boundaries match
      p(3)%y = NEAREST(yB ,-1.0)
    ELSE                                         !Need South neighbor
      p(3)%y = NEAREST(yB2,-1.0)
    END IF

  END IF

ELSE                                             !Cell2 same refinement

  IF( p(1)%y <= 0.0 )THEN                        !Point South of P1-P2
    p(3)%x = NEAREST(xL,-1.0)
    p(3)%y = NEAREST(yB,-1.0)
  ELSE                                           !Point North of P1-P2
    p(3)%x = NEAREST(xL,1.0)
    p(3)%y = NEAREST(yT,1.0)
  END IF

END IF

CALL GetCell( p(3),xc,yc,grd,nfld,ifld )

p(3)%x = xc - x0
p(3)%y = yc - y0

RETURN
END

!============================================================================

SUBROUTINE GetPlotFieldVal( xpnt,ypnt,fval,grdI,UserFunction,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd
USE cellstr_fd
USE sagdef_fd
USE getcellval_itrf
USE setpoints_itrf
USE pointval_fi

!  Returns the field value at a specified location for the plotfield (field-3)
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

REAL,    INTENT( IN  ) :: xpnt,ypnt     !Location (project coords)
REAL,    INTENT( OUT ) :: fval          !Field value
INTEGER, INTENT( IN  ) :: grdI          !Field ID
LOGICAL, INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( getp_cell_str ), DIMENSION(4) :: p
TYPE( SAGgrid_str   ), POINTER      :: grd

REAL xmax,ymax,xx,yy,ratx,raty,c1,c2,c3,c4

INTEGER i, nfld, alloc_stat, npts, irv

INTEGER, DIMENSION(1) :: ifld
REAL,    DIMENSION(1) :: fvalp

INTERFACE
  INTEGER FUNCTION UserFunction( dat,mxgrd,p0 )
    USE cellstr_fd
    REAL, POINTER, DIMENSION(:) :: dat   !Pointer to Grid data
    INTEGER                     :: mxgrd !Data field size
    TYPE( getp_cell_str )       :: p0    !Cell descriptor
  END FUNCTION UserFunction
END INTERFACE

!==== Initialize return

fval = 0.0
npts = 0

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetPlotFieldVal'
  RETURN
END IF

!==== Make sure point is within data domain (or at least within EPS)

IF( (xpnt+EPS < grd%xmin) .OR. (ypnt+EPS < grd%ymin) )GOTO 9999

xmax = grd%xmin + FLOAT(grd%nx)*grd%dx
ymax = grd%ymin + FLOAT(grd%ny)*grd%dy

IF( (xpnt-EPS > xmax) .OR. (ypnt-EPS > ymax) )GOTO 9999

!==== Convert real coordinates to Grid coordinates

xp = (xpnt-grd%xmin)/grd%dx
yp = (ypnt-grd%ymin)/grd%dy

!==== Refined grids - use triangulation
!     Fixed   grids - use bilinear interpolation

nfld    = 1
ifld(1) = 1

IF( grd%type == SAG_GRID_NONE )THEN

!-- Allocate cell structure field pointers

  npts = 4

  DO i = 1,npts
    ALLOCATE( p(i)%f(3),STAT=alloc_stat )
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    irv  = UserFunction( grd%ipdat,grd%mxgrd,p(1) )
    fval = p(1)%f(3)

  ELSE

    IF( p(1)%x >= 0. )THEN
      p(2)%x = xp + 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp + 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp + 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    ELSE
      p(2)%x = xp - 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp - 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp - 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    END IF

    ratx = ABS(p(1)%x)
    raty = ABS(p(1)%y)

    c1 = (1.-ratx)*(1.-raty)
    c2 = (   ratx)*(1.-raty)
    c3 = (   ratx)*(   raty)
    c4 = (1.-ratx)*(   raty)

    irv = UserFunction( grd%ipdat,grd%mxgrd,p(1) )
    irv = UserFunction( grd%ipdat,grd%mxgrd,p(2) )
    irv = UserFunction( grd%ipdat,grd%mxgrd,p(3) )
    irv = UserFunction( grd%ipdat,grd%mxgrd,p(4) )

    fval = c1*p(1)%f(3) + c2*p(2)%f(3) + c3*p(3)%f(3) + c4*p(4)%f(3)

  END IF

ELSE

!==== Find the bottom level cell that contains the point
!       Note: GetCellValue - Point location
!                            in  = Grid coordinates (x=0-m0, y=0-n0)
!                            out = Cell coordinates (x=0=1 , y=0-1 )

!Allocate cell structure field pointers

  npts = 3

  DO i = 1,npts
    ALLOCATE( p(i)%f(3),STAT=alloc_stat )
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

!==== Set cell boundaries in Grid coordinates

  xR = x0 + 0.5*p(1)%hx
  xL = x0 - 0.5*p(1)%hx
  yT = y0 + 0.5*p(1)%hy
  yB = y0 - 0.5*p(1)%hy

!==== Reference to cell center

  xp = xp - x0
  yp = yp - y0

!==== Check to see if need to triangulate
!       If point is at (0,0) in cell-center coordinates justr return cell value

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    irv  = UserFunction( grd%ipdat,grd%mxgrd,p(1) )
    fval = p(1)%f(3)

  ELSE

!==== Triangulate

    IF( p(1)%y >= p(1)%x )THEN !Top/left

      IF( p(1)%y >= -p(1)%x )THEN !Top
        CALL SetTopPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Left
        CALL SetLeftPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    ELSE !Bottom/right

      IF( p(1)%y <= -p(1)%x )THEN !Bottom
        CALL SetBottomPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Right
        CALL SetRightPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    END IF

    irv = UserFunction( grd%ipdat,grd%mxgrd,p(1) )
    irv = UserFunction( grd%ipdat,grd%mxgrd,p(2) )
    irv = UserFunction( grd%ipdat,grd%mxgrd,p(3) )

    p(1)%f(1) = p(1)%f(3)
    p(2)%f(1) = p(2)%f(3)
    p(3)%f(1) = p(3)%f(3)

    CALL find_value( xp,yp,p(1),p(2),p(3),fvalp,1,log_interp )
    fval = fvalp(1)

  END IF

END IF

9999 CONTINUE

! Allocate cell structure field pointers

DO i = 1,npts
  IF( ASSOCIATED(p(i)%f) )DEALLOCATE( p(i)%f,STAT=alloc_stat)
END DO

RETURN
END

!============================================================================

SUBROUTINE GetPlotFieldMCVal( xpnt,ypnt,fval,grdI,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd
USE cellstr_fd
USE sagdef_fd
USE getcellval_itrf
USE setpoints_itrf
USE pointval_fi

!  Returns the field value at a specified location for the plotfield (field-3)
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

REAL,                 INTENT( IN  ) :: xpnt,ypnt     !Location (project coords)
REAL, DIMENSION(*),   INTENT( OUT ) :: fval          !Field value
INTEGER,              INTENT( IN  ) :: grdI          !Field ID
LOGICAL,              INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( getp_cell_str ), DIMENSION(4) :: p
TYPE( SAGgrid_str   ), POINTER      :: grd

REAL xmax,ymax,xx,yy,ratx,raty,c1,c2,c3,c4
REAL SMALL

INTEGER i, j, k, nfld, alloc_stat, npts

INTEGER, DIMENSION(:),ALLOCATABLE :: ifld

!==== Initialize return

npts  = 0
SMALL = 1.E-30

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetPlotFieldVal'
  RETURN
END IF

!==== Make sure point is within data domain (or at least within EPS)

IF( (xpnt+EPS < grd%xmin) .OR. (ypnt+EPS < grd%ymin) )GOTO 9999

xmax = grd%xmin + FLOAT(grd%nx)*grd%dx
ymax = grd%ymin + FLOAT(grd%ny)*grd%dy

IF( (xpnt-EPS > xmax) .OR. (ypnt-EPS > ymax) )GOTO 9999

!==== Convert real coordinates to Grid coordinates

xp = (xpnt-grd%xmin)/grd%dx
yp = (ypnt-grd%ymin)/grd%dy

!==== Refined grids - use triangulation
!     Fixed   grids - use bilinear interpolation

nfld = grd%mxfld

ALLOCATE(ifld(nfld),STAT=alloc_stat)

DO i = 1,nfld
  ifld(i) = i
END DO

IF( grd%type == SAG_GRID_NONE )THEN

!-- Allocate cell structure field pointers

  npts = 4

  DO i = 1,npts
    ALLOCATE( p(i)%f(nfld),STAT=alloc_stat )
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    DO j = 1,nFld
      k = p(1)%id + (j-1)*grd%mxgrd
      p(1)%f(j) = MAX(grd%ipdat(k),SMALL)
    END DO
    fval(1:nfld) = p(1)%f(1:nfld)

  ELSE

    IF( p(1)%x >= 0. )THEN
      p(2)%x = xp + 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp + 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp + 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    ELSE
      p(2)%x = xp - 0.99
      p(2)%y = yp
      CALL GetCellValue( p(2),xx,yy,grd,nfld,ifld )
      IF( p(1)%y >= 0. )THEN
        p(3)%x = xp - 0.99
        p(3)%y = yp + 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp + 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      ELSE
        p(3)%x = xp - 0.99
        p(3)%y = yp - 0.99
        CALL GetCellValue( p(3),xx,yy,grd,nfld,ifld )
        p(4)%x = xp
        p(4)%y = yp - 0.99
        CALL GetCellValue( p(4),xx,yy,grd,nfld,ifld )
      END IF
    END IF

    ratx = ABS(p(1)%x)
    raty = ABS(p(1)%y)

    c1 = (1.-ratx)*(1.-raty)
    c2 = (   ratx)*(1.-raty)
    c3 = (   ratx)*(   raty)
    c4 = (1.-ratx)*(   raty)

    DO i = 1,npts
      DO j = 1,nFld
        k = p(i)%id + (j-1)*grd%mxgrd
        p(i)%f(j) = MAX(grd%ipdat(k),SMALL)
      END DO
    END DO

    DO j = 1,nFld
      fval(j) = c1*p(1)%f(j) + c2*p(2)%f(j) + c3*p(3)%f(j) + c4*p(4)%f(j)
    END DO

  END IF

ELSE

!==== Find the bottom level cell that contains the point
!       Note: GetCellValue - Point location
!                            in  = Grid coordinates (x=0-m0, y=0-n0)
!                            out = Cell coordinates (x=0=1 , y=0-1 )

!Allocate cell structure field pointers

  npts = 3

  DO i = 1,npts
    ALLOCATE( p(i)%f(nfld),STAT=alloc_stat )
  END DO

  p(1)%x = xp
  p(1)%y = yp
  CALL GetCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

  p(1)%x = p(1)%x - 0.5
  p(1)%y = p(1)%y - 0.5

!==== Set cell boundaries in Grid coordinates

  xR = x0 + 0.5*p(1)%hx
  xL = x0 - 0.5*p(1)%hx
  yT = y0 + 0.5*p(1)%hy
  yB = y0 - 0.5*p(1)%hy

!==== Reference to cell center

  xp = xp - x0
  yp = yp - y0

!==== Check to see if need to triangulate
!       If point is at (0,0) in cell-center coordinates justr return cell value

  IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

    DO j = 1,nFld
      k = p(1)%id + (j-1)*grd%mxgrd
      p(1)%f(j) = MAX(grd%ipdat(k),SMALL)
    END DO
    fval(1:nfld) = p(1)%f(1:nfld)

  ELSE

!==== Triangulate

    IF( p(1)%y >= p(1)%x )THEN !Top/left

      IF( p(1)%y >= -p(1)%x )THEN !Top
        CALL SetTopPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Left
        CALL SetLeftPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    ELSE !Bottom/right

      IF( p(1)%y <= -p(1)%x )THEN !Bottom
        CALL SetBottomPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      ELSE !Right
        CALL SetRightPoints( p(1:3),grd,nfld,ifld,GetCellValue )
      END IF

    END IF

    DO i = 1,npts
      DO j = 1,nFld
        k = p(i)%id + (j-1)*grd%mxgrd
        p(i)%f(j) = MAX(grd%ipdat(k),SMALL)
      END DO
    END DO

    CALL find_value( xp,yp,p(1),p(2),p(3),fval,nfld,log_interp )

  END IF

END IF

9999 CONTINUE

! Allocate cell structure field pointers

DO i = 1,npts
  IF( ASSOCIATED(p(i)%f) )DEALLOCATE( p(i)%f,STAT=alloc_stat)
END DO

IF( ALLOCATED(ifld) )DEALLOCATE(ifld,STAT=alloc_stat)

RETURN
END


!============================================================================

SUBROUTINE GetBottomVal( xpnt,ypnt,fval,grdI,nfld,ifld,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd
USE cellstr_fd
USE sagdef_fd
USE getcellval_itrf
USE setpoints_itrf
USE pointval_fi

!  Returns the bottom level field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: xpnt,ypnt     !Location (project coords)
REAL, DIMENSION(*),    INTENT( OUT ) :: fval          !Field value
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
INTEGER,               INTENT( IN  ) :: nfld          !Number of fields
INTEGER, DIMENSION(*), INTENT( IN  ) :: ifld          !Field numbers
LOGICAL,               INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( getp_cell_str ), DIMENSION(4) :: p
TYPE( SAGgrid_str   ), POINTER      :: grd

REAL    xmax, ymax
INTEGER i, alloc_stat, npts

!==== Initialize return

DO i = 1,nfld
  fval(i) = 0.0
END DO

npts = 0

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetBottomVal'
  GOTO 9999
END IF

!==== Make sure point is within data domain

IF( (xpnt < grd%xmin) .OR. (ypnt < grd%ymin) )GOTO 9999

xmax = grd%xmin + FLOAT(grd%nx)*grd%dx
ymax = grd%ymin + FLOAT(grd%ny)*grd%dy

IF( (xpnt > xmax) .OR. (ypnt > ymax) )GOTO 9999

!==== Convert real coordinates to Grid coordinates

xp = (xpnt-grd%xmin)/grd%dx
yp = (ypnt-grd%ymin)/grd%dy

!==== Find the bottom level cell that contains the point
!       Note: GetCellValue - Point location
!                            in  = Grid coordinates (x=0-m0, y=0-n0)
!                            out = Cell coordinates (x=0=1 , y=0-1 )

!==== Allocate cell structure field pointers

npts = 3

DO i = 1,npts
  ALLOCATE( p(i)%f(nfld),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating field pointers'
    eRoutine = 'GetPointVal'
    GOTO 9999
  END IF
END DO

p(1)%x = xp
p(1)%y = yp
CALL GetBottomCellValue( p(1),x0,y0,grd,nfld,ifld )

!==== reference cell location to cell center instead of Lower/Left corner

p(1)%x = p(1)%x - 0.5
p(1)%y = p(1)%y - 0.5

!==== Set cell boundaries in Grid coordinates

xR = x0 + 0.5*p(1)%hx
xL = x0 - 0.5*p(1)%hx
yT = y0 + 0.5*p(1)%hy
yB = y0 - 0.5*p(1)%hy

!==== Reference to cell center

xp = xp - x0
yp = yp - y0

!==== Check to see if need to triangulate
!       If point is at (0,0) in cell-center coordinates justr return cell value

IF( p(1)%x == 0.0 .AND. p(1)%y == 0.0 )THEN

  DO i = 1,nfld
    fval(i) = p(1)%f(i)
  END DO

ELSE

!==== Triangulate

  IF( p(1)%y >= p(1)%x )THEN !Top/left

    IF( p(1)%y >= -p(1)%x )THEN !Top
      CALL SetTopPoints( p(1:3),grd,nfld,ifld,GetBottomCellValue )
    ELSE !Left
      CALL SetLeftPoints( p(1:3),grd,nfld,ifld,GetBottomCellValue )
    END IF

  ELSE !Bottom/right

    IF( p(1)%y <= -p(1)%x )THEN !Bottom
      CALL SetBottomPoints( p(1:3),grd,nfld,ifld,GetBottomCellValue )
    ELSE !Right
      CALL SetRightPoints( p(1:3),grd,nfld,ifld,GetBottomCellValue )
    END IF

  END IF

  CALL find_value( xp,yp,p(1),p(2),p(3),fval,nfld,log_interp )

END IF

9999 CONTINUE

!==== Deallocate cell structure field pointers

IF( npts > 0 )THEN
  DO i = 1,npts
    IF( ASSOCIATED(p(i)%f) )DEALLOCATE( p(i)%f,STAT=alloc_stat )
  END DO
END IF

RETURN
END

!============================================================================

SUBROUTINE GetCenterVal( ilev,xpnt,ypnt,fval,grdI,nfld,ifld,log_interp )

USE PtrGrdStrItf
USE error_fi
USE sagstr_fd
USE cellstr_fd
USE sagdef_fd
USE getcellval_itrf
USE setpoints_itrf
USE pointval_fi

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: xpnt,ypnt     !Location (project coords)
REAL, DIMENSION(*),    INTENT( OUT ) :: fval          !Field value
INTEGER,               INTENT( IN  ) :: ilev          !grid level
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
INTEGER,               INTENT( IN  ) :: nfld          !Number of fields
INTEGER, DIMENSION(*), INTENT( IN  ) :: ifld          !Field numbers
LOGICAL,               INTENT( IN  ) :: log_interp    !Interpolation flag true=log false=linear

TYPE( SAGgrid_str ), POINTER :: grd

REAL    xmax, ymax
REAL    xc,yc
INTEGER i,j ,ix, iy, jcell

!==== Initialize return

DO i = 1,nfld
  fval(i) = 0.0
END DO

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = IV_ERROR
  eMessage = 'No field associated with grid ID'
  eRoutine = 'GetCenterVal'
  GOTO 9999
END IF

!==== Make sure point is within data domain

IF( (xpnt < grd%xmin) .OR. (ypnt < grd%ymin) )GOTO 9999

xmax = grd%xmin + FLOAT(grd%nx)*grd%dx
ymax = grd%ymin + FLOAT(grd%ny)*grd%dy

IF( (xpnt > xmax) .OR. (ypnt > ymax) )GOTO 9999

!==== Convert real coordinates to Grid coordinates

xc = (xpnt-grd%xmin)/grd%dx
yc = (ypnt-grd%ymin)/grd%dy

ix = MIN(INT(xc)+1,grd%nx)
iy = MIN(INT(yc)+1,grd%ny)

jcell = (iy-1)*grd%nx + ix

ix = INT(xc)
iy = INT(yc)

DO j = 1,ilev

  IF( grd%ipgrd(jcell) > 0 ) THEN

    xc = xc - FLOAT(ix)
    yc = yc - FLOAT(iy)

    xc = xc + xc
    yc = yc + yc

    ix = INT(xc)
    iy = INT(yc)

    jcell = grd%ipgrd(jcell) + iy + iy + ix

  ELSE

    EXIT

  END IF

END DO

IF( j < ilev )THEN
  CALL GetBottomVal( xpnt,ypnt,fval,grdI,nfld,ifld,log_interp )
ELSE
  DO i = 1,nfld
    fval(i) = grd%ipdat((ifld(i)-1)*grd%mxgrd+jcell)
  END DO
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetBottomCellValue( p,xc,yc,grd,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE cellstr_fd

! Gets the cell value and location for the specified location

IMPLICIT NONE

TYPE( getp_cell_str ), INTENT( INOUT ) :: p            !Cell structure
REAL,                  INTENT( OUT   ) :: xc,yc        !Location
TYPE( SAGgrid_str ),   POINTER         :: grd          !SAG grid
INTEGER,               INTENT( IN    ) :: nfld         !Number of fields
INTEGER,DIMENSION(*),  INTENT( IN    ) :: ifld         !Field numbers

INTEGER ix, iy, i

p%x = MAX(EPS,MIN(p%x,FLOAT(grd%nx)-EPS))
p%y = MAX(EPS,MIN(p%y,FLOAT(grd%ny)-EPS))

ix  = MIN(INT(p%x)+1,grd%nx)
iy  = MIN(INT(p%y)+1,grd%ny)

xc  = FLOAT(ix) - 0.5
yc  = FLOAT(iy) - 0.5

p%id = (iy-1)*grd%nx + ix
p%hx = 1.0
p%hy = 1.0

p%x  = p%x - FLOAT(INT(p%x))
p%y  = p%y - FLOAT(INT(p%y))

DO WHILE( grd%ipgrd(p%id) > 0 )

  p%x  = p%x + p%x
  p%y  = p%y + p%y

  p%hx = 0.5*p%hx
  p%hy = 0.5*p%hy

  ix   = INT(p%x)
  iy   = INT(p%y)
  p%id = grd%ipgrd(p%id) + iy + iy + ix

  xc = xc + (FLOAT(ix) - 0.5)*p%hx
  yc = yc + (FLOAT(iy) - 0.5)*p%hy

  p%x = p%x - FLOAT(INT(p%x))
  p%y = p%y - FLOAT(INT(p%y))

END DO

DO i = 1,nfld
 p%f(i) = grd%ipdat((ifld(i)-1)*grd%mxgrd+p%id)
END DO

RETURN
END
