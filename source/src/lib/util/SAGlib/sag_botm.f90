!*******************************************************************************
!               SAG_BottomCount
!*******************************************************************************
INTEGER FUNCTION SAG_BottomCount( grd )

USE sagdef_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ix,iy,icell0,count,total,ir

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_CountCells( grd,icell0,count )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: grd
    INTEGER,             INTENT(    IN ) :: icell0
    INTEGER,             INTENT( INOUT ) :: count
  END FUNCTION SAG_CountCells
END INTERFACE

SAG_BottomCount = 0
total           = 0

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell0 = (iy-1)*grd%nx + ix
    count  = 0
    ir = SAG_CountCells( grd,icell0,count )
    IF( ir /= SAG_OK )GOTO 9999
    total = total + count

  END DO
END DO

IF( total < grd%nx*grd%ny )GOTO 9999 !Mostly likely type=NONE with refined grid

SAG_BottomCount = total

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_BottomLevelID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomLevelID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

INTEGER ix,iy,icell0,count,total,ret

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  RECURSIVE SUBROUTINE SAG_CountLevels( grd,icell0,count0,ret )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: grd
    INTEGER,             INTENT( IN    ) :: icell0
    INTEGER,             INTENT( INOUT ) :: count0
    INTEGER,             INTENT( OUT   ) :: ret
  END SUBROUTINE SAG_CountLevels
END INTERFACE

SAG_BottomLevelID = 0
total             = 0

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell0 = (iy-1)*grd%nx + ix
    count  = 0
    CALL SAG_CountLevels( grd,icell0,count,ret )
    IF( ret /= SAG_OK )GOTO 9999
    total = MAX(total,count)

  END DO
END DO

SAG_BottomLevelID = total

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_BottomMinMaxID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomMinMaxID( grdI,ifld,restore,fldmx )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE sagmaxval_usr
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: grdI
INTEGER,            INTENT( IN  ) :: ifld
LOGICAL,            INTENT( IN  ) :: restore
REAL, DIMENSION(2), INTENT( OUT ) :: fldmx

TYPE ( SAGgrid_str ), POINTER :: grd
INTEGER nfld
INTEGER, DIMENSION(1) :: jfld

INTEGER, EXTERNAL :: SAG_BottomFunctionID
INTEGER, EXTERNAL :: SAG_MaxValue

SAG_BottomMinMaxID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==== Check if sturcture has been defined

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

nfld        = 1
jfld(1)     = ifld
ifld_maxval = ifld
out_maxval  = -1.E+36
out_minval  =  1.E+36

SAG_BottomMinMaxID = SAG_BottomFunctionID( grdI,SAG_MaxValue,nfld,jfld,restore,.TRUE. )
IF( SAG_BottomMinMaxID == SAG_OK )THEN
  fldmx(1) = out_minval
  fldmx(2) = out_maxval
ELSE
  fldmx(1) = 0.
  fldmx(2) = 0.
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_BottomValueID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomValueID( id,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Pushes the data values to the bottom grid level

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: id
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

INTERFACE
  INTEGER FUNCTION SAG_BottomValue( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomValue
END INTERFACE

!------ Initialize

SAG_BottomValueID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

grd%pushtype = 1        !No multi-distribution

!------ Loop over fields

DO i = 1,nfld
  SAG_BottomValueID = SAG_BottomValue( grd,ifld(i) )
  IF( SAG_BottomValueID == SAG_ERROR )GOTO 9999
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_BottomValue( grd,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Pushes the data values to the bottom grid level

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld

INTEGER isize, irv, alloc_stat

INTEGER, DIMENSION(:),     POINTER :: flag !internal flags array
REAL,    DIMENSION(:), ALLOCATABLE :: drt, drb, dlt, dlb !gradient data

INTERFACE
  SUBROUTINE SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN    ) :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientBoth

  SUBROUTINE SAG_SetGradientVert( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN    ) :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientVert

  SUBROUTINE SAG_SetGradientHorz( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN    ) :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientHorz

  SUBROUTINE SAG_SetBottomValueBoth( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb
  END SUBROUTINE SAG_SetBottomValueBoth

  SUBROUTINE SAG_SetBottomValueHorz( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb
  END SUBROUTINE SAG_SetBottomValueHorz

  SUBROUTINE SAG_SetBottomValueVert( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb
  END SUBROUTINE SAG_SetBottomValueVert

  INTEGER FUNCTION SAG_UpperClear( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION

END INTERFACE

!==== Initialize

SAG_BottomValue = SAG_ERROR

flag => grd%ipflg

!==== Check to see if field is already a Bottom Value field

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( flag(ifld) /= SAG_GRID_BOTM )THEN

!==== Set up gradient arrays

  isize = grd%ncells

  ALLOCATE( drt(isize),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( drb(isize),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( dlt(isize),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( dlb(isize),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

!==== Compute gradients and set bottom values

  flag(ifld) = SAG_GRID_BOTM

  SELECT CASE( grd%type )
    CASE( SAG_GRID_BOTH )
      CALL SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )
      CALL SAG_SetBottomValueBoth( grd,ifld,drt,drb,dlt,dlb )

    CASE( SAG_GRID_HORZ )
      CALL SAG_SetGradientHorz( grd,ifld,drt,drb,dlt,dlb )
      CALL SAG_SetBottomValueHorz( grd,ifld,drt,drb,dlt,dlb )

    CASE( SAG_GRID_VERT )
      CALL SAG_SetGradientVert( grd,ifld,drt,drb,dlt,dlb )
      CALL SAG_SetBottomValueVert( grd,ifld,drt,drb,dlt,dlb )

    CASE( SAG_GRID_NONE )

    CASE DEFAULT
      LastError = SAG_ERR_TYPE
      LastParm  = grd%type
      GOTO 9999

  END SELECT

!==== Clear upper values

  irv = SAG_UpperClear( grd,ifld )
  IF( irv /= SAG_OK )THEN
    SAG_BottomValue = irv
    GOTO 9999
  END IF
END IF

!==== Return

SAG_BottomValue = SAG_OK

9999 CONTINUE

IF( ALLOCATED( drt ))DEALLOCATE( drt,STAT=alloc_stat )
IF( ALLOCATED( drb ))DEALLOCATE( drb,STAT=alloc_stat )
IF( ALLOCATED( dlt ))DEALLOCATE( dlt,STAT=alloc_stat )
IF( ALLOCATED( dlb ))DEALLOCATE( dlb,STAT=alloc_stat )

RETURN
END
!*******************************************************************************
!               SAG_SetBottomValueBoth
!*******************************************************************************
SUBROUTINE SAG_SetBottomValueBoth( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb

INTEGER ix,iy,id

TYPE( SAGcell_str ) p0

!==== Initialize

nx   = grd%nx
ny   = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

! Initialize cell structure values not used

p0%lev = 0
p0%x   = 0.0
p0%y   = 0.0
p0%hx  = 1.0
p0%hy  = 1.0

!==== Compute the bottom value

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ix,iy,id,p0) IF(nx*ny > 1)
!$OMP SINGLE
DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p0%id  = id
    p0%d   = pdat(id)
    p0%drt = grt(id)
    p0%dlt = glt(id)
    p0%drb = grb(id)
    p0%dlb = glb(id)

!$OMP TASK FIRSTPRIVATE(p0) DEFAULT(SHARED)
    CALL SAG_BottomValueBoth( p0 )
!$OMP END TASK

  END DO
END DO
!$OMP END SINGLE
!$OMP END PARALLEL

RETURN
END
!*******************************************************************************
!               SAG_BottomValueBoth
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_BottomValueBoth( p0 )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGcell_str ) p

REAL    xrt, xlt, xrb, xlb, psum
REAL    del, dcx, f1, f2
INTEGER ipt
LOGICAL DoInterp

IF( pgrd(p0%id) /= 0 )THEN

  DO ipt = 0,3

    p    = p0
    p%id = pgrd(p0%id) + ipt

    DoInterp = .NOT.UseSpecial

    IF( UseSpecial )THEN
      IF( p%d == Special )THEN
        IF( pdat(p%id) == Special )THEN
          p%d = Special
          p%drt = 0.0
          p%drb = 0.0
          p%dlt = 0.0
          p%dlb = 0.0
        ELSE
          p%d = pdat(p%id)
          p%drt = grt(p%id)
          p%drb = grb(p%id)
          p%dlt = glt(p%id)
          p%dlb = glb(p%id)
        END IF
      ELSE
        IF( pdat(p%id) == Special )THEN
          IF( pgrd(p%id) == 0 )THEN
            p%d = pdat(p%id)
            p%drt = grt(p%id)
            p%drb = grb(p%id)
            p%dlt = glt(p%id)
            p%dlb = glb(p%id)
          END IF
        ELSE
          DoInterp = .TRUE.
        END IF
      END IF
    END IF

    IF( DoInterp )THEN
      IF( p%d > 0.0 )THEN
        SELECT CASE( ipt )
          CASE( 0 )
            xlb = 0.5*p%dlb
            xrt = -0.125*(3.*p%dlb - p%drt)
            xlt = 0.5*p%dlt
            xrb = 0.5*p%drb
            del = EXP(xlb)
          CASE( 1 )
            xrb = 0.5*p%drb
            xlt = -0.125*(3.*p%drb - p%dlt)
            xrt = 0.5*p%drt
            xlb = 0.5*p%dlb
            del = EXP(xrb)
          CASE( 2 )
            xlt = 0.5*p%dlt
            xrb = -0.125*(3.*p%dlt - p%drb)
            xrt = 0.5*p%drt
            xlb = 0.5*p%dlb
            del = EXP(xlt)
          CASE( 3 )
            xrt = 0.5*p%drt
            xlb = -0.125*(3.*p%drt - p%dlb)
            xlt = 0.5*p%dlt
            xrb = 0.5*p%drb
            del = EXP(xrt)
          CASE DEFAULT
        END SELECT
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = p0%d*del
          dcx = ABS(p0%d*del)
        ELSE
          p%d = p0%d*del + pdat(p%id)
          dcx = ABS(p0%d*del) + ABS(pdat(p%id))
        END IF
        IF( dcx > TINY(dcx) )THEN
          f1 = ABS(p0%d*del)/dcx
        ELSE
          f1 = 0.0
        END IF
        f2 = 1.0 - f1
      ELSE
        xrt = 0.0
        xlb = 0.0
        xlt = 0.0
        xrb = 0.0
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = 0.0
        ELSE
          p%d = pdat(p%id)
        END IF
        f1 = 0.0
        f2 = 1.0
      END IF

      p%drt = xrt*f1 + f2*grt(p%id)
      p%drb = xrb*f1 + f2*grb(p%id)
      p%dlt = xlt*f1 + f2*glt(p%id)
      p%dlb = xlb*f1 + f2*glb(p%id)

      psum = 2.0*LOG(0.25*(EXP(0.5*p%drt) + EXP(0.5*p%drb) + EXP(0.5*p%dlt) + EXP(0.5*p%dlb)))

      p%drt = p%drt - psum
      p%drb = p%drb - psum
      p%dlt = p%dlt - psum
      p%dlb = p%dlb - psum

    END IF

    CALL SAG_BottomValueBoth( p )

  END DO

ELSE

  pdat(p0%id) = p0%d

END IF

RETURN
END
!*******************************************************************************
!               SAG_SetBottomValueHorz
!*******************************************************************************
SUBROUTINE SAG_SetBottomValueHorz( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb

INTEGER ix, iy, id

TYPE( SAGcell_str ) p0

!==== Initialize

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Compute the bottom value

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p0%id  = id
    p0%d   = pdat(id)
    p0%drt = grt(id)
    p0%dlt = glt(id)

    CALL SAG_BottomValueHorz( p0 )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_BottomValueHorz
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_BottomValueHorz( p0 )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGcell_str ) p

REAL    xrt, xlt
REAL    del, dcx, f1, f2
INTEGER ipt
LOGICAL DoInterp

IF( pgrd(p0%id) /= 0 )THEN

  DO ipt = 0,1

    p    = p0
    p%id = pgrd(p0%id) + ipt

    DoInterp = .NOT.UseSpecial

    IF( UseSpecial )THEN
      IF( p%d == Special )THEN
        IF( pdat(p%id) == Special )THEN
          p%d = Special
          p%drt = 0.0
          p%dlt = 0.0
        ELSE
          p%d = pdat(p%id)
          p%drt = grt(p%id)
          p%dlt = glt(p%id)
        END IF
      ELSE
        IF( pdat(p%id) == Special )THEN
          IF( pgrd(p%id) == 0 )THEN
            p%d = pdat(p%id)
            p%drt = grt(p%id)
            p%dlt = glt(p%id)
          END IF
        ELSE
          DoInterp = .TRUE.
        END IF
      END IF
    END IF

    IF( DoInterp )THEN
      IF( p%d > 0.0 )THEN
        SELECT CASE( ipt )
          CASE( 0 )
            xlt = 0.5*p%dlt
            xrt = -xlt
            del = EXP(xlt)
          CASE( 1 )
            xrt = 0.5*p%drt
            xlt = -xrt
            del = EXP(xrt)
          CASE DEFAULT
        END SELECT
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = p0%d*del
          dcx = ABS(p0%d*del)
        ELSE
          p%d = p0%d*del + pdat(p%id)
          dcx = ABS(p0%d*del) + ABS(pdat(p%id))
        END IF
        IF( dcx > TINY(dcx) )THEN
          f1 = ABS(p0%d*del)/dcx
        ELSE
          f1 = 0.0
        END IF
        f2 = 1.0 - f1
      ELSE
        xrt = 0.0
        xlt = 0.0
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = 0.0
        ELSE
          p%d = pdat(p%id)
        END IF
        f1 = 0.0
        f2 = 1.0
      END IF

      p%drt = xrt*f1 + f2*grt(p%id)
      p%dlt = xlt*f1 + f2*glt(p%id)

    END IF

    CALL SAG_BottomValueHorz( p )

  END DO

ELSE

  pdat(p0%id) = p0%d

END IF

RETURN
END
!*******************************************************************************
!               SAG_SetBottomValueVert
!*******************************************************************************
SUBROUTINE SAG_SetBottomValueVert( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
REAL, DIMENSION(*),  TARGET       :: drt, drb, dlt, dlb

INTEGER ix, iy, id

TYPE( SAGcell_str ) p0

!==== Initialize

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Compute the bottom value

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p0%id  = id
    p0%d   = pdat(id)
    p0%drt = grt(id)
    p0%drb = grb(id)

    CALL SAG_BottomValueVert( p0 )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_BottomValueVert
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_BottomValueVert( p0 )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGcell_str ) p

REAL    xrt, xrb
REAL    del, dcx, f1, f2
INTEGER ipt
LOGICAL DoInterp

IF( pgrd(p0%id) /= 0 )THEN

  DO ipt = 0,1

    p    = p0
    p%id = pgrd(p0%id) + ipt

    DoInterp = .NOT.UseSpecial

    IF( UseSpecial )THEN
      IF( p%d == Special )THEN
        IF( pdat(p%id) == Special )THEN
          p%d = Special
          p%drt = 0.0
          p%drb = 0.0
        ELSE
          p%d = pdat(p%id)
          p%drt = grt(p%id)
          p%drb = grb(p%id)
        END IF
      ELSE
        IF( pdat(p%id) == Special )THEN
          IF( pgrd(p%id) == 0 )THEN
            p%d = pdat(p%id)
            p%drt = grt(p%id)
            p%drb = grb(p%id)
          END IF
        ELSE
          DoInterp = .TRUE.
        END IF
      END IF
    END IF

    IF( DoInterp )THEN
      IF( p%d > 0.0 )THEN
        SELECT CASE( ipt )
          CASE( 0 )
            xrb = 0.5*p%drb
            xrt = -xrb
            del = EXP(xrb)
          CASE( 1 )
            xrt = 0.5*p%drt
            xrb = -xrt
            del = EXP(xrt)
          CASE DEFAULT
        END SELECT
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = p0%d*del
          dcx = ABS(p0%d*del)
        ELSE
          p%d = p0%d*del + pdat(p%id)
          dcx = ABS(p0%d*del) + ABS(pdat(p%id))
        END IF
        IF( dcx > TINY(dcx) )THEN
          f1 = ABS(p0%d*del)/dcx
        ELSE
          f1 = 0.0
        END IF
        f2 = 1.0 - f1
      ELSE
        xrt = 0.0
        xrb = 0.0
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = 0.0
        ELSE
          p%d = pdat(p%id)
        END IF
        f1 = 0.0
        f2 = 1.0
      END IF

      p%drt = xrt*f1 + f2*grt(p%id)
      p%drb = xrb*f1 + f2*grb(p%id)

    END IF

    CALL SAG_BottomValueVert( p )

  END DO

ELSE

  pdat(p0%id) = p0%d

END IF

RETURN
END

!*******************************************************************************
!               SAG_BottomSimpleID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomSimpleID( id,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Pushes the data values to the bottom grid level without gradients

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: id
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

INTERFACE
  INTEGER FUNCTION SAG_BottomSimple( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomSimple
END INTERFACE

!------ Initialize

SAG_BottomSimpleID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

!------ Loop over fields

DO i = 1,nfld
  SAG_BottomSimpleID = SAG_BottomSimple( grd,ifld(i) )
  IF( SAG_BottomSimpleID == SAG_ERROR )GOTO 9999
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_BottomSimple( grd,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Pushes the data values to the bottom grid level without gradients

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld

INTEGER, DIMENSION(:), POINTER :: flag !internal flags array

INTERFACE
  SUBROUTINE SAG_SetBottomSimpleBoth( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END SUBROUTINE SAG_SetBottomSimpleBoth
END INTERFACE

!==== Initialize

SAG_BottomSimple = SAG_ERROR

flag => grd%ipflg

!==== Check to see if field is already a Bottom Value field

IF( ifld <= 0 .OR. ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

IF( flag(ifld) /= SAG_GRID_BOTM )THEN

!==== Set bottom values

  flag(ifld) = SAG_GRID_BOTM

  SELECT CASE( grd%type )
    CASE( SAG_GRID_BOTH )
      CALL SAG_SetBottomSimpleBoth( grd,ifld )

    CASE( SAG_GRID_NONE )

    CASE DEFAULT
      LastError = SAG_ERR_TYPE
      LastParm  = grd%type
      GOTO 9999

  END SELECT

END IF

!==== Return

SAG_BottomSimple = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_SetBottomSimpleBoth
!*******************************************************************************
SUBROUTINE SAG_SetBottomSimpleBoth( grd,ifld )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld

INTEGER ix, iy, id

TYPE( SAGcell_str ) p0

!==== Initialize

nx   = grd%nx
ny   = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

! Initialize cell structure values not used

p0%id  = 0
p0%lev = 0
p0%x   = 0.0
p0%y   = 0.0
p0%hx  = 1.0
p0%hy  = 1.0
p0%d   = 0.0
p0%drt = 0.0
p0%drb = 0.0
p0%dlt = 0.0
p0%dlb = 0.0

!==== Compute the bottom value

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p0%id = id
    p0%d  = pdat(id)

    CALL SAG_BottomSimpleBoth( p0 )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_BottomSimpleBoth
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_BottomSimpleBoth( p0 )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGcell_str ) p

INTEGER ipt
LOGICAL DoInterp

IF( pgrd(p0%id) /= 0 )THEN

  DO ipt = 0,3

    p    = p0
    p%id = pgrd(p0%id) + ipt

    DoInterp = .NOT.UseSpecial

    IF( UseSpecial )THEN
      IF( p%d == Special )THEN
        IF( pdat(p%id) == Special )THEN
          p%d = Special
        ELSE
          p%d = pdat(p%id)
        END IF
      ELSE
        IF( pdat(p%id) == Special )THEN
          IF( pgrd(p%id) == 0 )THEN
            p%d = pdat(p%id)
          END IF
        ELSE
          DoInterp = .TRUE.
        END IF
      END IF
    END IF

    IF( DoInterp )THEN
      IF( p%d > 0.0 )THEN
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = p0%d
        ELSE
          p%d = p0%d + pdat(p%id)
        END IF
      ELSE
        IF( UseSpecial .AND. pdat(p%id) == Special )THEN
          p%d = 0.0
        ELSE
          p%d = pdat(p%id)
        END IF
      END IF

    END IF

    CALL SAG_BottomSimpleBoth( p )

  END DO

ELSE

  pdat(p0%id) = p0%d

END IF

RETURN
END
!*******************************************************************************
!               SAG_BottomValueCN2ID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomValueCN2ID( id )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE saggrd_fi
USE PtrGrdStrItf

!     Pushes the data values to the bottom grid level using 2 clipped normals if required

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id

INTEGER, DIMENSION(2) :: ifld

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i, nfld, alloc_stat, nlev

REAL, DIMENSION(:,:), ALLOCATABLE :: dat2      ! Storage for second component mean & variance

INTEGER, EXTERNAL :: SAG_BottomLevelID

INTERFACE

  INTEGER FUNCTION SAG_BottomValue( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomValue

  INTEGER FUNCTION SAG_BottomValueCN2( grd,mx,dat2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER       :: grd
    INTEGER,               INTENT( IN  ) :: mx
    REAL, DIMENSION(2,mx), INTENT( OUT ) :: dat2
  END FUNCTION SAG_BottomValueCN2

END INTERFACE

!------ Initialize

SAG_BottomValueCN2ID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

grd%pushtype = 1        !No multi-distribution by default

IF( grd%type == SAG_GRID_BOTH )THEN

  ALLOCATE( dat2(2,grd%ncells),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = grd%ncells
    GOTO 9999
  END IF

  nlev = SAG_BottomLevelID( id )

  SAG_BottomValueCN2ID = SAG_BottomValueCN2( grd,grd%ncells,dat2 )

  DEALLOCATE( dat2,STAT=alloc_stat )

ELSE

!------ Loop over fields

  nfld = 2; ifld = (/ 1,2 /)

  DO i = 1,nfld
    SAG_BottomValueCN2ID = SAG_BottomValue( grd,ifld(i) )
    IF( SAG_BottomValueCN2ID == SAG_ERROR )GOTO 9999
  END DO

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_BottomValueCN2( grd,mx,dat2 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE reallocate

!------  Pushes the data values to the bottom grid level

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER       :: grd
INTEGER,               INTENT( IN  ) :: mx
REAL, DIMENSION(2,mx), INTENT( OUT ) :: dat2

INTEGER isize, irv, alloc_stat, i
LOGICAL lComp2

INTEGER, DIMENSION(:),       POINTER :: flag !internal flags array
REAL,    DIMENSION(:,:), ALLOCATABLE :: drt, drb, dlt, dlb !gradient data

INTERFACE
  SUBROUTINE SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN    ) :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientBoth

  SUBROUTINE SAG_SetBottomValueCN2Both( grd,mx,dat2,drt,drb,dlt,dlb,lComp2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER       :: grd
    INTEGER,               INTENT( IN  ) :: mx
    REAL, DIMENSION(2,mx), INTENT( OUT ) :: dat2
    REAL, DIMENSION(mx,2),  TARGET        :: drt, drb, dlt, dlb
    LOGICAL,               INTENT( OUT ) :: lComp2
  END SUBROUTINE SAG_SetBottomValueCN2Both

  INTEGER FUNCTION SAG_UpperClear( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION

END INTERFACE

!==== Initialize

SAG_BottomValueCN2 = SAG_ERROR

flag => grd%ipflg

!==== Check to see if field is already a Bottom Value field

IF( flag(1) /= SAG_GRID_BOTM )THEN

!==== Set up gradient arrays

  isize = grd%ncells

  ALLOCATE( drt(isize,2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( drb(isize,2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( dlt(isize,2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

  ALLOCATE( dlb(isize,2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = isize
    GOTO 9999
  END IF

!==== Compute gradients and set bottom values

  flag(1) = SAG_GRID_BOTM
  flag(2) = SAG_GRID_BOTM

  CALL SAG_SetGradientBoth( grd,1,drt(1,1),drb(1,1),dlt(1,1),dlb(1,1) )
  CALL SAG_SetGradientBoth( grd,2,drt(1,2),drb(1,2),dlt(1,2),dlb(1,2) )
  CALL SAG_SetBottomValueCN2Both( grd,mx,dat2,drt,drb,dlt,dlb,lComp2 )

  IF( lComp2 )THEN   ! 2 clipped normals required
    irv = reallocate_real1d( grd%ipdat,2*grd%mxgrd )
    irv = reallocate_integer1d( grd%ipflg,2 )
    grd%pushtype = 2        !Set multi-distribution value
    grd%mxfld    = 5
    grd%nvart    = 5

!--- Copy second pdf component into fields 4 and 5 (1 and 2 are first component)


    DO i = 1,grd%ncells
      IF( grd%ipgrd(i) == 0 )THEN
        grd%ipdat(3*grd%mxgrd+i) = dat2(1,i)
        grd%ipdat(4*grd%mxgrd+i) = dat2(2,i)
      ELSE
        grd%ipdat(3*grd%mxgrd+i) = 0.0
        grd%ipdat(4*grd%mxgrd+i) = 0.0
      END IF
    END DO

  END IF

!==== Clear upper values

  irv = SAG_UpperClear( grd,1 )
  IF( irv == SAG_OK )irv = SAG_UpperClear( grd,2 )
  IF( irv /= SAG_OK )THEN
    SAG_BottomValueCN2 = irv
    GOTO 9999
  END IF

END IF

!==== Return

SAG_BottomValueCN2 = SAG_OK

9999 CONTINUE

IF( ALLOCATED(drt) )DEALLOCATE( drt,STAT=alloc_stat )
IF( ALLOCATED(drb) )DEALLOCATE( drb,STAT=alloc_stat )
IF( ALLOCATED(dlt) )DEALLOCATE( dlt,STAT=alloc_stat )
IF( ALLOCATED(dlb) )DEALLOCATE( dlb,STAT=alloc_stat )

RETURN
END
!*******************************************************************************
!               SAG_SetBottomValueCN2Both
!*******************************************************************************
SUBROUTINE SAG_SetBottomValueCN2Both( grd,mx,dat2,drt,drb,dlt,dlb,lComp2 )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER       :: grd
INTEGER,               INTENT( IN  ) :: mx
REAL, DIMENSION(2,mx), INTENT( OUT ) :: dat2
REAL, DIMENSION(mx,2), TARGET        :: drt, drb, dlt, dlb
LOGICAL,               INTENT( OUT ) :: lComp2

INTEGER ix, iy, id

TYPE( SAGcell_str ), DIMENSION(2) :: pMean, pVar

!==== Initialize

nx   = grd%nx
ny   = grd%ny
pgrd => grd%ipgrd
pdat  => grd%ipdat(1:grd%ncells)
pdat2 => grd%ipdat(grd%mxgrd+1:grd%mxgrd+grd%ncells)

grt2 => drt(:,1:2)
grb2 => drb(:,1:2)
glt2 => dlt(:,1:2)
glb2 => dlb(:,1:2)

! Initialize cell structure values not used

pMean(1)%lev = 0
pMean(1)%x   = 0.0
pMean(1)%y   = 0.0
pMean(1)%hx  = 1.0
pMean(1)%hy  = 1.0

pVar(1)%lev = 0
pVar(1)%x   = 0.0
pVar(1)%y   = 0.0
pVar(1)%hx  = 1.0
pVar(1)%hy  = 1.0

pMean(2) = pMean(1)
pVar(2)  = pVar(1)

lComp2 = .FALSE.

!==== Compute the bottom value

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    pMean(1)%id  = id
    pMean(1)%d   = pdat(id)
    pMean(1)%drt = grt2(id,1)
    pMean(1)%dlt = glt2(id,1)
    pMean(1)%drb = grb2(id,1)
    pMean(1)%dlb = glb2(id,1)

    pVar(1)%id  = id
    pVar(1)%d   = pdat2(id)
    pVar(1)%drt = grt2(id,2)
    pVar(1)%dlt = glt2(id,2)
    pVar(1)%drb = grb2(id,2)
    pVar(1)%dlb = glb2(id,2)

    pMean(2)%id  = id
    pMean(2)%d   = 0.0
    pMean(2)%drt = 0.0
    pMean(2)%dlt = 0.0
    pMean(2)%drb = 0.0
    pMean(2)%dlb = 0.0

    pvar(2) = pMean(2)

    CALL SAG_BottomValueCN2Both( pMean,pVar,mx,dat2,lComp2 )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_BottomValueCN2Both
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_BottomValueCN2Both( pMean0,pVar0,mx,dat2,lComp2 )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), DIMENSION(2), INTENT( INOUT ) :: pMean0, pVar0
INTEGER,                           INTENT( IN    ) :: mx
REAL, DIMENSION(2,mx),             INTENT( OUT   ) :: dat2
LOGICAL,                           INTENT( INOUT ) :: lComp2

REAL, PARAMETER :: CHECK_INCREASE_HIGH  = 0.95
REAL, PARAMETER :: CHECK_INCREASE_LOW   = 0.25
REAL, PARAMETER :: CHECK_INCREASE_RANGE = CHECK_INCREASE_HIGH - CHECK_INCREASE_LOW
REAL, PARAMETER :: CHECK_DECREASE_HIGH  = 0.3
REAL, PARAMETER :: CHECK_DECREASE_LOW   = 0.2
REAL, PARAMETER :: CHECK_DECREASE_RANGE = CHECK_DECREASE_HIGH - CHECK_DECREASE_LOW

TYPE( SAGcell_str ), DIMENSION(2) :: pMean, pVar

REAL xrt, xlt, xrb, xlb, addFrac, facV
REAL yrt, ylt, yrb, ylb, rat0, rat1, ratc
REAL delM, delV, dcxM, dcxV, f1m, f2m, f1v, f2v

INTEGER ipt, id, i, nc
LOGICAL DoInterp, addComp, check

IF( pgrd(pMean0(1)%id) /= 0 )THEN

  DO ipt = 0,3

    pMean    = pMean0
    pVar     = pVar0
    pMean(1)%id = pgrd(pMean(1)%id) + ipt
    pVar(1)%id  = pgrd(pVar(1)%id)  + ipt

    DoInterp = .NOT.UseSpecial

    pMean(1)%lev = pMean(1)%lev + 1
    id = pMean(1)%id

    IF( UseSpecial )THEN
      IF( pMean(1)%d == Special )THEN
        IF( pdat(id) == Special )THEN
          pMean(1)%d   = Special
          pMean(1)%drt = 0.0
          pMean(1)%drb = 0.0
          pMean(1)%dlt = 0.0
          pMean(1)%dlb = 0.0
          pVar(1)%d   = Special
          pVar(1)%drt = 0.0
          pVar(1)%drb = 0.0
          pVar(1)%dlt = 0.0
          pVar(1)%dlb = 0.0
        ELSE
          pMean(1)%d   = pdat(id)
          pMean(1)%drt = grt2(id,1)
          pMean(1)%drb = grb2(id,1)
          pMean(1)%dlt = glt2(id,1)
          pMean(1)%dlb = glb2(id,1)
          pVar(1)%d   = pdat2(id)
          pVar(1)%drt = grt2(id,2)
          pVar(1)%drb = grb2(id,2)
          pVar(1)%dlt = glt2(id,2)
          pVar(1)%dlb = glb2(id,2)
        END IF
      ELSE
        IF( pdat(id) == Special )THEN
          IF( pgrd(id) == 0 )THEN
            pMean(1)%d   = pdat(id)
            pMean(1)%drt = grt2(id,1)
            pMean(1)%drb = grb2(id,1)
            pMean(1)%dlt = glt2(id,1)
            pMean(1)%dlb = glb2(id,1)
            pVar(1)%d   = pdat2(id)
            pVar(1)%drt = grt2(id,2)
            pVar(1)%drb = grb2(id,2)
            pVar(1)%dlt = glt2(id,2)
            pVar(1)%dlb = glb2(id,2)
          END IF
        ELSE
          DoInterp = .TRUE.
        END IF
      END IF
    END IF

    IF( DoInterp )THEN

      IF( pMean(1)%d > 0.0 )THEN
        nc = 1
        IF( pMean(2)%d > 0.0 )nc = 2

        DO i = 1,nc

          SELECT CASE( ipt )
            CASE( 0 )
              xlb = 0.5*pMean(i)%dlb
              xrt = -0.125*(3.*pMean(i)%dlb - pMean(i)%drt)
              xlt = 0.5*pMean(i)%dlt
              xrb = 0.5*pMean(i)%drb
              delM = EXP(xlb)
              ylb = 0.5*pVar(i)%dlb
              yrt = -0.125*(3.*pVar(i)%dlb - pVar(i)%drt)
              ylt = 0.5*pVar(i)%dlt
              yrb = 0.5*pVar(i)%drb
              delV = EXP(ylb)
            CASE( 1 )
              xrb = 0.5*pMean(i)%drb
              xlt = -0.125*(3.*pMean(i)%drb - pMean(i)%dlt)
              xrt = 0.5*pMean(i)%drt
              xlb = 0.5*pMean(i)%dlb
              delM = EXP(xrb)
              yrb = 0.5*pVar(i)%drb
              ylt = -0.125*(3.*pVar(i)%drb - pVar(i)%dlt)
              yrt = 0.5*pVar(i)%drt
              ylb = 0.5*pVar(i)%dlb
              delV = EXP(yrb)
            CASE( 2 )
              xlt = 0.5*pMean(i)%dlt
              xrb = -0.125*(3.*pMean(i)%dlt - pMean(i)%drb)
              xrt = 0.5*pMean(i)%drt
              xlb = 0.5*pMean(i)%dlb
              delM = EXP(xlt)
              ylt = 0.5*pVar(i)%dlt
              yrb = -0.125*(3.*pVar(i)%dlt - pVar(i)%drb)
              yrt = 0.5*pVar(i)%drt
              ylb = 0.5*pVar(i)%dlb
              delV = EXP(ylt)
            CASE( 3 )
              xrt = 0.5*pMean(i)%drt
              xlb = -0.125*(3.*pMean(i)%drt - pMean(i)%dlb)
              xlt = 0.5*pMean(i)%dlt
              xrb = 0.5*pMean(i)%drb
              delM = EXP(xrt)
              yrt = 0.5*pVar(i)%drt
              ylb = -0.125*(3.*pVar(i)%drt - pVar(i)%dlb)
              ylt = 0.5*pVar(i)%dlt
              yrb = 0.5*pVar(i)%drb
              delV = EXP(yrt)
            CASE DEFAULT
          END SELECT

          IF( UseSpecial .AND. pdat(id) == Special )THEN

            pMean(i)%d = pMean0(i)%d*delM
            pVar(i)%d  = pVar0(i)%d *delV
            delM = pMean0(i)%d*delM
            delV = pVar0(i)%d *delV
            dcxM = ABS(delM)
            dcxV = ABS(delV)

          ELSE

            IF( nc == 1 )THEN

              delM = pMean0(1)%d*delM
              delV = pVar0(1)%d *delV

              rat0 = delV/(delM*delM)
              rat1 = (delV+pdat2(id))/(delM+pdat(id))**2
              IF( pdat2(id) > 0.0 )THEN
                ratc = delV*pdat(id)/(delM*pdat2(id))   !Test for sums from same signal
                check = (ratc < 0.8) .OR. (ratc > 1.2)
              ELSE
                check = .FALSE.
              END IF
              IF( rat0 < 400.0 .AND. check )THEN
                IF( rat0 < CHECK_INCREASE_HIGH*rat1 )THEN     !New cell is more uncertain
                  addComp = (pdat(id) > 0.3*delM) .OR. (pdat2(id) > 0.3*delV)
                  IF( addComp )THEN
                    ratc = pdat2(id)/pdat(id)**2
                    IF( rat0 < CHECK_INCREASE_LOW*rat1 )THEN
                      addFrac = 1.0
                    ELSE
                      addFrac = (CHECK_INCREASE_HIGH - rat0/rat1)/CHECK_INCREASE_RANGE
                      addFrac = 0.1 + 0.9*addFrac
                    END IF
                  END IF
                ELSE IF( rat1 < CHECK_DECREASE_HIGH*rat0 )THEN   !New cell is more certain
                  ratc = pdat2(id)/pdat(id)**2
                  addComp = (ratc < rat1)    !No new component if sum is less uncertain than either
                  IF( addComp )THEN
                    IF( rat1 < CHECK_DECREASE_LOW*rat0 )THEN
                      addFrac = 1.0
                    ELSE
                      addFrac = (CHECK_DECREASE_HIGH - rat1/rat0)/CHECK_DECREASE_RANGE
                      addFrac = 0.5 + 0.5*addFrac
                    END IF
                  END IF
                ELSE
                  addComp = .FALSE.
                END IF
              ELSE
                addComp = .FALSE.
              END IF

              IF( addComp )THEN
                lComp2 = .TRUE.
                pVar(1)%lev = pMean(1)%lev
                pMean(1)%d = delM + (1.0-addFrac)*pdat(id)
                pMean(2)%d = addFrac*pdat(id)
                IF( pdat2(id) > 0.0 .AND. (1.0-addFrac) > 0.0 )THEN
                  IF( rat1 > rat0 )THEN
                    facV = addFrac*rat0 + (1.0-addFrac)*MIN(2.0*rat0,rat1)
                    facV = facV*pMean(1)%d**2 - delV
                    facV = MIN(facV,(1.0-addFrac)*pdat2(id))
                  ELSE
                    facV = addFrac*rat0 + (1.0-addFrac)*MAX(0.5*rat0,rat1)
                    facV = facV*pMean(1)%d**2 - delV
                    facV = MAX(facV,(1.0-addFrac)*pdat2(id))
                    IF( (pdat2(id)-facV)/pmean(2)%d**2 < ratc )THEN
                      facV = pdat2(id) - ratc*pmean(2)%d**2
                    END IF
                  END IF
                  pVar(1)%d = delV + facV
                  pVar(2)%d = pdat2(id) - facV
                ELSE
                  pVar(1)%d = delV
                  pVar(2)%d = pdat2(id)
                END IF
                dcxM = ABS(pMean(1)%d)
                dcxV = ABS(pVar(1)%d)
                pMean(2)%drt = grt2(id,1)
                pMean(2)%drb = grb2(id,1)
                pMean(2)%dlt = glt2(id,1)
                pMean(2)%dlb = glb2(id,1)
                pVar(2)%drt  = grt2(id,2)
                pVar(2)%drb  = grb2(id,2)
                pVar(2)%dlt  = glt2(id,2)
                pVar(2)%dlb  = glb2(id,2)
              ELSE
                pMean(1)%d = delM + pdat(id)
                pVar(1)%d  = delV + pdat2(id)
                dcxM = ABS(delM) + ABS(pdat(id))
                dcxV = ABS(delV) + ABS(pdat2(id))
              END IF

            ELSE

              delM = pMean(i)%d*delM
              delV = pVar(i)%d *delV

              IF( i == 2 )THEN
                pMean(i)%d = delM + pdat(id)
                pVar(i)%d  = delV + pdat2(id)
                dcxM = ABS(delM) + ABS(pdat(id))
                dcxV = ABS(delV) + ABS(pdat2(id))
              ELSE
                pMean(i)%d = delM
                pVar(i)%d  = delV
                dcxM = ABS(delM)
                dcxV = ABS(delV)
              END IF

           END IF
          END IF

          IF( dcxM > TINY(dcxM) )THEN
            f1m = ABS(delM)/dcxM
          ELSE
            f1m = 0.0
          END IF
          IF( dcxV > TINY(dcxV) )THEN
            f1v = ABS(delV)/dcxV
          ELSE
            f1v = 0.0
          END IF
          f2m = 1.0 - f1m
          f2v = 1.0 - f1v

          pMean(i)%drt = xrt*f1m + f2m*grt2(id,1)
          pMean(i)%drb = xrb*f1m + f2m*grb2(id,1)
          pMean(i)%dlt = xlt*f1m + f2m*glt2(id,1)
          pMean(i)%dlb = xlb*f1m + f2m*glb2(id,1)

          pVar(i)%drt = yrt*f1v + f2v*grt2(id,2)
          pVar(i)%drb = yrb*f1v + f2v*grb2(id,2)
          pVar(i)%dlt = ylt*f1v + f2v*glt2(id,2)
          pVar(i)%dlb = ylb*f1v + f2v*glb2(id,2)

        END DO  ! Components

      ELSE  ! First non-zero cell encountered

        IF( UseSpecial .AND. pdat(id) == Special )THEN
          pMean(1)%d = 0.0
          pVar(1)%d  = 0.0
        ELSE
          pMean(1)%d = pdat(id)
          pVar(1)%d  = pdat2(id)
        END IF

        pMean(1)%drt = grt2(id,1)
        pMean(1)%drb = grb2(id,1)
        pMean(1)%dlt = glt2(id,1)
        pMean(1)%dlb = glb2(id,1)

        pVar(1)%drt = grt2(id,2)
        pVar(1)%drb = grb2(id,2)
        pVar(1)%dlt = glt2(id,2)
        pVar(1)%dlb = glb2(id,2)

      END IF

    END IF   ! Interpolation

    CALL SAG_BottomValueCN2Both( pMean,pVar,mx,dat2,lComp2 )

  END DO  ! 4 subcells

ELSE ! Bottom cell

  id = pMean0(1)%id

!--- Store components

  IF( pMean0(1)%d < 0.01*pMean0(2)%d )THEN

    IF( SQRT(pVar0(1)%d)/pMean0(1)%d > 1.5*SQRT(pVar0(2)%d)/pMean0(2)%d )THEN
      IF( pVar0(1)%d < 0.01*pVar0(2)%d )THEN
        pMean0(1)%d = pMean0(1)%d + pMean0(2)%d
        pVar0(1)%d  = pVar0(1)%d  + pVar0(2)%d

        pMean0(2)%d = 0.0
        pVar0(2)%d  = 0.0
      END IF
    END IF

  ELSE IF( pMean0(2)%d < 0.01*pMean0(1)%d )THEN

    IF( SQRT(pVar0(2)%d)/pMean0(2)%d > 1.5*SQRT(pVar0(1)%d)/pMean0(1)%d )THEN
      IF( pVar0(2)%d < 0.01*pVar0(1)%d )THEN
        pMean0(1)%d = pMean0(1)%d + pMean0(2)%d
        pVar0(1)%d  = pVar0(1)%d  + pVar0(2)%d

        pMean0(2)%d = 0.0
        pVar0(2)%d  = 0.0
      END IF
    END IF

  END IF

  pdat(id)  = pMean0(1)%d
  pdat2(id) = pVar0(1)%d

  dat2(1,id) = pMean0(2)%d
  dat2(2,id) = pVar0(2)%d

END IF

RETURN
END
