!*******************************************************************************
!               SAG_CombineGrid
!*******************************************************************************
INTEGER FUNCTION SAG_CombineGridID( grdI1,grdI2,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI1, grdI2
INTEGER, EXTERNAL     :: UserFunction

TYPE ( SAGgrid_str ), POINTER  :: grd1, grd2

INTERFACE
  INTEGER FUNCTION SAG_CombineGrid( grd1,grd2,UserFunction )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd1
    TYPE( SAGgrid_str ), POINTER :: grd2
    INTEGER, EXTERNAL            :: UserFunction
  END FUNCTION SAG_CombineGrid
END INTERFACE

!==== Initialize result

SAG_CombineGridID = SAG_ERROR

!==== Point to structures

grd1 => SAG_PtrGrdStr( grdI1 )
IF( .NOT.ASSOCIATED( grd1 ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI1
  GOTO 9999
END IF

grd2 => SAG_PtrGrdStr( grdI2 )
IF( .NOT.ASSOCIATED( grd2 ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI2
  GOTO 9999
END IF

SAG_CombineGridID = SAG_CombineGrid( grd1,grd2,UserFunction )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_CombineGrid( grd1,grd2,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER  :: grd1
TYPE( SAGgrid_str ), POINTER  :: grd2
INTEGER,             EXTERNAL :: UserFunction

TYPE( SAGcell_str ) p1,p2

INTEGER ix,iy,icell,Error

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_CombineWalk( grd1,grd2,p1,p2,UserFunction )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd1,grd2
    TYPE( SAGcell_str ), INTENT( IN ) :: p1,p2
    INTEGER,             EXTERNAL     :: UserFunction
  END FUNCTION SAG_CombineWalk
END INTERFACE

!==== Initialize result

SAG_CombineGrid = SAG_ERROR

!==== Initial error checking

IF( grd1%nx /= grd2%nx )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 1
  GOTO 9999
END IF

IF( grd1%ny /= grd2%ny )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 2
  GOTO 9999
END IF

IF( grd1%type /= grd2%type )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 3
  GOTO 9999
END IF

!==== Walk the grid

DO ix = 1,grd1%nx
  DO iy = 1,grd1%ny

    icell = (iy-1)*grd1%nx + ix

    p1%id  = icell
    p1%lev = 0
    p1%x   = FLOAT(ix) - 0.5
    p1%y   = FLOAT(iy) - 0.5
    p1%hx  = 1.0
    p1%hy  = 1.0
    p1%d   = 0.0
    p1%drt = 0.0
    p1%drb = 0.0
    p1%dlt = 0.0
    p1%dlb = 0.0

    p2 = p1

    Error = SAG_CombineWalk( grd1,grd2,p1,p2,UserFunction )
    IF( Error /= SAG_OK )GOTO 9999

  END DO
END DO

IF( grd1%ncells /= grd2%ncells )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 5
  GOTO 9999
END IF

!==== Set Return value

SAG_CombineGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CombineWalk
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_CombineWalk( grd1,grd2,p1,p2,UserFunction ) RESULT (irv)

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd1,grd2
TYPE( SAGcell_str ), INTENT( IN ) :: p1,p2

INTERFACE
  INTEGER FUNCTION UserFunction(dat1,mx1,dat2,mx2,p1,p2)
    USE sagcel_fd
    REAL,    POINTER, DIMENSION(:)    :: dat1, dat2
    INTEGER,             INTENT( IN ) :: mx1, mx2
    TYPE( SAGcell_str ), INTENT( IN ) :: p1, p2
  END FUNCTION UserFunction
  INTEGER FUNCTION SAG_Reallocate( grd,mxgrd,mxfld)
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER, INTENT( IN )             :: mxgrd
    INTEGER, INTENT( IN )             :: mxfld
  END FUNCTION SAG_Reallocate
END INTERFACE

TYPE( SAGcell_str ) p1x,p2x

INTEGER ipt,nrfm,ix,iy,icell,ifld,Error,mxgrd
REAL    rfx,rfy,xx,yy

INTEGER, POINTER, DIMENSION(:) :: igrd1
INTEGER, POINTER, DIMENSION(:) :: igrd2
REAL,    POINTER, DIMENSION(:) :: dat1
REAL,    POINTER, DIMENSION(:) :: dat2

irv = SAG_ERROR

IF( .NOT.ASSOCIATED( grd1%ipgrd ) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED( grd2%ipgrd ) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

igrd1 => grd1%ipgrd
igrd2 => grd2%ipgrd
dat1  => grd1%ipdat
dat2  => grd2%ipdat

SELECT CASE( grd1%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
    rfx  = 0.5
    rfy  = 0.5
  CASE( SAG_GRID_HORZ )
    nrfm = 1
    rfx  = 0.5
    rfy  = 1.0
  CASE( SAG_GRID_VERT )
    nrfm = 1
    rfx  = 1.0
    rfy  = 0.5
  CASE (SAG_GRID_NONE)
    nrfm = -1
    rfx  = 1.0
    rfy  = 1.0
  CASE DEFAULT
    GOTO 9999
END SELECT

Error = UserFunction( dat1,grd1%mxgrd,dat2,grd2%mxgrd,p1,p2 )
IF( Error /= SAG_OK )THEN
  LastError = SAG_ERR_USER
  GOTO 9999
END IF

IF( igrd1(p1%id) /= 0 .OR. igrd2(p2%id) /= 0 )THEN

!== Check for need to expand grid

  IF( igrd1(p1%id) == 0 )THEN

!== If grid too small reallocate : Only add 10% more since original guess should have been close
    IF( grd1%ncells+nrfm+1 > grd1%mxgrd )THEN
      mxgrd = NINT(1.1*FLOAT(grd1%mxgrd))
      Error = SAG_Reallocate(grd1,mxgrd,grd1%mxfld)
      IF( Error /= SAG_OK )THEN
        LastError = SAG_ERR_INCOMP
        LastParm  = 4
        GOTO 9999
      END IF
      igrd1 => grd1%ipgrd
      dat1  => grd1%ipdat
    END IF
    icell = grd1%ncells+1
    igrd1(p1%id) = icell
    DO ipt = 0,nrfm
      igrd1(icell+ipt) = 0
      DO ifld = 1,grd1%mxfld
        dat1(icell+ipt+grd1%mxgrd*(ifld-1)) = 0.
      END DO
    END DO
    grd1%ncells = grd1%ncells + nrfm + 1

  ELSE IF( igrd2(p2%id) == 0 )THEN

!== If grid too small reallocate : Only add 10% more since original guess should have been close
    IF( grd2%ncells+nrfm+1 > grd2%mxgrd )THEN
      mxgrd = NINT(1.1*FLOAT(grd2%mxgrd))
      Error = SAG_Reallocate(grd2,mxgrd,grd2%mxfld)
      IF( Error /= SAG_OK )THEN
        LastError = SAG_ERR_INCOMP
        LastParm  = 4
        GOTO 9999
      END IF
      igrd2 => grd2%ipgrd
      dat2  => grd2%ipdat
    END IF
    icell = grd2%ncells+1
    igrd2(p2%id) = icell
    DO ipt = 0,nrfm
      igrd2(icell+ipt) = 0
      DO ifld = 1,grd2%mxfld
        dat2(icell+ipt+grd2%mxgrd*(ifld-1)) = 0.
      END DO
    END DO
    grd2%ncells = grd2%ncells + nrfm + 1

  END IF

  DO ipt = 0,nrfm

!== Must redefine igrd1 and igrd2 because this is a recursive function and grids may be
!   reallocated in the preceeding section. These statements assure that we are pointing to
!   the correct data.

    igrd1 => grd1%ipgrd
    igrd2 => grd2%ipgrd

    iy = ipt/NINT(1./rfx)
    ix = ipt - 2*iy
    xx = (FLOAT(ix)-0.5)*(1.-rfx)*p1%hx
    yy = (FLOAT(iy)-0.5)*(1.-rfy)*p1%hy

    p1x%id = igrd1(p1%id) + ipt
    p1x%x  = p1%x + xx
    p1x%y  = p1%y + yy
    p1x%hx = rfx*p1%hx
    p1x%hy = rfy*p1%hy

    xx = (FLOAT(ix)-0.5)*(1.-rfx)*p2%hx
    yy = (FLOAT(iy)-0.5)*(1.-rfy)*p2%hy

    p2x%id = igrd2(p2%id) + ipt
    p2x%x  = p2%x + xx
    p2x%y  = p2%y + yy
    p2x%hx = rfx*p2%hx
    p2x%hy = rfy*p2%hy

    Error = SAG_CombineWalk( grd1,grd2,p1x,p2x,UserFunction )
    IF( Error /= SAG_OK )GOTO 9999
  END DO

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END

!*******************************************************************************

INTEGER FUNCTION SAG_UnionGridID( grdI1,grdI2 )

!------ Creates union of two grids in grdI1

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI1, grdI2

TYPE ( SAGgrid_str ), POINTER  :: grd1, grd2

INTERFACE
  INTEGER FUNCTION SAG_UnionGrid( grd1,grd2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd1
    TYPE( SAGgrid_str ), POINTER :: grd2
  END FUNCTION SAG_UnionGrid
END INTERFACE

!==== Initialize result

SAG_UnionGridID = SAG_ERROR

!==== Point to structures

grd1 => SAG_PtrGrdStr( grdI1 )

!---- No check on first grid - if not allocated, just copy

grd2 => SAG_PtrGrdStr( grdI2 )
IF( .NOT.ASSOCIATED( grd2 ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI2
  GOTO 9999
END IF

SAG_UnionGridID = SAG_UnionGrid( grd1,grd2 )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_UnionGrid( grd1,grd2 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER  :: grd1
TYPE( SAGgrid_str ), POINTER  :: grd2

TYPE( SAGcell_str ) p1,p2

INTEGER ix,iy,icell,Error,alloc_stat

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_UnionWalk( grd1,grd2,p1,p2 )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd1,grd2
    TYPE( SAGcell_str ), INTENT( IN ) :: p1,p2
  END FUNCTION SAG_UnionWalk
END INTERFACE

!==== Initialize result

SAG_UnionGrid = SAG_ERROR

!==== Initial error checking

IF( grd1%nx /= grd2%nx )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 1
  GOTO 9999
END IF

IF( grd1%ny /= grd2%ny )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 2
  GOTO 9999
END IF

IF( grd1%type /= grd2%type )THEN
  LastError = SAG_ERR_INCOMP
  LastParm  = 3
  GOTO 9999
END IF

IF( ASSOCIATED(grd1%ipgrd) )THEN

!==== Walk the grid

  DO ix = 1,grd1%nx
    DO iy = 1,grd1%ny

      icell = (iy-1)*grd1%nx + ix

      p1%id  = icell
      p1%lev = 0
      p1%x   = FLOAT(ix) - 0.5
      p1%y   = FLOAT(iy) - 0.5
      p1%hx  = 1.0
      p1%hy  = 1.0
      p1%d   = 0.0
      p1%drt = 0.0
      p1%drb = 0.0
      p1%dlt = 0.0
      p1%dlb = 0.0

      p2 = p1

      Error = SAG_UnionWalk( grd1,grd2,p1,p2 )
      IF( Error /= SAG_OK )GOTO 9999

    END DO
  END DO

ELSE

  ALLOCATE( grd1%ipgrd(grd2%ncells),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    LastError = SAG_ERR_INCOMP
    LastParm  = 5
    GOTO 9999
  END IF

  DO ix = 1,grd2%ncells
    grd1%ipgrd(ix) = grd2%ipgrd(ix)
  END DO

  grd1%ncells = grd2%ncells
  grd1%mxgrd  = grd1%ncells

END IF

!==== Set Return value

SAG_UnionGrid = SAG_OK

9999 CONTINUE

RETURN
END

!*******************************************************************************

RECURSIVE INTEGER FUNCTION SAG_UnionWalk( grd1,grd2,p1,p2 ) RESULT (irv)

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd1, grd2
TYPE( SAGcell_str ), INTENT( IN ) :: p1, p2

INTERFACE
  INTEGER FUNCTION SAG_ReallocateGrid( grd,mxgrd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER, INTENT( IN )             :: mxgrd
  END FUNCTION SAG_ReallocateGrid
END INTERFACE

TYPE( SAGcell_str ) p1x,p2x

INTEGER ipt, nrfm, icell, Error, mxgrd

INTEGER, POINTER, DIMENSION(:) :: igrd1
INTEGER, POINTER, DIMENSION(:) :: igrd2

irv = SAG_ERROR

IF( .NOT.ASSOCIATED( grd1%ipgrd ) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED( grd2%ipgrd ) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

igrd1 => grd1%ipgrd
igrd2 => grd2%ipgrd

SELECT CASE( grd1%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE( SAG_GRID_HORZ )
    nrfm = 1
  CASE( SAG_GRID_VERT )
    nrfm = 1
  CASE (SAG_GRID_NONE)
    nrfm = -1
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( igrd2(p2%id) /= 0 )THEN

!== Check for need to expand grid

  IF( igrd1(p1%id) == 0 )THEN

!== If grid too small reallocate : Only add 10% more since original guess should have been close
    IF( grd1%ncells+nrfm+1 > grd1%mxgrd )THEN
      mxgrd = NINT(1.1*FLOAT(grd1%mxgrd))
      Error = SAG_ReallocateGrid( grd1,mxgrd )
      IF( Error /= SAG_OK )THEN
        LastError = SAG_ERR_INCOMP
        LastParm  = 4
        GOTO 9999
      END IF
      igrd1 => grd1%ipgrd
    END IF
    icell = grd1%ncells+1
    igrd1(p1%id) = icell
    grd1%ncells = grd1%ncells + nrfm + 1

  END IF

  DO ipt = 0,nrfm

!== Must redefine igrd1 because this is a recursive function and grid may be reallocated
!   in the preceeding section. These statements assure that we are pointing to
!   the correct data.

    igrd1 => grd1%ipgrd

    p1x%id = igrd1(p1%id) + ipt
    p2x%id = igrd2(p2%id) + ipt

    Error = SAG_UnionWalk( grd1,grd2,p1x,p2x )
    IF( Error /= SAG_OK )GOTO 9999
  END DO

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END

!*******************************************************************************

INTEGER FUNCTION SAG_ReallocateGrid( grd,mxgrd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER, INTENT( IN )             :: mxgrd

INTEGER, DIMENSION(:), POINTER :: old_grid !grid data
INTEGER, DIMENSION(:), POINTER :: new_grid !grid data

INTEGER i, ios

SAG_ReallocateGrid = SAG_ERROR

IF( mxgrd < grd%ncells )THEN
  LastError = SAG_ERR_MXGRD
  LastParm  = grd%ncells
  GOTO 9999
END IF

old_grid => grd%ipgrd

NULLIFY( new_grid )

IF( mxgrd > 0 )THEN

  ALLOCATE( new_grid(mxgrd),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxgrd
    GOTO 9999
  END IF

ELSE

  LastError = SAG_ERR_MXGRD
  LastParm  = mxgrd
  GOTO 9999

END IF

IF( ASSOCIATED(new_grid) )THEN

  DO i = 1,grd%ncells
    new_grid(i) = old_grid(i)
  END DO
  DO i = grd%ncells+1,mxgrd
    new_grid(i) = 0
  END DO

  grd%mxgrd = mxgrd

  grd%ipgrd => new_grid

  IF( ASSOCIATED(old_grid) )DEALLOCATE( old_grid,STAT=ios )

  SAG_ReallocateGrid = SAG_OK

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SAG_merge( grd1Data,mxgrd1,grd2Data,mxgrd2,p1,p2 )

USE sagdef_fd
USE sagcel_fd
USE sagmrg_usr

IMPLICIT NONE

REAL, POINTER, DIMENSION(:)        :: grd1Data,grd2Data
INTEGER,              INTENT( IN ) :: mxgrd1,mxgrd2
TYPE ( SAGcell_str ), INTENT( IN ) :: p1,p2

INTEGER i1,i2,i

i1 = (ifld_mrg-1)*mxgrd1 + p1%id
i2 = (jfld_mrg-1)*mxgrd2 + p2%id

DO i = 1,nfld_mrg

  grd1Data(i1) = grd2Data(i2)

  i1 = i1 + mxgrd1
  i2 = i2 + mxgrd2

END DO

SAG_merge = SAG_OK

RETURN
END

!===============================================================================

LOGICAL FUNCTION SAG_CheckCombTooManyCells()

USE sagerr_fd
USE sagerr_fi

IMPLICIT NONE

SAG_CheckCombTooManyCells = LastError == SAG_ERR_INCOMP .AND. &
                            LastParm  == 4

RETURN
END
