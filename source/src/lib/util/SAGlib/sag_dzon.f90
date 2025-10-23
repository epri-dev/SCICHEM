RECURSIVE INTEGER FUNCTION SAG_Dezone( grdI,nc,UserRoutine )

USE sagdef_fd
USE sagerr_fd
USE sagerr_fi
USE sagstr_fd
USE PtrGrdStrItf
USE grdlock_fi

!------ remove lowest grid level cells

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI                 !Grid ID
INTEGER, INTENT( IN ) :: nc                   !Flag >0 for cleanup
         EXTERNAL     :: UserRoutine         !Cell dezone function

TYPE( SAGgrid_str ), POINTER  :: grd

INTEGER  m0, n0, ncell, nvart, mn0, i, j, ip, ic
INTEGER  ix, iy, icell0, ipo, iout, nstart, ios
INTEGER  mlev

INTEGER, ALLOCATABLE, DIMENSION(:) :: lgrdp
INTEGER, POINTER,     DIMENSION(:) :: igrd
REAL,    POINTER,     DIMENSION(:) :: dat

INTEGER, EXTERNAL :: SAG_FindMaxLevID

INTERFACE
  RECURSIVE SUBROUTINE SAG_DezoneCell( grd,lgrdp,icell0,mlev,UserRoutine )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER, DIMENSION(*), INTENT( INOUT ) :: lgrdp
    INTEGER,               INTENT( IN    ) :: icell0
    INTEGER,               INTENT( IN    ) :: mlev
                           EXTERNAL        :: UserRoutine
  END SUBROUTINE SAG_DezoneCell
END INTERFACE

SAG_Dezone = SAG_ERROR

!------ Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!------ Cleanup grid from aborted dezone

IF( nc > 0 )THEN

  DO i = 1,grd%ncells
    IF( grd%ipgrd(i) > nc )grd%ipgrd(i) = 0
  END DO

  grd%ncells = nc

END IF

!------ Set local variables

m0    = grd%nx
n0    = grd%ny
ncell = grd%ncells
nvart = grd%nvart
IF( grd%maxlev < 0 )THEN
!$OMP ATOMIC WRITE
  grd%maxlev = SAG_FindMaxLevID( grdI )
END IF

ALLOCATE( lgrdp(ncell),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = grdI
  GOTO 9999
END IF

igrd => grd%ipgrd
dat  => grd%ipdat

!------ Set backward pointers

mn0 = m0*n0 - 3

DO i = 1,ncell
  lgrdp(i) = 0
END DO

DO i = 1,ncell
  IF( igrd(i) > 0 )THEN
    ip = (igrd(i)-mn0)/4
    lgrdp(ip) = i
  END IF
END DO

!------ Start on level-0 grid

DO ix = 1,m0
  DO iy = 1,n0

!------ Locate position on level-0

    mlev   = 0
    icell0 = (iy-1)*m0 + ix

!------ Check for refinement

    IF( igrd(icell0) /= 0 )THEN
      CALL SAG_DezoneCell( grd,lgrdp,icell0,mlev,UserRoutine )
    END IF

  END DO
END DO

!------ Compress grid

nstart = m0*n0 + 1
iout   = nstart

DO i = nstart,ncell,4

  ip = (i - mn0)/4

  IF( lgrdp(ip) /= 0 )THEN

!$OMP ATOMIC WRITE
    igrd(lgrdp(ip)) = iout

    DO j = 0,3

      IF( iout /= i+j )THEN

!---- Move grid pointers

!$OMP ATOMIC WRITE
        igrd(iout) = igrd(i+j)

!---- Move grid data

        DO ic = 1,grd%nvart
!$OMP ATOMIC WRITE
          dat(iout + grd%mxgrd*(ic-1)) = dat(i+j + grd%mxgrd*(ic-1))
        END DO

!---- Move auxiliary data

        DO ic = 1,grd%naux
          IF( grd%aux(ic)%alloc )THEN
            IF( ASSOCIATED(grd%aux(ic)%srf_data(iout)%data) )THEN
              DEALLOCATE( grd%aux(ic)%srf_data(iout)%data,STAT=ios )
              NULLIFY( grd%aux(ic)%srf_data(iout)%data )
            END IF
            IF( ASSOCIATED(grd%aux(ic)%srf_data(i+j)%data) )THEN
              grd%aux(ic)%srf_data(iout)%data => grd%aux(ic)%srf_data(i+j)%data
              NULLIFY( grd%aux(ic)%srf_data(i+j)%data )
            END IF
          END IF
        END DO

      END IF

!---- Update pointers

      IF( igrd(iout) /= 0 )THEN
        ipo = (igrd(iout)-mn0)/4
        lgrdp(ipo) = iout
      END IF

      iout = iout + 1

    END DO

  END IF

END DO

!---- Clear dezoned auxiliary space

IF( grd%naux > 0 )THEN
  DO i = iout,ncell
    DO j = 1,grd%naux
      IF( grd%aux(j)%alloc ) THEN
        DEALLOCATE( grd%aux(j)%srf_data(i)%data,STAT=ios )
        NULLIFY( grd%aux(j)%srf_data(i)%data )
      END IF
    END DO
  END DO
END IF

!$OMP ATOMIC WRITE
grd%ncells = iout - 1

!$OMP ATOMIC UPDATE
grd%maxlev = grd%maxlev - 1

DEALLOCATE( lgrdp )

SAG_Dezone = SAG_OK

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SAG_DezoneCell( grd,lgrdp,icell0,mlev,UserRoutine )

USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER, DIMENSION(*), INTENT( INOUT ) :: lgrdp
INTEGER,               INTENT( IN    ) :: icell0
INTEGER,               INTENT( IN    ) :: mlev

INTERFACE
  SUBROUTINE UserRoutine(dat,mx,icell0,i0)
    INTEGER, INTENT( IN )          :: mx
    INTEGER, INTENT( IN )          :: i0
    INTEGER, INTENT( IN )          :: icell0
    REAL,    POINTER, DIMENSION(:) :: dat
  END SUBROUTINE UserRoutine
END INTERFACE

INTEGER i, ip, mlevn, i0, icell

INTEGER, POINTER, DIMENSION(:) :: igrd

!------ Initialize

igrd  => grd%ipgrd
i0    =  igrd(icell0)
mlevn =  mlev + 1

!------- Move down levels until maxlev or no more refinement

IF( mlevn < grd%maxlev )THEN

!------- Check the four sub-cells

  DO i = 0,3
    icell = i0 + i
    IF( igrd(icell) /= 0 )THEN
      CALL SAG_DezoneCell( grd,lgrdp,icell,mlevn,UserRoutine )
    END IF
  END DO

ELSE !at maxlev

!------ Set the grid pointers

  ip = (i0 - grd%nx*grd%ny + 3)/4
  igrd(icell0) = 0
  lgrdp(ip)    = 0

!------ Dezone the 4 subcells (at i0) into the parent cell (at icell0)

  CALL UserRoutine( grd%ipdat,grd%mxgrd,icell0,i0 )

END IF

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SAG_DezoneAux8( grdI,nc,n8,dat8,UserRoutine )

USE sagdef_fd
USE sagerr_fd
USE sagerr_fi
USE sagstr_fd
USE PtrGrdStrItf
USE grdlock_fi

!------ remove lowest grid level cells

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: grdI                 !Grid ID
INTEGER,               INTENT( IN    ) :: nc                   !Flag >0 for cleanup
INTEGER,               INTENT( IN    ) :: n8                   !No. of REAL(8) fields
REAL(8), DIMENSION(*), INTENT( INOUT ) :: dat8                 !REAL(8) data
         EXTERNAL                     :: UserRoutine          !Cell dezone function

TYPE( SAGgrid_str ), POINTER  :: grd

INTEGER  m0, n0, ncell, nvart, mn0, i, j, ip, ic
INTEGER  ix, iy, icell0, ipo, iout, nstart, ios
INTEGER  mlev

INTEGER, ALLOCATABLE, DIMENSION(:) :: lgrdp
INTEGER, POINTER,     DIMENSION(:) :: igrd
REAL,    POINTER,     DIMENSION(:) :: dat

INTEGER, EXTERNAL :: SAG_FindMaxLevID

INTERFACE
  RECURSIVE SUBROUTINE SAG_DezoneCellAux8( grd,lgrdp,icell0,mlev,n8,dat8,UserRoutine )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER, DIMENSION(*), INTENT( INOUT ) :: lgrdp
    INTEGER,               INTENT( IN    ) :: icell0
    INTEGER,               INTENT( IN    ) :: n8
    REAL(8), DIMENSION(*), INTENT( INOUT ) :: dat8
    INTEGER,               INTENT( IN    ) :: mlev
                           EXTERNAL        :: UserRoutine
  END SUBROUTINE SAG_DezoneCellAux8
END INTERFACE

SAG_DezoneAux8 = SAG_ERROR

!------ Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!------ Cleanup grid from aborted dezone

IF( nc > 0 )THEN

  DO i = 1,grd%ncells
    IF( grd%ipgrd(i) > nc )grd%ipgrd(i) = 0
  END DO

  grd%ncells = nc

END IF

!------ Set local variables

m0    = grd%nx
n0    = grd%ny
ncell = grd%ncells
nvart = grd%nvart
IF( grd%maxlev < 0 )THEN
!$OMP ATOMIC WRITE
  grd%maxlev = SAG_FindMaxLevID( grdI )
END IF

ALLOCATE( lgrdp(ncell),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = grdI
  GOTO 9999
END IF

igrd => grd%ipgrd
dat  => grd%ipdat

!------ Set backward pointers

mn0 = m0*n0 - 3

DO i = 1,ncell
  lgrdp(i) = 0
END DO

DO i = 1,ncell
  IF( igrd(i) > 0 )THEN
    ip = (igrd(i)-mn0)/4
    lgrdp(ip) = i
  END IF
END DO

!------ Start on level-0 grid

DO ix = 1,m0
  DO iy = 1,n0

!------ Locate position on level-0

    mlev   = 0
    icell0 = (iy-1)*m0 + ix

!------ Check for refinement

    IF( igrd(icell0) /= 0 )THEN
      CALL SAG_DezoneCellAux8( grd,lgrdp,icell0,mlev,n8,dat8,UserRoutine )
    END IF

  END DO
END DO

!------ Compress grid

nstart = m0*n0 + 1
iout   = nstart

DO i = nstart,ncell,4

  ip = (i - mn0)/4

  IF( lgrdp(ip) /= 0 )THEN

!$OMP ATOMIC WRITE
    igrd(lgrdp(ip)) = iout

    DO j = 0,3

      IF( iout /= i+j )THEN

!---- Move grid pointers

!$OMP ATOMIC WRITE
        igrd(iout) = igrd(i+j)

!---- Move grid data

        DO ic = 1,grd%nvart
!$OMP ATOMIC WRITE
          dat(iout + grd%mxgrd*(ic-1)) = dat(i+j + grd%mxgrd*(ic-1))
        END DO

        DO ic = 1,n8
!$OMP ATOMIC WRITE
          dat8(iout + grd%mxgrd*(ic-1)) = dat8(i+j + grd%mxgrd*(ic-1))
        END DO

!---- Move auxiliary data

        DO ic = 1,grd%naux
          IF( grd%aux(ic)%alloc )THEN
            IF( ASSOCIATED(grd%aux(ic)%srf_data(iout)%data) )THEN
              DEALLOCATE( grd%aux(ic)%srf_data(iout)%data,STAT=ios )
              NULLIFY( grd%aux(ic)%srf_data(iout)%data )
            END IF
            IF( ASSOCIATED(grd%aux(ic)%srf_data(i+j)%data) )THEN
              grd%aux(ic)%srf_data(iout)%data => grd%aux(ic)%srf_data(i+j)%data
              NULLIFY( grd%aux(ic)%srf_data(i+j)%data )
            END IF
          END IF
        END DO

      END IF

!---- Update pointers

      IF( igrd(iout) /= 0 )THEN
        ipo = (igrd(iout)-mn0)/4
        lgrdp(ipo) = iout
      END IF

      iout = iout + 1

    END DO

  END IF

END DO

!---- Clear dezoned auxiliary space

IF( grd%naux > 0 )THEN
  DO i = iout,ncell
    DO j = 1,grd%naux
      IF( grd%aux(j)%alloc ) THEN
        DEALLOCATE( grd%aux(j)%srf_data(i)%data,STAT=ios )
        NULLIFY( grd%aux(j)%srf_data(i)%data )
      END IF
    END DO
  END DO
END IF

!$OMP ATOMIC WRITE
grd%ncells = iout - 1

!$OMP ATOMIC UPDATE
grd%maxlev = grd%maxlev - 1

DEALLOCATE( lgrdp )

SAG_DezoneAux8 = SAG_OK

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SAG_DezoneCellAux8( grd,lgrdp,icell0,mlev,n8,dat8,UserRoutine )

USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER, DIMENSION(*), INTENT( INOUT ) :: lgrdp
INTEGER,               INTENT( IN    ) :: icell0
INTEGER,               INTENT( IN    ) :: mlev
INTEGER,               INTENT( IN    ) :: n8
REAL(8), DIMENSION(*), INTENT( INOUT ) :: dat8

INTERFACE
  SUBROUTINE UserRoutine(dat,mx,icell0,i0,n8,dat8)
    INTEGER,               INTENT( IN    ) :: mx
    INTEGER,               INTENT( IN    ) :: i0
    INTEGER,               INTENT( IN    ) :: icell0
    INTEGER,               INTENT( IN    ) :: n8
    REAL(8), DIMENSION(*), INTENT( INOUT ) :: dat8
    REAL,    POINTER, DIMENSION(:)         :: dat
  END SUBROUTINE UserRoutine
END INTERFACE

INTEGER i, ip, mlevn, i0, icell

INTEGER, POINTER, DIMENSION(:) :: igrd

!------ Initialize

igrd  => grd%ipgrd
i0    =  igrd(icell0)
mlevn =  mlev + 1

!------- Move down levels until maxlev or no more refinement

IF( mlevn < grd%maxlev )THEN

!------- Check the four sub-cells

  DO i = 0,3
    icell = i0 + i
    IF( igrd(icell) /= 0 )THEN
      CALL SAG_DezoneCellAux8( grd,lgrdp,icell,mlevn,n8,dat8,UserRoutine )
    END IF
  END DO

ELSE !at maxlev

!------ Set the grid pointers

  ip = (i0 - grd%nx*grd%ny + 3)/4
  igrd(icell0) = 0
  lgrdp(ip)    = 0

!------ Dezone the 4 subcells (at i0) into the parent cell (at icell0)

  CALL UserRoutine( grd%ipdat,grd%mxgrd,icell0,i0,n8,dat8 )

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION SAG_FindMaxLevID( grdI )

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ix, iy, icell0, mlevi, nrfm, mxlev

INTERFACE
  SUBROUTINE SAG_FindBotLev( grd,nrfm,icell0,mlev0 )
    USE sagstr_fd
    TYPE ( SAGgrid_str ), POINTER         :: grd
    INTEGER,              INTENT( IN    ) :: nrfm, icell0
    INTEGER,              INTENT( INOUT ) :: mlev0
  END SUBROUTINE SAG_FindBotLev
END INTERFACE

SAG_FindMaxLevID = -999

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE DEFAULT
    nrfm = 1
END SELECT
mxlev = 0

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell0 = (iy-1)*grd%nx + ix
    mlevi  = 0
    CALL SAG_FindBotLev( grd,nrfm,icell0,mlevi )
    mxlev  = MAX(mxlev,mlevi)

  END DO
END DO

SAG_FindMaxLevID = mxlev

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SAG_FindBotLev( grd,nrfm,icell0,mlev0 )

USE sagstr_fd

IMPLICIT NONE

TYPE ( SAGgrid_str ), POINTER         :: grd
INTEGER,              INTENT( IN    ) :: nrfm, icell0
INTEGER,              INTENT( INOUT ) :: mlev0

INTEGER, POINTER, DIMENSION(:) :: igrd

INTEGER mlevx, mlev, i, icell

INTERFACE
  RECURSIVE SUBROUTINE SAG_limget( irg,nrfm,icell0,mlev0 )
    INTEGER, DIMENSION(:), POINTER         :: irg
    INTEGER,               INTENT( IN    ) :: nrfm
    INTEGER,               INTENT( IN    ) :: icell0
    INTEGER,               INTENT( INOUT ) :: mlev0
  END SUBROUTINE SAG_limget
END INTERFACE

igrd => grd%ipgrd

IF( igrd(icell0) /= 0 )THEN
  mlevx = mlev0 + 1
  DO i = 0,nrfm
    icell = igrd(icell0) + i
    mlev  = mlevx
    CALL SAG_limget( igrd,nrfm,icell,mlev )
    mlev0 = MAX(mlev,mlev0)
  END DO
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SAG_limget( irg,nrfm,icell0,mlev0 )

IMPLICIT NONE

INTEGER, DIMENSION(:), POINTER         :: irg
INTEGER,               INTENT( IN    ) :: nrfm
INTEGER,               INTENT( IN    ) :: icell0
INTEGER,               INTENT( INOUT ) :: mlev0

INTEGER mlevx, mlev, i, icell

IF( irg(icell0) /= 0 )THEN
  mlevx = mlev0 + 1
  DO i = 0,nrfm
    icell = irg(icell0) + i
    mlev  = mlevx
    CALL SAG_limget( irg,nrfm,icell,mlev )
    mlev0 = MAX(mlev,mlev0)
  END DO
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION SAG_Rezone( grdI, UserRoutine )

USE sagdef_fd
USE sagerr_fd
USE sagerr_fi
USE sagstr_fd
USE PtrGrdStrItf

!------ Average to lowest grid level cells

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI                 !Grid ID
         EXTERNAL     :: UserRoutine          !Cell dezone function

TYPE( SAGgrid_str ), POINTER  :: grd

INTEGER  m0, n0
INTEGER  ix, iy, icell0, ilev

INTERFACE
  RECURSIVE SUBROUTINE SAG_RezoneCell( grd,icell0,UserRoutine,ilev0 )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER         :: grd
    INTEGER,               INTENT( IN    ) :: icell0
    INTEGER,               INTENT( IN    ) :: ilev0
                           EXTERNAL        :: UserRoutine
  END SUBROUTINE SAG_RezoneCell
END INTERFACE

SAG_Rezone = SAG_ERROR

!------ Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!------ Set local variables

m0    = grd%nx
n0    = grd%ny

!------ Start on level-0 grid

ilev = 0

DO ix = 1,m0
  DO iy = 1,n0

!------ Locate position on level-0

    icell0 = (iy-1)*m0 + ix

!------ Check for refinement

    CALL SAG_RezoneCell( grd,icell0,UserRoutine,ilev )
  END DO
END DO

SAG_Rezone = SAG_OK


9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SAG_RezoneCell( grd,icell0,UserRoutine,ilev0 )

USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER         :: grd
INTEGER,               INTENT( IN    ) :: icell0
INTEGER,               INTENT( IN    ) :: ilev0

INTERFACE
  SUBROUTINE UserRoutine(dat,mx,icell0,i0)
    INTEGER, INTENT( IN )          :: mx
    INTEGER, INTENT( IN )          :: i0
    INTEGER, INTENT( IN )          :: icell0
    REAL,    POINTER, DIMENSION(:) :: dat
  END SUBROUTINE UserRoutine
END INTERFACE

INTEGER i, i0, icell, ilev

!------ Initialize

i0 = grd%ipgrd(icell0)

!------- Move down levels until maxlev or no more refinement

IF( i0 > 0 )THEN

!------- Check the four sub-cells

  ilev = ilev0 + 1

  DO i = 0,3
    icell = i0 + i
    CALL SAG_RezoneCell( grd,icell,UserRoutine,ilev )
  END DO

!------ Rezone the 4 subcells (at i0) into the parent cell (at icell0)


  CALL UserRoutine( grd%ipdat,grd%mxgrd,icell0,i0 )

!------ scale up the parent cell values (at icell0)

  grd%ipdat(icell0)           = grd%ipdat(icell0)*2.
  DO i = 2,grd%nvart
    grd%ipdat(icell0+(i-1)*grd%mxgrd) = grd%ipdat(icell0+(i-1)*grd%mxgrd)*4.
  END DO
END IF

RETURN
END
