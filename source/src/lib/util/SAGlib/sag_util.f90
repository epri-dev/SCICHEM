!*******************************************************************************
!               SAG_CountCells
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_CountCells( grd,icell0,count ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER         :: grd
INTEGER,             INTENT( IN    ) :: icell0
INTEGER,             INTENT( INOUT ) :: count

INTEGER i,icell,nrfm

INTEGER, POINTER, DIMENSION(:) :: igrd

irv = SAG_ERROR

igrd => grd%ipgrd

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE( SAG_GRID_HORZ,SAG_GRID_VERT )
    nrfm = 1
  CASE( SAG_GRID_NONE )
    nrfm = -1
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( igrd(icell0) /= 0 )THEN
  DO i = 0,nrfm
    icell = igrd(icell0) + i
    irv = SAG_CountCells( grd,icell,count )
    IF( irv /= SAG_OK )GOTO 9999
  END DO
ELSE
  count = count + 1
END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CountLevels
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_CountLevels( grd,icell0,count0,ret )

USE sagdef_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER         :: grd
INTEGER,             INTENT( IN    ) :: icell0
INTEGER,             INTENT( INOUT ) :: count0
INTEGER,             INTENT( OUT   ) :: ret

INTEGER i,icell,nrfm,count,countx

INTEGER, DIMENSION(:), POINTER :: igrd

ret = SAG_ERROR

igrd => grd%ipgrd

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE( SAG_GRID_HORZ,SAG_GRID_VERT )
    nrfm = 1
  CASE( SAG_GRID_NONE )
    nrfm = -1
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( igrd(icell0) /= 0 )THEN
  countx = count0 + 1
  DO i = 0,nrfm
    icell = igrd(icell0) + i
    count = countx
    CALL SAG_CountLevels( grd,icell,count,ret )
    IF( ret /= SAG_OK )GOTO 9999
    count0 = MAX(count,count0)
  END DO
END IF

ret = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_GetValue
!*******************************************************************************
REAL FUNCTION SAG_GetValue( dat,id,x )

USE saggrd_fi

IMPLICIT NONE

REAL, DIMENSION(:), POINTER      :: dat
INTEGER,            INTENT( IN ) :: id
REAL,               INTENT( IN ) :: x

IF( id == 0 )THEN
  SAG_GetValue = x
ELSE IF( UseSpecial )THEN
  IF( dat(id) == Special )THEN
    SAG_GetValue = x
  ELSE
    SAG_GetValue = dat(id)
  END IF
ELSE
  SAG_GetValue = dat(id)
END IF

RETURN
END
!*******************************************************************************
!               SAG_PutValue
!*******************************************************************************
SUBROUTINE SAG_PutValue( dat,id,x )

IMPLICIT NONE

REAL, DIMENSION(:), POINTER      :: dat
INTEGER,            INTENT( IN ) :: id
REAL,               INTENT( IN ) :: x

dat(id) = x

RETURN
END
!*******************************************************************************
!               SAG_InitCell
!*******************************************************************************
SUBROUTINE SAG_InitCell( q )

USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ),   INTENT( OUT ) :: q

q%id  = 0
q%lev = 0
q%x   = 0.
q%y   = 0.
q%hx  = 0.
q%hy  = 0.
q%d   = 0.
q%drt = 0.
q%drb = 0.
q%dlt = 0.
q%dlb = 0.

RETURN
END
!*******************************************************************************
!               SAG_GetID
!*******************************************************************************
SUBROUTINE SAG_GetID( igrd,p,ipt,q )

USE sagcel_fd

IMPLICIT NONE

INTEGER, DIMENSION(:), POINTER         :: igrd
TYPE( SAGcell_str ),   INTENT( IN    ) :: p
INTEGER,               INTENT( IN    ) :: ipt
TYPE( SAGcell_str ),   INTENT( INOUT ) :: q

IF( p%id == 0 )THEN
  q%id = 0
ELSE
  IF( igrd(p%id) == 0 )THEN
    q%id = 0
  ELSE
    q%id = igrd(p%id) + ipt
  END IF
END IF

RETURN
END
!*******************************************************************************
!               SAG_UpperClear
!*******************************************************************************
INTEGER FUNCTION SAG_UpperClearID( grdI,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Zero upper (not bottom) cells

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
INTEGER, INTENT( IN ) :: ifld

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_UpperClear( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_UpperClear
END INTERFACE

SAG_UpperClearID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_UpperClearID = SAG_UpperClear( grd,ifld )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_UpperClear
!*******************************************************************************
INTEGER FUNCTION SAG_UpperClear( grd,ifld )

USE sagdef_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld

INTEGER ix,iy,icell0,ir

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_ClearUpperCells( grd,ifld,icell0 )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
    INTEGER,             INTENT( IN ) :: icell0
  END FUNCTION SAG_ClearUpperCells
END INTERFACE

SAG_UpperClear = SAG_ERROR

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell0 = (iy-1)*grd%nx + ix
    ir = SAG_ClearUpperCells( grd,ifld,icell0 )
    IF( ir /= SAG_OK )GOTO 9999

  END DO
END DO

SAG_UpperClear = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_ClearUpperCells
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_ClearUpperCells( grd,ifld,icell0 ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER,             INTENT( IN ) :: ifld
INTEGER,             INTENT( IN ) :: icell0

INTEGER i,icell,nrfm

INTEGER, POINTER, DIMENSION(:) :: igrd
REAL,    POINTER, DIMENSION(:) :: dat

irv = SAG_ERROR

igrd => grd%ipgrd
dat  => grd%ipdat((ifld-1)*grd%mxgrd+1:)

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE( SAG_GRID_HORZ,SAG_GRID_VERT )
    nrfm = 1
  CASE( SAG_GRID_NONE )
    nrfm = -1
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( igrd(icell0) /= 0 )THEN
  dat(icell0) = 0.0
  DO i = 0,nrfm
    icell = igrd(icell0) + i
    irv = SAG_ClearUpperCells( grd,ifld,icell )
    IF( irv /= SAG_OK )GOTO 9999
  END DO
END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_SetSpecialValue
!*******************************************************************************
INTEGER FUNCTION SAG_SetSpecialValue( flag,value )

USE sagdef_fd
USE saggrd_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: flag
REAL,    INTENT( IN ) :: value

IF( flag )THEN
  Special    = value
  UseSpecial = .TRUE.
ELSE
  UseSpecial = .FALSE.
END IF

SAG_SetSpecialValue = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_GetSpecialValue
!*******************************************************************************
INTEGER FUNCTION SAG_GetSpecialValue( flag,value )

USE sagdef_fd
USE saggrd_fi

IMPLICIT NONE

LOGICAL, INTENT( OUT ) :: flag
REAL,    INTENT( OUT ) :: value

IF( UseSpecial )THEN
  value = Special
  flag  = .TRUE.
ELSE
  value = 0.0
  flag  = .FALSE.
END IF

SAG_GetSpecialValue = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_SetDefaultValue
!*******************************************************************************
INTEGER FUNCTION SAG_SetDefaultValue( value )

USE sagdef_fd
USE saggrd_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: value

Defval = value

SAG_SetDefaultValue = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_GetDefaultValue
!*******************************************************************************
INTEGER FUNCTION SAG_GetDefaultValue( value )

USE sagdef_fd
USE saggrd_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: value

value = Defval

SAG_GetDefaultValue = SAG_OK

RETURN
END
!*******************************************************************************
!                SAG_CopyGridID
!*******************************************************************************
INTEGER FUNCTION SAG_CopyGridID( grd1I,grd2I,DataFlag )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

INTEGER, INTENT( IN ) :: grd1I
INTEGER, INTENT( IN ) :: grd2I
INTEGER, INTENT( IN ) :: DataFlag

TYPE ( SAGgrid_str ), POINTER  :: grd1
TYPE ( SAGgrid_str ), POINTER  :: grd2

INTERFACE
  INTEGER FUNCTION SAG_CopyGrid( grd1,grd2,DataFlag )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd1
    TYPE( SAGgrid_str ), POINTER      :: grd2
    INTEGER,             INTENT( IN ) :: DataFlag
  END FUNCTION SAG_CopyGrid
END INTERFACE

SAG_CopyGridID = SAG_ERROR

grd1 => SAG_PtrGrdStr( grd1I )
IF( .NOT.ASSOCIATED( grd1 ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grd1I
  GOTO 9999
END IF

grd2 => SAG_PtrGrdStr( grd2I )
IF( .NOT.ASSOCIATED( grd2 ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grd2I
  GOTO 9999
END IF

SAG_CopyGridID = SAG_CopyGrid( grd1,grd2,DataFlag )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CopyGrid
!*******************************************************************************
INTEGER FUNCTION SAG_CopyGrid( grd1,grd2,DataFlag )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     copy a SAG grid structure

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd1
TYPE( SAGgrid_str ), POINTER      :: grd2
INTEGER,             INTENT( IN ) :: DataFlag

INTEGER i

INTEGER,              DIMENSION(:), POINTER :: grid1
REAL,                 DIMENSION(:), POINTER :: data1
INTEGER,              DIMENSION(:), POINTER :: flag1
CHARACTER(4),         DIMENSION(:), POINTER :: name1
TYPE( SAGblock_str ), DIMENSION(:), POINTER :: block1

INTEGER,              DIMENSION(:), POINTER :: grid2
REAL,                 DIMENSION(:), POINTER :: data2
INTEGER,              DIMENSION(:), POINTER :: flag2
CHARACTER(4),         DIMENSION(:), POINTER :: name2
TYPE( SAGblock_str ), DIMENSION(:), POINTER :: block2
TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: aux2

INTEGER k,j,i1,i2,mxgrd,mxfld,mxnam,ios,ftype,nblk,naux
INTEGER Flag,ifld,jfld,nunit,rec,status,ID
LOGICAL do_header,do_grid,do_data,do_init

CHARACTER(PATH_MAXLENGTH) filename

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
END INTERFACE

SAG_CopyGrid = SAG_ERROR

!==== Initialize

do_init   = DataFlag > 0
do_header = ABS(DataFlag) < SAG_COPY_FIELD
IF( do_header )THEN
  Flag = ABS(DataFlag)
  do_grid = Flag /= SAG_COPY_NULL
  do_data = do_grid .AND. Flag /= SAG_COPY_GRID
  ifld    = -1
ELSE
  Flag    = ABS(DataFlag) - SAG_COPY_FIELD
  do_grid = Flag == SAG_COPY_NULL
  do_data = Flag > SAG_COPY_NULL
  ifld    = Flag/SAG_TO_FIELD
  jfld    = Flag - SAG_TO_FIELD*ifld
END IF

IF( do_header )THEN

!====== Initialize receiving grid if called for

  IF( Flag == SAG_COPY_FULL .AND. do_init )THEN
    i = SAG_FreeGrid( grd2 )
    IF( i /= SAG_OK )GOTO 9999
    i = SAG_InitGrid( grd1%file,grd1%nunit,grd1%type,grd1%mxgrd, &
                      grd1%mxfld,0,grd2 )
    IF( i /= SAG_OK )GOTO 9999
  END IF

!==== Save receiving pointers and file data

  grid2  => grd2%ipgrd
  data2  => grd2%ipdat
  flag2  => grd2%ipflg
  name2  => grd2%ipnam
  block2 => grd2%ipblk
  aux2   => grd2%aux

  mxgrd = grd2%mxgrd
  mxfld = grd2%mxfld
  mxnam = grd2%mxnam

  ftype = grd2%ftype
  nblk  = grd2%nblk
  naux  = grd2%naux

  nunit    = grd2%nunit
  filename = grd2%file
  rec      = grd2%record
  status   = grd2%status

!==== Copy grid structure

  ID = grd2%ID

  grd2 = grd1

  grd2%ID = ID

!==== Set receiving pointers

  grd2%ipgrd => grid2
  grd2%ipdat => data2
  grd2%ipflg => flag2
  grd2%ipnam => name2
  grd2%ipblk => block2
  grd2%aux   => aux2
  grd2%mxgrd = mxgrd
  grd2%mxfld = mxfld
  grd2%mxnam = mxnam

  grd2%ftype = ftype
  grd2%nblk  = nblk
  grd2%naux  = naux

  grd2%nunit  = nunit
  grd2%file   = filename
  grd2%record = rec
  grd2%status = status

!==== Copy blocks if necessary

  IF( Flag == SAG_COPY_FULL )THEN
    IF( grd1%nblk > 0 )THEN

      IF( .NOT.ASSOCIATED(grd1%ipblk) )THEN
        LastError = SAG_ERR_COPY
        LastParm = 5
        GOTO 9999
      END IF

      block1 => grd1%ipblk

      IF( ASSOCIATED(grd2%ipblk) )THEN
        DO i = 1,grd2%nblk
          IF( ASSOCIATED(grd2%ipblk(i)%fldnam) )DEALLOCATE( grd2%ipblk(i)%fldnam,STAT=ios )
        END DO
        DEALLOCATE( grd2%ipblk,STAT=ios )
      END IF

      ALLOCATE( block2(grd1%nblk),STAT=ios )
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = grd1%ftype
        GOTO 9999
      END IF

      grd2%ipblk => block2
      grd2%nblk  =  grd1%nblk
      grd2%ftype =  grd1%ftype

      DO i = 1,grd2%nblk
        block2(i) = block1(i)
        IF( ASSOCIATED(grd2%ipblk(i)%fldnam) )NULLIFY( grd2%ipblk(i)%fldnam )
        IF( ASSOCIATED(grd1%ipblk(i)%fldnam) )THEN
          j = SIZE( grd1%ipblk(i)%fldnam )
          ALLOCATE( grd2%ipblk(i)%fldnam(j),STAT=ios )
          IF( ios /= 0 )THEN
            LastError = SAG_ERR_MALLOC
            LastParm  = j
            GOTO 9999
          END IF
          grd2%ipblk(i)%fldnam = grd1%ipblk(i)%fldnam
        END IF
      END DO

    END IF
  END IF

!==== Copy file names if requested

  IF( Flag == SAG_COPY_FULL )THEN

    name1 => grd1%ipnam

    IF( ASSOCIATED(grd1%ipnam) )THEN
      IF( ASSOCIATED(grd2%ipnam) )THEN
        IF( grd1%nvart > grd2%mxnam )THEN
          LastError = SAG_ERR_COPY
          LastParm = 4
          GOTO 9999
        END IF
      ELSE
        ALLOCATE( name2(grd2%nvart),STAT=ios )
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_MALLOC
          LastParm  = grd2%nvart
          GOTO 9999
        END IF
        grd2%mxnam = grd2%nvart
        grd2%ipnam => name2
      END IF

      DO k = 1,grd1%nvart
        name2(k) = name1(k)
      END DO

    END IF

  END IF
END IF

!==== Copy grid/data if requested

IF( do_grid )THEN

!====== Check for valid copy and copy grid

  IF( .NOT.ASSOCIATED(grd1%ipgrd) )THEN
    LastError = SAG_ERR_COPY
    LastParm  = 1
    GOTO 9999
  END IF

  IF( .NOT.ASSOCIATED(grd2%ipgrd) )THEN
    LastError = SAG_ERR_COPY
    LastParm  = -1
    GOTO 9999
  END IF

  IF( grd1%ncells > grd2%mxgrd )THEN
    LastError = SAG_ERR_COPY
    LastParm = 3
    GOTO 9999
  END IF

  grid1 => grd1%ipgrd
  grid2 => grd2%ipgrd

  DO k = 1,grd1%ncells
    grid2(k) = grid1(k)
  END DO

  grd2%ncells = grd1%ncells

END IF

!====== Check for valid copy and copy data if requested

IF( do_data )THEN

  IF( .NOT.ASSOCIATED(grd1%ipdat) )THEN
    LastError = SAG_ERR_COPY
    LastParm  = 2
    GOTO 9999
  END IF

  IF( .NOT.ASSOCIATED(grd2%ipdat) )THEN
    LastError = SAG_ERR_COPY
    LastParm  = -2
    GOTO 9999
  END IF

  data1 => grd1%ipdat
  flag1 => grd1%ipflg
  data2 => grd2%ipdat
  flag2 => grd2%ipflg

  IF( ifld <= 0 )THEN
    IF( grd1%mxfld > grd2%mxfld )THEN
      LastError = SAG_ERR_COPY
      LastParm = 4
      GOTO 9999
    END IF

    DO j = 1,grd1%mxfld
      i1 = (j-1)*grd1%mxgrd
      i2 = (j-1)*grd2%mxgrd
      DO k = 1,grd1%ncells
        data2(i2+k) = data1(i1+k)
      END DO
    END DO

    DO k = 1,grd1%mxfld
      flag2(k) = flag1(k)
    END DO
  ELSE

    IF( ifld <= 0 .OR. ifld > grd2%mxfld )THEN
      LastError = SAG_ERR_IFLD
      LastParm  = ifld
      GOTO 9999
    END IF

    IF( jfld <= 0 .OR. jfld > grd1%mxfld )THEN
      LastError = SAG_ERR_IFLD
      LastParm  = jfld
      GOTO 9999
    END IF

    IF( grd1%ncells /= grd2%ncells )THEN
      LastError = SAG_ERR_IFLD
      LastParm  = grd2%ncells
      GOTO 9999
    END IF

    i1 = (jfld-1)*grd1%mxgrd
    i2 = (ifld-1)*grd2%mxgrd
    DO k = 1,grd1%ncells
      data2(i2+k) = data1(i1+k)
    END DO

    flag2(ifld) = flag1(jfld)

  END IF

END IF

SAG_CopyGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CopyAuxID
!*******************************************************************************
INTEGER FUNCTION SAG_CopyAuxID( grd1I,i1,grd2I,i2 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

INTEGER, INTENT( IN ) :: grd1I
INTEGER, INTENT( IN ) :: i1
INTEGER, INTENT( IN ) :: grd2I
INTEGER, INTENT( IN ) :: i2

TYPE ( SAGgrid_str ), POINTER  :: grd1
TYPE ( SAGgrid_str ), POINTER  :: grd2

INTERFACE
  INTEGER FUNCTION SAG_CopyAux( grd1,i1,grd2,i2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd1
    INTEGER,             INTENT( IN ) :: i1
    TYPE( SAGgrid_str ), POINTER      :: grd2
    INTEGER,             INTENT( IN ) :: i2
  END FUNCTION SAG_CopyAux
END INTERFACE

SAG_CopyAuxID = SAG_ERROR

grd1 => SAG_PtrGrdStr( grd1I )
IF( .NOT.ASSOCIATED(grd1) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grd1I
  GOTO 9999
END IF

grd2 => SAG_PtrGrdStr( grd2I )
IF( .NOT.ASSOCIATED(grd2) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grd2I
  GOTO 9999
END IF

SAG_CopyAuxID = SAG_CopyAux( grd1,i1,grd2,i2 )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CopyAux
!*******************************************************************************
INTEGER FUNCTION SAG_CopyAux( grd1,i1,grd2,i2 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     copy a SAG grid structure

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd1
INTEGER,             INTENT( IN ) :: i1
TYPE( SAGgrid_str ), POINTER      :: grd2
INTEGER,             INTENT( IN ) :: i2

INTEGER i, j, ios

TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: aux

SAG_CopyAux = SAG_ERROR

!---- Allocate receiving aux space if necessary

IF( grd2%naux > 0 )THEN
  IF( .NOT.ASSOCIATED(grd2%aux) )THEN
    ALLOCATE( aux(grd2%naux),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd2%naux
      GOTO 9999
    END IF
    grd2%aux => aux
    DO i = 1,grd2%naux
      grd2%aux(i)%alloc = .FALSE.
      NULLIFY( grd2%aux(i)%srf_data )
    END DO
  END IF
END IF

!---- Check for input errors

IF( i1 > grd1%naux .OR. i1 < 1 )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = i1
  GOTO 9999
END IF
IF( i2 > grd2%naux .OR. i2 < 1 )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = i2
  GOTO 9999
END IF

!----- Clear receiving aux space if necessary

IF( grd2%aux(i2)%alloc )THEN
  DO i = 1,grd2%mxgrd
    IF( ASSOCIATED(grd2%aux(i2)%srf_data(i)%data) )THEN
      DEALLOCATE( grd2%aux(i2)%srf_data(i)%data,STAT=ios )
      NULLIFY( grd2%aux(i2)%srf_data(i)%data )
    END IF
  END DO
  DEALLOCATE( grd2%aux(i2)%srf_data,STAT=ios )
  NULLIFY( grd2%aux(i2)%srf_data )
  grd2%aux(i2)%alloc = .FALSE.
END IF

IF( grd1%aux(i1)%alloc )THEN
  ALLOCATE( grd2%aux(i2)%srf_data(grd2%mxgrd),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = grd2%mxgrd
    GOTO 9999
  END IF
  grd2%aux(i2)%alloc = .TRUE.
  DO i = 1,grd1%ncells
    IF( ASSOCIATED(grd1%aux(i1)%srf_data(i)%data) )THEN
      j = SIZE( grd1%aux(i1)%srf_data(i)%data )
      ALLOCATE( grd2%aux(i2)%srf_data(i)%data(j),STAT=ios )
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = j
        GOTO 9999
      END IF
      grd2%aux(i2)%srf_data(i)%data = grd1%aux(i1)%srf_data(i)%data
    ELSE
      NULLIFY( grd2%aux(i2)%srf_data(i)%data )
    END IF
  END DO
  DO i = grd1%ncells+1,grd2%mxgrd
    NULLIFY( grd2%aux(i2)%srf_data(i)%data )
  END DO
END IF

SAG_CopyAux = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_IsRefined
!*******************************************************************************
LOGICAL FUNCTION SAG_IsRefined( id,igrd )

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: id
INTEGER, DIMENSION(:), POINTER      :: igrd

IF( id /= 0 )THEN
  SAG_IsRefined = igrd(id) /= 0
ELSE
  SAG_IsRefined = .FALSE.
END IF

RETURN
END
!*******************************************************************************
!                SAG_NewGrdStr
!*******************************************************************************
INTEGER FUNCTION SAG_NewGrdStr( id )

! Allocate grid structure; update linked list

USE sagdef_fd
USE sagerr_fd
USE saglst_fd
USE sagerr_fi

!  Add a SAG list structure to linked list

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: id

TYPE( SAGlist_str ), POINTER :: Plist,NewList

INTEGER ios,IDprev,IDnext

SAG_NewGrdStr = SAG_ERROR

IF( .NOT.ASSOCIATED(Grdlist%First) )THEN ! list is empty; this will be the first grid structure

  ALLOCATE( Plist,STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = 1
    GOTO 9999
  END IF

  id            =  1
  Plist%id      =  id
  Grdlist%First => Plist
  Grdlist%Last  => Plist

  NULLIFY( Grdlist%First%next,Grdlist%First%prev )
  NULLIFY( Grdlist%Last%next, Grdlist%Last%prev )

  ALLOCATE( Plist%grd,STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = 1
    GOTO 9999
  END IF
  NULLIFY( Plist%grd%ipdat,Plist%grd%ipgrd,Plist%grd%ipblk, &
           Plist%grd%ipnam,Plist%grd%ipflg,Plist%grd%aux )

  ALLOCATE( Plist%triT,STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = 1
    GOTO 9999
  END IF
  NULLIFY( Plist%triT%iptri,Plist%triT%ipconn,Plist%triT%ipedge, &
                                              Plist%triT%ipenod )
  NULLIFY( Plist%triT%nodeT%ipcell,Plist%triT%nodeT%ipnode, &
                                   Plist%triT%nodeT%ipdata )
  Plist%grd%id = id

ELSE ! list is not empty; add to list

  Plist  => Grdlist%First
  IDprev =  0
  id     =  -1

  DO WHILE( ASSOCIATED(Plist) )

    IDnext = Plist%id

    IF( IDnext > IDprev + 1 )THEN ! insert in list since id's are not sequential
      id = IDprev + 1
      ALLOCATE( NewList,STAT=ios )
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = 1
        GOTO 9999
      END IF
      NewList%id        =  id
      NewList%next      => Plist
      NewList%prev      => Plist%prev
      Plist%prev        => NewList
      IF( ASSOCIATED(NewList%prev) )NewList%prev%next => NewList

      ALLOCATE( NewList%grd,STAT=ios )
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = 1
        GOTO 9999
      END IF
      NULLIFY( NewList%grd%ipdat,NewList%grd%ipgrd,NewList%grd%ipblk, &
               NewList%grd%ipnam,NewList%grd%ipflg,NewList%grd%aux )

      ALLOCATE( NewList%triT,STAT=ios )
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = 1
        GOTO 9999
      END IF
      NULLIFY( NewList%triT%iptri,NewList%triT%ipconn,NewList%triT%ipedge, &
                                                      NewList%triT%ipenod )
      NULLIFY( NewList%triT%nodeT%ipcell,NewList%triT%nodeT%ipnode, &
                                         NewList%triT%nodeT%ipdata )

      NewList%grd%id = id

      EXIT
    END IF

    IDprev = IDnext
    Plist  => Plist%next

  END DO

  IF( id == 1 )THEN ! check if structure added to front of list

    GrdList%First => NewList

  ELSE IF( id == -1 )THEN ! add to end of list

    id = IDprev + 1
    ALLOCATE( Grdlist%Last%next,STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = 1
      GOTO 9999
    END IF

    Grdlist%Last%next%prev => Grdlist%Last
    Grdlist%Last           => Grdlist%Last%next
    Grdlist%Last%id        =  id
    NULLIFY( Grdlist%Last%next )

    ALLOCATE( Grdlist%Last%grd,STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = 1
      GOTO 9999
    END IF
    NULLIFY( Grdlist%Last%grd%ipdat,Grdlist%Last%grd%ipgrd,Grdlist%Last%grd%ipblk, &
             Grdlist%Last%grd%ipnam,Grdlist%Last%grd%ipflg,Grdlist%Last%grd%aux )
    Grdlist%Last%grd%id = id

    ALLOCATE( GrdList%Last%triT,STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = 1
      GOTO 9999
    END IF
    NULLIFY( GrdList%Last%triT%iptri,GrdList%Last%triT%ipconn, &
             GrdList%Last%triT%ipedge,GrdList%Last%triT%ipenod )
    NULLIFY( GrdList%Last%triT%nodeT%ipcell,GrdList%Last%triT%nodeT%ipnode, &
                                            GrdList%Last%triT%nodeT%ipdata )
  END IF

END IF

SAG_NewGrdStr = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_RmvGrdStr
!*******************************************************************************
INTEGER FUNCTION SAG_RmvGrdStr( id )

USE sagdef_fd
USE sagerr_fd
USE saglst_fd
USE sagerr_fi
USE PtrGrdStrItf

!  Remove a SAG list structure from linked list

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id

TYPE( SAGlist_str ),      POINTER :: Plist
TYPE( SAGgrid_str ),      POINTER :: grd
TYPE( SAGTriangleT_str ), POINTER :: triT

INTEGER irv, ios

INTEGER, EXTERNAL :: SAG_FreeGridID

SAG_RmvGrdStr = SAG_ERROR

grd => SAG_PtrGrdStr( id )     ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN ! Not Associated - just return
  SAG_RmvGrdStr = SAG_OK
  GOTO 9999
END IF

!  Deallocate structure pointers

irv = SAG_FreeGridID( id )
DEALLOCATE( grd,STAT=ios )

triT => SAG_PtrTriStr( id ) ! Associate "local" grid structure pointer
IF( ASSOCIATED(triT) )THEN
  CALL SAG_FreeTriangleID( id )
  DEALLOCATE( triT,STAT=ios )
END IF


! Now take care of linked list; check first and last in list

IF( ASSOCIATED(GrdList%First) )THEN

  IF( GrdList%First%id == id )THEN
    Plist => GrdList%First%next
    DEALLOCATE( GrdList%First,STAT=ios )
    IF( ASSOCIATED(Plist) )THEN
      GrdList%First => Plist
      NULLIFY( Plist%prev )
    ELSE
      NULLIFY( GrdList%First )
      NULLIFY( GrdList%Last  )
    END IF
    SAG_RmvGrdStr = SAG_OK
  ELSE IF( GrdList%Last%id == id )THEN
    Plist => GrdList%Last%prev
    DEALLOCATE( GrdList%Last,STAT=ios )
    GrdList%Last => Plist
    NULLIFY( Plist%next )
    SAG_RmvGrdStr = SAG_OK
  END IF

ELSE

  GOTO 9999 ! list is empty

END IF

IF( SAG_RmvGrdStr == SAG_OK )GOTO 9999 !done

! deallocate from the list "interior"

Plist => GrdList%First%next

DO WHILE( ASSOCIATED(Plist) )

  IF( Plist%id == id )THEN
    Plist%next%prev => Plist%prev
    Plist%prev%next => Plist%next
    DEALLOCATE( Plist,STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = 1
      GOTO 9999
    END IF
    SAG_RmvGrdStr = SAG_OK
    EXIT
  END IF

  Plist => Plist%next

END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_ClearList
!*******************************************************************************
INTEGER FUNCTION SAG_ClearList()

USE sagdef_fd
USE saglst_fd

! Deallocate all SAG grid structures

IMPLICIT NONE

TYPE( SAGlist_str ), POINTER :: Plist, NextList

INTEGER ios, irv

INTERFACE
  INTEGER FUNCTION SAG_FreeGrid( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
  END FUNCTION SAG_FreeGrid

  SUBROUTINE SAG_FreeTriangle( triT )
    USE sagtri_fd
    TYPE( SAGTriangleT_str ), POINTER :: triT
  END SUBROUTINE SAG_FreeTriangle
END INTERFACE

SAG_ClearList = SAG_OK

IF( .NOT.ASSOCIATED(GrdList%First) )RETURN

Plist => GrdList%First

DO WHILE( ASSOCIATED(Plist) )

  irv = SAG_FreeGrid( Plist%grd )
  DEALLOCATE( Plist%grd,STAT=ios )
  IF( ASSOCIATED(Plist%triT) )THEN
    CALL SAG_FreeTriangle( Plist%triT )
    DEALLOCATE( Plist%triT,STAT=ios )
  END IF

  NextList => Plist%next

  DEALLOCATE( Plist,STAT=ios )

  Plist => NextList

END DO

NULLIFY( GrdList%First,GrdList%Last )

RETURN
END
!*******************************************************************************
!                SAG_PtrGrdStr
!*******************************************************************************
FUNCTION SAG_PtrGrdStr( id ) RESULT( grd )

USE saglst_fd

! Point to SAG grid structure with specified id

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id

TYPE( SAGgrid_str ), POINTER :: grd ! function result is a pointer to a grid structure
TYPE( SAGlist_str ), POINTER :: Plist

NULLIFY( grd )

Plist => GrdList%First

DO WHILE( ASSOCIATED(Plist) )

  IF( Plist%id == id )THEN
    grd => Plist%grd
    EXIT
  END IF

  Plist => Plist%next

END DO

RETURN
END
!*******************************************************************************
!                SAG_PtrAuxStr
!*******************************************************************************
FUNCTION SAG_PtrAuxStr( id ) RESULT( grd_aux )

USE saglst_fd

! Point to SAG grid structure with specified id

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id

TYPE( SAGfield_aux ),DIMENSION(:), POINTER :: grd_aux ! function result is a pointer to a grid aux structure
TYPE( SAGlist_str  ),              POINTER :: Plist

NULLIFY( grd_aux )

Plist => GrdList%First

DO WHILE( ASSOCIATED(Plist) )

  IF( Plist%id == id )THEN
    grd_aux => Plist%grd%aux
    EXIT
  END IF

  Plist => Plist%next

END DO

RETURN
END
!*******************************************************************************
!                SAG_PtrTriStr
!*******************************************************************************
FUNCTION SAG_PtrTriStr( id ) RESULT( triT )

USE sagtri_fd
USE saglst_fd

! Point to SAG grid structure with specified id

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id

TYPE( SAGTriangleT_str ), POINTER :: triT ! function result is a pointer to a grid structure
TYPE( SAGlist_str ),      POINTER :: Plist

NULLIFY( triT )

Plist => GrdList%First

DO WHILE( ASSOCIATED(Plist) )

  IF( Plist%id == id )THEN
    IF( ASSOCIATED(Plist%triT) )triT => Plist%triT
    EXIT
  END IF

  Plist => Plist%next

END DO

RETURN
END
!*******************************************************************************
!                SAG_InitGridStr
!*******************************************************************************
INTEGER FUNCTION SAG_InitGridStr( id,time,m0,n0,xmin,ymin,dx,dy,maxlev,delmin )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Initialize a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id,m0,n0,maxlev
REAL,    INTENT( IN ) :: time,xmin,ymin,dx,dy,delmin

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER iv, i0

SAG_InitGridStr = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

! check to make sure there is enough space

IF( m0*n0 > grd%mxgrd )THEN
  LastError = SAG_ERR_MXGRD
  LastParm  = m0*n0
  GOTO 9999
END IF

! set grid parameters in structure

grd%time   = time !Time
grd%ncells = m0*n0 !Total no. of cells
grd%nx     = m0 !Primary grid dimensions
grd%ny     = n0
grd%xmin   = xmin !Grid origin
grd%ymin   = ymin
grd%dx     = dx !Grid size
grd%dy     = dy
IF( maxlev < 0 )THEN
  grd%maxlev = -99 !Max. refinement level
ELSE
  grd%maxlev = -maxlev !Max. refinement level
END IF
grd%delmin = delmin !Minimum grid size

! clear top level grid

grd%ipgrd(1:m0*n0) = 0

DO iv = 1,grd%nvart
  i0 = (iv-1)*grd%mxgrd
  grd%ipdat(i0+1:i0+m0*n0) = 0.
END DO

SAG_InitGridStr = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_InitGridFile
!*******************************************************************************
INTEGER FUNCTION SAG_InitGridFileID( id,nblk,nv,version,ifld,iaux,blkname,vname )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Initialize a SAG grid structure

IMPLICIT NONE

INTEGER,                    INTENT( IN ) :: id
INTEGER,                    INTENT( IN ) :: nblk
INTEGER,                    INTENT( IN ) :: nv
INTEGER,                    INTENT( IN ) :: version
INTEGER,      DIMENSION(*), INTENT( IN ) :: ifld
INTEGER,      DIMENSION(*), INTENT( IN ) :: iaux
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: blkname
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: vname

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_InitGridFile( grd,nblk,nv,version,ifld,iaux,blkname,vname )
    USE sagstr_fd
    TYPE ( SAGgrid_str ),       POINTER      :: grd
    INTEGER,                    INTENT( IN ) :: nblk
    INTEGER,                    INTENT( IN ) :: nv
    INTEGER,                    INTENT( IN ) :: version
    INTEGER,      DIMENSION(*), INTENT( IN ) :: ifld
    INTEGER,      DIMENSION(*), INTENT( IN ) :: iaux
    CHARACTER(*), DIMENSION(*), INTENT( IN ) :: blkname
    CHARACTER(*), DIMENSION(*), INTENT( IN ) :: vname
  END FUNCTION SAG_InitGridFile
END INTERFACE

SAG_InitGridFileID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

SAG_InitGridFileID = SAG_InitGridFile( grd,nblk,nv,version,ifld, &
                                                iaux,blkname,vname )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_InitGridFile( grd,nblk,nv,version,ifld,iaux,blkname,vname )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG grid structure

IMPLICIT NONE

TYPE ( SAGgrid_str ),       POINTER      :: grd
INTEGER,                    INTENT( IN ) :: nblk
INTEGER,                    INTENT( IN ) :: nv
INTEGER,                    INTENT( IN ) :: version
INTEGER,      DIMENSION(*), INTENT( IN ) :: ifld
INTEGER,      DIMENSION(*), INTENT( IN ) :: iaux
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: blkname
CHARACTER(*), DIMENSION(*), INTENT( IN ) :: vname

INTEGER i, j, nn, ios, naux, nfld

SAG_InitGridFile = SAG_ERROR

! set file header parameters

grd%ftype   = nblk    !No. of blocks
grd%nblk    = nblk    !No. of blocks
grd%nvart   = nv      !No. of fields
grd%version = version !SCIPUFF version

! allocate block pointer; set names

IF( .NOT.ASSOCIATED(grd%ipblk) )THEN
  ALLOCATE( grd%ipblk(nblk),STAT=ios )
  IF(ios /= 0)THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = nblk
    GOTO 9999
  END IF
ELSE
  LastError = SAG_ERR_INVALID
  LastParm  = grd%id
  GOTO 9999
END IF

naux = 0
nn   = 0
DO i = 1,nblk
  grd%ipblk(i)%name = TRIM(blkname(i))
  grd%ipblk(i)%ifld = ifld(i)
  grd%ipblk(i)%iaux = iaux(i)
  IF( iaux(i) > 0 )naux = naux + 1
  IF( i < nblk )THEN
    nfld = ifld(i+1) - ifld(i)
  ELSE
    nfld = nv + 1 - ifld(i)
  END IF
  grd%ipblk(i)%nfld = nfld
  ALLOCATE( grd%ipblk(i)%fldnam(nfld),STAT=ios )
  IF(ios /= 0)THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = nblk
    GOTO 9999
  END IF
  DO j = 1,nfld
    nn = nn + 1
    grd%ipblk(i)%fldnam(j) = TRIM(vname(nn))
  END DO
END DO

! --- Set up auxiliary data

grd%naux = naux      !No. of auxiliary data fields
IF( naux > 0 )THEN
  ALLOCATE( grd%aux(naux),STAT=ios )
  IF(ios /= 0)THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = nblk
    GOTO 9999
  END IF
  DO i = 1,naux
    grd%aux(i)%alloc = .FALSE.
    NULLIFY( grd%aux(i)%srf_data )
  END DO
END IF

! set field names

IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
  ALLOCATE( grd%ipnam(grd%nvart),STAT=ios )
  IF(ios /= 0)THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = grd%nvart
    GOTO 9999
  END IF
END IF

DO i = 1,nv
  grd%ipnam(i) = TRIM(vname(i))
END DO

SAG_InitGridFile = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FillGridStrGrid
!*******************************************************************************
INTEGER FUNCTION SAG_FillGridStrGridID( id,ncell,Igrd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

! Fill data field ifld of a SAG grid structure with array Fdata

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: id
INTEGER,               INTENT( IN ) :: ncell
INTEGER, DIMENSION(*), INTENT( IN ) :: Igrd

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_FillGridStrGrid( grd,ncell,Igrd )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
    INTEGER,               INTENT( IN ) :: ncell
    INTEGER, DIMENSION(*), INTENT( IN ) :: Igrd
  END FUNCTION SAG_FillGridStrGrid
END INTERFACE

SAG_FillGridStrGridID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

SAG_FillGridStrGridID = SAG_FillGridStrGrid( grd,ncell,Igrd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_FillGridStrGrid( grd,ncell,Igrd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

! Fill data field ifld of a SAG grid structure with array Fdata

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER      :: grd
INTEGER,               INTENT( IN ) :: ncell
INTEGER, DIMENSION(*), INTENT( IN ) :: Igrd

INTEGER i

SAG_FillGridStrGrid = SAG_ERROR

IF( ncell > grd%mxgrd )THEN
  LastError = SAG_ERR_NCELL
  LastParm  = ncell
  GOTO 9999
END IF

grd%ncells = ncell

DO i = 1,ncell
  grd%ipgrd(i) = Igrd(i)
END DO

SAG_FillGridStrGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FillGridStrField
!*******************************************************************************
INTEGER FUNCTION SAG_FillGridStrFieldID( id,ifld,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Fill data field ifld of a SAG grid structure with array Fdata

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: id
INTEGER,            INTENT( IN ) :: ifld
REAL, DIMENSION(*), INTENT( IN ) :: Fdata

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_FillGridStrField( grd,ifld,Fdata )
    USE sagstr_fd
    TYPE ( SAGgrid_str ),  POINTER      :: grd
    INTEGER,               INTENT( IN ) :: ifld
    REAL,    DIMENSION(*), INTENT( IN ) :: Fdata
  END FUNCTION SAG_FillGridStrField
END INTERFACE

SAG_FillGridStrFieldID = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

SAG_FillGridStrFieldID = SAG_FillGridStrField( grd,ifld,Fdata )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_FillGridStrField( grd,ifld,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Fill data field ifld of a SAG grid structure with array Fdata

IMPLICIT NONE

TYPE ( SAGgrid_str ),  POINTER      :: grd
INTEGER,               INTENT( IN ) :: ifld
REAL,    DIMENSION(*), INTENT( IN ) :: Fdata

INTEGER i, i0

SAG_FillGridStrField = SAG_ERROR

IF( ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

i0 = (ifld-1)*grd%mxgrd

DO i = 1,grd%ncells
  grd%ipdat(i+i0) = Fdata(i)
END DO

SAG_FillGridStrField = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_GetGridStrHead
!*******************************************************************************
INTEGER FUNCTION SAG_GetGridStrHead( grdI,time,ncell,nv,mxgrd,mxlev, &
                                          xmin,ymin,dx,dy,nx,ny )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Initialize a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN )  :: grdI
INTEGER, INTENT( OUT ) :: ncell,nv,mxgrd,mxlev,nx,ny
REAL,    INTENT( OUT ) :: time,xmin,ymin,dx,dy

TYPE( SAGgrid_str ), POINTER :: grd

SAG_GetGridStrHead = SAG_ERROR

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

! get parameters from structure

time  = grd%time
ncell = grd%ncells
nv    = grd%nvart
mxgrd = grd%mxgrd
mxlev = grd%maxlev
xmin  = grd%xmin
ymin  = grd%ymin
dx    = grd%dx
dy    = grd%dy
nx    = grd%nx
ny    = grd%ny

SAG_GetGridStrHead = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_GetGridStrData
!*******************************************************************************
INTEGER FUNCTION SAG_GetGridStrData( grdI,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Get entire data field of a SAG grid and put into array Fdata

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: grdI
REAL, DIMENSION(*), INTENT( OUT ) :: Fdata

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER iv, i, i0

SAG_GetGridStrData = SAG_ERROR

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

DO iv = 1,grd%nvart
  i0 = (iv-1)*grd%mxgrd
  DO i = i0+1,i0+grd%mxgrd
    Fdata(i) = grd%ipdat(i)
  END DO
END DO

SAG_GetGridStrData = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_PutGridStrData
!*******************************************************************************
INTEGER FUNCTION SAG_PutGridStrData( grdI,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Put entire data field Fdata into a SAG grid structure

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
REAL,    INTENT( IN ) :: Fdata(1)

TYPE ( SAGgrid_str ), POINTER :: grd

INTEGER iv, i, i0

SAG_PutGridStrData = SAG_ERROR

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

DO iv = 1,grd%nvart
  i0 = (iv-1)*grd%mxgrd
  DO i = i0+1,i0+grd%mxgrd
    grd%ipdat(i) = Fdata(i)
  END DO
END DO

SAG_PutGridStrData = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_GetGridStrField
!*******************************************************************************
INTEGER FUNCTION SAG_GetGridStrField( id,ifld,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Get data field ifld from a SAG grid structure

IMPLICIT NONE

INTEGER,             INTENT( IN  ) :: id, ifld
REAL, DIMENSION(*),  INTENT( OUT ) :: Fdata

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i,i0

SAG_GetGridStrField = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

IF( ifld > grd%mxfld )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = ifld
  GOTO 9999
END IF

i0 = (ifld-1)*grd%mxgrd

DO i = 1,grd%mxgrd
  Fdata(i) = grd%ipdat(i+i0)
END DO

SAG_GetGridStrField = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_GetGridStrGrid
!*******************************************************************************
INTEGER FUNCTION SAG_GetGridStrGrid( id,Fdata )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!    Get grid array for a SAG grid structure

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: id
INTEGER, DIMENSION(*), INTENT( OUT ) :: Fdata

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER i

SAG_GetGridStrGrid = SAG_ERROR

grd => SAG_PtrGrdStr( id ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = id
  GOTO 9999
END IF

DO i = 1,grd%mxgrd
  Fdata(i) = grd%ipgrd(i)
END DO

SAG_GetGridStrGrid = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_SetNvart
!*******************************************************************************
INTEGER FUNCTION SAG_SetNvart( grdI,nvart )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Put nvart into SAG grid structure header

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
INTEGER, INTENT( IN ) :: nvart

TYPE( SAGgrid_str ), POINTER :: grd

SAG_SetNvart = SAG_ERROR

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

grd%nvart = nvart

SAG_SetNvart = SAG_OK

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SAG_GetMaxLevID( grdI )

USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

SAG_GetMaxLevID = -999

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_GetMaxLevID = grd%maxlev

9999 CONTINUE

RETURN
END
!===============================================================================

REAL FUNCTION SAG_GetDelminID( grdI )

USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

SAG_GetDelminID = 0.

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_GetDelminID = grd%delmin

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SAG_GetNcellID( grdI )

USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER grdI

TYPE( SAGgrid_str ), POINTER :: grd

SAG_GetNcellID = 0

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_GetNcellID = grd%ncells

9999 CONTINUE

RETURN
END

!*******************************************************************************
!                SAG_ReallocateID
!*******************************************************************************
INTEGER FUNCTION SAG_ReallocateID( grdI,mxgrd,mxfld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

INTEGER, INTENT( IN ) :: grdI
INTEGER, INTENT( IN ) :: mxgrd
INTEGER, INTENT( IN ) :: mxfld

TYPE ( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_Reallocate( grd,mxgrd,mxfld)
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER, INTENT( IN )             :: mxgrd
    INTEGER, INTENT( IN )             :: mxfld
  END FUNCTION SAG_Reallocate
END INTERFACE

SAG_ReallocateID = SAG_ERROR

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_ReallocateID = SAG_Reallocate( grd,mxgrd,mxfld )

9999 CONTINUE

RETURN
END

!*******************************************************************************
!                SAG_Reallocate
!*******************************************************************************
INTEGER FUNCTION SAG_Reallocate( grd,mxgrd,mxfld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

TYPE( SAGgrid_str ), POINTER      :: grd
INTEGER, INTENT( IN )             :: mxgrd
INTEGER, INTENT( IN )             :: mxfld

INTEGER, DIMENSION(:), POINTER :: old_grid !grid data
REAL,    DIMENSION(:), POINTER :: old_data !surface data
INTEGER, DIMENSION(:), POINTER :: old_flag !internal flags array

INTEGER, DIMENSION(:), POINTER :: new_grid !grid data
REAL,    DIMENSION(:), POINTER :: new_data !surface data
INTEGER, DIMENSION(:), POINTER :: new_flag !internal flags array

TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: old_aux
TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: new_aux

INTEGER i, j, ios

SAG_Reallocate = SAG_ERROR

IF( mxgrd < grd%ncells )THEN
  LastError = SAG_ERR_MXGRD
  LastParm  = grd%ncells
  GOTO 9999
END IF

IF( mxfld < grd%mxfld )THEN
  LastError = SAG_ERR_MXFLD
  LastParm  = grd%mxfld
  GOTO 9999
END IF

old_grid => grd%ipgrd
old_data => grd%ipdat
old_flag => grd%ipflg

NULLIFY( new_grid )
NULLIFY( new_data )
NULLIFY( new_flag )
NULLIFY( new_aux  )

IF( mxgrd > 0 )THEN

  ALLOCATE( new_grid(mxgrd),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxgrd
    GOTO 9999
  END IF

  IF( mxfld > 0 )THEN
    ALLOCATE( new_data(mxgrd*mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxgrd*mxfld
      GOTO 9998
    END IF

    ALLOCATE( new_flag(mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxfld
      GOTO 9997
    END IF
  ELSE
    LastError = SAG_ERR_MXFLD
    LastParm  = mxfld
    GOTO 9999
  END IF

ELSE
  LastError = SAG_ERR_MXGRD
  LastParm  = mxgrd
  GOTO 9999
END IF

IF( ASSOCIATED(new_grid) .AND. ASSOCIATED(new_data) .AND. ASSOCIATED(new_flag) )THEN

  DO i = 1,grd%ncells
    new_grid(i) = old_grid(i)
  END DO
  DO i = grd%ncells+1,mxgrd
    new_grid(i) = 0
  END DO

  DO i = 1,grd%mxfld
    new_flag(i) = old_flag(i)
    DO j = 1,grd%mxgrd
      new_data((i-1)*mxgrd+j) = old_data((i-1)*grd%mxgrd+j)
    END DO
    DO j = grd%mxgrd+1,mxgrd
      new_data((i-1)*mxgrd+j) = 0.0
    END DO
  END DO

  DO i = grd%mxfld+1,mxfld
    new_flag(i) = SAG_GRID_FULL
    DO j = 1,mxgrd
      new_data((i-1)*mxgrd+j) = 0.0
    END DO
  END DO

  IF( grd%naux > 0 )THEN
    old_aux => grd%aux
    ALLOCATE( new_aux(grd%naux),STAT=ios )
    IF(ios /= 0)THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = grd%naux
      GOTO 9996
    END IF
    DO i = 1,grd%naux
      IF( old_aux(i)%alloc )THEN
        new_aux(i)%alloc = .TRUE.
        ALLOCATE( new_aux(i)%srf_data(mxgrd),STAT=ios )
        IF( ios /= 0 )THEN
          LastError = SAG_ERR_MALLOC
          LastParm  = mxgrd
          GOTO 9995
        END IF
        DO j = 1,grd%mxgrd
          IF( ASSOCIATED(old_aux(i)%srf_data(j)%data) )THEN
            new_aux(i)%srf_data(j)%data => old_aux(i)%srf_data(j)%data
          ELSE
            NULLIFY(new_aux(i)%srf_data(j)%data)
          END IF
        END DO
        DO j = grd%mxgrd+1,mxgrd
          NULLIFY(new_aux(i)%srf_data(j)%data)
        END DO
      ELSE
        new_aux(i)%alloc = .FALSE.
        NULLIFY( new_aux(i)%srf_data )
      END IF
    END DO
  ELSE
    NULLIFY(old_aux)
  END IF

  grd%mxgrd = mxgrd
  grd%mxfld = mxfld

  grd%ipgrd => new_grid
  grd%ipflg => new_flag
  grd%ipdat => new_data

  IF( ASSOCIATED(new_aux) )grd%aux => new_aux

  IF( ASSOCIATED(old_flag) )DEALLOCATE( old_flag,STAT=ios )
  IF( ASSOCIATED(old_data) )DEALLOCATE( old_data,STAT=ios )
  IF( ASSOCIATED(old_grid) )DEALLOCATE( old_grid,STAT=ios )
  IF( ASSOCIATED(old_aux) )THEN
    DO i = 1,grd%naux
      IF( ASSOCIATED(old_aux(i)%srf_data) )DEALLOCATE( old_aux(i)%srf_data,STAT=ios)
    END DO
    DEALLOCATE( old_aux ,STAT=ios )
  END IF

  SAG_Reallocate = SAG_OK

END IF

9999 CONTINUE

RETURN

9995 DO i = 1,grd%naux
       IF( ASSOCIATED(new_aux(i)%srf_data) )DEALLOCATE( new_aux(i)%srf_data,STAT=ios)
     END DO
     IF( ASSOCIATED(new_aux ) )DEALLOCATE( new_aux ,STAT=ios )
9996 IF( ASSOCIATED(new_flag) )DEALLOCATE( new_flag,STAT=ios )
9997 IF( ASSOCIATED(new_data) )DEALLOCATE( new_data,STAT=ios )
9998 IF( ASSOCIATED(new_grid) )DEALLOCATE( new_grid,STAT=ios )
GOTO 9999

END
