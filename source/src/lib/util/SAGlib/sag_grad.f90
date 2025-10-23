!*******************************************************************************
!               SAG_SetGradient
!*******************************************************************************
INTEGER FUNCTION SAG_SetGradient( grdI,ifld,drt,drb,dlt,dlb )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,            INTENT( IN  )   :: grdI
INTEGER,            INTENT( IN  )   :: ifld
REAL, DIMENSION(*), INTENT( INOUT ) :: drt,drb,dlt,dlb

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE

  SUBROUTINE SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN  )   :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientBoth

  SUBROUTINE SAG_SetGradientVert( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN  )   :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientVert

  SUBROUTINE SAG_SetGradientHorz( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN  )   :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientHorz

  SUBROUTINE SAG_SetGradientNone( grd,ifld,drt,drb,dlt,dlb )
    USE sagstr_fd
    TYPE( SAGgrid_str ),        POINTER         :: grd
    INTEGER,                    INTENT( IN  )   :: ifld
    REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb
  END SUBROUTINE SAG_SetGradientNone

END INTERFACE

SAG_SetGradient = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_SetGradient = SAG_OK
SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH  )
    CALL SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )
  CASE( SAG_GRID_HORZ )
    CALL SAG_SetGradientHorz( grd,ifld,drt,drb,dlt,dlb )
  CASE( SAG_GRID_VERT )
    CALL SAG_SetGradientVert( grd,ifld,drt,drb,dlt,dlb )
  CASE( SAG_GRID_NONE )
    CALL SAG_SetGradientNone( grd,ifld,drt,drb,dlt,dlb )
  CASE DEFAULT
    SAG_SetGradient = SAG_ERROR
    LastError = SAG_ERR_TYPE
    LastParm  = grd%type
END SELECT

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_SetGradientNone
!*******************************************************************************
SUBROUTINE SAG_SetGradientNone( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),        POINTER         :: grd
INTEGER,                    INTENT( IN  )   :: ifld
REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb

INTEGER i

!==== Save grid arrays in saggrd_fi

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Clear gradient arrays

DO i = 1,grd%ncells
  drt(i) = 0.0
  drb(i) = 0.0
  dlt(i) = 0.0
  dlb(i) = 0.0
END DO

RETURN
END
!*******************************************************************************
!               SAG_SetGradientBoth
!*******************************************************************************
SUBROUTINE SAG_SetGradientBoth( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),        POINTER         :: grd
INTEGER,                    INTENT( IN    ) :: ifld
REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb

INTEGER ix,iy,id

TYPE( SAGcell_str ) p1, pr, pt, pl, pb, prt, prb, plt, plb

!==== Save grid arrays in saggrd_fi

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Compute the gradients

!$OMP PARALLEL IF(nx*ny > 1)
!$OMP SINGLE
DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p1%id = id

    IF( ix < nx )THEN
      IF( iy < ny )THEN
        prt%id = id + nx + 1
      ELSE
        prt%id = 0
      END IF
      IF( iy > 1 )THEN
        prb%id = id - nx + 1
      ELSE
        prb%id = 0
      END IF
      pr%id = id + 1
    ELSE
      pr%id = 0
      prt%id = 0
      prb%id = 0
    END IF

    IF( iy < ny )THEN
      pt%id = id + nx
    ELSE
      pt%id = 0
    END IF

    IF( ix > 1 )THEN
      IF( iy < ny )THEN
        plt%id = id + nx - 1
      ELSE
        plt%id = 0
      END IF
      IF( iy > 1 )THEN
        plb%id = id - nx - 1
      ELSE
        plb%id = 0
      END IF
      pl%id = id - 1
    ELSE
      pl%id  = 0
      plt%id = 0
      plb%id = 0
    END IF

    IF( iy > 1 )THEN
      pb%id = id - nx
    ELSE
      pb%id = 0
    END IF

!$OMP TASK FIRSTPRIVATE(p1,pr,pl,pt,pb,prt,prb,plt,plb) DEFAULT(SHARED)
    CALL SAG_GradientBoth( p1,pr,pl,pt,pb,prt,prb,plt,plb )
!$OMP END TASK

  END DO
END DO
!$OMP END SINGLE
!$OMP END PARALLEL

RETURN
END
!*******************************************************************************
!               SAG_GradientBoth
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_GradientBoth( p,pr,pl,pt,pb,prt,prb,plt,plb )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p,pr,pt,pl,pb,prt,prb,plt,plb

INTERFACE
  REAL FUNCTION SAG_GetValue( dat,id,x )
    REAL, DIMENSION(:), POINTER      :: dat
    INTEGER,            INTENT( IN ) :: id
    REAL,               INTENT( IN ) :: x
  END FUNCTION SAG_GetValue

  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

TYPE( SAGcell_str ) q,qr,qt,ql,qb,qrt,qrb,qlt,qlb

REAL xc,xr,xl,xt,xb,xrt,xlt,xrb,xlb
REAL gsum

REAL, EXTERNAL :: SAG_GradRat

!==== Set gradient

IF( pgrd(p%id) > 0 )THEN

  xc = SAG_GetValue( pdat,p%id,0.0 )
  IF( xc /= 0.0 )THEN
    xr = SAG_GetValue( pdat,pr%id,xc )
    xl = SAG_GetValue( pdat,pl%id,xc )
    xt = SAG_GetValue( pdat,pt%id,xc )
    xb = SAG_GetValue( pdat,pb%id,xc )
    IF( SAG_GetValue(pdat,pt%id,Special) == Special )THEN
      xrt = SAG_GetValue( pdat,prt%id,xr )
      xlt = SAG_GetValue( pdat,plt%id,xl )
    ELSE
      xrt = SAG_GetValue( pdat,prt%id,xt )
      xlt = SAG_GetValue( pdat,plt%id,xt )
    END IF
    IF( SAG_GetValue(pdat,pb%id,Special) == Special )THEN
      xrb = SAG_GetValue( pdat,prb%id,xr )
      xlb = SAG_GetValue( pdat,plb%id,xl )
    ELSE
      xrb = SAG_GetValue( pdat,prb%id,xb )
      xlb = SAG_GetValue( pdat,plb%id,xb )
    END IF
    grt(p%id) = SAG_GradRat(0.25,xr+xt+xrt,xc)
    grb(p%id) = SAG_GradRat(0.25,xr+xb+xrb,xc)
    glt(p%id) = SAG_GradRat(0.25,xl+xt+xlt,xc)
    glb(p%id) = SAG_GradRat(0.25,xl+xb+xlb,xc)
    gsum = 0.25*(SQRT(grt(p%id)) + SQRT(grb(p%id)) + SQRT(glt(p%id)) + SQRT(glb(p%id)))
    gsum = gsum*gsum
    grt(p%id) = LOG(grt(p%id)/gsum)
    grb(p%id) = LOG(grb(p%id)/gsum)
    glt(p%id) = LOG(glt(p%id)/gsum)
    glb(p%id) = LOG(glb(p%id)/gsum)
  ELSE
    grt(p%id)=0.0
    grb(p%id)=0.0
    glt(p%id)=0.0
    glb(p%id)=0.0
  END IF

  CALL SAG_GetID( pgrd,p  ,0,q )
  CALL SAG_GetID( pgrd,p  ,1,qr )
  CALL SAG_GetID( pgrd,p  ,2,qt )
  CALL SAG_GetID( pgrd,pl ,1,ql )
  CALL SAG_GetID( pgrd,pb ,2,qb )
  CALL SAG_GetID( pgrd,p  ,3,qrt )
  CALL SAG_GetID( pgrd,pb ,3,qrb )
  CALL SAG_GetID( pgrd,pl ,3,qlt )
  CALL SAG_GetID( pgrd,plb,3,qlb )

  CALL SAG_GradientBoth( q,qr,ql,qt,qb,qrt,qrb,qlt,qlb )

  CALL SAG_GetID( pgrd,p  ,1,q )
  CALL SAG_GetID( pgrd,pr ,0,qr )
  CALL SAG_GetID( pgrd,p  ,3,qt )
  CALL SAG_GetID( pgrd,p  ,0,ql )
  CALL SAG_GetID( pgrd,pb ,3,qb )
  CALL SAG_GetID( pgrd,pr ,2,qrt )
  CALL SAG_GetID( pgrd,prb,2,qrb )
  CALL SAG_GetID( pgrd,p  ,2,qlt )
  CALL SAG_GetID( pgrd,pb ,2,qlb )

  CALL SAG_GradientBoth( q,qr,ql,qt,qb,qrt,qrb,qlt,qlb)

  CALL SAG_GetID( pgrd,p  ,2,q )
  CALL SAG_GetID( pgrd,p  ,3,qr )
  CALL SAG_GetID( pgrd,pt ,0,qt )
  CALL SAG_GetID( pgrd,pl ,3,ql )
  CALL SAG_GetID( pgrd,p  ,0,qb )
  CALL SAG_GetID( pgrd,pt ,1,qrt )
  CALL SAG_GetID( pgrd,p  ,1,qrb )
  CALL SAG_GetID( pgrd,plt,1,qlt )
  CALL SAG_GetID( pgrd,pl ,1,qlb )

  CALL SAG_GradientBoth( q,qr,ql,qt,qb,qrt,qrb,qlt,qlb)

  CALL SAG_GetID( pgrd,p  ,3,q )
  CALL SAG_GetID( pgrd,pr ,2,qr )
  CALL SAG_GetID( pgrd,pt ,1,qt )
  CALL SAG_GetID( pgrd,p  ,2,ql )
  CALL SAG_GetID( pgrd,p  ,1,qb )
  CALL SAG_GetID( pgrd,prt,0,qrt )
  CALL SAG_GetID( pgrd,pr ,0,qrb )
  CALL SAG_GetID( pgrd,pt ,0,qlt )
  CALL SAG_GetID( pgrd,p  ,0,qlb )

  CALL SAG_GradientBoth( q,qr,ql,qt,qb,qrt,qrb,qlt,qlb)

ELSE

  grt(p%id) = 0.0
  grb(p%id) = 0.0
  glt(p%id) = 0.0
  glb(p%id) = 0.0

END IF

RETURN
END
!*******************************************************************************
!               SAG_SetGradientHorz
!*******************************************************************************
SUBROUTINE SAG_SetGradientHorz( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),        POINTER         :: grd
INTEGER,                    INTENT( IN    ) :: ifld
REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb

INTEGER ix,iy,id

TYPE( SAGcell_str ) p1,pr,pl

!==== Save grid arrays in saggrd_fi

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
pdat => grd%ipdat(grd%mxgrd*(ifld-1)+1:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Compute the gradients

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p1%id = id

    IF( ix < nx )THEN
      pr%id = id + 1
    ELSE
      pr%id = 0
    END IF

    IF( ix > 1 )THEN
      pl%id = id - 1
    ELSE
      pl%id  = 0
    END IF

    CALL SAG_GradientHorz( p1,pr,pl )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_GradientHorz
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_GradientHorz( p,pr,pl )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p,pr,pl

INTERFACE
  REAL FUNCTION SAG_GetValue( dat,id,x )
    REAL, DIMENSION(:), POINTER      :: dat
    INTEGER,            INTENT( IN ) :: id
    REAL,               INTENT( IN ) :: x
  END FUNCTION SAG_GetValue

  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

TYPE( SAGcell_str ) q,qr,ql

REAL xc,xr,xl

REAL, EXTERNAL :: SAG_GradRat

!==== Set gradient

IF( pgrd(p%id) > 0 )THEN

  xc = SAG_GetValue( pdat,p%id,0.0 )
  IF( xc /= 0.0 )THEN
    IF( pr%id /= 0 )THEN
      xr = SAG_GetValue( pdat,pr%id,xc )
      grt(p%id) = LOG(SAG_GradRat(0.5,xr,xc))
    ELSE
      grt(p%id) = 0.0
    END IF
    IF( pl%id /= 0 )THEN
      xl = SAG_GetValue(pdat,pl%id,xc)
      glt(p%id) = LOG(SAG_GradRat(0.5,xl,xc))
    ELSE
      glt(p%id) = 0.0
    END IF
  ELSE
    grt(p%id) = 0.0
    glt(p%id) = 0.0
  END IF

  CALL SAG_GetID( pgrd,p ,0,q )
  CALL SAG_GetID( pgrd,p ,1,qr )
  CALL SAG_GetID( pgrd,pl,1,ql )

  CALL SAG_GradientHorz( q,qr,ql )

  CALL SAG_GetID( pgrd,p ,1,q )
  CALL SAG_GetID( pgrd,pr,0,qr )
  CALL SAG_GetID( pgrd,p ,0,ql )

  CALL SAG_GradientHorz( q,qr,ql )

ELSE

  grt(p%id) = 0.0
  glt(p%id) = 0.0

END IF

RETURN
END
!*******************************************************************************
!               SAG_SetGradientVert
!*******************************************************************************
SUBROUTINE SAG_SetGradientVert( grd,ifld,drt,drb,dlt,dlb )

USE sagstr_fd
USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),        POINTER         :: grd
INTEGER,                    INTENT( IN    ) :: ifld
REAL, DIMENSION(*), TARGET, INTENT( INOUT ) :: drt,drb,dlt,dlb

INTEGER ix,iy,id,i

TYPE( SAGcell_str ) p1,pt,pb

!==== Save grid arrays in saggrd_fi

nx    = grd%nx
ny    = grd%ny
pgrd => grd%ipgrd
i = grd%mxgrd*(ifld-1)+1
pdat => grd%ipdat(i:)

grt => drt(1:grd%ncells)
grb => drb(1:grd%ncells)
glt => dlt(1:grd%ncells)
glb => dlb(1:grd%ncells)

!==== Compute the gradients

DO ix = 1,nx
  DO iy = 1,ny

    id = (iy-1)*nx + ix

    p1%id = id

    IF( iy < ny )THEN
      pt%id = id + nx
    ELSE
      pt%id = 0
    END IF

    IF( iy > 1 )THEN
      pb%id = id - nx
    ELSE
      pb%id = 0
    END IF

    CALL SAG_GradientVert( p1,pt,pb )

  END DO
END DO

RETURN
END
!*******************************************************************************
!               SAG_GradientVert
!*******************************************************************************
RECURSIVE SUBROUTINE SAG_GradientVert( p,pt,pb )

USE saggrd_fi
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGcell_str ), INTENT( IN ) :: p,pt,pb

INTERFACE
  REAL FUNCTION SAG_GetValue( dat,id,x )
    REAL, DIMENSION(:), POINTER      :: dat
    INTEGER,            INTENT( IN ) :: id
    REAL,               INTENT( IN ) :: x
  END FUNCTION SAG_GetValue

  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

TYPE( SAGcell_str ) q,qt,qb

REAL xc,xt,xb

REAL, EXTERNAL :: SAG_GradRat

!==== Set gradient

IF( pgrd(p%id) > 0 )THEN

  xc = SAG_GetValue( pdat,p%id,0.0 )
  IF( xc /= 0.0 )THEN
    xt = SAG_GetValue( pdat,pt%id,xc )
    xb = SAG_GetValue( pdat,pb%id,xc )
    grt(p%id) = LOG(SAG_GradRat(0.5,xt,xc))
    grb(p%id) = LOG(SAG_GradRat(0.5,xb,xc))
  ELSE
    grt(p%id) = 0.0
    grb(p%id) = 0.0
  END IF

  CALL SAG_GetID( pgrd,p ,0,q )
  CALL SAG_GetID( pgrd,p ,1,qt )
  CALL SAG_GetID( pgrd,pb,1,qb )

  CALL SAG_GradientVert( q,qt,qb )

  CALL SAG_GetID( pgrd,p ,1,q )
  CALL SAG_GetID( pgrd,pt,0,qt )
  CALL SAG_GetID( pgrd,p ,0,qb )

  CALL SAG_GradientVert( q,qt,qb )

ELSE

  grt(p%id) = 0.0
  grb(p%id) = 0.0

END IF

RETURN
END
!*******************************************************************************
!               SAG_GradRat
!*******************************************************************************
REAL FUNCTION SAG_GradRat( fac,sum,xc )

IMPLICIT NONE

REAL, INTENT( IN ) :: fac,sum,xc

REAL rat

IF( sum < xc*1.E-6 )THEN
  rat = 1.E-6
ELSE IF( sum > xc*1.E+6 )THEN
  rat = 1.E+6
ELSE
  rat = sum/xc
END IF

SAG_GradRat = fac*(rat + 1.)

RETURN
END
