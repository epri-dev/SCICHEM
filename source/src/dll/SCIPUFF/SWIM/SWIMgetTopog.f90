!==============================================================================
!  SWIM function returning terrain elevation and slopes at (X,Y)
!==============================================================================

MODULE SWIMgetTopog_fi

  USE SWIMmetField_fd
  USE SWIMinterp_fd
  USE SWIMparam_fd
  USE SWIMinterpPointer

  IMPLICIT NONE

  TYPE Topog
    REAL :: H, Hx, Hy
    REAL :: D
  END TYPE Topog

  CONTAINS

!------------------------------------------------------------------------------

  RECURSIVE FUNCTION SWIMgetTopog( grid,x,y ) RESULT( r )

  TYPE( Topog )                 :: r
  TYPE( MetGrid ), INTENT( IN ) :: grid
  REAL,            INTENT( IN ) :: x, y

  TYPE( met1dh ) :: mx, my
  TYPE( meth   ) :: mxy

  REAL    xfac, yfac
  INTEGER nxb, nyb, nxyb
  INTEGER i, ipx, ipy

  REAL, DIMENSION(:), POINTER :: H, Hx, Hy, D

  IF( BTEST(grid%type,GTB_TERRAIN) )THEN

!------ Define locals

    nxb  = grid%nX
    nyb  = grid%nY
    nxyb = grid%nXY

    H  => grid%terrain%H
    Hx => grid%terrain%Hx
    Hy => grid%terrain%Hy
    D  => grid%terrain%D

    xfac = 1.; yfac = 1. !Not used

!------ Interpolation factors

    CALL get_1dfac( x,grid%Xmin,grid%dX,nxb,xfac,mx )
    CALL get_1dfac( y,grid%Ymin,grid%dY,nyb,yfac,my )

    CALL SetMXY( mx,my,mxy,nxb,nxyb,.TRUE. )

!------ Interpolate terrain height and stretch factor

    CALL IntXY( mxy,H,r%H)
    CALL IntXY( mxy,D,r%D)

!------ Interpolate terrain gradients (defined at "staggered" locations)

    i   = mxy%ij
    ipx = (my%i-1)*nxb + MIN(mx%i+1,nxb)
    ipy = (MIN(my%i+1,nyb)-1)*nxb + mx%i

    r%Hx = my%rm1*Hx(i) + my%rat*Hx(ipy)
    r%Hy = mx%rm1*Hy(i) + mx%rat*Hy(ipx)

  ELSE

    r%H = 0.; r%Hx = 0.; r%Hy = 0.
    r%D = 1.

  END IF

  RETURN

  END FUNCTION SWIMgetTopog

END MODULE SWIMgetTopog_fi
