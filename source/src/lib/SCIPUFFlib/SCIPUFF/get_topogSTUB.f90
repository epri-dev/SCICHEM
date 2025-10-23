SUBROUTINE get_topogPrj( x,y,h,hx,hy )

!------ Find topography height, slope and stretch factor at (x,y) for 'best' field

USE met_fi

IMPLICIT NONE

REAL, INTENT( IN )  :: x, y
REAL, INTENT( OUT ) :: h, hx, hy

INTEGER ifld, i, irv
REAL    dx2, delx2, xmap, ymap
REAL    xp, yp

INTEGER, EXTERNAL :: SWIMcnvCoord
LOGICAL, EXTERNAL :: CheckHorizDomainPrj

ifld = 1

IF( numMet > 1 )THEN

  IF( PrjCoord%type /= MetGrid(ifld)%coord%type )THEN
    irv = SWIMcnvCoord( x,y,PrjCoord,xp,yp,MetGrid(ifld)%coord )
    CALL SWIMmapfac( MetGrid(ifld)%coord,xp,yp,xmap,ymap )
  ELSE
    xp = x; yp = y
    CALL SWIMmapfac( MetGrid(ifld)%coord,x,y,xmap,ymap )
  END IF

  dx2 = (MetGrid(1)%dx/xmap)**2 + (MetGrid(1)%dy/ymap)**2

  DO i = 2,numMet

    IF( PrjCoord%type /= MetGrid(i)%coord%type )THEN
      irv = SWIMcnvCoord( x,y,PrjCoord,xp,yp,MetGrid(i)%coord )
      CALL SWIMmapfac( MetGrid(i)%coord,xp,yp,xmap,ymap )
    ELSE
      xp = x; yp = y
      CALL SWIMmapfac( MetGrid(i)%coord,x,y,xmap,ymap )
    END IF

    IF( CheckHorizDomainPrj( MetGrid(i),xp,yp ) )THEN
      delx2 = (MetGrid(i)%dx/xmap)**2 + (MetGrid(i)%dy/ymap)**2
      IF( delx2 < dx2 )THEN
        ifld = i; dx2 = delx2
      END IF
    END IF

  END DO

END IF

CALL get_topogIn( x,y,h,hx,hy,ifld )

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckHorizDomainPrj( grid,x,y )

USE SWIMgridStr_fd

IMPLICIT NONE

TYPE( SWIMgridStr ), INTENT( IN ) :: grid
REAL,                INTENT( IN ) :: x, y

CheckHorizDomainPrj = x >= grid%xmin                          .AND. &
                      x <= grid%xmin+FLOAT(grid%nx-1)*grid%dx .AND. &
                      y >= grid%ymin                          .AND. &
                      y <= grid%ymin+FLOAT(grid%ny-1)*grid%dy

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE get_topogIn( x,y,h,hx,hy,ifld )

USE met_fi, ONLY: MetGrid,PrjCoord,hmin
USE constants_fd
USE coordinate_fd
USE metparam_fd
USE SWIMgridStr_fd
USE basic_fd

!------ Find topography height, slope and stretch factor at (x,y) for met field ifld

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x, y
REAL,    INTENT( OUT ) :: h, hx, hy
INTEGER, INTENT( IN  ) :: ifld

INTEGER ig, jg, i
REAL    rx ,ry
REAL    xp, yp, lat, lon, rot, c, s

TYPE( SWIMgridStr ), POINTER :: grd

TYPE( MapCoord ) coord

INTEGER, EXTERNAL :: SWIMcnvCoord
REAL,    EXTERNAL :: sind, cosd

NULLIFY(grd)

grd => MetGrid(MAX(ifld,1))

IF( grd%lter )THEN

  IF( PrjCoord%type /= grd%coord%type )THEN
    i = SWIMcnvCoord( x,y,PrjCoord,xp,yp,grd%coord )
    CALL frac( xp,grd%xmin,grd%dx,grd%nx,rx,ig )
    CALL frac( yp,grd%ymin,grd%dy,grd%ny,ry,jg )
  ELSE
    xp = x; yp = y
    CALL frac( x,grd%xmin,grd%dx,grd%nx,rx,ig )
    CALL frac( y,grd%ymin,grd%dy,grd%ny,ry,jg )
  END IF

  i = (jg-1)*grd%nx + ig

  h = (1.-rx)*(ry*grd%H(i+grd%nx  ) + (1.-ry)*grd%H(i  )) + &
          rx *(ry*grd%H(i+grd%nx+1) + (1.-ry)*grd%H(i+1)) + &
          grd%Hmin - hmin

  hx = (1.-ry)*grd%Hx(i) + ry*grd%Hx(i+grd%nx)
  hy = (1.-rx)*grd%Hy(i) + rx*grd%Hy(i+1)

!------ Rotate into project into E-N components (only for special map projections)

  SELECT CASE( grd%coord%type )
    CASE( I_LAMBERT,I_POLAR,I_RPOLAR,I_ROTLL )

      IF( PrjCoord%type == I_LATLON )THEN
        lon = x; lat = y
      ELSE IF( grd%coord%type == I_ROTLL )THEN
        lon = xp; lat = yp
      ELSE
        coord%type = I_LATLON
        i = SWIMcnvCoord( x,y,PrjCoord,lon,lat,coord )
      END IF

      IF( grd%coord%type == I_RPOLAR )THEN
        rot = SIND(lat)
        c   = 1. + rot**2
        s   = (c*grd%coord%sp0 + grd%coord%cp0**2*rot)*SIND(lon-grd%coord%Lon0)
        c   = c*COSD(lon-grd%coord%Lon0) + grd%coord%cp0*COSD(lat)
        rot = ATAN2(s,c)
      ELSE IF( grd%coord%type == I_ROTLL )THEN
        c   = grd%coord%cp0*COSD(lat) - grd%coord%sp0*SIND(lat)*COSD(lon) !N.B. lat,lon on rotated sphere
        s   = grd%coord%sp0*SIND(lon)
        rot = ATAN2(s,c)
      ELSE
        rot = (lon - grd%coord%Lon0)*PI180 * grd%coord%n
      END IF

      IF( ABS(rot) > 0.001 ) THEN
        c  = COS(rot)
        s  = SIN(rot)
        rx =  c*hx + s*hy
        ry = -s*hx + c*hy
        hx = rx
        hy = ry
      END IF

  END SELECT

ELSE

  h = 0.; hx = 0.; hy = 0.

END IF

NULLIFY(grd)

RETURN
END
