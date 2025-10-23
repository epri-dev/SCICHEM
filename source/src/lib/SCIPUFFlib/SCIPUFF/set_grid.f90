SUBROUTINE set_grid()

USE scipuff_fi
USE met_fi
USE files_fi
USE SWIMparam_fd

!------ setup calculation grid variables
!
!------ assumes uniform x and y grids

IMPLICIT NONE

LOGICAL lgrd, lgrdx, lgrdy
REAL    dxx, dyy, hresx, hresy, zmin, hmax
REAL    xmin1, xmax1, ymin1, ymax1, dxb, dyb, xmap, ymap
REAL    xmin0, xmax0, xlen0, xlen
INTEGER ios, nxb, nyb

INTEGER, EXTERNAL :: max_vres
LOGICAL, EXTERNAL :: CheckDomainOverlap
REAL,    EXTERNAL :: SWIMgetHmax

!------ Setup project coordinate structure

CALL SetPrjCoord()

!------ Set met grid locals

xmin1 = MetGrid(1)%xmin; nxb = MetGrid(1)%nx; dxb = MetGrid(1)%dx
ymin1 = MetGrid(1)%ymin; nyb = MetGrid(1)%ny; dyb = MetGrid(1)%dy

xmax1 = xmin1 + FLOAT(nxb-1)*dxb
ymax1 = ymin1 + FLOAT(nyb-1)*dyb

IF( MetGrid(1)%coord%type /= PrjCoord%type ) &
  CALL GetDomainPrjExtents( MetGrid(1)%coord,xmin1,xmax1,ymin1,ymax1,dxb,dyb,nxb,nyb )

!------ Set default calculation domain

IF( xmin == DEF_VAL_R )xmin = xmin1
IF( ymin == DEF_VAL_R )ymin = ymin1
IF( xmax == DEF_VAL_R )xmax = xmax1
IF( ymax == DEF_VAL_R )ymax = ymax1

IF( xmin == DEF_VAL_R .OR. xmax == DEF_VAL_R .OR. &
    ymin == DEF_VAL_R .OR. ymax == DEF_VAL_R )THEN
  nError   = UK_ERROR
  eRoutine = 'set_grid'
  eMessage = 'Must set calculation domain'
  CALL ReportFileName( eInform,'File=',file_inp )
  GOTO 9999
END IF

!------ check puff and met background grids

IF( nxb > 1 .AND. nyb > 1 )THEN

  lgrd = CheckDomainOverlap( xmin1,xmax1,ymin1,ymax1,dxb,dyb,lgrdx,lgrdy )

  IF( lgrdx .AND. (lmap == I_LATLON .AND. (xmax1-xmin1) >= 359.9) )THEN
    nError   = DM_ERROR
    eRoutine = 'set_grid:Domain mismatch'
    eMessage = 'Do you want SCIPUFF to use global met longitude range?'
    WRITE(eInform,'(A,2F10.2)') 'Met Domain     : ',xmin1,xmax1
    WRITE(eAction,'(A,2F10.2)') 'Project Domain : ',xmin,xmax
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )THEN
      nError = WN_ERROR
      GOTO 9999
    ELSE
      xmin = xmin1
      xmax = xmax1
      lgrd = lgrdy
    END IF
  END IF

  IF( lgrd )THEN
    nError   = DM_ERROR
    eRoutine = 'set_grid:Domain mismatch'
    eMessage = 'Do you want SCIPUFF to adjust the project domain and continue?'
    WRITE(eInform,'(A,4F10.2)') 'Met Domain     : ',xmin1,xmax1,ymin1,ymax1
    WRITE(eAction,'(A,4F10.2)') 'Project Domain : ',xmin,xmax,ymin,ymax
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )THEN
      nError = WN_ERROR
      GOTO 9999
    ELSE
      IF( lgrdx )THEN
        xmin0 = xmin
        xmax0 = xmax
        xlen0 = xmax0 - xmin0
        xmin  = MAX(xmin,xmin1-dxb*0.999)
        xmax  = MIN(xmax,xmax1+dxb*0.999)
        xlen  = xmax - xmin
        IF( PrjCoord%type == I_LATLON .AND. xlen < 0.9*xlen0 )THEN
          eRoutine = 'set_grid:Domain X-mismatch'
          eMessage = 'West-East extent greatly reduced. Use the Met domain and continue?'
          WRITE(eInform,'(A,2F10.2)') 'Met Domain      : ',xmin1,xmax1
          WRITE(eAction,'(A,2F10.2)') 'Adjusted Domain : ',xmin,xmax
          CALL WarningMessage( .TRUE. )
          IF( nError == NO_ERROR )THEN
            xmin = xmin1
            xmax = xmax1
            nError = NO_ERROR
          END IF
        END IF
      END IF
      IF( lgrdy )THEN
        ymin = MAX(ymin,ymin1-dyb*0.999)
        ymax = MIN(ymax,ymax1+dyb*0.999)
        IF( lmap == I_LATLON )THEN
          ymin = MAX(ymin,-POLARCAP_LAT)
          ymax = MIN(ymax, POLARCAP_LAT)
        END IF
      END IF
      IF( xmin > xmax .OR. ymin > ymax )THEN
        nError   = DM_ERROR
        eMessage = 'Project domain is outside the met/terrain grid'
        GOTO 9999
      END IF
    END IF
  END IF

END IF

!------ Initialize multicomponent chemistry background

IF( multicomp )THEN
  CALL CheckAmbDomain()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Check for global calculation (longitude) & polar cap

IF( lmap == I_LATLON .AND. (xmax-xmin) >= 359.9 )THEN

  xmin = xmin1
  xmax = xmin + 360.

  global_lon = .TRUE.

  IF( ymax >= POLARCAP_LAT-0.1 )THEN
    ymax = POLARCAP_LAT
    polarcap_n = .TRUE.
  ELSE
    polarcap_n = .FALSE.
  END IF
  IF( ymin <= -POLARCAP_LAT+0.1 )THEN
    ymin = -POLARCAP_LAT
    polarcap_s = .TRUE.
  ELSE
    polarcap_s = .FALSE.
  END IF

  IF( polarcap_s .OR. polarcap_n )THEN
    CALL CheckGlobalMet()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

ELSE

  global_lon = .FALSE.
  polarcap_n = .FALSE.
  polarcap_s = .FALSE.

END IF

!------ Get max terrain (relative to hmin); add to zmax

hmax = SWIMgetHmax()
zmax = zmax + hmax

zmax = CEILING(zmax/vres)*vres

!------ Vertical resolution

zmin = 0.
nz   = MIN(MAX(NINT(zmax/vres),1),max_vres())
dzg  = zmax/FLOAT(nz)

!------ Horizontal

IF( hres == DEF_VAL_R )THEN
  IF( nxb == 1 )THEN
    dxx = xmax - xmin
  ELSE
    dxx = dxb
  END IF
  IF( nyb == 1 )THEN
    dyy = ymax - ymin
  ELSE
    dyy = dyb
  END IF
  hresx = dxx
  hresy = dyy
ELSE
  hresx = hres
  hresy = hres
END IF

hresx = hresx*FLOAT(2**ABS(mgrd))
hresy = hresy*FLOAT(2**ABS(mgrd))

IF( xmax <= xmin )xmax = xmin + hresx
IF( ymax <= ymin )ymax = ymin + hresy

nx  = MAX(NINT((xmax-xmin)/hresx + 0.499999),1)
dxg = (xmax-xmin)/FLOAT(nx)

ny  = MAX(NINT((ymax-ymin)/hresy + 0.499999),1)
dyg = (ymax-ymin)/FLOAT(ny)

CALL mapfac( 0.5*(xmax+xmin),0.5*(ymax+ymin),xmap,ymap )

IF( nx == 1 .AND. ny == 1 )THEN
  delx2 = (1.E6*MAX(dxg/xmap,dyg/ymap))**2
ELSE
  delx2 = (dxsplt*MIN(dxg/xmap,dyg/ymap))**2
END IF
delz2 = (dzsplt*dzg)**2

MetGrid(1)%delx2 = delx2

lMetGridComplete(1) = .TRUE.

!------ write out puff grid

WRITE(lun_log,IOSTAT=ios,FMT="(/,A)") 'Computational domain'
WRITE(lun_log,IOSTAT=ios,FMT="('xmin,xmax,dx: ',3ES12.4)  ") xmin,xmax,dxg
WRITE(lun_log,IOSTAT=ios,FMT="('ymin,ymax,dy: ',3ES12.4)  ") ymin,ymax,dyg
WRITE(lun_log,IOSTAT=ios,FMT="('zmin,zmax,dz: ',3ES12.4,/)") zmin,zmax,dzg
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'set_grid'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

!------ Reset puff grid for IPGRD

nx  = MIN(nx,2)
dxg = (xmax-xmin)/FLOAT(nx)

ny  = MIN(ny,2)
dyg = (ymax-ymin)/FLOAT(ny)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE GetDomainPrjExtents( coord,x1,x2,y1,y2,dx,dy,nxb,nyb )

USE met_fi

IMPLICIT NONE

TYPE( MapCoord ), INTENT( IN    ) :: coord
REAL,             INTENT( INOUT ) :: x1, x2, y1, y2, dx, dy
INTEGER,          INTENT( IN    ) :: nxb, nyb

INTEGER irv, i
REAL    x0, y0, xn, yn, xtem, ytem, tem1, tem2, xmap, ymap

INTEGER, EXTERNAL :: SWIMcnvCoord

x0 = x1; y0 = y1
xn = x2; yn = y2

x1 = -HUGE(0.); x2 = HUGE(0.)
y1 = -HUGE(0.); y2 = HUGE(0.)

DO i = 1,nyb
  ytem = y0 + FLOAT(i-1)*dy
  irv  = SWIMcnvCoord( x0,ytem,coord,xtem,tem1,PrjCoord )
  x1   = MAX(x1,xtem)
  irv  = SWIMcnvCoord( xn,ytem,coord,xtem,tem1,PrjCoord )
  x2   = MIN(x2,xtem)
END DO
DO i = 1,nxb
  xtem = x0 + FLOAT(i-1)*dx
  irv  = SWIMcnvCoord( xtem,y0,coord,tem1,ytem,PrjCoord )
  y1   = MAX(y1,ytem)
  irv  = SWIMcnvCoord( xtem,yn,coord,tem1,ytem,PrjCoord )
  y2   = MIN(y2,ytem)
END DO

xtem = 0.5*(x0+xn)
ytem = 0.5*(y0+yn)

irv = SWIMcnvCoord( xtem,ytem,coord,tem1,tem2,PrjCoord )

CALL mapfac( tem1,tem2,xmap,ymap )
CALL SWIMmapfac( coord,xtem,ytem,tem1,tem2 )

dx = dx*xmap/tem1
dy = dy*ymap/tem2

RETURN
END

!==============================================================================

LOGICAL FUNCTION PointInBox( x,y,x1,x2,y1,y2 )

IMPLICIT NONE

REAL, INTENT( IN ) :: x, y           !Point being checked
REAL, INTENT( IN ) :: x1, x2, y1, y2 !Bounding rectangle (min/max x, min/max y)

PointInBox = (x >= x1) .AND. (x <= x2) .AND. &
             (y >= y1) .AND. (y <= y2)

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckDomainOverlap( xmin1,xmax1,ymin1,ymax1,dxb,dyb,lgrdx,lgrdy ) RESULT( lgrd )

!------ Check overlap of project domain [(xmin,ymin) (xmax,ymax)] with (met) domain defined by
!       [(xmin1,ynin1) (xmax1,ymax1)]

USE scipuff_fi

IMPLICIT NONE

REAL,    INTENT( IN  ) :: xmin1, xmax1, ymin1, ymax1  !Met domain extents (in project coordinates)
REAL,    INTENT( IN  ) :: dxb, dyb                    !Met domain grid spacing
LOGICAL, INTENT( OUT ) :: lgrdx, lgrdy                !TRUE if project domain extends beyond
                                                      !met domain in x- or y-directions

!------ Check if project domain extends outside met domain in either direction

lgrdx = (xmin < xmin1-dxb) .OR. (xmax > xmax1+dxb)
lgrdy = (ymin < ymin1-dyb) .OR. (ymax > ymax1+dyb)

!------ Special checks for longitude

IF( lgrdx .AND. lmap == I_LATLON )THEN
  IF( xmax-xmin >= 359.9  .AND. xmax1-xmin1 >= 359.9 )THEN
    lgrdx = .FALSE.                  !Project and Met are global
    xmax  = xmin + 360.
  ELSE IF( xmax-xmin >= 359.9 )THEN
    xmin = xmin1                     !Project is global; reset project western boundary to match Met
    xmax = xmin + 360.
    lgrdx = (xmin < xmin1-dxb) .OR. (xmax > xmax1+dxb)
  ELSE IF( xmin > xmax1 )THEN
    IF( xmax-360. > xmin1 )THEN      !Complete miss; check if subtracting 360 helps
      xmin = xmin - 360.
      xmax = xmax - 360.
      lgrdx = (xmin < xmin1-dxb) .OR. (xmax > xmax1+dxb)
    END IF
  ELSE IF( xmax < xmin1 )THEN
    IF( xmin+360. < xmax1 )THEN
      xmin = xmin + 360.             !Complete miss; check if adding 360 helps
      xmax = xmax + 360.
      lgrdx = (xmin < xmin1-dxb) .OR. (xmax > xmax1+dxb)
    END IF
  END IF
END IF

lgrd = lgrdx .OR. lgrdy

RETURN
END
