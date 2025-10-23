!--------------------------------------------------------------------------
SUBROUTINE draw_source( plotTime,showText )

USE files_fi
USE default_fd
USE GUItool_fi
USE GUImatl_fi
USE create_fi
USE pcscipuf_fi
USE SwinAPI
USE contri_fi
USE contri_D

IMPLICIT NONE

REAL,    INTENT( IN ) :: plotTime
LOGICAL, INTENT( IN ) :: showText

TYPE ( T_RECT ) MyRect
TYPE ( T_POINT ) MyPt

REAL ssize,dtx

INTEGER i
REAL    tdur,urel,vrel
REAL    x,y,xwid,xhgt,xoff,yoff,fszx
LOGICAL lin

CHARACTER(52) text

LOGICAL, EXTERNAL :: CheckPoint,hasError

CALL set_domain( MyRect,ssize )

CALL gsplci( 1 )
CALL gslwsc( 1.0 )

CALL SizeRelList( scenario(BASE_LEVEL) )
IF( hasError() )GOTO 9999
CALL GUI_SCIP_scenario( project(BASE_LEVEL),scenario(BASE_LEVEL), &
                        materials(BASE_LEVEL),reldef,relList,SIZE(relList),relMCList,SIZE(relMCList) )
IF( hasError() )GOTO 9999

IF( showText )THEN
  IF( lprint )THEN
    fszx = 0.375*fsz
  ELSE
    fszx = 0.5*fsz
  END IF

  text = 'M'
  x = 0.5*(MyRect%left+MyRect%right)/Dscale
  y = 0.5*(MyRect%top+MyRect%bottom)/Dscale
  CALL GetNCARFontSize( x,y,text(1:1),fszx,0.,-1.,xwid,xhgt )
  xoff =  0.0
  yoff = -1.2*xhgt
END IF

DO i = 1,reldef%scnHead%number

  IF( relList(i)%tRel <= plotTime )THEN
    lin = relList(i)%xRel /= DEFERRED_D .AND. relList(i)%yRel /= DEFERRED_D
    lin = lin .AND. relList(i)%xRel /= DEF_VAL_D .AND. relList(i)%yRel /= DEF_VAL_D
    lin = lin .AND. relList(i)%xRel /= NOT_SET_D .AND. relList(i)%yRel /= NOT_SET_D
    lin = lin .AND. CheckPoint( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),MyPt,Dscale,MyRect )
    IF( lin )THEN
      SELECT CASE( relList(i)%type )
        CASE( HR_INST )
          CALL draw_inst( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),ssize )

        CASE( HR_FILE )
          CALL draw_inst( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),ssize )

        CASE( HR_CONT )
          CALL draw_cont( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),ssize )

        CASE( HR_MOVE )
          CALL GetMovingParms( relList(i)%relData,tdur,urel,vrel )
          dtx = MIN(tdur,(plotTime-relList(i)%tRel))*3600.
          CALL draw_move( relList(i)%xRel,relList(i)%yRel,urel,vrel,dtx,ssize )

        CASE( HR_STACK,HR_STACK3,HR_PRIME,HR_STACKF,HR_STACKFP,HR_AREA,HR_AREAF )
          CALL draw_cont( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),ssize )

        CASE( HR_POOL )
          CALL draw_cont( SNGL(relList(i)%xRel),SNGL(relList(i)%yRel),ssize )

        CASE DEFAULT

      END SELECT

      IF( showText .AND. LEN_TRIM(relList(i)%relDisplay) > 0 )THEN
        x = SNGL(relList(i)%xRel) + xoff
        y = SNGL(relList(i)%yRel) + yoff
        text = TRIM(relList(i)%relDisplay)
        CALL plchhq( x,y,text,fszx,0.,0. )
      END IF

    END IF
  END IF

END DO

9999 CONTINUE

CALL DeallocateRelList()

CALL gslwsc( 0.36 )

RETURN
END

!===============================================================================

SUBROUTINE GetMovingParms( relData,tdur,urel,vrel )

USE Tooluser_fd

IMPLICIT NONE

TYPE( relMoveT ), INTENT( IN  ) :: relData
REAL,             INTENT( OUT ) :: tdur, urel, vrel

tdur = relData%duration
urel = relData%velX
vrel = relData%velY

RETURN
END

!===============================================================================

SUBROUTINE draw_inst( x0,y0,size )

IMPLICIT NONE

REAL, INTENT( IN ) :: x0, y0, size

INTEGER n, irv
REAL    x, y

REAL, DIMENSION(4) :: xr, yr

INTEGER, EXTERNAL :: PlotTransform

irv = PlotTransform( 1,x0,y0,x,y )

xr(1) = x - 0.5*size
xr(2) = x + 0.5*size
xr(3) = x
xr(4) = xr(1)

yr(1) = y - 0.33333*size
yr(2) = yr(1)
yr(3) = y + 0.66667*size
yr(4) = yr(1)

n = 4
CALL gpl( n,xr,yr )

RETURN
END

!===============================================================================

SUBROUTINE draw_cont( x0,y0,size )

IMPLICIT NONE

REAL, INTENT( IN ) :: x0, y0, size

INTEGER n, irv
REAL    x, y

REAL, DIMENSION(4) :: xr, yr

INTEGER, EXTERNAL :: PlotTransform

irv = PlotTransform( 1,x0,y0,x,y )

xr(1) = x - 0.5*size
xr(2) = x + 0.5*size
xr(3) = x
xr(4) = xr(1)

yr(1) = y + 0.33333*size
yr(2) = yr(1)
yr(3) = y - 0.66667*size
yr(4) = yr(1)

n = 4
CALL gpl( n,xr,yr )

RETURN
END

!===============================================================================

SUBROUTINE draw_move( x0,y0,u,v,dt,size )

IMPLICIT NONE

REAL, INTENT( IN ) :: x0, y0, u, v, dt, size

INTEGER n, irv
REAL    x, y, xmap, ymap

REAL, DIMENSION(2) :: xr, yr

INTEGER, EXTERNAL :: PlotTransform

CALL GUImapfac( x0,y0,xmap,ymap )

x = x0 + u*dt*xmap
y = y0 + v*dt*ymap

irv = PlotTransform( 1,x0,y0,xr(1),yr(1) )
irv = PlotTransform( 1,x ,y ,xr(2),yr(2) )

n = 2
CALL gpl( n,xr,yr )

CALL draw_cont( x0,y0,size )

RETURN
END

!===============================================================================

SUBROUTINE draw_sfc( x0,y0,size,xp0,yp0 )

IMPLICIT NONE

REAL, INTENT( IN ) :: x0, y0, size, xp0, yp0

INTEGER n, irv
REAL    x, y

REAL, DIMENSION(5) :: xr, yr

INTEGER, EXTERNAL :: PlotTransform

irv = PlotTransform( 1,x0,y0,x,y )

xr(1) = x - 0.5*size
xr(2) = x + 0.5*size
xr(3) = xr(2)
xr(4) = xr(1)
xr(5) = xr(1)

yr(1) = y - 0.5*size
yr(2) = yr(1)
yr(3) = y + 0.5*size
yr(4) = yr(3)
yr(5) = yr(1)

n = 5
CALL gpl( n,xr,yr )

irv = PlotTransform( 1,x0,y0,xp0,yp0 )

RETURN
END

!===============================================================================

SUBROUTINE set_domain( MyRect,size )

USE contri_fi
USE ToolUser_fd
USE contri_D

IMPLICIT NONE

TYPE  T_RECT
  SEQUENCE
  INTEGER left,top,right,bottom
END TYPE  T_RECT

REAL size
TYPE( T_RECT ) MyRect

REAL    xdum,xhgt
CHARACTER(2) wx

REAL, DIMENSION(2) :: x, y, xp, yp
INTEGER irv

INTEGER, EXTERNAL :: real2integer
INTEGER, EXTERNAL :: DataTransform

xp(1) = xmnd
yp(1) = ymnd

xp(2) = xmxd
yp(2) = ymxd

irv = DataTransform( 2,xp,yp,x,y )

Dscale = 10.**(8-INT(LOG10(MAX(ABS(x(1)),ABS(x(2)),ABS(y(1)),ABS(y(2))))))

MyRect%left   = real2integer( Dscale*(x(1)-0.01*(x(2)-x(1))) )
MyRect%right  = real2integer( Dscale*(x(2)+0.01*(x(2)-x(1))) ) + 1
MyRect%top    = real2integer( Dscale*(y(1)-0.01*(y(2)-y(1))) )
MyRect%bottom = real2integer( Dscale*(y(2)+0.01*(y(2)-y(1))) ) + 1

wx = 'Mg'
CALL GetNCARFontSize( 0.,0.,wx,1.,0.,0.,xdum,xhgt )
size = 0.531*xhgt*fsz

RETURN
END

!===============================================================================

SUBROUTINE draw_weather( ptime )

USE files_fi
USE errorParam_fd
USE contri_fi
USE SwinAPI
USE contri_D

IMPLICIT NONE

INTEGER, PARAMETER :: WXS_SURF = 1
INTEGER, PARAMETER :: WXS_PROF = 2

REAL, INTENT( IN ) :: ptime

TYPE( T_RECT ) MyRect
TYPE( T_POINT ) MyPt

INTEGER ios, i
REAL    timx, ssize, xx, yy, xxp, yyp

INTEGER     nwx
REAL        xwx(1000),ywx(1000)
CHARACTER(4) wx
NAMELIST / wxsloc / wx, nwx, xwx, ywx

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: CheckPoint

nError  = NO_ERROR
eAction = ' '

CALL set_domain( MyRect,ssize )

OPEN(UNIT=lun_log,FILE=file_log,STATUS='old',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError  = OP_ERROR
  eRoutine = 'DrawWeather'
  eMessage = 'Unable to open Log file for Weather station reading'
  eInform  = TRIM(file_log)
  GOTO 9999
END IF

CALL gsplci( 1 )
CALL gslwsc( 1.0 )

timx = ptime*3600.

DO
  READ(lun_log,wxsloc,IOSTAT=ios)
  IF( ios > 0 )THEN
    nError   = RD_ERROR
    eMessage = 'Error reading WXSLOC namelist'
    GOTO 9999
  ELSE IF( ios < 0 )THEN
    EXIT
  END IF
  CALL cupper( wx )

  DO i = 1,nwx
    xx = xwx(i)
    yy = ywx(i)
    IF( CheckPoint(xx,yy,MyPt,Dscale,MyRect) )THEN
      CALL draw_sfc( xx,yy,ssize,xxp,yyp )
      yyp = yyp - 0.375*ssize
      CALL plchhq( xxp,yyp,wx(1:1),0.66*fsz,0.,0. )
    END IF
  END DO
END DO

9999 CONTINUE

CLOSE(lun_log,IOSTAT=ios)
CALL gslwsc( 0.36 )

RETURN
END

!===============================================================================

SUBROUTINE GUImapfac( x,y,xmap,ymap )

USE PlotTrans_fi
USE coordinate_fd

!      Horizontal coordinate transform function
!               (x,y) are puff coordinates
!               If dx is in meters then  dx*xmap is in puff coords

IMPLICIT NONE

REAL, INTENT( IN )  :: x, y
REAL, INTENT( OUT ) :: xmap, ymap

REAL ytem
REAL pi, pi180, sphfac, sphfacr

pi      = 4.*ATAN(1.)
pi180   = pi/180.
sphfac  = pi180*6371.2E3 ! deg-to-rad conversion / earths's radius
sphfacr = 1.0/sphfac

SELECT CASE( ABS(DataCoordinate%Mode) )

  CASE( I_LATLON )

     ytem = MIN( ABS(y),85.0 )

     xmap = sphfacr/COS(ytem*pi180)
     ymap = sphfacr

  CASE( I_METERS )

     xmap = 1.
     ymap = 1.

  CASE DEFAULT ! (I_CARTESIAN)

     xmap = 1.0E-3
     ymap = 1.0E-3

END SELECT

RETURN
END
