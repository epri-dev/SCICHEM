!*******************************************************************************
!                PopAreaField
!*******************************************************************************
RECURSIVE INTEGER FUNCTION PopAreaField( userID,grdI,Field,PlotType,contourHead,contourList )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN    ) :: userID       !USER ID tag
INTEGER,                                                            INTENT( IN    ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN    ) :: Field        !Field descriptor
TYPE( SCIPPlotTypeT ),                                              INTENT( IN    ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN    ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( INOUT ) :: contourList  !Contour array

!==============================================================================
! Local variables
!==============================================================================
TYPE( ARAPDrawT ) :: PopDraw
INTEGER           :: irv
INTEGER           :: currentState

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL  :: NullFill
INTEGER, EXTERNAL  :: NullDraw

INTERFACE
  SUBROUTINE DrawField( grdID,Field,PlotType,contourHead,contourList,GUIdraw,UserFill,UserDraw )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList !Contour array
    TYPE( ARAPDrawT ),                         INTENT( IN    ) :: GUIdraw      !Draw instructions
    INTEGER, EXTERNAL                                          :: UserFill     !Address of User supplied
                                                                               !fill routine passed by value
    INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                               !draw routine passed by value
  END SUBROUTINE DrawField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
PopAreaField = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==============================================================================
! Use DrawField to compute population/Area
!==============================================================================

IF( Aborted() )GOTO 9999

CALL init_error()

PopDraw%fillContour = SCIPfalse
PopDraw%drawContour = SCIPfalse
PopDraw%fill_Lo     = SCIPfalse
PopDraw%fill_Hi     = SCIPtrue
CALL DrawField( grdI,Field,PlotType,contourHead,contourList,PopDraw,NullFill,NullDraw )
IF( nError /= NO_ERROR )GOTO 9999

!==============================================================================
! Set return value
!==============================================================================
PopAreaField = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging( )

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
! CellPopArea - Calculate "expected" Population/Area of a cell
!*******************************************************************************
RECURSIVE INTEGER FUNCTION CellPopArea( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE PopArea_fi
USE param_fd
USE Tooluser_fd
USE error_fi
USE plotfunc_fi
USE sagpop_fi

IMPLICIT NONE

!===============================================================================
! Function Arguments
!===============================================================================
REAL, POINTER, DIMENSION(:)      :: dat    !Grid data array
INTEGER,              INTENT(IN) :: mxgrd  !Maximum grid size
TYPE ( SAGCELL_STR ), INTENT(IN) :: p0     !Cell data structure

!===============================================================================
! Local variables
!===============================================================================
REAL    cVar, cVar2                  !Cell mean(s)
REAL    cSig, cSig2                  !Cell variance(s)

REAL    prob                         !Cell probability of exceeding a value
REAL(8) answer

REAL, PARAMETER :: SMALL    = 1.E-30 !Small number for probability calculation
REAL, PARAMETER :: SKIP     = 1.E-6  !Small probability
REAL, PARAMETER :: NOT_SET  = -1.    !Initialization value

INTEGER i                            !Loop counter

!===============================================================================
! Functions
!===============================================================================
REAL,    EXTERNAL :: CellExceedValue                !Probability of exceeding a value
REAL(8), EXTERNAL :: PopAreaInCell                  !Interface to Population data

!===============================================================================
! Initialize
!===============================================================================

CellPopArea = SAG_ERROR

cVar = dat(p0%id)
cSig = dat(p0%id + mxgrd)
IF( PlotFunc_nComp == 2 )THEN
  cVar2 = dat(p0%id + 3*mxgrd)
  cSig2 = dat(p0%id + 4*mxgrd)
ELSE
  cVar2 = 0.0
  cSig2 = 0.0
END IF

prob = CellExceedValue( cVar,cSig,cVar2,cSig2,level(1),SMALL )
IF( prob >= SKIP )THEN
  answer = PopAreaInCell(.FALSE.,threadID,p0)
!$OMP ATOMIC UPDATE
  PopArea(1) = PopArea(1) + DBLE(prob)*answer
  DO i = 2,nlev
    prob = CellExceedValue( cVar,cSig,cVar2,cSig2,level(i),SMALL )
!$OMP ATOMIC UPDATE
    PopArea(i) = PopArea(i) + DBLE(prob)*answer
  END DO
END IF

!===============================================================================
! Return
!===============================================================================

CellPopArea = SAG_OK

9999 CONTINUE

RETURN
END
!******************************************************************************
! PolygonPopArea - Calculate "expected" Population/Area of a polygon
!******************************************************************************
RECURSIVE INTEGER FUNCTION PolygonPopArea( np,xp,yp,ilev )

USE sagdef_fd
USE PopArea_fi
USE param_fd
USE Tooluser_fd
USE error_fi
USE sagpop_fi

IMPLICIT NONE

!===============================================================================
! Function Arguments
!===============================================================================
INTEGER              , INTENT( IN    ) :: np      !Number of points in closed polygon
REAL(8),DIMENSION(np), INTENT( INOUT ) :: xp,yp   !Polygon nodes (grid coordinates)
INTEGER              , INTENT( INOUT ) :: ilev    !Level to associate area/population with

!===============================================================================
! Local variables
!===============================================================================
REAL(8) :: poly_PopArea    !area/population of polygon

!===============================================================================
! Functions
!===============================================================================
REAL(8), EXTERNAL :: PopAreaInPolygon

!===============================================================================
! Initialize
!===============================================================================
PolygonPopArea = SAG_ERROR

!===============================================================================
! ilev < 0 -> Set visibility : Here we set all polygons visible
!             0 = Not Visible
!             n = Visible
!===============================================================================
IF( ilev < 0 )THEN
  ilev = np

!===============================================================================
! nlev > 0 -> Compute
!===============================================================================
ELSE IF( ilev > 0 )THEN

  poly_PopArea = PopAreaInPolygon(threadID,np,xp,yp)

!===============================================================================
! Add to appropriate level
!===============================================================================
!$OMP ATOMIC UPDATE
  PopArea(ilev) = PopArea(ilev) + poly_PopArea

END IF

!===============================================================================
! Return
!===============================================================================
PolygonPopArea = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
! Null Population function
!*******************************************************************************
INTEGER FUNCTION NullPopArea()

USE sagdef_fd

IMPLICIT NONE

NullPopArea = SAG_ERROR

RETURN
END
!******************************************************************************
! CellExceedValue - clipped normal probability of exceeding a specified value
!******************************************************************************
RECURSIVE REAL FUNCTION CellExceedValue( v1,v2,vv1,vv2,val,small )

USE error_fi

IMPLICIT NONE

!===============================================================================
! Function Arguments
!===============================================================================
REAL, INTENT( IN ) :: v1    !Mean
REAL, INTENT( IN ) :: v2    !Variance
REAL, INTENT( IN ) :: vv1   !Mean - second cpt
REAL, INTENT( IN ) :: vv2   !Variance - second cpt
REAL, INTENT( IN ) :: val   !Exceed value
REAL, INTENT( IN ) :: small !small number = effective zerp

!===============================================================================
! Local variables
!===============================================================================
REAL sig    !Sigma
REAL ccoc   !Sigma/Mean
REAL gbar   !Gaussian Mean
REAL sigg   !Gaussian Sigma
REAL arg    !Error function argument
REAL sig1, sig2, x1, x2, test, p1, p2, ratPDF

LOGICAL PDF1, PDF2

REAL, PARAMETER :: ARGLIM = 1.E6        ! Error function argument limit
REAL, PARAMETER :: RATMIN = 0.001       ! Minimum sigC/C
REAL, PARAMETER :: SQRT2  = 1.414213562 ! SQRT(2.0)

REAL, EXTERNAL :: erfc   !Complementary error function
REAL, EXTERNAL :: ProbExceed2

IF( vv1 == 0.0 )THEN    !Single component pdf
!===============================================================================
! Compute if mean is significant
!===============================================================================
  IF( v1 > small )THEN

!===============================================================================
! Find Gaussian parameters from clipped normal values
!===============================================================================
    sig  = SQRT(MAX(v2,0.0))
    ccoc = MAX(sig/v1,RATMIN)
    CALL clnpar( ccoc,gbar,sigg )

!===============================================================================
! Compute error function argument
!===============================================================================
    arg = (val/v1 - gbar)/sigg

!===============================================================================
! Compute error function
!===============================================================================
    IF( arg > ARGLIM )THEN
      CellExceedValue = 0.
    ELSE IF(arg < -ARGLIM)THEN
      CellExceedValue = 1.
    ELSE
      CellExceedValue = 0.5*erfc( arg/SQRT2 )
    END IF

  ELSE
!===============================================================================
! Return zero if mean is not significant
!===============================================================================

    CellExceedValue = 0.

  END IF

ELSE     ! 2-cpt pdf

  x1 = v1 + vv1
  x2 = v2 + vv2

  sig1 = SQRT(v2)
  sig2 = SQRT(vv2)

  test = ABS(LOG(v1*sig2/(vv1*sig1)))
  IF( test <= 0.05 )THEN
    PDF1 = .TRUE.
    PDF2 = .FALSE.
  ELSE IF( test >= 0.5 )THEN
    PDF2 = .TRUE.
    PDF1 = .FALSE.
  ELSE
    PDF1 = .TRUE.
    PDF2 = .TRUE.
    ratPDF = (test-0.05)/0.45
  END IF

  IF( PDF1 )THEN
    sig = SQRT( MAX(x2,0.0) )
    IF( x1 > small )THEN
      ccoc = MAX(sig/x1,RATMIN)
      CALL clnpar( ccoc,gbar,sigg )
      arg = (val/x1 - gbar)/sigg
      IF( arg > 1.E6 )THEN
        p1 = 0.
      ELSE IF( arg < -1.E6 )THEN
        p1 = 1.
      ELSE
        p1 = 0.5*erfc( arg/SQRT2 )
      END IF
    ELSE
      p1 = 0.0
    END IF
  END IF

  IF( PDF2 )THEN
    p2 = ProbExceed2( v1,sig1,vv1,sig2,val )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( PDF1 )THEN
      CellExceedValue = MAX( (1.0-ratPDF)*p1 + ratPDF*p2,small )
    ELSE
      CellExceedValue = MAX( p2,small )
    END IF
  ELSE
    CellExceedValue = MAX( p1,small )
  END IF

END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                PopAreaInCell
!*******************************************************************************
RECURSIVE REAL(8) FUNCTION PopAreaInCell( fallout,threadID,p0 ) RESULT( returnValue )

!------ get Poplualtion or area of a Cell

USE param_fd
USE PopArea_fi
USE error_fi
USE sagcel_fd

IMPLICIT NONE

LOGICAL             , INTENT( IN ) :: fallout
INTEGER             , INTENT( IN ) :: threadID
TYPE ( SAGcell_str ), INTENT( IN ) :: p0

REAL(8) xp(4), yp(4), xpop, ypop, dx, dy
INTEGER np

!===============================================================================
! Functions
!===============================================================================
REAL(8), EXTERNAL :: PopAreaInPolygon         !Interface to population data

returnValue = 0.0D0

xpop = gridDef%xOrigin + gridDef%dx*DBLE(p0%x)
ypop = gridDef%yOrigin + gridDef%dy*DBLE(p0%y)
dx   = gridDef%dx*DBLE(p0%hx)
dy   = gridDef%dy*DBLE(p0%hy)

np = 4

xp(1) = xpop - 0.5*dx
xp(2) = xp(1)
xp(3) = xpop + 0.5*dx
xp(4) = xp(3)
yp(1) = ypop - 0.5*dy
yp(2) = ypop + 0.5*dy
yp(3) = yp(2)
yp(4) = yp(1)

returnValue = PopAreaInPolygon( threadID,np,xp,yp )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                PopAreaInPolygon
!*******************************************************************************
RECURSIVE REAL(8) FUNCTION PopAreaInPolygon( threadID,n,x,y ) RESULT( returnValue )

!------ Get population or ara in a polygon

USE param_fd
USE PopArea_fi
USE error_fi
USE SCIPresults_fd
USE constants_fd

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: threadID
INTEGER,                         INTENT( IN    ) :: n
REAL(8), DIMENSION(*),           INTENT( INOUT ) :: x, y

INTEGER irv

TYPE( GridDefinitionPopInCell ) :: tmpGridDef !Requires if multi threaded

!===============================================================================
! Functions
!===============================================================================
REAL(8), EXTERNAL :: AreaOfPolygon              !Area of a polygon

returnValue = -HUGE(0.0D0)

IF( griddef%doPop )THEN
  GOTO 9999
ELSE
  IF( gridDef%coord==I_LATLON)THEN
    tmpGridDef = gridDef
    tmpGridDef%reference%x   = 0.0D0
    tmpGridDef%reference%y   = 0.0D0
    tmpGridDef%reference%lon = SUM(x(1:n))/DBLE(n)
    tmpGridDef%reference%lat = SUM(y(1:n))/DBLE(n)
    tmpGridDef%yfac = 1000.D0*DBLE(SPHFACR)                            !(Km to Degrees)
    tmpGridDef%xfac = tmpGridDef%yfac/COSD(tmpGridDef%reference%lat)   !(Km to Degrees)
    CALL TransformToKM(tmpGridDef,n,x,y)
  END IF
  IF( nError /= 0 )GOTO 9999
  returnValue = AreaOfPolygon(x,y,n)
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
! AreaOfPolygon : compute area within a polygon
!******************************************************************************
RECURSIVE REAL(8) FUNCTION AreaOfPolygon( xp,yp,np ) RESULT(area)

IMPLICIT NONE

!===============================================================================
! Function Arguments
!===============================================================================
INTEGER              , INTENT( IN ) :: np     !Number of points in closed polygon
REAL(8),DIMENSION(np), INTENT( IN ) :: xp,yp  !Polygon nodes

!===============================================================================
! Local variables
!===============================================================================
INTEGER i       !Loop counter

!===============================================================================
! Initialize
!===============================================================================
area = 0.0d0

!===============================================================================
! Loop over nodes
!===============================================================================
DO i = 2,np-1
  area = area + ((xp(i)-xp(1))*(yp(i+1)-yp(1))-(yp(i)-yp(1))*(xp(i+1)-xp(1)))
END DO

!===============================================================================
! Set return value
!===============================================================================
area = ABS(0.5d0*area)

RETURN
END
!*******************************************************************************
!                TransformToKM
!*******************************************************************************
RECURSIVE SUBROUTINE TransformToKM( griddef,n,x,y )

!------ Transform (in place) x,y into cordinate system with KM units

USE param_fd
USE PopArea_fd

IMPLICIT NONE

TYPE( GridDefinitionPopInCell ), INTENT( IN    ) :: gridDef
INTEGER,                         INTENT( IN    ) :: n
REAL(8), DIMENSION(*),           INTENT( INOUT ) :: x, y

INTEGER i

SELECT CASE( gridDef%coord )

  CASE( I_LATLON )

    DO i = 1,n
      x(i) = gridDef%reference%x + (x(i)-gridDef%reference%lon)/gridDef%xfac
      y(i) = gridDef%reference%y + (y(i)-gridDef%reference%lat)/gridDef%yfac
    END DO

END SELECT

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                AreaInCell
!*******************************************************************************
RECURSIVE REAL(8) FUNCTION AreaInCell( p0 ) RESULT( returnValue )

!------ get area of a Cell

USE PopArea_fi
USE error_fi
USE sagcel_fd
USE constants_fd

IMPLICIT NONE

TYPE ( SAGcell_str ), INTENT( IN ) :: p0

REAL(8) xp(4), yp(4), xo, yo, dx, dy
INTEGER np

TYPE( GridDefinitionPopInCell ) :: tmpGridDef !Requires if multi threaded

!===============================================================================
! Functions
!===============================================================================
REAL(8), EXTERNAL :: AreaOfPolygon              !Area of a polygon

returnValue = 0.0D0

xo = gridDef%xOrigin + gridDef%dx*DBLE(p0%x)
yo = gridDef%yOrigin + gridDef%dy*DBLE(p0%y)
dx   = gridDef%dx*DBLE(p0%hx)
dy   = gridDef%dy*DBLE(p0%hy)

np = 4

xp(1) = xo - 0.5*dx
xp(2) = xp(1)
xp(3) = xo + 0.5*dx
xp(4) = xp(3)
yp(1) = yo - 0.5*dy
yp(2) = yo + 0.5*dy
yp(3) = yp(2)
yp(4) = yp(1)

IF( gridDef%coord==I_LATLON)THEN
  tmpGridDef = gridDef
  tmpGridDef%reference%x   = 0.0D0
  tmpGridDef%reference%y   = 0.0D0
  tmpGridDef%reference%lon = SUM(xp(1:np))/DBLE(np)
  tmpGridDef%reference%lat = SUM(yp(1:np))/DBLE(np)
  tmpGridDef%yfac = 1000.D0*DBLE(SPHFACR)                            !(Km to Degrees)
  tmpGridDef%xfac = tmpGridDef%yfac/COSD(tmpGridDef%reference%lat)   !(Km to Degrees)
  CALL TransformToKM(tmpGridDef,np,xp,yp)
END IF
IF( nError /= 0 )GOTO 9999

returnValue = AreaOfPolygon(xp,yp,np)

9999 CONTINUE

RETURN
END
