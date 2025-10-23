!==============================================================================
! CreateGrid
!==============================================================================
SUBROUTINE CreateGrid()

USE Extract_fi

IMPLICIT NONE

SELECT CASE( GridType )
  CASE( LINE1D )
    CALL CreateLine()
    IF( nError/= NO_ERROR )GOTO 9999
    CALL allocateGrid( nxy )
    IF( nError/= NO_ERROR )GOTO 9999
    CALL BuildLine()
  CASE( AUTO )
    CALL CreateGridAuto()
    IF( nError/= NO_ERROR )GOTO 9999
    CALL allocateGrid( nxy )
    IF( nError/= NO_ERROR )GOTO 9999
    CALL BuildGrid()
  CASE( CUSTOM )
    CALL CreateGridCustom()
  CASE( NATIVE )

  CASE( UNIFORM )
    CALL CreateGridUniform()
    IF( nError/= NO_ERROR )GOTO 9999
    CALL allocateGrid( nxy )
    IF( nError/= NO_ERROR )GOTO 9999
    CALL BuildGrid()
  CASE DEFAULT
    nError   = UK_ERROR
    eMessage = 'Unrecognized Grid Extraction Type'
END SELECT
IF( nError/= NO_ERROR )GOTO 9999

IF( GridType /= NATIVE )THEN
  IF( output3D )THEN
    CALL allocateFldGrd( nxy,nFields )
  ELSE
    CALL allocateFldGrd( nxy,2 )
  END IF
  IF( nError/= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateLine
!==============================================================================
SUBROUTINE CreateLine()

USE Extract_fi

IMPLICIT NONE

SELECT CASE( LineType )
  CASE( HORZ_LINE )
    CALL CreateLineHorizontal()
  CASE( VERT_LINE )
    CALL CreateLineProfile()
  CASE DEFAULT
    nError   = UK_ERROR
    eMessage = 'Unrecognized Line Extraction Type'
END SELECT
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateLineHorizontal
!==============================================================================
SUBROUTINE CreateLineHorizontal()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER ios, itry, irv
INTEGER nx0,ny0

REAL x0,y0,dx0,dy0, xStart, xEnd, yStart, yEnd
REAL, DIMENSION(2) :: x

CHARACTER(128) string
LOGICAL defX, defY

INTEGER, EXTERNAL :: getInput

defX = .FALSE.
defY = .FALSE.

xStart = ClassData(CD_XMIN)
xEnd   = ClassData(CD_XMAX)
yStart = ClassData(CD_YMIN)
yEnd   = ClassData(CD_YMAX)

IF( xStart == DEF_VAL_R )defX = .TRUE.
IF( xEnd   == DEF_VAL_R )defX = .TRUE.
IF( yStart == DEF_VAL_R )defY = .TRUE.
IF( yEnd   == DEF_VAL_R )defY = .TRUE.

IF( defX .OR. defY )THEN
  irv = SCIPGetFieldDomain( callerID,FieldIds(1),nx0,ny0,x0,y0,dx0,dy0 )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error getting Field domain' )
    GO TO 9999
  END IF
  IF( xStart == DEF_VAL_R )xStart = x0
  IF( xEnd   == DEF_VAL_R )xEnd   = x0 + FLOAT(nx0)*dx0
  IF( yStart == DEF_VAL_R )yStart = y0
  IF( yEnd   == DEF_VAL_R )yEnd   = y0 + FLOAT(ny0)*dy0
END IF

WRITE(6,'(A,2G16.7)')'Slice start :',xStart,yStart
WRITE(6,'(A,2G16.7,/)')'Slice end   :',xEnd,yEnd

defX = .FALSE.
defY = .FALSE.

!== Start Point

ios = 1
itry = 0
string = 'Line Start Point'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,2,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defX = .TRUE.
  ELSE IF( ios == 0 )THEN
    Xmin = x(1)
    Ymin = x(2)
  END IF
END DO

!== End Point

ios  = 1
itry = 0
string = 'Line End   Point'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,2,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defY = .TRUE.
  ELSE IF( ios == 0 )THEN
    Xmax = x(1)
    Ymax = x(2)
  END IF
END DO

!== No. line Points

ios  = 1
itry = 0
string = 'No. Line  Points'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    nx = 51
  ELSE IF( ios == 0 )THEN
    nx = NINT(x(1))
  END IF
END DO

IF( defX .OR. defY )THEN
  IF( defX )THEN
    Xmin = xStart
    Ymin = yStart
  END IF
  IF( defY )THEN
    Xmax = xEnd
    Ymax = yEnd
  END IF
END IF

IF( nx <= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'CreateLineHorizontal'
  WRITE(eInform,'(A,I0)')'Invalid number of line points: ',nx
  GO TO 9999
END IF
ny = 0

WRITE(6,'(/,"Xstr,Ystr = ",2ES10.3,I4)')Xmin,Ymin
WRITE(6,'(  "Xend,Yend = ",2ES10.3,I4)')Xmax,Ymax

nxy = nx

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateLineProfile
!==============================================================================
SUBROUTINE CreateLineProfile()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER ios, itry, irv
INTEGER nx0,ny0

REAL x0,y0,dx0,dy0, xStart, xEnd, yStart, yEnd
REAL, DIMENSION(2) :: x

CHARACTER(128) string
LOGICAL defX, defN

INTEGER, EXTERNAL :: getInput

defX = .FALSE.
defN = .FALSE.

xStart = Fields(1)%coordinate%vertSlice%startPt%x
yStart = Fields(1)%coordinate%vertSlice%startPt%y
xEnd   = Fields(1)%coordinate%vertSlice%endPt%x
yEnd   = Fields(1)%coordinate%vertSlice%endPt%y

WRITE(6,'(A,2G16.7)')'Slice start (0.0):',xStart,yStart
WRITE(6,'(A,2G16.7,/)')'Slice end   (1.0):',xEnd,yEnd

!== Profile Location

ios  = 1
itry = 0
string = 'Profile Location [0.0-1.0]'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defX = .TRUE.
  ELSE IF( ios == 0 )THEN
    Xmin = MIN(MAX(0.0,x(1)),1.0)
  END IF
END DO

!== No. Profile Points

ios  = 1
itry = 0
string = 'No. Profile Pts.'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defN = .TRUE.
  ELSE IF( ios == 0 )THEN
    ny = NINT(x(1))
  END IF
END DO

IF( defX )THEN
  linePos = 0.5
ELSE
  linePos = Xmin
END IF
Xmin = xStart + linePos*(xEnd-xStart)
Xmax = yStart + linePos*(yEnd-yStart)

irv = SCIPGetFieldDomain( callerID,FieldIds(1),nx0,ny0,x0,y0,dx0,dy0 )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error getting Field domain' )
  GO TO 9999
END IF
Ymin = y0
Ymax = Ymin + FLOAT(ny0)*dy0

nx = 0
IF( defN )THEN
 ny = ClassData(CD_VRES)
END IF

IF( ny <= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'CreateLineProfile'
  WRITE(eInform,'(A,I0)')'Invalid number of line points: ',ny
  GO TO 9999
END IF

WRITE(6,'(/,"Xprf,Yprf     = ",2ES10.3)')Xmin,Xmax
WRITE(6,'(  "Zmin,Zmax     = ",2ES10.3)')Ymin,Ymax
WRITE(6,'(  "Line position = ", ES10.3)')linePos

nxy = ny

9999 CONTINUE

RETURN
END
!==============================================================================
! BuildLine
!==============================================================================
SUBROUTINE BuildLine()

USE Extract_fi

IMPLICIT NONE

INTEGER i

SELECT CASE( LineType )
  CASE( HORZ_LINE )
    IF( nx <= 1 )THEN
      dx = (xMax - xMin)
    ELSE
      dx = (xMax - xMin)/FLOAT(nx-1)
    END IF
    IF( nx <= 1 )THEN
      dy = (yMax - yMin)
    ELSE
      dy = (yMax - yMin)/FLOAT(nx-1)
    END IF
    DO i = 1,nx
      Xgrd(i) = Xmin + (i-1)*dx
      Ygrd(i) = Ymin + (i-1)*dy
    END DO
  CASE( VERT_LINE )
    IF( ny <= 1 )THEN
      dy = (yMax - yMin)
    ELSE
      dy = (yMax - yMin)/FLOAT(ny-1)
    END IF
    DO i = 1,ny
      Xgrd(i) = linePos
      Ygrd(i) = Ymin + (i-1)*dy
    END DO
  CASE DEFAULT
    nError   = UK_ERROR
    eMessage = 'Unrecognized Line Extraction Type'
END SELECT
IF( nError/= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateGridAuto
!==============================================================================
SUBROUTINE CreateGridAuto()

USE Extract_fi

IMPLICIT NONE

INTEGER i, irv

CHARACTER(128) string

CALL getAutoGridContour( FieldIds(1),Fields(1),FldMax(1),FldMin(1) )
IF( nError /= NO_ERROR )GOTO 9999

xMin =  HUGE(1.0)
xMax = -HUGE(1.0)
yMin = xMin
yMax = xMax
DO i = 1,nPoint
  xMin = MIN(xMin,cPoints(i)%x)
  xMax = MAX(xMax,cPoints(i)%x)
  yMin = MIN(yMin,cPoints(i)%y)
  yMax = MAX(yMax,cPoints(i)%y)
END DO
dx   = xMax - xMin
xMin = xMin - 0.05*dx
xMax = xMax + 0.05*dx
dy   = yMax - yMin
yMin = yMin - 0.05*dy
yMax = yMax + 0.05*dy

WRITE(string,*)xMin,xMax
string = ADJUSTL(string)
WRITE(6,'(A,T26,A)')'Selected Grid Range (X) ',': '//TRIM(string)
WRITE(string,*)yMin,yMax
string = ADJUSTL(string)
WRITE(6,'(A,T26,A)')'Selected Grid Range (Y) ',': '//TRIM(string)

WRITE(6,'(/,A,T26,A,$)')'Grid points (nx,ny)',': '
READ(lun_in,*)nx,ny
WRITE(6,'(/,"nx, ny = ",I4,1x,I4)')nx,ny

IF( nx <= 0 .OR. ny <= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'CreateGridAuto'
  WRITE(eMessage,'(A,I0,I0)')'Invalid number of grid points: ',nx,ny
  GO TO 9999
END IF

nxy = nx*ny

9999 CONTINUE

IF( ALLOCATED(cPoints  )   )DEALLOCATE( cPoints,    STAT=irv )
IF( ALLOCATED(cLines   )   )DEALLOCATE( cLines,     STAT=irv )
IF( ALLOCATED(contourList) )DEALLOCATE( contourList,STAT=irv )

RETURN
END
!==============================================================================
! CreateGridCustom
!==============================================================================
SUBROUTINE CreateGridCustom()

USE Extract_fi

IMPLICIT NONE

INTEGER        i, ios
CHARACTER(128) grdfile


WRITE(6,'(/,A,$)')'Grid: (Nx*Ny)[< 0 for file] :'
READ(lun_in,*)nxy
WRITE(6,'(/,"Nx*Ny = ",I7)')nxy
IF( nxy < 0 )THEN
  lun_grd = 12
  WRITE(6,'(A,$)')'Grid file ? :'
  READ(lun_in,'(A)',IOSTAT=ios)grdfile
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateGridCustom'
    eMessage = 'Error reading custom grid filename'
    WRITE(eInform,'(A)')'File='//TRIM(grdfile)
    GOTO 9999
  END IF
  grdfile = ADJUSTL(grdfile)
  OPEN( lun_grd,FILE=TRIM(grdfile),STATUS='OLD',IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateGridCustom'
    eMessage = 'Error opening custom grid file'
    WRITE(eInform,'(A)')'File='//TRIM(grdfile)
    GOTO 9999
  END IF
  READ(lun_grd,*,IOSTAT=ios)nxy
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateGridCustom'
    eMessage = 'Error reading nxy from custom grid file'
    WRITE(eInform,'(A)')'File='//TRIM(grdfile)
    GOTO 9999
  END IF
ELSE
  lun_grd = lun_in
END IF

CALL allocateGrid( nxy )
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,nxy
  IF( lun_grd == lun_in )THEN
    WRITE(6,'(/,"xGrd(",I3.3,"), yGrd(",I3.3,") : ",$)')i,i
    READ(lun_in,*,IOSTAT=ios)xGrd(i),yGrd(i)
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'CreateGridCustom'
      eMessage = 'Error reading xGrd, yGrd from standard input'
      WRITE(eInform,'(A,I0,I0)')'ios for i = ',ios,i
      GOTO 9999
    END IF
  ELSE
    READ(lun_grd,*,IOSTAT=ios)xGrd(i),yGrd(i)
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'CreateGridCustom'
      eMessage = 'Error reading xGrd, yGrd from custom grid file'
      WRITE(eInform,'(A,I0,I0)')'ios for i = ',ios,i
      WRITE(eAction,'(A)')'File='//TRIM(grdfile)
      GOTO 9999
    END IF
  END IF
  WRITE(6,'(/,I3.3,1X,F13.5,1X,F13.5)')i,xGrd(i),yGrd(i)
END DO
IF( lun_grd /= lun_in )THEN
  CLOSE( lun_grd,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateGridCustom'
    eMessage = 'Error closing custom grid file'
    WRITE(eInform,'(A)')'File='//TRIM(grdfile)
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateGridUniform
!==============================================================================
SUBROUTINE CreateGridUniform()

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER ios, itry, irv
INTEGER nx0,ny0

REAL x0,y0,dx0,dy0
REAL, DIMENSION(2) :: x

CHARACTER(128) string
LOGICAL defX, defY

INTEGER, EXTERNAL :: getInput

defX = .FALSE.
defY = .FALSE.

!== X range

ios  = 1
itry = 0
string = 'Grid X range'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,2,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defX = .TRUE.
  ELSE IF( ios == 0 )THEN
    Xmin = x(1)
    Xmax = x(2)
  END IF
END DO

!== No. X pts

ios  = 1
itry = 0
string = 'No. of X pts'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    nx = 21
  ELSE IF( ios == 0 )THEN
    nx = NINT(x(1))
  END IF
END DO

!== Y Range

ios  = 1
itry = 0
string = 'Grid Y range'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,2,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    defY = .TRUE.
  ELSE IF( ios == 0 )THEN
    Ymin = x(1)
    Ymax = x(2)
  END IF
END DO

!== No. Y pts

ios  = 1
itry = 0
string = 'No. of Y pts'
DO WHILE( ios > 0 )
  itry = itry + 1
  ios = getInput( string,1,x )
  IF( ios > 0 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Error reading input, try again'
    ELSE
      nError   = UK_ERROR
      eMessage = 'Error reading '//TRIM(string)
      GOTO 9999
    END IF
  ELSE IF( ios < 0 )THEN
    ny = 21
  ELSE IF( ios == 0 )THEN
    ny = NINT(x(1))
  END IF
END DO

IF( defX .OR. defY )THEN
  irv = SCIPGetFieldDomain( callerID,FieldIds(1),nx0,ny0,x0,y0,dx0,dy0 )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Error getting Field domain' )
    GOTO 9999
  END IF
  IF( defX )THEN
    Xmin = x0
    Xmax = Xmin + FLOAT(nx0)*dx0
  END IF
  IF( defY )THEN
    Ymin = y0
    Ymax = Ymin + FLOAT(ny0)*dy0
  END IF
END IF

IF( nx <= 0 .OR. ny <= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'CreateGridUniform'
  WRITE(eMessage,'(A,I0,I0)')'Invalid number of grid points: ',nx,ny
  GOTO 9999
END IF

WRITE(6,'(/,"Xmin,Xmax,Nx = ",2ES10.3,I4)')Xmin,Xmax,nx
WRITE(6,'(  "Ymin,Ymax,Ny = ",2ES10.3,I4)')Ymin,Ymax,ny

nxy = nx*ny

9999 CONTINUE

RETURN
END
!==============================================================================
! BuildGrid
!==============================================================================
SUBROUTINE BuildGrid()

USE Extract_fi

IMPLICIT NONE

INTEGER i,j,in

IF( xMin > xMax )THEN
 dx = xMin
 xMin = xMax
 xMax = dx
END IF

IF( yMin > yMax )THEN
 dy = yMin
 yMin = yMax
 yMax = dy
END IF

IF( nx <= 1 )THEN
  dx = (xMax - xMin)
ELSE
  dx = (xMax - xMin)/FLOAT(nx-1)
END IF

IF( ny <= 1 )THEN
  dy = (yMax - yMin)
ELSE
  dy = (yMax - yMin)/FLOAT(ny-1)
END IF

DO i = 1,nx
  DO j = 1,ny
    in = (i-1)*ny + j
    xGrd(in) = xMin + (i-1)*dx
    yGrd(in) = yMin + (j-1)*dy
  END DO
END DO

9999 CONTINUE

RETURN
END
!==============================================================================
! CreateContour
!==============================================================================
SUBROUTINE getAutoGridContour( fID,thisField,dmx,dmn )

USE Extract_fi
USE contour_fd
USE SCIPtool

IMPLICIT NONE

INTEGER                :: fID
TYPE( SCIPPlotFieldT ) :: thisField
REAL                   :: dmx
REAL                   :: dmn

INTEGER        irv
CHARACTER(128) string

contourHead%number    = 1
contourHead%DrawMode  = PLOT_OFF
contourHead%LabelMode = PLOT_OFF
contourHead%Scale     = 1.0
contourHead%Unit      = 'default'
contourMode           = CLOSE_CONTOUR

ALLOCATE( contourList(contourHead%number),STAT=irv )
IF( irv /= 0 )THEN
  nError   = irv
  eRoutine = 'getAutoGridContour'
  eMessage = 'Error allocating contour list array'
  GOTO 9999
END IF

contourList(1)%label = TRIM(ClassStr(thisField%class)%string)

WRITE(string,*)dmn,dmx
string = ADJUSTL(string)

WRITE(6,'(A,T26,A)')TRIM(contourList(1)%label)//' range ('//TRIM(thisField%units)//')',': '//TRIM(string)
WRITE(6,'(/,A,T26,A,$)')'Minimum value of interest ',': '
READ(lun_in,*)contourList(1)%contour

WRITE(6,'(/,"Minimum contour level = ",1pE13.5)')contourList(1)%contour

CALL CreateContours( fID,thisField,dmx,dmn )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END


