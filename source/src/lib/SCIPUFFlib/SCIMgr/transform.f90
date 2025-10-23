!*******************************************************************************
!                TransformF
!*******************************************************************************
INTEGER FUNCTION TransformF( Cin,Cout,np,xp,yp )

USE field_fd
USE error_fi
USE param_fd
USE default_fd
USE SCIMgr_fd
USE constants_fd
USE SCIMgrState

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
REAL, DIMENSION(np),          INTENT( INOUT ) :: xp   !x coordinate arrray
REAL, DIMENSION(np),          INTENT( INOUT ) :: yp   !y coordinate arrray

INTEGER            :: currentState,irv

!==== initialize

TransformF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== transform points

CALL init_error

CALL Transform( Cin,Cout,np,xp,yp )

IF( nError == NO_ERROR ) TransformF = SCIPsuccess

!==== finish

irv = SCIMgrSetState( currentState )

RETURN

END
!*******************************************************************************
!                Transform
!*******************************************************************************
SUBROUTINE Transform( Cin,Cout,np,xp,yp )

USE field_fd
USE error_fi
USE param_fd
USE default_fd
USE SCIMgr_fd
USE constants_fd
USE datums

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
REAL, DIMENSION(np),          INTENT( INOUT ) :: xp   !x coordinate arrray
REAL, DIMENSION(np),          INTENT( INOUT ) :: yp   !y coordinate arrray

!==============================================================================
! Local variables
!==============================================================================
REAL,PARAMETER :: TOL  = 1.E-2        !Tolerance for coincident point calculation
REAL,PARAMETER :: TOLR = 1./TOL

INTEGER ios
INTEGER i
INTEGER irv
INTEGER ModeIn
INTEGER ModeOut
LOGICAL VertSlice
REAL    xfac                               !X conversion to Lon
REAL    yfac                               !Y conversion to Lat
INTEGER zone                               !UTM zone
REAL,DIMENSION(:), ALLOCATABLE :: x        !X work array
REAL,DIMENSION(:), ALLOCATABLE :: y        !Y work array
CHARACTER(24)  :: string                   !error string
LOGICAL        :: test                     !local test
TYPE( SCIPPointT ) :: StartPt
TYPE( SCIPPointT ) :: EndPt
REAL               :: LenIn
REAL               :: LenOut
REAL               :: DLen
REAL               :: DirIn
REAL               :: DirOut
REAL               :: DDir
REAL               :: S1
REAL               :: S2

LOGICAL,EXTERNAL   :: CheckReferenceSet

!==============================================================================
! Check to see if can do anything
!==============================================================================

IF( SIGN(1,Cin%mode) /= SIGN(1,Cout%mode) )GOTO 9997

!==============================================================================
! Initialize transformation
!==============================================================================
ModeIn  = ABS(Cin%mode)
ModeOut = ABS(Cout%mode)

VertSlice = Cin%mode < 0

IF( ModeIn /= ModeOut )THEN
  IF( ModeIn == I_CARTESIAN )THEN
    IF( .NOT.CheckReferenceSet(Cin) )GOTO 9994
  END IF

  IF( ModeOut == I_CARTESIAN )THEN
    IF( .NOT.CheckReferenceSet(Cout) )GOTO 9994
  END IF
END IF

!==============================================================================
! Check to see if there is anything to do
!==============================================================================
IF( ModeIn == ModeOut )THEN
  SELECT CASE( ModeIn )
    CASE( I_UTM )
      test = Cin%UTMZone == Cout%UTMZone
    CASE( I_CARTESIAN )
      test = Cin%reference%x   == Cout%reference%x   .AND. &
             Cin%reference%y   == Cout%reference%y   .AND. &
             Cin%reference%lat == Cout%reference%lat .AND. &
             Cin%reference%lon == Cout%reference%lon
      IF( .NOT.test )THEN
        test = .NOT.CheckReferenceSet(Cin) .OR. .NOT.CheckReferenceSet(Cout)
      END IF
    CASE DEFAULT
      test= .TRUE.
  END SELECT

  IF( VertSlice )THEN
    xfac = TOLR*SPACING(MAX(ABS(Cin%vertSlice%startPt%x),ABS(Cin%vertSlice%endPt%x)))
    yfac = TOLR*SPACING(MAX(ABS(Cin%vertSlice%startPt%y),ABS(Cin%vertSlice%endPt%y)))
    test = test .AND. Cin%vertSlice%resolution == Cout%vertSlice%resolution
    test = test .AND. ABS(Cin%vertSlice%startPt%x  - Cout%vertSlice%startPt%x) <= xfac
    test = test .AND. ABS(Cin%vertSlice%startPt%y  - Cout%vertSlice%startPt%y) <= yfac
    test = test .AND. ABS(Cin%vertSlice%endPt%x    - Cout%vertSlice%endPt%x  ) <= xfac
    test = test .AND. ABS(Cin%vertSlice%endPt%y    - Cout%vertSlice%endPt%y  ) <= yfac
  END IF

  IF( test )GOTO 9999
END IF

!==============================================================================
! Vertical slice transformation
!==============================================================================
IF( VertSlice )THEN

!==============================================================================
! Allocate work space
!==============================================================================
  ALLOCATE( x(np),STAT=ios )
  IF( ios /= 0 )GOTO 9998

!==============================================================================
! Convert input slice end points to LLA
!==============================================================================
  SELECT CASE(  ModeIn )
    CASE( I_UTM )
      zone = Cin%UTMZone
      irv = UTM2LL( Cin%UTMZone,Cin%vertSlice%startPt%x, &
                                Cin%vertSlice%startPt%y,yfac,xfac )
      IF( irv /= 0 )THEN
        CALL setUTMerror( 'UTM2LL',irv )
        GOTO 9999
      ELSE
        StartPt%x = xfac
        StartPt%y = yfac
      END IF
      irv = UTM2LL( Cin%UTMZone,Cin%vertSlice%endPt%x, &
                                Cin%vertSlice%endPt%y,yfac,xfac )
      IF( irv /= 0 )THEN
        CALL setUTMerror( 'UTM2LL',irv )
        GOTO 9999
      ELSE
        EndPt%x = xfac
        EndPt%y = yfac
      END IF
    CASE( I_CARTESIAN )
      yfac = SPHFACR*1000.
      xfac = yfac/COS(PI180*Cin%reference%lat)
      StartPt%x = (Cin%vertSlice%startPt%x - Cin%reference%x)*xfac &
                + Cin%reference%lon
      StartPt%y = (Cin%vertSlice%startPt%y - Cin%reference%y)*yfac &
                + Cin%reference%lat
      EndPt%x   = (Cin%vertSlice%endPt%x   - Cin%reference%x)*xfac &
                + Cin%reference%lon
      EndPt%y   = (Cin%vertSlice%endPt%y   - Cin%reference%y)*yfac &
                + Cin%reference%lat
    CASE DEFAULT
      StartPt = Cin%vertSlice%startPt
      EndPt   = Cin%vertSlice%endPt
  END SELECT

!==============================================================================
! Convert LLA slice end points to output mode
!==============================================================================
  SELECT CASE(  ModeOut )
    CASE( I_UTM )
      zone = Cout%UTMZone
      irv = LL2UTM( StartPt%y,StartPt%x,zone,xfac,yfac )
      IF( irv /= 0 )THEN
        CALL setUTMerror( 'LL2UTM',irv )
        GOTO 9999
      ELSE
        StartPt%x = xfac
        StartPt%y = yfac
      END IF
      irv = LL2UTM( EndPt%y,EndPt%x,zone,xfac,yfac )
      IF( irv /= 0 )THEN
        CALL setUTMerror( 'LL2UTM',irv )
        GOTO 9999
      ELSE
        EndPt%x = xfac
        EndPt%y = yfac
      END IF
    CASE( I_CARTESIAN )
      yfac = SPHFAC*0.001
      xfac = yfac*COS(PI180*Cout%reference%lat)
      StartPt%x = (StartPt%x - Cout%reference%lon)*xfac + Cout%reference%x
      StartPt%y = (StartPt%y - Cout%reference%lat)*yfac + Cout%reference%y
      EndPt%x   = (EndPt%x   - Cout%reference%lon)*xfac + Cout%reference%x
      EndPt%y   = (EndPt%y   - Cout%reference%lat)*yfac + Cout%reference%y
    CASE DEFAULT
  END SELECT

!==============================================================================
! Check to see if end points are coincident
!==============================================================================
  xfac = Cout%vertSlice%endPt%x - Cout%vertSlice%startPt%x
  yfac = Cout%vertSlice%endPt%y - Cout%vertSlice%startPt%y
  DLen = Cout%vertSlice%endPt%x*Cout%vertSlice%startPt%y - &
         Cout%vertSlice%endPt%y*Cout%vertSlice%startPt%x
  test = ABS(StartPt%x*yfac - StartPt%y*xfac + DLen ) < TOL
  IF( test ) THEN
    test = test .AND. ABS(EndPt%x*yfac - EndPt%y*xfac + DLen ) < TOL
  END IF
  IF( .NOT.test )GOTO 9996

!==============================================================================
! Transform data points
!==============================================================================
  LenIn  = SQRT( ( EndPt%x - StartPt%x )**2 + ( EndPt%y - StartPt%y )**2 )
  LenOut = SQRT( ( Cout%vertSlice%endPt%x - Cout%vertSlice%startPt%x )**2 + &
                 ( Cout%vertSlice%endPt%y - Cout%vertSlice%startPt%y )**2 )
  DLen   = SQRT( ( Cout%vertSlice%startPt%x - StartPt%x )**2 + &
                 ( Cout%vertSlice%startPt%y - StartPt%y )**2 )

  xfac = TOLR*SPACING(MAX(ABS(Cout%vertSlice%endPt%x),ABS(Cout%vertSlice%startPt%x), &
                          ABS(Cout%vertSlice%endPt%y),ABS(Cout%vertSlice%startPt%y)))

  IF( LenOut < xfac )GOTO 9995

  DirIn  = ATAN2( EndPt%y - StartPt%y,EndPt%x - StartPt%x )
  DirOut = ATAN2( Cout%vertSlice%endPt%y - Cout%vertSlice%startPt%y, &
                  Cout%vertSlice%endPt%x - Cout%vertSlice%startPt%x )
  DDir   = ATAN2( Cout%vertSlice%startPt%y - StartPt%y, &
                  Cout%vertSlice%startPt%x - StartPt%x )

  S1 = SIGN(1.,DirIn*DirOut)
  S2 = SIGN(1.,DirIn*DDir)
  xfac = ( LenIn*FLOAT(Cout%vertSlice%resolution) )/(LenOut*FLOAT(Cin%vertSlice%resolution))
  yfac = ( DLen*FLOAT(Cout%vertSlice%resolution) )/(LenOut)
  DO i = 1,np
    x(i) = S1*(xfac*xp(i) - S2*yfac)
  END DO

!==============================================================================
! Copy results back
!==============================================================================
  DO i = 1,np
    xp(i) = x(i)
  END DO

!==============================================================================
! Horizontal slice transformation
!==============================================================================
ELSE

!==============================================================================
! Allocate work space
!==============================================================================
  ALLOCATE( x(np),y(np),STAT=ios )
  IF( ios /= 0 )GOTO 9998

!==============================================================================
! Transform from input coordinate to Lat/Lon
!==============================================================================
  SELECT CASE(  ModeIn )
    CASE( I_UTM )
      zone = Cin%UTMZone
      DO i = 1,np
        irv = UTM2LL( zone,xp(i),yp(i),yfac,xfac )
        IF( irv /= 0 )THEN
          CALL setUTMerror( 'UTM2LL',irv )
          GOTO 9999
        ELSE
          x(i) = xfac
          y(i) = yfac
        END IF
      END DO
    CASE( I_CARTESIAN )
      yfac = SPHFACR*1000.
      xfac = yfac/COS(PI180*Cin%reference%lat)
      DO i = 1,np
        x(i) = (xp(i) - Cin%reference%x)*xfac + Cin%reference%lon
        y(i) = (yp(i) - Cin%reference%y)*yfac + Cin%reference%lat
      END DO
    CASE DEFAULT
      DO i = 1,np
        x(i) = xp(i)
        y(i) = yp(i)
      END DO
  END SELECT

!==============================================================================
! Transform Lat/Lon to output coordinate
!==============================================================================
  SELECT CASE(  ModeOut )
    CASE( I_UTM )
      zone = Cout%UTMZone
      DO i = 1,np
        irv = LL2UTM( y(i),x(i),zone,xfac,yfac )
        IF( irv /= 0 )THEN
          CALL setUTMerror( 'LL2UTM',irv )
          GOTO 9999
        ELSE
          x(i) = xfac
          y(i) = yfac
        END IF
      END DO
    CASE( I_CARTESIAN )
      yfac = SPHFAC*0.001
      xfac = yfac*COS(PI180*Cout%reference%lat)
      DO i = 1,np
        x(i) = (x(i) - Cout%reference%lon)*xfac + Cout%reference%x
        y(i) = (y(i) - Cout%reference%lat)*yfac + Cout%reference%y
      END DO
    CASE DEFAULT
  END SELECT

!==============================================================================
! Copy results back
!==============================================================================
  DO i = 1,np
    xp(i) = x(i)
    yp(i) = y(i)
  END DO

END IF

9999 CONTINUE

!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(x) )DEALLOCATE( x,STAT=ios )
IF( ALLOCATED(y) )DEALLOCATE( y,STAT=ios )

RETURN

!==============================================================================
! Error section
!==============================================================================
! Allocation error
!==============================================================================
9998 CONTINUE
nError = UK_ERROR
eRoutine = 'Transform'
eMessage = 'Failure to allocate work space'
WRITE(string,*,IOSTAT=i)ios
IF( i /= 0 )string = '**ERROR**'
eInform = 'Allocation Error='//TRIM(ADJUSTL(string))
WRITE(string,*,IOSTAT=i)np
IF( i /= 0 )string = '**ERROR**'
eInform = TRIM(eInform)//' : Request='//TRIM(ADJUSTL(string))
GOTO 9999

!==============================================================================
! Invalid transform request - different modes
!==============================================================================
9997 CONTINUE
nError = UK_ERROR
eRoutine = 'Transform'
eMessage = 'Invalid Transform request'
eInform  = 'Cannot transform between Vertical/Horizontal Coordinate modes'
GOTO 9999

!==============================================================================
! Invalid transform request - different vertical slice
!==============================================================================
9996 CONTINUE
nError = UK_ERROR
eRoutine = 'Transform'
eMessage = 'Invalid Transform request'
eInform  = 'In/Out represent different vertical slices'
GOTO 9999

!==============================================================================
! Invalid transform request - different vertical slice
!==============================================================================
9995 CONTINUE
nError = UK_ERROR
eRoutine = 'Transform'
eMessage = 'Invalid Transform request'
eInform  = 'Out represents zero length vertical slice'
GOTO 9999

!==============================================================================
! Invalid transform request - No reference point
!==============================================================================
9994 CONTINUE
nError = UK_ERROR
eRoutine = 'Transform'
eMessage = 'Invalid Transform request'
eInform  = 'Reference point not specified'
GOTO 9999

END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                TransformPtF
!*******************************************************************************
INTEGER FUNCTION TransformPtF( Cin,Cout,np,pt )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                           INTENT( IN    ) :: np   !number of points
TYPE( SCIPPointT ), DIMENSION(np), INTENT( INOUT ) :: pt   !pt arrray

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

!==== initialize

TransformPtF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== transform point

IF( .NOT.Aborted() )THEN

  CALL init_error

  CALL TransformPt( Cin,Cout,np,pt )

  IF( nError == NO_ERROR ) TransformPtF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                TransformPt
!*******************************************************************************
SUBROUTINE TransformPt( Cin,Cout,np,pt )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                           INTENT( IN    ) :: np   !number of points
TYPE( SCIPPointT ), DIMENSION(np), INTENT( INOUT ) :: pt   !pt arrray

!==============================================================================
! Local variables
!==============================================================================
REAL, DIMENSION(:), ALLOCATABLE :: xp
REAL, DIMENSION(:), ALLOCATABLE :: yp
INTEGER ios
INTEGER i

IF( Aborted() ) GOTO 9999

ALLOCATE( xp(np),yp(np),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'TransformPt'
  eMessage = 'Failed to allocate work space'
  GOTO 9999
END IF

DO i = 1,np
  xp(i) = pt(i)%x
  yp(i) = pt(i)%y
END DO

CALL Transform( Cin,Cout,np,xp,yp )
IF( nError == NO_ERROR )THEN
  DO i = 1,np
    pt(i)%x = xp(i)
    pt(i)%y = yp(i)
  END DO
END IF

9999 CONTINUE

IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(yp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                TransformPuffF
!*******************************************************************************
INTEGER FUNCTION TransformPuffF( Cin,Cout,np,puff )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE default_fd
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
TYPE( puffT ), DIMENSION(np), INTENT( INOUT ) :: puff !puff arrray

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

!==== initialize

TransformPuffF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== transform puffs

IF( .NOT.Aborted() )THEN

  CALL init_error

  CALL TransformPuff( Cin,Cout,np,puff )

  IF( nError == NO_ERROR ) TransformPuffF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                TransformPuff
!*******************************************************************************
SUBROUTINE TransformPuff( Cin,Cout,np,puff )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE abort
USE default_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
TYPE( puffT ), DIMENSION(np), INTENT( INOUT ) :: puff !puff arrray

!==============================================================================
! Local variables
!==============================================================================
REAL, DIMENSION(:), ALLOCATABLE :: xp
REAL, DIMENSION(:), ALLOCATABLE :: yp
INTEGER ios
INTEGER i
TYPE( SCIPFieldCoordinateT ) :: inCoord
TYPE( SCIPFieldCoordinateT ) :: outCoord

ALLOCATE( xp(np),yp(np),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'TransformPuff'
  eMessage = 'Failed to allocate work space'
  GOTO 9999
END IF

IF( Aborted() ) GOTO 9999

DO i = 1,np
  xp(i) = SNGL(puff(i)%xbar)
  yp(i) = SNGL(puff(i)%ybar)
END DO

inCoord%mode                 = Cin%mode
inCoord%UTMZone              = Cin%UTMZone
inCoord%reference            = Cin%reference
inCoord%vertSlice%resolution = NOT_SET_I
inCoord%vertSlice%startPt%x  = NOT_SET_R
inCoord%vertSlice%startPt%y  = NOT_SET_R
inCoord%vertSlice%endPt      = inCoord%vertSlice%startPt

outCoord%mode                 = Cout%mode
outCoord%UTMZone              = Cout%UTMZone
outCoord%reference            = Cout%reference
outCoord%vertSlice%resolution = NOT_SET_I
outCoord%vertSlice%startPt%x  = NOT_SET_R
outCoord%vertSlice%startPt%y  = NOT_SET_R
outCoord%vertSlice%endPt      = outCoord%vertSlice%startPt

CALL Transform( inCoord,outCoord,np,xp,yp )

IF( nError == NO_ERROR )THEN
  DO i = 1,np
    puff(i)%xbar = DBLE(xp(i))
    puff(i)%ybar = DBLE(yp(i))
  END DO
END IF

9999 CONTINUE

IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(yp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
!*******************************************************************************
!                CheckReferenceSet
!*******************************************************************************
LOGICAL FUNCTION CheckReferenceSet( Cin )

USE tooluser_fd
USE default_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ), INTENT( IN ) :: Cin  !Input Coordinate data

CheckReferenceSet = .TRUE.

IF( Cin%reference%x == NOT_SET_R .OR. Cin%reference%x == DEF_VAL_R )THEN
  CheckReferenceSet  = .FALSE.
ELSE IF( Cin%reference%y == NOT_SET_R .OR. Cin%reference%y == DEF_VAL_R )THEN
  CheckReferenceSet  = .FALSE.
ELSE IF( Cin%reference%lon == NOT_SET_R .OR. Cin%reference%lon == DEF_VAL_R )THEN
  CheckReferenceSet  = .FALSE.
ELSE IF( Cin%reference%lat == NOT_SET_R .OR. Cin%reference%lat == DEF_VAL_R )THEN
  CheckReferenceSet  = .FALSE.
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                TransformXYF
!*******************************************************************************
INTEGER FUNCTION TransformXYF( Cin,Cout,xp,yp )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
REAL,                              INTENT( INOUT ) :: xp   !x coordinate
REAL,                              INTENT( INOUT ) :: yp   !y coordinate

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

!==== initialize

TransformXYF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== transform point

IF( .NOT.Aborted() )THEN

  CALL init_error

  CALL TransformXY( Cin,Cout,xp,yp )

  IF( nError == NO_ERROR ) TransformXYF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                TransformXY
!*******************************************************************************
SUBROUTINE TransformXY( Cin,Cout,xp,yp)

USE field_fd
USE SCIMgr_fd
USE error_fi
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
REAL,                              INTENT( INOUT ) :: xp   !x coordinate
REAL,                              INTENT( INOUT ) :: yp   !y coordinate

!==============================================================================
! Local variables
!==============================================================================
REAL, DIMENSION(:), ALLOCATABLE :: x
REAL, DIMENSION(:), ALLOCATABLE :: y
INTEGER ios
INTEGER i, np

IF( Aborted() ) GOTO 9999

np =1

ALLOCATE( x(np),y(np),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'TransformXY'
  eMessage = 'Failed to allocate work space'
  GOTO 9999
END IF

DO i = 1,np
  x(i) = xp
  y(i) = yp
END DO

CALL Transform( Cin,Cout,np,x,y )
IF( nError == NO_ERROR )THEN
  DO i = 1,np
    xp = x(i)
    yp = y(i)
  END DO
END IF

9999 CONTINUE

IF( ALLOCATED(x) )DEALLOCATE( x,STAT=ios )
IF( ALLOCATED(y) )DEALLOCATE( y,STAT=ios )

RETURN
END
