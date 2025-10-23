!==============================================================================
!  SWIM utility functions
!==============================================================================

REAL FUNCTION SWIMPGTtoInvL( pgt ) RESULT( invL )

IMPLICIT NONE

REAL(8), INTENT( IN ) :: pgt

!------ Equivalent Monin-Obukhov lengths for PGT classes

REAL, DIMENSION(7), PARAMETER :: invL_PGT = (/&
       -1./5., -1./12.5, -1./50., -1./1000., 1./25., 1./13., 1./5. /)

REAL    rat
INTEGER i

INTEGER, EXTERNAL :: SWIMlimit

i   = SWIMlimit( INT(pgt),1,6 )
rat = SNGL(pgt) - FLOAT(i)

invL = invL_PGT(i) + rat*(invL_PGT(i+1)-invL_PGT(i))

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMlimit( i,imin,imax )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i, imin, imax

SWIMlimit = MAX( i,imin )
SWIMlimit = MIN( SWIMlimit,imax )

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION SWIMrlimit( r,rmin,rmax )

IMPLICIT NONE

REAL, INTENT( IN ) :: r, rmin, rmax

SWIMrlimit = MAX( r,rmin )
SWIMrlimit = MIN( SWIMrlimit,rmax )

RETURN
END
!===============================================================================

INTEGER FUNCTION SWIMconvPrjCoord( First,Obs )

USE SWIMparam_fd
USE SWIM_fi
USE constants_fd
USE datums

!------ Converts Obs location to Project coordinates

IMPLICIT NONE

TYPE( FirstObs ), INTENT( IN ) :: First
TYPE( ObsMet   ), POINTER      :: Obs

INTEGER irv
REAL    xmap, ymap
REAL    lat, lon

INTEGER, EXTERNAL :: CheckLon
LOGICAL, EXTERNAL :: HasPrjReference

SWIMconvPrjCoord = SWIMfailure

!------ Obs in cartesian

IF( BTEST(First%type,OTB_XY) )THEN

  IF( Prj%coord == I_CARTESIAN .OR. Prj%coord == I_UTM )THEN

    SWIMconvPrjCoord = SWIMresult
    RETURN !No conversion required

  ELSE IF( Prj%coord == I_METERS )THEN

    Obs%x = Obs%x * 1.E3
    Obs%y = Obs%y * 1.E3

  ELSE !Lat/lon project

    IF( HasPrjReference() )THEN
      ymap = SPHFACR * 1.E3
      xmap = ymap/COS( Prj%Lat0*PI180 )
      Obs%x = Prj%Lon0 + (Obs%x - Prj%Xref)*xmap
      Obs%y = Prj%Lat0 + (Obs%y - Prj%Yref)*ymap
      irv = CheckLon( Obs%x,Prj%xmin,Prj%xmax )
      IF( irv /= SWIMsuccess )GOTO 9999
    ELSE
      error%Number  = IV_ERROR
      error%Routine = 'SWIMconvPrjCoord'
      error%Message = 'Must set map reference location'
      error%Inform  = 'Obs & Project domains in different coord. systems'
      GOTO 9999
    END IF

  END IF

ELSE  !Obs in Lat/Lon

  IF( Prj%coord == I_LATLON )THEN

    irv = CheckLon( Obs%x,Prj%xmin,Prj%xmax )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE IF( Prj%coord == I_UTM )THEN

    lat = Obs%y; lon = Obs%x
    CALL AdjustLongitude( lon )
    irv = LL2UTM( lat,lon,Prj%UTMzone,Obs%x,Obs%y )

    IF( irv == 14 .OR. irv == 18 )THEN
      ymap = SPHFAC * 1.E-3
      xmap = ymap*COS( Prj%Lat0*PI180 )
      Obs%x = Prj%Xref + (lon - Prj%Lon0)*xmap
      Obs%y = Prj%Yref + (lat - Prj%Lat0)*ymap
    ELSE IF( irv /= 0 )THEN
      CALL SWIMsetUTMerror( 'LL2UTM',irv )
      GOTO 9999
    END IF

  ELSE !Cartesian

    IF( HasPrjReference() )THEN
      IF( Prj%coord == I_METERS )THEN
        ymap = SPHFAC
      ELSE
        ymap = SPHFAC * 1.E-3
      END IF
      xmap = ymap*COS( Prj%Lat0*PI180 )
      Obs%x = Prj%Xref + (Obs%x - Prj%Lon0)*xmap
      Obs%y = Prj%Yref + (Obs%y - Prj%Lat0)*ymap
    ELSE
      error%Number  = IV_ERROR
      error%Routine = 'SWIMconvPrjCoord'
      error%Message = 'Must set map reference location'
      error%Inform  = 'Obs & Project domains in different coord. systems'
      GOTO 9999
    END IF

  END IF

END IF

SWIMconvPrjCoord = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION MapLoc( map_in,xin,yin,map_out,xout,yout,xmap,ymap )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

!------ returns location in output coordinate system (Cartesian or Lat/lon)
!       as well as the map transformation.  Cartesian coordinates are assumed
!       to be in km.  The reference lat/lon must be defined prior to calling
!       this routine.

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: map_in, map_out
REAL,    INTENT( IN  ) :: xin, yin
REAL,    INTENT( OUT ) :: xout, yout, xmap, ymap

MapLoc = SWIMresult

!------ output location is same as input if coord. systems are identical

IF( map_out == map_in .OR. &
    map_out == I_CARTESIAN .AND. map_in  == I_UTM .OR. &
    map_in  == I_CARTESIAN .AND. map_out == I_UTM )THEN

  xout = xin
  yout = yin
  xmap = 1.
  ymap = 1.

  GOTO 9999

END IF

!------ check if reference location has been set for lat/lon input/output

IF( map_in == I_LATLON .OR. map_out == I_LATLON )THEN
  IF( Prj%lon0 == NOT_SET_R .OR. Prj%lat0 == NOT_SET_R .OR. &
      Prj%lon0 == DEF_VAL_R .OR. Prj%lat0 == DEF_VAL_R )THEN
    MapLoc = SWIMfailure
    error%Number   = IV_ERROR
    error%Routine = 'MapLoc'
    error%Message = 'Must set map reference location'
    error%Inform  = 'Run / Met domains in different coord. systems'
    GOTO 9999
  END IF
END IF

!------ if output is Lat/lon  : xmap = dx(out)/dx(in) = 180/(pi*R*cos(lat0))
!                               ymap = dy(out)/dy(in) = 180/(pi*R)
!                               xout = lon0 + (xin-xref)*xmap
!                               yout = lat0 + (yin-yref)*ymap

IF( map_out == I_LATLON )THEN

  IF( map_in == I_METERS )THEN
    xmap = 1.
  ELSE
    xmap = 1.E3
  END IF

  ymap = SPHFACR * xmap
  xmap = ymap/COS( Prj%lat0*PI180 )
  xout = Prj%lon0 + (xin - Prj%xref)*xmap
  yout = Prj%lat0 + (yin - Prj%yref)*ymap

ELSE IF( map_in == I_LATLON )THEN

!------ if input is Lat/lon and output is Cartesian:
!                               xmap = dx(out)/dx(in) = R*pi/180*cos(lat0)
!                               ymap = dy(out)/dy(in) = R*pi/180
!                               xout = xref + (xin-lon0)*xmap
!                               yout = yref + (yin-lat0)*ymap

  IF( map_out == I_METERS )THEN
    xmap = 1.
  ELSE
    xmap = 1.E-3
  END IF
  ymap = sphfac * xmap
  xmap = ymap*COS( Prj%lat0*PI180 )
  xout = Prj%xref + (xin - Prj%lon0)*xmap
  yout = Prj%yref + (yin - Prj%lat0)*ymap

ELSE IF( map_in == I_CARTESIAN .OR. map_in == I_UTM )THEN

!------ if input is Cartesian and output is Meters:

  xmap = 1.E3
  ymap = 1.E3
  xout = xin*xmap
  yout = yin*ymap

ELSE

!------ if input is Meters and output is Cartesian: error

  MapLoc = SWIMfailure
  error%Number   = IV_ERROR
  error%Routine = 'MapLoc'
  error%Message = 'Cannot have meters input / kilometers output'
  GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMgetLL( x,y,lon,lat )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetLL

USE SWIMparam_fd
USE SWIM_fi
USE constants_fd
USE datums

!------ Converts project coordinates (x,y) to LL

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y
REAL, INTENT( OUT ) :: lon, lat

INTEGER irv
REAL    xmap, ymap

LOGICAL, EXTERNAL :: HasPrjReference

SWIMgetLL = SWIMfailure

!------ Project is in LL

IF( prj%coord == I_LATLON )THEN

  lon = x
  lat = y

!------ Project is in UTM

ELSE IF( prj%coord == I_UTM )THEN

  irv = UTM2LL( Prj%UTMzone,x,y,lat,lon )
  IF( irv /= 0 )THEN
    CALL SWIMsetUTMerror( 'UTM2LL',irv )
    GOTO 9999
  END IF

!------ Project is Cartesian (and needs a LL reference)

ELSE IF( HasPrjReference() )THEN

  ymap = SPHFACR * 1.E3
  xmap = ymap/COS( Prj%Lat0*PI180 )
  lon = Prj%Lon0 + (x - Prj%Xref)*xmap
  lat = Prj%Lat0 + (y - Prj%Yref)*ymap

ELSE

  lon = NOT_SET_R
  lat = NOT_SET_R

  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetLL'
  error%Message = 'Unable to convert to lat/lon coordinates'
  GOTO 9999

END IF

SWIMgetLL = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SWIMsetPrjCoord()

USE SWIM_fi

IMPLICIT NONE

CALL SWIMinitCoord( PrjCoord )

PrjCoord%type          = Prj%coord
PrjCoord%zone          = Prj%UTMzone
PrjCoord%reference%x   = Prj%Xref
PrjCoord%reference%y   = Prj%Yref
PrjCoord%reference%lat = Prj%Lat0
PrjCoord%reference%lon = Prj%Lon0

RETURN
END

!===============================================================================

SUBROUTINE SWIMinitCoord( coord )

USE SWIM_fi

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMinitCoord

IMPLICIT NONE

TYPE( MapCoord ), INTENT( OUT ) :: coord

coord%type          = NOT_SET_I
coord%zone          = NOT_SET_I
coord%reference%x   = NOT_SET_R
coord%reference%y   = NOT_SET_R
coord%reference%lat = NOT_SET_R
coord%reference%lon = NOT_SET_R

coord%Lat0   = NOT_SET_R
coord%Lon0   = NOT_SET_R
coord%Lat1   = NOT_SET_R
coord%Lat2   = NOT_SET_R
coord%Rearth = NOT_SET_R
coord%n      = NOT_SET_R
coord%f      = NOT_SET_R
coord%m0     = NOT_SET_R
coord%y0     = NOT_SET_R
coord%sp0    = NOT_SET_R
coord%cp0    = NOT_SET_R
coord%sl0    = NOT_SET_R
coord%cl0    = NOT_SET_R
coord%cc0    = NOT_SET_R
coord%sc0    = NOT_SET_R
coord%cs0    = NOT_SET_R
coord%ss0    = NOT_SET_R

RETURN
END

!===============================================================================

LOGICAL FUNCTION HasPrjReference()

USE SWIM_fi

IMPLICIT NONE

HasPrjReference = Prj%Lat0 /= NOT_SET_R .AND. &
                  Prj%Lon0 /= NOT_SET_R .AND. &
                  Prj%Xref /= NOT_SET_R .AND. &
                  Prj%Yref /= NOT_SET_R

RETURN
END

!===============================================================================

INTEGER FUNCTION SetRefGrid( grid )

USE SWIM_fi
USE SWIMparam_fd
USE GridCoord_fd
USE datums

IMPLICIT NONE

TYPE( GridCoord ), INTENT( INOUT ) :: grid

INTEGER irv
REAL    xmap, ymap, xout, yout, xEps, yEps

INTEGER, EXTERNAL :: MapLoc
INTEGER, EXTERNAL :: SWIMwarningMessage
LOGICAL, EXTERNAL :: HasPrjReference, ChkUtmRef

SetRefGrid = SWIMfailure

!------ Always define reference location for UTM

IF( Prj%coord == I_UTM )THEN

  IF( grid%coord == I_CARTESIAN )THEN

    IF( grid%lon0 /= NOT_SET_R .AND. grid%lat0 /= NOT_SET_R )THEN
      irv = UTM2LL( Prj%UTMzone,grid%x0,grid%y0,Prj%lat0,Prj%lon0 )
      IF( irv /= 0 )THEN
        CALL SWIMsetUTMerror( 'UTM2LL',irv )
        GOTO 9999
      END IF
      IF( .NOT.ChkUtmRef(  Prj%lat0,Prj%lon0,grid%lat0,grid%lon0) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetRefGrid'
        error%Message = 'Grid origin mismatch (wrong zone or non-UTM met input)'
        WRITE(error%Inform,'(A,2F8.2)') 'UTM project lat/lon=',Prj%lat0,Prj%lon0
        WRITE(error%Action,'(A,2F8.2)') 'Met/terrain lat/lon=',grid%lat0,grid%lon0
        GOTO 9999
      END IF
    END IF

    Prj%Xref = grid%x0 + 0.5*FLOAT(grid%nX-1)*grid%dx
    Prj%Yref = grid%y0 + 0.5*FLOAT(grid%nY-1)*grid%dy
    irv = UTM2LL( Prj%UTMzone,Prj%Xref,Prj%Yref,Prj%lat0,Prj%lon0 )
    IF( irv /= 0 )THEN
      CALL SWIMsetUTMerror( 'UTM2LL',irv )
      GOTO 9999
    END IF

  ELSE IF( grid%coord == I_UTM )THEN

    IF( Prj%UTMzone /= grid%UTMzone )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SetRefGrid'
      error%Message = 'UTM zone mismatch'
      WRITE(error%Inform,'(A,I3)') 'Project     zone=',Prj%UTMzone
      WRITE(error%Action,'(A,I3)') 'Met/terrain zone=',grid%UTMzone
      GOTO 9999
    END IF

    Prj%Xref = grid%x0 + 0.5*FLOAT(grid%nX-1)*grid%dx
    Prj%Yref = grid%y0 + 0.5*FLOAT(grid%nY-1)*grid%dy
    irv = UTM2LL( Prj%UTMzone,Prj%Xref,Prj%Yref,Prj%lat0,Prj%lon0 )
    IF( irv /= 0 )THEN
      CALL SWIMsetUTMerror( 'UTM2LL',irv )
      GOTO 9999
    END IF

  ELSE IF( grid%coord == I_LATLON )THEN

    Prj%lon0 = grid%lon0 + 0.5*FLOAT(grid%nx-1)*grid%dx
    Prj%lat0 = grid%lat0 + 0.5*FLOAT(grid%ny-1)*grid%dy
    irv = LL2UTM( Prj%lat0,Prj%lon0,Prj%UTMzone,Prj%Xref,Prj%Yref )
    IF( irv /= 0 )THEN
      CALL SWIMsetUTMerror( 'LL2UTM',irv )
      GOTO 9999
    END IF

  END IF

!------ Otherwise, set based on met grid if not already set

ELSE

  IF( Prj%lon0 == NOT_SET_R .OR. Prj%lat0 == NOT_SET_R )THEN

    IF( grid%lon0 /= NOT_SET_R .AND. grid%lat0 /= NOT_SET_R )THEN
      Prj%lon0 = grid%lon0
      Prj%lat0 = grid%lat0
      IF( grid%coord == I_CARTESIAN )THEN
        Prj%Xref = grid%x0
        Prj%Yref = grid%y0
      END IF
    END IF

  ELSE
!------ Check for consistency between project and met reference locations

    SELECT CASE( grid%coord )
      CASE( I_CARTESIAN,I_METERS,I_UTM )

      IF( grid%lon0 /= NOT_SET_R .AND. grid%lat0 /= NOT_SET_R )THEN

        irv = MapLoc( I_CARTESIAN,grid%x0,grid%y0,I_LATLON,xout,yout,xmap,ymap )
        IF( irv /= SWIMsuccess )GOTO 9999

        xEps = 0.1*FLOAT(MAX(grid%nx-1,1)) * grid%dx * xmap
        yEps = 0.1*FLOAT(MAX(grid%ny-1,1)) * grid%dy * ymap

        IF( ABS(grid%lon0-xout) > xEps .OR. ABS(grid%lat0-yout) > yEps )THEN

          error%Number  = WN_ERROR
          error%Routine = 'SetRefGrid'
          WRITE(error%Message,'(A,4F10.2)') 'Met lat,lon,x,y     : ',grid%lat0,grid%lon0,grid%x0,grid%y0
          WRITE(error%Inform ,'(A,4F10.2)') 'Project lat,lon,x,y : ',Prj%lat0,Prj%lon0,Prj%xref,Prj%yref
          error%Action = 'Reference location mismatch: Continue using met reference?'

          irv = SWIMwarningMessage( 'Reference location mismatch' )
          IF( irv /= SWIMsuccess )GOTO 9999

          Prj%lon0 = grid%lon0
          Prj%lat0 = grid%lat0
          Prj%xref = grid%x0
          Prj%yref = grid%y0

        END IF

      END IF

    END SELECT

  END IF

END IF

SetRefGrid = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

LOGICAL FUNCTION ChkUtmRef( lat0,lon0,xlat0,xlon0 )

REAL, PARAMETER :: TOL = 0.001

REAL, INTENT( IN ) :: lat0,lon0,xlat0,xlon0

REAL del

del = MAX(ABS(xlat0-lat0),ABS(xlon0-lon0))

ChkUtmRef = ( MIN(del,360.-del) < TOL )

RETURN
END

!===============================================================================

INTEGER FUNCTION CheckLon( x,xmin,xmax )

USE SWIM_fi
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

REAL, INTENT( INOUT ) :: x
REAL, INTENT( IN    ) :: xmin, xmax

INTEGER, PARAMETER :: MAX = 3

INTEGER icount

CHARACTER(80) xsav

CheckLon = SWIMresult

IF( x == DEFERRED_R )GOTO 9999

icount = 0
WRITE(xsav,*) x

IF( x < xmin )THEN
  DO WHILE( xmin - x > 180. .AND. icount < MAX)
    x = x + 360.
    icount = icount + 1
  END DO
ELSE IF( x > xmax )THEN
  DO WHILE( x - xmax > 180. .AND. icount < MAX)
    x = x - 360.
    icount = icount + 1
  END DO
END IF

IF( icount >= MAX )THEN
  error%Number  = UK_ERROR
  error%Routine = 'check_lon'
  error%Message = 'Invalid longitude='//TRIM(xsav)
  CheckLon = SWIMfailure
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION dist2( x,y,xi,yi,xfac,yfac )

IMPLICIT NONE

REAL, INTENT( IN ) :: x, y, xi, yi, xfac, yfac

dist2 = ((x-xi)/xfac)**2 + ((y-yi)/yfac)**2

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION JulianPrj( t ) RESULT( jday )

!------ Set julian day based on project time

USE SWIM_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: t   !Project time

INTEGER iyr, nday

INTEGER, EXTERNAL :: days_in_year

jday = 0

IF( Prj%julStart == NOT_SET_I )RETURN

jday = Prj%julStart + INT( (Prj%hourStart + t/3600.)/24. )

iyr = Prj%yearStart
IF( iyr /= NOT_SET_I )THEN
  nday = days_in_year( iyr )
ELSE
  nday = 365
END IF

DO WHILE( jday > nday )
  jday = jday - nday
  IF( iyr /= NOT_SET_I )THEN
    iyr = iyr + 1
    nday = days_in_year( iyr )
  ELSE
    nday = 365
  END IF
END DO

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckUTMref( lat0,lon0,xlat0,xlon0 )

REAL, PARAMETER :: TOL = 0.1

REAL, INTENT( IN ) :: lat0,lon0,xlat0,xlon0

REAL del

del = MAX(ABS(xlat0-lat0),ABS(xlon0-lon0))

CheckUTMref = ( MIN(del,360.-del) < TOL )

RETURN
END

!===============================================================================

SUBROUTINE SetSkip( nx,ny,max1D,iskp,nxo,nyo )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: nx, ny, max1D
INTEGER, INTENT( OUT ) :: iskp, nxo, nyo

REAL arg

IF( nx == 1 .OR. ny == 1 )THEN

  iskp = 1
  nxo  = nx
  nyo  = ny

ELSE

  arg  = FLOAT( MAX(nx,ny) ) / FLOAT( max1D )
  iskp = CEILING( arg  )
  iskp = MIN(iskp,nx-1,ny-1)
  nxo  = (nx+iskp-1)/iskp
  nyo  = (ny+iskp-1)/iskp

END IF

RETURN
END

!==============================================================================

SUBROUTINE smooth( f,nx,ny )

!------ Apply 1-2-1 filter in 2D

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: nx, ny
REAL , DIMENSION(*), INTENT( INOUT ) :: f

REAL, ALLOCATABLE, DIMENSION(:) :: tem

INTEGER i,j,jm,jp,i0,ip0,im0,im,ip,ios

ALLOCATE( tem(nx*ny),STAT=ios )
IF( ios /= 0 )RETURN

DO i = 1,nx*ny
  tem(i) = f(i)
END DO

DO j = 1,ny
  jm  = MAX(j-1,1)
  jp  = MIN(j+1,ny)
  i0  = (j-1)*nx
  ip0 = (jm-1)*nx
  im0 = (jp-1)*nx
  DO i = 1,nx
    im = MAX(i-1,1)
    ip = MIN(i+1,nx)
    f(i0+i) = 4.*tem(i0+i) + &
      2.*(tem(im0+i)+tem(ip0+i)+tem(i0+im)+tem(i0+ip)) + &
      1.*(tem(im0+im)+tem(im0+ip)+tem(ip0+im)+tem(ip0+ip))
    f(i0+i) = f(i0+i)/16.
  END DO
END DO

DEALLOCATE( tem )

RETURN
END

!==============================================================================

SUBROUTINE smoothPtr( f,nx,ny )

!------ Apply 1-2-1 filter in 2D

IMPLICIT NONE

INTEGER,        INTENT( IN ) :: nx, ny
REAL , DIMENSION(:), POINTER :: f

REAL, ALLOCATABLE, DIMENSION(:) :: tem

INTEGER i,j,jm,jp,i0,ip0,im0,im,ip,ios

ALLOCATE( tem(nx*ny),STAT=ios )
IF( ios /= 0 )RETURN

DO i = 1,nx*ny
  tem(i) = f(i)
END DO

DO j = 1,ny
  jm  = MAX(j-1,1)
  jp  = MIN(j+1,ny)
  i0  = (j-1)*nx
  ip0 = (jm-1)*nx
  im0 = (jp-1)*nx
  DO i = 1,nx
    im = MAX(i-1,1)
    ip = MIN(i+1,nx)
    f(i0+i) = 4.*tem(i0+i) + &
      2.*(tem(im0+i)+tem(ip0+i)+tem(i0+im)+tem(i0+ip)) + &
      1.*(tem(im0+im)+tem(im0+ip)+tem(ip0+im)+tem(ip0+ip))
    f(i0+i) = f(i0+i)/16.
  END DO
END DO

DEALLOCATE( tem )

RETURN
END

!===============================================================================

INTEGER FUNCTION SetTerrainGrad( grid ) RESULT( irv )

USE SWIM_fi

IMPLICIT NONE

INTERFACE
  INTEGER FUNCTION SetTerrainGrad2( gridx,gridp )
    USE SWIM_fi
    TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
    TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp
  END FUNCTION SetTerrainGrad2
END INTERFACE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

irv = SetTerrainGrad2( gridx=grid  )

RETURN
END

!===============================================================================

INTEGER FUNCTION SetTerrainGrad2( gridx,gridp )

!------ Build relative depth arrays and gradients

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp

INTEGER i, j, is, ip, jp, ispx, ispy, nxb, nyb
REAL    xmap, ymap, xbar, ybar, xb, yb, dxi, dyi, Hmax

REAL, DIMENSION(:), POINTER :: H, Hx, Hy
REAL, DIMENSION(:), POINTER :: D, Du, Dv
REAL, DIMENSION(:), POINTER :: Psrf, Px, Py

TYPE( MetGrid ), POINTER :: grid

SetTerrainGrad2 = SWIMfailure

IF( PRESENT(gridx) )THEN
  grid => gridx
ELSE IF( PRESENT(gridp) )THEN
  grid => gridp
ELSE
  GOTO 9999
END IF

!------ Assign pointers

H => grid%terrain%H; Hx => grid%terrain%Hx; Hy => grid%terrain%Hy
D => grid%terrain%D; Du => grid%terrain%Du; Dv => grid%terrain%Dv
Psrf => grid%sigma%Psrf; Px => grid%sigma%Px;   Py => grid%sigma%Py

!------ Define locals

dxi = 1./grid%dX
dyi = 1./grid%dY

nxb = grid%nX
nyb = grid%nY

!------ Arrays for sigma (pressure) coordinates

IF( ANY(BTEST(grid%type,(/GTB_SIGMA,GTB_Z3D/))) )THEN

!------ D is pressure difference between domain top and surface

  DO is = 1,grid%nXY
    D(is) = 1.0 !grid%sigma%Psrf(is) - grid%sigma%Ptop
  END DO

ELSE ! Terrain-following height coordinates

!------ Check terrain height

  Hmax = 0.
  DO i = 1,grid%nXY
    Hmax = MAX(Hmax,H(i))
  END DO

  IF( grid%Ztop < 1.5*Hmax )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetTerrainGrad'
    error%Message = 'Top of vertical grid too low'
    error%Inform  = 'Must exceed twice maximum terrain elevation'
    WRITE(error%Action,FMT="('Max. elev.=',ES11.3,' Grid top=',ES11.3)") Hmax,grid%Ztop
    GOTO 9999
  END IF

!------ D at grid "center" location

  DO is = 1,grid%nXY
    D(is) = 1.0 - H(is)/grid%Ztop
  END DO

END IF
!------ D and gradients at staggered (u,v) locations

DO i = 1,nxb
  ip   = MIN(i+1,nxb)
  xb   = grid%Xmin + FLOAT(i-1)*grid%dX
  xbar = xb + 0.5*grid%dX

  DO j = 1,nyb
    jp   = MIN(j+1,nyb)
    yb   = grid%Ymin + FLOAT(j-1)*grid%dY
    ybar = yb + 0.5*grid%dY

    is   = (j -1)*nxb + i
    ispx = (j -1)*nxb + ip
    ispy = (jp-1)*nxb + i

    CALL SWIMmapfac( grid%coord,xbar,yb,xmap,ymap )
    IF( BTEST(grid%type,GTB_SIGMA) )THEN
      Px(is) = -2.*(Psrf(ispx)-Psrf(is))*dxi*xmap / (Psrf(ispx)+Psrf(is))
    END IF
    Hx(is) = (H(ispx)-H(is))*dxi*xmap

    CALL SWIMmapfac( grid%coord,xb,ybar,xmap,ymap )

    IF( BTEST(grid%type,GTB_SIGMA) )THEN
      Py(is) = -2.*(Psrf(ispy)-Psrf(is))*dyi*ymap / (Psrf(ispy)+Psrf(is))
    END IF
    Hy(is) = (H(ispy)-H(is))*dyi*ymap

    IF( BTEST(grid%type,GTB_STAGGERB) )THEN
      Du(is) = 0.25*(D(is)+D(ispx)+D(ispy)+D(ispy-i+ip))
      Dv(is) = Du(is)
    ELSE IF( BTEST(grid%type,GTB_STAGGER) )THEN
      Du(is) = 0.5*(D(ispx)+D(is))
      Dv(is) = 0.5*(D(ispy)+D(is))
    ELSE
      Du(is) = D(is)
      Dv(is) = D(is)
    END IF

  END DO

END DO

SetTerrainGrad2 = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetFieldGridMapType( type )

!------ Set field grid map bits based on project coordinates

USE SWIM_fi
USE SWIMparam_fd

INTEGER, INTENT( INOUT ) :: type

SELECT CASE( Prj%coord )
  CASE( I_LATLON )
    type = IBSET(type,GTB_LATLON)
  CASE( I_UTM )
    type = IBSET(type,GTB_UTM)
  CASE( I_CARTESIAN )
    type = IBSET(type,GTB_CARTESIAN)
  CASE( I_METERS )
    type = IBSET(type,GTB_METERS)
END SELECT

RETURN
END

!==============================================================================

INTEGER FUNCTION SetGridLL( grid )

!------ Set grid lat/lon arrays if possible

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER irv, alloc_stat, i, j, i0
REAL    xm, ym

TYPE( MapCoord ) :: coord

INTEGER, EXTERNAL :: SWIMcnvCoord
LOGICAL, EXTERNAL :: CheckRef

SetGridLL     = SWIMfailure  !Initialize failure
error%Number  = UK_ERROR
error%Routine = 'SetGridLL'

!----- Only need to set lat/lon type for output coordinate

coord%type = I_LATLON

!------ Cannot set lat/lon for Cartesian grid w/o reference location

IF( .NOT.BTEST(grid%type,GTB_CARTESIAN) .OR. CheckRef(grid%coord%reference) )THEN

!------ Allocate 2d lat/lon arrays

  ALLOCATE( grid%lon(grid%nXY), &
            grid%lat(grid%nXY), &
            grid%sunrise(grid%nXY), &
            grid%sunset(grid%nXY),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Message = 'Error allocating lat/lon grid'
    GOTO 9999
  END IF

  IF( grid%nXY == 1 )THEN
    xm = 0.5*(grid%Xmin+grid%Xmax)
    ym = 0.5*(grid%Ymin+grid%Ymax)
    irv = SWIMcnvCoord( xm,ym,grid%coord,grid%lon(1),grid%lat(1),coord )
  ELSE
    DO j = 1,grid%nY
      i0 = (j-1)*grid%nX
      ym = grid%Ymin + FLOAT(j-1)*grid%dY
      DO i = 1,grid%nX
        xm = grid%Xmin + FLOAT(i-1)*grid%dX
        irv = SWIMcnvCoord( xm,ym,grid%coord,grid%lon(i0+i),grid%lat(i0+i),coord )
        IF( irv /= SWIMsuccess )GOTO 9999
      END DO
    END DO
  END IF

END IF

!---- Allocate solar elevation field for activity decay

IF( Prj%decay )THEN
  ALLOCATE( grid%sunfac(grid%nXY),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Message = 'Error allocating decay factor grid'
    GOTO 9999
  END IF
END IF

SetGridLL = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!===============================================================================

REAL FUNCTION GetTimeMet( iday,imonth,iyear,ihour,imin,isec,t_offset ) RESULT( t )

!------ Convert to SCIPUFF time

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iday, imonth, iyear, ihour, imin, isec
REAL,    INTENT( IN ) :: t_offset

INTEGER iyy, jul, jyy, iyy0

INTEGER, EXTERNAL :: julian_day, days_in_year

iyy = iyear; CALL SetYear( iyy )

IF( iyy /= NOT_SET_I .AND. imonth /= NOT_SET_I )THEN

  jul  = julian_day( imonth,iday,iyy )
  iyy0 = Prj%yearStart

  IF( iyy > iyy0 )THEN
    DO jyy = iyy0,iyy-1
      jul = jul + days_in_year( jyy )
    END DO
  ELSE IF( iyy < iyy0 )THEN
    DO jyy = iyy0-1,iyy,-1
      jul = jul - days_in_year( jyy )
    END DO
  END IF

ELSE

  jul = iday

END IF

t = (jul - Prj%julStart)*86400. + ihour*3600. + imin*60. &
                                      + isec - t_offset
IF( Reverse )t = -t

RETURN
END

!==============================================================================

REAL FUNCTION SWIMtimeOffset()

!------ Set time offset based on project time

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

IF( Prj%local .EQV. Prj%localMet )THEN
  SWIMtimeOffset = Prj%hourStart*3600.
ELSE
  IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
    error%Number   = IV_ERROR
    error%Routine  = 'SWIMtimeOffset'
    error%Message  = 'Time zone not set'
    SWIMtimeOffset = NOT_SET_R
    GOTO 9999
  END IF
  IF( Prj%local )THEN
    SWIMtimeOffset = (Prj%hourStart - Prj%timeZone)*3600.
  ELSE
    SWIMtimeOffset = (Prj%hourStart + Prj%timeZone)*3600.
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION ObsTimeOffset( ObsLocal )

!------ Set time offset based on project time

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: ObsLocal

IF( Prj%local .EQV. ObsLocal )THEN
  ObsTimeOffset = Prj%hourStart*3600.
ELSE
  IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
    error%Number   = IV_ERROR
    error%Routine  = 'ObsTimeOffset'
    error%Message  = 'Time zone not set'
    ObsTimeOffset = NOT_SET_R
    GOTO 9999
  END IF
  IF( Prj%local )THEN
    ObsTimeOffset = (Prj%hourStart - Prj%timeZone)*3600.
  ELSE
    ObsTimeOffset = (Prj%hourStart + Prj%timeZone)*3600.
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE TimeConvert( ti,local_out,lymd_out,hour,min,sec,year,month,day,string )

USE SWIM_fi

!------ Convert "time since start" to local or UTC
!       Output year, month, day, hour, min, sec if appropriate

IMPLICIT NONE

REAL,         INTENT( IN  ) :: ti
LOGICAL,      INTENT( IN  ) :: local_out, lymd_out
INTEGER,      INTENT( OUT ) :: hour, min, sec, year, month, day
CHARACTER(*), INTENT( OUT ) :: string

CHARACTER(3), DIMENSION(12), PARAMETER ::  name_month = (/ &
                                            'JAN','FEB','MAR','APR','MAY','JUN', &
                                            'JUL','AUG','SEP','OCT','NOV','DEC' /)

CHARACTER(1) symbol, day_thing

REAL to, tx, rmin, rsec

IF( Reverse )THEN
  to = -ti + 0.5  !Add 1/2 second for rounding
ELSE
  to = ti + 0.5
END IF

IF( local_out )THEN
  IF( Prj%local )THEN
    to = to/3600. + Prj%hourStart
  ELSE
    to = to/3600. + Prj%hourStart + Prj%timeZone
  END IF
  symbol = 'L'
ELSE
  IF( Prj%local )THEN
    to = to/3600. + Prj%hourStart - Prj%timeZone
  ELSE
    to = to/3600. + Prj%hourStart
  END IF
  symbol = 'Z'
END IF

IF( lymd_out )THEN

  CALL YearMonthDay( to,Prj%yearStart,Prj%monthStart,Prj%dayStart,tx,year,month,day )

  IF( year >= 2000 )THEN
    year = year - 2000
  ELSE IF( year >= 1900 )THEN
    year = year - 1900
  END IF

  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  rsec = INT(60.*(rmin-FLOAT(min)))
  sec  = INT(rsec)

  WRITE(string,100) day,name_month(month),year,hour,min,sec,symbol

ELSE

  year  = NOT_SET_I
  month = NOT_SET_I

  day  = INT(to/24.)
  tx   = to - FLOAT(day)*24.
  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  rsec = INT(60.*(rmin-FLOAT(min)))
  sec  = NINT(rsec)

  IF( day == 0 )THEN
    day_thing ='-'
  ELSE
    day_thing ='+'
  END IF

  IF( day < 10 )THEN
    WRITE(string,101) day_thing,day,hour,min,sec,symbol
  ELSE IF( day < 100 )THEN
    WRITE(string,102) day_thing,day,hour,min,sec,symbol
  ELSE IF( day < 1000 )THEN
    WRITE(string,103) day_thing,day,hour,min,sec,symbol
  END IF

100 FORMAT(I2.2,'-',A3,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2,A1)
101 FORMAT('DAY',A1,I1.1,' ',I2.2,':',I2.2,':',I2.2,A1)
102 FORMAT('DAY',A1,I2.2,' ',I2.2,':',I2.2,':',I2.2,A1)
103 FORMAT('DAY',A1,I3.3,' ',I2.2,':',I2.2,':',I2.2,A1)

END IF

RETURN
END

!=============================================================================

SUBROUTINE YearMonthDay( t,yr,mnth,day,to,yro,mntho,dayo )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: yr ,mnth ,day
REAL,    INTENT( IN  ) :: t
INTEGER, INTENT( OUT ) :: yro,mntho,dayo
REAL,    INTENT( OUT ) :: to

INTEGER days, iday, jul, julo
LOGICAL leap

INTEGER, EXTERNAL :: julian_day
LOGICAL, EXTERNAL :: leap_year

INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
           (/0,31,59,90,120,151,181,212,243,273,304,334/)

iday = INT(t/24.)
to   = t - 24.*FLOAT(iday)
IF( to < 0. )THEN
  iday = iday - 1
  to = to + 24.
END IF

jul  = julian_day( mnth,day,yr )
julo = jul + iday

yro = yr

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo <= 0 )
  julo = julo + days
  yro  = yro - 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo > days )
  julo = julo - days
  yro  = yro + 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

DO mntho = 12,1,-1
  IF( mntho >= 3 .AND. leap )THEN
    days = NDAY(mntho) + 1
  ELSE
    days = NDAY(mntho)
  END IF
  IF( julo > days )EXIT
END DO

dayo = julo - days

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION LocalTime( t )

!------ Define local time of day in hours

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t  !Project time in seconds

LocalTime = t/3600. + Prj%hourStart

IF( .NOT.Prj%local )THEN
  IF( Prj%TimeZone == NOT_SET_R .OR. Prj%TimeZone == DEF_VAL_R )THEN
    LocalTime = NOT_SET_R
    RETURN
  END IF
  LocalTime = LocalTime + Prj%TimeZone
END IF

LocalTime = MODULO( LocalTime,24. )

RETURN
END

!==============================================================================

REAL FUNCTION UTCtime( t )

!------ Define UTC time of day in hours

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t  !Project time in seconds

UTCtime = t/3600. + Prj%hourStart

IF( Prj%local )THEN
  IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
    UTCtime = NOT_SET_R
    RETURN
  END IF
  UTCtime  = UTCtime - Prj%timeZone
END IF

UTCtime = MODULO( UTCtime,24. )

RETURN
END

!===============================================================================

INTEGER FUNCTION CheckMet( fld,grid )

!------ Check for patently ridiculous values in met fields

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( IN ) :: fld
TYPE( MetGrid   ), INTENT( IN ) :: grid

INTEGER nxy, k, i0, k1, nz, i, j, nc
REAL    u2, tmp, z

CheckMet = SWIMfailure

error%Number  = IV_ERROR
error%Routine = 'CheckMet'

!------ Ignore first level for staggered grid

nxy = grid%nXY

IF( BTEST(grid%type,GTB_STAGGER) )THEN
  k1 = nxy
  nz = grid%nZ - 1
ELSE
  k1 = 0
  nz = grid%nZ
END IF

!------ Check temperature

IF( ASSOCIATED(fld%Tpot) )THEN

  error%Message = 'Background potential temperature > '; j = LEN_TRIM(error%Message) + 1

  ZloopT : DO k = 1,nz
    i0 = (k-1)*nxy+ k1

    DO i = 1,nxy

      IF( BTEST(grid%type,GTB_SIGMA) )THEN
        z = grid%sigma%Z((i0+i))
      ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN
        IF(  BTEST(grid%type,GTB_Z3DW) )THEN
          z = 0.5*(fld%Z(i0+i)+fld%Z(i0+i+nxy))
        ELSE
          z = fld%Z(i0+i)
        END IF
      ELSE
        z = grid%Z(k)*grid%terrain%D(i)
      END IF
      IF( z <= 0. )CYCLE

      z = z + grid%terrain%h(i) + grid%Hmin

      IF( z > ZMAX_CHECK )EXIT ZloopT

      IF( fld%Tpot(i0+i) > MAXLIM_TPOT )THEN
        tmp = MAXLIM_TPOT
      ELSE IF( fld%Tpot(i0+i) < MINLIM_TEMP )THEN
        tmp = MINLIM_TEMP; error%Message(j-1:j-1) = '<'
      ELSE
        CYCLE
      END IF

      CALL c_format( tmp,nc,error%Message(j+1:) )
      error%Message = TRIM(error%Message)//' K'

      z = z - (grid%terrain%h(i) + grid%Hmin)
      error%Inform = 'Height (AGL) = '; j = LEN_TRIM(error%Inform) + 1
      CALL c_format( z,nc,error%Inform(j+1:) )
      error%Inform = TRIM(error%Inform)//' m  Potential Temp = '; j = LEN_TRIM(error%Inform) + 1
      CALL c_format( fld%Tpot(i0+i),nc,error%Inform(j+1:) )

      GOTO 9999
    END DO

  END DO ZloopT

END IF


!------ Check velocity


ZloopU : DO k = 1,nz
  i0 = (k-1)*nxy+ k1

  DO i = 1,nxy

    IF( BTEST(grid%type,GTB_SIGMA) )THEN
      z = grid%sigma%Z((i0+i))
    ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN
      IF(  BTEST(grid%type,GTB_Z3DW) )THEN
        z = 0.5*(fld%Z(i0+i)+fld%Z(i0+i+nxy))
      ELSE
        z = fld%Z(i0+i)
      END IF
    ELSE
      z = grid%Z(k)*grid%terrain%D(i)
    END IF
    z = z + grid%terrain%h(i) + grid%Hmin

    IF( z > ZMAX_CHECK )EXIT ZloopU

    u2 = fld%U(i0+i)**2+fld%V(i0+i)**2

    IF( u2 <= MAXLIM_VEL2 )CYCLE

    error%Message = 'Background wind speed > '; j = LEN_TRIM(error%Message) + 1
    CALL c_format( MAXLIM_VEL,nc,error%Message(j+1:) )
    error%Message = TRIM(error%Message)//' m/s'

    z = z - (grid%terrain%h(i) + grid%Hmin)
    error%Inform = 'Height (AGL) = '; j = LEN_TRIM(error%Inform) + 1
    CALL c_format( z,nc,error%Inform(j+1:) )
    error%Inform = TRIM(error%Inform)//' m  Speed = '; j = LEN_TRIM(error%Inform) + 1
    CALL c_format( SQRT(u2),nc,error%Inform(j+1:) )

    GOTO 9999
  END DO

END DO ZloopU


CheckMet = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION Z2Sigmac( z,ps,pt,a1,a2 ) RESULT( sigmac)

!------ Compute complement sigma (1-sigma) for a given height and
!       surface pressure; pt, a1, a2 are reference profile parameters

IMPLICIT NONE

REAL, INTENT( IN  ) :: z              !Height (meters - MSL)
REAL, INTENT( IN  ) :: ps, pt, a1, a2

REAL p

p      = -(a1 + SQRT(a1*a1 + 4.*a2*z))/ (2.*a2)
p      = EXP(p)
sigmac = 1. - (p-pt) / (ps-pt)

RETURN
END

!==============================================================================

SUBROUTINE SetOmega( grid,u,v,w,ww,inverse )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ),   INTENT( IN ) :: grid
REAL, DIMENSION(:),     POINTER :: u, v, w, ww
LOGICAL, OPTIONAL, INTENT( IN ) :: inverse      !Set w from omega(ww), if true

REAL, DIMENSION(:), POINTER :: d, d1, d2
REAL, DIMENSION(:), POINTER :: ddx, ddy
REAL, DIMENSION(:), POINTER :: z
REAL, DIMENSION(:), POINTER :: zb, ps
REAL, DIMENSION(:), POINTER :: wIn, wOut

INTEGER ix, jy, k, ixm, jym, is, ism, jsm, i, ip, im, jm, ipm, jpm, i0, j0, jm0
INTEGER imjm, ixp, jyp, jp0, isp, jsp
INTEGER nxy, nx, ny, nz
REAL    zz, tem1, tem2, fac, facz, zbtop
REAL    up, um, vp, vm, upp, ump, vpp, vmp, zzw
LOGICAL lW

IF( PRESENT(inverse) )THEN
  lW = inverse
ELSE
  lW = .FALSE.
END IF

IF( lW )THEN
  wIn => ww; wOut => w
  fac = -1.
ELSE
  wIn => w ; wOut => ww
  fac = 1.
END IF

nx  = grid%nX
ny  = grid%nY
nz  = grid%nZ
nxy = grid%nXY

d  => grid%terrain%D
d1 => grid%terrain%Du
d2 => grid%terrain%Dv

IF( BTEST(grid%type,GTB_SIGMA) )THEN

  ddx => grid%sigma%Px
  ddy => grid%sigma%Py
  ps  => grid%Sigma%Psrf
  zbtop = 1.0

ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN            !wOut=wIn if vertical coord is 3d Z

  IF( BTEST(grid%type,GTB_STAGGERZ) )nz = nz + 1
  DO k = 1,nz
    i0 = (k-1)*nxy
    DO is = 1,nxy
      i = i0 + is
      wOut(i) = wIn(i)
    END DO
  END DO
  GOTO 9999

ELSE

  ddx => grid%terrain%Hx
  ddy => grid%terrain%Hy
  zbtop = grid%Ztop
  facz  = 1.0

END IF

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN

  nz = nz+1
  z  => grid%Zw
  zb => grid%Z

  IF( lW )THEN
    DO i = 1,nxy
      wIn(i) = 0.
    END DO
  END IF

!$OMP PARALLEL SHARED( wOut,wIn,grid,nz,nxy,ny,nx,z,zb,zbtop,fac,u,v,ddx,ddy )
!$OMP DO PRIVATE(k,i0,zz,zzw,jy,jym,jyp,j0,jm0,jp0,ix,ixm,ixp,is,ism,isp,i,im,ip,&
!$OMP            ipm,jsm,jm,jsp,jpm,imjm,upp,ump,up,um,vpp,vmp,vp,vm,facz,tem1,tem2)
  DO k = 1,nz-1
    i0 = (k-1)*nxy
    zz = 0.25*(1.-z(k)/zbtop)*fac
    zzw = (1.-z(k))*fac

    DO jy = 1,ny

      jym = MAX(jy-1,1)
      jyp = MIN(jy+1,ny)

      j0  = (jy-1)*nx
      jm0 = (jym-1)*nx
      jp0 = (jyp-1)*nx

      DO ix = 1,nx

        ixm = MAX(ix-1,1)
        ixp = MIN(ix+1,nx)

        is  = j0 + ix
        ism = j0 + ixm
        isp = j0 + ixp

        i   = i0 + is
        im  = i0 + ism
        ip  = i  + nxy
        ipm = im + nxy

        jsm = jm0 + ix
        jm  = i0 + jsm
        jsp = jp0 + ix

        jpm = jm + nxy

        IF( BTEST(grid%type,GTB_STAGGERB) )THEN
          imjm = i0 + jm0 + ixm
          upp = 0.5*(u(ip) + u(jpm)); ump = 0.5*(u(ipm) + u(imjm+nxy))
          up  = 0.5*(u(i ) + u(jm )); um  = 0.5*(u(im ) + u(imjm))
          vpp = 0.5*(v(ip) + v(ipm)); vmp = 0.5*(v(jpm) + v(imjm+nxy))
          vp  = 0.5*(v(i ) + v(im )); vm  = 0.5*(v(jm ) + v(imjm))
        ELSE                        !This is for Arakawa C
          upp = u(ip); ump = u(ipm)
          up  = u(i);  um  = u(im)
          vpp = v(ip); vmp = v(jpm)
          vp  = v(i);  vm  = v(jm)
        END IF

        IF( BTEST(grid%type,GTB_SIGMA) )THEN

          IF( k == 1 )THEN
            facz = grid%sigma%Z(i)/zb(k) * zzw
          ELSE
            facz = (grid%sigma%Z(i)-grid%sigma%Z(i-nxy)) / (zb(k)-zb(k-1)) * zzw
          END IF

          tem1 = 0.25*(up+um+upp+ump)
          tem2 = 0.25*(vp+vm+vpp+vmp)

          tem1 = 0.5*tem1*(ddx(is)+ddx(ism))
          tem2 = 0.5*tem2*(ddy(is)+ddy(jsm))

          wOut(i) = wIn(i) + facz*(tem1+tem2)  !facz = -dz/d(sigma)

        ELSE

          tem1 = ddx(is)*d1(is)*(up+upp) + ddx(ism)*d1(ism)*(um+ump)
          tem2 = ddy(is)*d2(is)*(vp+vpp) + ddy(jsm)*d2(jsm)*(vm+vmp)

          wOut(i) = wIn(i) - (tem1+tem2)*zz/d(is)

        END IF

      END DO
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

  z => grid%Z

!$OMP PARALLEL SHARED( wOut,wIn,grid,z,nz,nxy,ny,nx,fac,ddx,ddy,u,v )
!$OMP DO PRIVATE(k,i0,zz,jy,j0,ix,is,i)
  DO k = 1,nz-1
    i0 = (k-1)*nxy
    zz = (1.-z(k)/grid%Ztop)*fac

    DO jy = 1,ny
      j0  = (jy-1)*nx

      DO ix = 1,nx
        is  = j0 + ix
        i   = i0 + is

        wOut(i) = wIn(i) - (ddx(is)*u(i) + ddy(is)*v(i))*zz

      END DO
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

END IF

IF( .NOT.lW )THEN
  DO i = 1,nxy
    wOut(i) = 0.
  END DO
END IF

i0 = (nz-1)*nxy
DO is = 1,nxy
  wOut(i0+is) = wOut(i0+is-nxy)
END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetSigmaZw( grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER is, k, ip, nxy

REAL, DIMENSION(:), POINTER :: z, zw

nxy = grid%nXY

z  => grid%sigma%Z
zw => grid%sigma%Zw

DO is = 1,nxy
  ip = is
  zw(ip) = 0.
  DO k = 2,grid%nZ
    ip = ip + nxy
    zw(ip) = 0.5*(z(ip)+z(ip-nxy))
  END DO
  zw(ip+nxy) = 2.*z(ip)-zw(ip)
END DO

RETURN
END
!===============================================================================

SUBROUTINE SetSigmaZ( grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER is, k, ip, nxy

REAL, DIMENSION(:), POINTER :: z, zw

nxy = grid%nXY

z  => grid%sigma%Z
zw => grid%sigma%Zw

DO is = 1,nxy
  DO k = 1,grid%nZ
    ip = (k-1)*nxy + is
    z(ip) = 0.5*(zw(ip)+zw(ip+nxy))
    ip = ip + nxy
  END DO
END DO

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION getFieldIndex( i ) RESULT( index )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i

index = IBITS(i,POS_INDEX,LEN_INDEX)

RETURN
END

!===============================================================================

SUBROUTINE setFieldIndex( index,i )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: index
INTEGER, INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_INDEX,index,POS_INDEX)

RETURN
END

!===============================================================================

INTEGER FUNCTION getSmoothIndex( i ) RESULT( index )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i

index = IBITS(i,POS_SMOOTH,LEN_SMOOTH)

RETURN
END

!===============================================================================

SUBROUTINE SetSmoothIndex( index,i )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: index
INTEGER, INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_SMOOTH,index,POS_SMOOTH)

RETURN
END

!==============================================================================

SUBROUTINE SetHmin( grid )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER i

grid%Hmin = HUGE(0.); grid%Hmax = -HUGE(0.)
DO i = 1,grid%nXY
  grid%Hmin = MIN(grid%terrain%H(i),grid%Hmin)
  grid%Hmax = MAX(grid%terrain%H(i),grid%Hmax)
END DO

DO i = 1,grid%nXY
  grid%terrain%H(i) = grid%terrain%H(i) - grid%Hmin
END DO

grid%Hmax = grid%Hmax - grid%Hmin

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckInDomain( xi,yi,grid ) RESULT( lIn )

USE SWIMmetField_fd

IMPLICIT NONE

REAL,            INTENT( IN ) :: xi, yi
TYPE( MetGrid ), INTENT( IN ) :: grid

IF( grid%nXY == 1 )THEN
  lIn = .TRUE.
ELSE
  lIn = .FALSE.
  IF( xi >= grid%xmin )THEN
    IF( xi <= grid%xmax )THEN
      IF( yi >= grid%ymin )THEN
        lIn = yi <= grid%ymax
      END IF
    END IF
  END IF
END IF

RETURN
END

!===============================================================================

SUBROUTINE CheckDomMinMax( x,y,xmin,ymin,xmax,ymax )

IMPLICIT NONE

REAL, INTENT( IN    ) :: x, y
REAL, INTENT( INOUT ) :: xmin, ymin, xmax, ymax

xmin = MIN(xmin,x); ymin = MIN(ymin,y)
xmax = MAX(xmax,x); ymax = MAX(ymax,y)

RETURN
END

!==============================================================================

REAL FUNCTION SWIMsetHmin()

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER i

!------ Overall minimum elevation

SWIMsetHmin = HUGE(1.)

DO i = 1,numField
  IF( .NOT.BTEST(field(i)%status,FSB_TERRAIN) )THEN
    SWIMsetHmin = MIN(SWIMsetHmin,field(i)%grid%Hmin)
  END IF
END DO

RETURN
END
