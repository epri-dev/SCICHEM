!===============================================================================

SUBROUTINE map_loc( map_in,xin,yin,map_out,xout,yout,xmap,ymap )

USE scipuff_fi

!------ returns location in output coordinate system (Cartesian or Lat/lon)
!       as well as the map transformation.  Cartesian coordinates are assumed
!       to be in km.  The reference lat/lon must be defined prior to calling
!       this routine.

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: map_in, map_out
REAL,    INTENT( IN  ) :: xin, yin
REAL,    INTENT( OUT ) :: xout, yout, xmap, ymap

REAL, EXTERNAL :: cosd

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
  IF( lon0 == NOT_SET_R .OR. lat0 == NOT_SET_R .OR. &
      lon0 == DEF_VAL_R .OR. lat0 == DEF_VAL_R )THEN
    nError   = IV_ERROR
    eRoutine = 'map_loc'
    eMessage = 'Must set reference location for LL/Cartesian transformation'
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
  xmap = ymap/cosd( lat0 )
  xout = lon0 + (xin - xref)*xmap
  yout = lat0 + (yin - yref)*ymap

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
  ymap = SPHFAC * xmap
  xmap = ymap*cosd( lat0 )
  xout = xref + (xin - lon0)*xmap
  yout = yref + (yin - lat0)*ymap

ELSE IF( map_in == I_CARTESIAN .OR. map_in == I_UTM )THEN

!------ if input is Cartesian and output is Meters:

  xmap = 1.E3
  ymap = 1.E3
  xout = xin*xmap
  yout = yin*ymap

ELSE

!------ if input is Meters and output is Cartesian: error

  nError   = IV_ERROR
  eRoutine = 'map_loc'
  eMessage = 'Cannot have meters input / kilometers output'
  GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE ClearMetGrid()

USE met_fi

IMPLICIT NONE

INTEGER i, alloc_stat

IF( ALLOCATED(MetGrid) )THEN
  DO i = 1,numMet
    IF( ASSOCIATED(MetGrid(i)%zi) )DEALLOCATE( MetGrid(i)%zi,STAT=alloc_stat )
    IF( ASSOCIATED(MetGrid(i)%H ) )DEALLOCATE( MetGrid(i)%H ,STAT=alloc_stat )
    IF( ASSOCIATED(MetGrid(i)%Hx) )DEALLOCATE( MetGrid(i)%Hx,STAT=alloc_stat )
    IF( ASSOCIATED(MetGrid(i)%Hy) )DEALLOCATE( MetGrid(i)%Hy,STAT=alloc_stat )
  END DO
  DEALLOCATE( MetGrid,STAT=alloc_stat )
END IF

IF( ALLOCATED(lMetGridComplete) )DEALLOCATE( lMetGridComplete,STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE AllocMetGridArrays( grid )

USE scipuff_fi
USE met_fi
USE error_fi
USE coordinate_fd

IMPLICIT NONE

TYPE( SWIMgridStr ), INTENT( INOUT ) :: grid

INTEGER alloc_stat, nxy

nxy = grid%nx * grid%ny

ALLOCATE( grid%zi(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'AllocMetGridArrays'
  eMessage = 'Error allocating mixing height'
  GOTO 9999
END IF

IF( grid%lter )THEN
  ALLOCATE( grid%H(nxy),grid%Hx(nxy),grid%Hy(nxy),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AllocMetGridArrays'
    eMessage = 'Error allocating terrain fields'
    GOTO 9999
  END IF
ELSE
  NULLIFY( grid%H,grid%Hx,grid%Hy )
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetPrjCoord()

USE scipuff_fi
USE met_fi

IMPLICIT NONE

PrjCoord%type          = lmap
PrjCoord%zone          = utm_zone
PrjCoord%reference%x   = xref
PrjCoord%reference%y   = yref
PrjCoord%reference%lat = lat0
PrjCoord%reference%lon = lon0

PrjCoord%Lat0   = NOT_SET_R
PrjCoord%Lon0   = NOT_SET_R
PrjCoord%Lat1   = NOT_SET_R
PrjCoord%Lat2   = NOT_SET_R
PrjCoord%Rearth = NOT_SET_R
PrjCoord%n      = NOT_SET_R
PrjCoord%f      = NOT_SET_R
PrjCoord%m0     = NOT_SET_R
PrjCoord%y0     = NOT_SET_R
PrjCoord%sp0    = NOT_SET_R
PrjCoord%cp0    = NOT_SET_R
PrjCoord%sl0    = NOT_SET_R
PrjCoord%cl0    = NOT_SET_R
PrjCoord%cc0    = NOT_SET_R
PrjCoord%sc0    = NOT_SET_R
PrjCoord%cs0    = NOT_SET_R
PrjCoord%ss0    = NOT_SET_R

RETURN
END
