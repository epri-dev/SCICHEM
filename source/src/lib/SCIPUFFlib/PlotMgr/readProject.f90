SUBROUTINE ReadProject( Project )

!------ Read project

USE prjstruct_fd
USE scipuff_fi
USE SCIMgr_fi

IMPLICIT NONE

TYPE( projectIDT ), INTENT( IN ) :: Project

CHARACTER(PATH_MAXLENGTH) string
CHARACTER(PATH_MAXLENGTH) AddExtension

string = TRIM(Project%name)
CALL AddPath( string,TRIM(Project%path) )
string = TRIM(AddExtension(string,'inp'))
CALL SetFileNamesT( string )
CALL SetFileUnitsT()

CALL SetupStopFiles()

CALL set_version( iversion_code )

CALL read_prj()
IF( nError == VN_ERROR )THEN !Ignore version error here
  CALL init_error()          !since ok for plotting
END IF

!------ Load incidents

ProjectID = Project

RETURN
END

!==============================================================================

INTEGER FUNCTION GetPrjCoordF( userID,np,lon,lat,xp,yp )

!------ Convert (lon,lat) to project coordinates

USE SCIMgr_fd
USE SCIMgrState
USE abort
USE scipuff_fi
USE met_fi

IMPLICIT NONE

INTEGER,             INTENT( IN  ) :: userID        !Caller ID
INTEGER,             INTENT( IN  ) :: np
REAL, DIMENSION(np), INTENT( IN  ) :: lon, lat
REAL, DIMENSION(np), INTENT( OUT ) :: xp, yp

INTEGER i, irv, currentState

TYPE( MapCoord ) :: coord

INTEGER, EXTERNAL :: SWIMcnvCoord

GetPrjCoordF = SCIPfailure

IF( lmap == I_LATLON )THEN
  DO i = 1,np
    xp(i) = lon(i)
    yp(i) = lat(i)
  END DO
  GetPrjCoordF = SCIPsuccess
  RETURN  !No conversion necessary
END IF

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( Aborted() )GOTO 9999

!------ Setup lat/lon coordinate structure

coord%type          = I_LATLON
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

!------ Loop over points

DO i = 1,np
  xp(i) = NOT_SET_R; yp(i) = NOT_SET_R
  irv = SWIMcnvCoord( lon(i),lat(i),coord,xp(i),yp(i),PrjCoord )
  IF( xp(i) == NOT_SET_R )GOTO 9999
END DO

GetPrjCoordF = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
