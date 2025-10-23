!*******************************************************************************
!                EIS Definition module
!*******************************************************************************
MODULE eisfile_fi

  SAVE

  REAL,DIMENSION(3)    :: EIS_origin
  REAL                 :: EIS_xmap
  REAL                 :: EIS_ymap

END MODULE eisfile_fi
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildHeaderEIS
!*******************************************************************************
INTEGER FUNCTION BuildHeaderEIS( grdI,Field,nlev,level,Mxs,string )

USE EISfile_fi
USE field_fd
USE time_fi
USE project_fi
USE nextRel_fi
USE error_fi
USE files_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                      INTENT( IN )  :: grdI     !SAG grid ID
TYPE( SCIPPlotFieldT ),       INTENT( IN )  :: Field    !Field definition
INTEGER,                      INTENT( IN  ) :: nlev     !Number of contours
REAL,DIMENSION(nlev),         INTENT( IN  ) :: level    !Contour values
INTEGER,                      INTENT( IN  ) :: Mxs      !Maximum number of strings
CHARACTER(*), DIMENSION(Mxs), INTENT( OUT ) :: string   !Footer strings

!==============================================================================
! Local variables
!==============================================================================
LOGICAL lexist
INTEGER idum
INTEGER i
INTEGER ios
INTEGER hr
INTEGER min
INTEGER sec
INTEGER,DIMENSION(3) :: TOD_prj
INTEGER,DIMENSION(3) :: TOD_plt
REAL    plotTime
CHARACTER(80)  tmpstring
CHARACTER(PATH_MAXLENGTH) filestr

TYPE( releaseSpecT ) relSpec

!==============================================================================
! Finction calls
!==============================================================================
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==============================================================================
! Initialize
!==============================================================================
BuildHeaderEIS = 0

!==============================================================================
! Project Information
!==============================================================================
TOD_prj(1) = 0
TOD_prj(2) = 0
TOD_prj(3) = 0
EIS_origin(1) = 0.0
EIS_origin(2) = 0.0
EIS_origin(3) = 0.0
EIS_xmap = 1.0e-3
EIS_ymap = 1.0e-3
plotTime = MOD( Field%userTime, 24.0 )
IF( LEN_TRIM(Field%project) > 0 )THEN
  filestr = TRIM(Field%project)
  CALL AddPath( filestr,TRIM(Field%path) )
  filestr = TRIM(AddExtension( filestr,'inp' ))
  INQUIRE( FILE=filestr,EXIST=lexist )
  IF( lexist )THEN
    CALL SetFileNamesT( filestr )
    CALL SetFileUnitsT()
    CALL set_version( iversion_code )
    CALL read_prj()
    hr  =  INT(           tstart                            )
    min =  INT(      60.*(tstart - FLOAT(hr))               )
    sec = NINT( 60.*(60.*(tstart - FLOAT(hr)) - FLOAT(min)) )
    TOD_prj(1) = hr
    TOD_prj(2) = min
    TOD_prj(3) = sec
    plotTime = MOD( tstart + Field%userTime, 24.0 )
    OPEN( UNIT=lun_scn,FILE=file_scn,STATUS='OLD',ACTION="READ",IOSTAT=ios )
    IF( ios == 0 )THEN
      CALL InitReleaseSpec( relSpec )
      CALL ReadNamelistScn( lun_scn,relSpec )
      IF( nError == NO_ERROR )THEN
        EIS_origin(1) = SNGL(relSpec%release%xrel)
        EIS_origin(2) = SNGL(relSpec%release%yrel)
      END IF
      CLOSE(UNIT=lun_scn,IOSTAT=ios)
    END IF
    CALL mapfac( EIS_origin(1),EIS_origin(2),EIS_xmap,EIS_ymap )
  END IF
  CALL deallocate_read_prj()
END IF

hr  =  INT(           plotTime                            )
min =  INT(      60.*(plotTime - FLOAT(hr))               )
sec = NINT( 60.*(60.*(plotTime - FLOAT(hr)) - FLOAT(min)) )
TOD_plt(1) = hr
TOD_plt(2) = min
TOD_plt(3) = sec

idum = 0

!==============================================================================
! EIS header
!==============================================================================
IF( BuildHeaderEIS < Mxs )THEN
  WRITE(tmpstring,'(3I5)',IOSTAT=ios)(TOD_prj(i),i=1,3)
  IF( ios /= 0 )THEN
    tmpstring = '** ERROR **'
  END IF
  string(BuildHeaderEIS+1) = TRIM(tmpstring)
  BuildHeaderEIS = BuildHeaderEIS + 1
END IF
IF( BuildHeaderEIS < Mxs )THEN
  WRITE(tmpstring,'(3I5)',IOSTAT=ios)(TOD_plt(i),i=1,3)
  IF( ios /= 0 )THEN
    tmpstring = '** ERROR **'
  END IF
  string(BuildHeaderEIS+1) = TRIM(tmpstring)
  BuildHeaderEIS = BuildHeaderEIS + 1
END IF
IF( BuildHeaderEIS < Mxs )THEN
  WRITE(tmpstring,'(1PE15.4)',IOSTAT=ios)FLOAT(idum)
  IF( ios /= 0 )THEN
    tmpstring = '** ERROR **'
  END IF
  string(BuildHeaderEIS+1) = TRIM(tmpstring)
  BuildHeaderEIS = BuildHeaderEIS + 1
END IF
IF( BuildHeaderEIS < Mxs )THEN
  WRITE(tmpstring,'(3I5)',IOSTAT=ios)nlev,idum,idum+1
  IF( ios /= 0 )THEN
    tmpstring = '** ERROR **'
  END IF
  string(BuildHeaderEIS+1) = TRIM(tmpstring)
  BuildHeaderEIS = BuildHeaderEIS + 1
END IF
IF( BuildHeaderEIS < Mxs )THEN
  WRITE(tmpstring,'(1P3E15.4)',IOSTAT=ios)(level(i),i=1,nlev)
  IF( ios /= 0 )THEN
    tmpstring = '** ERROR **'
  END IF
  string(BuildHeaderEIS+1) = TRIM(tmpstring)
  BuildHeaderEIS = BuildHeaderEIS + 1
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildFooterEIS
!*******************************************************************************
INTEGER FUNCTION BuildFooterEIS( Mxs,string )

USE eisfile_fi
USE field_fd
USE error_fi
USE Write_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                      INTENT( IN  ) :: Mxs      !Maximum number of strings
CHARACTER(*), DIMENSION(Mxs), INTENT( OUT ) :: string   !Footer strings

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios
REAL,DIMENSION(1) :: x
REAL,DIMENSION(1) :: y
TYPE( SCIPFieldCoordinateT ) :: TransStruct
INTEGER outMode

!==============================================================================
! Initialize
!==============================================================================
BuildFooterEIS = 0

x = EIS_origin(1)
y = EIS_origin(2)

IF( WriteMode == I_LATLON )THEN
  TransStruct = Coordinate
  TransStruct%mode = I_LATLON
  CALL Transform( Coordinate,TransStruct,1,x,y )
  IF( nError /= NO_ERROR )GOTO 9999
  outMode = TransStruct%mode
ELSE
  outMode = Coordinate%mode
END IF

IF( outMode /= I_LATLON )THEN
  GOTO 9999
END IF
!==============================================================================
! Footer String
!==============================================================================
IF( BuildFooterEIS < Mxs )THEN
  WRITE(string(BuildFooterEIS+1),'(A8,2F15.6)',IOSTAT=ios)'Degrees',x,y
  IF( ios /= 0 )GOTO 9999
  BuildFooterEIS = BuildFooterEIS + 1
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyEIS
!*******************************************************************************
INTEGER FUNCTION WriteBodyEIS( npoly,xpoly,ypoly,nlev )

USE sagdef_fd
USE sagwrt_usr
USE eisfile_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                INTENT( IN  ) :: npoly  !number of points
REAL, DIMENSION(npoly), INTENT( IN  ) :: xpoly  !x coordinate arrray
REAL, DIMENSION(npoly), INTENT( IN  ) :: ypoly  !y coordinate arrray
INTEGER,                INTENT( IN  ) :: nlev   !Contour index

!==============================================================================
! Local variables
!==============================================================================
INTEGER lun
INTEGER ios
INTEGER i
REAL,DIMENSION(:),ALLOCATABLE :: xp
REAL,DIMENSION(:),ALLOCATABLE :: yp

!==============================================================================
! Formats
!==============================================================================
100 FORMAT(I5)
200 FORMAT(1P3E15.4)

!==============================================================================
! Initialize
!==============================================================================
WriteBodyEIS = SAG_ERROR

lun = uwrite%lun

!==============================================================================
! Allocate work space
!==============================================================================
ALLOCATE( xp(npoly),yp(npoly),STAT=ios )
IF( ios /= 0 )GOTO 9999

DO i = 1,npoly
  xp(i) = xpoly(i)
  yp(i) = ypoly(i)
END DO

!==============================================================================
! Transform data - Meters from origin
!==============================================================================
DO i = 1,npoly
  xp(i) = (xp(i) - EIS_origin(1))/EIS_xmap
  yp(i) = (yp(i) - EIS_origin(2))/EIS_ymap
END DO

!==============================================================================
! Write Line Header
!==============================================================================
  i = 1
  WRITE(lun,100,IOSTAT=ios)1
  IF( ios /= 0 )GOTO 9999
  WRITE(lun,100,IOSTAT=ios)npoly
  IF( ios /= 0 )GOTO 9999

!==============================================================================
! Write Line
!==============================================================================
DO i = 1,npoly
  WRITE(lun,200,IOSTAT=ios)xp(i),yp(i),EIS_origin(3)
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyEIS = SAG_OK

9999 CONTINUE
!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(xp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
