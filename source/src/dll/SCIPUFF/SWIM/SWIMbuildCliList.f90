INTEGER FUNCTION SWIMbuildCliList( t,grid,Obs )

!------ Fill linked-list of obs for 2d climatology
!
!------ Assumes that data has already been loaded in Obs%ObsArray and
!       that linked-list has already been initialized

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE constants_fd

IMPLICIT NONE

REAL,             INTENT( IN    ) :: t
TYPE( MetGrid  ), INTENT( IN    ) :: grid
TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER alloc_stat, irv, i, j, i0, ihr, nv, ista, n1, n2
REAL    time_obs

REAL(8), DIMENSION(:), ALLOCATABLE :: var8

TYPE( ObsMet ), POINTER :: CurrentObs

INTERFACE

  INTEGER FUNCTION SWIMparseObsRecord( First,obs,var8,n1,n2 )
    USE SWIMobs_fd
    TYPE( FirstObs ),               INTENT( IN ) :: First
    TYPE( ObsMet   ),               POINTER      :: obs
    REAL(8), DIMENSION(First%nVar), INTENT( IN ) :: var8
    INTEGER,                        INTENT( IN ) :: n1, n2
  END FUNCTION SWIMparseObsRecord

  SUBROUTINE SWIMgetObsTerrain( grid,First,Obs )
    USE SWIMmetField_fd
    USE SWIMobs_fd
    TYPE( MetGrid ), INTENT( IN ) :: grid
    TYPE( FirstObs ),INTENT( IN ) :: First
    TYPE( ObsMet  ),      POINTER :: Obs
  END SUBROUTINE SWIMgetObsTerrain

  INTEGER FUNCTION SWIMconvertObsThermo( grid,Obs,CurrentObs )
    USE SWIMmetField_fd
    USE SWIMobs_fd
    TYPE( MetGrid  ), INTENT( IN ) :: grid
    TYPE( FirstObs ), INTENT( IN ) :: Obs
    TYPE( ObsMet ),   POINTER      :: CurrentObs
  END FUNCTION SWIMconvertObsThermo

  SUBROUTINE CheckObsFirstZlev( Obs )
    USE SWIMobs_fd
    TYPE( ObsMet  ), POINTER :: Obs
  END SUBROUTINE CheckObsFirstZlev

  SUBROUTINE SWIMclearGridList( First )
    USE SWIMobs_fd
    TYPE( FirstObsGridList ), POINTER :: First
  END SUBROUTINE SWIMclearGridList

  SUBROUTINE PutObsInCell( First,CurrentObs )
    USE SWIMobs_fd
    TYPE( FirstObsGridList ), INTENT( INOUT ) :: First
    TYPE( ObsMet   ),         POINTER         :: CurrentObs
  END SUBROUTINE PutObsInCell

  INTEGER FUNCTION SWIMconvPrjCoord( First,Obs )
    USE SWIMobs_fd
    TYPE( FirstObs ), INTENT( IN ) :: First
    TYPE( ObsMet   ), POINTER      :: Obs
  END FUNCTION SWIMconvPrjCoord

END INTERFACE

INTEGER, EXTERNAL :: SWIMsetGridList
INTEGER, EXTERNAL :: PostProgressMessage

message%bString  = 'reading surface climatology file(s)'

irv = PostProgressMessage( message )

SWIMbuildCliList = SWIMfailure

!------ Check if list needs to be updated

IF( Obs%time /= NOT_SET_R .AND. Obs%time > t )THEN
  SWIMbuildCliList = SWIMresult; GOTO 9999
END IF

!------ Check if worth building grid for nearest-obs searches

IF( ASSOCIATED(Obs%GridList) )CALL SWIMclearGridList( Obs%GridList )
irv = SWIMsetGridList( Obs,field(1)%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Define locals

ihr  = Obs%unit
nv   = Obs%nVar
ista = 0
n1   = 1
n2   = nv

ALLOCATE( var8(nv),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

!------ Set values for each obs location

CurrentObs => Obs%obs

DO WHILE( ASSOCIATED(CurrentObs) )

  ista = ista + 1
  i0   = ((ista-1)*24 + ihr)*nv

  DO j = 1,nv
    IF( Obs%ObsArray(i0+j) == OBP_NODATA )THEN
      var8(j) = OBP_BADDATA
    ELSE
      var8(j) = DBLE(Obs%ObsArray(i0+j))
    END IF
  END DO

!------ Set time for first obs (at this new time)

  IF( ista == 1 )CALL SWIMgetTimeObs( Obs,var8,time_obs,Obs%time )

!------ 'Clear' old values by setting nz = 0

  CurrentObs%Vel%nz = 0

!------ Extract data

  irv = SWIMparseObsRecord( Obs,CurrentObs,var8,n1,n2 )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Check lowest level

  CALL CheckObsFirstZlev( CurrentObs )

!------ Set horizontal location

  irv = SWIMconvPrjCoord( Obs,CurrentObs )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Set terrain height and landuse parameters; add to profile levels

  CALL SWIMgetObsTerrain( grid,Obs,CurrentObs )

!------ Thermodynamic conversions, i.e. absolute humidity from
!       relative and conversion to potential temperature

  irv = SWIMconvertObsThermo( grid,Obs,CurrentObs )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Put in cell for nearest obs search

  IF( ASSOCIATED(Obs%GridList) )CALL PutObsInCell( Obs%GridList,CurrentObs )

!------ Point to next obs

  CurrentObs => CurrentObs%nextObs

END DO

!----- Increment cyclic index into climo array; increment Julian day if necessary

Obs%unit = Obs%unit+1

IF( Obs%unit == 24 )THEN
  Obs%unit = 0
  DO i = 1,Obs%numObs
    i0 = (i-1)*24*nv
    DO ihr = 1,24
      j = i0 + (ihr-1)*nv
      Obs%ObsArray(j+3) = Obs%ObsArray(j+3) + 1.
    END DO
  END DO
END IF

SWIMbuildCliList = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var8) )DEALLOCATE( var8,STAT=alloc_stat )

RETURN
END

