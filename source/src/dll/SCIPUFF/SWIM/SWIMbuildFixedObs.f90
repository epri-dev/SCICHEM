INTEGER FUNCTION SWIMbuildFixedObs( grid,Obs )

!------ Build single obs with fixed wind

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( MetGrid  ), INTENT( IN    ) :: grid
TYPE( FirstObs ), INTENT( INOUT ) :: Obs

TYPE( ObsMet ), POINTER :: CurrentObs

INTEGER irv, ios, j, k

INTERFACE

  INTEGER FUNCTION SWIMnewObs( First,obs )
    USE SWIMobs_fd
    TYPE( FirstObs ),INTENT( INOUT ) :: First
    TYPE( ObsMet   ),POINTER         :: obs
  END FUNCTION SWIMnewObs

  SUBROUTINE SWIMgetObsTerrain( grid,First,Obs )
    USE SWIMmetField_fd
    USE SWIMobs_fd
    TYPE( MetGrid ), INTENT( IN ) :: grid
    TYPE( FirstObs ),INTENT( IN ) :: First
    TYPE( ObsMet  ),      POINTER :: Obs
  END SUBROUTINE SWIMgetObsTerrain

END INTERFACE
INTEGER, EXTERNAL :: PostProgressMessage

message%bString  = 'setting fixed (single observation) meteorology'

irv = PostProgressMessage( message )

SWIMbuildFixedObs = SWIMfailure

!------ If fixed obs already built previously, set numObs = 0

IF( Obs%numObs > 0 )THEN

  Obs%PrevNumObs    =  Obs%numObs
  Obs%PrevNumObsDom =  Obs%numObsDom
  Obs%PrevTime      =  Obs%time
  Obs%PrevZmax      =  Obs%Zmax
  Obs%PrevObs       => Obs%Obs

  Obs%numObs = 0

  NULLIFY( Obs%Obs )

  SWIMbuildFixedObs = SWIMresult
  GOTO 9999

END IF

!------ Create obs / linked list

irv = SWIMnewObs( Obs,CurrentObs )
IF( irv /= SWIMsuccess )GOTO 9999

Obs%time = 0.
Obs%obs  => CurrentObs

CurrentObs%id = 'Fixed'
CurrentObs%x  = 0.5*(grid%Xmin+grid%Xmax)
CurrentObs%y  = 0.5*(grid%Ymin+grid%Ymax)

CurrentObs%Vel%nZ   = 1
CurrentObs%Vel%z(1) = Obs%zref
Obs%Zmax            = Obs%zref
READ(Obs%Source,FMT=*,IOSTAT=ios) CurrentObs%Vel%u(1),CurrentObs%Vel%v(1)
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMbuildFixedObs'
  error%Message = 'Error setting fixed wind velocity'
  GOTO 9999
END IF

IF( CurrentObs%Vel%u(1)**2 + CurrentObs%Vel%v(1)**2 > MAXLIM_VEL2 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMbuildFixedObs'
  error%Message = 'Invalid fixed wind speed'
  error%Inform  = 'Maximum speed is '; j = LEN_TRIM(error%Inform) + 1
  CALL c_format( MAXLIM_VEL,k,error%Inform(j+1:) )
  error%Inform = TRIM(error%Inform)//' m/s'
  error%Action  = 'U,V = '; j = LEN_TRIM(error%Action) + 1
  CALL c_format( CurrentObs%Vel%u(1),k,error%Action(j+1:) )
  error%Action(j+k:j+k) = ','; j = LEN_TRIM(error%Action)
  CALL c_format( CurrentObs%Vel%v(1),k,error%Action(j+1:) )
  GOTO 9999
END IF

CALL SWIMgetObsTerrain( grid,Obs,CurrentObs )

SWIMbuildFixedObs = SWIMresult

9999 CONTINUE

RETURN
END


