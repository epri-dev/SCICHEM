INTEGER FUNCTION SWIMinitFixed( Obs )

!------ Set FirstObs structure for fixed wind input

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER alloc_stat

SWIMinitFixed = SWIMfailure

!----- Set type

Obs%type = IBSET(0,OTB_FIXED)
Obs%type = IBSET(Obs%type,OTB_UV)

!------ Reference height is assumed to be 10m

Obs%zref = 10.

!------ Set number of variables = 2 (for U & V)

Obs%nVarFixed = 0
Obs%nVar      = 2

!------ Setup indicies and conversion factors for obs variables

ALLOCATE( Obs%VarID(Obs%nVar),Obs%Conv(Obs%nVar),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitFixed'
  error%Message = 'Error allocating obs variable id and conversion factor arrays'
  error%Inform  = 'Fixed winds'
  GOTO 9999
END IF

Obs%VarID(1) = OVP_U
Obs%VarID(2) = OVP_V

Obs%Conv = 1.

!------ Set time parameters

Obs%local       = Prj%localMet
Obs%timeOffset  = Prj%hourStart*3600.
Obs%timeBin     = Prj%timeBin

!------ Set single-level vertical grid

Obs%nz = 1
ALLOCATE( Obs%z(1),STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitFixed'
  error%Message = 'Error allocating single height array for fixed winds'
  GOTO 9999
END IF
Obs%z(1) = Obs%zref

!------ Initial time 'not set'

Obs%time     = NOT_SET_R
Obs%PrevTime = NOT_SET_R

Obs%NumObs        = 0
Obs%NumObsDom     = 0
Obs%Zmax          = 0.
Obs%PrevNumObs    = 0
Obs%PrevNumObsDom = 0
Obs%PrevZmax      = 0.

NULLIFY( Obs%Obs,Obs%PrevObs )

Obs%lAERMET = .FALSE.
Obs%lASOS1  = .FALSE.
Obs%AERid   = 'NOT_SET'

Obs%nASOS1repeat = 0

SWIMinitFixed = SWIMresult

9999 CONTINUE


RETURN
END

