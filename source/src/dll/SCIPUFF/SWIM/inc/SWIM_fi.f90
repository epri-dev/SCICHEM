!==============================================================================
!
!  SWIM module (local to SWIM)
!
!  16 Apr 2001 - Initial definition
!  13 Aug 2001 - Initial CVS repository
!
!==============================================================================

MODULE SWIM_fi

  USE basic_fd
  USE SWIMinit_fd
  USE SWIMmetField_fd
  USE SWIMobs_fd
  USE SWIMerr_fd
  USE logMessage_fd
  USE default_fd
  USE message_fd
  USE uniformGridT_fd
  USE MapCoord_fd
  USE netcdf_fd

  SAVE

  INTEGER :: numObsSrc, numField, numFieldMax, unitMet
  INTEGER :: numThreads

  TYPE( FirstObs ), DIMENSION(:), ALLOCATABLE, TARGET :: ObsSrc
  TYPE( MetField ), DIMENSION(:), ALLOCATABLE, TARGET :: field

  TYPE( SWIMerror ) :: error
  COMMON / CommonError / error
!$OMP THREADPRIVATE(/CommonError/)

  TYPE( PrjInput ) :: Prj

  TYPE( MapCoord ) :: PrjCoord

  INTEGER(LEN_ADDRESS) :: prevCallback, Callback
  INTEGER              :: CallerID, ClockDelay, ClockUpdate
  INTEGER              :: prevCallerID, prevClockDelay, prevClockUpdate


  TYPE( messageT ) :: message

  TYPE( LogMessage ), POINTER :: FirstLogMsg
  TYPE( LogMessage ), POINTER :: LogMsg

  LOGICAL :: ResetButtons
  LOGICAL :: lInitOutput

  REAL    :: MCWIFWtFac
  REAL    :: R_OBS
  REAL    :: E2BO
  REAL    :: DU2EPS
  INTEGER :: SCM_MAXITER

  REAL :: MaxCappedBL

  LOGICAL :: Reverse
  INTEGER :: StopMode, SWIMresult
  LOGICAL :: SWIMinProgress, SWIMiteration, SWIMmessageEnabled

  LOGICAL  :: lISDavail
  CHARACTER(PATH_MAXLENGTH) :: ISDhistoryFile

END MODULE SWIM_fi
