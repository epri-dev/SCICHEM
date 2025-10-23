MODULE time_fd

  USE DefSize_fd

!==== Parameters ==============================================================

!==== SCIP Time reference types

  INTEGER,PARAMETER :: HT_UTC    = 0
  INTEGER,PARAMETER :: HT_LOCAL  = 1

!==== timeT ===================================================================

  TYPE  timeT
    SEQUENCE
    INTEGER    reference !local
    INTEGER    year      !year_start,year_end
    INTEGER    month     !month_start,month_end
    INTEGER    day       !day_start,day_end
    REAL       hour      !tstart,tend
    REAL       runTime   !tend_hr
  END TYPE  timeT

  INTEGER, PARAMETER :: SIZE_timeT = 4*KIND(1) + 2*KIND(1.)

!==== stepT ===================================================================

  TYPE  stepT
    SEQUENCE
    REAL  max       !delt
    REAL  output    !dt_save
  END TYPE  stepT

  INTEGER, PARAMETER :: SIZE_stepT = 2*KIND(1.)

!==== startT ==================================================================

  TYPE  startT
    SEQUENCE
    REAL        zone
    TYPE ( timeT ) time
  END TYPE  startT

  INTEGER, PARAMETER :: SIZE_startT = SIZE_timeT + KIND(1.)

!==== endT ====================================================================

  TYPE  endT
    SEQUENCE
    TYPE ( timeT ) time
    TYPE ( stepT ) step
  END TYPE  endT

  INTEGER, PARAMETER :: SIZE_endT = SIZE_timeT + SIZE_stepT

!==== ctrlT ===================================================================

  TYPE  ctrlT
    SEQUENCE
    REAL                      runTime
    CHARACTER(PATH_MAXLENGTH) name
    CHARACTER(PATH_MAXLENGTH) path
  END TYPE  ctrlT

  INTEGER, PARAMETER :: SIZE_ctrlT = KIND(1.) + 2*PATH_MAXLENGTH

!==== SimpleTimeT ===================================================================

  TYPE  SimpleTimeT
    SEQUENCE
    INTEGER    Year
    INTEGER    Month
    INTEGER    Day
    REAL       Hour
  END TYPE  SimpleTimeT

  INTEGER, PARAMETER :: SIZE_SimpleTimeT = 3*KIND(1) + 1*KIND(1.)

!==== temporalT ===============================================================

  TYPE  temporalT
    SEQUENCE
    TYPE( startT ) start
    TYPE( endT   ) end
  END TYPE  temporalT

  INTEGER, PARAMETER :: SIZE_temporalT = SIZE_startT + SIZE_endT

!==== statusT =================================================================

  TYPE statusT
    SEQUENCE
    INTEGER        status
    INTEGER        nPuffType
    TYPE ( timeT ) time
  END TYPE  statusT

END MODULE time_fd
