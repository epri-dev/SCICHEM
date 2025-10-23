MODULE flags_fd

!==== SCIP input flags types

  INTEGER,PARAMETER :: HF_OFF        = 0

!---- "method" flag bits

  INTEGER,PARAMETER :: HFB_DYNAMIC   = 0
  INTEGER,PARAMETER :: HF_DYNAMIC    = 2**HFB_DYNAMIC
  INTEGER,PARAMETER :: HFB_DENSE     = 1
  INTEGER,PARAMETER :: HF_DENSE      = 2**HFB_DENSE
  INTEGER,PARAMETER :: HFB_STATIC    = 2
  INTEGER,PARAMETER :: HF_STATIC     = 2**HFB_STATIC

!---- "mode" flag bits

  INTEGER,PARAMETER :: HFB_FAST   = 0
  INTEGER,PARAMETER :: HF_FAST    = 2**HFB_FAST

  INTEGER,PARAMETER :: HFB_REVERSE = 3
  INTEGER,PARAMETER :: HF_REVERSE  = 2**HFB_REVERSE

  INTEGER, PARAMETER :: HFB_EVAP2D = 8  !Reserved not a user option
  INTEGER, PARAMETER :: HF_EVAP2D  = 2**HFB_EVAP2D

  INTEGER,PARAMETER :: HFB_DINCRMNT = 11
  INTEGER,PARAMETER :: HF_DINCRMNT  = 2**HFB_DINCRMNT

!---- "start" flag bits

  INTEGER,PARAMETER :: HFB_RESTART = 0
  INTEGER,PARAMETER :: HF_RESTART  = 2**HFB_RESTART

!==== auditT ==================================================================

  TYPE  auditT
    SEQUENCE
    CHARACTER(80) title    !title
    CHARACTER(32) analyst  !audit_analyst
    CHARACTER(32) class    !audit_class
    CHARACTER(32) version  !audit_version
    CHARACTER(32) date     !audit_date
  END TYPE  auditT

  INTEGER, PARAMETER :: SIZE_auditT = 80 + 4*32

!==== flagsT ===================================================================

  TYPE  flagsT
    SEQUENCE
    INTEGER      start        !(new/restart)
    INTEGER      method       !dynamic,dense_gas,static,multicomp
    INTEGER      mode         !run_mode,hazard
    INTEGER      prjEffects   !SCIP 4.0 project effecs (incident based)
    TYPE ( auditT ) audit
  END TYPE  flagsT

  INTEGER, PARAMETER :: SIZE_flagsT = SIZE_auditT + 4*KIND(1)

END MODULE flags_fd
