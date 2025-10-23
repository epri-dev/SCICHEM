!*******************************************************************************
!            UnloadFlags
!*******************************************************************************
SUBROUTINE UnloadFlags( flags )

USE flags_fd
USE scipuff_fi
USE files_fi

!     Load SCIPUFF commons from an SCIP Flags structure

IMPLICIT NONE

TYPE( flagsT ), INTENT( IN ) :: flags

!==== Unload

run_mode = 0

!==== Method flags

dynamic   = BTEST(flags%method,HFB_DYNAMIC)
dense_gas = BTEST(flags%method,HFB_DENSE)
static    = BTEST(flags%method,HFB_STATIC)

!==== Mode flags

IF( BTEST(flags%mode,HFB_FAST)     )run_mode = IBSET(run_mode,FAST_MODE)
IF( BTEST(flags%mode,HFB_REVERSE)  )run_mode = IBSET(run_mode,REVERSE_MODE)
IF( BTEST(flags%mode,HFB_DINCRMNT) )run_mode = IBSET(run_mode,DINCRMNT)

!==== Effects flags

prjEffects = flags%prjEffects

!==== Start flags

flag_start = flags%start

!==== Restart Flag (not fully in SCIPUFF)

IF( .NOT.BTEST(flags%start,HFB_RESTART) )THEN
  file_rst = ' '
  path_rst = ' '
  time_rst = 0.0
END IF

!==== Audit

title         = flags%audit%title
audit_analyst = flags%audit%analyst
audit_class   = flags%audit%class
audit_version = ' ' !To be set by SCIPUFF
audit_date    = ' ' !To be set by SCIPUFF

!==== Misc

create = .TRUE. !TOOL will always assume create=T

RETURN
END
!*******************************************************************************
!            LoadFlags
!*******************************************************************************
SUBROUTINE LoadFlags( flags )

USE flags_fd
USE scipuff_fi

!     Load an SCIP Flags structure from SCIPUFF commons

IMPLICIT NONE

TYPE( flagsT ), INTENT( OUT ) :: flags

!==== Load

flags%mode   = 0
flags%method = 0

!==== Method flags

IF( dynamic )THEN
   flags%method = IBSET(flags%method,HFB_DYNAMIC)
ELSE
   flags%method = IBCLR(flags%method,HFB_DYNAMIC)
END IF

IF( dense_gas )THEN
   flags%method = IBSET(flags%method,HFB_DENSE)
ELSE
   flags%method = IBCLR(flags%method,HFB_DENSE)
END IF

IF( static )THEN
   flags%method = IBSET(flags%method,HFB_STATIC)
ELSE
   flags%method = IBCLR(flags%method,HFB_STATIC)
END IF

!==== Mode flags

IF( BTEST(run_mode,FAST_MODE)     )flags%mode = IBSET(flags%mode,HFB_FAST)
IF( BTEST(run_mode,REVERSE_MODE)  )flags%mode = IBSET(flags%mode,HFB_REVERSE)
IF( BTEST(run_mode,DINCRMNT)      )flags%mode = IBSET(flags%mode,HFB_DINCRMNT)

!==== Effects flags

flags%prjEffects = prjEffects

!==== Start flags

flags%start = flag_start

!==== Audit

flags%audit%title   = title
flags%audit%analyst = audit_analyst
flags%audit%class   = audit_class
flags%audit%version = audit_version
flags%audit%date    = audit_date

RETURN
END
