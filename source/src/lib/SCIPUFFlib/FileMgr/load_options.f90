!*******************************************************************************
!            UnloadOptions
!*******************************************************************************
SUBROUTINE UnloadOptions( option )

USE options_fd
USE met_fi
USE scipuff_fi
USE sampler_fi, ONLY: lOutputVariance

!     Load SCIPUFF commons from an SCIP Options structure

IMPLICIT NONE

TYPE( optionsT ), INTENT( IN ) :: option

!==== Unload

substrate_type = option%substrate

mgrd     = option%mGrd
nzbl     = option%nzBL

delmin   = option%delMin
cmin     = option%massMin
t_avg    = option%timeAvg

wwtrop   = option%wwTrop
sltrop   = option%slTrop
epstrop  = option%epsTrop

uu_calm  = option%uuCalm
sl_calm  = option%slCalm

z_dosage = option%zDosage

smpfile  = option%samplerFile

dt_smp   = option%dtSampler

lOutputVariance = option%lOutputVariance

RETURN
END
!*******************************************************************************
!            LoadOptions
!*******************************************************************************
SUBROUTINE LoadOptions( option )

USE options_fd
USE scipuff_fi
USE met_fi
USE sampler_fi, ONLY: lOutputVariance

!     Load an SCIP Options structure from SCIPUFF commons

IMPLICIT NONE

TYPE( optionsT ), INTENT( OUT ) :: option

!==== Load

option%mGrd      = mgrd
option%nzBL      = nzbl
option%substrate = substrate_type

option%delMin   = delmin
option%massMin  = cmin
option%timeAvg  = t_avg

option%wwTrop   = wwtrop
option%slTrop   = sltrop
option%epsTrop  = epstrop

option%uuCalm   = uu_calm
option%slCalm   = sl_calm

option%zDosage  = z_dosage

option%samplerFile = smpfile

option%dtSampler = dt_smp

option%lOutputVariance = lOutputVariance

RETURN
END
