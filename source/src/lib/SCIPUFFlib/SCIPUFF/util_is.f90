!===============================================================================

LOGICAL FUNCTION IsGas( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsGas = BTEST(i,MATID_GAS)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsLiquid( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsLiquid = BTEST(i,MATID_LIQ)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsParticle( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsParticle = BTEST(i,MATID_PRT)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsWetParticle( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsWetParticle = BTEST(i,MATID_WETP)

RETURN
  END

!===============================================================================

LOGICAL FUNCTION IsEvap( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

LOGICAL IsLiquid

IsEvap = BTEST(i,MATID_EVAP) .AND. IsLiquid(i)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsAerosol( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsAerosol = BTEST(i,MATID_AEROSOL)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsMulti( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsMulti = BTEST(i,MATID_MULTI)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsMultiDep( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsMultiDep = BTEST(i,MATID_MULTI_DEP)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsMultiDos( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsMultiDos = BTEST(i,MATID_MULTI_DOS)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsNullSensor( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsNullSensor = BTEST(i,MATID_NULL_SENSOR)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsSatSensor( i )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i    !Class ID

IsSatSensor = BTEST(i,MATID_SAT_SENSOR)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsReady( id,dat )

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: dat
CHARACTER(*), INTENT( IN ) :: id

INTEGER, PARAMETER :: STATUS_COMPLETE  = 1

IsReady = (IAND(dat,STATUS_COMPLETE) == STATUS_COMPLETE) .OR. &
               (LEN_TRIM(id) == 0)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsFastMode()

USE scipuff_fi

IMPLICIT NONE

IsFastMode = BTEST(run_mode,FAST_MODE)

RETURN
END

!===============================================================================

LOGICAL FUNCTION IsReverseMode()

USE scipuff_fi

IMPLICIT NONE

IsReverseMode = BTEST(run_mode,REVERSE_MODE)

RETURN
END

!===============================================================================

INTEGER FUNCTION SetClass( class )

USE class_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: class

SetClass = 0

SELECT CASE( class )
  CASE( MAT_GAS )    !====== Basic Gas Material

    SetClass = IBSET(SetClass,MATID_GAS)

  CASE( MAT_PRT )    !====== Basic Particle Material

    SetClass = IBSET(SetClass,MATID_PRT)

  CASE( MAT_LIQ )    !====== Basic Liquid Material

    SetClass = IBSET(SetClass,MATID_LIQ)

  CASE( MAT_WET )    !====== Wet particle Material

    SetClass = IBSET(SetClass,MATID_WETP)

  CASE( MAT_AER )    !====== Basic Aerosol Material

    SetClass = IBSET(SetClass,MATID_AEROSOL)


  CASE( MAT_NULL )   !====== Null Sensor material

    SetClass = IBSET(SetClass,MATID_GAS)
    SetClass = IBSET(SetClass,MATID_NULL_SENSOR)

  CASE( MAT_SSAT )   !====== Saturated Sensor material

    SetClass = IBSET(SetClass,MATID_GAS)
    SetClass = IBSET(SetClass,MATID_SAT_SENSOR)

END SELECT

RETURN
END

!===============================================================================

INTEGER FUNCTION AddClass( iClass,iAdd )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iClass, iAdd

AddClass = IBSET(iClass,iAdd)

RETURN
END

!===============================================================================

INTEGER FUNCTION ClearClass( iClass,iClr )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iClass,iClr

ClearClass = IBCLR(iClass,iClr)

RETURN
END

!===============================================================================

INTEGER FUNCTION SetClassEvap( iClass )

USE class_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iClass

LOGICAL, EXTERNAL :: IsLiquid

IF( IsLiquid(iClass) )THEN
  SetClassEvap = IBSET(iClass,MATID_EVAP)
END IF

RETURN
END

!===============================================================================-

SUBROUTINE fix_tzone( tz )

IMPLICIT NONE

REAL, INTENT( INOUT ) :: tz

!------ changes local time of 0000Z (UTC) to time zone

IF( ABS(tz) > 12. )THEN
  tz = tz - SIGN(24.,tz)
END IF

RETURN
END

!=============================================================================

SUBROUTINE year_month_day( t,yr,mnth,day,to,yro,mntho,dayo )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: yr ,mnth ,day
REAL,    INTENT( IN  ) :: t
INTEGER, INTENT( OUT ) :: yro,mntho,dayo
REAL,    INTENT( OUT ) :: to

INTEGER days, iday, jul, julo
LOGICAL leap

INTEGER, EXTERNAL :: julian_day
LOGICAL, EXTERNAL :: leap_year

INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
           (/0,31,59,90,120,151,181,212,243,273,304,334/)

iday = INT(t/24.)
to   = t - 24.*FLOAT(iday)
IF( to < 0. )THEN
  iday = iday - 1
  to = to + 24.
END IF

jul  = julian_day( mnth,day,yr )
julo = jul + iday

yro = yr

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo <= 0 )
  yro  = yro - 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
  julo = julo + days
END DO

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo > days )
  julo = julo - days
  yro  = yro + 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

DO mntho = 12,1,-1
  IF( mntho >= 3 .AND. leap )THEN
    days = NDAY(mntho) + 1
  ELSE
    days = NDAY(mntho)
  END IF
  IF( julo > days )EXIT
END DO

dayo = julo - days

RETURN
END

