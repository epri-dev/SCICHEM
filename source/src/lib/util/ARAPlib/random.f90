SUBROUTINE init_seed( init,iseed )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: init
INTEGER, INTENT( OUT ) :: iseed

iseed = ABS(init)
IF( MOD(iseed,2) == 0 )iseed = iseed + 1

RETURN
END

!===============================================================================

SUBROUTINE new_seed( new,iseed )

IMPLICIT NONE

INTEGER, INTENT( OUT   ) :: new
INTEGER, INTENT( INOUT ) :: iseed

REAL    xx
INTEGER jseed

CALL ran( xx,iseed )

jseed = ISHFTC(iseed,1,3)

IF( MOD(jseed,2) == 0 )jseed = jseed + 1

CALL ran( xx,jseed )
DO WHILE( jseed < 100000 )
  CALL ran( xx,jseed )
END DO

IF( MOD(jseed,2) == 0 )jseed = jseed + 1

new = jseed

RETURN
END

!===============================================================================

SUBROUTINE ran( xx,iseed )

IMPLICIT NONE

REAL,    INTENT( OUT   ) :: xx
INTEGER, INTENT( INOUT ) :: iseed

iseed = iseed*125
iseed = ABS(iseed - (iseed/2796203) * 2796203)

xx = iseed/2796203.0

RETURN
END
