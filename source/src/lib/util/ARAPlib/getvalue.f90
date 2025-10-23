SUBROUTINE get_value( line,nch,paste1,paste2,err )

IMPLICIT NONE

CHARACTER(1), PARAMETER :: BLANK = ' '

CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(1), INTENT( IN    ) :: paste1, paste2
LOGICAL,      INTENT( OUT   ) :: err

INTEGER i

err = .FALSE.

IF( (nch <= 0) .OR. (line == BLANK) )THEN
  err  = .TRUE.
  GOTO 9999
END IF

DO i = 1, nch
  IF( line(i:i) == paste1 )line(i:i) = paste2
END DO

DO WHILE( line(1:1) == BLANK )
  line = line(2:)
  nch  = nch - 1
  IF( nch <= 0 )THEN
    err = .TRUE.
    GOTO 9999
  END IF
END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_i( line,nch,i_value,err )

IMPLICIT NONE

CHARACTER(1), PARAMETER :: BLANK = ' '

CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( INOUT ) :: nch
INTEGER,      INTENT( OUT   ) :: i_value
LOGICAL,      INTENT( OUT   ) :: err

CHARACTER(5) cfmt

INTEGER ll, ios

err = .FALSE.

IF( (nch <= 0) .OR. (line == BLANK) )THEN
  err  = .TRUE.
  GOTO 9999
END IF

DO WHILE( line(1:1) == BLANK )
  line = line(2:)
  nch  = nch - 1
  IF( nch <= 0 )THEN
    err = .TRUE.
    GOTO 9999
  END IF
END DO

ll = INDEX(line,BLANK) - 1
IF( ll <= 0 )ll = nch

WRITE(cfmt,2000) ll
2000 FORMAT('(I',I2,')')

READ(line,cfmt,IOSTAT=ios)i_value
IF( ios /= 0 )THEN
  err = .TRUE.
  GOTO 9999
END IF

line(1:) = line(ll+1:)
nch = nch - ll

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_r( line,nch,r_value,err )

IMPLICIT NONE

CHARACTER(1), PARAMETER :: BLANK =' '

CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( INOUT ) :: nch
REAL,         INTENT( OUT   ) :: r_value
LOGICAL,      INTENT( OUT   ) :: err

CHARACTER(7) cfmt

INTEGER ll, ios

err = .FALSE.

IF( (nch <= 0) .OR. (line == BLANK) )THEN
  err  = .TRUE.
  GOTO 9999
END IF

DO WHILE( line(1:1) == BLANK )
  line = line(2:)
  nch  = nch - 1
  IF( nch <= 0 )THEN
    err = .TRUE.
    GOTO 9999
  END IF
END DO

ll = INDEX(line,BLANK) - 1
IF( ll <= 0 )ll = nch

WRITE(cfmt,2000) ll
2000 FORMAT('(F',I2,'.0)')

READ(line,cfmt,IOSTAT=ios)r_value
IF( ios /= 0 )THEN
  err = .TRUE.
  GOTO 9999
END IF

line(1:) = line(ll+1:)
nch = nch - ll

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_c( line,nch,delimit,c_value,ncc,err )

IMPLICIT NONE

CHARACTER(1), PARAMETER :: BLANK =' '

CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(*), INTENT( IN    ) :: delimit
CHARACTER(*), INTENT( OUT   ) :: c_value
INTEGER,      INTENT( OUT   ) :: ncc
LOGICAL,      INTENT( OUT   ) :: err

INTEGER i, ncd, j, ll

err = .FALSE.

IF( (nch <= 0) .OR. (line == BLANK) )THEN
  err  = .TRUE.
  GOTO 9999
END IF

DO WHILE( line(1:1) == BLANK )
  line = line(2:)
  nch  = nch - 1
  IF( nch <= 0 )THEN
    err = .TRUE.
    GOTO 9999
  END IF
END DO

IF( delimit == BLANK )THEN
  ll = INDEX(line,BLANK) - 1
  IF( ll <= 0 )ll = nch
  c_value = line(1:ll)
  ncc     = ll
  line(1:) = line(ll+2:)
  nch = nch - (ll+2) + 1
ELSE
  ncd = LEN(delimit)
  i   = INDEX(line,delimit)
  j   = INDEX(line(i+ncd:),delimit)
  IF( (i <= 0) .OR. (j <= 0) )THEN
    err = .TRUE.
    GOTO 9999
  ELSE
    c_value = line(i+ncd:i+ncd+j-2)
    ncc = j-1
    line = line(i+ncd+j-1+ncd:)
    nch  = nch - (i+ncd+j-1+ncd) + 1
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_d( line,nch,d_value,err )

IMPLICIT NONE

CHARACTER(1), PARAMETER :: BLANK =' '

CHARACTER(*),     INTENT( INOUT ) :: line
INTEGER,          INTENT( INOUT ) :: nch
DOUBLE PRECISION, INTENT( OUT   ) :: d_value
LOGICAL,          INTENT( OUT   ) :: err

CHARACTER(7) cfmt

INTEGER ll, ios

err = .FALSE.

IF( (nch <= 0) .OR. (line == BLANK) )THEN
  err  = .TRUE.
  GOTO 9999
END IF

DO WHILE( line(1:1) == BLANK )
  line = line(2:)
  nch  = nch - 1
  IF( nch <= 0 )THEN
    err = .TRUE.
    GOTO 9999
  END IF
END DO

ll = INDEX(line,BLANK) - 1
IF( ll <= 0 )ll = nch

WRITE(cfmt,2000) ll
2000 FORMAT('(F',I2,'.0)')

READ(line,cfmt,IOSTAT=ios)d_value
IF( ios /= 0 )THEN
  err = .TRUE.
  GOTO 9999
END IF

line(1:) = line(ll+1:)
nch = nch - ll

9999 CONTINUE

RETURN
END
