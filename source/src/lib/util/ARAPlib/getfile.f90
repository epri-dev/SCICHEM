SUBROUTINE get_file( prompt,file,nch )

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: prompt
CHARACTER(*), INTENT( OUT ) :: file
INTEGER,      INTENT( OUT ) :: nch

INTEGER ieof

WRITE(6,100,ADVANCE='NO')prompt

CALL get_line( 5,nch,file,ieof )

IF( ieof /= 0 )THEN

  WRITE(6,2100)prompt

  file = ' '
  nch  = 0

END IF

RETURN

100  FORMAT(' ',A,' file : ')
2100 FORMAT(' ***** EOF getting ',A,' file.')

END

!============================================================================

SUBROUTINE get_line( nu,nch,cline,ieof )

IMPLICIT NONE

INTEGER      nu, nch, ieof
CHARACTER(*) cline

INTEGER i

ieof = 0
READ(nu,'(A)',END=900) cline

DO i = LEN(cline),1, -1
  IF( cline(i:i) /=' ' )THEN
  nch = i
  GOTO 1000
  END IF
END DO

nch = 0
GOTO 1000

900 CONTINUE
nch  = 0
ieof = -1

1000 CONTINUE

RETURN
END
