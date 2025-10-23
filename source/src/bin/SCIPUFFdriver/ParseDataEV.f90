INTEGER FUNCTION ParseDataEV()

USE SCIPUFFdriver_fi

IMPLICIT NONE

ParseDataEV = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )
!  CASE( 'EVENTPER' )
!  CASE( 'EVENTLOC' )
!  CASE( 'INCLUDED' )
  CASE( 'FINISHED' )
  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for EV pathway: '//TRIM(carg(ikwrd))
!    WRITE(*,'(A)') 'Invalid keyword for EV pathway: '//TRIM(carg(ikwrd))
!    GOTO 9999
END SELECT

ParseDataEV = SUCCESS

9999 CONTINUE

RETURN
END

