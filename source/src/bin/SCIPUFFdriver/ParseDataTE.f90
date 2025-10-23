INTEGER FUNCTION ParseDataTE()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i

CHARACTER(128), EXTERNAL :: BuildFileNameAERMOD

ParseDataTE = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'TELUFILE' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient TE TELUFILE input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,*) 'Too many met input files'
      WRITE(*,*) 'Max is ',MAXMETINP
      GOTO 9999
    END IF

    BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    metInp(nMet)%file = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))
    metInp(nMet)%type = MET_SCITER

  CASE( 'VERTLYRS' )

    DO i = 1,narg-ikwrd
      nvertlev = nvertlev + 1
      IF( nvertlev > HS_MAXZB )THEN
        WRITE(*,*) 'TE VERTLYRS: Too many levels specified'
        WRITE(*,*) 'Maximum is ',HS_MAXZB
        GOTO 9999
      END IF
      READ(carg(ikwrd+i),*,IOSTAT=ios) new%weather%terrain%mc%z(nvertlev)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading TE VERTLYRS'
        GOTO 9999
      END IF
    END DO

  CASE( 'FINISHED' )

  CASE DEFAULT
    WRITE(*,'(A)') 'Invalid keyword for TE pathway: '//TRIM(carg(ikwrd))
    GOTO 9999

END SELECT

ParseDataTE = SUCCESS

9999 CONTINUE

RETURN
END
