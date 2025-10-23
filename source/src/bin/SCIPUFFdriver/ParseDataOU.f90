INTEGER FUNCTION ParseDataOU()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios
REAL    dtOut

CHARACTER(512), EXTERNAL :: BuildFileNameAERMOD

ParseDataOU = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'METOUTPT' )

    IF( narg-ikwrd > 0 )THEN
      READ(carg(ikwrd+1),*,IOSTAT=ios) dtOut
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading OU METOUTPT output interval'
        GOTO 9999
      END IF
      IF( dtOut > 0. )THEN
        new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_OUTPUT)
        new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_OUTMET)
        new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_2D)
        new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_3D)
        new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_3D)
        new%weather%flags%tOutput  = dtOut
      END IF
    END IF

    IF( BTEST(new%weather%flags%doOutput,HOB_OUTPUT) )THEN
      IF( narg-ikwrd > 1 )THEN
        SELECT CASE( carg(ikwrd+2)(1:1) )
          CASE( 'H' )
            !Default output interval unit is hours
          CASE( 'M' )
            new%weather%flags%tOutput = new%weather%flags%tOutput/60.
          CASE( 'S' )
            new%weather%flags%tOutput = new%weather%flags%tOutput/3600.
          CASE DEFAULT
        END SELECT
      END IF
    END IF

  CASE( 'METFORMT','METFMT','METFRMAT','METFRMT' )

    IF( narg-ikwrd > 0 )THEN
      SELECT CASE( TRIM(carg(ikwrd+1)) )
        CASE( 'ASCII','FORMAT','FORMATTED','TEXT' )
          new%weather%flags%doOutput = IBSET(new%weather%flags%doOutput,HOB_ASCII)
        CASE DEFAULT
          new%weather%flags%doOutput = IBCLR(new%weather%flags%doOutput,HOB_ASCII)
      END SELECT
    END IF

  CASE( 'METFILE'  )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      metFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

  CASE( 'POSTFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      postFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
      IF( narg-ikwrd-1 > 0 )THEN
        IF( TRIM(carg(ikwrd+2)) == 'VARIANCE' )new%input%option%lOutputVariance = .TRUE.
      END IF
    END IF

  CASE( 'SAMPFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      smpFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

  CASE( 'PUFFFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      puffFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

  CASE( 'PRJFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      prjFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

  CASE( 'CONCFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      concFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF
  CASE( 'DEPOFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      depFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

  CASE( 'DOSEFILE' )
    IF( narg-ikwrd > 0 )THEN
      BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif
      dosFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used
    END IF

!  CASE( 'RECTABLE' )
!  CASE( 'MAXTABLE' )
!  CASE( 'DAYTABLE' )
!  CASE( 'MAXIFILE' )
!  CASE( 'PLOTFILE' )
!  CASE( 'TOXXFILE' )
!  CASE( 'RANKFILE' )
!  CASE( 'EVALFILE' )
!  CASE( 'SEASONHR' )
!  CASE( 'SUMMFILE' )
!  CASE( 'FILEFORM' )
!  CASE( 'EVENTOUT' )
!  CASE( 'FINISHED' )

  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for OU pathway: '//TRIM(carg(ikwrd))
!    WRITE(*,'(A)') 'Invalid keyword for OU pathway: '//TRIM(carg(ikwrd))
!    GOTO 9999

END SELECT

ParseDataOU = SUCCESS

9999 CONTINUE

RETURN
END
