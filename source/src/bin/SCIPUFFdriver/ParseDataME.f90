INTEGER FUNCTION ParseDataME()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i, j, k, n

CHARACTER(256) string

CHARACTER(256), EXTERNAL :: BuildFileNameAERMOD

CHARACTER(1), PARAMETER :: Q  = CHAR(39)  !Single quote
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

ParseDataME = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'SURFFILE' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME SURFFILE input'
      GOTO 9999
    ELSE IF( narg-ikwrd > 1 )THEN
      WRITE(*,'(A)') 'Too much ME SURFFILE input'
      WRITE(*,'(A)') 'Only file name should be specified'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
      WRITE(*,*) 'Max is ',MAXMETINP
      GOTO 9999
    END IF
    isrf = nMet

    BACKSPACE(lun,IOSTAT=ios)

#if defined(LINUX)
!------ Re-read line "as is" since case matters for path and file names on Linux
!       and to handle files names with spaces

    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )

#else
!------ Re-read line to handle files names with spaces

    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    metInp(nMet)%file = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))
    metInp(nMet)%type = MET_AERSRF

  CASE( 'PROFFILE' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME PROFFILE input'
      GOTO 9999
    ELSE IF( narg-ikwrd > 1 )THEN
      WRITE(*,'(A)') 'Too much ME PROFFILE input'
      WRITE(*,'(A)') 'Only file name should be specified'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
      WRITE(*,*) 'Max is ',MAXMETINP
      GOTO 9999
    END IF
    iprf = nMet

    BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#else
    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    metInp(nMet)%file = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))
    metInp(nMet)%type = MET_AERPRF

  CASE( 'SURFDATA' )

    IF( metInp(isrf)%type /= MET_AERSRF )THEN
      WRITE(*,'(A)') 'File associated with ME SURFDATA is not a SURFFILE'
      GOTO 9999
    END IF

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient ME SURFDATA input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) metInp(isrf)%staNum
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME SURFDATA station number'
      GOTO 9999
    END IF

    READ(carg(ikwrd+2),*,IOSTAT=ios) metInp(isrf)%year
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME SURFDATA year'
      GOTO 9999
    END IF

    i = ikwrd+2
    IF( narg-ikwrd == 3 .OR. narg-ikwrd == 5 )THEN
      i = i + 1
      metInp(isrf)%name = TRIM(carg(i))
    END IF

    IF( narg-ikwrd > 4 )THEN
      i = i + 1
      READ(carg(i),*,IOSTAT=ios) metInp(isrf)%x
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME SURFDATA x coordinate'
        GOTO 9999
      END IF
      i = i + 1
      READ(carg(i),*,IOSTAT=ios) metInp(isrf)%y
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME SURFDATA y coordinate'
        GOTO 9999
      END IF
    END IF

  CASE( 'UAIRDATA' )

    IF( metInp(iprf)%type /= MET_AERPRF )THEN
      WRITE(*,'(A)') 'File associated with ME UAIRDATA is not PROFILE'
      GOTO 9999
    END IF

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient ME UAIRDATA input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) metInp(iprf)%staNum
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME UAIRDATA station number'
      GOTO 9999
    END IF

    READ(carg(ikwrd+2),*,IOSTAT=ios) metInp(iprf)%year
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME UAIRDATA year'
      GOTO 9999
    END IF

    i = ikwrd+2
    IF( narg-ikwrd == 3 .OR. narg-ikwrd == 5 )THEN
      i = i + 1
      metInp(iprf)%name = TRIM(carg(i))
    END IF

    IF( narg-ikwrd > 4 )THEN
      i = i + 1
      READ(carg(i),*,IOSTAT=ios) metInp(iprf)%x
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME UAIRDATA x coordinate'
        GOTO 9999
      END IF
      i = i + 1
      READ(carg(i),*,IOSTAT=ios) metInp(iprf)%y
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME UAIRDATA y coordinate'
        GOTO 9999
      END IF
    END IF

!  CASE( 'SITEDATA' )
    !Ignore

  CASE( 'ASOS1MIN' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME ASOS1MIN input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
      WRITE(*,*) 'Max is ',MAXMETINP
      GOTO 9999
    END IF
    isrf = nMet

    BACKSPACE(lun,IOSTAT=ios)

#if defined(LINUX)
!------ Re-read line "as is" since case matters for path and file names on Linux
!       and to handle files names with spaces

    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )

#else
!------ Re-read line to handle files names with spaces

    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    metInp(nMet)%file = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))
    metInp(nMet)%type = MET_ASOS1M

    IF( carg(ikwrd+1)(1:1) == QQ .OR. carg(ikwrd+1)(1:1) == Q )THEN  !Look for possible 2nd file name

      k = 0
      DO j = ikwrd+2,narg
        IF( INDEX(carg(j)(1:1),QQ) > 0 .OR. INDEX(carg(j)(1:1),Q) > 0 )THEN
          k = J; EXIT
        END IF
      END DO
      IF( k > 0 )THEN
        string = ''
        DO j = k,narg
          string = TRIM(string)//' '//TRIM(carg(j))
        END DO
        metInp(nMet)%file = TRIM(metInp(nMet)%file)//' '//QQ//TRIM(BuildFileNameAERMOD(carg(k),string,''))//QQ
      END IF

      IF( narg >= ikwrd+3 )THEN
        k = 2
        DO j = narg,2,-1
          n = LEN_TRIM(carg(j))
          IF( INDEX(carg(j)(n:n),QQ) > 0 .OR. INDEX(carg(j)(n:n),Q) > 0)THEN
            k = j; EXIT
          END IF
        END DO
        IF( narg == k+2 )THEN
          READ(carg(k+1),*,IOSTAT=ios) metInp(nMet)%y
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading ME ASOS1MIN latitude'
            GOTO 9999
          END IF
          READ(carg(k+2),*,IOSTAT=ios) metInp(nMet)%x
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading ME ASOS1MIN longitude'
            GOTO 9999
          END IF
        END IF

      END IF

    ELSE  !Quotes not used for file names

      IF( narg == ikwrd+2 .OR. narg == ikwrd+4 )metInp(nMet)%file = TRIM(metInp(nMet)%file)//' '//QQ//TRIM(carg(ikwrd+2))//QQ

      IF( narg == ikwrd+3 .OR. narg == ikwrd+4 )THEN
        READ(carg(narg-1),*,IOSTAT=ios) metInp(nMet)%y
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading ME ASOS1MIN latitude'
          GOTO 9999
        END IF
        READ(carg(narg),*,IOSTAT=ios) metInp(nMet)%x
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading ME ASOS1MIN longitude'
          GOTO 9999
        END IF
      END IF

    END IF

  CASE( 'PROFBASE' )

    IF( metInp(iprf)%type /= MET_AERPRF )THEN   !Assumed to come after PROFILE
      WRITE(*,'(A)') 'File associated with ME PROFBASE is not PROFILE'
      GOTO 9999
    END IF

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME PROFBASE input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) metInp(iprf)%baseElev
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME PROFBASE'
      GOTO 9999
    END IF

  CASE( 'SCISRF','SCISFC' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME SCISRF input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
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
    metInp(nMet)%type = MET_SCISRF

  CASE( 'SCIPRF','SCIUA' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME SCIPRF input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
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
    metInp(nMet)%type = MET_SCIPRF

  CASE( 'TIMEBIN' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME TIMEBIN input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%weather%met%timeBin
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME TIMEBIN'
      GOTO 9999
    END IF

    lSetTimeBin = .TRUE.

  CASE( 'FIXED' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient ME FIXED input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > 1 )THEN
      WRITE(*,'(A)') 'No other met allowed with FIXED input'
      GOTO 9999
    END IF

    metInp(nMet)%file = TRIM(carg(ikwrd+1))//' '//TRIM(carg(ikwrd+2))//' 1'  !Speed, direction (1 => m/s)
    metInp(nMet)%type = MET_SCIFIX

  CASE( 'GRIDFILE','METFILE' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient ME GRIDFILE input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
      WRITE(*,*) 'Max is ',MAXMETINP
      GOTO 9999
    END IF

    BACKSPACE(lun,IOSTAT=ios)
#if defined(LINUX)
    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
    CALL cupper( carg(2) )
#else
    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    SELECT CASE( TRIM(carg(ikwrd+1)) )
      CASE( 'MEDOC' )
        metInp(nMet)%type = MET_SCIGRD
      CASE( 'MEDOC_LIST','MEDOCLIST' )
        metInp(nMet)%type = MET_MEDLIS
      CASE DEFAULT
        WRITE(*,'(A)') 'Invalid met file type'
        WRITE(*,'(A)') 'Only MEDOC or MEDOC_LIST supported'
        GOTO 9999
    END SELECT

    metInp(nMet)%file = TRIM(BuildFileNameAERMOD(carg(ikwrd+2),line,path_in))

    IF( narg-ikwrd > 2 )THEN
      j = ikwrd+2
      DO i = ikwrd+2,narg  !Check if file name contained quotes
        IF( INDEX(carg(i),'"') > 0 .OR. INDEX(carg(i),"'") > 0 )THEN
          j = i+1
        END IF
      END DO
      IF( j <= narg )THEN
        SELECT CASE( TRIM(carg(j)) )
          CASE( 'UTC','GMT','GREENWICH','Z','ZULU' )
            new%input%time%start%time%reference = HT_UTC
            new%input%time%end%time%reference   = HT_UTC
          CASE DEFAULT
        END SELECT
      END IF
    END IF

  CASE( 'SCILIST','SCILIS' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME SCILIST input'
      GOTO 9999
    END IF

    nMet = nMet + 1
    IF( nMet > MAXMETINP )THEN
      WRITE(*,'(A)') 'Too many met input files'
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
    metInp(nMet)%type = MET_SCILIS

  CASE( 'ROUGHNES','RGHNESS','ROUGHNSS','ROUGHNS','RGHNSS','ZRUF' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient ME ROUGHNES input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) zruf
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading ME ROUGHNES'
      GOTO 9999
    END IF

    lZruf = .TRUE.

  CASE( 'CALCULBL','CALCUBL','CALCBL' )

    IF( narg-ikwrd > 0 )THEN
      IF( TRIM(carg(ikwrd+1)) == 'ON' )new%weather%bl%type = HB_BLCALC
    END IF

  CASE( 'SIMPLEBL' )

    IF( narg-ikwrd > 0 )THEN
      IF( TRIM(carg(ikwrd+1)) == 'ON' )new%weather%bl%type = HB_BLSIMPLE
    END IF

  CASE( 'PRECIP','PRCPTYPE' )

    IF( narg-ikwrd > 0 )THEN
      new%weather%precip%type = HP_PRCPINP
      SELECT CASE( TRIM(carg(ikwrd+1)) )
        CASE( 'METFILE' )
          new%weather%precip%type = HP_PRCPMET
        CASE( 'LGTRAIN' )
          new%weather%precip%class = HP_RAIN + HP_LIGHT
        CASE( 'MODRAIN' )
          new%weather%precip%class = HP_RAIN + HP_MODERATE
        CASE( 'HVYRAIN' )
          new%weather%precip%class = HP_RAIN + HP_HEAVY
        CASE( 'LGTSNOW' )
          new%weather%precip%class = HP_SNOW + HP_LIGHT
        CASE( 'MODSNOW' )
          new%weather%precip%class = HP_SNOW + HP_MODERATE
        CASE( 'HVYSNOW' )
          new%weather%precip%class = HP_SNOW + HP_HEAVY
        CASE DEFAULT
          new%weather%precip%type = HP_NONE
      END SELECT
    END IF

  CASE( 'USELSVAR' )

    IF( narg-ikwrd > 0 )THEN
      SELECT CASE( TRIM(carg(ikwrd+1)) )
        CASE( 'ON','YES','OPER' )
          new%weather%lsv%type = HL_LSVOPER
        CASE( 'NO','NONE','OFF' )
          new%weather%lsv%type = HL_NONE
        CASE( 'MODEL' )
          new%weather%lsv%type = HL_LSVMOD
        CASE( 'INPUT' )
          new%weather%lsv%type = HL_LSVINP
          IF( narg-ikwrd < 3 )THEN
            WRITE(*,'(A)') 'Insufficient ME USELSVAR input'
            WRITE(*,'(A)') 'INPUT requires variance and length scale'
            GOTO 9999
          END IF
          READ(carg(ikwrd+2),*,IOSTAT=ios) new%weather%lsv%uu
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading ME USELVAR UU'
            GOTO 9999
          END IF
          READ(carg(ikwrd+3),*,IOSTAT=ios) new%weather%lsv%sl
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading ME USELVAR SL'
            GOTO 9999
          END IF
        CASE DEFAULT
          new%weather%lsv%type = HL_LSVOPER
      END SELECT
      lSetLSV = .TRUE.
    END IF

  CASE( 'STARTEND' )

    IF( .NOT.lTime )THEN  !Ignore if start/end times set in CO pathway

      IF( narg-ikwrd < 6 )THEN
        WRITE(*,'(A)') 'Insufficient ME STARTEND input'
        GOTO 9999
      END IF

      i = ikwrd + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Year
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND start year'
        GOTO 9999
      END IF

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Month
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND start month'
        GOTO 9999
      END IF

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Day
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND start day'
        GOTO 9999
      END IF

      IF( narg-ikwrd > 6 )THEN
        i = i + 1
        READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Hour
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading ME STARTEND start hour'
          GOTO 9999
        END IF
      ELSE
        new%input%time%start%time%hour = 1.  !Assumed start hour
      END IF

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%end%time%Year
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND end year'
        GOTO 9999
      END IF

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%end%time%Month
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND end month'
        GOTO 9999
      END IF

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%end%time%Day
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading ME STARTEND end day'
        GOTO 9999
      END IF

      IF( narg-ikwrd > 7 )THEN
        i = i + 1
        READ(carg(i),*,IOSTAT=ios) new%input%time%end%time%Hour
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading ME STARTEND end hour'
          GOTO 9999
        END IF
      ELSE
        new%input%time%end%time%hour = 23.999        !Assumed end hour
      END IF

!      new%input%time%start%time%reference = HT_LOCAL !Default, but set here explicitly anyway
!      new%input%time%end%time%reference   = HT_LOCAL

      new%input%time%end%time%runtime = NOT_SET_R

      lTime = .TRUE.

    END IF

  CASE( 'TIMEREF' )

    IF( narg-ikwrd > 0 )THEN
      IF( TRIM(carg(ikwrd+1)) == 'UTC' )THEN
        new%weather%flags%reference = HT_UTC
      ELSE IF( TRIM(carg(ikwrd+1)) == 'LOCAL' )THEN
        new%weather%flags%reference = HT_LOCAL        !This is the default
      END IF
      lSetMetTcnv = .TRUE.
    END IF

!  CASE( 'DAYRANGE' )
    !Ignore

!  CASE( 'SCIMBYHR' )
    !Ignore

!  CASE( 'WDROTATE' )
    !Ignore

!  CASE( 'WINDCATS' )
    !Ignore

  CASE( 'FINISHED' )

  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for ME pathway: '//TRIM(carg(ikwrd))
!    WRITE(*,'(A)') 'Invalid keyword for ME pathway: '//TRIM(carg(ikwrd))
!    GOTO 9999

END SELECT

ParseDataME = SUCCESS

9999 CONTINUE

RETURN
END
