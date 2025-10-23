INTEGER FUNCTION ParseDataRE()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER alloc_stat, ios, i
INTEGER :: startIndx, endIndx
REAL    Xinit, Xdelta, Yinit, Ydelta, Dirini, Dirinc, dist, ang

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: BuildFileNameAERMOD

INTEGER, EXTERNAL :: StartArg, FindRelName
REAL,    EXTERNAL :: sind, cosd

ParseDataRE = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'INCLUDED' )

    IF( narg > ikwrd )THEN
      OPEN(FILE=TRIM(carg(ikwrd+1)),UNIT=lun_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error opening RE INCLUDED file '//TRIM(carg(ikwrd+1))
        GOTO 9999
      END IF
      lIncluded = .TRUE.
    END IF

  CASE( 'SAMPFILE','SAMFILE','SMPFILE','RCPTFILE','RECPFILE' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient RE SAMPFILE input'
      GOTO 9999
    END IF

    BACKSPACE(lun,IOSTAT=ios)

#if defined(LINUX)
!------ Re-read line "as is" since case matters for path and file names on Linux
!       and to handle files names with spaces

    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )

#else
!------ Re-read line to handle files names with spaces

    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    new%input%option%samplerFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))

  CASE( 'DELTSAMP' )

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%option%dtSampler  !N.B. Not the same as output interval in SAM file (receptor%dtSampler)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading RE DELTSAMP '
      GOTO 9999
    END IF

  CASE( 'ELEVUNIT' )

    IF( narg > ikwrd )THEN
      IF( TRIM(carg(ikwrd+1) ) == 'FEET' )receptor%elevCnv = 0.3048
    END IF

  CASE( 'GRIDCART' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient RE GRIDCART input'
      GOTO 9999
    END IF

    IF( TRIM(carg(ikwrd+2)) == 'STA' )THEN
      n_re = n_re + 1
      i_re = n_re
      IF( i_re > MAXNUMRE )THEN
        WRITE(*,'(A)') 'Too many receptor networks'
        WRITE(*,'(A,I2)') 'Maximum is ',MAXNUMRE
        GOTO 9999
      END IF
      receptor_net(i_re)%id   = TRIM(carg(ikwrd+1))
      receptor_net(i_re)%type = 'GRIDCART'
    END IF

    receptor_net(i_re)%x0    = 0. !NOT_SET_R
    receptor_net(i_re)%y0    = 0. !NOT_SET_R
    receptor_net(i_re)%srcid = 'NOT_SET'

    DO
      line = ' '
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
      IF( lerr )THEN
        WRITE(*,'(A)') 'Error reading RE GRIDCART parameters'
        GOTO 9999
      END IF
      IF( carg(1)(1:1) == '*' .OR. narg == 1 )CYCLE

      ikwrd = StartArg( 'RE','GRIDCART',receptor_net(i_re)%id )

      SELECT CASE( TRIM(carg(ikwrd)) )

        CASE( 'XYINC' )
          IF( narg-ikwrd < 6 )THEN
            WRITE(*,'(A)') 'Insufficient RE GRIDCART XYINC input'
            GOTO 9999
          END IF
          receptor_net(i_re)%srcid = 'NOT_SET'

          line = ''
          DO i = 1,6
            line = TRIM(line)//' '//carg(ikwrd+i)
          END DO
          READ(line,*,IOSTAT=ios) Xinit,receptor_net(i_re)%nx,Xdelta, &
                                  Yinit,receptor_net(i_re)%ny,Ydelta
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading RE GRIDCART XYINC input'
            GOTO 9999
          END IF

          ALLOCATE( receptor_net(i_re)%x(receptor_net(i_re)%nx),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDCART x-grid'
            GOTO 9999
          END IF
          DO i = 1,receptor_net(i_re)%nx
            receptor_net(i_re)%x(i) = Xinit + FLOAT(i-1)*Xdelta
          END DO

          ALLOCATE( receptor_net(i_re)%y(receptor_net(i_re)%ny),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDCART y-grid'
            GOTO 9999
          END IF
          DO i = 1,receptor_net(i_re)%ny
            receptor_net(i_re)%y(i) = Yinit + FLOAT(i-1)*Ydelta
          END DO

        CASE( 'XPNTS' )
          receptor_net(i_re)%nx = narg-ikwrd
          ALLOCATE( receptor_net(i_re)%x(narg-ikwrd),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDPOLAR DIST'
            GOTO 9999
          END IF
          DO i = ikwrd+1,narg
            READ(carg(i),*,IOSTAT=ios) receptor_net(i_re)%x(i-ikwrd)
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDCART XPNTS value'
              GOTO 9999
            END IF
          END DO

        CASE( 'YPNTS' )

          receptor_net(i_re)%ny = narg-ikwrd
          ALLOCATE( receptor_net(i_re)%y(narg-ikwrd),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDPOLAR DDIR'
            GOTO 9999
          END IF
          DO i = ikwrd+1,narg
            READ(carg(i),*,IOSTAT=ios) receptor_net(i_re)%y(i-ikwrd)
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDCART YPNTS value'
              GOTO 9999
            END IF
          END DO

        CASE( 'END' )
          CONTINUE
          EXIT

        CASE DEFAULT
          IF( narg-ikwrd > 1 )THEN
            IF( TRIM(carg(ikwrd+2)) == 'END' )EXIT
          END IF
          WRITE(*,*) 'Unrecognized RE GRIDCART input: '//TRIM(carg(ikwrd))
          GOTO 9999

      END SELECT

    END DO

  CASE( 'GRIDPOLR' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient RE GRIDPOLR input'
      GOTO 9999
    END IF

    IF( TRIM(carg(ikwrd+2)) == 'STA' )THEN
      n_re = n_re + 1
      i_re = n_re
      IF( i_re > MAXNUMRE )THEN
        WRITE(*,'(A)') 'Too many receptor networks'
        WRITE(*,'(A,I2)') 'Maximum is ',MAXNUMRE
        GOTO 9999
      END IF
      receptor_net(i_re)%id   = TRIM(carg(ikwrd+1))
      receptor_net(i_re)%type = 'GRIDPOLR'
    END IF

    DO
      line = ' '
      CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
      IF( lerr )THEN
        WRITE(*,'(A)') 'Error reading RE GRIDPOLAR parameters'
        GOTO 9999
      END IF
      IF( carg(1)(1:1) == '*' .OR. narg == 1 )CYCLE

      ikwrd = StartArg( 'RE','GRIDPOLR',receptor_net(i_re)%id )

      IF( TRIM(carg(ikwrd)) == TRIM(receptor_net(i_re)%id) )THEN
        ikwrd = ikwrd + 1
        IF( ikwrd > narg )CYCLE
      END IF

      SELECT CASE( TRIM(carg(ikwrd)) )

        CASE( 'ORIG' )
          IF( narg-ikwrd == 0 )THEN
            receptor_net(i_re)%srcid = 'NOT_SET'
            receptor_net(i_re)%x0 = 0.
            receptor_net(i_re)%y0 = 0.
          ELSE IF( narg-ikwrd == 1 )THEN
            receptor_net(i_re)%srcid = TRIM(carg(ikwrd+1))
            i = FindRelName( carg(ikwrd+1) )
            IF( i == 0 )THEN
              WRITE(*,'(A)') 'Invalid RE GRIDPOLR source id'
              GOTO 9999
            END IF
            receptor_net(i_re)%x0 = relList(i)%xRel  !N.B. Already converted to km
            receptor_net(i_re)%y0 = relList(i)%yRel
          ELSE
            receptor_net(i_re)%srcid = 'NOT_SET'
            READ(carg(ikwrd+1),*,IOSTAT=ios) receptor_net(i_re)%x0
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDPOLAR x-origin'
              GOTO 9999
            END IF
            READ(carg(ikwrd+2),*,IOSTAT=ios) receptor_net(i_re)%y0
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDPOLAR y-origin'
              GOTO 9999
            END IF
          END IF

        CASE( 'DIST' )
          receptor_net(i_re)%nx = narg-ikwrd
          ALLOCATE( receptor_net(i_re)%x(narg-ikwrd),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDPOLAR DIST'
            GOTO 9999
          END IF
          DO i = ikwrd+1,narg
            READ(carg(i),*,IOSTAT=ios) receptor_net(i_re)%x(i-ikwrd)  !Meters
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDPOLAR DIST value'
              GOTO 9999
            END IF
          END DO

        CASE( 'DDIR' )
          receptor_net(i_re)%ny = narg-ikwrd
          ALLOCATE( receptor_net(i_re)%y(narg-ikwrd),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDPOLAR DDIR'
            GOTO 9999
          END IF
          DO i = ikwrd+1,narg
            READ(carg(i),*,IOSTAT=ios) receptor_net(i_re)%y(i-ikwrd)
            IF( ios /= 0 )THEN
              WRITE(*,'(A)') 'Error reading RE GRIDPOLAR DDIR value'
              GOTO 9999
            END IF
          END DO

        CASE( 'GDIR' )
          IF( narg-ikwrd < 3 )THEN
            WRITE(*,'(A)') 'Insufficient RE GRIDPOLR GDIR input'
            GOTO 9999
          END IF

          line = TRIM(carg(ikwrd+1))//' '//TRIM(carg(ikwrd+2))//' '//TRIM(carg(ikwrd+3))
          READ(line,*,IOSTAT=ios)  receptor_net(i_re)%ny, Dirini, Dirinc
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading RE GRIDPOLAR GDIR values'
            GOTO 9999
          END IF

          ALLOCATE( receptor_net(i_re)%y(receptor_net(i_re)%ny),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            WRITE(*,'(A)') 'Error allocating array for RE GRIDPOLAR GDIR'
            GOTO 9999
          END IF
          DO i = 1,receptor_net(i_re)%ny
            receptor_net(i_re)%y(i) = Dirini + FLOAT(i-1)*Dirinc
          END DO

        CASE( 'END' )
          EXIT

        CASE DEFAULT
          IF( narg-ikwrd > 1 )THEN
            IF( TRIM(carg(ikwrd+2)) == 'END' )EXIT
          END IF

          WRITE(*,*) 'Unrecognized RE GRIDPOLAR input'
          GOTO 9999

      END SELECT

    END DO

  CASE( 'DISCCART' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient RE DISCCART input'
      GOTO 9999
    END IF

    IF( .NOT.ASSOCIATED(First_Disc) )THEN
      ALLOCATE( First_Disc,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating pointer to first discrete receptor'
        GOTO 9999
      END IF
      NULLIFY( First_Disc%next )
      Disc_re => First_Disc
      ndisc_re = 1
    ELSE
      ALLOCATE( Disc_re%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,*) 'Error allocating pointer to next discrete receptor'
        GOTO 9999
      END IF
      Disc_re => Disc_re%next
      NULLIFY( Disc_re%next )
      ndisc_re = ndisc_re + 1
    END IF

    WRITE(Disc_re%ID,*) ndisc_re
    Disc_re%ID = 'DiscRE_'//TRIM(ADJUSTL(Disc_re%ID))

    line = TRIM(carg(ikwrd+1))//' '//TRIM(carg(ikwrd+2))
    READ(line,*,IOSTAT=ios)  Disc_re%x, Disc_re%y
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading RE DISCCART location'
      GOTO 9999
    END IF

    IF( receptor%lFlagPole .AND. narg-ikwrd > 2 )THEN
      READ(carg(ikwrd+3),*,IOSTAT=ios)  Disc_re%Zflag
    ELSE
      Disc_re%Zflag = NOT_SET_R
    END IF

  CASE( 'DISCPOLR' )

    SELECT CASE( new%input%domain%domain%coord )
      CASE( I_UTM,I_CARTESIAN,I_METERS )
      CASE DEFAULT
        WRITE(*,'(A)') 'Invalid project coordinates for RE DISCPOLR input'
        GOTO 9999
    END SELECT

    IF( narg-ikwrd < 3 )THEN
      WRITE(*,'(A)') 'Insufficient RE DISCPOLR input'
      GOTO 9999
    END IF

    IF( .NOT.ASSOCIATED(First_Disc) )THEN
      ALLOCATE( First_Disc,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating pointer to first discrete receptor'
        GOTO 9999
      END IF
      NULLIFY( First_Disc%next )
      Disc_re => First_Disc
      ndisc_re = 1
    ELSE
      ALLOCATE( Disc_re%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating pointer to next discrete receptor'
        GOTO 9999
      END IF
      Disc_re => Disc_re%next
      NULLIFY( Disc_re%next )
      ndisc_re = ndisc_re + 1
    END IF

    WRITE(Disc_re%ID,*) ndisc_re
    Disc_re%ID = 'DiscRE_'//TRIM(ADJUSTL(Disc_re%ID))

    i = FindRelName( carg(ikwrd+1) )
    IF( i == 0 )THEN
      WRITE(*,'(A)') 'Invalid RE DISCPOLR source id'
      GOTO 9999
    END IF

    line = TRIM(carg(ikwrd+2))//' '//TRIM(carg(ikwrd+3))
    READ(line,*,IOSTAT=ios)  dist,ang
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading RE DISCPOLR location'
      GOTO 9999
    END IF

    ang = 90. - ang
    Disc_re%x = relList(i)%xRel/xfac + dist*cosd(ang)  !N.B. Multiplied by xfac in genSCIPUFFsensor
    Disc_re%y = relList(i)%yRel/xfac + dist*sind(ang)

    IF( receptor%lFlagPole .AND. narg-ikwrd > 2 )THEN
      READ(carg(narg),*,IOSTAT=ios)  Disc_re%Zflag
    ELSE
      Disc_re%Zflag = NOT_SET_R
    END IF

  CASE( 'EVALCART' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient RE EVALCART input'
      GOTO 9999
    END IF

    IF( .NOT.ASSOCIATED(First_Disc) )THEN
      ALLOCATE( First_Disc,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating pointer to first discrete receptor'
        GOTO 9999
      END IF
      NULLIFY( First_Disc%next )
      Disc_re => First_Disc
      ndisc_re = 1
    ELSE
      ALLOCATE( Disc_re%next,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating pointer to next discrete receptor'
        GOTO 9999
      END IF
      Disc_re => Disc_re%next
      NULLIFY( Disc_re%next )
      ndisc_re = ndisc_re + 1
    END IF

    line = TRIM(carg(ikwrd+1))//' '//TRIM(carg(ikwrd+2))
    READ(line,*,IOSTAT=ios)  Disc_re%x, Disc_re%y
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading RE EVALCART location'
      GOTO 9999
    END IF

    IF( receptor%lFlagPole .AND. narg-ikwrd > 2 )THEN
      READ(carg(ikwrd+5),*,IOSTAT=ios)  Disc_re%Zflag  !Assumed to be 5th argument, but ambiguous since some preceeding arguments are optional
    ELSE
      Disc_re%Zflag = NOT_SET_R
    END IF

    IF( narg-ikwrd > 5 )THEN
      Disc_re%ID = 'Arc'//TRIM(carg(ikwrd+6))
      IF( narg-ikwrd > 6 )Disc_re%ID = TRIM(Disc_re%ID)//'_'//TRIM(carg(ikwrd+7))
    ELSE
      WRITE(Disc_re%ID,*) ndisc_re
      Disc_re%ID = 'EvalRE_'//TRIM(ADJUSTL(Disc_re%ID))
    END IF

   CASE( 'MULTLIST' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient RE MULTLIST input'
      GOTO 9999
    END IF

    MCList = TRIM(line)

    CALL deblank(MCList)

    !Check if MCLIST start with "(" and ends with ")"

    startIndx = INDEX(MCList,'(')
    IF( startIndx <= 0) THEN
      WRITE(*,'(A)') 'MULTLIST must start with "("'
      GOTO 9999
    END IF

    endIndx = INDEX(MCList,')')
    IF( endIndx <= 0 ) THEN
      WRITE(*,'(A)') 'MULTLIST must end with ")"'
      GOTO 9999
    END IF

    MCList = MCList(startIndx:endIndx)


  CASE( 'FINISHED' )

  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for RE pathway: '//TRIM(carg(ikwrd))

END SELECT

ParseDataRE = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION StartArg( MyPathway,MySubPathway,NetID ) RESULT( k )

USE SCIPUFFdriver_fi

IMPLICIT NONE

CHARACTER(2), INTENT( IN ) :: MyPathway
CHARACTER(8), INTENT( IN ) :: MySubPathway
CHARACTER(8), INTENT( IN ) :: NetID

k = 1

IF( narg < 2 )RETURN

CALL CUPPER( carg(k) )
IF( TRIM(carg(k)) == MyPathway )k = k + 1

CALL CUPPER( carg(k) )
IF( TRIM(carg(k)) == MySubPathway )k = k + 1

IF( LEN_TRIM(NetID) > 0 )THEN
  CALL CUPPER( carg(k) )
  IF( TRIM(carg(k)) == TRIM(NetID) )k = k + 1
END IF

RETURN
END
