INTEGER FUNCTION ParseDataSO()

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

INTEGER ios, irv, i, j
INTEGER alloc_stat
LOGICAL lexist

CHARACTER(128), EXTERNAL :: BuildFileNameAERMOD

REAL, PARAMETER :: TenYears = 10.*365.*24.

INTEGER, EXTERNAL :: ReallocRelList, FindRelName, FindMatlName, WritePrimeFile
INTEGER, EXTERNAL :: SetSrcParamMC

ParseDataSO = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'INCLUDED' )

    IF( narg > ikwrd )THEN
      OPEN(FILE=TRIM(carg(ikwrd+1)),UNIT=lun_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error opening SO INCLUDED file '//TRIM(carg(ikwrd+1))
        GOTO 9999
      END IF
      lIncluded = .TRUE.
    END IF

  CASE( 'ELEVUNIT' )

    IF( narg > ikwrd )THEN
      IF( TRIM(carg(ikwrd+1) ) == 'FEET' )elevCnv = 0.3048
    END IF

  CASE( 'LOCATION' )

    IF( narg-ikwrd < 4 )THEN
      WRITE(*,'(A)') 'Insufficient SO LOCATION input'
      GOTO 9999
    END IF

    i = new%scnHead%number

    i = i + 1
    IF( i > new%scnHead%max )THEN
      irv = ReallocRelList( 10 )  !Increment by 10
      IF( irv /= SUCCESS )GOTO 9999
    END IF

    new%scnHead%number = i

    relList(i)%relName    = TRIM(carg(ikwrd+1))
    relList(i)%relDisplay = ' '
    relList(i)%status     = 1                   !HS_VALID
    relList(i)%padding    = -65535              !NOT_SET_I

    SELECT CASE( TRIM(carg(ikwrd+2)) )
      CASE( 'POINT','STACK' )
        relList(i)%type = HR_STACK
        CALL InitRelStack()

      CASE( 'PLMRISE' )
        relList(i)%type = HR_PRIME
        CALL InitRelStack()

      CASE( 'SCICONT' )
        relList(i)%type = HR_CONT
        CALL InitRelCont()

      CASE( 'SCIINST' )
        relList(i)%type = HR_INST
        CALL InitRelInst()

      CASE( 'VOLUME' )
        relList(i)%type = HR_VOL
        CALL InitRelCont()

      CASE( 'AREA' )
        relList(i)%type = HR_AREA
        CALL InitRelArea()

      CASE( 'AREACIRC' )
        relList(i)%type = HR_AREAC  !?????
        CALL InitRelStack()

      CASE( 'AREAPOLY' )
        relList(i)%type = -9999

      CASE DEFAULT
        WRITE(*,'(A)') 'Invalid source type for SCIPUFF'
        GOTO 9999

    END SELECT

    SrcFlag(i) = relList(i)%type  !Save original source type (only relevant for Volume source currently)

    READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%xRel
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading SO Xs'
      GOTO 9999
    END IF

    READ(carg(ikwrd+4),*,IOSTAT=ios) relList(i)%yRel
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading SO Ys'
      GOTO 9999
    END IF

    relList(i)%xRel = relList(i)%xRel*xfac
    relList(i)%yRel = relList(i)%yRel*xfac - yoff

    IF( narg-ikwrd > 4 )THEN
      READ(carg(ikwrd+5),*,IOSTAT=ios) relList(i)%zRel !**** not sure if should use this
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading SO Zs'
        GOTO 9999
      END IF
    ELSE
      relList(i)%zRel = 0.
    END IF

    relList(i)%zRel = 0. !*****
    relList(i)%tRel = 0. !*****

    relList(i)%material = TRIM(mtlList(1)%name) !Default to first material

  CASE( 'MATERIAL' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO MATERIAL input'
      GOTO 9999
    END IF

    i = FindRelName( carg(ikwrd+1) )
    IF( i == 0 )THEN
      WRITE(*,'(A)') 'Invalid SO MATERIAL source id'
      GOTO 9999
    END IF

    j = FindMatlName(TRIM(carg(ikwrd+2)))
    IF( j < 1 )THEN
      WRITE(*,'(A)') 'Invalid SO MATERIAL material id'
      GOTO 9999
    END IF

    relList(i)%material = TRIM(mtlList(j)%name)

  CASE( 'MULTCOMP' )

    IF( new%scnHead%number == 0 )THEN
      WRITE(*,'(A)') 'SO MULTCOMP must follow all SO LOCATION inputs'
      GOTO 9999
    END IF

    nMC = narg - ikwrd

    IF( nMC == 0 )THEN
      WRITE(*,*) 'No specie names specified with SO MULTCOMP keyword'
      GOTO 9999
    END IF

    IF( ALLOCATED(MCrate) )DEALLOCATE(MCrate,STAT=alloc_stat )

    ALLOCATE( MCname(nMC),MCrate(nMC,new%scnHead%number),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      WRITE(*,'(A)') 'Error allocating arrays for SO MULTCOMP specie names and rates'
      GOTO 9999
    END IF

    DO i = 1,nMC
       MCname(i) = TRIM(carg(ikwrd+i))
    END DO

    MCrate = 0.

  CASE( 'SRCDURAT','SRCDUR','SRCDURTN' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO SRCDURAT input'
      GOTO 9999
    END IF

    i = FindRelName( carg(ikwrd+1) )
    IF( i == 0 )THEN
      WRITE(*,'(A)') 'Invalid SO SRCDURAT source id'
      GOTO 9999
    END IF

!**** This must be specified before SRCPARAM *****
    READ(carg(ikwrd+2),*,IOSTAT=ios) relStackData%duration

  CASE( 'SRCPARAM' )

    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO SRCPARAM input'
      GOTO 9999
    END IF

    i = FindRelName( carg(ikwrd+1) )
    IF( i == 0 )THEN
      WRITE(*,'(A)') 'Invalid SO SRCPARAM source id'
      GOTO 9999
    END IF

    SELECT CASE(  relList(i)%type )

      CASE( HR_STACK,HR_PRIME,HR_STACKF,HR_STACKFP )

        IF( narg-ikwrd < 6 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM POINT input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relStackData%rate
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO POINT emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO POINT height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relStackData%exitTemp
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO POINT exit temperature'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relStackData%exitVel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO POINT exit velocity'
          GOTO 9999
        END IF

        READ(carg(ikwrd+6),*,IOSTAT=ios) relStackData%diameter
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO POINT stack diameter'
          GOTO 9999
        END IF

        relStackData%rate       = relStackData%rate * 1.E-3       !Convert to kg/s
        relStackData%exitTemp   = relStackData%exitTemp + ABSZERO !Convert to deg-C
        relStackData%activeFrac = 1.0

        IF( relStackData%duration == NOT_SET_R )relStackData%duration = TenYears

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relStackData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relStackData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relStackData,relList(i)%relData)

        ios = SetSrcParamMC( i,6,'POINT' )
        IF( ios /= 1 )GOTO 9999

      CASE( HR_AREA ) !New area source

        IF( narg-ikwrd < 6 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM AREA input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relAreaData%rate !per unit area
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relAreaData%xlen !X-length
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA X-length'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relAreaData%ylen !Y-length
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA Y-length'
          GOTO 9999
        END IF

        READ(carg(ikwrd+6),*,IOSTAT=ios) relAreaData%ang !Rotation angle (CCW)
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA angle'
          GOTO 9999
        END IF

        IF( narg-ikwrd > 6 )THEN
          READ(carg(ikwrd+7),*,IOSTAT=ios) relAreaData%sigma  !Sigma-z ** Not Used **
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO AREA sigma-z'
            GOTO 9999
          END IF
        END IF

        relAreaData%rate = relAreaData%rate * 1.E-3       !Convert to kg/s
        relAreaData%activeFrac = 1.0

        IF( relAreaData%duration == NOT_SET_R )relAreaData%duration = TenYears

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relAreaData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relAreaData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relAreaData,relList(i)%relData)

        relList(i)%type = HR_AREA

        ios = SetSrcParamMC( i,7,'AREA' )
        IF( ios /= 1 )GOTO 9999

      CASE( -HR_AREA ) !ARERMOD area source; use stack structure to store area parameters

        IF( narg-ikwrd < 6 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM AREA input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relStackData%rate !per unit area
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relStackData%exitTemp !X-length
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA X-length'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relStackData%exitVel  !Y-length
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA Y-length'
          GOTO 9999
        END IF

        READ(carg(ikwrd+6),*,IOSTAT=ios) relStackData%diameter !Rotation angle
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREA angle'
          GOTO 9999
        END IF

        IF( narg-ikwrd > 6 )THEN
          READ(carg(ikwrd+7),*,IOSTAT=ios) relStackData%sigma  !Sigma-z ** Not Used **
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO AREA sigma-z'
            GOTO 9999
          END IF
        END IF

        relStackData%rate = relStackData%rate * 1.E-3       !Convert to kg/s
        relStackData%activeFrac = 1.0

        IF( relStackData%duration == NOT_SET_R )relStackData%duration = TenYears

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relStackData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relStackData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relStackData,relList(i)%relData)

        relList(i)%type = HR_AREA !-HR_STACK  !Set type for later processing

        ios = SetSrcParamMC( i,7,'AREA' )
        IF( ios /= 1 )GOTO 9999

      CASE( HR_AREAC ) !ARERMOD circular area source

        IF( narg-ikwrd < 4 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM AREACIRC input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relAreaData%rate !per unit area
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREACIRC emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREACIRC height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relAreaData%xlen !Radius
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO AREACIRC radius'
          GOTO 9999
        END IF

        IF( narg-ikwrd > 4 )THEN
          READ(carg(ikwrd+5),*,IOSTAT=ios) relAreaData%sigma  !Sigma-z ** Not Used **
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO AREACIRC sigma-z'
            GOTO 9999
          END IF
        END IF

        relAreaData%rate = relAreaData%rate * 1.E-3       !Convert to kg/s
        relAreaData%activeFrac = 1.0

!------ Set parameters for equivalent rectangle area source

        relAreaData%xlen = SQRTPI*relAreaData%xlen   !X-length
        relAreaData%ylen = relAreaData%xlen          !Y-length
        relAreaData%ang  = 0.                        !Rotation

        IF( relAreaData%duration == NOT_SET_R )relAreaData%duration = TenYears

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relAreaData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relAreaData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relAreaData,relList(i)%relData)

        relList(i)%type = HR_AREA  !Set type for later processing

        ios = SetSrcParamMC( i,5,'AREACIRC' )
        IF( ios /= 1 )GOTO 9999

      CASE( HR_INST )

        IF( narg-ikwrd < 6 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM SCIINT input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relInstData%mass
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCIINT mass'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCIINT height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relInstData%buoyancy
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCIINT buoyancy'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relInstData%momentum
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCIINT vertical momentum'
          GOTO 9999
        END IF

        READ(carg(ikwrd+6),*,IOSTAT=ios) relInstData%sigX
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCIINT X-spread'
          GOTO 9999
        END IF

        IF( narg > ikwrd+7 )THEN

          READ(carg(ikwrd+7),*,IOSTAT=ios) relInstData%sigY
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO SCIINT Y-spread'
            GOTO 9999
          END IF

          READ(carg(ikwrd+8),*,IOSTAT=ios) relInstData%sigZ
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO SCIINT vertical spread'
            GOTO 9999
          END IF

        ELSE

          relInstData%sigy = relInstData%sigX
          relInstData%sigZ = relInstData%sigX

        END IF

        relInstData%activeFrac = 1.0

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relInstData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relInstData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relInstData,relList(i)%relData)

        ios = SetSrcParamMC( i,8,'SCIINT' )
        IF( ios /= 1 )GOTO 9999

      CASE( HR_CONT )

        IF( narg-ikwrd < 6 )THEN
          WRITE(*,'(A)') 'Insufficient SO SRCPARAM SCICONT input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relContData%rate
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCICONT emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCICONT height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relContData%buoyancy
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCICONT buoyancy'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relContData%momentum
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCICONT vertical momentum'
          GOTO 9999
        END IF

        READ(carg(ikwrd+6),*,IOSTAT=ios) relContData%sigY
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO SCICONT lateral spread'
          GOTO 9999
        END IF

        IF( narg > ikwrd+6 )THEN
          READ(carg(ikwrd+7),*,IOSTAT=ios) relContData%sigZ
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading SO SCICONT vertical spread'
            GOTO 9999
          END IF
        ELSE
          relContData%sigZ = relContData%sigY
        END IF

        IF( relContData%duration == NOT_SET_R )relContData%duration = TenYears

        relContData%activeFrac = 1.0

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relContData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relContData%distribution = partMatl10%nSizeBins
        END IF

        relList(i)%relData = TRANSFER(relContData,relList(i)%relData)

        ios = SetSrcParamMC( i,7,'SCICONT' )
        IF( ios /= 1 )GOTO 9999

      CASE( HR_VOL )  !AERMOD volume source

        IF( narg-ikwrd < 5 )THEN
          WRITE(*,'(A)') 'Insufficient SO VOLUME input'
          GOTO 9999
        END IF

        READ(carg(ikwrd+2),*,IOSTAT=ios) relContData%rate
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO VOLUME emission rate'
          GOTO 9999
        END IF

        READ(carg(ikwrd+3),*,IOSTAT=ios) relList(i)%zRel
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO VOLUME height'
          GOTO 9999
        END IF

        READ(carg(ikwrd+4),*,IOSTAT=ios) relContData%sigY
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO VOLUME lateral spread'
          GOTO 9999
        END IF

        READ(carg(ikwrd+5),*,IOSTAT=ios) relContData%sigZ
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading SO VOLUME vertical spread'
          GOTO 9999
        END IF

        relContData%rate = relContData%rate * 1.E-3       !Convert to kg/s

        relContData%buoyancy = 0.  !AERMOD volumes sources are passive
        relContData%momentum = 0.
        relContData%activeFrac = 1.0

        IF( relContData%duration == NOT_SET_R )relContData%duration = TenYears

        IF( TRIM( relList(i)%material) == 'PM2.5' )THEN
          relContData%distribution = partMatl2p5%nSizeBins
        ELSE IF( TRIM( relList(i)%material) == 'PM10' )THEN
          relContData%distribution = partMatl10%nSizeBins
        END IF
        relList(i)%relData = TRANSFER(relContData,relList(i)%relData)

        relList(i)%type = HR_CONT  !Set to standard SCIPUFF continuous release type

        ios = SetSrcParamMC( i,5,'VOLUME' )
        IF( ios /= 1 )GOTO 9999

      CASE DEFAULT
        WRITE(*,'(A)') 'Invalid source'
        WRITE(*,*) 'Only POINT, AREA, AREAC, VOLUME, SCICONT and SCIINST recognized'
        GOTO 9999

    END SELECT

  CASE( 'BUILDHGT' )
      IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO BUILDHGT input'
      GOTO 9999
    END IF

    i = WritePrimeFile()
    IF( i < SUCCESS )GOTO 9999

  CASE( 'BUILDLEN' )
    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO BUILDLEN input'
      GOTO 9999
    END IF

    i = WritePrimeFile()
    IF( i < SUCCESS )GOTO 9999

  CASE( 'BUILDWID' )
    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO BUILDWID input'
      GOTO 9999
    END IF

    i = WritePrimeFile()
    IF( i < SUCCESS )GOTO 9999

  CASE( 'XBADJ'    )
    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO XBADJ input'
      GOTO 9999
    END IF

    i = WritePrimeFile()
    IF( i < SUCCESS )GOTO 9999

  CASE( 'YBADJ'    )
    IF( narg-ikwrd < 2 )THEN
      WRITE(*,'(A)') 'Insufficient SO YBADJ input'
      GOTO 9999
    END IF

    i = WritePrimeFile()
    IF( i < SUCCESS )GOTO 9999

!  CASE( 'AREAVERT' )
    !Ignore

!  CASE( 'URBANSRC' )
    !Ignore

!  CASE( 'EMISFACT' )
    !Ignore

!  CASE( 'EMISUNIT' )
    !Ignore

!  CASE( 'CONCUNIT' )
    !Ignore

!  CASE( 'DEPOUNIT' )
    !Ignore

!  CASE( 'PARTDIAM' )
    !Ignore

!  CASE( 'MASSFRAX' )
    !Ignore

!  CASE( 'PARTDENS' )
    !Ignore

!  CASE( 'METHOD_2' )
    !Ignore

!  CASE( 'GASDEPOS' )
    !Ignore

!  CASE( 'NO2RATIO' )
    !Ignore

  CASE( 'HOUREMIS' )

    IF( narg-ikwrd < 1 )THEN
      WRITE(*,'(A)') 'Insufficient SO HOUREMIS input'
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

    emiFile = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in)) !If no path included, input file path used

    INQUIRE(FILE=TRIM(emiFile),EXIST=lexist)
    IF( .NOT.lexist )THEN
      WRITE(*,'(A)') 'Hourly emission file does not exist'
      WRITE(*,'(A)') 'File name = '//TRIM(emiFile)
      GOTO 9999
    END IF

!------ Set file bit and name in release structures.
!       N.B. Assumes all releases get emissions from file

    DO i = 1,new%scnHead%number
      relList(i)%type = IBSET(relList(i)%type,HRB_FILE)
      SELECT CASE( relList(i)%type )
        CASE( HR_STACKF )
          relStackFileData   =  TRANSFER(relList(i)%relData,relStackFileData)
          relStackFileData%relFile = TRIM(emiFile)
          relList(i)%relData = TRANSFER(relStackFileData,relList(i)%relData)
        CASE( HR_AREAF )
          relAreaFileData   =  TRANSFER(relList(i)%relData,relAreaFileData)
          relAreaFileData%relFile = TRIM(emiFile)
          relList(i)%relData = TRANSFER(relAreaFileData,relList(i)%relData)
        CASE( HR_CONTF )
          relContFileData   =  TRANSFER(relList(i)%relData,relContFileData)
          relContFileData%relFile = TRIM(emiFile)
          relList(i)%relData = TRANSFER(relContFileData,relList(i)%relData)
        CASE( HR_STACKFP )
          relStackFileData   =  TRANSFER(relList(i)%relData,relStackFileData)
          relStackFileData%relFile = TRIM(emiFile)
          relList(i)%relData = TRANSFER(relStackFileData,relList(i)%relData)
        CASE DEFAULT
          WRITE(*,'(A)') 'Invalid source type with emissions file'
          GOTO 9999
      END SELECT
    END DO

!  CASE( 'INCLUDED' )
    !Ignore

!  CASE( 'OLMGROUP' )
    !Ignore

!  CASE( 'PSDGROUP' )
    !Ignore

!  CASE( 'SRCGROUP' )
    !Ignore

  CASE( 'AREASRCN' )

    IF( narg > ikwrd )THEN
      READ(carg(ikwrd+1),*,IOSTAT=ios) nAreaSrc
      nAreaSrc = MAX(nAreaSrc,1)
    END IF

  CASE( 'FINISHED' )

  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for SO pathway: '//TRIM(carg(ikwrd))
!    WRITE(*,'(A)') 'Invalid keyword for SO pathway: '//TRIM(carg(ikwrd))
!    GOTO 9999

END SELECT

ParseDataSO = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION WritePrimeFile()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER i, ios

INTEGER, EXTERNAL :: FindRelName

IF( .NOT.lPRIME )THEN     !Ignore all PRIME-related input
  WritePrimeFile = SUCCESS
  IF( .NOT.lPrimeFile )THEN
    WRITE(*,'(A)') 'RUNPRIME model is set to False.'
    WRITE(*,'(A)') '**NOTE** RUNPRIME is set to False by default.'
    WRITE(*,'(A)') 'SCICHEM will ignore building downwash effects.'
    WRITE(*,'(A)') 'Please refer to README.md and Users Guide for details.'
    lPrimeFile = .TRUE.
  END IF
  GOTO 9999
END IF

WritePrimeFile = FAILURE

IF( .NOT.lPrimeFile )THEN

  primeFile = TRIM(prjname)//'.pri'
  OPEN(UNIT=lun_pri,FILE=TRIM(primeFile),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
  IF( ios /= 0 )THEN
    WRITE(*,'(A)') 'Error opening PRIME input file: '//TRIM(primeFile)
    GOTO 9999
  END IF
  lPrimeFile = .TRUE.

END IF

i = FindRelName( carg(ikwrd+1) )
IF( i == 0 )THEN
  WRITE(*,'(A)') 'Invalid SO SRCPARAM source id'
  GOTO 9999
END IF

IF( .NOT.(relList(i)%type /= HR_STACK   .OR. &
          relList(i)%type /= HR_STACKF  .OR. &
          relList(i)%type /= HR_STACKFP .OR. &
          relList(i)%type /= HR_PRIME)   )THEN
  WRITE(*,*) 'Only POINT sources can be associated with buildings'
  GOTO 9999
END IF

IF( lRelPrime(i) .eqv. .FALSE. )THEN
  relList(i)%type = IBSET(relList(i)%type,HRB_PRIME)
  relList(i)%relDisplay = TRIM(primeFile)
  lRelPrime(i) = .TRUE.
END IF

IF( ikwrd == 1 )THEN
  WRITE(lun_pri,FMT='(A)',IOSTAT=ios) 'SO '//TRIM(line)
ELSE
  WRITE(lun_pri,FMT='(A)',IOSTAT=ios) TRIM(line)
END IF
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error writing to PRIME file'
  GOTO 9999
END IF

WritePrimeFile = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReallocRelList( inc )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: inc

INTEGER alloc_stat, i

TYPE( releaseT ), DIMENSION(:), ALLOCATABLE :: tmpList
LOGICAL,          DIMENSION(:), ALLOCATABLE :: tmplRelPrime
REAL,             DIMENSION(:), ALLOCATABLE :: tmpSrcFlag

ReallocRelList = FAILURE

!------ Allocate temporary list

ALLOCATE( tmpList(new%scnHead%max),tmplRelPrime(new%scnHead%max),tmpSrcFlag(new%scnHead%max),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating temporary release array'
  GOTO 9999
END IF

!------ Fill it with current releases

DO i = 1,new%scnHead%max
  tmpList(i)      = relList(i)
  tmplRelPrime(i) = lRelPrime(i)
  tmpSrcFlag(i)   = SrcFlag(i)
END DO

!------ Deallocate old list; reallocate with larger size

DEALLOCATE( relList,lRelPrime,SrcFlag,STAT=alloc_stat )

ALLOCATE( relList(new%scnHead%max+inc),lRelPrime(new%scnHead%max+inc),SrcFlag(new%scnHead%max+inc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error re-allocating new release array'
  GOTO 9999
END IF

!------ Copy current releases back; increment new%scnHead%max

DO i = 1,new%scnHead%max
  relList(i)   = tmpList(i)
  lRelPrime(i) = tmplRelPrime(i)
  SrcFlag(i)   = tmpSrcFlag(i)
END DO

DO i = new%scnHead%max+1,new%scnHead%max+inc
  CALL InitRelList( relList(i) )
END DO

lRelPrime(new%scnHead%max+1:new%scnHead%max+inc) = .FALSE.

new%scnHead%max = new%scnHead%max + inc

!------ Deallocate temporary list

DEALLOCATE( tmpList,tmplRelPrime,tmpSrcFlag,STAT=alloc_stat )

ReallocRelList = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE InitRelStack()

USE SCIPUFFdriver_fi

IMPLICIT NONE

relStackData%distribution = 1         !Default for gas materials
relStackData%rate         = NOT_SET_R
relStackData%duration     = NOT_SET_R
relStackData%diameter     = NOT_SET_R
relStackData%MMD          = NOT_SET_R
relStackData%sigma        = NOT_SET_R
relStackData%exitVel      = NOT_SET_R
relStackData%exitTemp     = NOT_SET_R
relStackData%dryFrac      = NOT_SET_R
relStackData%nextUpdtTime = DEF_VAL_R
relStackData%padding      = NOT_SET_I

RETURN
END

!==============================================================================

SUBROUTINE InitRelArea()

USE SCIPUFFdriver_fi

IMPLICIT NONE

relAreaData%distribution = 1         !Default for gas materials
relAreaData%rate         = NOT_SET_R
relAreaData%duration     = NOT_SET_R
relAreaData%xlen         = NOT_SET_R
relAreaData%ylen         = NOT_SET_R
relAreaData%ang          = NOT_SET_R
relAreaData%MMD          = NOT_SET_R
relAreaData%exitVel      = NOT_SET_R
relAreaData%exitTemp     = NOT_SET_R
relAreaData%dryFrac      = NOT_SET_R
relAreaData%nextUpdtTime = DEF_VAL_R
relAreaData%padding      = NOT_SET_I

RETURN
END

!==============================================================================

SUBROUTINE InitRelCont()

USE SCIPUFFdriver_fi

IMPLICIT NONE

relContData%distribution = 1         !Default for gas materials
relContData%rate         = NOT_SET_R
relContData%duration     = NOT_SET_R
relContData%sigY         = NOT_SET_R
relContData%sigZ         = NOT_SET_R
relContData%MMD          = NOT_SET_R
relContData%sigma        = NOT_SET_R
relContData%momentum     = NOT_SET_R
relContData%buoyancy     = NOT_SET_R
relContData%dryFrac      = NOT_SET_R
relContData%nextUpdtTime = DEF_VAL_R
relContData%padding      = NOT_SET_I

RETURN
END

!==============================================================================

SUBROUTINE InitRelInst()

USE SCIPUFFdriver_fi

IMPLICIT NONE

relInstData%distribution = 1         !Default for gas materials
relInstData%mass         = NOT_SET_R
relInstData%sigX         = NOT_SET_R
relInstData%sigY         = NOT_SET_R
relInstData%sigZ         = NOT_SET_R
relInstData%MMD          = NOT_SET_R
relInstData%sigma        = NOT_SET_R
relInstData%momentum     = NOT_SET_R
relInstData%buoyancy     = NOT_SET_R
relInstData%dryFrac      = NOT_SET_R
relInstData%nRandom      = NOT_SET_I
relInstData%ranSpread    = NOT_SET_I
relInstData%padding      = NOT_SET_I

RETURN
END

!==============================================================================

INTEGER FUNCTION SetSrcParamMC( i,i0,srcType ) RESULT( irv )

USE SCIPUFFdriver_fi

INTEGER,      INTENT( IN ) :: i,i0
CHARACTER(*), INTENT( IN ) :: srcType

INTEGER j, ios

IF( nMC < 1 )THEN
  irv = 1
  GOTO 9999
ELSE
  irv = 0
END IF

IF( narg-ikwrd >= i0+nMC )THEN
  DO j = 1,nMC
    READ(carg(ikwrd+i0+j),*,IOSTAT=ios) MCrate(j,i)
    IF( ios /= 0 )THEN
      irv = 1
      WRITE(*,'(A)') 'Error reading SO SRCPARAM '//TRIM(srcType)//' multicomponent specie rate'
      WRITE(*,*) 'Source ',TRIM(carg(ikwrd+1)),' Specie ',MCname(j)
      GOTO 9999
    END IF
  END DO
ELSE
  DO j = 1,nMC
    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
    IF( lerr )THEN
      WRITE(*,'(A)') 'Error reading multicomponent input following SO SRCPARAM '//TRIM(srcType)//' line'
      WRITE(*,'(A)') 'Specie '//MCname(j)
      GOTO 9999
    ELSE IF( narg < 2 )THEN
      WRITE(*,'(A)') 'Invalid multicomponent input following SO SRCPARAM '//TRIM(srcType)//' line'
      WRITE(*,'(A)') 'Specie '//MCname(j)
      GOTO 9999
    ELSE IF( TRIM(carg(1)) /= TRIM(MCname(j)) )THEN
      WRITE(*,'(A)') 'Wrong multicomponent specie: ',TRIM(carg(1))
      WRITE(*,'(A)') 'Expecting '//TRIM(MCname(j))
      GOTO 9999
    END IF
    READ(carg(2),*,IOSTAT=ios) MCrate(j,i)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading multicomponent input following SO SRCPARAM '//TRIM(srcType)//' line'
      GOTO 9999
    END IF
  END DO
END IF

irv = 1

9999 CONTINUE

RETURN
END
