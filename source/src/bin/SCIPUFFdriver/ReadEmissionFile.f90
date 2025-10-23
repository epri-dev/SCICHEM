INTEGER FUNCTION SetEmissions()

!----- Setup for variable emissions.
!      Also used if building data included since PRIME requires updating

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

INTEGER ios, alloc_stat, irv, i, nrel, iRel, j, jRel
INTEGER year, month, day
REAL    hour, rate, temp, vel
LOGICAL lMClist

CHARACTER(32) relName

INTEGER, EXTERNAL :: ParseHourEmis, FindRelName, ReadNextEmission, julian_day

SetEmissions = FAILURE

nrel = nrel0  !new%scnHead%number

ALLOCATE( emiRate1(nrel),emiTemp1(nrel),emiVel1(nrel), &
          emiRate2(nrel),emiTemp2(nrel),emiVel2(nrel), STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating arrays for emission file'
  GOTO 9999
END IF

IF( nMC > 0 )THEN
  ALLOCATE( emiMC1(nMC,nrel),emiMC2(nMC,nrel),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    WRITE(*,'(A)') 'Error allocating multicomponent arrays for emission file'
    GOTO 9999
  END IF
END IF

!------ Set initial emissions based on SO values

DO iRel = 1,nrel

  SELECT CASE( relList(iRel)%type )
    CASE( HR_STACK,HR_PRIME,HR_STACKF,HR_STACKFP )
      relStackData   = TRANSFER(relList(iRel)%relData,relStackData)
      emiRate1(iRel) = relStackData%rate
      IF( iAreaSrc(iRel,1) > 0 )THEN
        emiTemp1(iRel) = DEF_VAL_R
        emiVel1(iRel)  = 0.01
      ELSE
        emiTemp1(iRel) = relStackData%exitTemp - ABSZERO  !Added back in UpdateStackEmission
        emiVel1(iRel)  = relStackData%exitVel
      END IF
    CASE( HR_AREA,HR_AREAF )
      relAreaData   = TRANSFER(relList(iRel)%relData,relAreaData)
      emiRate1(iRel) = relAreaData%rate
      emiTemp1(iRel) = DEF_VAL_R
      emiVel1(iRel)  = 0.01
    CASE( HR_CONT,HR_CONTF )
      relContData    = TRANSFER(relList(iRel)%relData,relContData)
      emiRate1(iRel) = relContData%rate
      emiTemp1(iRel) = relContData%buoyancy
      emiVel1(iRel)  = relContData%momentum
  END SELECT

  emiRate2(iRel) = emiRate1(iRel)
  emiTemp2(iRel) = emiTemp1(iRel)
  emiVel2(iRel)  = emiVel1(iRel)


  IF( nMC > 0 )THEN
    i = (iRel-1)*nMC
    DO j = 1,nMC
      emiMC1(j,iRel) = relMCList(i+j)%MCmass
      emiMC2(j,iRel) = emiMC1(j,iRel)
    END DO
  END IF

END DO

IF( LEN_TRIM(emiFile) > 0 )THEN

  OPEN(UNIT=lun_emi,FILE=TRIM(emiFile),STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    WRITE(*,'(A)') 'Error opening hourly emission file'
    WRITE(*,'(A)') 'File name = '//TRIM(emiFile)
    GOTO 9999
  END IF

!------ Determine which releases are defined in emission file
!       N.B. Assumes all will be defined at beginning of file

  DO i = 1,nrel

    irv = ParseHourEmis( year,month,day,hour,relName,rate,temp,vel,MCrate,lMClist )
    IF( irv /= SUCCESS )GOTO 9999

    iRel = FindRelName( relName )
    IF( iRel == 0 )THEN
      WRITE(*,*) 'No match for source in emission file'
      WRITE(*,*) 'Source name = '//TRIM(relName)
      WRITE(*,'(A)') 'File   name = '//TRIM(emiFile)
      GOTO 9999
    END IF

    IF( relList(iRel)%status == 0 )CYCLE  !If release has already been set

    relList(iRel)%status = 0   !HS_INVALID

    IF( iAreaSrc(iRel,1) > 1 )THEN
      DO j = 1,iAreaSrc(iRel,1)-1
        jRel = iAreaSrc(iRel,2) + j
        relList(jRel)%status = 0
      END DO
    END IF

    SELECT CASE( relList(iRel)%type )
      CASE( HR_STACK,HR_PRIME,HR_STACKF )
        relStackData          = TRANSFER(relList(iRel)%relData,relStackData)
        relStackData%rate     = 0.
        relStackData%exitTemp = 0.
        relStackData%exitVel  = 0.
        relStackData%duration = 1000000.  !Persist indefinitely (reset if emissions file used)
        relList(iRel)%relData = TRANSFER(relStackData,relList(iRel)%relData)
      CASE( HR_AREA,HR_AREAF )
        relAreaData           = TRANSFER(relList(iRel)%relData,relAreaData)
        relAreaData%rate      = 0.
        relAreaData%exitTemp  = 0.
        relAreaData%exitVel   = 0.01
        relAreaData%duration  = 1000000.  !Persist indefinitely (reset if emissions file used)
        relList(iRel)%relData = TRANSFER(relAreaData,relList(iRel)%relData)
      CASE( HR_CONT,HR_CONTF )
        relContData           = TRANSFER(relList(iRel)%relData,relContData)
        relContData%rate      = 0.
        relContData%buoyancy  = 0.
        relContData%momentum  = 0.
        relContData%duration  = 1000000.
        relList(iRel)%relData = TRANSFER(relContData,relList(iRel)%relData)
    END SELECT

    emiRate1(iRel) = 0.; emiTemp1(iRel) = 0.; emiVel1(iRel) = 0.
    emiRate2(iRel) = 0.; emiTemp2(iRel) = 0.; emiVel2(iRel) = 0.

    IF( nMC > 0 )THEN
      emiMC1(:,iRel) = 0.
      emiMC2(:,iRel) = 0.
    END IF


  END DO

  REWIND(lun_emi,IOSTAT=ios)

!------ Set starting Julian day and hour (for computing release time)

  jul0 = julian_day( new%input%time%start%time%Month, &
                     new%input%time%start%time%Day,   &
                     new%input%time%start%time%Year )

  hour0 = new%input%time%start%time%hour
  year0 = new%input%time%start%time%Year

  CALL SetYear( year0 )

  tEmi     = -HUGE(0.)*0.01  !Initialize so that first time will be read when updating release
  tNextEmi = -HUGE(0.)*0.01  !Apply 1E-2 factor to avoid precision problems with very large numbers

  lEmissionFile  = .TRUE.
  lReadEmission  = .TRUE.

ELSE

  lEmissionFile  = .FALSE.
  lReadEmission  = .FALSE.
  tEmi           = 0.
  tNextEmi       = HUGE(0.)*0.01

END IF

!------ Set PRIME releases for update

DO i = 1,nrel
  IF( relList(i)%type == HR_PRIME )relList(i)%status = 0
END DO

lIntrpEmission = .FALSE.  !***Use constant emissions rate (for 1 hour)

SetEmissions = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadNextEmission( t )

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t

INTEGER irv, ios, year, month, day, i
REAL    hour, trel, rate, temp, vel, rho, area
LOGICAL lFirst
LOGICAL lMClist

CHARACTER(32) relName

INTEGER, EXTERNAL :: ParseHourEmis, FindRelName
REAL,    EXTERNAL :: GetRelTime, GetMatlDensity

ReadNextEmission = SUCCESS !(Almost) always a success

IF( .NOT.lReadEmission )GOTO 9999

!------ Read down until emission time is beyond current time

DO WHILE( tNextEmi <= t )

  lFirst = .TRUE.

  DO

!------ Read date, time, release name, emission parameters

    irv = ParseHourEmis( year,month,day,hour,relName,rate,temp,vel,MCrate,lMClist )
    IF( irv /= SUCCESS )THEN
      IF( lFirst )THEN
        emiRate1 = emiRate2; emiTemp1 = emiTemp2; emiVel1 = emiVel2
        IF( nMC > 0 )emiMC1 = emiMC2
        tEmi = tNextEmi
        IF( irv < 0 )THEN
          lReadEmission = .FALSE.   !Assume error is EOF; read no more
        ELSE
          ReadNextEmission = FAILURE
          WRITE(*,'(A,1x,I4,1x,I2,1x,I2,1x,F4.1)') '**ERROR** reading emissions file at ',year,month,day,hour
          WRITE(*,'(A)') 'For Source name '//TRIM(relName)
          WRITE(*,'(A)') 'Check format for emission file '//TRIM(emiFile)
        ENDIF
        GOTO 9999
      ELSE
        BACKSPACE(lun_emi,IOSTAT=ios)
        EXIT
      END IF
    END IF

    tRel = GetRelTime( day,month,year,hour )  !Time since start of calculation

    IF( lFirst )THEN
      emiRate1 = emiRate2; emiTemp1 = emiTemp2; emiVel1 = emiVel2
      IF( nMC > 0 )emiMC1 = emiMC2
      tEmi     = tNextEmi
      tNextEmi = tRel
      lFirst   = .FALSE.
    ELSE IF( tRel > tNextEmi )THEN
      BACKSPACE(lun_emi,IOSTAT=ios)
      IF( lMClist )THEN
        DO i = 1,nMC
          BACKSPACE(lun_emi,IOSTAT=ios)
        END DO
      END IF
      EXIT
    END IF

 !------ Find index into release structure array

    i = FindRelName( relName )
    IF( i == 0 )THEN
      WRITE(*,'(A)') 'Not match for source in emission file'
      WRITE(*,'(A)') 'Source name = '//TRIM(relName)
      WRITE(*,'(A)') 'File   name = '//TRIM(emiFile)
      ReadNextEmission = FAILURE
      GOTO 9999
    END IF

    rate = rate*1.E-3   !Assume rate on file is g/s

!------ Set area source parameters, i.e.
!       multiply rate by area, set zero buoyancy and small velocity (but realizable)

    IF( iAreaSrc(i,1) > 0 )THEN
      temp = DEF_VAL_R
      rho = GetMatlDensity( relList(i)%material )
      IF( rho > 0. )THEN
        vel = MAX(rate/rho,0.01)
      ELSE
        vel = 0.01
      END IF
      relAreaData = TRANSFER(relList(i)%relData,relAreaData)
      area = relAreaData%xlen * relAreaData%ylen
      rate = rate * area
      IF( nMC > 0 )MCrate(:,i) = MCrate(:,i) * area
    END IF

!------ Check that AERMOD-style volume source has zero momentum and buoyancy

    IF( SrcFlag(i) == HR_VOL )THEN
      IF( temp > 0. .OR. vel > 0. )THEN
        WRITE(*,'(A)') 'AERMOD-style volume source cannot have momentum or buoyancy'
        WRITE(*,'(A)') 'Source name = '//TRIM(relName)
        WRITE(*,'(A)') 'File   name = '//TRIM(emiFile)
        ReadNextEmission = FAILURE
        GOTO 9999
      END IF
    END IF

!------ Set release parameters for temporal interpolation

    emiRate2(i) = rate; emiTemp2(i) = temp; emiVel2(i) = vel;
    IF( nMC > 0 )emiMC2(:,i) = MCrate(:,1)

  END DO

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ParseHourEmis( yr,month,day,hour,relName,rate,temp,vel,rateMC,lMClist )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER,      INTENT( OUT ) :: yr,month,day
REAL,         INTENT( OUT ) :: hour
CHARACTER(*), INTENT( OUT ) :: relName
REAL,         INTENT( OUT ) :: rate, temp, vel
REAL, DIMENSION(*), INTENT( OUT ) :: rateMC
LOGICAL,            INTENT( OUT ) :: lMClist

INTEGER i
INTEGER ios, relType

INTEGER, EXTERNAL :: FindRelType

ios = FAILURE
ParseHourEmis = ios

1 line = ' '
CALL get_next_data( lun_emi,line,nch,kwrd,narg,carg,MAXN,lerr )
IF( lerr )GOTO 9999

SELECT CASE( line(1:1) )
  CASE( '#','*','%','!','\','/' )
    GOTO 1
END SELECT

!------ Check if line is constructed properly

CALL CUPPER( carg(1) )
IF( TRIM(carg(1)) == 'SO' )THEN
  IF( TRIM(carg(2)) == 'HOUREMIS' )THEN
    ikwrd = 2                           !Skip 'SO HOUREMIS' keywords
  ELSE
    WRITE(*,'(A)') 'Missing HOUREMIS keyword in emmisions file'
    WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
    GOTO 9999
  END IF
ELSE
  ikwrd = 0
END IF

IF( narg < ikwrd+5 )THEN
  WRITE(*,'(A)') 'Insufficient SO HOUREMIS file input'
  WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
  GOTO 9999
END IF

relName = TRIM(carg(ikwrd+5))  !Release name

relType = FindRelType( relName )

!------ Read date and time

line = ' '
line = TRIM(carg(ikwrd+1))//' '//TRIM(carg(ikwrd+2))//' '// &
       TRIM(carg(ikwrd+3))//' '//TRIM(carg(ikwrd+4))
READ(line,*,IOSTAT=ios) yr,month,day,hour
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error reading date/time in emission file'
  WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
  GOTO 9999
END IF

!------ Read release parameters.
!       N.B. 'Missing' values set to zero

rate = 0.; temp = 0.; vel = 0.
IF( nMC > 0 )rateMC(1:nMC) = 0.

IF( narg > ikwrd+5 )THEN
  READ(carg(ikwrd+6),*,IOSTAT=ios) rate
  IF( ios /= 0 )THEN
    WRITE(*,'(A)') 'Error reading emmisions rate'
    WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
    GOTO 9999
  END IF
END IF

IF( narg > ikwrd+6 )THEN
  READ(carg(ikwrd+7),*,IOSTAT=ios) temp
  IF( ios /= 0 )THEN
    WRITE(*,'(A)') 'Error reading emmisions temperature'
    WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
    GOTO 9999
  END IF
END IF

IF( narg > ikwrd+7 )THEN
  READ(carg(ikwrd+8),*,IOSTAT=ios) vel
  IF( ios /= 0 )THEN
    WRITE(*,'(A)') 'Error reading emmisions velocity'
    WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
    GOTO 9999
  END IF
END IF

lMClist = .FALSE.
IF( nMC > 0 )THEN
  IF( narg-ikwrd >= 8+nMC )THEN
    DO i = 1,nMC
      READ(carg(ikwrd+8+i),*,IOSTAT=ios) rateMC(i)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading SO HOUREMIS multicomponent specie rate'
        WRITE(*,'(A)') 'Specie '//MCname(i)
        WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
        GOTO 9999
      END IF
    END DO
  ELSE
    lMClist = .TRUE.
    DO i = 1,nMC
      CALL get_next_data( lun_emi,line,nch,kwrd,narg,carg,MAXN,lerr )
      IF( lerr )THEN
        WRITE(*,'(A)') 'Error reading SO HOUREMIS multicomponent specie rate'
        WRITE(*,'(A)') 'Specie '//MCname(i)
        WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
        GOTO 9999
      ELSE IF( narg < 2 )THEN
        WRITE(*,'(A)') 'Invalid multicomponent input following SO HOUREMIS line'
        WRITE(*,'(A)') 'Specie '//MCname(i)
        WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
        GOTO 9999
      ELSE IF( TRIM(carg(1)) /= TRIM(MCname(i)) )THEN
        WRITE(*,'(A)') 'Wrong multicomponet specie: ',TRIM(carg(1))
        WRITE(*,'(A)') 'Expecting '//TRIM(MCname(i))
        WRITE(*,'(A)') 'File name = '//TRIM(ADJUSTL(emiFile))
        GOTO 9999
      END IF
      READ(carg(2),*,IOSTAT=ios) rateMC(i)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading multicomponent input following SO HOUREMIS line'
        GOTO 9999
      END IF
    END DO
  END IF
END IF

ParseHourEmis = SUCCESS

9999 CONTINUE

IF( ios /= 0 )ParseHourEmis = ios

RETURN
END

!==============================================================================

INTEGER FUNCTION FindRelName( relName ) RESULT( iRel )

!------ Match release name in list.
!       N.B. Assumes that "extra" releases associated with area sources
!       come after the initial source and that they have the same name

USE SCIPUFFdriver_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: relName

INTEGER i

iRel = 0

DO i = 1,new%scnHead%number
  IF( TRIM(relName) == TRIM(relList(i)%relName) )THEN
    iRel = i; EXIT
  END IF
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION FindRelType( relName ) RESULT( relType )

!------ Match release name in list.
!       N.B. Assumes that "extra" releases associated with area sources
!       come after the initial source and that they have the same name

USE SCIPUFFdriver_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: relName

INTEGER i

relType = -999

DO i = 1,new%scnHead%number
  IF( TRIM(relName) == TRIM(relList(i)%relName) )THEN
    relType = relList(i)%type
    EXIT
  END IF
END DO

RETURN
END


