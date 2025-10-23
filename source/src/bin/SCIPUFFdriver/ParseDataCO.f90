INTEGER FUNCTION ParseDataCO()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i, n
REAL    x

CHARACTER(80) string

ParseDataCO = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'TITLEONE' )

    string = ''
    DO i = ikwrd+1,narg
      string = TRIM(string)//' '//carg(i)
    END DO
    new%input%flags%audit%title = TRIM(string)

  CASE( 'TITLETWO' )  !Analyst's name?

    string = ''
    DO i = ikwrd+1,narg
      string = TRIM(string)//' '//carg(i)
    END DO
    new%input%flags%audit%analyst = TRIM(string)

  CASE( 'MODELOPT' )

    DO i = ikwrd+1,narg
      SELECT CASE( TRIM(carg(i)) )
        CASE( 'DEFAULT','DFAULT' )
          new%input%flags%method = IBSET(new%input%flags%method,HFB_DYNAMIC)
        CASE( 'CONC' )
          receptor%lConc = .TRUE.
        CASE( 'DEPOS' )
          receptor%lDepTot= .TRUE.
        CASE( 'DDEP' )
          receptor%lDepDry= .TRUE.
        CASE( 'PASSIVE' )
          new%input%flags%method = IBCLR(new%input%flags%method,HFB_DYNAMIC)
        CASE( 'DENSGAS' )
          new%input%flags%method = IBSET(new%input%flags%method,HFB_DENSE)
        CASE( 'NOSTATIC' )
          new%input%flags%method = IBCLR(new%input%flags%method,HFB_STATIC)
        CASE( 'FASTMODE' )
          new%input%flags%mode = IBSET(new%input%flags%method,HFB_FAST)
        CASE DEFAULT
          !Ignore other strings
       END SELECT
    END DO

  CASE( 'AVERTIME' )

    IF( narg > ikwrd )THEN

      READ(carg(ikwrd+1),*,IOSTAT=ios) receptor%dtSampler
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO AVERTIME'
        GOTO 9999
      END IF
      IF( narg > ikwrd+1 )THEN
        SELECT CASE( TRIM(carg(ikwrd+2)) )
          CASE( 'HOURS' )
            receptor%dtSampler = receptor%dtSampler*3600.
          CASE( 'MINUTES' )
            receptor%dtSampler = receptor%dtSampler*60.
          CASE( 'SECONDS' )
          CASE DEFAULT
            receptor%dtSampler = receptor%dtSampler*3600.  !Default hours
        END SELECT
      ELSE
        receptor%dtSampler = receptor%dtSampler*3600.
      END IF

      new%input%option%timeAvg = receptor%dtSampler

      IF( receptor%dtSampler <= 0. )THEN
        receptor%dtSampler = DEF_VAL_R
      END IF
!      ELSE
!        new%input%option%timeAvg = receptor%dtSampler
!      END IF

      lSetTimeAvg = .TRUE.

    END IF

    receptor%lAvg = receptor%dtSampler /= DEF_VAL_R

!  CASE( 'URBANOPT' )
    !Ignore

  CASE( 'POLLUTID' )

    WRITE(*,'(A)') 'POLLUTID keyword not used in SCICHEM'
    WRITE(*,'(A)') 'Default material is gas TRAC but can be modified in MA pathway'
    WRITE(*,'(A)') 'IMC file must be defined in MA pathway'
    GOTO 9999
!    IF( narg > ikwrd )THEN
!      nmat = 1
!      mtlList(1)%name = TRIM(carg(ikwrd+1))
!      mtlList(1)%type = HM_GAS
!      gasMatl(1)%gasDeposition = 0. !Default
!      gasMatl(1)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDOS) !Initialize to output group dosage
!    END IF

!  CASE( 'HALFLIFE' )
    !Ignore

!  CASE( 'DCAYCOEF' )
    !Ignore

  CASE( 'GASDEPDF' )

    WRITE(*,'(A)') 'GASDEPDF keyword not used in SCICHEM'
    WRITE(*,'(A)') 'Default material is gas TRAC with zero deposition'
    WRITE(*,'(A)') 'But can be modified in MA pathway'
    GOTO 9999
!    IF( narg > ikwrd )THEN
!      READ(carg(ikwrd+1),*,IOSTAT=ios) gasMatl(1)%gasDeposition
!      IF( ios /= 0 )THEN
!        WRITE(*,'(A)') 'Error reading CO GASDEPDF'
!        GOTO 9999
!      END IF
!    END IF

!  CASE( 'GDLANUSE' )
    !Ignore

!  CASE( 'GDSEASON' )
    !Ignore

!  CASE( 'NO2EQUIL' )
    !Ignore

!  CASE( 'NO2STACK' )
    !Ignore

!  CASE( 'OZONEFIL' )
    !Ignore

  CASE( 'FLAGPOLE' )

    receptor%lFlagPole = .TRUE.
    IF( narg > ikwrd )THEN
      READ(carg(ikwrd+1),*,IOSTAT=ios) receptor%Flagdf
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO FLAGPOLE'
        GOTO 9999
      END IF
    END IF

  CASE( 'RUNORNOT' )

    SELECT CASE( TRIM(carg(ikwrd+1)) )
      CASE( 'RUN' )
        lSetupOnly = .FALSE.
      CASE( 'NOT' )
        lSetupOnly = .TRUE.
      CASE DEFAULT
        lSetupOnly = .FALSE.
      END SELECT

!  CASE( 'EVENTFIL' )
    !Ignore

!  CASE( 'SAVEFILE' )
    !Ignore

!  CASE( 'INITFILE' )
    !Ignore

!  CASE( 'MULTYEAR' )
    !Ignore

!  CASE( 'DEBUGOPT' )
    !Ignore

!  CASE( 'ERRORFIL' )
    !Ignore

  CASE( 'PROJECTN' )

    isProj = .TRUE.

    SELECT CASE( TRIM(carg(ikwrd+1)) )
      CASE( 'UTM' )

        IF( narg < ikwrd+5 )THEN
          WRITE(*,'(A)') 'Insufficient CO PROJECTN UTM input'
          WRITE(*,'(A)') 'Requires datum zone hemisphers (N or S) units (KM or METERS)'
          WRITE(*,'(A)') 'Example: CO PROJECTN UTM WGS84 13 N KM'
          GOTO 9999
        END IF

        new%input%domain%domain%coord = I_UTM

        SELECT CASE( TRIM(carg(ikwrd+2)) )
          CASE( 'NAD83','WGS84' )
          CASE DEFAULT
            WRITE(*,'(A)') '*** Warning ********************************'
            WRITE(*,'(A)') 'SCIPUFF only supports NAD83/WGS84 datum'
            WRITE(*,'(A)') 'Inputs using other datums will be inaccurate'
            WRITE(*,'(A)') '********************************************'
        END SELECT

        READ(carg(ikwrd+3),*,IOSTAT=ios) new%input%domain%domain%zoneUTM
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading CO PROJECTN UTM zone'
          GOTO 9999
        END IF

!        IF( carg(ikwrd+4)(1:1) == 'S' )yoff = 10000. !False northing of equator (km)
        IF( carg(ikwrd+4)(1:1) == 'S' )THEN
          IF( new%input%domain%domain%zoneUTM > 0 )new%input%domain%domain%zoneUTM  = -new%input%domain%domain%zoneUTM
        END IF

        IF( carg(ikwrd+5)(1:1) == 'K' )THEN
          xfac = 1.   !Input  be km
        ELSE IF( carg(ikwrd+5)(1:1) == 'M' )THEN
          xfac = 1.E-3
        ELSE
          WRITE(*,'(A)') 'UTM Coordinate system units must be meters or kilometers'
          GOTO 9999
        END IF


      CASE( 'CARTESIAN','CART' )

        IF( narg < ikwrd+7 )THEN
          WRITE(*,'(A)') 'Insufficient CO PROJECTN CARTESIAN input'
          GOTO 9999
        END IF

        line = ''
        DO i = 1,4
          line = TRIM(line)//' '//carg(ikwrd+2+i)  !Skip 'PROJECTN CARTESIAN datum' ('datum' ignored)
        END DO
        READ(line,*,IOSTAT=ios) new%input%domain%reference%lon, &
                                new%input%domain%reference%lat, &
                                new%input%domain%reference%x, &
                                new%input%domain%reference%y
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading CO PROJECTN CARTESIAN reference location'
          GOTO 9999
        END IF

        new%input%domain%domain%coord = I_CARTESIAN

        IF( carg(ikwrd+7)(1:1) == 'K' )THEN
          xfac = 1.      !Input in km
        ELSE IF( carg(ikwrd+7)(1:1) == 'M' )THEN
          xfac = 1.E-3   !Input in m
        ELSE
          WRITE(*,'(A)') 'Cartesian coordinate system units must be meters or kilometers'
          GOTO 9999
        END IF

      CASE( 'LONLAT','LATLON','LL','LLN' )

        new%input%domain%domain%coord = I_LATLON  !All other input ignored
        xfac = 1.

      CASE DEFAULT
        WRITE(*,'(A)') 'Invalid geographic projection: '//ADJUSTL(TRIM(carg(ikwrd+1)))
        GOTO 9999

    END SELECT

    lDomRef = .TRUE.

  CASE( 'DOMAIN' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO DOMAIN input'
      GOTO 9999
    END IF

    SELECT CASE( TRIM(carg(ikwrd+1)) )

    CASE( 'RECEPTOR','RECEPTORS','SENSOR','SENSORS','SAMPLER','SAMPLERS' )

        ! Disable automatic domain setting
        !lDomRecptr = .TRUE.
        WRITE(*,'(A)') 'Automatic setting of DOMAIN from RECEPTORS is not supported'
        WRITE(*,'(A)') 'Please set domain and rerun'
        GOTO 9999

        IF( narg > ikwrd+1 )THEN
          SELECT CASE( TRIM(carg(ikwrd+2)) )
            CASE( 'M','METERS' )
              new%input%domain%domain%coord = I_CARTESIAN !I_METERS
              xfac = 1.E-3
            CASE( 'CARTESIAN','KM','KILOMETERS' )
              new%input%domain%domain%coord = I_CARTESIAN
            CASE( 'LATLON','LL','LLA' )
              new%input%domain%domain%coord = I_LATLON
            CASE( 'UTM' )
              new%input%domain%domain%coord = I_UTM
            CASE DEFAULT
              WRITE(*,'(A)') 'Invalid coordinates for CO DOMAIN RECEPTOR nput'
              GOTO 9999
          END SELECT
        ELSE
          new%input%domain%domain%coord = I_CARTESIAN  !I_METERS
        END IF

      CASE( 'DEFAULT','MET' )
        !Default domain based on met (only valid for MEDOC or terrain input)

      CASE DEFAULT

        IF( narg < ikwrd+5 )THEN
          WRITE(*,'(A)') 'Insufficient CO DOMAIN input'
          GOTO 9999
        END IF

        line = ''
        DO i = 1,5
          line = TRIM(line)//' '//carg(ikwrd+i)
        END DO
        READ(line,*,IOSTAT=ios) new%input%domain%domain%xMin, &
                                new%input%domain%domain%xMax, &
                                new%input%domain%domain%yMin, &
                                new%input%domain%domain%yMax, &
                                new%input%domain%domain%zMax

        lDomLimits = .TRUE.
        lSetZmax   = .TRUE.

    END SELECT

  CASE( 'HORIZRES' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO HORIZRES input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%domain%domain%hRes
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO HORIZRES'
      GOTO 9999
    END IF

  CASE( 'VERTIRES' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO HORIZRES VERTIRES'
      GOTO 9999
    END IF
    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%domain%domain%vRes
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO VERTIRES'
      GOTO 9999
    END IF

  CASE( 'LT_00UTC' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO LT_00UTC input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%time%start%zone
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO LT_00UTC'
      GOTO 9999
    END IF

    lSetTzone = .TRUE.

  CASE( 'TIMEZONE' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO TIMEZONE input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) x
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO TIMEZONE'
      GOTO 9999
    END IF

    IF( x < 0. )THEN
      new%input%time%start%zone = 24. + x
    ELSE
      new%input%time%start%zone = x
    END IF

    lSetTzone = .TRUE.

  CASE( 'GRDSPLIT' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO GRDSPLIT input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%option%mgrd
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO GRDSPLIT'
      GOTO 9999
    END IF

  CASE( 'MASSMIN' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient CO MASSMIN input'
      GOTO 9999
    END IF

    READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%option%massMin
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO MASSMIN'
      GOTO 9999
    END IF

  CASE( 'STARTEND' )

    n = narg-ikwrd

    IF( n < 6 )THEN
      WRITE(*,'(A)') 'Insufficient CO STARTEND input'
      GOTO 9999
    ELSE IF( MOD(n,2) /= 0 )THEN
      WRITE(*,'(A)') 'Invalid CO STARTEND input'
      WRITE(*,'(A)') 'Start and End date/times must use same input'
      GOTO 9999
    ELSE
      n = n/2
    END IF

    i = ikwrd + 1
    READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Year
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND start year'
      GOTO 9999
    END IF

    READ(carg(n+i),*,IOSTAT=ios) new%input%time%end%time%Year
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND end year'
      GOTO 9999
    END IF

    i = i + 1
    READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Month
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND start month'
      GOTO 9999
    END IF

    READ(carg(n+i),*,IOSTAT=ios) new%input%time%end%time%Month
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND end month'
      GOTO 9999
    END IF

    i = i + 1
    READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Day
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND start day'
      GOTO 9999
    END IF

    READ(carg(n+i),*,IOSTAT=ios) new%input%time%end%time%Day
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading CO STARTEND end day'
      GOTO 9999
    END IF

    IF( n > 3 )THEN

      i = i + 1
      READ(carg(i),*,IOSTAT=ios) new%input%time%start%time%Hour
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO STARTEND start hour'
        GOTO 9999
      END IF

      READ(carg(n+i),*,IOSTAT=ios) new%input%time%end%time%Hour
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO STARTEND end hour'
        GOTO 9999
      END IF

      IF( n > 4 )THEN

        i = i + 1
        READ(carg(i),*,IOSTAT=ios) x
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading CO STARTEND start minute'
          GOTO 9999
        END IF
        new%input%time%start%time%Hour = new%input%time%start%time%Hour + x/60.

        READ(carg(n+i),*,IOSTAT=ios) x
        IF( ios /= 0 )THEN
          WRITE(*,'(A)') 'Error reading CO STARTEND end minute'
          GOTO 9999
        END IF
        new%input%time%end%time%Hour = new%input%time%end%time%Hour + x/60.

        IF( n > 5 )THEN

          i = i + 1
          READ(carg(i),*,IOSTAT=ios) x
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading CO STARTEND start second'
            GOTO 9999
          END IF
          new%input%time%start%time%Hour = new%input%time%start%time%Hour + x/3600.

          READ(carg(n+i),*,IOSTAT=ios) x
          IF( ios /= 0 )THEN
            WRITE(*,'(A)') 'Error reading CO STARTEND end second'
            GOTO 9999
          END IF
          new%input%time%end%time%Hour = new%input%time%end%time%Hour + x/3600.

        END IF

      END IF

    ELSE

      new%input%time%start%time%hour = 1.       !Assumed start hour
      new%input%time%end%time%hour   = 23.999   !Assumed end hour

    END IF

!    new%input%time%start%time%reference = HT_LOCAL  !Default, but set here explicitly anyway
!    new%input%time%end%time%reference   = HT_LOCAL

    new%input%time%end%time%runtime = NOT_SET_R

    lTime = .TRUE.

  CASE( 'TIMEREF' )

    IF( narg-ikwrd > 0 )THEN
      IF( TRIM(carg(ikwrd+1)) == 'LOCAL' )THEN
        new%input%time%start%time%reference = HT_LOCAL  !This is the default
        new%input%time%end%time%reference   = HT_LOCAL
      ELSE IF( TRIM(carg(ikwrd+1)) == 'UTC' )THEN
        new%input%time%start%time%reference = HT_UTC
        new%input%time%end%time%reference   = HT_UTC
      END IF
      lSetPrjTcnv = .TRUE.
    END IF

  CASE( 'MAXTSTEP' )

    IF( narg-ikwrd > 0 )THEN

      READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%time%end%step%max
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO MAXTSTEP'
        GOTO 9999
      END IF

      IF( narg-ikwrd > 1 )THEN
        IF( carg(ikwrd+2)(1:1) == 'M' )THEN
          new%input%time%end%step%max = new%input%time%end%step%max*60.
        ELSE IF( carg(ikwrd+2)(1:1) == 'H' )THEN
          new%input%time%end%step%max = new%input%time%end%step%max*3600.
        END IF
      END IF

      lSetDT = .TRUE.

    END IF

  CASE( 'OUTPTINT' )

    IF( narg-ikwrd > 0 )THEN

      READ(carg(ikwrd+1),*,IOSTAT=ios) new%input%time%end%step%output
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading CO OUTPTINT'
        GOTO 9999
      END IF

      IF( narg-ikwrd > 1 )THEN
        IF( carg(ikwrd+2)(1:1) == 'M' )THEN
          new%input%time%end%step%output = new%input%time%end%step%output/60.
        ELSE IF( carg(ikwrd+2)(1:1) == 'S' )THEN
          new%input%time%end%step%output = new%input%time%end%step%output/3600.
        ELSE IF( carg(ikwrd+2)(1:1) /= 'H' )THEN
        END IF
      ELSE
        new%input%time%end%step%output = new%input%time%end%step%output/3600. !Default units are seconds
      END IF                                                                  !but passed to SCIPUFF as hours

      lSetDTout = .TRUE.

    END IF

  CASE( 'MAXPUFFS','MAXMET1D','MAXSURFG' )
    !Ignore as these were read in ReadPathwaySC/ParseSC

  CASE( 'RUNPRIME' )

    IF( narg-ikwrd > 0 )THEN
      SELECT CASE( ADJUSTL(carg(ikwrd+1)(1:1)) )
        CASE( 'Y','y' )
          WRITE(*,'(A)') '**NOTE** Changing RUNPRIME to True from default value of False.'
          WRITE(*,'(A)') 'SCICHEM is setup to ignore building downwash effects by default.'
          WRITE(*,'(A)') 'Please refer to README.md and Users Guide for details.'
          lPRIME = .TRUE.
        CASE( 'N','n' )
          lPRIME = .FALSE.
        CASE DEFAULT
          lPRIME = .FALSE.  ! PRIME default set to false for SCICHEM version 3.1
      END SELECT
    END IF

  CASE( 'DELPRJFI' )
      IF( narg-ikwrd > 0 )lDeletePrj = carg(ikwrd+1)(1:1) == 'Y'

  CASE( 'FINISHED' )

    IF( nmat == 1 )mtlList(1)%matData = TRANSFER(gasMatl(1),mtlList(1)%matData)

    IF( .NOT. isProj )THEN
      WRITE(*,'(A)') 'Missing mandatory keyword PROJECTN'
      WRITE(*,'(A)') 'PROJECTN must be provided'
      GOTO 9999
    END IF

  CASE DEFAULT
    WRITE(*,'(A)') 'Ignored keyword for CO pathway: '//TRIM(carg(ikwrd))

END SELECT

ParseDataCO = SUCCESS

9999 CONTINUE

RETURN
END
