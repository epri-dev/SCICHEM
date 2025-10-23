INTEGER FUNCTION GenSCIPUFFSensor()

!------ Generate SCIPUFF sensors from AERMOD receptors

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, irv, i

CHARACTER(300) string

INTEGER, EXTERNAL :: GenPolarGrid, GenRectGrid, GenDiscList

GenSCIPUFFSensor = FAILURE

!------ Initialize number of samplers to 0

nsamp = 0

!------ Initialize for finding computational domain

xmin = HUGE(0.); xmax = -xmin; ymin = xmin; ymax = xmax

!------ Check if sampler input file specified
!       If so, all other receptor input (if any) ignored

IF( LEN_TRIM(new%input%option%samplerFile) > 0 )THEN
  GenSCIPUFFSensor = SUCCESS
  GOTO 9999
END IF


IF( n_re == 0 .AND. .NOT.ASSOCIATED(First_Disc) )THEN
  !WRITE(*,*) 'Abort run: no receptors defined'
  !WRITE(*,'(A)') '*** Warning ********************************'
  !WRITE(*,'(A)') 'no receptors defined'
  GenSCIPUFFSensor = SUCCESS
  GOTO 9999
END IF

!------ Open sensor input file

new%input%option%samplerFile = TRIM(new%project%name)//'.sam'
!new%input%option%samplerPath = ' ' !TRIM(new%project%path)

OPEN(FILE=TRIM(new%input%option%samplerFile),UNIT=lun_sam,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error opening SCIPUFF sensor input file'
  WRITE(*,'(A)') 'File name = '//TRIM(new%input%option%samplerFile)
  GOTO 9999
END IF

!------ Write header
!       N.B. Coordinates assumed same as project
!       Time-averaging turned off; output at large time-step interval

string = 'SCIPUFF SENSOR'
IF( receptor%lAvg )THEN
  string = TRIM(string)//' TAVG '
  i = LEN_TRIM(string)
  WRITE(string(i+2:),*) receptor%dtSampler
  string = TRIM(string)//' SEC '
ELSE IF( receptor%dtSampler /= DEF_VAL_R )THEN
  string = TRIM(string)//' OUTPUT '
  IF( MOD(receptor%dtSampler,new%input%time%end%step%max) /= 0. )THEN
    WRITE(*,'(A)') 'Output interval must be multiple of basic time step'
    WRITE(*,'(A,ES12.5)') 'Output interval: ',receptor%dtSampler
    WRITE(*,'(A,ES12.5)') 'Time step      : ',new%input%time%end%step%max
    GOTO 9999
  END IF
  i = LEN_TRIM(string)
  WRITE(string(i+2:),*) new%input%time%end%step%max
  string = TRIM(string)//' SEC '
ELSE IF( new%input%option%dtSampler == DEF_VAL_R )THEN
  string = TRIM(string)//' OUTPUT '
  i = LEN_TRIM(string)
  WRITE(string(i+2:),*) new%input%time%end%step%max
  string = TRIM(string)//' SEC '
END IF

WRITE(lun_sam,FMT='(A)',IOSTAT=ios) TRIM(string)

sensorClass = 'CONC'  !Default sensor

!------ Look at receptor networks
!       N.B. These are valid only for cartesian projects

IF( n_re > 0 )THEN

  IF(  .NOT.(new%input%domain%domain%coord == I_CARTESIAN .OR. &
             new%input%domain%domain%coord == I_UTM) )THEN
    WRITE(*,'(A)') 'Receptor networks are only valid with Carteisan or UTM coordinate projects'
    GOTO 9999
  END IF

  DO i = 1,n_re
    SELECT CASE( TRIM(receptor_net(i)%type ) )
      CASE( 'GRIDCART'  )
        irv = GenRectGrid( receptor_net(i) )
        IF( irv /= SUCCESS )GOTO 9999

      CASE( 'GRIDPOLR' )
        irv = GenPolarGrid( receptor_net(i) )
        IF( irv /= SUCCESS )GOTO 9999

      CASE( 'INCLUDED'  )
        GOTO 9999

    END SELECT
  END DO

END IF

!------ Go down linked-list of discrete receptors

IF( ASSOCIATED(First_Disc) )THEN
  irv = GenDiscList()
  IF( irv /= SUCCESS )GOTO 9999
END IF

CLOSE(UNIT=lun_sam,IOSTAT=ios)

GenSCIPUFFSensor = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION GenDiscList()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, imat
REAL    x, y, z
CHARACTER(99) string

GenDiscList = FAILURE

z = 0. !Default FLAGPOLE

!------ Loop over materials

DO imat = 1,nmat

  IF( LEN_TRIM(mtlList(nmat)%file) > 0 )THEN
    sensorClass = 'MC'
  ELSE
    sensorClass = 'CONC'
  END IF

  Disc_re => First_Disc

  DO WHILE( ASSOCIATED(Disc_re) )
    nsamp = nsamp + 1

    x = Disc_re%x * xfac
    y = Disc_re%y * xfac - yoff
    IF( receptor%lFlagPole )THEN
      IF( Disc_re%zflag == NOT_SET_R )THEN
        z = receptor%Flagdf
      ELSE
        z = Disc_re%zflag
      END IF
    ELSE
      z = 0.
    END IF

    IF( sensorClass == 'MC' .AND. LEN_TRIM(MCList) > 0 )THEN
      string = ' '//TRIM(sensorClass)//' '//TRIM(mtlList(imat)%name)//':'//TRIM(MCLIST)
      WRITE(lun_sam,FMT='(2ES15.5,F10.1,A)',ADVANCE='NO',IOSTAT=ios) x,y,z,TRIM(string)
      string = ' '//TRIM(Disc_re%id)
      WRITE(lun_sam,FMT='(A)',ADVANCE='YES',IOSTAT=ios)TRIM(string)
    ELSE
      string = ' '//TRIM(sensorClass)//' '//TRIM(mtlList(imat)%name)//' '//TRIM(Disc_re%id)
      WRITE(lun_sam,FMT='(2ES15.5,F10.1,A)',IOSTAT=ios) x,y,z,TRIM(string)
    END IF
    xmin = MIN(x,xmin); xmax = MAX(x,xmax);
    ymin = MIN(y,ymin); ymax = MAX(y,ymax);

    Disc_re => Disc_re%next
  END DO

END DO

GenDiscList = SUCCESS

RETURN
END

!==============================================================================

INTEGER FUNCTION GenRectGrid( recptr )

USE SCIPUFFdriver_fi

IMPLICIT NONE

TYPE( NetRE ), INTENT( IN ) :: recptr

INTEGER ios, i, j, is, imat
REAL    x, y, z
CHARACTER(12) netName
CHARACTER(20) sensorName
CHARACTER(99) string

IF( receptor%lFlagPole )THEN
  z = receptor%Flagdf
ELSE
  z = 0.
END IF

IF( TRIM(recptr%id) /= 'NOT_SET' )THEN
  netName = TRIM(recptr%id)//'_'
ELSE
  netName = 'GRIDRECT_'
END IF

is = 0

!----- Loop over materials

DO imat = 1,nmat

  IF( LEN_TRIM(mtlList(nmat)%file) > 0 )THEN
    sensorClass = 'MC'
  ELSE
    sensorClass = 'CONC'
  END IF

!----- Loop over grid
!      N.B. GRIDCART input is always meters relative to (0,0)

  DO j = 1,recptr%ny
    y = (recptr%y0 + recptr%y(j)) * 1.E-3 - yoff

    DO i = 1,recptr%nx
      is    = is + 1
      nsamp = nsamp + 1

      x = (recptr%x0 + recptr%x(i)) * 1.E-3

      WRITE(sensorName,FMT='(A,I8.8)') TRIM(netName),is
      string = ' '//TRIM(sensorClass)//' '//TRIM(mtlList(imat)%name)//' '//TRIM(sensorName)
      WRITE(lun_sam,FMT='(2ES15.5,F10.1,A)',IOSTAT=ios) x,y,z,string

      xmin = MIN(x,xmin); xmax = MAX(x,xmax);
      ymin = MIN(y,ymin); ymax = MAX(y,ymax);

    END DO

  END DO

END DO

GenRectGrid = SUCCESS

RETURN
END

!==============================================================================

INTEGER FUNCTION GenPolarGrid( recptr )

USE SCIPUFFdriver_fi

IMPLICIT NONE

TYPE( NetRE ), INTENT( IN ) :: recptr

INTEGER ios, i, j, is, imat
REAL    ang, dist, cs, sn, x, y, z
CHARACTER(12) netName
CHARACTER(20) sensorName
CHARACTER(99) string
REAL, EXTERNAL :: cosd,sind

IF( receptor%lFlagPole )THEN
  z = receptor%Flagdf
ELSE
  z = 0.
END IF

IF( TRIM(recptr%id) /= 'NOT_SET' )THEN
  netName = TRIM(recptr%id)//'_'
ELSE
  netName = 'GRIDPOLR_'
END IF

is = 0

!----- Loop over materials

DO imat = 1,nmat

  IF( LEN_TRIM(mtlList(nmat)%file) > 0 )THEN
    sensorClass = 'MC'
  ELSE
    sensorClass = 'CONC'
  END IF

!----- NY angles; NX radial distances

  DO j = 1,recptr%ny
    ang = recptr%y(j)
    cs = cosd(ang); sn = sind(ang)

    DO i = 1,recptr%nx
      is    = is + 1
      nsamp = nsamp + 1

      dist = recptr%x(i) * 1.E-3 !Assumed to be in meters
      x = dist*cs + recptr%x0
      y = dist*sn + recptr%y0;

      WRITE(sensorName,FMT='(A,I8.8)') TRIM(netName),is
      string = ' '//TRIM(sensorClass)//' '//TRIM(mtlList(imat)%name)//' '//TRIM(sensorName)
      WRITE(lun_sam,FMT='(2ES15.5,F10.1,A)',IOSTAT=ios) x,y,z,string

      xmin = MIN(x,xmin); xmax = MAX(x,xmax);
      ymin = MIN(y,ymin); ymax = MAX(y,ymax);

    END DO

  END DO

END DO

GenPolarGrid = SUCCESS

RETURN
END

