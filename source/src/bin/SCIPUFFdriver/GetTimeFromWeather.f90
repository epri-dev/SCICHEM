INTEGER FUNCTION GetTimeFromWeather()

!------ Get start/end times from met files

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i, j
INTEGER year, month, day, jday, ihour, imin, isec
REAL    hour, lon, tzc

CHARACTER(8) flag

CHARACTER(8), DIMENSION(2) :: string

INTEGER, EXTERNAL :: SkipMetTimeAERMOD

GetTimeFromWeather = FAILURE

IF( nMet == 0 )THEN
  WRITE(*,*) 'No met input files defined'
  GOTO 9999
END IF

!------ Use AERMET SFC or MEDOC files; error otherwise

DO i = 1,nMet

  IF( metInp(i)%type == MET_AERSRF )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_met,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

!------ Set local time reference

    READ(lun_met,FMT='(2(2X,A8))',IOSTAT=ios) string
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading lat/lon from '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    CALL cupper( string(2) )

    j = LEN_TRIM(string(2))
    READ(string(2)(1:j-1),*,IOSTAT=ios) lon
    IF( string(2)(j:j) == 'W' )lon = -lon

    tzc  = FLOAT(INT(lon+7.5)/15)
    IF( lon < -7.5 )tzc = tzc - 1.
    IF( tzc < 0.   )tzc = tzc + 24.

    new%input%time%start%zone = tzc

!------ Read first time for start

    READ(lun_met,*,IOSTAT=ios) year,month,day,jday,hour

    CALL SetYear( year )

    CALL YearMonthDay( hour,year,month,day,new%input%time%start%time%hour,  &
                                           new%input%time%start%time%Year,  &
                                           new%input%time%start%time%Month, &
                                           new%input%time%start%time%Day )

    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading met file '//TRIM(metInp(1)%file)
      GOTO 9999
    END IF

!------ Read to bottom of file for end time

    DO WHILE( ios == 0 )
      READ(lun_met,*,IOSTAT=ios) year,month,day,jday,hour
      IF( ios > 0 )THEN
        WRITE(*,'(A)') 'Error reading met file '//TRIM(metInp(1)%file)
        GOTO 9999
      END IF

      CALL SetYear( year )

      CALL YearMonthDay( hour,year,month,day,new%input%time%end%time%hour, &
                                             new%input%time%end%time%Year,  &
                                             new%input%time%end%time%Month, &
                                             new%input%time%end%time%Day )

    END DO

    lTime = .TRUE.

  ELSE IF( metInp(i)%type == MET_SCIGRD )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_met,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    lformat = .TRUE.
    READ(lun_met,FMT='(A1)',IOSTAT=ios) flag(1:1)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading first character of met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    IF( flag(1:1) /= 'F' .OR. flag(1:1) /= 'f' )THEN
      lformat = .FALSE.
      CLOSE(lun_met,IOSTAT=ios)
      OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_met,STATUS='OLD',FORM='UNFORMATTED', &
                                                          ACTION='READ',IOSTAT=ios)
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
        GOTO 9999
      END IF
      READ(lun_met,IOSTAT=ios) flag
      IF( flag(1:1) /= 'B' .AND. flag(1:1) /= 'b' )THEN
        WRITE(*,*) 'Error reading met file '//TRIM(metInp(i)%file)
        WRITE(*,'(A)') 'First character must be F or B'
        GOTO 9999
      END IF
    END IF

    CALL SkipModelHeaderAERMOD()

!------ Read first time

    ios = SkipMetTimeAERMOD( day,month,year,ihour,imin,isec )
    IF( ios /= 0 )THEN
      WRITE(*,*) 'Error reading first time on met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    hour = FLOAT(ihour) + FLOAT(imin)/60. + FLOAT(isec)/3600.
    CALL SetYear( year )
    CALL YearMonthDay( hour,year,month,day,new%input%time%start%time%hour,  &
                                           new%input%time%start%time%Year,  &
                                           new%input%time%start%time%Month, &
                                           new%input%time%start%time%Day )

!------ Search down file, read times until end

    ios = SkipMetTimeAERMOD( day,month,year,ihour,imin,isec )

    IF( ios /= 0 )THEN
      WRITE(*,*) 'Only one time met file '//TRIM(metInp(i)%file)
      WRITE(*,*) 'At least two required to set run time'
      GOTO 9999
    END IF

    DO WHILE( ios == 0 )

      hour = FLOAT(ihour) + FLOAT(imin)/60. + FLOAT(isec)/3600.
      CALL SetYear( year )
      CALL YearMonthDay( hour,year,month,day,new%input%time%end%time%hour,  &
                                             new%input%time%end%time%Year,  &
                                             new%input%time%end%time%Month, &
                                             new%input%time%end%time%Day )

      ios = SkipMetTimeAERMOD( day,month,year,ihour,imin,isec )

    END DO

    lTime = .TRUE.

  END IF

END DO

IF( .NOT.lTime )THEN
  WRITE(*,*) 'SFC or MEDOC file required to set calculation times'
  GOTO 9999
END IF

GetTimeFromWeather = SUCCESS

9999 CONTINUE

CLOSE(UNIT=lun_met,IOSTAT=ios)

RETURN
END

#if !defined(LINUX)
!===============================================================================

SUBROUTINE SetYear( iyy )

USE default_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: iyy

IF( iyy /= NOT_SET_I )THEN

  IF( iyy < 50 )THEN
    iyy = iyy + 2000
  ELSE IF( iyy < 100 )THEN
    iyy = iyy + 1900
  END IF

END IF

RETURN
END

!==============================================================================

SUBROUTINE YearMonthDay( t,yr,mnth,day,to,yro,mntho,dayo )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: yr ,mnth ,day
REAL,    INTENT( IN  ) :: t
INTEGER, INTENT( OUT ) :: yro,mntho,dayo
REAL,    INTENT( OUT ) :: to

INTEGER days, iday, jul, julo
LOGICAL leap

INTEGER, EXTERNAL :: julian_day
LOGICAL, EXTERNAL :: leap_year

INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
           (/0,31,59,90,120,151,181,212,243,273,304,334/)

iday = INT(t/24.)
to   = t - 24.*FLOAT(iday)
IF( to < 0. )THEN
  iday = iday - 1
  to = to + 24.
END IF

jul  = julian_day( mnth,day,yr )
julo = jul + iday

yro = yr

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo <= 0 )
  julo = julo + days
  yro  = yro - 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo > days )
  julo = julo - days
  yro  = yro + 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

DO mntho = 12,1,-1
  IF( mntho >= 3 .AND. leap )THEN
    days = NDAY(mntho) + 1
  ELSE
    days = NDAY(mntho)
  END IF
  IF( julo > days )EXIT
END DO

dayo = julo - days

RETURN
END
#endif

!==============================================================================
! MEDOC file subroutines
!==============================================================================

SUBROUTINE SkipModelHeaderAERMOD()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i, nStg3D, nStg2D

CHARACTER(8) mapProj, string

!------ Rewind to first record

REWIND(lun_met,IOSTAT=ios)

!------ For formatted file, skip until "standard" format record

IF( lformat )THEN

  mapProj = 'XXXXXXXX'

  DO WHILE( TRIM(mapProj) /= 'FFFFFFFF' )
    READ(lun_met,'(A8)',IOSTAT=ios) mapProj
    IF( ios /= 0 )EXIT
    CALL CUPPER( mapProj )
  END DO

  BACKSPACE(lun_met,IOSTAT=ios) !Position to read format record

!------ Binary files requires more "parsing" of records

ELSE

  READ(lun_met,IOSTAT=ios) mapProj
  IF( ios /= 0 )GOTO 9999
  IF( mapProj == 'BBBBBBBB' )THEN
    BACKSPACE(lun_met,IOSTAT=ios) !Position to read format record
    GOTO 9999
  END IF

!------ Skip map projection data record (where appropriate)

  SELECT CASE( TRIM(mapProj(2:8)) )
    CASE( 'LAMBERT','POLAR','MERCATR','MERCATO','MERCTR','UTM' )
      READ(lun_met,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9999
  END SELECT

!------ Check vertical coordinate

  READ(lun_met,IOSTAT=ios) string
  IF( ios /= 0 )GOTO 9999

!------ Skip sigma coordinate inputs

  SELECT CASE( TRIM(ADJUSTL(string)) )
    CASE( 'SIGMA','SIGMAF' )

      string = TRIM(ADJUSTL(string))
      READ(lun_met,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9999

      IF( TRIM(string) == 'SIGMAF' )THEN
        READ(lun_met,IOSTAT=ios)
        IF( ios /= 0 )GOTO 9999
      END IF

  END SELECT

!------ Skip grid staggering records

  READ(lun_met,IOSTAT=ios) nStg3D, nStg2D
  IF( ios /= 0 )GOTO 9999

  IF( nStg3d > 0 )THEN
    DO i = 1,nStg3d
      READ(lun_met,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9999
    END DO
  END IF

  IF( nStg2d > 0 )THEN
    DO i = 1,nStg2d
      READ(lun_met,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9999
    END DO
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipMetTimeAERMOD( day,month,year,ihour,imin,isec ) RESULT( ios )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: day, month, year, ihour, imin, isec

INTEGER imax, jmax, kmax,n2, n3, i

INTEGER, EXTERNAL :: SkipMetHeaderAERMOD, SkipMet3DAERMOD, SkipMet2DAERMOD

ios = SkipMetHeaderAERMOD( imax,jmax,kmax,n2,n3,day,month,year,ihour,imin,isec )
IF( ios /= 0 )GOTO 9999

DO i = 1,n3
  ios = SkipMet3DAERMOD( imax,jmax,kmax )
  IF( ios /= 0 )GOTO 9999
END DO

DO i = 1,n2
  ios = SkipMet2DAERMOD( imax,jmax )
  IF( ios /= 0 )GOTO 9999
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipMetHeaderAERMOD( imax,jmax,kmax,n2,n3,day,month,year,ihour,imin,isec ) &
                                                                        RESULT( ios )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: imax, jmax, kmax, n2, n3
INTEGER, INTENT( OUT ) :: day,month,year,ihour,imin,isec

REAL    dum
INTEGER ndum, n, k

CHARACTER(8) cdum

IF( lformat )THEN
  READ(lun_met,*,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,*,IOSTAT=ios)   !Skip codename record
  IF( ios == 0 )READ(lun_met,'(6(I12,1X))',IOSTAT=ios) day,month,year,ihour,imin,isec
  IF( ios == 0 )READ(lun_met,*,IOSTAT=ios)   !Skip dummy record
  IF( ios == 0 )READ(lun_met,'(6(I12,1X))',IOSTAT=ios) imax,jmax,kmax,ndum,n3,n2
  IF( ios == 0 )READ(lun_met,*,IOSTAT=ios)   !Skip grid records
  IF( ios == 0 )READ(lun_met,*,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,'(6(F12.4,1X))',IOSTAT=ios) (dum,k=1,kmax+11)
  IF( ios == 0 .AND. ndum > 0 )THEN         !Skip field names
    READ(lun_met,'(6(A8,1X))',IOSTAT=ios) (cdum,n=1,ndum), &
                (cdum,n=1,n3),(cdum,n=1,n3), &
                (cdum,n=1,n2),(cdum,n=1,n2)
  ELSE
    READ(lun_met,'(6(A8,1X))',IOSTAT=ios) &
                (cdum,n=1,n3),(cdum,n=1,n3), &
                (cdum,n=1,n2),(cdum,n=1,n2)
  END IF
ELSE
  READ(lun_met,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)   !Skip codename record
  IF( ios == 0 )READ(lun_met,IOSTAT=ios) day,month,year,ihour,imin,isec
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)   !Skip dummy record
  IF( ios == 0 )READ(lun_met,IOSTAT=ios) imax,jmax,kmax,ndum,n3,n2
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)
  IF( ios == 0 )READ(lun_met,IOSTAT=ios)  !Skip field names
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipMet3DAERMOD( imax,jmax,kmax ) RESULT( ios )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imax, jmax, kmax

INTEGER i, ntot
REAL    dum

ntot = imax*jmax*kmax

IF( lformat )THEN
  READ(lun_met,'(6(F12.4,1X))',IOSTAT=ios) (dum,i=1,ntot)
ELSE
  READ(lun_met,IOSTAT=ios) (dum,i=1,ntot)
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipMet2DAERMOD( imax,jmax ) RESULT( ios )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imax, jmax

INTEGER i, ntot
REAL    dum

ntot = imax*jmax

IF( lformat )THEN
  READ(lun_met,'(6(F12.4,1X))',IOSTAT=ios) (dum,i=1,ntot)
ELSE
  READ(lun_met,IOSTAT=ios) (dum,i=1,ntot)
END IF

RETURN
END

