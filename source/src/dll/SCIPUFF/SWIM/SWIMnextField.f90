INTEGER FUNCTION SWIMnextField( t,fld )

USE SWIM_fi
USE SWIMparam_fd
USE message_fd

IMPLICIT NONE

REAL, INTENT( IN )                :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, iflag
INTEGER hour, min, sec, year, month, day
LOGICAL lymd
INTEGER is, k, n1, n2
REAL, DIMENSION(:), POINTER :: z3d

CHARACTER(128) string

TYPE ( messageT ) caution

INTERFACE
  INTEGER FUNCTION SWIMupdateSmoothField( fldi,iflag )
    USE SWIMmetField_fd
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fldi
    INTEGER,                  INTENT( IN    ) :: iflag
  END FUNCTION SWIMupdateSmoothField
  INTEGER FUNCTION SWIMMcWIF( grid,meanField )
    USE SWIMmetField_fd
    TYPE( MetGrid   ), TARGET, INTENT( INOUT ) :: grid
    TYPE( MetMean3D ),         INTENT( INOUT ) :: meanField
  END FUNCTION SWIMMcWIF
END INTERFACE

INTEGER, EXTERNAL :: SWIMinterpTime
INTEGER, EXTERNAL :: SWIMupdateObsField, SWIMupdateGriddedMet
INTEGER, EXTERNAL :: SWIMupdatePolarField, SWIMshearGrad
INTEGER, EXTERNAL :: SWIMinterpNest, CheckMet
INTEGER, EXTERNAL :: SWIMinterpTimeAssm
INTEGER, EXTERNAL :: SWIMaddLogMessage, getFieldIndex
INTEGER, EXTERNAL :: PostCautionMessage

SWIMnextField = SWIMfailure

!------ Move fields at next time to current time (unless initializing)

IF( .NOT.BTEST(fld%status,FSB_DOINIT) )THEN
  IF( fld%tNext < fld%t )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMnextField'
    error%Message = 'Time on meteorological input is non-increasing'
    GOTO 9999
  END IF
  fld%t   = fld%tNext
  irv = SWIMinterpTime( 0.,fld )
  fld%status = IBCLR(fld%status,FSB_FIRST)
END IF

!------ Get next field

IF( BTEST(fld%type,FTB_NEST) )THEN        !Interpolated onto nested domain

  irv = SWIMinterpNest( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_OBS) )THEN    !Field based on observations

  irv = SWIMupdateObsField( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_SMOOTH) )THEN  !Averaged onto coarse grid

  iflag = 2
  irv = SWIMupdateSmoothField( fld,iflag )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_NPOLE) .OR. &  !Averaged from North/South boundaries
         BTEST(fld%type,FTB_SPOLE) )THEN   !of global fields

  irv = SWIMupdatePolarField( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE                                       !Gridded met

  irv = SWIMupdateGriddedMet( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Obs assimilation

IF( fld%nObsSource > 0 .AND. .NOT.BTEST(fld%type,FTB_OBS) )THEN

  IF( BTEST(fld%status,FSB_DOINIT) )THEN
    irv = SWIMinterpTimeAssm( 0.,fld )
    fld%t1 = fld%t2
  END IF

  irv = SWIMupdateObsField( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------- Mass-consistent adjustment if requested

IF( BTEST(fld%status,FSB_UPDATE) )THEN

  IF( BTEST(fld%type,FTB_MCWIF) )THEN

    irv = SWIMMcWIF( fld%grid,fld%NextField )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

END IF

!------ Check met for unrealistic temperature & velocity

irv = CheckMet( fld%NextField,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Compute measure of velocity gradient inhomogeneity

IF( BTEST(fld%type,FTB_DU2) )THEN
  IF( BTEST(fld%status,FSB_UPDATE) )THEN
    irv = SWIMshearGrad( fld%NextField,fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

IF( BTEST(fld%status,FSB_DOINIT) .AND. BTEST(fld%grid%type,GTB_Z3D) )THEN
  string = CHAR(13)  !Carriage return
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
  WRITE(string,'("Meteorology grid ",I2)') getFieldIndex( fld%index )
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
  irv = SWIMaddLogMessage( 'z:   3d height (AGL) field at domain center' )
  is = (fld%grid%nY/2-1)*fld%grid%nX + fld%grid%nX/2
  IF( BTEST(fld%grid%type,GTB_SIGMA) .OR. BTEST(fld%grid%type,GTB_Z3DW) )THEN
    z3d => fld%grid%sigma%Z
  ELSE
    z3d => fld%NextField%Z
  END IF
  IF( z3d(1) < 0. )THEN
    n1 = 2
  ELSE
    n1 = 1
  END IF
  n2 = MIN0(fld%grid%nZ,n1+5)
  DO WHILE( n1 <= fld%grid%nZ )
    WRITE(string,"('   ',6ES12.4)",IOSTAT=irv) (z3d(is+(k-1)*fld%grid%nXY),k=n1,n2)
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
    n1 = n1+6; n2 = MIN0(fld%grid%nZ,n1+5)
  END DO
END IF

!------ Move 'next' field into 'current' field for initialization

IF( BTEST(fld%status,FSB_DOINIT) )THEN

  IF( fld%tNext > t + 900. )THEN
    caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMnextField'
    caution%aString = 'First meteorology time is after calculation start'
    lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
    CALL TimeConvert( fld%tNext,Prj%local,lymd,hour,min,sec,year,month,day,string )
    caution%bString = 'Met   time: '//string
    CALL TimeConvert( t,Prj%local,lymd,hour,min,sec,year,month,day,string )
    caution%cString = 'Start time: '//string
    irv = PostCautionMessage( caution )
  END IF

  irv = SWIMinterpTime( 0.,fld )

  fld%t      = fld%tNext
  fld%status = IBCLR(fld%status,FSB_DOINIT)

  IF( fld%tNext > t )fld%status = IBSET(fld%status,FSB_FIRST)
  fld%status = IBSET(fld%status,FSB_SECOND)

ELSE

  IF( BTEST(fld%status,FSB_SECOND) )THEN
    IF( fld%tNext < t - 900. .AND. .NOT.BTEST(fld%status,FSB_UPDATE) )THEN
      caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMnextField'
      caution%aString = 'Last meteorology time is before calculation start'
      lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
      CALL TimeConvert(  fld%tNext,Prj%local,lymd,hour,min,sec,year,month,day,string )
      caution%bString = 'Met   time: '//string
      CALL TimeConvert( t,Prj%local,lymd,hour,min,sec,year,month,day,string )
      caution%cString = 'Start time: '//string
      irv = PostCautionMessage( caution )
    END IF
    fld%status = IBCLR(fld%status,FSB_SECOND)
  END IF

END IF

SWIMnextField = SWIMresult

9999 CONTINUE

RETURN
END
