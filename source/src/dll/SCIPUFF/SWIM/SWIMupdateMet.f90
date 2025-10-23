INTEGER FUNCTION SWIMupdateMet( t,SWIMstatus )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMupdateMet

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

REAL,    INTENT( IN  ) :: t
INTEGER, INTENT( OUT ) :: SWIMstatus

INTEGER i, irv
REAL    rate
INTEGER jul, season
REAL    zruf, hc, alphac, albedo ,bowen

INTEGER, EXTERNAL :: SWIMnextField, SWIMinterpTime
INTEGER, EXTERNAL :: SWIMupdateBL, SWIMsetSunFac
INTEGER, EXTERNAL :: SWIMaddSmoothPotential
INTEGER, EXTERNAL :: PostProgressMessage, PostCheckMessage
REAL,    EXTERNAL :: SetRate
INTEGER, EXTERNAL :: GetSeason, JulianPrj, SetLandUse
LOGICAL, EXTERNAL :: HasPrjReference

!------ Initialize

ResetButtons = .FALSE.
SWIMupdateMet = SWIMfailure
SWIMstatus    = 0

IF( Prj%create )THEN         !Should not be called with create=T
  SWIMupdateMet = SWIMresult
  GOTO 9999
END IF

!------ Post message

SWIMinProgress = .TRUE.

message%aString  = TRIM(Prj%prjName)//' : Updating Weather'
CALL SWIMtimeMessage( message%bString,t )
message%cString  = ''

irv = PostProgressMessage( message )

!------ Update surface parameters seasonally, if appropriate

IF( (Prj%BL%i_cat /= NOT_SET_I .AND. Prj%BL%i_cat /= 0) .AND. HasPrjReference() )THEN

  jul    = JulianPrj( t )
  season = GetSeason( Prj%Lat0,jul )

  irv = SetLandUse( Prj%BL%i_cat,season,Prj%BL%i_wet,zruf,hc,alphac,albedo,bowen )

  Prj%BL%zruf   = zruf
  Prj%BL%hc     = hc
  Prj%BL%alpha  = alphac
  Prj%BL%Bowen  = bowen
  Prj%BL%albedo = albedo

END IF
!------ Loop over met fields

MetFieldLoop : DO i = 1,numField

!------ Skip if field has already been updated
!       N.B. Allow second call to improve u*, L estimates in SWIMupdateBL

  IF( field(i)%t >= t .AND. .NOT.BTEST(field(i)%status,FSB_DOINITBL2) )CYCLE

!------ Get next time if necessary

  DO WHILE( t >= field(i)%tNext .AND. BTEST(field(i)%status,FSB_UPDATE) )

    irv = SWIMnextField( t,field(i) )
    IF( irv /= SWIMsuccess )GOTO 9999

    IF( Prj%time == Prj%timeEnd .AND. field(i)%tNext == Prj%time )THEN
      field(i)%status = IBCLR(field(i)%status,FSB_UPDATE)
    END IF

    SWIMstatus = IBSET(SWIMstatus,SSB_NEWMET)
    IF( BTEST(field(i)%status,FSB_EXPAND) )THEN
      SWIMstatus      = IBSET(SWIMstatus,SSB_EXPAND)
      field(i)%status = IBCLR(field(i)%status,FSB_EXPAND)
      irv = SWIMaddSmoothPotential( i )
      IF( irv /= SWIMsuccess )GOTO 9999
    END IF

    irv = PostCheckMessage()

    IF( BTEST(field(i)%status,FSB_FIRST) )EXIT

  END DO

!------ Temporal interpolation

  IF( BTEST(field(i)%status,FSB_UPDATE) .AND. .NOT.BTEST(field(i)%status,FSB_FIRST) )THEN
    rate = SetRate( t,field(i)%tNext,field(i)%t )
    irv  = SWIMinterpTime( rate,field(i) )
  END IF

!------ Update surface parameters, if appropriate
!       N.B. These are updated in SWIMinterpTime if defined on terrain file

  IF( (Prj%BL%i_cat /= NOT_SET_I .AND. Prj%BL%i_cat /= 0) .AND. HasPrjReference() )THEN
    IF( .NOT.BTEST(field(i)%grid%type,GTB_LANDUSE) )THEN
      IF( .NOT.BTEST(field(i)%grid%type,GTB_ZRUF)   )field(i)%grid%landcover%roughness = Prj%bl%zruf
      IF( .NOT.BTEST(field(i)%grid%type,GTB_HCNP)   )field(i)%grid%landcover%canopyHt  = Prj%bl%hc
      IF( .NOT.BTEST(field(i)%grid%type,GTB_ALPHA)  )field(i)%grid%landcover%alpha     = Prj%bl%alpha
      IF( .NOT.BTEST(field(i)%grid%type,GTB_BOWEN)  )field(i)%grid%landcover%Bowen     = Prj%bl%bowen
      IF( .NOT.BTEST(field(i)%grid%type,GTB_ALBEDO) )field(i)%grid%landcover%albedo    = Prj%bl%albedo
    END IF
  END IF

  irv = PostCheckMessage()

!------ Update boundary layer parameters and turbulence

  IF( field(i)%BLtype /= BLP_NONE )THEN
    irv = SWIMupdateBL( t,field(i) )
    IF( irv /= SWIMsuccess )GOTO 9999
    field(i)%tBL = t
  END IF

  irv = PostCheckMessage()

!------ Update solar decay factors

  IF( Prj%decay )THEN
    irv = SWIMsetSunFac( t,field(i)%grid )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Update current time

  field(i)%t = t

!------ Set SWIM update status

  IF( BTEST(field(i)%status,FSB_UPDATE) )SWIMstatus = IBSET(SWIMstatus,SSB_UPDATE)

END DO MetFieldLoop


!------ Set so MaxCappedBL will be updated if needed

MaxCappedBL = NOT_SET_R

irv = PostCheckMessage()

SWIMupdateMet = SWIMresult

9999 CONTINUE

IF( ResetButtons )CALL enableSWIMhalt( SWIM_DISABLE )
SWIMinProgress = .FALSE.

RETURN
END

!==============================================================================

REAL FUNCTION SetRate( t,tNext,tLast ) RESULT( rate )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t, tNext, tLast

REAL delt

REAL, EXTERNAL :: SWIMrlimit

delt = tNext - tLast

IF( delt > SMALL )THEN
  rate = (tNext - t)/delt
  rate = SWIMrlimit( rate,0.,1. )
ELSE
  rate = 0.
END IF

RETURN
END
