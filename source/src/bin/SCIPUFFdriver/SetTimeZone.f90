SUBROUTINE SetTimeZone()

USE SCIPUFFdriver_fi
USE datums

IMPLICIT NONE

INTEGER irv
REAL    lon0, lat0, xmid, ymid, tzone

IF( new%input%domain%domain%coord == I_CARTESIAN .AND. new%input%domain%reference%lon /= NOT_SET_R )THEN

  lon0  = new%input%domain%reference%lon
  tzone = FLOAT(INT(lon0+7.5)/15)
  IF( lon0 < -7.5 )tzone = tzone - 1.
  new%input%time%start%zone = tzone

ELSE IF( new%input%domain%domain%coord == I_UTM .AND. &
         new%input%domain%domain%xMin /= DEF_VAL_R .AND. &
         new%input%domain%domain%xMax /= DEF_VAL_R .AND. &
         new%input%domain%domain%yMin /= DEF_VAL_R .AND. &
         new%input%domain%domain%yMax /= DEF_VAL_R )THEN

  xmid  = 0.5*(new%input%domain%domain%xMin+new%input%domain%domain%xMax)
  ymid  = 0.5*(new%input%domain%domain%yMin+new%input%domain%domain%yMax)
  irv = UTM2LL( new%input%domain%domain%zoneUTM,xmid,ymid,lat0,lon0 )
  IF( irv /= 0 )RETURN

  tzone = FLOAT(INT(lon0+7.5)/15)
  IF( lon0 < -7.5 )tzone = tzone - 1.
  IF( tzone < 0 )tzone = tzone + 24.
  new%input%time%start%zone = tzone

ELSE IF( new%input%domain%domain%coord == I_LATLON .AND. &
         new%input%domain%domain%xMin /= DEF_VAL_R .AND. &
         new%input%domain%domain%xMax /= DEF_VAL_R )THEN

  lon0  = 0.5*(new%input%domain%domain%xMin+new%input%domain%domain%xMax)
  tzone = FLOAT(INT(lon0+7.5)/15)
  IF( lon0 < -7.5 )tzone = tzone - 1.
  IF( tzone < 0 )tzone = tzone + 24.
  new%input%time%start%zone = tzone

END IF

RETURN
END
