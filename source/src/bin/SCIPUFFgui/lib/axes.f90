!***********************************************************************
!                AxesTransform
!***********************************************************************
SUBROUTINE axes_transform( data,ref,plot,ldataL,lplotC,ld2p,lsc )

USE resource_fd
USE default_fd
USE param_fd
USE pcscipuf_fi
USE plotdlg_fi
USE contri_fi

!
!     This routine sets the plot axes values
!
IMPLICIT NONE

REAL, DIMENSION(6) :: data !Data Axes Set
REAL, DIMENSION(4) :: ref  !Reference Point
REAL, DIMENSION(6) :: plot !Plot Axes Set

LOGICAL   ldataL  !Data Lat/Lon Flag
LOGICAL   lplotC  !Plot Cartesian Flag
LOGICAL   ld2p    !Transform Flag
LOGICAL   lsc     !Scale Flag

INTEGER i, ilev
INTEGER(POINTER_LEN) iwnd
LOGICAL lok
LOGICAL no_defaultx, no_defaulty
REAL    xmin, ymin, lat, lon, pi180, sphfac

IF( (ldataL.AND. .NOT.lplotC) .OR. (.NOT.ldataL.AND. lplotC) )THEN

  IF( ld2p )THEN
    plot(1) = data(1)
    plot(2) = data(2)
    plot(4) = data(4)
    plot(5) = data(5)
    IF( lsc )THEN
      plot(3) = data(3)
      plot(6) = data(6)
    ELSE
      IF( plot(1) /= DEF_VAL_R )plot(1) = plot(1)*plot(3)/data(3)
      IF( plot(2) /= DEF_VAL_R )plot(2) = plot(2)*plot(3)/data(3)
      IF( plot(4) /= DEF_VAL_R )plot(4) = plot(4)*plot(6)/data(6)
      IF( plot(5) /= DEF_VAL_R )plot(5) = plot(5)*plot(6)/data(6)
    END IF
  ELSE
    data(1) = plot(1)
    data(2) = plot(2)
    data(4) = plot(4)
    data(5) = plot(5)
    IF( lsc )THEN
      data(3) = plot(3)
      data(6) = plot(6)
    ELSE
      IF( data(1) /= DEF_VAL_R )data(1) = data(1)*data(3)/plot(3)
      IF( data(2) /= DEF_VAL_R )data(2) = data(2)*data(3)/plot(3)
      IF( data(4) /= DEF_VAL_R )data(4) = data(4)*data(6)/plot(6)
      IF( data(5) /= DEF_VAL_R )data(5) = data(5)*data(6)/plot(6)
    END IF
  END IF

  RETURN

END IF

lok = .TRUE.
DO i = 1,4
  lok = lok .AND. ref(i) /= NOT_SET_R
END DO

IF( .NOT.lok )GOTO 9999

no_defaultx = .TRUE.
IF( ref(2) == DEF_VAL_R )THEN
  IF( ldataL )THEN
    lon = axesdef(DEFAULT_LEVEL)%Lon0
  ELSE
    GOTO 9999
  END IF
ELSE
  lon = ref(2)
END IF

no_defaulty = .TRUE.
IF( ref(1) == DEF_VAL_R )THEN
  IF( ldataL )THEN
    lat = axesdef(DEFAULT_LEVEL)%Lat0
  ELSE
    GOTO 9999
  END IF
ELSE
  lat = ref(1)
END IF

IF( ref(3) == DEF_VAL_R )THEN
  IF( ldataL )THEN
    GOTO 9999
  ELSE
    xmin = 0.
  END IF
ELSE
  xmin = ref(3)
END IF

IF( ref(4) == DEF_VAL_R )THEN
  IF( ldataL )THEN
    GOTO 9999
  ELSE
    ymin = 0.
  END IF
ELSE
  ymin = ref(4)
END IF

pi180 = 4.*ATAN(1.)/180.
sphfac = pi180*6371.2

IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN    !Data is Lat/Lon
  IF( no_defaultx )THEN
    xod  = lon
    xop  = xmin
    xsdp = sphfac*COS(lat*pi180)
  END IF
  IF( no_defaulty )THEN
    yod  = lat
    yop  = ymin
    ysdp = sphfac
  END IF
ELSE                                    !Data is Cartesian
  IF( no_defaultx )THEN
    xop  = lon
    xod  = xmin
    xsdp = 1./(sphfac*COS(lat*pi180))
  END IF
  IF( no_defaulty )THEN
    yop  = lat
    yod  = ymin
    ysdp = 1./sphfac
  END IF
  IF( project(BASE_LEVEL)%MapCoord == I_METERS )THEN
    xsdp = xsdp/1000.
    ysdp = ysdp/1000.
  END IF
END IF

IF( ld2p )THEN

  IF( data(1) == DEF_VAL_R .OR. .NOT.no_defaultx )THEN
    plot(1) = DEF_VAL_R
  ELSE
    plot(1) = xop + (data(1)/data(3)-xod)*xsdp
  END IF

  IF( data(2) == DEF_VAL_R .OR. .NOT.no_defaultx )THEN
    plot(2) = DEF_VAL_R
  ELSE
    plot(2) = xop + (data(2)/data(3)-xod)*xsdp
  END IF

  IF( data(4) == DEF_VAL_R .OR. .NOT.no_defaulty )THEN
    plot(4) = DEF_VAL_R
  ELSE
    plot(4) = yop + (data(4)/data(6)-yod)*ysdp
  END IF

  IF( data(5) == DEF_VAL_R .OR. .NOT.no_defaulty )THEN
    plot(5) = DEF_VAL_R
  ELSE
    plot(5) = yop + (data(5)/data(6)-yod)*ysdp
  END IF

  IF( lsc )THEN
    plot(3) = data(3)
    plot(6) = data(6)
  END IF

  IF( plot(1) /= DEF_VAL_R )plot(1) = plot(1)*plot(3)
  IF( plot(2) /= DEF_VAL_R )plot(2) = plot(2)*plot(3)
  IF( plot(4) /= DEF_VAL_R )plot(4) = plot(4)*plot(6)
  IF( plot(5) /= DEF_VAL_R )plot(5) = plot(5)*plot(6)

ELSE

  IF( plot(1) == DEF_VAL_R .OR. .NOT.no_defaultx )THEN
    data(1) = DEF_VAL_R
  ELSE
    data(1) = xod + (plot(1)/plot(3)-xop)/xsdp
  END IF

  IF( plot(2) == DEF_VAL_R .OR. .NOT.no_defaultx )THEN
    data(2) = DEF_VAL_R
  ELSE
    data(2) = xod + (plot(2)/plot(3)-xop)/xsdp
  END IF
!        data(3) = plot(3)
  IF( plot(4) == DEF_VAL_R .OR. .NOT.no_defaulty )THEN
    data(4) = DEF_VAL_R
  ELSE
    data(4) = yod + (plot(4)/plot(6)-yop)/ysdp
  END IF

  IF( plot(5) == DEF_VAL_R .OR. .NOT.no_defaulty )THEN
    data(5) = DEF_VAL_R
  ELSE
    data(5) = yod + (plot(5)/plot(6)-yop)/ysdp
  END IF

  IF( lsc )THEN
    data(3) = plot(3)
    data(6) = plot(6)
  END IF

  IF( data(1) /= DEF_VAL_R )data(1) = data(1)*data(3)
  IF( data(2) /= DEF_VAL_R )data(2) = data(2)*data(3)
  IF( data(4) /= DEF_VAL_R )data(4) = data(4)*data(6)
  IF( data(5) /= DEF_VAL_R )data(5) = data(5)*data(6)

END IF

RETURN

9999 CONTINUE

CALL FindHwndListId( IDB_AXES,iwnd,ilev )
IF( ilev > 0 )THEN
  IF( ld2p )THEN
    DO i = 1,6
      plot(i) = NOT_SET_R
    END DO
  ELSE
    DO i = 1,6
      data(i) = NOT_SET_R
    END DO
  END IF
ELSE
  IF( ld2p )THEN
    plot(1) = data(1)
    plot(2) = data(2)
    plot(4) = data(4)
    plot(5) = data(5)
    IF( lsc )THEN
      plot(3) = data(3)
      plot(6) = data(6)
    END IF
  ELSE
    data(1) = plot(1)
    data(2) = plot(2)
    data(4) = plot(4)
    data(5) = plot(5)
    IF( lsc )THEN
      data(3) = plot(3)
      data(6) = plot(6)
    END IF
  END IF
END IF

RETURN
END
!***********************************************************************
!                PointTransform
!***********************************************************************
SUBROUTINE point_transform( xi,yi,xo,yo,ld2p )

USE resource_fd
USE default_fd
USE param_fd
USE pcscipuf_fi
USE plotdlg_fi
USE contri_fi
!
!     This routine sets the plot axes values
!
IMPLICIT NONE

REAL      xi,yi !X,Y in
REAL      xo,yo !X,Y out
LOGICAL   ld2p  !Transform Flag

INTEGER i,zn
LOGICAL lok,ldataL,lplotC
REAL    xmin,ymin,lat,lon,pi180,sphfac

ldataL = project(BASE_LEVEL)%MapCoord == I_LATLON
lplotC = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON

IF( (ldataL .AND. .NOT.lplotC) .OR. (.NOT.ldataL .AND. lplotC) )THEN
  xo = xi
  yo = yi
  RETURN
END IF

lok = .TRUE.
DO i = 1,4
  lok = lok .AND. axesdef(BASE_LEVEL)%dbreal(i) /= NOT_SET_R
END DO

IF( .NOT.lok )GOTO 9999

IF( project(BASE_LEVEL)%MapCoord == I_UTM .OR. axesdef(BASE_LEVEL)%MapCoord == I_UTM )THEN
  CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                                   0,lon,lat,xmin,ymin,zn )
ELSE

  IF( axesdef(BASE_LEVEL)%Lon0 == DEF_VAL_R )THEN
    IF( ldataL )THEN
      lon = axesdef(DEFAULT_LEVEL)%Lon0
    ELSE
      GOTO 9999
    END IF
  ELSE
    lon = axesdef(BASE_LEVEL)%Lon0
  END IF

  IF( axesdef(BASE_LEVEL)%Lat0 == DEF_VAL_R )THEN
    IF( ldataL )THEN
      lat = axesdef(DEFAULT_LEVEL)%Lat0
    ELSE
      GOTO 9999
    END IF
  ELSE
    lat = axesdef(BASE_LEVEL)%Lat0
  END IF

  IF( axesdef(BASE_LEVEL)%X0 == DEF_VAL_R )THEN
    IF( ldataL )THEN
      GOTO 9999
    ELSE
      xmin = axesdef(DEFAULT_LEVEL)%X0
    END IF
  ELSE
    xmin = axesdef(BASE_LEVEL)%X0
  END IF

  IF( axesdef(BASE_LEVEL)%Y0 == DEF_VAL_R )THEN
    IF( ldataL )THEN
      GOTO 9999
    ELSE
      ymin = axesdef(DEFAULT_LEVEL)%Y0
    END IF
  ELSE
    ymin = axesdef(BASE_LEVEL)%Y0
  END IF
END IF

pi180 = 4.*ATAN(1.)/180.
sphfac = pi180*6371.2

IF( ldataL )THEN                     !Data is Lat/Lon
  xod  = lon
  xop  = xmin
  xsdp = sphfac*COS(lat*pi180)
  yod  = lat
  yop  = ymin
  ysdp = sphfac
ELSE                                 !Data is Cartesian
  xop  = lon
  xod  = xmin
  xsdp = 1./(sphfac*COS(lat*pi180))
  yop  = lat
  yod  = ymin
  ysdp = 1./sphfac
END IF

IF( project(BASE_LEVEL)%MapCoord == I_METERS )THEN !Data is Meters
  xsdp = xsdp/1000.
  ysdp = ysdp/1000.
END IF

IF( xi == DEF_VAL_R )THEN
  xo = DEF_VAL_R
ELSE IF( xi == NOT_SET_R )THEN
  xo = NOT_SET_R
ELSE
  IF( ld2p )THEN
    xo = xop + (xi-xod)*xsdp
  ELSE
    xo = xod + (xi-xop)/xsdp
  END IF
END IF

IF( yi == DEF_VAL_R )THEN
  yo = DEF_VAL_R
ELSE IF( yi == NOT_SET_R )THEN
  yo = NOT_SET_R
ELSE
  IF( ld2p )THEN
    yo = yop + (yi-yod)*ysdp
  ELSE
    yo = yod + (yi-yop)/ysdp
  END IF
END IF

RETURN

9999 CONTINUE

xo = xi
yo = yi

RETURN
END
