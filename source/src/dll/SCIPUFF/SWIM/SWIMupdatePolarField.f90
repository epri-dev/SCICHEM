INTEGER FUNCTION SWIMupdatePolarField( fld )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMintrpGrid_fi

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

TYPE( MetField ), POINTER :: fldp

INTERFACE
  SUBROUTINE SWIMinterpPolar( type,inpGrid,inpFld,inpLSV, &
                                   outGrid,outFld,outLSV  )
    USE SWIM_fi
    INTEGER(8),          INTENT( IN  ) :: type
    TYPE( MetGrid     ), INTENT( IN  ) :: inpGrid
    TYPE( MetMean3D   ), INTENT( IN  ) :: inpFld
    TYPE( MetVariance ), INTENT( IN  ) :: inpLSV
    TYPE( MetGrid     ), INTENT( IN  ) :: outGrid
    TYPE( MetMean3D   ), INTENT( OUT ) :: outFld
    TYPE( MetVariance ), INTENT( OUT ) :: outLSV
  END SUBROUTINE SWIMinterpPolar
END INTERFACE

SWIMupdatePolarField = SWIMfailure

!------ Point to parent grid

fldp => field(fld%gridSource%unit)

!------ Check for initialization


IF( BTEST(fld%status,FSB_DOINIT) )THEN

  fld%tNext = fldp%t

  CALL SWIMinterpPolar( type    = fld%type,      &
                        inpGrid = fldp%grid,     &
                        inpFld  = fldp%Field,    &
                        inpLSV  = fldp%LSV,      &
                        outGrid = fld%grid,      &
                        outFld  = fld%NextField, &
                        outLSV  = fld%NextLSV )

!------ Get next time (if available)

ELSE IF( BTEST(fldp%status,FSB_UPDATE) )THEN

  fld%tNext = fldp%tNext

  CALL SWIMinterpPolar( type    = fld%type,       &
                        inpGrid = fldp%grid,      &
                        inpFld  = fldp%NextField, &
                        inpLSV  = fldp%NextLSV,   &
                        outGrid = fld%grid,       &
                        outFld  = fld%NextField,  &
                        outLSV  = fld%NextLSV )
  IF( error%Number /= NO_ERROR )GOTO 9999

ELSE

  fld%status = IBCLR(fld%status,FSB_UPDATE)

END IF

SWIMupdatePolarField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SWIMinterpPolar( type,inpGrid,inpFld,inpLSV, &
                                 outGrid,outFld,outLSV  )

!------ Setup polar field (average northern/southern edge of parent field)

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER(8),          INTENT( IN  ) :: type
TYPE( MetGrid     ), INTENT( IN  ) :: inpGrid
TYPE( MetMean3D   ), INTENT( IN  ) :: inpFld
TYPE( MetVariance ), INTENT( IN  ) :: inpLSV
TYPE( MetGrid     ), INTENT( IN  ) :: outGrid
TYPE( MetMean3D   ), INTENT( OUT ) :: outFld
TYPE( MetVariance ), INTENT( OUT ) :: outLSV

INTEGER i0, k0, nx, i, k
REAL    lat, lon, up, vp, fac, sumU, sumV

nx = inpGrid%nX

IF( BTEST(outGrid%type,GTB_NPOLE) )THEN
  i0  = (inpGrid%nY-1)*nx      !Average along northern edge of parent domain
  lat = inpGrid%Ymax
ELSE
  i0  = 0                      !Average along southern edge
  lat = inpGrid%Ymin
END IF

fac = 1./FLOAT(nx-1) !Assumes extra grid point for periodic boundary

DO k = 1,outGrid%nZ

  k0   = (k-1)*inpGrid%nXY + i0
  sumU = 0.
  sumV = 0.

  DO i = 2,nx
    lon = inpGrid%Xmin + FLOAT(i-1)*inpGrid%dX
    CALL SWIMLLtoPolarCartVel( lon,lat,inpFld%U(k0+i),inpFld%V(k0+i),up,vp )
    sumU = sumU + up
    sumV = sumV + vp
  END DO
  outFld%U(k) = sumU * fac
  outFld%V(k) = sumV * fac

  IF( BTEST(type,FTB_T) )outFld%Tpot(k)  = SUM(inpFld%Tpot(i+2:i+nx))  * fac
  IF( BTEST(type,FTB_P) )outFld%Press(k) = SUM(inpFld%Press(i+2:i+nx)) * fac
  IF( BTEST(type,FTB_H) )outFld%Humid(k) = SUM(inpFld%Humid(i+2:i+nx)) * fac
  IF( BTEST(type,FTB_QCLD) )outFld%Qcloud(k) = SUM(inpFld%Qcloud(i+2:i+nx)) * fac

  IF( BTEST(type,FTB_LSV) )THEN
    outLSV%UU(k) = 0.5*(SUM(inpLSV%UU(i+2:i+nx)) + SUM(inpLSV%VV(i+2:i+nx))) * fac
    outLSV%VV(k) = outLSV%UU(k)
    outLSV%UV(k) = 0.
    IF( BTEST(type,FTB_LSVL) )outLSV%SL(k) = SUM(inpLSV%SL(i+2:i+nx)) * fac
  END IF


END DO

RETURN
END

!==============================================================================

SUBROUTINE SWIMLLtoPolarCart( lon,lat,xp,yp )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMLLtoPolarCart

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lon, lat
REAL, INTENT( OUT ) :: xp, yp

REAL   rlat, rlon, rphi

rlat = lat*PI180
rlon = lon*PI180
rphi = Reqtr*COS(rlat)/SQRT(1.- EC2*SIN(rlat)**2)
xp   = rphi*COS(rlon)
yp   = rphi*SIN(rlon)

RETURN
END

!==============================================================================

SUBROUTINE SWIMPolarCarttoLL( xp,yp,lon,lat )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMPolarCarttoLL

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: xp, yp
REAL, INTENT( OUT ) :: lon, lat  !lat needs to be multiplied by -1 in
                                 !calling routine if in Southern Hemisphere
REAL   rlat, rlon, rphi

rlon = ATAN2(yp,xp)
lon  = rlon/PI180

IF( yp == 0. )THEN
  IF( xp == 0. )THEN
    rlat = PI/2.0
  ELSE
    rphi = (Reqtr*COS(rlon)/xp)**2
    rlat = ASIN(SQRT((1.- rphi)/(EC2-rphi)))
  END IF
ELSE
  rphi = (Reqtr*SIN(rlon)/yp)**2
  rlat = ASIN(SQRT((1.- rphi)/(EC2-rphi)))
END IF

lat = rlat/PI180

RETURN
END

!==============================================================================

SUBROUTINE SWIMLLtoPolarCartVel( lon,lat,uc,vc,up,vp )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMLLtoPolarCartVel

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lon, lat
REAL, INTENT( IN  ) :: uc, vc
REAL, INTENT( OUT ) :: up, vp

REAL   rlat, rlon

rlat = lat*PI180
rlon = lon*PI180

up = -uc*SIN(rlon) - vc*COS(rlon)*SIGN(1.,rlat)
vp =  uc*COS(rlon) - vc*SIN(rlon)*SIGN(1.,rlat)

RETURN
END
