INTEGER FUNCTION InitLSV( fld )

!------ Initialize large-scale variablility

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, n, i

INTEGER, EXTERNAL :: SWIMallocVariance

InitLSV = SWIMfailure

!------ Reset operational LSV

IF( Prj%LSVtype == LVP_OPER )Prj%LSVtype = LVP_MODEL

!------ Allocate and set values based on LSV type

fld%LSVtype = Prj%LSVtype

SELECT CASE( fld%LSVtype )

  CASE(  LVP_INPUT )   !Single input value

    irv = SWIMallocVariance( 1,fld%LSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    fld%LSV%UU(1) = Prj%LSVuu
    fld%LSV%VV(1) = Prj%LSVuu
    fld%LSV%UV(1) = 0.
    fld%LSV%SL(1) = Prj%LSVscale

  CASE(  LVP_MODEL )   !Gifford model function of latitude

    IF( .NOT.ASSOCIATED(fld%grid%lat) )THEN
      error%Number  = IV_ERROR
      error%Routine = 'InitLSV'
      error%Message = 'Lat/lon project or reference required for Model LSV'
      GOTO 9999
    END IF

    irv = SWIMallocVariance( fld%grid%nXY,fld%LSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    IF( BTEST(fld%type,FTB_SMOOTH) )THEN
      CALL SetSmoothModelLSV( fld )
    ELSE
      CALL SetModelLSV( fld )
    END IF

  CASE(  LVP_MET )   !Given on met input file

    IF( .NOT.BTEST(fld%type,FTB_LSV) )THEN
      error%Number  = IV_ERROR
      error%Routine = 'InitLSV'
      error%Message = 'Large-scale variability not given on met input file'
      GOTO 9999
    END IF

    n = fld%grid%nXY*fld%grid%nZ
    IF( BTEST(fld%grid%type,GTB_STAGGER) )n = n + fld%grid%nXY

    irv = SWIMallocVariance( n,fld%LSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    irv = SWIMallocVariance( n,fld%NextLSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    IF( BTEST(fld%type,FTB_OBS) )THEN
      DO i = 1,n
        fld%LSV%UU(i) = 0.; fld%NextLSV%UU(i) = 0.
        fld%LSV%UV(i) = 0.; fld%NextLSV%UV(i) = 0.
        fld%LSV%VV(i) = 0.; fld%NextLSV%VV(i) = 0.
        fld%LSV%SL(i) = 0.; fld%NextLSV%SL(i) = 0.
      END DO
    END IF

    IF( .NOT.BTEST(fld%type,FTB_LSVL) )THEN           !Only a single length scale used
      DEALLOCATE( fld%LSV%SL,fld%NextLSV%SL,STAT=irv )
      ALLOCATE( fld%LSV%SL(1),fld%NextLSV%SL(1),STAT=irv )
      IF( irv /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'InitLSV'
        error%Message = 'Error allocating Large-scale variability scale'
        GOTO 9999
      END IF

      fld%LSV%SL(1)     = Prj%LSVscale
      fld%NextLSV%SL(1) = Prj%LSVscale
    END IF

    IF( fld%nObsSource > 0 .AND. .NOT.BTEST(fld%type,FTB_OBS) )THEN

      irv = SWIMallocVariance( n,fld%LSV1 )
      IF( irv /= SWIMsuccess )GOTO 9999

      irv = SWIMallocVariance( n,fld%LSV2 )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( .NOT.BTEST(fld%type,FTB_LSVL) )DEALLOCATE( fld%LSV1%SL,fld%LSV2%SL,STAT=irv )

    END IF
END SELECT

InitLSV = SWIMresult

9999 CONTINUE
RETURN
END

!===============================================================================

SUBROUTINE SetModelLSV( fld )

!------ Gifford model (function of latitude)

USE SWIMparam_fd
USE SWIM_fi
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

REAL, PARAMETER :: LSVmodelScaleFac = 1.4

INTEGER i, j, i0, is

REAL slb0, uubj, slbj
REAL x, y, xmap, ymap

!------ If horizontal grid is 2d, scale is based on grid size

IF( fld%grid%nXY > 1 )THEN

  x = 0.5*(fld%grid%Xmin+fld%grid%Xmax)
  y = 0.5*(fld%grid%Ymin+fld%grid%Ymax)
  CALL SWIMmapfac( fld%grid%coord,x,y,xmap,ymap )
  slb0 = LSVmodelScaleFac*SQRT((fld%grid%dX/xmap)**2 + (fld%grid%dY/ymap)**2)

ELSE

!------ Otherwise, default to full spectrum

  slb0 = DEF_VAL_R

END IF

!------ Loop over latitude (assumed to already be set)

DO j = 1,fld%grid%nY
  i0 = (j-1)*fld%grid%nX

  DO i = 1,fld%grid%nX
    is = i0 + i

    CALL UUgifford( fld%grid%lat(is),uubj,slbj )
    IF( slbj > slb0 .AND. slb0 /= DEF_VAL_R )THEN
      uubj = uubj*(slb0/slbj)**TwoThirds
      slbj = slb0
    END IF

    fld%LSV%UU(is) = uubj
    fld%LSV%VV(is) = uubj
    fld%LSV%UV(is) = 0.
    fld%LSV%SL(is) = slbj

  END DO
END DO

RETURN
END

!===============================================================================

SUBROUTINE UUgifford( lat,uu,sl )

USE constants_fd
USE SWIM_fi

IMPLICIT NONE

REAL, INTENT( IN )  :: lat
REAL, INTENT( OUT ) :: uu, sl

REAL xlat, ylat

!------ Limit latitude between 15 and 75 degrees

xlat = MIN(75.,ABS(lat))*PI180
ylat = MAX(15.,ABS(lat))*PI180

!------ Set (horizontal) variance and scale

uu = Prj%epstrop/(FCOR0*COS(xlat))
sl = SQRT(2.*uu)/(FCOR0*SIN(ylat))

RETURN
END

!===============================================================================

SUBROUTINE SetSmoothModelLSV( fld )

!------ Gifford model (function of latitude)

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, j, i0, alloc_stat

REAL, DIMENSION(:), POINTER :: UU, SL, UUsmth, SLsmth

TYPE( MetField ), POINTER :: fldp

INTERFACE

  SUBROUTINE Smooth1dy( fldp,grid,fld )
    USE SWIM_fi
    REAL, DIMENSION(:),  POINTER      :: fldp, fld
    TYPE( MetGrid ),     INTENT( IN ) :: grid
  END SUBROUTINE Smooth1dy

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

END INTERFACE

!------ Point to parent

fldp => field(fld%gridSource%unit)

!------ Smooth UU and scale

IF( fldp%grid%coord%type == I_LATLON )THEN

  UU => fldp%LSV%UU(1:fldp%grid%nXY-fldp%grid%nX+1:fldp%grid%nX)
  SL => fldp%LSV%SL(1:fldp%grid%nXY-fldp%grid%nX+1:fldp%grid%nX)
  NULLIFY( UUsmth,SLsmth )
  ALLOCATE( UUsmth(fld%grid%nY),SLsmth(fld%grid%nY),STAT=alloc_stat )
  IF( alloc_stat /= 0 )RETURN
  CALL Smooth1dy( UU,fld%grid,UUsmth )
  CALL Smooth1dy( SL,fld%grid,SLsmth )
  DO j = 1,fld%grid%nY
    i0 = (j-1)*fld%grid%nX
    DO i = 1,fld%grid%nX
      fld%LSV%UU(i0+i) = UUsmth(j)
      fld%LSV%SL(i0+i) = SLsmth(j)
    END DO
  END DO
  DEALLOCATE( UUsmth,SLsmth,STAT=alloc_stat )

ELSE

  CALL Smooth2d( fldp%LSV%UU,fld%grid,fld%LSV%UU )
  CALL Smooth2d( fldp%LSV%SL,fld%grid,fld%LSV%SL )

END IF

!------ Assumme VV = UU

DO j = 1,fld%grid%nXY
  fld%LSV%VV(j) = fld%LSV%UU(j)
  fld%LSV%UV(j) = 0.
END DO

RETURN
END

!==============================================================================

SUBROUTINE Smooth1dy( fldp,grid,fld )

!------ Perform smoothing 1d (y-varying) fields

USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

REAL, DIMENSION(:),  POINTER      :: fldp, fld
TYPE( MetGrid ),     INTENT( IN ) :: grid

Type( SmoothPt ), DIMENSION(:), POINTER :: ySmooth

INTEGER nys
INTEGER j, jj, jp
REAL    fmin, fmax

REAL, EXTERNAL :: SWIMrlimit

nys = grid%Smooth%yNsmth; ySmooth => grid%Smooth%ySmooth

DO j = 1,grid%nY
  fmin =  HUGE(fmin)
  fmax = -fmin
  fld(j) = 0.
  DO jj = 1,nys
    jp  = ySmooth(j)%ip(jj)
    fld(j) = fld(j) + ySmooth(j)%wt(jj)*fldp(jp)
    fmin = MIN(fmin,fldp(jp))
    fmax = MAX(fmax,fldp(jp))
  END DO
  fld(j) = SWIMrlimit( fld(j),fmin,fmax )
END DO

RETURN
END
