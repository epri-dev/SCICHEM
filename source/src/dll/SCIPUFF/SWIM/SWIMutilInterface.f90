INTEGER FUNCTION SWIMnumFields(  numMetMax )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMnumFields

USE SWIM_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: numMetMax

IF( numField > 0 )THEN
  SWIMnumFields = numField
  numMetMax     = numFieldMax
ELSE
  SWIMnumFields = -1
  numMetMax     = 0
END IF

RETURN
END

!==============================================================================

REAL FUNCTION SWIMgetHmin() RESULT( Hmin )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetHmin

USE SWIM_fi
USE default_fd

IMPLICIT NONE

IF( numField > 0 )THEN
  Hmin = Prj%Hmin
ELSE
  Hmin = NOT_SET_R
END IF

RETURN
END

!==============================================================================

REAL FUNCTION SWIMgetHmax() RESULT( Hmax )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetHmax

USE SWIM_fi
USE default_fd

IMPLICIT NONE

INTEGER i

IF( numField > 0 )THEN
  Hmax = Prj%Hmin
  DO i = 1,numField
    Hmax = MAX(Hmax,field(i)%grid%Hmax+field(i)%grid%Hmin)
  END DO
  Hmax = Hmax - Prj%Hmin
ELSE
  Hmax = NOT_SET_R
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetGrid( ifld,MetFieldGrid )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetGrid

USE SWIMgridStr_fd
USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: ifld
TYPE( SWIMgridStr ), INTENT( INOUT ) :: MetFieldGrid

INTEGER ifldp
INTEGER irv, i
REAL    xmap, ymap
REAL    xmet, ymet, xc, yc, sum1, sum2

TYPE( MetGrid ), POINTER :: grid

INTEGER, EXTERNAL :: SWIMcnvCoord

SWIMgetGrid = SWIMfailure

IF( numField < 1 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetGrid'
  error%Message = 'Met fields not set'
  GOTO 9999
ELSE IF( ifld < 1 .OR. ifld > numFieldMax )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetGrid'
  error%Message = 'Invalid field number'
  GOTO 9999
END IF

IF( ifld > numField )THEN
  SWIMgetGrid = SWIMnull
  GOTO 9999
END IF

!------ Copy basic grid parameters

grid => field(ifld)%grid

MetFieldGrid%nx   = grid%nX  ; MetFieldGrid%ny   = grid%nY
MetFieldGrid%dx   = grid%dX  ; MetFieldGrid%dy   = grid%dY
MetFieldGrid%xmin = grid%Xmin; MetFieldGrid%ymin = grid%Ymin
IF( BTEST(field(ifld)%type,FTB_NPOLE) .OR. BTEST(field(ifld)%type,FTB_SPOLE) )THEN
  MetFieldGrid%dy = MetFieldGrid%dy / SPHFACR
  MetFieldGrid%dx = MetFieldGrid%dy
END IF

MetFieldGrid%lter = BTEST(grid%type,GTB_TERRAIN)

!------ Set map coordinate structure

MetFieldGrid%coord = grid%coord

!------ Set domain extent in project coordinates

IF( PrjCoord%type == MetFieldGrid%coord%type )THEN

  MetFieldGrid%xminPrj = MetFieldGrid%xmin
  MetFieldGrid%yminPrj = MetFieldGrid%ymin
  MetFieldGrid%xmaxPrj = MetFieldGrid%xmin + FLOAT(MetFieldGrid%nx-1)*MetFieldGrid%dx
  MetFieldGrid%ymaxPrj = MetFieldGrid%ymin + FLOAT(MetFieldGrid%ny-1)*MetFieldGrid%dy

ELSE

  sum1 = 0.
  sum2 = 0.
  DO i = 1,grid%nY
    ymet = grid%Ymin + FLOAT(i-1)*grid%dY
    irv = SWIMcnvCoord( grid%Xmin,ymet,MetFieldGrid%coord,xc,yc,PrjCoord )
    sum1 = sum1 + xc
    irv = SWIMcnvCoord( grid%Xmax,ymet,MetFieldGrid%coord,xc,yc,PrjCoord )
    sum2 = sum2 + xc
  END DO
  MetFieldGrid%xminPrj = sum1 / FLOAT(grid%nY)
  MetFieldGrid%xmaxPrj = sum2 / FLOAT(grid%nY)

  sum1 = 0.
  sum2 = 0.
  DO i = 1,grid%nX
    xmet = grid%Xmin + FLOAT(i-1)*grid%dX
    irv = SWIMcnvCoord( xmet,grid%Ymin,MetFieldGrid%coord,xc,yc,PrjCoord )
    sum1 = sum1 + yc
    irv = SWIMcnvCoord( xmet,grid%Ymax,MetFieldGrid%coord,xc,yc,PrjCoord )
    sum2 = sum2 + yc
  END DO
  MetFieldGrid%yminPrj = sum1 / FLOAT(grid%nX)
  MetFieldGrid%ymaxPrj = sum2 / FLOAT(grid%nX)

END IF

!------ Define grid resolution for puff splitting (based on base grid for smooth fields)

ifldp = ifld
DO WHILE( BTEST(field(ifldp)%type,FTB_SMOOTH) )
  ifldp = field(ifldp)%gridSource%unit
END DO

!------ Get map factor at project domain center

IF( PrjCoord%type /= field(1)%grid%coord%type )THEN
  xc = 0.5*(MAX(Prj%xmin,MetFieldGrid%xminPrj)+MIN(Prj%xmax,MetFieldGrid%xmaxPrj))
  yc = 0.5*(MAX(Prj%ymin,MetFieldGrid%yminPrj)+MIN(Prj%ymax,MetFieldGrid%ymaxPrj))
  irv = SWIMcnvCoord( xc,yc,PrjCoord,xmet,ymet,grid%coord )
ELSE
  xmet = 0.5*(Prj%xmax+Prj%xmin)
  ymet = 0.5*(Prj%ymax+Prj%ymin)
END IF

CALL SWIMmapfac( field(ifldp)%grid%coord,xmet,ymet,xmap,ymap )

MetFieldGrid%delx2 = MIN(field(ifldp)%grid%dX/xmap,field(ifldp)%grid%dY/ymap)**2

IF( BTEST(field(ifld)%type,FTB_SMOOTH) )THEN
  MetFieldGrid%basic = field(ifld)%gridSource%unit
ELSE
  MetFieldGrid%basic = 0
END IF

SWIMgetGrid = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetGridType( ifld,itype )

!------ Get field type, i.e., polar or not

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetGridType

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ifld
INTEGER, INTENT( OUT ) :: itype

SWIMgetGridType = SWIMfailure

IF( numField < 1 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetGridType'
  error%Message = 'Met fields not set'
  GOTO 9999
ELSE IF( ifld < 1 .OR. ifld > numField )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetGridType'
  error%Message = 'Invalid field number'
  GOTO 9999
END IF

IF( BTEST(field(ifld)%grid%type,GTB_NPOLE) )THEN
  itype = NORTHPOLE
ELSE IF( BTEST(field(ifld)%grid%type,GTB_SPOLE) )THEN
  itype = SOUTHPOLE
ELSE
  itype = NOTAPOLE
END IF

SWIMgetGridType = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetMetFieldID( x,y,z,shh,lAddMet ) RESULT( ifld )

!------ Get SWIM field index
!       N.B. Create smooth field if appropriate

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetMetFieldID

USE SWIMparam_fd
USE SWIM_fi
USE SWIMpuff_fd
USE SWIMcurrentMet_fi, ONLY: lSaveCoord, thisThread

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x, y    !Horizontal location (project coord)
REAL,    INTENT( IN  ) :: z       !Height AGL
REAL,    INTENT( IN  ) :: shh     !Horizontal puff size
LOGICAL, INTENT( OUT ) :: lAddMet

TYPE( PuffMetRequest ) :: Request

INTEGER, EXTERNAL :: SelectMetField, SWIMlimit

!------ Initialize failure

ifld    = -1
lAddMet = .FALSE.

!------ Setup request

Request%type = 0
Request%type = IBSET(Request%type,RTB_SMOOTH)
Request%type = IBSET(Request%type,RTB_AGL)

Request%iField = -1
Request%X      = x
Request%Y      = y
Request%Shh    = MAX(Shh,0.)
Request%z      = z
Request%SigZ   = 0.

lSaveCoord = .FALSE.

!------ Get field index

ifld = SelectMetField( Request )
IF( ifld < 1 )GOTO 9999

ifld = SWIMlimit( ifld,1,numField )

!------ Indicate that a met (smooth) field has been added
!       N.B. List of met fields should be updated in calling routine

lAddMet = BTEST(Request%type,RTB_ADDMET)

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetTerrain( ifld,MetFieldGrid )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetTerrain

USE SWIMgridStr_fd
USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: ifld
TYPE( SWIMgridStr ), INTENT( INOUT ) :: MetFieldGrid

INTEGER i

SWIMgetTerrain = SWIMfailure

IF( numField < 1 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetTerrain'
  error%Message = 'Met fields not set'
  GOTO 9999
ELSE IF( ifld < 1 .OR. ifld > numField )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetTerrain'
  error%Message = 'Invalid field number'
  GOTO 9999
END IF

MetFieldGrid%Hmin = field(ifld)%grid%Hmin

DO i = 1,field(ifld)%grid%nXY
  MetFieldGrid%H(i)  = field(ifld)%grid%terrain%H(i)
  MetFieldGrid%Hx(i) = field(ifld)%grid%terrain%Hx(i)
  MetFieldGrid%Hy(i) = field(ifld)%grid%terrain%Hy(i)
END DO

SWIMgetTerrain = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetMixingHt( ifld,MetFieldGrid )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetMixingHt

USE SWIMgridStr_fd
USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: ifld
TYPE( SWIMgridStr ), INTENT( INOUT ) :: MetFieldGrid

INTEGER i

SWIMgetMixingHt = SWIMfailure

IF( numField < 1 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetMixingHt'
  error%Message = 'Met fields not set'
  GOTO 9999
ELSE IF( ifld < 1 .OR. ifld > numField )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetMixingHt'
  error%Message = 'Invalid field number'
  GOTO 9999
ELSE IF( field(ifld)%grid%nXY /= MetFieldGrid%nx*MetFieldGrid%ny )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMgetMixingHt'
  error%Message = 'Met grid dimensions mismatch'
  GOTO 9999
END IF

DO i = 1,field(ifld)%grid%nXY
  MetFieldGrid%zi(i) = field(ifld)%BL%zi(i)
END DO

SWIMgetMixingHt = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMputMixingHt( ifld,MetFieldGrid )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMputMixingHt

USE SWIM_fi
USE SWIMparam_fd
USE SWIMgridStr_fd

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: ifld
TYPE( SWIMgridStr ), INTENT( INOUT ) :: MetFieldGrid

INTEGER irv, i, nxy

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

SWIMputMixingHt = SWIMfailure

!------ Check grid ID is positive

IF( ifld < 1 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMputMixingHt'
  error%Message = 'Invalid field number'
  GOTO 9999
END IF

!------ Ignore requests for non-existent fields

IF( ifld > numField )THEN
  SWIMputMixingHt = SWIMresult
  GOTO 9999
END IF

!------ Check for allocation

IF( .NOT.ASSOCIATED(field(ifld)%BL%zi) )THEN
  IF( BTEST(field(ifld)%type,FTB_NEST) )THEN  !Skip if nested field
    SWIMputMixingHt = SWIMresult
    GOTO 9999
  ELSE
    error%Number  = IV_ERROR
    error%Routine = 'SWIMputMixingHt'
    error%Message = 'Field not allocated'
    GOTO 9999
  END IF
END IF

!------ Check grid parameters - expand 1D met arrays if required

IF( MetFieldGrid%nx /= field(ifld)%grid%nX .OR. &
    MetFieldGrid%ny /= field(ifld)%grid%nY )THEN
  IF( field(ifld)%grid%nX == 1 .AND. field(ifld)%grid%nY == 1 )THEN
    nxy = MetFieldGrid%nx * MetFieldGrid%ny
    irv = SWIMexpandPtrArray( field(ifld)%BL%zi,nxy )
    IF( BTEST(field(ifld)%type,FTB_ZI) )THEN
      irv = SWIMexpandPtrArray( field(ifld)%nextBL%zi,nxy )
    END IF
    IF( irv /= SWIMsuccess )THEN
      error%Inform = 'Mixing Height'
      GOTO 9999
    END IF
  ELSE
    error%Number  = IV_ERROR
    error%Routine = 'SWIMputMixingHt'
    error%Message = 'Invalid field dimensions'
    GOTO 9999
  END IF
ELSE
  nxy = field(ifld)%grid%nXY
END IF

DO i = 1,nxy
  field(ifld)%BL%zi(i) = MetFieldGrid%zi(i)
END DO

SWIMputMixingHt = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SWIMgetUmax( umax,qmax,slmax )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetUmax

USE SWIM_fi
USE SWIMparam_fd

!------ Find maximum velocity for calculation of evaporation time step

IMPLICIT NONE

REAL, INTENT( OUT ) :: umax,     qmax,     slmax

INTEGER i
REAL    uumax, sum_sl, sl

TYPE( MetField ), POINTER :: fld

!------ Set grid based on field 1

fld => Field(1) !DSH - perhaps should be 'best'

!----- Mean velocity - squared

umax = -HUGE(0.)
DO i = 1,fld%grid%nXY
  umax = MAX( fld%BLaux%ubl(i)**2 + fld%BLaux%vbl(i)**2,umax )
END DO

!------ Boundary layer turbulence and energy-weighted scale

qmax  = -HUGE(0.)
DO i = 1,SIZE(fld%BLaux%uu_sh)
  IF( fld%BLaux%uu_sh(i) + fld%BLaux%uu_buoy(i) > qmax )THEN
    qmax = fld%BLaux%uu_sh(i) + fld%BLaux%uu_buoy(i)
    sl   = fld%BLaux%sbl(i)
  END IF
END DO

sum_sl = qmax*sl

!------- Include large-scale variance

SELECT CASE( fld%LSVtype )

  CASE( LVP_INPUT )

    qmax   = qmax   + 2.*fld%LSV%UU(1)
    sum_sl = sum_sl + 2.*fld%LSV%UU(1)*fld%LSV%SL(1)

  CASE( LVP_MODEL )

    uumax = -HUGE(0.)
    DO i = 1,SIZE(fld%LSV%UU)
      IF( fld%LSV%UU(i) > uumax )THEN
        uumax = fld%LSV%UU(i)
        sl    = fld%LSV%SL(i)
      END IF
    END DO

    qmax   = qmax   + 2.*uumax
    sum_sl = sum_sl + 2.*uumax*sl

  CASE( LVP_MET )

    uumax = -HUGE(0.)
    DO i = 1,SIZE(fld%LSV%UU)
      IF( fld%LSV%UU(i) + fld%LSV%VV(i) > uumax )THEN
        uumax = fld%LSV%UU(i) + fld%LSV%VV(i)
      END IF
    END DO
    sl = fld%LSV%SL(1)

    qmax   = qmax   + 2.*uumax
    sum_sl = sum_sl + 2.*uumax*sl

END SELECT


!------ Add variance to mean-squared

slmax = sum_sl / (qmax+1.E-10)
umax  = SQRT(0.5*(umax + qmax))
qmax  = SQRT(0.5*qmax)

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgenZgrid( x,y,zmax,maxz,zgrid,nz )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgenZgrid

USE SWIM_fi
USE SWIMparam_fd
USE SWIMcurrentMet_fi
USE SWIMpuff_fd
USE SWIMinterpPointer

IMPLICIT NONE

REAL,               INTENT( IN  ) :: x, y, zmax
INTEGER,            INTENT( IN  ) :: maxz
REAL, DIMENSION(*), INTENT( OUT ) :: zgrid
INTEGER,            INTENT( OUT ) :: nz

TYPE( PuffMetRequest ) :: Request

INTEGER  ifld, i, k
REAL     zi, z, d, dum

TYPE( MetField ),   POINTER :: fld
REAL, DIMENSION(:), POINTER :: Zsigma

INTEGER, EXTERNAL :: SetMetField
INTEGER, EXTERNAL :: SWIMcnvCoord

SWIMgenZgrid = SWIMfailure

!------ Build request

Request%X = x; Request%Y = y; Request%iField = 0

Request%SigZ = 0.
Request%Zcap = 0.
Request%Shh  = 0.

Request%type = IBSET(0,RTB_AGL)

!------ First base on 10m reference height

Request%Z = 10.

lSaveCoord = .TRUE.

ifld = SetMetField( Request )
IF( ifld < 1 )GOTO 9999

fld => field(ifld)

Request%X = xFld(ifld); Request%Y = yFld(ifld)

!------ Get BL depth & depth factor

CALL SetXYfac( Request,fld%grid,mx,my,mxu,myv )
CALL SetMXY( mx,my,mxy,fld%grid%nX,fld%grid%nXY,.FALSE. )

CALL IntXY( mxy,fld%BL%zi,zi )
CALL IntXY( mxy,fld%grid%terrain%D,d )

!------ Loop over grid levels

nz = 0

IF( fld%grid%nZ > 1 )THEN

  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN

    DO k = 1,fld%grid%nZ
      i  = (k-1)*fld%grid%nXY + 1
      nz = nz + 1
      Zsigma => fld%grid%sigma%Z(i:); CALL IntXY( mxy,Zsigma,zgrid(nz) )
      IF( nz == maxz )EXIT
    END DO

  ELSE

    DO i = 1,fld%grid%nZ
      nz        = nz + 1
      zgrid(nz) = fld%grid%Z(i)*d
      IF( nz == maxz )EXIT
    END DO

  END IF

!----- Check if grid spans BL

  IF( nz < maxz  .AND. zgrid(nz) < 0.95*zi )THEN
    nz = nz + 1
    zgrid(nz) = zi
  END IF

!------ Add levels from other met field if grid does not exceed zmax

  IF( nz < maxz .AND. zgrid(nz) < zmax .AND. numField > 1 )THEN

    Request%Z = zmax
    Request%X = x
    Request%Y = y

    ifld = SetMetField( Request )
    IF( ifld < 1 )GOTO 9999

    fld => field(ifld)

    Request%X = xFld(ifld); Request%Y = yFld(ifld)

    CALL SetXYfac( Request,fld%grid,mx,my,mxu,myv )
    CALL SetMXY( mx,my,mxy,fld%grid%nX,fld%grid%nXY,.FALSE. )

    CALL IntXY( mxy,fld%grid%terrain%D,d )

    IF( fld%grid%Ztop*d > zgrid(nz) )THEN

      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN

        DO k = 1,fld%grid%nZ
          i  = (k-1)*fld%grid%nXY + 1
          Zsigma => fld%grid%sigma%Z(i:); CALL IntXY( mxy,Zsigma,zgrid(nz+1) )
          IF( zgrid(nz+1) <= zgrid(nz) )CYCLE
          nz = nz + 1
          IF( nz == maxz )EXIT
        END DO

      ELSE

        DO i = 1,fld%grid%nZ
          z = fld%grid%Z(i)*d
          IF( z <= zgrid(nz) )CYCLE
          nz        = nz + 1
          zgrid(nz) = z
          IF( nz == maxz )EXIT

        END DO

      END IF

    END IF

  END IF

ELSE  !Cover BL if field does not have multiple levels

  nz = nz + 1
  zgrid(nz) = 10.
  dum = (zi-10.)/4.
  IF( dum > 0. )THEN
    DO i = 1,4
      nz = nz + 1
      zgrid(nz) = zgrid(nz-1) + dum
    END DO
  END IF

END IF

!------ Add levels upto zmax if necessary

DO WHILE( nz < maxz .AND. zgrid(nz) < zmax )
  nz = nz + 1
  zgrid(nz) = zgrid(nz-1) + 1000.
END DO

!------ Check if final grid extends to zmax
!       If not, fill in grid from zi to zmax

IF( zgrid(nz) < 0.95*zmax )THEN

  DO i = 1,nz
    IF( zgrid(i) >= zi )EXIT
  END DO

  IF( maxz-i > 10 )THEN

    dum = (zmax-zi)/FLOAT(maxz-i)
    DO k = i+1,maxz
      zgrid(k) = zgrid(k-1) + dum
    END DO

  ELSE

    dum = zmax/FLOAT(maxz)
    DO k = 1,maxz
      zgrid(k) = FLOAT(K)*dum
    END DO

  END IF

END IF

SWIMgenZgrid = SWIMresult

9999 CONTINUE

lSaveCoord = .FALSE.

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetRefLoc( xref,yref,lat0,lon0 )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetRefLoc

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, INTENT( OUT ) :: xref, yref, lat0, lon0

xref = Prj%Xref
yref = Prj%Yref
lat0 = Prj%Lat0
lon0 = Prj%Lon0

SWIMgetRefLoc = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMgetSpcDepMet( x,y,dxx,z,jday,SunZen,lat,lon,tsrf,ustr,ml,lucat )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetSpcDepMet

USE SWIM_fi
USE SWIMparam_fd
USE SWIMpuff_fd
USE SWIMcurrentMet_fi
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

REAL,               INTENT( IN  ) :: x, y     !Location (project coord)
REAL,               INTENT( IN  ) :: dxx      !Grid box size (m^2)
REAL,               INTENT( IN  ) :: z        !Height (agl) (m)

INTEGER,            INTENT( OUT ) :: jday     !Julian Day
REAL,               INTENT( OUT ) :: SunZen   !Solar zenith (0=overhead)
REAL,               INTENT( OUT ) :: lat      !Puff Latitude
REAL,               INTENT( OUT ) :: lon      !Puff Longitude
REAL,               INTENT( OUT ) :: tsrf     !Temperature (K)
REAL,               INTENT( OUT ) :: ustr     !ustar
REAL,               INTENT( OUT ) :: ml       !ML
INTEGER,            INTENT( OUT ) :: lucat    !Landuse category

INTEGER irv, ifld
REAL    t, tUTC, t_sun, elv

TYPE( PuffMetRequest ) :: Request
TYPE( PuffMet        ) :: Met
TYPE( MapCoord       ) :: coord

INTEGER, EXTERNAL :: SWIMcurrentMet, SWIMcnvCoord, SWIMcurrentSrfMet
INTEGER, EXTERNAL :: JulianPrj
REAL,    EXTERNAL :: fun_rhoa, UTCtime, SolarTime, LocalTime

SWIMgetSpcDepMet = SWIMfailure

!------ Setup request

Request%Zcap   = 0.
Request%SigZ   = 0.
Request%Shh    = dxx
Request%type   = 0
Request%iField = 0

Request%X = x; Request%Y = y
Request%z = z; Request%type = IBSET(Request%type,RTB_AGL)

irv = SWIMcurrentSrfMet( x,y,Met )
IF( irv /= SWIMsuccess )GOTO 9999

tsrf = Met%Mean%T

!------ Get basic parameters

irv = SWIMcurrentMet( Request,Met )
IF( irv /= SWIMsuccess )GOTO 9999

ustr = Met%BL%ustardep
ml   = Met%BL%L

!------ Get lat/lon and zenith angle

ifld = Met%iField    !Use field just returned

IF( PrjCoord%type /= field(ifld)%grid%coord%type )THEN
  irv = SWIMcnvCoord( x,y,PrjCoord,Request%X,Request%Y,field(ifld)%grid%coord )
  IF( irv /= SWIMsuccess )GOTO 9999
ELSE
  Request%X = x; Request%Y = y
END IF

!------ Get surface parameters for project landuse category; set those not set

CALL SetXYfac( Request,field(ifld)%grid,mx,my,mxu,myv )
CALL SetMXY( mx,my,mxy,field(ifld)%grid%nX,field(ifld)%grid%nXY,.FALSE. )

IF( BTEST( field(ifld)%grid%type,GTB_LANDUSE) )THEN
  luCat = field(ifld)%grid%landcover%LandUse(mxy%ij)
ELSE
  luCat = Prj%BL%i_cat
END IF

!------ Get solar angle and azimuth for specific lat/lon

t    = field(ifld)%t
jday = JulianPrj( t )
tUTC = UTCTime( t )

IF( PrjCoord%type == I_LATLON )THEN
  lat = y; lon = x
ELSE IF( field(ifld)%grid%coord%type == I_LATLON )THEN
  lat = Request%Y; lon = Request%X
ELSE
  CALL SWIMinitCoord( coord )
  coord%type = I_LATLON
  irv = SWIMcnvCoord( x,y,PrjCoord,lon,lat,coord )
END IF

IF( tUTC == NOT_SET_R )THEN
  t_sun = LocalTime( t )
ELSE
  t_sun = SolarTime( tUTC,lon )
END IF

CALL sun( lat,jday,t_sun,elv )

SunZen = 90. - elv

SWIMgetSpcDepMet = SWIMresult

9999 CONTINUE

RETURN
END

