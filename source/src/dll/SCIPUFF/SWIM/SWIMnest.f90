!==============================================================================

LOGICAL FUNCTION SWIMaddNewNest( NestDomain,ifld ) RESULT( lNest )

!------ Check if nest is required based on domain and resolution
!       N.B. NestDomain must be in lat/lon or UTM coordinates

USE SWIMparam_fd
USE SWIM_fi
USE uniformGridT_fd

IMPLICIT NONE

TYPE( uniformGridT ), INTENT( IN  ) :: NestDomain
INTEGER,              INTENT( OUT ) :: ifld

INTEGER irv, i
REAL    xc, yc, xmap, ymap, xmin, xmax, ymin, ymax, xmaxNest, ymaxNest
REAL    dssNest, fac, facBest
REAL    area, areaNest
REAL    dum

TYPE( MapCoord ) :: NestCoord, fCoord

INTEGER, EXTERNAL :: SWIMcnvCoord

!------ Initialize

lNest = .TRUE.
ifld  = -1

!------ Set coordinate structure for nest

NestCoord%type = NestDomain%mode

SELECT CASE( NestDomain%mode )
  CASE( I_UTM )
    NestCoord%zone = NestDomain%UTMZone

  CASE( I_LATLON )
    NestCoord%zone = 0

  CASE DEFAULT
    GOTO 9999

END SELECT

!------ Map factor based on center of nest domain

xmaxNest = NestDomain%xmin+FLOAT(NestDomain%nx-1)*NestDomain%dx
ymaxNest = NestDomain%ymin+FLOAT(NestDomain%ny-1)*NestDomain%dy

xc = 0.5*(NestDomain%xmin+xmaxNest)
yc = 0.5*(NestDomain%ymin+ymaxNest)

CALL SWIMmapfac( NestCoord,xc,yc,xmap,ymap )

!------ Nest grid spacing in meters

dssNest = (NestDomain%dx/xmap)**2 + (NestDomain%dy/ymap)**2

!------ Area of nest domain

areaNest = (xmaxNest-NestDomain%xmin) * (ymaxNest-NestDomain%ymin)

!------ Loop over current met fields; check domain overlap and resolution

DO i = 1,NumField

  xc = 0.5*(field(i)%grid%Xmin+field(i)%grid%Xmax)
  yc = 0.5*(field(i)%grid%Ymin+field(i)%grid%Ymax)

  fCoord = field(i)%grid%coord

  IF( NestCoord%type == fCoord%type )THEN

    xmin = field(i)%grid%Xmin; xmax = field(i)%grid%Xmax
    ymin = field(i)%grid%Ymin; ymax = field(i)%grid%Ymax

  ELSE

    irv = SWIMcnvCoord( field(i)%grid%Xmin,yc,fCoord,xmin,dum,NestCoord )
    irv = SWIMcnvCoord( field(i)%grid%Xmax,yc,fCoord,xmax,dum,NestCoord )
    irv = SWIMcnvCoord( xc,field(i)%grid%Ymin,fCoord,dum,ymin,NestCoord )
    irv = SWIMcnvCoord( xc,field(i)%grid%Ymax,fCoord,dum,ymax,NestCoord )

  END IF

  xmin = MAX( NestDomain%xmin,xmin )
  xmax = MIN( xmaxNest,       xmax )
  ymin = MAX( NestDomain%ymin,ymin )
  ymax = MIN( ymaxNest,       ymax )

  area = MAX(xmax-xmin,0.) * MAX(ymax-ymin,0.)

  IF( area / areaNest > 0.8 )THEN

    CALL SWIMmapfac( fCoord,xc,yc,xmap,ymap )

    fac = (field(i)%grid%dX/xmap)**2 + (field(i)%grid%dY/ymap)**2
    fac = LOG(dssNest/fac)

    IF( fac >= -0.4  )THEN  !A field with suitable resolution already exists
      lNest = .FALSE.
      EXIT
    ELSE IF( ifld == -1 )THEN
      facBest = fac
      ifld    = i
    ELSE
      CALL BestGridRes( i,fac,ifld,facBest )
    END IF

  END IF

END DO

9999 CONTINUE

IF( ifld == -1 )ifld = 1

RETURN
END

