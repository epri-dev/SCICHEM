INTEGER FUNCTION SWIMupdateObsField( t,fld )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, INTENT( IN )                :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

TYPE( ObsMet ), POINTER :: CurrentObs, PrevObs

INTEGER irv, jObs
INTEGER k, i0, is
INTEGER i
LOGICAL lAssm
REAL    tObs
LOGICAL lNewObs
LOGICAL lFixed

REAL, DIMENSION(:), POINTER :: su, sv, au, av

INTERFACE
  SUBROUTINE PutObsInCell( First,CurrentObs )
    USE SWIM_fi
    TYPE( FirstObsGridList ), INTENT( INOUT ) :: First
    TYPE( ObsMet   ),         POINTER         :: CurrentObs
  END SUBROUTINE PutObsInCell
END INTERFACE

INTEGER, EXTERNAL :: SWIMbuildObsList, SWIMbuildFixedObs
INTEGER, EXTERNAL :: SWIMsetObsFieldGrid, SWIMexpandField, WrtLogWxsList
INTEGER, EXTERNAL :: SWIMinterpObs, SWIMsetGridList
LOGICAL, EXTERNAL :: SWIMcheckObsGrid , CheckInDomain

SWIMupdateObsField = SWIMfailure
IF( BTEST(fld%type,FTB_OBS) )fld%status = IBCLR(fld%status,FSB_UPDATE)

!------ Define 'current' overall obs time for updating obs lists

tObs = t
lAssm = .FALSE.
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( BTEST(ObsSrc(jObs)%type,OTB_PRF) .OR. BTEST(ObsSrc(jObs)%type,OTB_SRF) )THEN
    tObs = MIN(tObs,ObsSrc(jObs)%time)
  END IF
END DO

!------ Update obs lists

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)

  IF( BTEST(ObsSrc(jObs)%type,OTB_FIXED) )THEN
    irv = SWIMbuildFixedObs( fld%grid,ObsSrc(jObs) )
  ELSE
    lAssm = .NOT.BTEST(fld%type,FTB_OBS)
    irv = SWIMbuildObsList( tObs,fld%grid,ObsSrc(jObs),lAssm )
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

  IF( ObsSrc(jObs)%numObs > 0 )THEN
    IF( lAssm )THEN
      IF( BTEST(fld%status,FSB_FIRSTASSM) .AND. ObsSrc(jObs)%PrevNumObs > 0 )THEN
        fld%status = IBSET(fld%status,FSB_UPDATE)
        fld%status = IBCLR(fld%status,FSB_FIRSTASSM)
      END IF
    ELSE
      fld%status = IBSET(fld%status,FSB_UPDATE)
    END IF
  END IF

  IF( BTEST(fld%status,FSB_DOINIT) .AND. ObsSrc(jObs)%numObs > 0 )THEN
    irv = WrtLogWxsList( ObsSrc(jObs) )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

END DO

IF( .NOT.BTEST(fld%status,FSB_UPDATE) )THEN
  SWIMupdateObsField = SWIMresult
  GOTO 9999
END IF

!------ Count number of obs within met field domain
!       and compute local map factors

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  ObsSrc(jobs)%numObsDom = 0
  CurrentObs => ObsSrc(jobs)%obs
  DO WHILE( ASSOCIATED(CurrentObs) )
    CALL SWIMmapfac( fld%grid%coord,CurrentObs%x,CurrentObs%y,CurrentObs%xfac,CurrentObs%yfac )
    IF( CurrentObs%Vel%nz > 0 )THEN
      IF( CheckInDomain(CurrentObs%x,CurrentObs%y,fld%grid) )THEN
        PrevObs => CurrentObs%PrevObs
        lNewObs = .TRUE.
        DO WHILE( ASSOCIATED(PrevObs) )
          IF( TRIM(CurrentObs%id) == TRIM(PrevObs%id) )THEN
            lNewObs = .FALSE.
            EXIT
          END IF
          PrevObs => PrevObs%PrevObs
        END DO
        IF( lNewObs )ObsSrc(jobs)%numObsDom = ObsSrc(jobs)%numObsDom + 1
      END IF
    END IF
    CurrentObs => CurrentObs%NextObs
  END DO
END DO

!------ Check if grid needs to be expanded

IF( fld%grid%nXY == 1 )THEN
  IF( SWIMcheckObsGrid(fld) )THEN

    irv = SWIMsetObsFieldGrid( fld )
    IF( irv /= SWIMsuccess )GOTO 9999

    irv = SWIMexpandField( fld )
    IF( irv /= SWIMsuccess )GOTO 9999

    DO i = 1,fld%nObsSource
      jObs = fld%iObsSource(i)

      irv = SWIMsetGridList( ObsSrc(jObs),fld%grid )
      IF( irv /= SWIMsuccess )GOTO 9999

      CurrentObs => ObsSrc(jObs)%obs

      DO WHILE( ASSOCIATED(CurrentObs) )
        IF( ASSOCIATED(ObsSrc(jObs)%GridList) )THEN
          CALL PutObsInCell( ObsSrc(jObs)%GridList,CurrentObs )
        END IF
        CurrentObs => CurrentObs%nextObs
      END DO

    END DO

  END IF
END IF

!------ Combine obs

irv = SWIMinterpObs( fld )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set weights for mass-consistent adjustment

IF( MCWIFWtFac > 0. )THEN

  IF( BTEST(fld%type,FTB_MCWIF) )THEN
    lFixed = .FALSE.
    DO i = 1,fld%nObsSource
      jObs = fld%iObsSource(i)
      lFixed = lFixed .OR. BTEST(ObsSrc(jObs)%type,OTB_FIXED)
    END DO

    IF( .NOT.lFixed )THEN
      DO k = 2,fld%grid%nZ
        i0 = (k-1)*fld%grid%nXY
        su => fld%ObsWt%su(i0+1:); au => fld%grid%McWif%alphaU(i0+1:)
        sv => fld%ObsWt%sv(i0+1:); av => fld%grid%McWif%alphaV(i0+1:)
        DO is = 1,fld%grid%nXY
          au(is) = EXP(-2.*su(is)*MCWIFWtFac)
          av(is) = EXP(-2.*sv(is)*MCWIFWtFac)
        END DO
      END DO
      i0 = fld%grid%nZ*fld%grid%nXY
      DO is = 1,fld%grid%nXY
        fld%grid%McWif%alphaU(i0+is) = fld%grid%McWif%alphaU(i0+is-fld%grid%nXY)
        fld%grid%McWif%alphaV(i0+is) = fld%grid%McWif%alphaV(i0+is-fld%grid%nXY)
      END DO
      i0 = fld%grid%nXY
      DO is = 1,fld%grid%nXY
        fld%grid%McWif%alphaU(is) = fld%grid%McWif%alphaU(i0+is)
        fld%grid%McWif%alphaV(is) = fld%grid%McWif%alphaV(i0+is)
      END DO
    END IF

  END IF

END IF

SWIMupdateObsField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

LOGICAL FUNCTION SWIMcheckObsGrid( fld ) RESULT( l2d )

!------ Check if observations require 2d horizontal grid

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( IN ) :: fld

REAL, PARAMETER :: RES_FAC = 0.1

INTEGER iObs, jObs
INTEGER i, j
REAL    xc, yc, xfac, yfac, hresx, xi, yi, xj, yj

TYPE( ObsMet ), POINTER :: iCurrentObs, jCurrentObs

REAL, EXTERNAL :: SetHres

l2d = .FALSE.

!------ TRUE if any obs source is Forecast or Analysis

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( BTEST(ObsSrc(jObs)%type,OTB_FCST) .OR. BTEST(ObsSrc(jObs)%type,OTB_ANLY) )THEN
    l2d = .TRUE.; GOTO 9999
  END IF
END DO

!------ Check separation between stations
!       Get map factors at domain center

xc = 0.5*(fld%grid%Xmin+fld%grid%Xmax)
yc = 0.5*(fld%grid%Ymin+fld%grid%Ymax)

CALL SWIMmapfac( fld%grid%coord,xc,yc,xfac,yfac )

!------ Puff resolution

hresx = SetHres()

!------ TRUE if separation between any two stations exceeds fraction of resolution

hresx = RES_FAC * hresx / MAX(xfac,yfac)

jLoop : DO j = 1,fld%nObsSource
  jObs = fld%iObsSource(j)
  IF( ObsSrc(jObs)%numObs == 0 )CYCLE jLoop

  jCurrentObs => ObsSrc(jObs)%obs

  DO WHILE( ASSOCIATED(jCurrentObs) )
    xj = jCurrentObs%x;  yj = jCurrentObs%y

    iLoop : DO i = 1,fld%nObsSource
      iObs = fld%iObsSource(i)
      IF( ObsSrc(iObs)%numObs == 0 )CYCLE iLoop

      iCurrentObs => ObsSrc(iObs)%obs

      DO WHILE( ASSOCIATED(iCurrentObs) )
        xi = iCurrentObs%x;  yi = iCurrentObs%y

        IF( ((xi-xj)/xfac)**2 + ((yi-yj)/yfac)**2 > hresx**2 )THEN
          l2d = .TRUE.; EXIT jLoop
        END IF

        iCurrentObs => iCurrentObs%nextObs
      END DO
    END DO iLoop

    jCurrentObs => jCurrentObs%nextObs
  END DO
END DO jLoop

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetObsFieldGrid( fld )

!------ Build horizontal grid

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER(8) n1,n2
REAL       hresx

REAL,           EXTERNAL :: SetHres
CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMsetObsFieldGrid = SWIMfailure

!------ Set horizontal resolution from project

hresx = SetHres()

!------ Define grid size and resolution

fld%grid%nX = MIN(MAX(NINT((Prj%xmax-Prj%xmin)/hresx),1)+1,Prj%MAX1D_MET)
fld%grid%nY = MIN(MAX(NINT((Prj%ymax-Prj%ymin)/hresx),1)+1,Prj%MAX1D_MET)

fld%grid%nXY = fld%grid%nX*fld%grid%nY

fld%grid%dX = (Prj%xmax-Prj%xmin)/FLOAT(fld%grid%nX-1)
fld%grid%dY = (Prj%ymax-Prj%ymin)/FLOAT(fld%grid%nY-1)

fld%grid%Xmin = Prj%xmin; fld%grid%Xmax = Prj%xmax
fld%grid%Ymin = Prj%ymin; fld%grid%Ymax = Prj%ymax

n1 = fld%grid%nX
n2 = fld%grid%nY
IF( n1*n2 > HUGE(0) )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMsetObsFieldGrid'
  error%Message = 'Grid size too large'
  error%Inform  =  ArraySizeStr( 2,(/fld%grid%nX,fld%grid%nY /) )
  GOTO 9999
END IF

SWIMsetObsFieldGrid = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION SetHres() RESULT( hresx )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

IF( Prj%hres == DEF_VAL_R )THEN
  hresx = MIN(Prj%xmax-Prj%xmin,Prj%ymax-Prj%ymin)/10.
ELSE
  hresx = Prj%Hres
END IF

RETURN
END


