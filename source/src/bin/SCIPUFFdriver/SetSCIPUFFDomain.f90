INTEGER FUNCTION SetSCIPUFFDomain()

!------ Default domain will be based on met input if possible, unless
!       1. Domain is set in CO pathway, or
!       2. User sets flag for AERMOD-style procedure whereby domain is based on
!          range of receptors and sources. N.B. xmin, etc. set in GenSCIPUFFSensor
!       N.B. Only gridded met (incl. terrain) can be used (but not observations alone)

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

REAL, PARAMETER :: LMIN = 3. !Minimum domain length (kilometers)

INTEGER irv, i
REAL    xc, yc, xlen, ylen
REAL    ytem, xmap, ymap
LOGICAL lValidMet

INTEGER, EXTERNAL :: GetDomRefFromWeather

SetSCIPUFFDomain = FAILURE

IF( .NOT.lDomLimits )THEN

  IF( .NOT.lDomRecptr )THEN

    lValidMet = .FALSE.
    DO i = 1,nMet
      SELECT CASE( metInp(i)%type )
        CASE( MET_SCIGRD,MET_SCITER,MET_SCILIS,MET_MEDLIS ) !N.B. No further check on list input
          lValidMet = .TRUE.; EXIT
      END SELECT
    END DO
    IF( .NOT.lValidMet )THEN
      WRITE(*,'(A)') 'Domain cannot be set from meteorology input'
      WRITE(*,'(A)') 'User must specify project domain'
      GOTO 9999
      lDomRecptr = .TRUE.
    END IF

   END IF

  IF( lDomRecptr )THEN

    IF( nsamp == 0 )THEN
      WRITE(*,'(A)') 'Abort run: no sensors defined for setting domain'
      GOTO 9999
    END IF

!------ Make sure release locations are within domain

    DO i = 1,new%scnHead%number
      xmin = MIN(xmin,relList(i)%xRel); xmax = MAX(xmax,relList(i)%xRel)
      ymin = MIN(ymin,relList(i)%yRel); ymax = MAX(ymax,relList(i)%yRel)
    END DO

!------ Increase domain width (determined by sensors) by 50%
!       w/ minimum of LMIN x LMIN

    xc = 0.5*(xmin+xmax); xlen = xmax-xmin
    yc = 0.5*(ymin+ymax); ylen = ymax-ymin

    IF( new%input%domain%domain%coord == I_LATLON  )THEN
       ytem = MIN( ABS(yc),POLARCAP_LAT )
       xmap = SPHFACR/COS(ytem*PI180) * 1.E3
       ymap = SPHFACR * 1.E3
    ELSE
      xmap = 1./xfac; ymap = 1./xfac
    END IF

    IF( xlen > LMIN*xmap/1.5 )THEN
      xlen = 1.5*xlen
    ELSE
      xlen = LMIN*xmap
    END IF

    IF( ylen > LMIN*ymap/1.5 )THEN
      ylen = 1.5*ylen
    ELSE
      ylen = LMIN*ymap
    END IF

!------ Maintain center location

    xmin = xc - 0.5*xlen; xmax = xc + 0.5*xlen
    ymin = yc - 0.5*ylen; ymax = yc + 0.5*ylen

    IF( .NOT.lDomRef )new%input%domain%domain%coord = I_CARTESIAN !I_METERS !******
    new%input%domain%domain%xMin  = xmin
    new%input%domain%domain%xMax  = xmax
    new%input%domain%domain%yMin  = ymin
    new%input%domain%domain%yMax  = ymax

  END IF

ELSE

  new%input%domain%domain%xMin  = new%input%domain%domain%xMin * xfac
  new%input%domain%domain%xMax  = new%input%domain%domain%xMax * xfac
  new%input%domain%domain%yMin  = new%input%domain%domain%yMin * xfac - yoff
  new%input%domain%domain%yMax  = new%input%domain%domain%yMax * xfac - yoff

  new%input%domain%domain%xMin  = MIN(xmin,new%input%domain%domain%xMin)
  new%input%domain%domain%xMax  = MAX(xmax,new%input%domain%domain%xMax)
  new%input%domain%domain%yMin  = MIN(ymin,new%input%domain%domain%yMin)
  new%input%domain%domain%yMax  = MAX(ymax,new%input%domain%domain%yMax)

  xc = 0.5*(new%input%domain%domain%xMin+new%input%domain%domain%xMax)
  yc = 0.5*(new%input%domain%domain%yMin+new%input%domain%domain%yMax)

END IF

IF( .NOT.lDomRef )THEN

  irv = GetDomRefFromWeather()
  IF( irv /= SUCCESS )GOTO 9999

  xc = 0.5*(new%input%domain%domain%xMin+new%input%domain%domain%xMax)
  yc = 0.5*(new%input%domain%domain%yMin+new%input%domain%domain%yMax)
  new%input%domain%reference%x = xc
  new%input%domain%reference%y = yc

END IF

SetSCIPUFFDomain = SUCCESS

9999 CONTINUE

RETURN
END

