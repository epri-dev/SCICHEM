INTEGER FUNCTION OutputMEDOC( ifld )

!------ Output met field in MEDOC format

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifld

CHARACTER(8)   fflag
CHARACTER(8)   codename
CHARACTER(128) string
CHARACTER(128) string2

CHARACTER(PATH_MAXLENGTH) nestname, file, path

INTEGER alloc_stat, unit, ios, irv, i, k
INTEGER idum, ndum
INTEGER nvar3d, nvar2d, nv, k0
INTEGER nxy, nz
INTEGER year, month, day, hour, min, sec
REAL    dum, x0, y0, xlon0, xlat0, dx, dy, zbtop_med, fac
REAL    tem, pr, zp
LOGICAL lformat, lout_2d, lout_3d, lymd

TYPE( MapCoord ) :: coord

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: name3d, name2d, unit3d, unit2d

REAL, DIMENSION(:), ALLOCATABLE :: ztem
REAL, DIMENSION(:), ALLOCATABLE :: wrk

TYPE( MetField ), POINTER :: fld

REAL, DIMENSION(:), POINTER :: u, v, w, th, h, p, z
REAL, DIMENSION(:), POINTER :: qc
REAL, DIMENSION(:), POINTER :: hs, zruf, hc, alpha, bowen, albedo
REAL, DIMENSION(:), POINTER :: zi, hflx, ustr2, prcp
REAL, DIMENSION(:), POINTER :: LAI

INTEGER, DIMENSION(:), POINTER :: luc

INTEGER, EXTERNAL :: SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: OutputVarianceFields, OutputBLprofFields
REAL,    EXTERNAL :: fun_rhoa

!----- Setup pointer to field

fld => field(ifld)

!------ Initialize error messages

OutputMEDOC = SWIMfailure

error%Number  = WR_ERROR
error%Routine = 'OutputMEDOC'
error%Message = 'Error writing out met fields'
CALL ReportFileName( error%Action,'File=',fld%fileOut )

unit    = fld%unitOut
lformat = Prj%lFormat
lout_3d = Prj%lOut3D
lout_2d = Prj%lOut2D

codename = 'SWIM    '

!------ Initialize "dummy" variables

dum  = NOT_SET_MED_R
idum = NOT_SET_MED_I

!------ Point to next field (if it exists)

nestname = ' '
IF( ifld < numField )THEN
  DO i = ifld+1,numField
    IF( .NOT.(BTEST(field(i)%type,FTB_SMOOTH) .OR. &
              BTEST(field(i)%type,FTB_NPOLE)  .OR. &
              BTEST(field(i)%type,FTB_SPOLE)) )THEN
      nestname = TRIM(field(i)%fileOut)
      CALL SplitName( nestname,file,path )
      nestname = TRIM(file)  !Strip path from nest file name
      EXIT
    END IF
  END DO
END IF

nxy = fld%grid%nXY
nz  = fld%grid%nZ

u  => fld%Field%U
v  => fld%Field%V
w  => fld%Field%W
th => fld%Field%Tpot
h  => fld%Field%Humid
p  => fld%Field%Press
qc => fld%Field%Qcloud
z  => fld%Field%Z

hs     => fld%grid%terrain%H
zruf   => fld%grid%landcover%roughness
hc     => fld%grid%landcover%canopyHt
alpha  => fld%grid%landcover%alpha
albedo => fld%grid%landcover%albedo
bowen  => fld%grid%landcover%Bowen
luc    => fld%grid%landcover%LandUse

zi    => fld%BL%zi
hflx  => fld%BL%HeatFlux
IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                       BTEST(fld%type,FTB_NEST)) )THEN
  ustr2 => fld%BL%ustr
ELSE
  ustr2 => fld%BLaux%ustr2
END IF
IF( BTEST(fld%type,FTB_PRATE) )prcp => fld%BL%prcp
IF( BTEST(fld%type,FTB_LAI) )LAI => fld%BL%LAI

!------ First record - file format ('FFFFFFFF' for formatted, 'BBBBBBBB' for binary)

IF( lformat )THEN
  fflag = 'FFFFFFFF'
  WRITE(unit,'(A8)',IOSTAT=ios) fflag
ELSE
  fflag = 'BBBBBBBB'
  WRITE(unit,IOSTAT=ios) fflag
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Formatted/binary flag'
  GOTO 9999
END IF

!------ Second record - name of code, staggered grid indicator, and name of
!       associated output file, e.g., nested met

IF( lformat )THEN
  IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
    string = 'T       '
  ELSE
    string = 'F       '
  END IF
  WRITE(unit,'(A8,1X,A8,1X,A)',IOSTAT=ios) codename,string,TRIM(nestname)
ELSE
  WRITE(unit,IOSTAT=ios) codename,BTEST(fld%grid%type,GTB_STAGGER),nestname
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Codename record'
  GOTO 9999
END IF

!------ Get time

lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )

CALL TimeConvert( fld%t,Prj%localMet,lymd,hour,min,sec,year,month,day,string )

IF( year  == NOT_SET_I )year  = idum
IF( month == NOT_SET_I )month = idum

!------ Third record - current calculation time
!                      (day  month  year  hour  minute  second)

IF( lformat )THEN
  WRITE(unit,'(6(I12,1X))',IOSTAT=ios) day,month,year,hour,min,sec
ELSE
  WRITE(unit,IOSTAT=ios) day,month,year,hour,min,sec
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Codename record'
  GOTO 9999
END IF

!------ Fourth record - initial time of calculation
!                       (day  month  year   hour   minute second)
!       note:  not used

IF( lformat )THEN
  WRITE(unit,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  WRITE(unit,IOSTAT=ios) (idum,i=1,6)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Unused time record'
  GOTO 9999
END IF

!------ Fifth record  - number of grid points, key points, variables
!                    nx     - number of grid points from west to east
!                    ny     - number of grid points from south to north
!                    nz     - number of vertical grid points
!                    nreper - number of key points (cities, towns, etc.)
!                    nvar3d - number of 3d variables
!                    nvar2d - number of 2d variables
!     note:  nreper not used

ndum = 0

IF( lout_3d )THEN
  nvar3d = 2
  IF( BTEST(fld%type,FTB_W)   )nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_T)   )nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_H)   )nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_P)   )nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_QCLD))nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_Z)   )nvar3d = nvar3d + 1
  IF( BTEST(fld%type,FTB_UU)  )nvar3d = nvar3d + 6
  IF( BTEST(fld%type,FTB_LSV) )nvar3d = nvar3d + 3
ELSE
  nvar3d = 0
END IF

nvar2d = 0
IF( BTEST(fld%grid%type,GTB_TERRAIN) )nvar2d = nvar2d + 1 !terrain
                                      nvar2d = nvar2d + 1 !roughness
IF( BTEST(fld%grid%type,GTB_ALBEDO)  )nvar2d = nvar2d + 1 !albedo
IF( BTEST(fld%grid%type,GTB_BOWEN)   )nvar2d = nvar2d + 1 !bowen ratio
IF( BTEST(fld%grid%type,GTB_LANDUSE) )nvar2d = nvar2d + 1 !landuse category
                                      nvar2d = nvar2d + 2 !canopy height & attenuation coeff
IF( lout_2d .AND. fld%BLType /= BLP_NONE )THEN
  nvar2d = nvar2d + 3 !zi,hflux,ustar
  IF( BTEST(fld%type,FTB_PRATE) )nvar2d = nvar2d + 1 !precipitation rate
  IF( BTEST(fld%type,FTB_LAI) )nvar2d = nvar2d + 1   !leaf area index
END IF

IF( lformat )THEN
  WRITE(unit,'(6(I12,1X))',IOSTAT=ios) fld%grid%nX,fld%grid%nY,fld%grid%nZ,ndum,nvar3d,nvar2d
ELSE
  WRITE(unit,IOSTAT=ios) fld%grid%nX,fld%grid%nY,fld%grid%nZ,ndum,nvar3d,nvar2d
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Grid dimensions and no. of fields'
  GOTO 9999
END IF

!------ Sixth record - profile flags, indexing flags
!                    nevt   - number of time evolutions
!                    itmax  - number of times per evolution
!                    nevtpr - number of time profiles
!                    itmpro - number of times per profile
!                    iindex - vertical indexing flag
!                             = 1 ==> s(1)=0, s(kmax)=1
!                             = 2 ==> s(1)=1, s(kmax)=0
!                    iksurf - surface layer flag
!                             = 1 ==> surface layer in s=0
!                             = 2 ==> surface layer in s=1
!       note:  not used

IF( lformat )THEN
  WRITE(unit,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  WRITE(unit,IOSTAT=ios) (idum,i=1,6)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Dummy (sixth) record'
  GOTO 9999
END IF

!------ Seventh record - source information
!                    numsou - number of sources
!                    ndputi - number of particle sizes
!                    iunite - dimension flag
!                             = 1 ==> units of mass
!                             = 2 ==> units of activity
!       note:  not used

IF( lformat )THEN
  WRITE(unit,'(3(I12,1X))',IOSTAT=ios) (idum,i=1,3)
ELSE
  WRITE(unit,IOSTAT=ios) (idum,i=1,3)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Dummy (seventh) record'
  GOTO 9999
END IF

!------ Eighth record - grid and timing information  (kmax+11 values)
!                    zb     - vertical coordinates (m) (kmax values)
!                             sz = s * ztop
!                    dx     - grid spacing in (w-e) direction (m)
!                    dy     - grid spacing in (s-n) direction (m)
!                    x0     - x coord of sw corner of grid (km)
!                    y0     - y coord of sw corner of grid (km)
!                    xlat0  - latitude of sw corner of grid (deg)
!                    xlon0  - longitude of sw corner of grid (deg)
!                    tdeb0  - initial time for storage of time
!                               evolutions
!                    tdeb1  - initial time for storage of time profiles
!                    deltt0 - increment for storage of time evolutions
!                    deltt1 - increment for storage of time profiles
!                    zbtop  - top of domain (m)
!             note:  tdeb0, tdeb1, deltt0, deltt1 not used

SELECT CASE( fld%grid%coord%type )

  CASE( I_LATLON )
    x0     = dum
    y0     = dum
    xlon0  = fld%grid%Xmin
    xlat0  = fld%grid%Ymin
    dx     = fld%grid%dX
    dy     = fld%grid%dY

  CASE( I_LAMBERT,I_POLAR,I_MERCATOR )
    x0    = fld%grid%Xmin
    y0    = fld%grid%Ymin
    xlon0 = fld%gridSource%Lon0
    xlat0 = fld%gridSource%Lat0
    dx    = fld%grid%dX * 1.E3
    dy    = fld%grid%dY * 1.E3

  CASE( I_RPOLAR )
    x0    = fld%grid%Xmin
    y0    = fld%grid%Ymin
    xlon0 = fld%grid%coord%Lon0
    xlat0 = fld%grid%coord%Lat0
    dx    = fld%grid%dX * 1.E3
    dy    = fld%grid%dY * 1.E3

  CASE( I_ROTLL )
    x0    = fld%grid%Xmin
    y0    = fld%grid%Ymin
    xlon0 = fld%grid%coord%Lon0
    xlat0 = fld%grid%coord%Lat0
    dx    = fld%grid%dX
    dy    = fld%grid%dY

  CASE DEFAULT
    x0 = fld%grid%Xmin
    y0 = fld%grid%Ymin
    IF( .NOT.ASSOCIATED(fld%grid%lon) )THEN
      xlon0 = dum
      xlat0 = dum
    ELSE IF( fld%grid%nXY > 1 )THEN
      xlon0 = fld%grid%lon(1)
      xlat0 = fld%grid%lat(1)
    ELSE
      coord%type = I_LATLON
      irv = SWIMcnvCoord( x0,y0,fld%grid%coord,xlon0,xlat0,coord )
    END IF
    dx = fld%grid%dX * 1.E3
    dy = fld%grid%dY * 1.E3

END SELECT

!------ Vertical coordinate

ALLOCATE( ztem(fld%grid%nZ),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating vertical grid'
  GOTO 9999
END IF

!------ Sigma coordinates

IF( BTEST(fld%grid%type,GTB_SIGMA) )THEN

  DO i = 1,nz
    ztem(i) = 1. - fld%grid%Z(i)
  END DO

  zbtop_med = 0.  !Not set for sigma-p

!------ Or, re-define vertical coordinate in MEDOC convention

ELSE

  IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
    zbtop_med = fld%grid%Ztop + fld%grid%Hmin
    fac = zbtop_med/fld%grid%Ztop
    DO i = 1,nz
      ztem(i) = fld%grid%Z(i) * fac
    END DO
  ELSE
    DO i = 1,nz
      ztem(i) = fld%grid%Z(i)
    END DO
    zbtop_med = fld%grid%Ztop
  END IF

END IF

IF( lformat )THEN
  WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (ztem(k),k=1,nz),dx,dy,x0,y0, &
                                            xlat0,xlon0,(dum,i=1,4),zbtop_med
ELSE
  WRITE(unit,IOSTAT=ios) (ztem(k),k=1,nz),dx,dy,x0,y0, &
                         xlat0,xlon0,(dum,i=1,4),zbtop_med
END IF

DEALLOCATE( ztem,STAT=alloc_stat )

IF( ios /= 0 )THEN
  error%Inform = 'Vertical grid,dx,dy,origin,etc.'
  GOTO 9999
END IF

!------ Ninth record - names and units (nreper+2*nvar3d+2*nvar2d values)
!                    nomrep - key point names - nreper values
!                    name3d - names of 3d variables - nvar3d values
!                    unit3d - units of 3d variables - nvar3d values
!                    name2d - names of 2d variables - nvar2d values
!                    unit2d - units of 2d variables - nvar2d values
!       note:  nomrep not used

IF( lout_3d )THEN

  ALLOCATE( name3d(nvar3d),unit3d(nvar3d),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Message = 'Error allocating field names and units'
    GOTO 9999
  END IF

  name3d(1) = 'U'
  unit3d(1) = 'M/S'

  name3d(2) = 'V'
  unit3d(2) = 'M/S'

  nv = 2

  IF( BTEST(fld%type,FTB_W) )THEN
    nv = nv + 1
    name3d(nv) = 'W'
    unit3d(nv) = 'M/S'
  END IF

  IF( BTEST(fld%type,FTB_T) )THEN
    nv = nv + 1
    name3d(nv) = 'T'
    unit3d(nv) = 'K'
  END IF

  IF( BTEST(fld%type,FTB_H) )THEN
    nv = nv + 1
    name3d(nv) = 'H'
    unit3d(nv) = '%'
  END IF

  IF( BTEST(fld%type,FTB_P) )THEN
    nv = nv + 1
    name3d(nv) = 'P'
    unit3d(nv) = 'MB'
  END IF

  IF( BTEST(fld%type,FTB_QCLD) )THEN
    nv = nv + 1
    name3d(nv) = 'QCLD'
    unit3d(nv) = 'G/M3'
  END IF

  IF( BTEST(fld%type,FTB_Z) )THEN
    nv = nv + 1
    name3d(nv) = 'Z'
    unit3d(nv) = 'M'
  END IF

  IF( BTEST(fld%type,FTB_UU) )CALL AddBLprofNames( nv,name3d,unit3d )

  IF( BTEST(fld%type,FTB_LSV) )CALL AddVarianceNames( nv,name3d,unit3d,'L', &
                                                      BTEST(fld%type,FTB_LSVL) )


END IF

ALLOCATE( name2d(nvar2d),unit2d(nvar2d),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating 2d field names and units'
  GOTO 9999
END IF

nv = 0

IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
  nv = nv + 1
  name2d(nv) = 'REL'
  unit2d(nv) = 'M'
END IF

nv = nv + 1
IF( BTEST(fld%type,FTB_ZRUF) )THEN
  name2d(nv) = 'ZRUF(T)'
ELSE
  name2d(nv) = 'ZRUF'
END IF
unit2d(nv) = 'M'

IF( BTEST(fld%grid%type,GTB_ALBEDO) )THEN
  nv = nv + 1
  name2d(nv) = 'ALBD'
  unit2d(nv) = 'NONE'
END IF

IF( BTEST(fld%grid%type,GTB_BOWEN) )THEN
  nv = nv + 1
  name2d(nv) = 'BR'
  unit2d(nv) = 'NONE'
END IF

IF( BTEST(fld%grid%type,GTB_LANDUSE) )THEN
  nv = nv + 1
  name2d(nv) = 'LU'
  unit2d(nv) = 'NONE'
END IF

nv = nv + 1
name2d(nv) = 'HCNP'
unit2d(nv) = 'M'

nv = nv + 1
name2d(nv) = 'ALPH'
unit2d(nv) = 'NONE'

IF( lout_2d .AND. fld%BLType /= BLP_NONE )THEN

  name2d(nv+1) = 'ZI'
  unit2d(nv+1) = 'M'

  name2d(nv+2) = 'HFLX'
  unit2d(nv+2) = 'W/M2'

  IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                         BTEST(fld%type,FTB_NEST)) )THEN
    name2d(nv+3) = 'USTR'
    unit2d(nv+3) = 'M/S'
  ELSE
    name2d(nv+3) = 'USTRX'
    unit2d(nv+3) = 'M/S'
  END IF

  nv = nv+3

  IF( BTEST(fld%type,FTB_ACCPR) )THEN
    nv = nv + 1
    name2d(nv) = 'ACCPR'
    unit2d(nv) = 'MM'
  ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
    nv = nv + 1
    name2d(nv) = 'PRATE'
    unit2d(nv) = 'MM/HR'
  END IF

  IF( BTEST(fld%type,FTB_LAI) )THEN
    nv = nv + 1
    name2d(nv) = 'LAI'
    unit2d(nv) = 'M2/M2'
  END IF

END IF

IF( lformat )THEN
  WRITE(unit,'(6(A8,1X))',IOSTAT=ios)(name3d(i),i=1,nvar3d), &
                                     (unit3d(i),i=1,nvar3d), &
                                     (name2d(i),i=1,nvar2d), &
                                     (unit2d(i),i=1,nvar2d)
ELSE
  WRITE(unit,IOSTAT=ios) (name3d(i),i=1,nvar3d), &
                         (unit3d(i),i=1,nvar3d), &
                         (name2d(i),i=1,nvar2d), &
                         (unit2d(i),i=1,nvar2d)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Field names and units'
  GOTO 9999
END IF

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  k0 = nxy
ELSE
  k0 = 0
END IF

output3D : IF( lout_3d )THEN

!------ Eleventh record - 3d variables
!                       (nvar3d sets of nx*ny*nz values)

  IF( lformat )THEN
    WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (u(i+k0),i=1,nxy*nz)
  ELSE
    WRITE(unit,IOSTAT=ios) (u(i+k0),i=1,nxy*nz)
  END IF

  IF( ios /= 0 )THEN
    error%Inform = 'Field variable U'
    GOTO 9999
  END IF

  IF( lformat )THEN
    WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (v(i+k0),i=1,nxy*nz)
  ELSE
    WRITE(unit,IOSTAT=ios) (v(i+k0),i=1,nxy*nz)
  END IF

  IF( ios /= 0 )THEN
    error%Inform = 'Field variable V'
    GOTO 9999
  END IF

  IF( BTEST(fld%type,FTB_W) )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (w(i),i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (w(i),i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable W'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_T) )THEN

    fac = (1000./PSURF)**KAPPA

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (th(i+k0)*fac,i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (th(i+k0)*fac,i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable T'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_H) )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (h(i+k0),i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (h(i+k0),i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable H'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_P) )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (EXP(p(i+k0))*PSURF,i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (EXP(p(i+k0))*PSURF,i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable P'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_QCLD) )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (qc(i+k0),i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (qc(i+k0),i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable QCLD'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_Z) )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (z(i+k0),i=1,nxy*nz)
    ELSE
      WRITE(unit,IOSTAT=ios) (z(i+k0),i=1,nxy*nz)
    END IF

    IF( ios /= 0 )THEN
      error%Inform = 'Field variable Z'
      GOTO 9999
    END IF

  END IF

  IF( BTEST(fld%type,FTB_UU) )THEN
    ios  = OutputBLprofFields( lformat,unit,k0,nxy*nz,fld%BLprof )
    IF( ios /= SWIMsuccess )THEN
      error%Inform = TRIM(error%Inform)//' for BL turbulence profiles'
      GOTO 9999
    END IF
  END IF

  IF( BTEST(fld%type,FTB_LSV) )THEN
    ios  = OutputVarianceFields( lformat,unit,k0,nxy*nz,fld%LSV,BTEST(fld%type,FTB_LSVL) )
    IF( ios /= SWIMsuccess )THEN
      error%Inform = TRIM(error%Inform)//' for large-scale variance'
      GOTO 9999
    END IF
  END IF


END IF output3D

output2D : IF( nvar2d > 0 )THEN

  IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (hs(i)+fld%grid%Hmin,i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (hs(i)+fld%grid%Hmin,i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Terrain elevation field'
      GOTO 9999
    END IF
  END IF

  IF( lformat )THEN
    WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (zruf(i),i=1,nxy)
  ELSE
    WRITE(unit,IOSTAT=ios) (zruf(i),i=1,nxy)
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Surface roughness'
    GOTO 9999
  END IF

  IF( BTEST(fld%grid%type,GTB_ALBEDO) )THEN
    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (albedo(i),i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (albedo(i),i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Surface albedo'
      GOTO 9999
    END IF
  END IF

  IF( BTEST(fld%grid%type,GTB_BOWEN) )THEN
    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (bowen(i),i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (bowen(i),i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Bowen ratio'
      GOTO 9999
    END IF
  END IF

  IF( BTEST(fld%grid%type,GTB_LANDUSE) )THEN
    IF( lformat )THEN
      WRITE(unit,'(6(F12.0,1X))',IOSTAT=ios) (FLOAT(luc(i)),i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (FLOAT(luc(i)),i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Landuse category'
      GOTO 9999
    END IF
  END IF

  IF( lformat )THEN
    WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (hc(i),i=1,nxy)
  ELSE
    WRITE(unit,IOSTAT=ios) (hc(i),i=1,nxy)
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Canopy height'
    GOTO 9999
  END IF

  IF( lformat )THEN
    WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (alpha(i),i=1,nxy)
  ELSE
    WRITE(unit,IOSTAT=ios) (alpha(i),i=1,nxy)
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Canopy velocity attenuation parameter'
    GOTO 9999
  END IF

  outputBL : IF( lout_2d .AND. fld%BLType /= BLP_NONE )THEN

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (zi(i),i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (zi(i),i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Boundary layer depth'
      GOTO 9999
    END IF

    ALLOCATE( wrk(nxy),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Message = 'Error allocating heat flux array'
      GOTO 9999
    END IF

    IF( fld%BLType /= BLP_SBL )THEN
      DO i = 1,nxy
        IF( .NOT.(BTEST(fld%type,FTB_T) .AND. BTEST(fld%type,FTB_P)) )THEN
          zp = fld%grid%Hmin+hs(i)
          CALL stnd_atmos( zp,pr,tem,dum,SA_TA )
        END IF
        IF( BTEST(fld%type,FTB_P) )pr  = EXP(p(i+k0))
        IF( BTEST(fld%type,FTB_T) )tem = th(i+k0)*pr**KAPPA
        wrk(i) = hflx(i) * (fun_rhoa( tem,pr*PSURF )*CP)
      END DO
    ELSE
      DO i = 1,nxy
        wrk(i) = hflx(i) * RHOCP
      END DO
    END IF

    IF( lformat )THEN
      WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (wrk(i),i=1,nxy)
    ELSE
      WRITE(unit,IOSTAT=ios) (wrk(i),i=1,nxy)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Sensible heat flux'
      GOTO 9999
    END IF

    IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                           BTEST(fld%type,FTB_NEST)) )THEN
      IF( lformat )THEN
        WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (ustr2(i),i=1,nxy)
      ELSE
        WRITE(unit,IOSTAT=ios) (ustr2(i),i=1,nxy)
      END IF
    ELSE
      IF( lformat )THEN
        WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (SQRT(ustr2(i)),i=1,nxy)
      ELSE
        WRITE(unit,IOSTAT=ios) (SQRT(ustr2(i)),i=1,nxy)
      END IF
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'U* - shear stress velocity'
      GOTO 9999
    END IF

    IF( BTEST(fld%type,FTB_PRATE) )THEN
      IF( lformat )THEN
        WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (prcp(i),i=1,nxy)
      ELSE
        WRITE(unit,IOSTAT=ios) (prcp(i),i=1,nxy)
      END IF
    ENDIF
    IF( ios /= 0 )THEN
      error%Inform = 'Precipitation rate'
      GOTO 9999
    END IF

    IF( BTEST(fld%type,FTB_LAI) )THEN
      IF( lformat )THEN
        WRITE(unit,'(6(1ES12.5,1X))',IOSTAT=ios) (LAI(i),i=1,nxy)
      ELSE
        WRITE(unit,IOSTAT=ios) (LAI(i),i=1,nxy)
      END IF
    ENDIF
    IF( ios /= 0 )THEN
      error%Inform = 'Leaf area index'
      GOTO 9999
    END IF

  END IF outputBL

END IF output2D

!------ Write message to log file

CALL SWIMtimeMessage( string2,fld%t )

WRITE(string,'(A,I2,A)',IOSTAT=ios) 'Outputting met field ',ifld,' at '//TRIM(string2)
ios = SWIMaddLogMessage( string )
IF( ios /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

OutputMEDOC = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

IF( ALLOCATED(wrk)    )DEALLOCATE( wrk,STAT=alloc_stat )
IF( ALLOCATED(name3d) )DEALLOCATE( name3d,STAT=alloc_stat )
IF( ALLOCATED(unit3d) )DEALLOCATE( unit3d,STAT=alloc_stat )
IF( ALLOCATED(name2d) )DEALLOCATE( name2d,STAT=alloc_stat )
IF( ALLOCATED(unit2d) )DEALLOCATE( unit2d,STAT=alloc_stat )

RETURN

END

!==============================================================================

SUBROUTINE AddBLprofNames( nv,name3d,unit3d )

IMPLICIT NONE

INTEGER,                    INTENT( INOUT ) :: nv
CHARACTER(*), DIMENSION(*), INTENT( OUT   ) :: name3d, unit3d

nv = nv + 1
name3d(nv) = 'UUB'
unit3d(nv) = 'M2/S2'

nv = nv + 1
name3d(nv) = 'VVB'
unit3d(nv) = 'M2/S2'

nv = nv + 1
name3d(nv) = 'WWB'
unit3d(nv) = 'M2/S2'

nv = nv + 1
name3d(nv) = 'WTB'
unit3d(nv) = 'K-M/S'

nv = nv + 1
name3d(nv) = 'SLB'
unit3d(nv) = 'M'

nv = nv + 1
name3d(nv) = 'SZB'
unit3d(nv) = 'M'

RETURN
END

!==============================================================================

SUBROUTINE AddVarianceNames( nv,name3d,unit3d,appString,lSL )

IMPLICIT NONE

INTEGER,                    INTENT( INOUT ) :: nv
CHARACTER(*), DIMENSION(*), INTENT( OUT   ) :: name3d, unit3d
CHARACTER(*),               INTENT( IN    ) :: appString
LOGICAL,                    INTENT( IN    ) :: lSL

nv = nv + 1
name3d(nv) = 'UU'//TRIM(appString)
unit3d(nv) = 'M2/S2'

nv = nv + 1
name3d(nv) = 'VV'//TRIM(appString)
unit3d(nv) = 'M2/S2'

nv = nv + 1
name3d(nv) = 'UV'//TRIM(appString)
unit3d(nv) = 'M2/S2'

IF( lSL )THEN
  nv = nv + 1
  SELECT CASE( TRIM(appString) )
    CASE( 'L' )
      name3d(nv) = 'SLH'
    CASE DEFAULT
      name3d(nv) = 'SL'//TRIM(appString)
  END SELECT
  unit3d(nv) = 'M'
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION OutputBLprofFields( lFormat,iUnit,iOff,nTot,BLprof ) RESULT( irv )

USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

LOGICAL,           INTENT( IN ) :: lFormat
INTEGER,           INTENT( IN ) :: iUnit, iOff, nTot
TYPE( MetBLprof ), INTENT( IN ) :: BLprof

INTEGER ios, i

irv = SWIMfailure

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%UU(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%UU(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'UU-variance'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%VV(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%VV(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'VV-variance'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%WW(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%WW(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'WW-variance'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%WT(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%WT(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'WT-correlation'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%SL(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%SL(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Buoyancy length scale'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (BLprof%SZ(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (BLprof%SZ(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'Shear length scale'
  GOTO 9999
END IF

irv = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION OutputVarianceFields( lFormat,iUnit,iOff,nTot,fldVar,lSL ) &
                                                              RESULT( irv )

USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

LOGICAL,             INTENT( IN ) :: lFormat
INTEGER,             INTENT( IN ) :: iUnit, iOff, nTot
TYPE( MetVariance ), INTENT( IN ) :: fldVar
LOGICAL,             INTENT( IN ) :: lSL

INTEGER ios, i

irv = SWIMfailure

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (fldVar%UU(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (fldVar%UU(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'UU-variance'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (fldVar%VV(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (fldVar%VV(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'VV-variance'
  GOTO 9999
END IF

IF( lFormat )THEN
  WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (fldVar%UV(i+iOff),i=1,nTot)
ELSE
  WRITE(iUnit,IOSTAT=ios) (fldVar%UV(i+iOff),i=1,nTot)
END IF

IF( ios /= 0 )THEN
  error%Inform = 'UV-variance'
  GOTO 9999
END IF

IF( lSL )THEN

  IF( SIZE(fldVar%SL) >= iOff+nTot )THEN
    IF( lFormat )THEN
      WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (fldVar%SL(i+iOff),i=1,nTot)
    ELSE
      WRITE(iUnit,IOSTAT=ios) (fldVar%SL(i+iOff),i=1,nTot)
    END IF
  ELSE
    IF( lFormat )THEN
      WRITE(iUnit,'(6(1ES12.5,1X))',IOSTAT=ios) (NOT_SET_R,i=1,nTot)
    ELSE
      WRITE(iUnit,IOSTAT=ios) (NOT_SET_R,i=1,nTot)
    END IF
  END IF

  IF( ios /= 0 )THEN
    error%Inform = 'Variance length scale'
    GOTO 9999
  END IF

END IF

irv = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION OutputModelHeader( unit,lformat,grid )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: unit
LOGICAL,         INTENT( IN ) :: lformat
TYPE( MetGrid ), INTENT( IN ) :: grid

INTEGER ios, i, j, nStg2D, nStg3D
REAL    TsrfRef, lapseRef, Tiso, fac

INTEGER, DIMENSION(3) :: iStg3D

CHARACTER(80) string
CHARACTER(8)  mapProj

OutputModelHeader = SWIMfailure

error%Number  = WR_ERROR
error%Routine = 'OutputModelHeader'
error%Message = 'Error writing MEDOC file'
error%Inform  = 'Attempting to write numerical model header records'

!------ First character of mapProj depends on formatted/binary flag

IF( lformat )THEN
  mapProj(1:1) = 'F'
ELSE
  mapProj(1:1) = 'B'
END IF

!------ The rest of mapProj depends on the map projection

SELECT CASE( grid%coord%type )
  CASE( I_LATLON )
    mapProj(2:) = 'LATLON'

  CASE( I_UTM )
    mapProj(2:) = 'UTM'

  CASE( I_LAMBERT )
    mapProj(2:) = 'LAMBERT'

  CASE( I_POLAR )
    mapProj(2:) = 'POLAR  '

  CASE( I_RPOLAR )
    mapProj(2:) = 'RPOLAR'

  CASE( I_ROTLL )
    mapProj(2:) = 'ROTLL'

  CASE( I_MERCATOR )
    mapProj(2:) = 'MERCATR'

  CASE DEFAULT !I_CARTESIAN & I_METERS
    mapProj(2:) = 'CART'

END SELECT

IF( lformat )THEN
  WRITE(unit,'(A8)',IOSTAT=ios) mapProj
  IF( ios /= 0 )GOTO 9999
ELSE
  WRITE(unit,IOSTAT=ios) mapProj
  IF( ios /= 0 )GOTO 9999
END IF

!------ Standard latitude(s)

SELECT CASE( grid%coord%type )
  CASE( I_UTM )
    IF( lformat )THEN
      WRITE(unit,'(A,I2)',IOSTAT=ios) "# Map Parameters       = ",grid%coord%zone
    ELSE
      WRITE(unit,IOSTAT=ios) grid%coord%zone
    END IF

  CASE( I_LAMBERT )
    IF( lformat )THEN
      WRITE(unit,'(A,2F12.5)',IOSTAT=ios) "# Map Parameters       = ",grid%coord%Lat1,grid%coord%Lat2
    ELSE
      WRITE(unit,IOSTAT=ios ) grid%coord%Lat1,grid%coord%Lat2
    END IF

  CASE( I_POLAR,I_MERCATOR )
    IF( lformat )THEN
      WRITE(unit,'(A,2F12.5)',IOSTAT=ios) "# Map Parameters       = ",grid%coord%Lat1
    ELSE
      WRITE(unit,IOSTAT=ios ) grid%coord%Lat1
    END IF

END SELECT

IF( ios /= 0 )GOTO 9999

!------ Vertical coordinate

IF( BTEST(grid%type,GTB_SIGMA) )THEN
  string = 'SIGMAF  '

ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN
  string = 'HEIGHT  '   !3D height are always output AGL

ELSE
  string = 'SIGMAZ  '

END IF

IF( lformat )THEN
  WRITE(unit,'(A)',IOSTAT=ios ) "# Vertical Coordinate  = "//TRIM(string)
ELSE
  WRITE(unit,IOSTAT=ios) string(1:8)
END IF
IF( ios /= 0 )GOTO 9999

!------ Top pressure level for non-hydrostatic MM5 Sigma-p coordinate

IF( BTEST(grid%type,GTB_SIGMA) )THEN

  IF( lformat )THEN
    WRITE(unit,'(A24,2(ES12.4),1X)',IOSTAT=ios) "# Top & Nominal Press  =",grid%sigma%Ptop*grid%sigma%P00,grid%sigma%P00
    IF( ios /= 0 )GOTO 9999
  ELSE
    WRITE(unit,IOSTAT=ios) grid%sigma%Ptop*grid%sigma%P00,grid%sigma%P00
  END IF
  IF( ios /= 0 )GOTO 9999

!------ Base state parameters for

  fac = -G0/RGAS

  TsrfRef  = grid%sigma%a1*fac
  lapseRef = grid%sigma%a2*fac*2.

  IF( grid%sigma%aiso > 0 )THEN
    Tiso = grid%sigma%aiso*fac
  ELSE
    Tiso = -999.
  END IF

  IF( lformat )THEN
    WRITE(unit,'(A24,6(F12.5),1X)',IOSTAT=ios) "# Tsrf, Lapse, Tiso    =",TsrfRef,lapseRef,Tiso
    IF( ios /= 0 )GOTO 9999
  ELSE
    WRITE(unit,IOSTAT=ios) TsrfRef,lapseRef,Tiso
  END IF
  IF( ios /= 0 )GOTO 9999

END IF

!------ Setup staggered grids
!       N.B.  2d fields currently always at cell centers
!             Only velocity componenents are staggered
!             Shifts in SWIM are always in positive coordinate direction,
!             even if the input source is shifted in the opposite direciton
!

nStg2D = 0
nStg3D = 0

IF( BTEST(grid%type,GTB_STAGGER) )THEN
  nStg3D = nStg3D + 2                                      !U,V
  IF( BTEST(grid%type,GTB_STAGGERZ) )nStg3D = nStg3D + 1   !W
  IF( BTEST(grid%type,GTB_Z3DW)     )nStg3D = nStg3D + 1   !Z
END IF

IF( lformat )THEN
  WRITE(unit,'(A,2I5)',IOSTAT=ios) '# No. Staggered Fields = ',nStg3D,nStg2D
ELSE
  WRITE(unit,IOSTAT=ios) nStg3D,nStg2D
END IF
IF( ios /= 0 )GOTO 9999

DO i = 1,nStg3D

  iStg3D = 0

  SELECT CASE( i )
    CASE( 1 )
      string = '       U'           !8 characters for variable names
      iStg3D(1) = 1
      IF( BTEST(grid%type,GTB_STAGGERB) )iStg3D(2) = 1
    CASE( 2 )
      string = '       V'
      iStg3D(2) = 1
      IF( BTEST(grid%type,GTB_STAGGERB) )iStg3D(1) = 1
    CASE( 3 )
      string = '       W'
      iStg3D(3) = -1                !Output W down from U level
    CASE( 4 )
      string = '       Z'
      iStg3D(3) = 1                 !Output Z shifted up from first U level
  END SELECT

  IF( lformat )THEN
    WRITE(unit,'(A,3I5)',IOSTAT=ios) '# Staggered Field      = '//string(1:8),(iStg3D(j),j=1,3)
  ELSE
    WRITE(unit,IOSTAT=ios) string(1:8),(iStg3D(j),j=1,3)
  END IF

END DO

OutputModelHeader = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END
