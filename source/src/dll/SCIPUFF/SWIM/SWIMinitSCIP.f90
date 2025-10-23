INTEGER FUNCTION SWIMinitSCIP( Src )

!------ Initialize grid source structure for SCIP gridded file

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: Src

CHARACTER(PATH_MAXLENGTH) FileName
CHARACTER(PATH_MAXLENGTH) line

INTEGER unit, ios, k
INTEGER imax, jmax, kmax, nt, nvar3d
INTEGER imonth, iday, iyear, itime, ihour, imin, isec
REAL    dx, dy, xlat0, xlon0, xlen

REAL, EXTERNAL :: GetTimeMet

SWIMinitSCIP = SWIMfailure

unit     = Src%unit
FileName = TRIM(Src%Source(1))

!------ Initialize error

error%Number   = RD_ERROR
error%Routine = 'SWIMinitSCIP'
error%Message = 'Error reading SCIP gridded met file'
CALL ReportFileName( error%Inform,'File=',FileName )

!------ Open file

OPEN(UNIT=unit,FILE=FileName,STATUS='OLD',FORM='FORMATTED', &
                                            ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening SCIP gridded met file'
  GOTO 9999
END IF

!------ Ignore comments at top of file

line = '#'
DO WHILE( line(1:1) == '#' )
  READ(unit,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    error%Action = 'Attempting to read header records'
    GOTO 9999
  END IF
END DO

!------ Read grid parameters and number of variables

READ(line,'(4F10.3,5I5)',IOSTAT=ios) xlon0,xlat0,dx,dy,imax,jmax,kmax,nt,nvar3d
IF( ios /= 0 )THEN
  error%Action = 'Attempting to read grid parameters and no. of fields'
  GOTO 9999
END IF

IF( ABS(xlon0) > 360. .OR. ABS(xlat0) > 90. .OR. &
    dx <= 0. .OR. dy <= 0. .OR. imax <= 0 .OR. jmax <= 0 .OR. kmax <= 0 .OR. nt <= 0 .OR. nvar3d <= 0 )THEN
  error%Action = 'Invalid grid parameters'
  GOTO 9999
END IF

Src%nX = imax
Src%nY = jmax
Src%nZ = kmax

Src%nXY = imax*jmax

Src%nVar2d = 0
Src%nVar3d = nvar3d

CALL AdjustLongitudeDom( xlon0,FLOAT(imax-1)*dx )

Src%type = IBSET(Src%type,GSB_LATLON)
Src%dX   = dx
Src%dY   = dy
Src%X0   = xlon0
Src%Y0   = xlat0
Src%Lon0 = xlon0
Src%Lat0 = xlat0

xlen = FLOAT(imax-1)*dx
IF( 360.-xlen > 0.1*dx .AND. 360.-xlen < 1.1*dx )Src%type = IBSET(Src%type,GSB_PERIODIC)

ALLOCATE( Src%Z(kmax),Src%P(kmax),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating vertical grid arrays for SCIP gridded met file'
  GOTO 9999
END IF

!------ Read vertical grid levels and corresponding pressure levels

READ(unit,'(10F8.2)',IOSTAT=ios) (Src%Z(k),k=1,kmax) !Height
IF( ios /= 0 )THEN
  error%Action = 'Attempting to read height levels'
  GOTO 9999
END IF

READ(unit,'(10F8.2)',IOSTAT=ios) (Src%P(k),k=1,kmax) !Pressure
IF( ios /= 0 )THEN
  error%Action = 'Attempting to read pressure levels'
  GOTO 9999
END IF

Src%Ztop = Src%Z(kmax)
Src%P    = LOG(Src%P/PSURF)

!------ Set variable ID's (these are fixed)

IF( nvar3d == 0 )nvar3d = 4 !Default is U,V,T,H

IF( nvar3d /= 4 )THEN
  error%Message = 'Non-standard number of variables on SCIP gridded met file'
  GOTO 9999
END IF

ALLOCATE( Src%Var3dID(nvar3d),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array for variable names in SCIP gridded file'
  GOTO 9999
END IF

Src%Var3dID(1) = GVP_U
Src%Var3dID(2) = GVP_V
Src%Var3dID(3) = GVP_T
Src%Var3dID(4) = GVP_H

!------ Read first time (for wind uncertainty)

DO
  READ(unit,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    error%Action = 'Attempting to read first time record'
    GOTO 9999
  END IF
  IF( LEN_TRIM(line) > 0 )EXIT
END DO

READ(line,'(3(1X,I2),1X,I4)',IOSTAT=ios) imonth,iday,iyear,itime
IF( ios /= 0 )THEN
  error%Action = 'Attempting to read month,day,year,time'
  GOTO 9999
END IF

ihour = itime/100
imin  = itime - ihour*100
isec  = 0

Src%tFirst = GetTimeMet( iday,imonth,iyear,ihour,imin,isec, &
                                             Src%timeOffset )
Src%tFcst = Src%tFirst

BACKSPACE(unit,IOSTAT=ios)  !Position back to time record

!------ A SWIMming success

CALL SWIMclearError()
SWIMinitSCIP = SWIMresult

9999 CONTINUE

RETURN
END
