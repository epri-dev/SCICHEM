INTEGER FUNCTION SWIMreadSCIP( fld )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ),  INTENT( INOUT ) :: fld

CHARACTER(PATH_MAXLENGTH) FileName
CHARACTER(PATH_MAXLENGTH) line
CHARACTER(128) string

INTEGER unit, ios, irv, n
INTEGER iyear, imonth, iday, ihour, imin, isec, itime

REAL,    EXTERNAL :: GetTimeMet
INTEGER, EXTERNAL :: SCIP3dField
INTEGER, EXTERNAL :: PostProgressMessage, SWIMaddLogMessage

message%cString  = 'Reading SCIP gridded file'

irv = PostProgressMessage( message )

SWIMreadSCIP = SWIMfailure

FileName = TRIM(fld%GridSource%Source(1))
unit = fld%GridSource%unit

CALL ReportFileName( line,'Reading met file ',FileName)
irv = SWIMaddLogMessage( line )
IF( irv /= SWIMsuccess )GOTO 9999

error%Number  = RD_ERROR
error%Routine = 'SWIMreadSCIP'
error%Message = 'Error reading SCIP gridded met file'
CALL ReportFileName( error%Inform,'File=',FileName)

!------ Read time record (ignore blank lines)

DO
  READ(unit,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios < 0 )THEN  !End of file
    SWIMreadSCIP = SWIMresult
    CALL SWIMclearError()
    fld%status = IBCLR(fld%status,FSB_UPDATE)
    GOTO 9999
  ELSE IF( ios > 0 )THEN
    error%Action = 'Attempting to time record'
    GOTO 9999  !Error
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

fld%tNext = GetTimeMet( iday,imonth,iyear,ihour,imin,isec, &
                                    fld%gridSource%timeOffset )

CALL c_format( fld%tNext/3600.,n,message%cString )
message%cString = 't= '//message%cString(1:n)//'hrs'
irv = PostProgressMessage( message )

WRITE(string,"('t =',1PG11.4,'hrs.')",IOSTAT=irv) fld%tNext/3600.
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read 3d fields

irv = SCIP3dField( fld )
IF( irv /= SWIMsuccess )GOTO 9999

SWIMreadSCIP = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIP3dField( fld )

!------ Read 3d fields

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, ios, unit, iv, i, k0, k, is
INTEGER nx, ny, nz, nxy
REAL    pres

REAL, DIMENSION(:), POINTER :: P
REAL, DIMENSION(:), POINTER :: U, V, Tpot, Humid, Press

INTERFACE
  INTEGER FUNCTION Read3dVarGridded( src,nx,ny,nz,var,ks0,ishft,jshft,var_src )
    USE SWIM_fi
    TYPE( GridSrc ),   INTENT( IN ) :: src
    INTEGER,           INTENT( IN ) :: nx, ny, nz
    REAL, DIMENSION(:),POINTER      :: var
    INTEGER,           INTENT( IN ) :: ks0
    INTEGER, OPTIONAL, INTENT( IN ) :: ishft
    INTEGER, OPTIONAL, INTENT( IN ) :: jshft
    REAL, DIMENSION(:), OPTIONAL, POINTER :: var_src
  END FUNCTION Read3dVarGridded
END INTERFACE

SCIP3dField = SWIMfailure

!------ Define locals

IF( BTEST(fld%type,FTB_INTRP) )THEN

  nx    = fld%gridSource%nX
  ny    = fld%gridSource%nY
  nz    = fld%gridSource%nZ

  U     => fld%GridSource%ReadField%U
  V     => fld%GridSource%ReadField%V
  Tpot  => fld%GridSource%ReadField%Tpot
  Press => fld%GridSource%ReadField%Press
  Humid => fld%GridSource%ReadField%Humid

ELSE

  nx    = fld%grid%nX
  ny    = fld%grid%nY
  nz    = fld%grid%nZ

  U     => fld%NextField%U
  V     => fld%NextField%V
  Tpot  => fld%NextField%Tpot
  Press => fld%NextField%Press
  Humid => fld%NextField%Humid

END IF

nxy  = nx*ny
k0   = 0
unit = fld%GridSource%unit

!------ Loop over variables

DO iv = 1,fld%GridSource%nVar3d

  READ(unit,'(A)',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number = RD_ERROR
    error%Routine = 'SCIP3dField'
    error%Message = 'Error reading field name'
    CALL ReportFileName( error%Inform,'File=',fld%GridSource%Source(1) )
    GOTO 9999
  END IF

  SELECT CASE( fld%GridSource%Var3dID(iv) )

    CASE( GVP_U )
      irv = Read3dVarGridded( fld%GridSource,nx,ny,nz,U,k0 )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading U field'
        GOTO 9999
      END IF

    CASE( GVP_V )
      irv = Read3dVarGridded( fld%GridSource,nx,ny,nz,V,k0 )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading V field'
        GOTO 9999
      END IF

    CASE( GVP_T )
      irv = Read3dVarGridded( fld%GridSource,nx,ny,nz,Tpot,k0 )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading temperature field'
        GOTO 9999
      END IF

    CASE( GVP_H )
      irv = Read3dVarGridded( fld%GridSource,nx,ny,nz,Humid,k0 )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading humidity field'
        GOTO 9999
      END IF

  END SELECT

END DO

!------ Fill pressure array with vertical profile;
!       Convert actual temperature (C) to potential temperature (K)

Tpot = Tpot - ABSZERO

P => fld%GridSource%P

DO k = 1,nz
  k0 = (k-1)*nxy
  pres = PSURF*EXP(P(k))
  DO is = 1,nxy
    i = k0 + is
    Tpot(i)  = Tpot(i)*(PSURF/pres)**KAPPA
    Press(i) = P(k)
  END DO
END DO

SCIP3dField = SWIMresult

9999 CONTINUE

RETURN
END

