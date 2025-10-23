INTEGER FUNCTION SWIMreadMEDOC( fld )

USE SWIM_fi
USE SWIMparam_fd
USE Char8Array_fd
USE reallocate
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

REAL, PARAMETER :: EXPAND = 0.1
REAL, PARAMETER :: Z_EPS  = 1.E-5

TYPE( Char8Array ) Var3d, Var2d

CHARACTER(PATH_MAXLENGTH) NestSrc
CHARACTER(PATH_MAXLENGTH) FileName
CHARACTER(128) string
CHARACTER(8)   codename

INTEGER unit, ios, irv, i, n
INTEGER imax, jmax, kmax, ndum, nvar3d, nvar2d
INTEGER nz, nx, ny, nxy, j, k, k0, j0, jp0, ii, ip, jp, nAdd, alloc_stat
INTEGER n1, n2
LOGICAL lWriteZ
INTEGER ifld
REAL    dx, dy, x0, y0, xlat0, xlon0, zbtop_med, fac, zs
REAL    Hmax, dz, tMEDOC
LOGICAL lformat, lstagger, lend

TYPE ( messageT ) :: caution

REAL, DIMENSION(:), POINTER :: z
REAL, DIMENSION(:), POINTER :: u, v, D, Du, Dv

INTERFACE

  INTEGER FUNCTION MEDOCgridRecord( unit,lformat,z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )
    USE SWIM_fi
    INTEGER, INTENT( IN  )      :: unit
    LOGICAL, INTENT( IN  )      :: lformat
    INTEGER, INTENT( IN  )      :: kmax
    REAL, DIMENSION(:), POINTER :: z
    REAL,         INTENT( OUT ) :: dx, dy, x0, y0, xlat0, xlon0, zbtop_med
  END FUNCTION MEDOCgridRecord

  INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )
    USE SWIM_fi
    REAL, DIMENSION(:),      POINTER         :: zMC
    INTEGER,                 INTENT( IN    ) :: nzMC
    TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid
  END FUNCTION SWIMinitMcWIF

  INTEGER FUNCTION InitAnalysisUncertainty( fld,tAnlys,Unc )
    USE SWIM_fi
    TYPE( MetField ), TARGET, INTENT( IN  ) :: fld
    REAL,                     INTENT( IN  ) :: tAnlys
    TYPE( MetVariance ),      INTENT( OUT ) :: Unc
  END FUNCTION InitAnalysisUncertainty

  SUBROUTINE AdjustCanopyPrf( fld,is )
    USE SWIM_fi
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fld
    INTEGER,                  INTENT( IN    ) :: is
  END SUBROUTINE AdjustCanopyPrf

END INTERFACE

INTEGER, EXTERNAL :: MEDOCformatRecord, MEDOCcodenameRecord
INTEGER, EXTERNAL :: MEDOCtimeRecord, MEDOCdimensionRecord, MEDOCvarnameRecord
INTEGER, EXTERNAL :: MEDOC3dField, MEDOC2dField, MEDOCpotentialTemp, ConvertRelHumid
INTEGER, EXTERNAL :: SWIMsetPrecipProb
INTEGER, EXTERNAL :: ConvertCloudWater
INTEGER, EXTERNAL :: MEDOCopenFile, MEDOCgetNextTime, MEDOCpositionToBreak
INTEGER, EXTERNAL :: PostCautionMessage, getSmoothIndex
INTEGER, EXTERNAL :: MEDOCexpand, CheckUstar
INTEGER, EXTERNAL :: SWIMaddLogMessage, PostProgressMessage
INTEGER, EXTERNAL :: SWIMwarningMessage
INTEGER, EXTERNAL :: backup, SkipToBreak
INTEGER, EXTERNAL :: MEDOCskipModelHeader, getFieldIndex
REAL,    EXTERNAL :: SWIMsetHmin

!------ Nullify all local pointers than can be deallocated

NULLIFY(Var3d%string)
NULLIFY(Var3d%unit)
NULLIFY(Var2d%string)
NULLIFY(Var2d%unit)

!------ Setup messaging

message%cString = 'Reading MEDOC file'

irv = PostProgressMessage( message )

SWIMreadMEDOC = SWIMfailure

IF( BTEST(fld%GridSource%type,GSB_MEDOCLIST) )THEN
  ifld = getSmoothIndex( fld%GridSource%unit )
ELSE
  ifld = 1
END IF

CALL ReportFileName( string,'Reading met file ',fld%gridSource%Source(ifld))

irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Define some locals

unit    = getFieldIndex( fld%GridSource%unit )
lformat = .NOT.BTEST(fld%GridSource%type,GSB_BINARY)

!------ Position for reverse mode

IF( Reverse )THEN

  lend = fld%tNext == fld%GridSource%tFirst !Check if positioned at first time on file

  IF( .NOT.lend )THEN

    fld%GridSource%nBreak = fld%GridSource%nBreak - 1

    irv = backup( unit,fld%gridSource%nSkipBreak )
    IF( irv == SWIMfailure )irv = SkipToBreak( fld%gridSource )
    IF( irv /= SWIMsuccess )THEN
      error%Number  = RD_ERROR
      error%Routine = 'SWIMreadMEDOC'
      error%Inform  = 'Error backspacing MEDOC file in Reverse Mode'
      GOTO 9999
    END IF

  END IF

ELSE

  lend = .FALSE.

END IF

!------ Read file type record

IF( .NOT.lend )THEN
  irv = MEDOCskipModelHeader( fld%gridSource )
  IF( irv == SWIMnull )THEN
    lend = .TRUE.
  ELSE IF( irv /= SWIMsuccess )THEN
    GOTO 9999
  END IF
  IF( .NOT.lend )THEN
    irv = MEDOCformatRecord( unit,lformat,lend )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

!------ End-of-file: if climatology statistics file, rewind; otherwise, exit
!       Except if list input. If so, read next file in list

IF( lend )THEN

    IF( ifld < fld%GridSource%nSource )THEN          !Read next file in list

1000  CONTINUE

      CLOSE(unit,IOSTAT=ios)
      ifld = ifld + 1

      FileName = TRIM(fld%GridSource%Source(ifld))
      irv = MEDOCopenFile( unit,FileName,lformat )
      IF( irv /= SWIMsuccess )GOTO 9999

      CALL ReportFileName( string,'Reading next MEDOOC file ',fld%gridSource%Source(ifld))
      irv = SWIMaddLogMessage( string )
      IF( irv /= SWIMsuccess )GOTO 9999

      fld%GridSource%nBreak = 0
      irv = MEDOCpositionToBreak( fld%GridSource )   !Positions to prj%time, not current time
      IF( irv /= SWIMsuccess )GOTO 9999
      CALL setSmoothIndex( fld%GridSource%unit,ifld )

      tMEDOC = -HUGE(0.)
      DO WHILE( tMEDOC <= fld%tNext )                !Position past previous nextTime
        irv = MEDOCgetNextTime( fld%GridSource,tMEDOC )
        IF( irv == SWIMnull )THEN                        !EOF for this file
          IF( ifld < fld%GridSource%nSource )GOTO 1000   !Check next file if there is one
                                                         !Otherwise, give warning and continue
          caution%aString = 'No later time available on next MEDOC file in list'
          caution%bString = 'File = '//TRIM(FileName)
          caution%cString = ' '
          irv = PostCautionMessage( caution )
          error%Number  = WN_ERROR
          error%Routine = 'SWIMreadMEDOC'
          error%Inform  = 'Error reading next MEDOC file in list'
          error%Message = 'No later time available on next MEDOC file in list'
          error%Inform  = 'File = '//TRIM(FileName)
          error%Action  = 'Do you want to continue?'
          irv = SWIMwarningMessage( 'User elected not to continue' )
          IF( irv /= SWIMsuccess )GOTO 9999
          fld%status = IBCLR(fld%status,FSB_UPDATE)     !Clear update status & exit
          SWIMreadMEDOC = SWIMresult
          GOTO 9999
        ELSE IF( irv /= SWIMsuccess )THEN
          error%Number  = RD_ERROR
          error%Routine = 'SWIMreadMEDOC'
          error%Inform  = 'Error reading next MEDOC file in list'
          GOTO 9999
        END IF
        fld%GridSource%nBreak = fld%GridSource%nBreak + 1
      END DO

      lend = .FALSE.
      fld%GridSource%nBreak = fld%GridSource%nBreak - 1
      irv = backup( unit,fld%GridSource%nSkipBreak )

      irv = MEDOCformatRecord( unit,lformat,lend )
      IF( irv /= SWIMsuccess )GOTO 9999

    ELSE

    fld%status = IBCLR(fld%status,FSB_UPDATE)        !Clear update status & exit
    SWIMreadMEDOC = SWIMresult
    GOTO 9999

    END IF
END IF

!------ Read code name and staggered grid flag

irv = MEDOCcodenameRecord( unit,lformat,codename,lstagger,NestSrc )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read date and time

irv = MEDOCtimeRecord( unit,lformat,fld%gridSource,fld%tNext,.TRUE. )
IF( irv /= SWIMsuccess )GOTO 9999

CALL c_format( fld%tNext/3600.,n,message%cString )
message%cString = 't= '//message%cString(1:n)//'hrs'
irv = PostProgressMessage( message )

WRITE(string,"('t =',1PG11.4,'hrs.')",IOSTAT=irv) fld%tNext/3600.
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Check for reasonable forecast time (really analysis time)

irv = MEDOCtimeRecord( unit,lformat,fld%gridSource,tMEDOC,.FALSE. )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read grid dimensions and number of variables

irv = MEDOCdimensionRecord( unit,lformat,imax,jmax,kmax,ndum,nvar3d,nvar2d )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Check that dimensions match initial values

IF( fld%GridSource%nX /= imax .OR. &
    fld%GridSource%nY /= jmax .OR. &
    fld%GridSource%nZ /= kmax )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMreadMEDOC'
  CALL ReportFileName( error%Message,'Grid dimensions changed in MEDOC file ',fld%gridSource%Source(1))
  WRITE(error%Inform,FMT='(A,3I4)') 'Initial nx,ny,nz: ',fld%GridSource%nX,fld%GridSource%nY, &
                                                                 fld%GridSource%nZ
  WRITE(error%Action,FMT='(A,3I4)') 'Current         : ',imax,jmax,kmax
  GOTO 9999
END IF

IF( fld%GridSource%nVar2d /= nvar2d .OR. &
    fld%GridSource%nVar3d /= nvar3d )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMreadMEDOC'
  CALL ReportFileName( error%Message,'No. of fields changed in MEDOC file ',fld%gridSource%Source(1))
  WRITE(error%Inform,FMT='(A,3I4)') 'Initial nvar2d,nvar3d: ',fld%GridSource%nVar2d,fld%GridSource%nVar3d
  WRITE(error%Action,FMT='(A,3I4)') 'Current              : ',nvar2d,nvar3d
  GOTO 9999
END IF

!------ Read vertical grid and horizontal grid spacing and origin

irv = MEDOCgridRecord( unit,lformat,fld%gridSource%Z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Put sigma-z grid into NextZ

IF( BTEST(fld%gridSource%type,GSB_SIGMAZ) .AND. .NOT.BTEST(fld%type,FTB_MCWIF) )THEN

  fac = fld%gridSource%Ztop / (fld%gridSource%Ztop - fld%grid%Hmin)

  lWriteZ = .FALSE.
  DO k = 1,fld%grid%nZ
    IF( ABS(fld%NextZ(k)*fac-fld%gridSource%Z(fld%gridSource%kStart-1+k)) > Z_EPS*fld%gridSource%Z(fld%gridSource%kStart-1+k) )THEN
      lWriteZ = .TRUE.
      EXIT
    END IF
  END DO

!------ Check top level if there were changes to lower levels

  IF( lWriteZ )THEN
    k = fld%grid%nZ
    IF( ABS(fld%NextZ(k)*fac-fld%gridSource%Z(fld%gridSource%kStart-1+k)) > Z_EPS*fld%gridSource%Z(fld%gridSource%kStart-1+k) )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMreadMEDOC'
      WRITE(error%Message,'(A,2ES16.7)' ) 'MEDOC top sigma-z levels differ: ',fld%NextZ(k)*fac,fld%gridSource%Z(fld%gridSource%kStart-1+k)
      error%Inform  = 'They must be the same at each time'
      GOTO 9999
    END IF
  END IF

  DO k = 1,fld%grid%nZ
    fld%NextZ(k) = fld%gridSource%Z(fld%gridSource%kStart-1+k) / fac
  END DO

  IF( lWriteZ )THEN
    string = CHAR(13)  !Carriage return
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
    WRITE(string,'("Meteorology grid ",I2,": Modified vertical levels")') getFieldIndex( fld%index )
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
    n1 = 1; n2 = MIN(fld%grid%nZ,6)
    WRITE(string,103,IOSTAT=ios) (fld%NextZ(k)*fac,k=n1,n2)
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

    DO WHILE( n2 < fld%grid%nZ )
      n1 = n1+6; n2 = MIN(fld%grid%nZ,n1+5)
      WRITE(string,104,IOSTAT=ios) (fld%NextZ(k)*fac,k=n1,n2)
      irv = SWIMaddLogMessage( string )
      IF( irv /= SWIMsuccess )GOTO 9999
    END DO

103 FORMAT('z: ',6ES12.4)
104 FORMAT('   ',6ES12.4)

  END IF

END IF

!------ Read variable names and units

Var3d%n = nvar3d
Var2d%n = nvar2d

irv = MEDOCvarnameRecord( unit,lformat,Var3d,Var2d,ndum )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read 3d fields

irv = MEDOC3dField( fld )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read 2d fields

irv = MEDOC2dField( fld )
IF( irv /= SWIMsuccess )GOTO 9999

fld%GridSource%nBreak = fld%GridSource%nBreak + 1

!------ Finish some terrain-related things (first time only)

IF( BTEST(fld%status,FSB_DOINIT) )THEN

!------- Set sigma-z array for plotting if 3d time-varying height field is given

  IF( ANY(BTEST(fld%gridSource%type,(/ GSB_ZMSL,GSB_ZAGL /))) )THEN

    nz  = fld%grid%nZ
    nxy = fld%grid%nXY

    IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
      k0 = fld%grid%nxy
    ELSE
      k0 = 0
    END IF

    zs = HUGE(0.)
    DO ip = 1,nxy
      IF( fld%grid%terrain%h(ip) < zs )THEN
        zs = fld%grid%terrain%h(ip)
        ii = ip
      END IF
    END DO

    fac = fld%NextField%Z((nz-1)*nxy+ii)
    fac = (fac + zs)/fac

    DO k = 1,nz
      i = (k-1)*fld%grid%nxy + k0 + ii
      fld%grid%Z(k) = fac*fld%NextField%Z(i)
    END DO

    IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
      IF( BTEST(fld%gridSource%type,GSB_ZSTAGGER) )THEN   !Field%Z is at cell top; set grid%Z to cell center
        fld%grid%Zw(1) = 0.
        fld%grid%Zw(2:nz+1) = fld%grid%Z(1:nz)
        DO k = 1,nz
          fld%grid%Z(k) = 0.5*(fld%grid%Zw(k)+fld%grid%Zw(k+1))
        END DO
      ELSE
        fld%grid%Zw(1) = 0.
        DO k = 2,nz
          fld%grid%Zw(k) = 0.5*(fld%grid%Z(k)+fld%grid%Z(k-1))
        END DO
        fld%grid%Zw(nz+1) = 2.*fld%grid%Zw(nz) - fld%grid%Zw(nz-1)
      END IF
      fld%grid%Ztop = fld%grid%Zw(nz)
    ELSE
      fld%grid%Ztop = fld%grid%Z(nz)
    END IF

  END IF

!------ Setup McWif if vertical velocity not provided
!       N.B. Only implemented for Sigma-z vertical grids, A or C-stagger and nx,ny > 2

  IF( .NOT.BTEST(fld%type,FTB_W) .AND. BTEST(fld%grid%type,GTB_TERRAIN) )THEN

    irv = SWIMaddLogMessage( 'Terrain on MEDOC file without vertical velocity' )
    IF( irv /= SWIMsuccess )GOTO 9999

    IF( .NOT.ANY(BTEST(fld%gridSource%type,(/ GSB_SIGMAZ,GSB_SIGMAZM /))) .OR. &
        BTEST(fld%grid%type,GTB_STAGGERB) )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMreadMEDOC'
      CALL ReportFileName( error%Message,'Terrain w/o vertical velocity ',fld%gridSource%Source(1))
      error%Inform = 'Incompatible grid for computing vertical velocity'
      error%Action = 'Requires A- or C-stagger with sigma-z vertical coordinate'
      GOTO 9999
    END IF

    IF( fld%grid%nX > 2 .AND. fld%grid%nY > 2 )THEN

      irv = SWIMaddLogMessage( 'Vertical velocity generated by mass-consistent adjustments' )
      IF( irv /= SWIMsuccess )GOTO 9999
      irv = SWIMaddLogMessage( 'Horizontal velocity will be modified from file input' )
      IF( irv /= SWIMsuccess )GOTO 9999

      nz  = fld%grid%nZ
      nxy = fld%grid%nXY

      ALLOCATE( z(nz),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999

      z(1:nz) = fld%grid%Z(1:nz)

      DEALLOCATE( fld%grid%Z,STAT=alloc_stat )
      DEALLOCATE( fld%grid%Zw,STAT=alloc_stat )

!------ Check that top of grid is well above maximum terrain elevation

      Hmax = 0.
      DO i = 1,nxy
        Hmax = MAX(Hmax,fld%grid%terrain%H(i))
      END DO

      IF( z(nz) < 4.*Hmax )THEN

!------ If not, expand grid

        dz = z(nz) - z(nz-1)
        nAdd = LOG( (4.*Hmax-z(nz))*EXPAND/dz + 1. ) / LOG(EXPAND + 1.)

        irv = reallocate_real1d( z,nAdd )
        IF( irv /= 0 )THEN
          error%Number   = UK_ERROR
          error%Routine  = 'SWIMreadMEDOC'
          error%Message  = 'Error expanding vertical grid'
          GOTO 9999
        END IF

        dz = dz*(1.+EXPAND)
        DO k = 1,nAdd
          z(k+nz) = z(k-1+nz) + dz
          dz = dz*(1.+EXPAND)
        END DO
        nz = nz + nAdd

      ELSE

        nAdd = 0

      END IF

!------ Initialize mass-consistent model

      irv = SWIMinitMcWIF( z,nz,fld%grid )
      IF( irv /= SWIMsuccess )GOTO 9999

      fld%type = IBSET(fld%type,FTB_MCWIF)
      fld%type = IBSET(fld%type,FTB_W)

!------ Allocate and intialize W

      n = nxy*nz  !Additional level added in MEDOCexpand

      ALLOCATE( fld%NextField%W(n),fld%Field%W(n),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMreadMEDOC'
        error%Message = 'Error allocating W field'
        error%Inform  = 'Attempting to use MC-SCIPUFF with MEDOC file missing W'
        CALL ReportFileName( error%Action,'File=',fld%gridSource%Source(1) )
      END IF

      IF( .NOT.BTEST(fld%gridSource%type,GSB_STAGGER) .OR. nAdd > 0 )THEN
        irv = MEDOCexpand( fld,nAdd )
        IF( irv /= SWIMsuccess )GOTO 9999
      END IF

      DEALLOCATE( z,STAT=alloc_stat )

    ELSE

!------ Terrain and no W on small grid: average terrain (W=0 implicitly)

      fac = SUM(fld%grid%terrain%H) / FLOAT(fld%grid%nXY)
      fld%grid%terrain%H = fac

      irv = SWIMaddLogMessage( 'Vertical velocity not computed for small horizontal grids' )
      IF( irv /= SWIMsuccess )GOTO 9999
      irv = SWIMaddLogMessage( 'Zero vertical velocity and constant terrain assumed' )
      IF( irv /= SWIMsuccess )GOTO 9999

    END IF

  END IF

END IF

!------ Average horizontal velocity components if using McWIF with unstaggered input

IF( BTEST(fld%type,FTB_MCWIF) )THEN

  IF( .NOT.BTEST(fld%gridSource%type,GSB_STAGGER) )THEN

    nx = fld%grid%nX; ny = fld%grid%nY

    u  => fld%NextField%U
    v  => fld%NextField%V
    D  => fld%grid%terrain%D
    Du => fld%grid%terrain%Du
    Dv => fld%grid%terrain%Dv

    DO k = 1,fld%grid%nz
      k0 = k*fld%grid%nXY
      DO j = 1,ny
        j0 = (j-1)*nx
        jp0 = (MIN(j+1,ny)-1)*nx
        DO i = 1,nx
          ii = i + j0
          ip = MIN(i+1,nx) + j0
          jp = i + jp0
          u(ii+k0) = 0.5*(u(ii+k0)*D(ii)+u(ip+k0)*D(ip)) / Du(ii)
          v(ii+k0) = 0.5*(v(ii+k0)*D(ii)+v(jp+k0)*D(jp)) / Dv(ii)
        END DO
      END DO
    END DO

  END IF

  DO i = 1,fld%grid%nXY*(fld%grid%nZ+1)
    fld%NextField%W(i) = 0.
  END DO

END IF

!------ Set grid level for setting surface layer profile (above canopy)
!       N.B. set sigma%z to 3d field since that's used in set_kbl

IF( BTEST(fld%status,FSB_DOINIT) )THEN
  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
    IF( .NOT.BTEST(fld%grid%type,GTB_SIGMA) )THEN
      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        IF( BTEST(fld%grid%type,GTB_Z3DW) )THEN
          fld%grid%sigma%Zw => fld%NextField%Z
          CALL SetSigmaZ( fld%grid )
        ELSE
          fld%grid%sigma%Z => fld%NextField%Z(fld%grid%nXY+1:)
        END IF
      ELSE
        fld%grid%sigma%Z => fld%NextField%Z
      END IF
    END IF
  END IF
END IF

CALL set_kbl( fld%grid )
IF( error%Number /= NO_ERROR )GOTO 9999

IF( Prj%BL%hc > 0. .AND. .NOT.BTEST(fld%grid%type,GTB_HCNP) .AND. &
                         .NOT.BTEST(fld%grid%type,GTB_ZRUF) )THEN

  IF( .NOT.BTEST(fld%status,FSB_CANOPYWARN) )THEN
    DO i = 1,fld%grid%nXY
      IF( fld%grid%landcover%kbl(i) > 1 )THEN
        error%Number  = WN_ERROR
        error%Routine = 'SWIMreadMEDOC'
        error%Message = 'Influence height of user-specified canopy is above first MEDOC level.'
        error%Inform  = 'Wind profile will be modified.'
        error%Action  = 'Do you wish to continue?'
        irv = SWIMwarningMessage( 'User elected not to continue' )
        IF( irv /= SWIMsuccess )GOTO 9999
        EXIT
      END IF
    END DO
    fld%status = IBSET(fld%status,FSB_CANOPYWARN)
  END IF

  DO i = 1,fld%grid%nXY
    IF( fld%grid%landcover%kbl(i) > 1 )THEN
      CALL AdjustCanopyPrf( fld,i )
    END IF
  END DO

END IF

!------ Set rain probability

IF( ASSOCIATED(fld%BL%prcp) )THEN
  IF( ASSOCIATED(fld%BL%rainprb) )THEN
    irv = SWIMsetPrecipProb( fld )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

!------ Convert to relative humidity if required

IF( BTEST(fld%gridSource%type,GSB_ABSHUMID) )THEN
  irv = ConvertRelHumid( fld )                       !N.B. must be called before MEDOCpotentialTemp
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Convert cloud water mixing ratio to gm/m^3

IF( BTEST(fld%gridSource%type,GSB_QCLDMIX) )THEN
  irv = ConvertCloudWater( fld )                     !N.B. must be called before MEDOCpotentialTemp
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Convert absolute temperature to potential if required

IF( BTEST(fld%gridSource%type,GSB_ABSTEMP) )THEN
  irv = MEDOCpotentialTemp( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Convert sfc heat flux from W/m**2 to C-m/s

IF( BTEST(fld%type,FTB_HFLX) )CALL ConvertHeatFlux( fld )

!------ Check for zero pbl height

IF( BTEST(fld%type,FTB_ZI) )CALL FixZeroZi( fld )

!------ Check for zero friction velocity

IF( BTEST(fld%type,FTB_UST) )THEN
  irv = CheckUstar( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Position for reverse mode

IF( Reverse )THEN
  fld%GridSource%nBreak = fld%GridSource%nBreak - 1
  irv = backup( unit,fld%gridSource%nSkipBreak )
  IF( irv == SWIMfailure )irv = SkipToBreak( fld%gridSource )
  IF( irv /= SWIMsuccess )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMreadMEDOC'
    error%Inform  = 'Error backspacing MEDOC file in Reverse Mode'
    GOTO 9999
  END IF
END IF

SWIMreadMEDOC = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(Var3d%string) )DEALLOCATE( Var3d%string,STAT=ios )
IF( ASSOCIATED(Var3d%unit  ) )DEALLOCATE( Var3d%unit,STAT=ios )
IF( ASSOCIATED(Var2d%string) )DEALLOCATE( Var2d%string,STAT=ios )
IF( ASSOCIATED(Var2d%unit)   )DEALLOCATE( Var2d%unit,STAT=ios )

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOC3dField( fld )

!------ Read 3d fields

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, ios, iv, i, i0, ntot, ks0, k, k0, is, js, ks
INTEGER nx, ny, nz, nxy, nzt, n3d
INTEGER unit
REAL    VarianceScale, dum, fac

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

INTEGER, EXTERNAL :: getFieldIndex

MEDOC3dField = SWIMfailure

!------ Define locals for grid dimensions

ntot = fld%gridSource%nX*fld%gridSource%nY*fld%gridSource%nZ

nxy = fld%grid%nXY
nx  = fld%grid%nX
ny  = fld%grid%nY
nz  = fld%grid%nZ

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  ks0 = nxy
  n3d = nxy*(nz+1)
ELSE
  ks0 = 0
  n3d = nxy*nz
END IF

unit = getFieldIndex( fld%gridSource%unit )

!------Loop over 3d fields

DO iv = 1,fld%gridSource%nVar3d

  IF( ASSOCIATED(fld%gridSource%Var3dShift) )THEN
    is = fld%gridSource%Var3dShift(1,iv)
    js = fld%gridSource%Var3dShift(2,iv)
  ELSE
    is = 0
    js = 0
  END IF
  ks = ks0

  SELECT CASE( fld%gridSource%Var3dID(iv) )

    CASE( GVP_U )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%U,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading U field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%U(is) = fld%NextField%U(is+nxy)
        END DO
      END IF

      CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%U,n3d )

      VarianceScale = fld%gridSource%Conv3d(iv)**2 !Save for UUL and/or UUE, etc.

    CASE( GVP_V )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%V,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading V field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%V(is) = fld%NextField%V(is+nxy)
        END DO
      END IF

      CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%V,n3d )

    CASE( GVP_W )
      IF( ASSOCIATED(fld%gridSource%Var3dShift) )THEN
        IF( fld%gridSource%Var3dShift(3,iv) == -1 )ks = 0
      END IF
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%W,ks,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading W field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        k0 = nz*nxy                     !Top BC
        DO i = 1,nxy
          fld%NextField%W(k0+i) = 0.
        END DO
        IF( ks /= 0 )THEN               !Bottom BC (only if first level was above ground)
          DO i = 1,nxy
            fld%NextField%W(i) = 0.
          END DO
        END IF
      END IF

      CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%W,n3d )

    CASE( GVP_T )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%Tpot,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading temperature field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%Tpot(is) = fld%NextField%Tpot(is+nxy)
        END DO
      END IF

      SELECT CASE( NINT(fld%gridSource%Conv3d(iv)) )
        CASE( OBP_CELSIUS )
          DO is = 1,n3d
            fld%NextField%Tpot(is) = fld%NextField%Tpot(is) - ABSZERO
           END DO
       CASE( OBP_FAHRENHEIT )
          DO i = 1,n3d
            fld%NextField%Tpot(i) = (fld%NextField%Tpot(i)-32.)*0.555555 - ABSZERO
          END DO
      END SELECT

      IF( .NOT.BTEST(fld%gridSource%type,GSB_ABSTEMP) )THEN !Scale to PSURF reference from standard
        fac = (PSURF/1000.)**KAPPA
        DO is = 1,n3d
          fld%NextField%Tpot(is) = fld%NextField%Tpot(is) *  fac
         END DO
      END IF

    CASE( GVP_P )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%Press,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading pressure field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%Press(is) = fld%NextField%Press(is+nxy)
        END DO
        nzt = nz+1
      ELSE
        nzt = nz
      END IF

      DO k = 1,nzt
        k0 = (k-1)*nxy
        DO is = 1,nxy
          fld%NextField%Press(k0+is) = LOG(fld%NextField%Press(k0+is)/PSURF)
        END DO
      END DO

    CASE( GVP_H )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%Humid,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading humidity field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%Humid(is) = fld%NextField%Humid(is+nxy)
        END DO
      END IF

      IF( fld%gridSource%Conv3d(iv) > 0. )CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%Humid,n3d )

    CASE( GVP_QCLD )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%Qcloud,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading cloud water field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextField%Qcloud(is) = fld%NextField%Qcloud(is+nxy)
        END DO
      END IF

      IF( fld%gridSource%Conv3d(iv) > 0. )CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%Qcloud,n3d )

    CASE( GVP_Z )
      IF( ASSOCIATED(fld%gridSource%Var3dShift) )THEN
        IF( fld%gridSource%Var3dShift(3,iv) == -1 )ks = 0
      END IF
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextField%Z,ks,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading height field'
        GOTO 9999
      END IF

      IF( fld%gridSource%Conv3d(iv) > 0. )CALL ScaleArray( fld%gridSource%Conv3d(iv),fld%NextField%Z,n3d )

      IF( BTEST(fld%gridSource%type,GSB_ZMSL) )THEN  !Subtract terrain elevation if reading 3d height MSL field
        DO k = 1,nz
          i0 = ks0 + (k-1)*nxy
          DO is = 1,nxy
            fld%NextField%Z(i0+is) = fld%NextField%Z(i0+is) - (fld%grid%terrain%h(is) + fld%grid%Hmin)
          END DO
        END DO
      END IF

      IF( BTEST(fld%gridSource%type,GSB_ZSTAGGER) )THEN      !Check if Z is vertically staggered

        IF( .NOT.BTEST(fld%grid%type,GTB_STAGGERZ) )THEN     !Average Z to cell centers only if W is not staggered

          IF( fld%gridSource%Var3dShift(3,iv) == 1 )THEN     !Z above cell center; average down
            DO k = nz,2,-1
              i0 = ks0 + (k-1)*nxy
              DO is = 1,nxy
                i = i0 + is
                fld%NextField%Z(i) = 0.5*(fld%NextField%Z(i)+fld%NextField%Z(i-nxy))
              END DO
            END DO
            DO i = 1,nxy
              fld%NextField%Z(ks0+i) = 0.5*fld%NextField%Z(ks0+i)
            END DO
          ELSE                                               !Z below cell center; average up
            DO k = 1,nz-1
              i0 = ks0 + (k-1)*nxy
              DO is = 1,nxy
                i = i0 + is
                fld%NextField%Z(i) = 0.5*(fld%NextField%Z(i)+fld%NextField%Z(i+nxy))
              END DO
            END DO
            i0 = ks0 + (nz-1)*nxy
            DO is = 1,nxy
              i = i0 + is
              fld%NextField%Z(i) = 2.*fld%NextField%Z(i) - fld%NextField%Z(i-nxy)
            END DO
          END IF

        ELSE

          IF( ks /= 0 )THEN               !Bottom BC if first level was above ground
            DO i = 1,nxy
              fld%NextField%Z(i) = 0.
            END DO
          ELSE                            !Othwerwise, set top level (nz+1)
            k0 = nz*nxy
            DO i = 1,nxy
              fld%NextField%Z(k0+i) = 2.*fld%NextField%Z(k0+i-nxy) - fld%NextField%Z(k0+i-2*nxy)
            END DO
          END IF

        END IF

      ELSE

        IF( ks0 > 0 )THEN                 !Bottom BC: set level below ground
          DO is = 1,nxy
            fld%NextField%Z(is) = -fld%NextField%Z(ks0+is)
          END DO
        END IF

      END IF

      DO k = 1,nz
        i0 = ks0 + (k-1)*nxy
        DO is = 1,nxy
          IF( fld%NextField%Z(i0+is) <= 0. )THEN
            error%Number  = IV_ERROR
            error%Routine = 'MEDOC3dField'
            WRITE(error%Message,FMT='(A,ES12.4)') '3D height field has invalid value',fld%NextField%Z(i0+is)
            WRITE(error%Inform,FMT='(A,I6)') 'Horizontal grid index =',is
            WRITE(error%Action,FMT='(A,I4)') 'Vertical level =',k
            GOTO 9999
          END IF
        END DO
      END DO

    CASE( GVP_TKE )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextQLprof%QQ,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading TKE field'
        GOTO 9999
      END IF

      IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
        DO is = 1,nxy
          fld%NextQLprof%QQ(is) = fld%NextQLprof%QQ(is+nxy)
        END DO
      END IF

    CASE( GVP_UUL )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextLSV%UU,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading UUL field'
        GOTO 9999
      END IF

    CASE( GVP_VVL )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextLSV%VV,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading VVL field'
        GOTO 9999
      END IF

    CASE( GVP_UVL )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextLSV%UV,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading UVL field'
        GOTO 9999
      END IF

    CASE( GVP_SHL )
      irv = Read3dVarGridded( fld%gridSource,nx,ny,nz,fld%NextLSV%SL,ks0,is,js )
      IF( irv /= SWIMsuccess )THEN
        error%Inform = 'Error reading SHL field'
        GOTO 9999
      END IF

    CASE DEFAULT
      IF( BTEST(fld%gridSource%type,GSB_BINARY) )THEN
        READ(unit,IOSTAT=ios) (dum,i=1,ntot)
      ELSE
        READ(unit,'(6(F12.4,1X))',IOSTAT=ios) (dum,i=1,ntot)
      END IF
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'MEDOC3dField'
        CALL ReportFileName( error%Message,'Error reading MEDOC file',fld%gridSource%Source(1) )
        error%Inform  = 'Error reading unused field'
        GOTO 9999
      END IF

  END SELECT
END DO

!------ Scale UUL, UUE if necessary

IF( Prj%LSVType == LVP_MET .AND. VarianceScale /= 1.0 )THEN
  CALL ScaleArray( VarianceScale,fld%NextLSV%UU,n3d )
  CALL ScaleArray( VarianceScale,fld%NextLSV%VV,n3d )
  CALL ScaleArray( VarianceScale,fld%NextLSV%UV,n3d )

  DO k = 1,nz
    k0 = (k-1)*nxy + ks0
    DO is = 1,nxy
      i = k0 + is
      IF( fld%NextLSV%UV(i)**2 > fld%NextLSV%UU(i)*fld%NextLSV%VV(i) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'MEDOC3dField'
        error%Message = 'Unrealizable large-scale variability fields'
        error%Inform  = 'UVL*UVL > UUL*VVL'
        GOTO 9999
       END IF
    END DO
  END DO

END IF

MEDOC3dField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOC2dField( fld )

!------ Read 2d fields

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, iv, i, ntot, nchk, ios
INTEGER unit
REAL    dum

REAL, DIMENSION(:), POINTER :: luc

INTERFACE
  INTEGER FUNCTION MEDOCRead2dVar( src,grid,var )
    USE SWIM_fi
    TYPE( GridSrc ), INTENT( IN ) :: src
    TYPE( MetGrid ), INTENT( IN ) :: grid
    REAL, DIMENSION(:), POINTER   :: var
  END FUNCTION MEDOCRead2dVar
END INTERFACE

INTEGER, EXTERNAL :: getFieldIndex

MEDOC2dField  = SWIMfailure
error%Number  = IV_ERROR
error%Routine = 'MEDOC2dField'

unit = getFieldIndex( fld%gridSource%unit )

!------ Total 2d grid size

ntot = fld%gridSource%nX*fld%gridSource%nY
nchk = fld%grid%nX*fld%grid%nY

!------ Loop over 2d fields

DO iv = 1,fld%gridSource%nVar2d
  SELECT CASE( fld%gridSource%Var2dID(iv) )

    CASE( GVP_TERRAIN )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%terrain%H )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( GVP_ZRUF )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%landcover%roughness )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
        IF( fld%grid%landcover%roughness(i) <= 0. )THEN
          fld%grid%landcover%roughness(i) = 1.E-4
        END IF
      END DO

    CASE( GVP_HCNP )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%landcover%canopyHt )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
     	  IF( fld%grid%landcover%canopyHt(i) < 0. )THEN
          error%Message = 'Invalid canopy height: must be non-negative'
          GOTO 9999
        END IF
      END DO

    CASE( GVP_ALPHA )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%landcover%alpha )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
     	  IF( fld%grid%landcover%alpha(i) < 0. )THEN
          error%Message = 'Invalid alpha: must be non-negative'
          GOTO 9999
        END IF
      END DO

    CASE( GVP_ALBEDO )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%landcover%albedo )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
     	  IF( fld%grid%landcover%albedo(i) < 0. )THEN
          error%Message = 'Invalid albedo: must be non-negative'
          GOTO 9999
        END IF
      END DO

    CASE( GVP_BOWEN )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%grid%landcover%Bowen )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
     	  IF( fld%grid%landcover%Bowen(i) < 0. )THEN
          error%Message = 'Invalid Bowen ratio: must be non-negative'
          GOTO 9999
        END IF
      END DO

    CASE( GVP_LANDUSE )
      ALLOCATE( luc(nchk),STAT=ios )
      IF( ios /= 0 )THEN
        error%Message = 'Error allocating landuse category array'
        GOTO 9999
      END IF
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,luc )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
        IF( luc(i) < 1. )THEN
          error%Message = 'Invalid landuse category: must be positive'
          GOTO 9999
        ELSE
          fld%grid%landcover%LandUse(i) = NINT(luc(i))
        END IF
      END DO
      DEALLOCATE( luc,STAT=ios)

    CASE( GVP_ZI )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%zi )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( GVP_HFLX )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%HeatFlux )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( GVP_UST )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%ustr )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( GVP_ZRUFT )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%zruf )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
        IF( fld%NextBL%zruf(i) <= 0. )THEN
          fld%NextBL%zruf(i) = 1.E-4
        END IF
      END DO

    CASE( GVP_PRATE,GVP_ACCPR )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%prcp )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nchk
     	  IF( fld%NextBL%prcp(i) < 0. )THEN
          error%Message = 'Invalid precipitation rate: must be non-negative'
          GOTO 9999
        END IF
      END DO

      IF( fld%gridSource%Conv2d(iv) > 0. )CALL ScaleArray( fld%gridSource%Conv2d(iv),fld%NextBL%prcp,fld%grid%nXY )

    CASE( GVP_CC )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%cc )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( GVP_LAI )
      irv = MEDOCRead2dVar( fld%gridSource,fld%grid,fld%NextBL%LAI )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE DEFAULT
      IF( BTEST(fld%gridSource%type,GSB_BINARY) )THEN
        READ(unit,IOSTAT=ios) (dum,i=1,ntot)
      ELSE
        READ(unit,*,IOSTAT=ios) (dum,i=1,ntot)
      END IF
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'MEDOC2dField'
        error%Message = 'Error reading MEDOC file'
        GOTO 9999
      END IF

  END SELECT
END DO

MEDOC2dField = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCRead2dVar( src,grid,var )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( IN ) :: src
TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:), POINTER   :: var

INTEGER im, jm, nx, ny, nxym, ios
INTEGER unit
INTEGER iskip, jskip
INTEGER i, j, ii, jj, j0, jj0, ivar, iwrk

REAL, DIMENSION(:), ALLOCATABLE :: wrk

INTEGER, EXTERNAL :: getFieldIndex

MEDOCRead2dVar = SWIMfailure

unit = getFieldIndex( src%unit )

!------ Set total array sizes and other locals

im = src%nX; jm = src%nY
nx = grid%nX; ny = grid%nY

nxym = im*jm

iskip = src%iSkip; jskip = src%jSkip

!------ Allocate work array for reading entire array

ALLOCATE( wrk(nxym),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'MEDOCRead2dVar'
  error%Message = 'Error allocating temporary array'
  error%Inform  = 'Reading MEDOC file'
  GOTO 9999
END IF

!------ Read field into work array

IF( BTEST(src%type,GSB_BINARY) )THEN
  READ(unit,IOSTAT=ios) (wrk(i),i=1,nxym)
ELSE
  READ(unit,'(6(f12.0,1x))',IOSTAT=ios) (wrk(i),i=1,nxym)
END IF

IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'MEDOCRead2dVar'
  error%Message = 'Error reading MEDOC file'
  GOTO 9999
END IF

!------ Copy out (accounting for skipping)

DO jj = 1,ny
  j   = MIN((jj-1)*jskip + src%jStart,src%jEnd)
  j0  = (j-1)*im
  jj0 = (jj-1)*nx
  DO ii = 1,nx
    i    = MIN((ii-1)*iskip + src%iStart,src%iEnd)
    ivar = jj0 + ii
    iwrk = j0  + i
    var(ivar) = wrk(iwrk)
  END DO
END DO

IF( BTEST(src%type,GSB_PERIODIC) )THEN
  DO j = 1,ny
    j0 = (j-1)*nx
    var(j0+nx) = var(j0+1)
  END DO
END IF

MEDOCRead2dVar = SWIMresult

9999 CONTINUE

IF( ALLOCATED(wrk) )DEALLOCATE( wrk,STAT=ios )

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCpotentialTemp( fld )

!------ Convert gridded absolute temperature (TA) to potential temperature

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, k, nxy, k0, ip, nzt, alloc_stat
REAL    hs, ht, t, tz

REAL, DIMENSION(:), ALLOCATABLE :: P

MEDOCpotentialTemp = SWIMfailure

nxy = fld%grid%nXY

IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
  k0  = nxy
  nzt = fld%grid%nZ + 1
ELSE
  k0  = 0
  nzt = fld%grid%nZ
END IF

ALLOCATE( P(SIZE(fld%NextField%Tpot)),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'MEDOCpotentialTemp'
  error%Message = 'Error allocating work array'
  error%Inform  = 'Reading MEDOC file'
  GOTO 9999
END IF

IF( BTEST(fld%type,FTB_P) )THEN

  IF( k0 > 0 )THEN
    DO i = 1,nxy
      fld%NextField%Press(i) = fld%NextField%Press(i+nxy)
    END DO
  END IF

!$OMP PARALLEL SHARED(fld,nxy,nzt,P)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy*nzt
    P(i) = EXP(fld%NextField%Press(i))  !Pressure ratio
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

!$OMP PARALLEL SHARED(fld,nxy,P)
!$OMP DO PRIVATE(i,hs,k,ip,ht,t,tz)
  DO i = 1,nxy
    hs = fld%grid%terrain%H(i) + fld%grid%Hmin
    DO k = 1,fld%grid%nZ
      ip = (k-1)*nxy + i + k0
      ht = hs + fld%grid%Z(k)*fld%grid%terrain%D(i)
      CALL stnd_atmos( ht,P(ip),t,tz,SA_TA )
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF( k0 > 0 )THEN
    DO i = 1,nxy
      P(i) = P(i+nxy)
    END DO
  END IF

END IF

!$OMP PARALLEL SHARED(fld,nxy,nzt,P)
!$OMP DO PRIVATE(i)
DO i = 1,nxy*nzt
  fld%NextField%Tpot(i) = fld%NextField%Tpot(i)/P(i)**KAPPA
END DO
!$OMP END DO
!$OMP END PARALLEL

MEDOCpotentialTemp = SWIMresult

9999 CONTINUE

IF( ALLOCATED(P) )DEALLOCATE( P,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION ConvertRelHumid( fld )

!------ Convert MEDOC absolute humidity field to relative
!       N.B. uses absolute temperature BEFORE conversion to potential temperature
!       if gridded input is absolute (fld%gridSource%type bit GSB_ABSTEMP)

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, k, nxy, k0, ip, nzt, alloc_stat

REAL    hs, ht, hsx, pres

REAL, DIMENSION(:), ALLOCATABLE :: P, Ta

REAL, EXTERNAL :: StndTemp, StndLogP

ConvertRelHumid = SWIMfailure

nxy = fld%grid%nXY

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  k0  = nxy
  nzt = fld%grid%nZ + 1
ELSE
  k0  = 0
  nzt = fld%grid%nZ
END IF

ALLOCATE( P(nxy*nzt),Ta(nxy*nzt),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'ConvertRelHumid'
  error%Message = 'Error allocating work arrays'
  error%Inform  = 'Reading MEDOC file'
  GOTO 9999
END IF

!------ Setup pressure field

IF( BTEST(fld%type,FTB_P) )THEN

  IF( k0 > 0 )THEN
     DO i = 1,nxy
      fld%NextField%Press(i) = fld%NextField%Press(i+nxy)
    END DO
  END IF

!$OMP PARALLEL SHARED(fld,nzt,nxy,P)
!$OMP DO PRIVATE(i)
  DO i = 1,nzt*nxy
    P(i) = EXP(fld%NextField%Press(i)) !Pressure ratio
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

!$OMP PARALLEL SHARED(fld,nxy,P)
!$OMP DO PRIVATE(i,hs,k,ip,ht)
  DO i = 1,nxy
    hs = fld%grid%terrain%H(i) + fld%grid%Hmin
    DO k = 1,fld%grid%nZ
      ip = (k-1)*nxy+ i + k0
      ht = hs + fld%grid%Z(k)*fld%grid%terrain%D(i)
      P(ip) = EXP(StndLogP( ht ))
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF( k0 > 0 )THEN
     DO i = 1,nxy
      P(i) = P(i+nxy)
    END DO
  END IF

END IF

!------ Setup absolute temperature field

IF( BTEST(fld%type,FTB_T) .AND. k0 > 0 )THEN
  DO i = 1,nxy
    fld%NextField%Tpot(i) = fld%NextField%Tpot(i+nxy)
  END DO
END IF

IF( BTEST(fld%gridSource%type,GSB_ABSTEMP) )THEN

!$OMP PARALLEL SHARED(fld,nxy,nzt,Ta)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy*nzt
    Ta(i) = fld%NextField%Tpot(i)     !'Tpot' is actual T at this point
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE IF( BTEST(fld%type,FTB_T) )THEN

!$OMP PARALLEL SHARED(fld,nzt,nxy,Ta,P)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy*nzt
    Ta(i) = fld%NextField%Tpot(i) * P(i)**KAPPA !'Tpot' is potential T
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

!$OMP PARALLEL SHARED(fld,nxy,Ta)
!$OMP DO PRIVATE(i,hs,k,ip,ht)
  DO i = 1,nxy
    hs = fld%grid%terrain%H(i) + fld%grid%Hmin
    DO k = 1,fld%grid%nZ
      ip = (k-1)*nxy + i + k0
      ht = hs + fld%grid%Z(k)*fld%grid%terrain%D(i)
      Ta(ip) = StndTemp( ht)
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF( k0 > 0 )THEN
    DO i = 1,nxy
      Ta(i) = Ta(i+nxy)
    END DO
  END IF

END IF

!------ Get saturation humidity (gm moisture / gm dry air)
!       and set relative humidity field

!$OMP PARALLEL SHARED(fld,nzt,nxy,Ta,P)
!$OMP DO PRIVATE(i,pres,hsx)
DO i = 1,nxy*nzt
  pres = P(i)*PSURF
  CALL RelHumid( fld%NextField%Humid(i),Ta(i),pres,hsx )
  fld%NextField%Humid(i) = hsx
END DO
!$OMP END DO
!$OMP END PARALLEL

ConvertRelHumid = SWIMresult

9999 CONTINUE

IF( ALLOCATED(P)  )DEALLOCATE( P,STAT=alloc_stat )
IF( ALLOCATED(Ta) )DEALLOCATE( Ta,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION ConvertCloudWater( fld )

!------ Convert cloud water in mixing ration to g/m^3
!       N.B. uses absolute temperature BEFORE conversion to potential temperature
!       if gridded input is absolute (fld%gridSource%type bit GSB_ABSTEMP)

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, k, nxy, k0, i0, ip, nzt, alloc_stat
REAL    hs, ht, pres

REAL, DIMENSION(:), ALLOCATABLE :: P, Ta

REAL, EXTERNAL :: StndTemp, StndLogP, fun_rhoa

ConvertCloudWater = SWIMfailure

nxy = fld%grid%nXY

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  k0  = nxy
  nzt = fld%grid%nZ + 1
ELSE
  k0  = 0
  nzt = fld%grid%nZ
END IF

ALLOCATE( P(nxy*nzt),Ta(nxy*nzt),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'ConvertRelHumid'
  error%Message = 'Error allocating work arrays'
  error%Inform  = 'Reading MEDOC file'
  GOTO 9999
END IF

!------ Setup pressure field

IF( BTEST(fld%type,FTB_P) )THEN

  IF( k0 > 0 )THEN
     DO i = 1,nxy
      fld%NextField%Press(i) = fld%NextField%Press(i+nxy)
    END DO
  END IF

   DO k = 1,nzt
    i0 = (k-1)*nxy
    DO i = 1,nxy
      P(i+i0) = EXP(fld%NextField%Press(i+i0)) !Pressure ratio
    END DO
  END DO

ELSE

  DO i = 1,nxy
    hs = fld%grid%terrain%H(i) + fld%grid%Hmin
    DO k = 1,fld%grid%nZ
      ip = (k-1)*nxy+ i + k0
      ht = hs + fld%grid%Z(k)*fld%grid%terrain%D(i)
      P(ip) = EXP(StndLogP( ht ))
    END DO
  END DO

  IF( k0 > 0 )THEN
     DO i = 1,nxy
      P(i) = P(i+nxy)
    END DO
  END IF

END IF

!------ Setup absolute temperature field

IF( BTEST(fld%type,FTB_T) .AND. k0 > 0 )THEN
  DO i = 1,nxy
    fld%NextField%Tpot(i) = fld%NextField%Tpot(i+nxy)
  END DO
END IF

IF( BTEST(fld%gridSource%type,GSB_ABSTEMP) )THEN

  DO i = 1,nxy*nzt
    Ta(i) = fld%NextField%Tpot(i)     !'Tpot' is actual T at this point
  END DO

ELSE IF( BTEST(fld%type,FTB_T) )THEN

  DO k = 1,nzt
    i0 = (k-1)*nxy
    DO i = 1,nxy
      Ta(i0+i) = fld%NextField%Tpot(i0+i) * P(i0+i)**KAPPA !'Tpot' is potential T
    END DO
  END DO

ELSE

  DO i = 1,nxy
    hs = fld%grid%terrain%H(i) + fld%grid%Hmin
    DO k = 1,fld%grid%nZ
      ip = (k-1)*nxy + i + k0
      ht = hs + fld%grid%Z(k)*fld%grid%terrain%D(i)
      Ta(ip) = StndTemp( ht)
    END DO
  END DO

  IF( k0 > 0 )THEN
    DO i = 1,nxy
      Ta(i) = Ta(i+nxy)
    END DO
  END IF

END IF

!------ Get air density and multiply
!       N.B. assumes Qcloud already multiplied by 1E3

DO k = 1,nzt
  i0 = (k-1)*nxy
  DO i = 1,nxy
    ip = i0 + i
    pres = P(ip)*PSURF
    fld%NextField%Qcloud(ip) = fld%NextField%Qcloud(ip) *  fun_rhoa( Ta(ip),pres )
  END DO
END DO

ConvertCloudWater = SWIMresult

9999 CONTINUE

IF( ALLOCATED(P)  )DEALLOCATE( P,STAT=alloc_stat )
IF( ALLOCATED(Ta) )DEALLOCATE( Ta,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE ConvertHeatFlux( fld )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, nxy
REAL    hs, t, tz, pr
LOGICAL tflag, pflag

REAL, EXTERNAL :: fun_rhoa

tflag = BTEST(fld%type,FTB_T)
pflag = BTEST(fld%type,FTB_P)

nxy = fld%grid%nXY

DO i = 1,nxy
  hs = fld%grid%terrain%H(i) + fld%grid%Hmin
  IF( .NOT.(tflag .AND. pflag) )CALL stnd_atmos( hs,pr,t,tz,SA_TA )
  IF( pflag )pr = EXP(fld%NextField%Press(i))
  IF( tflag )t  = fld%NextField%Tpot(i)*pr**KAPPA
  fld%NextBL%HeatFlux(i) = fld%NextBL%HeatFlux(i) / (fun_rhoa( t,pr*PSURF )*CP)
END DO

RETURN
END

!==============================================================================

SUBROUTINE FixZeroZi( fld )

!------ Estimate Zi when model value is zero

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

REAL, PARAMETER :: RIC = 10.

REAL, DIMENSION(:), POINTER :: U, V, T, P, Zi, H0, zruf, Hc, alp

INTEGER i, j, i0, i0m, is, im, ism, jsm, ksm
REAL    zref, uref, vref, tref, cc, ustar, L, hs, ri, ts
LOGICAL tflag, pflag

U => fld%NextField%U
V => fld%NextField%V
T => fld%NextField%Tpot
P => fld%NextField%Press

Zi => fld%NextBL%zi
H0 => fld%NextBL%HeatFlux

IF( BTEST(fld%type,FTB_ZRUF) )THEN
  zruf => fld%NextBL%zruf
ELSE
  zruf => fld%grid%landcover%roughness
END IF
Hc   => fld%grid%landcover%canopyHt
alp  => fld%grid%landcover%alpha

cc = Prj%BL%cc

tflag = BTEST(fld%type,FTB_T)
pflag = BTEST(fld%type,FTB_P)

DO j = 1,fld%grid%nY
  i0 = (j-1)*fld%grid%nX
  IF( BTEST(fld%grid%type,GTB_STAGGER) )i0m = (MAX(j-1,1)-1)*fld%grid%nX

  DO i = 1,fld%grid%nX
    is = i0 + i
    IF( BTEST(fld%grid%type,GTB_STAGGER) )im = MAX(i-1,1)

    IF( Zi(is) <= 0. )THEN

      IF( BTEST(fld%grid%type,GTB_SIGMA) )THEN
        zref = fld%grid%sigma%Z(is)
      ELSE IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
          zref = fld%NextField%Z(is+fld%grid%nXY)
          IF( BTEST(fld%grid%type,GTB_Z3DW) )zref = 0.5*zref !If Z is defined on W-point
        ELSE
          zref = fld%NextField%Z(is)
        END IF
      ELSE
        zref = fld%grid%Z(1)*fld%grid%terrain%D(is)
      END IF

      IF( BTEST(fld%type,FTB_HFLX) )THEN

        IF( H0(is) <= 0. )THEN

          IF( BTEST(fld%grid%type,GTB_STAGGERB) )THEN
            ism = i0  + im
            jsm = i0m + i
            ksm = i0m + im
            uref = 0.25*(U(is)+U(ism)+U(jsm)+U(ksm))
            vref = 0.25*(V(is)+V(ism)+V(jsm)+V(ksm))
          ELSE IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
            ism  = i0  + im
            jsm  = i0m + i
            uref = 0.5*(U(is)+U(ism))
            vref = 0.5*(V(is)+V(jsm))
          ELSE
            uref = U(is); vref = V(is)
          END IF

          uref = SQRT(uref**2+vref**2)

          IF( tflag )THEN
            tref  = T(is)
          ELSE
            hs = fld%grid%terrain%H(is) + fld%grid%Hmin
            CALL stnd_atmos( hs,ri,tref,ts,SA_TPOT )
          END IF

          IF( BTEST(fld%type,FTB_UST) )THEN
            ustar = fld%NextBL%ustr(is)
          ELSE
            ustar = -1.
          END IF

          CALL hdayus( uref,tref,zref,zruf(is),cc,Hc(is),alp(is),.TRUE.,H0(is),ustar,L )

          ts = -H0(is)/MAX(ustar,SMALL)        !Theta-star
          ts = 0.74*ts/VONK*LOG(zref/zruf(is)) !Surface temp difference

          ri = ts*zref * G0/(tref*ustar**2)

          IF( ri > RIC )THEN
            Zi(is) = RIC/ri * zref
          ELSE
            Zi(is) = zref
          END IF

!          Zi(is) = MIN(5.*ABS(L),zref)

        ELSE

          fld%NextBL%zi(is) = zref  !Set to lowest grid level for positive heat flux

        END IF

      ELSE

        fld%NextBL%zi(is) = zref    !Set to lowest grid level if heat flux unavailable

      END IF

    END IF

  END DO
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION CheckUstar( fld )

!------ Check for zero ustar; also set to minimum value, if necessary
!       N.B. Exactly zero values only allowed with very, very small velocity

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

REAL, PARAMETER :: UST_MIN = 1.E-4

REAL, DIMENSION(:), POINTER :: U, V, ust

INTEGER i, j, i0, i0m, is, im, ism, jsm, ksm
REAL    uref, vref

CheckUstar = SWIMfailure

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  i0 = fld%grid%nXY
ELSE
  i0 = 0
END IF

U   => fld%NextField%U(i0+1:)
V   => fld%NextField%V(i0+1:)
ust => fld%NextBL%ustr

DO j = 1,fld%grid%nY
  i0 = (j-1)*fld%grid%nX
  IF( BTEST(fld%grid%type,GTB_STAGGER) )i0m = (MAX(j-1,1)-1)*fld%grid%nX

  DO i = 1,fld%grid%nX
    is = i0 + i

    IF( ust(is) < 1.E-10 )THEN

      IF( BTEST(fld%grid%type,GTB_STAGGER) )im = MAX(i-1,1)

      IF( BTEST(fld%grid%type,GTB_STAGGERB) )THEN
        ism = i0  + im
        jsm = i0m + i
        ksm = i0m + im
        uref = MINVAL(ABS((/U(is),U(ism),U(jsm),U(ksm)/)))          !Take minium of surrounding velocities
        vref = MINVAL(ABS((/V(is),V(ism),V(jsm),V(ksm)/)))
      ELSE IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
        ism  = i0  + im
        jsm  = i0m + i
        uref = MINVAL(ABS((/U(is),U(ism)/)))                        !Take minium of surrounding velocities
        vref = MINVAL(ABS((/V(is),V(jsm)/)))
      ELSE
        uref = U(is); vref = V(is)
      END IF

      uref = SQRT(uref**2+vref**2)

      IF( uref > 1.E-6 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'CheckUstar'
        error%Message = 'Invalid USTAR on MEDOC file'
        error%Inform  = 'Value must be greater than zero'
        error%Action  = 'File '//TRIM(fld%gridSource%Source(1))
        GOTO 9999
      END IF

    END IF

    ust(is) = MAX(ust(is),UST_MIN)  !Set minimum value

  END DO
END DO

CheckUstar = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE AdjustCanopyPrf( fld,is )

!------ Adjust wind profiles for user-defined canopy
!       Assume neutral canopy profile below 2 x canopy height (approx)

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetField ), TARGET, INTENT( INOUT ) :: fld
INTEGER,                  INTENT( IN    ) :: is

INTEGER nx, ny, nxy
INTEGER i, j, ip, jp, ispx, ispy, ispxy, kbl, ik, k
REAL    hc, alp, zrfu, zrfv, ufac, vfac, fac, du, dv
LOGICAL lStgC

REAL, DIMENSION(:), ALLOCATABLE :: zu, zv
TYPE( MetGrid ), POINTER :: grd

REAL, EXTERNAL :: ulog

grd => fld%grid

IF( grd%landcover%kbl(is) == 1 )GOTO 9999

nx = grd%nX; ny = grd%nY; nxy = grd%nXY

j = (is-1) / nx + 1
i = is - (j-1)*nx

hc   = grd%landcover%canopyHt(is)  !Constant (if we're in this subroutine)
alp  = grd%landcover%alpha(is)     !Constant
zrfu = grd%landcover%roughness(is)
kbl  = grd%landcover%kbl(is)       !From set_kbl

ALLOCATE( zu(kbl),STAT=ip )
IF( ip /= 0 )GOTO 9999

lStgC = .FALSE.

IF( BTEST(grd%type,GTB_Z3D) )THEN
  ik = (kbl-1)*nxy + is
  zu = grd%sigma%Z(is:ik:nxy)
ELSE
  du = grd%terrain%d(is)
  zu = grd%Z(1:kbl)
END IF

IF( BTEST(grd%type,GTB_STAGGER) )THEN

  ip = MIN(i+1,nx)
  jp = MIN(j+1,ny)
  ispx = (j -1)*nx + ip
  ispy = (jp-1)*nx + i

  IF( BTEST(grd%type,GTB_STAGGERB) )THEN

    ispxy = (jp-1)*nx + ip
    zrfu  = 0.25*(zrfu+grd%landcover%roughness(ispx) + &
                       grd%landcover%roughness(ispy) + grd%landcover%roughness(ispxy))

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      ik = (kbl-1)*nxy+ispxy
      zu = 0.25*(zu+grd%sigma%Z(ispx:ik:nxy) + &
                    grd%sigma%Z(ispy:ik:nxy) + grd%sigma%Z(ispxy:ik:nxy))

    ELSE
      du = 0.25*(grd%terrain%d(is) + grd%terrain%d(ispx) + grd%terrain%d(ispy) + &
                                                           grd%terrain%d(ispxy))
      zu = zu*du
    END IF

    ufac = ulog( hc,alp,zu(kbl),zrfu,0. )
    vfac = ufac

  ELSE

    ALLOCATE( zv(kbl),STAT=ip )
    IF( ip /= 0 )GOTO 9999

    zrfv = 0.5*(zrfu+grd%landcover%roughness(ispy))
    zrfu = 0.5*(zrfu+grd%landcover%roughness(ispx))

    IF( BTEST(grd%type,GTB_Z3D) )THEN
      ik  = (kbl-1)*nxy+MAX(ispx,ispy)
      zu = 0.5*(zu+grd%sigma%Z(ispx:ik:nxy))
      zv = 0.5*(zv+grd%sigma%Z(ispy:ik:nxy))
    ELSE
      du = 0.5*(grd%terrain%d(is)+grd%terrain%d(ispx))
      dv = 0.5*(grd%terrain%d(is)+grd%terrain%d(ispy))
      zu = zu*du
      zv = zu*dv
    END IF

    ufac = ulog( hc,alp,zu(kbl),zrfu,0. )
    vfac = ulog( hc,alp,zv(kbl),zrfv,0. )

    lStgC = .TRUE.

  END IF

ELSE

  IF( .NOT.BTEST(grd%type,GTB_Z3D) )zu = zu*du

  ufac = ulog( hc,alp,zu(kbl),zrfu,0. )
  vfac = ufac

END IF

IF( BTEST(grd%type,GTB_STAGGERZ) )THEN
  ip = is + nxy
ELSE
  ip = is
END IF

ik = (kbl-1)*nxy + ip
ufac = fld%NextField%U(ik)/ufac
vfac = fld%NextField%V(ik)/vfac

DO k = 1,kbl-1
  ik  = (k-1)*nxy + ip

  IF( zu(k) <= 0. )THEN
    fac = 0.
  ELSE
    fac = ulog( hc,alp,zu(k),zrfu,0. )
  END IF
  fld%NextField%U(ik) = ufac * fac

  IF( lStgC )THEN
    IF( zv(k) <= 0. )THEN
      fac = 0.
    ELSE
      fac = ulog( hc,alp,zv(k),zrfv,0. )
    END IF
  END IF
  fld%NextField%V(ik) = vfac * fac

END DO

IF( BTEST(grd%type,GTB_STAGGERZ) )THEN
  fld%NextField%U(is) = fld%NextField%U(is+nxy)
  fld%NextField%V(is) = fld%NextField%V(is+nxy)
END IF

9999 CONTINUE

IF( ALLOCATED(zu) )DEALLOCATE( zu,STAT=ip )
IF( ALLOCATED(zv) )DEALLOCATE( zv,STAT=ip )

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCexpand( fld,nAdd )

!------ Expand fields when changing from unstaggered to staggered grid
!       and/or adding levels to the top of the domain.
!       N.B. Only used with MEDOC files without W but with terrain

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld
INTEGER,          INTENT( IN    ) :: nAdd

INTEGER irv
LOGICAL AddStagger

INTERFACE
  INTEGER FUNCTION AddSlice3D( grid,var3d,nAdd,AddStagger,lFill )
    USE SWIM_fi
    TYPE( MetGrid ),    INTENT( IN ) :: grid
    REAL, DIMENSION(:), POINTER      :: var3d
    INTEGER,            INTENT( IN ) :: nAdd
    LOGICAL,            INTENT( IN ) :: AddStagger
    LOGICAL,            INTENT( IN ) :: lFill
  END FUNCTION AddSlice3D
END INTERFACE

MEDOCexpand  = SWIMfailure
error%Inform = 'Attempting to use MC-SCIPUFF with MEDOC missing vertical velocity'
CALL ReportFileName( error%Action,'File=',fld%gridSource%Source(1) )

AddStagger = .NOT.BTEST(fld%gridSource%type,GSB_STAGGER)

!------ Horizontal velocity components (N.B. W done in calling routine)

irv = AddSlice3D( fld%grid,fld%Field%U,nAdd,AddStagger,.FALSE. )
IF( irv /= SWIMsuccess )THEN
  error%Message  = TRIM(error%Message)//'Field U'; GOTO 9999
END IF

irv = AddSlice3D( fld%grid,fld%NextField%U,nAdd,AddStagger,.TRUE. )
IF( irv /= SWIMsuccess )THEN
  error%Message  = TRIM(error%Message)//'NextField U'; GOTO 9999
END IF

irv = AddSlice3D( fld%grid,fld%Field%V,nAdd,AddStagger,.FALSE. )
IF( irv /= SWIMsuccess )THEN
  error%Message  = TRIM(error%Message)//'Field V'; GOTO 9999
END IF

irv = AddSlice3D( fld%grid,fld%NextField%V,nAdd,AddStagger,.TRUE. )
IF( irv /= SWIMsuccess )THEN
  error%Message  = TRIM(error%Message)//'NextField V'; GOTO 9999
END IF

IF( BTEST(fld%type,FTB_W) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%w,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field W'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%W,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField W'; GOTO 9999
  END IF

END IF

!------ Velocity shear gradient field

IF( BTEST(fld%type,FTB_DU2) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%dU2,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field dU2'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%dU2,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField dU2'; GOTO 9999
  END IF

END IF

!------ Temperature, Humidity and Pressure, if available

IF( BTEST(fld%type,FTB_T) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%Tpot,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field Tpot'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%Tpot,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField Tpot'; GOTO 9999
  END IF

END IF

IF( BTEST(fld%type,FTB_H) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%Humid,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field Humid'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%Humid,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField Humid'; GOTO 9999
  END IF

END IF

IF( BTEST(fld%type,FTB_P) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%Press,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field Press'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%Press,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField Press'; GOTO 9999
  END IF

END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN

  irv = AddSlice3D( fld%grid,fld%Field%Qcloud,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'Field Qcloud'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextField%Qcloud,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextField Qcloud'; GOTO 9999
  END IF

END IF

!------ Large-scale variability, if available

IF( BTEST(fld%type,FTB_LSV) )THEN

  irv = AddSlice3D( fld%grid,fld%LSV%UU,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'LSV UU'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextLSV%UU,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextLSV UU'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%LSV%VV,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'LSV VV'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextLSV%VV,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextLSV VV'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%LSV%UV,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'LSV UV'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextLSV%UV,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextLSV UV'; GOTO 9999
  END IF

  IF( BTEST(fld%type,FTB_LSVL) )THEN
    irv = AddSlice3D( fld%grid,fld%LSV%SL,nAdd,AddStagger,.FALSE. )
    IF( irv /= SWIMsuccess )THEN
      error%Message  = TRIM(error%Message)//'LSV SL'; GOTO 9999
    END IF
    irv = AddSlice3D( fld%grid,fld%NextLSV%SL,nAdd,AddStagger,.TRUE. )
    IF( irv /= SWIMsuccess )THEN
      error%Message  = TRIM(error%Message)//'NextLSV SL'; GOTO 9999
    END IF
  END IF

END IF

!------ Boundary Layer Profiles (not currently available with MEDOC)

IF( BTEST(fld%type,FTB_UU) )THEN

  irv = AddSlice3D( fld%grid,fld%BLprof%UU,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof UU'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%UU,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof UU'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%BLprof%VV,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof VV'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%VV,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof VV'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%BLprof%WW,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof WW'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%WW,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof WW'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%BLprof%WT,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof WT'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%WT,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof WT'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%BLprof%SL,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof SL'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%SL,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof SL'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%BLprof%SZ,nAdd,AddStagger,.FALSE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'BLprof SZ'; GOTO 9999
  END IF

  irv = AddSlice3D( fld%grid,fld%NextBLprof%SZ,nAdd,AddStagger,.TRUE. )
  IF( irv /= SWIMsuccess )THEN
    error%Message  = TRIM(error%Message)//'NextBLprof SZ'; GOTO 9999
  END IF

END IF

MEDOCexpand = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION AddSlice3D( grid,var3d,nAdd,AddStagger,lFill )

!------ Add levels to 3d field var3d; fill with values from second level
!       N.B. Only used when changing from un- to staggered grids with MEDOC files
!       without W but with terrain
!       If nAdd > 0, copy top level nAdd times

USE SWIM_fi
USE SWIMparam_fd
USE reallocate

IMPLICIT NONE

TYPE( MetGrid ),    INTENT( IN ) :: grid
REAL, DIMENSION(:), POINTER      :: var3d
INTEGER,            INTENT( IN ) :: nAdd
LOGICAL,            INTENT( IN ) :: AddStagger
LOGICAL,            INTENT( IN ) :: lFill

INTEGER irv, nxy, i, k, nTop, i0, k0

AddSlice3D  = SWIMsuccess

nxy = grid%nXY

k0 = nxy*nAdd
IF( AddStagger )k0 = k0 + nxy

irv = reallocate_real1d( var3d,k0 )
IF( irv /= 0 )THEN
  error%Number   = UK_ERROR
  error%Routine  = 'AddSlice3D'
  error%Message  = 'Error re-allocating '
  AddSlice3D     = SWIMfailure
  GOTO 9999
END IF

IF( .NOT.lFill )GOTO 9999

ntop = grid%nZ - nAdd

IF( AddStagger )THEN

  DO k = ntop,1,-1
    i0 = k*nxy
    DO i = 1,nxy
      var3d(i0+i) = var3d(i0+i-nxy)
    END DO
  END DO

  DO i = 1,nxy
    var3d(i) = var3d(i+nxy)
  END DO

END IF

IF( nAdd > 0 )THEN

  k0 = nTop*nxy
  DO k = nTop+1,grid%nZ
    i0 = k*nxy
    DO i = 1,nxy
      var3d(i0+i) = var3d(k0+i)
    END DO
  END DO

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetMEDOCterrain( fld )

!------ Separate read for MEDOC terrain
!       N.B. Open temporary unit since lun_met is positioned for SWIMreadMEDOC

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
!USE SWIMutilArrayPtr
!USE charT_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, lun, idum, nr, iv, i, k, ip, nz
INTEGER imax, jmax, kmax, ndum, nvar3d, nvar2d
REAL    a1, a2, r, ps, pt, zs, riso, aiso, ziso, fac
LOGICAL lformat

CHARACTER(128) string

TYPE( GridSrc ) :: tmpSrc

INTEGER, EXTERNAL :: MEDOCopenFile, MEDOCreadModelHeader, MEDOCdimensionRecord, MEDOC2dField
INTEGER, EXTERNAL :: PostProgressMessage, SkipLine, SWIMaddLogMessage, SetTerrainGrad
INTEGER, EXTERNAL :: getFieldIndex
REAL,    EXTERNAL :: SWIMsetHmin

SWIMsetMEDOCterrain = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'SWIMsetMEDOCterrain'
error%Message = 'Error reading MEDOC file'

!------ Setup messaging

message%bString = 'Reading MEDOC terrain'

irv = PostProgressMessage( message )

!------ Define locals

lun     = getFieldIndex( fld%gridSource%unit )
lformat = .NOT.BTEST(fld%gridSource%type,GSB_BINARY)

REWIND( lun,IOSTAT=irv )

CALL nullifyGridSrc( tmpSrc )
tmpSrc%type = IZERO8

!------ Skip model header (if it exists)

irv = MEDOCreadModelHeader( lun,lformat,tmpSrc,idum )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read file type record

!irv = MEDOCformatRecord( lun,lformat,lend )
irv = SkipLine( lun,lformat,1 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read code name and staggered grid flag

!irv = MEDOCcodenameRecord( lun,lformat,codename,lstagger,NestSrc )
irv = SkipLine( lun,lformat,1 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read date and time

!irv = MEDOCtimeRecord( lun,lformat,Src,tMEDOC )
irv = SkipLine( lun,lformat,1 )
IF( irv /= SWIMsuccess )GOTO 9999

!irv = MEDOCtimeRecord( lun,lformat,Src,tAnlys )  !Analysis time
irv = SkipLine( lun,lformat,1 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read grid dimensions and number of variables

irv = MEDOCdimensionRecord( lun,lformat,imax,jmax,kmax,ndum,nvar3d,nvar2d )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read vertical grid and horizontal grid spacing and origin
!       N.B. Skip unused lines before grid info

!irv = MEDOCgridRecord( lun,lformat,Src%Z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )
IF( lformat )THEN
  nr = 2 +  (fld%gridSource%nZ+11+5)/6          !(kmax+11+5)/6
ELSE
  nr = 3
END IF

irv = SkipLine( lun,lformat,nr )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read variable names and units; special location also
!       i.e., names and units for 2d & 3d fields

!irv = MEDOCvarnameRecord( lun,lformat,Var3d,Var2d,ndum )
IF( lformat )THEN
  nr = (ndum+2*fld%gridSource%nVar3d+2*fld%gridSource%nVar2d+5)/6
  nr = nr + (3*ndum+5)/6
ELSE
  IF( ndum > 0 )THEN
    nr = 2
  ELSE
    nr = 1
  END IF
END IF

irv = SkipLine( lun,lformat,nr )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Skip 3D FIELDS

IF( lformat )THEN
  nr = (fld%gridSource%nX*fld%gridSource%nY*fld%gridSource%nZ+5)/6
  nr = fld%gridSource%nVar3d * nr
ELSE
  nr =  fld%gridSource%nVar3d
END IF

irv = SkipLine( lun,lformat,nr )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read all 2D field at first timebreak
!       N.B. Set ID array to read only time-independent fields

ALLOCATE( tmpSrc%Var2dID(fld%gridSource%nVar2d),STAT=irv )
IF( irv /= 0 )GOTO 9999

nr = 0
DO iv = 1,fld%gridSource%nVar2d
  SELECT CASE( fld%gridSource%Var2dID(iv) )
    CASE( GVP_TERRAIN,GVP_ZRUF,GVP_HCNP,GVP_ALPHA,GVP_ALBEDO,GVP_BOWEN,GVP_LANDUSE )
      tmpSrc%Var2dID(iv) = GVP_NONE                  !These fields won't need to be read in subsequent calls to MEDOC2dField
      nr = nr + 1
    CASE DEFAULT
      tmpSrc%Var2dID(iv) = fld%gridSource%Var2dID(iv)
      fld%gridSource%Var2dID(iv) = GVP_NONE          !Ignore these fields in this call to MEDOC2dField, e.g., time-varying
  END SELECT
END DO

IF( nr > 0 )THEN  !Only read if necessary. N.B. Arrays initialized in SetGridParam
  irv = MEDOC2dField( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

fld%gridSource%Var2dID = tmpSrc%Var2dID  !Copy back array ID's (but set to not read terrain, etc. again)

!------ Set Hmin

CALL SetHmin( fld%grid )

WRITE(string,'(A,ES12.4)',IOSTAT=irv) 'Minimum terrain elevation (MEDOC) =',fld%grid%hmin
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

Prj%Hmin = SWIMsetHmin()

!------ Setup vertical grid for sigma coordinates (based on MM5 reference profiles)
!       N.B. sigma height arrays are AGL

IF( BTEST(fld%gridSource%type,GSB_SIGMAF) )THEN

  a2 = -RGAS*fld%gridSource%lapseRef/(2.*G0)
  a1 = -RGAS*fld%gridSource%TsrfRef/G0
  pt = fld%gridSource%Ptop / fld%gridSource%P00 !Top pressure divided by nominal sea level pressure

  fld%grid%Ztop = -HUGE(0.)
  fld%grid%sigma%Ptop = pt
  fld%grid%sigma%P00  = fld%gridSource%P00
  fld%grid%Sigma%a1 = a1
  fld%grid%Sigma%a2 = a2

  IF( fld%gridSource%Tiso > 0. ) THEN
    aiso = RGAS*fld%gridSource%Tiso/G0
    riso = -(aiso + a1)/a2
    ziso = a2*riso*riso + a1*riso - fld%grid%Hmin
  ELSE
    riso = -HUGE(0.)
    aiso = -999.
  END IF
  fld%grid%Sigma%aiso = aiso

  nz = fld%grid%nZ

  DO i = 1,fld%grid%nXY

    zs = fld%grid%terrain%H(i)+fld%grid%Hmin
    r  = -(a1 + SQRT(a1*a1 + 4.*a2*zs))/ (2.*a2)
    ps = EXP(r)
    fld%grid%sigma%Psrf(i) = ps - pt  !This is p* (normalized by P00)

    DO k = 1,nz
      ip = (k-1)*fld%grid%nXY + i
      r  = LOG(fld%grid%Z(k)*fld%grid%sigma%Psrf(i) + pt)
      IF( r > riso )THEN
        fld%grid%sigma%Z(ip) = a2*r*r + a1*r - fld%grid%Hmin
      ELSE
        fld%grid%sigma%Z(ip) = ziso + aiso*(riso-r)
      END IF
      fld%grid%Ztop = MAX(fld%grid%sigma%Z(ip),fld%grid%Ztop)
      fld%grid%sigma%Z(ip) = fld%grid%sigma%Z(ip) - fld%grid%terrain%H(i)
    END DO

    IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
      fld%grid%sigma%Zw(i) = 0.
      DO k = 2,nz+1
        ip = (k-1)*fld%grid%nXY + i
        r  = LOG(fld%grid%Zw(k)*fld%grid%sigma%Psrf(i) + pt)
        IF( r > riso )THEN
          fld%grid%sigma%Zw(ip) = a2*r*r + a1*r - fld%grid%Hmin - fld%grid%terrain%H(i)
        ELSE
          fld%grid%sigma%Zw(ip) = ziso + aiso*(riso-r) - fld%grid%terrain%H(i)
        END IF
      END DO
    END IF

  END DO

  DO k = 1,nz
    fld%grid%Z(k) = 1. - fld%grid%Z(k)  !Redefine Z as sigma-complement for later convenience
  END DO

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    DO k = 1,nz+1
      fld%grid%Zw(k) = 1. - fld%grid%Zw(k)
    END DO
  END IF

!------- 3d time-varying height field

ELSE IF( ANY(BTEST(fld%gridSource%type,(/ GSB_ZMSL,GSB_ZAGL /))) )THEN

  fac = 1.0  !Setting Z and Zw deferred until 3d fields are read

!------ Otherwise, vertical coordinate is terrain-following height
!       N.B. SIGMAZM source requires no re-definition

ELSE IF( BTEST(fld%gridSource%type,GSB_SIGMAZM) )THEN

  fac = 1.0

ELSE !Default is Standard MEDOC

  fld%gridSource%type = IBSET(fld%gridSource%type,GSB_SIGMAZ)  !Make sure bit is set

!------ Redefine top of vertical grid
!       (MEDOC definition is relative to MSL; SCIPUFF is above Hmin)

  fld%grid%Ztop = fld%gridSource%Ztop - fld%grid%Hmin

!------ Scale (transformed) vertical grid consistent with new Ztop

  fac = 1. - fld%grid%Hmin / fld%gridSource%Ztop

  DO i = 1,fld%grid%nZ
    fld%grid%Z(i) = fld%grid%Z(i) * fac
  END DO

END IF

!------ Set error if conformal projections are used w/ terain but w/o vertical velocity

IF( .NOT.BTEST(fld%type,FTB_W) )THEN
  SELECT CASE( fld%grid%coord%type )
    CASE( I_LAMBERT,I_POLAR,I_RPOLAR,I_ROTLL,I_MERCATOR )
      error%Number  = IV_ERROR
      error%Message = 'Conformal projections w/ terrain require vertical velocity'
      GOTO 9999
  END SELECT
END IF

!------ Finish standard vertical grid and terrain gradients unless this will
!       be superseded by McWIF setup (because vertical velocity not present)

IF( BTEST(fld%type,FTB_W) )THEN

  IF( .NOT.BTEST(fld%grid%type,GTB_SIGMA) )THEN
    IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
      DO i = 1,SIZE(fld%grid%zw)
       fld%grid%Zw(i) = fld%grid%Zw(i) * fac
      END DO
    END IF
  END IF

  irv = SetTerrainGrad( fld%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

IF( ASSOCIATED(tmpSrc%Var2dID) )DEALLOCATE( tmpSrc%Var2dID,STAT=irv )

!------ Success

SWIMsetMEDOCterrain = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

REWIND( lun,IOSTAT=irv )

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipLine( unit,lformat,nr ) RESULT( irv )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: unit, nr
LOGICAL, INTENT( IN ) :: lformat

INTEGER i

DO i = 1,nr
  IF( lformat )THEN
    READ(unit,*,IOSTAT=irv,ERR=9999)
  ELSE
    READ(unit,IOSTAT=irv,ERR=9999)
  END IF
END DO

9999 CONTINUE

IF( irv == 0 )THEN
  irv = SWIMsuccess
ELSE
  irv = SWIMfailure
END IF

RETURN
END
