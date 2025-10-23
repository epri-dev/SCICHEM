INTEGER FUNCTION SWIMreadWRF( t,fld )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMutilArrayPtr
USE string_utils
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

REAL,             INTENT( IN    ) :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

CHARACTER(256) :: vname

CHARACTER(NF_MAX_NAME) varnam, string

INTEGER irv, alloc_stat, ifile, fid, id_var, ivtype, id_aux, iv
INTEGER i, j, k, j0, k0, is, nx, ny, nz, nxy, ks, nzp
INTEGER nxi, nyi, nxyi, ndims, natts, n_times, itx
INTEGER ishft, jshft, ks0, ix, iy, iz, it, ip
INTEGER inc, nend, i1, i2, iSeason, jul
REAL    tWRF, T0, zs, fac
LOGICAL lend
REAL    xtem, ytem, lat, lon


INTEGER(C_INTPTR_T) k_len
INTEGER(C_INTPTR_T), DIMENSION(:), ALLOCATABLE :: istart, icount

INTEGER, DIMENSION(:), ALLOCATABLE :: dimids

REAL,    DIMENSION(:,:,:,:), ALLOCATABLE :: wrk3d, aux3d
REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: wrk2d, aux2d
REAL,    DIMENSION(:),       POINTER     :: pwrk, pfld

TYPE( MapCoord ) :: coord

INTEGER, EXTERNAL :: SWIMaddLogMessage, PostProgressMessage
INTEGER, EXTERNAL :: ConvertRelHumid, ComputeBuoyFlux, AddAuxPrecip, SWIMsetPrecipProb
INTEGER, EXTERNAL :: ConvertCloudWater
REAL,    EXTERNAL :: GetWRFTime
LOGICAL, EXTERNAL :: CheckNextFile

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull, StripNull

INTERFACE

  INTEGER FUNCTION Read3dVarGridded( src,nx,ny,nz,var,ks0,ishft,jshft,var_src )
    USE SWIM_fi
    TYPE( GridSrc ),              INTENT( IN ) :: src
    INTEGER,                      INTENT( IN ) :: nx, ny, nz
    REAL, DIMENSION(:),           POINTER      :: var
    INTEGER,                      INTENT( IN ) :: ks0
    INTEGER,            OPTIONAL, INTENT( IN ) :: ishft
    INTEGER,            OPTIONAL, INTENT( IN ) :: jshft
    REAL, DIMENSION(:), OPTIONAL, POINTER      :: var_src
  END FUNCTION Read3dVarGridded

  SUBROUTINE Copy2dVar( src,grid,wrk,var )
    USE SWIM_fi
    TYPE( GridSrc ), INTENT( IN ) :: src
    TYPE( MetGrid ), INTENT( IN ) :: grid
    REAL, DIMENSION(:), POINTER   :: wrk
    REAL, DIMENSION(:), POINTER   :: var
  END SUBROUTINE Copy2dVar

  SUBROUTINE AdjustCanopyPrf( fld,is )
    USE SWIM_fi
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fld
    INTEGER,                  INTENT( IN    ) :: is
  END SUBROUTINE AdjustCanopyPrf

  REAL FUNCTION get_real1( fid,var_name,is ) RESULT( x )
    INTEGER,           INTENT( IN ) :: fid
    CHARACTER*(*),     INTENT( IN ) :: var_name
    INTEGER, OPTIONAL, INTENT( IN ) :: is
  END FUNCTION get_real1

END INTERFACE

INTEGER, EXTERNAL :: GetWRFnTimes, SWIMlimit
INTEGER, EXTERNAL :: JulianPrj, GetWRFSeason, SWIMcnvCoord

!------ Initialize failure

SWIMreadWRF = SWIMfailure

!------ Define locals

ifile = fld%gridSource%unit
nxi   = fld%gridSource%nX
nyi   = fld%gridSource%nY
nxyi  = nxi*nyi

nxy = fld%grid%nXY
nx  = fld%grid%nX
ny  = fld%grid%nY
nz  = fld%grid%nZ

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  ks0 = nxy
ELSE
  ks0 = 0
END IF

NULLIFY( pwrk )

!------ Initialize error structure

error%Number  = RD_ERROR
error%Routine = 'SWIMreadWRF'
error%Message = 'Error reading WRF file'
CALL ReportFileName( error%Inform,'File=',fld%gridSource%Source(ifile) )

!------ Open current file and check times

irv = nc_open( AddNull(fld%gridSource%source(ifile)),NF_NOWRITE,fid )
IF( irv /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening WRF file'
  error%Action = StringCToF( nc_strerror( irv ) )
  GOTO 9999
END IF

n_times = GetWRFnTimes( fid ); IF( n_times < 1 )GOTO 9999

IF( BTEST(fld%status,FSB_DOINIT) )THEN  !Always read from first file

  IF( Reverse )THEN
    inc = -1; nend = 1;
  ELSE
    inc =  1; nend = n_times
  END IF
  itx = n_times - nend + 1 - inc
  DO
    itx = itx + inc
    tWRF = GetWRFTime( fld%gridSource%Source(ifile),fid,fld%gridSource%tFcst,itx )
    IF( tWRF > t )THEN
      itx = SWIMlimit( itx-inc,1,n_times )
      EXIT
    END IF
    IF( itx == nend )EXIT
  END DO

  tWRF = GetWRFTime( fld%gridSource%Source(ifile),fid,fld%gridSource%tFcst,itx )

ELSE

  IF( Reverse )THEN
    inc = -1; i1 = n_times; i2 = 1; nend = -1
  ELSE
    inc =  1; i1 = 1; i2 = n_times; nend = fld%gridSource%nSource
  END IF

  DO WHILE( GetWRFTime(fld%gridSource%Source(ifile),fid,fld%gridSource%tFcst,i2) <= t )

    irv = nc_close( fid )

    lend = CheckNextFile( ifile,nend )

    IF( lend )THEN
      fld%status = IBCLR(fld%status,FSB_UPDATE)        !Clear update status & exit
      SWIMreadWRF = SWIMresult
      GOTO 9999
    END IF

!------ Open next file

    irv = nc_open( AddNull(fld%gridSource%source(ifile)),NF_NOWRITE,fid )
    IF( irv /= 0 )THEN
      error%Number  = OP_ERROR
      error%Message = 'Error opening WRF file'
      error%Action = StringCToF( nc_strerror( irv ) )
      GOTO 9999
    END IF

    n_times = GetWRFnTimes( fid ); IF( n_times < 1 )GOTO 9999
    fld%gridSource%unit = ifile  !Save for next time

    i2 = n_times

  END DO

!------ Find appropriate timebreak

  DO itx = i1,i2,inc
    tWRF = GetWRFTime( fld%gridSource%Source(ifile),fid,fld%gridSource%tFcst,itx )
    IF( tWRF > t )EXIT
  END DO

END IF

!------ Setup messaging

message%cString  = 'Reading WRF file'

irv = SWIMaddLogMessage( 'Reading met file '//TRIM(fld%gridSource%Source(ifile)) )
IF( irv /= SWIMsuccess )GOTO 9999

WRITE(string,"('t =',1PG11.4,'hrs.')",IOSTAT=irv) tWRF/3600.
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Allocate arrays for setting dimensions

ALLOCATE( istart(4),icount(4),dimids(4),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array to read 3d WRF fields'
  GOTO 9999
END IF

!------ Loop over 3d fields

DO iv = 1,fld%gridSource%nVar3d

  vname = TRIM(fld%gridSource%Var3dName(iv));  error%Action = 'Field= '//vname


  irv = nc_inq_varid( fid,AddNull(vname),id_var ); IF( irv /= 0 )GOTO 9999
  irv = nc_inq_var( fid,id_var,varnam,ivtype,ndims,dimids,natts )

  IF( irv /= 0 )GOTO 9999

  IF( ndims /= 4 )THEN
    error%Message = 'Invalid number of dimensions. Must be 4'
    GOTO 9999
  END IF

  istart = 0

!------- Check dimensions (for staggered grids)

  DO j = 1,ndims

    irv = nc_inq_dim( fid,dimids(j),varnam,k_len );
    IF( irv /= 0 )THEN
      error%Message = StringCToF( nc_strerror( irv ) )
      GOTO 9999
    END IF

    SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )
      CASE( 'Time' )
        icount(j) = 1;  it = j
        istart(j) = itx-1
      CASE( 'west_east' )
        icount(j) = nxi; ix = j
      CASE( 'west_east_stag' )
        icount(j) = nxi; ix = j
        istart(j) = 1
      CASE( 'south_north' )
        icount(j) = nyi; iy = j
      CASE( 'south_north_stag' )
        icount(j) = nyi; iy = j
        istart(j) = 1
      CASE( 'bottom_top' )
        icount(j) = nz; iz = j; ks = ks0; nzp = nz
      CASE( 'bottom_top_stag' )
        icount(j) = nz+1; iz = j ; ks = 0; nzp = nz+1 !this is W & Z
        istart(j) = 0
    END SELECT

  END DO

!------ Setup 3d work arrays and 3d pointer for use in Read3dVarGridded and Copy2dVar
!       N.B. dimensions may vary for each field

  ALLOCATE( pwrk(nxyi*nzp), &
            wrk3d(icount(ix),icount(iy),icount(iz),icount(it)), &
            aux3d(icount(ix),icount(iy),icount(iz),icount(it)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Message = 'Error allocating arrays to read 3d WRF fields'
    GOTO 9999
  END IF


  irv = nc_get_vara_float( fid,id_var,istart,icount,wrk3d )
  IF( irv /= 0 )THEN
    error%Message = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  SELECT CASE( fld%gridSource%Var3dID(iv) )
    CASE( GVP_U )
      pfld => fld%NextField%U

    CASE( GVP_V )
      pfld => fld%NextField%V

    CASE( GVP_W )
      pfld => fld%NextField%W

    CASE( GVP_T )
      pfld => fld%NextField%Tpot

    CASE( GVP_P )
      pfld => fld%NextField%Press

      irv = nc_inq_varid( fid,AddNull('PB'),id_aux ); IF( irv /= 0 )GOTO 9999

      irv = nc_get_vara_float( fid,id_aux,istart,icount,aux3d )
      IF( irv /= 0 )THEN
        error%Message = StringCToF( nc_strerror( irv ) )
        GOTO 9999
      END IF

      CALL AddAux3d( nxi,nyi,nz,wrk3d,aux3d,1.E-2 )

    CASE( GVP_Z )
      pfld => fld%NextField%Z

      irv = nc_inq_varid( fid,AddNull('PHB'),id_aux ); IF( irv /= 0 )GOTO 9999

      irv = nc_get_vara_float( fid,id_aux,istart,icount,aux3d )
      IF( irv /= 0 )THEN
        error%Message = StringCToF( nc_strerror( irv ) )
        GOTO 9999
      END IF

      CALL AddAux3d( nxi,nyi,nzp,wrk3d,aux3d,1./G0 )

    CASE( GVP_H )
      pfld => fld%NextField%Humid

    CASE( GVP_QCLD )
      pfld => fld%NextField%Qcloud

  END SELECT

!------ Copy into 1d array

  DO k = 1,nzp
    k0 = (k-1)*nxyi
    DO j = 1,nyi
      j0 = (j-1)*nxi
      DO i = 1,nxi
        ip = k0 + j0 + i
        pwrk(ip) = wrk3d(i,j,k,1) !idx(4),idx(3),idx(2),idx(1))
      END DO
    END DO
  END DO

  IF( ASSOCIATED(fld%gridSource%Var3dShift) )THEN
    ishft = fld%gridSource%Var3dShift(1,iv)
    jshft = fld%gridSource%Var3dShift(2,iv)
  ELSE
    ishft = 0
    jshft = 0
  END IF

  irv = Read3dVarGridded( fld%gridSource,nx,ny,nzp,pfld,ks,ishft,jshft,pwrk )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Bottom bc (assume vertically staggered)

  IF( fld%gridSource%Var3dID(iv) == GVP_W )THEN
    k = nz*nxy                                         !Top BC
    DO i = 1,nxy
      fld%NextField%W(k+i) = 0.
    END DO
  ELSE IF( fld%gridSource%Var3dID(iv) == GVP_Z )THEN
    DO is = 1,nxy
      pfld(is) = fld%grid%terrain%H(is)+fld%grid%Hmin  !Set bottom slice explicitly
    END DO
  ELSE
    DO is = 1,nxy
      pfld(is) = pfld(is+nxy)
    END DO
  END IF

!------ Scale fields if appropriate

  IF( fld%gridSource%Conv3d(iv) > 0. )CALL ScaleArray( fld%gridSource%Conv3d(iv),pfld,SIZE(pfld) )

!------ Deallocate work arrays

  IF( ASSOCIATED(pwrk)  )DEALLOCATE(pwrk, STAT=alloc_stat)
  IF( ALLOCATED(wrk3d)  )DEALLOCATE(wrk3d,STAT=alloc_stat)
  IF( ALLOCATED(aux3d)  )DEALLOCATE(aux3d,STAT=alloc_stat)

END DO

!------ Extra processing for certain 3d fields:
!
!------ Convert pressure to log(P/PSURF); add reference temperature

T0 = 300. !get_real1( fid,AddNull('T00') )

!$OMP PARALLEL SHARED(fld,T0)
!$OMP DO PRIVATE(i)
DO i = 1,fld%grid%nXY*(fld%grid%nZ+1)
  fld%NextField%Press(i) = LOG(fld%NextField%Press(i)/PSURF)
  fld%NextField%Tpot(i)  = fld%NextField%Tpot(i) + T0
END DO
!$OMP END DO
!$OMP END PARALLEL

!------ 3D height: subtract terrain; average to cell center

!$OMP PARALLEL SHARED(nxy,fld)
!$OMP DO PRIVATE(is,ip,k,k0)
DO is = 1,nxy
  DO k = fld%grid%nZ+1,2,-1
    k0 = (k-1)*nxy
    ip = k0 + is
    fld%NextField%Z(ip) = 0.5*(fld%NextField%Z(ip)+fld%NextField%Z(ip-nxy))
    fld%NextField%Z(ip) = fld%NextField%Z(ip) - (fld%grid%terrain%H(is)+fld%grid%Hmin)
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL
DO is = 1,nxy
  fld%NextField%Z(is) = -fld%NextField%Z(is+nxy)
END DO

!------ Scale potential temperature to PSURF

IF( .NOT.BTEST(fld%gridSource%type,GSB_ABSTEMP) )THEN
  fac = (PSURF/1000.)**KAPPA
  DO is = 1,SIZE(fld%NextField%Tpot)
    fld%NextField%Tpot(is) = fld%NextField%Tpot(is) *  fac
  END DO
END IF

!------ Convert to relative humidity

IF( BTEST(fld%type,FTB_H) )THEN
  irv = ConvertRelHumid( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Convert cloud water to g/m^3

IF( BTEST(fld%type,FTB_QCLD) )THEN
  irv = ConvertCloudWater( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Set grid%Z array so that it's useful for output and plotting
!       N.B. Only do upon initialization

IF( BTEST(fld%status,FSB_DOINIT) )THEN

  zs = HUGE(0.)
  DO is = 1,nxy
    IF( fld%grid%terrain%h(is) < zs )THEN
      zs = fld%grid%terrain%h(is)
      iv = is
    END IF
  END DO

  fac = fld%NextField%Z((nz-1)*nxy+iv)
  fac = (fac + zs)/fac

  DO k = 1,nz
    i = (k-1)*fld%grid%nxy + nxy + iv
    fld%grid%Z(k) = fac*fld%NextField%Z(i)
  END DO

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    fld%grid%Zw(1) = 0.
    DO k = 2,nz
      fld%grid%Zw(k) = 0.5*(fld%grid%Z(k)+fld%grid%Z(k-1))
    END DO
    fld%grid%Zw(nz+1) = 2.*fld%grid%Zw(nz) - fld%grid%Zw(nz-1)
    fld%grid%Ztop = fld%grid%Zw(nz)
  ELSE
    fld%grid%Ztop = fld%grid%Z(nz)
  END IF

END IF

!------ Allocate local 2d work space

DEALLOCATE(pwrk,STAT=alloc_stat)

IF( ALLOCATED(wrk2d)  )DEALLOCATE(wrk2d, STAT=alloc_stat)
IF( ALLOCATED(aux2d)  )DEALLOCATE(aux2d, STAT=alloc_stat)
IF( ALLOCATED(istart) )DEALLOCATE(istart,STAT=alloc_stat)
IF( ALLOCATED(icount) )DEALLOCATE(icount,STAT=alloc_stat)
IF( ALLOCATED(dimids) )DEALLOCATE(dimids,STAT=alloc_stat)

ALLOCATE( pwrk(nxyi),istart(3),icount(3),dimids(3),STAT=alloc_stat )  !Allocate local array
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array to read 2d WRF fields'
  GOTO 9999
END IF

!------ Read 2d fields

DO iv = 1,fld%gridSource%nVar2d

  SELECT CASE( fld%gridSource%Var2dID(iv) ) !Skip fixed or "derived" 2d fields
    CASE( GVP_TERRAIN,GVP_ZRUF,GVP_ALPHA,GVP_ALBEDO,GVP_LANDUSE )
      CYCLE
  END SELECT

  vname = TRIM(fld%gridSource%Var2dName(iv)); error%Action = 'Field= '//vname

  irv = nc_inq_varid( fid,AddNull(vname),id_var ); IF( irv /= 0 )GOTO 9999

  irv = nc_inq_var( fid,id_var,varnam,ivtype,ndims,dimids,natts )
  IF( irv /= 0 )GOTO 9999

  IF( ndims /= 3 )THEN
    error%Message = 'Invalid number of dimensions. Must be 3'
    GOTO 9999
  END IF

  istart = 0

!------- Check dimensions

  DO j = 1,ndims

    irv = nc_inq_dim( fid,dimids(j),varnam,k_len );
    IF( irv /= 0 )THEN
      error%Message = StringCToF( nc_strerror( irv ) )
      GOTO 9999
    END IF

    SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )
      CASE( 'Time' )
        icount(j) = 1;  it = j
        istart(j) = itx-1
      CASE( 'west_east' )
        icount(j) = nxi; ix = j
      CASE( 'south_north' )
        icount(j) = nyi; iy = j
    END SELECT

  END DO

!------ Setup work arrays (assume all 2d fields have same dimensions and sizes)

  IF( .NOT.ALLOCATED(wrk2d) )THEN

    ALLOCATE( wrk2d(icount(ix),icount(iy),icount(it)), &
              aux2d(icount(ix),icount(iy),icount(it)),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Message = 'Error allocating arrays to read 2d WRF fields'
      GOTO 9999
    END IF

  END IF


  irv = nc_get_vara_float( fid,id_var,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Message = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  SELECT CASE( fld%gridSource%Var2dID(iv) )
    CASE( GVP_HFLX )
      pfld => fld%NextBL%HeatFlux

      IF( fld%gridSource%lBuoy )THEN
        irv = ComputeBuoyFlux( fid,ix,iy,it,istart,icount,wrk2d ) !Read extra fields
        IF( irv /= SWIMsuccess )GOTO 9999
      END IF

    CASE( GVP_ACCPR )
      pfld => fld%NextBL%prcp
      IF( fld%gridSource%lPrecipAux )THEN
        irv = AddAuxPrecip( fid,ix,iy,it,istart,icount,wrk2d )    !Read and add other precipitation fields
        IF( irv /= SWIMsuccess )GOTO 9999
      END IF

    CASE( GVP_ZRUFT )
      pfld => fld%NextBL%zruf

      IF( vname(1:8) == 'LU_INDEX' )THEN
        jul = JulianPrj( fld%t )
        xtem = 0.5*(fld%grid%Xmin+fld%grid%Xmax); ytem = 0.5*(fld%grid%Ymin+fld%grid%Ymax)
        coord%type = I_LATLON
        irv = SWIMcnvCoord( xtem,ytem,fld%grid%coord,lon,lat,coord )
        iSeason = GetWRFSeason( lat,jul )
        CALL WRFcat2zruf( fld%gridSource%LUmodel,iSeason,nxi,nyi,wrk2d )
      END IF

    CASE( GVP_ZI )
      pfld => fld%NextBL%Zi

    CASE( GVP_UST )
      pfld => fld%NextBL%Ustr

    CASE( GVP_LAI )
      pfld => fld%NextBL%LAI

  END SELECT

!------ Copy into 1d array

  DO j = 1,nyi
    j0 = (j-1)*nxi
    DO i = 1,nxi
      is = j0 + i
      pwrk(is) = wrk2d(i,j,1)
    END DO
  END DO

!------ Account for offsets and skipping

  CALL Copy2dVar( fld%gridSource,fld%grid,pwrk,pfld )

END DO

!------ Convert heat flux to C-m/s

IF( BTEST(fld%type,FTB_HFLX) )CALL ConvertHeatFlux( fld )

!------ Fix zero PBL height

IF( BTEST(fld%type,FTB_ZI) )CALL FixZeroZi( fld )

!------ Set rain probability

IF( ASSOCIATED(fld%BL%prcp) )THEN
  IF( ASSOCIATED(fld%BL%rainprb) )THEN
    fld%grid%sigma%Z => fld%NextField%Z(fld%grid%nXY+1:)
    irv = SWIMsetPrecipProb( fld )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

!------ Close file

IF( fid > 0 )THEN
  irv = nc_close( fid )
  IF( irv /= 0 )THEN
    error%Message = 'Error closing WRF file'
    error%Action = StringCToF( nc_strerror( irv ) )
  END IF
END IF

!------ Set time

fld%tNext = tWRF

!------ SWIMming success

CALL SWIMclearError()

SWIMreadWRF = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(pwrk)  )DEALLOCATE(wrk3d, STAT=alloc_stat)
IF( ALLOCATED(wrk3d)  )DEALLOCATE(wrk3d, STAT=alloc_stat)
IF( ALLOCATED(aux3d)  )DEALLOCATE(aux3d, STAT=alloc_stat)
IF( ALLOCATED(wrk2d)  )DEALLOCATE(wrk2d, STAT=alloc_stat)
IF( ALLOCATED(aux2d)  )DEALLOCATE(aux2d, STAT=alloc_stat)
IF( ALLOCATED(istart) )DEALLOCATE(istart,STAT=alloc_stat )
IF( ALLOCATED(icount) )DEALLOCATE(icount,STAT=alloc_stat )
IF( ALLOCATED(dimids) )DEALLOCATE(dimids,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE AddAux3d( nx,ny,nz,wrk3d,aux3d,fac )

IMPLICIT NONE

INTEGER,                      INTENT( IN    ) :: nx, ny, nz
REAL, DIMENSION( nx,ny,nz,1), INTENT( INOUT ) :: wrk3d
REAL, DIMENSION( nx,ny,nz,1), INTENT( IN    ) :: aux3d
REAL,                         INTENT( IN    ) :: fac

INTEGER i, j, k

DO k = 1,nz
  DO j = 1,ny
    DO i = 1,nx
      wrk3d(i,j,k,1) = (wrk3d(i,j,k,1) + aux3d(i,j,k,1)) * fac
    END DO
  END DO
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION ComputeBuoyFlux( fid,ix,iy,it,istart,icount,Hflx )

!------ Compute buoyancy flux.
!       Hflx is sensible heat flux as input and buoyancy flux as ouptut

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE string_utils
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

INTEGER,                                   INTENT( IN    ) :: fid
INTEGER,                                   INTENT( IN    ) :: ix, iy, it
INTEGER(C_INTPTR_T), DIMENSION(3),        INTENT( IN    ) :: istart, icount
REAL, DIMENSION( icount(ix),icount(iy),*), INTENT( INOUT ) :: Hflx

INTEGER alloc_stat, irv, id
INTEGER nxi, nyi, i, j
REAL    eps

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull

REAL, DIMENSION( :,:,:), ALLOCATABLE :: wrk2d, aux2d

ComputeBuoyFlux = SWIMfailure

!------ Allocate work arrays

ALLOCATE( wrk2d(icount(ix),icount(iy),icount(it)), &
          aux2d(icount(ix),icount(iy),icount(it)),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Number  = UK_ERROR
  error%Message = 'Error allocating arrays to compute buoyancy flux'
  GOTO 9999
END IF

nxi = icount(ix)
nyi = icount(iy)

!------ Humidity mixing ratio at 2m

irv = nc_inq_varid( fid,AddNull('Q2'),id )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading Q2'
  GOTO 9999
END IF

irv = nc_get_vara_float( fid,id,istart,icount,aux2d )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading Q2'
  GOTO 9999
END IF

!------ (1+eps*Q) * Hflux

eps = 1./MR - 1.

DO j = 1,nyi
  DO i = 1,nxi
    Hflx(i,j,1) = Hflx(i,j,1)*(1. + eps*aux2d(i,j,1))
  END DO
END DO

!------ Potential temperature at 2m

irv = nc_inq_varid( fid,AddNull('TH2'),id )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading TH2'
  GOTO 9999
END IF

irv = nc_get_vara_float( fid,id,istart,icount,wrk2d )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading TH2'
  GOTO 9999
END IF

!------ eps*Cp' * Theta  (Cp modified for moisture by factor 1+0.8*Q)

DO j = 1,nyi
  DO i = 1,nxi
    wrk2d(i,j,1) = eps*CP*(1.+0.8*aux2d(i,j,1))*wrk2d(i,j,1)
  END DO
END DO

!------ Vapor flux

irv = nc_inq_varid( fid,AddNull('QFX'),id )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading QFX'
  GOTO 9999
END IF

irv = nc_get_vara_float( fid,id,istart,icount,aux2d )
IF( irv /= 0 )THEN
  error%Routine = 'ComputeBuoyFlux'
  error%Message = StringCToF( nc_strerror( irv ) )
  error%Inform = 'Reading QFX'
  GOTO 9999
END IF

!------ (1+eps*Q) * Hflux + eps*Cp' * Theta * Qflux

DO j = 1,nyi
  DO i = 1,nxi
    Hflx(i,j,1) = Hflx(i,j,1) + wrk2d(i,j,1)*aux2d(i,j,1)
  END DO
END DO

CALL SWIMclearError()

ComputeBuoyFlux = SWIMresult

9999 CONTINUE

IF( ALLOCATED(wrk2d) )DEALLOCATE( wrk2d,STAT=alloc_stat )
IF( ALLOCATED(aux2d) )DEALLOCATE( aux2d,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION AddAuxPrecip( fid,ix,iy,it,istart,icount,precip )

!------ Add shallow and non-convective precipitation (if present)
!       precip is convective amount as input and total as ouptut

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE string_utils

IMPLICIT NONE

INTEGER,                                   INTENT( IN    ) :: fid
INTEGER,                                   INTENT( IN    ) :: ix, iy, it
INTEGER(LEN_ADDRESS), DIMENSION(3),        INTENT( IN    ) :: istart, icount
REAL, DIMENSION( icount(ix),icount(iy),*), INTENT( INOUT ) :: precip  !Accumulated total precipitation

INTEGER alloc_stat, irv, id
INTEGER nxi, nyi, i, j

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull

REAL, DIMENSION( :,:,:), ALLOCATABLE :: wrk2d

AddAuxPrecip = SWIMfailure

!------ Allocate work arrays

ALLOCATE( wrk2d(icount(ix),icount(iy),icount(it)),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Routine = 'AddAuxPrecip'
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array to read extra precipitation fields'
  GOTO 9999
END IF

nxi = icount(ix)
nyi = icount(iy)

!------ Shallow precipitation

irv = nc_inq_varid( fid,AddNull('RAINSH'),id )
IF( irv == 0 )THEN

  irv = nc_get_vara_float( fid,id,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Routine = 'AddAuxPrecip'
    error%Message = StringCToF( nc_strerror( irv ) )
    error%Inform = 'Reading RAINSH'
    GOTO 9999
  END IF

  DO j = 1,nyi
    DO i = 1,nxi
      precip(i,j,1) = precip(i,j,1) + wrk2d(i,j,1)
    END DO
  END DO

END IF

!------ Non-convective precipitation

irv = nc_inq_varid( fid,AddNull('RAINNC'),id )
IF( irv == 0 )THEN

  irv = nc_get_vara_float( fid,id,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Routine = 'AddAuxPrecip'
    error%Message = StringCToF( nc_strerror( irv ) )
    error%Inform = 'Reading RAINNC'
    GOTO 9999
  END IF

  DO j = 1,nyi
    DO i = 1,nxi
      precip(i,j,1) = precip(i,j,1) + wrk2d(i,j,1)
    END DO
  END DO

END IF

CALL SWIMclearError()

AddAuxPrecip = SWIMresult

9999 CONTINUE

IF( ALLOCATED(wrk2d) )DEALLOCATE( wrk2d,STAT=alloc_stat )

RETURN
END
