INTEGER FUNCTION SWIMupdateGriddedMet( t,fld )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMintrpGrid_fi
USE SWIMutilArrayPtr

IMPLICIT NONE

REAL,             INTENT( IN    ) :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv

INTERFACE

  SUBROUTINE SetPeriodicBC( type,grd,MeanFld,VarFld,BLFld )
    USE SWIM_fi
    INTEGER(KIND=8),               INTENT( IN    ) :: type
    TYPE( MetGrid     ),           INTENT( IN    ) :: grd
    TYPE( MetMean3D   ), OPTIONAL, INTENT( INOUT ) :: MeanFld
    TYPE( MetVariance ), OPTIONAL, INTENT( INOUT ) :: VarFld
    TYPE( MetBLparam  ), OPTIONAL, INTENT( INOUT ) :: BLFld
  END SUBROUTINE SetPeriodicBC

END INTERFACE

INTEGER, EXTERNAL :: SWIMreadMEDOC, SWIMreadSCIP
INTEGER, EXTERNAL :: SWIMreadWRF

SWIMupdateGriddedMet = SWIMfailure

IF( BTEST(fld%gridSource%type,GSB_MEDOC) )THEN

  irv = SWIMreadMEDOC( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%gridSource%type,GSB_SCIP) )THEN

  irv = SWIMreadSCIP( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%gridSource%type,GSB_WRF) )THEN

  irv = SWIMreadWRF( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Interpolate [gridSource] 'read field' onto 'next field', if necessary
!       N.B. Only relevant for (deprecated) SCIP gridded format or stand-alone UWM

IF( BTEST(fld%type,FTB_INTRP) .AND. BTEST(fld%status,FSB_UPDATE) )THEN

  CALL SWIMinterpGridded( fieldType = fld%type,                 &
                          BLType    = fld%BLtype,               &
                          time      = fld%tNext,                &
                          grid      = fld%grid,                 &
                          IntrpFac  = fld%gridSource%IntrpFac,  &
                          nxi       = fld%gridSource%nX,        &
                          nyi       = fld%gridSource%nY,        &
                          nzi       = fld%gridSource%nZ,        &
                          inpFld    = fld%gridSource%ReadField, &
                          inpBL     = fld%gridSource%ReadBL,    &
                          inpLSV    = fld%gridSource%ReadLSV,   &
                          inpZ      = fld%gridSource%Z,         &
                          nxo       = fld%grid%nX,              &
                          nyo       = fld%grid%nY,              &
                          nzo       = fld%grid%nZ,              &
                          outFld    = fld%NextField,            &
                          outBl     = fld%NextBL,               &
                          outLSV    = fld%NextLSV )
  IF( error%Number /= NO_ERROR )GOTO 9999

END IF

!------ Copy fields into "background" for obs assimilation

IF( fld%nObsSource > 0 )THEN

  CALL CopyNextField2Assm( fld )
  fld%t2 = fld%tNext

  IF( BTEST(fld%status,FSB_DOINIT) .AND. BTEST(fld%type,FTB_ZRUF) )THEN
    CALL CopyArray( fld%grid%landcover%roughness,fld%NextBL%zruf,fld%grid%nXY )
  END IF

END IF

SWIMupdateGriddedMet = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION Read3dVarGridded( src,nx,ny,nz,var,ks0,ishft,jshft,var_src )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ),   INTENT( IN ) :: src
INTEGER,           INTENT( IN ) :: nx, ny, nz
REAL, DIMENSION(:),POINTER      :: var
INTEGER,           INTENT( IN ) :: ks0
INTEGER, OPTIONAL, INTENT( IN ) :: ishft
INTEGER, OPTIONAL, INTENT( IN ) :: jshft
REAL, DIMENSION(:), OPTIONAL, POINTER :: var_src

INTEGER im, jm, km, nxp
INTEGER nxy, nxym, ntot, ks, ios
INTEGER unit
INTEGER iskip, jskip
INTEGER i, j, k, ii, jj, kk, j0, jj0, kk0, k0
INTEGER is, js

REAL, DIMENSION(:), POINTER :: wrk

INTEGER, EXTERNAL :: getFieldIndex

CHARACTER(128), EXTERNAL :: ArraySizeStr

Read3dVarGridded = SWIMfailure

unit = getFieldIndex( Src%unit )

!------ Set total array sizes and other locals

nxy = nx*ny
im  = src%nX; jm = src%nY; km = src%nZ
nxym = im*jm
ntot = nxym*km

iskip = src%iSkip; jskip = src%jSkip
ks    = src%kStart

!------ Allocate work array for reading entire array

IF( BTEST(src%type,GSB_WRF) .AND. .NOT.PRESENT(var_src) )THEN
  error%Number  = IV_ERROR
  error%Routine = 'Read3dVarGridded'
  error%Message = 'Source field not present for setting WRF arrays'
  GOTO 9999
END IF
IF( .NOT.PRESENT(var_src) )THEN
  ALLOCATE( wrk(ntot),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'Read3dVarGridded'
    error%Message = 'Error allocating temporary array'
    error%Inform  = 'Reading gridded met file '//TRIM(src%Source(1))
    error%Action  = ArraySizeStr( 3,(/im,jm,km/) )
    GOTO 9999
  END IF
END IF

!------ Read field into work array

IF( BTEST(src%type,GSB_BINARY) )THEN
  READ(unit,IOSTAT=ios) (wrk(i),i=1,ntot)

ELSE IF( BTEST(src%type,GSB_MEDOC) )THEN
  READ(unit,'(6(F12.0,1X))',IOSTAT=ios) (wrk(i),i=1,ntot)

ELSE IF( BTEST(src%type,GSB_WRF) )THEN
  IF( SIZE(var_src) > ntot )km = SIZE(var_src)/nxym
  wrk => var_src
  ios = 0

ELSE !SCIP gridded
  READ(src%unit,'(10F8.0)',IOSTAT=ios) (wrk(i),i=1,ntot)

END IF

IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'Read3dVarGridded'
  error%Message = 'Error reading gridded met file '//TRIM(src%Source(1))
  GOTO 9999
END IF

!------ Put into met array after skipping

IF( BTEST(src%type,GSB_PERIODIC) )THEN
  nxp = nx-1
ELSE
  nxp = nx
END IF

is = 0; js = 0

IF( PRESENT(ishft) )THEN
  IF( ishft == -1 )is = 1
END IF
IF( PRESENT(jshft) )THEN
  IF( jshft == -1 )js = 1
END IF

!$OMP PARALLEL SHARED(var,wrk,nx,ny,nz,nxy,nxym,nxp,src,iskip,jskip,ks0,im,km,is,ks)
!$OMP DO PRIVATE(k,kk,k0,kk0,j,j0,jj,jj0,i,ii)
DO k = 1,nz

  IF( src%lVertFlip )THEN
    kk = nz-k+1
    k0 = (kk-1)*nxy + ks0
  ELSE
    k0 = (k-1)*nxy + ks0
  END IF
  kk  = MIN( ks + k-1 , km )
  kk0 = (kk-1)*nxym

  DO j = 1,ny
    j0 = (j-1)*nx
    jj = MIN( src%jStart+js + (j-1)*jskip, src%jEnd )
    jj0 = kk0 + (jj-1)*im
    DO i = 1,nxp
      ii = MIN( src%iStart+is + (i-1)*iskip, src%iEnd )
      var(j0+i+k0) = wrk(jj0+ii)
    END DO
  END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

IF( BTEST(src%type,GSB_PERIODIC) )THEN
  DO k = 1,nz
    k0 = (k-1)*nxy + ks0
    DO j = 1,ny
      j0 = (j-1)*nx + k0
      var(j0+nx) = var(j0+1)
    END DO
  END DO
END IF

Read3dVarGridded = SWIMresult

9999 CONTINUE

IF( PRESENT(var_src) )THEN
  NULLIFY(wrk)
ELSE
  IF( ASSOCIATED(wrk) )DEALLOCATE( wrk,STAT=ios )
END IF

RETURN
END

!==============================================================================

SUBROUTINE SetPeriodicBC( type,grd,MeanFld,VarFld,BLFld )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(KIND=8),               INTENT( IN    ) :: type
TYPE( MetGrid     ),           INTENT( IN    ) :: grd
TYPE( MetMean3D   ), OPTIONAL, INTENT( INOUT ) :: MeanFld
TYPE( MetVariance ), OPTIONAL, INTENT( INOUT ) :: VarFld
TYPE( MetBLparam  ), OPTIONAL, INTENT( INOUT ) :: BLFld

INTEGER nx, ny, nxy, i, k, k0

nx = grd%nX; ny = grd%nY ; nxy = nx*ny

DO k = 1,grd%nZ
  k0 = (k-1)*nxy

  DO i = 1,ny

    IF( PRESENT(MeanFld) )THEN
      MeanFld%U(k0+i*nx) = MeanFld%U(k0+(i-1)*nx+1)
      MeanFld%V(k0+i*nx) = MeanFld%V(k0+(i-1)*nx+1)
      IF( BTEST(type,FTB_W) )MeanFld%W(k0+i*nx)     = MeanFld%W(k0+(i-1)*nx+1)
      IF( BTEST(type,FTB_T) )MeanFld%Tpot(k0+i*nx)  = MeanFld%Tpot(k0+(i-1)*nx+1)
      IF( BTEST(type,FTB_P) )MeanFld%Press(k0+i*nx) = MeanFld%Press(k0+(i-1)*nx+1)
      IF( BTEST(type,FTB_H) )MeanFld%Humid(k0+i*nx) = MeanFld%Humid(k0+(i-1)*nx+1)
      IF( BTEST(type,FTB_QCLD) )MeanFld%Qcloud(k0+i*nx) = MeanFld%Qcloud(k0+(i-1)*nx+1)
    END IF

    IF( PRESENT(VarFld) )THEN
      VarFld%UU(k0+i*nx) = VarFld%UU(k0+(i-1)*nx+1)
      VarFld%VV(k0+i*nx) = VarFld%VV(k0+(i-1)*nx+1)
      VarFld%UV(k0+i*nx) = VarFld%UV(k0+(i-1)*nx+1)
      IF( ASSOCIATED(VarFld%SL) )VarFld%SL(k0+i*nx) = VarFld%SL(k0+(i-1)*nx+1)
    END IF

  END DO

END DO

IF( PRESENT(BLFld) )THEN
  DO i = 1,ny
    BLFld%zi(i*nx)       = BLFld%zi((i-1)*nx+1)
    BLFld%HeatFlux(i*nx) = BLFld%HeatFlux((i-1)*nx+1)
  END DO
END IF

RETURN
END

!==============================================================================

SUBROUTINE Copy2dVar( src,grid,wrk,var )

!------ Copy subsection (with skipping) into 2d field array

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( IN ) :: src
TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:), POINTER   :: wrk
REAL, DIMENSION(:), POINTER   :: var

INTEGER im, jm, nx, ny, nxym
INTEGER iskip, jskip
INTEGER i, j, ii, jj, j0, jj0, ivar, iwrk

!------ Set total array sizes and other locals

im = src%nX; jm = src%nY
nx = grid%nX; ny = grid%nY

nxym = im*jm

iskip = src%iSkip; jskip = src%jSkip

!------ Copy out (accounting for skipping)

DO jj = 1,ny
  j   = (jj-1)*jskip + src%jStart
  j0  = (j-1)*im
  jj0 = (jj-1)*nx
  DO ii = 1,nx
    i    = (ii-1)*iskip + src%iStart
    ivar = jj0 + ii
    iwrk = j0  + i
    var(ivar) = wrk(iwrk)
  END DO
END DO

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckNextFile( ifile,iend ) RESULT( lend )

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: ifile
INTEGER, INTENT( IN    ) :: iend

lend = .FALSE.

ifile = ifile + SIGN(1,iend)
IF( SIGN(ifile,iend) > iend )lend = .TRUE.

RETURN
END

