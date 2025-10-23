INTEGER FUNCTION SWIMexpandField( fld )

!------ Expand fields from single profile (or point) to 3d

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, alloc_stat, nxy, nxyz, kblSave, i

INTEGER, EXTERNAL :: SWIMexpand3dField, SWIMexpandBLParam, SWIMexpandVariance
INTEGER, EXTERNAL :: SWIMexpandObsWt, SWIMexpandBLaux, SWIMexpandBLProfile
INTEGER, EXTERNAL :: SWIMdeallocVariance
INTEGER, EXTERNAL :: GridLogMsg, SetGridLL
INTEGER, EXTERNAL :: InitLSV

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMexpandField = SWIMfailure

!------ Set grid sizes for allocating; deallocate & allocate

nxy = fld%grid%nXY; nxyz = nxy*fld%grid%nZ

DEALLOCATE( fld%grid%terrain%H,fld%grid%terrain%D, &
            fld%grid%terrain%Du,fld%grid%terrain%Dv,STAT=alloc_stat )

ALLOCATE( fld%grid%terrain%H(nxy),fld%grid%terrain%D(nxy), &
          fld%grid%terrain%Du(nxy),fld%grid%terrain%Dv(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMexpandField'
  error%Message = 'Error allocating terrain arrays'
  error%Inform  = ArraySizeStr( 2,(/fld%grid%nX,fld%grid%nY/) )
  GOTO 9999
END IF

kblSave = fld%grid%landcover%kbl(1)
DEALLOCATE( fld%grid%landcover%roughness,fld%grid%landcover%canopyHt, &
            fld%grid%landcover%alpha,fld%grid%landcover%Bowen, &
            fld%grid%landcover%albedo,fld%grid%landcover%kbl,STAT=alloc_stat )

ALLOCATE( fld%grid%landcover%roughness(nxy),fld%grid%landcover%canopyHt(nxy), &
          fld%grid%landcover%alpha(nxy),fld%grid%landcover%Bowen(nxy), &
          fld%grid%landcover%albedo(nxy),fld%grid%landcover%kbl(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'SWIMexpandField'
  error%Message = 'Error allocating landcover arrays'
  error%Inform  = ArraySizeStr( 2,(/fld%grid%nX,fld%grid%nY/) )
  GOTO 9999
END IF

!------ Assume flat terrain and uniform landcover

DO i = 1,nxy

  fld%grid%terrain%H(i)  = 0.
  fld%grid%terrain%D(i)  = 1.
  fld%grid%terrain%Du(i) = 1.
  fld%grid%terrain%Dv(i) = 1.

  fld%grid%landcover%roughness(i) = Prj%BL%zruf
  fld%grid%landcover%canopyHt(i)  = Prj%BL%hc
  fld%grid%landcover%alpha(i)     = Prj%BL%alpha
  fld%grid%landcover%Bowen(i)     = Prj%BL%Bowen
  fld%grid%landcover%albedo(i)    = Prj%BL%albedo
  fld%grid%landcover%kbl(i)       = kblSave

END DO

IF( nxy >= 6 )fld%type = IBSET(fld%type,FTB_DU2)

!------ Allocate met fields

irv = SWIMexpand3dField( fld%type,nxyz,fld%Field )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMexpand3dField( fld%type,nxyz,fld%NextField )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%type,FTB_UU) )THEN

  irv = SWIMexpandBLProfile( nxyz,fld%BLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

  irv = SWIMexpandBLProfile( nxyz,fld%NextBLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

irv = SWIMexpandBLParam( fld%type,nxy,fld%BL,fld%NextBL )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMexpandBLaux( nxy,fld%BLaux )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMexpandObsWt( fld%type,nxy,nxyz,fld%obsWt )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Compute lat/lon (& related) arrays if possible

DEALLOCATE( fld%grid%lon,fld%grid%lat,STAT=alloc_stat )
DEALLOCATE( fld%grid%sunrise,fld%grid%sunset,STAT=alloc_stat )
DEALLOCATE( fld%grid%sunfac,STAT=alloc_stat )

irv =  SetGridLL( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

fld%BLaux%JulianDay = -100000 !To recompute sunrise/setset in SWIMupdateBL, if appropriate

!------ Expand large-scale variability

SELECT CASE ( Prj%LSVType )

  CASE( LVP_MET )

    irv = SWIMexpandVariance( nxyz,fld%LSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    irv = SWIMexpandVariance( nxyz,fld%NextLSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    DEALLOCATE( fld%LSV%SL,fld%NextLSV%SL,STAT=irv )  !Only a single length scale used
    ALLOCATE( fld%LSV%SL(1),fld%NextLSV%SL(1),STAT=irv )
    IF( irv /= 0 )THEN
    END IF

    fld%LSV%SL(1)     = Prj%LSVscale
    fld%NextLSV%SL(1) = Prj%LSVscale

  CASE( LVP_MODEL )

    irv = SWIMdeallocVariance( fld%LSV )
    IF( irv /= SWIMsuccess )GOTO 9999

    irv = InitLSV( fld )
    IF( irv /= SWIMsuccess )GOTO 9999

END SELECT

fld%status = IBSET(fld%status,FSB_EXPAND)

!------ Log messages

irv = GridLogMsg( fld )
IF( irv /= SWIMsuccess )GOTO 9999

!------ SWIMming success

SWIMexpandField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpand3dField( type,n,field3D )

!------ Expand 3d met field arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),        INTENT( IN    ) :: type
INTEGER,           INTENT( IN    ) :: n
TYPE( MetMean3D ), INTENT( INOUT ) :: field3D

INTEGER irv, i

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpand3dField = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpand3dField'
error%Message = 'Error expanding 3d met field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ U

irv = SWIMexpandPtrArray( field3D%U,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Horizontal velocity component U'
  GOTO 9999
END IF

!------ V

irv = SWIMexpandPtrArray( field3D%V,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Horizontal velocity component V'
  GOTO 9999
END IF

!------ W

IF( BTEST(type,FTB_W) )THEN
  irv = SWIMexpandPtrArray( field3D%W,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Vertical velocity component W'
    GOTO 9999
  END IF
END IF

!------ Potential Temperature

IF( BTEST(type,FTB_T) )THEN
  irv = SWIMexpandPtrArray( field3D%Tpot,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Temperature'
    GOTO 9999
  END IF
END IF

!------ Pressure

IF( BTEST(type,FTB_P) )THEN
  irv = SWIMexpandPtrArray( field3D%Press,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Pressure'
    GOTO 9999
  END IF
END IF

!------ Humidity

IF( BTEST(type,FTB_H) )THEN
  irv = SWIMexpandPtrArray( field3D%Humid,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Humidity'
    GOTO 9999
  END IF
END IF

!------ Cloud liquid water

IF( BTEST(type,FTB_QCLD) )THEN
  irv = SWIMexpandPtrArray( field3D%Qcloud,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Cloud water'
    GOTO 9999
  END IF
END IF

!------ Shear inhomogeneity (for splitting & smooth fields)

IF( BTEST(type,FTB_DU2) )THEN
  ALLOCATE( field3D%dU2(n),STAT=irv )
  IF( irv /= 0 )THEN
    error%Inform = 'Horizontal shear inhomogeneity, dU2'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%dU2(i) = 0.
  END DO
END IF

!------ SWIMming Success

SWIMexpand3dField = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandBLParam( type,n,BL,nextBL )

!------ Allocate input boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),         INTENT( IN    ) :: type
INTEGER,            INTENT( IN    ) :: n
TYPE( MetBLparam ), INTENT( INOUT ) :: BL, nextBL

INTEGER irv

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpandBLParam = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpandBLParam'
error%Message = 'Error expanding boundary layer field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ Mixing height - check for previous expansion in putMixingHt

IF( SIZE(BL%zi) == 1 )THEN
  irv = SWIMexpandPtrArray( BL%zi,n )
  IF( BTEST(type,FTB_ZI) )irv = SWIMexpandPtrArray( nextBL%zi,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Mixing Height'
    GOTO 9999
  END IF
ELSE IF( SIZE(BL%zi) /= n )THEN
  error%Message = 'Error expanding mixing ht array'
  error%Inform  = 'Grid already 2D but wrong size'
  GOTO 9999
END IF

!------ Heat Flux

irv = SWIMexpandPtrArray( BL%HeatFlux,n )
IF( BTEST(type,FTB_HFLX) )irv = SWIMexpandPtrArray( nextBL%HeatFlux,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Heat Flux'
  GOTO 9999
END IF

!------ U*

IF( BTEST(type,FTB_UST) )THEN
  irv = SWIMexpandPtrArray( BL%ustr,n )
  IF( BTEST(type,FTB_UST) )irv = SWIMexpandPtrArray( nextBL%ustr,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Friction Velocity'
    GOTO 9999
  END IF
END IF

!------ Monin-Obukhov length

irv = SWIMexpandPtrArray( BL%invMOL,n )
IF( BTEST(type,FTB_MOL) )irv = SWIMexpandPtrArray( nextBL%invMOL,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Monin-Obukhov length'
  GOTO 9999
END IF

!------ Precipitation

IF( BTEST(type,FTB_PRCP) .OR. BTEST(type,FTB_PRATE) )THEN
  irv = SWIMexpandPtrArray( BL%prcp,n )
  irv = SWIMexpandPtrArray( nextBL%prcp,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Precipitation'
    GOTO 9999
  END IF
END IF

IF( ASSOCIATED(BL%rainprb) )THEN
  irv = SWIMexpandPtrArray( BL%rainprb,n )
  irv = SWIMexpandPtrArray( nextBL%rainprb,n )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Rain probability'
    GOTO 9999
  END IF
END IF

!------ Cloud cover

irv = SWIMexpandPtrArray( BL%cc,n )
IF( BTEST(type,FTB_CLDCV) )irv = SWIMexpandPtrArray( nextBL%cc,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Cloud cover'
  GOTO 9999
END IF

!------ Leaf area index

irv = SWIMexpandPtrArray( BL%LAI,n )
IF( BTEST(type,FTB_LAI) )irv = SWIMexpandPtrArray( nextBL%LAI,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Leaf area index'
  GOTO 9999
END IF

!------ SWIMming Success

SWIMexpandBLParam = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandBLProfile( n,BLprof )

!------ Allocate boundary layer turbulence profile arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: n
TYPE( MetBLprof ), INTENT( INOUT ) :: BLprof

INTEGER irv

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpandBLProfile = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpandBLProfile'
error%Message = 'Error expanding boundary layer turbulence profile arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

irv = SWIMexpandPtrArray( BLprof%UU,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field UU'
  GOTO 9999
END IF

irv = SWIMexpandPtrArray( BLprof%VV,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field VV'
  GOTO 9999
END IF

irv = SWIMexpandPtrArray( BLprof%WW,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field WW'
  GOTO 9999
END IF

irv = SWIMexpandPtrArray( BLprof%WT,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field WT'
  GOTO 9999
END IF

irv = SWIMexpandPtrArray( BLprof%SL,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field SL'
  GOTO 9999
END IF

irv = SWIMexpandPtrArray( BLprof%SZ,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Profile Boundary Layer Field SZ'
  GOTO 9999
END IF

!------ SWIMming Success

SWIMexpandBLProfile = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandBLaux( n,BLaux )

!------ Allocate auxilliary (not input) boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: n
TYPE( MetBLaux ), INTENT( INOUT ) :: BLaux

INTEGER irv

INTERFACE

  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray

  INTEGER FUNCTION SWIMexpandIntegerPtrArray( var,n )
    INTEGER, DIMENSION(:), POINTER :: var
    INTEGER,          INTENT( IN ) :: n
  END FUNCTION SWIMexpandIntegerPtrArray

END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpandBLaux = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpandBLaux'
error%Message = 'Error expanding auxiliary boundary layer field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ 2D arrays

                        irv = SWIMexpandPtrArray( BLaux%zsl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%usl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%vsl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%ubl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%vbl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%tbl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%pbl,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%wspd2,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%ustr2,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%wstr2,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%uu_sh,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%uu_buoy,n )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%sbl,n )

IF( irv /= SWIMsuccess )THEN
  error%Inform = '2D arrays'
  GOTO 9999
END IF

!------ Diffusivity arrays

                        irv = SWIMexpandPtrArray( BLaux%aDiff,n*BLaux%nz )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%bDiff,n*BLaux%nz )
IF( irv == SWIMsuccess )irv = SWIMexpandPtrArray( BLaux%qq,   n*BLaux%nz )
IF( irv /= SWIMsuccess )THEN
  error%Inform = '3D diffusivity arrays'
  GOTO 9999
END IF

!------ SWIMming Success

SWIMexpandBLaux = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandObsWt( type,n2,n3,obsWt )

!------ Allocate observation interpolation weight arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),       INTENT( IN    ) :: type
INTEGER,          INTENT( IN    ) :: n2, n3
TYPE( MetObsWt ), INTENT( INOUT ) :: obsWt

INTEGER irv

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpandObsWt = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpandObsWt'
error%Message = 'Error expanding met observation interpolation arrays'
error%Action  = ArraySizeStr( 1,(/n3/) )

!------ U

irv = SWIMexpandPtrArray( obsWt%su,n3 )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Horizontal velocity component U'
  GOTO 9999
END IF

!------ V

irv = SWIMexpandPtrArray( obsWt%sv,n3 )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Horizontal velocity component V'
  GOTO 9999
END IF

!------ Potential Temperature

IF( BTEST(type,FTB_T) )THEN
  irv = SWIMexpandPtrArray( obsWt%st,n3 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Temperature'
    GOTO 9999
  END IF
END IF

!------ Pressure

IF( BTEST(type,FTB_P) )THEN
  irv = SWIMexpandPtrArray( obsWt%sp,n3 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Pressure'
    GOTO 9999
  END IF
END IF

!------ Humidity

IF( BTEST(type,FTB_H) )THEN
  irv = SWIMexpandPtrArray( obsWt%sh,n3 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Humidity'
    GOTO 9999
  END IF
END IF

!------ Cloud liquid water

IF( BTEST(type,FTB_QCLD) )THEN
  irv = SWIMexpandPtrArray( obsWt%sqc,n3 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Cloud water'
    GOTO 9999
  END IF
END IF

error%Action = ArraySizeStr( 1,(/n2/) )  !Set for 2d fields

!------ Mixing height

IF( BTEST(type,FTB_ZI) )THEN
  irv = SWIMexpandPtrArray( obsWt%szi,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Mixing Height'
    GOTO 9999
  END IF
END IF

!------ Heat Flux

IF( BTEST(type,FTB_HFLX) )THEN
  irv = SWIMexpandPtrArray( obsWt%shflx,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Heat Flux'
    GOTO 9999
  END IF
END IF

!------ Friction Velocity

IF( BTEST(type,FTB_UST) )THEN
  irv = SWIMexpandPtrArray( obsWt%sustr,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Heat Flux'
    GOTO 9999
  END IF
END IF

!------ Monin-Obukhov length

IF( BTEST(type,FTB_MOL) )THEN
  irv = SWIMexpandPtrArray( obsWt%smol,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Monin-Obukhov length'
    GOTO 9999
  END IF
END IF

!------ Precipitation

IF( BTEST(type,FTB_PRCP) .OR. BTEST(type,FTB_PRATE) )THEN
  irv = SWIMexpandPtrArray( obsWt%sprcp,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Precipitation'
    GOTO 9999
  END IF
END IF

!------ Cloud cover

IF( BTEST(type,FTB_CLDCV) )THEN
  irv = SWIMexpandPtrArray( obsWt%scc,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Cloud cover'
    GOTO 9999
  END IF
END IF

!------ Leaf area index

IF( BTEST(type,FTB_LAI) )THEN
  irv = SWIMexpandPtrArray( obsWt%slai,n2 )
  IF( irv /= SWIMsuccess )THEN
    error%Inform = 'Leaf area index'
    GOTO 9999
  END IF
END IF

!------ SWIMming Success

SWIMexpandObsWt = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandVariance( n,var )

!------ Expand 3d met field arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,             INTENT( IN    ) :: n
TYPE( MetVariance ), INTENT( INOUT ) :: var

INTEGER irv

INTERFACE
  INTEGER FUNCTION SWIMexpandPtrArray( var,n )
    REAL, DIMENSION(:), POINTER :: var
    INTEGER,       INTENT( IN ) :: n
  END FUNCTION SWIMexpandPtrArray
END INTERFACE

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMexpandVariance = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMexpandVariance'
error%Message = 'Error expanding 3d variance field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ UU

irv = SWIMexpandPtrArray( var%UU,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Velocity variance UU'
  GOTO 9999
END IF

!------ VV

irv = SWIMexpandPtrArray( var%VV,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Velocity variance VV'
  GOTO 9999
END IF

!------ UV

irv = SWIMexpandPtrArray( var%UV,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Velocity correlation UV'
  GOTO 9999
END IF

!------ SL

irv = SWIMexpandPtrArray( var%SL,n )
IF( irv /= SWIMsuccess )THEN
  error%Inform = 'Length scale SL'
  GOTO 9999
END IF

!------ SWIMming Success

SWIMexpandVariance = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexpandPtrArray( var,n )

USE SWIMparam_fd
USE SWIM_fi, ONLY: SWIMresult

IMPLICIT NONE

REAL, DIMENSION(:), POINTER :: var
INTEGER,       INTENT( IN ) :: n

INTEGER alloc_stat, m, ncopy, i, j, i0

REAL, DIMENSION(:), POINTER :: tem

SWIMexpandPtrArray = SWIMfailure

m = SIZE(var); ncopy = n/m

tem => var; NULLIFY( var )

ALLOCATE( var(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO j = 1,m
  i0 = (j-1)*ncopy
  DO i = 1,ncopy
    var(i0+i) = tem(j)
  END DO
END DO

DEALLOCATE( tem,STAT=alloc_stat ); NULLIFY( tem )

SWIMexpandPtrArray = SWIMresult

9999 CONTINUE

RETURN
END
