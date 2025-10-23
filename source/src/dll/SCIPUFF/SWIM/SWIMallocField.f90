INTEGER FUNCTION SWIMalloc3dField( type,n,field3D )

!------ Allocate 3d met field arrays

USE SWIM_fi
USE SWIMparam_fd

INTEGER(8),        INTENT( IN    ) :: type
INTEGER,           INTENT( IN    ) :: n
TYPE( MetMean3D ), INTENT( INOUT ) :: field3D

INTEGER alloc_stat, i

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMalloc3dField = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMalloc3dField'
error%Message = 'Error allocating 3d met field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ U & V

ALLOCATE( field3D%U(n),field3D%V(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Horizontal velocity components, U & V'
  GOTO 9999
END IF

DO i = 1,n
  field3D%U(i) = NOT_SET_R
  field3D%V(i) = NOT_SET_R
END DO


IF( BTEST(type,FTB_DU2) )THEN
  ALLOCATE( field3D%dU2(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Horizontal shear inhomogeneity, dU2'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%dU2(i) = NOT_SET_R
  END DO
END IF

!------ W

IF( BTEST(type,FTB_W) )THEN
  ALLOCATE( field3D%W(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Vertical velocity component W'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%W(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%W )
END IF

!------ Potential Temperature

IF( BTEST(type,FTB_T) )THEN
  ALLOCATE( field3D%Tpot(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Temperature'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%Tpot(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%Tpot )
END IF

!------ Pressure

IF( BTEST(type,FTB_P) )THEN
  ALLOCATE( field3D%Press(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Pressure'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%Press(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%Press )
END IF

!------ Humidity

IF( BTEST(type,FTB_H) )THEN
  ALLOCATE( field3D%Humid(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Humidity'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%Humid(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%Humid )
END IF

!------ Cloud liquid water

IF( BTEST(type,FTB_QCLD) )THEN
  ALLOCATE( field3D%Qcloud(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Cloud water'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%Qcloud(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%Qcloud )
END IF

!------ 3D height field

IF( BTEST(type,FTB_Z) )THEN
  ALLOCATE( field3D%Z(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = '3D height field'
    GOTO 9999
  END IF
  DO i = 1,n
    field3D%Z(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( field3D%Z )
END IF

!------ SWIMming Success

SWIMalloc3dField = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocBLParam( type,n,BL,nextBL )

!------ Allocate input boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

INTEGER(8),         INTENT( IN    ) :: type
INTEGER,            INTENT( IN    ) :: n
TYPE( MetBLparam ), INTENT( INOUT ) :: BL, nextBL

INTEGER alloc_stat, i

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMallocBLParam = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMallocBLParam'
error%Message = 'Error allocating boundary layer field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ Mixing height

ALLOCATE( BL%zi(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Mixing Height'
  GOTO 9999
END IF
DO i = 1,n
  BL%zi(i) = NOT_SET_R                               !Set for output at t=0
END DO
IF( BTEST(type,FTB_ZI) )THEN
  ALLOCATE( nextBL%zi(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Mixing Height'
    GOTO 9999
  END IF
  DO i = 1,n
    nextBL%zi(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( nextBL%zi )
END IF

!------ Heat Flux

ALLOCATE( BL%HeatFlux(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Heat Flux'
  GOTO 9999
END IF
DO i = 1,n
  BL%HeatFlux(i) = NOT_SET_R
END DO
IF( BTEST(type,FTB_HFLX) )THEN
  ALLOCATE( nextBL%HeatFlux(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Heat Flux'
    GOTO 9999
  END IF
  DO i = 1,n
    nextBL%HeatFlux(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( nextBL%HeatFlux )
END IF

!------ U*

IF( BTEST(type,FTB_UST) )THEN
  ALLOCATE( BL%ustr(n),nextBL%ustr(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Friction velocity'
    GOTO 9999
  END IF
  DO i = 1,n
    BL%ustr(i)     = NOT_SET_R
    nextBL%ustr(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( BL%ustr,nextBL%ustr )
END IF

!------ Monin-Obukhov length

ALLOCATE( BL%invMOL(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Monin-Obukhov length'
  GOTO 9999
END IF
IF( BTEST(type,FTB_MOL) )THEN
  ALLOCATE( nextBL%invMOL(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Monin-Obukhov length'
    GOTO 9999
  END IF
  DO i = 1,n
    nextBL%invMOL(i) = NOT_SET_R
  END DO
ELSE
  NULLIFY( nextBL%invMOL )
END IF

!------ Initialize MOL for (potential) use in SWIMupdateBL (SetSqVel)

DO i = 1,n
  BL%invMOL(i) = 0.
END DO

!------ Precipitation

IF( (BTEST(type,FTB_PRCP) .OR. BTEST(type,FTB_PRATE)) .AND. Prj%BL%pr_type == -1. )THEN
  ALLOCATE( BL%prcp(n),nextBL%prcp(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Precipitation'
    GOTO 9999
  END IF
  DO i = 1,n
    BL%prcp(i)     = NOT_SET_R
    nextBL%prcp(i) = NOT_SET_R
  END DO
ELSE IF( Prj%BL%pr_type == -1. )THEN
  error%Message = 'Precipitation not available on Met file'
  error%Inform  = 'Cannot specify as input from file'
  GOTO 9999
ELSE
  ALLOCATE( BL%prcp(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Precipitation'
    GOTO 9999
  END IF
  BL%prcp = Prj%BL%pr_type
  NULLIFY( nextBL%prcp )
END IF

!------ Cloud cover

ALLOCATE( BL%cc(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Cloud cover'
  GOTO 9999
END IF
DO i = 1,n
  BL%cc(i) = 0.
END DO
IF( BTEST(type,FTB_CLDCV) )THEN
  ALLOCATE( nextBL%cc(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Cloud cover'
    GOTO 9999
  END IF
  DO i = 1,n
    nextBL%cc(i) = 0.
  END DO
ELSE
  NULLIFY( nextBL%cc )
END IF

!------ Time-dependent roughness

IF( BTEST(type,FTB_ZRUF) )THEN
  ALLOCATE( BL%zruf(n),nextBL%zruf(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Roughness'
    GOTO 9999
  END IF
  DO i = 1,n
    BL%zruf(i)     = NOT_SET_R
    nextBL%zruf(i) = NOT_SET_R
  END DO
END IF

!------ Leaf area index

ALLOCATE( BL%LAI(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'LAI'
  GOTO 9999
END IF
DO i = 1,n
  BL%LAI(i)     = 0.
END DO
IF( BTEST(type,FTB_LAI) )THEN
  ALLOCATE( nextBL%LAI(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'LAI'
    GOTO 9999
  END IF
  DO i = 1,n
    nextBL%LAI(i) = 0.
  END DO
END IF

!------ SWIMming Success

SWIMallocBLParam = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocBLaux( n,BLaux )

!------ Allocate auxilliary (not input) boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

INTEGER,          INTENT( IN    ) :: n
TYPE( MetBLaux ), INTENT( INOUT ) :: BLaux

INTEGER alloc_stat, i

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMallocBLaux = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMallocBLaux'
error%Message = 'Error allocating auxiliary boundary layer field arrays'
error%Action  = ArraySizeStr( 1,(/n/) )

!------ 2D arrays

ALLOCATE( BLaux%zsl(n),BLaux%usl(n),BLaux%vsl(n),BLaux%ubl(n),BLaux%vbl(n), &
          BLaux%tbl(n),BLaux%pbl(n),BLaux%wspd2(n),BLaux%ustr2(n),BLaux%wstr2(n), &
          BLaux%uu_sh(n),BLaux%uu_buoy(n),BLaux%sbl(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = '2D arrays'
  GOTO 9999
END IF

!------ Initialize zsl for (potential) use in SWIMupdateBL (SetSqVel)

DO i = 1,n
  BLaux%zsl(i) = 100.
END DO

!------ Diffusivity arrays

ALLOCATE( BLaux%aDiff(n*BLaux%nz),BLaux%bDiff(n*BLaux%nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = '3D diffusivity arrays'
  GOTO 9999
END IF

!------ QQ array for profile boundary layer

ALLOCATE( BLaux%qq(n*BLaux%nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = '3D QQ (profile BL) array'
  GOTO 9999
END IF

!------ Another SWIMming Success

SWIMallocBLaux = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocObsWt( type,n2,n3,obsWt )

!------ Allocate observation interpolation weight arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),       INTENT( IN    ) :: type
INTEGER,          INTENT( IN    ) :: n2, n3
TYPE( MetObsWt ), INTENT( INOUT ) :: obsWt

INTEGER alloc_stat, i

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMallocObsWt = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMallocObsWt'
error%Message = 'Error allocating met observation interpolation arrays'
error%Action  = ArraySizeStr( 1,(/n3/) )

!------ U & V

ALLOCATE( obsWt%su(n3),obsWt%sv(n3),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Inform = 'Horizontal velocity components, U & V'
  GOTO 9999
END IF
DO i = 1,n3
  obsWt%su(i) = 0.
  obsWt%sv(i) = 0.
END DO

!------ Potential Temperature

IF( BTEST(type,FTB_T) )THEN
  ALLOCATE( obsWt%st(n3),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Temperature'
    GOTO 9999
  END IF
  DO i = 1,n3
    obsWt%st(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%st )
END IF

!------ Pressure

IF( BTEST(type,FTB_P) )THEN
  ALLOCATE( obsWt%sp(n3),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Pressure'
    GOTO 9999
  END IF
  DO i = 1,n3
    obsWt%sp(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%sp )
END IF

!------ Humidity

IF( BTEST(type,FTB_H) )THEN
  ALLOCATE( obsWt%sh(n3),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Humidity'
    GOTO 9999
  END IF
  DO i = 1,n3
    obsWt%sh(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%sh )
END IF

!------ Cloud liquid water

IF( BTEST(type,FTB_QCLD) )THEN
  ALLOCATE( obsWt%sqc(n3),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Cloud water'
    GOTO 9999
  END IF
  DO i = 1,n3
    obsWt%sqc(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%sqc )
END IF

error%Action = ArraySizeStr( 1,(/n2/) ) !Set for 2d fields

!------ Mixing height

IF( BTEST(type,FTB_ZI) )THEN
  ALLOCATE( obsWt%szi(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Mixing Height'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%szi(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%szi )
END IF

!------ Heat Flux

IF( BTEST(type,FTB_HFLX) )THEN
  ALLOCATE( obsWt%shflx(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Heat Flux'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%shflx(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%shflx )
END IF

!------ Friction Velocity

IF( BTEST(type,FTB_UST) )THEN
  ALLOCATE( obsWt%sustr(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Friction Velocity'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%sustr(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%sustr )
END IF

!------ Monin-Obukhov length

IF( BTEST(type,FTB_MOL) )THEN
  ALLOCATE( obsWt%smol(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Monin-Obukhov length'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%smol(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%smol )
END IF

!------ Precipitation

IF( BTEST(type,FTB_PRCP) .OR. BTEST(type,FTB_PRATE) )THEN
  ALLOCATE( obsWt%sprcp(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Precipitation'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%sprcp(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%sprcp )
END IF

!------ Cloud cover

IF( BTEST(type,FTB_CLDCV) )THEN
  ALLOCATE( obsWt%scc(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Cloud cover'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%scc(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%scc )
END IF

!------ Leaf area index

IF( BTEST(type,FTB_LAI) )THEN
  ALLOCATE( obsWt%slai(n2),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Inform = 'Lear area index'
    GOTO 9999
  END IF
  DO i = 1,n2
    obsWt%slai(i) = 0.
  END DO
ELSE
  NULLIFY( obsWt%slai )
END IF

!------ SWIMming Success

SWIMallocObsWt = SWIMresult
CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocVariance( n,var )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,             INTENT( IN  ) :: n
TYPE( MetVariance ), INTENT( OUT ) :: var

INTEGER alloc_stat

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMallocVariance = SWIMfailure

ALLOCATE( var%UU(n),var%VV(n),var%UV(n),var%SL(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMallocVariance'
  error%Message = 'Error allocating met variance arrays'
  error%Inform  = ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF

SWIMallocVariance = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocBLprof( n,BLprof )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,           INTENT( IN  ) :: n
TYPE( MetBLprof ), INTENT( OUT ) :: BLprof

INTEGER alloc_stat

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMallocBLprof = SWIMfailure

ALLOCATE( BLprof%UU(n),BLprof%VV(n),BLprof%WW(n), &
          BLprof%WT(n),BLprof%SL(n),BLprof%SZ(n), STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMallocBLprof'
  error%Message = 'Error allocating met BL profile arrays'
  error%Inform  = ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF

SWIMallocBLprof = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocQLprof( n,QLprof )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,           INTENT( IN  ) :: n
TYPE( MetQLprof ), INTENT( OUT ) :: QLprof

INTEGER alloc_stat

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMallocQLprof = SWIMfailure

ALLOCATE( QLprof%QQ(n),QLprof%SL(n),QLprof%Tpot(n),QLprof%Press(n), STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMallocQLprof'
  error%Message = 'Error allocating met QQ Model profile arrays'
  error%Inform  = ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF

SWIMallocQLprof = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMallocTerrain( grid )

!------ Allocate terrain and landuse arrays

USE SWIM_fi
USE SWIMparam_fd

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER alloc_stat, n

CHARACTER(128), EXTERNAL :: ArraySizeStr

!------ Initialize failure

SWIMallocTerrain = SWIMfailure

!------ Allocate based on full grid size

IF( ASSOCIATED(grid%terrain%H ) )DEALLOCATE( grid%terrain%H, STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Hx) )DEALLOCATE( grid%terrain%Hx,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Hy) )DEALLOCATE( grid%terrain%Hy,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%D ) )DEALLOCATE( grid%terrain%D, STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Du) )DEALLOCATE( grid%terrain%Du,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Dv) )DEALLOCATE( grid%terrain%Dv,STAT=alloc_stat )

IF( ASSOCIATED(grid%landcover%roughness) )DEALLOCATE( grid%landcover%roughness,STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%canopyHt ) )DEALLOCATE( grid%landcover%canopyHt, STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%alpha    ) )DEALLOCATE( grid%landcover%alpha,    STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%Bowen    ) )DEALLOCATE( grid%landcover%Bowen,    STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%albedo   ) )DEALLOCATE( grid%landcover%albedo,   STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%kbl      ) )DEALLOCATE( grid%landcover%kbl,      STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%LandUse  ) )DEALLOCATE( grid%landcover%LandUse,  STAT=alloc_stat )

n = grid%nXY

ALLOCATE( grid%terrain%H(n),grid%terrain%Hx(n),grid%terrain%Hy(n), &
          grid%terrain%D(n),grid%terrain%Du(n),grid%terrain%Dv(n), &
          grid%landcover%roughness(n),grid%landcover%canopyHt(n),  &
          grid%landcover%alpha(n),grid%landcover%Bowen(n),  &
          grid%landcover%albedo(n),grid%landcover%kbl(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMallocTerrain'
  error%Message = 'Error allocating terrain/landuse arrays'
  error%Inform  = ArraySizeStr( 2,(/grid%nX,grid%nY/) )
  GOTO 9999
END IF

IF( BTEST(grid%type,GTB_LANDUSE) )THEN
  ALLOCATE( grid%landcover%LandUse(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number   = UK_ERROR
    error%Routine = 'SWIMallocTerrain'
    error%Message = 'Error allocating LandUse array'
    error%Inform  =  ArraySizeStr(2,(/grid%nX,grid%nY/))
    GOTO 9999
  END IF
END IF

!------ SWIMming Success

SWIMallocTerrain = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdealloc3dField( field3D )

!------ Deallocate 3d met field arrays

USE SWIM_fi
USE SWIMparam_fd

TYPE( MetMean3D ), INTENT( INOUT ) :: field3D

INTEGER alloc_stat

IF( ASSOCIATED(field3D%U)     )DEALLOCATE( field3D%U,    STAT=alloc_stat )
IF( ASSOCIATED(field3D%V)     )DEALLOCATE( field3D%V,    STAT=alloc_stat )
IF( ASSOCIATED(field3D%dU2)   )DEALLOCATE( field3D%dU2,  STAT=alloc_stat )
IF( ASSOCIATED(field3D%W)     )DEALLOCATE( field3D%W,    STAT=alloc_stat )
IF( ASSOCIATED(field3D%Tpot)  )DEALLOCATE( field3D%Tpot ,STAT=alloc_stat )
IF( ASSOCIATED(field3D%Press) )DEALLOCATE( field3D%Press,STAT=alloc_stat )
IF( ASSOCIATED(field3D%Humid) )DEALLOCATE( field3D%Humid,STAT=alloc_stat )
IF( ASSOCIATED(field3D%Qcloud))DEALLOCATE( field3D%Qcloud,STAT=alloc_stat )
IF( ASSOCIATED(field3D%Z)     )DEALLOCATE( field3D%Z,    STAT=alloc_stat )

SWIMdealloc3dField = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocBLParam( BL )

!------ Deallocate input boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

TYPE( MetBLparam ), INTENT( INOUT ) :: BL

INTEGER alloc_stat

IF( ASSOCIATED(BL%zi)       )DEALLOCATE( BL%zi,STAT=alloc_stat )
IF( ASSOCIATED(BL%HeatFlux) )DEALLOCATE( BL%HeatFlux,STAT=alloc_stat )
IF( ASSOCIATED(BL%ustr)     )DEALLOCATE( BL%ustr,STAT=alloc_stat )
IF( ASSOCIATED(BL%invMOL)   )DEALLOCATE( BL%invMOL,STAT=alloc_stat )
IF( ASSOCIATED(BL%cc)       )DEALLOCATE( BL%cc,STAT=alloc_stat )
IF( ASSOCIATED(BL%prcp)     )DEALLOCATE( BL%prcp,STAT=alloc_stat )

IF( ASSOCIATED(BL%rainprb) )DEALLOCATE( BL%rainprb,STAT=alloc_stat )
IF( ASSOCIATED(BL%zruf  )  )DEALLOCATE( BL%zruf,STAT=alloc_stat )

!------ SWIMming Success

SWIMdeallocBLParam = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocQLprof( QL,nextQL )

!------ Deallocate model turblence profiles

USE SWIM_fi
USE SWIMparam_fd

TYPE( MetQLprof ), INTENT( INOUT ) :: QL, nextQL

INTEGER alloc_stat

IF( ASSOCIATED(QL%QQ)    )DEALLOCATE( QL%QQ,STAT=alloc_stat )
IF( ASSOCIATED(QL%SL)    )DEALLOCATE( QL%SL,STAT=alloc_stat )
IF( ASSOCIATED(QL%Tpot)  )DEALLOCATE( QL%Tpot,STAT=alloc_stat )
IF( ASSOCIATED(QL%Press) )DEALLOCATE( QL%Press,STAT=alloc_stat )

IF( ASSOCIATED(nextQL%QQ)    )DEALLOCATE( nextQL%QQ,STAT=alloc_stat )
IF( ASSOCIATED(nextQL%SL)    )DEALLOCATE( nextQL%SL,STAT=alloc_stat )
IF( ASSOCIATED(nextQL%Tpot)  )DEALLOCATE( nextQL%Tpot,STAT=alloc_stat )
IF( ASSOCIATED(nextQL%Press) )DEALLOCATE( nextQL%Press,STAT=alloc_stat )

!------ SWIMming Success

SWIMdeallocQLprof = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocBLaux( BLaux )

!------ Deallocate auxilliary (not input) boundary layer arrays

USE SWIM_fi
USE SWIMparam_fd

TYPE( MetBLaux ), INTENT( INOUT ) :: BLaux

INTEGER alloc_stat

IF( ASSOCIATED(BLaux%zsl)     )DEALLOCATE( BLaux%zsl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%usl)     )DEALLOCATE( BLaux%usl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%vsl)     )DEALLOCATE( BLaux%vsl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%ubl)     )DEALLOCATE( BLaux%ubl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%vbl)     )DEALLOCATE( BLaux%vbl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%tbl)     )DEALLOCATE( BLaux%tbl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%pbl)     )DEALLOCATE( BLaux%pbl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%wspd2)   )DEALLOCATE( BLaux%wspd2,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%ustr2)   )DEALLOCATE( BLaux%ustr2,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%wstr2)   )DEALLOCATE( BLaux%wstr2,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%uu_sh)   )DEALLOCATE( BLaux%uu_sh,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%uu_buoy) )DEALLOCATE( BLaux%uu_buoy,STAT=alloc_stat )
IF( ASSOCIATED(BLaux%sbl)     )DEALLOCATE( BLaux%sbl,    STAT=alloc_stat )
IF( ASSOCIATED(BLaux%aDiff)   )DEALLOCATE( BLaux%aDiff,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%BDiff)   )DEALLOCATE( BLaux%bDiff,  STAT=alloc_stat )
IF( ASSOCIATED(BLaux%qq)      )DEALLOCATE( BLaux%qq,     STAT=alloc_stat )

!------ SWIMming Success

SWIMdeallocBLaux = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocObsWt( obsWt )

!------ Deallocate observation interpolation weight arrays

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetObsWt ), INTENT( INOUT ) :: obsWt

INTEGER alloc_stat

DEALLOCATE( obsWt%su,obsWt%sv,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%st)    )DEALLOCATE( obsWt%st,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%sp)    )DEALLOCATE( obsWt%sp,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%sh)    )DEALLOCATE( obsWt%sh,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%sqc)   )DEALLOCATE( obsWt%sqc,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%szi)   )DEALLOCATE( obsWt%szi,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%shflx) )DEALLOCATE( obsWt%shflx,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%sustr) )DEALLOCATE( obsWt%sustr,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%smol)  )DEALLOCATE( obsWt%smol,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%sprcp) )DEALLOCATE( obsWt%sprcp,STAT=alloc_stat )
IF( ASSOCIATED(obsWt%scc)   )DEALLOCATE( obsWt%scc,STAT=alloc_stat )

!------ SWIMming Success

SWIMdeallocObsWt = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocGrid( grid )

!------ Deallocate grid arrays, including terrain, landuse and McWif

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER alloc_stat, i

IF( ASSOCIATED(grid%Z)       )DEALLOCATE( grid%Z,      STAT=alloc_stat )
IF( ASSOCIATED(grid%Zw)      )DEALLOCATE( grid%Zw,     STAT=alloc_stat )
IF( ASSOCIATED(grid%lon)     )DEALLOCATE( grid%lon,    STAT=alloc_stat )
IF( ASSOCIATED(grid%lat)     )DEALLOCATE( grid%lat,    STAT=alloc_stat )
IF( ASSOCIATED(grid%sunrise) )DEALLOCATE( grid%sunrise,STAT=alloc_stat )
IF( ASSOCIATED(grid%sunset)  )DEALLOCATE( grid%sunset, STAT=alloc_stat )
IF( ASSOCIATED(grid%sunfac)  )DEALLOCATE( grid%sunfac, STAT=alloc_stat )


IF( BTEST(grid%type,GTB_Z3D) )THEN
  IF( BTEST(grid%type,GTB_SIGMA) )THEN
    IF( ASSOCIATED(grid%sigma%Z)  )DEALLOCATE( grid%sigma%Z,   STAT=alloc_stat )
    IF( ASSOCIATED(grid%sigma%Zw) )DEALLOCATE( grid%sigma%Zw,  STAT=alloc_stat )
  ELSE
    IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
      IF( BTEST(grid%type,GTB_Z3DW) )THEN
        IF( ASSOCIATED(grid%sigma%Z) )DEALLOCATE( grid%sigma%Z,STAT=alloc_stat )
      ELSE
        IF( ASSOCIATED(grid%sigma%Zw) )DEALLOCATE( grid%sigma%Zw,STAT=alloc_stat )
      END IF
    END IF
  END IF
END IF
IF( ASSOCIATED(grid%sigma%Psrf) )DEALLOCATE( grid%sigma%Psrf,STAT=alloc_stat )
IF( ASSOCIATED(grid%sigma%Px)   )DEALLOCATE( grid%sigma%Px,  STAT=alloc_stat )
IF( ASSOCIATED(grid%sigma%Py)   )DEALLOCATE( grid%sigma%Py,  STAT=alloc_stat )

IF( ASSOCIATED(grid%terrain%H ) )DEALLOCATE( grid%terrain%H, STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Hx) )DEALLOCATE( grid%terrain%Hx,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Hy) )DEALLOCATE( grid%terrain%Hy,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%D ) )DEALLOCATE( grid%terrain%D, STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Du) )DEALLOCATE( grid%terrain%Du,STAT=alloc_stat )
IF( ASSOCIATED(grid%terrain%Dv) )DEALLOCATE( grid%terrain%Dv,STAT=alloc_stat )

IF( ASSOCIATED(grid%landcover%roughness) )DEALLOCATE( grid%landcover%roughness,STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%canopyHt ) )DEALLOCATE( grid%landcover%canopyHt, STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%alpha    ) )DEALLOCATE( grid%landcover%alpha,    STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%Bowen    ) )DEALLOCATE( grid%landcover%Bowen,    STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%albedo   ) )DEALLOCATE( grid%landcover%albedo,   STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%kbl      ) )DEALLOCATE( grid%landcover%kbl,      STAT=alloc_stat )
IF( ASSOCIATED(grid%landcover%LandUse  ) )DEALLOCATE( grid%landcover%LandUse,  STAT=alloc_stat )

IF( ASSOCIATED(grid%McWIF%z)        )DEALLOCATE( grid%McWIF%z,       STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%zt)       )DEALLOCATE( grid%McWIF%zt,      STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%dzi)      )DEALLOCATE( grid%McWIF%dzi,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%dzti)     )DEALLOCATE( grid%McWIF%dzti,    STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%alphaFFT) )DEALLOCATE( grid%McWIF%alphaFFT,STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%cza)      )DEALLOCATE( grid%McWIF%cza,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%czb)      )DEALLOCATE( grid%McWIF%czb,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%xevec)    )DEALLOCATE( grid%McWIF%xevec,   STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%yevec)    )DEALLOCATE( grid%McWIF%yevec,   STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%xsn)      )DEALLOCATE( grid%McWIF%xsn,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%ysn)      )DEALLOCATE( grid%McWIF%ysn,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%xcs)      )DEALLOCATE( grid%McWIF%xcs,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%ycs)      )DEALLOCATE( grid%McWIF%ycs,     STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%xeval)    )DEALLOCATE( grid%McWIF%xeval,   STAT=alloc_stat )
IF( ASSOCIATED(grid%McWIF%yeval)    )DEALLOCATE( grid%McWIF%yeval,   STAT=alloc_stat )

IF( ASSOCIATED(grid%Smooth%xSmooth) )THEN
  DO i = 1,grid%nX
    DEALLOCATE( grid%Smooth%xSmooth(i)%ip,grid%Smooth%xSmooth(i)%wt,STAT=alloc_stat )
    NULLIFY( grid%Smooth%xSmooth(i)%ip,grid%Smooth%xSmooth(i)%wt )
  END DO
  DO i = 1,grid%nY
    DEALLOCATE( grid%Smooth%ySmooth(i)%ip,grid%Smooth%ySmooth(i)%wt,STAT=alloc_stat )
    NULLIFY( grid%Smooth%ySmooth(i)%ip,grid%Smooth%ySmooth(i)%wt )
  END DO
  DEALLOCATE( grid%Smooth%xSmooth,grid%Smooth%ySmooth,STAT=alloc_stat )
  NULLIFY( grid%Smooth%xSmooth,grid%Smooth%ySmooth )
END IF

IF( ASSOCIATED(grid%Smooth%xSmoothU) )THEN
  DO i = 1,grid%nX
    DEALLOCATE( grid%Smooth%xSmoothU(i)%ip,grid%Smooth%xSmoothU(i)%wt,STAT=alloc_stat )
    NULLIFY( grid%Smooth%xSmoothU(i)%ip,grid%Smooth%xSmoothU(i)%wt )
  END DO
  DO i = 1,grid%nY
    DEALLOCATE( grid%Smooth%ySmoothV(i)%ip,grid%Smooth%ySmoothV(i)%wt,STAT=alloc_stat )
    NULLIFY( grid%Smooth%ySmoothV(i)%ip,grid%Smooth%ySmoothV(i)%wt )
  END DO
  DEALLOCATE( grid%Smooth%xSmoothU,grid%Smooth%ySmoothV,STAT=alloc_stat )
  NULLIFY( grid%Smooth%xSmoothU,grid%Smooth%ySmoothV )
END IF

!------ SWIMming Success

SWIMdeallocGrid = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMnullifyGrid( grid )

!------ Nullify pointers for grid structure

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

CALL nullifyMetGrid( grid )

SWIMnullifyGrid = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocGridSrc( src )

USE SWIM_fi
USE SWIMparam_fd

TYPE( GridSrc ), INTENT( INOUT ) :: src

INTEGER alloc_stat, irv

INTEGER, EXTERNAL :: SWIMdeallocIntrpFac

IF( ASSOCIATED(src%Source)  )DEALLOCATE( src%Source,STAT=alloc_stat )
IF( ASSOCIATED(src%Z)       )DEALLOCATE( src%Z,STAT=alloc_stat )
IF( ASSOCIATED(src%Zw)      )DEALLOCATE( src%Zw,STAT=alloc_stat )
IF( ASSOCIATED(src%P)       )DEALLOCATE( src%P,STAT=alloc_stat )
IF( ASSOCIATED(src%var2dID) )DEALLOCATE( src%var2dID,STAT=alloc_stat )
IF( ASSOCIATED(src%var3dID) )DEALLOCATE( src%var3dID,STAT=alloc_stat )
IF( ASSOCIATED(src%Conv2d)  )DEALLOCATE( src%Conv2d,STAT=alloc_stat )
IF( ASSOCIATED(src%Conv3d)  )DEALLOCATE( src%Conv3d,STAT=alloc_stat )
IF( ASSOCIATED(src%iStg3d)     )DEALLOCATE( src%iStg3d,STAT=alloc_stat )
IF( ASSOCIATED(src%iStg2d)     )DEALLOCATE( src%iStg2d,STAT=alloc_stat )
IF( ASSOCIATED(src%Var3dShift) )DEALLOCATE( src%Var3dShift,STAT=alloc_stat )
IF( ASSOCIATED(src%Var2dShift) )DEALLOCATE( src%Var2dShift,STAT=alloc_stat )

IF( ASSOCIATED(src%ReadField%U)     )DEALLOCATE( src%ReadField%U    ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%V)     )DEALLOCATE( src%ReadField%V    ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%W)     )DEALLOCATE( src%ReadField%W    ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%Tpot)  )DEALLOCATE( src%ReadField%Tpot ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%Humid) )DEALLOCATE( src%ReadField%Humid,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%Press) )DEALLOCATE( src%ReadField%Press,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadField%Qcloud))DEALLOCATE( src%ReadField%Qcloud,STAT=alloc_stat )

IF( ASSOCIATED(src%ReadBL%zi)       )DEALLOCATE( src%ReadBL%zi      ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadBL%HeatFlux) )DEALLOCATE( src%ReadBL%HeatFlux,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadBL%invMOL)   )DEALLOCATE( src%ReadBL%invMOL  ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadBL%prcp)     )DEALLOCATE( src%ReadBL%prcp    ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadBL%cc )      )DEALLOCATE( src%ReadBL%cc      ,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadBL%ustr)     )DEALLOCATE( src%ReadBL%ustr    ,STAT=alloc_stat )

IF( ASSOCIATED(src%ReadLSV%UU) )DEALLOCATE( src%ReadLSV%UU,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadLSV%VV) )DEALLOCATE( src%ReadLSV%VV,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadLSV%UV) )DEALLOCATE( src%ReadLSV%UV,STAT=alloc_stat )
IF( ASSOCIATED(src%ReadLSV%SL) )DEALLOCATE( src%ReadLSV%SL,STAT=alloc_stat )

irv = SWIMdeallocIntrpFac( src%IntrpFac )

IF( src%unit > 0 )CLOSE(src%unit,IOSTAT=alloc_stat)

SWIMdeallocGridSrc = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMdeallocIntrpFac( IntrpFac )

USE SWIM_fi
USE SWIMparam_fd

TYPE( GridIntrp ), INTENT( INOUT ) :: IntrpFac

INTEGER alloc_stat

IF( ASSOCIATED(IntrpFac%mh)        )DEALLOCATE( IntrpFac%mh ,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mhu)       )DEALLOCATE( IntrpFac%mhu,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mhV)       )DEALLOCATE( IntrpFac%mhv,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mz)        )DEALLOCATE( IntrpFac%mz ,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mzu)       )DEALLOCATE( IntrpFac%mzu,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mzv)       )DEALLOCATE( IntrpFac%mzv,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%mzw)       )DEALLOCATE( IntrpFac%mzw,      STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%DiffLandU) )DEALLOCATE( IntrpFac%DiffLandU,STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%DiffLandV) )DEALLOCATE( IntrpFac%DiffLandV,STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%zbluS)     )DEALLOCATE( IntrpFac%zbluS,    STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%zblvS)     )DEALLOCATE( IntrpFac%zblvS,    STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%zblu)      )DEALLOCATE( IntrpFac%zblu,     STAT=alloc_stat )
IF( ASSOCIATED(IntrpFac%zblv)      )DEALLOCATE( IntrpFac%zblv,     STAT=alloc_stat )

SWIMdeallocIntrpFac = SWIMresult

RETURN
END
!==============================================================================

INTEGER FUNCTION SWIMdeallocVariance( var )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetVariance ), INTENT( INOUT ) :: var

INTEGER alloc_stat

SWIMdeallocVariance = SWIMfailure

IF( ASSOCIATED(var%UU) )DEALLOCATE( var%UU,STAT=alloc_stat )
IF( ASSOCIATED(var%VV) )DEALLOCATE( var%VV,STAT=alloc_stat )
IF( ASSOCIATED(var%UV) )DEALLOCATE( var%UV,STAT=alloc_stat )
IF( ASSOCIATED(var%SL) )DEALLOCATE( var%SL,STAT=alloc_stat )

SWIMdeallocVariance = SWIMresult

RETURN
END
