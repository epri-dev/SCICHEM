INTEGER FUNCTION SWIMreallocMetField( n )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n

INTEGER alloc_stat, i, m

TYPE( MetField ), DIMENSION(:), ALLOCATABLE, TARGET :: temField

IF( n == 0 )THEN
  SWIMreallocMetField = SWIMresult
  RETURN
ELSE
  SWIMreallocMetField = SWIMfailure
END IF

IF( numFieldMax > 0 )THEN

  ALLOCATE( temField(numFieldMax),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMreallocMetField'
    error%Message = 'Error allocating temporary fields'
    GOTO 9999
  END IF

  temField = field

  DO i = 1,numField
    IF( field(i)%grid%numMG > 0 )temField(i)%grid%MGgrid%prevMGgrid => temField(i)%grid
  END DO

  DEALLOCATE( field,STAT=alloc_stat )

END IF

m = ABS(n)

numFieldMax = numFieldMax + m
numField    = numField + MAX(n,0)

ALLOCATE( field(numFieldMax),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMreallocMetField'
  error%Message = 'Error allocating met fields'
  GOTO 9999
END IF

IF( numFieldMax-m > 0 )THEN

  field(1:numFieldMax-m) = temField

  DO i = 1,numFieldMax-m
    IF( field(i)%grid%numMG > 0 )field(i)%grid%MGgrid%prevMGgrid => field(i)%grid
  END DO

END IF

!----- Nullify all pointers in newly allocated fields

DO i = numFieldMax-m+1,numFieldMax

  CALL nullifyMetGrid( field(i)%grid )
  CALL nullifyGridSrc( field(i)%gridSource )
  CALL nullifyMetMean3D( field(i)%Field )
  CALL nullifyMetMean3D( field(i)%NextField )
  CALL nullifyMetBLparam( field(i)%BL )
  CALL nullifyMetBLparam( field(i)%NextBL )
  CALL nullifyMetBLaux( field(i)%BLaux )
  CALL nullifyMetVariance( field(i)%LSV )
  CALL nullifyMetVariance( field(i)%NextLSV )
  CALL nullifyMetQLprof( field(i)%QLprof )
  CALL nullifyMetQLprof( field(i)%NextQLprof )
  CALL nullifyMetObsWt( field(i)%obsWt )
  NULLIFY( field(i)%NextZ )

  field(i)%type    = 0
  field(i)%unitOut = 0

  field(i)%nObsSource = 0; field(i)%iObsSource = 0

  CALL nullifyMetMean3D( field(i)%Field1 )
  CALL nullifyMetMean3D( field(i)%Field2 )
  CALL nullifyMetBLparam( field(i)%BL1 )
  CALL nullifyMetBLparam( field(i)%BL2 )
  CALL nullifyMetVariance( field(i)%LSV1 )
  CALL nullifyMetVariance( field(i)%LSV2 )

END DO

IF( ALLOCATED(temField) )DEALLOCATE( temField,STAT=alloc_stat )

SWIMreallocMetField = SWIMresult

9999 CONTINUE

RETURN
    END

!==============================================================================

SUBROUTINE nullifyMetGrid( grid )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTERFACE
  SUBROUTINE nullifyMetGrid2( gridx,gridp )
    USE SWIMmetField_fd
    TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
    TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp
  END SUBROUTINE nullifyMetGrid2
END INTERFACE

CALL nullifyMetGrid2( gridx=grid )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetGrid2( gridx,gridp )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp

TYPE( MetGrid ), POINTER :: grid

IF( PRESENT(gridx) )THEN
  grid => gridx
ELSE IF( PRESENT(gridp) )THEN
  grid => gridp
ELSE
  RETURN
END IF

NULLIFY( grid%Z,grid%Zw )
NULLIFY( grid%lat,grid%lon )
NULLIFY( grid%sunrise,grid%sunset,grid%sunfac )

NULLIFY( grid%terrain%H,grid%terrain%Hx,grid%terrain%Hy )
NULLIFY( grid%terrain%D,grid%terrain%Du,grid%terrain%Dv )

NULLIFY( grid%landcover%roughness )
NULLIFY( grid%landcover%canopyHt )
NULLIFY( grid%landcover%alpha )
NULLIFY( grid%landcover%Bowen )
NULLIFY( grid%landcover%albedo )
NULLIFY( grid%landcover%kbl )
NULLIFY( grid%landcover%LandUse )

NULLIFY( grid%McWIF%z,grid%McWIF%zt )
NULLIFY( grid%McWIF%dzi,grid%McWIF%dzti )
NULLIFY( grid%McWIF%alphaFFT )
NULLIFY( grid%McWIF%cza,grid%McWIF%czb )
NULLIFY( grid%McWIF%xevec,grid%McWIF%yevec )
NULLIFY( grid%McWIF%xsn,grid%McWIF%ysn )
NULLIFY( grid%McWIF%xcs,grid%McWIF%ycs )
NULLIFY( grid%McWIF%xeval,grid%McWIF%yeval )
NULLIFY( grid%McWIF%alphaU,grid%McWIF%alphaV )

NULLIFY( grid%Smooth%xSmooth,grid%Smooth%ySmooth )
NULLIFY( grid%Smooth%xSmoothU,grid%Smooth%ySmoothV )

NULLIFY( grid%sigma%Z )
NULLIFY( grid%sigma%Zw )
NULLIFY( grid%sigma%Psrf)
NULLIFY( grid%sigma%Px )
NULLIFY( grid%sigma%Py )

NULLIFY( grid%MGgrid )
NULLIFY( grid%prevMGgrid )

grid%Hmin  = 0.
grid%Hmax  = 0.
grid%numMG = 0

RETURN
END

!==============================================================================

SUBROUTINE nullifyGridSrc( src )

USE SWIMmetField_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src

NULLIFY( src%Source )
NULLIFY( src%Z )
NULLIFY( src%Zw )
NULLIFY( src%P )
NULLIFY( src%var2dID )
NULLIFY( src%var3dID )
NULLIFY( src%Conv2d )
NULLIFY( src%Conv3d )
NULLIFY( src%var2dName )
NULLIFY( src%var3dName )
NULLIFY( src%iStg3d )
NULLIFY( src%iStg2d )
NULLIFY( src%Var3dShift )
NULLIFY( src%Var2dShift )

CALL nullifyGridIntrp( src%IntrpFac )

CALL nullifyMetMean3D( src%ReadField )

CALL nullifyMetBLparam( src%ReadBL )

CALL nullifyMetVariance( src%ReadLSV )

src%nSource   = 0
src%lVertFlip = .FALSE.

src%nStg3d = 0
src%nStg2d = 0
src%unit   = -1
src%type   = IZERO8

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetMean3D( Field )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: Field

NULLIFY( Field%U     )
NULLIFY( Field%V     )
NULLIFY( Field%W     )
NULLIFY( Field%Tpot  )
NULLIFY( Field%Humid )
NULLIFY( Field%Press )
NULLIFY( Field%Qcloud)
NULLIFY( Field%dU2   )
NULLIFY( Field%Z     )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetBLparam( BL )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetBLparam ), INTENT( INOUT ) :: BL

NULLIFY( BL%zi       )
NULLIFY( BL%HeatFlux )
NULLIFY( BL%invMOL   )
NULLIFY( BL%prcp     )
NULLIFY( BL%rainprb  )
NULLIFY( BL%cc       )
NULLIFY( BL%zruf     )
NULLIFY( BL%ustr     )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetBLaux( BLaux )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetBLaux ), INTENT( INOUT ) :: BLaux

NULLIFY( BLaux%zsl )
NULLIFY( BLaux%usl )
NULLIFY( BLaux%vsl )
NULLIFY( BLaux%ubl )
NULLIFY( BLaux%vbl )
NULLIFY( BLaux%tbl )
NULLIFY( BLaux%pbl )
NULLIFY( BLaux%wspd2 )
NULLIFY( BLaux%ustr2 )
NULLIFY( BLaux%wstr2 )
NULLIFY( BLaux%aDiff )
NULLIFY( BLaux%bDiff )
NULLIFY( BLaux%uu_sh )
NULLIFY( BLaux%uu_buoy )
NULLIFY( BLaux%sbl )
NULLIFY( BLaux%qq )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetVariance( var )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetVariance ), INTENT( INOUT ) :: var

NULLIFY( var%UU )
NULLIFY( var%VV )
NULLIFY( var%UV )
NULLIFY( var%SL )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetQLprof( QL )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetQLprof ), INTENT( INOUT ) :: QL

NULLIFY( QL%QQ )
NULLIFY( QL%SL )
NULLIFY( QL%Tpot )
NULLIFY( QL%Press )

RETURN
END

!==============================================================================

SUBROUTINE nullifyGridIntrp( Intrp )

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( GridIntrp ), INTENT( INOUT ) :: Intrp

NULLIFY( Intrp%mh  )
NULLIFY( Intrp%mhu )
NULLIFY( Intrp%mhv )
NULLIFY( Intrp%mz  )
NULLIFY( Intrp%mzu )
NULLIFY( Intrp%mzv )
NULLIFY( Intrp%mzw )
NULLIFY( Intrp%DiffLandU )
NULLIFY( Intrp%DiffLandV )
NULLIFY( Intrp%zbluS )
NULLIFY( Intrp%zblvS )
NULLIFY( Intrp%zblu )
NULLIFY( Intrp%zblv )

RETURN
END

!==============================================================================

SUBROUTINE nullifyMetObsWt( obsWt )


USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetObsWt ), INTENT( INOUT ) :: obsWt

NULLIFY( obsWt%su )
NULLIFY( obsWt%sv )
NULLIFY( obsWt%st )
NULLIFY( obsWt%sh )
NULLIFY( obsWt%sp )
NULLIFY( obsWt%sqc )
NULLIFY( obsWt%szi   )
NULLIFY( obsWt%shflx )
NULLIFY( obsWt%sustr )
NULLIFY( obsWt%smol  )
NULLIFY( obsWt%sprcp )
NULLIFY( obsWt%scc   )

RETURN
END
