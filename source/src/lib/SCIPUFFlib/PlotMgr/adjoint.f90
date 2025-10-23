SUBROUTINE CreateAdjointField( Field,ClassData,sblk,spuff,stype,slice,grdI )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE srfparam_fd
USE surface_fi
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd
USE adjoint_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL, DIMENSION(*),                 INTENT( IN    ) :: ClassData    !Additional Class data
TYPE( sfield_block ), DIMENSION(*), INTENT( OUT   ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT   ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: stype ! dezone types
TYPE( slice_str    ),               INTENT( OUT   ) :: slice ! slice definition
INTEGER,                            INTENT( OUT )   :: grdI

INTEGER nClassData, irv, alloc_stat, iTime

REAL, DIMENSION(7)              :: AdjDomain
REAL, DIMENSION(:), ALLOCATABLE :: PlotData    !Plot domain data

INTEGER, EXTERNAL :: AllocateMaxLoc, SAG_RmvGrdStr

!--- Initialize adjoint data

grdI = -1

CALL InitAdjointPlot( Field )
IF( nError /= NO_ERROR )GOTO 9999

IF( adjClass == HP_ADJOINT )THEN
  AdjDomain(CD_XMIN) = ClassData(CD_XMIN)
  AdjDomain(CD_XMAX) = ClassData(CD_XMAX)
  AdjDomain(CD_YMIN) = ClassData(CD_YMIN)
  AdjDomain(CD_YMAX) = ClassData(CD_YMAX)
  IF( Field%category == HP_HSLICE )THEN
    AdjDomain(CD_ZMIN) = ClassData(CD_ZMIN) !Slice height
  ELSE IF( Field%category == HP_VSLICE )THEN
    AdjDomain(CD_ZMIN) = ClassData(CD_ZMIN)
    AdjDomain(CD_ZMAX) = ClassData(CD_ZMAX)
    AdjDomain(CD_VRES) = ClassData(CD_VRES) !No. of levels
  END IF
END IF

SELECT CASE( Field%category )
	CASE( HP_VSLICE )
    nClassData = CD_NUM_VSLICE + 1

	CASE( HP_HSLICE )
    nClassData = CD_NUM_HSLICE + 1

  CASE( HP_SSLICE )
    nClassData = CD_NUM_SSLICE + 1

  CASE DEFAULT
    nClassData = 1

END SELECT

nDim    = 0
DimX(1) = 0
nTime   = nPuffTime
adjTime => PuffTime

IF( adjChoice == ADJ_MAXLOC )THEN

  IF( adjClass == HP_ADJ_SFC )THEN
    nDim    = 1
    nTime   = nSrfTime
    DimX(1) = nTime
    adjTime => SrfTime
  END IF

  iMaxLoc = AllocateMaxLoc()
  IF( iMaxLoc <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateAdjointField'
    eMessage = 'Error allocating associated line plot storage'
    GOTO 9999
  END IF

  mlev = 0
  DO WHILE( nTime-1 > 2**(mlev+1) )
    mlev = mlev + 1
  END DO

  IF( adjClass == HP_ADJOINT )THEN
    CALL MaxAdjointPlot( Field,AdjDomain,ClassData(nClassdata),sblk,spuff,stype,slice,grdI )
  ELSE
    CALL MaxAdjointPlotCont( Field,ClassData(nClassdata),slice,-1,grdI )
  END IF
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  IF( adjClass == HP_ADJOINT )THEN

    IF( FastSearch )THEN

      FillGrid = .FALSE.
      CALL CreateAdjointPlot( Field,AdjDomain,ClassData(nClassData),Field%timeID, &
                              sblk,spuff,stype,slice,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

      ALLOCATE( PlotData(nClassData),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'CreateAdjointField'
        eMessage = 'Error allocating PlotData storage'
        GOTO 9999
      END IF

      PlotData(CD_ADJ_XMASK_MIN) = AdjMaxSearch%xMask(1)
      PlotData(CD_ADJ_XMASK_MAX) = AdjMaxSearch%xMask(2)
      PlotData(CD_ADJ_YMASK_MIN) = AdjMaxSearch%yMask(1)
      PlotData(CD_ADJ_YMASK_MAX) = AdjMaxSearch%yMask(2)

      IF( grdI /= 0 )irv = SAG_RmvGrdStr( grdI )

      FillGrid = .TRUE.
      grdI = -1
      CALL CreateAdjointPlot( Field,AdjDomain,PlotData(nClassData),Field%timeID, &
                              sblk,spuff,stype,slice,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

      DEALLOCATE( PlotData,STAT=alloc_stat )

    ELSE

      FillGrid = .TRUE.
      CALL CreateAdjointPlot( Field,AdjDomain,ClassData(nClassData),Field%timeID, &
                              sblk,spuff,stype,slice,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    END IF

  ELSE

    nDim    = 0
    DimX(1) = 0
    nTime   = nSrfTime
    adjTime => SrfTime

    iMaxLoc = AllocateMaxLoc()
    IF( iMaxLoc <= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'CreateAdjointField'
      eMessage = 'Error allocating associated line plot storage'
      GOTO 9999
    END IF

    iTime = Field%timeID
    CALL MaxAdjointPlotCont( Field,ClassData(nClassdata),slice,iTime,grdI )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

END IF

9999 CONTINUE

!--- Deallocate adjoint data

CALL ExitAdjointPlot()

RETURN
END

!=======================================================================

SUBROUTINE InitAdjointPlot( Field )

USE scipuff_fi
USE plotlist_fi
USE files_fi
USE adjoint_fi
USE field_fd
USE SCIPResults_fd

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( IN ) :: Field

INTEGER alloc_stat, i, j, k, imat, ikind
REAL    delxA, delyA, deltA, ubar, vbar, utot, xmap, ymap

LOGICAL, EXTERNAL :: IsNullSensor, IsSatSensor

!--- Initialize pointers
  NULLIFY(Amat)
  NULLIFY(Nmat)
  NULLIFY(Tmat)
  NULLIFY(Smat)
  NULLIFY(wt)
  NULLIFY(gamT)

!--- Set class/choice value for adjoint plot

adjClass  = ClassID(Field%class)
adjChoice = Field%choice - ntypm

IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN
  ikind = Field%kind - ClassChoiceComb(Field%class,Field%choice)%ikind + 1
ELSE
  ikind = 1
END IF

FastSearch = (ikind == 1)

Probabilistic = (t_avg == DEF_VAL_R) .AND. (adjClass == HP_ADJOINT .OR. adjClass == HP_ADJ_SFC)

ALLOCATE( adjI(ntypm),Amat(ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitAdjointPlot'
  eMessage = 'Error allocating adjoint slice ID storage'
  GOTO 9999
END IF

nNull = 0
nSat  = 0
DO i = 1,ntypm
  adjI(i) = -1
  IF ( IsNullSensor(material(i)%icls) )nNull = nNull + 1
  IF (  IsSatSensor(material(i)%icls) )nSat  = nSat  + 1
END DO

nTrigger = ntypm - nNull - nSat
nHit     = nTrigger + nSat

ALLOCATE( Nmat(nNull),Smat(nSat),Tmat(nHit),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitAdjointPlot'
  eMessage = 'Error allocating adjoint material storage'
  GOTO 9999
END IF

i = 0
j = 0
k = 0
DO imat = 1,ntypm
  IF( IsNullSensor(material(imat)%icls) )THEN
    j = j + 1
    Nmat(j)%imat = imat
    Nmat(j)%m => Amat(imat)
    Amat(imat)%trigger = .FALSE.
  ELSE IF( IsSatSensor(material(imat)%icls) )THEN
    k = k + 1
    Smat(k)%imat = imat
    Smat(k)%m => Amat(imat)
    Amat(imat)%trigger = .TRUE.
    Tmat(nTrigger+k) = Smat(k)
  ELSE
    i = i + 1
    Tmat(i)%imat = imat
    Tmat(i)%m => Amat(imat)
    Amat(imat)%trigger = .TRUE.
  END IF
END DO

ALLOCATE( AdjDistS(ntypm,ntypm),AdjDistT(ntypm,ntypm),AdjDistV(ntypm,ntypm), &
          STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( AdjHitDistS(ntypm,ntypm), &
                               AdjHitDistT(ntypm,ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Failed to allocate adjoint storage'
  eRoutine = 'InitAdjointPlot'
  GOTO 9999
END IF

DO i = 1,ntypm
  CALL mapfac( AdjMat(i)%xrel,AdjMat(i)%yrel,xmap,ymap )
  DO j = i,ntypm
    IF( i == j )THEN
      AdjDistS(i,i) = 0.0
      AdjDistT(i,i) = 0.0
      AdjDistV(i,i) = 0.0
    ELSE
      deltA = AdjMat(i)%trel-AdjMat(j)%trel
      IF( AdjMat(i)%umet == NOT_SET_R .OR. AdjMat(j)%umet == NOT_SET_R .OR. &
          AdjMat(i)%vmet == NOT_SET_R .OR. AdjMat(j)%vmet == NOT_SET_R )THEN
        utot = 0.0
      ELSE
        ubar  = 0.5*(AdjMat(i)%umet+AdjMat(j)%umet)
        vbar  = 0.5*(AdjMat(i)%vmet+AdjMat(j)%vmet)
        utot  = SQRT(ubar*ubar+vbar*vbar)
      END IF
      delxA = (AdjMat(i)%xrel-AdjMat(j)%xrel)/xmap
      delyA = (AdjMat(i)%yrel-AdjMat(j)%yrel)/ymap
      IF( utot > 0.0 )THEN
        AdjDistS(i,j) = ABS((ubar*delxA + vbar*delyA)/utot - utot*deltA)
        AdjDistT(i,j) = ABS(vbar*delxA - ubar*delyA)/utot
      ELSE
        AdjDistS(i,j) = SQRT(delxA*delxA + delyA*delyA)
        AdjDistT(i,j) = AdjDistS(i,j)
      END IF
      AdjDistV(i,j) = ABS(AdjMat(i)%zrel-AdjMat(j)%zrel)

      AdjDistS(j,i) = AdjDistS(i,j)
      AdjDistT(j,i) = AdjDistT(i,j)
      AdjDistV(j,i) = AdjDistV(i,j)

      AdjHitDistS(i,j) = AdjDistS(i,j)
      AdjHitDistS(j,i) = AdjHitDistS(i,j)

      AdjHitDistT(i,j) = AdjDistT(i,j)
      AdjHitDistT(j,i) = AdjHitDistT(i,j)

    END IF
  END DO
END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE MaxAdjointPlot( Field,AdjDomain,ClassData,sblk,spuff,stype,slice,grdI )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE srfparam_fd
USE surface_fi
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd
USE adjoint_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL, DIMENSION(*),                 INTENT( INOUT ) :: AdjDomain    !Spatial slice domain
REAL, DIMENSION(*),                 INTENT( IN    ) :: ClassData    !Source Estimation data
TYPE( sfield_block ), DIMENSION(*), INTENT( OUT   ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT   ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: stype ! dezone types
TYPE( slice_str    ),               INTENT( OUT   ) :: slice ! slice definition
INTEGER,                            INTENT( OUT   ) :: grdI  ! fieldID

INTEGER i, irv, im, ix, i1, il, ig, igm, alloc_stat
REAL    fmax, fmaxm, tMin, tMax

REAL, DIMENSION(:), ALLOCATABLE :: PlotData    !Plot domain data

LOGICAL, DIMENSION(:), ALLOCATABLE :: DoneID   !Flag to indicate time level has been computed

TYPE( AdjointMaxLocTime_str ), POINTER :: Aptr

REAL,    DIMENSION(:), POINTER :: tLoc         !Time (relative to Max)
REAL,    DIMENSION(:), POINTER :: fLoc         !LocMax value
REAL,    DIMENSION(:), POINTER :: fMass        !Mass Estimate value

INTEGER, EXTERNAL :: SAG_RmvGrdStr

grdI = -1

IF( FastSearch )THEN
  FillGrid = .FALSE.
  AdjMaxSearch%SrcFunc = -999.99
ELSE
  FillGrid = .TRUE.
END IF

Aptr  => AdjLocMax(iMaxLoc)
tLoc  => Aptr%tLoc
fLoc  => Aptr%fLoc
fMass => Aptr%fMass

ALLOCATE( DoneID(nTime),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'MaxAdjointPlot'
  eMessage = 'Error allocating time flag storage'
  GOTO 9999
END IF

DoneID = .FALSE.

im = 1
CALL SetMaxLocInst( Field,AdjDomain,ClassData,im,sblk,spuff,stype,slice,igm,fmaxm )
IF( nError /= NO_ERROR )GOTO 9999

DoneID(im) = .TRUE.

ix = MAX(1,2**(mlev-2))

i = 2

DO WHILE( i < ix+1 )

  CALL SetMaxLocInst( Field,AdjDomain,ClassData,i,sblk,spuff,stype,slice,ig,fmax )
  IF( nError /= NO_ERROR )GOTO 9999

  DoneID(i) = .TRUE.

  IF( fmax > fmaxm )THEN
    fmaxm = fmax
    im    = i
    IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
    igm = ig
  ELSE IF( ig > 0 )THEN
    irv = SAG_RmvGrdStr( ig )
  END IF

  i = 2*i

END DO

DO i = ix+1,nTime,ix

  CALL SetMaxLocInst( Field,AdjDomain,ClassData,i,sblk,spuff,stype,slice,ig,fmax )
  IF( nError /= NO_ERROR )GOTO 9999

  DoneID(i) = .TRUE.

  IF( fmax > fmaxm )THEN
    fmaxm = fmax
    im    = i
    IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
    igm = ig
  ELSE IF( ig > 0 )THEN
    irv = SAG_RmvGrdStr( ig )
  END IF

END DO

DO il = 3,mlev

  ix = 2**(mlev-il)
  i1 = im - ix

  DO i = i1,i1+2*ix,2*ix

    IF( i > 0 .AND. i <= nTime )THEN
     IF( .NOT.DoneID(i) )THEN
        CALL SetMaxLocInst( Field,AdjDomain,ClassData,i,sblk,spuff,stype,slice,ig,fmax )
        IF( nError /= NO_ERROR )GOTO 9999
     ENDIF
    ELSE
      ig   = 0
      fmax = 0.0
    END IF

    IF( fmax > fmaxm )THEN
      fmaxm = fmax
      im    = i
      IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
      igm = ig
    ELSE IF( ig > 0 )THEN
      irv = SAG_RmvGrdStr( ig )
    END IF

  END DO

END DO

IF( FastSearch )THEN

  ALLOCATE( PlotData(4),STAT=alloc_stat )   ! nClassData
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'MaxAdjointPlot'
    eMessage = 'Error allocating PlotData storage'
    GOTO 9999
  END IF

  PlotData(CD_ADJ_XMASK_MIN) = AdjMaxSearch%xMask(1)
  PlotData(CD_ADJ_XMASK_MAX) = AdjMaxSearch%xMask(2)
  PlotData(CD_ADJ_YMASK_MIN) = AdjMaxSearch%yMask(1)
  PlotData(CD_ADJ_YMASK_MAX) = AdjMaxSearch%yMask(2)

  IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )

  FillGrid = .TRUE.

  igm = -1

  CALL CreateAdjointPlot( Field,AdjDomain,PlotData,AdjMaxSearch%TimeID,sblk,spuff,stype,slice,igm )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( AdjMaxSearch%TimeID == 0 )AdjMaxSearch%TimeID = 1
  Field%timeID   = AdjMaxSearch%TimeID
  Field%UserTime = adjTime(AdjMaxSearch%TimeID)%time%runTime

  DEALLOCATE( PlotData,STAT=alloc_stat )

ELSE

  Field%timeID   = im
  Field%UserTime = adjTime(im)%time%runTime

END IF

grdI = igm

Aptr%ID   = igm
Aptr%nLoc = nTime

tMin =  HUGE(1.0)
tMax = -HUGE(1.0)

DO i = 1,nTime
  IF( fLoc(i) >= 0.0 )THEN
    IF( fLoc(i) < 0.2*fLoc(im) .AND. fMass(i) > 10.0*fMass(im) )THEN
      fLoc(i) = -1.0
      tLoc(i) = NOT_SET_R
    ELSE
      tLoc(i) = adjTime(i)%time%runTime - adjTime(im)%time%runTime
      tMin = MIN(tMin,tLoc(i))
      tMax = MAX(tMax,tLoc(i))
    END IF
  ELSE
    fLoc(i) = -1.0
    tLoc(i) = NOT_SET_R
  END IF
  IF( fMass(i) < 0. )fMass(i) = 0.
END DO

IF( tMax-tMin < 1.0 )THEN
  DO i = 1,nTime
    IF( tLoc(i) /= NOT_SET_R )tLoc(i) = 60.0*tLoc(i)
  END DO
  Aptr%xLab = 't - '//TRIM(adjTime(im)%string)//' (mins)'
ELSE
  Aptr%xLab = 't - '//TRIM(adjTime(im)%string)//' (hrs)'
END IF

Aptr%MassUnit = TRIM(material(1)%unit)

9999 CONTINUE

IF( ALLOCATED(DoneID) )DEALLOCATE( DoneID,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetMaxLocInst( Field,AdjDomain,ClassData,i,sblk,spuff,stype,slice,ig,fmax )

USE error_fi
USE surface_fi
USE slice_fd
USE field_fd
USE adjoint_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL, DIMENSION(*),                 INTENT( INOUT ) :: AdjDomain    !Spatial slice domain
REAL, DIMENSION(*),                 INTENT( IN    ) :: ClassData    !Source Estimation data
TYPE( sfield_block ), DIMENSION(*), INTENT( OUT   ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT   ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: stype ! dezone types
INTEGER,                            INTENT( IN    ) :: i     ! timeID
TYPE( slice_str    ),               INTENT( OUT   ) :: slice ! slice definition
INTEGER,                            INTENT( OUT   ) :: ig    ! fieldID
REAL,                               INTENT( OUT   ) :: fmax  ! field max

INTEGER icell
REAL    mass

REAL, EXTERNAL :: AdjointMax

CALL CreateAdjointPlot( Field,AdjDomain,ClassData,i,sblk,spuff,stype,slice,ig )
IF( nError == NF_ERROR )THEN
  fmax = 0.0
  ig   = 0
  CALL init_error()
ELSE IF( nError /= NO_ERROR )THEN
  GOTO 9999
ELSE
  icell = -1
  fmax = AdjointMax( ig,1,icell )
  AdjLocMax(iMaxLoc)%fLoc(i) = fmax
  mass = AdjointMax( ig,2,icell )
  AdjLocMax(iMaxLoc)%fMass(i) = mass
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE MaxAdjointPlotCont( Field,ClassData,slice,iTime,grdI )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE srfparam_fd
USE surface_fi
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd
USE adjoint_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: Field
REAL, DIMENSION(*),     INTENT( IN    ) :: ClassData    !Source estimation info
TYPE( slice_str    ),   INTENT( OUT   ) :: slice ! slice definition
INTEGER,                INTENT( IN    ) :: iTime ! timeID
INTEGER,                INTENT( OUT   ) :: grdI  ! fieldID

INTEGER i, irv, im, ix, i1, il, ig, igm
INTEGER j, jm, jx, j1, jl, ij, ijm, jlev, iold
INTEGER alloc_stat
REAL    fmax, fmaxm, fmaxim, fmaxjm, tMin, tMax

REAL, DIMENSION(:), ALLOCATABLE :: PlotData    !Plot domain data

TYPE( AdjointMaxLocTime_str ), POINTER :: Aptr

REAL,    DIMENSION(:), POINTER :: tLoc         !Time (relative to Max)
REAL,    DIMENSION(:), POINTER :: fLoc         !LocMax value
REAL,    DIMENSION(:), POINTER :: fMass        !Mass Estimate value

INTEGER, EXTERNAL :: SAG_RmvGrdStr

grdI = -1

IF( FastSearch )THEN
  FillGrid             = .FALSE.
  AdjMaxSearch%SrcFunc = -999.99
ELSE
  FillGrid = .TRUE.
END IF

Aptr  => AdjLocMax(iMaxLoc)
tLoc  => Aptr%tLoc
fLoc  => Aptr%fLoc
fMass => Aptr%fMass

!--- Set initial start time for search

IF( iTime > 0 )THEN
  IF( iTime == 1 )THEN
    nError   = IV_ERROR
    eRoutine = 'MaxAdjointPlotCont'
    eMessage = 'Cannot create estimate for first stored time'
    GOTO 9999
  END IF
  im = iTime
ELSE
  im = 1 + 2**mlev
END IF

!--- Initialize field-ID and max field value for search

igm   = 0
fmaxm = -1.0

!--- Set initial duration for search

jlev = 0
DO WHILE( im-1 > 2**(jlev+1) )
  jlev = jlev + 1
END DO

jm = 1 + 2**jlev

!--- Create field for initial search time and duration

IF( jm <= im-1 )THEN

  CALL SetMaxLocCont( Field,ClassData,iTime,im,jm,slice,igm,fmaxm )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!--- fmaxm is overall field max, fmaxjm is max over duration for current start time

fmaxjm = fmaxm

!--- Search durations for initial search time

DO jl = 0,jlev

  jx = 2**(jlev-jl)
  j1 = jm - jx

  DO j = j1,j1+2*jx,2*jx

    IF( j > 0 .AND. j <= im-1 )THEN
      CALL SetMaxLocCont( Field,ClassData,iTime,im,j,slice,ig,fmax )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      ig   = 0
      fmax = 0.0
    END IF

    IF( fmax > fmaxjm )THEN
      fmaxjm = fmax
      jm     = j
    END IF

    IF( fmax > fmaxm )THEN
      fmaxm = fmax
      IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
      igm = ig
    ELSE IF( ig > 0 )THEN
      irv = SAG_RmvGrdStr( ig )
    END IF

  END DO

END DO

!--- Search over start times, if requested

IF( iTime <= 0 )THEN

  fmaxim = fmaxm

!--- Loop over levels for start time

  DO il = 0,mlev

    ix = 2**(mlev-il)
    i1 = im - ix

    iold = -999

!--- Check start times at this level

    DO i = i1,i1+2*ix,2*ix

      IF( i > 0 .AND. i <= nTime )THEN

!--- Set levels for duration search

        jlev = 0
        DO WHILE( i-1 > 2**(jlev+1) )
          jlev = jlev + 1
        END DO

!--- Initial duration

        jm = 1 + 2**jlev

        IF( jm <= i-1 )THEN

          CALL SetMaxLocCont( Field,ClassData,iTime,i,jm,slice,ig,fmaxjm )
          IF( nError /= NO_ERROR )GOTO 9999

          IF( fmaxjm > fmaxm )THEN
            fmaxm = fmaxjm
            IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
            igm = ig
          ELSE IF( ig > 0 )THEN
            irv = SAG_RmvGrdStr( ig )
          END IF

        ELSE

          fmaxjm = 0.0

        END IF

        j1 = 1

!--- Loop over duration levels

        DO jl = 0,jlev

          jx = 2**(jlev-jl)
          j1 = jm - jx

!--- Check durations at this level

          DO j = j1,j1+2*jx,2*jx

            IF( j > 0 .AND. j <= i-1 )THEN
              CALL SetMaxLocCont( Field,ClassData,iTime,i,j,slice,ig,fmax )
              IF( nError /= NO_ERROR )GOTO 9999
            ELSE
              ig   = 0
              fmax = 0.0
            END IF

            IF( fmax > fmaxjm )THEN
              fmaxjm = fmax
              jm     = j
            END IF

            IF( fmax > fmaxm )THEN
              fmaxm = fmax
              IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )
              igm = ig
            ELSE IF( ig > 0 )THEN
              irv = SAG_RmvGrdStr( ig )
            END IF

          END DO
        END DO

      END IF

      IF( fmaxjm > fmaxim )THEN
        fmaxim = fmaxjm
        im     = i
      END IF

      iold = i

    END DO

  END DO

  Field%timeID   = im
  Field%UserTime = adjTime(im)%time%runTime

  tMin =  HUGE(1.0)
  tMax = -HUGE(1.0)
  fmax = 0.0
  jm   = 1
  DO j = 1,nTime
    ij = (j-1)*nTime + im
    IF( fLoc(ij) > fmax )THEN
      fmax = fLoc(ij)
      jm   = j
    END IF
  END DO

  ijm = (jm-1)*nTime + im

  DO i = 1,nTime
    DO j = 1,nTime
      ij = (j-1)*nTime + i
      IF( fLoc(ij) == fmax .AND. ij /= ijm )THEN
        fLoc(ij) = fLoc(ij) - SPACING(fLoc(ij))
      END IF
    END DO
  END DO

  DO i = 1,nTime
    fmax = 0.0
    jm   = 1
    DO j = 1,nTime
      ij = (j-1)*nTime + i
      IF( fLoc(ij) > fmax )THEN
        fmax = fLoc(ij)
        jm   = j
      END IF
    END DO
    ij = (jm-1)*nTime + i
    IF( fLoc(ij) >= 0.0 )THEN
      IF( fLoc(ij) < 0.2*fLoc(ijm) .AND. fMass(ij) > 10.0*fMass(ijm) )THEN
        DO j = 1,nTime
          fLoc((j-1)*nTime+i) = -1.0
        END DO
        tLoc(i) = NOT_SET_R
      ELSE
        tLoc(i) = adjTime(i)%time%runTime - adjTime(im)%time%runTime
        tMin = MIN(tMin,tLoc(i))
        tMax = MAX(tMax,tLoc(i))
      END IF
    ELSE
      tLoc(i) = NOT_SET_R
    END IF
  END DO

  IF( tMax-tMin < 1.0 )THEN
    DO i = 1,nTime
      IF( tLoc(i) /= NOT_SET_R )tLoc(i) = 60.0*tLoc(i)
    END DO
    Aptr%xLab = 't - '//TRIM(adjTime(im)%string)//' (mins)'
  ELSE
    Aptr%xLab = 't - '//TRIM(adjTime(im)%string)//' (hrs)'
  END IF

  Aptr%MassUnit = TRIM(material(1)%unit)//'/s'

!--- Duration plot

  tmin = 0.0
  tmax = 0.0
  DO j = 1,nTime
    ij = (j-1)*nTime + im
    IF( fLoc(ij) >= 0.0 )THEN
      IF( fLoc(ij) < 0.2*fLoc(ijm) .AND. fMass(ij) > 10.0*fMass(ijm) )THEN
        fLoc(ij) = -1.0
        Aptr%axisX(1)%val(j) = NOT_SET_R
      ELSE
        Aptr%axisX(1)%val(j) = adjTime(im)%time%runTime - adjTime(im-j)%time%runTime
        tMin = MIN(tMin,Aptr%axisX(1)%val(j))
        tMax = MAX(tMax,Aptr%axisX(1)%val(j))
      END IF
    ELSE
      Aptr%axisX(1)%val(j) = NOT_SET_R
    END IF
  END DO

  Aptr%titleX(1) = 'Duration Estimate'
  IF( tMax-tMin < 1.0 )THEN
    DO j = 1,nTime
      IF( Aptr%axisX(1)%val(j) /= NOT_SET_R )THEN
        Aptr%axisX(1)%val(j) = 60.0*Aptr%axisX(1)%val(j)
      END IF
    END DO
    Aptr%xLabX(1) = 'Duration (mins)'
  ELSE
    Aptr%xLabX(1) = 'Duration (hrs)'
  END IF

ELSE

  tMin =  HUGE(1.0)
  tMax = -HUGE(1.0)

  DO i = 1,nTime
    IF( fLoc(i) >= 0.0 )THEN
      IF( fLoc(i) < 0.2*fLoc(im) .AND. fMass(i) > 10.0*fMass(im) )THEN
        fLoc(i) = -1.0
        tLoc(i) = NOT_SET_R
      ELSE
        tLoc(i) = adjTime(im-i)%time%runTime - adjTime(im)%time%runTime
        tMin = MIN(tMin,tLoc(i))
        tMax = MAX(tMax,tLoc(i))
      END IF
    ELSE
      tLoc(i) = NOT_SET_R
    END IF
  END DO

  IF( tMax-tMin < 1.0 )THEN
    DO i = 1,nTime
      IF( tLoc(i) /= NOT_SET_R )tLoc(i) = 60.0*tLoc(i)
    END DO
    Aptr%xLab = 'Duration (mins)'
  ELSE
    Aptr%xLab = 'Duration (hrs)'
  END IF

  Aptr%MassUnit = TRIM(material(1)%unit)//'/s'

END IF

IF( FastSearch )THEN

  ALLOCATE( PlotData(4),STAT=alloc_stat )   ! nClassData
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'MaxAdjointPlotCont'
    eMessage = 'Error allocating PlotData storage'
    GOTO 9999
  END IF

  PlotData(CD_ADJ_XMASK_MIN) = AdjMaxSearch%xMask(1)
  PlotData(CD_ADJ_XMASK_MAX) = AdjMaxSearch%xMask(2)
  PlotData(CD_ADJ_YMASK_MIN) = AdjMaxSearch%yMask(1)
  PlotData(CD_ADJ_YMASK_MAX) = AdjMaxSearch%yMask(2)

  IF( igm /= 0 )irv = SAG_RmvGrdStr( igm )

  FillGrid = .TRUE.

  igm = -1

  CALL CreateAdjointPlotCont( Field,PlotData,AdjMaxSearch%TimeID,AdjMaxSearch%DurID,slice,igm )
  IF( nError /= NO_ERROR )GOTO 9999

  Field%timeID   = AdjMaxSearch%TimeID
  Field%UserTime = adjTime(AdjMaxSearch%TimeID)%time%runTime

  DEALLOCATE( PlotData,STAT=alloc_stat )

END IF

grdI = igm

Aptr%ID   = igm
Aptr%nLoc = nTime

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetMaxLocCont( Field,ClassData,iTime,i,j,slice,ig,fmax )

USE error_fi
USE slice_fd
USE field_fd
USE adjoint_fi

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: Field
REAL, DIMENSION(*),     INTENT( IN    ) :: ClassData    !Source estimation info
INTEGER,                INTENT( IN    ) :: iTime ! timeID
INTEGER,                INTENT( IN    ) :: i     ! Start time ID
INTEGER,                INTENT( IN    ) :: j     ! Duration ID
TYPE( slice_str    ),   INTENT( OUT   ) :: slice ! slice definition
INTEGER,                INTENT( OUT   ) :: ig    ! fieldID
REAL,                   INTENT( OUT   ) :: fmax  ! field max

INTEGER ij, icell
REAL    mass

REAL, EXTERNAL :: AdjointMax

CALL CreateAdjointPlotCont( Field,ClassData,i,j,slice,ig )
IF( nError == NF_ERROR )THEN
  fmax = 0.0
  ig   = 0
  CALL init_error()
ELSE IF( nError /= NO_ERROR )THEN
  GOTO 9999
ELSE
  icell = -1
  IF( iTime > 0 )THEN
    ij = j
  ELSE
    ij = (j-1)*dimX(1) + i
  END IF
  fmax = AdjointMax( ig,1,icell )
  AdjLocMax(iMaxLoc)%fLoc(ij) = fmax
  mass = AdjointMax( ig,2,icell )
  AdjLocMax(iMaxLoc)%fMass(ij) = mass
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE CreateAdjointPlot( Field,AdjDomain,ClassData,timeID,sblk,spuff,stype,slice,grdI )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE srfparam_fd
USE surface_fi
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd
USE adjoint_fi
USE files_fi
USE abort
USE omp_lib

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL, DIMENSION(*),                 INTENT( INOUT ) :: AdjDomain    !Spatial slice domain
REAL, DIMENSION(*),                 INTENT( IN    ) :: ClassData    !Source estimation info
INTEGER,                            INTENT( IN    ) :: timeID       !Timebreak
TYPE( sfield_block ), DIMENSION(*), INTENT( OUT   ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT   ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: stype ! dezone types
TYPE( slice_str    ),               INTENT( OUT   ) :: slice ! slice definition
INTEGER,                            INTENT( OUT   ) :: grdI

INTEGER i, imat, alloc_stat, irv, icell
REAL    cmax, smax
LOGICAL massFlag

CHARACTER(64) bname ! block name

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_CopyGridID, SAG_RmvGrdStr, SAG_UnionGridID
INTEGER, EXTERNAL :: SourceLocFunc
INTEGER, EXTERNAL :: SAG_InitGridID, SAG_GetSpecialValue
REAL,    EXTERNAL :: AdjointMax, ProbSrc
INTEGER, EXTERNAL :: SAG_BottomSimpleID
INTEGER, EXTERNAL :: SAG_Rezone, DezoneCell
INTEGER, EXTERNAL :: SAG_BottomValueID

grdI = -1

IF( .NOT.MemoryField )THEN  !------ Read puff file

  CALL ReadPuffsID( timeID )
  IF( nError /= NO_ERROR )THEN
    CALL deallocatePuffs()
    GOTO 9999
  END IF
  IF( Aborted() )GOTO 9999

END IF

ALLOCATE( gamT(ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateAdjointPlot'
  eMessage = 'Error allocating adjoint storage'
  GOTO 9999
END IF

!------ Clear puff surface structures

DO i = 1,ntypp
  spuff(i)%nblocks = 0
  spuff(i)%icld    = 0
  ALLOCATE( spuff(i)%iblk(1) )
END DO

!------ Set slice info from ClassData

slice%cat    = Field%category
slice%xmin   = AdjDomain(CD_XMIN)
slice%xmax   = AdjDomain(CD_XMAX)
slice%ymin   = AdjDomain(CD_YMIN)
slice%ymax   = AdjDomain(CD_YMAX)
slice%time   = t
slice%maxlev = Field%maxLev

slice%xminS = NOT_SET_R
slice%yminS = NOT_SET_R
slice%xmaxS = NOT_SET_R
slice%ymaxS = NOT_SET_R

IF( Field%maxCells <= 0 )THEN
  slice%maxcell = MAXSG
ELSE
  slice%maxcell = Field%maxCells
END IF

IF( Field%category == HP_HSLICE )THEN

  slice%data(SD_HEIGHT)   = AdjDomain(CD_ZMIN) !Slice height
  slice%data(SD_BASEGRID) = FLOAT(10)          !base grid

ELSE IF( Field%category == HP_VSLICE )THEN

  slice%data(SD_ZMIN)     = AdjDomain(CD_ZMIN)
  slice%data(SD_ZMAX)     = AdjDomain(CD_ZMAX)
  slice%data(SD_ZRES)     = AdjDomain(CD_VRES) !No. of levels
  slice%data(SD_BASEGRID) = FLOAT(20)          !base grid

ELSE

  slice%data(SD_BASEGRID) = FLOAT(10)      !base grid

END IF

slice%nblk = 1
slice%nfld = 5

!---- Initialize fixed data (same for all materials)

sblk(1)%type  = SBLK_PLOT_ADJ
sblk(1)%field = 1
sblk(1)%flags = 0

stype(1) = DEZONE_MEAN
stype(2) = DEZONE_VAR
stype(3) = DEZONE_SCALE
stype(4) = DEZONE_SCALE
stype(5) = DEZONE_SCALE

!------ Initialize domain mask
!==========================================================
!
! TEMPORARILY REMOVE UNTIL PROPER EFFECT MANAGER AVAILABLE
!
!==========================================================
IF( .FALSE. )THEN
IF( ClassData(CD_ADJ_XMASK_MIN) == DEF_VAL_R )THEN
  xminMask = HUGE(1.)
ELSE
  xminMask = ClassData(CD_ADJ_XMASK_MIN)
END IF
IF( ClassData(CD_ADJ_XMASK_MAX) == DEF_VAL_R )THEN
  xmaxMask = -HUGE(1.)
ELSE
  xmaxMask = ClassData(CD_ADJ_XMASK_MAX)
END IF
IF( ClassData(CD_ADJ_YMASK_MIN) == DEF_VAL_R )THEN
  yminMask = HUGE(1.)
ELSE
  yminMask = ClassData(CD_ADJ_YMASK_MIN)
END IF
IF( ClassData(CD_ADJ_YMASK_MAX) == DEF_VAL_R )THEN
  ymaxMask = -HUGE(1.)
ELSE
  ymaxMask = ClassData(CD_ADJ_YMASK_MAX)
END IF
!==========================================================
ELSE
  xminMask =  HUGE(1.)
  xmaxMask = -HUGE(1.)
  yminMask =  HUGE(1.)
  ymaxMask = -HUGE(1.)
END IF
!==========================================================

!----- Create concentration slice for each adjoint material

DO i = 1,ntypm

  IF( Aborted() )GOTO 9999

!----- Build "trigger" fields first, then "saturated", then "null"

  IF( i <= nHit )THEN
    imat = Tmat(i)%imat
  ELSE
    imat = Nmat(i-nHit)%imat
  END IF

!------ Define single block structure for slice field

  CALL set_block_name( material(imat),1,bname )

  sblk(1)%id   = imat + 65536
  sblk(1)%name = TRIM(bname)

!------ Clear puff pointers for slice field

  spuff(1:ntypp)%nblocks = 0

!------ Initialize appropriate puffs for slice field

  CALL init_spuff( spuff,sblk(1),1,material(imat),1 )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL CreateSlice( sblk,spuff,stype,slice,adjI(imat) )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Ensure field is pushed to bottom grid level

  IF( FastSearch )THEN
    irv = SAG_BottomSimpleID( adjI(imat),slice%nfld,I_FIELD )
  ELSE
    irv = SAG_BottomValueID( adjI(imat),slice%nfld,I_FIELD )
  END IF
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateAdjointPlot'
    eMessage = 'Error pushing slice data to bottom level'
    GOTO 9999
  END IF

  icell = -1
  cmax = AdjointMax( adjI(imat),1,icell )
  IF( cmax == 0.0 )THEN
    adjI(imat) = -1
    CYCLE
  END IF

  IF( Probabilistic )THEN
    smax = AdjointMax( adjI(imat),2,icell )
    gamT(imat) = ProbSrc( cmax,SQRT(smax),0.0 )
  ELSE
    gamT(imat) = 1.0
  END IF

  IF( i <= nHit )THEN
    CALL SetAdjointMask( adjI(imat),cmax )
    IF( i == nHit .AND. .NOT.FastSearch )THEN
      slice%xminS = xminMask
      slice%yminS = yminMask
      slice%xmaxS = xmaxMask
      slice%ymaxS = ymaxMask
    END IF
  END IF

  IF( FastSearch )THEN
    IF( .NOT. FillGrid )THEN
      CALL init_dezone( slice%nfld,stype )
      IF( nError /= NO_ERROR )GOTO 9999
      irv = SAG_Rezone( adjI(imat),DezoneCell )
      CALL exit_dezone()
    END IF
  END IF

END DO

!----- Reset adjusted domain

AdjDomain(CD_XMIN) = slice%xmin
AdjDomain(CD_XMAX) = slice%xmax
AdjDomain(CD_YMIN) = slice%ymin
AdjDomain(CD_YMAX) = slice%ymax

massFlag = (adjChoice == ADJ_MAXLOC) .OR. (adjChoice == ADJ_MASS)
nScale   = 3

CALL SourceLocationSearch( slice%maxcell,slice,grdI,timeID,timeID,massFlag )

9999 CONTINUE

IF( .NOT.MemoryField )CALL deallocatePuffs()

DO imat = 1,ntypm
  IF( adjI(imat) > 0 )irv = SAG_RmvGrdStr( adjI(imat) )
END DO

IF( nError /= NO_ERROR )THEN
  IF( grdI > 0 )THEN
    irv = SAG_RmvGrdStr( grdI )
    grdI = -1
  END IF
END IF

IF( ASSOCIATED(wt)      )DEALLOCATE( wt,     STAT=alloc_stat )
IF( ASSOCIATED(gamT)    )DEALLOCATE( gamT,   STAT=alloc_stat )
IF( ALLOCATED(AdjGrd)  )DEALLOCATE( AdjGrd, STAT=alloc_stat )
IF( ALLOCATED(pCost)   )DEALLOCATE( pCost,   STAT=alloc_stat )
IF( ALLOCATED(pSimplex))DEALLOCATE( pSimplex,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE CreateAdjointPlotCont( Field,ClassData,timeID,durID,slice,grdI )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE srfparam_fd
USE surface_fi
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd
USE adjoint_fi
USE files_fi
USE abort

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL, DIMENSION(*),                 INTENT( IN    ) :: ClassData    !Source estimation info
INTEGER,                            INTENT( IN    ) :: timeID       !Timebreak
INTEGER,                            INTENT( IN    ) :: durID        !Delta timebreak duration
TYPE( slice_str    ),               INTENT( OUT   ) :: slice        !slice definition
INTEGER,                            INTENT( OUT   ) :: grdI

INTEGER, DIMENSION(3)                               :: stype ! dezone types

INTEGER i, alloc_stat, irv, iDur, im ,jm, j
INTEGER ncell, nv, nxx, nyy
INTEGER mxgrd, mxlev, maxcells, icell
REAL    time, xx, yy, dx, dy, cmax, smax
REAL    ubar, vbar, utot, tCont
LOGICAL massFlag

INTEGER, EXTERNAL :: SAG_RmvGrdStr
INTEGER, EXTERNAL :: SAG_GetGridStrHead
INTEGER, EXTERNAL :: SAG_BottomSimpleID
INTEGER, EXTERNAL :: SAG_Rezone, DezoneCell
INTEGER, EXTERNAL :: SAG_BottomValueID
REAL,    EXTERNAL :: AdjointMax, ProbSrc

grdI = -1
iDur = -1

im = timeID
jm = DurID

maxcells = 0

stype(1) = DEZONE_MEAN
stype(2) = DEZONE_VAR
stype(3) = DEZONE_SCALE

slice%nfld = 3

ALLOCATE( gamT(ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateAdjointPlotCont'
  eMessage = 'Error allocating adjoint storage'
  GOTO 9999
END IF

tCont = 3600.*(adjTime(im)%time%runTime - adjTime(im-jm)%time%runTime)

DO i = 1,ntypm
  DO j = i+1,ntypm


    IF( AdjMat(i)%umet == NOT_SET_R .OR. AdjMat(j)%umet == NOT_SET_R .OR. &
        AdjMat(i)%vmet == NOT_SET_R .OR. AdjMat(j)%vmet == NOT_SET_R )THEN
      utot = 0.
    ELSE
      ubar = 0.5*(AdjMat(i)%umet+AdjMat(j)%umet)
      vbar = 0.5*(AdjMat(i)%vmet+AdjMat(j)%vmet)
      utot = SQRT(ubar*ubar+vbar*vbar)
    END IF

    AdjHitDistS(i,j) = MAX(0.0,AdjDistS(i,j)-utot*tCont)
    AdjHitDistT(i,j) = MAX(0.0,AdjDistT(i,j)-MAX(AdjMat(i)%tDur,AdjMat(j)%tDur))

    AdjHitDistS(j,i) = AdjHitDistS(i,j)
    AdjHitDistT(j,i) = AdjHitDistT(i,j)

  END DO
END DO

DO i = 1,ntypm

  CALL ReadSrfAdjField( MAX(1,im-jm),lun_dos,file_dos,i,iDur )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL ReadSrfAdjField( im,lun_dos,file_dos,i,adjI(i) )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL SubtractSrfAdjField( iDur,adjI(i) )
  IF( nError /= NO_ERROR )GOTO 9999

  irv = SAG_RmvGrdStr( iDur )
  iDur = -1

  IF( FastSearch )THEN
    irv = SAG_BottomSimpleID( adjI(i),slice%nfld,I_FIELD )
  ELSE
    irv = SAG_BottomValueID( adjI(i),slice%nfld,I_FIELD )
  END IF
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateAdjointPlotCont'
    eMessage = 'Error pushing data to bottom level'
    GOTO 9999
  END IF

  icell = -1
  cmax = AdjointMax( adjI(i),1,icell )
  IF( cmax == 0.0 )THEN
    irv = SAG_RmvGrdStr( adjI(i) )
    adjI(i) = -1
    CYCLE
  END IF

  IF( Probabilistic )THEN
    smax = AdjointMax( adjI(i),2,icell )
    gamT(i) = ProbSrc( cmax,SQRT(smax),0.0 )
  ELSE
    gamT(i) = 1.0
  END IF

  irv = SAG_GetGridStrHead( adjI(i),time,ncell,nv,mxgrd,mxlev,xx,yy,dx,dy,nxx,nyy )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateAdjointPlotCont'
    eMessage = 'Error getting grid size'
    GOTO 9999
  END IF

  maxcells = MAX(maxcells,ncell)

  IF( FastSearch )THEN
    IF( .NOT.FillGrid )THEN
      CALL init_dezone( slice%nfld,stype )
      IF( nError /= NO_ERROR )GOTO 9999
      irv = SAG_Rezone( adjI(i),DezoneCell )
      CALL exit_dezone()
    END IF
  END IF

END DO

!==========================================================
!
! TEMPORARILY REMOVE UNTIL PROPER EFFECT MANAGER AVAILABLE
!
!==========================================================
IF( .FALSE. )THEN
IF( ClassData(CD_ADJ_XMASK_MIN) == DEF_VAL_R )THEN
  xminMask = -HUGE(1.)
ELSE
  xminMask = ClassData(CD_ADJ_XMASK_MIN)
END IF
IF( ClassData(CD_ADJ_XMASK_MAX) == DEF_VAL_R )THEN
  xmaxMask = HUGE(1.)
ELSE
  xmaxMask = ClassData(CD_ADJ_XMASK_MAX)
END IF
IF( ClassData(CD_ADJ_YMASK_MIN) == DEF_VAL_R )THEN
  yminMask = -HUGE(1.)
ELSE
  yminMask = ClassData(CD_ADJ_YMASK_MIN)
END IF
IF( ClassData(CD_ADJ_YMASK_MAX) == DEF_VAL_R )THEN
  ymaxMask = HUGE(1.)
ELSE
  ymaxMask = ClassData(CD_ADJ_YMASK_MAX)
END IF
!==========================================================
ELSE
  xminMask = -HUGE(1.)
  xmaxMask =  HUGE(1.)
  yminMask = -HUGE(1.)
  ymaxMask =  HUGE(1.)
END IF
!==========================================================

IF( Field%maxCells <= 0 )THEN
  slice%maxcell = MAXSG
ELSE
  slice%maxcell = Field%maxCells
END IF

massFlag = (adjChoice == ADJ_MAXLOC) .OR. (adjChoice == ADJ_MASS)
nScale   = 1

CALL SourceLocationSearch( maxcells,slice,grdI,timeID,durID,massFlag )

9999 CONTINUE

DO i = 1,ntypm
  IF( adjI(i) > 0 )irv = SAG_RmvGrdStr( adjI(i) )
END DO

IF( nError /= NO_ERROR )THEN
  IF( grdI > 0 )THEN
    irv = SAG_RmvGrdStr( grdI )
    grdI = -1
  END IF
END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SourceLocationSearch( maxcells,slice,grdI,timeID,durID,massFlag )

USE scipuff_fi
USE sagdef_fd
USE slice_fd
USE PtrGrdStrItf
USE adjoint_fi
USE abort

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: maxcells
TYPE( slice_str    ),  INTENT( INOUT ) :: slice        !slice definition
INTEGER,               INTENT( OUT   ) :: grdI
INTEGER,               INTENT( IN    ) :: timeID
INTEGER,               INTENT( IN    ) :: durID
LOGICAL,               INTENT( IN    ) :: massFlag

INTEGER, PARAMETER :: NSUB = 2

INTEGER ix, iy, i0, iCell, ilev
INTEGER isub, isub0
INTEGER i, j, alloc_stat, irv, imat
LOGICAL flag
REAL    rfx, rfy, hx, hy
REAL    srcfunc, xt, yt

INTEGER, DIMENSION(NSUB) :: maxid0, maxid
REAL,    DIMENSION(NSUB) :: maxsrcfunc
REAL,    DIMENSION(NSUB) :: xpoint,ypoint
REAL,    DIMENSION(NSUB) :: xpmax,ypmax

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_CopyGridID, SAG_UnionGridID
INTEGER, EXTERNAL :: SAG_BottomFunctionID, SourceLocFunc
INTEGER, EXTERNAL :: SAG_InitGridID, SAG_GetSpecialValue
REAL,    EXTERNAL :: AdjointMax, ProbSrc
REAL,    EXTERNAL :: CellLocFunc, EvaluateCost

!------ Set index and allocate SAG grid structure for Source Location Function

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'Error creating slice grid for output field'
  GOTO 9999
END IF

irv = SAG_InitGridID( ' ',0,SAG_GRID_BOTH,maxcells,slice%nfld,slice%nfld,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'Error creating slice field'
  GOTO 9999
END IF

!---- Copy first slice grid to initialize output grid

imat = 1
DO WHILE( adjI(imat) < 0 .AND. imat < ntypm )
  imat = imat + 1
END DO
IF( adjI(imat) > 0 )THEN
  irv = SAG_CopyGridID( adjI(imat),grdI,SAG_COPY_GRID )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'SourceLocationSearch'
    eMessage = 'Error copying SAG grid'
    GOTO 9999
  END IF
ELSE
  nError   = NF_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'No data to construct field'
  GOTO 9999
END IF

DO i = imat+1,ntypm
  IF( adjI(i) > 0 )THEN
    irv = SAG_UnionGridID( grdI,adjI(i) )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'SourceLocationSearch'
      eMessage = 'Error creating merged SAG grid'
      GOTO 9999
    END IF
  END IF
END DO

grd => SAG_PtrGrdStr( grdI )
grd%mxfld = 3
grd%nvart = 3
ALLOCATE( grd%ipdat(3*grd%mxgrd),grd%ipflg(3),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( AdjGrd(ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'Error allocating output field data storage'
  GOTO 9999
END IF

!---- Clear all output fields

DO j = 1,3
  grd%ipflg(j) = SAG_GRID_FULL
  DO i = 1,grd%ncells
    grd%ipdat((j-1)*grd%mxgrd+i) = 0.0
  END DO
END DO

IF( nHit < 2 )THEN
  nError   = UK_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'Error: need at least two tracers for source estimation'
  GOTO 9999
END IF

ALLOCATE( wt(nHit),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SourceLocationSearch'
  eMessage = 'Error allocating adjoint wt storage'
  GOTO 9999
END IF

!---- Calculate SourceLocFunc

irv = SAG_GetSpecialValue( flag,special )

slice%xmin = grd%xmin
slice%xmax = grd%xmin + grd%nx*grd%dx
slice%ymin = grd%ymin
slice%ymax = grd%ymin + grd%ny*grd%dy

IF( FastSearch .AND. .NOT.FillGrid )THEN

  xminMask = slice%xmin
  xmaxMask = slice%xmax
  yminMask = slice%ymin
  ymaxMask = slice%ymax
  xcMask   = 0.5*(slice%xmin + slice%xmax)
  ycMask   = 0.5*(slice%ymin + slice%ymax)
  xdMask   = 0.5*ABS(slice%xmax - slice%xmin)
  ydMask   = 0.5*ABS(slice%ymax - slice%ymin)

  maxsrcfunc = -999.
  xpmax      = -999.
  ypmax      = -999.
  ilev       = 0
  maxid      = 0

  DO ix = 1,grd%nx
    DO iy = 1,grd%ny
      icell = (iy-1)*grd%nx + ix
      xt = grd%xmin + (FLOAT(ix) - 0.5)*grd%dx
      yt = grd%ymin + (FLOAT(iy) - 0.5)*grd%dy
      srcfunc = CellLocFunc( ilev,xt,yt )
!!DEC$ IF DEFINED (DBGOUT)
!      WRITE(88,'(2(I5,1x),2(F13.5,1x),1pE13.5)')ilev,icell,xt,yt,srcfunc
!!DEC$ ENDIF
      CALL SetMaxList( icell,xt,yt,srcfunc,maxid,xpmax,ypmax,maxsrcfunc,NSUB )
    END DO
  END DO


!------ SAG_GRID_BOTH
  rfx   = 0.5
  rfy   = 0.5

  DO

    isub0  = 0
    DO isub = 1,NSUB
      icell = maxid(isub)
      i0 = grd%ipgrd(icell)
      isub0 = isub0 + i0
    END DO
    IF( isub0 == 0 )EXIT

    maxsrcfunc = -999.
    ilev = ilev + 1

!------- Move down levels until maxlev or no more refinement

    maxid0 = maxid
    xpoint = xpmax
    ypoint = ypmax
    maxid  = 0

    DO isub = 1,NSUB

      icell = maxid0(isub)
      hx    = (rfx)**(ilev - 1)
      hy    = (rfy)**(ilev - 1)

!------- Move down levels until maxlev or no more refinement

      i0 = grd%ipgrd(icell)

      IF( i0 <= 0 )CYCLE

!------- Check the four sub-cells

      DO i = 0,3
        icell = i0 + i
        iy = i/NINT(1./rfx)
        ix = i - 2*iy
        xt = xpoint(isub) + (FLOAT(ix)-0.5)*(1.-rfx)*hx*grd%dx
        yt = ypoint(isub) + (FLOAT(iy)-0.5)*(1.-rfy)*hy*grd%dy
        srcfunc = CellLocFunc( ilev,xt,yt )
        CALL SetMaxList( icell,xt,yt,srcfunc,maxid,xpmax,ypmax,maxsrcfunc,NSUB )
      END DO

    END DO

  END DO

  IF( maxsrcfunc(1) > 0. )THEN

    nSearchVar = 2

    ALLOCATE( pCost(nSearchVar+1),pSimplex(nSearchVar+1,nSearchVar),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SourceLocationSearch'
      WRITE(eMessage,'(A,I6)') 'Error allocating Simplex arrays for nSearchVar = :',nSearchVar
      GOTO 9999
    END IF

    pMass = 0.  ! set mass calculation in SourceLocFunc .FALSE.
    pSimplex(1,1) = xpmax(1)       ! MAX
    pSimplex(1,2) = ypmax(1)
    pCost(1) = EvaluateCost( pSimplex(1,:) )

    rfx = MIN(2.*hx,1.)*grd%dx
    rfy = MIN(2.*hy,1.)*grd%dy
    rad = SQRT(rfx*rfx + rfy*rfy)
    pSimplex(1,1) = MIN(MAX(slice%xmin,xpmax(1)),slice%xmax)                ! TOP
    pSimplex(1,2) = MIN(MAX(slice%ymin,ypmax(1)+rad),slice%ymax)
    pSimplex(2,1) = MIN(MAX(slice%xmin,xpmax(1)-rad*.866),slice%xmax)       ! LEFT
    pSimplex(2,2) = MIN(MAX(slice%ymin,ypmax(1)-rad*.5),slice%ymax)
    pSimplex(3,1) = MIN(MAX(slice%xmin,xpmax(1)+rad*.866),slice%xmax)       ! RIGHT
    pSimplex(3,2) = MIN(MAX(slice%ymin,ypmax(1)-rad*.5),slice%ymax)
    Px(1) = slice%xmax
    Px(2) = slice%xmin
    Py(3) = slice%ymax
    Py(4) = slice%ymin


    DO i = 1,nSearchVar+1
      pCost(i) = EvaluateCost( pSimplex(i,:) )
    END DO

    CALL amoeba( pSimplex,pCost,nSearchVar+1,nSearchVar,nSearchVar,1.E-3,EvaluateCost,iter )

!---- Write optimized solution

    maxsrcfunc(1) = -1.*pCost(1)
    IF( massFlag )THEN
      pMass = -1.
    ELSE
      pMass = 0.
    END IF
    pCost(1) = EvaluateCost( pSimplex(1,:) )

!==== Convert real coordinates to Grid coordinates

    xpt = (pSimplex(1,1)-grd%xmin)/grd%dx
    ypt = (pSimplex(1,2)-grd%ymin)/grd%dy
    ix  = MIN(INT(xpt)+1,grd%nx)
    iy  = MIN(INT(ypt)+1,grd%ny)

    icell = (iy-1)*grd%nx + ix

    ix = INT(xpt)
    iy = INT(ypt)

    DO WHILE( grd%ipgrd(icell) > 0 )
      xpt = xpt - FLOAT(ix)
      ypt = ypt - FLOAT(iy)
      xpt = xpt + xpt
      ypt = ypt + ypt
      ix  = INT(xpt)
      iy  = INT(ypt)
      icell = grd%ipgrd(icell) + iy + iy + ix
    END DO

    grd%ipdat(icell)           = -pCost(1) ! maxsrcfunc
    grd%ipdat(icell+grd%mxgrd) = pMass

    IF( AdjMaxSearch%SrcFunc < maxsrcfunc(1) )THEN
      AdjMaxSearch%SrcFunc    = maxsrcfunc(1)
      AdjMaxSearch%mass       = pMass
      AdjMaxSearch%TimeID     = timeID
      AdjMaxSearch%DurID      = durID
      AdjMaxSearch%xMax       = pSimplex(1,1)
      AdjMaxSearch%yMax       = pSimplex(1,2)
      AdjMaxSearch%xMask(1)   = MINVAL(Px)
      AdjMaxSearch%xMask(2)   = MAXVAL(Px)
      AdjMaxSearch%yMask(1)   = MINVAL(Py)
      AdjMaxSearch%yMask(2)   = MAXVAL(Py)
    END IF

 END IF

ELSE

  nActive = 0
  CALL InitAbortedCount()
  irv = SAG_BottomFunctionID( grdI,SourceLocFunc,3,I_FIELD,.FALSE.,.FALSE. )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'SourceLocationSearch'
    eMessage = 'Error computing Source Location Function'
    GOTO 9999
  END IF

END IF

9999 CONTINUE

IF( ASSOCIATED(wt)      )DEALLOCATE( wt,     STAT=alloc_stat )
IF( ASSOCIATED(gamT)    )DEALLOCATE( gamT,   STAT=alloc_stat )
IF( ALLOCATED(AdjGrd)  )DEALLOCATE( AdjGrd, STAT=alloc_stat )
IF( ALLOCATED(pCost)   )DEALLOCATE( pCost,   STAT=alloc_stat )
IF( ALLOCATED(pSimplex))DEALLOCATE( pSimplex,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

INTEGER FUNCTION SourceLocFunc( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE field_fd
USE scipuff_fi
USE adjoint_fi
USE abort

IMPLICIT NONE

REAL, POINTER, DIMENSION(:) :: dat   !Pointer to Grid data
INTEGER                     :: mxgrd !Data field size
TYPE ( SAGcell_str )        :: p0    !Cell descriptor

INTEGER imat, i, j, iTrigger
LOGICAL flag
REAL    mass

REAL, DIMENSION(5) :: fval

REAL, EXTERNAL :: SetSourceLocProb, SetSourceMassEst

SourceLocFunc = SAG_ERROR

IF( AbortedCount() )GOTO 9999

xpt = grd%xmin + p0%x*grd%dx
ypt = grd%ymin + p0%y*grd%dy

IF( xpt < xminMask .OR. xpt > xmaxMask .OR. ypt < yminMask .OR. ypt > ymaxMask )THEN
  SELECT CASE( adjChoice )
    CASE( ADJ_LOC )
      dat(p0%id) = 0.0
    CASE( ADJ_MASS )
      dat(p0%id) = special
    CASE( ADJ_MAXLOC )
      dat(p0%id) = 0.0
      dat(p0%id+grd%mxgrd) = special
  END SELECT
  SourceLocFunc = SAG_OK
  GOTO 9999
END IF

!$OMP ATOMIC
nActive = nActive + 1
iTrigger = 0
DO imat = 1,ntypm
  IF( adjI(imat) > 0 )THEN
    CALL GetBottomVal( xpt,ypt,fval,adjI(imat),2+nscale,I_FIELD,.TRUE. )
    Amat(imat)%mean = fval(1) - 1.0E-30
    Amat(imat)%sig  = fval(2)
    IF( Amat(imat)%sig > 0.0 )THEN
      Amat(imat)%ScaleT = fval(3)/Amat(imat)%sig
      IF( nScale == 3 )THEN
        Amat(imat)%ScaleS = fval(4)/Amat(imat)%sig
        Amat(imat)%ScaleV = fval(5)/Amat(imat)%sig
      ELSE
        Amat(imat)%ScaleS = Amat(imat)%ScaleT
        Amat(imat)%ScaleV = Amat(imat)%ScaleT
      END IF
      Amat(imat)%sig = SQRT(Amat(imat)%sig)
    ELSE
      Amat(imat)%ScaleT = 1.0
      Amat(imat)%ScaleS = 1.0
      Amat(imat)%ScaleV = 1.0
    END IF
    Amat(imat)%min = material(imat)%prop(3)
  ELSE
    Amat(imat)%mean = 0.0
    Amat(imat)%sig  = 0.0
  END IF
  IF( Amat(imat)%trigger )THEN
    iTrigger = iTrigger + 1
    IF( iTrigger > 1 )THEN
      i = iTrigger
      DO WHILE( i > 1 )
        IF( Amat(imat)%mean <= Tmat(i-1)%m%mean )EXIT
        i = i - 1
      END DO
      IF( i < iTrigger )THEN
        DO j = iTrigger,i+1,-1
          Tmat(j)%m => Tmat(j-1)%m
          Tmat(j)%imat = Tmat(j-1)%imat
        END DO
        Tmat(i)%m => Amat(imat)
        Tmat(i)%imat = imat
      ELSE
        Tmat(iTrigger)%m => Amat(imat)
        Tmat(iTrigger)%imat = imat
      END IF
    ELSE
      Tmat(1)%m => Amat(imat)
      Tmat(1)%imat = imat
    END IF
  END IF
END DO

flag = adjClass == HP_ADJ_SFC
pMass = 0

SELECT CASE( adjChoice )
  CASE( ADJ_LOC )
    dat(p0%id) = SetSourceLocProb( flag,mass )
    IF( flag )dat(p0%id+grd%mxgrd) = mass
  CASE( ADJ_MASS )
    dat(p0%id) = SetSourceMassEst()
  CASE( ADJ_MAXLOC )
    dat(p0%id) = SetSourceLocProb( .TRUE.,mass )
    dat(p0%id+grd%mxgrd) = mass
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

SourceLocFunc = SAG_OK

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION CellLocFunc( ilev,xt,yt )

USE scipuff_fi
USE field_fd
USE adjoint_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ilev
REAL,    INTENT( IN ) :: xt, yt

REAL     mass
INTEGER  imat, i, j, iTrigger

REAL, DIMENSION(5) :: fval

REAL, EXTERNAL :: SetSourceLocProb, SetSourceMassEst

iTrigger = 0
DO imat = 1,ntypm
  IF( adjI(imat) > 0 )THEN
    CALL GetCenterVal( ilev,xt,yt,fval,adjI(imat),2+nscale,I_FIELD,.TRUE. )
    Amat(imat)%mean = MAX(0.0,fval(1)-1.0E-30)
    Amat(imat)%sig  = fval(2)
    IF( Amat(imat)%sig > 1.1E-30 )THEN
      Amat(imat)%ScaleT = fval(3)/Amat(imat)%sig
      IF( nScale == 3 )THEN
        Amat(imat)%ScaleS = fval(4)/Amat(imat)%sig
        Amat(imat)%ScaleV = fval(5)/Amat(imat)%sig
      ELSE
        Amat(imat)%ScaleS = Amat(imat)%ScaleT
        Amat(imat)%ScaleV = Amat(imat)%ScaleT
      END IF
      Amat(imat)%sig = SQRT(Amat(imat)%sig)
    ELSE
      Amat(imat)%ScaleT = 1.0
      Amat(imat)%ScaleS = 1.0
      Amat(imat)%ScaleV = 1.0
    END IF
    Amat(imat)%min  = material(imat)%prop(3)
  ELSE
    Amat(imat)%mean = 0.0
    Amat(imat)%sig  = 0.0
  END IF
  IF( Amat(imat)%trigger )THEN
    iTrigger = iTrigger + 1
    IF( iTrigger > 1 )THEN
      i = iTrigger
      DO WHILE( i > 1 )
        IF( Amat(imat)%mean <= Tmat(i-1)%m%mean )EXIT
        i = i - 1
      END DO
      IF( i < iTrigger )THEN
        DO j = iTrigger,i+1,-1
          Tmat(j)%m => Tmat(j-1)%m
          Tmat(j)%imat = Tmat(j-1)%imat
        END DO
        Tmat(i)%m => Amat(imat)
        Tmat(i)%imat = imat
      ELSE
        Tmat(iTrigger)%m => Amat(imat)
        Tmat(iTrigger)%imat = imat
      END IF
    ELSE
      Tmat(1)%m => Amat(imat)
      Tmat(1)%imat = imat
    END IF
  END IF
END DO

pMass       = 0
CellLocFunc = SetSourceLocProb( .FALSE.,mass )

IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetMaxList( icell,xp,yp,srcfunc,maxid,xpmax,ypmax,maxsrcfunc,NSUB )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icell,NSUB
REAL,    INTENT( IN ) :: srcfunc, xp, yp

INTEGER, DIMENSION(NSUB), INTENT( INOUT ) :: maxid
REAL,    DIMENSION(NSUB), INTENT( INOUT ) :: maxsrcfunc
REAL,    DIMENSION(NSUB), INTENT( INOUT ) :: xpmax
REAL,    DIMENSION(NSUB), INTENT( INOUT ) :: ypmax

INTEGER  i, j

IF( srcfunc <= maxsrcfunc(NSUB) )RETURN

i = NSUB - 1
DO WHILE( i > 0 )
  IF( srcfunc < maxsrcfunc(i) )EXIT
  i = i - 1
END DO
i = i + 1

DO j = NSUB,i,-1
  IF( j == 1 )EXIT
  maxid(j)      = maxid(j-1)
  xpmax(j)      = xpmax(j-1)
  ypmax(j)      = ypmax(j-1)
  maxsrcfunc(j) = maxsrcfunc(j-1)
END DO

maxsrcfunc(i) = srcfunc
xpmax(i) = xp
ypmax(i) = yp
maxid(i) = icell

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION SetSourceMassEst()

USE adjoint_fi
USE error_fi

IMPLICIT NONE

REAL, PARAMETER :: GAMMA_MIN = 1.0E-04
REAL, PARAMETER :: CUTOFF    = 1.0E-20

INTEGER i, j, nwt, imat
REAL    prb, sumwt
REAL    wtt, gam

REAL, EXTERNAL  :: ProbSrc

SetSourceMassEst = 0.0
sumwt            = 0.0
nwt              = 0

DO i = 1,nHit
  IF( Tmat(i)%m%mean <= CUTOFF )CYCLE
  gam = ProbSrc( Tmat(i)%m%mean,Tmat(i)%m%sig,0.0 )
  IF( gam <= GAMMA_MIN )CYCLE
  wtt = gam
  DO j = 1,nNull
    IF( Nmat(j)%m%mean <= CUTOFF )CYCLE
    prb = ProbSrc( Nmat(j)%m%mean,Nmat(j)%m%sig,Nmat(j)%m%min*Tmat(i)%m%mean/gam )
    wtt = wtt*(1.-prb)
  END DO
  imat = Tmat(i)%imat
  SetSourceMassEst = SetSourceMassEst + wtt*gam/Tmat(i)%m%mean
  sumwt = sumwt + wtt
  nwt   = nwt   + 1
END DO

IF( sumwt > 0.0 .AND. 2*nwt >= nHit )THEN
  SetSourceMassEst = SetSourceMassEst/sumwt
ELSE
  SetSourceMassEst = special
END IF

RETURN
END


!------------------------------------------------------------------------------
REAL FUNCTION SetSourceLocProb( flag,mass )

USE adjoint_fi
USE scipuff_fi
USE error_fi

IMPLICIT NONE

LOGICAL, INTENT( IN  ) :: flag
REAL,    INTENT( INOUT ) :: mass

REAL, EXTERNAL :: RatioSetSourceLocProb

SetSourceLocProb = RatioSetSourceLocProb( flag,mass )

RETURN
END

!------------------------------------------------------------------------------

INTEGER FUNCTION AllocateMaxLoc()

!--- Allocates new "associated" data structure with size nTime

USE adjoint_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, jj, nTot

TYPE( AdjointMaxLocTime_str ), DIMENSION(:), ALLOCATABLE :: tem

AllocateMaxLoc = 0

IF( nLocMax == 0 )THEN

  ALLOCATE( AdjLocMax(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  NULLIFY( AdjLocMax(1)%tLoc   )
  NULLIFY( AdjLocMax(1)%fLoc   )
  NULLIFY( AdjLocMax(1)%fMass  )
  NULLIFY( AdjLocMax(1)%DimX   )
  NULLIFY( AdjLocMax(1)%titleX )
  NULLIFY( AdjLocMax(1)%xlabX  )
  NULLIFY( AdjLocMax(1)%axisX  )

  nLocMax = 1

ELSE

  ALLOCATE( tem(nLocMax),STAT=alloc_stat )

  DO i = 1,nLocMax
    tem(i) = AdjLocMax(i)
    NULLIFY( tem(i)%tLoc  )
    NULLIFY( tem(i)%fLoc  )
    NULLIFY( tem(i)%fMass )
    nTot = tem(i)%nLoc
    IF( tem(i)%nDim > 0 )THEN
      DO j = 1,tem(i)%nDim
        nTot = nTot * tem(i)%dimX(j)
      END DO
      NULLIFY( tem(i)%DimX,tem(i)%titleX,tem(i)%xlabX,tem(i)%axisX )
      ALLOCATE( tem(i)%DimX(tem(i)%nDim),tem(i)%titleX(tem(i)%nDim), &
                tem(i)%axisX(tem(i)%nDim),tem(i)%xlabX(tem(i)%nDim),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,tem(i)%nDim
        tem(i)%DimX(j)   = AdjLocMax(i)%DimX(j)
        tem(i)%titleX(j) = AdjLocMax(i)%titleX(j)
        tem(i)%xlabX(j)  = AdjLocMax(i)%xlabX(j)
        ALLOCATE( tem(i)%axisX(j)%val(tem(i)%DimX(j)),STAT=alloc_stat )
        IF( alloc_stat /= 0 )GOTO 9999
        DO jj = 1,tem(i)%DimX(j)
          tem(i)%axisX(j)%val(jj)  = AdjLocMax(i)%axisX(j)%val(jj)
        END DO
      END DO
    END IF
    ALLOCATE( tem(i)%tLoc(tem(i)%nLoc),tem(i)%fLoc(nTot), &
              tem(i)%fMass(nTot),STAT=alloc_stat )
    IF( alloc_stat /= 0 )GOTO 9999
    DO j = 1,tem(i)%nLoc
      tem(i)%tLoc(j)  = AdjLocMax(i)%tLoc(j)
    END DO
    DO j = 1,nTot
      tem(i)%fLoc(j)  = AdjLocMax(i)%fLoc(j)
      tem(i)%fMass(j) = AdjLocMax(i)%fMass(j)
    END DO
  END DO

  DEALLOCATE( AdjLocMax,STAT=alloc_stat )

  nLocMax = nLocMax + 1
  ALLOCATE( AdjLocMax(nLocMax),STAT=alloc_stat )

  DO i = 1,nLocMax - 1
    tem(i) = AdjLocMax(i)
    NULLIFY( AdjLocMax(i)%tLoc  )
    NULLIFY( AdjLocMax(i)%fLoc  )
    NULLIFY( AdjLocMax(i)%fMass )
    nTot = AdjLocMax(i)%nLoc
    IF( AdjLocMax(i)%nDim > 0 )THEN
      DO j = 1,AdjLocMax(i)%nDim
        nTot = nTot * AdjLocMax(i)%DimX(j)
      END DO
      NULLIFY( AdjLocMax(i)%DimX   )
      NULLIFY( AdjLocMax(i)%titleX )
      NULLIFY( AdjLocMax(i)%xlabX  )
      NULLIFY( AdjLocMax(i)%axisX  )
      ALLOCATE( AdjLocMax(i)%DimX(AdjLocMax(i)%nDim), &
                AdjLocMax(i)%titleX(AdjLocMax(i)%nDim), &
                AdjLocMax(i)%axisX(AdjLocMax(i)%nDim), &
                AdjLocMax(i)%xlabX(AdjLocMax(i)%nDim),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,AdjLocMax(i)%nDim
        AdjLocMax(i)%DimX(j)   = tem(i)%DimX(j)
        AdjLocMax(i)%titleX(j) = tem(i)%titleX(j)
        AdjLocMax(i)%xlabX(j)  = tem(i)%xlabX(j)
        ALLOCATE( AdjLocMax(i)%axisX(j)%val(tem(i)%DimX(j)),STAT=alloc_stat )
        IF( alloc_stat /= 0 )GOTO 9999
        DO jj = 1,tem(i)%DimX(j)
          AdjLocMax(i)%axisX(j)%val(jj) = tem(i)%axisX(j)%val(jj)
        END DO
      END DO
    END IF
    ALLOCATE( AdjLocMax(i)%tLoc(tem(i)%nLoc),AdjLocMax(i)%fLoc(tem(i)%nLoc), &
              AdjLocMax(i)%fMass(tem(i)%nLoc),STAT=alloc_stat )
    IF( alloc_stat /= 0 )GOTO 9999
    DO j = 1,tem(i)%nLoc
      AdjLocMax(i)%tLoc(j) =  tem(i)%tLoc(j)
      AdjLocMax(i)%fLoc(j)  = tem(i)%fLoc(j)
      AdjLocMax(i)%fMass(j) = tem(i)%fMass(j)
    END DO
  END DO

END IF

!--- Allocate new structure and clear data fields

AdjLocMax(nLocMax)%nLoc = nTime
AdjLocMax(nLocMax)%nDim = nDim

nTot = nTime
IF( nDim > 0 )THEN
  ALLOCATE( AdjLocMax(nLocMax)%DimX(nDim),AdjLocMax(nLocMax)%titleX(nDim), &
            AdjLocMax(nLocMax)%axisX(nDim),AdjLocMax(nLocMax)%xlabX(nDim),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
  DO i = 1,nDim
    nTot = nTot * dimX(i)
    AdjLocMax(nLocMax)%DimX(i) = dimX(i)
    ALLOCATE( AdjLocMax(nLocMax)%axisX(i)%val(DimX(i)),STAT=alloc_stat )
    IF( alloc_stat /= 0 )GOTO 9999
  END DO
END IF

ALLOCATE( AdjLocMax(nLocMax)%tLoc(nTime),AdjLocMax(nLocMax)%fLoc(nTot), &
          AdjLocMax(nLocMax)%fMass(nTot),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO j = 1,nTime
  AdjLocMax(nLocMax)%tLoc(j)  = -1.0
END DO
DO j = 1,nTot
  AdjLocMax(nLocMax)%fLoc(j)  = -1.0
  AdjLocMax(nLocMax)%fMass(j) = -1.0
END DO

AllocateMaxLoc = nLocMax

9999 CONTINUE

IF( ALLOCATED(tem) )DEALLOCATE( tem,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE RemoveMaxLoc( ID )

!--- Deallocates "associated" data for field-ID

USE adjoint_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER alloc_stat, i, j, idel, nTot

IF( nLocMax == 0 )THEN

  GOTO 9999

ELSE IF( nLocMax == 1 )THEN

  IF( ID == AdjLocMax(1)%ID )THEN
    IF( ASSOCIATED(AdjLocMax(1)%tLoc)   )DEALLOCATE( AdjLocMax(1)%tLoc,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%fLoc)   )DEALLOCATE( AdjLocMax(1)%fLoc,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%fMass)  )DEALLOCATE( AdjLocMax(1)%fMass, STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%DimX)   )DEALLOCATE( AdjLocMax(1)%DimX,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%titleX) )DEALLOCATE( AdjLocMax(1)%titleX,STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%xlabX)  )DEALLOCATE( AdjLocMax(1)%xlabX, STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(1)%axisX) )THEN
      DO j = 1,AdjLocMax(1)%nDim
        IF( ASSOCIATED(AdjLocMax(1)%axisX(j)%val) )THEN
          DEALLOCATE( AdjLocMax(1)%axisX(j)%val,STAT=alloc_stat )
        END IF
      END DO
      DEALLOCATE( AdjLocMax(1)%axisX,STAT=alloc_stat )
    END IF
    DEALLOCATE( AdjLocMax,STAT=alloc_stat )
    nLocMax = 0
  END IF

  GOTO 9999

ELSE

  idel = 0

  DO i = 1,nLocMax
    IF( ID == AdjLocMax(i)%ID )THEN
      idel = i
      EXIT
    END IF
  END DO

  IF( idel > 0 .AND. idel < nLocMax )THEN

    DO i = idel,nLocMax - 1
      AdjLocMax(i)%nLoc = AdjLocMax(i+1)%nLoc
      AdjLocMax(i)%nDim = AdjLocMax(i+1)%nDim
      nTot = AdjLocMax(i)%nLoc
      IF( AdjLocMax(i)%nDim > 0 )THEN
        IF( ASSOCIATED(AdjLocMax(i)%DimX)   )DEALLOCATE( AdjLocMax(i)%DimX,  STAT=alloc_stat )
        IF( ASSOCIATED(AdjLocMax(i)%titleX) )DEALLOCATE( AdjLocMax(i)%titleX,STAT=alloc_stat )
        IF( ASSOCIATED(AdjLocMax(i)%xlabX)  )DEALLOCATE( AdjLocMax(i)%xlabX, STAT=alloc_stat )
        IF( ASSOCIATED(AdjLocMax(1)%axisX) )THEN
          DO j = 1,AdjLocMax(1)%nDim
            IF( ASSOCIATED(AdjLocMax(1)%axisX(j)%val) )THEN
              DEALLOCATE( AdjLocMax(1)%axisX(j)%val,STAT=alloc_stat )
            END IF
          END DO
          DEALLOCATE( AdjLocMax(1)%axisX, STAT=alloc_stat )
        END IF
        ALLOCATE( AdjLocMax(i)%DimX(AdjLocMax(i)%nDim), &
                  AdjLocMax(i)%titleX(AdjLocMax(i)%nDim), &
                  AdjLocMax(i)%axisX(AdjLocMax(i)%nDim), &
                  AdjLocMax(i)%xlabX(AdjLocMax(i)%nDim),STAT=alloc_stat )
        IF( alloc_stat /= 0 )GOTO 9999
        DO j = 1,AdjLocMax(i)%nDim
          nTot = nTot * AdjLocMax(i)%DimX(j)
        END DO
      END IF
      IF( ASSOCIATED(AdjLocMax(i)%tLoc)  )DEALLOCATE( AdjLocMax(i)%tLoc, STAT=alloc_stat )
      IF( ASSOCIATED(AdjLocMax(i)%fLoc)  )DEALLOCATE( AdjLocMax(i)%fLoc, STAT=alloc_stat )
      IF( ASSOCIATED(AdjLocMax(i)%fMass) )DEALLOCATE( AdjLocMax(i)%fMass,STAT=alloc_stat )
      NULLIFY( AdjLocMax(i)%tLoc  )
      NULLIFY( AdjLocMax(i)%fLoc  )
      NULLIFY( AdjLocMax(i)%fMass )
      ALLOCATE( AdjLocMax(i)%tLoc(AdjLocMax(i+1)%nLoc),AdjLocMax(i)%fLoc(nTot), &
                AdjLocMax(i)%fMass(nTot),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,AdjLocMax(i)%nLoc
        AdjLocMax(i)%tLoc(j)  = AdjLocMax(i+1)%tLoc(j)
      END DO
      DO j = 1,nTot
        AdjLocMax(i)%fLoc(j)  = AdjLocMax(i+1)%fLoc(j)
        AdjLocMax(i)%fMass(j) = AdjLocMax(i+1)%fMass(j)
      END DO
    END DO

  END IF

  nLocMax = nLocMax - 1

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE exitAdjLocMax()

!--- Deallocates all "associated" data

USE adjoint_fi

IMPLICIT NONE

INTEGER i, j, alloc_stat

IF( ALLOCATED(AdjLocMax) )THEN
  DO i = 1,SIZE(AdjLocMax)
    IF( ASSOCIATED(AdjLocMax(i)%tLoc  ) )DEALLOCATE( AdjLocMax(i)%tLoc,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%fLoc  ) )DEALLOCATE( AdjLocMax(i)%fLoc,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%fMass ) )DEALLOCATE( AdjLocMax(i)%fMass, STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%DimX  ) )DEALLOCATE( AdjLocMax(i)%DimX,  STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%titleX) )DEALLOCATE( AdjLocMax(i)%titleX,STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%xlabX ) )DEALLOCATE( AdjLocMax(i)%xlabX, STAT=alloc_stat )
    IF( ASSOCIATED(AdjLocMax(i)%axisX) )THEN
      DO j = 1,AdjLocMax(i)%nDim
        IF( ASSOCIATED(AdjLocMax(i)%axisX(j)%val) )THEN
          DEALLOCATE( AdjLocMax(i)%axisX(j)%val,STAT=alloc_stat )
        END IF
      END DO
      DEALLOCATE( AdjLocMax(i)%axisX, STAT=alloc_stat )
    END IF
  END DO
  DEALLOCATE( AdjLocMax,STAT=alloc_stat )
END IF

nLocMax = 0

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE CheckAdjointMax( ID,nPlots,nLines,nPoints )

!--- Returns size of "associated" data for field-ID

USE adjoint_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ID
INTEGER, INTENT( OUT ) :: nPlots
INTEGER, INTENT( OUT ) :: nLines
INTEGER, INTENT( OUT ) :: nPoints

INTEGER i, j, jDim, n, np, im
REAL    fmax

REAL, EXTERNAL :: FindDimAdjointMax

nPlots  = 0
nLines  = 0
nPoints = 0

DO i = 1,nLocMax
  IF( ID == AdjLocMax(i)%ID )THEN
    nPlots = 2 + AdjLocMax(i)%nDim
    nLines = 2 + AdjLocMax(i)%nDim
    DO jDim = 0,AdjLocMax(i)%nDim
      IF( jDim == 0 )THEN
        n  = AdjLocMax(i)%nLoc
        np = 2
      ELSE
        n  = AdjLocMax(i)%DimX(jDim)
        np = 1
      END IF
      DO j = 1,n
        fmax = FindDimAdjointMax( jDim,j,i,im )
        IF( fmax >= 0.0 )nPoints = nPoints + np
      END DO
    END DO
    EXIT
  END IF
END DO

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION FindDimAdjointMax( iDim,iVal,ID,im ) RESULT( fmax )

!--- Returns max loc function for the iVal point in the iDim dimension

USE adjoint_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: iDim
INTEGER, INTENT( IN  ) :: iVal
INTEGER, INTENT( IN  ) :: ID
INTEGER, INTENT( OUT ) :: im

INTEGER i, j, ntot, ndiv, nmod

fmax = -1.0

ntot = nTime
IF( iDim == 0 )THEN
  ndiv = 1
  nmod = nTime
ELSE
  ndiv = nTime
  nmod = AdjLocMax(ID)%DimX(iDim)
END IF

DO i = 1,AdjLocMax(ID)%nDim
  ntot = ntot * AdjLocMax(ID)%DimX(i)
  IF( i < iDim )ndiv = ndiv*AdjLocMax(ID)%DimX(i)
END DO

DO i = 1,ntot
  j = MOD((i-1)/ndiv,nmod) + 1
  IF( j == iVal )THEN
    IF( AdjLocMax(ID)%fLoc(i) > fmax )THEN
      fmax = AdjLocMax(ID)%fLoc(i)
      im   = i
    END IF
  END IF
END DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetAdjointMax( ID,Lines,Points,Titles,Axes,LineID )

!--- Returns "associated" data for field-ID

USE adjoint_fi
USE field_fd
USE charT_fd

IMPLICIT NONE

INTEGER,                          INTENT( IN  ) :: ID
TYPE( SCIPLineT  ), DIMENSION(*), INTENT( OUT ) :: Lines
TYPE( SCIPPointT ), DIMENSION(*), INTENT( OUT ) :: Points
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Titles
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Axes
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: LineID

INTEGER i, j, nPoints, iDim, im, ijm, ioff
REAL    fmax, fmaxj

REAL, EXTERNAL :: FindDimAdjointMax

LineID(1)%string = ' '

nPoints = 0

SearchLoop: DO i = 1,nLocMax

!---- Find requested data

  IF( ID == AdjLocMax(i)%ID )THEN

!---- Line 1 is Location Function vs Release Time

    Lines(1)%index  = 1
    Lines(1)%start  = 1
    Titles(1)%string = 'Location Function'
    Axes(1)%string   = TRIM(AdjLocMax(i)%xLab)//'|'//' '
    DO j = 1,AdjLocMax(i)%nLoc
      IF( FindDimAdjointMax( 0,j,i,im ) >= 0.0 )THEN
        nPoints = nPoints + 1
        Points(nPoints)%x = -AdjLocMax(i)%tLoc(j)
        Points(nPoints)%y =  AdjLocMax(i)%fLoc(im)
      END IF
    END DO

    Lines(1)%number = nPoints

!---- Line 1 is Mass Estimate vs Release Time

    Lines(2)%index  = 2
    Lines(2)%start  = npoints + 1
    Titles(2)%string = 'Mass Estimate ('//TRIM(AdjLocMax(i)%MassUnit)//')'
    Axes(2)%string   = TRIM(AdjLocMax(i)%xLab)//'|'//' '
    DO j = 1,AdjLocMax(i)%nLoc
      IF( FindDimAdjointMax( 0,j,i,im ) >= 0.0 )THEN
        nPoints = nPoints + 1
        Points(nPoints)%x = -AdjLocMax(i)%tLoc(j)
        Points(nPoints)%y =  AdjLocMax(i)%fMass(im)
      END IF
    END DO
    Lines(2)%number = nPoints - Lines(2)%start + 1

!---- Check any extra lines

    IF( AdjLocMax(i)%nDim > 0 )THEN

      ioff = AdjLocMax(i)%nLoc

      DO iDim = 1,AdjLocMax(i)%nDim

        Lines(2+iDim)%index = 2 + iDim
        Lines(2+iDim)%start = nPoints + 1
        Titles(2+iDim)%string = TRIM(AdjLocMax(i)%titleX(iDim))
        Axes(2+iDim)%string   = TRIM(AdjLocMax(i)%xLabX(iDim))//'|'//' '

!---- Find max value

        fmax = 0.0
        im   = 1
        DO j = 1,AdjLocMax(i)%DimX(iDim)
          fmaxj = FindDimAdjointMax( iDim,j,i,ijm )
          IF( fmaxj > fmax )THEN
            fmax = AdjLocMax(i)%fLoc(ijm)
            im   = MOD(ijm-1,ioff)+1
          END IF
        END DO

!---- Extract values along dimension through max

        DO j = 1,AdjLocMax(i)%DimX(iDim)
          IF( AdjLocMax(i)%fLoc(im) >= 0.0 )THEN
            nPoints = nPoints + 1
            Points(nPoints)%x = AdjLocMax(i)%axisX(iDim)%val(j)
            Points(nPoints)%y = AdjLocMax(i)%fLoc(im)
          END IF
          im = im + ioff
        END DO

        Lines(2+iDim)%number = nPoints - Lines(2+iDim)%start + 1
        ioff = ioff*AdjLocMax(i)%DimX(iDim)

      END DO

    END IF

    EXIT SearchLoop   ! Found requested data and completed transfer

  END IF

END DO SearchLoop

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION AdjointMax( grdI,ifld,icell )

USE PtrGrdStrItf
USE adjoint_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: grdI
INTEGER, INTENT( IN    ) :: ifld
INTEGER, INTENT( INOUT ) :: icell

INTEGER i, ioff

grd => SAG_PtrGrdStr( grdI )

ioff = (ifld-1)*grd%mxgrd

IF( icell <= 0 )THEN
  AdjointMax = 0.0
  DO i = 1,grd%ncells
    IF( grd%ipdat(i+ioff) > AdjointMax )THEN
      AdjointMax = grd%ipdat(i+ioff)
      icell      = i
    END IF
  END DO
ELSE
  AdjointMax = grd%ipdat(icell+ioff)
END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetAdjointMask( grdI,cmax )

USE PtrGrdStrItf
USE sagdef_fd
USE sagcel_fd
USE sagstr_fd
USE adjoint_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI
REAL,    INTENT( IN ) :: cmax

INTEGER ix, iy, icell, i

TYPE( SAGcell_str ) p0

INTEGER, EXTERNAL :: SetMaskArea

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_BottomWalk( grd,p0,UserFunction )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    TYPE( SAGcell_str ), INTENT( IN ) :: p0
    INTEGER, EXTERNAL                 :: UserFunction
  END FUNCTION SAG_BottomWalk
END INTERFACE

grd => SAG_PtrGrdStr( grdI )

MaxAdjVal = cmax

!==== Walk the grid

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell = (iy-1)*grd%nx + ix

    p0%id  = icell
    p0%x   = FLOAT(ix) - 0.5
    p0%y   = FLOAT(iy) - 0.5
    p0%hx  = 1.0
    p0%hy  = 1.0
    p0%lev = 0

    i = SAG_BottomWalk( grd,p0,SetMaskArea )
    IF( i /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'SetAdjointMask'
      eMessage = 'Error setting mask area'
      GOTO 9999
    END IF
  END DO
END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

INTEGER FUNCTION SetMaskArea( dat,mx,p )

USE sagdef_fd
USE sagcel_fd
USE adjoint_fi

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: mx
REAL, DIMENSION(:),  POINTER      :: dat
TYPE( SAGcell_str ), INTENT( IN ) :: p

REAL, PARAMETER :: CUTOFF = 1.0E-02

REAL xx

IF( dat(p%ID) > CUTOFF*MaxAdjVal )THEN
  xx = grd%xmin+p%x*grd%dx
!$OMP ATOMIC UPDATE
  xminMask = MIN(xminMask,xx)
!$OMP ATOMIC UPDATE
  xmaxMask = MAX(xmaxMask,xx)
  xx = grd%ymin+p%y*grd%dy
!$OMP ATOMIC UPDATE
  yminMask = MIN(yminMask,xx)
!$OMP ATOMIC UPDATE
  ymaxMask = MAX(ymaxMask,xx)
END IF

SetMaskArea = SAG_OK

RETURN
END

!=======================================================================

SUBROUTINE ExitAdjointPlot()

USE adjoint_fi

IMPLICIT NONE

INTEGER alloc_stat

CALL ExitAdjointMat()

IF( ALLOCATED(AdjDistS) )DEALLOCATE( AdjDistS,STAT=alloc_stat )
IF( ALLOCATED(AdjDistT) )DEALLOCATE( AdjDistT,STAT=alloc_stat )
IF( ALLOCATED(AdjDistV) )DEALLOCATE( AdjDistV,STAT=alloc_stat )

IF( ALLOCATED(AdjHitDistS) )DEALLOCATE( AdjHitDistS,STAT=alloc_stat )
IF( ALLOCATED(AdjHitDistT) )DEALLOCATE( AdjHitDistT,STAT=alloc_stat )

IF( ALLOCATED(adjI) )DEALLOCATE( adjI,STAT=alloc_stat )

IF( ASSOCIATED(Amat) )DEALLOCATE( Amat,STAT=alloc_stat )
IF( ASSOCIATED(Nmat) )DEALLOCATE( Nmat,STAT=alloc_stat )
IF( ASSOCIATED(Tmat) )DEALLOCATE( Tmat,STAT=alloc_stat )
IF( ASSOCIATED(Smat) )DEALLOCATE( Smat,STAT=alloc_stat )

RETURN
END

!=============================================================================================

REAL FUNCTION EvaluateCost( pVec )

USE scipuff_fi
USE adjoint_fi

IMPLICIT NONE

REAL, DIMENSION(nSearchVar), INTENT( IN ) :: pVec

INTEGER imat, i, j, iTrigger
REAL    mass

REAL, DIMENSION(2) :: fval

REAL, EXTERNAL :: SetSourceLocProb

EvaluateCost = 0.0

xpt = pVec(1)
ypt = pVec(2)

IF( xpt < xminMask .OR. xpt > xmaxMask .OR. ypt < yminMask .OR. ypt > ymaxMask )THEN
  EvaluateCost = ABS(xpt - xcMask)/xdMask + ABS(ypt - ycMask)/ydMask
  GOTO 9999
END IF

iTrigger = 0

DO imat = 1,ntypm

  IF( adjI(imat) > 0 )THEN
    CALL GetBottomVal( xpt,ypt,fval,adjI(imat),2,I_FIELD,.TRUE. )
    Amat(imat)%mean = fval(1) - 1.0E-30
    Amat(imat)%sig  = SQRT(fval(2))
    Amat(imat)%min  = material(imat)%prop(3)
  ELSE
    Amat(imat)%mean = 0.0
  END IF

  IF( Amat(imat)%trigger )THEN

    iTrigger = iTrigger + 1

    IF( iTrigger > 1 )THEN
      i = iTrigger
      DO WHILE( i > 1 )
        IF( Amat(imat)%mean <= Tmat(i-1)%m%mean )EXIT
        i = i - 1
      END DO
      IF( i < iTrigger )THEN
        DO j = iTrigger,i+1,-1
          Tmat(j)%m => Tmat(j-1)%m
          Tmat(j)%imat = Tmat(j-1)%imat
        END DO
        Tmat(i)%m => Amat(imat)
        Tmat(i)%imat = imat
      ELSE
        Tmat(iTrigger)%m => Amat(imat)
        Tmat(iTrigger)%imat = imat
      END IF
    ELSE
      Tmat(1)%m => Amat(imat)
      Tmat(1)%imat = imat
    END IF

  END IF

END DO

IF( pMass < 0.0 )THEN
  EvaluateCost = -1.*SetSourceLocProb( .TRUE.,mass )
  pMass = mass
ELSE
  EvaluateCost = -1.*SetSourceLocProb( .FALSE.,mass )
END IF

9999 CONTINUE

RETURN
END

!=================================================================================================

SUBROUTINE amoeba( p,y,mp,np,ndim,ftol,funk,iter )

IMPLICIT NONE

!INTEGER, PARAMETER :: ITMAX = 5000    !Maximum allowed function evaluations
INTEGER, PARAMETER :: ITMAX = 50      !Maximum allowed function evaluations
REAL,    PARAMETER :: TINY  = 1.E-6   !A small number.

INTEGER iter, mp, ndim, np
REAL    ftol, p(mp,np), y(mp)

REAL, EXTERNAL :: funk

! USES amotry,funk

!Multidimensional minimization of the function funk(x) where x(1:ndim) is a vector
!in ndim dimensions, by the downhill simplex method of Nelder and Mead. The matrix
!p(1:ndim+1,1:ndim) is input. Its ndim+1 rows are ndim-dimensional vectors which are
!the vertices of the starting simplex. Also input is the vector y(1:ndim+1), whose components
!must be pre-initialized to the values of funk evaluated at the ndim+1 vertices (rows)
!of p; and ftol the fractional convergence tolerance to be achieved in the function value
!(n.b.!). On output, p and y will have been reset to ndim+1 new points all within ftol of
!a minimum function value, and iter gives the number of function evaluations taken.

INTEGER i, ihi, ilo, inhi, j, m, n
REAL    rtol, sum, swap, ysave, ytry, psum(ndim)

REAL, EXTERNAL :: amotry

iter = 0

1 CONTINUE

DO n = 1,ndim     !Enter here when starting or have just overall contracted.
  sum = 0.        !Recompute psum.
  DO m = 1,ndim+1
    sum = sum + p(m,n)
  END DO
  psum(n) = sum
END DO

2 CONTINUE

ilo = 1     !Enter here when have just changed a single point.

IF( y(1) > y(2) )THEN  ! Determine which point is the highest (worst), next-highest,
  ihi  = 1             ! and lowest (best),
  inhi = 2
ELSE
  ihi  = 2
  inhi = 1
END IF

DO i = 1,ndim+1        ! by looping over the points in the simplex.
  IF( y(i) <= y(ilo) )ilo = i
  IF( y(i) >  y(ihi) )THEN
    inhi = ihi
    ihi  = i
  ELSE IF( y(i) > y(inhi) )THEN
    IF( i /= ihi )inhi = i
  END IF
END DO

rtol = 2.*ABS(y(ihi)-y(ilo))/(ABS(y(ihi))+ABS(y(ilo))+TINY)

!Compute the fractional range from highest to lowest and return if satisfactory.

IF( rtol < ftol )THEN   !If returning, put best point and value in slot 1.
  swap   = y(1)
  y(1)   = y(ilo)
  y(ilo) = swap
  DO n = 1,ndim
    swap     = p(1,n)
    p(1,n)   = p(ilo,n)
    p(ilo,n) = swap
  END DO
  RETURN
END IF

IF( iter >= ITMAX )THEN
  iter = -ITMAX
  RETURN               !’ITMAX exceeded in amoeba’
END IF

iter = iter + 2

!Begin a new iteration. First extrapolate by a factor -1 through the face of the simplex across
!from the high point, i.e., reflect the simplex from the high point.

ytry = amotry( p,y,psum,mp,np,ndim,funk,ihi,-1.0 )

IF( ytry <= y(ilo) )THEN

!-- Gives a result better than the best point, so try an additional extrapolation by a factor 2.

  ytry = amotry( p,y,psum,mp,np,ndim,funk,ihi,2.0 )

ELSE IF( ytry >= y(inhi) )THEN

!The reflected point is worse than the second-highest, so look for an intermediate lower point,
!i.e., do a one-dimensional contraction.

  ysave = y(ihi)
  ytry  = amotry( p,y,psum,mp,np,ndim,funk,ihi,0.5 )

  IF( ytry >= ysave )THEN     !Can’t seem to get rid of that high point. Better contract
    DO i = 1,ndim+1           !around the lowest (best) point.
      IF( i /= ilo )THEN
        DO j = 1,ndim
          psum(j) = 0.5*(p(i,j)+p(ilo,j))
          p(i,j)  = psum(j)
        END DO
        y(i) = funk( psum )
      END IF
    END DO
    iter = iter + ndim   ! Keep track of function evaluations.
    GOTO 1               ! Go back for the test of doneness and the next iteration.
  END IF

ELSE

  iter = iter - 1        !  Correct the evaluation count.

END IF

GOTO 2
END

!=================================================================================================

REAL FUNCTION amotry( p,y,psum,mp,np,ndim,funk,ihi,fac )

USE adjoint_fi, ONLY: Px, Py, Pv

IMPLICIT NONE

INTEGER ihi, mp, ndim, np
REAL    fac, p(mp,np), psum(np), y(mp)

REAL, EXTERNAL ::  funk

! USES funk

!Extrapolates by a factor fac through the face of the simplex across from the high point,
!tries it, and replaces the high point if the new point is better.

INTEGER j
REAL    fac1, fac2, ytry, ptry(ndim)

fac1 = (1.-fac)/ndim
fac2 = fac1 - fac

DO j = 1,ndim
  ptry(j) = psum(j)*fac1 - p(ihi,j)*fac2
END DO

ytry = funk( ptry )    ! Evaluate the function at the trial point.

IF( ytry < 0. )THEN

  IF( ptry(1) < Px(1) )THEN
    Px(1) = ptry(1)
    Py(1) = ptry(2)
    Pv(1) = -ytry
  END IF

  IF( ptry(1) > Px(2) )THEN
    Px(2) = ptry(1)
    Py(2) = ptry(2)
    Pv(2) = -ytry
  END IF

  IF( ptry(2) < Py(3) )THEN
    Px(3) = ptry(1)
    Py(3) = ptry(2)
    Pv(3) = -ytry
  END IF

  IF( ptry(2) > Py(4) )THEN
    Px(4) = ptry(1)
    Py(4) = ptry(2)
    Pv(4) = -ytry
  END IF

END IF

IF( ytry < y(ihi) )THEN    ! If it’s better than the highest, then replace the highest.
  y(ihi) = ytry
  DO j = 1,ndim
    psum(j)  = psum(j) - p(ihi,j) + ptry(j)
    p(ihi,j) = ptry(j)
  END DO
END IF

amotry = ytry

RETURN
END
