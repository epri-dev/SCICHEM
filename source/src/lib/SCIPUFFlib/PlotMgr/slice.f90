SUBROUTINE InitSlice( Field,ClassData,sblk,spuff,stype,slice )

!------ Setup surface and puff structures for generating slice field of single
!       material / subgroup

USE field_fd
USE SCIPresults_fd
USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE slice_fd
USE plotlist_fi
USE classdata_fd
USE met_fi
USE chem_fi
IMPLICIT NONE

TYPE( SCIPPlotFieldT ),             INTENT( INOUT ) :: Field
REAL,                 DIMENSION(*), INTENT( IN    ) :: ClassData
TYPE( sfield_block ), DIMENSION(*), INTENT( OUT   ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT   ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: stype ! dezone types
TYPE( slice_str    ),               INTENT( OUT   ) :: slice ! slice definition

CHARACTER(64) bname ! block name plus temporary error messaging
INTEGER i, nblk, imat, igrp
INTEGER ikind
INTEGER mcID
INTEGER n
REAL    dxa, dya
CHARACTER(16) MCunits

LOGICAL, EXTERNAL :: IsLiquid

INTEGER, EXTERNAL :: output_groups

!------ Clear puff surface structures

DO i = 1,ntypp
  spuff(i)%nblocks = 0
  spuff(i)%icld    = 0
  ALLOCATE( spuff(i)%iblk(1) )
END DO

!------ Pullout input from ClassData

slice%cat    = Field%category
slice%xmin   = ClassData(CD_XMIN)
slice%xmax   = ClassData(CD_XMAX)
slice%ymin   = ClassData(CD_YMIN)
slice%ymax   = ClassData(CD_YMAX)
slice%time   = t
slice%maxlev = Field%maxLev
IF( Field%maxCells <= 0 )THEN
  slice%maxcell = MAXSG
ELSE
  slice%maxcell = Field%maxCells
END IF

IF( Field%category == HP_HSLICE )THEN

  slice%data(SD_HEIGHT)   = ClassData(CD_ZMIN) !Slice height
  slice%data(SD_BASEGRID) = FLOAT(10)          !base grid

  IF( lter .AND. ClassData(CD_ZMIN) < hmin )THEN
    nError = IV_ERROR
    eRoutine = 'InitSlice'
    eMessage = 'For projects with terrain the slice height specification is MSL.'
    eMessage = TRIM(eMessage)//' Requested slice height is below the minimum terrain'
    WRITE(bname,'(F8.2)')hmin
    eInform = 'elevation of '//TRIM(ADJUSTL(bname))//'m. Best height for display depends on terrain gradients and wind velocity.'
    eAction = 'Try plotting terrain and integrated concentration to determine appropriate slice heights.'
    GOTO 9999
  END IF

ELSE IF( Field%category == HP_VSLICE .OR. Field%category == HP_HINT )THEN

  slice%data(SD_ZMIN)     = ClassData(CD_ZMIN)
  slice%data(SD_ZMAX)     = ClassData(CD_ZMAX)
  slice%data(SD_ZRES)     = ClassData(CD_VRES) !No. of levels
  slice%data(SD_BASEGRID) = FLOAT(20)          !base grid

ELSE

  slice%data(SD_BASEGRID) = FLOAT(10)      !base grid

END IF

!------ Initialize domain mask to be ignored

slice%xminS = NOT_SET_R
slice%yminS = NOT_SET_R
slice%xmaxS = NOT_SET_R
slice%ymaxS = NOT_SET_R

nblk = 1
!flag = .FALSE. !Unused

!------ Get material and subgroup for slice field & set units

IF( Field%choice > ntypm )THEN
  i = Field%choice - nPchoice + nPchoiceMC
  IF( i < 1 .OR. i > nPchoiceMC )THEN
    nError   = IV_ERROR
    eRoutine = 'InitSlice'
    eMessage = 'Invalid field Choice for concentration plot'
    GOTO 9999
  END IF
  imat = ChoiceMCID(i)
  IF( ClassID(Field%class) == HP_INTCONC )THEN
    ikind = Field%kind - is_kindMC(2,i) + 1
  ELSE
    ikind = Field%kind - is_kindMC(1,i) + 1
  END IF
  IF( output_groups(material(imat)) > 1 )THEN
    igrp = 0
  ELSE
    igrp = 1
  END IF

  mcID = material(imat)%mcID
  IF( mat_mc%type(mcID) == MC_CHEM )THEN
    n    =  mat_mc%ID(mcID)
    chem => chemMC(n,1)
    IF( chem%lStepAmb )THEN

!      CALL InitChemStepAmb( .TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999

    ELSE IF( chem%lAmbFile )THEN

      CALL InitChemAmb()
      IF( nError /= NO_ERROR )GOTO 9999

      CALL ChemAmbPrjRes( dxa,dya )       !Set plot resolution based on grid size of ambient field
      IF( nError /= NO_ERROR )GOTO 9999

      IF( slice%xmin == NOT_SET_R .OR. slice%xmin == DEF_VAL_R .OR. &
          slice%xmax == NOT_SET_R .OR. slice%xmax == DEF_VAL_R .OR. &
          slice%ymin == NOT_SET_R .OR. slice%ymin == DEF_VAL_R .OR. &
          slice%ymax == NOT_SET_R .OR. slice%ymax == DEF_VAL_R )THEN

        slice%data(SD_BASEGRID) = MAX(NINT((xmax-xmin)/dxa), &
                                      NINT((ymax-ymin)/dya), &
                                      NINT( slice%data(SD_BASEGRID)))

      ELSE

        slice%data(SD_BASEGRID) = MAX(NINT(ABS(slice%xmax-slice%xmin)/dxa), &
                                      NINT(ABS(slice%ymax-slice%ymin)/dya), &
                                      NINT( slice%data(SD_BASEGRID)))

      END IF

    END IF
  END IF

ELSE

imat = Field%choice

IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN
  igrp = Field%kind - ClassChoiceComb(Field%class,Field%choice)%ikind + 1
ELSE
  igrp = 1
END IF

!------ Check for Total

IF( igrp == ClassChoiceComb(Field%class,Field%choice)%nkind .AND. &
    igrp > 1 )THEN
  igrp = 0
END IF

END IF

Field%units = TRIM(ClassChoiceComb(Field%class,Field%choice)%units)
IF( Field%choice > ntypm )THEN
  IF( ClassID(Field%class) /= HP_INTCONC )THEN
    CALL InitMCUnits( imat,Field%kind,MCunits )
    IF( TRIM(MCunits) /= TRIM(NOT_SET_C) )Field%units = MCunits
  END IF
END IF

!------ Define single block structure for slice field

CALL set_block_name( material(imat),igrp,bname )

sblk(1)%type  = SBLK_PLOT
sblk(1)%field = 1
sblk(1)%id    = imat + 65536*igrp
IF( Field%choice > ntypm )THEN
  sblk(1)%type = SBLK_PLOT_MC
  sblk(1)%id   = imat + 65536*ikind    !Encode multi-component kind
END IF
sblk(1)%flags = 0
sblk(1)%name  = TRIM(bname)

!------ Set block flags if necessary

IF( igrp == 0 )sblk(1)%flags = IBSET(sblk(1)%flags,SFLAG_TOT)
IF( IsLiquid(material(imat)%icls) )THEN
  IF( igrp == 1 )sblk(1)%flags = IBSET(sblk(1)%flags,SFLAG_VAP)
  IF( igrp == 2 )sblk(1)%flags = IBSET(sblk(1)%flags,SFLAG_LIQ)
END IF

!------ Initialize appropriate puffs for slice field

CALL init_spuff( spuff,sblk(1),nblk,material(imat),igrp )
IF( nError /= NO_ERROR )GOTO 9999

!------ Set number of blocks and fields

slice%nblk = 1
slice%nfld = 3

!------ Set dezone types

stype(1) = DEZONE_MEAN
stype(2) = DEZONE_VAR
stype(3) = DEZONE_SCALE

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CreateSlice( sblk,spuff,stype,slice,grdI )

!------ Create a concentration field slice

USE scipuff_fi
USE surface_fd
USE sagdef_fd
USE slice_fd
USE slice_fi
USE field_fd
USE srfevap_fi
USE met_fi

IMPLICIT NONE

!------ Subroutine arguments

TYPE( sfield_block ), DIMENSION(*), INTENT( IN    ) :: sblk  !Surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN    ) :: spuff !Puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( IN    ) :: stype !Dezone types
TYPE( slice_str ),                  INTENT( INOUT ) :: slice !Slice definition structure
INTEGER,                            INTENT( INOUT ) :: grdI  !Index of slice SAG grid structure

!------ Local variables

REAL,PARAMETER :: EPS = 0.01

INTEGER irv, lun, nfld, mlev
INTEGER nxs, nys

REAL    xmins, ymins, ymaxs, dxs, dys, xlen, ylen
REAL    delmins, tx

CHARACTER(8) file

LOGICAL adjust
INTEGER startID, endID, xID, yID

!------ Function declarations

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_InitGridStr, SAG_SetNvart
REAL   , EXTERNAL :: NiceAltitude

!------ Set local storage

nfld = slice%nfld
lun  = 0          ! file information not used
file = ' '
tx   = slice%time
mlev = slice%maxlev
delmins = 0.

!------ Check if horizontal limits need to be set

Xeps = EPS*FLOAT(nx)*dxg
Yeps = EPS*FLOAT(ny)*dyg

s_xmin = xmin
s_ymin = ymin
s_xmax = xmin + FLOAT(nx)*dxg
s_ymax = ymin + FLOAT(ny)*dyg
d_xmin = s_xmin - Xeps
d_ymin = s_ymin - Yeps
d_xmax = s_xmax + Xeps
d_ymax = s_ymax + Yeps

IF( slice%xmin == DEF_VAL_R )slice%xmin = s_xmin
IF( slice%xmax == DEF_VAL_R )slice%xmax = s_xmax
IF( slice%ymin == DEF_VAL_R )slice%ymin = s_ymin
IF( slice%ymax == DEF_VAL_R )slice%ymax = s_ymax

!------ Ensure horizontal limits are within the domain

adjust = .FALSE.
IF( slice%xmin < d_xmin .OR. &
    slice%xmin > d_xmax .OR. &
    slice%xmax < d_xmin .OR. &
    slice%xmax > d_xmax .OR. &
    slice%ymin < d_ymin .OR. &
    slice%ymin > d_ymax .OR. &
    slice%ymax < d_ymin .OR. &
    slice%ymax > d_ymax )THEN
  adjust = .TRUE.
  eRoutine = 'CreateSlice'
  nError   = WN_ERROR
  eMessage = 'Slice request extends beyond computational domain'
  eInform  = 'Do you want to try to auto-adjust the request and continue?'
  eAction  = 'Click YES to auto-adjust : NO to abort'
  CALL WarningMessage( .TRUE. )
  IF( nError /= NO_ERROR )THEN
    nError   = IV_ERROR
    eMessage = 'Slice request extends beyond computational domain'
    eInform  = 'Such requests can generate invalid results'
    eAction  = 'Adjust the request and try again'
    GOTO 9999
  END IF
END IF

IF( adjust )THEN
  SELECT CASE( slice%cat )

    CASE( HP_HSLICE,HP_VINT,HP_SSLICE ) ! horizontal plots

      slice%xmin = MAX( slice%xmin,s_xmin )
      slice%xmax = MIN( slice%xmax,s_xmax )
      slice%ymin = MAX( slice%ymin,s_ymin )
      slice%ymax = MIN( slice%ymax,s_ymax )

    CASE( HP_VSLICE,HP_HINT ) ! vertical slice

!     ID scheme - center square (5) = domain
!                   7 | 8 | 9
!                   ---------
!                   4 | 5 | 6
!                   ---------
!                   1 | 2 | 3

      xID = 2
      IF( slice%xmin < d_xmin )xID = 1
      IF( slice%xmin > d_xmax )xID = 3
      yID = 3
      IF( slice%ymin < d_ymin )yID = 0
      IF( slice%ymin > d_ymax )yID = 6
      startID = xID + yID

      xID = 2
      IF( slice%xmax < d_xmin )xID = 1
      IF( slice%xmax > d_xmax )xID = 3
      yID = 3
      IF( slice%ymax < d_ymin )yID = 0
      IF( slice%ymax > d_ymax )yID = 6
      endID = xID + yID

!     Set rest of parameters used by intersect routines (via slice_fi)

      s_delx = slice%xmax - slice%xmin
      s_dely = slice%ymax - slice%ymin
      s_x0   = slice%xmin
      s_y0   = slice%ymin

!     Compute intersections

      CALL slice_intersect( startID,slice%xmin,slice%ymin )
      IF( nError /= NO_ERROR )GOTO 9999

      CALL slice_intersect( endID,slice%xmax,slice%ymax )
      IF( nError /= NO_ERROR )GOTO 9999

!     Check to see if now in domain - if not -> error

      IF( slice%xmin < d_xmin .OR. &
          slice%xmin > d_xmax .OR. &
          slice%xmax < d_xmin .OR. &
          slice%xmax > d_xmax .OR. &
          slice%ymin < d_ymin .OR. &
          slice%ymin > d_ymax .OR. &
          slice%ymax < d_ymin .OR. &
          slice%ymax > d_ymax )THEN
        nError = IV_ERROR
        eRoutine = 'CreateSlice'
        eInform  = 'Requested slice does not intersect computational domain'
        eMessage = 'Unable to process plot request'
        GOTO 9999
      END IF

  END SELECT
END IF

!------ Set index and allocate SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CreateSlice'
  eMessage = 'Error creating slice grid'
  GOTO 9999
END IF

!------ Initialize SAG grid structure

SELECT CASE( slice%cat )

  CASE( HP_HSLICE,HP_VINT,HP_SSLICE ) ! horizontal plots

    xmins = slice%xmin
    ymins = slice%ymin
    xlen  = slice%xmax - slice%xmin
    ylen  = slice%ymax - slice%ymin
    IF( xlen <= 0. .OR. ylen <= 0. )THEN
      nError = IV_ERROR
      eRoutine = 'CreateSlice'
      eMessage = 'Error creating slice field'
      eInform  = 'Invalid slice specification : lengths must be > 0'
      GOTO 9999
    END IF
    IF( xlen > ylen )THEN
      nxs = NINT( slice%data(SD_BASEGRID) )
      nys = MAX(3,NINT(FLOAT(nxs)*ylen/xlen))
    ELSE
      nys = NINT( slice%data(SD_BASEGRID) )
      nxs = MAX(3,NINT(FLOAT(nys)*xlen/ylen))
    END IF
    dxs = xlen/FLOAT(nxs)
    dys = ylen/FLOAT(nys)

    irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,slice%maxcell,nfld,nfld,grdI )

    IF( irv /= SAG_OK )THEN
      nError = UK_ERROR
      eRoutine = 'CreateSlice'
      eMessage = 'Error creating slice field'
      GOTO 9999
    END IF

  CASE( HP_VSLICE,HP_HINT ) ! vertical slice

    xmins = 0.
    nxs   = NINT( slice%data(SD_BASEGRID) )
    dxs   = 1./FLOAT(nxs)

    nys   = NINT( slice%data(SD_ZRES) + 5. )

    IF( slice%data(SD_ZMIN) == DEF_VAL_R )THEN
      ymins = NiceAltitude( hmin )
    ELSE
      ymins = slice%data(SD_ZMIN)
    END IF
    IF( slice%data(SD_ZMAX) == DEF_VAL_R )THEN
      IF( zmax == DEF_VAL_R )THEN
        ymaxs = NiceAltitude( 2500.+hmin )
      ELSE
        ymaxs = NiceAltitude( zmax+hmin )
      END IF
    ELSE
      ymaxs = slice%data(SD_ZMAX)
    END IF

    dys = (ymaxs-ymins)/FLOAT(nys)
    IF( dys <= 0. )THEN
      nError = IV_ERROR
      eRoutine = 'CreateSlice'
      eMessage = 'Error creating vertical slice field'
      eInform  = 'Invalid slice specification : vertical dimension must be > 0'
      GOTO 9999
    END IF

    xlen  = (slice%xmax - slice%xmin)**2 + (slice%ymax - slice%ymin)**2
    IF( xlen < TINY(0.)  )THEN
      nError = IV_ERROR
      eRoutine = 'CreateSlice'
      eMessage = 'Error creating vertical slice field'
      eInform  = 'Invalid slice specification : end points must not be colocated'
      GOTO 9999
    END IF

    irv = SAG_InitGridID( file,lun,SAG_GRID_HORZ,slice%maxcell,nfld,nfld,grdI )
    IF( irv /= SAG_OK )THEN
      nError = UK_ERROR
      eRoutine = 'CreateSlice'
      eMessage = 'Error creating vertical slice field'
      GOTO 9999
    END IF

  CASE DEFAULT

    nError = IV_ERROR
    eRoutine = 'CreateSlice'
    eMessage = 'Invalid slice category'
    GOTO 9999

END SELECT

!------ Initialize adaptive grid

irv = SAG_InitGridStr( grdI,tx,nxs,nys,xmins,ymins,dxs,dys,mlev,delmins )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CreateSlice'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

irv = SAG_SetNvart( grdI,nfld )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CreateSlice'
  eMessage = 'Error initializing nvart'
  GOTO 9999
END IF

!------ Turn off surface evaporation time-step updating

dx_evap = NOT_SET_R


!------ Compute slice fields

SELECT CASE( slice%cat )

  CASE( HP_HSLICE )

    CALL CreateSliceHoriz( grdI,slice,sblk,spuff )
    IF( nError /= NO_ERROR )GOTO 9999

  CASE( HP_VINT )

    CALL CreateSliceVint( grdI,slice,sblk,spuff )
    IF( nError /= NO_ERROR )GOTO 9999

  CASE( HP_SSLICE )

    CALL CreateSliceSurf( grdI,slice,sblk,spuff,stype )
    IF( nError /= NO_ERROR )GOTO 9999

  CASE( HP_VSLICE )

    CALL CreateSliceVert( grdI,slice,sblk,spuff )
    IF( nError /= NO_ERROR )GOTO 9999

  CASE( HP_HINT )

    CALL CreateSliceHint( grdI,slice,sblk,spuff )
    IF( nError /= NO_ERROR )GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE slice_intersect( id,x,y )

USE slice_fi

INTEGER, INTENT( IN  ) :: id
REAL,    INTENT( OUT ) :: x
REAL,    INTENT( OUT ) :: y

REAL, EXTERNAL :: xIntersect, yIntersect

SELECT CASE( id )
  CASE( 1 )
    x = s_xmin
    y = yIntersect( x )
    IF( y < s_ymin )THEN
      y = s_ymin
      x = xIntersect( y )
    END IF
  CASE( 2 )
    y = s_ymin
    x = xIntersect( y )
  CASE( 3 )
    x = s_xmax
    y = yIntersect( x )
    IF( y < s_ymin )THEN
      y = s_ymin
      x = xIntersect( y )
    END IF
  CASE( 4 )
    x = s_xmin
    y = yIntersect( x )
  CASE( 6 )
    x = s_xmax
    y = yIntersect( x )
  CASE( 7 )
    x = s_xmin
    y = yIntersect( x )
    IF( y > s_ymax )THEN
      y = s_ymax
      x = xIntersect( y )
    END IF
  CASE( 8 )
    y = s_ymax
    x = xIntersect( y )
  CASE( 9 )
    x = s_xmax
    y = yIntersect( x )
    IF( y > s_ymax )THEN
      y = s_ymax
      x = xIntersect( y )
    END IF
END SELECT

RETURN
END

!==============================================================================

REAL FUNCTION xIntersect( yarg )

USE slice_fi
USE error_fi

REAL, INTENT( IN ) :: yarg

IF( ABS(s_dely) > ZERO_SLOPE )THEN
  xIntersect = s_x0 + s_delx*(yarg-s_y0)/s_dely
ELSE
  xIntersect = d_xmin
  nError   = IV_ERROR
  eRoutine = 'xIntersect'
  eMessage = 'Unable to process plot request'
  eInform  = 'Requested slice does not intersect computational domain'
END IF

RETURN
END

!==============================================================================

REAL FUNCTION yIntersect( xarg )

USE slice_fi
USE error_fi

REAL, INTENT( IN ) :: xarg

IF( ABS(s_delx) > ZERO_SLOPE )THEN
  yIntersect = s_y0 + s_dely*(xarg-s_x0)/s_delx
ELSE
  yIntersect = d_ymin
  nError   = IV_ERROR
  eRoutine = 'yIntersect'
  eMessage = 'Unable to process plot request'
  eInform  = 'Requested slice does not intersect computational domain'
END IF

RETURN
END

!==============================================================================

SUBROUTINE CreateSliceSurf( grdI,slice,sblk,spuff,stype )

!------ Create a surface concentration slice

USE scipuff_fi
USE surface_fd
USE srfparam_fd
USE surface_dose_fd
USE slice_fd
USE utilsrf
USE grdlock_fi
USE omp_lib
USE step_p_fi, ONLY: myTid

IMPLICIT NONE

!------ Subroutine arguments

INTEGER, INTENT( IN ) :: grdI ! index of slice SAG grid structure
LOGICAL myOK

TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN ) :: spuff ! puff-to-surface structure
INTEGER,              DIMENSION(*), INTENT( IN ) :: stype ! dezone types

!------ Local variables

INTEGER i, ityp
REAL    crat, cratt, dum
LOGICAL lmask

REAL, DIMENSION(ISRF_TOT) :: sdat

TYPE( puff_totalcc ) pt
TYPE( mask_fd ) mask

IF( sblk(1)%type == SBLK_PLOT_MC )THEN
  CALL InitSliceSurfMC( grdI,sblk )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!lmask = slice%xminS /= NOT_SET_R .AND. slice%yminS /= NOT_SET_R .AND. &
!        slice%xmaxS /= NOT_SET_R .AND. slice%ymaxS /= NOT_SET_R
lmask = .NOT.(slice%xminS == NOT_SET_R .OR. slice%yminS == NOT_SET_R .OR. &
              slice%xmaxS == NOT_SET_R .OR. slice%ymaxS == NOT_SET_R)
IF( lmask )THEN
  mask%xmin = slice%xminS
  mask%ymin = slice%yminS
  mask%xmax = slice%xmaxS
  mask%ymax = slice%ymaxS
END IF

myOK = ManageGrdLock(nthreads,1,INIT_GRDLOCK)
CALL InitOMPError()
!------ Loop over puffs

!$OMP PARALLEL DEFAULT(SHARED)
nError   = NO_ERROR
myLockID = 1
myTId    = OMP_GET_THREAD_NUM() + 1
!$OMP DO PRIVATE(i,ityp,sdat,pt,crat,cratt,dum) SCHEDULE(DYNAMIC,1)
PuffLoop : DO i = 1,npuf

!$OMP FLUSH( omp_error )
  IF( ANY(omp_error(:)%nError /= NO_ERROR) ) CYCLE PuffLoop !Try next puff
  IF( puff(i)%idtl == I_REMOVE )CYCLE

  ityp = puff(i)%ityp

  IF( spuff(ityp)%nblocks > 0 )THEN

    CALL settle( puff(i),z_dosage,0.,puff(i)%sr,6.0,dum )
    IF( nError /= NO_ERROR )THEN
      omp_error(myTId)%nError   = nError
      omp_error(myTId)%eRoutine = eRoutine
      omp_error(myTId)%eMessage = eMessage
      omp_error(myTId)%eInform  = eInform
      omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
      CYCLE
    END IF

    crat = MAX((puff(i)%cc-puff(i)%ccb)/puff(i)%c,0.0)
    IF( typeID(ityp)%ltot )THEN
      CALL get_totalcc( puff(i),pt )
      cratt = MAX((pt%cct-pt%cctb)/puff(i)%c,0.0)
    ELSE
      cratt = crat
    END IF

   CALL set_sdep( sdat,puff(i)%sr,crat,cratt,puff(i)%si, &
                                    puff(i)%cc/puff(i)%c,1.,1.,1. )

    IF( lmask )THEN
      CALL surface_dose( puff(i),sdat,grdI,z_dosage,sblk,spuff,stype,mask )
    ELSE
      CALL surface_dose( puff(i),sdat,grdI,z_dosage,sblk,spuff,stype )
    END IF
    IF( nError /= NO_ERROR )THEN
      omp_error(myTId)%nError   = nError
      omp_error(myTId)%eRoutine = eRoutine
      omp_error(myTId)%eMessage = eMessage
      omp_error(myTId)%eInform  = eInform
      omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
      CYCLE
    END IF

  END IF

END DO PuffLoop
!$OMP END DO
!$OMP END PARALLEL
myOK = ManageGrdLock(0,0,CLR_GRDLOCK)
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE InitSliceSurfMC( grdI,sblk )

USE scipuff_fi
USE surface_fd
USE srfparam_fd
USE slice_fd

IMPLICIT NONE

INTEGER,                            INTENT( IN ) :: grdI  ! index of slice SAG grid structure
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure

INTEGER imat, mcID, icomp

!------ Local variables

imat  = MOD(sblk(1)%id,65536)
mcID  = material(imat)%mcID
icomp = sblk(1)%id/65536

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemSliceSurfMC( mat_mc%ID(mcID),icomp,grdI )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitSliceSurfMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END
!==============================================================================

SUBROUTINE CreateSliceHoriz( grdI,slice,sblk,spuff )

!------ Create a horizontal concentration slice

USE scipuff_fi
USE met_fi
USE surface_fd
USE srfdos_fd
USE srfparam_fd
USE slice_fd
USE sagstr_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTid
USE omp_lib

IMPLICIT NONE

!------ Subroutine arguments

INTEGER,                            INTENT( IN ) :: grdI  ! index of slice SAG grid structure
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN ) :: spuff ! puff-to-surface structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure

!------ Local variables

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER, DIMENSION(:), ALLOCATABLE :: ig
REAL,    DIMENSION(:), ALLOCATABLE :: cfac

INTEGER i, ityp, ng, ios
INTEGER n2
REAL,    DIMENSION(1) :: cfac2
INTEGER, DIMENSION(1) :: ig2
REAL    zslice

REAL, DIMENSION(ISRF_TOT) :: sdat

TYPE( srf_gauss_str ) :: s

INTERFACE

  SUBROUTINE SliceGaussHoriz( srf,p,s,ng,ig,cfac,zslice )
    USE puffstruct_fd
    USE sagstr_fd
    USE srfdos_fd
    TYPE( SAGgrid_str   ), POINTER         :: srf
    TYPE( puff_str ),      INTENT( IN    ) :: p
    TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
    INTEGER,               INTENT( IN    ) :: ng
    INTEGER, DIMENSION(*), INTENT( IN    ) :: ig
    REAL,    DIMENSION(*), INTENT( IN    ) :: cfac
    REAL,                  INTENT( IN    ) :: zslice
  END SUBROUTINE SliceGaussHoriz

  SUBROUTINE horizontal_topography_spv( zslice,srf )
    USE sagstr_fd
    REAL,                INTENT( IN ) :: zslice
    TYPE( SAGgrid_str ), POINTER      :: srf
  END SUBROUTINE horizontal_topography_spv

END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'CreateSliceHoriz'
  GOTO 9999
END IF

!------ Allocate space for slice fields

ALLOCATE( ig(slice%nfld),STAT=ios )
IF( ios == 0 )ALLOCATE( cfac(slice%nfld),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSliceHoriz'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ Set slice height relative to reference

zslice = slice%data(SD_HEIGHT) - hmin

IF( sblk(1)%type == SBLK_PLOT_MC )THEN
  CALL InitSliceHorizMC( grdI,slice,sblk )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Loop over puffs

CALL InitOMPError()
!$OMP PARALLEL DEFAULT(SHARED)
nError = NO_ERROR
myTId = OMP_GET_THREAD_NUM() + 1
!$OMP DO PRIVATE(i,ityp,sdat,s,ng,ig,cfac,n2,ig2,cfac2) SCHEDULE(DYNAMIC,1)
PuffLoop : DO i = 1,npuf

!$OMP FLUSH( omp_error )
  IF( ANY(omp_error(:)%nError /= NO_ERROR) ) CYCLE PuffLoop !Try next puff
  IF( puff(i)%idtl == I_REMOVE )CYCLE PuffLoop !Try next puff
  ityp = puff(i)%ityp

  IF( spuff(ityp)%nblocks > 0 )THEN

    CALL InitGaussHoriz( puff(i),zslice,sdat,s )
    s%xminS = slice%xminS
    s%yminS = slice%yminS
    s%xmaxS = slice%xmaxS
    s%ymaxS = slice%ymaxS

    IF( sdat(ISRF_C) > 0. )THEN

      CALL set_srf_var( puff(i),sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

      IF( ng <= 0 )CYCLE PuffLoop !Try next puff

      CALL SliceGaussHoriz( srf,puff(i),s,ng,ig,cfac,zslice )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE PuffLoop
      END IF

    END IF

  END IF

END DO PuffLoop
!$OMP END DO
!$OMP END PARALLEL
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

!------ Set special value on cells below terrain

IF( lter )CALL horizontal_topography_spv( zslice,srf )

9999 CONTINUE

IF( ALLOCATED(ig)   )DEALLOCATE( ig,STAT=ios )
IF( ALLOCATED(cfac) )DEALLOCATE( cfac,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE InitSliceHorizMC( grdI,slice,sblk )

USE scipuff_fi
USE surface_fd
USE srfparam_fd
USE slice_fd

IMPLICIT NONE

INTEGER,                            INTENT( IN ) :: grdI  ! index of slice SAG grid structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure

INTEGER imat, mcID, icomp

!------ Local variables

imat  = MOD(sblk(1)%id,65536)
mcID  = material(imat)%mcID
icomp = sblk(1)%id/65536

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemSliceHorizMC( mat_mc%ID(mcID),icomp,grdI,slice )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitSliceHorizMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END
!==============================================================================

SUBROUTINE CreateSliceVint( grdI,slice,sblk,spuff )

!------ Create a vertically-integrated concentration slice

USE scipuff_fi
USE surface_fd
USE srfdos_fd
USE srfparam_fd
USE slice_fd
USE sagstr_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTid
USE omp_lib

IMPLICIT NONE

!------ Subroutine arguments

INTEGER, INTENT( IN ) :: grdI ! index of slice SAG grid structure

TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN ) :: spuff ! puff-to-surface structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure

!------ Local variables

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER, DIMENSION(:), ALLOCATABLE :: ig
REAL,    DIMENSION(:), ALLOCATABLE :: cfac

INTEGER i, ityp, ng, ios
INTEGER n2
REAL,    DIMENSION(1) :: cfac2
INTEGER, DIMENSION(1) :: ig2

REAL, DIMENSION(ISRF_TOT) :: sdat

TYPE( srf_gauss_str ) :: s

INTERFACE
  SUBROUTINE SliceGaussVint( srf,s,ng,ig,cfac )
    USE sagstr_fd
    USE srfdos_fd
    TYPE( SAGgrid_str   ), POINTER      :: srf
    TYPE( srf_gauss_str ), INTENT( IN ) :: s
    INTEGER,               INTENT( IN ) :: ng
    INTEGER, DIMENSION(*), INTENT( IN ) :: ig
    REAL,    DIMENSION(*), INTENT( IN ) :: cfac
  END SUBROUTINE SliceGaussVint
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError = UK_ERROR
  eRoutine = 'CreateSliceVint'
  GOTO 9999
END IF

!------ Allocate space for slice fields

ALLOCATE( ig(slice%nfld),STAT=ios )
IF( ios == 0 )ALLOCATE( cfac(slice%nfld),STAT=ios )

IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'CreateSliceVint'
  eMessage = 'Error allocating slice arrays'
  GOTO 9999
END IF

!------ Loop over puffs

CALL InitOMPError()
!$OMP PARALLEL DEFAULT(SHARED)
nError = NO_ERROR
myTId = OMP_GET_THREAD_NUM() + 1
!$OMP DO PRIVATE(i,ityp,sdat,s,ng,ig,cfac,n2,ig2,cfac2) SCHEDULE(DYNAMIC,1)
PuffLoop : DO i = 1,npuf

!$OMP FLUSH( omp_error )
  IF( ANY(omp_error(:)%nError /= NO_ERROR) ) CYCLE PuffLoop !Try next puff
  IF( puff(i)%idtl == I_REMOVE )CYCLE PuffLoop !Try next puff

  ityp = puff(i)%ityp

  IF( spuff(ityp)%nblocks > 0 )THEN

    CALL InitGaussVint( puff(i),sdat,s )
    s%xminS = slice%xminS
    s%yminS = slice%yminS
    s%xmaxS = slice%xmaxS
    s%ymaxS = slice%ymaxS

    IF( sdat(ISRF_C) > 0. )THEN

      CALL set_srf_var( puff(i),sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

      IF( ng <= 0 )CYCLE PuffLoop !try next puff

      CALL SliceGaussVint( srf,s,ng,ig,cfac )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

    END IF

  END IF

END DO PuffLoop
!$OMP END DO
!$OMP END PARALLEL
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(ig)   )DEALLOCATE( ig,STAT=ios )
IF( ALLOCATED(cfac) )DEALLOCATE( cfac,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE CreateSliceVert( grdI,slice,sblk,spuff )

!------ Create a surface concentration slice

USE scipuff_fi
USE met_fi
USE surface_fd
USE srfdos_fd
USE srfparam_fd
USE slice_fd
USE sagstr_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTid
USE omp_lib

IMPLICIT NONE

!------ Subroutine arguments

INTEGER, INTENT( IN ) :: grdI ! index of slice SAG grid structure

TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN ) :: spuff ! puff-to-surface structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure

!------ Local variables

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER, DIMENSION(:), ALLOCATABLE :: ig
REAL,    DIMENSION(:), ALLOCATABLE :: cfac

INTEGER i, ityp, ng, ios
INTEGER n2
REAL,    DIMENSION(1) :: cfac2
INTEGER, DIMENSION(1) :: ig2

REAL, DIMENSION(ISRF_TOT) :: sdat

TYPE( srf_gauss_str ) :: s

INTERFACE
  SUBROUTINE SliceGaussVert( srf,p,s,ng,ig,cfac )
    USE puffstruct_fd
    USE sagstr_fd
    USE srfdos_fd
    TYPE( SAGgrid_str   ), POINTER      :: srf
    TYPE( puff_str ),      INTENT( IN ) :: p
    TYPE( srf_gauss_str ), INTENT( IN ) :: s
    INTEGER,               INTENT( IN ) :: ng
    INTEGER, DIMENSION(*), INTENT( IN ) :: ig
    REAL,    DIMENSION(*), INTENT( IN ) :: cfac
  END SUBROUTINE SliceGaussVert
  SUBROUTINE InitGaussVert( srf,p,slice,sdat,s )
    USE srfdos_fd
    USE puffstruct_fd
    USE sagstr_fd
    USE slice_fd
    TYPE( SAGgrid_str ),   POINTER       :: srf
    TYPE( puff_str ),      INTENT( IN  ) :: p
    TYPE( slice_str ),     INTENT( IN  ) :: slice
    REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
    TYPE( srf_gauss_str ), INTENT( OUT ) :: s
  END SUBROUTINE InitGaussVert
  SUBROUTINE vertical_topography_spv( slice,srf,hmin )
    USE sagstr_fd
    USE slice_fd
    TYPE( slice_str ),   INTENT( IN ) :: slice
    TYPE( SAGgrid_str ), POINTER      :: srf
    REAL,                INTENT( IN ) :: hmin
  END SUBROUTINE vertical_topography_spv
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSliceVert'
  GOTO 9999
END IF

!------ Allocate space for slice fields

ALLOCATE( ig(slice%nfld),STAT=ios )
IF( ios == 0 )ALLOCATE( cfac(slice%nfld),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSliceVert'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

IF( sblk(1)%type == SBLK_PLOT_MC )THEN
  CALL InitSliceVertMC( grdI,slice,sblk )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Loop over puffs

CALL InitOMPError()
!$OMP PARALLEL DEFAULT(SHARED)
nError = NO_ERROR
myTId = OMP_GET_THREAD_NUM() + 1
!$OMP DO PRIVATE(i,ityp,sdat,s,ng,ig,cfac,n2,ig2,cfac2) SCHEDULE(DYNAMIC,1)
PuffLoop : DO i = 1,npuf

!$OMP FLUSH( omp_error )
  IF( ANY(omp_error(:)%nError /= NO_ERROR) ) CYCLE PuffLoop !Try next puff
  IF( puff(i)%idtl == I_REMOVE )CYCLE

  ityp = puff(i)%ityp

  IF( spuff(ityp)%nblocks > 0 )THEN

    CALL InitGaussVert( srf,puff(i),slice,sdat,s )

    IF( sdat(ISRF_C) > 0. )THEN

      CALL set_srf_var( puff(i),sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

      IF( ng <= 0 )CYCLE PuffLoop !try next puff

      CALL SliceGaussVert( srf,puff(i),s,ng,ig,cfac )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

    END IF

  END IF

END DO PuffLoop
!$OMP END DO
!$OMP END PARALLEL
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

!------ Set special value on cells below terrain

IF( lter .OR. srf%ymin < 0.0 )CALL vertical_topography_spv( slice,srf,hmin )

9999 CONTINUE

IF( ALLOCATED(ig)   )DEALLOCATE( ig,STAT=ios )
IF( ALLOCATED(cfac) )DEALLOCATE( cfac,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE CreateSliceHint( grdI,slice,sblk,spuff )

!------ Create a surface concentration slice

USE scipuff_fi
USE met_fi
USE surface_fd
USE srfdos_fd
USE srfparam_fd
USE slice_fd
USE sagstr_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTid
USE omp_lib

IMPLICIT NONE

!------ Subroutine arguments

INTEGER, INTENT( IN ) :: grdI ! index of slice SAG grid structure

TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(*), INTENT( IN ) :: spuff ! puff-to-surface structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure

!------ Local variables

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER, DIMENSION(:), ALLOCATABLE :: ig
REAL,    DIMENSION(:), ALLOCATABLE :: cfac

INTEGER i, ityp, ng, ios
INTEGER n2

REAL,    DIMENSION(1)        :: cfac2
INTEGER, DIMENSION(1)        :: ig2
REAL,    DIMENSION(ISRF_TOT) :: sdat

TYPE( srf_gauss_str ) :: s

INTERFACE
  SUBROUTINE SliceGaussVert( srf,p,s,ng,ig,cfac )
    USE puffstruct_fd
    USE sagstr_fd
    USE srfdos_fd
    TYPE( SAGgrid_str   ), POINTER      :: srf
    TYPE( puff_str ),      INTENT( IN ) :: p
    TYPE( srf_gauss_str ), INTENT( IN ) :: s
    INTEGER,               INTENT( IN ) :: ng
    INTEGER, DIMENSION(*), INTENT( IN ) :: ig
    REAL,    DIMENSION(*), INTENT( IN ) :: cfac
  END SUBROUTINE SliceGaussVert
  SUBROUTINE InitGaussHint( srf,p,slice,sdat,s )
    USE srfdos_fd
    USE puffstruct_fd
    USE sagstr_fd
    USE slice_fd
    TYPE( SAGgrid_str ),   POINTER       :: srf
    TYPE( puff_str ),      INTENT( IN  ) :: p
    TYPE( slice_str ),     INTENT( IN  ) :: slice
    REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
    TYPE( srf_gauss_str ), INTENT( OUT ) :: s
  END SUBROUTINE InitGaussHint
END INTERFACE

!----- set SAG grid structure

srf => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ))THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSliceHint'
  GOTO 9999
END IF

!------ Allocate space for slice fields

ALLOCATE( ig(slice%nfld),STAT=ios )
IF( ios == 0 )ALLOCATE( cfac(slice%nfld),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateSliceHint'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ Loop over puffs

CALL InitOMPError()
!$OMP PARALLEL DEFAULT(SHARED)
nError = NO_ERROR
myTId = OMP_GET_THREAD_NUM() + 1
!$OMP DO PRIVATE(i,ityp,sdat,s,ng,ig,cfac,n2,ig2,cfac2) SCHEDULE(DYNAMIC,1)
PuffLoop : DO i = 1,npuf

!$OMP FLUSH( omp_error )
  IF( ANY(omp_error(:)%nError /= NO_ERROR) ) CYCLE PuffLoop !Try next puff
  IF( puff(i)%idtl == I_REMOVE )CYCLE

  ityp = puff(i)%ityp

  IF( spuff(ityp)%nblocks > 0 )THEN

    CALL InitGaussHint( srf,puff(i),slice,sdat,s )

    IF( sdat(ISRF_C) > 0. )THEN

      CALL set_srf_var( puff(i),sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

      IF( ng <= 0 )CYCLE PuffLoop !try next puff

      CALL SliceGaussVert( srf,puff(i),s,ng,ig,cfac )
      IF( nError /= NO_ERROR )THEN
        omp_error(myTId)%nError   = nError
        omp_error(myTId)%eRoutine = eRoutine
        omp_error(myTId)%eMessage = eMessage
        omp_error(myTId)%eInform  = eInform
        omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
        CYCLE
      END IF

    END IF

  END IF

END DO PuffLoop
!$OMP END DO
!$OMP END PARALLEL
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(ig)   )DEALLOCATE( ig,STAT=ios )
IF( ALLOCATED(cfac) )DEALLOCATE( cfac,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE InitSliceVertMC( grdI,slice,sblk )

USE scipuff_fi
USE surface_fd
USE srfparam_fd
USE slice_fd

IMPLICIT NONE

INTEGER,                            INTENT( IN ) :: grdI  ! index of slice SAG grid structure
TYPE( slice_str ),                  INTENT( IN ) :: slice ! slice definition structure
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk  ! surface block structure

INTEGER imat, mcID, icomp

!------ Local variables

imat  = MOD(sblk(1)%id,65536)
mcID  = material(imat)%mcID
icomp = sblk(1)%id/65536

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemSliceVertMC( mat_mc%ID(mcID),icomp,grdI,slice )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitSliceVertMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION NiceAltitude( alt )

IMPLICIT NONE

REAL, INTENT( IN ) :: alt

REAL z,zn

z = ABS(alt)

IF( z <= 2000. )THEN
  zn = INT(z/100.)*100.
ELSE IF( z <= 10000. )THEN
  zn = INT(z/500.)*500.
ELSE
  zn = INT(z/1000.)*1000.
END IF

NiceAltitude = SIGN(zn,alt)

RETURN
END

