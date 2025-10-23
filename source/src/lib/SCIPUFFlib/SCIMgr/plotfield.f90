INTEGER FUNCTION CreateFieldF( UserID,FieldX,ClassData )

USE prjstruct_fd
USE plotlist_fi
USE field_fd
USE scipuff_fi
USE files_fi
USE srfaux_fi
USE surface_fd
USE slice_fd
USE sagdef_fd
USE surface_fi
USE PtrGrdStrItf
USE SCIMgr_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,                INTENT( IN  )   :: UserID       !USER ID Tag
TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: FieldX       !Field descriptor
REAL, DIMENSION(*),     INTENT( IN  )   :: ClassData    !Additional Class data

INTEGER irv, currentState

TYPE( projectIDT ) :: project

INTEGER, EXTERNAL  :: CreateField

!==== Initialize

CreateFieldF = -1

IF( SCIMgrCheckState(HS_IDLESYNC) )THEN     !Available during SYNC callback or while idle
  IF( SCIMgrCheckState(HS_SYNCSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )THEN
  project%ID      = 0
  project%version = 0
  project%path    = TRIM(FieldX%path)
  project%name    = TRIM(FieldX%project)
  CALL SetupFileNames( project )
END IF

!==== Create field

IF( .NOT.Aborted() )THEN

  CreateFieldF = CreateField( FieldX,ClassData )

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!
!*******************************************************************************
INTEGER FUNCTION CreateField( FieldX,ClassData )

USE prjstruct_fd
USE plotlist_fi
USE field_fd
USE scipuff_fi
USE files_fi
USE srfaux_fi
USE surface_fd
USE slice_fd
USE sagdef_fd
USE surface_fi
USE PtrGrdStrItf
USE SCIMgr_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: FieldX       !Field descriptor
REAL, DIMENSION(*),     INTENT( IN  )   :: ClassData    !Additional Class data

TYPE( sfield_block ), DIMENSION(1)  :: sblk  ! surface block structure
TYPE( sfield_puff  ), DIMENSION(:),          &
                        ALLOCATABLE :: spuff ! puff-to-surface structure
TYPE( slice_str    )                :: slice ! slice definition
TYPE( projectIDT )                  :: Project
TYPE( SCIPPlotFieldT )              :: Field

INTEGER, DIMENSION(2) :: ifld
INTEGER, DIMENSION(4) :: ifld2
INTEGER, DIMENSION(5) :: stype ! dezone types

INTEGER grdI, irv, nfld, alloc_stat, ichoice, i, save_blkType
INTEGER ios
INTEGER Total2Evap, ikind
REAL    xbar, ybar, xmap, ymap, xlen, ylen, dist
LOGICAL vSlice

TYPE( SAGgrid_str ), POINTER :: grd

!------ External Functions

INTEGER, EXTERNAL :: SAG_RmvGrdStr, SAG_BottomValueID, SAG_InitError
INTEGER, EXTERNAL :: SAG_UpperClearID, SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_GetMaxLevID, SAG_FindMaxLevID
INTEGER, EXTERNAL :: SAG_BottomValueCN2ID, SAG_TrianglesID
INTEGER, EXTERNAL :: SAG_LastError
REAL,    EXTERNAL :: SAG_GetDelMinID
LOGICAL, EXTERNAL :: IsEvap

!------ Initialize

Field = FieldX
grdI = -1
CreateField = grdI

irv = SAG_InitError()
irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

IF( Aborted() )GOTO 9999

CALL ModuleInitError()

!------ Read project file

IF( .NOT.MemoryField )THEN

  Project%ID      = 0
  Project%version = 0
  Project%path    = TRIM(Field%path)
  Project%name    = TRIM(Field%project)
  CALL ReadProject( Project )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() )GOTO 9999
END IF

!------ Build list of plot choices

CALL BuildPlotList()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - CreateField'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpField(-1,Field,ClassData)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF
!------ Reset plot choice

Field%choice = ChoiceOrder(FieldX%choice)

!------ Check plot request

IF( Field%category == HP_TABLE )THEN
  nError   = IV_ERROR
  eRoutine = 'CreateField'
  eMessage = 'Invalid field request (category)'
  eInform  = 'Use SCIPGetFieldTable for table requests'
  GOTO 9999
END IF

CALL CheckPlotReq( Field )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Set field coordinate data

Field%coordinate%mode = lmap
SELECT CASE( ABS(Field%coordinate%mode) )
  CASE( HD_UTM )
    Field%coordinate%UTMZone       = utm_zone
    Field%coordinate%reference%x   = xref
    Field%coordinate%reference%y   = yref
    Field%coordinate%reference%lat = lat0
    Field%coordinate%reference%lon = lon0
  CASE( HD_CARTESIAN )
    Field%coordinate%UTMZone       = NOT_SET_I
    Field%coordinate%reference%x   = xref
    Field%coordinate%reference%y   = yref
    Field%coordinate%reference%lat = lat0
    Field%coordinate%reference%lon = lon0
  CASE DEFAULT
    Field%coordinate%UTMZone       = NOT_SET_I
    Field%coordinate%reference%x   = NOT_SET_R
    Field%coordinate%reference%y   = NOT_SET_R
    Field%coordinate%reference%lat = NOT_SET_R
    Field%coordinate%reference%lon = NOT_SET_R
END SELECT
Field%coordinate%vertSlice%resolution = NOT_SET_I
Field%coordinate%vertSlice%startPt%x  = NOT_SET_R
Field%coordinate%vertSlice%startPt%y  = NOT_SET_R
Field%coordinate%vertSlice%endPt      = Field%coordinate%vertSlice%startPt
Field%coordinate%horzSlice%height     = NOT_SET_R
Field%coordinate%horzSlice%mode       = NOT_SET_I

IF( Aborted() )GOTO 9999

!------ Create appropriate output field

vSlice = .FALSE.

SELECT CASE( ClassID(Field%class) )

  CASE( HP_CONC,HP_INTCONC )

    IF( .NOT.MemoryField )THEN  !------ Read puff file

      CALL ReadPuffsID( Field%timeID )
      IF( nError /= NO_ERROR )THEN
        CALL deallocatePuffs()
        GOTO 9999
      END IF
      IF( Aborted() )GOTO 9999

    END IF

    ALLOCATE( spuff(ntypp),STAT=alloc_stat )

    CALL InitSlice( Field,ClassData,sblk,spuff,stype,slice )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( Aborted() )GOTO 9999

    CALL CreateSlice( sblk,spuff,stype,slice,grdI )

    IF( .NOT.MemoryField )CALL deallocatePuffs()

    DEALLOCATE( spuff,STAT=alloc_stat )

    IF( Field%category == HP_VSLICE .OR. Field%category == HP_HINT )THEN
      Field%coordinate%mode = -Field%coordinate%mode
      Field%coordinate%vertSlice%resolution = 1  !NINT( slice%data(SD_BASEGRID) )
      Field%coordinate%vertSlice%startPt%x  = slice%xmin
      Field%coordinate%vertSlice%startPt%y  = slice%ymin
      Field%coordinate%vertSlice%endPt%x    = slice%xmax
      Field%coordinate%vertSlice%endPt%y    = slice%ymax
      vSlice = .TRUE.
    ELSE IF( Field%category == HP_HSLICE )THEN
      Field%coordinate%horzSlice%height = slice%data(SD_HEIGHT)
      IF( lter )THEN
        Field%coordinate%horzSlice%mode = 1
      ELSE
        Field%coordinate%horzSlice%mode = 0
      END IF
    ELSE
      Field%coordinate%horzSlice%height = 0.0
      Field%coordinate%horzSlice%mode   = 0
    END IF

    CASE( HP_DEP )
    Total2Evap = 0
    IF( Field%choice <= ntypm )THEN
      ikind = Field%kind - is_kind(Field%choice) + 1
      IF( IsEvap(material(Field%choice)%icls) .AND. ikind > 2 )THEN
        Total2Evap = ikind - 2  !1=Sum Vap+Liq  2=Read Total w/o 2Evap
      END IF
    END IF

    IF( Total2Evap > 0 )THEN

      CALL CreateLiquidTotal( Total2Evap,Field,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    ELSE

    IF( MemoryField )THEN  !------ Copy surface file

      CALL CopySrfField( Field,srfdep,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    ELSE                   !------ Read surface file

      CALL ReadSrfField( Field,lun_dep,file_dep,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    END IF

    END IF

    Field%units = TRIM(ClassChoiceComb(Field%class,Field%choice)%units)
    Field%coordinate%horzSlice%height = 0.0
    Field%coordinate%horzSlice%mode   = 0

  CASE( HP_DOS )

    IF( MemoryField )THEN  !------ Copy surface file

      CALL CopySrfField( Field,srfdos,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    ELSE                   !------ Read surface file

      CALL ReadSrfField( Field,lun_dos,file_dos,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    END IF

    Field%units = TRIM(ClassChoiceComb(Field%class,Field%choice)%units)
    Field%coordinate%horzSlice%height = z_dosage
    Field%coordinate%horzSlice%mode   = 0

  CASE( HP_CMAX,HP_EMAX )

    IF( MemoryField )THEN  !------ Error

      nError   = IV_ERROR
      eRoutine = 'CreateField'
      eMessage = 'Invalid field request'
      eInform  = 'Max Concentration fields not available during calculation'
      GOTO 9999

    ELSE                   !------ Read surface file

      file_tmp = file_dos(1:LEN_TRIM(file_prj)-3)//'ave'
      CALL ReadSrfField( Field,lun_tmp,file_tmp,grdI )
      IF( nError /= NO_ERROR )GOTO 9999

    END IF

    Field%units = TRIM(Field%units)//'/m3'
    Field%coordinate%horzSlice%height  = z_dosage
    Field%coordinate%horzSlice%mode    = 0

  CASE( HP_MET )

    CALL CreateMetPlot( Field,ClassData,grdI )
    IF( nError /= NO_ERROR )GOTO 9999

  CASE( HP_ADJOINT,HP_ADJ_SFC )

    ALLOCATE( spuff(ntypp),STAT=alloc_stat )

    CALL CreateAdjointField( Field,ClassData,sblk,spuff,stype,slice,grdI )
    IF( nError /= NO_ERROR )GOTO 9999

    DEALLOCATE( spuff,STAT=alloc_stat )

    IF( Field%category == HP_VSLICE )THEN
      Field%coordinate%mode = -Field%coordinate%mode
      Field%coordinate%vertSlice%resolution = 1
      Field%coordinate%vertSlice%startPt%x  = slice%xmin
      Field%coordinate%vertSlice%startPt%y  = slice%ymin
      Field%coordinate%vertSlice%endPt%x    = slice%xmax
      Field%coordinate%vertSlice%endPt%y    = slice%ymax
      vSlice = .TRUE.
    ELSE IF( Field%category == HP_HSLICE)THEN
      Field%coordinate%horzSlice%height = slice%data(SD_HEIGHT)
      IF( lter )THEN
        Field%coordinate%horzSlice%mode = 1
      ELSE
        Field%coordinate%horzSlice%mode = 0
      END IF
    ELSE
      Field%coordinate%horzSlice%height = 0.0
      Field%coordinate%horzSlice%mode   = 0
    END IF
  CASE DEFAULT

    grdI = -1
    nError   = UK_ERROR
    eRoutine = 'CreateField'
    eMessage = 'Unknown field request (class)'
    eInform  = 'Field class='//TRIM(ClassString(Field%class))
    GOTO 9999

END SELECT

IF( Aborted() )GOTO 9999

9999 CONTINUE

!------ Remove grid structure on error

IF( nError /= NO_ERROR .AND. grdI > 0 )THEN

  irv  = SAG_RmvGrdStr( grdI )
  grdI = -1

ELSE IF( grdI > 0 )THEN     !Otherwise, push mean/var to bottom level
                            !and zero upper cells for field 3
  IF( CatClassComb(Field%category,Field%class)%type == SCIPtrue .AND. UsePlotCN2 )THEN
    irv = SAG_BottomValueCN2ID( grdI )
    IF( irv == SAG_OK )irv = SAG_UpperClearID( grdI,3 )
    IF( irv /= SAG_OK )THEN
      irv  = SAG_RmvGrdStr( grdI )
      grdI = -1
    END IF
    grd => SAG_PtrGrdStr( grdI )              ! Associate "local" grid structure pointer
    IF( grd%pushtype == 2 )THEN
      nfld = 4; ifld2 = (/ 1,2,4,5 /)
      irv = SAG_UpperClearID( grdI,5 )
      irv = SAG_TrianglesID( grdI,nfld,ifld2,.TRUE.)
      IF( irv /= SAG_OK )THEN
        nError = SAG_LastError( eInform,eAction )
        IF( nError == NO_ERROR )nError = UK_ERROR
        eRoutine = 'CreateField'
        eMessage = 'Error computing field triangles'
        GOTO 9999
      END IF
      CALL SmoothCN2( grdI )
      IF( nError /= NO_ERROR )GOTO 9999
      CALL SAG_FreeTriangleID( grdI )
    END IF
  ELSE
    nfld = 2; ifld = (/ 1,2 /)
    irv = SAG_BottomValueID( grdI,nfld,ifld )
    IF( irv == SAG_OK )irv = SAG_UpperClearID( grdI,3 )
    IF( irv /= SAG_OK )THEN
      irv  = SAG_RmvGrdStr( grdI )
      grdI = -1
    END IF
  END IF

END IF

CreateField = grdI

IF( grdI > 0 )THEN

  Field%interpType = classInterp(Field%class)
  Field%resolution = 0.0
  Field%fldLev     = 0

  grd => SAG_PtrGrdStr( grdI )              ! Associate "local" grid structure pointer
  IF( .NOT.ASSOCIATED(grd) )GOTO 9999       ! Real trouble if this is ever an error

  grd%PlotType = NOT_SET_I                  ! Clear plot field
  IF( ClassID(Field%class) == HP_ADJOINT )THEN
    IF( Field%choice - ntypm == ADJ_MASS )grd%PlotType = -1   !Indicate special Mass Estimate field
  END IF
  grd%PlotData = 0.0

  Field%fldLev = SAG_GetMaxLevID( grdI )      !get grd%maxlev
  IF( Field%fldLev < 0 )THEN                  ! Actual grd maxlev not limited by grd%maxlev
    Field%fldLev = SAG_FindMaxLevID( grdI )   ! get actual grd maxlev
    IF( vSlice )THEN                          !
      xlen = slice%xmax - slice%xmin
      ylen = slice%ymax - slice%ymin
      dist = SQRT( xlen*xlen + ylen*ylen )    ! prj coord.
      xbar = 0.5*(slice%xmax + slice%xmin)    !   set grid center
      ybar = 0.5*(slice%ymax + slice%ymin)    !   set grid center
      CALL mapfac( xbar,ybar,xmap,ymap )      !   get mapfacs
      Field%resolution = grd%dx*dist/FLOAT(grd%nx)/xmap*     &   !   compute delmin
                           (0.5**ABS(Field%fldLev))    !
    ELSE                                      !
      xbar = grd%xmin + 0.5*(grd%nx-1)*grd%dx !   set grid center
      ybar = grd%ymin + 0.5*(grd%ny-1)*grd%dy !
      CALL mapfac( xbar,ybar,xmap,ymap )      !   get mapfacs
      Field%resolution = MIN(grd%dx/xmap, &   !   compute delmin
                           grd%dy/ymap)*  &   !
                           (0.5**ABS(Field%fldLev))    !
    END IF                                    !
    Field%resolution = -Field%resolution      !
  ELSE                                        ! grd limited
    Field%resolution = SAG_GetDelMinID( grdI )!   get delmin
  END IF

  !Clean up grid structure

  grd%file  = ' '
  grd%nvart = grd%mxfld
  grd%mxnam = grd%mxfld
  IF( ASSOCIATED(grd%ipnam) )THEN
    DEALLOCATE( grd%ipnam,STAT=alloc_stat )
    NULLIFY( grd%ipnam )
  END IF

  IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
    ALLOCATE( grd%ipnam(grd%nvart),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'CreateField'
      eMessage = 'Error allocating final grd name list'
      WRITE(eInform,*)'AllocStat =',ios,' : Nfields =',grd%nvart
      GOTO 9999
    END IF
    grd%ipnam = ' '
    grd%ipnam(1) = 'Mean'
    IF( CatClassComb(Field%category,Field%class)%type == SCIPtrue )THEN
      grd%ipnam(2) = 'Var '
    ELSE
      grd%ipnam(2) = 'None'
    END IF
    grd%ipnam(3) = 'Plot'
    IF( grd%nvart > 3 )grd%ipnam(4) = 'ID  '
  END IF

  IF( grd%naux > 0 .AND. ASSOCIATED(grd%ipblk) )THEN
    grd%nblk = 1
    save_blkType = grd%ipblk(1)%type
  ELSE
    grd%nblk = 0
  END IF

  IF( ASSOCIATED(grd%ipblk) )THEN
    DO i = 1,SIZE(grd%ipblk)
      IF( ASSOCIATED(grd%ipblk(i)%fldnam) )THEN
        DEALLOCATE( grd%ipblk(i)%fldnam,STAT=alloc_stat )
      END IF
    END DO
    DEALLOCATE( grd%ipblk,STAT=alloc_stat )
    NULLIFY( grd%ipblk )
  END IF

  IF( grd%nblk > 0 )THEN
    ALLOCATE( grd%ipblk(grd%nblk),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'CreateField'
      eMessage = 'Error allocating final grd block list'
      WRITE(eInform,*)'AllocStat =',ios,' : Nblocks =',grd%nblk
      GOTO 9999
    END IF
    grd%ipblk(1)%type = save_blkType
    grd%ipblk(1)%iaux = 1
    IF( save_blkType == 0 )grd%ipblk(1)%iaux = 0
    grd%ipblk(1)%ifld = NOT_SET_I
    grd%ipblk(1)%nfld = NOT_SET_I
    NULLIFY( grd%ipblk(1)%fldnam )
    grd%ipblk(1)%name = 'Plot only'
  END IF

  grd%ftype = grd%nblk

  grd%version = iversion

END IF

!--- Set return Field

ichoice = FieldX%choice
FieldX  = Field
FieldX%choice = ichoice

!------ Deallocate plot choice arrays

CALL ClearPlotLists()

IF( .NOT.MemoryField )THEN

!------ Deallocate SCIPUFF arrays (that may have been used in creating plot)

  IF( ALLOCATED( srfnam    ) )DEALLOCATE( srfnam,    STAT=alloc_stat )
  IF( ALLOCATED( srftyp    ) )DEALLOCATE( srftyp,    STAT=alloc_stat )
  IF( ALLOCATED( srf_block ) )DEALLOCATE( srf_block, STAT=alloc_stat )
  IF( ALLOCATED( srf_effect) )DEALLOCATE( srf_effect,STAT=alloc_stat )
  IF( ALLOCATED(evapTemp)     )DEALLOCATE( evapTemp,    STAT=alloc_stat )
  IF( ALLOCATED(evapFld)      )DEALLOCATE( evapFld,     STAT=alloc_stat )
  IF( ALLOCATED(evapTempID)   )DEALLOCATE( evapTempID,  STAT=alloc_stat )
  CALL deallocateSrfPuff()
  IF( ASSOCIATED( ibaux_srf ) )DEALLOCATE( ibaux_srf,STAT=alloc_stat )
  IF( ASSOCIATED( cmaux_srf ) )DEALLOCATE( cmaux_srf,STAT=alloc_stat )
  IF( ASSOCIATED( cvaux_srf ) )DEALLOCATE( cvaux_srf,STAT=alloc_stat )
  CALL deallocatePuffs()               !Should already have been done but just in case
  CALL deallocate_read_prj()

END IF

IF( ALLOCATED(spuff) )DEALLOCATE( spuff,STAT=alloc_stat )
      grd  => SAG_PtrGrdStr( grdI )

RETURN
END

!===================================================================================

SUBROUTINE SmoothCN2( grdI )

USE error_fi
USE sagtri_fd
USE sagstr_fd
USE saggrd_fi
USE constants_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

INTEGER i, j, inode, n2, mxdata, iter, nsmth, nsmtho
INTEGER alloc_stat, jp, ii, jnode
REAL    rr1, rr2, m1bar, m2bar, v1bar, v2bar
REAL    dr1, dr2, delM, delV
REAL    aa, bb, cc, frac
REAL    r1hat, r2hat
LOGICAL lcontinue, found

REAL, DIMENSION(:), ALLOCATABLE :: m1, v1, r1, m2, v2, r2

TYPE SAG_neighbor
  INTEGER                        :: n_neighbor
  INTEGER, DIMENSION(:), POINTER :: neighbor
END TYPE SAG_neighbor

TYPE( SAG_neighbor ), DIMENSION(:), ALLOCATABLE :: nList

TYPE( SAGgrid_str ),      POINTER :: grd
TYPE( SAGtriangleT_str ), POINTER :: triT

grd  => SAG_PtrGrdStr( grdI )
triT => SAG_PtrTriStr( grdI )

mxdata = triT%nodeT%mxdata

n2 = triT%nodeT%nnode

ALLOCATE( nList(n2),STAT=alloc_stat )
IF( alloc_Stat == 0 )ALLOCATE( m1(n2),v1(n2),m2(n2),v2(n2),r1(n2),r2(n2),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SmoothCN2'
  eMessage = 'Error allocating node list'
  WRITE(eInform,*)'AllocStat =',alloc_stat,' : N =',triT%nodeT%nnode
  GOTO 9999
END IF

DO i = 1,triT%nodeT%nnode
  nList(i)%n_neighbor = 0
END DO

DO i = 1,triT%ntri
  DO j = 1,3
    inode = triT%iptri(i)%nid(j)
    nList(inode)%n_neighbor = nList(inode)%n_neighbor + 2
  END DO
END DO

DO i = 1,triT%nodeT%nnode
  ALLOCATE( nList(i)%neighbor(nList(i)%n_neighbor),STAT=alloc_stat )
  nList(i)%n_neighbor = 0
END DO

DO i = 1,triT%ntri
  DO j = 1,3
    inode = triT%iptri(i)%nid(j)
    IF( nList(inode)%n_neighbor == 0 )THEN
      nList(inode)%n_neighbor = 2
      jp = j + 1
      IF( jp > 3 )jp = 1
      nList(inode)%neighbor(1) = triT%iptri(i)%nid(jp)
      jp = jp + 1
      IF( jp > 3 )jp = 1
      nList(inode)%neighbor(2) = triT%iptri(i)%nid(jp)
    ELSE
      found = .FALSE.
      jp = j + 1
      IF( jp > 3 )jp = 1
      jnode = triT%iptri(i)%nid(jp)
      DO ii = 1,nList(inode)%n_neighbor
        IF( nList(inode)%neighbor(ii) == jnode )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        nList(inode)%n_neighbor = nList(inode)%n_neighbor + 1
        nList(inode)%neighbor(nList(inode)%n_neighbor) = jnode
      END IF
      found = .FALSE.
      jp = jp + 1
      IF( jp > 3 )jp = 1
      jnode = triT%iptri(i)%nid(jp)
      DO ii = 1,nList(inode)%n_neighbor
        IF( nList(inode)%neighbor(ii) == jnode )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        nList(inode)%n_neighbor = nList(inode)%n_neighbor + 1
        nList(inode)%neighbor(nList(inode)%n_neighbor) = jnode
      END IF
    END IF
  END DO
END DO

lcontinue = .TRUE.
iter = 1

DO WHILE( iter < 25 .AND. lcontinue )

  nsmth  = 0
  nsmtho = 0
  iter   = iter + 1

!---- Copy current node values into local storage

!$OMP PARALLEL DEFAULT (SHARED)
!$OMP DO SCHEDULE(DYNAMIC,1)
  DO i = 1,triT%nodeT%nnode

    m1(i) = triT%nodeT%ipdata((i-1)*mxdata+1)
    v1(i) = triT%nodeT%ipdata((i-1)*mxdata+2)

    IF( UseSpecial .AND. m1(i) == Special )THEN
      r1(i) = Special
      m2(i) = 0.0
      v2(i) = 0.0
      r2(i) = 0.0
    ELSE
      IF( m1(i) > 0.0 )THEN
        r1(i) = SQRT(v1(i))/m1(i)
      ELSE
        r1(i) = 0.0
      END IF

      IF( triT%nodeT%ipdata((i-1)*mxdata+3) > 0.0 )THEN
        m2(i) = triT%nodeT%ipdata((i-1)*mxdata+3)
        v2(i) = triT%nodeT%ipdata((i-1)*mxdata+4)
        r2(i) = SQRT(v2(i))/m2(i)
        nsmtho = nsmtho + 1
      ELSE
        m2(i) = 0.0
        v2(i) = 0.0
        r2(i) = 0.0
      END IF
    END IF

  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

!--- Apply smoothing at each node

!$OMP PARALLEL DEFAULT (SHARED)
!$OMP DO PRIVATE(i,n2,rr1,rr2,r1hat,r2hat,j,inode,frac,delV,delM,dr1,dr2,aa,bb,cc,m1bar,m2bar, &
!$OMP            v1bar,v2bar) SCHEDULE(DYNAMIC,1)
  DO i = 1,triT%nodeT%nnode

    IF( UseSpecial .AND. m1(i) == Special )CYCLE

!--- Average surrounding cell values

    n2  = 0
    rr1 = 0.0
    rr2 = 0.0

    DO j = 1,nList(i)%n_neighbor
      inode = nList(i)%neighbor(j)
      IF( m2(inode) > 0.0 )THEN
        rr1 = rr1 + r1(inode)
        rr2 = rr2 + r2(inode)
        n2  = n2 + 1
      END IF
    END DO

!--- Process only nodes with 2-Gaussian neighbors

    IF( n2 > 1 )THEN

      rr1 = rr1/FLOAT(n2)
      rr2 = rr2/FLOAT(n2)

      IF( r2(i) > 0.0 )THEN

!---  Set smoothed node value if 2-Gaussian

        r1hat = 0.5*(r1(i) + rr1)
        r2hat = 0.5*(r2(i) + rr2)

        IF( ABS(r1hat-r1(i)) > 0.01*r1hat .OR. ABS(r2hat-r2(i)) > 0.01*r2hat )THEN
 !$OMP ATOMIC
          nsmth = nsmth + 1
          frac = 1.0
        ELSE
          frac = 0.0
          delV = 0.0
        END IF

        delM = 0.0

        DO WHILE( delM == 0.0 .AND. frac > 1.0E-2 )

          dr1 = r1(i) + frac*(r1hat - r1(i))
          dr2 = r2(i) + frac*(r2hat - r2(i))

          aa = dr1*dr1 + dr2*dr2
          bb = 2.*(dr1*dr1*m1(i) - dr2*dr2*m2(i))
          cc = v1(i) + v2(i) - dr1*dr1*m1(i)*m1(i) - dr2*dr2*m2(i)*m2(i)
          cc = bb*bb + 4.*aa*cc

          IF( cc > 0.0 )THEN
            delM = (-bb + SQRT(cc))/2.0/aa
            IF( delM > -0.5*m1(i) .AND. delM < 0.5*m2(i) )THEN
              delV = dr1*dr1*(m1(i)+delM)**2 - v1(i)
            ELSE
              IF( bb < 0.0 )THEN
                delM = (-bb - SQRT(cc))/2.0/aa
                IF( delM > -0.5*m1(i) .AND. delM < 0.5*m2(i) )THEN
                  delV = dr1*dr1*(m1(i)+delM)**2 - v1(i)
                ELSE
                  delM = 0.0
                  delV = 0.0
                END IF
              ELSE
                delM = 0.0
                delV = 0.0
              END IF
            END IF
          ELSE
            delM = 0.0
            delV = 0.0
          END IF

          IF( delV > 0.0 )THEN
            IF( delV > 0.5*v2(i) )THEN
              delM = 0.0
              delV = 0.0
            END IF
          ELSE
            IF( delV < -0.5*v1(i) )THEN
              delM = 0.0
              delV = 0.0
            END IF
          END IF

          IF( delM == 0.0 )frac = 0.5*frac

        END DO

        triT%nodeT%ipdata((i-1)*mxdata+1) = m1(i) + delM
        triT%nodeT%ipdata((i-1)*mxdata+2) = v1(i) + delV
        triT%nodeT%ipdata((i-1)*mxdata+3) = m2(i) - delM
        triT%nodeT%ipdata((i-1)*mxdata+4) = v2(i) - delV

      ELSE     !1-Gaussian point with 2-Gaussian neighbors

        IF( n2 >= 2 )THEN

          m1bar = 0.0
          m2bar = 0.0
          v1bar = 0.0
          v2bar = 0.0
          DO j = 1,nList(i)%n_neighbor
            inode = nList(i)%neighbor(j)
            IF( m2(inode) > 0.0 )THEN
              m1bar = m1bar + m1(inode)
              m2bar = m2bar + m2(inode)
              v1bar = v1bar + v1(inode)
              v2bar = v2bar + v2(inode)
            END IF
          END DO
          m1bar = m1bar/FLOAT(n2)
          m2bar = m2bar/FLOAT(n2)
          v1bar = v1bar/FLOAT(n2)
          v2bar = v2bar/FLOAT(n2)

          cc = 1.0
          IF( m2bar > triT%nodeT%ipdata((i-1)*mxdata+1) )cc = triT%nodeT%ipdata((i-1)*mxdata+1)/m2bar
          IF( v2bar > triT%nodeT%ipdata((i-1)*mxdata+2) )cc = MIN(cc, triT%nodeT%ipdata((i-1)*mxdata+2)/v2bar)
          triT%nodeT%ipdata((i-1)*mxdata+3) = 0.5*m2bar*cc
          triT%nodeT%ipdata((i-1)*mxdata+4) = 0.25*v2bar*cc*cc
          triT%nodeT%ipdata((i-1)*mxdata+1) = triT%nodeT%ipdata((i-1)*mxdata+1) &
                                            - triT%nodeT%ipdata((i-1)*mxdata+3)
          triT%nodeT%ipdata((i-1)*mxdata+2) = triT%nodeT%ipdata((i-1)*mxdata+2) &
                                            - triT%nodeT%ipdata((i-1)*mxdata+4)

!$OMP ATOMIC
          nsmth = nsmth + 1

        END IF

      END IF

    ELSE

!---- Set node to single Gaussian if no 2-Gaussian neighbors

      IF( m2(i) > 0.0 )THEN
        triT%nodeT%ipdata((i-1)*mxdata+1) = m1(i) + m2(i)
        triT%nodeT%ipdata((i-1)*mxdata+2) = v1(i) + v2(i)
        triT%nodeT%ipdata((i-1)*mxdata+3) = 0.0
        triT%nodeT%ipdata((i-1)*mxdata+4) = 0.0
      END IF

    END IF

  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

  lcontinue = (nsmth > 0.1*nsmtho) .OR. (iter <= 3)

END DO

!--- Update grid cell values after smoothing

DO i = 1,triT%nodeT%nnode
  j = triT%nodeT%ipnode(i)%id
  grd%ipdat(j)             = triT%nodeT%ipdata((i-1)*mxdata+1)
  grd%ipdat(j+grd%mxgrd)   = triT%nodeT%ipdata((i-1)*mxdata+2)
  grd%ipdat(j+3*grd%mxgrd) = triT%nodeT%ipdata((i-1)*mxdata+3)
  grd%ipdat(j+4*grd%mxgrd) = triT%nodeT%ipdata((i-1)*mxdata+4)
END DO

9999 CONTINUE

IF( ALLOCATED(nList) )THEN
  DO i = 1,triT%nodeT%nnode
    IF( ASSOCIATED(nList(i)%neighbor) )DEALLOCATE( nList(i)%neighbor,STAT=alloc_stat )
  END DO
  DEALLOCATE( nList,STAT=alloc_stat )
END IF

IF( ALLOCATED(m1) )DEALLOCATE( m1,STAT=alloc_stat )
IF( ALLOCATED(v1) )DEALLOCATE( v1,STAT=alloc_stat )
IF( ALLOCATED(r1) )DEALLOCATE( r1,STAT=alloc_stat )
IF( ALLOCATED(m2) )DEALLOCATE( m2,STAT=alloc_stat )
IF( ALLOCATED(v2) )DEALLOCATE( v2,STAT=alloc_stat )
IF( ALLOCATED(r2) )DEALLOCATE( r2,STAT=alloc_stat )

RETURN
END

!===================================================================================

SUBROUTINE CheckSmoothCN2( grdI )

!---- Routine for smoothing exceedance fields

USE error_fi
USE sagtri_fd
USE sagstr_fd
USE saggrd_fi
USE constants_fd
USE PlotFunc_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

INTEGER irv, i, j, n2, inode, mxdata, iter
INTEGER alloc_stat, jp, ii, jnode, nfld, nrr
REAL    rr1, zero_val
LOGICAL found

INTEGER, DIMENSION(1) :: ifld

REAL, DIMENSION(:), ALLOCATABLE :: m1

TYPE SAG_neighbor
  INTEGER                        :: n_neighbor
  INTEGER, DIMENSION(:), POINTER :: neighbor
END TYPE SAG_neighbor

TYPE( SAG_neighbor ), DIMENSION(:), ALLOCATABLE :: nList

TYPE( SAGgrid_str ),      POINTER :: grd
TYPE( SAGtriangleT_str ), POINTER :: triT

INTEGER, EXTERNAL :: SAG_TrianglesID

IF( PlotFunc_nComp <= 1 )RETURN

IF( PlotFunc_nfun == PLOT_FUNCTION_PROB_CLIP )THEN
  zero_val = 1.0E-28
ELSE IF( PlotFunc_nfun == PLOT_FUNCTION_ICPROB_CLIP )THEN
  zero_val = 1.0E-30
ELSE
  RETURN
END IF

grd  => SAG_PtrGrdStr( grdI )
triT => SAG_PtrTriStr( grdI )

nfld = 1
ifld(1) = 3

irv = SAG_TrianglesID( grdI,nfld,ifld,.TRUE. )

mxdata = triT%nodeT%mxdata

n2 = triT%nodeT%nnode

ALLOCATE( nList(n2),m1(n2),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CheckSmoothCN2'
  eMessage = 'Error allocating node list'
  WRITE(eInform,*)'AllocStat =',alloc_stat,' : N =',triT%nodeT%nnode
  GOTO 9999
END IF

DO i = 1,triT%nodeT%nnode
  nList(i)%n_neighbor = 0
END DO

DO i = 1,triT%ntri
  DO j = 1,3
    inode = triT%iptri(i)%nid(j)
    nList(inode)%n_neighbor = nList(inode)%n_neighbor + 2
  END DO
END DO

DO i = 1,triT%nodeT%nnode
  ALLOCATE( nList(i)%neighbor(nList(i)%n_neighbor),STAT=alloc_stat )
  nList(i)%n_neighbor = 0
END DO

DO i = 1,triT%ntri
  DO j = 1,3
    inode = triT%iptri(i)%nid(j)
    IF( nList(inode)%n_neighbor == 0 )THEN
      nList(inode)%n_neighbor = 2
      jp = j + 1
      IF( jp > 3 )jp = 1
      nList(inode)%neighbor(1) = triT%iptri(i)%nid(jp)
      jp = jp + 1
      IF( jp > 3 )jp = 1
      nList(inode)%neighbor(2) = triT%iptri(i)%nid(jp)
    ELSE
      found = .FALSE.
      jp = j + 1
      IF( jp > 3 )jp = 1
      jnode = triT%iptri(i)%nid(jp)
      DO ii = 1,nList(inode)%n_neighbor
        IF( nList(inode)%neighbor(ii) == jnode )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        nList(inode)%n_neighbor = nList(inode)%n_neighbor + 1
        nList(inode)%neighbor(nList(inode)%n_neighbor) = jnode
      END IF
      found = .FALSE.
      jp = jp + 1
      IF( jp > 3 )jp = 1
      jnode = triT%iptri(i)%nid(jp)
      DO ii = 1,nList(inode)%n_neighbor
        IF( nList(inode)%neighbor(ii) == jnode )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        nList(inode)%n_neighbor = nList(inode)%n_neighbor + 1
        nList(inode)%neighbor(nList(inode)%n_neighbor) = jnode
      END IF
    END IF
  END DO
END DO

iter = 1

DO WHILE( iter < 5 )

  iter   = iter + 1

!---- Copy current node values into local storage

  DO i = 1,triT%nodeT%nnode
    m1(i) = triT%nodeT%ipdata(i)
  END DO

!--- Apply smoothing at each node

!$OMP PARALLEL DEFAULT (SHARED)
!$OMP DO PRIVATE(rr1,j,inode,nrr) SCHEDULE(DYNAMIC,1)
  DO i = 1,triT%nodeT%nnode

    IF( m1(i) == zero_val )CYCLE
    IF( UseSpecial .AND. m1(i) == Special )CYCLE

!--- Average surrounding cell values

    rr1 = 0
    nrr = 0

    DO j = 1,nList(i)%n_neighbor
      inode = nList(i)%neighbor(j)
      IF( UseSpecial .AND. m1(inode) == Special )CYCLE
      nrr = nrr + 1
      rr1 = rr1 + m1(inode)
    END DO

!---  Set smoothed node value

    IF( nrr > 0 )THEN
      triT%nodeT%ipdata(i) = 0.5*(m1(i) + rr1/FLOAT(nrr))
    END IF

  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

END DO

!--- Update grid cell values after smoothing

DO i = 1,triT%nodeT%nnode
  j = triT%nodeT%ipnode(i)%id
  grd%ipdat(j+2*grd%mxgrd) = triT%nodeT%ipdata(i)
END DO

9999 CONTINUE

CALL SAG_FreeTriangleID( grdI )

IF( ALLOCATED(nList) )THEN
  DO i = 1,triT%nodeT%nnode
    IF( ASSOCIATED(nList(i)%neighbor) )DEALLOCATE( nList(i)%neighbor,STAT=alloc_stat )
  END DO
  DEALLOCATE( nList,STAT=alloc_stat )
END IF

IF( ALLOCATED(m1) )DEALLOCATE( m1,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE CreateLiquidTotal( itot,FieldX,grdI )

!================
! Assumes the Liquid Plot Kinds are in the following order
! 1 Vapor
! 2 Liquid
! 3 Total (Sum of Vapor + Liquid)
! 4 Total w/o 2nd Evaporation
!================

USE plotlist_fi
USE field_fd
USE scipuff_fi
USE files_fi
USE sagdef_fd
USE sagmrg_usr
USE surface_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: itot       !Type of Total 1=sum 2=read
INTEGER,                INTENT( INOUT ) :: grdI       !Grid
TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: FieldX     !Field descriptor

INTEGER grdIL, grdIV, irv, j

TYPE( SCIPPlotFieldT )        :: Field2Evap
TYPE( SAGgrid_str ), POINTER  :: grdl,grdv

INTEGER, EXTERNAL :: SAG_CombineGridID, SAG_RmvGrdStr, SAG_LastError
INTEGER, EXTERNAL :: SAG_CellAddID, SAG_CellAddVarianceID, SAG_merge

! Copy input Field for modification during create process

Field2Evap = FieldX

!==== Sum Vapor + Liquid

IF( itot == 1 )THEN

! Initialize working grids
  grdIL = -1
  grdIV = -1

! Get Liquid

  Field2Evap%kind = FieldX%kind - 1
  IF( MemoryField )THEN  !------ Copy surface file

    CALL CopySrfField( Field2Evap,srfdep,grdIL )
    IF( nError /= NO_ERROR )GOTO 9998

  ELSE                   !------ Read surface file

    CALL ReadSrfField( Field2Evap,lun_dep,file_dep,grdIL )
    IF( nError /= NO_ERROR )GOTO 9998

  END IF

! Get Vapor

  Field2Evap%kind = FieldX%kind - 2
  IF( MemoryField )THEN  !------ Copy surface file

    CALL CopySrfField( Field2Evap,srfdep,grdIV )
    IF( nError /= NO_ERROR )GOTO 9998

  ELSE                   !------ Read surface file

    CALL ReadSrfField( Field2Evap,lun_dep,file_dep,grdIV )
    IF( nError /= NO_ERROR )GOTO 9998

  END IF
  FieldX%units = TRIM(Field2Evap%units)

! Get Grid pointers

  grdl => SAG_PtrGrdStr( grdIL )             ! Associate "local" grid structure pointer
  IF( .NOT.ASSOCIATED(grdl) )GOTO 9997       ! Real trouble if this is ever an error

  grdv => SAG_PtrGrdStr( grdIV )             ! Associate "local" grid structure pointer
  IF( .NOT.ASSOCIATED(grdv) )GOTO 9997       ! Real trouble if this is ever an error

! Add Means
! Merge Vapor Mean onto Liquid grid in Plot location
! Add Vapor Mean to Liquid Mean
! Clear Plot location

  nfld_mrg = 1    ! number of fields to move
  jfld_mrg = 1    ! first field of donor
  ifld_mrg = 3    ! first field of recipient
  irv = SAG_CombineGridID( grdIL,grdIV,SAG_merge )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'CreateLiquidTotal'
    eMessage = 'Error merging Vapor Mean onto Liquid grid'
    GOTO 9998
  END IF
  irv = SAG_CellAddID( grdIL,1,3,1. )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'CreateLiquidTotal'
    eMessage = 'Error adding Vapor Mean onto Liquid Mean'
    GOTO 9998
  END IF

  DO j = 1,grdl%ncells
    grdl%ipdat(j+2*grdl%mxgrd) = 0.0
  END DO

! Add Variances
! Merge Vapor variance onto Liquid grid in Plot location
! Add Vapor Mean to Liquid Mean
! Clear Plot location

  jfld_mrg = 2    ! first field of donor
  ifld_mrg = 3    ! first field of recipient
  irv = SAG_CombineGridID( grdIL,grdIV,SAG_merge )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'CreateLiquidTotal'
    eMessage = 'Error merging Vapor Mean onto Liquid grid'
    GOTO 9998
  END IF
  irv = SAG_CellAddVarianceID( grdIL,2,3,1. )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'CreateLiquidTotal'
    eMessage = 'Error adding Vapor Mean onto Liquid Mean'
    GOTO 9998
  END IF

  DO j = 1,grdl%ncells
    grdl%ipdat(j+2*grdl%mxgrd) = 0.0
  END DO

! Assign final grid ID value
  grdI = grdIL

! Clean up working grid
  irv  = SAG_RmvGrdStr( grdIV )

!==== Read Total from dep file

ELSE

  Field2Evap%kind = FieldX%kind - 1
  IF( MemoryField )THEN  !------ Copy surface file

    CALL CopySrfField( Field2Evap,srfdep,grdI )
    IF( nError /= NO_ERROR )GOTO 9999

  ELSE                   !------ Read surface file

    CALL ReadSrfField( Field2Evap,lun_dep,file_dep,grdI )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF
  FieldX%units = TRIM(Field2Evap%units)

END IF

9999 CONTINUE

RETURN

9998 CONTINUE

! Remove working grids on error
irv  = SAG_RmvGrdStr( grdIL )
irv  = SAG_RmvGrdStr( grdIV )
GOTO 9999

9997 CONTINUE
nError = UK_ERROR
eRoutine = 'CreateLiquidTotal'
eMessage = 'Unassociated grid pointer encountered'
GOTO 9998

END

!==============================================================================

INTEGER FUNCTION DeleteFieldF( userID,FieldID )

USE sagdef_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: userID       !USER ID tag
INTEGER, INTENT( INOUT ) :: FieldID      !SAG grid ID

INTEGER irv
INTEGER currentState

INTEGER, EXTERNAL :: SAG_RmvGrdStr

DeleteFieldF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( Aborted() )GOTO 9999

IF( FieldID < -1 )THEN

  nError   = UK_ERROR
  eRoutine = 'DeleteFieldF'
  eMessage = 'Invalid SAG field ID'
  WRITE(eInform,'(A,I5,A)')'Field ID =',FieldID,' : (Must be positive)'

ELSE IF( FieldID <= 0 )THEN

  DeleteFieldF = SCIPsuccess  !Handle 0,-1 as valid but empty field IDs

ELSE

  irv = SAG_RmvGrdStr( FieldID )

  IF( irv == SAG_OK )THEN
    CALL RemoveMaxLoc( FieldID )
    DeleteFieldF = SCIPsuccess
    FieldID         = 0
  ELSE
    nError   = UK_ERROR
    eRoutine = 'DeleteFieldF'
    eMessage = 'Error deleting SAG field'
    WRITE(eInform,'(A,I5)')'Field ID =',FieldID
  END IF

END IF

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!====================================================================

INTEGER FUNCTION GetFieldDomainF( userID,grdI,m0,n0,xmin,ymin,dx,dy )

USE SCIMgr_fd
USE sagstr_fd
USE PtrGrdStrItf
USE SCIMgrState
USE abort
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: userID       !USER ID tag
INTEGER, INTENT( IN  ) :: grdI         !SAG grid ID
INTEGER, INTENT( OUT ) :: m0           !Base grid number X direction
INTEGER, INTENT( OUT ) :: n0           !Base grid number Y direction
REAL,    INTENT( OUT ) :: xmin         !Origin X direction
REAL,    INTENT( OUT ) :: ymin         !Origin Y direction
REAL,    INTENT( OUT ) :: dx           !Base grid size X direction
REAL,    INTENT( OUT ) :: dy           !Base grid size Y direction

INTEGER currentState, irv

!==== initialize

GetFieldDomainF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== count contour lines and points

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL GetFieldDomain( grdI,m0,n0,xmin,ymin,dx,dy )

  IF( nError == NO_ERROR )GetFieldDomainF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!====================================================================

SUBROUTINE GetFieldDomain( grdI,m0,n0,xmin,ymin,dx,dy )

USE SCIMgr_fd
USE sagstr_fd
USE PtrGrdStrItf
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: grdI         !SAG grid ID
INTEGER, INTENT( OUT ) :: m0           !Base grid number X direction
INTEGER, INTENT( OUT ) :: n0           !Base grid number Y direction
REAL,    INTENT( OUT ) :: xmin         !Origin X direction
REAL,    INTENT( OUT ) :: ymin         !Origin Y direction
REAL,    INTENT( OUT ) :: dx           !Base grid size X direction
REAL,    INTENT( OUT ) :: dy           !Base grid size Y direction

TYPE( SAGgrid_str ), POINTER  :: grd

grd => SAG_PtrGrdStr( grdI )         ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(grd) )THEN
  nError = UK_ERROR
  eRoutine = 'FieldDomain'
  eMessage = 'Unable to determine field domain'
  eInform = 'Field pointer points to a non-existent grid'
  GOTO 9999
END IF

m0 = grd%nx
n0 = grd%ny

xmin = grd%xmin
ymin = grd%ymin

dx = grd%dx
dy = grd%dy

9999 CONTINUE

RETURN
END

!============================================================================

INTEGER FUNCTION GetFieldValueF( userID,grdI,Field,PlotType,xpnt,ypnt,fpnt )

USE SCIMgr_fd
USE SCIMgrState
USE error_fi

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
REAL,                  INTENT( IN  ) :: xpnt          !Location (project coords)
REAL,                  INTENT( IN  ) :: ypnt          !Location (project coords)
REAL,                  INTENT( OUT ) :: fpnt          !Field value

INTEGER  currentState, irv

GetFieldValueF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

CALL GetFieldValue( grdI,Field,PlotType,xpnt,ypnt,fpnt )
IF( nError /= NO_ERROR )GOTO 9999

GetFieldValueF = SCIPsuccess

9999 CONTINUE

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!============================================================================

INTEGER FUNCTION GetFieldValuesF( userID,grdI,Field,PlotType,npnt,xpnt,ypnt,fpnt )

USE SCIMgr_fd
USE SCIMgrState
USE abort
USE error_fi

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
INTEGER,               INTENT( IN  ) :: npnt          !Number of points
REAL, DIMENSION(npnt), INTENT( IN  ) :: xpnt          !Location (project coords)
REAL, DIMENSION(npnt), INTENT( IN  ) :: ypnt          !Location (project coords)
REAL, DIMENSION(npnt), INTENT( OUT ) :: fpnt          !Field value

INTEGER  i
INTEGER  currentState, irv

GetFieldValuesF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( Aborted() )GOTO 9999

i = 1
irv = SCIPsuccess
CALL InitAbortedCount()
DO WHILE( irv == SCIPsuccess .AND. i<=npnt )
  IF( AbortedCount() )GOTO 9999
  CALL GetFieldValue( grdI,Field,PlotType,xpnt(i),ypnt(i),fpnt(i) )
  IF( nError /= NO_ERROR )GOTO 9999
  i = i + 1
END DO

GetFieldValuesF = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!============================================================================

SUBROUTINE GetFieldValue( grdI,Field,PlotType,xpnt,ypnt,fpnt )

USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE PtrGrdStrItf
USE SCIMgrState
USE sagdef_fd

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
REAL,                  INTENT( IN  ) :: xpnt          !Location (project coords)
REAL,                  INTENT( IN  ) :: ypnt          !Location (project coords)
REAL,                  INTENT( OUT ) :: fpnt          !Field value

INTEGER nfld
LOGICAL log_interp

INTEGER, DIMENSION(2) :: ifld
REAL,    DIMENSION(2) :: fval

REAL x, y, xmin, xmax

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER, EXTERNAL :: PlotFunctionPointValue

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'GetFieldValue'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

log_interp = Field%interpType == SCIPoff

y = ypnt
x = xpnt
IF( Field%coordinate%mode == HD_LATLON )THEN
  xmin = grd%xmin
  xmax = grd%xmin + grd%nx*grd%dx
  CALL check_lon( x,xmin,xmax )
  IF( nError /= NO_ERROR )THEN
    eInform = 'Setting field location'
    GOTO 9999
  END IF
END IF

!==============================================================================
! Check if plot field has already been created
!==============================================================================

IF( grd%PlotType /= PlotType%type .OR. grd%PlotData /= PlotType%data )THEN

  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )

      PlotFunc_nfun = PLOT_FUNCTION_NULL
      PlotFunc_data = 0.0

    CASE( HP_PROB )

      PlotFunc_nfun = PLOT_FUNCTION_PROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_EXCEED )

      PlotFunc_nfun = PLOT_FUNCTION_ICPROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_VARIANCE )

      PlotFunc_nfun =  PLOT_FUNCTION_SWITCH
      PlotFunc_data =  0.0

    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'SCIPGetFieldValue'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9999

  END SELECT

!==============================================================================
! Set auxiliary plot data
!==============================================================================
  IF( grd%naux <= 0 )THEN
    AuxType = 0
  ELSE
    AuxType = grd%ipblk(1)%type
    AuxData => grd%aux(1)
  END IF

  IF( grd%type == SAG_GRID_NONE )THEN
    PlotFunc_small = -1.E36  !set to some parameter?
  ELSE
    PlotFunc_small = 1.E-30  !set to some parameter?
  END IF
  PlotFunc_spv = HP_SPV

  CALL GetPlotFieldVal( x,y,fpnt,grdI,PlotFunctionPointValue,log_interp )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

!-------- Get data values

  nfld     = 1
  ifld(1)  = 3         !Plot field
  CALL GetPointVal( x,y,fval,grdI,nfld,ifld,log_interp )
  IF( nError /= NO_ERROR )GOTO 9999

  fpnt = fval(1)

END IF

9999 CONTINUE

RETURN
END

!============================================================================

INTEGER FUNCTION GetFieldMCValueF( userID,grdI,Field,PlotType,xpnt,ypnt,fpnt )

USE SCIMgr_fd
USE SCIMgrState
USE error_fi

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
REAL,                  INTENT( IN  ) :: xpnt          !Location (project coords)
REAL,                  INTENT( IN  ) :: ypnt          !Location (project coords)
REAL,DIMENSION(*),     INTENT( OUT ) :: fpnt          !Field values

INTEGER  currentState, irv

GetFieldMCValueF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

CALL GetFieldMCValue( grdI,Field,PlotType,xpnt,ypnt,fpnt )
IF( nError /= NO_ERROR )GOTO 9999

GetFieldMCValueF = SCIPsuccess

9999 CONTINUE

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!============================================================================

SUBROUTINE GetFieldMCValue( grdI,Field,PlotType,xpnt,ypnt,fpnt )

USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE PtrGrdStrItf
USE SCIMgrState
USE sagdef_fd

!  Returns the field value at a specified location for field no. ifld
!  from the SAG grid identified by grdI.
!
!  NB Assumes the field has been "pushed" to the bottom level, since it
!     does not use gradients

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
REAL,                  INTENT( IN  ) :: xpnt          !Location (project coords)
REAL,                  INTENT( IN  ) :: ypnt          !Location (project coords)
REAL,DIMENSION(*),     INTENT( OUT ) :: fpnt          !Field values

INTEGER i,nfld, alloc_stat
LOGICAL log_interp

INTEGER, DIMENSION(:), ALLOCATABLE :: ifld

REAL x, y, xmin, xmax

TYPE( SAGgrid_str ), POINTER :: grd

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'GetFieldValue'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

log_interp = Field%interpType == SCIPoff

y = ypnt
x = xpnt
IF( Field%coordinate%mode == HD_LATLON )THEN
  xmin = grd%xmin
  xmax = grd%xmin + grd%nx*grd%dx
  CALL check_lon( x,xmin,xmax )
  IF( nError /= NO_ERROR )THEN
    eInform = 'Setting field location'
    GOTO 9999
  END IF
END IF

!==============================================================================
! Check if plot field has already been created
!==============================================================================

IF( grd%PlotType /= PlotType%type .OR. grd%PlotData /= PlotType%data )THEN

  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )

      PlotFunc_nfun = PLOT_FUNCTION_NULL
      PlotFunc_data = 0.0

    CASE( HP_PROB )

      PlotFunc_nfun = PLOT_FUNCTION_PROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_EXCEED )

      PlotFunc_nfun = PLOT_FUNCTION_ICPROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_VARIANCE )

      PlotFunc_nfun =  PLOT_FUNCTION_SWITCH
      PlotFunc_data =  0.0

    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'SCIPGetFieldValue'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9999

  END SELECT

!==============================================================================
! Set auxiliary plot data
!==============================================================================
  IF( grd%naux <= 0 )THEN
    AuxType = 0
  ELSE
    AuxType = grd%ipblk(1)%type
    AuxData => grd%aux(1)
  END IF

  IF( grd%type == SAG_GRID_NONE )THEN
    PlotFunc_small = -1.E36  !set to some parameter?
  ELSE
    PlotFunc_small = 1.E-30  !set to some parameter?
  END IF
  PlotFunc_spv = HP_SPV

  CALL GetPlotFieldMCVal( x,y,fpnt,grdI,log_interp )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

!-------- Get data values

  nfld     = grd%mxfld

  ALLOCATE(ifld(nfld),STAT=alloc_stat)
  DO i = 1,nfld
    ifld(i) = i
  END DO

  CALL GetPointVal( x,y,fpnt,grdI,nfld,ifld,log_interp )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!*******************************************************************************
!                GetFieldSizeF
!*******************************************************************************
INTEGER FUNCTION GetFieldSizeF( UserID,grdI,Field,PlotType,nNode,nTri )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE error_fi
USE write_fd
USE write_noFile
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN  ) :: UserID     !User ID tag
INTEGER,               INTENT( IN  ) :: grdI       !SAG grid ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field        !Field descriptor
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType     !Plot definition
INTEGER,               INTENT( OUT ) :: nNode,nTri !Number of Nodes,Triangles

!==============================================================================
! Local Variables
!==============================================================================
INTEGER            :: irv
INTEGER            :: currentState
TYPE( ARAPWriteT ) :: GUIWrite !Draw instructions

!==============================================================================
! Dummy Variables
!==============================================================================
TYPE( SCIPContourHeaderT )                :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(1) :: contourList  !Contour array
INTEGER                                   :: nComment !User supplied comments
TYPE( char128T ),            DIMENSION(1) :: Comment  !User supplied comments

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_InitError

INTERFACE
  SUBROUTINE WriteField( grdID,Field,PlotType,contourHead,contourList,GUIWrite,nComment,Comment )
    USE field_fd
    USE contourlist_fd
    USE charT_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPWriteT ),                        INTENT( IN    ) :: GUIWrite     !Draw instructions
    INTEGER,                                   INTENT( IN    ) :: nComment     !Number of User supplied comments
    TYPE( char128T ),     DIMENSION(nComment), INTENT( IN    ) :: Comment      !User supplied comments
  END SUBROUTINE WriteField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
GetFieldSizeF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==============================================================================
! Initialize error
!==============================================================================
IF( Aborted() )GOTO 9999

CALL init_error()

irv = SAG_InitError()

nComment                  = 0
Comment(1)%string         = ' '
contourHead%number        = 0
contourHead%scale         = 1
contourHead%labelMode     = PLOT_OFF
contourHead%drawMode      = PLOT_OFF
contourHead%unit          = 'default'
contourList(1)%contour    = 0.0
contourList(1)%value      = 0.0
contourList(1)%label      = ' '
contourList(1)%area       = 0.0
contourList(1)%population = 0.0
GUIWrite%mode      = COUNT_FILE
CALL WriteField( grdI,Field,PlotType,contourHead,contourList, &
                      GUIWrite,nComment,Comment )

IF( nError /= NO_ERROR )THEN
  eRoutine = 'SCIPGetField'
  GOTO 9999
END IF

nNode = numNodes
nTri  = numTriangles

GetFieldSizeF = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                GetFieldF
!*******************************************************************************
INTEGER FUNCTION GetFieldF( UserID,grdI,Field,PlotType,nodes,triangles )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE error_fi
USE write_fd
USE write_noFile
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                     INTENT( IN  ) :: UserID     !User ID tag
INTEGER,                                     INTENT( IN  ) :: grdI       !SAG grid ID
TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
TYPE( SCIPPlotFieldNodeT ),     DIMENSION(*),INTENT( OUT ) :: nodes        !Plot definition
TYPE( SCIPPlotFieldTriangleT ), DIMENSION(*),INTENT( OUT ) :: triangles    !Plot definition

!==============================================================================
! Local Variables
!==============================================================================
INTEGER            :: irv
INTEGER            :: ios
INTEGER            :: i
INTEGER            :: currentState
TYPE( ARAPWriteT ) :: GUIWrite !Draw instructions
REAL               :: z

!==============================================================================
! Dummy Variables
!==============================================================================
TYPE( SCIPContourHeaderT )                :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(1) :: contourList  !Contour array
INTEGER                                   :: nComment !User supplied comments
TYPE( char128T ),            DIMENSION(1) :: Comment  !User supplied comments

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_InitError

INTERFACE
  SUBROUTINE WriteField( grdID,Field,PlotType,contourHead,contourList,GUIWrite,nComment,Comment )
    USE field_fd
    USE contourlist_fd
    USE charT_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPWriteT ),                        INTENT( IN    ) :: GUIWrite     !Draw instructions
    INTEGER,                                   INTENT( IN    ) :: nComment     !Number of User supplied comments
    TYPE( char128T ),     DIMENSION(nComment), INTENT( IN    ) :: Comment      !User supplied comments
  END SUBROUTINE WriteField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
GetFieldF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==============================================================================
! Initialize error
!==============================================================================
IF( Aborted() )GOTO 9999

CALL init_error()

irv = SAG_InitError()

nComment                  = 0
Comment(1)%string         = ' '
contourHead%number        = 0
contourHead%scale         = 1
contourHead%labelMode     = PLOT_OFF
contourHead%drawMode      = PLOT_OFF
contourHead%unit          = 'default'
contourList(1)%contour    = 0.0
contourList(1)%value      = 0.0
contourList(1)%label      = ' '
contourList(1)%area       = 0.0
contourList(1)%population = 0.0
GUIWrite%mode      = NO_FILE
CALL WriteField( grdI,Field,PlotType,contourHead,contourList, &
                      GUIWrite,nComment,Comment )

IF( nError /= NO_ERROR )THEN
  eRoutine = 'GetFieldF'
  GOTO 9999
END IF

IF( field%category /= HP_VSLICE )THEN
  z = Field%coordinate%vertSlice%startPt%x
ELSE
  z = 0.0
END IF

IF( Aborted() )GOTO 9999

DO i = 1,numNodes
  nodes(i) = nfNode(i)
  nodes(i)%z = z
END DO

IF( Aborted() )GOTO 9999

DO i = 1,numTriangles
  triangles(i) = nfTri(i)
END DO

GetFieldF = SCIPsuccess

9999 CONTINUE

IF( ALLOCATED(nfNode) )DEALLOCATE( nfNode,STAT=ios )
IF( ALLOCATED(nfTri)  )DEALLOCATE( nfTri, STAT=ios )
numNodes = 0
numTriangles = 0

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                GetFieldAssocSizeF
!*******************************************************************************
INTEGER FUNCTION GetFieldAssocSizeF( UserID,grdI,nPlots,nLines,nPoints )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: UserID
INTEGER, INTENT( IN  ) :: grdI
INTEGER, INTENT( OUT ) :: nPlots
INTEGER, INTENT( OUT ) :: nLines
INTEGER, INTENT( OUT ) :: nPoints

INTEGER irv, currentState

INTEGER, EXTERNAL :: SAG_InitError

!==============================================================================
! Initialize return value
!==============================================================================
GetFieldAssocSizeF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==============================================================================
! Initialize error
!==============================================================================
IF( Aborted() )GOTO 9999

CALL init_error()

irv = SAG_InitError()

CALL CheckAdjointMax( grdI,nPlots,nLines,nPoints )

GetFieldAssocSizeF = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                GetFieldAssocDataF
!*******************************************************************************
INTEGER FUNCTION GetFieldAssocDataF( UserID,grdI,Lines,Points,Titles,Axes,LineID )

USE field_fd
USE SCIMgr_fd
USE error_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: UserID
INTEGER, INTENT( IN  ) :: grdI
TYPE( SCIPLineT  ), DIMENSION(*), INTENT( OUT ) :: Lines
TYPE( SCIPPointT ), DIMENSION(*), INTENT( OUT ) :: Points
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Titles
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Axes
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: LineID

INTEGER irv, currentState

INTEGER, EXTERNAL :: SAG_InitError

!==============================================================================
! Initialize return value
!==============================================================================
GetFieldAssocDataF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==============================================================================
! Initialize error
!==============================================================================
IF( Aborted() )GOTO 9999

CALL init_error()

irv = SAG_InitError()

CALL GetAdjointMax( grdI,Lines,Points,Titles,Axes,LineID )

GetFieldAssocDataF = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
