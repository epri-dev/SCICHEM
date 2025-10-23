SUBROUTINE create_it( srf )

USE scipuff_fi
USE sagdef_fd
USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: srf

REAL, PARAMETER :: FAST_FRAC = 0.00038 !normally have 10 cells -> limits to 8 levels (0.1/(2**8)) but get 9 with global domain
!REAL, PARAMETER :: FAST_FRAC = 0.00078 !normally have 10 cells -> limits to 7 levels (0.1/(2**7)) but get 8 with global domain
!REAL, PARAMETER :: FAST_FRAC = 0.00150 !normally have 10 cells -> limits to 6 levels (0.1/(2**6)) but get 7 with global domain
!REAL, PARAMETER :: FAST_FRAC = 0.00310 !normally have 10 cells -> limits to 5 levels (0.1/(2**5)) but get 6 with global domain

INTEGER irv, nxs, nys, mlev
REAL    tx, dxs, dys, xmap, ymap, xbar, ybar, srf_delmin
INTEGER SAG_InitGridStr, SAG_Create

!------ Set grid parameters

tx    = 0.0
dxs   = FLOAT(nx)*dxg
dys   = FLOAT(ny)*dyg

CALL set_surf_grid( dxs,dys,nxs,nys )

IF( lChemAmbDosRes )THEN       !Ensure resolution is adequate for ambient fields
  nxs = MAX(nxs,CEILING(0.999*dxs/xChemAmbDosRes))
  nys = MAX(nys,CEILING(0.999*dys/xChemAmbDosRes))
END IF

dxs = dxs/FLOAT(nxs)
dys = dys/FLOAT(nys)
IF( delmin == DEF_VAL_R )THEN
  IF( BTEST(run_mode,FAST_MODE) )THEN
    xbar = xmin + 0.5*(nxs-1)*dxs
    ybar = ymin + 0.5*(nys-1)*dys
    CALL mapfac( xbar,ybar,xmap,ymap )
    mlev = MAX(INT(LOG(FAST_FRAC*FLOAT(MIN(nxs,nys)))*RLOGR2),0)
  ELSE
    mlev = 99
  END IF
ELSE IF( delmin > 0. )THEN
  xbar = xmin + 0.5*(nxs-1)*dxs
  ybar = ymin + 0.5*(nys-1)*dys
  CALL mapfac( xbar,ybar,xmap,ymap )
  mlev = MAX(INT(LOG(delmin/MAX(dxs/xmap,dys/ymap))*RLOGR2),0)
ELSE
  mlev = 99
END IF
!mlev = -mlev

IF( delmin == DEF_VAL_R )THEN
  IF( BTEST(run_mode,FAST_MODE) )THEN
    srf_delmin = MIN(dxs/xmap,dys/ymap)*(0.5**ABS(mlev))
  ELSE
    srf_delmin = 0.
  END IF
ELSE IF( delmin == 0. )THEN
  srf_delmin = 0.
ELSE
  srf_delmin = MIN(dxs/xmap,dys/ymap)*(0.5**ABS(mlev))
END IF

!-----  Initialize adaptive grid

irv = SAG_InitGridStr( srf,tx,nxs,nys,xmin,ymin,dxs,dys,mlev,srf_delmin )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'create_surface'
  eMessage = 'Error initializing surface grid structure'
  GOTO 9999
END IF

irv = SAG_Create( srf )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'create_surface'
  eMessage = 'Error creating surface file'
  GOTO 9999
END IF

9999  CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE set_surf_grid( dxs,dys,nxs,nys )

IMPLICIT NONE

REAL,    INTENT( IN  ) :: dxs, dys
INTEGER, INTENT( OUT ) :: nxs, nys

IF( dxs > dys )THEN
  nxs = 10
  nys = MAX(3,NINT(FLOAT(nxs)*dys/dxs))
ELSE
  nys = 10
  nxs = MAX(3,NINT(FLOAT(nys)*dxs/dys))
END IF

RETURN
END

!=======================================================================

SUBROUTINE read_surface( lun,file,srf )

USE error_fi
USE sagdef_fd
USE surface_fi

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: lun  !file unit number
CHARACTER(*), INTENT( IN  ) :: file !file name
INTEGER,      INTENT( OUT ) :: srf  !grid structure id

INTEGER      irv, nvar
REAL         tx
CHARACTER(8) names(1)

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_ReadBreakID
INTEGER, EXTERNAL :: CheckSrfEOF

!------ Setup new SAG grid structure pointer

irv = SAG_NewGrdStr( srf )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'read_surface'
  eMessage = 'Error creating surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,MAXSG,0,0,srf )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'read_surface'
  eMessage = 'Error creating surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!----- read grid structure from last timebreak

tx   = 1.E+36  ! read last timebreak
nvar = -1      ! read all fields
names(1) = ' ' ! not used but should define anyway

irv = SAG_ReadBreakID( srf,tx,nvar,names )
IF( irv /= SAG_OK )THEN
  irv = CheckSrfEOF( srf )
  IF( irv /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'read_surface'
    eMessage = 'Error reading surface file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION CheckSrfEOF( srfI )

!----- zero top level grid if at eof

USE sagstr_fd
USE sagerr_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: srfI

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER i, iv, i0

srf => SAG_PtrGrdStr( srfI ) ! Associate "local" grid structure pointer

IF( srf%status == SAG_EOF_REC )THEN

  srf%ncells = srf%nx*srf%ny

  DO i = 1,srf%ncells
    srf%ipgrd(i) = 0
    DO iv = 1,srf%nvart
      i0 = (iv-1)*srf%mxgrd
      srf%ipdat(i0+i) = 0.
    END DO
  END DO

  srf%status  = SAG_HEAD_REC

  CheckSrfEOF = 0

ELSE

  CheckSrfEOF = 1

END IF

RETURN
END

!=======================================================================

SUBROUTINE init_srf_blocks( nmatl )

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE srfaux_fi
USE UtilMtlAux


IMPLICIT NONE

!  Initialize the surface field arrays

INTEGER, INTENT( IN ) :: nmatl

INTEGER nhaz, nvs, nblk, i, j, ihaz, ityp
INTEGER id, type, alloc_stat
INTEGER nsg, ishift

LOGICAL lhaz
LOGICAL ltot

CHARACTER(64) bname

INTEGER, EXTERNAL :: output_groups
LOGICAL, EXTERNAL :: IsEvap
LOGICAL, EXTERNAL :: IsMultiDep, IsMultiDos

!------ Initialize

depint      = .FALSE.
nsrf_blocks = 0
n_effects   = 0

nhaz = 1

!------ Initialize puff surface structures

ALLOCATE( srf_puff(ntypp,2),STAT=alloc_stat )

DO i = 1,ntypp
  srf_puff(i,1)%nblocks = 0
  srf_puff(i,2)%nblocks = 0
  srf_puff(i,1)%icld    = 0
  srf_puff(i,2)%icld    = 0
  ALLOCATE( srf_puff(i,1)%iblk(1),STAT=alloc_stat )
  ALLOCATE( srf_puff(i,2)%iblk(1),STAT=alloc_stat )
END DO

!------ Deposition fields

nblk     = 0
nvs      = 0
nsrf_aux = 0

DO ityp = 1,nmatl

  nsg = output_groups( material(ityp) )

  DO ihaz = 1,nhaz !set up for combined run

    lhaz = ihaz == 2

    type = SBLK_STD
    IF( IsMultiDep(material(ityp)%icls) )type = SBLK_MULTI_DEP

    IF( material(ityp)%lsrft .OR. (type == SBLK_MULTI_DEP .AND. nsg>1) )THEN
      material(ityp)%lsrft = .TRUE.

      ltot = .TRUE.
      id   = ityp
      CALL set_block_name( material(ityp),0,bname )
      CALL init_sblock( nblk,nvs,type,id,ltot,lhaz,bname )
      IF( nError /= NO_ERROR )GOTO 9999

      CALL init_spuff( srf_puff(1,1),srf_block(nblk),nblk,material(ityp),0 )
      IF( nError /= NO_ERROR )GOTO 9999

      IF( type == SBLK_MULTI_DEP )type = SBLK_STD

    END IF

    IF( material(ityp)%lsrfg .OR. type == SBLK_MULTI_DEP )THEN
      material(ityp)%lsrfg = .TRUE.
      ltot = .FALSE.

      DO i = 1,nsg
        id = ityp + 65536*i

        IF( IsEvap(material(ityp)%icls) .AND. i == nsg )type = SBLK_EVAP

        CALL set_block_name( material(ityp),i,bname )
        CALL init_sblock( nblk,nvs,type,id,ltot,lhaz,bname )
        IF( nError /= NO_ERROR )GOTO 9999

        CALL init_spuff( srf_puff(1,1),srf_block(nblk),nblk,material(ityp),i )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO

    END IF

  END DO ! end loop for hazard combination

END DO

nEvapBlk = 0
nEvapFld = 5
IF( substrate_type /= 0    )nEvapFld = nEvapFld + 2
IF( BTEST(run_mode,EVAP2D) )nEvapFld = nEvapFld + 1
DO i = 1,nblk
  IF( srf_block(i)%type == SBLK_EVAP )THEN
    nEvapBlk = nEvapBlk + 1
  END IF
END DO

IF( nEvapBlk > 0 )THEN
  ALLOCATE( evapTemp(MAXSG*nEvapFld*nEvapBlk),STAT=alloc_stat )
  IF( alloc_stat == 0 )ALLOCATE( evapFld(nEvapBlk),evapTempID(nblk),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eMessage = 'Unable to allocate temporary storage'
    eInform  = 'for secondary evaporation materials'
    eRoutine = 'init_srf_blocks'
    GOTO 9999
  END IF

  nEvapBlk = 0
  DO i = 1,nblk
    IF( srf_block(i)%type == SBLK_EVAP )THEN
      nEvapBlk = nEvapBlk + 1
      evapFld(nEvapBlk) = srf_block(i)%field
      evapTempID(i) = nEvapBlk
    ELSE
      evapTempID(i) = 0
    END IF
  END DO
END IF

!------ Reset ntyps to account for effects fields

ntyps       = nvs ! - 1
surface     = ntyps > 0
nsrf_blocks = nblk
ndep_blocks = nblk

!------ Setup dose fields

DO ityp = 1,nmatl

  nsg = output_groups( material(ityp) )

  DO ihaz = 1,nhaz

    type = SBLK_STD
    IF( IsMultiDos(material(ityp)%icls) )type = SBLK_MULTI_DOS

    lhaz = ihaz == 2

    ltot = .TRUE.
    id   = ityp

    IF( material(ityp)%ldost .OR. (type == SBLK_MULTI_DOS .AND. nsg>1) )THEN
      material(ityp)%ldost = .TRUE.

      CALL set_block_name( material(ityp),0,bname )
      CALL init_sblock( nblk,nvs,type,id,ltot,lhaz,bname )
      IF( nError /= NO_ERROR )GOTO 9999

      CALL init_spuff( srf_puff(1,2),srf_block(nblk),nblk,material(ityp),0 )
      IF( nError /= NO_ERROR )GOTO 9999

      IF( type == SBLK_MULTI_DOS )type = SBLK_STD

      IF( BTEST(run_mode,REVERSE_MODE) )srf_block(nblk)%field = 1

    END IF

    IF( material(ityp)%ldosg .OR. type == SBLK_MULTI_DOS )THEN
      material(ityp)%ldosg = .TRUE.

      ltot = .FALSE.

      DO i = 1,nsg

        id = ityp + 65536*i

        CALL set_block_name( material(ityp),i,bname )
        CALL init_sblock( nblk,nvs,type,id,ltot,lhaz,bname )
        IF( nError /= NO_ERROR )GOTO 9999

        CALL init_spuff( srf_puff(1,2),srf_block(nblk),nblk,material(ityp),i )
        IF( nError /= NO_ERROR )GOTO 9999

        IF( BTEST(run_mode,REVERSE_MODE) )srf_block(nblk)%field = 1

      END DO

    END IF

  END DO

END DO

!------ Reset ntypd to account for effects fields

ntypd       = nvs - ntyps
dose        = ntypd > 0
nsrf_blocks = nblk

IF( nsrf_blocks > ndep_blocks .AND. ndep_blocks > 0 )THEN
  ishift = srf_block(ndep_blocks+1)%field - 1
  DO i = ndep_blocks+1,nsrf_blocks
    srf_block(i)%field =  srf_block(i)%field - ishift
  END DO
  DO i = 1,ntypp
    DO j = 1,srf_puff(i,2)%nblocks
      srf_puff(i,2)%iblk(j) = srf_puff(i,2)%iblk(j) - ndep_blocks
    END DO
  END DO
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE init_spuff( spuff,block,nblk,matl,isg )

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE UtilMtlAux

IMPLICIT NONE

TYPE( sfield_puff  ), DIMENSION(*), INTENT( OUT ) :: spuff
TYPE( sfield_block ),               INTENT( IN  ) :: block
INTEGER,                            INTENT( IN  ) :: nblk, isg

TYPE( material_str ) matl

INTEGER ips, ipe, ip

INTEGER, EXTERNAL :: num_puff_types
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

ips = matl%ioffp

IF( BTEST(block%flags,SFLAG_TOT) )THEN
  ips = ips + 1
  ipe = ips + num_puff_types( matl ) - 1
ELSE
  IF( IsLiquid(matl%icls) .AND. isg == 2 )THEN
    ips = ips + 2
    ipe = ips + GetSubgroups( matl,mat_aux )  ! Includes aerosol from liquid dosage/deposition
  ELSE
    ips = ips + isg
    ipe = ips
  END IF
END IF

!---- Eliminate aerosol group for old version liquid plots

IF( IsLiquid(matl%icls) )THEN
  IF( iversion < 2400 .AND. ipe>ips )ipe = ipe - 1
END IF

DO ip = ips,ipe
  CALL set_spuff( spuff(ip),nblk )
END DO

!---- Include liquid aerosol type for vapor dosage/deposition

IF( IsLiquid(matl%icls) .AND. isg == 1 .AND. iversion >= 2400 )THEN
  ip = matl%ioffp + GetSubgroups( matl,mat_aux ) + 2
  CALL set_spuff( spuff(ip),nblk )
END IF

!---- Set additional wet particle types

IF( IsWetParticle(matl%icls) .AND. .NOT.BTEST(block%flags,SFLAG_TOT) )THEN
  ips = ips + GetSubgroups( matl,mat_aux )
  ipe = ipe + GetSubgroups( matl,mat_aux )
  DO ip = ips,ipe
    CALL set_spuff( spuff(ip),nblk )
  END DO
END IF

RETURN
END

!=======================================================================

SUBROUTINE set_spuff( spuff,nblk )

USE surface_fi
USE reallocate
USE error_fi

IMPLICIT NONE

TYPE( sfield_puff), INTENT( INOUT ) :: spuff
INTEGER,            INTENT( IN    ) :: nblk

INTEGER npblk, irv

npblk = spuff%nblocks + 1
IF( npblk > SIZE(spuff%iblk) )THEN
  irv = reallocate_integer1d( spuff%iblk,1 )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'init_spuff'
    eMessage = 'Error reallocating surface puff storage'
    GOTO 9999
  END IF
END IF

spuff%iblk(npblk) =  nblk
spuff%nblocks     = npblk

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE init_sblock( nblk,nvs,type,id,ltot,lhaz,bname )

USE scipuff_fi
USE surface_fi
USE srfparam_fd

IMPLICIT NONE

INTEGER,      INTENT( INOUT ) :: nblk, nvs
INTEGER,      INTENT( IN )    :: type, id
LOGICAL,      INTENT( IN )    :: ltot, lhaz
CHARACTER(*), INTENT( IN )    :: bname

INTEGER i0
INTEGER imat

CALL alloc_sblock( nblk )
IF( nError /= NO_ERROR )GOTO 9999

srf_block(nblk)%type  = type
srf_block(nblk)%field = nvs + 1
srf_block(nblk)%id    = id
srf_block(nblk)%flags = 0
srf_block(nblk)%iaux  = 0
srf_block(nblk)%name  = TRIM(bname)

IF( ltot )THEN
  srf_block(nblk)%flags = IBSET(srf_block(nblk)%flags,SFLAG_TOT)
END IF

IF( INDEX(bname,'vapor')  /= 0 )srf_block(nblk)%flags = IBSET(srf_block(nblk)%flags,SFLAG_VAP)
IF( INDEX(bname,'liquid') /= 0 )srf_block(nblk)%flags = IBSET(srf_block(nblk)%flags,SFLAG_LIQ)

!------ Set surface field names - NB dezone_lev relies on this order
!       (specifically - must have M,V,S and D,X consecutively)

i0 = nvs
CALL alloc_srfnam( nvs,2 )
IF( nError /= NO_ERROR )GOTO 9999

srfnam(i0+1) = 'Mean'
srftyp(i0+1) = DEZONE_MEAN
srfnam(i0+2) = 'Var'
srftyp(i0+2) = DEZONE_VAR
IF( BTEST(run_mode,REVERSE_MODE) )srftyp(i0+2) = DEZONE_MEAN

SELECT CASE( type )

  CASE( SBLK_STD )    !Standard block - mean, var, scale
    i0 = nvs
    CALL alloc_srfnam( nvs,1 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(i0+1) = 'Scl'
    srftyp(i0+1) = DEZONE_SCALE
    IF( BTEST(run_mode,REVERSE_MODE) )srftyp(i0+1) = DEZONE_MEAN

  CASE( SBLK_EVAP )   !Liquid secondary evaporation
    i0 = nvs
    CALL alloc_srfnam( nvs,3 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(i0+1) = 'Scl'
    srftyp(i0+1) = DEZONE_SCALE
    srfnam(i0+2) = 'Drop'     !Droplet diameter (m)
    srftyp(i0+2) = DEZONE_AVG
    srfnam(i0+3) = 'Awet'     !Wetted area
    srftyp(i0+3) = DEZONE_AVG
    IF( substrate_type /= 0 )THEN !Allocate fields for absorption/desorption
      CALL alloc_srfnam( nvs,2 )
      IF( nError /= NO_ERROR )GOTO 9999
      srfnam(i0+4) = 'Sfcm'    !Surface mass
      srftyp(i0+4) = DEZONE_AVG
      srfnam(i0+5) = 'Frnt'    !Frontal depth (m)
      srftyp(i0+5) = DEZONE_AVG
    END IF

    IF( BTEST(run_mode,EVAP2D) )THEN
       i0 = nvs
       CALL alloc_srfnam( nvs,1 )
       IF( nError /= NO_ERROR )GOTO 9999
       srfnam(i0+1) = 'd2'
       srftyp(i0+1) = DEZONE_AVG
    END IF

  CASE( SBLK_MULTI_DOS )   !Multi-component dosage
    i0 = nvs
    CALL alloc_srfnam( nvs,1 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(i0+1) = 'Scl'
    srftyp(i0+1) = DEZONE_SCALE
    imat = id - (id/65536)*65536
    CALL InitSBlockDosMC( imat,nvs )

  CASE( SBLK_MULTI_DEP )   !Multi-component deposition
    i0 = nvs
    CALL alloc_srfnam( nvs,1 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(i0+1) = 'Scl'
    srftyp(i0+1) = DEZONE_SCALE
    imat = id - (id/65536)*65536
    CALL InitSBlockDepMC( imat,nvs )

  CASE DEFAULT

END SELECT

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE InitSBlockDosMC( imat,nvs )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: imat
INTEGER, INTENT( INOUT ) :: nvs

INTEGER mcID

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemSBlockDos( nvs,mat_mc%ID(mcID) )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitSBlockDosMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE InitSBlockDepMC( imat,nvs )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: imat
INTEGER, INTENT( INOUT ) :: nvs

INTEGER mcID

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemSBlockDep( nvs,mat_mc%ID(mcID) )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitSBlockDepMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE alloc_sblock( n )

USE surface_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: n

TYPE( sfield_block ), DIMENSION(:), ALLOCATABLE :: tem_block

INTEGER alloc_stat

IF( n > 0 )THEN
  ALLOCATE( tem_block(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'alloc_sblock'
    eMessage = 'Error allocating temporary surface block'
    GOTO 9999
  END IF
  tem_block = srf_block
  DEALLOCATE( srf_block,STAT=alloc_stat )
END IF

ALLOCATE( srf_block(n+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'alloc_sblock'
  eMessage = 'Error allocating surface block'
  GOTO 9999
END IF

IF( n > 0 )srf_block(1:n) = tem_block

n = n + 1

9999 CONTINUE

DEALLOCATE( tem_block,STAT=alloc_stat )

RETURN
END

!=======================================================================

SUBROUTINE alloc_effblock( n )

USE surface_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: n

TYPE( sfield_effect ), DIMENSION(:), ALLOCATABLE :: tem_block

INTEGER alloc_stat

IF( n > 0 )THEN
  ALLOCATE( tem_block(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'alloc_effblock'
    eMessage = 'Error allocating temporary surface block'
    GOTO 9999
  END IF
  tem_block = srf_effect
  DEALLOCATE( srf_effect,STAT=alloc_stat )
END IF

ALLOCATE( srf_effect(n+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'alloc_effblock'
  eMessage = 'Error allocating surface block'
  GOTO 9999
END IF

IF( n > 0 )srf_effect(1:n) = tem_block

n = n + 1

9999 CONTINUE

DEALLOCATE( tem_block,STAT=alloc_stat )

RETURN
END

!=======================================================================

SUBROUTINE alloc_srfnam( n,inc )

USE surface_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: n
INTEGER, INTENT( IN    ) :: inc

CHARACTER(4), DIMENSION(:), ALLOCATABLE :: tem_nam
INTEGER,      DIMENSION(:), ALLOCATABLE :: tem_typ

INTEGER alloc_stat

IF( n > 0 )THEN
  ALLOCATE( tem_nam(n),tem_typ(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'alloc_srfnam'
    eMessage = 'Error allocating temporary array'
    GOTO 9999
  END IF
  tem_nam = srfnam
  tem_typ = srftyp
  DEALLOCATE( srfnam,srftyp,STAT=alloc_stat )
END IF

ALLOCATE( srfnam(n+inc),srftyp(n+inc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'alloc_srfnam'
  eMessage = 'Error allocating surface name array'
  GOTO 9999
END IF

IF( n > 0 )THEN
  srfnam(1:n) = tem_nam; srftyp(1:n) = tem_typ
END IF

n = n + inc

9999 CONTINUE

DEALLOCATE( tem_nam,tem_typ,STAT=alloc_stat )

RETURN
END

!=======================================================================

SUBROUTINE set_block_name( matl,i,bname )

USE struct_fd

IMPLICIT NONE

TYPE( material_str ), INTENT( IN )  :: matl
INTEGER,              INTENT( IN )  :: i
CHARACTER(*),         INTENT( OUT ) :: bname

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( i == 0 )THEN
  bname = 'Total '//matl%cmat
ELSE IF( IsLiquid(matl%icls) )THEN
  IF( i == 1 )THEN
    bname = TRIM(matl%cmat)//' vapor'
  ELSE
    bname = TRIM(matl%cmat)//' liquid'
  END IF
ELSE IF( IsParticle(matl%icls) .OR. IsWetParticle(matl%icls) )THEN
  WRITE(bname,101) TRIM(matl%cmat),i
101 FORMAT(A,' Size group',I3.2)
ELSE
  bname = matl%cmat
END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemSBlockDep( nvs,ID )

USE chem_fi
USE error_fi
USE files_fi
USE surface_fi
USE srfparam_fd

!====   Reads multi-component definitions from project.imc

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: nvs
INTEGER, INTENT( IN    ) :: ID

INTEGER i
INTEGER ios

WRITE(lun_log,'(A)',IOSTAT=ios) 'Chemistry Species/Group wet and dry deposition Output List:'

!---- Multi-component means

chem => chemMC(ID,1)

DO i = 1,chem%nSpecies

  IF( chem%species(i)%ldep )THEN
    CALL alloc_srfnam( nvs,2 )
    IF( nError /= NO_ERROR )GOTO 9999
    WRITE(lun_log,'(4x,A)',IOSTAT=ios) TRIM(chem%species(i)%name)
    srfnam(nvs-1) = TRIM(chem%species(i)%name(1:4))//'(DRY)'
    srftyp(nvs-1) = DEZONE_AVG
    srfnam(nvs)   = TRIM(chem%species(i)%name(1:4))//'(WET)'
    srftyp(nvs)   = DEZONE_AVG
  END IF

END DO

IF( chem%nOutGroup > 0 )THEN
  DO i = 1,chem%nOutGroup
    CALL alloc_srfnam( nvs,2 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(nvs-1)   = TRIM(chem%OutGroup(i)%Name(1:4))
    srfnam(nvs)     = TRIM(chem%OutGroup(i)%Name(1:4))
    srftyp(nvs-1)   = DEZONE_AVG
    srftyp(nvs)     = DEZONE_AVG
    WRITE(lun_log,'(4x,A)',IOSTAT=ios) TRIM(chem%OutGroup(i)%Name)
  END DO
END IF
WRITE(lun_log,*,IOSTAT=ios)

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemSBlockDos( nvs,ID )

USE chem_fi
USE files_fi
USE error_fi
USE surface_fi
USE srfparam_fd

!====   Reads multi-component definitions from project.imc

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: nvs
INTEGER, INTENT( IN    ) :: ID

INTEGER i
INTEGER ios

WRITE(lun_log,'(A)',IOSTAT=ios) 'Chemistry Species/Group Dose Output List:'

!---- Multi-component means

chem => chemMC(ID,1)

DO i = 1,chem%nSpecies

  IF( chem%species(i)%ldos )THEN
    CALL alloc_srfnam( nvs,1 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(nvs) = TRIM(chem%species(i)%name(1:4))
    srftyp(nvs) = DEZONE_AVG
    WRITE(lun_log,'(4x,A)',IOSTAT=ios) TRIM(chem%species(i)%name)
  END IF

END DO

IF( chem%nOutGroup > 0 )THEN
  DO i = 1,chem%nOutGroup
    CALL alloc_srfnam( nvs,1 )
    IF( nError /= NO_ERROR )GOTO 9999
    srfnam(nvs) = TRIM(chem%OutGroup(i)%Name(1:4))
    srftyp(nvs) = DEZONE_AVG
    WRITE(lun_log,'(4x,A)',IOSTAT=ios) TRIM(chem%OutGroup(i)%Name)
  END DO
END IF
WRITE(lun_log,*,IOSTAT=ios)

9999 CONTINUE

RETURN
END
