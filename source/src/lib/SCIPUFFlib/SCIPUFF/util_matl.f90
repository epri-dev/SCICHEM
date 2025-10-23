!          Auxiliary Storage Rules (v2800 and higher)
!          -----------------------
!
!  1. Dynamic data
!     1.1 Basic dynamics - if dynamic
!                        - gas has NAUX_DYNAMICS_GAS(=12),wcb,ctb,wcp,ctp,ucb,ucp,vcb,vcp,u,v,w,t
!                        - others have NAUX_DYNAMICS_PART(=8),wcb,ctb,wcp,ctp,ucb,ucp,vcb,vcp
!     1.2 Buoyant gas    - if buoy_gas all puffs have NAUX_BUOY(=2) bcb,bcp
!     1.3 Dense gas      - if dense
!                        - all puffs have NAUX_DENSE_GAS(=11)
!                                u,v,dudx,dudy,dvdx.dvdy,u0,X,Y,sn,cs
!
!  2. Total-CC - all multigroup puffs have NAUX_TOTALCC(=2),cctb,cct
!
!  3. Liquid   - liquid puffs have NAUX_LIQUID(=5), d,sigd,t,ccs,tevap
!
!  4. Aerosol  - aerosol puffs have NAUX_AEROSOL(=4), fl,tevap,co,fw
!
!  5. Rad - radiological puffs have tRel
!
!  6. NWPN - NWPN puffs have variable number
!                 NB assumed to be particle material
!
!  7. Static - static puffs have 3 variables, sr,isnxt,isprv
!               Located after typeID%npaux, so doesn't account for NWPN
!
!===============================================================================

INTEGER FUNCTION NumMaterialAux( icls,igrp,ltot )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icls
INTEGER, INTENT( IN ) :: igrp
LOGICAL, INTENT( IN ) :: ltot

INTEGER NumDenseAux

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid
LOGICAL, EXTERNAL :: IsAerosol

NumMaterialAux = 0

IF( dynamic )THEN

  IF( IsGas(icls) )THEN
    NumMaterialAux = NumMaterialAux + NAUX_DYNAMICS_GAS
  ELSE IF( IsParticle(icls) )THEN
    NumMaterialAux = NumMaterialAux + NAUX_DYNAMICS_PART
  ELSE IF( IsWetParticle(icls) )THEN
    NumMaterialAux = NumMaterialAux + NAUX_DYNAMICS_PART
  ELSE IF( IsLiquid(icls) )THEN
    IF( igrp == 1 )THEN
      NumMaterialAux = NumMaterialAux + NAUX_DYNAMICS_GAS
    ELSE
      NumMaterialAux = NumMaterialAux + NAUX_DYNAMICS_PART
    END IF
  END IF

  IF( IsGas(icls) )THEN
    NumDenseAux = NAUX_DENSE_GAS
  ELSE IF( IsParticle(icls) )THEN
    NumDenseAux = NAUX_DENSE_PART
  ELSE IF( IsWetParticle(icls) )THEN
    NumDenseAux = NAUX_DENSE_PART
  ELSE IF( IsLiquid(icls) )THEN
    IF( igrp == 1 )THEN
      NumDenseAux = NAUX_DENSE_GAS
    ELSE
      NumDenseAux = NAUX_DENSE_PART
    END IF
  ELSE IF( IsAerosol(icls) )THEN
    NumDenseAux = NAUX_DENSE_GAS
  END IF

END IF

IF( buoy_gas )NumMaterialAux = NumMaterialAux + NAUX_BUOY

IF( dense_gas )NumMaterialAux = NumMaterialAux + NumDenseAux

IF( ltot )NumMaterialAux = NumMaterialAux + NAUX_TOTALCC

IF( IsLiquid(icls) .AND. igrp > 1 )THEN
  NumMaterialAux = NumMaterialAux + NAUX_LIQUID
ELSE IF( IsWetParticle(icls) )THEN
  NumMaterialAux = NumMaterialAux + NAUX_LIQUID
END IF

IF( IsAerosol(icls) )NumMaterialAux = NumMaterialAux + NAUX_AEROSOL

RETURN
END

!=============================================================================

SUBROUTINE CheckSubgroups( nsg,mat )

USE struct_fd
USE param_fd
USE error_fi

IMPLICIT NONE

INTEGER,              INTENT( IN ) :: nsg
TYPE( material_str ), INTENT( IN ) :: mat

INTEGER icls, mxsgp_matl, ios

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

icls = mat%icls

IF( IsParticle(icls) )THEN
  mxsgp_matl = MAXSGP
ELSE IF( IsWetParticle(icls) )THEN
  mxsgp_matl = MAXSGP
ELSE IF( IsLiquid(icls) )THEN
  mxsgp_matl = MAXSGP - 1
ELSE IF( IsAerosol(icls) )THEN
  mxsgp_matl = 1
ELSE
  mxsgp_matl = 1
END IF

IF( nsg <= 0 .OR. nsg > mxsgp_matl )THEN
  nError   = IV_ERROR
  eMessage = 'Invalid number of size bins for '//TRIM(mat%cmat)
  WRITE(eAction,'(A,I5)',IOSTAT=ios)'Range is 1 -',mxsgp_matl
END IF

RETURN
END

!=============================================================================

SUBROUTINE set_rel_type( rmat,group,ityp,idist )

! ---- Set puff type and distribution parameter from
!      release material and subgroup
!      Don't change subgroup for particles/liquids for storeage of instantaneous releases
!      subgroup for gases is constant. Only subgroup of aerosol matters and change won't affect this routine.

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: rmat
INTEGER,      INTENT( INOUT ) :: group
INTEGER,      INTENT( OUT   ) :: ityp
INTEGER,      INTENT( OUT   ) :: idist

INTEGER i, nsg, grp

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

idist = 0
ityp  = 0
CALL cupper( rmat )

DO i = 1,ntypm
  IF( TRIM(rmat) == TRIM(material(i)%cmat) )ityp = i
END DO

IF( ityp == 0 )THEN
  DO i = 1,16
    ityp = ICHAR(rmat(i:i))
    IF( ityp == 0 )THEN
      rmat(i:i) = ' '
      CONTINUE
    END IF
  END DO
  nError   = IV_ERROR
  eMessage = 'Unknown release material in scenario'
  eRoutine = 'set_rel_material'
  GOTO 9999
ELSE
  IF( IsParticle(material(ityp)%icls) .OR. IsWetParticle(material(ityp)%icls) )THEN
    nsg = GetSubgroups( material(ityp),mat_aux )
    grp = group                !Don't change subgroup for particles
    IF( grp < 0 )grp = nsg + 1
    idist = grp - nsg
    IF( idist > 0 )THEN
      ityp = material(ityp)%ioffp + 1
    ELSE
      ityp = material(ityp)%ioffp + grp
    END IF
  ELSE IF( IsLiquid(material(ityp)%icls) )THEN
    IF( group == 0 )THEN
      grp   = 0
      idist = 0
      ityp  = material(ityp)%ioffp + 1
    ELSE
      nsg = GetSubgroups( material(ityp),mat_aux )
      grp = group                !Don't change subgroup for liquids
      IF( grp < 0 )grp = nsg + 1
      idist = grp - nsg
      IF( idist > 0 )THEN
        ityp = material(ityp)%ioffp + 2
      ELSE
        ityp = material(ityp)%ioffp + grp + 1
      END IF
    END IF
    grp = grp + 1
  ELSE IF( IsAerosol(material(ityp)%icls) )THEN
    idist = 0
    ityp  = material(ityp)%ioffp + 1
    IF( group == 0 )group = 2       !OK to change subgroup for aerosol
    grp = group                     !For check below
  ELSE
    idist = 0
    ityp  = material(ityp)%ioffp + group
    grp = group                     !For check below
  END IF
  IF( idist > 1 .OR. grp <= 0 )THEN
    nError   = IV_ERROR
    eMessage = 'Non-existent subgroup for material '//TRIM(rmat)
    eRoutine = 'set_rel_material'
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetTypeID()

USE scipuff_fi
USE class_fd
USE UtilMtlAux
USE files_fi

!     Sets the typeID structures for the materials
!       Dependent on several global variables having been set elsewhere
!         ntypp     = number of puff types (should be initialized to zero somewhere)
!         ntypd     = number of dep  types (should be initialized to zero somewhere)
!         ntyps     = number of dos  types (should be initialized to zero somewhere)
!         hazard    = hazard flag
!         multicomp = multicomponent flag

IMPLICIT NONE

INTEGER imtl, nsg, i0
INTEGER i, typeID_sz, alloc_stat
INTEGER ngp
INTEGER ios

LOGICAL ltot

CHARACTER(256) fname

INTEGER, EXTERNAL :: ClearClass,AddClass
INTEGER, EXTERNAL :: NumMaterialAux
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsGas

!==== Clear associated liquid definitions for wet particles

nWetPartLiq = 0
IF( ALLOCATED(WetPartLiquidMat) )DEALLOCATE( WetPartLiquidMat,STAT=alloc_stat )

!==== Loop over materials

DO imtl = 1,ntypm

!==== Reset some material flags as necessary

  material(imtl)%ioffp = ntypp

  nsg = MAX(GetSubgroups(material(imtl),mat_aux),1)
  IF( IsLiquid(material(imtl)%icls) )THEN
    nsg = nsg + 2         ! Extra groups for vapor and 2-phase
  ElSE IF( IsWetParticle(material(imtl)%icls) )THEN
    ngp = nsg             ! Save actual number of size bins
    nsg = nsg + nsg       ! Double number for wet bins
  END IF

  ltot = nsg > 1

!==== Set typeID values

  IF( ALLOCATED(typeID) )THEN
    typeID_sz = SIZE(typeID)
    IF( ntypp+nsg > typeID_sz )THEN
      CALL realloc_ntypp( typeID_sz,ntypp+nsg-typeID_sz )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  ELSE
    ALLOCATE( typeID(ntypp+nsg),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SetTypeID'
      eMessage = 'Error allocating typeID storage'
      GOTO 9999
    END IF
  END IF

  ntypp = ntypp + nsg

  i0 = material(imtl)%ioffp

! --- Set characteristics for all puffs

  DO i = 1,nsg
    typeID(i0+i)%imat  = imtl
    typeID(i0+i)%igrp  = i
    typeID(i0+i)%ltot  = ltot
    typeID(i0+i)%ipmc  = 0
    typeID(i0+i)%mcID  = 0
    typeID(i0+i)%icls  = material(imtl)%icls
    typeID(i0+i)%npaux = NumMaterialAux( material(imtl)%icls,i,ltot )
  END DO

! --- Reset first liquid puff as vapor type

  IF( IsLiquid(material(imtl)%icls) )THEN
    typeID(i0+1)%icls  = ClearClass( typeID(i0+1)%icls,MATID_LIQ )
    typeID(i0+1)%icls  = AddClass( typeID(i0+1)%icls,MATID_GAS )
    typeID(i0+1)%npaux = NumMaterialAux( typeID(i0+1)%icls,1,ltot )
  END IF

! --- Reset last liquid puff as 2-phase aerosol type

  IF( IsLiquid(material(imtl)%icls) )THEN
    typeID(i0+nsg)%icls  = ClearClass( typeID(i0+nsg)%icls,MATID_LIQ )
    typeID(i0+nsg)%icls  = AddClass( typeID(i0+nsg)%icls,MATID_AEROSOL )
    typeID(i0+nsg)%icls  = AddClass( typeID(i0+nsg)%icls,MATID_GAS )
    typeID(i0+nsg)%npaux = NumMaterialAux( typeID(i0+nsg)%icls,1,ltot )
  END IF

  IF( IsAerosol(material(imtl)%icls) )THEN
    typeID(i0+1)%icls  = AddClass( typeID(i0+1)%icls,MATID_GAS )
    typeID(i0+1)%npaux = NumMaterialAux( typeID(i0+1)%icls,1,ltot )
  END IF

! --- Reset first nbins for wet particles as 'dry' particles

  IF( IsWetParticle(material(imtl)%icls) )THEN
    DO i = 1,ngp
      typeID(i0+i)%icls  = ClearClass( typeID(i0+i)%icls,MATID_WETP )
      typeID(i0+i)%icls  = AddClass( typeID(i0+i)%icls,MATID_PRT )
      typeID(i0+i)%npaux = NumMaterialAux( typeID(i0+i)%icls,i,ltot )
! --- Reset "wet" groups to actual group
      typeID(i0+ngp+i)%igrp  = i
    END DO
  END IF

!---- Check for auxiliary input file

  IF( LEN_TRIM(material(imtl)%file) /= 0 .AND. &
          INDEX(material(imtl)%file,CHAR(0)) == 0 .AND. &
          TRIM(material(imtl)%file) /= 'intentionally bl' )THEN
    fname = TRIM(material(imtl)%file)
    CALL AddPath( fname,material(imtl)%path )

    OPEN( UNIT=lun_tmp,FILE=fname,STATUS='OLD',FORM='FORMATTED', &
          IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'SetTypeID'
      eMessage = 'Error opening auxiliary material file'
      CALL ReportFileName( eInform,'File=',fname )
      eAction  = 'Make sure file exists'
      GOTO 9999
    END IF

! --- Check wet particles for liquid component input

    IF( IsWetParticle(material(imtl)%icls) )THEN
      CALL CheckWetPartLiquid( imtl,fname )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

! --- Check any material for multicomponent input

    IF( IsGas(material(imtl)%icls) )THEN
      CALL InitMaterialMC( imtl,fname )
      IF( nError /= NO_ERROR )GOTO 9999
      multicomp = .TRUE.
      material(imtl)%icls = AddClass( material(imtl)%icls,MATID_MULTI )
    END IF

    CLOSE( UNIT=lun_tmp,IOSTAT=ios )

  END IF

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckWetPartLiquid( imat,fname )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: imat
CHARACTER(*), INTENT( IN ) :: fname

INTEGER i, alloc_stat, ios

CHARACTER(16) LiqName

TYPE( liquid_material ), DIMENSION(:), ALLOCATABLE :: TemLiquidMat

IF( nWetPartLiq > 0 )THEN
  ALLOCATE( TemLiquidMat(nWetPartLiq),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CheckWetPartLiquid'
    eMessage = 'Error allocating wet particle auxiliary liquid storage'
    GOTO 9999
  END IF
  DO i = 1,nWetPartLiq
    TemLiquidMat(i) = WetPartLiquidMat(i)
  END DO
  DEALLOCATE( WetPartLiquidMat,STAT=alloc_stat )
END IF

nWetPartLiq = nWetPartLiq + 1

ALLOCATE( WetPartLiquidMat(nWetPartLiq),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CheckWetPartLiquid'
  eMessage = 'Error allocating wet particle auxiliary liquid storage'
  GOTO 9999
END IF

IF( nWetPartLiq > 1 )THEN
  DO i = 1,nWetPartLiq-1
    WetPartLiquidMat(i) = TemLiquidMat(i)
  END DO
  DEALLOCATE( TemLiquidMat,STAT=alloc_stat )
END IF

!==== Fill data structures from namelist

CALL ReadWetPartLiquid( fname,lun_tmp,WetPartLiquidMat(nWetPartLiq),LiqName )
IF( nError /= NO_ERROR )GOTO 9999

material(imat)%AuxMatID = nWetPartLiq

WRITE(lun_log,'(A)',IOSTAT=ios) 'Using liquid material '//TRIM(LiqName)// &
                            ' as medium for wet particle material '// &
                            TRIM(material(imat)%cmat)
9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE ExitAuxMatl()

USE scipuff_fi

IMPLICIT NONE

INTEGER ios

!==== Clear associated liquid definitions for wet particles

nWetPartLiq = 0
IF( ALLOCATED(WetPartLiquidMat) )DEALLOCATE( WetPartLiquidMat,STAT=ios )

RETURN
END

!===============================================================================

SUBROUTINE realloc_ntypp( n,inc )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n, inc

TYPE( type_str ), DIMENSION(:), ALLOCATABLE :: tem_type

INTEGER alloc_stat

ALLOCATE( tem_type(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'realloc_ntypp'
  eMessage = 'Error allocating temporary storage for puff type'
  GOTO 9999
END IF
tem_type = typeID; DEALLOCATE( typeID,STAT=alloc_stat )

ALLOCATE( typeID(n+inc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'realloc_ntypp'
  eMessage = 'Error allocating storage for puff type'
  GOTO 9999
END IF
typeID(1:n) = tem_type

9999 CONTINUE

DEALLOCATE( tem_type,STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE InitMaterialMC( imat,fname )

USE scipuff_fi
USE files_fi
USE chem_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: imat
CHARACTER(*), INTENT( IN ) :: fname

INTEGER ios

!---- Initialize gas-phase chemistry - only MC-type so far

CALL InitChemMC( lun_tmp,fname,imat )
IF( nError /= NO_ERROR )GOTO 9999

IF( DoChemistry .AND. nthreads > 1 )THEN
  nError = IV_ERROR
  eRoutine = 'InitMaterialMC'
  eMessage = 'Invalid calculation mode'
  eInform  = 'Chemistry cannot be run with multiple threads'
  eAction  = 'Set NumThreads = 1 in INI file'
  GOTO 9999
END IF

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ios )

RETURN
END
