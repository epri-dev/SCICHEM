!***********************************************************************
!               GUI_SCIP_material
!***********************************************************************
SUBROUTINE GUI_SCIP_material( prjdlg,matdlg,tool,mtlList,mxMtl )

USE resource_fd
USE tooluser_fd
USE ErrorParam_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE GUImatl_fi

IMPLICIT NONE

TYPE( pmaterialT ) tool
TYPE( materialT  ) mtlList(*)
TYPE( matdef_str ) matdlg
TYPE( ProjectStructure ) prjdlg
INTEGER mxMtl

INTEGER i

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsNullSensor, IsSatSensor

LOGICAL, EXTERNAL :: hasError

!==== Project id member

tool%project = prjdlg%ID

!==== Material List header

tool%mtlHead%max    = mxMtl
tool%mtlHead%number = matdlg%nmatl

!==== Material List

DO i = 1,matdlg%nmatl

  mtlList(i)%name  = matdlg%material(i)%cmat
  mtlList(i)%units = matdlg%material(i)%unit
  mtlList(i)%file  = matdlg%material(i)%file
  mtlList(i)%path  = matdlg%material(i)%path

  mtlList(i)%type = SCIPnull

  IF( IsGas(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_GAS)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_GAS)
  END IF

  IF( IsLiquid(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_LIQUID)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_LIQUID)
  END IF

  IF( IsNullSensor(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_NULLSENSOR)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_NULLSENSOR)
  END IF

  IF( IsSatSensor(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_SATSENSOR)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_SATSENSOR)
  END IF

  IF( IsAerosol(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_AEROSOL)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_AEROSOL)
  END IF

  IF( IsParticle(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_PARTICLE)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_PARTICLE)
  END IF

  IF( IsWetParticle(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_WETPARTICLE)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_WETPARTICLE)
  END IF

  IF( IsMulti(matdlg%material(i)%icls) )THEN
    mtlList(i)%type = IBSET(mtlList(i)%type,HMB_MULTICOMP)
  ELSE
    mtlList(i)%type = IBCLR(mtlList(i)%type,HMB_MULTICOMP)
  END IF

  IF( IsAerosol(matdlg%material(i)%icls) )THEN !NB - Must check AEROSOL
    CALL GUI_SCIP_Aerosol( i,matdlg,mtlList(i)%matData ) !     before GAS since
    IF( hasError() )GOTO 9999 !     aerosol materials
  ELSE IF( IsGas(matdlg%material(i)%icls) )THEN !     have both the GAS
    CALL GUI_SCIP_Gas( i,matdlg,mtlList(i)%matData ) !     and AEROSOL bits
    IF( hasError() )GOTO 9999 !     set
  ELSE IF( IsParticle(matdlg%material(i)%icls) )THEN
    CALL GUI_SCIP_Particle( i,matdlg,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE IF( IsLiquid(matdlg%material(i)%icls) )THEN
    CALL GUI_SCIP_Liquid( i,matdlg,mtlList(i)%type,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE IF( IsWetParticle(matdlg%material(i)%icls) )THEN
    CALL GUI_SCIP_Particle( i,matdlg,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE
    WRITE(string1,'(A,I3)')'Class =',matdlg%material(i)%icls
    CALL SetError( IV_ERROR, &
                  'Invalid material class', &
                   string1,' ', &
                  'GUI_SCIP_material' )
    GOTO 9999
  END IF

  mtlList(i)%puffIndex = matdlg%material(i)%ioffp

END DO

9999 CONTINUE

RETURN
END
!***********************************************************************
!               SCIP_GUI_material
!***********************************************************************
SUBROUTINE SCIP_GUI_material( matdlg,tool,mtlList )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE GUImatl_fi
USE errorParam_fd
USE class_fd

IMPLICIT NONE

TYPE( pmaterialT ) tool
TYPE( materialT  ) mtlList(*)
TYPE( matdef_str ) matdlg

INTEGER i
INTEGER icls

CHARACTER(16) class

INTEGER, EXTERNAL :: SetClass,AddClass
INTEGER, EXTERNAL :: SetClassEvap
LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: ClearClass

LOGICAL, EXTERNAL :: hasError

!==== Material structure header

matdlg%nmatl = 0
matdlg%nmaux = 0

!==== Loop over materials

IF( tool%mtlHead%number > tool%mtlHead%max )THEN
  WRITE(string1,'(A,I3,A,I3)')'No. materials = ',tool%mtlHead%number, &
                        ' : Max. allowed = ',tool%mtlHead%max
  CALL SetError( SZ_ERROR, &
                'Too many material types', &
                 string1,' ', &
                'SCIP_GUI_material' )
  GOTO 9999
END IF

DO i = 1,tool%mtlHead%number

  matdlg%nmatl = matdlg%nmatl + 1

  matdlg%material(i)%cmat = mtlList(i)%name
  matdlg%material(i)%unit = mtlList(i)%units
  matdlg%material(i)%file = mtlList(i)%file
  matdlg%material(i)%path = mtlList(i)%path

  IF (.FALSE. )THEN
  ELSE IF( BTEST(mtlList(i)%type,HMB_AEROSOL) )THEN
    class = MAT_AER
  ELSE IF( BTEST(mtlList(i)%type,HMB_NULLSENSOR) )THEN
    class = MAT_NULL
  ELSE IF( BTEST(mtlList(i)%type,HMB_SATSENSOR) )THEN
    class = MAT_SSAT
  ELSE IF( BTEST(mtlList(i)%type,HMB_LIQUID) )THEN
    class = MAT_LIQ
  ELSE IF( BTEST(mtlList(i)%type,HMB_WETPARTICLE) )THEN
    class = MAT_WET
  ELSE IF( BTEST(mtlList(i)%type,HMB_PARTICLE) )THEN
    class = MAT_PRT
  ELSE IF( BTEST(mtlList(i)%type,HMB_GAS) )THEN
    class = MAT_GAS
  ELSE
    WRITE(string1,'(A,I3)')'Class =',mtlList(i)%type
    CALL SetError( IV_ERROR, &
                  'Unable to determine material class', &
                   string1,' ', &
                  'SCIP_GUI_material' )
    GOTO 9999
  END IF
  matdlg%material(i)%ccls = class

  icls = SetClass( class )
  IF( BTEST(mtlList(i)%type,HMB_2NDEVAP) )icls = SetClassEvap(icls)
  IF( BTEST(mtlList(i)%type,HMB_MULTICOMP) )THEN
    IF( IsGas(icls) )THEN
      IF( LEN_TRIM(mtlList(i)%file) > 0 )icls = AddClass(icls,MATID_MULTI)
    END IF
  END IF

  matdlg%material(i)%icls = icls
  matdlg%material(i)%iaux = matdlg%nmaux + 1
  IF( IsMulti(icls) )THEN
    CALL SetComponentNamesMC( matdlg%material(i),matdlg%materialMC(i) )
    IF( hasError() ) THEN
      CALL AddErrorAction('Cannot load multicomponent names, continue anyway (Yes)?')
      CALL ShowWarningMessage( 0,.TRUE. )
      IF( .NOT.hasError() )THEN
        matdlg%material(i)%nmc = 0
        matdlg%material(i)%icls = ClearClass(matdlg%material(i)%icls,MATID_MULTI)
      ELSE
        CALL AddErrorAction('')
        GOTO 9999
      END IF
    END IF
  ELSE
    matdlg%material(i)%nmc = 0
  END IF
  matdlg%material(i)%ioffp = mtlList(i)%puffIndex
  matdlg%material(i)%ioffs = NOT_SET_I
  matdlg%material(i)%ioffd = NOT_SET_I

  IF( IsAerosol(icls) )THEN !NB - Must check AEROSOL
    CALL SCIP_GUI_Aerosol( i,matdlg,mtlList(i)%matData ) !     before GAS since
    IF( hasError() )GOTO 9999 !     aerosol materials
  ELSE IF( IsGas(icls) )THEN !     have both the GAS
    CALL SCIP_GUI_Gas( i,matdlg,mtlList(i)%matData ) !     and AEROSOL bits
    IF( hasError() )GOTO 9999 !     set (though not here)
  ELSE IF( IsParticle(icls) )THEN
    CALL SCIP_GUI_Particle( i,matdlg,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE IF( IsLiquid(icls) )THEN
    CALL SCIP_GUI_Liquid( i,matdlg,mtlList(i)%type,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE IF( IsWetParticle(icls) )THEN
    CALL SCIP_GUI_Particle( i,matdlg,mtlList(i)%matData )
    IF( hasError() )GOTO 9999
  ELSE
    WRITE(string1,'(A,I3)')'Class =',icls
    CALL SetError( IV_ERROR, &
                  'Invalid material class', &
                   string1,' ', &
                  'SCIP_GUI_material' )
    GOTO 9999
  END IF

END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SetComponentNamesMC
!*******************************************************************************
SUBROUTINE SetComponentNamesMC( matl,matlMC )

USE pcscipuf_fi
USE files_fi
USE GUImatl_fi
USE errorParam_fd
USE multcomp_fd
USE chem_fi
USE error_fi

IMPLICIT NONE

TYPE( material_str ) matl
TYPE( matMC_str    ) matlMC

INTEGER nmc
INTEGER ios, alloc_stat, i

CHARACTER(256) fname
CHARACTER(200) line

CHARACTER(1), PARAMETER :: SET_MODE      = '#'
CHARACTER(1), PARAMETER :: SPECIES_MODE  = 'S'

TYPE( ChemMC_str ), TARGET :: chemX

!This uses error_fi as local space for calling SCIPUFF routines
!These values must be initialzed before making the calls
nError = NO_ERROR
eMessage = 'There is no Error'
eInform  = ' '
eAction  = ' '
eRoutine = ' '

fname = TRIM(matl%file)
CALL AddPath( fname,matl%path )

OPEN( UNIT=lun_tmp,FILE=fname,STATUS='OLD',FORM='FORMATTED',IOSTAT=ios )
IF( ios /= 0 )THEN
  WRITE(string1,'(A,A)')'File='//TRIM(fname)
  CALL SetError( OP_ERROR, &
                'Error opening multicomponent file', &
                 string1,'Make sure file exists', &
                'SetComponentNamesMC' )
  GOTO 9999
END IF

chem => chemX

CALL ClearChemMC()

!====== Control Mode read - Namelist is always first

CALL ReadChemControl( lun_tmp,0 )
IF( nError /= NO_ERROR )GOTO 9998

!====== Species Definitions

DO

  READ(lun_tmp,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    WRITE(string1,'(A,A)')'File='//TRIM(fname)
    CALL SetError( RD_ERROR, &
                  'Error reading Multi-component input file', &
                   string1,' ', &
                  'SetComponentNamesMC' )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == SPECIES_MODE )THEN
      CALL ReadChemSpecies( lun_tmp,fname )
      IF( nError /= NO_ERROR )GOTO 9998
      EXIT
    END IF
  END IF

END DO

CALL SetChemPointers()
IF( nError /= NO_ERROR ) THEN
  CALL SetError( nError, &
                  TRIM(eMessage), &
                  TRIM(eInform),TRIM(eAction), &
                  TRIM(eRoutine) )
  GOTO 9999
END IF

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

IF( ASSOCIATED(matlMC%name) )DEALLOCATE( matlMC%name,STAT=alloc_stat )
ALLOCATE( matlMC%name(nmc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(string1,'(A,I10)')'Requst size = ',nmc
  CALL SetError( IV_ERROR, &
                'Error allocating multicomponent name list', &
                 string1,' ', &
                'SetComponentNamesMC' )
  GOTO 9999
END IF

DO i = 1,nmc
  matlMC%name(i) = TRIM(chem%Species(i)%name)
END DO

SELECT CASE( chem%cUnits )
  CASE( UNIT_MOLECULE )
    matlMC%units = 'Molecules/cm3'
  CASE( UNIT_PPM )
    matlMC%units = 'ppm'
  CASE DEFAULT
    matlMC%units = ' '
END SELECT

matl%nmc = nmc

9999 CONTINUE

CLOSE(UNIT=lun_tmp)

RETURN

9998 CONTINUE
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9999

END
!*******************************************************************************
!        GUI_SCIP_Gas
!*******************************************************************************
SUBROUTINE GUI_SCIP_Gas( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE files_fi
USE GUImatl_fi
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER imtl

TYPE( matdef_str ) matdlg
TYPE( matGasT ) mtlData
TYPE( gas_material ) pmatgas

!==== Set pointer

!==== Load

!==== Properties

mtlData%decayAmp         = matdlg%material(imtl)%prop(1)
mtlData%decayMin         = matdlg%material(imtl)%prop(2)
mtlData%minConcentration = matdlg%material(imtl)%prop(3)

!==== Surface flags

mtlData%save = SCIPnull
IF( matdlg%material(imtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF
IF( matdlg%material(imtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

CALL GetGasParam( pmatgas,matdlg%material(imtl)%iaux,matdlg%mat_aux )
mtlData%gasDensity    = pmatgas%rho
mtlData%gasDeposition = pmatgas%vd

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Liquid
!*******************************************************************************
SUBROUTINE GUI_SCIP_Liquid( imtl,matdlg,mtlType,mtlData )

USE resource_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE GUImatl_fi
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER imtl
TYPE( matdef_str ) matdlg
INTEGER mtlType
TYPE( matLiquidT ) mtlData

TYPE( gas_material ) pmatgas
TYPE( liquid_material ) pmatliq

INTEGER i

CHARACTER(128) eInform

!==== Set pointer

!==== Load

!==== Properties

mtlData%decayAmp         = matdlg%material(imtl)%prop(1)
mtlData%decayMin         = matdlg%material(imtl)%prop(2)
mtlData%minConcentration = matdlg%material(imtl)%prop(3)

!==== Surface flags

mtlData%save = SCIPnull
IF( matdlg%material(imtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF
IF( matdlg%material(imtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF
IF( matdlg%material(imtl)%lsrft )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
END IF
IF( matdlg%material(imtl)%ldost )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)
END IF

!==== Auxiliary data

CALL GetGasParam( pmatgas,matdlg%material(imtl)%iaux,matdlg%mat_aux )
mtlData%gasDensity    = pmatgas%rho
mtlData%gasDeposition = pmatgas%vd

CALL GetLiquidParam( pmatliq,matdlg%material(imtl)%iaux,2,matdlg%mat_aux )

IF( pmatliq%sf <= 0 )THEN
  mtlType = IBCLR(mtlType,HMB_2NDEVAP)
ELSE
  mtlType = IBSET(mtlType,HMB_2NDEVAP)
END IF

mtlData%nSizeBins        = pmatliq%nsg
mtlData%liquidDensity(1) = pmatliq%rho
mtlData%liquidDensity(2) = pmatliq%rhob
mtlData%antoine(1)       = pmatliq%a
mtlData%antoine(2)       = pmatliq%b
mtlData%antoine(3)       = pmatliq%c
mtlData%molWeight        = pmatliq%w
mtlData%srfTension       = pmatliq%st
mtlData%spreadFactor     = ABS(pmatliq%sf)
mtlData%viscosity        = pmatliq%viscosity
mtlData%liqSpecificHeat  = pmatliq%cpL
mtlData%gasSpecificHeat  = pmatliq%cpV

IF( mtlData%nSizeBins <= HS_MAXMTLBINSIZE )THEN
  mtlData%binBounds(1) = pmatliq%dmin
  mtlData%binSize(1)   = pmatliq%dbar
  DO i = 2,mtlData%nSizeBins
    CALL GetLiquidParam( pmatliq,matdlg%material(imtl)%iaux,i+1,matdlg%mat_aux )
    mtlData%binBounds(i)   = pmatliq%dmin
    mtlData%binSize(i)     = pmatliq%dbar
  END DO
  mtlData%binBounds(mtlData%nSizeBins+1) = pmatliq%dmax
ELSE
  WRITE(eInform,'(A,I4,A,I4)')'No. size bins =',mtlData%nSizeBins, &
                     ' : Max. allowed =',HS_MAXMTLBINSIZE
  CALL SetError( SZ_ERROR, &
                'Too many droplet size bins', &
                 eInform,' ', &
                'GUI_SCIP_Liquid' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Particle
!*******************************************************************************
SUBROUTINE GUI_SCIP_Particle( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE GUImatl_fi
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER imtl

TYPE( matdef_str ) matdlg
TYPE( matParticleT ) mtlData
TYPE( part_material ) pmatpart

INTEGER i

CHARACTER(128) eInform

!==== Set pointer

!==== Load

!==== Properties

mtlData%decayAmp         = matdlg%material(imtl)%prop(1)
mtlData%decayMin         = matdlg%material(imtl)%prop(2)
mtlData%minConcentration = matdlg%material(imtl)%prop(3)

!==== Surface flags

mtlData%save = SCIPnull
IF( matdlg%material(imtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF
IF( matdlg%material(imtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF
IF( matdlg%material(imtl)%lsrft )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
END IF
IF( matdlg%material(imtl)%ldost )THEN
 mtlData%save = IBSET(mtlData%save,HSB_TOTALDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)
END IF

!==== Auxiliary data

CALL GetParticleParam( pmatpart,matdlg%material(imtl)%iaux,1,matdlg%mat_aux )

mtlData%nSizeBins = pmatpart%nsg
mtlData%density   = pmatpart%rho

IF( mtlData%nSizeBins <= HS_MAXMTLBINSIZE )THEN
  mtlData%binBounds(1) = pmatpart%dmin
  mtlData%binSize(1)   = pmatpart%dbar
  DO i = 2,mtlData%nSizeBins
    CALL GetParticleParam( pmatpart,matdlg%material(imtl)%iaux,i,matdlg%mat_aux )
    mtlData%binBounds(i)   = pmatpart%dmin
    mtlData%binSize(i)     = pmatpart%dbar
  END DO
  mtlData%binBounds(mtlData%nSizeBins+1) = pmatpart%dmax
ELSE
  WRITE(eInform,'(A,I4,A,I4)')'No. size bins =',mtlData%nSizeBins, &
                     ' : Max. allowed =',HS_MAXMTLBINSIZE
  CALL SetError( SZ_ERROR, &
                'Too many particle size bins', &
                 eInform,' ', &
                'GUI_SCIP_Particle' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Aerosol
!*******************************************************************************
SUBROUTINE GUI_SCIP_Aerosol( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE files_fi
USE GUImatl_fi
USE UtilMtlAux

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

INTEGER imtl

TYPE( matdef_str ) matdlg
TYPE( matAerosolT ) mtlData
TYPE( aerosol_material ) pmataer

!==== Set pointer

!==== Load

!==== Properties

mtlData%decayAmp         = matdlg%material(imtl)%prop(1)
mtlData%decayMin         = matdlg%material(imtl)%prop(2)
mtlData%minConcentration = matdlg%material(imtl)%prop(3)

!==== Surface flags

mtlData%save = SCIPnull
IF( matdlg%material(imtl)%lsrfg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDEP)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDEP)
END IF
IF( matdlg%material(imtl)%ldosg )THEN
 mtlData%save = IBSET(mtlData%save,HSB_GROUPDOS)
ELSE
 mtlData%save = IBCLR(mtlData%save,HSB_GROUPDOS)
END IF
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDEP)
mtlData%save = IBCLR(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

CALL GetAerosolParam( pmataer,matdlg%material(imtl)%iaux,matdlg%mat_aux )

mtlData%gasDeposition   = pmataer%vd
mtlData%liquidDensity   = pmataer%rho
mtlData%antoine(1)      = pmataer%a
mtlData%antoine(2)      = pmataer%b
mtlData%antoine(3)      = pmataer%c
mtlData%molWeight       = pmataer%w
mtlData%liqSpecificHeat = pmataer%cpL
mtlData%gasSpecificHeat = pmataer%cpV

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Gas
!*******************************************************************************
SUBROUTINE SCIP_GUI_Gas( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE GUImatl_fi
USE files_fi
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER imtl

TYPE( matdef_str ) matdlg
TYPE( matGasT ) mtlData
TYPE( gas_material ) pmatgas

LOGICAL, EXTERNAL :: hasError

!==== Set pointer

!==== Unload

!==== Properties

matdlg%material(imtl)%prop(1) = mtlData%decayAmp
matdlg%material(imtl)%prop(2) = mtlData%decayMin
matdlg%material(imtl)%prop(3) = mtlData%minConcentration

!==== Surface flags

matdlg%material(imtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
matdlg%material(imtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
matdlg%material(imtl)%lsrft = .FALSE.
matdlg%material(imtl)%ldost = .FALSE.

!==== Auxiliary data

IF( matdlg%nmaux+MAXGMAUX > MAXMAUX )THEN
  CALL reallocate_mataux( matdlg%nmaux+MAXGMAUX )
  IF( hasError() )GOTO 9999
END IF
pmatgas%rho = mtlData%gasDensity
pmatgas%vd  = mtlData%gasDeposition
CALL PutGasParam( pmatgas,matdlg%material(imtl)%iaux,matdlg%mat_aux )
matdlg%nmaux = matdlg%nmaux + MAXGMAUX

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Liquid
!*******************************************************************************
SUBROUTINE SCIP_GUI_Liquid( imtl,matdlg,mtlType,mtlData )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE errorParam_fd
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER imtl
TYPE( matdef_str ) matdlg
INTEGER mtlType
TYPE( matLiquidT ) mtlData

TYPE( liquid_material ) pmatliq
TYPE( gas_material    ) pmatgas

INTEGER iaux
INTEGER nsg
INTEGER i

CHARACTER(128) eInform

LOGICAL, EXTERNAL :: hasError

!==== Set pointer

!==== Unload

!==== Properties

matdlg%material(imtl)%prop(1) = mtlData%decayAmp
matdlg%material(imtl)%prop(2) = mtlData%decayMin
matdlg%material(imtl)%prop(3) = mtlData%minConcentration

!==== Surface flags

matdlg%material(imtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
matdlg%material(imtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
matdlg%material(imtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
matdlg%material(imtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

nsg  = mtlData%nSizeBins
iaux = MAXLMAUXX+MAXLMAUX*nsg+MAXLMAUXP+MAXGMAUX

IF( matdlg%nmaux+iaux > MAXMAUX )THEN
  CALL reallocate_mataux( matdlg%nmaux+iaux )
  IF( hasError() )GOTO 9999
END IF

pmatgas%rho = mtlData%gasDensity
pmatgas%vd  = mtlData%gasDeposition
CALL PutGasParam( pmatgas,matdlg%material(imtl)%iaux,matdlg%mat_aux )

pmatliq%nsg   = nsg
pmatliq%rho   = mtlData%liquidDensity(1)
pmatliq%diff  = 0.0
DO i = 1,MAXLMAUXP-10
  pmatliq%dum(i) = NOT_SET_R
END DO
pmatliq%rhob      = mtlData%liquidDensity(2)
pmatliq%a         = mtlData%antoine(1)
pmatliq%b         = mtlData%antoine(2)
pmatliq%c         = mtlData%antoine(3)
pmatliq%w         = mtlData%molWeight
pmatliq%cpL       = mtlData%liqSpecificHeat
pmatliq%cpV       = mtlData%gasSpecificHeat
pmatliq%st        = mtlData%srfTension
pmatliq%sf        = mtlData%spreadFactor
pmatliq%viscosity = mtlData%viscosity

IF( BTEST(mtlType,HMB_2NDEVAP) )THEN
  pmatliq%sf = ABS(pmatliq%sf)
ELSE
  pmatliq%sf = -ABS(pmatliq%sf)
END IF

DO i = 1,nsg
  pmatliq%dmin = mtlData%binBounds(i)
  pmatliq%dbar = mtlData%binSize(i)
  pmatliq%dmax = mtlData%binBounds(i+1)
  IF( pmatliq%dbar <= 0.0 )THEN
    pmatliq%dbar = 0.5*(pmatliq%dmin + pmatliq%dmax)
  END IF
  CALL PutLiquidParam( pmatliq,matdlg%material(imtl)%iaux,i+1,matdlg%mat_aux )
END DO

matdlg%nmaux = matdlg%nmaux + iaux

IF( nsg > MAX_GROUP )THEN
  WRITE(eInform,*)'Allowed =',MAX_GROUP,'   :  material =',nsg
  CALL SetError( IV_ERROR, &
                'Too many subgroups in material', &
                 eInform,' ', &
                'SCIP_GUI_Liquid' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Particle
!*******************************************************************************
SUBROUTINE SCIP_GUI_Particle( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE errorParam_fd
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER imtl
TYPE( matdef_str ) matdlg
TYPE( matParticleT ) mtlData
TYPE( part_material ) pmatpart

INTEGER iaux
INTEGER nsg
INTEGER i

CHARACTER(128) eInform

LOGICAL, EXTERNAL :: hasError

!==== Set pointer

!==== Unload

!==== Properties

matdlg%material(imtl)%prop(1) = mtlData%decayAmp
matdlg%material(imtl)%prop(2) = mtlData%decayMin
matdlg%material(imtl)%prop(3) = mtlData%minConcentration

!==== Surface flags

matdlg%material(imtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
matdlg%material(imtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
matdlg%material(imtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
matdlg%material(imtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

nsg  = mtlData%nSizeBins
iaux = MAXPMAUXX+MAXPMAUX*nsg

IF( matdlg%nmaux+iaux > MAXMAUX )THEN
  CALL reallocate_mataux( matdlg%nmaux+iaux )
  IF( hasError() )GOTO 9999
END IF

pmatpart%nsg   = nsg
pmatpart%rho   = mtlData%density
pmatpart%vd    = 0.0
pmatpart%sigvd = 0.0
pmatpart%diff  = 0.0

DO i = 1,nsg
  pmatpart%dmin = mtlData%binBounds(i)
  pmatpart%dbar = mtlData%binSize(i)
  pmatpart%dmax = mtlData%binBounds(i+1)
  IF( pmatpart%dbar <= 0.0 )THEN
    pmatpart%dbar = 0.5*(pmatpart%dmin + pmatpart%dmax)
  END IF
  CALL PutParticleParam( pmatpart,matdlg%material(imtl)%iaux,i,matdlg%mat_aux )
END DO

matdlg%nmaux = matdlg%nmaux + iaux

IF( nsg > MAX_GROUP )THEN
  WRITE(eInform,*)'Allowed =',MAX_GROUP,'   :  material =',nsg
  CALL SetError( IV_ERROR, &
                'Too many subgroups in material', &
                 eInform,' ', &
                'SCIP_GUI_Particle' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Aerosol
!*******************************************************************************
SUBROUTINE SCIP_GUI_Aerosol( imtl,matdlg,mtlData )

USE resource_fd
USE tooluser_fd
USE GUImatl_fi
USE files_fi
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER imtl
TYPE( matdef_str ) matdlg
TYPE( matAerosolT ) mtlData
TYPE( aerosol_material ) pmataer

INTEGER i

LOGICAL, EXTERNAL :: hasError

!==== Set pointer

!==== Unload

!==== Properties

matdlg%material(imtl)%prop(1) = mtlData%decayAmp
matdlg%material(imtl)%prop(2) = mtlData%decayMin
matdlg%material(imtl)%prop(3) = mtlData%minConcentration

!==== Surface flags

matdlg%material(imtl)%lsrfg = BTEST(mtlData%save,HSB_GROUPDEP)
matdlg%material(imtl)%ldosg = BTEST(mtlData%save,HSB_GROUPDOS)
matdlg%material(imtl)%lsrft = BTEST(mtlData%save,HSB_TOTALDEP)
matdlg%material(imtl)%ldost = BTEST(mtlData%save,HSB_TOTALDOS)

!==== Auxiliary data

IF( matdlg%nmaux+MAXAMAUXP > MAXMAUX )THEN
  CALL reallocate_mataux( matdlg%nmaux+MAXAMAUXP )
  IF( hasError() )GOTO 9999
END IF

DO i = 1,MAXAMAUXP-8
  pmataer%dum(i) = NOT_SET_R
END DO
pmataer%vd  = mtlData%gasDeposition
pmataer%rho = mtlData%liquidDensity
pmataer%a   = mtlData%antoine(1)
pmataer%b   = mtlData%antoine(2)
pmataer%c   = mtlData%antoine(3)
pmataer%w   = mtlData%molWeight
pmataer%cpL = mtlData%liqSpecificHeat
pmataer%cpV = mtlData%gasSpecificHeat

CALL PutAerosolParam( pmataer,matdlg%material(imtl)%iaux,matdlg%mat_aux )

matdlg%nmaux = matdlg%nmaux + MAXAMAUXP

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        AllocateMtlList
!*******************************************************************************
SUBROUTINE AllocateMtlList(nMtl)

USE errorParam_fd
USE guitool_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER nMtl
INTEGER ios

CHARACTER(48) aString

IF( ALLOCATED(mtlList) )THEN
  IF( SIZE(mtlList) < nMtl )THEN
    DEALLOCATE(mtlList,STAT=ios)
    aString = 'Reallocate materials'
    ALLOCATE(mtlList(nMtl),STAT=ios)
    IF( ios /= 0 )GOTO 9999
  END IF
ELSE
  aString = 'Allocate materials'
  ALLOCATE(mtlList(MAX(1,nMtl)),STAT=ios)
  IF( ios /= 0 )GOTO 9999
END IF

1000 CONTINUE

RETURN

9999 CONTINUE
WRITE(string1,*)'new size =',nMtl
WRITE(string2,*)'Allocation call sequence ='//TRIM(aString)
CALL SetError( SZ_ERROR,'Allocation Error',string2,string1,'ReallocateScenario' )
GOTO 1000
END
!*******************************************************************************
!        DeallocateMtlList
!*******************************************************************************
SUBROUTINE DeallocateMtlList()

USE guitool_fi

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(mtlList) )DEALLOCATE(mtlList,STAT=ios)

RETURN
END
