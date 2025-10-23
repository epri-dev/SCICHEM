SUBROUTINE InitChemMC( lun,file,imat )

USE scipuff_fi
USE multcomp_fd
USE chem_fi
USE chemReactions_fd
USE files_fi
USE reallocate
USE class_fd
USE mpi_fi, ONLY: isSerial, AqAerInp

!====   Reads multi-component definitions from project.imc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: imat

CHARACTER(1), PARAMETER :: SET_MODE      = '#'
CHARACTER(1), PARAMETER :: SPECIES_MODE  = 'S'
CHARACTER(1), PARAMETER :: EQUATION_MODE = 'E'
CHARACTER(1), PARAMETER :: TABLE_MODE    = 'T'
CHARACTER(1), PARAMETER :: CONTROL_MODE  = 'C'
CHARACTER(1), PARAMETER :: BALANCE_MODE  = 'B'
CHARACTER(1), PARAMETER :: GROUP_MODE    = 'G'

INTEGER ID, alloc_stat
INTEGER ios, i, j, n, nsg, irv

CHARACTER(200) line
CHARACTER(PATH_MAXLENGTH) prjName
TYPE( ChemMC_str ), DIMENSION(:), ALLOCATABLE :: tmp

INTEGER, EXTERNAL :: num_puff_types
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripExtension

!===== Initialize multi-component structures

mat_mc%nMCtype = mat_mc%nMCtype + 1
IF( mat_mc%nMCtype == 1 )THEN
  ALLOCATE( mat_mc%type(1),mat_mc%ID(1),STAT=alloc_stat )
ELSE
  alloc_stat = reallocate_integer1d( mat_mc%type,1 )
  IF( alloc_stat == 0 )THEN
    alloc_stat = reallocate_integer1d( mat_mc%ID,1 )
  END IF
END IF
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating material multicomponent list'
  eRoutine = 'InitChemMC'
  GOTO 9999
END IF
mat_mc%type(mat_mc%nMCtype) = MC_CHEM
ID = 1
DO n = 1,mat_mc%nMCtype - 1
  IF( mat_mc%type(n) == MC_CHEM )ID = ID + 1
END DO
mat_mc%ID(mat_mc%nMCtype) = ID

!---- allocate/reallocate chem structure as required

IF( ID > 1 )THEN
  ALLOCATE( tmp(ID),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  DO i = 1,ID-1
    tmp(i) = ChemMC(i,1)
  END DO
  DEALLOCATE( ChemMC,STAT=alloc_stat )
  ALLOCATE( ChemMC(ID,nthreads),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  DO i = 1,ID-1
    ChemMC(i,1) = tmp(i)
  END DO
  DEALLOCATE( tmp,STAT=alloc_stat )
ELSE
  IF( ALLOCATED(chemMC) )DEALLOCATE( chemMC,STAT=alloc_stat )
  ALLOCATE( ChemMC(1,nthreads),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
END IF

!---- Initialize material multi-component

material(imat)%mcID = mat_mc%nMCtype
material(imat)%icls = IBSET(material(imat)%icls,MATID_MULTI)

chem => chemMC(ID,1)

CALL ClearChemMC()

ngaseous = 0
nparticle = 0

MonthDay = 0

!====== Control Mode read - Namelist is always first

CALL ReadChemControl( lun,ID )
IF( nError /= NO_ERROR )GOTO 9999

!====== Set up for diagnostic messages
CALL xsetf( 0 )
CALL xsetun( lun_log )

ngd_chem  = 0
nbad_chem = 0
tot_bad   = 0

!====== Species Definitions

DO

  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemMC'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == SPECIES_MODE )THEN
      CALL ReadChemSpecies( lun,file )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

REWIND( UNIT=lun,IOSTAT=ios )

!====== Reaction Equation Definitions

DO

  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemMC'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == EQUATION_MODE )THEN
      CALL ReadChemReactions( lun,file )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

REWIND( UNIT=lun,IOSTAT=ios )

!====== Group Output Mode

chem%nOutGroup = 0

DO

  READ(lun,'(A)',IOSTAT=ios) line

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'InitChemMC'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == GROUP_MODE )THEN
      CALL ReadChemGroup( lun,file )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

IF( chem%nOutGroup == 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'InitChemMC'
  WRITE(eMessage,"('Error reading output group information from input file ',A)")TRIM(file)
  eInform  = 'At least one output group name must be specified'
  GOTO 9999
END IF

REWIND( UNIT=lun,IOSTAT=ios )

!====== Table Mode

DO

  READ(lun,'(A)',IOSTAT=ios) line

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'InitChemMC'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == TABLE_MODE )THEN
      CALL ReadChemTable( lun,file )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

!====== AqAqer Mode
prjName          = StripExtension( file_prj )
AqAerInp%nError  = nError
AqAerInp%prjName = prjName
AqAerInp%mcFile  = TRIM(file)
CALL ReadAqAerEx( AqAerInp )
IF( AqAerInp%nError /= NO_ERROR )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadAqAer'
  eMessage = 'Error from AqAer module. See AqAer log for details'
  GO TO 9999
END IF

!====== Balance Mode

REWIND( UNIT=lun,IOSTAT=ios )

chem%lBalance = .FALSE.

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'InitChemMC'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == BALANCE_MODE )THEN
      CALL ReadChemBalance( lun,file )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

DO j = 1,chem%nSpecies
  IF( chem%species(j)%nit /= 0. )THEN
    chem%lBalance = .TRUE.
    EXIT
  END IF
END DO
IF( chem%lBalance )THEN
  WRITE(lun_log,*,IOSTAT=ios)
  WRITE(lun_log, *,IOSTAT=ios)'Conserving the following set of species:'
  DO j = 1,chem%nSpecies
    IF( chem%species(j)%nit > 0. )&
      WRITE(lun_log,'(A16,"(",F4.1")")',IOSTAT=ios) chem%species(j)%name, chem%species(j)%nit
  END DO
END IF

!====   Check reaction list

chem%thermal = .FALSE.

DO i = 1,chem%nReactions

!====   Check zenith table for completeness

  IF( chem%reaction(i)%type == ID_K_RAD )THEN
    IF( SIZE(chem%reaction(i)%data) /= chem%nZenith )THEN
      nError   = IV_ERROR
      eRoutine = 'InitChemMC'
      eMessage = 'No index in radiation K table'
      WRITE(eAction,*)'Reaction ID =',i
      CALL ReportFileName( eInform,'File=',file )
      GOTO 9999
    END IF
  END IF

!====   Store thermal reaction data

  IF( BTEST(chem%reaction(i)%class,ID_REACT_THERMAL) )THEN
    chem%thermal = .TRUE.
    IF( ASSOCIATED(chem%reaction(i)%data) )THEN
      irv = reallocate_dble1d(chem%reaction(i)%data,1)
    ELSE
      ALLOCATE( chem%reaction(i)%data(1),STAT=irv )
    END IF
    IF( irv /= 0  )THEN
      nError   = RD_ERROR
      eRoutine = 'InitChemMC'
      eMessage = 'Error reallocating reaction data for thermal'
      GOTO 9999
    END IF
    n = SIZE(chem%reaction(i)%data)
    chem%reaction(i)%data(n) = chem%reaction(i)%H
  END IF

END DO

CALL SetChemPointers()
IF( nError /= NO_ERROR )GOTO 9999

!====   Set Pointers

CALL SetStarSpecies( ID )
IF( nError /= NO_ERROR )GOTO 9999

!====   Initialize equilibrium calculation

CALL InitChemEquilibrium( ID )
IF( nError /= NO_ERROR )GOTO 9999

CALL SetChemAqaer( )
IF( nError /= NO_ERROR )GOTO 9999

!---- Set base project name
prjName = StripExtension( file_prj )
chem_aqaer%prjName = prjName

CALL InitAerAqEx( ID,chem_aqaer,ID_SPECIES_PARTICLE,nError )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'InitAerAq'
  eMessage = 'Error in InitAerAq. Check project qlog for details'
  GOTO 9999
END IF

CALL GetChemAqaer( )

!====   Initialize diagnostic variables
CALL InitChemDiagnostics( ID )

!====   Initialize chemMC structure for MPI

IF( nthreads > 1 )THEN
  DO i = 2,nthreads
    CALL CopyChemMC( ID,i )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO
END IF

!====   Write mass and concentration names and order to log file

WRITE(lun_log,*,IOSTAT=ios)
WRITE(lun_log, *,IOSTAT=ios)'Order of species masses in puff file:'
DO j = 1,chem%nSpecies
  WRITE(lun_log,'(''MC'',I3.3,8X,A16)',IOSTAT=ios) j,chem%species(j)%name
END DO
WRITE(lun_log, *,IOSTAT=ios)'Order of species concentrations in puff file:'
DO j = 1,chem%nStar
  WRITE(lun_log,'(''CM'',I3.3,8X,A16)',IOSTAT=ios) j,chem%star(j)%s%name
END DO
WRITE(lun_log,*,IOSTAT=ios)

!==== Initialize material and puff typeID variables

n = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium + chem%nStar + 4

material(imat)%nmc = n

DO i = 1,chem%nSpecies
  IF( chem%species(i)%ldep ) &
      material(imat)%icls = IBSET(material(imat)%icls,MATID_MULTI_DEP)
  IF( chem%species(i)%ldos ) &
      material(imat)%icls = IBSET(material(imat)%icls,MATID_MULTI_DOS)
END DO

IF( chem%nOutGroup > 0 )material(imat)%icls = IBSET(material(imat)%icls,MATID_MULTI_DEP)
IF( chem%nOutGroup > 0 )material(imat)%icls = IBSET(material(imat)%icls,MATID_MULTI_DOS)

nsg = num_puff_types( material(imat) )

DO i = material(imat)%ioffp+1,material(imat)%ioffp+nsg
  typeID(i)%mcID  = mat_mc%nMCtype
  typeID(i)%ipmc  = typeID(i)%npaux + 1
  typeID(i)%npaux = typeID(i)%npaux + n
  typeID(i)%icls  = IBSET(typeID(i)%icls,MATID_MULTI)
END DO

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eRoutine = 'InitChemMC'
eMessage = 'Error allocating structures'

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE RestartChemMC()

USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER i, ip

DO i = 1,mat_mc%nMCtype
  IF( mat_mc%type(i) == MC_CHEM )THEN

!====   Initialize equilibrium calculation

    CALL InitChemEquilibrium( mat_mc%ID(i) )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( nthreads > 1 )THEN
      DO ip = 2,nthreads
        CALL CopyChemRestartMC( mat_mc%ID(i),ip )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END IF

  END IF

END DO

!====== Set up for diagnostic messages

CALL xsetf( 0 )
CALL xsetun( lun_log )

ngd_chem  = 0
nbad_chem = 0
tot_bad   = 0

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE ReadChemControl( lun,ID )

USE scipuff_fi
USE multcomp_fd
USE chem_fi

!====   Reads multi-component control definitions from project.imc

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun
INTEGER, INTENT( IN ) :: ID

INTEGER ios, ic_units, ik_units, ie_units

CHARACTER(16)  emission_units     !g/s or ppm-m3/s
CHARACTER(16)  species_units
CHARACTER(16)  rate_species_units
CHARACTER(16)  rate_time_units
CHARACTER(PATH_MAXLENGTH) ambient_file

INTEGER io_units
LOGICAL step_ambient, step_chemistry
CHARACTER(PATH_MAXLENGTH) sfcflux_file
CHARACTER(16)  out_species_units
CHARACTER(256) particle_types, particle_units


NAMELIST / control / species_units, out_species_units, &
                     emission_units, rate_species_units, &
                     rate_time_units, particle_types, particle_units, &
                     ambient_file, step_ambient, sfcflux_file, rtol, step_chemistry
LOGICAL lerr
INTEGER i, nPTypes, nPUnits, alloc_stat
CHARACTER(256), DIMENSION(:), POINTER :: particleTypes
CHARACTER(256), DIMENSION(:), POINTER :: particleUnits

INTERFACE
  SUBROUTINE SplitString( InString,Separator,nSubStr,OutStrings,lerr )
    IMPLICIT NONE
    CHARACTER(*),   INTENT( IN    )       :: InString
    CHARACTER(1),   INTENT( IN    )       :: Separator
    INTEGER,        INTENT( INOUT )       :: nSubStr
    CHARACTER(256), DIMENSION(:), POINTER :: OutStrings
    LOGICAL,        INTENT( OUT   )       :: lerr
  END SUBROUTINE
END INTERFACE
#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

!===== Set default values

emission_units     = NOT_SET_C
species_units      = 'PPM'
rate_species_units = 'molecules/cm3'
rate_time_units    = 'sec'
ambient_file       = NOT_SET_C
step_ambient       = .FALSE.
sfcflux_file       = NOT_SET_C
out_species_units  = 'ug/m3'
NULLIFY(particleTypes)
NULLIFY(particleUnits)
nPUnits            = 0
nPTypes            = 0
particle_types     = NOT_SET_C
particle_units     = NOT_SET_C
step_chemistry     = .TRUE.
rtol               = 1.E-2

READ(lun,control,IOSTAT=ios)
IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadChemControl'
  eMessage = 'Error reading multicomponent CONTROL namelist'
  GOTO 9999
END IF

chem%flag = 0

#if defined(LINUX)
ios = RemoveCF( ambient_file )

#endif
IF( LEN_TRIM(ambient_file) == 0 )ambient_file = NOT_SET_C

CALL cupper( rate_time_units )
SELECT CASE( rate_time_units(1:1) )
  CASE( 'S' )
    chem%tConv = 1.0
  CASE( 'M' )
    chem%tConv = 1./60.0
  CASE( 'H' )
    chem%tConv = 1./3600.0
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid Rate time units'
    eInform  = 'Units='//TRIM(rate_time_units)
    eAction  = 'Must be secs, mins, or hrs'
    GOTO 9999
END SELECT

CALL cupper( rate_species_units )
SELECT CASE( TRIM(rate_species_units) )
  CASE( 'MOLECULES/CM3','MOLECULES/CC' )
    ik_units = UNIT_MOLECULE
  CASE( 'PPM' )
    ik_units = UNIT_PPM
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid Rate species units'
    eInform  = 'Units='//TRIM(rate_species_units)
    eAction  = 'Must be ppm or molecules/cc'
    GOTO 9999
END SELECT

CALL cupper( species_units )
SELECT CASE( TRIM(species_units) )
  CASE( 'PPM' )
    ic_units = UNIT_PPM
 CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid species units'
    eInform  = 'Units='//TRIM(species_units)
    eAction  = 'Must be ppm'
    GOTO 9999
END SELECT

CALL cupper( emission_units )
SELECT CASE( TRIM(emission_units) )
  CASE( 'PPM-M3/S' )
    ie_units = UNIT_PPM
  CASE( 'MOLECULES-M3/CM3-S','MOLECULES-M3/CC-S' )
    ie_units = UNIT_MOLECULE
  CASE( 'G/S ')
    ie_units = UNIT_G
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid species emission units'
    eInform  = 'Units='//TRIM(emission_units)
    eAction  = 'Must be ppm-m3/s, molecules-m3/cc-s, or g/s'
    GOTO 9999
END SELECT

CALL cupper( out_species_units )
SELECT CASE( TRIM(out_species_units) )
  CASE( 'UG/M3' )
    io_units = UNIT_UGM3
  !CASE( 'PPM' )
  !  io_units = UNIT_PPM
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid output species units'
    eInform  = 'Units='//TRIM(out_species_units)
    eAction  = 'Must be ug/m3'
    GOTO 9999
END SELECT

!--- split particle_types to read substrings
IF( particle_types /= NOT_SET_C )THEN
  CALL cupper( particle_types )
  nPTypes = 0
  CALL SplitString( particle_types,',',nPTypes,particleTypes,lerr )
  IF( lerr .OR. nPTypes == 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid MC particle types'
    eInform  = 'Types = '//TRIM(particle_types)
    eAction  = 'Must have at least one MC particle type'
    GOTO 9999
  END IF
END IF

!--- split particle_units to read substrings
IF( particle_units /= NOT_SET_C )THEN
  nPUnits = 0
  CALL SplitString( particle_units,',',nPUnits,particleUnits,lerr )
  IF( lerr .OR. nPUnits == 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid MC particle units'
    eInform  = 'Units = '//TRIM(particle_units)
    eAction  = 'Must have at least one MC particle unit'
    GOTO 9999
  END IF
END IF

!--- Check number of particle types and units are equal
IF( nPTypes /= nPUnits )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadChemControl'
  eMessage = 'Different number of MC particle types and units'
  WRITE(eInform,'("Number of MC particle types = ",I3,", MC particle units = ",I3)')nPTypes,nPUnits
  eAction  = 'Must have same number of MC particle types and units'
  GOTO 9999
END IF

!--- Assign particle types and units
IF( nPTypes == 0 )THEN
  chem%pTypeN = 1
ELSE
  chem%pTypeN = nPTypes
END IF
ALLOCATE( chem%pTypes(chem%pTypeN),chem%pUnits(chem%pTypeN),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating particle type and emission unit arrays'
  eRoutine = 'ReadChemControl'
  GOTO 9999
END IF
IF( nPTypes == 0 )THEN
  chem%pTypes(1) = 'P'
  chem%pUnits(1) = 'ug/m3'
ELSE
  DO i = 1,chem%pTypeN
    chem%pTypes(i) = particleTypes(i)(1:6)
    chem%pUnits(i) = particleUnits(i)(1:6)
  END DO
END IF

chem%oUnits = io_units
chem%cUnits = ic_units
chem%kUnits = ik_units
chem%eUnits = ie_units

!====   Set unit conversion flags

! -- rate constant conversion

IF( ic_units == ik_units )THEN
  chem%kConv = NO_CONVERSION
ELSE IF( ic_units == UNIT_PPM )THEN
  chem%kConv = MOLECULE_PPM
ELSE
  chem%kConv = -MOLECULE_PPM
END IF

! -- emission rate conversion

IF( ie_units == ic_units )THEN
  chem%eConvType = NO_CONVERSION
  chem%eConv     = 1.0
ELSE IF( ie_units == UNIT_MOLECULE )THEN
  chem%eConvType = MOLECULE_PPM         !Av(molecules/mole)*P0(N/m2)/R0(J/mole/K)/10**6
  chem%eConv     = 7.35E+15*(1./298.)   !Av=6.022E23  P0=1.013E5  R0=8.314E6 at STP
ELSE IF( ie_units == UNIT_PPM )THEN
  chem%eConvType = -MOLECULE_PPM
  chem%eConv     = 298/7.35E+15         !Av=6.022E23  P0=1.013E5  R0=8.314E6 at STP
ELSE IF( ie_units == UNIT_G )THEN
  SELECT CASE( ic_units )
    CASE( UNIT_PPM )
      chem%eConvType = G_PPM
      chem%eConv     = 82.06*298.     !RT/P * 10+6 (multiply by 1/MW later) at STP
    CASE( UNIT_MOLECULE )
      chem%eConvType = G_MOLECULE
      chem%eConv     = 6.022E17       !Av   * 10-6 (multiply by 1/MW later)
    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'ReadChemControl'
      eMessage = 'Invalid species units'
      eInform  = 'Units='//TRIM(species_units)
      GOTO 9999
    END SELECT
END IF

DoChemistry = step_chemistry

IF( ambient_file /= NOT_SET_C )THEN
  chem%ambFile  = TRIM(ambient_file)
  chem%lAmbFile = .TRUE.
  chem%lunAmb   = 300 + ID
  chem%lstepAmb = .FALSE. ! Always set to False(not step_ambient) for for SCICHEM 3.0(Beta 2)
ELSE
  chem%ambFile  = ' '
  chem%lAmbFile = .FALSE.
  chem%lunAmb   = 0
  chem%lstepAmb = .FALSE. ! ! Always set to False(not step_ambient) for for SCICHEM 3.0(Beta 2)
END IF

chem%lAddAmb  = .TRUE. ! Default set to True for SCICHEM 3.0
IF( sfcflux_file /= NOT_SET_C )THEN
  IF( .NOT. chem%lAmbFile )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadChemControl'
    eMessage = 'Invalid setting for surface flux'
    eInform  = 'Ambient file not found'
    eAction  = 'Must have a valid ambient file name'
    GOTO 9999
  END IF
  chem%sFlxFile  = TRIM(sfcflux_file)
  chem%lSfcFlx   = .TRUE.
  chem%lunSfcFlx = 301 + ID
ELSE
  chem%sFlxFile  = ''
  chem%lSfcFlx   = .FALSE.
  chem%lunSfcFlx = 0
END IF

chem%FileNotUsed = ' '

chem%rtol = rtol

9999 CONTINUE

IF( ASSOCIATED(particleTypes) )DEALLOCATE( particleTypes,STAT=alloc_stat )
IF( ASSOCIATED(particleUnits) )DEALLOCATE( particleUnits,STAT=alloc_stat )

RETURN
END

!========================================================================

SUBROUTINE ReadChemSpecies( lun,file )

USE scipuff_fi
USE files_fi
USE chem_fi
USE landuse_fd

!====   Reads multi-component species definitions from material.imc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

CHARACTER(1), PARAMETER :: SET_MODE = '#'
CHARACTER(1), PARAMETER :: BLANK    = ' '
CHARACTER(1), PARAMETER :: TAB      = '     '

CHARACTER(128) line
CHARACTER(16)  namex, ctype, dtype

INTEGER ios, nch, ncc, nspec
LOGICAL lerr, ldep, ldos, lamb
INTEGER irv, i, j
LOGICAL ldrydep
REAL    Henry0, TpFac, RxFac, SrfRFac
REAL    amb, tol_ode, vdep, scav, mw

TYPE( ChemSpecies_str ), POINTER :: spec

TYPE( landuse_init )file_landuse

INTEGER, EXTERNAL :: InitLandUseDep

#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

nspec  = 0
lamb   = .FALSE.
H2O    = NOT_SET_I

!---- Read species data

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'ReadChemSpecies'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )EXIT

!====   Change TABS to Blanks for reading

#if defined(LINUX)
  nch = RemoveCF( line )
#else
  nch = LEN_TRIM( line )
#endif
  CALL get_value( line,nch,TAB,BLANK,lerr )

!====   Read name

  CALL get_c( line,nch,BLANK,namex,ncc,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemSpecies'
    eMessage = 'Error reading Multi-component species name'
    eInform  = 'Line='//TRIM(line)
    GOTO 9999
  END IF

!====   Read type - Fast,Slow,Equilibrium or Particle (EPA)

  CALL get_c( line,nch,BLANK,ctype,ncc,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemSpecies'
    eMessage = 'Error reading Multi-component species type'
    eInform  = 'Line='//TRIM(line)
    GOTO 9999
  END IF

!====   Read rest of the data

  CALL get_r( line,nch,amb,    lerr )
  CALL get_r( line,nch,tol_ode,lerr )
  IF( lerr )GOTO 9998
  line = ADJUSTL( line )
  nch = LEN_TRIM( line )

!--- Ensure parentheses contain spaces for get_r

  DO WHILE( INDEX(line,')') /= 0 .AND. INDEX(line,' )') == 0 )
    i = INDEX(line,')')
    DO j = nch,i,-1
      line(j+1:j+1) = line(j:j)
    END DO
    line(i:i) = ' '
    nch = nch + 1
  END DO

  IF( line(1:1) == '(' )THEN
    line = line(2:)
    nch = nch - 1
    CALL get_r( line,nch,Henry0, lerr )
    CALL get_r( line,nch,TpFac,lerr )
    CALL get_r( line,nch,RxFac,lerr )
    CALL get_r( line,nch,SrfRFac,lerr )
    IF( Henry0 < 0. )THEN
      Henry0  = 0.
      ldrydep = .FALSE.
    ELSE
      ldrydep = .TRUE.
    END IF
    IF( lerr )GOTO 9998
    line = ADJUSTL( line )
    IF( line(1:1) /= ')' )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadChemSpecies'
      eMessage = 'Non-constant deposition velocity must end with close parenthesis'
      eInform  = 'Line='//TRIM(namex)//'   '//TRIM(line)
      GOTO 9999
    END IF
    line = line(2:)
    nch  = nch - 1
    vdep = NOT_SET_R
  ELSE
    CALL get_r( line,nch,vdep,lerr )
    Henry0  = NOT_SET_R
    TpFac   = NOT_SET_R
    RxFac   = NOT_SET_R
    SrfRFac = NOT_SET_R
    lDryDep = .FALSE.
  END IF

  line = ADJUSTL( line )
  nch = LEN_TRIM( line )
  CALL get_r( line,nch,scav,lerr )
  CALL get_r( line,nch,mw,  lerr )
  IF( lerr )GOTO 9998

  CALL get_c( line,nch,BLANK,dtype,ncc,lerr )
  IF( lerr )dtype = 'F'
  ldos = dtype(1:1) == 'T'

  CALL get_c( line,nch,BLANK,dtype,ncc,lerr )
  IF( lerr )dtype = 'F'
  ldep = dtype(1:1) == 'T'

  nspec = nspec + 1
  CALL ReallocateChemSpecies( nspec )
  IF( nError /= NO_ERROR )GOTO 9999

  spec => chem%species(nspec)

  spec%ID  = nspec

!====   Set species structure

  spec%lstar = .FALSE.

  ! Must have same species order in dos and dep file for sciDOSpost
  spec%ldos = ldos .OR. ldep
  spec%ldep = ldep .OR. ldos

  spec%name    = namex
  spec%ambient = amb
  IF( TRIM(namex) /= 'H2O' .AND. TRIM(namex) /= 'WATER' )THEN
    IF( amb < 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'ReadChemSpecies'
      eMessage = 'Negative ambient specified for species '//TRIM(namex)
      eInform  = 'Negative value only allowed for H2O/WATER'
      GOTO 9999
    END IF
  ELSE
    IF( amb >= 0.0 )H2O = nspec
  END IF

  spec%tol  = tol_ode
  spec%vdep = vdep
  spec%scav = scav
  spec%mw   = mw
  spec%nit = 0.
  spec%lim = NOT_SET_R

  spec%mass = 0.
  spec%conc = 0.
  spec%amb  = 0.
  spec%msav = 0.
  spec%csav = 0.
  spec%csav_noy = 0.
  spec%mddp = 0.
  spec%mwdp = 0.
  spec%mass2 = 0.
  spec%conc2 = 0.

  spec%Henry0  = Henry0
  spec%TpFac   = TpFac
  spec%RxFac   = RxFac
  spec%SrfRFac = SrfRFac
  spec%ldrydep = ldrydep

  SELECT CASE( ctype(1:1) )
    CASE( 'A','a' )

      spec%class = ID_SPECIES_AMBIENT

      spec%ldos = .FALSE.
      spec%ldep = .FALSE.

      lamb = .TRUE.

    CASE DEFAULT

      IF( lamb )THEN
        nError   = IV_ERROR
        eRoutine = 'ReadChemSpecies'
        eMessage = 'Ambient species must be specified at end of list'
        GOTO 9999
      END IF

      SELECT CASE( ctype(1:1) )
        CASE( 'E','e' )
          ! Change equlibrium species to fast for SCICHEM Beta2
          spec%class = ID_SPECIES_FAST
        CASE( 'S','s' )
          spec%class = ID_SPECIES_SLOW
        CASE( 'F','f' )
          spec%class = ID_SPECIES_FAST
        CASE( 'P','p' )
          spec%class = ID_SPECIES_PARTICLE
          spec%classAux = ctype
        CASE DEFAULT
          nError   = RD_ERROR
          eRoutine = 'ReadChemSpecies'
          eMessage = 'Invalid species type'
          eInform  = 'Type ='//TRIM(ctype)
          GOTO 9999
      END SELECT

  END SELECT

END DO

chem%nSpecies = nspec

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'ReadChemSpecies'
eMessage = 'Error reading Multi-component species data'
eInform  = 'Line='//TRIM(namex)//'   '//TRIM(line)
GOTO 9999

END

!========================================================================

SUBROUTINE ReallocateChemSpecies( nspec )

USE error_fi
USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nspec

INTEGER i, n, alloc_stat

TYPE( ChemSpecies_str ), DIMENSION(:), ALLOCATABLE :: tmp

IF( nspec == 1 )THEN
  ALLOCATE( chem%species(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
ELSE
  ALLOCATE( tmp(nspec),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  n = SIZE(chem%species)
  DO i = 1,n
    tmp(i) = chem%species(i)
  END DO
  DEALLOCATE( chem%species,STAT=alloc_stat )
  ALLOCATE( chem%species(nspec),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  DO i = 1,n
    chem%species(i) = tmp(i)
  END DO
  DEALLOCATE( tmp,STAT=alloc_stat )
END IF

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error allocating multicomponent arrays'
eRoutine = 'ReallocateChemSpecies'
GOTO 9999

END

!========================================================================

SUBROUTINE ReallocateChemReactions( nreact,line,rline )

USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN )                 :: nreact
CHARACTER(512), INTENT( IN )          :: line
CHARACTER(512), DIMENSION(:), POINTER :: rline

INTEGER                               :: i, n, alloc_stat
CHARACTER(512), DIMENSION(:),POINTER  :: tmp

IF( nreact == 1 )THEN
  ALLOCATE( rline(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  rline(1) = line
ELSE
  n = SIZE(rline)
  tmp => rline
  NULLIFY(rline)
  ALLOCATE( rline(nreact),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  DO i = 1,n
    rline(i) = tmp(i)
  END DO
  rline(nreact) = line
  DEALLOCATE( tmp,STAT=alloc_stat )
END IF

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error allocating reaction line arrays'
eRoutine = 'ReallocateChemReactions'
GOTO 9999

END

!========================================================================

SUBROUTINE GetChemSpec( line,nch,fac,ispec )

USE scipuff_fi
USE chem_fi

!====   Read next species and stoichiometric coefficient from reaction line
!       and return pointer to species in species list

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( OUT   ) :: nch
REAL   ,      INTENT( OUT   ) :: fac
INTEGER,      INTENT( OUT   ) :: ispec

CHARACTER(1), PARAMETER :: LBRK  = '['
CHARACTER(1), PARAMETER :: RBRK  = ']'
CHARACTER(1), PARAMETER :: LPRN  = '('
CHARACTER(1), PARAMETER :: RPRN  = ')'
CHARACTER(1), PARAMETER :: BLANK = ' '

INTEGER ncc, ios, i, ioff
LOGICAL lerr

CHARACTER(32) string

!====   Read coefficient if present

nch = LEN_TRIM(line)
CALL get_c( line,nch,BLANK,string,ncc,lerr )
IF( string(1:1) == LPRN )THEN
  READ(string(2:ncc-1),*,IOSTAT=ios) fac
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'GetChemSpec'
    eMessage = 'Error reading Multi-component input file'
    eInform  = 'Invalid species = '//TRIM(string)
    GOTO 9999
  END IF
  CALL get_c( line,nch,BLANK,string,ncc,lerr )
ELSE
  fac = 1.0
END IF

!====   Read species

IF( string(1:1) == LBRK )THEN
  ispec = 0
  IF( string(2:2) == RBRK )THEN
    nError   = RD_ERROR
    eRoutine = 'GetChemSpec'
    eMessage = 'Error reading Multi-component input file'
    eInform  = 'Invalid species = '//TRIM(string)
    GOTO 9999
  END IF
ELSE
  nError   = RD_ERROR
  eRoutine = 'GetChemSpec'
  eMessage = 'Error reading Multi-component input file'
  eInform  = 'Line = '//TRIM(string)//' '//TRIM(line)
  GOTO 9999
END IF

!====   Find species in species list

ioff = 0
DO i = 1,chem%nSpecies
  IF( TRIM(string(2:ncc-1)) == TRIM(chem%species(i)%name) )ispec = i + ioff
END DO

IF( ispec == 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'GetChemSpec'
  eMessage = 'Error reading Multi-component input file'
  eInform  = 'Invalid species = '//TRIM(string)
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE SetReactionClass( sclass,rclass )

USE chem_fi

!====   Set reaction class from species class

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: sclass
INTEGER, INTENT( INOUT ) :: rclass

SELECT CASE( sclass )

  CASE( ID_SPECIES_FAST )
    rclass = IBSET(rclass,ID_REACT_FAST)

  CASE( ID_SPECIES_SLOW )
    rclass = IBSET(rclass,ID_REACT_SLOW)

  CASE( ID_SPECIES_EQUILIBRIUM )
    rclass = IBSET(rclass,ID_REACT_EQUILIBRIUM)

  CASE DEFAULT

END SELECT

!--- Set all thermal reactions as fast

IF( BTEST(rclass,ID_REACT_THERMAL) )rclass = IBSET(rclass,ID_REACT_FAST)

RETURN
END

!========================================================================

SUBROUTINE ReadChemGroup( lun,file )

USE scipuff_fi
USE chem_fi
USE reallocate

!====   Reads output groups of multi-component species

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

CHARACTER(1), PARAMETER :: SET_MODE = '#'
CHARACTER(1), PARAMETER :: BLANK    = ' '
CHARACTER(1), PARAMETER :: TAB      = '     '

INTEGER  i, j, k, l, nch, ios, narg, maxn, ng, ncomp
LOGICAL  lerr
REAL     f

CHARACTER(16)  kwrd
CHARACTER(2000) line

CHARACTER(16), DIMENSION(:), ALLOCATABLE ::  carg

#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

!---- Get number of groups

ng = 0

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'ReadChemGroup'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
#if defined(LINUX)
  ios = RemoveCF(line)
#endif

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )EXIT

  ng = ng + 1

END DO

IF( ng == 0 )GOTO 9999 !No output groups defined

chem%nOutGroup = ng

ALLOCATE( chem%OutGroup(ng),carg(chem%nSpecies+1),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadChemGroup'
  eMessage = 'Error allocating Multi-component output group array'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

maxn = chem%nSpecies+1
narg = 0
kwrd = 'NONE'

DO i = 1,ng+1
  BACKSPACE( lun,IOSTAT=ios )
END DO

DO i = 1,ng
  NULLIFY( chem%OutGroup(i)%iComp,chem%OutGroup(i)%fComp )
END DO

!---- Loop over groups to get contributing species

DO i = 1,ng

  CALL get_next_data( lun,line,nch,kwrd,narg,carg,maxn,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemGroup'
    eMessage = 'Error reading Multi-component output group definition'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  IF( narg < 2 )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadChemGroup'
    eMessage = 'Insufficient input defining multi-component output group'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  chem%OutGroup(i)%Name = TRIM(carg(1))

  ncomp = 0

  DO j = 2,narg
    IF( carg(j)(1:1) /= '(' )ncomp = ncomp + 1
  END DO

  IF( ncomp < 1 )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadChemGroup'
    eMessage = 'No species defined for multi-component output group '//TRIM(carg(1))
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  chem%OutGroup(i)%nComp = ncomp
  ALLOCATE( chem%OutGroup(i)%iComp(ncomp),chem%OutGroup(i)%fComp(ncomp),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadChemGroup'
    eMessage = 'Error allocating component arrays for multi-component output group '//TRIM(carg(1))
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  chem%OutGroup(i)%iComp(1:ncomp) = 0
  chem%OutGroup(i)%fComp(1:ncomp) = 1.0

  ncomp = 1
  DO j = 2,narg
    IF( carg(j)(1:1) == '(' )THEN
      k = LEN_TRIM(carg(j)) !Assumed to be ')'
      k = MAX(k-1,2)
      READ(carg(j)(2:k),*,IOSTAT=ios) chem%OutGroup(i)%fComp(ncomp)
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadChemGroup'
        eMessage = 'Error reading component factor for multi-component output group '//TRIM(carg(1))
        CALL ReportFileName( eInform,'File=',file )
        GOTO 9999
      END IF
    ELSE
      DO k = 1,chem%nSpecies
        IF( TRIM(carg(j)) == chem%species(k)%name )THEN
          chem%OutGroup(i)%iComp(ncomp) = k
          EXIT
        END IF
      END DO
      IF( chem%OutGroup(i)%iComp(ncomp) == 0 )THEN
        f = chem%OutGroup(i)%fComp(ncomp)
        DO k = 1,i-1
          IF( TRIM(carg(j)) == TRIM(chem%OutGroup(k)%Name) )THEN
            ios = reallocate_integer1d( chem%OutGroup(i)%iComp,chem%OutGroup(k)%nComp-1 )
            IF( ios == 0 )THEN
              ios = reallocate_real1d( chem%OutGroup(i)%fComp,chem%OutGroup(k)%nComp-1 )
            END IF
            IF( ios /= 0 )THEN
              nError   = UK_ERROR
              eRoutine = 'ReadChemGroup'
              eMessage = 'Error reallocating list of group species and factors'
              CALL ReportFileName( eInform,'File=',file )
              GOTO 9999
            END IF
            chem%OutGroup(i)%iComp(chem%OutGroup(i)%nComp+1:chem%OutGroup(i)%nComp+chem%OutGroup(k)%nComp-1) = 0
            chem%OutGroup(i)%fComp(chem%OutGroup(i)%nComp+1:chem%OutGroup(i)%nComp+chem%OutGroup(k)%nComp-1) = 1.
            ncomp = ncomp - 1
            DO l = 1,chem%OutGroup(k)%nComp
              chem%OutGroup(i)%iComp(ncomp+l) = chem%OutGroup(k)%iComp(l)
              chem%OutGroup(i)%fComp(ncomp+l) = chem%OutGroup(k)%fComp(l)*f
            END DO
            ncomp = ncomp + chem%OutGroup(k)%nComp
            chem%OutGroup(i)%nComp = chem%OutGroup(i)%nComp + chem%OutGroup(k)%nComp - 1
            EXIT
          END IF
        END DO
      END IF
      IF( chem%OutGroup(i)%iComp(ncomp) == 0 )THEN
        nError   = IV_ERROR
        eRoutine = 'ReadChemGroup'
        eMessage = 'Invalid specie '//TRIM(carg(j))//' for multi-component output group '//TRIM(carg(1))
        CALL ReportFileName( eInform,'File=',file )
        GOTO 9999
      END IF
      ncomp = ncomp + 1
      IF( ncomp > chem%OutGroup(i)%nComp )EXIT
    END IF
  END DO

END DO

9999 CONTINUE

IF( ALLOCATED(carg) )DEALLOCATE( carg,STAT=ios )

RETURN
END

!========================================================================

SUBROUTINE ReadChemTable( lun,file )

USE scipuff_fi
USE chem_fi
USE chemReactions_fd
USE reallocate

!====   Reads multi-component radiation dependent rate table (zenith vs K) from project.imc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

CHARACTER(1), PARAMETER :: SET_MODE = '#'
CHARACTER(1), PARAMETER :: BLANK    = ' '
CHARACTER(1), PARAMETER :: TAB      = '     '

CHARACTER(128) line

INTEGER  nch, ir, i, nzenith, irv, ios, alloc_Stat
REAL     ang
LOGICAL  lerr
#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

nzenith = 0

chem%nZenith = nzenith

!---- Read table data

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'ReadChemTable'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
#if defined(LINUX)
  ios = RemoveCF(line)
#endif

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )EXIT

  nch = LEN_TRIM(line)
  CALL get_value( line,nch,TAB,BLANK,lerr )

!====   Read reaction ID (0->zenith data )

  CALL get_i( line,nch,ir,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemTable'
    eMessage = 'Error reading Multi-component table'
    GOTO 9999
  END IF

!====   Read zenith data

  IF( ir /= 0 .AND. nzenith == 0 )THEN

    nError   = RD_ERROR
    eRoutine = 'ReadChemTable'
    eMessage = 'Multi-component table must begin with zenith angles'
    GOTO 9999

  ELSE IF( ir == 0 )THEN

!====== Read data

    DO WHILE( nch > 0 )

      CALL get_r( line,nch,ang,lerr )
      IF( lerr )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadChemTable'
        eMessage = 'Error reading Multi-component table'
        GOTO 9999
      END IF

!====== Save data

      nzenith = nzenith + 1

      IF( nzenith == 1 )THEN
        ALLOCATE( chem%zenith(1),STAT=irv )
      ELSE
        irv = reallocate_real1d( chem%zenith,1 )
      END IF
      IF( irv /= 0  )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadChemTable'
        eMessage = 'Error allocating Multi-component zenith table'
        GOTO 9999
      END IF
      chem%zenith(nzenith) = ang

    END DO

    chem%nZenith = nzenith

!====   Read rate data

  ELSE

!====== Allocate

    IF( chem%reaction(ir)%type /= ID_K_RAD )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadChemTable'
      eMessage = 'Error reading Multi-component table'
      WRITE(eInform,'(A,I4,A)')'Reaction ID:',ir,' is not photochemical radiation type'
      GOTO 9999
    END IF


    IF( ASSOCIATED(chem%reaction(ir)%data) )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadChemTable'
      eMessage = 'Error reading Multi-component table'
      WRITE(eInform,'(A,I4,A)')'Reaction ID:',ir,' already defined'
      GOTO 9999
    END IF

    ALLOCATE( chem%reaction(ir)%data(nzenith),STAT=alloc_stat )

!====== Read data

    i = 0

    DO WHILE( nch > 0 )

      CALL get_r( line,nch,ang,lerr )
      IF( lerr )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadChemTable'
        eMessage = 'Error reading Multi-component table'
        GOTO 9999
      END IF

!====== Save data

      i = i + 1
      IF( i > nzenith )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadChemTable'
        eMessage = 'table data out of order'
        GOTO 9999
      END IF
      chem%reaction(ir)%data(i) = ang

    END DO

    IF( i < nzenith )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadChemTable'
      eMessage = 'Insufficient table data'
      GOTO 9999
    END IF

  END IF

END DO

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE ReadChemBalance( lun,file )

USE scipuff_fi
USE chem_fi
USE reallocate

!====   Reads multi-component species that must be balanced from project.imc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

CHARACTER(1), PARAMETER :: SET_MODE = '#'
CHARACTER(1), PARAMETER :: BLANK    = ' '
CHARACTER(1), PARAMETER :: TAB      = '     '

INTEGER  i, nch, ncc, ios
REAL nit
CHARACTER(16)  namex
CHARACTER(128) line
LOGICAL  lerr
#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

DO i = 1,chem%nSpecies
  chem%species(i)%nit = 0.
END DO

!---- Read species balance data

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eRoutine = 'ReadChemBalance'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
#if defined(LINUX)
  ios = RemoveCF(line)
#endif

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )EXIT

  nch = LEN_TRIM(line)
  CALL get_value( line,nch,TAB,BLANK,lerr )

!====   Read species name
  CALL get_c( line,nch,BLANK,namex,ncc,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemBalance'
    eMessage = 'Error reading Multi-component species name'
    eInform  = 'Line='//TRIM(line)
    GOTO 9999
  END IF

!====   Read number of molecules in species
  nit = 0.
  CALL get_r( line,nch,nit,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemBalance'
    eMessage = 'Error reading number of molecules in species'
    GOTO 9999
  END IF

  i = 1
  DO WHILE( i <= chem%nSpecies )
    IF( chem%species(i)%name == namex )EXIT
    i = i + 1
  END DO
  IF( i > chem%nSpecies )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadChemBalance'
    eMessage = 'Species for balance not in species list'
    WRITE(eInform,'(A,A)') 'Species name :',TRIM(namex)
    GOTO 9999
  END IF
  chem%species(i)%nit = nit

END DO

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE AllocChemDiagnostics()

USE scipuff_fi
USE chem_fi
USE diagnostics_fi

!====   Allocates diagnostic arrays

IMPLICIT NONE

INTEGER alloc_stat

ndvar = NDIAG*(nspectot+1) + 2

IF( ALLOCATED( dgname ) )DEALLOCATE( dgname,STAT=alloc_stat )
ALLOCATE( dgname(ndvar),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'InitChemDiagnostics'
  eMessage = 'Insufficient memory to allocate dgname name array'
  WRITE(eInform,*) 'Bytes requested =',(NDIAG*(nspectot+1) + 2)*4
  GO TO 9999
END IF

IF( ALLOCATED( emission ) )DEALLOCATE( emission,statics,boundary,STAT=alloc_stat )
ALLOCATE( emission(nspectot+1),statics(nspectot+1),boundary(nspectot+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'AllocChemDiagnostics'
  eMessage = 'Insufficient memory to allocate diagnostic arrays'
  WRITE(eInform,*) 'Bytes requested =',nspectot*3*4
  GO TO 9999
END IF

IF( ALLOCATED( transport ) )DEALLOCATE( transport,ddeposition,wdeposition,chemistry,STAT=alloc_stat )
ALLOCATE( transport(nspectot+1),ddeposition(nspectot+1),wdeposition(nspectot+1),chemistry(nspectot+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'AllocChemDiagnostics'
  eMessage = 'Insufficient memory to allocate diagnostic arrays'
  WRITE(eInform,*) 'Bytes requested =',nspectot*3*4
  GO TO 9999
END IF

IF( ALLOCATED( active ) )DEALLOCATE( active,removed,ppmfac,STAT=alloc_stat )
ALLOCATE( active(nspectot+1),removed(nspectot+1),ppmfac(nspectot+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'AllocChemDiagnostics'
  eMessage = 'Insufficient memory to allocate diagnostic arrays'
  WRITE(eInform,*) 'Bytes requested =',nspectot*3*4
  GO TO 9999
END IF

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE InitChemDiagnostics( ID )

USE files_fi
USE scipuff_fi
USE chem_fi
USE diagnostics_fi

!====   Reads multi-component species that must be balanced from project.imc

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER alloc_stat, ios, ioff, i, j

CHARACTER(8) fflag

IF( lDiagno )THEN

  chem => chemMC(ID,1)
  nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

  CALL AllocChemDiagnostics()

  ndump        = 0
  emission     = 0.
  statics      = 0.
  boundary     = 0.
  transport    = 0.
  ddeposition  = 0.
  wdeposition  = 0.
  chemistry    = 0.
  active       = 0.
  removed      = 0.
  ppmfac       = 0.

  IF( lDepBin )THEN
    OPEN( lun_dgn,FILE=file_dgn,IOSTAT=ios,FORM='UNFORMATTED' )
  ELSE
    OPEN( lun_dgn,FILE=file_dgn,IOSTAT=ios )
  END IF
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemDiagnostics'
    eMessage = 'Error opening diagnostic file'
    WRITE(eInform,*) 'File =',TRIM(file_dgn)
    GO TO 9999
  END IF

  dgname(1) = 't                 '
  dgname(2) = 'ndump             '
  ioff      = 2

  DO i = 1,nspectot
    DO j = 1,NDIAG
      dgname(ioff + (i-1)*NDIAG + j) = SFX(j)//'_'//chem%species(i)%name
    END DO
  END DO

  DO j = 1,NDIAG
    dgname(ioff + NDIAG*nspectot + j) = SFX(j)//'_TRACER          '
  END DO

  IF( lDepBin )THEN
    fflag = 'BBBBBBBB'
    WRITE(lun_dgn,IOSTAT=ios)fflag
    WRITE(lun_dgn,IOSTAT=ios)ndvar
    WRITE(lun_dgn,IOSTAT=ios)(dgname(j),j=1,ndvar)
  ELSE
    fflag = 'FFFFFFFF'
    WRITE(lun_dgn,'(A8)',IOSTAT=ios)fflag
    WRITE(lun_dgn,'(I5)',IOSTAT=ios)ndvar
    WRITE(lun_dgn,*,IOSTAT=ios)(dgname(j),j=1,ndvar)
  ENDIF
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemDiagnostics'
    eMessage = 'Error writing diagnostic variable names'
    WRITE(eInform,*) 'File =',TRIM(file_dgn)
    GO TO 9999
  END IF

9999 CONTINUE

  IF( ALLOCATED(dgname) )DEALLOCATE( dgname,STAT=alloc_stat )

END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetStarSpecies( ID )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER i, j
LOGICAL NewSpecies

NewSpecies = .FALSE.

chem => chemMC(ID,1)

chem%nStar = 0

!====   Loop over reactions

DO i = 1,chem%nReactions

!====== Quadratic reactions

  IF( .NOT.BTEST(chem%reaction(i)%class,ID_REACT_LINEAR) )THEN

!======== Reactant A

    CALL SetStar( ID,chem%reaction(i)%A,NewSpecies )

!======== Reactant B

    CALL SetStar( ID,chem%reaction(i)%B,NewSpecies )

!======== Products

    DO j = 1,chem%reaction(i)%nP
      CALL SetStar( ID,chem%reaction(i)%P(j),NewSpecies )
    END DO

  END IF

END DO

!====== Equilibrium species

DO i = 1,chem%nSpecies

  IF( chem%species(i)%class == ID_SPECIES_EQUILIBRIUM )THEN
    CALL SetStar( ID,chem%species(i)%ID,NewSpecies )
  END IF

END DO

CALL ReAllocStarSpecies( ID,NewSpecies )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetStar( ID,jA,lset )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ID
INTEGER, INTENT( IN    ) :: jA
LOGICAL, INTENT( INOUT ) :: lset

INTEGER iA

iA = jA

IF( iA <= chem%nSpecies )THEN

  IF( .NOT.chem%species(iA)%lstar .AND. &
           chem%species(iA)%class /= ID_SPECIES_AMBIENT )THEN
    chem%species(iA)%lstar = .TRUE.
    chem%nStar = chem%nStar + 1
    lset = .TRUE.
  END IF

END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ReAllocStarSpecies( ID,NewSpecies )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER i, j, alloc_stat
LOGICAL NewSpecies

!====   Check linear equations for influence

DO WHILE( NewSpecies )

  NewSpecies = .FALSE.

  DO i = 1,chem%nReactions

    IF( BTEST(chem%reaction(i)%class,ID_REACT_LINEAR) )THEN

      DO j = 1,chem%reaction(i)%nP

        IF( chem%species(chem%reaction(i)%P(j))%lstar )THEN
          CALL SetStar( ID,chem%reaction(i)%A,NewSpecies )
        END IF

      END DO

    END IF

  END DO

END DO

!====   Set pointers for "star" species

IF( chem%nStar > 0 )THEN

  IF( ASSOCIATED(chem%star) )DEALLOCATE( chem%star,STAT=alloc_stat )
  ALLOCATE( chem%star(chem%nStar),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'SetStarSpecies'
    eMessage = 'Error allocating multicomponent arrays'
    GOTO 9999
  END IF

  j = 0
  DO i = 1,chem%nSpecies
    IF( chem%species(i)%lstar )THEN
      j = j + 1
      chem%star(j)%s => chem%species(i)
    END IF
  END DO

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemEquilibrium( ID )

USE chem_fi
USE chemReactions_fd
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER i, j, k, iA, iB, iP, ii

INTEGER neq, nlin, elim, nsolve, is, js
INTEGER jj, kk
INTEGER alloc_stat

LOGICAL check

REAL,    DIMENSION(:,:,:), ALLOCATABLE :: Cq
REAL,    DIMENSION(:,:),   ALLOCATABLE :: Cl
INTEGER, DIMENSION(:),     ALLOCATABLE :: IndexEq
INTEGER, DIMENSION(:),     ALLOCATABLE :: item
INTEGER, DIMENSION(:),     ALLOCATABLE :: nquad

chem => chemMC(ID,1)

neq   = chem%nEquilibrium

!==== Check for equilibrium species

IF( neq <= 0 )THEN
  NULLIFY( chem%IndexEq,chem%TypeEq,chem%RowEq )
  RETURN
END IF

!==== Allocate and clear solver arrays

ALLOCATE( Cq(neq,neq,neq),Cl(neq,neq),IndexEq(neq),item(neq),nquad(neq),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'InitChemEquilibrium'
  eMessage = 'Error allocating solver temporary arrays'
  GOTO 9999
END IF

DO i = 1,neq
  nquad(i) = 0
  DO j = 1,neq
    Cl(i,j) = 0.0
    DO k = 1,neq
      Cq(i,j,k) = 0.0
    END DO
  END DO
END DO

!==== Loop over reactions and set solver coefficients

reaction => chem%eReact
species  => chem%species

DO i = 1,chem%nEquilReact

!====== Reaction has equilibrium reactants/products

  IF( .NOT.((reaction(i)%r%type == ID_K_CONST) &
            .AND. (reaction(i)%r%data(1) == 0.)) )THEN

    iA = reaction(i)%r%A
    iA = species(iA)%eqID

!======== Linear Reactions

    IF( BTEST(reaction(i)%r%class,ID_REACT_LINEAR) )THEN

!==========  Equilibrium reactant

      IF( iA > 0 )THEN

        Cl(iA,iA) = 1.
        DO j = 1,reaction(i)%r%nP
          iP = reaction(i)%r%P(j)
          iP = species(iP)%eqID
          IF( iP > 0 )Cl(iP,iA) = 1.
        END DO

      END IF

!======== Quadratic reaction

    ELSE

      iB = reaction(i)%r%B
      iB = species(iB)%eqID

!========== Equilibrium A : Non equilibrium B

      IF( iA > 0 .AND. iB == 0 )THEN

        Cl(iA,iA) = 1.
        DO j = 1,reaction(i)%r%nP
          iP = reaction(i)%r%P(j)
          iP = species(iP)%eqID
          IF( iP > 0 )Cl(iP,iA) = 1.
        END DO

!========== Equilibrium B : Non equilibrium A

      ELSE IF( iB > 0  .AND. iA == 0 )THEN

        Cl(iB,iB) = 1.
        DO j = 1,reaction(i)%r%nP
          iP = reaction(i)%r%P(j)
          iP = species(iP)%eqID
          IF( iP > 0 )Cl(iP,iB) = 1.
        END DO

!========== Equilibrium A : Equilibrium B -> Nonlinear terms

      ELSE IF( iA > 0 .AND. iB > 0  )THEN

        Cq(iA,iA,iB) = 1.
        Cq(iB,iA,iB) = 1.
        DO j = 1,reaction(i)%r%nP
          iP = reaction(i)%r%P(j)
          iP = species(iP)%eqID
          IF( iP > 0 )Cq(iP,iA,iB) = 1.
        END DO

      END IF
    END IF
  END IF

END DO

nsolve   = neq
ndir_eq  = 0
nsubs_eq = 0
nlin_eq  = 0

ALLOCATE( chem%IndexEq(neq),chem%TypeEq(neq), &
                            chem%RowEq(neq),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'InitChemEquilibrium'
  eMessage = 'Error allocating solver arrays'
  GOTO 9999
END IF

DO i = 1,neq
  IndexEq(i)            = i
  chem%IndexEq(i) = i
  chem%TypeEq(i)  = ISOLVE_EQ
END DO

!--- find species that can be solved directly

check = .TRUE.

DO WHILE( check )

  DO i = 1,nsolve
    item(i) = chem%IndexEq(i)
  END DO

  check = .FALSE.

  DO is = 1,nsolve

    i    = item(is)
    nlin = 0

    DO j = 1,neq
      jj = chem%IndexEq(j)
      IF( Cl(i,jj) /= 0. .AND. jj /= i )nlin = nlin + 1
      DO k = 1,neq
        kk = chem%IndexEq(k)
        IF( Cq(i,jj,kk) /= 0. )nquad(i) = nquad(i) + 1
      END DO
    END DO

    IF( nquad(i) == 0 .AND. nlin == 0 )THEN
      chem%IndexEq(nsolve) = i
      nsolve = nsolve - 1
      ndir_eq = ndir_eq + 1
      chem%TypeEq(i) = IDIR_EQ
      check = (nsolve > 0)
      DO k = 1,chem%nEquilibrium
        kk = chem%IndexEq(k)
        Cl(kk,i) = 0.
      END DO
    END IF
  END DO

  CALL SetIndexEq( ID,IndexEq )

END DO

!--- check for species that can substituted at the end

DO i = 1,chem%nEquilibrium
  ii = IndexEq(i)
  IF( chem%TypeEq(ii) == ISOLVE_EQ )THEN
    check = .TRUE.
    DO j = 1,neq
      jj = IndexEq(j)
      IF( Cl(jj,ii) /= 0. .AND. jj /= ii )check = .FALSE.
      DO k = 1,neq
        kk = IndexEq(k)
        IF( Cq(jj,ii,kk) /= 0. .OR. Cq(jj,kk,ii) /= 0. )check = .FALSE.
      END DO
    END DO
    IF ( check )THEN
      chem%IndexEq(nsolve) = ii
      nsolve = nsolve - 1
      chem%TypeEq(ii) = ISUBS_EQ
      nsubs_eq = nsubs_eq + 1
    END IF
  END IF
END DO

CALL SetIndexEq( ID,IndexEq )

!---check for species that can be eliminated in favor of another

check = .TRUE.

DO WHILE( check )

  DO i = 1,nsolve
    item(i) = chem%IndexEq(i)
  END DO

  check = .FALSE.

  DO is = 1,nsolve

    i = item(is)
    nlin = 0

    DO js = 1,nsolve
      j = chem%IndexEq(js)
      IF( Cl(i,j) /= 0.0 .AND. i /= j )THEN
        nlin = nlin + 1
        elim = j
      END IF
    END DO

    IF( nlin == 1 .AND. nquad(i) == 0 .AND. nsolve > 1 )THEN
      chem%TypeEq(i)       = elim
      chem%IndexEq(nsolve) = i
      nsolve = nsolve - 1
      nlin_eq = nlin_eq + 1
      check = .TRUE.
      DO k = 1,neq
        kk = IndexEq(k)
        IF( Cl(kk,i) /= 0. )Cl(k,elim) = 1.
      END DO
    END IF

    CALL SetIndexEq( ID,IndexEq )

  END DO

END DO

chem%nSolveEq  = nsolve
chem%nLinearEq = nlin_eq
chem%nDirectEq = ndir_eq
chem%nSubstEq  = nsubs_eq

DO i = 1,neq
  chem%RowEq(chem%IndexEq(i)) = i
END DO

9999 CONTINUE

DEALLOCATE( Cq,Cl,IndexEq,item,nquad,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetIndexEq( ID,IndexEq )

USE chem_fi

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: ID
INTEGER, DIMENSION(*), INTENT( IN ) :: IndexEq

INTEGER is, i, ii

is = 0

DO i = 1,chem%nEquilibrium
  ii = IndexEq(i)
  IF( chem%TypeEq(ii) == ISOLVE_EQ )THEN
    is = is + 1
    chem%IndexEq(is) = ii
  END IF
END DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ClearChemMC()

USE chem_fi

!====   Deallocates multi-component structure

IMPLICIT NONE

INTEGER j

chem%nSpecies   = 0
chem%nreactions = 0
chem%nZenith    = 0
chem%pTypeN     = 0

NULLIFY( chem%IndexEq,chem%TypeEq,chem%RowEq )
NULLIFY( chem%species,chem%reaction,chem%zenith )
NULLIFY( chem%fast,chem%slow,chem%equil,chem%star )
NULLIFY( chem%gaseous,chem%particle,chem%pTypes,chem%pUnits )
NULLIFY( chem_aqaer%species )
NULLIFY( chem%fReact,chem%sReact,chem%eReact,chem%pReact )
NULLIFY( chem%Ambient%ID,chem%Ambient%read,chem%Ambient%zGrid )
NULLIFY( chem%Ambient%stepAmb )
NULLIFY( chem%Ambient%HrAmb )
NULLIFY( chem%sFlux%sflx )
NULLIFY( chem%sFlux%sID,chem%sFlux%sRead )
NULLIFY( chem%OutGroup )
DO j = 1,12
  NULLIFY( chem%MonthlyAmb(j)%ID )
  NULLIFY( chem%MonthlyAmb(j)%read )
  NULLIFY( chem%MonthlyAmb(j)%zGrid )
  NULLIFY( chem%MonthlyAmb(j)%stepAmb )
  NULLIFY( chem%MonthlyAmb(j)%HrAmb )
  NULLIFY( chem%MonthlyAmb(j)%ht )
  NULLIFY( chem%MonthlyAmb(j)%zMid )
  NULLIFY( chem%MonthlyAmb(j)%amb )
END DO
NULLIFY( chem%Ambient%ht,chem%Ambient%zMid,chem%Ambient%amb )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ExitChemMC()

USE chem_fi
USE scipuff_fi
USE files_fi
USE diagnostics_fi

!====   Deallocates multi-component structure

IMPLICIT NONE

INTEGER i, j, alloc_stat, ios
INTEGER k
LOGICAL opn

IF( ALLOCATED(chemMC) )THEN

  DO i = 1,SIZE(chemMC(:,1))

    DO k = 1,nthreads
    chem => chemMC(i,k)
    IF( ASSOCIATED(chem%reaction) .AND. k == 1 )THEN
      DO j = 1,chem%nReactions
        IF( ASSOCIATED(chem%reaction(j)%data) )DEALLOCATE( chem%reaction(j)%data,STAT=alloc_stat )
        IF( ASSOCIATED(chem%reaction(j)%P)    )DEALLOCATE( chem%reaction(j)%P   ,STAT=alloc_stat )
        IF( ASSOCIATED(chem%reaction(j)%fP)   )DEALLOCATE( chem%reaction(j)%fP  ,STAT=alloc_stat )
      END DO
      DEALLOCATE( chem%reaction,STAT=alloc_stat )
    END IF
    IF( k == 1 )THEN
    IF( ASSOCIATED(chem%species) )DEALLOCATE( chem%species ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%zenith)  )DEALLOCATE( chem%zenith  ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%IndexEq) )DEALLOCATE( chem%IndexEq ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%TypeEq)  )DEALLOCATE( chem%TypeEq  ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%RowEq)   )DEALLOCATE( chem%RowEq   ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%fast)    )DEALLOCATE( chem%fast    ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%slow)    )DEALLOCATE( chem%slow    ,STAT=alloc_stat )
    END IF
    IF( ASSOCIATED(chem_aqaer%species)   )DEALLOCATE( chem_aqaer%species,STAT=alloc_stat )
    IF( k == 1 )THEN
    IF( ASSOCIATED(chem%gaseous)         )DEALLOCATE( chem%gaseous      ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%particle)        )DEALLOCATE( chem%particle     ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%pTypes)          )DEALLOCATE( chem%pTypes       ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%pUnits)          )DEALLOCATE( chem%pUnits       ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%equil)           )DEALLOCATE( chem%equil   ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%fReact)          )DEALLOCATE( chem%fReact  ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%sReact)          )DEALLOCATE( chem%sReact  ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%eReact)          )DEALLOCATE( chem%eReact  ,STAT=alloc_stat )
    IF( ASSOCIATED(chem%pReact)          )DEALLOCATE( chem%pReact  ,STAT=alloc_stat )
    IF( chem%lAmbFile )THEN
      IF( ASSOCIATED(chem%Ambient%ID)    )DEALLOCATE( chem%Ambient%ID   ,STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%read)  )DEALLOCATE( chem%Ambient%read ,STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%zGrid) )DEALLOCATE( chem%Ambient%zGrid,STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%ht)    )DEALLOCATE( chem%Ambient%ht,   STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%zMid)  )DEALLOCATE( chem%Ambient%zMid, STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%amb)   )DEALLOCATE( chem%Ambient%amb,  STAT=alloc_stat )
      IF( ASSOCIATED(chem%Ambient%HrAmb) )DEALLOCATE( chem%Ambient%HrAmb,STAT=alloc_stat )
      IF( lMonthAmb )THEN
        DO j = 1,12
          IF( ASSOCIATED(chem%MonthlyAmb(j)%ID)    )DEALLOCATE( chem%MonthlyAmb(j)%ID   ,STAT=alloc_stat )
          IF( ASSOCIATED(chem%MonthlyAmb(j)%read)  )DEALLOCATE( chem%MonthlyAmb(j)%read ,STAT=alloc_stat )
          IF( ASSOCIATED(chem%MonthlyAmb(j)%zGrid) )DEALLOCATE( chem%MonthlyAmb(j)%zGrid,STAT=alloc_stat )
          IF( ASSOCIATED(chem%MonthlyAmb(j)%ht)    )DEALLOCATE( chem%MonthlyAmb(j)%ht,   STAT=alloc_stat )
          IF( ASSOCIATED(chem%MonthlyAmb(j)%zMid)  )DEALLOCATE( chem%MonthlyAmb(j)%zMid, STAT=alloc_stat )
          IF( ASSOCIATED(chem%MonthlyAmb(j)%amb)   )DEALLOCATE( chem%MonthlyAmb(j)%amb,  STAT=alloc_stat )
        END DO
      END IF
      INQUIRE( UNIT=chem%lunAmb,OPENED=opn )
      IF ( opn )CLOSE( chem%lunAmb,IOSTAT=ios )
    END IF
    IF( chem%lStepAmb )THEN
      IF( ASSOCIATED(chem%Ambient%stepAmb) )DEALLOCATE( chem%Ambient%stepAmb,STAT=alloc_stat )
      IF( chem%lSfcFlx )THEN
        IF( ASSOCIATED(chem%sFlux%sID)     )DEALLOCATE( chem%sFlux%sID  ,STAT=alloc_stat )
        IF( ASSOCIATED(chem%sFlux%sRead)   )DEALLOCATE( chem%sFlux%sRead,STAT=alloc_stat )
        IF( ASSOCIATED(chem%sFlux%sflx)    )DEALLOCATE( chem%sFlux%sflx ,STAT=alloc_stat )
        INQUIRE( UNIT=chem%lunSfcFlx,OPENED=opn )
        IF ( opn )CLOSE( chem%lunSfcFlx,IOSTAT=ios )
      END IF
      INQUIRE( UNIT=lun_amr,OPENED=opn )
      IF ( opn )CLOSE( lun_amr,IOSTAT=ios )
    END IF
    IF( ASSOCIATED(chem%OutGroup) )THEN
      DO j = 1,chem%nOutGroup
        IF( ASSOCIATED(chem%OutGroup(j)%iComp) )DEALLOCATE( chem%OutGroup(j)%iComp ,STAT=alloc_stat )
        IF( ASSOCIATED(chem%OutGroup(j)%fComp) )DEALLOCATE( chem%OutGroup(j)%fComp ,STAT=alloc_stat )
      END DO
      DEALLOCATE( chem%OutGroup,STAT=alloc_stat )
    END IF
    END IF
  END DO
  END DO

  DEALLOCATE( chemMC,STAT=alloc_stat )
END IF

IF( lDiagno )THEN
  IF( ALLOCATED(dgname)    )DEALLOCATE( dgname,STAT=alloc_stat )
  IF( ALLOCATED(emission)  )DEALLOCATE( emission,statics,boundary,STAT=alloc_stat )
  IF( ALLOCATED(transport) )DEALLOCATE( transport,ddeposition,wdeposition,chemistry,STAT=alloc_stat )
  IF( ALLOCATED(active)    )DEALLOCATE( active,removed,ppmfac,STAT=alloc_stat )
  INQUIRE( UNIT=lun_dgn,OPENED=opn )
  IF ( opn )CLOSE( lun_dgn,IOSTAT=ios )
END IF

IF( ALLOCATED(pratebl) )DEALLOCATE( pratebl,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE CopyChemMC( ID,ip )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT (IN ) :: ID
INTEGER, INTENT (IN ) :: ip

INTEGER i, j, alloc_stat

!--- Make a copy of the first entry in chemMC into the ip=th thread storage
!    First copy all fixed variables, then reallocate all thepointers and copy data

chemMC(ID,ip) = chemMC(ID,1)

chem => chemMC(ID,ip)

!---- Multicomponent species

NULLIFY( chem%species)
ALLOCATE( chem%species(chem%nspecies),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
nError   = RD_ERROR
eRoutine = 'CopyChemMC'
eMessage = 'Error reallocating species data'
GOTO 9999
END IF

DO i = 1,chem%nspecies
  chem%species(i) = chemMC(ID,1)%species(i)
END DO

!---- Pointers for reactions

NULLIFY( chem%reaction )

ALLOCATE( chem%reaction(chem%nReactions),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'CopyChemMC'
  eMessage = 'Error allocating multicomponent arrays'
  GOTO 9999
END IF

DO i = 1,chem%nReactions
  chem%reaction(i) = chemMC(ID,1)%reaction(i)
END DO

!---- Pointers for "star" species

NULLIFY( chem%star )

ALLOCATE( chem%star(chem%nStar),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'CopyChemMC'
  eMessage = 'Error allocating multicomponent arrays'
  GOTO 9999
END IF

j = 0
DO i = 1,chem%nSpecies
  IF( chem%species(i)%lstar )THEN
    j = j + 1
    chem%star(j)%s => chem%species(i)
  END IF
END DO

!---- Pointers for fast/slow species

NULLIFY( chem%fast )
NULLIFY( chem%slow )
NULLIFY( chem%particle )
NULLIFY( chem%equil )

NULLIFY( chem%fReact )
NULLIFY( chem%sReact )
NULLIFY( chem%eReact )
NULLIFY( chem%pReact )

NULLIFY( chem%fast )
NULLIFY( chem%slow )
NULLIFY( chem%gaseous )
NULLIFY( chem%particle )
NULLIFY( chem%equil )

CALL SetChemPointers()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE CopyChemRestartMC( ID,ip )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT (IN ) :: ID
INTEGER, INTENT (IN ) :: ip

INTEGER i, j, alloc_stat

!----- Copy restart data, initialized after read_prj

chem => chemMC(ID,ip)

!---- Equilibrium solver arrays

chem%nSolveEq   =  chemMC(ID,1)%nSolveEq      !No. of species for nonlinear equilibrium solver
chem%nLinearEq  =  chemMC(ID,1)%nLinearEq     !No. of species that can be eliminated
chem%nDirectEq  =  chemMC(ID,1)%nDirectEq     !No. of species that can be solved directly
chem%nSubstEq   =  chemMC(ID,1)%nSubstEq      !No. of species that can be substituted
chem%IndexEq    => chemMC(ID,1)%IndexEq        !List of equilibrium species in the order that they must be solved
chem%TypeEq     => chemMC(ID,1)%TypeEq         !List of solution types
chem%RowEq      => chemMC(ID,1)%RowEq          !List of row numbers

RETURN
END
