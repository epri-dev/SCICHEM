!=======================================================================
!    SCIPUFF data - chemistry multicomponent
!=======================================================================
MODULE default_aqaer_fd

  INTEGER, PARAMETER :: DEF_VAL_I  =  (2**16-1)
  INTEGER, PARAMETER :: NOT_SET_I  = -DEF_VAL_I
  INTEGER, PARAMETER :: DEFERRED_I =  DEF_VAL_I - 1

  REAL, PARAMETER :: DEF_VAL_R  =    1.0e+36
  REAL, PARAMETER :: NOT_SET_R  =   -DEF_VAL_R
  REAL, PARAMETER :: DEFERRED_R = 2.*DEF_VAL_R

  CHARACTER(64), PARAMETER :: NOT_SET_C = 'NOT SET'

END MODULE default_aqaer_fd

MODULE basic_aqaer_fi

  SAVE
  REAL  rhoair ,rmuair ,rnu

END MODULE basic_aqaer_fi

MODULE basic_aqaer_fd

!==== Basic WINDOWS parameters

  INTEGER, PARAMETER :: TRUE  = 1
  INTEGER, PARAMETER :: FALSE = 0
  INTEGER, PARAMETER :: NULL  = 0

END MODULE basic_aqaer_fd

MODULE error_aqaer_fi

  INTEGER, PARAMETER :: EOF_ERROR =  -1  !EOF Error
  INTEGER, PARAMETER :: NO_ERROR  =   0  !No error
  INTEGER, PARAMETER :: NF_ERROR  =   1  !Not found error
  INTEGER, PARAMETER :: SZ_ERROR  =   2  !Size error
  INTEGER, PARAMETER :: IV_ERROR  =   3  !Invalid value error
  INTEGER, PARAMETER :: OP_ERROR  =   4  !Open error
  INTEGER, PARAMETER :: RD_ERROR  =   5  !Read error
  INTEGER, PARAMETER :: WR_ERROR  =   7  !Write error
  INTEGER, PARAMETER :: WN_ERROR  =  10  !Warning error
  INTEGER, PARAMETER :: AB_ERROR  =  11  !Abort error
  INTEGER, PARAMETER :: VN_ERROR  =  12  !Version error
  INTEGER, PARAMETER :: UK_ERROR  =  99  !Unknown error

  INTEGER, PARAMETER :: AqAerfailure    = -1
  INTEGER, PARAMETER :: AqAerSuccess     = 1
  INTEGER, PARAMETER :: AqAeraffirmative = AqAerSuccess

  SAVE

  TYPE AqAerError

    SEQUENCE

    INTEGER        :: nError
    CHARACTER(80)  :: eRoutine
    CHARACTER(128) :: eMessage
    CHARACTER(128) :: eInform
    CHARACTER(128) :: eAction

  END TYPE AqAerError

  TYPE( AqAerError ) :: error

END MODULE error_aqaer_fi

MODULE message_aqaer_fd

!==== messageT ================================================================

  TYPE  messageT
    SEQUENCE
    INTEGER        iParm
    INTEGER        jParm
    CHARACTER(128) aString
    CHARACTER(128) bString
    CHARACTER(128) cString
    CHARACTER(80)  routine
  END TYPE  messageT

END MODULE message_aqaer_fd

MODULE files_aqaer_fi

  SAVE

! File names
  INTEGER, PARAMETER :: PATH_MAXLENGTH = 128 !Maximum no of characters in a pathname

  CHARACTER(PATH_MAXLENGTH) file_log   ! log output
  INTEGER,PARAMETER ::  lun_log = 401

  LOGICAL isLogOpen

END MODULE files_aqaer_fi

MODULE precip_aqaer_fd

  INTEGER, PARAMETER :: NRAIN    =           3 !Maximum Number of Rain groups
  INTEGER, PARAMETER :: NSNOW    =           3 !Maximum Number of Snow groups
  INTEGER, PARAMETER :: NWASH    = NRAIN+NSNOW !Maximum Number of Precip. groups

END MODULE precip_aqaer_fd

MODULE multcomp_aqaer_fd

  INTEGER, PARAMETER :: UNIT_PPM      = 0 !PPM concentration units
  INTEGER, PARAMETER :: UNIT_MOLECULE = 1 !molecule/cm3 concentration units

  TYPE ChemSpecies_aqaer_str

    SEQUENCE
    CHARACTER(16) :: name
    INTEGER       :: class
    REAL          :: taudry
    REAL          :: tauwet
    !---- Working storage
    REAL    conc
    REAL    amb
    REAL    csav

  END TYPE ChemSpecies_aqaer_str

  TYPE ChemMC_aqaer_str

    SEQUENCE
    INTEGER  cUnits       !Concentration units type
     !---- Multicomponent species
    INTEGER  nSpecies     !Total No. of component species
    CHARACTER(128) prjName
    TYPE( ChemSpecies_aqaer_str ), DIMENSION(:), POINTER :: species

  END TYPE ChemMC_aqaer_str

END MODULE multcomp_aqaer_fd

MODULE chem_aqaer_fi

  USE multcomp_aqaer_fd

  SAVE

  !TYPE( ChemMC_aqaer_str ), TARGET :: chem_aqaer

  INTEGER nspecies
  INTEGER ID_SPECIES_PARTICLE  !Aerosol particle species class identifier

  LOGICAL laerosol, laqueous

  INTEGER :: IDSO4J = -1, IDSO4I = -1, IDNO3J = -1, IDNO3I = -1, IDNH4J = -1, &
             IDNH4I = -1,  IDNAJ = -1,  IDNAI = -1,  IDCLJ = -1,  IDCLI = -1, &
             IDHNO3 = -1,  IDNH3 = -1,  IDHCL = -1

END MODULE chem_aqaer_fi

MODULE AqAer_fi

  ! Common variables required for AqAer routines
  USE default_aqaer_fd
  USE basic_aqaer_fi
  USE files_aqaer_fi
  USE error_aqaer_fi
  USE precip_aqaer_fd
  USE chem_aqaer_fi
  USE aero_species_inc
  USE aer_sections_inc

  SAVE

  INTEGER, PARAMETER         :: MAX_SP   = 250  ! Maximum no. of components
  INTEGER, PARAMETER         :: MAX_G2AE = 100  ! Maximum no. gas species needed for aerosol module
  INTEGER, PARAMETER         :: MAX_G2AQ = 100  ! Maximum no. gas species needed for aqueous module

  INTEGER                    :: nsp           ! Number of total species
  INTEGER                    :: naero         ! Number of required species for aerosol chem
  INTEGER                    :: naerp         ! Number of aerosol particle species
  INTEGER                    :: naqueous      ! Number of required species for aqueous chem
  INTEGER                    :: n_g2ae_spc    ! Number of gas-phase species required for aerosol chem
  INTEGER                    :: n_g2aq_spc    ! Number of gas-phase species required for aqueous chem
  INTEGER                    :: nsec_aer      ! Number of aerosol size sections
  INTEGER                    :: ic_units      ! Gas concentration units

  INTEGER, DIMENSION(MAX_SP) :: index_aero    !pointers
  INTEGER, DIMENSION(MAX_SP) :: index_aqueous !pointers to aqueous species


  REAL                       :: hb, tk, patm  ! Humidity, temperature and pressure
  REAL                       :: lwc, prate    ! Liquid water content and precip. rate
  REAL                       :: fcc           ! Fractional cloud cover (convective)
  REAL                       :: fpc           ! Fraction of precip. that is convective
  REAL                       :: radyn         ! Aerodynamic resistance (s/m)
  REAL                       :: us2, ws2      ! u*-squared and w*-squared
  REAL                       :: rho_aer

  REAL, DIMENSION(MAX_SP)    :: diff_aer, vs_aer  !
  REAL, DIMENSION(MAX_SP)    :: dm_aer        ! diameter of aerosol size sections
  REAL, DIMENSION(MAX_SP)    :: vdry

  REAL, DIMENSION(0:NWASH,MAX_SEC) :: twash_aer
  REAL, DIMENSION(MAX_SP)    :: scavcoef      ! scavenging coefficients
  REAL, DIMENSION(MAX_SEC+1) :: secbnds_aer

  CHARACTER*128                   :: file_imc
  CHARACTER*16, DIMENSION(MAX_SP) :: name

  ! Names of aero and aqueous species (built from namelist file)
  CHARACTER*16, DIMENSION(MAX_SP) :: aero_names, aqueous_names

  INTEGER, DIMENSION(N_AE_SPCD)   :: cblk_map

  TYPE ChemMet_str

    SEQUENCE

    REAL tab
    REAL pb
    REAL hb
    REAL cldall
    REAL cldbot
    REAL cldtop
    REAL cldavg
    REAL pratepuf
    REAL fcc
    REAL fprcpc
    REAL xml
    REAL zb1
    REAL zruf
    REAL us2
    REAL ws2

  END TYPE

END MODULE AqAer_fi

!Aerosol Aqueous interface functions
!===================
! Public functions
!===================
!------------------------------------------------------------------------------
SUBROUTINE ReadAqAerEx( AqAerInp )
!!DEC$ ATTRIBUTES DLLEXPORT :: ReadAqAerEx

! Called from InitChemMC and RdAqAerSub

USE mpi_fi, ONLY: AqAerInpT

IMPLICIT NONE

TYPE( AqAerInpT ) :: AqAerInp

INTEGER ios

CALL ReadAqAer( AqAerInp%prjName,AqAerInp%mcFile,AqAerInp%nError )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitAerAqEx( ID, chem_aqaer, particleID, nError )
!!DEC$ ATTRIBUTES DLLEXPORT :: InitAerAqEx

! Called from InitChemMC,read_prj and InitChemSubMPI

USE AqAer_fi

IMPLICIT NONE

INTEGER,                  INTENT( IN    ) :: ID, particleID
TYPE( ChemMC_aqaer_str ), INTENT( INOUT ) :: chem_aqaer
INTEGER,                  INTENT( INOUT ) :: nError

CALL InitAerAq( chem_aqaer%prjName, ID, chem_aqaer, particleID, nError )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE StepAerAqEx( dt,nSpc,carea,lscale,chem_aqaer,chemMet,nError )

! Called from StepChem and stepChemAmb

USE AqAer_fi

IMPLICIT NONE

! Return updated aqueous and aerosol concentrations.
! Set chemSpc%species(i)%tauwet and chemSpc%species(i)%taudry

INTEGER                 , INTENT( IN    ) :: nSpc
REAL                    , INTENT( IN    ) :: dt
REAL                    , INTENT( IN    ) :: carea
REAL                    , INTENT( IN    ) :: lscale
TYPE( ChemMC_aqaer_str ), INTENT( INOUT ) :: chem_aqaer
TYPE( ChemMet_str      ), INTENT( IN    ) :: chemMet
INTEGER                 , INTENT( INOUT ) :: nError

CALL StepAerAq( dt,nSpc,carea,lscale,chem_aqaer,chemMet,nError )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE AqAerReportErrorEx( errorReport )
!!DEC$ ATTRIBUTES DLLEXPORT :: AqAerReportErrorEx

USE AqAer_fi
USE message_aqaer_fd

IMPLICIT NONE

TYPE( messageT ), INTENT( OUT ) :: errorReport

errorReport%iParm   = error%nError
errorReport%routine = TRIM(error%eRoutine)
errorReport%aString = TRIM(error%eMessage)
errorReport%bString = TRIM(error%eInform)
errorReport%cString = TRIM(error%eAction)

RETURN
END

!===================
! Private functions
!===================
!------------------------------------------------------------------------------
SUBROUTINE ReadAqAer( prjName,ImcFile,nError )
USE AqAer_fi

!====   Reads multi-component definitions from project.imc

IMPLICIT NONE

CHARACTER(*), INTENT( IN    ):: prjName     ! project name
CHARACTER(*), INTENT( IN    ):: ImcFile     ! imc file name
INTEGER,      INTENT( INOUT ):: nError

CHARACTER(1), PARAMETER   :: SET_MODE    = '#'
CHARACTER(1), PARAMETER   :: AQAER_MODE  = 'Q'

INTEGER lun
INTEGER ios

CHARACTER(200) line
CHARACTER(PATH_MAXLENGTH) pName

LOGICAL isOpen

CALL AqAerClearError( )

pName = prjName(1:PATH_MAXLENGTH-10)

  file_log = TRIM(pName)//'.qlog'

OPEN( lun_log,FILE=file_log,IOSTAT=ios )

INQUIRE(UNIT=lun_log,OPENED=isLogOpen )

!-- Initialize to false in case section is not present

laerosol       = .FALSE.
nsec_aer       = 1
secbnds_aer(1) = 0.4e-6
secbnds_aer(2) = 0.4e-6
rho_aer        = 1000.

laqueous       = .FALSE.
n_g2aq_spc     = 56

INQUIRE( FILE=ImcFile,OPENED=isOpen,NUMBER=lun )

IF( isOpen )THEN
  REWIND( UNIT=lun,IOSTAT=ios )
ELSE
  lun = 20
  OPEN( UNIT=lun,FILE=ImcFile,IOSTAT=ios )

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    error%nError   = OP_ERROR
    error%eRoutine = 'ReadAqAer'
    error%eMessage = 'Error reading Multi-component input file'
    WRITE(error%eInform,'("File = ",A)')TRIM(ImcFile)
    GOTO 9999
  END IF
END IF
file_imc = ImcFile

! Read the aerosol, aqueous section of the imc file for variables
! such as nsec_aer, secbnds_aer, rho_aer, cl2 and br concentration profiles

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    error%nError   = RD_ERROR
    error%eRoutine = 'ReadAqAer'
    error%eMessage = 'Error reading Multi-component input file'
    WRITE(error%eInform,'("File = ",A)')ImcFile
    GOTO 9999
  END IF

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )THEN
    IF( line(2:2) == AQAER_MODE )THEN
      CALL ReadAqAerNml( lun )
      IF( error%nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  END IF

END DO

9999 CONTINUE
IF( .NOT. isOpen )CLOSE( UNIT=lun,IOSTAT=ios )

IF( error%nError /= NO_ERROR )CALL AqAerPostError( )
nError = error%nError

RETURN
END

!------------------------------------------------------------------------------
SUBROUTINE ReadAqAerNml( lun )

USE AqAer_fi

!====   Reads aqueous aerosol definitions from project.imc

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

LOGICAL       aqueous            !flag for aqueous chemistry
LOGICAL       aerosol            !flag for aerosol thermodynamics

INTEGER ios

CHARACTER*128 cl2_file           !File containing Cl2 and HCl concs (for Hg chemistry)

! Names of gas-phase species in aerosol and aqueous chemistry
CHARACTER*16, DIMENSION(MAX_G2AE) :: g2ae_names
CHARACTER*16, DIMENSION(MAX_G2AQ) :: g2aq_names

! Names of particle phase species
CHARACTER*16, DIMENSION(MAX_SP) :: aerp_names

integer nerrs, i

NAMELIST / aqaer /  aqueous, aerosol, secbnds_aer, rho_aer, nsec_aer, &
                    cl2_file
NAMELIST / aerspec   / aerp_names
NAMELIST / g2ae_spec / g2ae_names

NAMELIST / naqsspec  / n_g2aq_spc
NAMELIST / g2aq_spec / g2aq_names

#if defined(LINUX)
INTEGER, EXTERNAL  :: RemoveCF
#endif

!===== Set default values

cl2_file       = NOT_SET_C

laerosol       = .FALSE.
aerosol        = .FALSE.
nsec_aer       = 1
secbnds_aer(1) = 0.4e-6
secbnds_aer(2) = 0.4e-6
rho_aer        = 1000.
aerp_names     = NOT_SET_C
g2ae_names     = NOT_SET_C
aero_names     = NOT_SET_C

laqueous       = .FALSE.
aqueous        = .FALSE.
n_g2aq_spc     = 56
aqueous_names  = NOT_SET_C
g2aq_names     = NOT_SET_C

error%eRoutine = 'ReadAqAerNml'
error%nError   = NO_ERROR

READ(lun,aqaer,IOSTAT=ios)
IF( ios > 0 )THEN
  error%nError   = RD_ERROR
  error%eMessage = 'Error reading multicomponent CONTROL namelist'
  GOTO 9999
END IF

laqueous  =  aqueous
laerosol  =  aerosol

! if aqueous is selected, then aerosol needs to be selected too.
IF( laqueous )THEN
  IF( .NOT. laerosol )THEN
    error%nError   = IV_ERROR
    error%eRoutine = 'ReadAqAerNml'
    error%eMessage = 'Aqueous chemistry requires aerosol module'
    WRITE(error%eAction,*)'Set aerosol to true in imc file'
    GO TO 9999
  END IF
END IF

IF (.NOT. laerosol) RETURN

naerp      = N_AE_SPC
n_g2ae_spc = NINORG + NPROD + NVAP + NAMINE
naero      = naerp + n_g2ae_spc

!debug
IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'naerp, naero: ',naerp, naero
!debug

IF (naero > MAX_SP) THEN
  error%nError   = RD_ERROR
  error%eRoutine = 'ReadAqAerNml'
  error%eMessage = 'No. of aerosol species > MAX_SP'
  GOTO 9999
END IF

READ(lun,aerspec,IOSTAT=ios)
IF( ios > 0 )THEN
  error%nError   = RD_ERROR
  error%eRoutine = 'ReadAqAerNml'
  error%eMessage = 'Error reading aerspec namelist'
  GOTO 9999
END IF

! Check that all particle species are named
nerrs = 0
DO i = 1,naerp
  IF(aerp_names(i) == NOT_SET_C)THEN
    IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'Error in aerp_names namelist for species ',i
    nerrs = nerrs + 1
    CYCLE
  END IF
!debug
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'i, aerp_name: ',i,aerp_names(i)
!debug
END DO

IF(nerrs > 0)THEN
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'terminating because of errors'
  error%nError   = RD_ERROR
  error%eRoutine = 'ReadAqAerNml'
  WRITE(error%eMessage,'(A,I3,A)')'Error in aerp_names namelist for ',nerrs, &
                            ' species'
  GOTO 9999
END IF

! Make sure there are no extra particle species in namelist
IF(aerp_names(naerp+1) /= NOT_SET_C)THEN
  error%nError   = RD_ERROR
  error%eRoutine = 'ReadAqAerNml'
  error%eMessage = 'More species than expected in aerp_names namelist'
!debug
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'i, aerp_name: ',naerp+1,aerp_names(naerp+1)
!debug
  GOTO 9999
END IF

aero_names(1:naerp) = aerp_names(1:naerp)

READ(lun,g2ae_spec,IOSTAT=ios)
IF( ios > 0 )THEN
  error%nError   = RD_ERROR
  error%eMessage = 'Error reading g2ae_spec namelist'
  GOTO 9999
END IF

! Check that all gas species for aerosol module are named
nerrs = 0
DO i = 1,n_g2ae_spc
  IF(g2ae_names(i) == NOT_SET_C)THEN
    error%nError = RD_ERROR
    WRITE(error%eMessage,*)'Error in g2ae_names namelist for species ',i
    GOTO 9999
  END IF
!debug
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'i, g2ae_name: ',i,g2ae_names(i)
!debug
END DO

! Make sure there are no extra particle species in namelist
IF(g2ae_names(n_g2ae_spc+1) /= NOT_SET_C)THEN
  error%nError   = RD_ERROR
  error%eRoutine = 'ReadAqAerNml'
  error%eMessage = 'More species than expected in g2ae_names namelist'
!debug
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'i, g2ae_name: ',n_g2ae_spc+1,g2ae_names(n_g2ae_spc+1)
!debug
  GOTO 9999
END IF

aero_names(naerp+1:naero) = g2ae_names(1:n_g2ae_spc)

IF (.NOT. laqueous) RETURN

READ(lun,naqsspec,IOSTAT=ios)
IF( ios > 0 )THEN
  error%nError   = RD_ERROR
  error%eMessage = 'Error reading naqsspec namelist'
  GOTO 9999
END IF

naqueous = naerp + n_g2aq_spc
aqueous_names(1:naerp) = aero_names(1:naerp)

!debug
IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'naqueous: ',naqueous
!debug

READ(lun,g2aq_spec,IOSTAT=ios)
IF( ios > 0 )THEN
  error%nError   = RD_ERROR
  error%eMessage = 'Error reading g2aq_spec namelist'
  GOTO 9999
END IF

aqueous_names(naerp+1:naqueous) = g2aq_names(1:n_g2aq_spc)

! Check that all aqueous species are named
! Only need to check from naerp+1, since aqueous_names(1:naerp) is set
nerrs = 0
DO i = naerp+1,naqueous
  IF(aqueous_names(i) == NOT_SET_C)THEN
    error%nError   = RD_ERROR
    WRITE(error%eMessage,'(A,I3)')'Error in aqueous_names namelist for species ',i
    nerrs = nerrs + 1
  END IF
!debug
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'i, aqueous_name: ',i,aqueous_names(i)
!debug
END DO

IF(nerrs > 0)THEN
  IF( isLogOpen )write(lun_log,*,IOSTAT=ios)'terminating because of errors'
  error%nError   = RD_ERROR
  WRITE(error%eMessage,'(A,I3,A)')'Error in aqueous_names namelist for ',nerrs, &
                            ' species'
  GOTO 9999
END IF

error%eRoutine = ''

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
SUBROUTINE InitAerAq( prjName, ID, chem_aqaer, particleID, nError )
! Sets Aerosol and Aqueous working variables and arrays such as
! index_aero, index_aerp, index_sec and
! index_aqueous, naqueous

! Also calls set_vdep_aer to set partmass, partarea, vs_aer, diff_aer

USE AqAer_fi

IMPLICIT NONE

CHARACTER(*),             INTENT( IN    ) :: prjName       ! project name
INTEGER,                  INTENT( IN    ) :: ID, particleID
TYPE( ChemMC_aqaer_str ), INTENT( INOUT ) :: chem_aqaer
INTEGER,                  INTENT( INOUT ) :: nError

INTEGER i,j,isp,ie,ios
LOGICAL lflag, NewSpecies
CHARACTER(PATH_MAXLENGTH) pName

INTEGER, EXTERNAL :: AqAerWarningMessage

pName = prjName(1:PATH_MAXLENGTH-10)
  file_log = TRIM(pName)//'.qlog'

INQUIRE(UNIT=lun_log,OPENED=isLogOpen,IOSTAT=ios )

IF( .NOT.isLogOpen )THEN
  OPEN( lun_log, FILE=file_log, IOSTAT=ios )
  IF( ios /= 0 )THEN
    error%nError = IV_ERROR
    error%eRoutine='InitAerAq'
    error%eMessage='Cannot open log file'
    WRITE(error%eInform,*)'Log file = ',TRIM(file_log)
    GO TO 9999
  END IF
  isLogOpen = .TRUE.
END IF

! From init_param
rmuair  = 1.6E-5
rhoair  = 1.2
rnu     = rmuair/rhoair

ID_SPECIES_PARTICLE = particleID
nsp = chem_aqaer%nSpecies

DO i = 1,chem_aqaer%nSpecies
  name(i) = chem_aqaer%species(i)%name
END DO

IF( .NOT. laerosol )THEN
  CALL set_vdep_aer()
  CALL init_wash_aerosol_chem()
END IF

NewSpecies = .FALSE.

IF( laerosol )THEN

  IF( isLogOpen )WRITE(lun_log,'("AEROSOL CALCULATIONS TURNED ON (MODULE 1.0)")',IOSTAT=ios)

  CALL init_aero(lun_log)
  IF( error%nError /= NO_ERROR )GOTO 9999

  DO i = 1,naero
    j = index_aero(i)
    CALL SetStar( ID,j,NewSpecies )
  END DO

END IF

IF( laqueous )THEN

  IF( isLogOpen )WRITE(lun_log, '("AQUEOUS CHEMISTRY TURNED ON (MODULE 1.0)")',IOSTAT=ios)

  CALL init_aqueous(lun_log)
  IF( error%nError /= NO_ERROR )GOTO 9999

  DO i = naerp + 1, naqueous
    j = index_aqueous(i)
    CALL SetStar( ID,j,NewSpecies ) !use concentrations to step
  END DO

END IF

IF( NewSpecies )CALL ReAllocStarSpecies( ID,NewSpecies )

9999 CONTINUE

IF( error%nError /= NO_ERROR )CALL AqAerPostError( )
nError = error%nError

RETURN
END

!------------------------------------------------------------------------------
SUBROUTINE set_vdep_aer( )
!*******************************************************************************
!
! FUNCTION:  Set the deposition velocity parameters for the aerosol particles
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                   ufall
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

USE AqAer_fi

IMPLICIT NONE

! --- LOCALS

INTEGER j, isec

REAL rp(13)

REAL dbp(13)
REAL dlb, dub
REAL vmin, vbar, vmax, fmin, fbar, fmax, fsum
REAL db_dust, drate, dbpx, ufall

LOGICAL off_end, less_than

REAL pvol, radius

rp(1)  =  0.0005e-6
rp(2)  =  0.0010e-6
rp(3)  =  0.0025e-6
rp(4)  =  0.0050e-6
rp(5)  =  0.0100e-6
rp(6)  =  0.0250e-6
rp(7)  =  0.0500e-6
rp(8)  =  0.2500e-6
rp(9)  =  0.5000e-6
rp(10) =  2.5000e-6
rp(11) =  5.0000e-6
rp(12) = 25.0000e-6
rp(13) = 50.0000e-6

dbp(1)  = 5.1e-6
dbp(2)  = 1.3e-6
dbp(3)  = 2.1e-7
dbp(4)  = 5.2e-8
dbp(5)  = 1.3e-8
dbp(6)  = 2.4e-9
dbp(7)  = 6.7e-10
dbp(8)  = 6.3e-11
dbp(9)  = 2.8e-11
dbp(10) = 4.9e-12
dbp(11) = 2.4e-12
dbp(12) = 1.e-20
dbp(13) = 1.e-20

DO isec = 1, nsec_aer

  dub = secbnds_aer(isec+1)
  dlb = secbnds_aer(isec)

  dm_aer(isec)   = SQRT(dub*dlb)
  radius         = 0.5 * dm_aer(isec)

  vmin = ufall( rhoair,rho_aer,rmuair,dlb )
  vbar = ufall( rhoair,rho_aer,rmuair,dm_aer(isec) )
  vmax = ufall( rhoair,rho_aer,rmuair,dub )
  fmin = 1.0/dlb
  fbar = 1.0/dm_aer(isec)
  fmax = 1.0/dub
  fsum = fmin + fbar + fmax
  vs_aer(isec) = (fmin*vmin    + fbar*vbar    + fmax*vmax)/fsum

  off_end   = .false.
  less_than = .true.
  j = 2
  DO WHILE( less_than )
   IF( 2.0*rp(j) >= dm_aer(isec) )THEN
      less_than = .false.
    ELSE
      j = j + 1
      IF( j > 13 )THEN
        off_end = .true.
        less_than = .false.
      END IF
    END IF
  END DO

  IF( off_end )THEN
    db_dust = 1.e-20
  ELSE
    drate = (0.5*dm_aer(isec)-rp(j-1))/(rp(j)-rp(j-1))
    dbpx = ALOG(dbp(j-1)) + drate*(ALOG(dbp(j))-ALOG(dbp(j-1)))
    dbpx = EXP(dbpx)
    db_dust = MIN(dbpx,rnu)
  END IF
  diff_aer(isec) = db_dust
  IF( isLogOpen )WRITE(lun_log,*,err=9998)'Aerosol (diam,rho,diff,vs):',isec, &
            dm_aer(isec),rho_aer,diff_aer(isec),vs_aer(isec)

END DO

9999    CONTINUE

IF( error%nError /= NO_ERROR )CALL AqAerPostError( )

RETURN

!------ set log write error and go to return

9998    CONTINUE
error%nError   = WR_ERROR
error%eRoutine = 'set_vdep_aer'
error%eMessage = 'Error writing SCIPUFF log file'
error%eInform  = 'File='//TRIM(file_log)
GO TO 9999

END

!------------------------------------------------------------------------------
SUBROUTINE init_wash_aerosol_chem()
!*******************************************************************************
!
! FUNCTION:  Initialize washout for the aerosol particles
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!          init_wash_chem                   ufall
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

USE AqAer_fi
USE MPI_fi, ONLY:myid

IMPLICIT NONE


! --- PARAMETERS

REAL, PARAMETER :: A_RAIN = 7.317e-04
REAL, PARAMETER :: B_RAIN = 0.21
REAL, PARAMETER :: A_SNOW = 5.215e-04
REAL, PARAMETER :: B_SNOW = 0.25
REAL, PARAMETER :: RHOW   = 1.00e+03
REAL, PARAMETER :: V_SNOW = 1.1

! --- LOCALS

INTEGER i, isec, ios

CHARACTER*7, DIMENSION(NWASH) :: nametmp
REAL       , DIMENSION(NWASH) :: pr, dr, washtmp

REAL, EXTERNAL :: ufall

LOGICAL isOpen

!-----  Set Rain Precipitation Groups (mm/hr)
!-----  RAIN

pr(1)  =  0.5
pr(2)  =  3.5
pr(3)  = 25.0

!-----  SNOW

pr(4)  =   5.0
pr(5)  =  20.0
pr(6)  = 100.0

nametmp(1) = 'LGTRAIN'
nametmp(2) = 'MODRAIN'
nametmp(3) = 'HVYRAIN'
nametmp(4) = 'LGTSNOW'
nametmp(5) = 'MODSNOW'
nametmp(6) = 'HVYSNOW'

!-----  Set Precipitation fall velocity (m/s)

DO isec = 1, nsec_aer
  twash_aer(0,isec) =  0.0  !No precipitation
END DO

DO i = 1,NRAIN
  dr(i)      =  A_RAIN*pr(i)**B_RAIN
  washtmp(i) =  ufall(rhoair,RHOW,rmuair,dr(i))
END DO

DO i = NRAIN+1,NWASH
  dr(i)      =  A_SNOW*pr(i)**B_SNOW
  washtmp(i) =  V_SNOW
END DO

!-----  Calculate Scavenging Coefficients (s)
INQUIRE(UNIT=lun_log,OPENED=isLogOpen )

IF( isLogOpen )WRITE(lun_log,*,IOSTAT=ios) 'Aerosol wash-out timescale (s):'
DO isec = 1, nsec_aer
  IF( isLogOpen )WRITE(lun_log,*,IOSTAT=ios) 'Size group: ',dm_aer(isec)
  DO i = 1,NWASH
    CALL init_wash_chem(i,isec,pr(i),dr(i),washtmp(i),twash_aer(i,isec))
    IF( isLogOpen )WRITE(lun_log,500) nametmp(i),twash_aer(i,isec)
  END DO
END DO
500   FORMAT(a7,' = ',1p,e12.4)

RETURN
END

!------------------------------------------------------------------------------
SUBROUTINE init_wash_chem(ipr,isec,pr,dr,vr,tauwo)
!*******************************************************************************
!
! FUNCTION: Initialize washout for an individual aerosol particle
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

USE AqAer_fi

IMPLICIT NONE

! --- ARGUMENTS

INTEGER ipr, isec
REAL pr, dr, vr, tauwo

! --- PARAMETERS
REAL, PARAMETER :: G0      = 9.81              !m/s/s (from constants_fd)
REAL, PARAMETER :: MUW = 1.00e-03
REAL, PARAMETER :: CNVFAC = 4.167e-07

! --- LOCALS

REAL sc3, tau
REAL re, re2, sc, st, sts, wi, h, e, e1, e2, e3, sc2


!-----  Calculate dimensionless groups

tau  = vs_aer(isec)/G0
re   = (0.5*dr*vr*rhoair)/rmuair
sc   = rmuair/(rhoair*diff_aer(isec))
st   = 2.*tau*(vr - vs_aer(isec))/dr
IF( ipr <= NRAIN )THEN
  wi = rmuair/MUW                               !RAIN
ELSE
  wi = 0.                                       !SNOW
END IF
h    = dm_aer(isec)/dr

re2  = SQRT(re)
sc2  = SQRT(sc)
sc3  = sc**(1./3.)
sts  = 0.0

e1   = 4.*h*(wi + (1.+2.*re2)*h)
e2   = ((st-sts)/(st-sts+0.666667))**1.5
e3   = 4.*(1. + 0.4*re2*sc3 + 0.16*re2*sc2)/(re*sc)

e    = (e1 + e2 + e3)/3.0

tauwo= CNVFAC*pr*e/dr

RETURN
END

!------------------------------------------------------------------------------
SUBROUTINE StepAerAq( dt,nSpc,carea,lscale,chem_aqaer,chemMet,nError )
! Called from step_mc (StepChem)

USE AqAer_fi

IMPLICIT NONE

! Return updated aqueous and aerosol concentrations.
! Set chemSpc%species(i)%tauwet and chemSpc%species(i)%taudry

INTEGER                 , INTENT( IN    ) :: nSpc
REAL                    , INTENT( IN    ) :: dt
REAL                    , INTENT( IN    ) :: carea
REAL                    , INTENT( IN    ) :: lscale
TYPE( ChemMC_aqaer_str ), INTENT( INOUT ) :: chem_aqaer
TYPE( ChemMet_str      ), INTENT( IN    ) :: chemMet
INTEGER                 , INTENT( INOUT ) :: nError

INTEGER i, j, isec, ii, ios
REAL    xml, zb1, zruf

REAL, DIMENSION(MAX_SP) :: conc     ! array of species puff concentrations
REAL, DIMENSION(MAX_SP) :: concsav  ! array of saved species concentrations
REAL, DIMENSION(MAX_SP) :: concamb  ! array of species ambient concentrations
REAL, DIMENSION(MAX_SP) :: conctot  ! array of species total concentrations

REAL dtaero
!====   Aerosol and aqueous-phase chemistry calculations

IF( laerosol )THEN

  ic_units = chem_aqaer%cUnits    ! concentration units

  ! --  units: dt(sec),conc(ppm or mol/cc)

  tk     = chemMet%tab       ! temp(k)
  patm   = chemMet%pb        ! press(atm)
  hb     = chemMet%hb        ! humidity(g H2O/g dry air)
  lwc    = chemMet%cldall    ! cloud liquid water content(g/m3)
  prate  = chemMet%pratepuf  ! precip rate (mm/hr)
  fcc    = chemMet%fcc
  fpc    = chemMet%fprcpc
  xml    = chemMet%xml
  zb1    = chemMet%zb1
  zruf   = chemMet%zruf
  us2    = chemMet%us2
  ws2    = chemMet%ws2

  CALL get_radyn(SQRT(us2), xml, zb1, zruf, radyn)

  !--- First step aqueous-phase chemistry

  IF( laqueous )THEN

    IF( chemMet%cldall > 0. )THEN

      concamb = 0.
      conc = 0.
      scavcoef = 0.
      !-- Load aqueous working array
      DO i = 1, naqueous
        j = index_aqueous(i)
        concamb(i) = MAX( 0.,chem_aqaer%species(j)%amb )
        conc(i)    = MAX( -concamb(i),chem_aqaer%species(j)%conc )
        concsav(i) = conc(i) + concamb(i) !save to report errors
      END DO

      ! -- Call with ambient and plume concentrations
      CALL step_aqueous(dt,concamb,conc,lscale,scavcoef)
      IF( error%nError /= NO_ERROR )THEN
        IF( isLogOpen )THEN
          WRITE(lun_log,*,IOSTAT=ios)'Aqueous module called with the following'
          WRITE(lun_log,*,IOSTAT=ios)'(solving for the plume concentrations)'
          WRITE(lun_log,*,IOSTAT=ios)'dt(s), clouds (g/m3), precip(mm/hr):'
          WRITE(lun_log,*,IOSTAT=ios) dt,lwc,prate
          WRITE(lun_log,*,IOSTAT=ios)'P (atm), T(K):'
          WRITE(lun_log,*,IOSTAT=ios) patm,tk
          WRITE(lun_log,*,IOSTAT=ios)'Conc (ppm or mol/cc for gas, ug/m3 for aerosols):'
          DO i = 1, naqueous
            j = index_aqueous(i)
            WRITE(lun_log,*,IOSTAT=ios) chem_aqaer%species(j)%name,concsav(i)
          END DO
        END IF
        GO TO 9998
      END IF

      !unload aqueous working arrays
      DO i = 1,naqueous
        j                            = index_aqueous(i)
        chem_aqaer%species(j)%conc   = MAX( conc(i), -chem_aqaer%species(j)%amb )
        chem_aqaer%species(j)%tauwet = scavcoef(i)
      END DO

    ELSE

      DO i = 1, naqueous
        j = index_aqueous(i)
        chem_aqaer%species(j)%tauwet = 0.
      END DO

    END IF  ! if cldall > 0.

  END IF  ! if laqueous > 0.

! -- Aerosol calculations

  !--- Load aerosol working array
  concamb = 0.
  conc    = 0.
  DO i = 1, naero
    j = index_aero(i)
    concamb(i) = MAX( 0.,chem_aqaer%species(j)%amb )
    conc(i)    = MAX( -concamb(i),chem_aqaer%species(j)%conc )
    SELECT CASE( TRIM(aero_names(i)) )
      CASE( 'NUMATKN','NUMACC','NUMCOR','SRFATKN','SRFACC','SRFCOR' )
        conc(i) = MAX(conc(i),0.)
    END SELECT
  END DO
  dtaero = dt
  CALL step_aerosol_chem( dtaero, concamb, conc )
  IF( error%nError /= NO_ERROR )GOTO 9998

  ! --- Set particle dry deposition velocities
  IF( carea > 0. )THEN
    DO i = 1, naerp
      conctot(i) = concamb(i) + conc(i)
    END DO
    CALL set_vdry( conctot )
    DO i = 1, naerp
      j = index_aero(i)
      chem_aqaer%species(j)%taudry = vdry(i)*carea
    END DO
  ELSE
    DO i = 1, naerp
      j = index_aero(i)
      chem_aqaer%species(j)%taudry = 0.
    END DO
  END IF
  !-- Unload aerosol working array
  DO i = 1, naero
    j                          = index_aero(i)
    chem_aqaer%species(j)%conc = MAX( conc(i), -chem_aqaer%species(j)%amb )
  END DO

END IF

9998 CONTINUE

IF( error%nError /= NO_ERROR )CALL AqAerPostError( )
nError = error%nError

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE get_radyn(ustar, xml, z1, zruf, radyn)
! Calculates aerodynamic resistance for deposition calculations
! Version 1.0, PKK, AER, January 2005, based on routines used in MCIP 2.3

IMPLICIT NONE

! --- PBL Constants; Hogstrom (1988)
REAL,          PARAMETER     :: VKAR  =  0.40
REAL,          PARAMETER     :: BETAH =  8.21
REAL,          PARAMETER     :: GAMAH = 11.60
REAL,          PARAMETER     :: PRO   =  0.95

! --- ARGUMENTS
REAL ustar      !Friction velocity (m/s)
REAL xml        !MO length (m)
REAL z1         !Height of first layer (m)
REAL zruf       !Roughness height (m)
REAL radyn      !aerodynamic resistance (s/m)

! --- LOCALS
REAL z1ol, zntol, alogz1z0, psih0, psih

z1ol     = z1 / xml
zntol    = zruf / xml
alogz1z0 = ALOG(z1/zruf)

IF( z1ol >= 0.0 )THEN

  IF( z1ol > 1.0 )THEN
    psih0 = 1.0 - BETAH - z1ol
  ELSE
    psih0 = -BETAH * z1ol
  END IF

  IF( zntol > 1.0 )THEN
     psih = psih0 - (1.0 - BETAH - zntol)
  ELSE
     psih = psih0 + BETAH * zntol
  END IF

ELSE

  psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - GAMAH*z1ol)) /  &
                           (1.0 + SQRT(1.0 - GAMAH*zntol)) )

END IF

radyn = PRO * ( alogz1z0 - psih ) / ( VKAR * ustar )

RETURN
END

!==============================================================================

INTEGER FUNCTION AqAerWarningMessage( NotAffirmativeString )

!----- Display warning message with yes/no buttons

USE AqAer_fi
USE mpi_fi
USE basic_aqaer_fd
USE message_aqaer_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: NotAffirmativeString

INTEGER irv, ios

TYPE( messageT  ) :: message

INTEGER, EXTERNAL :: PostReplyMessage

AqAerWarningMessage = AqAerfailure
  !------ Copy Warning Message

  message%iParm   = error%nError
  message%jParm   = TRUE
  message%routine = TRIM(error%eRoutine)
  message%aString = TRIM(error%eMessage)
  message%bString = TRIM(error%eInform)
  message%cString = TRIM(error%eAction)

  !------ Post warning

  irv = PostReplyMessage( message )
!------ If message is not affirmative, set Action and return

IF( irv /= AqAeraffirmative )THEN
  error%eAction = TRIM(NotAffirmativeString)
  GOTO 9999
END IF

!------ Otherwise, clear error

CALL AqAerClearError()

AqAerWarningMessage = AqAersuccess

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE AqAerClearError()

USE AqAer_fi

error%nError  = NO_ERROR
error%eRoutine = ' '
error%eMessage = ' '
error%eInform  = ' '
error%eAction  = ' '

RETURN
END

!==============================================================================

SUBROUTINE AqAerPostError( )

USE AqAer_fi

INTEGER lun
LOGICAL isOpen

INQUIRE(UNIT=lun_log,OPENED=isOpen )

IF( isOpen )THEN
  lun = lun_log
ELSE
  lun = 6
END IF
WRITE(lun_log,'("Error from routine ",A)',IOSTAT=ios)error%eRoutine
WRITE(lun_log,'(A)',IOSTAT=ios)error%eMessage
WRITE(lun_log,'(A)',IOSTAT=ios)error%eInform
WRITE(lun_log,'(A)',IOSTAT=ios)error%eAction

IF( isOpen )CLOSE(lun,IOSTAT=ios)

RETURN
END
