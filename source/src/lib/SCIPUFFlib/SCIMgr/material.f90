!*******************************************************************************
!            Load Project Material Input
!*******************************************************************************
INTEGER FUNCTION Load_Material( tool,mtlList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pmaterialT ),               INTENT( INOUT ) :: tool
TYPE( materialT ) , DIMENSION(*), INTENT( INOUT ) :: mtlList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Material = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadMaterial( tool%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Read namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchName = tool%control%searchName
ELSE
  searchName = ' '
END IF
CALL cupper( searchName )

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'inp' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL ReadMaterial( filename,lun_tmp,searchName ) !Material name
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from namelist results (SCIPUFF memory)

CALL LoadMaterial( tool%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_Material = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Material Input
!*******************************************************************************
INTEGER FUNCTION Write_Material( tool,mtlList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pmaterialT ), INTENT( INOUT ) :: tool
TYPE( materialT ) , INTENT( INOUT ) :: mtlList(1)

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Material = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadMaterial( tool%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Write namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchName = tool%control%searchName
ELSE
  searchName = ' '
END IF
CALL cupper( searchName )

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'inp' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL WriteMaterial( filename,lun_tmp,searchName ) !Material name
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Material = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Material Input
!*******************************************************************************
INTEGER FUNCTION Default_Material( mtlHead,mtlList )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( listHeadT ),               INTENT( IN    ) :: mtlHead
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlList

INTEGER i,result

INTEGER, EXTERNAL :: DefaultGasData
INTEGER, EXTERNAL :: DefaultAerosolData
INTEGER, EXTERNAL :: DefaultLiquidData
INTEGER, EXTERNAL :: DefaultParticleData

Default_Material = SCIPfailure

IF( mtlHead%max < mtlHead%number )GOTO 9999

DO i = 1,mtlHead%number
  mtlList(i)%puffIndex = NOT_SET_I
  mtlList(i)%units     = 'kg'
  mtlList(i)%file      = ' '
  mtlList(i)%path      = ' '
  IF( BTEST(mtlList(i)%type,HMB_AEROSOL) )THEN
    mtlList(i)%name = 'Aero'
    result = DefaultAerosolData( mtlList(i)%matData )
    IF( result /= SCIPsuccess )GOTO 9999
  ELSE IF( BTEST(mtlList(i)%type,HMB_GAS) )THEN
    mtlList(i)%name = 'Gas'
    result = DefaultGasData( mtlList(i)%matData )
    IF( result /= SCIPsuccess )GOTO 9999
  ELSE IF( BTEST(mtlList(i)%type,HMB_LIQUID) )THEN
    mtlList(i)%name = 'Lqd'
    result = DefaultLiquidData( mtlList(i)%matData )
    IF( result /= SCIPsuccess )GOTO 9999
  ELSE IF( BTEST(mtlList(i)%type,HMB_PARTICLE) )THEN
    mtlList(i)%name = 'Part'
    result = DefaultParticleData( mtlList(i)%matData )
    IF( result /= SCIPsuccess )GOTO 9999
  END IF
END DO

Default_Material = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Gas Material Input
!*******************************************************************************
INTEGER FUNCTION DefaultGasData( mtlDataIn )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matGasT ) mtlData

DefaultGasData = SCIPfailure

mtlData%minConcentration = NOT_SET_R
mtlData%decayAmp         = 0.0
mtlData%decayMin         = 0.0
mtlData%rNotUsed         = 0.0
mtlData%save             = HS_GROUPDOS
mtlData%gasDensity       = 1.2
mtlData%gasDeposition    = 0.0

mtlData%padding(1:HS_PADMTLGAS) = NOT_SET_I

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

DefaultGasData = SCIPsuccess

RETURN
END
!*******************************************************************************
!            Default Aerosol Material Input
!*******************************************************************************
INTEGER FUNCTION DefaultAerosolData( mtlDataIn )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

TYPE( matAerosolT ) mtlData

DefaultAerosolData = SCIPfailure

mtlData%minConcentration = NOT_SET_R
mtlData%decayAmp         = 0.0
mtlData%decayMin         = 0.0
mtlData%rNotUsed         = 0.0
mtlData%save             = HS_GROUPDOS
mtlData%gasDeposition    = 0.0

!==== These are number close to but not exactly those of Butane

mtlData%liquidDensity    = 579.00
mtlData%antoine(1)       = 6.8
mtlData%antoine(2)       = 935.9
mtlData%antoine(3)       = 238.7
mtlData%molWeight        = 58.0
mtlData%liqSpecificHeat  = 2400.0
mtlData%gasSpecificHeat  = 1700.0

mtlData%padding(1:HS_PADMTLAEROSOL) = NOT_SET_I

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

DefaultAerosolData = SCIPsuccess

RETURN
END
!*******************************************************************************
!            Default Liquid Material Input
!*******************************************************************************
INTEGER FUNCTION DefaultLiquidData( mtlDataIn )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

INTEGER i

TYPE( matLiquidT ) mtlData

DefaultLiquidData = SCIPfailure

mtlData%minConcentration = NOT_SET_R
mtlData%decayAmp         = 0.0
mtlData%decayMin         = 0.0
mtlData%rNotUsed         = 0.0
mtlData%save             = HS_GROUPDOS + HS_GROUPDEP
mtlData%gasDeposition    = 0.0

!==== These are number close to but not exactly those of H2SO4

mtlData%gasDensity       = 4.0
mtlData%liquidDensity(1) = 1841.0
mtlData%liquidDensity(2) = 0.0
mtlData%antoine(1)       = 8.1
mtlData%antoine(2)       = 2776.4
mtlData%antoine(3)       = 195.8
mtlData%molWeight        = 98.0
mtlData%gasSpecificHeat  = 1200
mtlData%liqSpecificHeat  = 1800
mtlData%srfTension       = 0.3
mtlData%spreadFactor     = 3.5
mtlData%nSizeBins        = 20

mtlData%binBounds( 1) = 0.
mtlData%binBounds( 2) = 1.25E-5
mtlData%binBounds( 3) = 1.69E-5
mtlData%binBounds( 4) = 2.29E-5
mtlData%binBounds( 5) = 3.11E-5
mtlData%binBounds( 6) = 4.21E-5
mtlData%binBounds( 7) = 5.70E-5
mtlData%binBounds( 8) = 7.73E-5
mtlData%binBounds( 9) = 1.05E-4
mtlData%binBounds(10) = 1.42E-4
mtlData%binBounds(11) = 1.92E-4
mtlData%binBounds(12) = 2.60E-4
mtlData%binBounds(13) = 3.53E-4
mtlData%binBounds(14) = 4.78E-4
mtlData%binBounds(15) = 6.47E-4
mtlData%binBounds(16) = 8.77E-4
mtlData%binBounds(17) = 1.19E-3
mtlData%binBounds(18) = 1.61E-3
mtlData%binBounds(19) = 2.18E-3
mtlData%binBounds(20) = 2.95E-3
mtlData%binBounds(21) = 4.00E-3
mtlData%binBounds(22:HS_MAXMTLBINSIZE+1) = NOT_SET_R

DO i = 1,mtlData%nSizeBins
  mtlData%binSize(i) = 0.5*(mtlData%binBounds(i) + mtlData%binBounds(i+1))
END DO
mtlData%binSize(mtlData%nSizeBins+1:HS_MAXMTLBINSIZE) = NOT_SET_R

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

DefaultLiquidData = SCIPsuccess

RETURN
END
!*******************************************************************************
!            Default Particle Material Input
!*******************************************************************************
INTEGER FUNCTION DefaultParticleData( mtlDataIn )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( matGenT ), INTENT( OUT ) :: mtlDataIn

INTEGER  i

TYPE( matParticleT ) mtlData

DefaultParticleData = SCIPfailure

mtlData%minConcentration = NOT_SET_R
mtlData%decayAmp         = 0.0
mtlData%decayMin         = 0.0
mtlData%rNotUsed         = 0.0
mtlData%save             = HS_TOTALDOS + HS_TOTALDEP

!==== These are number made up

mtlData%density       = 500.
mtlData%nSizeBins     = 20
mtlData%binBounds( 1) = 0.
mtlData%binBounds( 2) = 1.25E-5
mtlData%binBounds( 3) = 1.69E-5
mtlData%binBounds( 4) = 2.29E-5
mtlData%binBounds( 5) = 3.11E-5
mtlData%binBounds( 6) = 4.21E-5
mtlData%binBounds( 7) = 5.70E-5
mtlData%binBounds( 8) = 7.73E-5
mtlData%binBounds( 9) = 1.05E-4
mtlData%binBounds(10) = 1.42E-4
mtlData%binBounds(11) = 1.92E-4
mtlData%binBounds(12) = 2.60E-4
mtlData%binBounds(13) = 3.53E-4
mtlData%binBounds(14) = 4.78E-4
mtlData%binBounds(15) = 6.47E-4
mtlData%binBounds(16) = 8.77E-4
mtlData%binBounds(17) = 1.19E-3
mtlData%binBounds(18) = 1.61E-3
mtlData%binBounds(19) = 2.18E-3
mtlData%binBounds(20) = 2.95E-3
mtlData%binBounds(21) = 4.00E-3
mtlData%binBounds(22:HS_MAXMTLBINSIZE+1) = NOT_SET_R

DO i = 1,mtlData%nSizeBins
   mtlData%binSize(i) = 0.5*(mtlData%binBounds(i) + mtlData%binBounds(i+1))
END DO
mtlData%binSize(mtlData%nSizeBins+1:HS_MAXMTLBINSIZE) = NOT_SET_R

mtlData%padding(1:HS_PADMTLPARTICLE) = NOT_SET_I

mtlDataIn = TRANSFER(mtlData,mtlDataIn)

DefaultParticleData = SCIPsuccess

RETURN
END
