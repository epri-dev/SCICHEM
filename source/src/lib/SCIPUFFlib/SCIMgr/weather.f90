!*******************************************************************************
!            Load Project Weather Input
!*******************************************************************************
INTEGER FUNCTION Load_Weather( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pweatherT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Weather = SCIPfailure

tool%weather%met%input = 'NOT SET'

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadWeather( tool%weather )
IF( nError /= NO_ERROR )GOTO 9999

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'msc' ))
CALL AddPath( filename,tool%project%path )
CALL ReadWeather( filename,lun_msc,tool%weather%met%input,HS_MAXMETINP,  &
                  tool%weather%met%nStations ) !Size of list (returned)

!==== Force the load if error is simply the fact that one of the MET files
!     could not be opened

IF( nError /= NO_ERROR )THEN
  IF( nError /= OP_ERROR .OR. TRIM(eRoutine) == 'ReadWeather' )THEN
    GOTO 9999
  ELSE
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

!==== Set input data from namelist results (SCIPUFF memory)

CALL LoadWeather( tool%weather )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_Weather = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Weather Input
!*******************************************************************************
INTEGER FUNCTION Write_Weather( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pweatherT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Weather = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadWeather( tool%weather )
IF( nError /= NO_ERROR )GOTO 9999

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'msc' ))
CALL AddPath( filename,tool%project%path)
CALL WriteWeather( filename,lun_msc,tool%weather%met%input, &
                   tool%weather%met%nStations ) !Size of list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Weather = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Weather Input
!*******************************************************************************
INTEGER FUNCTION Default_Weather( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( pweatherT ), INTENT( INOUT ) :: tool

Default_Weather = SCIPfailure

tool%weather%flags%reference = HT_UTC
tool%weather%flags%doMC      = SCIPoff
tool%weather%flags%doOutput  = HO_OFF
tool%weather%flags%notUsed   = HF_OFF
tool%weather%flags%tOutput   = NOT_SET_R
tool%weather%flags%slHazard  = 100.

tool%weather%met%type      = HW_METFIX
tool%weather%met%nnPrf     = DEF_VAL_I
tool%weather%met%nnSfc     = DEF_VAL_I
tool%weather%met%timeBin   = 3600.
tool%weather%met%nStations = 0
tool%weather%met%monthDay  = 0
tool%weather%met%unitSpd   = HU_MPS
tool%weather%met%unitDir   = HU_DEG
tool%weather%met%speed     = 4.0
tool%weather%met%direction = 90.0
tool%weather%met%notUsed   = -1
tool%weather%met%input     = ' '
tool%weather%met%Use_TKE   = .FALSE.

tool%weather%bl%type       = HB_BLOPER
tool%weather%bl%wetness    = HM_MSTNORM
tool%weather%bl%landuse    = NOT_SET_C
tool%weather%bl%ziDay      = 1000.
tool%weather%bl%ziNight    = 50.
tool%weather%bl%hfluxDay   = 50.
tool%weather%bl%hfluxNight = 0.0
tool%weather%bl%canopy     = 1.0
tool%weather%bl%roughness  = NOT_SET_R
tool%weather%bl%albedo     = 0.16
tool%weather%bl%bowen      = 0.6
tool%weather%bl%cloud      = 0.0
tool%weather%bl%canopyParam= 2.0

tool%weather%lsv%type       = HL_LSVOPER
tool%weather%lsv%uu         = 0.0
tool%weather%lsv%sl         = 100.

tool%weather%precip%type    = HP_NONE
tool%weather%precip%class   = HP_NONE

tool%weather%terrain%type   = HT_NONE
tool%weather%terrain%file   = ' '
tool%weather%terrain%mc%notUsed = DEF_VAL_R
tool%weather%terrain%mc%maxIter = (/ 200,    100    /)
tool%weather%terrain%mc%eps     = (/ 1.0E-2, 1.0E-5 /)
tool%weather%terrain%mc%alpha   = (/ 0.001,  1.0    /)
tool%weather%terrain%mc%nz      = 0
tool%weather%terrain%mc%z       = 0.0

Default_Weather = SCIPsuccess

RETURN
END


