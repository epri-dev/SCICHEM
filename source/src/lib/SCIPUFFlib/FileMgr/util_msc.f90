!===============================================================================
!     CheckNamelistMet
!===============================================================================
SUBROUTINE CheckNamelistMet( UseMetType,UseBLType,UseLSVType,UsePrecipType )

USE scipuff_fi
USE met_fi

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: UseMetType
CHARACTER(*), INTENT( OUT ) :: UseBLType
CHARACTER(*), INTENT( OUT ) :: UseLSVType
CHARACTER(*), INTENT( OUT ) :: UsePrecipType


!==== basic input

CALL CheckRangeReal( zruf,1.E-30,1.E30,'Surface roughness' )
IF( nError /= NO_ERROR )GOTO 9999

!==== set Use types

CALL cupper(  met_type )
CALL cupper(   bl_type )
CALL cupper( ensm_type )
CALL cupper(   pr_type )

!==== Meteorology

SELECT CASE( TRIM(met_type) )
  CASE( 'OPWX_GRID' )
    UseMetType = 'MRF'

  CASE( 'OPWX_GRID2' )
    UseMetType = 'MEDOC'

  CASE( 'OPER_FCST' )
    UseMetType = 'GRIDDED'

  CASE( 'OPER_NOWG' )
    UseMetType = 'GRIDDED'

  CASE( 'OPWX_POST' )
    UseMetType = 'OBS'

  CASE( 'OPWX_CURR' )
    UseMetType = 'OBS'

  CASE( 'OPER_NOWO' )
    UseMetType = 'OBS'

  CASE DEFAULT
    UseMetType = met_type

END SELECT

!==== Boundary Layer

UseBLType = bl_type

!==== Large scale

SELECT CASE( TRIM(ensm_type) )
  CASE( 'OPER3.1' )
    UseLSVType = 'OPER'

  CASE( 'OPER' )
    UseLSVType = 'NONE'

  CASE DEFAULT
    UseLSVType = ensm_type

END SELECT

!==== Precipitation

UsePrecipType = pr_type

!==== check types

!==== Meteorology
!       lua        (except OBS - need to read file)
!       lsfc       (except OBS - need to read file)
!       update_ua = lua
!       update_bl = lsfc

lua  = .FALSE.
lsfc = .FALSE.
SELECT CASE( TRIM(UseMetType) )
  CASE( 'FIXED' ) !=>FIXED
    lsfc = .TRUE.

  CASE( 'CLIMO' ) !=>CLIMO

  CASE( 'CLI3D','CLI3DMC' ) !=>CLI3D

  CASE( 'GRIDDED' ) !=>GRIDDED
    lua  = .TRUE.

  CASE( 'MEDOC' ) !=>GRIDDED
    lua  = .TRUE.

  CASE( 'WRF' )
    lua  = .TRUE.

  CASE( 'ASSIM','SCIPUFF_LIST' )
    lua  = .TRUE.

  CASE( 'MEDOC_LIST','MEDOCLIST' )
    lua  = .TRUE.

  CASE( 'MRF','GRIDMC' ) !=>GRIDDED
    lua  = .TRUE.

  CASE( 'OBS' ) !=>OBS

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid meteorology (MET_TYPE)'
    eInform  = 'MET_TYPE='//TRIM(met_type)

END SELECT
IF( nError /= NO_ERROR )GOTO 9999

!==== Boundary Layer
!       Set lbl

lbl = .TRUE.
SELECT CASE( TRIM(UseBLType) )
  CASE( 'CALC'  )
  CASE( 'OBS'   )
  CASE( 'SBL'   )
  CASE( 'PROF'  )
  CASE( 'OPER'  ) !=>CALC or OBS
  CASE( 'MEDOC' )
  CASE( 'NONE'  )
    lbl = .FALSE.

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid boundary layer (BL_TYPE)'
    eInform  = 'BL_TYPE='//TRIM(bl_type)
END SELECT

IF( nError /= NO_ERROR )GOTO 9999

!==== Large-scale
!       set lensm
!           uu_ensm (except for INPUT)
!           sl_ensm (except for INPUT and OBS)

lensm = .TRUE.
SELECT CASE( TRIM(UseLSVType) )
  CASE( 'INPUT' )

  CASE( 'OPER'  )
    IF( sl_ensm == NOT_SET_R .OR. sl_ensm <= 0. )THEN
      sl_ensm = 1.E+10
    END IF
    uu_ensm = 0.

  CASE( 'MODEL' )
    sl_ensm = 1.E+10
    uu_ensm = 0.

  CASE( 'OBS' )
    uu_ensm = 0.

  CASE( 'NONE' )
    lensm = .FALSE.
    sl_ensm = 1.E+6
    uu_ensm = 0.

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid large-scale input (ENSM_TYPE)'
    eInform  = 'ENSM_TYPE='//TRIM(ensm_type)

END SELECT
IF( nError /= NO_ERROR )GOTO 9999

IF( lensm )THEN
  IF( uu_ensm == NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid large-scale input (UU_ENSM)'
    eInform  = 'UU_ENSM must be set'
  END IF
  IF( uu_ensm < 0. )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid large-scale input (UU_ENSM)'
    eInform  = 'UU_ENSM must not be negative'
  END IF
  IF( sl_ensm == NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid large-scale input (SL_ENSM)'
    eInform  = 'SL_ENSM must be set'
  END IF
  IF( sl_ensm <= 0. )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid large-scale input (SL_ENSM)'
    eInform  = 'SL_ENSM must be positive'
  END IF
END IF
IF( nError /= NO_ERROR )GOTO 9999

!==== Precipitation
!       lwash
!       pr_rate

SELECT CASE( TRIM(UsePrecipType) )
  CASE( 'METFILE' )
    pr_rate = -1.

  CASE( 'LGTRAIN' )
    pr_rate = 1.

  CASE( 'MODRAIN' )
    pr_rate = 2.

  CASE( 'HVYRAIN' )
    pr_rate = 3.

  CASE( 'LGTSNOW' )
    pr_rate = 4.

  CASE( 'MODSNOW' )
    pr_rate = 5.

  CASE( 'HVYSNOW' )
    pr_rate = 6.

  CASE( 'NONE' )
    pr_rate = 0.
    lwash = .FALSE.

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckNamelistMet'
    eMessage = 'Invalid precipitation (PR_TYPE)'
    eInform  = 'PR_TYPE='//TRIM(pr_type)

END SELECT
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!===============================================================================
!     CheckMetCombination
!===============================================================================
SUBROUTINE CheckMetCombinations( UseMetType,UseBLType,UseLSVType,UsePrecipType )

USE error_fi
USE met_fi

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: UseMetType
CHARACTER(*), INTENT( IN    ) :: UseBLType
CHARACTER(*), INTENT( IN    ) :: UseLSVType
CHARACTER(*), INTENT( IN    ) :: UsePrecipType

LOGICAL lOK

!==== Reset met type for mass-consistency

IF( lmc_ua )THEN
  SELECT CASE( TRIM(UseMetType) )
    CASE( 'MRF' )
      UseMetType = 'GRIDMC'

    CASE DEFAULT

  END SELECT
END IF

!==== Met/bl

IF( lbl )THEN
  SELECT CASE( TRIM(UseBLType) )
    CASE( 'OBS' )
      lOK = TRIM(UseMetType) == 'OBS'

    CASE( 'PROF' )
      lOK = UseMetType == 'OBS'

    CASE( 'MEDOC' )
!      lOK = ANY(UseMetType == (/'MEDOC','ASSIM','SCIPUFF_LIST'/))
      lOK = TRIM(UseMetType) == 'MEDOC'       .OR. &
            TRIM(UseMetType) == 'ASSIM'       .OR. &
            TRIM(UseMetType) == 'MEDOC_LIST'  .OR. &
            TRIM(UseMetType) == 'MEDOCLIST'   .OR. &
            TRIM(UseMetType) == 'SCIPUFF_LIST'
      lOK = lOK .OR. TRIM(UseMetType) == 'WRF'

    CASE DEFAULT
      lOK = .TRUE.

  END SELECT
  IF( .NOT.lOK )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckMetCombinations'
    eMessage = 'Invalid Meteorology:BoundaryLayer combination'
    eInform  = 'MET_TYPE='//TRIM(met_type)//' : BL_TYPE='//TRIM(bl_type)
    GOTO 9999
  END IF
END IF

!==== Met/ensm

IF( lensm )THEN
  SELECT CASE( TRIM(UseLSVType) )
    CASE( 'OBS' )
      lOK = TRIM(UseMetType) =='OBS'
    CASE DEFAULT
      lOK = .TRUE.
  END SELECT
  IF( .NOT.lOK )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckMetCombinations'
    eMessage = 'Invalid Meteorology:LargeScale combination'
    eInform  = 'MET_TYPE='//TRIM(met_type)//' : ENSM_TYPE='//TRIM(ensm_type)
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     ReadFixedInput
!===============================================================================
SUBROUTINE ReadFixedInput( lunit )

USE error_fi
USE met_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lunit

CHARACTER(128) fixed_line

INTEGER ios

#if defined(LINUX)
DO
#endif
READ(lunit,'(A)',IOSTAT=ios) fixed_line
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadFixedInput'
  eMessage = 'Error reading FIXED MET input line'
  GOTO 9999
END IF
#if defined(LINUX)
IF( LEN_TRIM(fixed_line) > 0 )EXIT
END DO
#endif

READ(fixed_line,*,IOSTAT=ios) fixed_spd,fixed_dir,unit_spd

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios < 0 )THEN
  unit_spd = 1
  READ(fixed_line,*,IOSTAT=ios) fixed_spd,fixed_dir
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadFixedInput'
  eMessage = 'Error reading FIXED MET parameters'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     ReadObsInput
!===============================================================================
SUBROUTINE ReadObsInput( lunit,lset )

USE met_fi
USE error_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lunit
LOGICAL, INTENT( IN ) :: lset

INTEGER, PARAMETER :: NOT_READ = 65535

CHARACTER(PATH_MAXLENGTH) filename(2), tmpname
CHARACTER(128) filetype
CHARACTER(2*PATH_MAXLENGTH+11) line, linex

INTEGER ios, i, nfile, nmin(2), ninp(2)
LOGICAL lexist

LOGICAL, EXTERNAL :: GetNextFilename

!==== Read input line as a character string

#if defined(LINUX)
DO
#endif
READ(lunit,'(A)',IOSTAT=ios) line
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadObsInput'
  eMessage = 'Error OBS MET input'
  eInform  = 'Input='//TRIM(line)
  GOTO 9999
END IF
#if defined(LINUX)
IF( LEN_TRIM(line) > 0 )EXIT
END DO
#endif

!==== Decipher input file into filename and ninp

nfile = 0
linex = line
DO WHILE( GetNextFilename(linex,tmpname,i) .AND. nfile < 2 )
  nfile = nfile + 1
  filename(nfile) = tmpname
  IF( i <= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadObsInput'
    eMessage = 'Error OBS MET parameters'
    eInform  = 'Input='//TRIM(line)
    GOTO 9999
  ELSE
    ninp(nfile) = i
  END IF
END DO

IF( nfile <= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadObsInput'
  eMessage = 'No OBS filenames decoded'
  eInform  = 'Input='//TRIM(line)
  GOTO 9999
END IF

!==== Determine filetype

nmin(1) = NOT_READ
nmin(2) = NOT_READ

DO i = 1,nfile

!------ Open file if it exists and read first non-comment line

  INQUIRE(FILE=filename(i),EXIST=lexist,IOSTAT=ios)
  IF( lexist .AND. ios == 0 )THEN
    OPEN(UNIT=lun_tmp,FILE=filename(i),STATUS='OLD',ACTION="READ",IOSTAT=ios)
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'read_obs_names'
      eMessage = 'Error opening met obs input file'
      CALL ReportFileName( eInform,'File=',filename(i) )
      CLOSE(lun_tmp,IOSTAT=ios)
      GOTO 9999
    END IF
    filetype ='#'
    ios = 0
    DO WHILE( filetype(1:1) =='#' .AND. ios == 0 )
      READ(lun_tmp,'(A)',IOSTAT=ios) filetype
    END DO
  ELSE
    filetype = ' '
  END IF

!------ Either file doesn't exist yet or couldn't find a non-comment line
!       so set filetype based on filename

  CheckFileType: DO

!------ Set SCIPUFF input based on file type

    CALL cupper( filetype )

    IF( TRIM(ADJUSTL(filetype)) == 'PROFILE' )THEN
      file_met  = filename(i)
      n_obs_min = MAX(ninp(i),1)
      nmin(2)   = n_obs_min
      EXIT
    ELSE IF( TRIM(ADJUSTL(filetype)) == 'SURFACE' )THEN
      file_sfc  = filename(i)
      n_sfc_min = MAX(ninp(i),1)
      nmin(1)   = n_sfc_min
      EXIT
    END IF

    IF( i == 1 )THEN
      filetype = filename(i)
      CALL cupper( filetype )
      ios = MAX(INDEX(filetype,'.UA'),INDEX(filetype,'.PRF'))
      IF( ios <= 0 )THEN
        filetype ='SURFACE'
      ELSE
        filetype ='PROFILE'
      END IF
    ELSE
      IF( nmin(2) /= NOT_READ )THEN
        filetype ='SURFACE'
      ELSE
        filetype ='PROFILE'
      END IF
    END IF

    CYCLE CheckFileType

  END DO CheckFileType

!------ Close the file if opened

  IF( lexist )CLOSE(lun_tmp,IOSTAT=ios)

END DO

!==== Set lua,lsfc if requested

IF( lset )THEN
  lua  = nmin(2) /= NOT_READ
  lsfc = nmin(1) /= NOT_READ
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     ReadGriddedInput
!===============================================================================
SUBROUTINE ReadGriddedInput( lunit,filetype )

USE error_fi
USE files_fi
USE DefSize_fd

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: lunit
CHARACTER(*), INTENT( OUT ) :: filetype


INTEGER   ios, i
REAL      lonx, latx

CHARACTER(8)   fflag
CHARACTER(PATH_MAXLENGTH) line
CHARACTER(PATH_MAXLENGTH) tmpname

LOGICAL, EXTERNAL :: GetNextFilename

!==== Read gridded file name

#if defined(LINUX)
DO
#endif
READ(lunit,'(A)',IOSTAT=ios) line
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadGriddedInput'
  eMessage = 'Error reading GRIDDED MET input'
  GOTO 9999
END IF
#if defined(LINUX)
IF( LEN_TRIM(line) > 0 )EXIT
END DO
#endif

IF( GetNextFilename(line,tmpname,i) )THEN
  file_met = tmpname
ELSE
  nError   = RD_ERROR
  eRoutine = 'ReadGriddedInput'
  eMessage = 'No Gridded filenames decoded'
  eInform  = 'Input='//TRIM(line)
  GOTO 9999
END IF

!==== determine file type; open file as 'formatted'

OPEN(UNIT=lun_tmp,FILE=file_met,STATUS='OLD',FORM='FORMATTED', &
                                          ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadGriddedInput'
  eMessage = 'Error opening GRIDDED MET input file'
  GOTO 9999
END IF

!------ Read first record

READ(lun_tmp,'(A)',IOSTAT=ios) line
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadGriddedInput'
  eMessage = 'Error reading GRIDDED MET input file'
  GOTO 9999
END IF

!------ If first record begins with 'F', this is a formatted MEDOC file

IF( TRIM(line(1:1)) == 'F' )THEN
  filetype = 'MEDOC'

ELSE IF(  line(1:3) == 'WRF' )THEN
  filetype = 'WRF'

ELSE IF(  line(1:5) == 'ASSIM' .OR. line(1:12) == 'SCIPUFF_LIST' )THEN
  filetype = 'ASSIM'

ELSE IF(  line(1:5) == 'MEDOC' )THEN
  filetype = 'MEDOC_LIST'

!------ If first record is a comment ('# ... '), assume a MRF (SCIP gridded) file

ELSE IF(  line(1:1) == '#' )THEN
  filetype = 'MRF'

ELSE

!------ Check if first record matches MRF format

  READ(line,FMT='(2F10.3)',IOSTAT=ios) lonx,latx

!------ If read error, check for unformatted MEDOC file

  IF( ios /= 0 )THEN

    CLOSE(lun_tmp,IOSTAT=ios)
    OPEN(UNIT=lun_tmp,FILE=file_met,STATUS='OLD',FORM='UNFORMATTED', &
                                             ACTION="READ",IOSTAT=ios)
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'ReadGriddedInput'
      eMessage = 'Error opening GRIDDED MET input file'
      GOTO 9999
    END IF
    READ(lun_tmp,IOSTAT=ios) fflag(1:1)
    IF( fflag(1:1) == 'B' )THEN
      filetype = 'MEDOC'
    ELSE !Error if not 'B...'
      nError   = IV_ERROR
      eRoutine = 'ReadGriddedInput'
      eMessage = 'Unrecognized GRIDDED MET input file'
      GOTO 9999
    END IF

!------ Check if lat/lon are valid values for MRF format

  ELSE

    IF( ABS(lonx) <= 360. .OR. ABS(latx) <= 180. )THEN
      filetype = 'MRF'
    ELSE
      nError   = IV_ERROR
      eRoutine = 'ReadGriddedInput'
      eMessage = 'Unrecognized GRIDDED MET input file'
      GOTO 9999
    END IF

  END IF

END IF

9999 CONTINUE

CLOSE( lun_tmp,IOSTAT=ios )

RETURN
END
!===============================================================================
!     WriteFixedInput
!===============================================================================
SUBROUTINE WriteFixedInput( lunit )

USE error_fi
USE met_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lunit

CHARACTER(128) fixed_line

INTEGER ios

WRITE(fixed_line,*,IOSTAT=ios) fixed_spd,fixed_dir,unit_spd
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteFixedInput'
  eMessage = 'Error preparing FIXED MET parameters'
  GOTO 9999
END IF

WRITE(lunit,'(A)',IOSTAT=ios)TRIM(fixed_line)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteFixedInput'
  eMessage = 'Error writing FIXED MET parameters'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteObsInput
!===============================================================================
SUBROUTINE WriteObsInput( lunit )

USE met_fi
USE error_fi
USE files_fi
USE DefSize_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lunit

CHARACTER(2*PATH_MAXLENGTH+11) line

INTEGER ios,nch

!==== Build input line from filenames and nmaxs

line = ' '

IF( n_sfc_min > 0 )THEN
  nch = LEN_TRIM(line)
  WRITE(line(nch+1:),'(I4,A,I3.3,A)',iostat=ios)n_sfc_min, &
                            ' @',LEN_TRIM(file_sfc),TRIM(file_sfc)
  IF( ios /= 0 )THEN
    nError = WR_ERROR
    eRoutine = 'WriteObsInput'
    eMessage = 'Error preparing OBS MET surface input'
    GOTO 9999
  END IF
END IF

IF( n_obs_min > 0 )THEN
  nch = LEN_TRIM(line)
  WRITE(line(nch+1:),'(I4,A,I3.3,A)',IOSTAT=ios)n_obs_min, &
                            ' @',LEN_TRIM(file_met),TRIM(file_met)
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'WriteObsInput'
    eMessage = 'Error preparing OBS MET profile input'
    GOTO 9999
  END IF
END IF

!==== Write input line as a character string

WRITE(lunit,'(A)',IOSTAT=ios) TRIM(line)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteObsInput'
  eMessage = 'Error writing OBS MET input'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteGriddedInput
!===============================================================================
SUBROUTINE WriteGriddedInput( lunit )

USE error_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lunit

INTEGER ios

!==== Write gridded file name

WRITE(lunit,'(A,I3.3,A)',IOSTAT=ios)'@',LEN_TRIM(file_met),TRIM(file_met)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteGriddedInput'
  eMessage = 'Error writing GRIDDED MET input'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     GetNextFilename
!===============================================================================
LOGICAL FUNCTION GetNextFilename( line,file,iparam )

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: line
CHARACTER(*), INTENT( OUT   ) :: file

INTEGER iparam

INTEGER i,j,nch,ios
LOGICAL new

!==== Initialize

GetNextFilename = .FALSE.
file   = ' '
iparam = 0

!==== Determine if filename exists and line type ($->old @->new)

nch = LEN_TRIM(line)
IF( nch <= 1 )GOTO 9999

i = INDEX(line,'$')
j = INDEX(line,'@')

IF( MAX(i,j) <= 0 )GOTO 9999

new = (j > 0) .AND. (j < i .OR. i == 0)

!==== New type @xxxFilename

IF( new )THEN

  IF( j > nch - 4 )THEN
    iparam = -997
    GOTO 9999
  END IF

  READ(line(j+1:j+3),*,IOSTAT=ios)i
  IF( (ios /= 0) .OR. (i <= 0) .OR. (i > (nch - (j+3))) )THEN
    iparam = -998
    GOTO 9999
  END IF

  IF( line(j+4:j+3+i) /= 'mc' )THEN
    file = line(j+4:j+3+i)
    IF( j > 1 )THEN
      READ(line(1:j-1),*,IOSTAT=ios)iparam
      IF( ios /= 0 )THEN
        iparam = -999
      END IF
    END IF
  END IF

  line = line(j+4+i:)

  GetNextFilename = .TRUE.

!==== Old type $Filename

ELSE

  j = INDEX(line(i+1:nch),' ')
  IF( j == 0 )THEN
    j = nch + 1
  ELSE
    j = j + i
  END IF
  IF( line(i+1:j-1) /='mc' )THEN
    file = line(i+1:j-1)
    IF( i > 1 )THEN
      READ(line(1:i-1),*,IOSTAT=ios)iparam
      IF( ios /= 0 )THEN
        iparam = -999
      END IF
    END IF
  END IF

  line = line(j+1:)

  GetNextFilename = .TRUE.

END IF

9999 CONTINUE

RETURN
END
