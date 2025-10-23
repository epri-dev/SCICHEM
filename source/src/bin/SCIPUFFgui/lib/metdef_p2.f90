!***********************************************************************
!               Ckeck Meteorology Definition
!***********************************************************************
SUBROUTINE check_meteorology_definition( iwnd_db,id_level,lok )

USE resource_fd
USE mettype_fd
USE errorParam_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi

!     This routine processes PUSHBUTTONs from RELDEF Dialog Box

IMPLICIT NONE

!INTEGER, PARAMETER :: TRUE = 1

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog level (for data storage)
LOGICAL              lok !return value

LOGICAL check_terrain
LOGICAL UTMterrain, check_lsv, check_bl
INTEGER jd_level,lmap
INTEGER(POINTER_LEN) jwnd_db

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckMeteorology'

!==== MET Choice

check_terrain = .FALSE.
check_lsv     = .TRUE.
check_bl      = .TRUE.
SELECT CASE( idbcmbo(1,id_level)+met_offset )
  CASE( MET_MEDOC )
    CALL check_file( 'Gridded met',dbtext(6,id_level),dbtext(5,id_level) )
    check_terrain = lcheck(2,id_level)
  CASE( MET_WRF )
    CALL check_file( 'Gridded met',dbtext(6,id_level),dbtext(5,id_level) )
  CASE( MET_ASSIM )
    CALL check_file( 'List of Grid/Obs met',dbtext(10,id_level),dbtext(9,id_level) )
  CASE( MET_MEDLIS )
    CALL check_file( 'List of gridded met',dbtext(10,id_level),dbtext(9,id_level) )
  CASE( MET_MRF )
    CALL check_file( 'Gridded met',dbtext(6,id_level),dbtext(5,id_level) )
    check_terrain = lcheck(2,id_level)
  CASE( MET_OBS )
    CALL check_file( 'Surface met',dbtext(4,id_level),dbtext(3,id_level) )
    IF( .NOT.hasError() )THEN
      CALL check_file( 'Upper Air met',dbtext(2,id_level),dbtext(1,id_level) )
    END IF
    IF( dbint(1,id_level) <= 0 )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a maximum number of nearest data points'
    END IF
    IF( dbint(2,id_level) <= 0 )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a maximum number of nearest data points'
    END IF
    check_terrain = lcheck(2,id_level)
  CASE( MET_SRF )
    CALL check_file( 'Surface met',dbtext(4,id_level),dbtext(3,id_level) )
    IF( dbint(2,id_level) <= 0 )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a maximum number of nearest data points'
    END IF
    check_terrain = lcheck(2,id_level)
  CASE( MET_UAIR )
    CALL check_file( 'Upper Air met',dbtext(2,id_level),dbtext(1,id_level) )
    IF( dbint(1,id_level) <= 0 )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a maximum number of nearest data points'
    END IF
    check_terrain = lcheck(2,id_level)
  CASE( MET_FIXED )
    IF( ABS(dbreal(12,id_level)) == DEF_VAL_R )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a wind speed'
    END IF
    IF( ABS(dbreal(13,id_level)) == DEF_VAL_R )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a wind direction'
    END IF
    check_terrain = lcheck(2,id_level)
  CASE DEFAULT
    nError   = IV_ERROR
    eMessage = 'Please specify a data type'
END SELECT
IF( hasError() .OR. nError /= NO_ERROR )GOTO 9999


!==== Surface roughness/Canopy

IF( ichoice(1,id_level) == 1 )THEN
  IF( ABS(dbreal(1,id_level)) == DEF_VAL_R )THEN
    nError   = IV_ERROR
    eMessage = 'Please specify a surface roughness'
  END IF
  IF( dbreal(1,id_level) <= 0.0 )THEN
    nError   = IV_ERROR
    eMessage = 'Please specify a surface roughness > 0.0'
  END IF
ELSE
  IF( ABS(dbreal(2,id_level)) == DEF_VAL_R )THEN
    nError   = IV_ERROR
    eMessage = 'Please specify a canopy height'
  END IF
  IF( dbreal(2,id_level) <= 0.0 )THEN
    nError   = IV_ERROR
    eMessage = 'Please specify a canopy height > 0.0'
  END IF
END IF
IF( nError /= NO_ERROR )GOTO 9999

!==== Boundary Layer

IF( check_bl )THEN
  SELECT CASE( idbcmbo(2,id_level)+bl_offset )
    CASE( BL_CALC,BL_OPERATIONAL )
      IF( ABS( dbreal(7,id_level) ) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a Bowen ratio'
      ELSE IF( dbreal(7,id_level) < 0.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a positive Bowen ratio'
      END IF
      IF( ABS(dbreal(8,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a surface albedo'
      ELSE IF( dbreal(8,id_level) < 0.0 .OR. dbreal(8,id_level) > 1.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a surface albedo between 0.0 and 1.0'
      END IF
      IF( ABS(dbreal(9,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a cloud cover fraction'
      ELSE IF( dbreal(9,id_level) < 0.0 .OR. dbreal(9,id_level) > 1.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a cloud cover fraction between 0.0 and 1.0'
      END IF
    CASE( BL_NONE,BL_OBS,BL_PROFILE,BL_MEDOC )
    CASE( BL_SIMPLE )
      IF( ABS(dbreal(3,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a minimum Zi'
      END IF
      IF( ABS(dbreal(4,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a maximum Zi'
      END IF
      IF( dbreal(3,id_level) <= 0.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a positive minimum Zi'
      END IF
      IF( dbreal(4,id_level) < dbreal(3,id_level) )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify the maximun Zi > minimum Zi'
      END IF
      IF( ABS(dbreal(5,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a minimum heat flux'
      END IF
      IF( ABS(dbreal(6,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a maximum heat flux'
      END IF
    CASE DEFAULT
      nError   = IV_ERROR
      eMessage = 'Please specify a boundary layer type'
  END SELECT
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Large Scale Variability

IF( check_lsv )THEN
  SELECT CASE( idbcmbo(3,id_level)+lsv_offset )
    CASE( LSV_INPUT )
      IF( ABS(dbreal(10,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a LSV length scale'
      ELSE IF( dbreal(10,id_level) <= 0.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a positive LSV length scale'
      END IF
      IF( ABS(dbreal(11,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a LSV variance'
      ELSE IF( dbreal(11,id_level) <= 0.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a positive LSV variance'
      END IF
    CASE( LSV_MODEL,LSV_NONE,LSV_OPERATIONAL )
    CASE( LSV_OBS )
      IF( ABS(dbreal(10,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a LSV length scale'
      ELSE IF( dbreal(10,id_level) <= 0.0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a positive LSV length scale'
      END IF
    CASE DEFAULT
      nError   = IV_ERROR
      eMessage = 'Please specify a large scale variance type'
  END SELECT
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Terrain

IF( check_terrain )THEN
  CALL check_file( 'Terrain',dbtext(8,id_level),dbtext(7,id_level) )
  IF( .NOT.hasError() )THEN
    CALL FindHwndListId( IDB_EDTPRJ,jwnd_db,jd_level )
    lmap = dlgDomain(jd_level)%spatial%domain%coord
    CALL check_UTM( dbtext(8,id_level),dbtext(7,id_level),lmap,UTMterrain )
    IF( UTMterrain )CALL InitError() !discard prj/ter mismatch errors
    IF( hasError() )GOTO 9999
      IF( dbint(3,id_level) < 0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a maximum number of FFT iterations'
      END IF
      IF( dbint(4,id_level) < 0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a maximum number of Point-relaxation iterations'
      END IF
      IF( dbint(5,id_level) < 0 )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a vertical grid'
      END IF
      IF( ABS(dbreal(16,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a minimum vertical adjustment parameter'
      END IF
      IF( ABS(dbreal(17,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a maximum vertical adjustment parameter'
      END IF
      IF( dbreal(16,id_level) > dbreal(17,id_level) )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid vertical adjustment parameter : Minimum > Maximum'
      END IF
      IF( ABS(dbreal(18,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a FFT error criteria'
      END IF
      IF( ABS(dbreal(19,id_level)) == DEF_VAL_R )THEN
        nError   = IV_ERROR
        eMessage = 'Please specify a Point-relaxation error criteria'
      END IF
  END IF
  IF( lcheck(3,id_level) )THEN
    IF( .NOT.lcheck(6,id_level).AND..NOT.lcheck(7,id_level) )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify 2D and/or 3D fields for met output'
    END IF
  END IF
ELSE
  IF( lcheck(3,id_level) )THEN
    IF( dbreal(21,id_level) == NOT_SET_R )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify a met output frequency'
    END IF
    IF( .NOT.lcheck(6,id_level).AND..NOT.lcheck(7,id_level) )THEN
      nError   = IV_ERROR
      eMessage = 'Please specify 2D and/or 3D fields for met output'
    END IF
  END IF
END IF
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE
lok = nError == NO_ERROR
IF( .NOT. lok )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
IF( hasError() )CALL ShowErrorMessage( iwnd_db )

RETURN
END
!***********************************************************************
!               Check file
!***********************************************************************
SUBROUTINE check_file( filetype,path,file )

USE errorParam_fd
USE DefSize_fd

IMPLICIT NONE

CHARACTER(*) filetype
CHARACTER(*) path
CHARACTER(*) file

CHARACTER(128) eString

CHARACTER(PATH_MAXLENGTH) filename,StripNull
LOGICAL CheckFile_NoError

filename = TRIM(StripNull(file))
CALL AddPath( filename,StripNull(path) )
IF( filename == ' ' .OR. LEN(TRIM(filename)) <= 0 )THEN
  CALL SetError( IV_ERROR, &
                'Please specify a '//TRIM(filetype)//' file',' ',' ', &
                'CheckMetFile' )
ELSE
  IF( .NOT.CheckFile_NoError(filename) )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to find '//TRIM(filetype)//' file', &
                  eString,' ', &
                  'CheckMetFile' )
  END IF
END IF

RETURN
END
!***********************************************************************
!               MeteorologyCombo
!***********************************************************************
SUBROUTINE meteorology_combo( iwnd_db,id_button,id_level,lok )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE errorParam_fd
USE dialog_fi
USE param_fd
USE basic_fd

!     This routine processes COMBOBOXes from the MATDEF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_button !Button ID number
INTEGER              id_level !Dialog level (for data storage)
LOGICAL              lok

LOGICAL lflag
CHARACTER(128), EXTERNAL :: StripNull

INTEGER irv, i, ichck

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckMeteorology'

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 ) !Changing Data type
    IF( idbcmbo(1,id_level)+met_offset == MET_FIXED )THEN
    ELSE IF( idbcmbo(1,id_level)+met_offset == MET_MEDOC  .OR. &
             idbcmbo(1,id_level)+met_offset == MET_MRF    .OR. &
             idbcmbo(1,id_level)+met_offset == MET_FIXED )THEN
      IF( idbcmbo(2,id_level)+bl_offset == BL_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Boundary Layer option is Observations'
      ELSE IF( idbcmbo(3,id_level)+lsv_offset == LSV_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Large Scale Variability option is Observations'
      END IF
    ELSE IF( idbcmbo(1,id_level)+met_offset == MET_ASSIM )THEN
      IF( idbcmbo(2,id_level)+bl_offset == BL_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Boundary Layer option is Observations'
      ELSE IF( idbcmbo(2,id_level)+bl_offset == BL_PROFILE )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Boundary Layer option is Profile'
      ELSE IF( idbcmbo(2,id_level)+bl_offset == BL_MEDOC )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Boundary Layer option is MEDOC'
      END IF
      IF( idbcmbo(3,id_level)+lsv_offset == LSV_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Large Scale Variability option is Observations'
      END IF
      IF( lcheck(2,id_level) )THEN
        CALL SetError( WN_ERROR,'External terrain file is specified', &
                                'Terrain must be specified in the List grid/obs file', &
                                'Do you want to remove the external terrain specification and continue?', &
                                'CheckMeteorology' )
        CALL WarningMessage( .TRUE. )
        IF( hasError() )THEN
          nError   = IV_ERROR
          eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
          eInform = 'External terrain file is specified'
          eAction = ' '
        ELSE
          lcheck(2,id_level) = .FALSE.
        END IF
      END IF
    END IF
    IF( idbcmbo(9,id_level) == PC_MET )THEN
      IF( idbcmbo(1,id_level)+met_offset == MET_FIXED )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Precipitation option precludes fixed winds'
      END IF
    END IF
    IF( idbcmbo(2,id_level)+bl_offset == BL_PROFILE )THEN
      IF( idbcmbo(1,id_level)+met_offset /= MET_UAIR )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(1,id_level))
        eInform = 'Boundary Layer option Profile must use Upper Air'
      END IF
    ELSE IF( idbcmbo(2,id_level)+bl_offset == BL_MEDOC )THEN
      IF( .NOT.ANY(idbcmbo(1,id_level)+met_offset == (/MET_MEDOC,MET_ASSIM,MET_WRF,MET_MEDLIS/)) )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(2,id_level))
        eInform = 'Boundary Layer option MEDOC must use MEDOC gridded'
      END IF
    ELSE IF( idbcmbo(1,id_level)+met_offset == MET_MEDOC  .OR. &
             idbcmbo(1,id_level)+met_offset == MET_MRF    .OR. &
             idbcmbo(1,id_level)+met_offset == MET_FIXED )THEN
      IF( idbcmbo(2,id_level)+bl_offset == BL_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(2,id_level))
        eInform = 'Weather data type is '// &
             StripNull(dbcmbo(1,id_level))
      END IF
    ELSE IF( idbcmbo(9,id_level) /= PC_CLEAR )THEN
      IF( idbcmbo(2,id_level)+bl_offset == BL_NONE )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(2,id_level))
        eInform = 'Precipitation type is '// &
             StripNull(dbcmbo(9,id_level))
      END IF
    END IF
  CASE( 3 ) !Changing Ens
  lflag = idbcmbo(1,id_level)+met_offset == MET_MEDOC  .OR. &
          idbcmbo(1,id_level)+met_offset == MET_MRF    .OR. &
          idbcmbo(1,id_level)+met_offset == MET_FIXED
  lflag = lflag .OR. idbcmbo(1,id_level)+met_offset == MET_WRF
  IF( lflag )THEN
      IF( idbcmbo(3,id_level)+lsv_offset == LSV_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(3,id_level))
        eInform  = 'Weather data type is '// &
                         StripNull(dbcmbo(1,id_level))
      END IF
    END IF
    IF( idbcmbo(1,id_level)+met_offset == MET_ASSIM )THEN
      IF( idbcmbo(3,id_level)+lsv_offset == LSV_OBS )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(3,id_level))
        eInform = 'Weather data type is '// &
                          StripNull(dbcmbo(1,id_level))
      END IF
    END IF
  CASE( 9 ) !Changing Precip
    IF( idbcmbo(1,id_level)+met_offset == MET_FIXED )THEN
      IF( idbcmbo(9,id_level) == PC_MET )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(9,id_level))
        eInform = 'Weather data type is '// &
                          StripNull(dbcmbo(1,id_level))
      END IF
    END IF
    IF( idbcmbo(2,id_level)+bl_offset == BL_NONE )THEN
      IF( idbcmbo(9,id_level) /= PC_CLEAR )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid Choice : '//StripNull(dbcmbo(9,id_level))
        eInform = 'Boundary Layer type is '// &
                              StripNull(dbcmbo(2,id_level))
      END IF
    END IF
  CASE DEFAULT
END SELECT

lok = nError == NO_ERROR
IF( lok )THEN
  irv = TRUE
  ichck = 3
  DO i = 1,ichck
    IF( idbcmbo(i,id_level) <= 0)irv = FALSE
  END DO
  CALL EnableControl( iwnd_db,IDB_BUTTON13,irv )
  CALL update_met_edit( iwnd_db,id_level,1 )
ELSE
  eAction  = 'Please choose another Option'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
!***********************************************************************
!                MeteorologyRadio
!***********************************************************************
SUBROUTINE meteorology_radio( iwnd_db,igroup,id_level )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE winAPI_fd

!     This routine processes RADIOBUTTONs from METDEF Dialog Box

IMPLICIT NONE

!INTEGER, PARAMETER :: SW_SHOWNORMAL         = 1
!INTEGER, PARAMETER :: SW_HIDE               = 0
!INTEGER, PARAMETER :: TRUE                  = 1
!INTEGER, PARAMETER :: FALSE                 = 0

INTEGER(POINTER_LEN) iwnd_db !Window handle
INTEGER              igroup !Group number
INTEGER              id_level !Data level

!---- Select Action based on group number

SELECT CASE(igroup)
  CASE( 0 ) !Roughness Type
    CALL show_met_srf( iwnd_db,ichoice(1,id_level) )
  CASE( 1 ) !Time Type
  CASE( 3 ) !HIST type
  CASE( 4 ) !FCST type
    IF( ichoice(5,id_level) == FCST_GRID )THEN
      CALL EnableControl( iwnd_db,IDB_BUTTON16,TRUE )
      CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_STATIC86,SW_SHOWNORMAL )
      CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
      CALL ShowControl( iwnd_db,IDB_STATIC87,SW_HIDE )
    ELSE
      CALL EnableControl( iwnd_db,IDB_BUTTON16,FALSE )
      CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_HIDE )
      CALL ShowControl( iwnd_db,IDB_STATIC86,SW_HIDE )
      CALL EnableControl( iwnd_db,IDB_BUTTON17,TRUE )
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_STATIC87,SW_SHOWNORMAL )
    END IF
  CASE DEFAULT
END SELECT

RETURN
END
!*******************************************************************************
!            Initialize Terrain parameters Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_terrain( iwnd_db,id_level )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE randef
USE winAPI_fd
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

!INTEGER, PARAMETER :: FALSE                 = 0
!INTEGER, PARAMETER :: SW_HIDE               = 0

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Data level

CHARACTER(128) StripNull

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, indxd
CHARACTER(16) number
REAL      dfac
LOGICAL lAssim
INTEGER i

CALL FindHwndListId( IDB_METDEF,jwnd_db,jd_level ) !  Parent = METDEF

lAssim = idbcmbo(1,jd_level) + met_offset == MET_ASSIM

lcheck(1,id_level) = lcheck(3,jd_level)
lcheck(2,id_level) = lcheck(2,jd_level) .OR. lAssim
lcheck(3,id_level) = lcheck(6,jd_level)
lcheck(4,id_level) = lcheck(7,jd_level)
lcheck(5,id_level) = lcheck(8,jd_level)
lcheck(6,id_level) = lcheck(9,jd_level)
lcheck(7,id_level) = lcheck(10,jd_level)
lcheck(8,id_level) = lcheck(11,jd_level)
lcheck(9,id_level) = lcheck(12,jd_level)

nradio(1,id_level) = nradio(9,jd_level)
ichoice(1,id_level) = ichoice(9,jd_level)

dbreal(1,id_level) = dbreal(16,jd_level)
dbreal(2,id_level) = dbreal(17,jd_level)
dbreal(3,id_level) = dbreal(18,jd_level)
dbreal(4,id_level) = dbreal(19,jd_level)
dbreal(6,id_level) = dbreal(21,jd_level)
IF( dbreal(6,id_level) == NOT_SET_R .OR. dbreal(6,id_level) == DEF_VAL_R )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dbreal(6,id_level) == 0. .OR. dbreal(6,id_level) >= 1. )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dbreal(6,id_level) >= 1./60. )THEN
  dfac  = 60.
  indxd = 2
ELSE
  dfac  = 3600.
  indxd = 1
END IF
idbcmbo(2,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO2,id_level )
dbreal(6,id_level)  = dbreal(6,id_level)*dfac

dbint(1,id_level) = dbint(3,jd_level)
dbint(2,id_level) = dbint(4,jd_level)
dbint(3,id_level) = nlst(jd_level)

dbtext(1,id_level) = TRIM(dbtext(7,jd_level))
dbtext(2,id_level) = TRIM(dbtext(8,jd_level))

string1 = StripNull( dbtext(1,id_level) )
string2 = StripNull( dbtext(2,id_level) )
CALL SetControlText( iwnd_db,IDB_STATIC22,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC23,string2 )
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )
CALL SetEditRs( iwnd_db,dbreal(6,id_level),6,1 )
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,2 )
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,6 )
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

WRITE(number,*)dbint(3,id_level)
number = ADJUSTL(number)
CALL SetControlText( iwnd_db,IDB_STATIC62,number )
nlst(id_level) = 0
IF( dbint(3,id_level) > 0 )THEN
  CALL build_list_numericX( iwnd_db,IDB_LIST1,id_level, &
                           dbint(3,id_level),jd_level,1.0 )
ELSE !  Uniform - Clear
  CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )
END IF

IF( save_flag(1) == FALSE )THEN
  CALL EnableControl( iwnd_db,ID_OK,FALSE )
  CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK2,FALSE )
END IF

DO indxd = 41,42
  CALL ShowControl( iwnd_db,STATIC_BASE+indxd,SW_HIDE )
END DO
CALL EnableControl (iwnd_db,IDB_REAL5,FALSE )
CALL ShowControl( iwnd_db,IDB_REAL5,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO1,SW_HIDE )

!IF( idbcmbo(1,jd_level) + met_offset == MET_MEDOC )THEN
!  lcheck(2,id_level) = .FALSE.
!  CALL EnableControl(iwnd_db,IDB_CHECK2,FALSE)
!  CALL hide_terrain(iwnd_db)
!ELSE
  IF( lcheck(2,id_level) )THEN
    CALL show_terrain( iwnd_db,save_flag(1), &
                       lcheck(7,id_level),lcheck(8,id_level), &
                       lcheck(5,id_level),lcheck(6,id_level) )
    CALL update_terrain_buttons( iwnd_db,save_flag(1) )
    IF( lAssim )THEN
      CALL EnableControl( iwnd_db,IDB_CHECK2,FALSE )
      CALL ShowControl( iwnd_db,IDB_CHECK2,SW_HIDE )
      CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
      CALL ShowControl( iwnd_db,IDB_BUTTON2,SW_HIDE )
      DO i = 19,23
        CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
      END DO
    END IF
  ELSE
    CALL hide_terrain( iwnd_db )
  END IF
!END IF

IF( lcheck(1,id_level) )THEN
  CALL show_metsave( iwnd_db,id_level,save_flag(1) )
ELSE
  CALL hide_metsave( iwnd_db,id_level )
END IF

RETURN
END
!*******************************************************************************
!            Save Terrain parameters Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_terrain( id_level )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
!
!     This routine saves the terrain Dialog Box
!
IMPLICIT NONE

INTEGER id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level,i
LOGICAL lAssim

CALL FindHwndListId( IDB_METDEF,jwnd_db,jd_level ) !  Parent = METDEF

lAssim = idbcmbo(1,jd_level) + met_offset == MET_ASSIM

lcheck(3,jd_level) = lcheck(1,id_level)
IF( lcheck(3,jd_level) )THEN
  ichoice(9,jd_level) = ichoice(1,id_level)
  lcheck(6,jd_level)  = lcheck(3,id_level)
  lcheck(7,jd_level)  = lcheck(4,id_level)
  dbreal(21,jd_level) = dbreal(6,id_level)
  IF( dbreal(21,jd_level) == NOT_SET_R )THEN
    dbreal(21,jd_level) = NOT_SET_R
  ELSE IF( dbreal(21,jd_level) /= DEF_VAL_R )THEN
    SELECT CASE( idbcmbo(2,id_level) )
      CASE( 1 )
        dbreal(21,jd_level) = dbreal(21,jd_level)/3600.
      CASE( 2 )
        dbreal(21,jd_level) = dbreal(21,jd_level)/60.
      CASE DEFAULT
    END SELECT
  END IF
END IF

IF( lAssim )THEN
  lcheck(2,jd_level) = .FALSE.
ELSE
  lcheck(2,jd_level) = lcheck(2,id_level) .AND. &
               ((lcheck(5,id_level) .AND. lcheck(7,id_level)) .OR. &
                (lcheck(6,id_level) .AND. lcheck(8,id_level)))
END IF

IF( lcheck(2,jd_level) .OR. lAssim)THEN
  IF( .NOT.lAssim )THEN
    lcheck(8,jd_level)  = lcheck(5,id_level)
    lcheck(9,jd_level)  = lcheck(6,id_level)
    lcheck(10,jd_level) = lcheck(7,id_level)
    lcheck(11,jd_level) = lcheck(8,id_level)
    lcheck(12,jd_level) = lcheck(9,id_level)
    dbtext(7,jd_level)  = TRIM(dbtext(1,id_level))
    dbtext(8,jd_level)  = TRIM(dbtext(2,id_level))
  END IF

  dbreal(16,jd_level) = dbreal(1,id_level)
  dbreal(17,jd_level) = dbreal(2,id_level)
  dbreal(18,jd_level) = dbreal(3,id_level)
  dbreal(19,jd_level) = dbreal(4,id_level)

  dbint(3,jd_level) = dbint(1,id_level)
  dbint(4,jd_level) = dbint(2,id_level)
  dbint(5,jd_level) = nlst(id_level)

  nlst(jd_level) = nlst(id_level)
  DO i = 1,nlst(id_level)
    dblst(jd_level,i) = dblst(id_level,i)
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Update Terrain buttons
!*******************************************************************************
SUBROUTINE update_terrain_buttons( iwnd,ienable )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE basic_fd

!     This routine enables/disables the subgroup buttons

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog handle
INTEGER              ienable

INTEGER enable(7), ID, ilev
CHARACTER(32) number

!     Buttons

CALL FindHwndList( iwnd,ID,ilev )
WRITE(number,*)nlst(ilev)
string1 = ADJUSTL(number)
CALL SetControlText( iwnd,IDB_STATIC62,string1 )

IF( nlst(ilev) > 0 .AND. ienable /= FALSE )THEN !Edit
  enable(1) = TRUE
ELSE
  enable(1) = FALSE
END IF
IF( nlst(ilev) < MAXZBG .AND. ienable /= FALSE )THEN !Edit
  enable(2) = TRUE
ELSE
  enable(2) = FALSE
END IF
enable(3) = enable(1) !Delete
enable(4) = enable(1) !Clear all
enable(5) = enable(2) !Compute
enable(6) = enable(2) !Load
IF( nlst(ilev) > 0 .AND. ienable /= FALSE )THEN !Edit
  enable(7) = TRUE
ELSE
  enable(7) = FALSE
END IF
CALL EnableButtons( iwnd,enable,4,7 )

RETURN
END
!***********************************************************************
!               TerrainButton
!***********************************************************************
SUBROUTINE terrain_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE plotdlg_fi
USE create_fi
USE winAPI_fd

!     This routine processes PUSHBUTTONs from CONTOUR Dialog Box

IMPLICIT NONE

!INTEGER, PARAMETER :: FALSE                 = 0
!INTEGER, PARAMETER :: TRUE                  = 1
!INTEGER, PARAMETER :: SW_HIDE               = 0
!INTEGER, PARAMETER :: SW_SHOWNORMAL         = 1

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_dialog !Dialog ID number
INTEGER              id_button !Button ID number
INTEGER              id_level !Dialog level (for data storage)

LOGICAL lok, verify_button, ldum, luse, lcat
INTEGER idum
CHARACTER(40) cdum

CHARACTER(PATH_MAXLENGTH) filenam, StripNull, AddNull

LOGICAL, EXTERNAL :: hasError

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 2 ) !File
    lok = .FALSE.
    filenam = StripNull( dbtext(1,id_level) )
    string2 = StripNull( dbtext(2,id_level) )
    CALL AddPath( filenam,string2 )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL SplitName( filenam,string2,string1 )
      dbtext(1,id_level) = AddNull( TRIM(string2) )
      dbtext(2,id_level) = AddNull( TRIM(string1) )
      CALL SetControlText( iwnd_db,IDB_STATIC22,string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC23,string1 )
      CALL check_ter_header( I_LATLON,filenam,ldum,cdum,idum,luse,lcat )
      IF( hasError() )THEN
        CALL ShowErrorMessage( hwnd_mw )
        lcheck(7,id_level) = .FALSE.
        lcheck(8,id_level) = .FALSE.
        lcheck(9,id_level) = .FALSE.
      ELSE
        lcheck(7,id_level) = .TRUE.
        lcheck(8,id_level) = luse
        lcheck(9,id_level) = lcat
      END IF
      IF( lcheck(7,id_level) )THEN
        CALL ShowControl( iwnd_db,IDB_CHECK5,SW_SHOWNORMAL )
        CALL EnableControl( iwnd_db,IDB_CHECK5,TRUE )
      ELSE
        CALL ShowControl( iwnd_db,IDB_CHECK5,SW_HIDE )
        CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
      END IF
      IF( lcheck(8,id_level) )THEN
        CALL ShowControl( iwnd_db,IDB_CHECK6,SW_SHOWNORMAL )
        CALL EnableControl( iwnd_db,IDB_CHECK6,TRUE )
      ELSE
        CALL ShowControl( iwnd_db,IDB_CHECK6,SW_HIDE )
        CALL EnableControl( iwnd_db,IDB_CHECK6,FALSE )
      END IF
    END IF
  CASE( 4 ) !EDIT Size Bin
    listedt%dialog = IDB_TERPARM
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_EDTLST )
    CALL update_terrain_buttons( iwnd_db,TRUE )
  CASE( 5 ) !NEW Size Bin
    listedt%dialog = IDB_TERPARM
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_NEWLST )
    CALL update_terrain_buttons( iwnd_db,TRUE )
  CASE( 6 ) !DELETE Size Bin
    CALL delete_list_numeric( iwnd_db,IDB_LIST1,id_level )
    CALL update_terrain_buttons( iwnd_db,TRUE )
  CASE( 7 ) !CLEAR ALL Size Bin
    string1 = 'Delete ALL grid definitions'
    lok = verify_button( iwnd_db,TRIM(string1) )
    IF( lok )THEN
      nlst(id_level) = 0
      CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )
      CALL update_terrain_buttons( iwnd_db,TRUE )
    END IF
  CASE( 8 ) !COMPUTE Size bins
    listedt%dialog = IDB_TERPARM
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_COMLST )
    CALL update_terrain_buttons( iwnd_db,TRUE )
  CASE( 9 ) !LOAD Size bins from file
    lok = .FALSE.
    filenam = TRIM(loadfile(11))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL file_list_numeric( iwnd_db,id_dialog,IDB_LIST1,id_level, &
                                       TRIM(filenam),1.0 ) !  Read Size bins
      CALL update_terrain_buttons( iwnd_db,TRUE )
      loadfile(11) = TRIM(filenam)
    END IF
  CASE( 10 ) !SAVE Size bins to file
    lok = .FALSE.
    filenam = TRIM(loadfile(11))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL save_list_numeric( iwnd_db,id_dialog,id_level, &
                                       TRIM(filenam),1.0 ) !  write Size bins
      loadfile(11) = TRIM(filenam)
    END IF
END SELECT

RETURN
END
!***********************************************************************
!               ValidDomain
!***********************************************************************
SUBROUTINE ValidDomain( ilev,lok )

USE resource_fd
USE mettype_fd
USE defineok_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE param_fd

IMPLICIT NONE

INTEGER ilev
LOGICAL lok, lvalid
LOGICAL, EXTERNAL :: hasError

lok = .FALSE.

!==== Is Domain defined

lvalid = BTEST(DefinedOK,DF_DOMAIN)
IF( .NOT.lvalid )GOTO 9999

!==== Is Domain valid

lvalid = dlgDomain(ilev)%spatial%domain%xMin /= DEF_VAL_R .AND. &
         dlgDomain(ilev)%spatial%domain%xMin /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgDomain(ilev)%spatial%domain%xMax /= DEF_VAL_R .AND. &
         dlgDomain(ilev)%spatial%domain%xMax /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgDomain(ilev)%spatial%domain%yMin /= DEF_VAL_R .AND. &
         dlgDomain(ilev)%spatial%domain%yMin /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgDomain(ilev)%spatial%domain%yMax /= DEF_VAL_R .AND. &
         dlgDomain(ilev)%spatial%domain%yMax /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

!==== If Domain is cartesian Is Reference point set and valid

IF( dlgDomain(ilev)%spatial%domain%coord == I_CARTESIAN .OR. &
    dlgDomain(ilev)%spatial%domain%coord == I_METERS )THEN
  lvalid = dlgDomain(ilev)%hasReference
  IF( .NOT.lvalid )GOTO 9999

  lvalid = dlgDomain(ilev)%spatial%reference%lon /= DEF_VAL_R .AND. &
           dlgDomain(ilev)%spatial%reference%lon /= NOT_SET_R
  IF( .NOT.lvalid )GOTO 9999

  lvalid = dlgDomain(ilev)%spatial%reference%lat /= DEF_VAL_R .AND. &
           dlgDomain(ilev)%spatial%reference%lat /= NOT_SET_R
  IF( .NOT.lvalid )GOTO 9999

  lvalid = dlgDomain(ilev)%spatial%reference%x /= DEF_VAL_R .AND. &
           dlgDomain(ilev)%spatial%reference%x /= NOT_SET_R
  IF( .NOT.lvalid )GOTO 9999

  lvalid = dlgDomain(ilev)%spatial%reference%y /= DEF_VAL_R .AND. &
           dlgDomain(ilev)%spatial%reference%y /= NOT_SET_R
  IF( .NOT.lvalid )GOTO 9999
END IF

!==== If Domain is UTM is zone set.  If so set the Reference point

IF( dlgDomain(ilev)%spatial%domain%coord == I_UTM )THEN
  lvalid = dlgDomain(ilev)%spatial%domain%zoneUTM /= NOT_SET_I .AND. &
           dlgDomain(ilev)%spatial%domain%zoneUTM /= DEF_VAL_I
  IF( .NOT.lvalid )GOTO 9999
  CALL set_utm_reference( ilev )
  lvalid = .NOT.hasError()
  CALL InitError()
  IF( .NOT.lvalid )GOTO 9999
END IF

lok = .TRUE.
9999  RETURN
END
!C***********************************************************************
!               ValidTime
!***********************************************************************
SUBROUTINE ValidTime( ilev,lok )

USE defineok_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE default_fd

IMPLICIT NONE

INTEGER ilev
LOGICAL lok, lvalid, check_YMD

lok = .FALSE.

!==== Is Time defined

lvalid = BTEST(DefinedOK,DF_TIME)
IF( .NOT.lvalid )GOTO 9999

!==== Is Start time valid

lvalid = check_YMD( dlgTime(ilev)%time%start%time )
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%start%time%year /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%start%time%year /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%start%time%month /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%start%time%month /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%start%time%day /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%start%time%day /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%start%time%hour /= DEF_VAL_R .AND. &
         dlgTime(ilev)%time%start%time%hour /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%start%time%runTime /= DEF_VAL_R .AND. &
         dlgTime(ilev)%time%start%time%runTime /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

IF( dlgTime(ilev)%time%start%time%reference == HT_LOCAL )THEN
  lvalid = dlgTime(ilev)%time%start%zone /= DEF_VAL_R .AND. &
           dlgTime(ilev)%time%start%zone /= NOT_SET_R
  IF( .NOT.lvalid )GOTO 9999
END IF

!==== Is End time valid

lvalid = check_YMD( dlgTime(ilev)%time%end%time )
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%end%time%year /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%end%time%year /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%end%time%month /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%end%time%month /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%end%time%day /= DEF_VAL_I .AND. &
         dlgTime(ilev)%time%end%time%day /= NOT_SET_I
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%end%time%hour /= DEF_VAL_R .AND. &
         dlgTime(ilev)%time%end%time%hour /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lvalid = dlgTime(ilev)%time%end%time%runTime /= DEF_VAL_R .AND. &
         dlgTime(ilev)%time%end%time%runTime /= NOT_SET_R
IF( .NOT.lvalid )GOTO 9999

lok = .TRUE.

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Compute Operational Time
!*******************************************************************************
SUBROUTINE ComputeOperTime( starttime,endtime,zone,dt )

USE resource_fd
USE mettype_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE SwinAPI

IMPLICIT NONE

TYPE( timeT ) endtime, starttime, start, end, current

REAL    dt, zone
LOGICAL ltimOK

TYPE( T_SYSTEMTIME ) stime

!==== Set start time to current time (GMT)

CALL GetSystemTime( stime )
current%year  = stime%wYear
current%month = stime%wMonth
current%day   = stime%wDay
current%hour  = FLOAT(stime%wHour) + &
                FLOAT(stime%wMinute*60 + stime%wSecond)/3600.
current%reference = HT_UTC

!==== Set start and end times and convert to GMT

start = starttime
CALL LocalToGMT( start,zone )

END = endtime
CALL LocalToGMT( end,zone )

!==== check to see if starting before current tim

CALL ComputeDurationGUI( current,start,ltimOK )

!==== OK -> start of run > current

IF( ltimOK )THEN

!====== Set end to end of run and compute delta between current and end of run

  CALL ComputeDurationGUI( current,end,ltimOK )
  dt = end%runTime

!==== Not OK -> start of run < current

ELSE

!====== save delta between start of run and current

  CALL ComputeDurationGUI( start,current,ltimOK )

!====== check to see if end > current

  CALL ComputeDurationGUI( current,end,ltimOK )

!====== OK -> end of run > current

  IF( ltimOK )THEN

!======== set delta to larger of the two

    IF( current%runTime > end%runTime )THEN
      dt = -current%runTime !start of run < current
    ELSE
      dt = end%runTime
    END IF

!====== Not OK -> end of run < current
  ELSE

!======== Use delta between start of run and current

    dt = -current%runTime

  END IF

END IF

RETURN
END
!***********************************************************************
!               Check UTM
!***********************************************************************
SUBROUTINE check_UTM( path,file,lmap,lUTM )

USE resource_fd
USE mettype_fd
USE errorParam_fd
USE files_fi
USE param_fd

IMPLICIT NONE

CHARACTER(*) path
CHARACTER(*) file
INTEGER     lmap
LOGICAL     lUTM

CHARACTER(8)   ter_map

INTEGER nskip
LOGICAL luse, lcat

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

LOGICAL, EXTERNAL :: hasError

lUTM = .FALSE.

filename = TRIM(StripNull(file))
CALL AddPath( filename,TRIM(StripNull(path)) )

CALL check_ter_header( lmap,filename,lUTM,ter_map,nskip,luse,lcat )
IF( hasError() )GOTO 9999

IF( lUTM )THEN
  nError   = IV_ERROR
  eRoutine = 'CheckTerrainCoordinates'
  IF( lmap == I_UTM )THEN
    eMessage = 'Selected terrain file does not contain UTM coordinates'
    eInform  = 'UTM projects require UTM terrain data'
  ELSE IF( lmap == I_LATLON )THEN
    eMessage = 'Selected terrain file does not contain LLA coordinates'
    eInform  = 'LLA projects require LLA terrain data'
  ELSE IF( lmap == I_CARTESIAN )THEN
    eMessage = 'Selected terrain file does not contain CARTESIAN coordinates'
    eInform  = 'CARTESIAN projects require CARTESIAN terrain data'
  ELSE
    eMessage = 'Terrain file does not contain proper coordinates'
    eInform  = 'Current project coordinates incompatible with selected terrain data'
  END IF
  eAction  = 'Select a different terrain file'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE check_ter_header( lmap_prj,filename,lerr,ter_map,nskip,luse,lcat )

USE coordinate_fd
USE files_fi
USE errorParam_fd

!------ check terrain file header for project compatibility

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: lmap_prj
LOGICAL,      INTENT( OUT ) :: lerr
CHARACTER(*), INTENT( IN  ) :: filename
CHARACTER(*), INTENT( OUT ) :: ter_map
LOGICAL,      INTENT( OUT ) :: luse
LOGICAL,      INTENT( OUT ) :: lcat
INTEGER,      INTENT( OUT ) :: nskip

CHARACTER(8) cmap
CHARACTER(80) line

INTEGER ios, i, lmap_met, imax, jmax
REAL    dum

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckTerrainHeader'

!------ open terrain file

OPEN(lun_ter,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eMessage = 'Error opening terrain file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

READ(lun_ter,'(A80)',IOSTAT=ios) line
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eMessage = 'Error reading terrain file (1st record)'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

CALL cupper( line )

IF( INDEX(line,'LAND_USE') /= 0 )THEN
  READ(lun_ter,'(A8)',IOSTAT=ios) ter_map
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eMessage = 'Error reading terrain file (2nd record)'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF
  CALL cupper( ter_map )
  luse = .TRUE.
  lcat = INDEX(line,'CATEGORY') /= 0
ELSE
  ter_map = line(1:8)
  luse = .FALSE.
  lcat = .FALSE.
END IF

IF( TRIM(ter_map) == 'SCALED' )THEN
  ter_map ='METERS'
END IF

!------ set map parameters

SELECT CASE( TRIM(ter_map) )
  CASE( 'LATLON' )
    lmap_met = I_LATLON
  CASE( 'M','KM','METERS' )
    lmap_met = I_CARTESIAN
  CASE( 'UTM' )
    lmap_met = I_UTM
  CASE DEFAULT
    nError   = IV_ERROR
    eMessage = 'Invalid map type on terrain file (1st record)'
    eInform  = 'Valid types are LATLON, M, METERS, KM or UTM'
    CALL ReportFileName( eAction,'File=',filename )
    GOTO 9999
END SELECT

!------ checks for UTM

lerr  = .FALSE.
nskip = 0

IF( lmap_prj == I_UTM .AND. lmap_met /= I_UTM )THEN
  lerr = .TRUE.
ELSE IF( lmap_prj == I_CARTESIAN .AND. lmap_met == I_UTM )THEN
  lerr = .TRUE.
ELSE IF( lmap_prj == I_METERS .AND. lmap_met == I_UTM )THEN
  lerr = .TRUE.
ELSE IF( lmap_prj == I_LATLON .AND. lmap_met == I_UTM )THEN
  READ(lun_ter,*,IOSTAT=ios) dum,dum,dum,dum,imax,jmax
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eMessage = 'Error reading terrain file (2nd record)'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF
  nskip = (imax*jmax+11)/12
  DO i = 1,nskip
    READ(lun_ter,*,IOSTAT=ios)
  END DO
  READ(lun_ter,'(a8)',IOSTAT=ios) cmap
  IF( ios == 0 )THEN
    ter_map = cmap
    CALL cupper( ter_map )
    IF( TRIM(ter_map) /='LATLON' )THEN
      lerr = .TRUE.
    END IF
  ELSE
    lerr = .TRUE.
  END IF
END IF

9999 CONTINUE
IF( nError /= NO_ERROR )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )

CLOSE(lun_ter,IOSTAT=ios)

RETURN
END

!***********************************************************************
!                Convert SCIP 2.0 met
!***********************************************************************
SUBROUTINE convert_SCIP20_met( iwnd_db,jversion,met,prj )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE param_fd
USE SCIAPIversion_fd

IMPLICIT NONE

INTEGER(POINTER_LEN)     iwnd_db
INTEGER                  jversion
TYPE( METDEF_DLG )       met
TYPE( ProjectStructure ) prj

LOGICAL check_slhazard

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'ConvertSCIP20Met'

check_slhazard = .FALSE.

!==== Check for LSV=OBS

IF( met%lsv == LSV_OBS )THEN
  IF( jversion <= 0 )THEN
    CALL SetError( IV_ERROR, &
                  'SCIP 2.0 LSV=OBSERVATION option is no longer supported', &
                  'Met uncertainty is accessed through the Hazard Area option', &
                  'Is this SCIP 2.0 met that should be converted?', &
                   eRoutine )
    CALL ShowWarningMessage( iwnd_db,.FALSE. )
    IF( .NOT.hasError() )THEN
      met%lsv = LSV_NONE
      check_slhazard = .TRUE.
    ELSE
      CALL InitError()
    END IF
    eRoutine = 'ConvertSCIP20Met'
  ELSE IF( jversion < SCIAPI_30_VERSION )THEN
    met%lsv = LSV_NONE
    check_slhazard = .TRUE.
    nError   = IV_ERROR
    eMessage = 'SCIP 2.0 LSV=OBSERVATION option is no longer supported'
    eInform  = 'Met uncertainty is accessed through the Hazard Area option'
  END IF
END IF

!==== Check for LSV=OPERATIONAL

IF( met%lsv == LSV_OPER2_0 )THEN
  met%lsv = LSV_NONE
  check_slhazard = .TRUE.
  nError = IV_ERROR
  eMessage = 'SCIP 2.0 LSV=OPERATIONAL option is no longer supported'
  eInform  = 'Met uncertainty is accessed through the Hazard Area option'
END IF

SELECT CASE( met%met )
  CASE DEFAULT
END SELECT

IF( check_slhazard )THEN
  IF( met%met == MET_OBS )met%slhazard = met%slb
END IF

!==== Report conversions

IF( nError /= NO_ERROR )THEN
  eAction  = 'This option has been converted to the current methodology'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
