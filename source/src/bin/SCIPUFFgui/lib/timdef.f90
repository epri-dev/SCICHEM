!MODULE tim_winty
!
!  INTEGER, PARAMETER :: FALSE         = 0
!  INTEGER, PARAMETER :: TRUE          = 1
!  INTEGER, PARAMETER :: SW_HIDE       = 0
!  INTEGER, PARAMETER :: SW_SHOWNORMAL = 1
!
!END MODULE tim_winty
!*******************************************************************************
!            Initialize TIME Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_time( iwnd_db,id_level )

USE resource_fd
!USE tim_winty
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE winAPI_fd
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER   enable(2),indxd,irv,i
REAL      dfac
!
!     Buttons
!
enable(1) = project(EDIT_LEVEL_1)%Edit !LOAD
enable(2) = project(EDIT_LEVEL_1)%Edit !DEFAULT
CALL EnableButtons( iwnd_db,enable,14,2 )
enable(1) = project(EDIT_LEVEL_1)%Edit !OK
CALL EnableButtons( iwnd_db,enable,1,1 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON1 ,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON14,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON15,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_1)%ID%name)//' TIME'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF
!
!     Radio Buttons
!
nradio(1,id_level)  = 2 !Time Type
IF( dlgTime(EDIT_LEVEL_2)%time%start%time%reference == HT_LOCAL )THEN
  ichoice(1,id_level) = 2
ELSE
  ichoice(1,id_level) = 1
END IF
!
!     Edit Boxes - Reals
!
dbreal(2,id_level) = dlgTime(EDIT_LEVEL_2)%time%start%time%hour !Start Time
dbreal(3,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%time%hour

IF( dlgTime(EDIT_LEVEL_2)%time%end%time%runTime == NOT_SET_R )THEN
  dfac = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%time%runTime == 0. .OR. dlgTime(EDIT_LEVEL_2)%time%end%time%runTime >= 1. )THEN
  dfac = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%time%runTime >= 1./60. )THEN
  dfac = 60.
  indxd = 2
ELSE
  dfac = 3600.
  indxd = 1
END IF
idbcmbo(1,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO1,id_level )
dbreal(4,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%time%runTime*dfac !Duration

IF( dlgTime(EDIT_LEVEL_2)%time%end%step%max == NOT_SET_R )THEN
  dfac = 1.
  indxd = 1
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%step%max == 0. .OR. dlgTime(EDIT_LEVEL_2)%time%end%step%max <= 300. )THEN
  dfac = 1.
  indxd = 1
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%step%max <= 1200. )THEN
  dfac = 60.
  indxd = 2
ELSE
  dfac = 3600.
  indxd = 3
END IF
idbcmbo(2,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO2,id_level )
dbreal(5,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%step%max/dfac !Max Time Step

IF( dlgTime(EDIT_LEVEL_2)%time%end%step%output == NOT_SET_R .OR. dlgTime(EDIT_LEVEL_2)%time%end%step%output == DEF_VAL_R )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%step%output == 0. .OR. dlgTime(EDIT_LEVEL_2)%time%end%step%output >= 1. )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%end%step%output >= 1./60. )THEN
  dfac  = 60.
  indxd = 2
ELSE
  dfac  = 3600.
  indxd = 1
END IF

idbcmbo(3,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO3,id_level )
dbreal(6,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%step%output*dfac !Output interval
lcheck(1,id_level) = dlgTime(EDIT_LEVEL_2)%default_save
!
!     Edit Boxes - Integers
!
dbint(1,id_level) = dlgTime(EDIT_LEVEL_2)%time%start%time%year
dbint(2,id_level) = dlgTime(EDIT_LEVEL_2)%time%start%time%month
dbint(3,id_level) = dlgTime(EDIT_LEVEL_2)%time%start%time%day
dbint(4,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%time%year
dbint(5,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%time%month
dbint(6,id_level) = dlgTime(EDIT_LEVEL_2)%time%end%time%day

IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == DEF_VAL_R )THEN
  dbint(7,id_level) = DEF_VAL_I
  dbint(8,id_level) = DEF_VAL_I
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == NOT_SET_R )THEN
  dbint(7,id_level) = NOT_SET_I
  dbint(8,id_level) = NOT_SET_I
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == DEFERRED_R )THEN
  dbint(7,id_level) = DEFERRED_I
  dbint(8,id_level) = DEFERRED_I
ELSE
  dbint(7,id_level) = INT(dlgTime(EDIT_LEVEL_2)%time%start%zone)
  dbint(8,id_level) = NINT(60.*(dlgTime(EDIT_LEVEL_2)%time%start%zone - FLOAT(dbint(7,id_level))))
  IF( dbint(8,id_level) > 59 )THEN
    dbint(8,id_level) = dbint(8,id_level) - 60
    dbint(7,id_level) = dbint(7,id_level) + 1
  END IF
END IF

IF( project(EDIT_LEVEL_2)%Restart )THEN
  CALL ClearList( iwnd_db,IDB_COMBO4 )
  DO i = 1,nTimeRestart
    CALL format_time( timeRestart(i)%time%runTime,string1,0 )
    CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv ) !Put String in COMBO BOX
  END DO
  idbcmbo(4,id_level) = project(EDIT_LEVEL_2)%RestartTimeIndx
  CALL format_time( timeRestart(idbcmbo(4,id_level))%time%runTime,string1,0 )
  dbcmbo(4,id_level) = TRIM(string1)
  CALL SetListSelString( iwnd_db,IDB_COMBO4,dbcmbo(4,id_level),irv ) !  Select this time in COMBO BOX
  CALL SplitName(project( EDIT_LEVEL_2)%RestartFile,string2,string1 )
  CALL SetControlText( iwnd_db,IDB_STATIC33,string2 )
  CALL SetControlText( iwnd_db,IDB_STATIC35,string1 )
ELSE
  idbcmbo(4,id_level) = NOT_SET_I
  dbcmbo(4,id_level) = ' '
END IF

CALL load_dialog_time( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Load TIME Dialog Box
!*******************************************************************************
SUBROUTINE load_dialog_time( iwnd_db,id_level )

USE resource_fd
!USE tim_winty
USE pcscipuf_fi
USE files_fi
USE winAPI_fd
!
!     This routine loads the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i

CALL SetEditRs( iwnd_db,dbreal(2,id_level),2,5 )
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,6 )
CALL FormatEditIs( iwnd_db,'(I2.2)',dbint(7,id_level),7,2 )
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  DO i = 2,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  END DO
  DO i = 1,8
    CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  END DO
  DO i = 1,nradio(1,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 1,4
    CALL EnableControl( iwnd_db,COMBO_BASE+i,FALSE )
  END DO
ELSE
  IF( project(EDIT_LEVEL_2)%Restart )THEN
    DO i = 1,3
      CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
    END DO
    DO i = 7,8
      CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,IDB_REAL2,FALSE )
    DO i = 1,nradio(1,id_level)
      CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
    END DO
  ELSE
    CALL EnableControl( iwnd_db,IDB_COMBO4,FALSE )
  END IF
END IF
IF( .NOT.project(EDIT_LEVEL_2)%Restart )THEN
  CALL ShowControl( iwnd_db,IDB_COMBO4,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE )
  DO i = 32,36
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Save TIME Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_time( id_level )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
!
!     This routine saves the NEW Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER  i,j

LOGICAL check_YMD

TYPE( TIME_DLG ) timtmp

REAL x

timtmp = dlgTime(EDIT_LEVEL_2)
!
!     Check Boxes
!
!
!     Radio Buttons
!
IF( .NOT.project(EDIT_LEVEL_2)%Restart )THEN
  IF( ichoice(1,id_level) == 2 )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%time%reference = HT_LOCAL
  ELSE
    dlgTime(EDIT_LEVEL_2)%time%start%time%reference = HT_UTC
  END IF
  dlgTime(EDIT_LEVEL_2)%time%end%time%reference = dlgTime(EDIT_LEVEL_2)%time%start%time%reference
  i = dbint(7,id_level)
  j = dbint(8,id_level)
  IF( i == NOT_SET_I .OR. j == NOT_SET_I )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%zone = NOT_SET_R
  ELSE IF( i == DEF_VAL_I .OR. j == DEF_VAL_I )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%zone = DEF_VAL_R
  ELSE IF( i == DEFERRED_I .OR. j == DEFERRED_I )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%zone = DEFERRED_R
  ELSE
    dlgTime(EDIT_LEVEL_2)%time%start%zone = FLOAT(i) + FLOAT(j)/60.
  END IF
END IF
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!      dlgTime(EDIT_LEVEL_2).start.zone   = dbreal(1,id_level)
!      dlgTime(EDIT_LEVEL_2).end.zone     = dbreal(1,id_level)

IF( .NOT.project(EDIT_LEVEL_2)%Restart )THEN
  dlgTime(EDIT_LEVEL_2)%time%start%time%hour = dbreal(2,id_level)
END IF

dlgTime(EDIT_LEVEL_2)%time%end%time%hour = dbreal(3,id_level)

x = dbreal(4,id_level)
IF( x /= NOT_SET_R )THEN
  SELECT CASE( idbcmbo(1,id_level) )
    CASE( 1 )
      x = x/3600.
    CASE( 2 )
      x = x/60.
    CASE DEFAULT
  END SELECT
END IF
dlgTime(EDIT_LEVEL_2)%time%end%time%runTime = x

dlgTime(EDIT_LEVEL_2)%time%end%step%max = dbreal(5,id_level)
SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    dlgTime(EDIT_LEVEL_2)%time%end%step%max = dlgTime(EDIT_LEVEL_2)%time%end%step%max*60.
  CASE( 3 )
    dlgTime(EDIT_LEVEL_2)%time%end%step%max = dlgTime(EDIT_LEVEL_2)%time%end%step%max*3600.
  CASE DEFAULT
END SELECT

dlgTime(EDIT_LEVEL_2)%time%end%step%output = dbreal(6,id_level)
IF( dlgTime(EDIT_LEVEL_2)%time%end%step%output /= DEF_VAL_R )THEN
  SELECT CASE( idbcmbo(3,id_level) )
    CASE( 1 )
      dlgTime(EDIT_LEVEL_2)%time%end%step%output = dlgTime(EDIT_LEVEL_2)%time%end%step%output/3600.
    CASE( 2 )
      dlgTime(EDIT_LEVEL_2)%time%end%step%output = dlgTime(EDIT_LEVEL_2)%time%end%step%output/60.
    CASE DEFAULT
  END SELECT
END IF
!
!     Edit Boxes - Integers
!
IF( .NOT.project(EDIT_LEVEL_2)%Restart )THEN
  dlgTime(EDIT_LEVEL_2)%time%start%time%year  = dbint(1,id_level)
  dlgTime(EDIT_LEVEL_2)%time%start%time%month = dbint(2,id_level)
  dlgTime(EDIT_LEVEL_2)%time%start%time%day   = dbint(3,id_level)

  IF( .NOT.check_YMD(dlgTime(EDIT_LEVEL_2)%time%start%time) )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%time%year  = NOT_SET_I
    dlgTime(EDIT_LEVEL_2)%time%start%time%month = NOT_SET_I
  END IF
  IF( dlgTime(EDIT_LEVEL_2)%time%start%time%day <= 0 )THEN
    dlgTime(EDIT_LEVEL_2)%time%start%time%day = NOT_SET_I
  END IF
END IF

dlgTime(EDIT_LEVEL_2)%time%end%time%year  = dbint(4,id_level)
dlgTime(EDIT_LEVEL_2)%time%end%time%month = dbint(5,id_level)
dlgTime(EDIT_LEVEL_2)%time%end%time%day   = dbint(6,id_level)

IF( .NOT.check_YMD(dlgTime(EDIT_LEVEL_2)%time%end%time) )THEN
  dlgTime(EDIT_LEVEL_2)%time%end%time%year  = NOT_SET_I
  dlgTime(EDIT_LEVEL_2)%time%end%time%month = NOT_SET_I
END IF
IF( dlgTime(EDIT_LEVEL_2)%time%end%time%day <= 0 )THEN
  dlgTime(EDIT_LEVEL_2)%time%end%time%day = NOT_SET_I
END IF
!
!     Combo Boxes
!
IF( project(EDIT_LEVEL_2)%Restart )THEN
  project(EDIT_LEVEL_2)%RestartTimeIndx = idbcmbo(4,id_level)
END IF
!
!     List Boxes
!
CALL time_string( dlgTime(EDIT_LEVEL_2)%time%start%time,dlgTime(EDIT_LEVEL_2)%startString )
CALL time_string( dlgTime(EDIT_LEVEL_2)%time%end%time,dlgTime(EDIT_LEVEL_2)%endString )
IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == NOT_SET_R )THEN
  CALL standard_time_zone( dlgDomain(EDIT_LEVEL_1),dlgTime(EDIT_LEVEL_2) )
END IF

dlgTime(EDIT_LEVEL_2)%default_save = lcheck(1,id_level) &
              .AND. dlgTime(EDIT_LEVEL_2)%time%end%step%output == dlgTime(DEFAULT_LEVEL)%time%end%step%output

RETURN
END
!***********************************************************************
!               StandardTimeZone
!***********************************************************************
SUBROUTINE standard_time_zone( domain,time )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE files_fi

IMPLICIT NONE

TYPE( DOMAIN_DLG ), INTENT( IN    ) :: domain
TYPE( TIME_DLG ),   INTENT( INOUT ) :: time

REAL tzc,xmid

IF( ((domain%spatial%domain%coord == I_CARTESIAN).OR.(domain%spatial%domain%coord == I_UTM) .OR. &
     (domain%spatial%domain%coord == I_METERS)) .AND. domain%spatial%reference%lon /= NOT_SET_R )THEN
  tzc = FLOAT(INT( domain%spatial%reference%lon+7.5)/15)
  IF( domain%spatial%reference%lon < -7.5 )tzc = tzc - 1.
ELSE IF( domain%spatial%domain%coord == I_LATLON .AND. &
         domain%spatial%domain%xMin /= DEF_VAL_R .AND. &
	       domain%spatial%domain%xMax /= DEF_VAL_R .AND. &
         domain%spatial%domain%xMin /= NOT_SET_R .AND. &
	       domain%spatial%domain%xMax /= NOT_SET_R )THEN
  xmid = 0.5*(domain%spatial%domain%xMin+ domain%spatial%domain%xMax)
  tzc  = FLOAT(INT(xmid+7.5)/15)
  IF( xmid < -7.5 )tzc = tzc - 1.
ELSE
  tzc = time%time%start%zone
END IF

IF( tzc /= DEF_VAL_R .AND. tzc /= NOT_SET_R )THEN
  IF( tzc < 0. )tzc = tzc + 24.
END IF

time%time%start%zone = tzc

RETURN
END
!***********************************************************************
!               DefaultButtonEdt
!***********************************************************************
SUBROUTINE default_button_edt( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE errorParam_fd
USE param_fd
USE GUItool_fi
USE SwinAPI

!     This routine processes LOAD/DEFAULT PUSHBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)

LOGICAL lok
CHARACTER(PATH_MAXLENGTH) filenam
INTEGER irv,id
TYPE( DOMAIN_DLG ) tmpdom
TYPE( TIME_DLG )   tmptim

!---- Select by Button number

IF( id_button == 1 )THEN !OK
  CALL PushButton( iwnd_db,IDB_BUTTON1,ID_OK,irv )
ELSE IF( id_button == 2 )THEN !Browse
  filenam = TRIM(dbtext(20,id_level))
  CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
  IF( lok )THEN
    dbtext(20,id_level) = TRIM(filenam)
    CALL SetControlText( iwnd_db,dbint(20,id_level),dbtext(20,id_level) )
  END IF
ELSE IF( id_button == 14 )THEN !LOAD
  lok = .FALSE.
  SELECT CASE( id_dialog )
    CASE( IDB_PRJDEF )
      id = 7
    CASE( IDB_OPTIONS )
      id = 3
    CASE( IDB_TIME )
      id = 1
    CASE( IDB_DOMAIN )
      id = 2
    CASE DEFAULT
  END SELECT
  filenam = TRIM(loadfile(id))
  CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
  IF( lok )THEN
    CALL read_namelist( iwnd_db,id_dialog,TRIM(filenam),lok ) !Read namelist
    IF( lok )THEN
      SELECT CASE( id_dialog )
        CASE( IDB_PRJDEF )
!==== SCIP Tool
          CALL SCIP_GUI_flags( project(EDIT_LEVEL_2),flags )
          CALL init_dialog_prjdef( iwnd_db,id_level )

        CASE( IDB_OPTIONS )
          CALL SCIP_GUI_options( dlgOptions(EDIT_LEVEL_2),prjOptions )
          CALL init_dialog_options( iwnd_db,id_level )

        CASE( IDB_TIME )
          tmptim = dlgTime(EDIT_LEVEL_2)
          CALL SCIP_GUI_time( dlgTime(EDIT_LEVEL_2),time )
!==== SCIP Tool
          IF( project(EDIT_LEVEL_1)%Restart )THEN
            IF( dlgTime(EDIT_LEVEL_2)%time%start%time%reference /= tmptim%time%start%time%reference )THEN
              CALL SetError( IV_ERROR, &
                             'Time has invalid reference', &
                             'Restart projects can only load times with the same reference', &
                             'Original time restored', &
                             'LoadTime' )
              CALL ShowErrorMessage( iwnd_db )
              dlgTime(EDIT_LEVEL_2) = tmptim
            ELSE
              IF( dlgTime(EDIT_LEVEL_2)%time%start%time%reference == HT_LOCAL )THEN
                IF( dlgTime(EDIT_LEVEL_2)%time%start%zone /= tmptim%time%start%zone )THEN
                  CALL SetError( IV_ERROR, &
                                 'Time has invalid time zone', &
                                 'Restart projects can only load times with the same time zone', &
                                 'Original time restored', &
                                 'LoadTime' )
                  CALL ShowErrorMessage( iwnd_db )
                  dlgTime(EDIT_LEVEL_2) = tmptim
                END IF
              ELSE
                dlgTime(EDIT_LEVEL_2)%time%start = tmptim%time%start
              END IF
            END IF
          END IF
          CALL init_dialog_time( iwnd_db,id_level )
          CALL UpdateDuration( iwnd_db )
          dlgTime(EDIT_LEVEL_2) = tmptim

        CASE( IDB_DOMAIN )
          tmpdom = dlgDomain(EDIT_LEVEL_2)
!==== SCIP Tool
          CALL SCIP_GUI_domain( dlgDomain(EDIT_LEVEL_2),domain )
          IF( project(EDIT_LEVEL_1)%Restart )THEN
            IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord /= tmpdom%spatial%domain%coord )THEN
              CALL SetError( IV_ERROR, &
                             'Domain has invalid coordinates', &
                             'Restart projects can only load domains with the same coordinates', &
                             'Original domain restored', &
                             'LoadDomain' )
              CALL ShowErrorMessage( iwnd_db )
              dlgDomain(EDIT_LEVEL_2) = tmpdom
            ELSE IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
              IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM /= tmpdom%spatial%domain%zoneUTM )THEN
                CALL SetError ( IV_ERROR, &
                               'Domain has invalid UTM zone', &
                               'Restart projects can only load domains with the same UTM zone', &
                               'Original domain restored', &
                               'LoadDomain' )
                CALL ShowErrorMessage( iwnd_db )
                dlgDomain(EDIT_LEVEL_2) = tmpdom
              END IF
            ELSE
              dlgDomain(EDIT_LEVEL_2)%hasReference = tmpdom%hasReference
              dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat = tmpdom%spatial%reference%lat
              dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon = tmpdom%spatial%reference%lon
              dlgDomain(EDIT_LEVEL_2)%spatial%reference%x   = tmpdom%spatial%reference%x
              dlgDomain(EDIT_LEVEL_2)%spatial%reference%y   = tmpdom%spatial%reference%y
              dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM = tmpdom%spatial%domain%zoneUTM
            END IF
          END IF
          CALL init_dialog_domain( iwnd_db,id_level )
          dlgDomain(EDIT_LEVEL_2) = tmpdom
          CALL CheckLocalCoordinates( iwnd_db,id_level,.FALSE.,lok )
          IF( .NOT.lok )CALL init_dialog_domain( iwnd_db,id_level )

        CASE DEFAULT

      END SELECT
    END IF
    loadfile(id) = TRIM(filenam)
  END IF
ELSE IF( id_button <= 15 )THEN !Default
  SELECT CASE( id_dialog )
    CASE( IDB_PRJDEF )
      project(EDIT_LEVEL_2)%Title = project(DEFAULT_LEVEL)%Title
      project(EDIT_LEVEL_2)%audit = project(DEFAULT_LEVEL)%audit
      CALL init_dialog_prjdef( iwnd_db,id_level )

    CASE( IDB_OPTIONS )
      dlgOptions(EDIT_LEVEL_2) = dlgOptions(DEFAULT_LEVEL)
      CALL init_dialog_options( iwnd_db,id_level )

    CASE( IDB_TIME )
      dlgTime(EDIT_LEVEL_2) = dlgTime(DEFAULT_LEVEL)
      CALL init_dialog_time( iwnd_db,id_level )
      CALL MyUpdateWindow( iwnd_db,.FALSE. )

    CASE( IDB_DOMAIN )
      tmpdom = dlgDomain(EDIT_LEVEL_2)
      dlgDomain(EDIT_LEVEL_2) = dlgDomain(DEFAULT_LEVEL)
      CALL init_dialog_domain( iwnd_db,id_level )
      CALL CheckLocalCoordinates( iwnd_db,id_level,.FALSE.,lok )
      dlgDomain(EDIT_LEVEL_2) = tmpdom
      CALL init_dialog_domain( iwnd_db,id_level )
      CALL MyUpdateWindow(iwnd_db,.FALSE.)

    CASE DEFAULT

  END SELECT
END IF

RETURN
END
!***********************************************************************
!               UpdateDuration
!***********************************************************************
SUBROUTINE UpdateDuration( iwnd_db )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db

TYPE( timeT ) end,start

INTEGER ID
INTEGER id_level,idlg
LOGICAL lok
INTEGER indxd,indx,irv
REAL      dfac

CHARACTER(128), EXTERNAL :: AddNull

CALL FindHwndList( iwnd_db,ID,id_level )

IF( ID == IDB_SCIPUF )THEN
  idlg = 1
ELSE
  idlg = 2
END IF

end%year  = dbint(4,id_level)
end%month = dbint(5,id_level)
end%day   = dbint(6,id_level)
end%hour  = dbreal(3,id_level)

IF( idlg == 2 )THEN
  IF( ichoice(1,id_level) == 2 )THEN
    start%reference = HT_LOCAL
  ELSE
    start%reference = HT_UTC
  END IF
  start%year  = dbint(1,id_level)
  start%month = dbint(2,id_level)
  start%day   = dbint(3,id_level)
  start%hour  = dbreal(2,id_level)
ELSE
  start = dlgTime(idlg)%time%start%time
END IF

CALL ComputeDurationGUI( start,end,lok )

IF( lok )THEN
  IF( end%runTime == 0. .OR. end%runTime >= 1. )THEN
    dfac = 1.
    indxd = 3
  ELSE IF( end%runTime >= 1./60. )THEN
    dfac = 60.
    indxd = 2
  ELSE
    dfac = 3600.
    indxd = 1
  END IF
  dbreal(4,id_level) = end%runTime*dfac
ELSE
  dbreal(4,id_level) = NOT_SET_R
  indxd = 3
END IF

CALL SetEditRs( iwnd_db,dbreal(4,id_level),4,1 )

indx = indxd - 1
CALL SetListSel( iwnd_db,IDB_COMBO1,indx,irv )
CALL GetListSel( iwnd_db,IDB_COMBO1,1,indx,irv )
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,IDB_COMBO1,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(1,id_level) = indx + 1
    dbcmbo(1,id_level)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF

RETURN
END
!***********************************************************************
!               UpdateEndTime
!***********************************************************************
SUBROUTINE UpdateEndTime( iwnd_db )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db

INTEGER ID,idlg
INTEGER id_level
LOGICAL lok

TYPE( timeT ) end,start

CALL FindHwndList( iwnd_db,ID,id_level )

IF( ID == IDB_SCIPUF )THEN
  idlg = 1
ELSE
  idlg = 2
END IF

IF( idlg == 2 )THEN
  IF( ichoice(1,id_level) == 2 )THEN
    start%reference = HT_LOCAL
  ELSE
    start%reference = HT_UTC
  END IF
  start%year  = dbint(1,id_level)
  start%month = dbint(2,id_level)
  start%day   = dbint(3,id_level)
  start%hour  = dbreal(2,id_level)
ELSE
  start = dlgTime(idlg)%time%start%time
END IF

end%runTime = dbreal(4,id_level)
IF( end%runTime /= NOT_SET_R )THEN
  SELECT CASE( idbcmbo(1,id_level) )
    CASE( 1 )
      end%runTime = end%runTime/3600.
    CASE( 2 )
      end%runTime = end%runTime/60.
    CASE DEFAULT
  END SELECT
  CALL ComputeEndTime( start,end,lok )
ELSE
  end%hour  = NOT_SET_R
  end%year  = NOT_SET_I
  end%month = NOT_SET_I
  end%day   = NOT_SET_I
  lok = .TRUE.
END IF

IF( lok )THEN
  dbreal(3,id_level)  = end%hour
  dbint(4,id_level)   = end%year
  dbint(5,id_level)   = end%month
  dbint(6,id_level)   = end%day
  CALL SetEditRs( iwnd_db,dbreal(3,id_level),3,1 )
  CALL SetEditIs( iwnd_db,dbint(4,id_level),4,3 )
END IF

RETURN
END
!***********************************************************************
!               CheckTimeDefinition
!***********************************************************************
SUBROUTINE check_time_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE GUItool_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

TYPE( timeT ) end,start

INTEGER i,irv,jhr,jmn
LOGICAL   end_ymd,start_ymd,check_end_time
REAL      tout,tstep

LOGICAL   Operational_Check

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'Check_Time_Defination'

Operational_Check = .FALSE.
check_end_time = .TRUE.

!=====Check time steps

!---- Not set

i    = 5
DO WHILE( i <= 6 .AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 5 )
        string1 = 'the maximum Time step'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%step%max,string2,irv )
      CASE( 6 )
        string1 = 'the Output interval'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%step%output,string2,irv )
      CASE DEFAULT
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = 'Please set '//TRIM(string1)
  GOTO 9999
END IF

!---- Output > time step

tstep = dbreal(5,id_level)
SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    tstep = tstep*60.
  CASE( 3 )
    tstep = tstep*3600.
  CASE DEFAULT
END SELECT

tout = dbreal(6,id_level)
SELECT CASE( idbcmbo(3,id_level) )
  CASE( 2 )
    tout = tout*60.
  CASE( 3 )
    tout = tout*3600.
  CASE DEFAULT
END SELECT

lok = tout >= tstep
IF( .NOT.lok )THEN
  nError = IV_ERROR
  irv = 0
  eMessage = 'Output interval must not be less than the maximum time step'
  GOTO 9999
END IF

!=====Time zone

!---- Not set

IF( Operational_Check )THEN
  i = 7
  DO WHILE( i <= 8 .AND. nError == NO_ERROR )
    IF( dbint(i,id_level) == NOT_SET_I )check_end_time = .FALSE.
    i = i + 1
  END DO
ELSE
  i = 7
  DO WHILE( i <= 8 .AND. nError == NO_ERROR )
    IF( dbint(i,id_level) == NOT_SET_I )THEN
      nError = IV_ERROR
      CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%start%zone,string2,irv )
      IF( irv > 0 )THEN
        jhr = INT(dlgTime(DEFAULT_LEVEL)%time%start%zone)
        jmn = NINT(60.*(dlgTime(DEFAULT_LEVEL)%time%start%zone - FLOAT(jhr)))
      ELSE IF( irv < 0 )THEN
        jhr = DEF_VAL_I
        jmn = DEF_VAL_I
      ELSE
        jhr = NOT_SET_I
        jmn = NOT_SET_I
      END IF
      SELECT CASE( i )
        CASE( 7 )
          string1 = 'the Time zone hour'
          CALL set_int_string(jhr,string2,irv)
        CASE( 8 )
          string1 = 'the Time zone minutes'
          CALL set_int_string(jmn,string2,irv)
        CASE DEFAULT
          string1 = 'all Time Zone parameters'
      END SELECT
    END IF
    i = i + 1
  END DO

  lok = nError == NO_ERROR

  IF( .NOT.lok )THEN
    eMessage = 'Please set '//TRIM(string1)
    GOTO 9999
  END IF
END IF

!=====Start/Stop times

!---- Not set

IF( Operational_Check )THEN
  i = 2
  DO WHILE( i <= 4 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )check_end_time = .FALSE.
    i = i + 1
  END DO
  end_ymd = .FALSE.
  DO i = 4,5
    end_ymd = dbint(i,id_level) /= NOT_SET_I
  END DO
  IF( end_ymd )THEN
    i = 4
    DO WHILE( i <= 6 .AND. nError == NO_ERROR )
      IF( dbint(i,id_level) == NOT_SET_I )check_end_time = .FALSE.
      i = i + 1
    END DO
  END IF

  start_ymd = .FALSE.
  DO i = 1,2
    start_ymd = dbint(i,id_level) /= NOT_SET_I
  END DO
  IF( start_ymd .OR. end_ymd )THEN
    i = 1
    DO WHILE( i <= 3 .AND. nError == NO_ERROR )
      IF( dbint(i,id_level) == NOT_SET_I )check_end_time = .FALSE.
      i = i + 1
    END DO
  END IF

ELSE

  i = 2
  DO WHILE( i <= 4 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 2 )
          string1 = 'the Start time'
          CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%start%time%hour,string2,irv )
        CASE( 3 )
          string1 = 'the End time'
          CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%time%hour,string2,irv )
        CASE( 4 )
          string1 = 'the Duration'
          CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%time%runTime,string2,irv )
        CASE DEFAULT
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO

  end_ymd = .FALSE.
  DO i = 4,5
    end_ymd = dbint(i,id_level) /= NOT_SET_I
  END DO
  IF( end_ymd )THEN
    i = 4
    DO WHILE( i <= 6 .AND. nError == NO_ERROR )
      IF( dbint(i,id_level) == NOT_SET_I )THEN
        nError = IV_ERROR
        SELECT CASE( i )
          CASE( 4 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%year,string2,irv )
            string1 = 'the End year'
          CASE( 5 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%month,string2,irv )
            string1 = 'the End month'
          CASE( 6 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%day,string2,irv )
            string1 = 'the End day'
          CASE DEFAULT
            string1 = 'all END TIME parameters'
        END SELECT
      END IF
      i = i + 1
    END DO
  END IF

  start_ymd = .FALSE.
  DO i = 1,2
    start_ymd = dbint(i,id_level) /= NOT_SET_I
  END DO
  IF( start_ymd .OR. end_ymd )THEN
    i = 1
    DO WHILE( i <= 3 .AND. nError == NO_ERROR )
      IF( dbint(i,id_level) == NOT_SET_I )THEN
        nError = IV_ERROR
        SELECT CASE( i )
          CASE( 1 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%start%time%year,string2,irv )
            string1 = 'the Start year'
          CASE( 2 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%start%time%month,string2,irv )
            string1 = 'the Start month'
          CASE( 3 )
            CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%start%time%day,string2,irv )
            string1 = 'the Start day'
          CASE DEFAULT
            string1 = 'all START TIME parameters'
        END SELECT
      END IF
      i = i + 1
    END DO
  END IF

  lok = nError == NO_ERROR

  IF( .NOT.lok )THEN
    eMessage = 'Please set '//TRIM(string1)
    GOTO 9999
  END IF

END IF

IF( check_end_time )THEN
  IF( ichoice(1,id_level) == 2 )THEN
    end%reference = HT_LOCAL
  ELSE
    end%reference = HT_UTC
  END IF
  end%year  = dbint(4,id_level)
  end%month = dbint(5,id_level)
  end%day   = dbint(6,id_level)
  end%hour  = dbreal(3,id_level)

  IF( ichoice(1,id_level) == 2 )THEN
    start%reference = HT_LOCAL
  ELSE
    start%reference = HT_UTC
  END IF
  start%year  = dbint(1,id_level)
  start%month = dbint(2,id_level)
  start%day   = dbint(3,id_level)
  start%hour  = dbreal(2,id_level)

  CALL ComputeDurationGUI( start,end,lok )

  IF( .NOT.lok )THEN
    nError = IV_ERROR
    irv = 0
    eMessage = 'End time must be greater than start time'
    GOTO 9999
  END IF
END IF

RETURN

9999 CONTINUE

eRoutine = 'CheckTime'
IF( irv == 0 )THEN
  eAction  = ' '
ELSE IF( irv < 0 )THEN
  eAction  = 'Default="default"'
ELSE
  eAction  = 'Default='//TRIM(string2)
END IF
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )

RETURN
END
!***********************************************************************
!               set_default_save
!***********************************************************************
SUBROUTINE set_default_save( t,ifac,deft,dt )

USE default_fd

IMPLICIT NONE

REAL,    INTENT( IN  ) :: t
INTEGER, INTENT( IN  ) :: ifac
REAL,    INTENT( IN  ) :: deft
REAL,    INTENT( OUT ) :: dt

REAL tdur

IF( t == NOT_SET_R .OR. t == DEF_VAL_R )THEN
  dt = deft
  RETURN
END IF

IF( ifac == 1 )THEN
  tdur = t/3600.
ELSE IF( ifac == 2 )THEN
  tdur = t/60.
ELSE
  tdur = t
END IF

IF( tdur <= 12.0 )THEN
  dt  = 1.
ELSE IF( tdur <= 24.0 )THEN
  dt  = 2.
ELSE IF( tdur <= 48.0 )THEN
  dt  = 4.
ELSE IF( tdur <= 72.0 )THEN
  dt  = 6.
ELSE IF( tdur <= 144.0 )THEN
  dt  = 12.
ELSE IF( tdur <= 288.0 )THEN
  dt  = 24.
ELSE
  dt  = FLOAT(NINT(tdur/12./24.))*24.
END IF

RETURN
END

!===============================================================================

SUBROUTINE LocalToGMT( Time,zone )

USE tooluser_fd
USE default_fd

TYPE( timeT ), INTENT( INOUT ) :: Time
REAL,          INTENT( IN    ) :: zone

INTEGER iday
REAL    hours,tz
LOGICAL check_YMD

INTEGER, EXTERNAL :: julian_day

IF( check_YMD(Time) )THEN
  IF( Time%reference == HT_LOCAL )THEN
    iday = julian_day( Time%month,Time%day,Time%year )
    tz = zone
    CALL fix_tzone( tz )
    hours = Time%hour - tz
    IF( hours < 0.0 )THEN
      hours = hours + 24.
      iday  = iday - 1
    END IF
    IF( hours > 24.0 )THEN
      hours = hours - 24.
      iday  = iday + 1
    END IF
    Time%hour = hours
    CALL julian_ymd( iday,Time%year,Time%month,Time%day )
  END IF
ELSE
  IF( Time%reference == HT_LOCAL )THEN
    iday = Time%day
    tz = zone
    CALL fix_tzone( tz )
    hours = Time%hour - tz
    IF( hours < 0.0 .AND. iday /= NOT_SET_I )THEN
      hours = hours + 24.
      iday  = iday  - 1
    END IF
    IF( hours > 24.0 .AND. iday /= NOT_SET_I )THEN
      hours = hours - 24.
      iday  = iday  + 1
    END IF
    Time%day = iday
    Time%hour = hours
  END IF
END IF

RETURN
END

!===============================================================================

SUBROUTINE GMTToLocal( Time,zone )

USE resource_fd
USE defineok_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi

TYPE( timeT ), INTENT( INOUT ) :: Time
REAL,          INTENT( IN    ) :: zone

INTEGER iday
REAL    hours,tz
LOGICAL check_YMD

INTEGER, EXTERNAL :: julian_day

IF( check_YMD(Time) )THEN
  IF( Time%reference == HT_LOCAL )THEN
    iday = julian_day( Time%month,Time%day,Time%year )
    tz = zone
    CALL fix_tzone( tz )
    hours = Time%hour + tz
    IF( hours > 24.0 )THEN
      hours = hours - 24.
      iday  = iday + 1
    END IF
    IF( hours < 0.0 )THEN
      hours = hours + 24.
      iday  = iday - 1
    END IF
    Time%hour = hours
    CALL julian_ymd( iday,Time%year,Time%month,Time%day )
  END IF
ELSE
  IF( Time%reference == HT_LOCAL )THEN
    iday  = Time%day
    tz = zone
    CALL fix_tzone( tz )
    hours = Time%hour + tz
    IF( hours < 0.0 .AND. iday /= NOT_SET_I )THEN
      hours = hours + 24.
      iday  = iday - 1
    END IF
    IF( hours > 24.0 .AND. iday /= NOT_SET_I )THEN
      hours = hours - 24.
      iday  = iday + 1
    END IF
    Time%day   = iday
    Time%hour  = hours
  END IF
END IF

RETURN
END
