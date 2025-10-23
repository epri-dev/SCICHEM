!*******************************************************************************
!            Initialize DOMAIN Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_domain( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the DOMAIN Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

CHARACTER(128), EXTERNAL :: AddNull

INTEGER   enable(2),i,irv,indx
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
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_1)%ID%name)//' DOMAIN'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF
!
!     Radio Buttons
!
nradio(1,id_level)  = 3 !Coordinate Type
ichoice(1,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord
!---- METERS FIX
IF( ichoice(1,id_level) == I_METERS )ichoice(1,id_level)=I_CARTESIAN
!---- METERS FIX
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
  dbint(3,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM !UTM Zone
ELSE
  dbint(3,id_level) = dlgDomain(DEFAULT_LEVEL)%spatial%domain%zoneUTM
END IF
!
!     Check Boxes
!
lcheck(1,id_level) = dlgDomain(EDIT_LEVEL_2)%hasReference !Reference point
!
!     Edit Boxes - Reals
!
CALL ClearList( iwnd_db,IDB_COMBO3 )
CALL ClearList( iwnd_db,IDB_COMBO4 )
string1 = 'Degrees'
CALL AddList( iwnd_db,IDB_COMBO3,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv )
string1 = 'Deg/Min/Sec'
CALL AddList( iwnd_db,IDB_COMBO3,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv )
idbcmbo(3,id_level) = 1
indx = idbcmbo(3,id_level) - 1
CALL SetListSel( iwnd_db,IDB_COMBO3,indx,irv )
CALL GetListSel( iwnd_db,IDB_COMBO3,1,indx,irv )
string2 = 'Not Set'
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,IDB_COMBO3,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(3,id_level) = indx + 1
    dbcmbo(3,id_level)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF
idbcmbo(4,id_level) = 1
indx = idbcmbo(4,id_level) - 1
CALL SetListSel( iwnd_db,IDB_COMBO4,indx,irv )
CALL GetListSel( iwnd_db,IDB_COMBO4,1,indx,irv )
string2 = 'Not Set'
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,IDB_COMBO4,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(4,id_level) = indx + 1
    dbcmbo(4,id_level)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF

dbreal(1,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon !Lon origin
dbreal(2,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat !Lat origin
CALL compute_DMS( dbreal(1,id_level),dbint(7,id_level), &
                  dbint(13,id_level),dbreal(13,id_level) )
CALL compute_DMS( dbreal(2,id_level),dbint(8,id_level), &
                  dbint(14,id_level),dbreal(14,id_level) )
IF( ichoice(1,id_level) == I_LATLON )THEN
  dbreal(5,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMin !X Domain Min.
  dbreal(6,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMax !X Domain Max
  dbreal(3,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMin !Y Domain Min
  dbreal(4,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMax !Y Domain Max
  DO i = 3,6
    CALL compute_DMS( dbreal(i,id_level),dbint(i+6,id_level), &
                      dbint(i+12,id_level),dbreal(i+12,id_level) )
  END DO
ELSE
  dbreal(3,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMin !X Domain Min.
  dbreal(4,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMax !X Domain Max
  dbreal(5,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMin !Y Domain Min
  dbreal(6,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMax !Y Domain Max
  DO i = 3,6
    dbint(i+6,id_level) = NOT_SET_I
    dbint(i+12,id_level)= NOT_SET_I
    dbreal(i+12,id_level)=NOT_SET_R
  END DO
END IF
dbreal(7,id_level)  = dlgDomain(EDIT_LEVEL_2)%spatial%domain%hRes !Horizontal res
dbreal(8,id_level)  = dlgDomain(EDIT_LEVEL_2)%spatial%domain%zMax !Z Domain Max
dbreal(9,id_level)  = dlgDomain(EDIT_LEVEL_2)%spatial%domain%vRes !vertical res
dbreal(10,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%x !X origin
dbreal(11,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%y !Y origin

CALL load_dialog_domain( iwnd_db,id_level )

IF( .NOT.UTM_init )CALL EnableControl( iwnd_db,IDB_RADIO03,FALSE )

RETURN
END
!*******************************************************************************
!            Load DOMAIN Dialog Box
!*******************************************************************************
SUBROUTINE load_dialog_domain( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
!
!     This routine loads the Domain Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i

CALL SetEditIs( iwnd_db,dbint(3,id_level),3,1 )
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,11 )
CALL SetEditIs( iwnd_db,dbint(7,id_level),7,6 )
CALL SetEditIs( iwnd_db,dbint(13,id_level),13,6 )
CALL SetEditRs( iwnd_db,dbreal(13,id_level),13,6 )
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )
CALL SetRadios( iwnd_db,ichoice(3,id_level),nradio(3,id_level),3,1 )
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )
CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                           idbcmbo(4,id_level),lcheck(1,id_level) )

IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  DO i = 1,11
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  END DO
  DO i = 13,18
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  END DO
  DO i = 7,12
    CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  END DO
  DO i = 13,18
    CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  END DO
  CALL EnableControl( iwnd_db,IDB_INT3,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  DO i = 1,nradio(1,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
ELSE
  IF( project(EDIT_LEVEL_1)%Restart )THEN
    DO i = 1,2
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    DO i = 10,11
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    DO i = 13,14
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    DO i = 7,8
      CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
    END DO
    DO i = 13,14
      CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,IDB_INT3,FALSE )
    CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
    DO i = 1,nradio(1,id_level)
      CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
    END DO
  END IF
END IF

RETURN
END
!*******************************************************************************
!            save DOMAIN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_domain( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
!
!     This routine saves the NEW Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

TYPE( DOMAIN_DLG ) domtmp

domtmp = dlgDomain(EDIT_LEVEL_2)
!
!     Radio Buttons
!
!---- METERS FIX
IF( ichoice(1,id_level) == I_CARTESIAN .AND. &
                           dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_METERS )THEN
  ichoice(1,id_level) = I_METERS
END IF
!---- METERS FIX
dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord = ichoice(1,id_level) !Coordinate Type
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM = dbint(3,id_level)
ELSE
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM = dlgDomain(BASE_LEVEL)%spatial%domain%zoneUTM
END IF
project(EDIT_LEVEL_1)%MapCoord = dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord !Enable/Disable Map drawing
!
!     Check Boxes
!
dlgDomain(EDIT_LEVEL_2)%hasReference   = lcheck(1,id_level) !Reference point
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_LATLON )THEN
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMin = dbreal(5,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMin = dbreal(3,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMax = dbreal(6,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMax = dbreal(4,id_level)
ELSE
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMin = dbreal(3,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMin = dbreal(5,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%xMax = dbreal(4,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%yMax = dbreal(6,id_level)
  IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
    CALL set_utm_reference( EDIT_LEVEL_2 )
    CALL InitError()
  END IF
END IF
dlgDomain(EDIT_LEVEL_2)%spatial%domain%hRes = dbreal(7,id_level)
dlgDomain(EDIT_LEVEL_2)%spatial%domain%zMax = dbreal(8,id_level)
dlgDomain(EDIT_LEVEL_2)%spatial%domain%vRes = dbreal(9,id_level)

IF( dlgDomain(EDIT_LEVEL_2)%hasReference )THEN
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon = dbreal( 1,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat = dbreal( 2,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%x   = dbreal(10,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%y   = dbreal(11,id_level)
ELSE
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lon
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lat
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%x   = dlgDomain(DEFAULT_LEVEL)%spatial%reference%x
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%y   = dlgDomain(DEFAULT_LEVEL)%spatial%reference%y
END IF

RETURN
END
!***********************************************************************
!               ShowLocalCoordinates
!***********************************************************************
SUBROUTINE ShowLocalCoordinates( iwnd_db,iopt,jopt,kopt,lref )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: iopt
INTEGER,              INTENT( IN ) :: jopt
INTEGER,              INTENT( IN ) :: kopt
LOGICAL,              INTENT( IN ) :: lref

INTEGER ienable,ishow,ienablex
INTEGER ienable1,ishow1
INTEGER ienable2,ishow2
INTEGER jenable,jshow
INTEGER kenable,kshow
INTEGER menable,mshow
INTEGER nenable,nshow
INTEGER id,ilev,i

IF( iopt == 1 )THEN
  ienablex = FALSE
  ienable  = FALSE
  ishow    = SW_HIDE
  ienable1 = FALSE
  ishow1   = SW_HIDE
  ienable2 = FALSE
  ishow2   = SW_HIDE
  jenable  = FALSE
  jshow    = SW_HIDE
  kenable  = TRUE
  kshow    = SW_SHOWNORMAL
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL ShowControl( iwnd_db,IDB_CHECK1 ,SW_HIDE )
  CALL SetControlText( iwnd_db,IDB_STATIC29,'Lon (E)' )
  CALL SetControlText( iwnd_db,IDB_STATIC28,'Lat (N)' )
  CALL SetControlText( iwnd_db,IDB_STATIC32,'(deg.)' )
  IF( jopt == 1 )THEN
    menable = FALSE
    mshow   = SW_HIDE
    nenable = TRUE
    nshow   = SW_SHOWNORMAL
  ELSE
    menable = TRUE
    mshow   = SW_SHOWNORMAL
    nenable = FALSE
    nshow   = SW_HIDE
  END IF
ELSE IF( iopt == 3 )THEN
  ienablex = FALSE
  ienable  = FALSE
  ishow    = SW_HIDE
  ienable1 = FALSE
  ishow1   = SW_HIDE
  ienable2 = FALSE
  ishow2   = SW_HIDE
  kenable = FALSE
  kshow   = SW_HIDE
  menable = FALSE
  mshow   = SW_HIDE
  nenable = TRUE
  nshow   = SW_SHOWNORMAL
  CALL FindHwndList( iwnd_db,id,ilev ) !  Find Dialog ID from List
  IF( id == IDB_SETUP )THEN
    jenable = FALSE
    jshow   = SW_HIDE
  ELSE
    IF( project(EDIT_LEVEL_1)%Restart )THEN
      jenable = FALSE
    ELSE
      jenable = TRUE
    END IF
    jshow = SW_SHOWNORMAL
  END IF
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL ShowControl( iwnd_db,IDB_CHECK1 ,SW_HIDE )
  CALL SetControlText( iwnd_db,IDB_STATIC28,'Easting' )
  CALL SetControlText( iwnd_db,IDB_STATIC29,'Mod N''ing' )
  CALL SetControlText( iwnd_db,IDB_STATIC32,'(km)' )
ELSE
  kenable = FALSE
  kshow   = SW_HIDE
  menable = FALSE
  mshow   = SW_HIDE
  nenable = TRUE
  nshow   = SW_SHOWNORMAL
  IF( lref )THEN
    ienablex = TRUE
    IF( project(EDIT_LEVEL_1)%Restart )THEN
      ienable = FALSE
    ELSE
      ienable = TRUE
    END IF
    ishow = SW_SHOWNORMAL
    IF( kopt /= 2 )THEN
      IF( project(EDIT_LEVEL_1)%Restart )THEN
        ienable1 = FALSE
      ELSE
        ienable1 = TRUE
      END IF
      ishow1   = SW_SHOWNORMAL
      ienable2 = FALSE
      ishow2   = SW_HIDE
    ELSE
      ienable1 = FALSE
      ishow1   = SW_HIDE
      IF( project(EDIT_LEVEL_1)%Restart )THEN
        ienable2 = FALSE
      ELSE
        ienable2 = TRUE
      END IF
      ishow2 = SW_SHOWNORMAL
    END IF
  ELSE
    ienablex = FALSE
    ienable  = FALSE
    ishow    = SW_HIDE
    ienable1 = FALSE
    ishow1   = SW_HIDE
    ienable2 = FALSE
    ishow2   = SW_HIDE
  END IF
  jenable = FALSE
  jshow   = SW_HIDE
  IF( project(EDIT_LEVEL_1)%Restart )THEN
    CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  ELSE
    CALL EnableControl( iwnd_db,IDB_CHECK1,TRUE )
  END IF
  CALL ShowControl( iwnd_db,IDB_CHECK1 ,SW_SHOWNORMAL )
  CALL SetControlText( iwnd_db,IDB_STATIC28,'X (km)' )
  CALL SetControlText( iwnd_db,IDB_STATIC29,'Y (km)' )
  CALL SetControlText( iwnd_db,IDB_STATIC32,'(km)' )
END IF

CALL EnableControl( iwnd_db,IDB_REAL1 ,ienable1 )
CALL EnableControl( iwnd_db,IDB_REAL2 ,ienable1 )
CALL EnableControl( iwnd_db,IDB_REAL10,ienable  )
CALL EnableControl( iwnd_db,IDB_REAL11,ienable  )
CALL EnableControl( iwnd_db,IDB_REAL13,ienable2 )
CALL EnableControl( iwnd_db,IDB_REAL14,ienable2 )
CALL EnableControl( iwnd_db,IDB_INT7  ,ienable2 )
CALL EnableControl( iwnd_db,IDB_INT8  ,ienable2 )
CALL EnableControl( iwnd_db,IDB_INT13 ,ienable2 )
CALL EnableControl( iwnd_db,IDB_INT14 ,ienable2 )
CALL EnableControl( iwnd_db,IDB_COMBO4,ienablex )
CALL ShowControl( iwnd_db,IDB_REAL1 ,ishow1 )
CALL ShowControl( iwnd_db,IDB_REAL2 ,ishow1 )
CALL ShowControl( iwnd_db,IDB_REAL10,ishow  )
CALL ShowControl( iwnd_db,IDB_REAL11,ishow  )
CALL ShowControl( iwnd_db,IDB_REAL13,ishow2 )
CALL ShowControl( iwnd_db,IDB_REAL14,ishow2 )
CALL ShowControl( iwnd_db,IDB_INT7  ,ishow2 )
CALL ShowControl( iwnd_db,IDB_INT8  ,ishow2 )
CALL ShowControl( iwnd_db,IDB_INT13 ,ishow2 )
CALL ShowControl( iwnd_db,IDB_INT14 ,ishow2 )
CALL ShowControl( iwnd_db,IDB_COMBO4,ishow  )
CALL ShowControl( iwnd_db,IDB_STATIC15,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC19,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC20,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC21,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC22,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC33,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC34,ishow )

CALL EnableControl( iwnd_db,IDB_INT3,jenable )
CALL ShowControl( iwnd_db,IDB_INT3    ,jshow )
CALL ShowControl( iwnd_db,IDB_STATIC60,jshow )
CALL ShowControl( iwnd_db,IDB_STATIC61,jshow )

CALL EnableControl( iwnd_db,IDB_COMBO3,kenable )
CALL ShowControl( iwnd_db,IDB_COMBO3,kshow )

DO i = 3,6
  CALL EnableControl( iwnd_db,REAL_BASE+i   ,nenable )
  CALL EnableControl( iwnd_db,REAL_BASE+i+12,menable )
  CALL EnableControl( iwnd_db,INT_BASE+i+6  ,menable )
  CALL EnableControl( iwnd_db,INT_BASE+i+12 ,menable )
  CALL ShowControl( iwnd_db,REAL_BASE+i   ,nshow )
  CALL ShowControl( iwnd_db,REAL_BASE+i+12,mshow )
  CALL ShowControl( iwnd_db,INT_BASE+i+6  ,mshow )
  CALL ShowControl( iwnd_db,INT_BASE+i+12 ,mshow )
END DO

RETURN
END
!***********************************************************************
!               CheckLocalCoordinates
!***********************************************************************
SUBROUTINE CheckLocalCoordinates( iwnd_db,id_level,lreset,lok )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE errorParam_fd
USE GUItool_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: id_level
LOGICAL,              INTENT( IN ) :: lreset

LOGICAL lok
INTEGER i
INTEGER(POINTER_LEN) ictrl

LOGICAL, EXTERNAL :: hasError

!==== Prevent/Warn on change of coordinate type if Operational Incidents have been defined

IF( ichoice(1,id_level) /= dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord )THEN
  IF( scenario(EDIT_LEVEL_1)%nrel > 0 )THEN
    CALL SetError( WN_ERROR, &
                  'Changing coordinate types will invalidate ALL release locations', &
                  'The release locations will need to be redefined in the RELEASE Editor.', &
                  'Do you want to make the change?', &
                  'CheckCoordinates' )
    CALL ShowWarningMessage( iwnd_db,.FALSE. )
    lok = .NOT.hasError()
    CALL InitError()
  ELSE
    lok = .TRUE.
  END IF
  IF( .NOT.lok )THEN
    ichoice(1,id_level) = dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord
    CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )
    ictrl = GetDlgItem( iwnd_db,RADIO_BASE+ichoice(1,id_level) )
    i = SetFocus( ictrl )
  ELSE
    IF( lreset )THEN
      IF( dbreal(3,id_level) /= DEF_VAL_R )dbreal(3,id_level) = NOT_SET_R
      IF( dbreal(4,id_level) /= DEF_VAL_R )dbreal(4,id_level) = NOT_SET_R
      IF( dbreal(5,id_level) /= DEF_VAL_R )dbreal(5,id_level) = NOT_SET_R
      IF( dbreal(6,id_level) /= DEF_VAL_R )dbreal(6,id_level) = NOT_SET_R
      DO i = 3,6
        CALL compute_DMS( dbreal(i,id_level),dbint(i+6,id_level), &
                          dbint(i+12,id_level),dbreal(i+12,id_level) )
      END DO
      CALL SetEditRs( iwnd_db,dbreal(3,id_level),3,4 )
      CALL SetEditIs( iwnd_db,dbint(9,id_level),9,4 )
      CALL SetEditIs( iwnd_db,dbint(15,id_level),15,4 )
      CALL SetEditRs( iwnd_db,dbreal(15,id_level),15,4 )
    END IF
  END IF
ELSE
  lok = .TRUE.
END IF

RETURN
END
!***********************************************************************
!               CheckDomainDefinition
!***********************************************************************
SUBROUTINE check_domain_definition( iwnd_db,id_level,lok )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE param_fd
USE GUItool_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER i, irv, nz_check, max_vres
LOGICAL check_domain_h, check_domain_v

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

nError = NO_ERROR

!---- Check to see that all resolution parameters are set

check_domain_h = .TRUE.
check_domain_v = .TRUE.

i = 7
DO WHILE( i <= 9 .AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 7 )
        CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%domain%hRes,string2,irv )
        string1 = 'the Horizontal resolution'
      CASE( 9 )
        CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%domain%vRes,string2,irv )
        string1 = 'the Vertical resolution'
      CASE DEFAULT
        string2 = ' '
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 2
END DO

!---- Check to see that all reference point parameters are set

IF( (ichoice(1,id_level) == I_CARTESIAN) .AND. lcheck(1,id_level) )THEN
  i = 1
  DO WHILE( i <= 2 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 1 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%reference%lon,string2,irv)
          string1 = 'the Reference Point Longitude'
        CASE( 2 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%reference%lat,string2,irv)
          string1 = 'the Reference Point Latitude'
        CASE DEFAULT
          string2 = ' '
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
  i = 10
  DO WHILE( i <= 11 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 10 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%reference%x,string2,irv)
          string1 = 'the Reference Point X value'
        CASE( 11 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%reference%y,string2,irv)
          string1 = 'the Reference Point Y value'
        CASE DEFAULT
          string2 = ' '
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
ELSE IF( ichoice(1,id_level) == I_UTM )THEN
  IF( dbint(3,id_level) == NOT_SET_I )THEN
    nError  = IV_ERROR
    string1 = 'the UTM Zone'
    irv = 0
  END IF
END IF

!---- Check to see that all domain parameters are set

  i = 3
  DO WHILE( i <= 6 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 3 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%domain%xMin,string2,irv)
          string1 = 'the minimum X value'
        CASE(  4 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%domain%xMax,string2,irv)
          string1 = 'the maximum X value'
        CASE( 5 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%domain%yMin,string2,irv)
          string1 = 'the minimum Y value'
        CASE( 6 )
          CALL set_real_string(dlgDomain(DEFAULT_LEVEL)%spatial%domain%yMax,string2,irv)
          string1 = 'the maximum Y value'
        CASE DEFAULT
          string2 = ' '
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
  IF( dbreal(8,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%domain%zMax,string2,irv )
    string1 = 'the maximum Z value'
  END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eInform  = ' '
  eRoutine = 'CheckDomain'
  IF( irv == 0 )THEN
    eAction  = ' '
  ELSE IF( irv < 0 )THEN
    eAction  = 'Default="default"'
  ELSE
    eAction  = 'Default='//TRIM(string2)
  END IF
  eMessage = 'Please set '//TRIM(string1)
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

!---- Check values where possible

IF( check_domain_h )THEN
  IF( dbreal(3,id_level) /= DEF_VAL_R .AND. dbreal(4,id_level) /= DEF_VAL_R )THEN
    IF( dbreal(4,id_level) <= dbreal(3,id_level) )THEN
      nError  = IV_ERROR
      string1 = 'The maximum value must be > the minimum value'
      string2 = 'horizontal domain'
      GOTO 9998
    END IF
    IF( ichoice(1,id_level) == I_LATLON )THEN
      IF( ABS(dbreal(3,id_level)) > 89.9 .OR. ABS(dbreal(4,id_level)) > 89.9 )THEN
        nError  = IV_ERROR
        string1 = 'The Latitude must be between -89.9 and 89.9'
        string2 = 'horizontal domain'
        GOTO 9998
      END IF
    END IF
  END IF

  IF( dbreal(5,id_level) /= DEF_VAL_R .AND. dbreal(6,id_level) /= DEF_VAL_R )THEN
    IF( dbreal(6,id_level) <= dbreal(5,id_level) )THEN
      nError  = IV_ERROR
      string1 = 'The maximum value must be > the minimum value'
      string2 = 'horizontal domain'
      GOTO 9998
    END IF
    IF( ichoice(1,id_level) == I_LATLON )THEN
      IF( ABS(dbreal(5,id_level)) > 360. .OR. ABS(dbreal(6,id_level)) > 360. )THEN
        nError  = IV_ERROR
        string1 = 'The Longitude must be between -180 and 180'
        string2 = 'horizontal domain'
        GOTO 9998
      END IF
    END IF
  END IF

END IF

IF( dbreal(9,id_level) /= DEF_VAL_R .AND. check_domain_v )THEN
  IF( dbreal(9,id_level) <= 0.0 )THEN
    nError  = IV_ERROR
    string1 = 'The vertical resolution must be > 0'
    string2 = 'vertical resolution'
    GOTO 9998
  END IF

  IF( dbreal(8,id_level) < dbreal(9,id_level) )THEN
    nError  = IV_ERROR
    string1 = 'The vertical domain must be > vertical resolution'
    string2 = 'vertical domain'
    GOTO 9998
  END IF

  nz_check = NINT( dbreal(8,id_level)/dbreal(9,id_level) )
  IF( nz_check > max_vres() )THEN
    nError = IV_ERROR
    WRITE(string3,*)max_vres()
    string3 = ADJUSTL(string3)
    string1 = 'The vertical resolution must be > (vertical domain)/' &
                                             //TRIM(string3)
    string2 = 'vertical resolution or vertical domain'
    GOTO 9998
  ELSE
    dbreal(9,id_level) = dbreal(8,id_level)/FLOAT(MAX(1,nz_check))
  END IF
END IF

9998 CONTINUE

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eRoutine = 'CheckDomain'
  eInform  = ' '
  eMessage = 'Invalid '//TRIM(string2)//' parameter'
  eAction  = TRIM(string1)
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Initialize OPTIONS Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_options( iwnd_db,id_level )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE SCIPtool
USE tooluser_fd

!     This routine initializes the PLOT Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

TYPE( char16T ), DIMENSION(:), ALLOCATABLE :: substrate
INTEGER ios, mode
INTEGER i, nsub
TYPE ( CMD ) MyCmd !Command Structure

INTEGER   enable(2)

CHARACTER(PATH_MAXLENGTH) samfile

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
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_1)%ID%name)//' OPTIONS'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF
!
!     Check Boxes
!
samfile = TRIM(dlgOptions(EDIT_LEVEL_2)%samplerFile)
lcheck(2,id_level) = samfile /= ' ' !sampler output
dbtext(2,id_level) = samfile
!
!     Edit Boxes - Reals
!
dbreal(1,id_level)  = dlgOptions(EDIT_LEVEL_2)%delMin
dbreal(2,id_level)  = dlgOptions(EDIT_LEVEL_2)%wwTrop
dbreal(3,id_level)  = dlgOptions(EDIT_LEVEL_2)%slTrop
dbreal(4,id_level)  = dlgOptions(EDIT_LEVEL_2)%epsTrop
dbreal(5,id_level)  = dlgOptions(EDIT_LEVEL_2)%massMin
dbreal(6,id_level)  = dlgOptions(EDIT_LEVEL_2)%timeAvg
dbreal(7,id_level)  = dlgOptions(EDIT_LEVEL_2)%uuCalm
dbreal(8,id_level)  = dlgOptions(EDIT_LEVEL_2)%slCalm
dbreal(9,id_level)  = dlgOptions(EDIT_LEVEL_2)%zDosage
dbreal(10,id_level) = dlgOptions(EDIT_LEVEL_2)%dtSampler
!
!     Edit Boxes - Integers
!
dbint(2,id_level) = dlgOptions(EDIT_LEVEL_2)%nzBL
dbint(1,id_level) = dlgOptions(EDIT_LEVEL_2)%mGrd

CALL ClearList( iwnd_db,IDB_COMBO1 )

mode = 0
  nsub = SCIPNumSubstrates( mode )
ALLOCATE( substrate(nsub),STAT=ios )
IF( ios == 0 )THEN
  ios = SCIPGetSubstrates( mode,substrate )
  IF( ios == SCIPsuccess )THEN
    DO i = 1,nsub
      CALL AddList( iwnd_db,IDB_COMBO1,-1,substrate(i)%string,ios )
    END DO
    IF( dlgOptions(EDIT_LEVEL_2)%substrate+1 > nsub )THEN
      DO i = nsub+1,dlgOptions(EDIT_LEVEL_2)%substrate+1
        WRITE(substrate(nsub)%string,'(A,I2.2)')'Substrate ',i
        CALL AddList( iwnd_db,IDB_COMBO1,-1,substrate(nsub)%string,ios )
      END DO
      CALL SetListSelString( iwnd_db,IDB_COMBO1,substrate(nsub)%string,ios )
    ELSE
      CALL SetListSelString( iwnd_db,IDB_COMBO1, &
                             substrate(dlgOptions(EDIT_LEVEL_2)%substrate+1)%string,ios )
    END IF
    MyCmd%id     = IDB_OPTIONS
    MyCmd%cntrl  = IDB_COMBO1
    MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
    MyCmd%button = MyCmd%cntrl - COMBO_BASE
    MyCmd%level  = id_level
    CALL process_combo( iwnd_db,MyCmd )
  END IF
END IF
IF( ALLOCATED(substrate) )DEALLOCATE( substrate,STAT=ios )

CALL load_dialog_options( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Load OPTIONS Dialog Box
!*******************************************************************************
SUBROUTINE load_dialog_options( iwnd_db,id_level )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
!
!     This routine loads the OPTIONS Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i

CALL SetChecks( iwnd_db,lcheck(1,id_level),1,2 )
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,10 )
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,2 )
IF( lcheck(2,id_level) )THEN
  IF( dbtext(2,id_level) == ' ' )THEN
    string1 = 'Please specify a sampler input file'
  ELSE
    string1 = 'Sampler Input file = '//TRIM(dbtext(2,id_level))
  END IF
  CALL EnableControl( iwnd_db,IDB_REAL10,TRUE )
  CALL ShowControl( iwnd_db,IDB_REAL10,SW_SHOWNORMAL )
  CALL ShowControl( iwnd_db,IDB_STATIC18,SW_SHOWNORMAL )
ELSE
  string1 = ' '
  CALL EnableControl( iwnd_db,IDB_REAL10,FALSE )
  CALL ShowControl( iwnd_db,IDB_REAL10,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC18,SW_HIDE )
END IF
CALL SetControlText( iwnd_db,IDB_STATIC17,string1 )
CALL EnableButtons( iwnd_db,lcheck(2,id_level),2,1 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK2,FALSE )
  CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
  DO i = 1,10
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  END DO
  DO i = 1,2
    CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  END DO
END IF

RETURN
END
!*******************************************************************************
!            save OPTIONS Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_options( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
!
!     This routine saves the NEW Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

CHARACTER(PATH_MAXLENGTH) samfile

!     Check Boxes

IF( lcheck(2,id_level) )THEN !Sampler output
  samfile = dbtext(2,id_level)
  dlgOptions(EDIT_LEVEL_2)%samplerFile = samfile
  dlgOptions(EDIT_LEVEL_2)%dtSampler   = dbreal(10,id_level)
ELSE
  dlgOptions(EDIT_LEVEL_2)%samplerFile = ' '
  dlgOptions(EDIT_LEVEL_2)%dtSampler   = dlgOptions(DEFAULT_LEVEL)%dtSampler
END IF
!
!     Edit Boxes - Reals
!
dlgOptions(EDIT_LEVEL_2)%delMin  = dbreal(1,id_level)
dlgOptions(EDIT_LEVEL_2)%wwTrop  = dbreal(2,id_level)
dlgOptions(EDIT_LEVEL_2)%slTrop  = dbreal(3,id_level)
dlgOptions(EDIT_LEVEL_2)%epsTrop = dbreal(4,id_level)
dlgOptions(EDIT_LEVEL_2)%massMin = dbreal(5,id_level)
dlgOptions(EDIT_LEVEL_2)%timeAvg = dbreal(6,id_level)
dlgOptions(EDIT_LEVEL_2)%uuCalm  = dbreal(7,id_level)
dlgOptions(EDIT_LEVEL_2)%slCalm  = dbreal(8,id_level)
dlgOptions(EDIT_LEVEL_2)%zDosage = dbreal(9,id_level)
!
!     Edit Boxes - Integers
!
dlgOptions(EDIT_LEVEL_2)%nzBL = dbint(2,id_level)
dlgOptions(EDIT_LEVEL_2)%mGrd = dbint(1,id_level)

dlgOptions(EDIT_LEVEL_2)%substrate = idbcmbo(1,id_level) - 1

RETURN
END
!***********************************************************************
!               CheckOptionsDefinition
!***********************************************************************
SUBROUTINE check_options_definition( iwnd_db,id_level,lok )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER i, irv
INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

nError = NO_ERROR

i = 1
DO WHILE( i <= 9 .AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%delMin,string2,irv )
        string1 = 'the Surface resolution'
      CASE( 2 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%wwTrop,string2,irv )
        string1 = 'the Tropopause Turbulence'
      CASE( 3 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slTrop,string2,irv )
        string1 = 'the Tropopause Scale'
      CASE( 4 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%epsTrop,string2,irv )
        string1 = 'the Tropopause Dissipation'
      CASE( 5 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%delMin,string2,irv )
        string1 = 'the Minimum puff mass'
      CASE( 6 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%timeAvg,string2,irv )
        string1 = 'the Conditional averaging time'
      CASE( 7 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%uuCalm,string2,irv )
        string1 = 'the Calm conditions turbulence'
      CASE( 8 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slCalm,string2,irv )
        string1 = 'the Calm conditions scale'
      CASE( 9 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%zDosage,string2,irv )
        string1 = 'the Surface dosage height'
      CASE DEFAULT
        string2 = ' '
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

IF( dbreal(10,id_level) == NOT_SET_R .AND. lcheck(2,id_level) )THEN
  nError = IV_ERROR
  CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%dtSampler,string2,irv )
  string1 = 'the Sampler minimum output interval'
END IF

i = 1
DO WHILE( i <= 2 .AND. nError == NO_ERROR )
  IF( dbint(i,id_level) == NOT_SET_I )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        string1 = 'the Puff Split Grid level'
      CASE( 2 )
        CALL set_int_string( dlgOptions(DEFAULT_LEVEL)%nzBL,string2,irv )
        string1 = 'the BL resolution'
      CASE DEFAULT
        string2 = ' '
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

i = 1
DO WHILE( i <= 9 .AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) < 0.0 )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%delMin,string2,irv )
        string1 = 'the Surface resolution'
      CASE( 2 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%wwTrop,string2,irv )
        string1 = 'the Tropopause Turbulence'
      CASE( 3 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slTrop,string2,irv )
        string1 = 'the Tropopause Scale'
      CASE( 4 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%epsTrop,string2,irv )
        string1 = 'the Tropopause Dissipation'
      CASE( 5 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%massMin,string2,irv )
        string1 = 'the Minimum puff mass'
      CASE( 6 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%timeAvg,string2,irv )
        string1 = 'the Conditional averaging time'
      CASE( 7 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%uuCalm,string2,irv )
        string1 = 'the Calm conditions turbulence'
      CASE( 8 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slCalm,string2,irv )
        string1 = 'the Calm conditions scale'
      CASE( 9 )
        CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%zDosage,string2,irv )
        string1 = 'the Surface dosage height'
      CASE DEFAULT
        string2 = ' '
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = TRIM(string1)//' cannot be negative'
  GOTO 9999
END IF

IF( dbreal(3,id_level) == 0.0 )THEN
  nError = IV_ERROR
  CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slTrop,string2,irv )
  string1 = 'the Tropopause Scale'
END IF
IF( dbreal(8,id_level) == 0.0 )THEN
  nError = IV_ERROR
  CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%slCalm,string2,irv )
  string1 = 'the Calm conditions scale'
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = TRIM(string1)//' must be greater than zero'
  GOTO 9999
END IF

IF( dbreal(9,id_level) < 0.0 )THEN
  nError = IV_ERROR
  CALL set_real_string( dlgOptions(DEFAULT_LEVEL)%zDosage,string2,irv )
  string1 = 'the Surface dosage height'
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = TRIM(string1)//' must be greater than or equal to zero'
  GOTO 9999
END IF

IF( lcheck(2,id_level) )THEN
  IF( dbtext(2,id_level) == ' ' )nError = IV_ERROR
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = 'Please specify a sampler input file'
  irv = 0
  GOTO 9999
END IF

RETURN

9999 CONTINUE
eInform  = ' '
eRoutine = 'CheckOptions'
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
!               OptionsButton
!***********************************************************************
SUBROUTINE options_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE dialog_fi

!     This routine processes LOAD/DEFAULT PUSHBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

LOGICAL lok

CHARACTER(PATH_MAXLENGTH) filenam

!---- Select by Button number

IF( id_button == 2 )THEN !Sampler output
  lok = .FALSE.
  filenam = dbtext(2,id_level)
  CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
  IF( lok )THEN
    dbtext(2,id_level) = TRIM(filenam)
    string2 = 'Sampler Input file = '//TRIM(dbtext(2,id_level))
    CALL SetControlText( iwnd_db,IDB_STATIC17,string2 )
  END IF
ELSE
  CALL default_button_edt( iwnd_db,id_dialog,id_button,id_level ) !Default buttons
END IF

RETURN
END
!*******************************************************************************
!            Initialize PRJDEF Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_prjdef( iwnd_db,id_level )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

INTEGER enable(2)
INTEGER irv
TYPE( CMD ) MyCmd !Command Structure
!
!     Buttons
!
enable(1) = project(EDIT_LEVEL_1)%Edit !LOAD
enable(2) = project(EDIT_LEVEL_1)%Edit !DEFAULT
CALL EnableButtons( iwnd_db,enable,14,2 )
enable(1) = project(EDIT_LEVEL_1)%Edit !OK
enable(2) = project(EDIT_LEVEL_1)%Edit !MC Browse
CALL EnableButtons( iwnd_db,enable,1,2 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON1 ,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON2 ,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON14,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON15,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_1)%ID%name)
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF
!
!     Edit Boxes - String
!
dbtext(1,id_level) = AddNull( project(EDIT_LEVEL_2)%Title )
dbtext(3,id_level) = AddNull( project(EDIT_LEVEL_2)%audit%Analyst )
dbtext(4,id_level) = AddNull( project(EDIT_LEVEL_2)%audit%Version )
dbtext(5,id_level) = AddNull( project(EDIT_LEVEL_2)%audit%CreateDate )
dbtext(6,id_level) = AddNull( project(EDIT_LEVEL_2)%ID%name )
dbtext(7,id_level) = AddNull( project(EDIT_LEVEL_2)%ID%path )

CALL ClearList( iwnd_db,IDB_COMBO1 )

CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
CALL EnableControl( iwnd_db,IDB_EDIT2,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO1,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_EDIT2,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC13,SW_HIDE )
dbcmbo(1,id_level) = ' '

CALL ClearList( iwnd_db,IDB_COMBO2 )

string1 = 'Passive (No dynamics) '
string2 = 'Dynamics (Buoyant)'
string3 = 'Dense gas dynamics'
CALL AddList( iwnd_db,IDB_COMBO2,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO2,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO2,-1,string3,irv )
IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  IF( project(EDIT_LEVEL_2)%DenseGas )THEN
    string4 = string3
  ELSE
    string4 = string2
  END IF
ELSE
  string4 = string1
END IF
CALL SetControlIcon( iwnd_db,IDB_STATIC71,scipuff_hndl )
CALL SetControlIcon( iwnd_db,IDB_STATIC72,dynamic_hndl )
CALL SetControlIcon( iwnd_db,IDB_STATIC73,dense_hndl )

CALL SetListSelString( iwnd_db,IDB_COMBO2,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO2
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Disable static puffs'
string2 = 'Enable static puffs'
CALL AddList( iwnd_db,IDB_COMBO6,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO6,-1,string2,irv )
IF( project(EDIT_LEVEL_2)%StaticPuffs )THEN
  string4 = string2
ELSE
  string4 = string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO6,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO6
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Hazard Area Off'
CALL AddList( iwnd_db,IDB_COMBO7,-1,string1,irv )
CALL SetListSelString( iwnd_db,IDB_COMBO7,string1,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO7
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Standard'
string2 = 'Fast'
CALL AddList( iwnd_db,IDB_COMBO8,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO8,-1,string2,irv )
IF( BTEST(project(EDIT_LEVEL_2)%Mode,FAST_MODE) )THEN
  string4 = string2
ELSE
  string4 = string1
END IF
string1 = 'Reverse Standard'
string2 = 'Reverse Fast'
CALL AddList(iwnd_db,IDB_COMBO8,-1,string1,irv)
CALL AddList(iwnd_db,IDB_COMBO8,-1,string2,irv)
IF( BTEST(project(2)%Mode,REVERSE_MODE) )THEN
  string1 = string4
  string4 = "Reverse "//string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO8,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO8
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Disable source nests'
string2 = 'Enable source nests'
CALL AddList( iwnd_db,IDB_COMBO9,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO9,-1,string2,irv )
IF( project(EDIT_LEVEL_2)%SourceNests )THEN
  string4 = string2
ELSE
  string4 = string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO9,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO9
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

CALL EnableControl( iwnd_db,IDB_COMBO10,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO10,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC25,SW_HIDE )

nradio(4,id_level)  = 3 !Dynamics Type
ichoice(4,id_level) = idbcmbo(2,id_level)

CALL load_dialog_prjdef( iwnd_db,id_level )

!==== Show/Hide hazard controls

CALL EnableControl( iwnd_db,IDB_COMBO7,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC23,SW_HIDE )

RETURN
END
!*******************************************************************************
!            Load PRJDEF Dialog Box
!*******************************************************************************
SUBROUTINE load_dialog_prjdef( iwnd_db,id_level )

USE resource_fd
USE SwinAPI
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
!
!     This routine loads the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i

CALL SetEditTs( iwnd_db,dbtext(1,id_level),1,7 )
DO i = 4,7
  CALL EnableControl( iwnd_db,EDIT_BASE+i,FALSE )
END DO
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  DO i = 1,3
    CALL EnableControl( iwnd_db,EDIT_BASE+i,FALSE )
  END DO
  CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO2,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO6,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO7,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO8,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO9,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO10,FALSE )
  CALL EnableControl( iwnd_db,IDB_RADIO31,FALSE)
  CALL EnableControl( iwnd_db,IDB_RADIO32,FALSE)
  CALL EnableControl( iwnd_db,IDB_RADIO33,FALSE)
ELSE
  IF( project(EDIT_LEVEL_1)%Restart )THEN
    CALL EnableControl( iwnd_db,IDB_COMBO2,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO6,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO7,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO8,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO9,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO10,FALSE )
  END IF
END IF

RETURN
END
!*******************************************************************************
!            save PRJDEF Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_prjdef( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE GUImatl_fi
USE files_fi
USE errorParam_fd
!
!     This routine saves the NEW Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

CHARACTER(128), EXTERNAL :: StripNull

LOGICAL, EXTERNAL :: hasError,lastError
!
!     Edit Boxes - Text
!
project(EDIT_LEVEL_2)%Title                = StripNull( dbtext(1,id_level) )
project(EDIT_LEVEL_2)%audit%Analyst        = StripNull( dbtext(3,id_level) )
project(EDIT_LEVEL_2)%audit%Classification = StripNull( dbcmbo(1,id_level) )
IF( project(EDIT_LEVEL_2)%audit%Classification == 'Other' )THEN
  project(EDIT_LEVEL_2)%audit%Classification = StripNull( dbtext(2,id_level) )
END IF

SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%Dynamic  = .TRUE.
    project(EDIT_LEVEL_2)%DenseGas = .FALSE.
  CASE( 3 )
    project(EDIT_LEVEL_2)%Dynamic  = .TRUE.
    project(EDIT_LEVEL_2)%DenseGas = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%Dynamic  = .FALSE.
    project(EDIT_LEVEL_2)%DenseGas = .FALSE.
END SELECT

SELECT CASE( idbcmbo(6,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%StaticPuffs = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%StaticPuffs = .FALSE.
END SELECT

project(EDIT_LEVEL_2)%Mode = 0

SELECT CASE( idbcmbo(8,id_level) )
  CASE( 1 )
    project(EDIT_LEVEL_2)%Mode = 0
  CASE( 2 )
    project(EDIT_LEVEL_2)%Mode = IBSET(project(EDIT_LEVEL_2)%Mode,FAST_MODE)
  CASE( 3 )
    project(EDIT_LEVEL_2)%Mode = IBSET(project(2)%Mode,REVERSE_MODE)
  CASE( 4 )
    project(EDIT_LEVEL_2)%Mode = IBSET(project(2)%Mode,REVERSE_MODE)
    project(EDIT_LEVEL_2)%Mode = IBSET(project(2)%Mode,FAST_MODE)
END SELECT

SELECT CASE( idbcmbo(9,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%SourceNests = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%SourceNests = .FALSE.
END SELECT

RETURN
END
!***********************************************************************
!               CheckProjectDefinition
!***********************************************************************
SUBROUTINE check_project_definition( iwnd_db,id_level,lok )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE errorParam_fd

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: CheckFile

nError  = NO_ERROR
eAction = ' '
eInform = ' '

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eRoutine = 'CheckProject'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!               set_utm_reference
!***********************************************************************
SUBROUTINE set_utm_reference( ilev )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE datums

!     set UTM reference point as middle of the domain

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ilev !dialog level

INTEGER irv

LOGICAL, EXTERNAL :: hasError

!      integer*4 i

IF( dlgDomain(ilev)%spatial%domain%coord == I_UTM )THEN
  IF( dlgDomain(ilev)%spatial%domain%zoneUTM == NOT_SET_I )THEN
    CALL SetError( IV_ERROR, &
                  'Invalid UTM reference zone', &
                  'Must set a reference zone for UTM runs', &
                  'Please set the zone in the DOMAIN dialog', &
                  'setUTMreference' )
    GOTO 9999
  END IF
  IF( dlgDomain(ilev)%spatial%domain%xMin == DEF_VAL_R .OR. &
      dlgDomain(ilev)%spatial%domain%xMax == DEF_VAL_R .OR. &
      dlgDomain(ilev)%spatial%domain%yMin == DEF_VAL_R .OR. &
      dlgDomain(ilev)%spatial%domain%yMax == DEF_VAL_R .OR. &
      dlgDomain(ilev)%spatial%domain%zoneUTM == DEF_VAL_I )THEN
      dlgDomain(ilev)%spatial%reference%x = NOT_SET_R
      dlgDomain(ilev)%spatial%reference%y = NOT_SET_R
  ELSE
    dlgDomain(ilev)%spatial%reference%x = 0.5*(dlgDomain(ilev)%spatial%domain%xMax + dlgDomain(ilev)%spatial%domain%xMin)
    dlgDomain(ilev)%spatial%reference%y = 0.5*(dlgDomain(ilev)%spatial%domain%yMax + dlgDomain(ilev)%spatial%domain%yMin)
    irv = UTM2LL( dlgDomain(ilev)%spatial%domain%zoneUTM, &
                  dlgDomain(ilev)%spatial%reference%x,dlgDomain(ilev)%spatial%reference%y, &
                  dlgDomain(ilev)%spatial%reference%lat,dlgDomain(ilev)%spatial%reference%lon )
    IF( irv /= 0 )THEN
      CALL setUTMerrorGUI( 'UTM2LL',irv )
      GOTO 9999
    END IF
  END IF
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!               setUTMerrorGUI
!***********************************************************************
SUBROUTINE setUTMerrorGUI( subr_name,ierr )

USE errorParam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: subr_name
INTEGER,      INTENT( IN ) :: ierr

CHARACTER(20) string
CHARACTER(20) dtms_string

IF( TRIM(subr_name) == 'UTMdomain' )THEN
  dtms_string = 'DtmsUTMfromLLAdomain'
  WRITE(string,*)ierr
  string = ADJUSTL(string)
ELSE
  dtms_string = 'DtmsExtendedZone'
  SELECT CASE( ierr )
    CASE( 3 )
      string = 'Longitude not in range [-180,+180]'
    CASE( 4 )
      string = 'Latitude not in range [-80,+84]'
    CASE( 12 )
      string = 'Invalid zone number'
    CASE( 13 )
      string = 'Northing not in range [-1E5,+1E5] km'
    CASE( 14 )
      string = 'Easting more than 9 deg from central meridian'
    CASE( 18 )
      string = 'More that 9.17 deg from central meridian'
    CASE( 20 )
      string = 'Error initializing DATUMS'
    CASE DEFAULT
      string = 'Unexpected error condition'
  END SELECT
END IF

CALL SetError( IV_ERROR,'Error from '//TRIM(dtms_string), &
               'Error = '//TRIM(string),' ',TRIM(subr_name) )

RETURN
END
