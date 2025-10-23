!*******************************************************************************
!            Initialize URBAN Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_urban( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE urban_fi
USE basic_fd
USE winAPI_fd, ONLY: SW_HIDE

!
!     This routine initializes the URBAN Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER   enable(2), renable, i, j

CHARACTER(128), EXTERNAL :: AddNull
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
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_1)%ID%name)//' URBAN'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF

!==== On/Off

IF( project(EDIT_LEVEL_1)%Edit )THEN
  renable = TRUE
ELSE
  renable = FALSE
END IF

nradio(1,id_level) = 2 !UDM On/Off
IF( urban(id_level)%udm%On )THEN
  ichoice(1,id_level) = 2
ELSE
  ichoice(1,id_level) = 1
END IF

nradio(2,id_level) = 2 !UWM On/Off
IF( urban(id_level)%uwm%On )THEN
  ichoice(2,id_level) = 2
ELSE
  ichoice(2,id_level) = 1
END IF

nradio(3,id_level) = 2 !MSS On/Off
IF( urban(id_level)%mss%On )THEN
  ichoice(3,id_level) = 2
ELSE
  ichoice(3,id_level) = 1
END IF

CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,3 )

DO i = 0,2
  DO j = 1,2
    CALL EnableControl( iwnd_db,RADIO_BASE+10*i+j,renable )
  END DO
END DO

!==== UDM Settings

CALL init_dialog_urban_UDM( iwnd_db,id_level )

!==== MSS Settings

CALL init_dialog_urban_MSS( iwnd_db,id_level )

!==== UWM Settings

CALL init_dialog_urban_UWM( iwnd_db,id_level )

!==== Database Settings

string1 = urban(id_level)%DBName
CALL AddPath( string1,urban(id_level)%DBPath )
dbtext(1,id_level) = AddNull( string1 )
CALL SetEditTs( iwnd_db,dbtext(1,id_level),1,1 )

!==== Enable/Disable and Show/Hide controls

CALL show_urban( iwnd_db,id_level,project(EDIT_LEVEL_1)%Edit )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Initialize URBAN UDM Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_urban_UDM( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE SwinAPI
USE urban_fi
!
!     This routine initializes the URBAN/UDM Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv, i

string1 = 'Off'
string2 = 'On'
CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO1,-1,string2,irv )
idbcmbo(1,id_level) = urban(id_level)%udm%ChannnelMode + 1

string1 = 'Bulk Only'
string2 = 'Individual'
CALL AddList( iwnd_db,IDB_COMBO2,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO2,-1,string2,irv )
idbcmbo(2,id_level) = urban(id_level)%udm%BuildingInter

string1 = 'Low'
string2 = 'Medium'
string3 = 'High'
CALL AddList( iwnd_db,IDB_COMBO3,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO3,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO3,-1,string3,irv )
idbcmbo(3,id_level) = urban(id_level)%udm%PuffSplit + 1

CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO4,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO4,-1,string3,irv )
idbcmbo(4,id_level) = urban(id_level)%udm%PuffMerge + 1

DO i = 1,4
  CALL SetListSel( iwnd_db,COMBO_BASE+i,idbcmbo(i,id_level)-1,irv )
END DO

dbreal(1,id_level) = urban(id_level)%udm%MaxSrcHgt
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Initialize URBAN MSS Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_urban_MSS( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE SwinAPI
USE urban_fi
!
!     This routine initializes the URBAN/MSS Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv, i

string1 = 'Low    (5m)'
string2 = 'Medium (4m)'
string3 = 'High   (3m)'
CALL AddList( iwnd_db,IDB_COMBO7,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO7,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO7,-1,string3,irv )
idbcmbo(7,id_level) = urban(id_level)%mss%HGrid + 1

string1 = 'Small  (0.6km)'
string2 = 'Medium (0.8km)'
string3 = 'Large  (1.0km)'
CALL AddList( iwnd_db,IDB_COMBO8,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO8,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO8,-1,string3,irv )
idbcmbo(8,id_level) = urban(id_level)%mss%HDomain + 1

string1 = 'Low    (21pts)'
string2 = 'Medium (26pts)'
string3 = 'High   (31pts)'
CALL AddList( iwnd_db,IDB_COMBO9,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO9,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO9,-1,string3,irv )
idbcmbo(9,id_level) = urban(id_level)%mss%VGrid + 1

string1 = 'Average building hgt'
string2 = 'User defined hgt'
CALL AddList( iwnd_db,IDB_COMBO10,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO10,-1,string2,irv )
idbcmbo(10,id_level) = urban(id_level)%mss%VCluster + 1

DO i = 7,10
  CALL SetListSel( iwnd_db,COMBO_BASE+i,idbcmbo(i,id_level)-1,irv )
END DO

dbreal(9,id_level) = urban(id_level)%mss%ResolHgt
CALL SetEditRs( iwnd_db,dbreal(9,id_level),9,1 )

dbint(2,id_level) = urban(id_level)%mss%PTimeStep
dbint(3,id_level) = urban(id_level)%mss%PSyncFac
dbint(4,id_level) = urban(id_level)%mss%NumPart
dbint(5,id_level) = urban(id_level)%mss%ConcTime

CALL SetEditIs( iwnd_db,dbint(2,id_level),2,4 )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Initialize URBAN UWM Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_urban_UWM( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE SwinAPI
USE urban_fi
!
!     This routine initializes the URBAN/UWM Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv, i

string1 = 'Coarse'
string2 = 'Medium'
string3 = 'Fine'
CALL AddList( iwnd_db,IDB_COMBO5,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO5,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO5,-1,string3,irv )
idbcmbo(5,id_level) = urban(id_level)%uwm%Grid

string1 = 'Auto calculate'
string2 = 'User defined'
CALL AddList( iwnd_db,IDB_COMBO6,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO6,-1,string2,irv )
idbcmbo(6,id_level) = urban(id_level)%uwm%Domain

DO i = 5,6
  CALL SetListSel( iwnd_db,COMBO_BASE+i,idbcmbo(i,id_level)-1,irv )
END DO

nradio(4,id_level) = 2 !LLA/UTM
IF( urban(id_level)%uwm%coor == UWM_LLA )THEN
  ichoice(4,id_level) = 1
ELSE
  ichoice(4,id_level) = 2
END IF
CALL SetRadios( iwnd_db,ichoice(4,id_level),nradio(4,id_level),4,1 )

dbint(1,id_level) = urban(id_level)%uwm%zone
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )

dbreal(2,id_level) = urban(id_level)%uwm%dsmin
dbreal(3,id_level) = urban(id_level)%uwm%dmax
dbreal(4,id_level) = urban(id_level)%uwm%dmin
dbreal(5,id_level) = urban(id_level)%uwm%xBL
dbreal(6,id_level) = urban(id_level)%uwm%xTR
dbreal(7,id_level) = urban(id_level)%uwm%yBL
dbreal(8,id_level) = urban(id_level)%uwm%yTR
CALL SetEditRs( iwnd_db,dbreal(2,id_level),2,7 )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            save URBAN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_urban( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
!
!     This routine saves the URBAN Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

!==== On/Off

urban(id_level)%udm%On = ichoice(1,id_level) == 2
urban(id_level)%uwm%On = ichoice(2,id_level) == 2
urban(id_level)%mss%On = ichoice(3,id_level) == 2

!==== UDM Settings

IF( urban(id_level)%udm%On )CALL save_dialog_urban_UDM( iwnd_db,id_level )

!==== MSS Settings

IF( urban(id_level)%mss%On )CALL save_dialog_urban_MSS( iwnd_db,id_level )

!==== UWM Settings

IF( urban(id_level)%uwm%On )CALL save_dialog_urban_UWM( iwnd_db,id_level )

!==== Database Settings

string1 = dbtext(1,id_level)
CALL SplitName( string1,urban(id_level)%DBName,urban(id_level)%DBPath )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            save URBAN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_urban_UDM( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
!
!     This routine saves the URBAN/UDM Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

urban(id_level)%udm%ChannnelMode  = idbcmbo(1,id_level)-1
urban(id_level)%udm%BuildingInter = idbcmbo(2,id_level)
urban(id_level)%udm%PuffSplit     = idbcmbo(3,id_level)-1
urban(id_level)%udm%PuffMerge     = idbcmbo(4,id_level)-1

urban(id_level)%udm%MaxSrcHgt = dbreal(1,id_level)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            save URBAN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_urban_MSS( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
!
!     This routine saves the URBAN/MSS Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

urban(id_level)%mss%HGrid    = idbcmbo( 7,id_level)-1
urban(id_level)%mss%HDomain  = idbcmbo( 8,id_level)-1
urban(id_level)%mss%VGrid    = idbcmbo( 9,id_level)-1
urban(id_level)%mss%VCluster = idbcmbo(10,id_level)-1

IF( urban(id_level)%mss%VCluster == MSS_USER )THEN
  urban(id_level)%mss%ResolHgt = dbreal(9,id_level)
END IF

urban(id_level)%mss%PTimeStep = dbint(2,id_level)
urban(id_level)%mss%PSyncFac  = dbint(3,id_level)
urban(id_level)%mss%NumPart   = dbint(4,id_level)
urban(id_level)%mss%ConcTime  = dbint(5,id_level)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            save URBAN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_urban_UWM( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
!
!     This routine saves the URBAN/UWM Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

urban(id_level)%uwm%Grid   = idbcmbo(5,id_level)
urban(id_level)%uwm%Domain = idbcmbo(6,id_level)

IF( urban(id_level)%uwm%Domain == UWM_AUTO )THEN
  urban(id_level)%uwm%dsmin = dbreal(2,id_level)
  urban(id_level)%uwm%dmax  = dbreal(3,id_level)
  urban(id_level)%uwm%dmin  = dbreal(4,id_level)
ELSE
  IF( ichoice(4,id_level) == 1 )THEN
    urban(id_level)%uwm%coor = UWM_LLA
  ELSE
    urban(id_level)%uwm%coor = UWM_UTM
    urban(id_level)%uwm%zone = dbint(1,id_level)
  END IF
  urban(id_level)%uwm%xBL = dbreal(5,id_level)
  urban(id_level)%uwm%xTR = dbreal(6,id_level)
  urban(id_level)%uwm%yBL = dbreal(7,id_level)
  urban(id_level)%uwm%yTR = dbreal(8,id_level)
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban( iwnd_db,id_level,edit )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Top level - Project in edit mode

LOGICAL lenable
INTEGER, DIMENSION(1) ::  enable

!==== UDM

lenable = edit .AND. (ichoice(1,id_level) == 2)
CALL show_urban_UDM( iwnd_db,id_level,lenable )

!==== MSS

lenable = edit .AND. (ichoice(3,id_level) == 2)
CALL show_urban_MSS( iwnd_db,id_level,lenable )

!==== UWM

lenable = edit .AND. (ichoice(2,id_level) == 2)
CALL show_urban_UWM( iwnd_db,id_level,lenable )

!==== Database

IF( edit )THEN
  enable(1) = TRUE
ELSE
  enable(1) = FALSE
END IF

CALL EnableControl( iwnd_db,IDB_EDIT1,enable(1) )
CALL EnableButtons( iwnd_db,enable,16,1 )

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban_UDM( iwnd_db,id_level,edit )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Project in edit mode and UDM on

INTEGER enable, i

IF( edit )THEN
  enable = TRUE
ELSE
  enable = FALSE
END IF

DO i = 1,4
  CALL EnableControl( iwnd_db,COMBO_BASE+i,enable )
END DO

CALL EnableControl( iwnd_db,IDB_REAL1,enable )

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban_MSS( iwnd_db,id_level,edit )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd
USE winAPI_fd, ONLY: SW_SHOWNORMAL, SW_HIDE

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Project in edit mode and MSS on

INTEGER enable, show, i

IF( edit )THEN
  enable = TRUE
ELSE
  enable = FALSE
END IF

DO i = 7,10
  CALL EnableControl( iwnd_db,COMBO_BASE+i,enable )
END DO
DO i = 2,5
  CALL EnableControl( iwnd_db,INT_BASE+i,enable )
END DO

IF( idbcmbo(10,id_level) == 2 )THEN
  enable = TRUE
  show   = SW_SHOWNORMAL
ELSE
  enable = FALSE
  show   = SW_HIDE
END IF

CALL EnableControl( iwnd_db,IDB_REAL9,enable )
CALL ShowControl( iwnd_db,IDB_REAL9,show )
CALL ShowControl( iwnd_db,IDB_STATIC69,show )
CALL ShowControl( iwnd_db,IDB_STATIC70,show )

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban_UWM( iwnd_db,id_level,edit )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Project in edit mode and UWM on

LOGICAL auto
INTEGER enable, i

IF( edit )THEN
  enable = TRUE
ELSE
  enable = FALSE
END IF

DO i = 5,6
  CALL EnableControl( iwnd_db,COMBO_BASE+i,enable )
END DO

auto = idbcmbo(6,id_level) == 1
CALL show_urban_UWM_auto( iwnd_db,id_level,edit,auto )

CALL show_urban_UWM_user( iwnd_db,id_level,edit,.NOT.auto )

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban_UWM_auto( iwnd_db,id_level,edit,auto )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd
USE winAPI_fd, ONLY: SW_SHOWNORMAL, SW_HIDE

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Project in edit mode and UWM on and domain is auto
LOGICAL,              INTENT( IN ) :: auto     !UWM domain is auto

INTEGER show, enable, i

IF( auto )THEN
  show = SW_SHOWNORMAL
ELSE
  show = SW_HIDE
END IF

IF( edit )THEN
  enable = TRUE
ELSE
  enable = FALSE
END IF

DO i = 45,48
  CALL ShowControl( iwnd_db,STATIC_BASE+i,show )
END DO

DO i = 2,4
  CALL EnableControl( iwnd_db,REAL_BASE+i,enable )
  CALL ShowControl( iwnd_db,REAL_BASE+i,show )
END DO

RETURN
END
!*******************************************************************************
!           Show/Hide URBAN Dialog Box Controls
!*******************************************************************************
SUBROUTINE show_urban_UWM_user( iwnd_db,id_level,edit,user )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE basic_fd
USE winAPI_fd, ONLY: SW_SHOWNORMAL, SW_HIDE

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
LOGICAL,              INTENT( IN ) :: edit     !Project in edit mode and UWM on and domain is auto
LOGICAL,              INTENT( IN ) :: user     !UWM domain is user

LOGICAL utm
INTEGER show, zshow, enable, zenable, i

utm = ichoice(4,id_level)==2

IF( user )THEN
  show = SW_SHOWNORMAL
  IF( utm )THEN
    zshow = SW_SHOWNORMAL
  ELSE
    zshow = SW_HIDE
  END IF
ELSE
  show  = SW_HIDE
  zshow = SW_HIDE
END IF

IF( edit )THEN
  enable  = TRUE
  IF( utm )THEN
    zenable = TRUE
  ELSE
    zenable = FALSE
  END IF
ELSE
  enable  = FALSE
  zenable = FALSE
END  IF

IF( utm )THEN
  string1 = 'Easting : '
  string2 = 'Northing : '
ELSE
  string1 = 'Longitude : '
  string2 = 'Latitude : '
END IF
CALL SetControlText( iwnd_db,IDB_STATIC54,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC55,string2 )

DO i = 50,55
  CALL ShowControl( iwnd_db,STATIC_BASE+i,show )
END DO

DO i = 5,8
  CALL EnableControl( iwnd_db,REAL_BASE+i,enable )
  CALL ShowControl( iwnd_db,REAL_BASE+i,show )
END DO

DO i = 31,32
  CALL EnableControl(iwnd_db,RADIO_BASE+i,enable )
  CALL ShowControl( iwnd_db,RADIO_BASE+i,show )
END DO

CALL EnableControl( iwnd_db,IDB_INT1,zenable )
CALL ShowControl( iwnd_db,IDB_INT1,zshow )
CALL ShowControl( iwnd_db,IDB_STATIC51,zshow )

RETURN
END
!***********************************************************************
!               Urban buttons
!***********************************************************************
SUBROUTINE urban_button( iwnd_db,id_button,id_level )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE errorParam_fd
USE param_fd
USE GUItool_fi
USE SwinAPI
USE urban_fi

!     This routine processes LOAD/DEFAULT PUSHBUTTONs from Urban Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

INTEGER irv
LOGICAL lok

CHARACTER(256) filename

LOGICAL, EXTERNAL :: hasError

SELECT CASE( id_button )
  CASE( 1 )                                          !OK
    CALL PushButton( iwnd_db,IDB_BUTTON1,ID_OK,irv )
  CASE( 14 )                                          !Load from file
    CALL GetUrbanFile( iwnd_db,filename )
    IF( LEN_TRIM(filename) > 0 )THEN
      CALL LoadUrbanFile( iwnd_db,filename,id_level )
    END IF
    IF( hasError() )CALL ShowErrorMessage( iwnd_db )
    CALL init_dialog_urban( iwnd_db,id_level )
    loadfile(19) = TRIM(filename)
  CASE( 15 )                                          !Load Default
    urban(EDIT_LEVEL_2) = urban(DEFAULT_LEVEL)
    CALL init_dialog_urban( iwnd_db,id_level )
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               GetUrbanFile
!***********************************************************************
SUBROUTINE GetUrbanFile( iwnd_db,filename )

USE pcscipuf_fi
USE create_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),      INTENT( IN  ) :: iwnd_db   !Dialog Box handle
CHARACTER(*),              INTENT( OUT ) :: filename  !Selected file

CHARACTER(128) ctmpf,ctmpd,ccust,inname
CHARACTER(200) cbuff
CHARACTER(128) label
CHARACTER(4)   cext
LOGICAL        lok
INTEGER        irv

CHARACTER(128), EXTERNAL :: StripNull

filename = ' '

ccust  = 'UDM/MSS (*.urb)'//cnull//'*.urb'//cnull//cnull
cbuff  = 'UWM (*.uwm)'//cnull//'*.uwm'//cnull// &
         'All Files (*.*)'//cnull//'*.*'//cnull//cnull
cext   = 'urb'
label  = 'Urban Files'
inname = loadfile(19)
lok    = .TRUE.
CALL SplitName( inname,ctmpf,ctmpd )
IF( ctmpf == ' ' )ctmpf = '*.'//TRIM(cext)
IF( ctmpd == ' ' )ctmpd = TRIM(path_tmp)
irv = EnableWindow( hwnd_pw,FALSE )
CALL OpenFileDlg( ccust,cbuff,ctmpf,ctmpd,cext,label,iwnd_db,MyAppInst,lok )
irv = EnableWindow( hwnd_pw,TRUE )
IF( lok )THEN
  filename = StripNull( ctmpf )
  ccust    = StripNull( ctmpd )
  CALL AddPath( filename,ccust )
END IF

RETURN
END
!***********************************************************************
!               LoadUrbanFile
!***********************************************************************
SUBROUTINE LoadUrbanFile( iwnd_db,filename,id_level )

USE create_fi
USE urban_fi
USE error_fi
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)
CHARACTER(*),         INTENT( IN ) :: filename !File to load from

CHARACTER(256) file
CHARACTER( 32) tail

INTEGER i, nch

LOGICAL pathError,remoteDrive

LOGICAL,        EXTERNAL :: hasError
CHARACTER(256), EXTERNAL :: AddExtension

pathError = .FALSE.
remoteDrive = .FALSE.

nch = LEN_TRIM(filename)
i   = INDEX(filename,'.',BACK=.TRUE.)

IF( i > 0 .AND. i < nch )THEN
  tail = filename(i+1:nch)
  CALL cupper( tail )
ELSE
  tail = ''
END IF

IF( TRIM(tail) == 'URB' )THEN
  CALL ReadURB( iwnd_db,filename,id_level,pathError,remoteDrive )
ELSE IF( TRIM(tail) == 'UWM')THEN
  CALL ReadUWM( iwnd_db,filename,id_level,pathError,remoteDrive )
ELSE
  tail = 'URB/UWM'
  file = AddExtension( filename,'urb' )
  CALL ReadURB( iwnd_db,file,id_level,pathError,remoteDrive )
  IF( hasError() )GOTO 9999
  file = AddExtension( filename,'uwm' )
  CALL ReadUWM( iwnd_db,file,id_level,pathError,remoteDrive )
  IF( hasError() )GOTO 9999
END IF

IF( remoteDrive )THEN
  CALL SetError( RD_ERROR, &
                'Urban database path in '//TRIM(filename)//' points to a remote drive', &
                'Urban database cannot reside on a remote drive', &
                'Urban database has been reset to default database', &
                'Read'//TRIM(tail) )
  CALL ShowErrorMessage( iwnd_db )
END IF
IF( pathError )THEN
  CALL SetError( RD_ERROR, &
                'Some path names in '//TRIM(filename)//' are invalid', &
                'Paths have been reset to default values',' ', &
                'Read'//TRIM(tail) )
  CALL ShowErrorMessage( iwnd_db )
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!               ReadURB
!***********************************************************************
SUBROUTINE ReadURB( iwnd_db,filename,id_level,pathError,remoteDrive )

USE urban_fi
USE files_fi
USE errorParam_fd
USE GUIparam_fd
USE SCIPresults_fd
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN )    :: iwnd_db     !Dialog Box handle
CHARACTER(*),         INTENT( IN )    :: filename    !File to load from
INTEGER,              INTENT( IN )    :: id_level    !ID of Structure to set
LOGICAL,              INTENT( INOUT ) :: pathError   !Error flag on checking paths
LOGICAL,              INTENT( INOUT ) :: remoteDrive !Error flag on remote drives

TYPE( UrbanSettings )        :: urb
CHARACTER(32), DIMENSION(20) :: carg
CHARACTER(256)               :: line, tmp
CHARACTER(128)               :: eString
CHARACTER(32)                :: kwrd
INTEGER                      :: narg
INTEGER                      :: nch
LOGICAL                      :: lerr
INTEGER                      :: ivalue
REAL(4)                      :: fvalue

LOGICAL,        EXTERNAL :: hasError, CheckPath, CheckFile, IsRemoteDrive
INTEGER,        EXTERNAL :: OpenUrbanFile
INTEGER,        EXTERNAL :: ReadInteger
REAL(4),        EXTERNAL :: ReadFloat
CHARACTER(256), EXTERNAL :: ReadNextLine

!==== Read data into local

urb%udm     = urban(DEFAULT_LEVEL)%udm
urb%mss     = urban(DEFAULT_LEVEL)%mss
urb%DBname  = urban(DEFAULT_LEVEL)%DBname
urb%DBPath  = urban(DEFAULT_LEVEL)%DBPath
urb%TerPath = urban(DEFAULT_LEVEL)%TerPath
urb%LUPath  = urban(DEFAULT_LEVEL)%LUPath
urb%PopPath = urban(DEFAULT_LEVEL)%PopPath

!==== Open file for reading

ivalue = OpenUrbanFile( iwnd_db,filename,.FALSE. )
IF( ivalue /= SCIPsuccess )GOTO 9999

!==== UDM line

carg = ' '

CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,20,lerr )
IF( lerr )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Error reading UDM settings', &
                 eString,'Settings='//TRIM(line), &
                'ReadURB' )
  GOTO 9999
END IF

!==== UDM On/Off

ivalue = ReadInteger( carg(1) )
IF( hasError() )GOTO 9999
urb%udm%On = (ivalue == 1)

IF( urb%udm%On )THEN

!==== UDM Channelling

  ivalue = ReadInteger( carg(2) )
  IF( hasError() )GOTO 9999
  urb%udm%ChannnelMode = ivalue

!==== UDM Maximum source height

  fvalue = ReadFloat( carg(6) )
  IF( hasError() )GOTO 9999
  urb%udm%MaxSrcHgt = fvalue

!==== UDM Building Interactions

  ivalue = ReadInteger( carg(12) )
  IF( hasError() )GOTO 9999
  urb%udm%BuildingInter = ivalue

!==== UDM Puff Splitting

  ivalue = ReadInteger( carg(13) )
  IF( hasError() )GOTO 9999
  urb%udm%PuffSplit = ivalue

!==== UDM Puff Merging

  ivalue = ReadInteger( carg(14) )
  IF( hasError() )GOTO 9999
  urb%udm%Puffmerge = ivalue

END IF

!==== MSS line

carg = ' '

CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,20,lerr )
IF( lerr )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Error reading MSS settings', &
                 eString,'Settings='//TRIM(line), &
                'ReadURB' )
  GOTO 9999
END IF

!==== MSS On/Off

ivalue = ReadInteger( carg(1) )
IF( hasError() )GOTO 9999
urb%mss%On = (ivalue == 1)

IF( urb%mss%On )THEN

!==== MSS Horizontal grid

  ivalue = ReadInteger( carg(2) )
  IF( hasError() )GOTO 9999
  urb%mss%HGrid = ivalue

!==== MSS Horizontal domain

  ivalue = ReadInteger( carg(3) )
  IF( hasError() )GOTO 9999
  urb%mss%HDomain = ivalue

!==== MSS Vertical grid

  ivalue = ReadInteger( carg(4) )
  IF( hasError() )GOTO 9999
  urb%mss%VGrid = ivalue

!==== MSS Vertical clustering

  ivalue = ReadInteger( carg(5) )
  IF( hasError() )GOTO 9999
  urb%mss%VCluster = ivalue

!==== MSS Enhamced resolution height

  IF( urb%mss%VCluster /= MSS_AVGHGT )THEN
    fvalue = ReadFloat( carg(6) )
    IF( hasError() )GOTO 9999
    urb%mss%ResolHgt = fvalue
  END IF

!==== MSS Particle release time step

  ivalue = ReadInteger( carg(7) )
  IF( hasError() )GOTO 9999
  urb%mss%PTimeStep = ivalue

!==== MSS Particle synchronization

  ivalue = ReadInteger( carg(8) )
  IF( hasError() )GOTO 9999
  urb%mss%PSyncFac = ivalue

!==== MSS Number of Particle per step

  ivalue = ReadInteger( carg(9) )
  IF( hasError() )GOTO 9999
  urb%mss%Numpart = ivalue

!==== MSS Concentration averaging time

  ivalue = ReadInteger( carg(10) )
  IF( hasError() )GOTO 9999
  urb%mss%ConcTime = ivalue

END IF

!==== Urban Database path

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999
IF( IsRemoteDrive(line) )THEN
  remoteDrive = .TRUE.
  CALL InitError()
ELSE
  IF( CheckPath(line) )THEN
    urb%DBPath = TRIM(ADJUSTL(line))
  ELSE
    pathError = .TRUE.
    CALL InitError()
  END IF
END IF

!==== Urban Database name

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

tmp = ADJUSTL(line)
CALL AddPath( tmp,urb%DBPath )
IF( CheckFile(tmp) )THEN
  urb%DBName = TRIM(ADJUSTL(line))
ELSE
  pathError = .TRUE.
  CALL InitError()
END IF

!==== Urban Terrain path

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

IF( CheckPath(line) )THEN
  urb%TerPath = TRIM(ADJUSTL(line))
ELSE
  pathError = .TRUE.
  CALL InitError()
END IF

!==== Urban Landuse path

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

IF( CheckPath(line) )THEN
  urb%LUPath = TRIM(ADJUSTL(line))
ELSE
  pathError = .TRUE.
  CALL InitError()
END IF

!==== Urban Population path

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

IF( CheckPath(line) )THEN
  urb%PopPath = TRIM(ADJUSTL(line))
ELSE
  pathError = .TRUE.
  CALL InitError()
END IF

!==== Move local to output

urban(id_level)%udm     = urb%udm
urban(id_level)%mss     = urb%mss
urban(id_level)%DBname  = urb%DBname
urban(id_level)%DBPath  = urb%DBPath
urban(id_level)%TerPath = urb%TerPath
urban(id_level)%LUPath  = urb%LUPath
urban(id_level)%PopPath = urb%PopPath

9999 CONTINUE

CALL CloseUrbanFile()

RETURN
END
!***********************************************************************
!               ReadUWM
!***********************************************************************
SUBROUTINE ReadUWM( iwnd_db,filename,id_level,pathError,remoteDrive )

USE urban_fi
USE files_fi
USE errorParam_fd
USE GUIparam_fd
USE SCIPresults_fd
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db     !Dialog Box handle
CHARACTER(*),         INTENT( IN    ) :: filename    !File to load from
INTEGER,              INTENT( IN    ) :: id_level    !ID of Structure to set
LOGICAL,              INTENT( INOUT ) :: pathError   !Error flag on checking paths
LOGICAL,              INTENT( INOUT ) :: remoteDrive !Error flag on checking paths

TYPE( UrbanSettings ) urb

CHARACTER(32), DIMENSION(10) :: carg
CHARACTER(256)               :: line, tmp
CHARACTER(128)               :: eString
CHARACTER(32)                :: kwrd
INTEGER                      :: narg
INTEGER                      :: nch
LOGICAL                      :: lerr
INTEGER                      :: ivalue
REAL(4)                      :: fvalue
INTEGER                      :: inext

LOGICAL,        EXTERNAL :: hasError, CheckPath, CheckFile, IsRemoteDrive
INTEGER,        EXTERNAL :: OpenUrbanFile
INTEGER,        EXTERNAL :: ReadInteger
REAL(4),        EXTERNAL :: ReadFloat
CHARACTER(256), EXTERNAL :: ReadNextLine

!==== Read data into local

urb%uwm    = urban(DEFAULT_LEVEL)%uwm
urb%DBname = urban(DEFAULT_LEVEL)%DBname
urb%DBPath = urban(DEFAULT_LEVEL)%DBPath

!==== Open file for reading

ivalue = OpenUrbanFile( iwnd_db,filename,.FALSE. )
IF( ivalue /= SCIPsuccess )GOTO 9999

!==== UDM line

carg = ' '

CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,10,lerr )
IF( lerr )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Error reading UWM settings', &
                 eString,'Settings='//TRIM(line), &
                'ReadUWM' )
  GOTO 9999
END IF

!==== UWM On/Off

ivalue = ReadInteger( carg(1) )
IF( hasError() )GOTO 9999

urb%uwm%On = (ivalue == 1)

IF( urb%uwm%On )THEN

!==== UWM Grid Resolution

  ivalue = ReadInteger( carg(2) )
  IF( hasError() )GOTO 9999
  urb%uwm%Grid = ivalue

!==== UWM Domain

  ivalue = ReadInteger( carg(3) )
  IF( hasError() )GOTO 9999
  urb%uwm%Domain = ivalue

!==== UDM Auto Domain

  IF( urb%uwm%Domain == UWM_AUTO )THEN

!====== UWM Minimum horizontal resolution

    fvalue = ReadFloat( carg(4) )
    IF( hasError() )GOTO 9999
    urb%uwm%dsmin = fvalue

!====== UWM Maximum domain size

    fvalue = ReadFloat( carg(5) )
    IF( hasError() )GOTO 9999
    urb%uwm%dmax = fvalue

!====== UWM Minimum domain size

    fvalue = ReadFloat( carg(6) )
    IF( hasError() )GOTO 9999
    urb%uwm%dmin = fvalue

  ELSE

!====== UWM coordinate system

    ivalue = ReadInteger( carg(4) )
    IF( hasError() )GOTO 9999
    urb%uwm%coor = ivalue

    inext = 5

!====== UWM UTM zone

    IF( urb%uwm%coor == UWM_UTM )THEN
      ivalue = ReadInteger( carg(5) )
      IF( hasError() )GOTO 9999
      urb%uwm%zone = ivalue
      inext = inext + 1

!====== UWM Domain bottom left X

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%xBL = fvalue
      inext = inext + 1

!====== UWM Domain bottom left Y

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%yBL = fvalue
      inext = inext + 1

!====== UWM Domain top right X

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%xTR = fvalue
      inext = inext + 1

!====== UWM Domain top right Y

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%yTR = fvalue
      inext = inext + 1

    ELSE

!====== LL Domain bottom left Y

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%yBL = fvalue
      inext = inext + 1

!====== LL Domain top right Y

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%yTR = fvalue
      inext = inext + 1

!====== LL Domain bottom left X

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%xBL = fvalue
      inext = inext + 1

!====== UWM Domain top right X

      fvalue = ReadFloat( carg(inext) )
      IF( hasError() )GOTO 9999
      urb%uwm%xTR = fvalue
      inext = inext + 1

   END IF

  END IF

END IF

!==== Urban Database path

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

IF( IsRemoteDrive(line) )THEN
  remoteDrive = .TRUE.
  CALL InitError()
ELSE
  IF( CheckPath(line) )THEN
    urb%DBPath = TRIM(ADJUSTL(line))
  ELSE
    pathError = .TRUE.
    CALL InitError()
  END IF
END IF

!==== Urban Database name

line = ReadNextLine( lun_tmp )
IF( hasError() )GOTO 9999

tmp = ADJUSTL(line)
CALL AddPath(tmp,urb%DBPath)
IF( CheckFile(tmp) )THEN
  urb%DBName = TRIM(ADJUSTL(line))
ELSE
  pathError = .TRUE.
  CALL InitError()
END IF

!==== Move local to output

urban(id_level)%uwm    = urb%uwm
urban(id_level)%DBname = urb%DBname
urban(id_level)%DBPath = urb%DBPath

9999 CONTINUE

CALL CloseUrbanFile()

RETURN
END
!***********************************************************************
!               WriteUrbanFile
!***********************************************************************
SUBROUTINE WriteUrbanFile( iwnd_db,id_level,filename )

USE create_fi
USE urban_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db     !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data)
CHARACTER(*),         INTENT( IN ) :: filename !File to load from

CHARACTER(256)file

LOGICAL,        EXTERNAL :: hasError
CHARACTER(256), EXTERNAL :: AddExtension

file = AddExtension(filename,'urb')
CALL WriteURB( iwnd_db,id_level,file )
IF( hasError() )GOTO 9999
file = AddExtension(filename,'uwm')
CALL WriteUWM( iwnd_db,id_level,file )
IF( hasError() )GOTO 9999

9999 CONTINUE

RETURN
END
!***********************************************************************
!               WriteURB
!***********************************************************************
SUBROUTINE WriteURB( iwnd_db,id_level,filename )

USE SCIPresults_fd
USE errorParam_fd
USE urban_fi
USE files_fi
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db     !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data)
CHARACTER(*),         INTENT( IN ) :: filename !File to write to

INTEGER irv, iline, nline

CHARACTER(128) eString

CHARACTER(256), DIMENSION(7) :: line

LOGICAL,       EXTERNAL :: hasError
INTEGER,       EXTERNAL :: OpenUrbanFile
CHARACTER(32), EXTERNAL :: WriteInteger
CHARACTER(32), EXTERNAL :: WriteFloat

!==== Open file for writing

irv = OpenUrbanFile( iwnd_db,filename,.TRUE. )
IF( irv /= SCIPsuccess )GOTO 9999

iline = 0

!==== Build UDM line

iline = iline + 1
IF( urban(id_level)%udm%On )THEN
  line(iline) = TRIM(ADJUSTL(WriteInteger(UDM_ON)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%udm%ChannnelMode)))//' 0 0 0'
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%udm%MaxSrcHgt)))//' 0.0 0.0 0.0 0.0 0.0'
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%udm%BuildingInter)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%udm%PuffSplit)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%udm%PuffMerge)))//' 0'
ELSE
  line(iline) = TRIM(ADJUSTL(WriteInteger(UDM_OFF)))
END IF

!==== Build MSS line

iline = iline + 1
IF( urban(id_level)%mss%On )THEN
  line(iline) = TRIM(ADJUSTL(WriteInteger(MSS_ON)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%HGrid)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%HDomain)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%VGrid)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%VCluster)))
  IF( urban(id_level)%mss%VCluster == MSS_USER )THEN
    line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%mss%ResolHgt)))
  ELSE
    line(iline) = TRIM(line(iline))//' 0.0'
  ENDIF
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%PTimeStep)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%PSyncFac)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%NumPart)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%mss%ConcTime)))
ELSE
  line(iline) = TRIM(ADJUSTL(WriteInteger(MSS_OFF)))
END IF

!==== Build Database lines

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%DBPath))

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%DBName))

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%TerPath))

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%LUPath))

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%PopPath))

!==== Write the lines

nline = iline
irv   = 0
iline = 1
DO WHILE( irv == 0 .AND. iline <= nLine )
  WRITE(lun_tmp,'(A)',IOSTAT=irv)TRIM(line(iline))
  iline = iline + 1
END DO

IF( irv /= 0 )THEN
  WRITE(line(1),*)irv
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( WR_ERROR, &
                'Error writing UDM settings', &
                 eString,'Error='//TRIM(ADJUSTL(line(1))), &
                'WriteUWM' )
END IF

9999 CONTINUE

CALL CloseUrbanFile()

RETURN
END
!***********************************************************************
!               WriteUWM
!***********************************************************************
SUBROUTINE WriteUWM( iwnd_db,id_level,filename )

USE SCIPresults_fd
USE errorParam_fd
USE urban_fi
USE files_fi
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db     !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data)
CHARACTER(*),         INTENT( IN ) :: filename !File to write to

INTEGER irv, iline, nline

CHARACTER(128) eString

CHARACTER(256), DIMENSION(3) :: line

LOGICAL,       EXTERNAL :: hasError
INTEGER,       EXTERNAL :: OpenUrbanFile
CHARACTER(32), EXTERNAL :: WriteInteger
CHARACTER(32), EXTERNAL :: WriteFloat

!==== Open file for writing

irv = OpenUrbanFile( iwnd_db,filename,.TRUE. )
IF( irv /= SCIPsuccess )GOTO 9999

iline = 0

!==== Build UWM line

iline = iline + 1
IF( urban(id_level)%uwm%On )THEN
  line(iline) = TRIM(ADJUSTL(WriteInteger(UWM_ON)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%uwm%Grid)))
  line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%uwm%Domain)))
  IF( urban(id_level)%uwm%Domain == UWM_AUTO )THEN
    line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%dsmin)))
    line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%dmax)))
    line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%dmin)))
  ELSE
    line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%uwm%coor)))
    IF( urban(id_level)%uwm%coor == UWM_UTM )THEN
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteInteger(urban(id_level)%uwm%zone)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%xBL)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%yBL)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%xTR)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%yTR)))
    ELSE
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%yBL)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%yTR)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%xBL)))
      line(iline) = TRIM(line(iline))//' '//TRIM(ADJUSTL(WriteFloat(urban(id_level)%uwm%xTR)))
    ENDIF
  ENDIF
ELSE
  line(iline) = TRIM(ADJUSTL(WriteInteger(UWM_OFF)))
END IF

!==== Build Database lines

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%DBPath))

iline = iline + 1
line(iline) = TRIM(ADJUSTL(urban(id_level)%DBName))

!==== Write the lines

nline = iline
irv   = 0
iline = 1
DO WHILE( irv == 0 .AND. iline <= nLine )
  WRITE(lun_tmp,'(A)',IOSTAT=irv)TRIM(line(iline))
  iline = iline + 1
END DO

IF( irv /= 0 )THEN
  WRITE(line(1),*)irv
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( WR_ERROR, &
                'Error writing UWM settings', &
                 eString,'Error='//TRIM(ADJUSTL(line(1))), &
                'WriteUWM' )
END IF

9999 CONTINUE

CALL CloseUrbanFile()

RETURN
END
!***********************************************************************
!               OpenUrbanFile
!***********************************************************************
INTEGER FUNCTION OpenUrbanFile( iwnd_db,filename,writeFlag )

USE files_fi
USE pcscipuf_fi
USE tooluser_fd
USE errorParam_fd

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd_db  !Dialog Box handle
CHARACTER(*),           INTENT( IN ) :: filename
LOGICAL,                INTENT( IN ) :: writeFlag

LOGICAL lexist
INTEGER ios

CHARACTER(128) eString

INTEGER, EXTERNAL :: sysDeleteFile

OpenUrbanFile = SCIPfailure

INQUIRE( FILE=filename,EXIST=lexist )

IF( writeFlag )THEN
  IF( lexist )THEN
    ios = sysDeleteFile( filename )
    IF( ios == SCIPfailure )THEN
      WRITE(string1,*)ios
      CALL ReportFileName( eString,'File=',filename )
      CALL SetError( OP_ERROR, &
                    'Error deleting existing urban file', &
                     eString,' ', &
                    'OpenUrbanFile' )
      GOTO 9999
    END IF
  END IF
  OPEN(UNIT=lun_tmp,FILE=filename,STATUS='NEW',IOSTAT=ios)
  IF( ios /= 0 )THEN
    WRITE(string1,*)ios
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Error opening urban file, Error='//TRIM(ADJUSTL(string1)), &
                   eString,' ', &
                  'OpenUrbanFile' )
  END IF
ELSE
  IF( lexist )THEN
    OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(string1,*)ios
      CALL SetError( OP_ERROR, &
                    'Error opening urban file, Error='//TRIM(ADJUSTL(string1)), &
                     eString,' ', &
                    'OpenUrbanFile' )
    END IF
  ELSE
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( NF_ERROR, &
                  'Urban file not found.', &
                   eString,'Urban setting reset to default values', &
                  'OpenUrbanFile' )
    CALL ShowInfoMessage( iwnd_db )
    OpenUrbanFile = SCIPnull
    GOTO 9999
  END IF
ENDIF

OpenUrbanFile = SCIPsuccess

9999 CONTINUE

RETURN
END
!***********************************************************************
!               CloseUrbanFile
!***********************************************************************
SUBROUTINE CloseUrbanFile()

USE files_fi

IMPLICIT NONE

LOGICAL lopen
INTEGER ios

INQUIRE(UNIT=lun_tmp,OPENED=lopen)

IF( lopen )CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN
END
!***********************************************************************
!               ReadInteger
!***********************************************************************
INTEGER FUNCTION ReadInteger( string )

USE default_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

CHARACTER(32) int
INTEGER ios,val

ReadInteger = NOT_SET_I

int = ADJUSTR(string)
READ(int,*,IOSTAT=ios)val
IF( ios /= 0 )THEN
  WRITE(int,*)ios
  CALL SetError( RD_ERROR, &
                'Unable to read integer from string ('//TRIM(string)//')', &
                'Error='//TRIM(ADJUSTL(int)),' ', &
                'ReadInteger' )
END IF

ReadInteger = val

9999 CONTINUE

RETURN
END
!***********************************************************************
!               ReadFloat
!***********************************************************************
REAL(4) FUNCTION ReadFloat( string )

USE default_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

CHARACTER(32) float
INTEGER ios
REAL(4) val

ReadFloat = NOT_SET_R

float = ADJUSTR(string)
READ(float,*,IOSTAT=ios)val
IF( ios /= 0 )THEN
  WRITE(float,*)ios
  CALL SetError( RD_ERROR, &
                'Unable to read REAL(4) from string ('//TRIM(string)//')', &
                'Error='//TRIM(ADJUSTL(float)),' ', &
                'ReadFloat' )
END IF

ReadFloat = val

9999 CONTINUE

RETURN
END
!***********************************************************************
!               ReadNextLine
!***********************************************************************
CHARACTER(256) FUNCTION ReadNextLine( lun )

USE errorParam_fd

IMPLICIT NONE

INTEGER, INTENT(IN) :: lun

CHARACTER(256) val
INTEGER ios

ReadNextLine = ' '

READ(lun,'(A)',IOSTAT=ios)val
IF( ios /= 0 )THEN
  WRITE(val,*)ios
  CALL SetError( RD_ERROR, &
                'Unable to read next line in urban file', &
                'Error='//TRIM(ADJUSTL(val)),' ', &
                'ReadNextLine' )
END IF

ReadNextLine = ADJUSTL(val)

9999 CONTINUE

RETURN
END
!***********************************************************************
!               WriteInteger
!***********************************************************************
CHARACTER(32) FUNCTION WriteInteger( value )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: value

INTEGER       ios
CHARACTER(32) string

string = ' '

WRITE(string,*,IOSTAT=ios)value

WriteInteger = TRIM(ADJUSTL(string))

RETURN
END
!***********************************************************************
!               WriteInteger
!***********************************************************************
CHARACTER(32) FUNCTION WriteFloat( value )

IMPLICIT NONE

REAL(4), INTENT( IN ) :: value

INTEGER irv
CHARACTER(32) string

string = ' '

CALL set_real_string( value,string,irv )

WriteFloat = TRIM(ADJUSTL(string))

RETURN

END
!***********************************************************************
!                set_default_urban
!***********************************************************************
SUBROUTINE set_default_urban()

USE urban_fi
USE defineok_fd
USE pcscipuf_fi
USE tooluser_fd
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER irv

LOGICAL,        EXTERNAL :: CheckPath, CheckFile
INTEGER,        EXTERNAL :: sysGetProfileString
CHARACTER(128), EXTERNAL :: AddNull

!==== UDM defaults

urban(DEFAULT_LEVEL)%udm%On            = .FALSE.
urban(DEFAULT_LEVEL)%udm%ChannnelMode  = UDM_ON
urban(DEFAULT_LEVEL)%udm%MaxSrcHgt     = 100.0
urban(DEFAULT_LEVEL)%udm%BuildingInter = UDM_INDIVIDUAL
urban(DEFAULT_LEVEL)%udm%PuffSplit     = UDM_MED
urban(DEFAULT_LEVEL)%udm%PuffMerge     = UDM_MED

urban(DEFAULT_LEVEL)%udm%I_reserve = 0
urban(DEFAULT_LEVEL)%udm%J_reserve = 0
urban(DEFAULT_LEVEL)%udm%F_reserve = 0.0

!==== MSS defaults

urban(DEFAULT_LEVEL)%mss%On        = .FALSE.
urban(DEFAULT_LEVEL)%mss%HGrid     = MSS_MED
urban(DEFAULT_LEVEL)%mss%HDomain   = MSS_MEDIUM
urban(DEFAULT_LEVEL)%mss%VGrid     = MSS_MED
urban(DEFAULT_LEVEL)%mss%VCluster  = MSS_AVGHGT
urban(DEFAULT_LEVEL)%mss%ResolHgt  = 20.0
urban(DEFAULT_LEVEL)%mss%PTimeStep = 5
urban(DEFAULT_LEVEL)%mss%PSyncFac  = 1
urban(DEFAULT_LEVEL)%mss%NumPart   = 500
urban(DEFAULT_LEVEL)%mss%ConcTime  = 60

!==== UWM defaults

urban(DEFAULT_LEVEL)%uwm%On     = .FALSE.
urban(DEFAULT_LEVEL)%uwm%Grid   = UWM_MEDIUM
urban(DEFAULT_LEVEL)%uwm%Domain = UWM_AUTO
urban(DEFAULT_LEVEL)%uwm%dsmin  = 50.0
urban(DEFAULT_LEVEL)%uwm%dmax   = 5000.0
urban(DEFAULT_LEVEL)%uwm%dmin   = 500.0
urban(DEFAULT_LEVEL)%uwm%coor   = UWM_UTM
urban(DEFAULT_LEVEL)%uwm%zone   = 12
urban(DEFAULT_LEVEL)%uwm%xBL    = 0.0
urban(DEFAULT_LEVEL)%uwm%yBL    = 0.0
urban(DEFAULT_LEVEL)%uwm%xTR    = 0.0
urban(DEFAULT_LEVEL)%uwm%yTR    = 0.0

!==== Urban database defaults

string4 = AddNull( TRIM(ini_file) )
string1 = AddNull( 'UrbanDatabase' )

!==== database path

string2 = AddNull( 'Path' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,urban(DEFAULT_LEVEL)%DBpath,string4 )
IF( irv == SCIPnull )THEN
  string3 = path_inv     !scipuff data directory
  CALL BackupPath( string3,1 )
  urban(DEFAULT_LEVEL)%DBpath = 'urban'
  CALL AddPath(urban(DEFAULT_LEVEL)%DBpath,string3)
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

IF( .NOT.CheckPath(urban(DEFAULT_LEVEL)%DBpath) )GOTO 9999

!==== database name

string2 = AddNull( 'Name' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,urban(DEFAULT_LEVEL)%DBname,string4 )
IF( irv == SCIPnull )THEN
  urban(DEFAULT_LEVEL)%DBname = 'SCIP.ged'
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

string3 = urban(DEFAULT_LEVEL)%DBname
CALL AddPath( string3,urban(DEFAULT_LEVEL)%DBpath )
IF( .NOT.CheckFile(string3) )GOTO 9999

!==== Other database paths
string1 = AddNull( 'Paths' )

!==== Terrain

string2 = AddNull( 'TerrainDir' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,urban(DEFAULT_LEVEL)%TerPath,string4 )
IF( irv == SCIPnull )THEN
  string3 = path_inv     !scipuff data directory
  CALL BackupPath( string3,1 )
  urban(DEFAULT_LEVEL)%TerPath = 'terrain'
  CALL AddPath( urban(DEFAULT_LEVEL)%TerPath,string3 )
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

IF( .NOT.CheckPath(urban(DEFAULT_LEVEL)%TerPath) )GOTO 9999

!==== Landuse

string2 = AddNull( 'LanduseDir' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,urban(DEFAULT_LEVEL)%LUPath,string4 )
IF( irv == SCIPnull )THEN
  urban(DEFAULT_LEVEL)%LUPath = urban(DEFAULT_LEVEL)%TerPath
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

IF( .NOT.CheckPath(urban(DEFAULT_LEVEL)%LUPath) )GOTO 9999

!==== Population

string2 = AddNull( 'PopDir' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,urban(DEFAULT_LEVEL)%PopPath,string4 )
IF( irv == SCIPnull )THEN
  string3 = path_inv     !scipuff data directory
  CALL BackupPath( string3,1 )
  urban(DEFAULT_LEVEL)%PopPath = 'population'
  CALL AddPath( urban(DEFAULT_LEVEL)%PopPath,string3 )
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

IF( .NOT.CheckPath(urban(DEFAULT_LEVEL)%PopPath) )GOTO 9999

urban(BASE_LEVEL) = urban(DEFAULT_LEVEL)

9999 CONTINUE

RETURN

9998  CONTINUE
CALL SetError( IV_ERROR, &
              'Failure from system function : '//TRIM(string1)//':'//TRIM(string2), &
               TRIM(string4),' ', &
              'sysGetProfileString' )
GOTO 9999

END
!***********************************************************************
!               BuildUrb
!***********************************************************************
SUBROUTINE build_urb( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE param_fd
USE urban_fi

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i,irv
LOGICAL on, show

i = 0

on = .FALSE.

i = i + 1
IF( i > mxlst  )GOTO 9999
IF( urban(EDIT_LEVEL_1)%udm%On )THEN
  on = .TRUE.
  list(i) = 'UDM on'

  IF( urban(EDIT_LEVEL_1)%udm%ChannnelMode /= urban(DEFAULT_LEVEL)%udm%ChannnelMode )THEN
    i = i + 1
    IF( i > mxlst  )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%udm%ChannnelMode )
      CASE( UDM_OFF )
        string1 = 'Off'
      CASE( UDM_ON )
        string1 = 'On'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' Channelling='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%udm%MaxSrcHgt /= urban(DEFAULT_LEVEL)%udm%MaxSrcHgt )THEN
    i = i + 1
    IF( i > mxlst  )GOTO 9999
    CALL set_real_string( urban(EDIT_LEVEL_1)%udm%MaxSrcHgt,string2,irv )
    string1 = TRIM(string2)//' m'
    list(i) = ' Max src hgt='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%udm%BuildingInter /= urban(DEFAULT_LEVEL)%udm%BuildingInter )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%udm%BuildingInter )
      CASE( UDM_BULKONLY )
        string1 = 'Bulk only'
      CASE( UDM_INDIVIDUAL )
        string1 = 'Individual blds'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' Bld interactions='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%udm%PuffSplit /= urban(DEFAULT_LEVEL)%udm%PuffSplit )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%udm%PuffSplit )
      CASE( UDM_LOW )
        string1 = 'Low'
      CASE( UDM_MED )
        string1 = 'Medium'
      CASE( UDM_HI )
        string1 = 'High'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' Puff Split='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%udm%PuffMerge /= urban(DEFAULT_LEVEL)%udm%PuffMerge )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%udm%PuffMerge )
      CASE( UDM_LOW )
        string1 = 'Low'
      CASE( UDM_MED )
        string1 = 'Medium'
      CASE( UDM_HI )
        string1 = 'High'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' Puff Merge='//TRIM(string1)
  END IF
ELSE
  list(i) = 'UDM off'
END IF

i = i + 1
IF( i > mxlst )GOTO 9999
IF( urban(EDIT_LEVEL_1)%mss%On )THEN
  on = .TRUE.
  list(i) = 'MSS on'

  IF( urban(EDIT_LEVEL_1)%mss%HGrid /= urban(DEFAULT_LEVEL)%mss%HGrid )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%mss%HGrid )
      CASE( MSS_LOW )
        string1 = 'Low'
      CASE( MSS_MED )
        string1 = 'Medium'
      CASE( MSS_HI )
        string1 = 'High'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' H Res='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%HDomain /= urban(DEFAULT_LEVEL)%mss%HDomain )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%mss%HDomain )
      CASE( MSS_SMALL )
        string1 = 'Small'
      CASE( MSS_MEDIUM )
        string1 = 'Medium'
      CASE( MSS_LARGE )
        string1 = 'Large'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' H Domain='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%VGrid /= urban(DEFAULT_LEVEL)%mss%VGrid )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%mss%VGrid )
      CASE( MSS_LOW )
        string1 = 'Low'
      CASE( MSS_MED )
        string1 = 'Medium'
      CASE( MSS_HI )
        string1 = 'High'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' V Res='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%VCluster /= urban(DEFAULT_LEVEL)%mss%VCluster )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%mss%VCluster )
      CASE( MSS_AVGHGT )
        string1 = 'Avg Bld Hgt'
      CASE( MSS_USER )
        CALL set_real_string( urban(EDIT_LEVEL_1)%mss%ResolHgt,string2,irv )
        string1 = TRIM(string2)//' m'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' V Cluster='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%PTimeStep /= urban(DEFAULT_LEVEL)%mss%PTimeStep )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    WRITE(string1,*)urban(EDIT_LEVEL_1)%mss%PTimeStep
    list(i) = ' Time Step='//TRIM(ADJUSTL(string1))//'s'
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%PSyncFac /= urban(DEFAULT_LEVEL)%mss%PSyncFac )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    WRITE(string1,*)urban(EDIT_LEVEL_1)%mss%PSyncFac
    list(i) = ' Sync fac='//TRIM(ADJUSTL(string1))
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%NumPart /= urban(DEFAULT_LEVEL)%mss%NumPart )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    WRITE(string1,*)urban(EDIT_LEVEL_1)%mss%NumPart
    list(i) = ' Part/Step='//TRIM(ADJUSTL(string1))
  END IF

  IF( urban(EDIT_LEVEL_1)%mss%ConcTime /= urban(DEFAULT_LEVEL)%mss%ConcTime )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    WRITE(string1,*)urban(EDIT_LEVEL_1)%mss%ConcTime
    list(i) = ' Conc Avg='//TRIM(ADJUSTL(string1))//'s'
  END IF
ELSE
  list(i) = 'MSS off'
END IF

i = i + 1
IF( i > mxlst )GOTO 9999
IF( urban(EDIT_LEVEL_1)%uwm%On )THEN
  on = .TRUE.
  list(i) = 'UWM on'

  IF( urban(EDIT_LEVEL_1)%uwm%Grid /= urban(DEFAULT_LEVEL)%uwm%Grid )THEN
    i = i + 1
    IF( i > mxlst )GOTO 9999
    SELECT CASE( urban(EDIT_LEVEL_1)%uwm%Grid )
      CASE( UWM_COARSE )
        string1 = 'Coarse'
      CASE( UWM_MEDIUM )
        string1 = 'Medium'
      CASE( UWM_FINE )
        string1 = 'Fine'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT
    list(i) = ' Resolution='//TRIM(string1)
  END IF

  IF( urban(EDIT_LEVEL_1)%uwm%Domain == UWM_AUTO )THEN
    show = urban(EDIT_LEVEL_1)%uwm%dsmin /= urban(DEFAULT_LEVEL)%uwm%dsmin
    show = show .OR. urban(EDIT_LEVEL_1)%uwm%dmin /= urban(DEFAULT_LEVEL)%uwm%dmin
    show = show .OR. urban(EDIT_LEVEL_1)%uwm%dmax /= urban(DEFAULT_LEVEL)%uwm%dmax
    IF( show )THEN
      i = i + 1
      IF( i > mxlst )GOTO 9999
      list(i) = ' Domain=Auto'
      i = i + 1
      IF( i > mxlst )GOTO 9999
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%dsmin,string2,irv )
      string1 = TRIM(string2)//' m'
      list(i) = '  Min Res='//TRIM(string1)
      i = i + 1
      IF( i > mxlst )GOTO 9999
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%dmin,string2,irv )
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%dmax,string3,irv )
      string1 = TRIM(ADJUSTL(string2))//'-'//TRIM(ADJUSTL(string3))
      list(i) = '  Range='//TRIM(string1)//' m'
    END IF
  ELSE
    show = urban(EDIT_LEVEL_1)%uwm%xBL /= urban(DEFAULT_LEVEL)%uwm%xBL
    show = show .OR. urban(EDIT_LEVEL_1)%uwm%yBL /= urban(DEFAULT_LEVEL)%uwm%yBL
    show = show .OR. urban(EDIT_LEVEL_1)%uwm%xTR /= urban(DEFAULT_LEVEL)%uwm%xTR
    show = show .OR. urban(EDIT_LEVEL_1)%uwm%yTR /= urban(DEFAULT_LEVEL)%uwm%yTR
    IF( show )THEN
      i = i + 1
      IF( i > mxlst )GOTO 9999
      list(i) = ' Domain=User defined'
      i = i + 1
      IF( i > mxlst )GOTO 9999
      SELECT CASE( urban(EDIT_LEVEL_1)%uwm%coor )
        CASE( UWM_UTM )
          WRITE(string2,*)urban(EDIT_LEVEL_1)%uwm%zone
          string1 = 'UTM zone '//TRIM(ADJUSTL(string2))
        CASE( UWM_LLA )
          string1 = 'Lat/Lon'
        CASE DEFAULT
          string1 = 'Unknown'
      END SELECT
      list(i) = '  Coordinates='//TRIM(string1)
      i = i + 1
      IF( i > mxlst )GOTO 9999
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%xBL,string2,irv )
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%yBL,string3,irv )
      string1 = TRIM(string2)//','//TRIM(string3)
      list(i) = '  BL corner=('//TRIM(string1)//')'
      i = i + 1
      IF( i > mxlst )GOTO 9999
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%xTR,string2,irv )
      CALL set_real_string( urban(EDIT_LEVEL_1)%uwm%yTR,string3,irv )
      string1 = TRIM(string2)//','//TRIM(string3)
      list(i) = '  TR corner=('//TRIM(string1)//')'
    END IF
  END IF
ELSE
  list(i) = 'UWM off'
END IF

IF( on )THEN
  i = i + 1
  IF( i > mxlst )GOTO 9999
  CALL ReportFileName( list(i),'Database=',urban(EDIT_LEVEL_1)%DBName )
  i = i + 1
  IF( i > mxlst )GOTO 9999
  CALL ReportFileName( list(i),'Datapath=',urban(EDIT_LEVEL_1)%DBPath )
END IF
i = i + 1

9999 CONTINUE

n = i - 1

RETURN
END
!*******************************************************************************
!           Convert UWM user domain on switch of coordinates
!*******************************************************************************
SUBROUTINE urban_convert_UWMuser( iwnd_db,id_level,old,new )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE urban_fi
USE datums
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db     !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level
INTEGER,              INTENT( IN ) :: old !Old coordinate selection 1=LLA,2=UTM
INTEGER,              INTENT( IN ) :: new !New coordinate selection

REAL    lon,lat
REAL    east,north
INTEGER zone
INTEGER irv

REAL,DIMENSION(4) :: old_values
INTEGER           :: old_zone

LOGICAL, EXTERNAL :: hasError

IF( old /= new )THEN

!==== Save original values in case of an error converting

  old_zone = dbint(1,id_level)
  DO irv = 1,4
    old_values(irv) = dbreal(irv+4,id_level)
  END DO

!==== Try to convert current values to new coordinate selection

  IF( old == 1 )THEN                                   !LLA to UTM
    lon = dbreal(5,id_level)
    lat = dbreal(7,id_level)
    zone = 0
    irv = LL2UTM( lat,lon,zone,east,north )
    IF( irv /= 0 )THEN
      CALL setUTMerrorGUI( 'LL2UTM',irv )
      GOTO 9999
    END IF
    dbreal(5,id_level) = east
    dbreal(7,id_level) = north
    lon = dbreal(6,id_level)
    lat = dbreal(8,id_level)
    irv = LL2UTM( lat,lon,zone,east,north )
    IF( irv /= 0 )THEN
      CALL setUTMerrorGUI( 'LL2UTM',irv )
      GOTO 9999
    END IF
    dbreal(6,id_level) = east
    dbreal(8,id_level) = north
    dbint(1,id_level) = zone
    CALL SetEditRs( iwnd_db,dbreal(5,id_level),5,4 )
    CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )
  ELSE                                                 !UTM to LLA
    east  = dbreal(5,id_level)
    north = dbreal(7,id_level)
    zone  = dbint(1,id_level)
    irv = UTM2LL( zone,east,north,lat,lon )
    IF( irv /= 0 )THEN
      CALL setUTMerrorGUI( 'LL2UTM',irv )
      GOTO 9999
    END IF
    dbreal(5,id_level) = lon
    dbreal(7,id_level) = lat
    east  = dbreal(6,id_level)
    north = dbreal(8,id_level)
    irv = UTM2LL( zone,east,north,lat,lon )
    IF( irv /= 0 )THEN
      CALL setUTMerrorGUI( 'LL2UTM',irv )
      GOTO 9999
    END IF
    dbreal(6,id_level) = lon
    dbreal(8,id_level) = lat
    CALL SetEditRs( iwnd_db,dbreal(5,id_level),5,4 )
  END IF
END IF

9999 CONTINUE

!==== Check for error

IF( hasError() )THEN

!==== Show error message

  CALL ShowErrorMessage( iwnd_db )

!==== Restore original values

  dbint(1,id_level) = old_zone
  DO irv = 1,4
    dbreal(irv+4,id_level) = old_values(irv)
  END DO
  ichoice(4,id_level) = old
  CALL SetEditRs( iwnd_db,dbreal(5,id_level),5,4 )
  CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )

!==== reset the RadioButtons

  CALL ResetRadioSelection( iwnd_db,id_level,4 )

END IF

RETURN
END
!*******************************************************************************
!               Reset Radio Selection
!*******************************************************************************
SUBROUTINE ResetRadioSelection( iwnd,ilev,indx )

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Box handle
INTEGER,                INTENT( IN ) :: ilev    !Edit level
INTEGER,                INTENT( IN ) :: indx    !Index of RadioButton group

!---- WIN API parameters

INTEGER i,irv,ngrp,igrp,isel
INTEGER(POINTER_LEN)ictrl

!==== Set up parameters

igrp = RADIO_BASE+10*(indx - 1)
ngrp = nradio(indx,ilev)
isel = ichoice(indx,ilev)

!==== Uncheck all but the selected

DO i = 1,ngrp
  IF( i == isel )CYCLE
  ictrl = GetDlgItem( iwnd,igrp+i )
  irv = SendMessage( ictrl,BM_SETCHECK,0,0 )
  irv = SendMessage( ictrl,BM_SETSTATE,FALSE,0 )
END DO

!==== Check the selected

IF( isel > 0 .AND. isel <= ngrp )THEN
  ictrl = GetDlgItem( iwnd,igrp+isel )
  irv = SendMessage( ictrl,BM_SETCHECK,1,0 )
  irv = SendMessage( ictrl,BM_SETSTATE,TRUE,0 )
  CALL SetFocusControl( iwnd,igrp+isel )
END IF

RETURN
END
!***********************************************************************
!               CheckUrbanDefinition
!***********************************************************************
SUBROUTINE check_urban_definition( iwnd_db,id_level,lok )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE urban_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN  ) :: iwnd_db  !Dialog Box handle
INTEGER,                INTENT( IN  ) :: id_level
LOGICAL,                INTENT( OUT ) :: lok

INTEGER i,imin,imax
REAL    rmin,rmax
LOGICAL check, inclusive

CHARACTER(128) eString

CHARACTER(PATH_MAXLENGTH) filename, pathname

LOGICAL,                  EXTERNAL :: hasError, CheckFile, CheckPath, IsRemoteDrive
CHARACTER(PATH_MAXLENGTH),EXTERNAL :: StripExtension

lok = .FALSE.

!==== Check integers

DO i = 1,5
  SELECT CASE( i )
    CASE( 1 )
      check = ichoice(2,id_level)==2 .AND. idbcmbo(6,id_level)==UWM_USER .AND. &
              ichoice(4,id_level)==2                                        !UWM on, domain=user, coor=UTM
    CASE DEFAULT
      check = ichoice(3,id_level)==2                                        !MSS on
  END SELECT
  IF( check )THEN
    SELECT CASE( i )
      CASE( 1 )       !UWM UTM zone
        string1 = 'UWM User defined domain UTM zone'
      CASE( 2 )       !MSS time step
        string1 = 'MSS particle time step'
      CASE( 3 )       !MSS sync factor
        string1 = 'MSS synchronization factor'
      CASE( 4 )       !MSS number of particles
        string1 = 'MSS number of particles per step'
      CASE( 5 )       !MSS conc avg time
        string1 = 'MSS concentration averaging time'
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT

    IF( dbint(i,id_level)==DEF_VAL_I .OR. &
        dbint(i,id_level)==NOT_SET_I .OR. &
        dbint(i,id_level)==DEFERRED_I )THEN
      CALL SetError( IV_ERROR, &
                     'Invalid '//TRIM(string1), &
                     'Value must not be "default",empty or "deferred"', &
                     'Reset '//TRIM(string1), &
                     'CheckUrbanDefinition' )
      GOTO 9999
    END IF

    SELECT CASE( i )
      CASE( 1 )       !UWM UTM zone
        imax = 60
        imin = 1
        string2 = '[1-60]'
      CASE( 2 )       !MSS time step
        imax = 10
        imin = 1
        string2 = '[1-10]'
      CASE( 3 )       !MSS sync factor
        imax = 10
        imin = 1
        string2 = '[1-10]'
      CASE( 4 )       !MSS number of particles
        imax = 10000
        imin = 100
        string2 = '[100-10000]'
      CASE( 5 )       !MSS conc avg time
        imax = 900
        imin = 5
        string2 = '[5-900]'
      CASE DEFAULT
        imax = HUGE(i)
        imin = -HUGE(i)
        string2 = '[Unknown]'
    END SELECT
    IF( dbint(i,id_level) < imin .OR. dbint(i,id_level) > imax )THEN
      CALL SetError( IV_ERROR, &
                     'Invalid '//TRIM(string1), &
                     'Value must be in range '//TRIM(string2), &
                     'Reset '//TRIM(string1), &
                     'CheckUrbanDefinition' )
      GOTO 9999
    END IF
  END IF
END DO

!==== check reals

DO i = 1,9
  SELECT CASE( i )
    CASE( 1 )
      check = ichoice(1,id_level)==2                                        !UDM on
    CASE( 2,3,4 )
      check = ichoice(2,id_level)==2 .AND. idbcmbo(6,id_level)==UWM_AUTO    !UWM on, domain=auto
    CASE( 5,6,7,8 )
      check = ichoice(2,id_level)==2 .AND. idbcmbo(6,id_level)==UWM_USER    !UWM on, domain=user
    CASE( 9 )
      check = ichoice(3,id_level)==2                                        !MSS on
    CASE DEFAULT
      check = .FALSE.
  END SELECT
  IF( check )THEN
    SELECT CASE( i )
      CASE( 1 )
        string1 = 'UDM Max Source Height'
      CASE( 2 )
        string1 = 'UWM auto domain min horizontal resolution'
      CASE( 3 )
        string1 = 'UWM auto domain maximum domain size'
      CASE( 4 )
        string1 = 'UWM auto domain minimum size'
      CASE( 5,7 )
        IF( ichoice(4,id_level)==2 )THEN
          string3 = 'easting'
        ELSE
          string3 = 'longitude'
        END IF
        string1 = 'UWM user domain South/West '//TRIM(string3)
      CASE( 6,8 )
        IF( ichoice(4,id_level)==2 )THEN
          string3 = 'northing'
        ELSE
          string3 = 'latitude'
        END IF
        string1 = 'UWM user domain North/East '//TRIM(string3)
      CASE DEFAULT
        string1 = 'Unknown'
    END SELECT

    IF( dbreal(i,id_level)==DEF_VAL_R .OR. &
        dbreal(i,id_level)==NOT_SET_R .OR. &
        dbreal(i,id_level)==DEFERRED_R )THEN
      CALL SetError( IV_ERROR, &
                     'Invalid '//TRIM(string1), &
                     'Value must not be "default",empty or "deferred"', &
                     'Reset '//TRIM(string1), &
                     'CheckUrbanDefinition' )
      GOTO 9999
    END IF

    inclusive = .FALSE.
    SELECT CASE( i )
      CASE( 1 )
        rmax = HUGE(rmax)
        rmin = 0
        string2 = '> 0'
      CASE( 2 )
        rmax = dbreal(4,id_level)
        rmin = 0
        string2 = '> 0 and < dmin'
      CASE( 3 )
        rmax = HUGE(rmax)
        rmin = 0
        string2 = '> 0'
      CASE( 4 )
        rmax = dbreal(3,id_level)
        rmin = 0
        string2 = '> 0 and < dmax'
      CASE( 5,6 )
        IF( ichoice(4,id_level)==2 )THEN
          rmax = 10000.
          rmin = -10000.
          string2 = 'in range [-10000 - 10000]'
        ELSE
          rmax = 360.
          rmin = -360.
          string2 = 'in range [-360 - 360]'
        END IF
        inclusive = .TRUE.
      CASE( 7,8 )
        IF( ichoice(4,id_level)==2 )THEN
          rmax = 10000.
          rmin = -10000.
          string2 = 'in range [-10000 - 10000]'
        ELSE
          rmax = 90.
          rmin = -90.
          string2 = 'in range [-90 - 90]'
        END IF
        inclusive = .TRUE.
      CASE DEFAULT
        rmax = HUGE(i)
        rmin = -HUGE(i)
        string2 = 'Unknown'
    END SELECT
    IF( inclusive )THEN
      IF( dbreal(i,id_level) <= rmin .OR. dbreal(i,id_level) >= rmax )THEN
        CALL SetError( IV_ERROR, &
                       'Invalid '//TRIM(string1), &
                       'Value must be '//TRIM(string2), &
                       'Reset '//TRIM(string1), &
                       'CheckUrbanDefinition' )
        GOTO 9999
      END IF
    ELSE
      IF( dbreal(i,id_level) < rmin .OR. dbreal(i,id_level) > rmax )THEN
        CALL SetError( IV_ERROR, &
                       'Invalid '//TRIM(string1), &
                       'Value must be '//TRIM(string2), &
                       'Reset '//TRIM(string1), &
                       'CheckUrbanDefinition' )
        GOTO 9999
      END IF
    END IF
  END IF
END DO

!==== check database

filename = dbtext(1,id_level)
IF( IsRemoteDrive(filename) )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( IV_ERROR, &
                'Urban database cannot reside on a remote drive', &
                 eString, &
                'Reset Urban database file specification', &
                'CheckUrbanDefinition' )
  GOTO 9999
END IF
IF( .NOT.CheckFile(filename) )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( NF_ERROR, &
                'Urban database file not Found', &
                 eString, &
                'Reset Urban database file specification', &
                'CheckUrbanDefinition' )
  GOTO 9999
END IF

pathname = StripExtension( filename )
pathname = TRIM(pathname)//'_DatabaseFiles'
IF( .NOT.CheckPath(pathname) )THEN
  CALL ReportFileName( eString,'Path=',pathname )
  CALL SetError( NF_ERROR, &
                'Required Urban database directory not Found', &
                 eString, &
                'Select another Urban database file (.ged)', &
                'CheckUrbanDefinition' )
  GOTO 9999
END IF

!==== Set return

lok = .TRUE.

9999 CONTINUE

IF( hasError() )CALL ShowErrorMessage( iwnd_db )

RETURN
END
!***********************************************************************
!               LoadProjectUrban
!***********************************************************************
SUBROUTINE LoadProjectUrban( iwnd_db,idlg,ID )

USE pcscipuf_fi
USE files_fi
USE urban_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN ) :: idlg
TYPE( projectIDT ),   INTENT( IN ) :: ID

urban(idlg) = urban(DEFAULT_LEVEL)
file_tmp = TRIM(ID%name)

CALL AddPath( file_tmp,TRIM(ID%path) )
CALL LoadUrbanFile( iwnd_db,file_tmp,idlg )

RETURN
END
!***********************************************************************
!               DefaultIntegerUrban
!***********************************************************************
SUBROUTINE DefaultIntegerUrban( id_button,value )

USE urban_fi
USE GUIparam_fd
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: id_button
INTEGER, INTENT( OUT ) :: value

SELECT CASE( id_button )
  CASE( 1 )
    value = urban(DEFAULT_LEVEL)%uwm%zone
  CASE( 2 )
    value = urban(DEFAULT_LEVEL)%mss%PTimeStep
  CASE( 3 )
    value = urban(DEFAULT_LEVEL)%mss%PSyncFac
  CASE( 4 )
    value = urban(DEFAULT_LEVEL)%mss%NumPart
  CASE( 5 )
    value = urban(DEFAULT_LEVEL)%mss%ConcTime
  CASE DEFAULT
    value = DEF_VAL_I
END SELECT

RETURN
END
!***********************************************************************
!               GetIntegerUrbanLimits
!***********************************************************************
SUBROUTINE GetIntegerUrbanLimits( id_button,imin,imax )

USE urban_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: id_button
INTEGER, INTENT( OUT ) :: imin
INTEGER, INTENT( OUT ) :: imax

SELECT CASE( id_button )
  CASE( 1 )       !UWM UTM zone
    imax = 60
    imin = 1
  CASE( 2 )       !MSS time step
    imax = 10
    imin = 1
  CASE( 3 )       !MSS sync factor
    imax = 10
    imin = 1
  CASE( 4 )       !MSS number of particles
    imax = 10000
    imin = 100
  CASE( 5 )       !MSS conc avg time
    imax = 900
    imin = 5
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               DefaultRealUrban
!***********************************************************************
SUBROUTINE DefaultRealUrban( id_button,value )

USE urban_fi
USE GUIparam_fd
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: id_button
REAL,    INTENT( OUT ) :: value

SELECT CASE( id_button )
  CASE( 1 )
    value = urban(DEFAULT_LEVEL)%udm%MaxSrcHgt
  CASE( 2 )
    value = urban(DEFAULT_LEVEL)%uwm%dsmin
  CASE( 3 )
    value = urban(DEFAULT_LEVEL)%uwm%dmax
  CASE( 4 )
    value = urban(DEFAULT_LEVEL)%uwm%dmin
  CASE( 5 )
    value = urban(DEFAULT_LEVEL)%uwm%xBL
  CASE( 6 )
    value = urban(DEFAULT_LEVEL)%uwm%xTR
  CASE( 7 )
    value = urban(DEFAULT_LEVEL)%uwm%yBL
  CASE( 8 )
    value = urban(DEFAULT_LEVEL)%uwm%yTR
  CASE( 9 )
    value = urban(DEFAULT_LEVEL)%mss%ResolHgt
  CASE DEFAULT
    value = DEF_VAL_R
END SELECT

RETURN
END
!***********************************************************************
!               LimitRealUrban
!***********************************************************************
SUBROUTINE LimitRealUrban( id_button,id_level,value )

USE urban_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: id_button
INTEGER, INTENT( IN    ) :: id_level
REAL,    INTENT( INOUT ) :: value

SELECT CASE( id_button )
  CASE( 1 )
    value = MAX(value,0.)
  CASE( 2 )
    value = MAX(value,0.)
  CASE( 3 )
    value = MAX(value,0.)
  CASE( 4 )
    value = MAX(value,0.)
  CASE( 5,6 )                           !xBL,xTR
    IF( ichoice(4,id_level) == 1 )THEN !LLA
      value = MAX(value,-360.)
      value = MIN(value,360.)
    ELSE                               !UTM
      value = MAX(value,-10000.)
      value = MIN(value,10000.)
    END IF
  CASE( 7,8 )                           !yBL,yTR
    IF( ichoice(4,id_level) == 1 )THEN !LLA
      value = MAX(value,-90.)
      value = MIN(value,90.)
    ELSE                               !UTM
      value = MAX(value,-10000.)
      value = MIN(value,10000.)
    END IF
  CASE( 9 )
    value = MAX(value,0.)
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               UrbanCombo
!***********************************************************************
SUBROUTINE urban_combo( iwnd_db,id_button,id_level )

USE urban_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,                INTENT( IN  ) :: id_button
INTEGER,                INTENT( IN  ) :: id_level

SELECT CASE( id_button )
  CASE( 6 )  !UWM Domain
    CALL show_urban_UWM( iwnd_db,id_level,(ichoice(2,id_level)==2) )
  CASE( 10 ) !MSS VCluster
    CALL show_urban_MSS( iwnd_db,id_level,(ichoice(3,id_level)==2) )
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               UrbanRadio
!***********************************************************************
SUBROUTINE urban_radio( iwnd_db,igroup,id_level,old_choice )

USE urban_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN  ) :: igroup
INTEGER,              INTENT( IN  ) :: id_level
INTEGER,              INTENT( IN  ) :: old_choice !For restoration after error

SELECT CASE( igroup )
  CASE( 0 )  !UDM On/Off
    CALL show_urban_UDM( iwnd_db,id_level,(ichoice(1,id_level)==2) )
  CASE( 1 )  !UWM On/Off
    CALL show_urban_UWM( iwnd_db,id_level,(ichoice(2,id_level)==2) )
  CASE( 2 )  !MSS On/Off
    CALL show_urban_MSS( iwnd_db,id_level,(ichoice(3,id_level)==2) )
  CASE( 3 )  !UWM USer defined domain coordinate
    CALL urban_convert_UWMuser(iwnd_db,id_level,old_choice,ichoice(4,id_level))
    CALL show_urban_UWM_user( iwnd_db,id_level,(ichoice(2,id_level)==2),(idbcmbo(6,id_level)==2) )
END SELECT

RETURN
END
!*******************************************************************************
!                IsRemoteDrive
!*******************************************************************************
LOGICAL FUNCTION IsRemoteDrive( file )

USE tooluser_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*) file

INTEGER, PARAMETER :: DRIVE_REMOTE = 4

INTEGER sysDriveType, drive

CHARACTER(PATH_MAXLENGTH) check

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull
LOGICAL,                   EXTERNAL :: hasError

IF( file(2:2) == ':' )THEN
  check = AddNull( file(1:2) )
ELSE
  check = AddNull( TRIM(file) )
END IF

drive = sysDriveType( check )

IsRemoteDrive = (drive == DRIVE_REMOTE)

RETURN
END
