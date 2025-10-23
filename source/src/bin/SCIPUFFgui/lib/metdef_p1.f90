!*******************************************************************************
!            Initialize Met Define Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_metdef( iwnd_db,id_level )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE units_fd
USE pcscipuf_fi
USE dialog_fi
USE randef
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER id_level             !Data level

INTEGER  i,enable(3),irv
CHARACTER(128) string, list(LIST_TYPES_MAX)

CHARACTER(9),DIMENSION(12),PARAMETER :: MONTH = (/'January  ','February ','March    ','April    ', &
                                                  'May      ','June     ','July     ','August   ', &
                                                  'September','October  ','November ','December ' /)

IF( project(EDIT_LEVEL_1)%Edit )THEN
  save_flag(1) = TRUE
ELSE
  save_flag(1) = FALSE
END IF

!==== BASIC Dialog Box

!     Buttons

IF( project(EDIT_LEVEL_1)%Edit )THEN
  enable(1) = TRUE !LOAD
  enable(2) = TRUE !DEFAULT
  enable(3) = TRUE !OK
ELSE
  enable(1) = FALSE !LOAD
  enable(2) = FALSE !DEFAULT
  enable(3) = FALSE !OK
END IF
CALL EnableButtons( iwnd_db,enable,13,3 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON13,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON14,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON15,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK ')
  string = ' '
  CALL ReportFileName( string(1:120),'Project ',project(EDIT_LEVEL_1)%ID%name )
  string = TRIM(string)//' WEATHER'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string )
END IF

!     Combo Boxes

list(MET_MEDOC  ) = 'MEDOC gridded data'
list(MET_MEDLIS ) = 'List of MEDOC files'
list(MET_WRF    ) = 'WRF gridded data'
list(MET_ASSIM  ) = 'List of grid/obs met files'
list(MET_MRF    ) = 'SCIP gridded data'
list(MET_OBS    ) = 'Observations'
list(MET_SRF    ) = 'Surface Observations Only'
list(MET_UAIR   ) = 'Upper Air Observations Only'
list(MET_FIXED  ) = 'Fixed Winds'
CALL ClearList( iwnd_db,IDB_COMBO1 )
met_types  = MET_TYPES_ADVANCED
met_offset = MET_OFFSET_ADVANCED
DO i = 1,met_types
  CALL AddList( iwnd_db,IDB_COMBO1,-1,list(i+met_offset),irv )
END DO

list(BL_CALC  )  = 'Calculated'
list(BL_NONE  )  = 'None'
list(BL_OBS   )  = 'Observations'
list(BL_SIMPLE)  = 'Simple Diurnal'
list(BL_PROFILE) = 'Profile'
list(BL_OPERATIONAL) = 'Operational'
list(BL_MEDOC)   = 'Gridded data'
CALL ClearList( iwnd_db,IDB_COMBO2 )
bl_types  = BL_TYPES_ADVANCED
bl_offset = BL_OFFSET_ADVANCED
DO i = 1,bl_types
  CALL AddList( iwnd_db,IDB_COMBO2,-1,list(i+bl_offset),irv )
END DO

list(LSV_INPUT) = 'Input'
list(LSV_MODEL) = 'Model'
list(LSV_NONE ) = 'None'
list(LSV_OBS  ) = 'Observations'
list(LSV_OPERATIONAL) = 'Operational'
CALL ClearList( iwnd_db,IDB_COMBO3 )
lsv_types  = LSV_TYPES_ADVANCED
lsv_offset = LSV_OFFSET_ADVANCED
DO i = 1,lsv_types
  CALL AddList( iwnd_db,IDB_COMBO3,-1,list(i+lsv_offset),irv )
END DO

list(MST_DRY   ) = 'Dry'
list(MST_NORMAL) = 'Normal'
list(MST_WET   ) = 'Wet'
CALL ClearList( iwnd_db,IDB_COMBO5 )
DO i = 1,MST_TYPES
  CALL AddList( iwnd_db,IDB_COMBO5,-1,list(i),irv )
END DO

list(PC_CLEAR        ) = 'No precipitation'
list(PC_LIGHT_RAIN   ) = 'Light rain'
list(PC_MODERATE_RAIN) = 'Moderate rain'
list(PC_HEAVY_RAIN   ) = 'Heavy rain'
list(PC_LIGHT_SNOW   ) = 'Light snow'
list(PC_MODERATE_SNOW) = 'Moderate snow'
list(PC_HEAVY_SNOW   ) = 'Heavy snow'
list(PC_MET          ) = 'Read weather file(s)'
CALL ClearList( iwnd_db,IDB_COMBO9 )
DO i = 1,PC_TYPES
  CALL AddList( iwnd_db,IDB_COMBO9,-1,list(i),irv )
END DO

list(UNIT_METERS_SEC) = 'm/s'
list(UNIT_KNOTS)      = 'kts'
list(UNIT_MILES_HOUR) = 'mph'
list(UNIT_KM_HOUR)    = 'kph'
list(UNIT_FEET_SEC)   = 'ft/s'
CALL ClearList( iwnd_db,IDB_COMBO7 )
DO i = 1,UNIT_SPEED_TYPES
  CALL AddList( iwnd_db,IDB_COMBO7,-1,list(i),irv )
END DO

list(UNIT_DEGREES) = 'deg'
CALL ClearList( iwnd_db,IDB_COMBO8 )
DO i = 1,UNIT_DIRECTION_TYPES
  CALL AddList( iwnd_db,IDB_COMBO8,-1,list(i),irv )
END DO

CALL ClearList( iwnd_db,IDB_COMBO10 )
DO i = 1,12
  CALL AddList( iwnd_db,IDB_COMBO10,-1,MONTH(i),irv )
END DO

list(1) = 'Day 01'
list(2) = 'Day 02'
CALL ClearList( iwnd_db,IDB_COMBO11 )
DO i = 1,2
  CALL AddList( iwnd_db,IDB_COMBO11,-1,list(i),irv )
END DO

!==== Met definitions

CALL SetControlFont( iwnd_db,IDB_LIST1,fixfont )
CALL SetControlFont( iwnd_db,IDB_LIST2,fixfont )
CALL init_dialog_meteorology( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Initialize Met Define Dialog Box
!*******************************************************************************
SUBROUTINE default_dialog_metdef( iwnd_db,id_level )

USE pcscipuf_fi
USE dialog_fi
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER id_level             !Data level

!==== Met definitions

CALL save_dialog_metdef( iwnd_db,id_level )

dbtext(1,id_level) = metdef(EDIT_LEVEL_2)%profile
dbtext(2,id_level) = metdef(EDIT_LEVEL_2)%surfile
dbtext(3,id_level) = metdef(EDIT_LEVEL_2)%medfile
dbtext(4,id_level) = metdef(EDIT_LEVEL_2)%terfile
dbtext(5,id_level) = metdef(EDIT_LEVEL_2)%asmfile
ichoice(1,id_level) = metdef(EDIT_LEVEL_2)%met

metdef(EDIT_LEVEL_2) = metdef(DEFAULT_LEVEL)

metdef(EDIT_LEVEL_2)%profile = dbtext(1,id_level)
metdef(EDIT_LEVEL_2)%surfile = dbtext(2,id_level)
metdef(EDIT_LEVEL_2)%medfile = dbtext(3,id_level)
metdef(EDIT_LEVEL_2)%terfile = dbtext(4,id_level)
metdef(EDIT_LEVEL_2)%asmfile = dbtext(5,id_level)
metdef(EDIT_LEVEL_2)%met = ichoice(1,id_level)

CALL init_dialog_metdef( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Initialize Met edit Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_meteorology( iwnd_db,id_level )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE dialog_fi
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER id_level             !Data level

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

INTEGER indx,irv,i,isel,jsel,indxd,ichck
REAL    dfac
LOGICAL lOpWxOK

LOGICAL,EXTERNAL :: hasError

CALL clear_met_edit( iwnd_db,id_level )

nradio(3,id_level) = 3 !Operational Type
nradio(5,id_level) = 2 !Fcst Grid/Post
nradio(8,id_level) = 2 !3D Clim Auto/User
nradio(9,id_level) = 2 !Mass consistent output format

lOpWxOK = .TRUE.

100 CONTINUE

ichoice(3,id_level) = 0
ichoice(5,id_level) = 0
ichoice(8,id_level) = 0
lcheck(4,id_level) = .FALSE.
lcheck(5,id_level) = .FALSE.

ichoice(8,id_level) = 1

nradio(2,id_level) = 2
IF( metdef(EDIT_LEVEL_2)%local )THEN
  ichoice(2,id_level) = 2
ELSE
  ichoice(2,id_level) = 1
END IF
nradio(1,id_level) = 2
IF( metdef(EDIT_LEVEL_2)%lcanopy )THEN
  ichoice(1,id_level) = 2
ELSE
  ichoice(1,id_level) = 1
END IF

lcheck(2, id_level) = metdef(EDIT_LEVEL_2)%lmc !terrain
lcheck(3, id_level) = metdef(EDIT_LEVEL_2)%lmcout !save terrain
lcheck(6, id_level) = metdef(EDIT_LEVEL_2)%l2Dsave !save terrain
lcheck(7, id_level) = metdef(EDIT_LEVEL_2)%l3Dsave !save terrain
lcheck(8, id_level) = metdef(EDIT_LEVEL_2)%luseter
lcheck(9, id_level) = metdef(EDIT_LEVEL_2)%luselc
lcheck(10,id_level) = metdef(EDIT_LEVEL_2)%lavailter
lcheck(11,id_level) = metdef(EDIT_LEVEL_2)%lavaillc
lcheck(12,id_level) = metdef(EDIT_LEVEL_2)%llccategory
IF( metdef(EDIT_LEVEL_2)%lmcformat )THEN
  ichoice(9,id_level) = 2
ELSE
  ichoice(9,id_level) = 1
END IF
CALL SplitName( metdef(EDIT_LEVEL_2)%medfile,string1,string2 )
dbtext(5,id_level) = AddNull(string1) !MEDOC       file
dbtext(6,id_level) = AddNull(string2) !MEDOC       directory

CALL SplitName( metdef(EDIT_LEVEL_2)%profile,string1,string2 )
dbtext(1,id_level) = AddNull(string1) !Profile     file
dbtext(2,id_level) = AddNull(string2) !Profile     directory

CALL SplitName( metdef(EDIT_LEVEL_2)%surfile,string1,string2 )
dbtext(3,id_level) = AddNull(string1) !Surface Obs file
dbtext(4,id_level) = AddNull(string2) !Surface Obs directory

CALL SplitName( metdef(EDIT_LEVEL_2)%terfile,string1,string2 )
dbtext(7,id_level) = AddNull(string1) !Terrain file
dbtext(8,id_level) = AddNull(string2) !Terrain directory

CALL SplitName( metdef(EDIT_LEVEL_2)%asmfile,string1,string2 )
dbtext( 9,id_level) = AddNull(string1) !ASSIM       file
dbtext(10,id_level) = AddNull(string2) !ASSIM       directory

DO i = 1,21
  dbreal(i,id_level) = metdef(EDIT_LEVEL_2)%dbreal(i)
END DO
dbreal(23,id_level) = metdef(EDIT_LEVEL_2)%dbreal(23)
IF( dbreal(14,id_level) == NOT_SET_R .OR. dbreal(14,id_level) == DEF_VAL_R )THEN
  dfac = 1.
  indxd = 1
ELSE IF( dbreal(14,id_level) == 0. .OR. dbreal(14,id_level) < 60. )THEN
  dfac = 1.
  indxd = 1
ELSE IF( dbreal(14,id_level) <  3600. )THEN
  dfac = 60.
  indxd = 2
ELSE
  dfac = 3600.
  indxd = 3
END IF
idbcmbo(12,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO12,id_level )
dbreal(14,id_level) = dbreal(14,id_level)/dfac

dbint(1,id_level)  = MAX(1,metdef(EDIT_LEVEL_2)%nobs)
dbint(2,id_level)  = MAX(1,metdef(EDIT_LEVEL_2)%nsfc)
dbint(3,id_level)  = metdef(EDIT_LEVEL_2)%nfft
dbint(4,id_level)  = metdef(EDIT_LEVEL_2)%nprm
dbint(5,id_level)  = metdef(EDIT_LEVEL_2)%nz

lcheck(15,id_level) = .TRUE.

IF( dbint(5,id_level) > 0 )THEN
  nlst(id_level) = dbint(5,id_level)
  DO i = 1,nlst(id_level)
!lst          dblst(i,id_level) = metdef(EDIT_LEVEL_2).z(i)
    dblst(id_level,i) = metdef(EDIT_LEVEL_2)%z(i)
  END DO
ELSE
  nlst(id_level) = 0
END IF

dbcmbo(1,id_level) = ' '
idbcmbo(1,id_level) = NOT_SET_I
dbcmbo(3,id_level) = ' '
idbcmbo(3,id_level) = NOT_SET_I

isel = metdef(EDIT_LEVEL_2)%met
jsel = metdef(EDIT_LEVEL_2)%lsv

1000 CONTINUE

IF( isel == 0 )isel = MET_OBS

isel = isel - met_offset

IF( isel > 0 )THEN
  indx = isel - 1
  irv = -999
  CALL SetListSel( iwnd_db,IDB_COMBO1,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO1,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO1,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(1,id_level) = indx + 1
      dbcmbo(1,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

dbcmbo(3,id_level) = ' '
idbcmbo(3,id_level) = NOT_SET_I
jsel = jsel - lsv_offset
IF( jsel > 0 )THEN
  indx = jsel - 1
  CALL SetListSel( iwnd_db,IDB_COMBO3,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO3,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO3,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(3,id_level) = indx + 1
      dbcmbo(3,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

dbcmbo(2,id_level) = ' '
idbcmbo(2,id_level) = NOT_SET_I
IF( metdef(EDIT_LEVEL_2)%bl > 0 )THEN
  indx = metdef(EDIT_LEVEL_2)%bl - bl_offset - 1
  CALL SetListSel( iwnd_db,IDB_COMBO2,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO2,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO2,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(2,id_level) = indx + 1
      dbcmbo(2,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

IF( isel <= 0 )THEN
  CALL EnableControl( iwnd_db,IDB_COMBO2,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO3,FALSE )
ELSE
  CALL EnableControl( iwnd_db,IDB_COMBO2,TRUE )
  CALL EnableControl( iwnd_db,IDB_COMBO3,TRUE )
END IF

dbcmbo(7,id_level) = ' '
idbcmbo(7,id_level) = NOT_SET_I
IF( metdef(EDIT_LEVEL_2)%unit_spd > 0 )THEN
  indx = metdef(EDIT_LEVEL_2)%unit_spd - 1
  CALL SetListSel( iwnd_db,IDB_COMBO7,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO7,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO7,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(7,id_level) = indx + 1
      dbcmbo(7,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

dbcmbo(8,id_level) = ' '
idbcmbo(8,id_level) = NOT_SET_I
IF( metdef(EDIT_LEVEL_2)%unit_dir > 0 )THEN
  indx = metdef(EDIT_LEVEL_2)%unit_dir - 1
  CALL SetListSel( iwnd_db,IDB_COMBO8,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO8,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO8,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(8,id_level) = indx + 1
      dbcmbo(8,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

dbcmbo(9,id_level) = ' '
idbcmbo(9,id_level) = NOT_SET_I
IF( metdef(EDIT_LEVEL_2)%precip > 0 )THEN
  indx = metdef(EDIT_LEVEL_2)%precip - 1
  CALL SetListSel( iwnd_db,IDB_COMBO9,indx,irv )
  CALL GetListSel( iwnd_db,IDB_COMBO9,1,indx,irv )
  IF( irv > 0 )THEN
    CALL GetListItem( iwnd_db,IDB_COMBO9,indx,string2,irv )
    IF( irv > 0 )THEN
      idbcmbo(9,id_level) = indx + 1
      dbcmbo(9,id_level)  = AddNull( TRIM(string2(1:irv)) )
    END IF
  END IF
END IF

dbcmbo(10,id_level) = ' '
idbcmbo(10,id_level) = NOT_SET_I
dbcmbo(11,id_level) = ' '
idbcmbo(11,id_level) = NOT_SET_I
indx = metdef(EDIT_LEVEL_2)%wetness - 1
CALL SetListSel( iwnd_db,IDB_COMBO5,indx,irv )
CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,IDB_COMBO5,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(5,id_level) = indx + 1
    dbcmbo(5,id_level)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF

CALL update_met_edit( iwnd_db,id_level,0 )

CALL load_met_edit( iwnd_db,id_level )

irv = TRUE
ichck = 3
DO i = 1,ichck
  IF( idbcmbo(i,id_level) <= 0 )irv = FALSE
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON13,irv )

RETURN
END
!*******************************************************************************
!            save MET define Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_metdef( iwnd_db,id_level )

USE resource_fd
USE mettype_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER id_level             !Data level

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

INTEGER i
LOGICAL lAssim
!
!     Combo Boxes
!

metdef(EDIT_LEVEL_2)%met = idbcmbo(1,id_level) + met_offset
metdef(EDIT_LEVEL_2)%bl  = idbcmbo(2,id_level) + bl_offset
metdef(EDIT_LEVEL_2)%lsv = idbcmbo(3,id_level) + lsv_offset

metdef(EDIT_LEVEL_2)%precip = idbcmbo(9,id_level)

lAssim = metdef(EDIT_LEVEL_2)%met == MET_ASSIM

!
!     Radio Buttons
!
metdef(EDIT_LEVEL_2)%local = ichoice(2,id_level) == 2
metdef(EDIT_LEVEL_2)%lcanopy = ichoice(1,id_level) == 2
IF( metdef(EDIT_LEVEL_2)%lcanopy )THEN
  metdef(EDIT_LEVEL_2)%canopy = dbreal(2,id_level)
  metdef(EDIT_LEVEL_2)%canopyParam = dbreal(23,id_level)
ELSE
  metdef(EDIT_LEVEL_2)%rough = dbreal(1,id_level)
END IF
!
!     Edit Boxes - Text
!
metdef(EDIT_LEVEL_2)%tbin = dbreal(14,id_level)
SELECT CASE( idbcmbo(12,id_level) )
  CASE( 2 )
    metdef(EDIT_LEVEL_2)%tbin = metdef(EDIT_LEVEL_2)%tbin*60.
  CASE( 3 )
    metdef(EDIT_LEVEL_2)%tbin = metdef(EDIT_LEVEL_2)%tbin*3600.
  CASE DEFAULT
END SELECT

metdef(EDIT_LEVEL_2)%lmc = .FALSE.
SELECT CASE( metdef(EDIT_LEVEL_2)%met )
  CASE( MET_MEDOC )
    string1 = StripNull( dbtext(5,id_level) )
    string2 = StripNull( dbtext(6,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%medfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level) !******************
    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin
  CASE (MET_WRF)
    string1 = StripNull( dbtext(5,id_level) )
    string2 = StripNull( dbtext(6,id_level) )
    CALL AddPath(string1,string2)
    metdef(EDIT_LEVEL_2)%medfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level) !******************
    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin
  CASE( MET_ASSIM )
    string1 = StripNull( dbtext( 9,id_level) )
    string2 = StripNull( dbtext(10,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%asmfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level) !******************
!    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin
  CASE (MET_MEDLIS)
    string1 = StripNull( dbtext(5,id_level) )
    string2 = StripNull( dbtext(6,id_level) )
    CALL AddPath(string1,string2)
    metdef(EDIT_LEVEL_2)%medfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level) !******************
    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin
  CASE( MET_MRF )
    string1 = StripNull( dbtext(5,id_level) )
    string2 = StripNull( dbtext(6,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%medfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level)
    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin
  CASE( MET_OBS )
    string1 = StripNull( dbtext(1,id_level) )
    string2 = StripNull( dbtext(2,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%profile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%nobs = dbint(1,id_level)
    string1 = StripNull( dbtext(3,id_level) )
    string2 = StripNull( dbtext(4,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%surfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%nsfc = dbint(2,id_level)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level)
  CASE( MET_SRF )
    string1 = StripNull( dbtext(3,id_level) )
    string2 = StripNull( dbtext(4,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%surfile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%nsfc = dbint(2,id_level)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level)
  CASE( MET_UAIR )
    string1 = StripNull( dbtext(1,id_level) )
    string2 = StripNull( dbtext(2,id_level) )
    CALL AddPath( string1,string2 )
    metdef(EDIT_LEVEL_2)%profile = TRIM(string1)
    metdef(EDIT_LEVEL_2)%nobs = dbint(1,id_level)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level)
  CASE( MET_FIXED )
    metdef(EDIT_LEVEL_2)%speed = dbreal(12,id_level)
    metdef(EDIT_LEVEL_2)%direction = dbreal(13,id_level)
    metdef(EDIT_LEVEL_2)%lmc = lcheck(2,id_level)
    metdef(EDIT_LEVEL_2)%unit_spd = idbcmbo(7,id_level)
    metdef(EDIT_LEVEL_2)%unit_dir = idbcmbo(8,id_level)
    metdef(EDIT_LEVEL_2)%tbin = metdef(DEFAULT_LEVEL)%tbin

  CASE DEFAULT
END SELECT

SELECT CASE( metdef(EDIT_LEVEL_2)%lsv )
  CASE( LSV_INPUT )
    metdef(EDIT_LEVEL_2)%slb = dbreal(10,id_level)
    metdef(EDIT_LEVEL_2)%uub = dbreal(11,id_level)
  CASE( LSV_OBS )
    metdef(EDIT_LEVEL_2)%slb = dbreal(10,id_level)
  CASE DEFAULT
END SELECT

SELECT CASE( metdef(EDIT_LEVEL_2)%bl )
  CASE( BL_CALC,BL_OPERATIONAL )
    metdef(EDIT_LEVEL_2)%bowen  = dbreal(7,id_level)
    metdef(EDIT_LEVEL_2)%albedo = dbreal(8,id_level)
    metdef(EDIT_LEVEL_2)%cloud  = dbreal(9,id_level)
    metdef(EDIT_LEVEL_2)%wetness = idbcmbo(5,id_level)
  CASE( BL_SIMPLE )
    metdef(EDIT_LEVEL_2)%zimin  = dbreal(3,id_level)
    metdef(EDIT_LEVEL_2)%zimax  = dbreal(4,id_level)
    metdef(EDIT_LEVEL_2)%hconst = dbreal(5,id_level)
    metdef(EDIT_LEVEL_2)%hdiur  = dbreal(6,id_level)
  CASE DEFAULT
END SELECT

metdef(EDIT_LEVEL_2)%slhazard = dbreal(15,id_level)
IF( metdef(EDIT_LEVEL_2)%lmc .OR. lAssim )THEN
  IF( .NOT. lAssim )THEN
  metdef(EDIT_LEVEL_2)%luseter = lcheck(8,id_level)
  metdef(EDIT_LEVEL_2)%luselc  = lcheck(9,id_level)
  metdef(EDIT_LEVEL_2)%lavailter = lcheck(10,id_level)
  metdef(EDIT_LEVEL_2)%lavaillc = lcheck(11,id_level)
  metdef(EDIT_LEVEL_2)%llccategory = lcheck(12,id_level)
  string1 = StripNull( dbtext(7,id_level) )
  string2 = StripNull( dbtext(8,id_level) )
  CALL AddPath( string1,string2 )
  metdef(EDIT_LEVEL_2)%terfile = TRIM(string1)
  END IF
  metdef(EDIT_LEVEL_2)%nfft = dbint(3,id_level)
  metdef(EDIT_LEVEL_2)%nprm = dbint(4,id_level)
  metdef(EDIT_LEVEL_2)%nz   = nlst(id_level)
  metdef(EDIT_LEVEL_2)%alpmin = dbreal(16,id_level)
  metdef(EDIT_LEVEL_2)%alpmax = dbreal(17,id_level)
  metdef(EDIT_LEVEL_2)%epsfft = dbreal(18,id_level)
  metdef(EDIT_LEVEL_2)%epsprm = dbreal(19,id_level)
  DO i = 1,metdef(EDIT_LEVEL_2)%nz
!lst          metdef(EDIT_LEVEL_2).z(i) = dblst(i,id_level)
    metdef(EDIT_LEVEL_2)%z(i) = dblst(id_level,i)
  END DO
END IF

metdef(EDIT_LEVEL_2)%lmcout = lcheck(3,id_level)
IF( metdef(EDIT_LEVEL_2)%lmcout )THEN
  metdef(EDIT_LEVEL_2)%lmcformat = ichoice(9,id_level) == 2
  metdef(EDIT_LEVEL_2)%l2Dsave = lcheck(6,id_level)
  metdef(EDIT_LEVEL_2)%l3Dsave = lcheck(7,id_level)
  metdef(EDIT_LEVEL_2)%tout    = dbreal(21,id_level)
END IF
!
!     List Boxes
!

RETURN
END
!*******************************************************************************
!                     Clear Met Description
!*******************************************************************************
SUBROUTINE clear_met_edit( iwnd_db,ilev )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi

!     This routine clears the met description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              ilev !Data level ID

INTEGER i

met_flags = 0

dbtext(1,ilev) = ' '
dbtext(2,ilev) = ' '
dbtext(3,ilev) = ' '
dbtext(4,ilev) = ' '
dbtext(5,ilev) = ' '
dbtext(6,ilev) = ' '
dbtext(7,ilev) = ' '
dbtext(8,ilev) = ' '

DO i = 1,20
  dbreal(i,ilev) = NOT_SET_R
END DO
dbreal(23,ilev) = NOT_SET_R

dbint(1,ilev) = NOT_SET_I
dbint(2,ilev) = NOT_SET_I
dbint(3,ilev) = NOT_SET_I
dbint(4,ilev) = NOT_SET_I

ichoice(1,ilev) = 1
ichoice(2,ilev) = 1
ichoice(8,ilev) = 1

DO i = 4,9
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO

DO i = 11,11
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO

DO i = 16,19
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO

DO i = 1,15
  CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  CALL ShowControl(  iwnd_db,REAL_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,REAL_BASE+23,FALSE )
CALL ShowControl  ( iwnd_db,REAL_BASE+23,SW_HIDE )

DO i = 1,2
  CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,INT_BASE+i,SW_HIDE )
END DO

DO i = 1,2
  CALL EnableControl( iwnd_db,LIST_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,LIST_BASE+i,SW_HIDE )
END DO

DO i = 4,12
  CALL EnableControl( iwnd_db,COMBO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,COMBO_BASE+i,SW_HIDE )
END DO

DO i = 1,2
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 11,12
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 21,23
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 41,42
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 71,72
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

CALL EnableControl( iwnd_db,IDB_CHECK4,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK4,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK5,SW_HIDE )

DO i = 10,14
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 20,25
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 26,28
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 30,35
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 39,49
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 50,57
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 60,64
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 65,68
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 70,74
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 80,84
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 85,89
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 90,90
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 95,99
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO

RETURN
END
!*******************************************************************************
!                     Load Met Description
!*******************************************************************************
SUBROUTINE load_met_edit( iwnd_db,id_level )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE dialog_fi

!     This routine loads the met description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Data level ID

CHARACTER(128)       StripNull

INTEGER i

CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,3 )
CALL SetRadios( iwnd_db,ichoice(5,id_level),nradio(5,id_level),5,1 )
CALL SetRadios( iwnd_db,ichoice(8,id_level),nradio(8,id_level),8,1 )

CALL SetChecks( iwnd_db,lcheck(2,id_level),2,5 )

string1 = StripNull( dbtext(1,id_level) )
string2 = StripNull( dbtext(2,id_level) )
CALL SetControlText( iwnd_db,IDB_STATIC23,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC24,string2 )

string1 = StripNull( dbtext(3,id_level) )
string2 = StripNull( dbtext(4,id_level) )
CALL SetControlText( iwnd_db,IDB_STATIC33,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC34,string2 )

string1 = StripNull( dbtext(5,id_level) )
string2 = StripNull( dbtext(6,id_level) )
CALL SetControlText( iwnd_db,IDB_STATIC63,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC64,string2 )

string1 = StripNull( dbtext( 9,id_level) )
string2 = StripNull( dbtext(10,id_level) )
CALL SetControlText( iwnd_db,IDB_STATIC98,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC99,string2 )

CALL SetEditRs(iwnd_db,dbreal( 1,id_level),1,15 )
CALL SetEditRs(iwnd_db,dbreal( 23,id_level),23,1 )
CALL SetEditIs(iwnd_db,dbint( 1,id_level),1,2 )

DO i = 16,19
  string3 = TRIM(StripNull(dbtext(i,id_level)))
  CALL SetFileString( 38,string3,string2 )
  string1 = '(file:'//TRIM(string2)//')'
  CALL SetControlText( iwnd_db,STATIC_BASE+70+i,string1 )
END DO

IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  DO i = 1,12
    CALL EnableControl( iwnd_db,COMBO_BASE+i,FALSE )
  END DO
  DO i = 1,nradio(1,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 11,10+nradio(2,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 21,20+nradio(3,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 41,40+nradio(5,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 71,70+nradio(8,id_level)
    CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  END DO
  DO i = 1,15
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  END DO
  CALL EnableControl( iwnd_db,REAL_BASE+23,FALSE )
  DO i = 1,2
    CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  END DO
  DO i = 4,9
    CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  END DO
  CALL EnableControl( iwnd_db,BUTTON_BASE+11,FALSE )
  DO i = 16,19
    CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  END DO
  CALL EnableControl( iwnd_db,BUTTON_BASE+7,TRUE )
  CALL EnableControl( iwnd_db,IDB_CHECK4,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
END IF

RETURN
END
!*******************************************************************************
!                     Update Met Description
!*******************************************************************************
SUBROUTINE update_met_edit( iwnd_db,ilev,iflag )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE dialog_fi

!     This routine updates the met description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              ilev !Data level ID
INTEGER              iflag !Data level ID

INTEGER jflag

IF( idbcmbo(1,ilev) <= 0)RETURN

CALL show_met_basic( iwnd_db,ilev )

SELECT CASE( idbcmbo(1,ilev)+met_offset )
  CASE( MET_MEDOC )
    CALL hide_assim( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL ShowControl( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL hide_tbin( iwnd_db )
    CALL show_gridded( iwnd_db )
    CALL show_time( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )

  CASE( MET_WRF )
    CALL hide_assim( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL ShowControl( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL hide_tbin( iwnd_db )
    CALL show_gridded( iwnd_db )
    CALL show_time( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )

  CASE( MET_ASSIM )
    CALL hide_gridded( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL ShowControl( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL show_tbin( iwnd_db )
    CALL show_assim( iwnd_db )
    CALL show_time( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )

  CASE( MET_MEDLIS )
    CALL hide_assim( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL ShowControl( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL hide_tbin( iwnd_db )
    CALL show_gridded( iwnd_db )
    CALL show_time( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )

  CASE( MET_MRF )
    CALL hide_assim( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )
    CALL hide_tbin( iwnd_db )
    CALL show_gridded( iwnd_db )
    CALL show_time( iwnd_db )

  CASE( MET_OBS )
    CALL hide_gridded( iwnd_db )
    CALL hide_assim( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL show_tbin( iwnd_db )
    CALL show_profile( iwnd_db )
    CALL show_surface( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )
    CALL show_time( iwnd_db )

  CASE( MET_SRF )
    CALL hide_profile( iwnd_db )
    CALL hide_gridded( iwnd_db )
    CALL hide_assim( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL show_tbin( iwnd_db )
    CALL show_surface( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )
    CALL show_time( iwnd_db )

  CASE( MET_UAIR )
    CALL hide_gridded( iwnd_db )
    CALL hide_assim( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_fixwnds( iwnd_db )
    CALL show_tbin( iwnd_db )
    CALL show_profile( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )
    CALL show_time( iwnd_db )

  CASE( MET_FIXED )
    CALL hide_gridded( iwnd_db )
    CALL hide_surface( iwnd_db )
    CALL hide_profile( iwnd_db )
    CALL hide_time( iwnd_db )
    CALL hide_assim( iwnd_db )
    CALL hide_tbin( iwnd_db )
    CALL show_fixwnds( iwnd_db )
    CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON7,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_BUTTON7,SW_SHOWNORMAL )

  CASE DEFAULT

END SELECT

IF( idbcmbo(2,ilev) <= 0 )GOTO 1000

IF( lcheck(2,ilev) .AND. lcheck(9,ilev) .AND. lcheck(11,ilev) )THEN
  IF( lcheck(12,ilev) )THEN
    jflag = 1
  ELSE
    jflag = 2
  END IF
ELSE
  jflag = 0
END IF

SELECT CASE( idbcmbo(2,ilev)+bl_offset )
  CASE( BL_CALC )
    CALL show_BL_calc( iwnd_db,0,jflag )

  CASE( BL_OPERATIONAL )
    CALL show_BL_calc( iwnd_db,1,jflag )

  CASE( BL_NONE )
    CALL hide_BL( iwnd_db )

  CASE( BL_OBS,BL_MEDOC )
    CALL show_BL_obs( iwnd_db )

  CASE( BL_SIMPLE )
    CALL show_BL_std( iwnd_db )

  CASE( BL_PROFILE )
    CALL show_BL_pro( iwnd_db )

  CASE DEFAULT

END SELECT

1000  CONTINUE

IF( idbcmbo(3,ilev) <= 0 )RETURN

SELECT CASE( idbcmbo(3,ilev)+lsv_offset )
  CASE( LSV_INPUT )
    CALL show_ens_inp( iwnd_db )

  CASE( LSV_MODEL,LSV_OPERATIONAL )
    CALL show_ens_MOD( iwnd_db )

  CASE( LSV_NONE )
    CALL hide_ens( iwnd_db )

  CASE( LSV_OBS )
    CALL show_ens_obs( iwnd_db )

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!                     Show Basic Met Description
!*******************************************************************************
SUBROUTINE show_met_basic( iwnd_db,ilev )

USE resource_fd
USE winAPI_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the met description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              ilev !Data level ID

INTEGER i

IF( BTEST(met_flags,MF_BASIC) )RETURN

met_flags = IBSET(met_flags,MF_BASIC)

DO i = 10,14,2
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO

DO i = 1,2
  CALL EnableControl( iwnd_db,RADIO_BASE+i,TRUE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_SHOWNORMAL )
END DO

CALL show_met_srf( iwnd_db,ichoice(1,ilev) )

CALL EnableControl( iwnd_db,IDB_COMBO9,TRUE )
CALL ShowControl  ( iwnd_db,IDB_COMBO9,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Show Basic Met Description
!*******************************************************************************
SUBROUTINE show_met_srf( iwnd_db,iopt )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the met description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              iopt !Option ID

IF( iopt == 1 )THEN
  IF( BTEST(met_flags,MF_CANOPY) )THEN
    met_flags = IBCLR(met_flags,MF_CANOPY)
    CALL EnableControl( iwnd_db,IDB_REAL2,FALSE )
    CALL ShowControl  ( iwnd_db,IDB_REAL2,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_REAL23,FALSE )
    CALL ShowControl  ( iwnd_db,IDB_REAL23,SW_HIDE )
  END IF
  IF( .NOT.BTEST(met_flags,MF_ROUGH) )THEN
    met_flags = IBSET(met_flags,MF_ROUGH)
    CALL EnableControl( iwnd_db,IDB_REAL1,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
  END IF
ELSE
  IF( BTEST(met_flags,MF_ROUGH) )THEN
    met_flags = IBCLR(met_flags,MF_ROUGH)
    CALL EnableControl( iwnd_db,IDB_REAL1,FALSE )
    CALL ShowControl  ( iwnd_db,IDB_REAL1,SW_HIDE )
  END IF
  IF( .NOT.BTEST(met_flags,MF_CANOPY) )THEN
    met_flags = IBSET(met_flags,MF_CANOPY)
    CALL EnableControl( iwnd_db,IDB_REAL2,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_REAL2,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_REAL23,TRUE )
    CALL ShowControl  ( iwnd_db,IDB_REAL23,SW_SHOWNORMAL )
  END IF
END IF

RETURN
END
!*******************************************************************************
!                     Show Ensemble Description
!*******************************************************************************
SUBROUTINE show_ens_inp( iwnd_db )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the ensemble description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( BTEST(met_flags,MF_ENSNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC54,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSNOPARM)
END IF

IF( .NOT.BTEST(met_flags,MF_ENSBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC50,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSBASIC)
END IF

IF( .NOT.BTEST(met_flags,MF_ENSSCALE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC51,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSSCALE)
  CALL EnableControl( iwnd_db,IDB_REAL10,TRUE )
  CALL ShowControl  ( iwnd_db,IDB_REAL10,SW_SHOWNORMAL )
END IF

IF( .NOT.BTEST(met_flags,MF_ENSTURB) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC52,SW_SHOWNORMAL )
  CALL ShowControl( iwnd_db,IDB_STATIC53,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSTURB)
  CALL EnableControl( iwnd_db,IDB_REAL11,TRUE)
  CALL ShowControl  ( iwnd_db,IDB_REAL11,SW_SHOWNORMAL )
END IF

RETURN
END
!*******************************************************************************
!                     Show Ensemple Description
!*******************************************************************************
SUBROUTINE show_ens_MOD( iwnd_db )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the ensemble description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( .NOT.BTEST(met_flags,MF_ENSBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC50,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSBASIC)
END IF

IF( BTEST(met_flags,MF_ENSSCALE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC51,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSSCALE)
  CALL EnableControl( iwnd_db,IDB_REAL10,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_REAL10,SW_HIDE )
END IF

IF( BTEST(met_flags,MF_ENSTURB) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC52,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC53,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSTURB)
  CALL EnableControl( iwnd_db,IDB_REAL11,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_REAL11,SW_HIDE )
END IF

IF( .NOT. BTEST(met_flags,MF_ENSNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC54,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSNOPARM)
END IF

RETURN
END
!*******************************************************************************
!                     Show Ensemple Description
!*******************************************************************************
SUBROUTINE hide_ens( iwnd_db )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the ensemble description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( BTEST(met_flags,MF_ENSBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC50,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSBASIC)
END IF

IF( BTEST(met_flags,MF_ENSSCALE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC51,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSSCALE)
  CALL EnableControl( iwnd_db,IDB_REAL10,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_REAL10,SW_HIDE )
END IF

IF( BTEST(met_flags,MF_ENSTURB) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC52,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC53,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSTURB)
  CALL EnableControl( iwnd_db,IDB_REAL11,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_REAL11,SW_HIDE )
END IF

IF( BTEST(met_flags,MF_ENSNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC54,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSNOPARM)
END IF

RETURN
END
!*******************************************************************************
!                     Show Ensemple Description
!*******************************************************************************
SUBROUTINE show_ens_obs( iwnd_db )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the ensemble description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( .NOT.BTEST(met_flags,MF_ENSBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC50,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSBASIC)
END IF

IF( BTEST(met_flags,MF_ENSNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC54,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSNOPARM)
END IF

IF( .NOT.BTEST(met_flags,MF_ENSSCALE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC51,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_ENSSCALE)
  CALL EnableControl( iwnd_db,IDB_REAL10,TRUE )
  CALL ShowControl  ( iwnd_db,IDB_REAL10,SW_SHOWNORMAL )
END IF

IF( BTEST(met_flags,MF_ENSTURB) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC52,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC53,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_ENSTURB)
  CALL EnableControl( iwnd_db,IDB_REAL11,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_REAL11,SW_HIDE )
END IF

RETURN
END
!*******************************************************************************
!                     Show BL Description
!*******************************************************************************
SUBROUTINE show_BL_calc( iwnd_db,iflag,jflag )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              iflag
INTEGER              jflag

INTEGER i

IF( BTEST(met_flags,MF_BLNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC55,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLNOPARM)
END IF

IF( BTEST(met_flags,MF_BLPROFILE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC56,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLPROFILE)
END IF

IF( .NOT.BTEST(met_flags,MF_BLBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC39,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLBASIC)
END IF

IF( BTEST(met_flags,MF_BLSIMPLE) )THEN
  met_flags = IBCLR(met_flags,MF_BLSIMPLE)
  DO i = 40,46
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
  END DO
  DO i = 3,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
  END DO
END IF

IF( .NOT.BTEST(met_flags,MF_BLCALC) )THEN
  met_flags = IBSET(met_flags,MF_BLCALC)
  IF( jflag == 0 )THEN
    DO i = 47,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 7,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,TRUE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
  ELSE
    met_flags = IBSET(met_flags,MF_LANDCOVER)
    DO i = 49,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 9,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,TRUE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    IF( jflag == 1 )THEN
      met_flags = IBSET(met_flags,MF_WETNESS)
      CALL EnableControl( iwnd_db,IDB_COMBO5,TRUE )
      CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
      CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_SHOWNORMAL )
    END IF
  END IF
ELSE
  IF( jflag == 0 )THEN
    IF( BTEST(met_flags,MF_LANDCOVER) )THEN
      met_flags = IBCLR(met_flags,MF_LANDCOVER)
      IF( BTEST(met_flags,MF_WETNESS) )THEN
        met_flags = IBCLR(met_flags,MF_WETNESS)
        CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
        CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
        CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
      END IF
      DO i = 47,48
        CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
      END DO
      DO i = 7,8
        CALL EnableControl( iwnd_db,REAL_BASE+i,TRUE )
        CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
      END DO
    END IF
  ELSE
    IF( .NOT.BTEST(met_flags,MF_LANDCOVER) )THEN
      met_flags = IBSET(met_flags,MF_LANDCOVER)
      DO i = 47,48
        CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
      END DO
      DO i = 7,8
        CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
        CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
      END DO
      IF( jflag == 1 )THEN
        met_flags = IBSET(met_flags,MF_WETNESS)
        CALL EnableControl( iwnd_db,IDB_COMBO5,TRUE )
        CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
        CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_SHOWNORMAL )
      END IF
    ELSE
      IF( jflag == 2 )THEN
        IF( BTEST(met_flags,MF_WETNESS) )THEN
          met_flags = IBCLR(met_flags,MF_WETNESS)
          CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
          CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
          CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
        END IF
      ELSE
        IF( .NOT.BTEST(met_flags,MF_WETNESS) )THEN
          met_flags = IBSET(met_flags,MF_WETNESS)
          CALL EnableControl( iwnd_db,IDB_COMBO5,TRUE )
          CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
          CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_SHOWNORMAL )
        END IF
      END IF
    END IF
  END IF
END IF

IF( iflag > 0 )THEN
  IF( .NOT.BTEST(met_flags,MF_BLOPER) )THEN
    met_flags = IBSET(met_flags,MF_BLOPER)
    CALL ShowControl( iwnd_db,IDB_STATIC57,SW_SHOWNORMAL )
  END IF
ELSE
  IF( BTEST(met_flags,MF_BLOPER) )THEN
    met_flags = IBCLR(met_flags,MF_BLOPER)
    CALL ShowControl( iwnd_db,IDB_STATIC57,SW_HIDE )
  END IF
END IF

RETURN
END
!*******************************************************************************
!                     Show BL Description
!*******************************************************************************
SUBROUTINE hide_BL( iwnd_db )

USE resource_fd
USE winAPI_fd
USE create_fi
USE met_flags_fd

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_BLNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC55,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLNOPARM)
END IF

IF( BTEST(met_flags,MF_BLPROFILE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC56,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLPROFILE)
END IF

IF( BTEST(met_flags,MF_BLBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC39,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLBASIC)
END IF

IF( BTEST(met_flags,MF_BLSIMPLE) )THEN
  met_flags = IBCLR(met_flags,MF_BLSIMPLE)
  DO i = 40,46
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
  END DO
  DO i = 3,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
  END DO
END IF

IF( BTEST(met_flags,MF_BLCALC) )THEN
  met_flags = IBCLR(met_flags,MF_BLCALC)
  IF( BTEST(met_flags,MF_LANDCOVER) )THEN
    met_flags = IBCLR(met_flags,MF_LANDCOVER)
    DO i = 49,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 9,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    IF( BTEST(met_flags,MF_WETNESS) )THEN
      met_flags = IBCLR(met_flags,MF_WETNESS)
      CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
      CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
      CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
    END IF
  ELSE
    DO i = 47,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 7,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
  END IF
END IF

IF( BTEST(met_flags,MF_BLOPER) )THEN
  met_flags = IBCLR(met_flags,MF_BLOPER)
  CALL ShowControl( iwnd_db,IDB_STATIC57,SW_HIDE )
END IF

RETURN
END
!*******************************************************************************
!                     Show terrain Description
!*******************************************************************************
SUBROUTINE show_terrain( iwnd_db,edit,lAvailT,lAvailL,lUseT,lUseL )

USE resource_fd
USE winAPI_fd
USE pcscipuf_fi
USE create_fi

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              edit
LOGICAL              lUseT
LOGICAL              lUseL
LOGICAL              lAvailT
LOGICAL              lAvailL

INTEGER i,enable,show,lenable

show = SW_SHOWNORMAL
lenable = TRUE
enable  = edit

CALL ShowControl( iwnd_db,IDB_STATIC11,show)

DO i = 19,23
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON2,edit )
CALL ShowControl  ( iwnd_db,IDB_BUTTON2,SW_SHOWNORMAL )
IF( lAvailT )THEN
  CALL ShowControl  ( iwnd_db,IDB_CHECK5,SW_SHOWNORMAL )
  CALL EnableControl( iwnd_db,IDB_CHECK5,edit )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_CHECK5,SW_HIDE )
END IF
IF( lAvailL )THEN
  CALL ShowControl  ( iwnd_db,IDB_CHECK6,SW_SHOWNORMAL )
  CALL EnableControl( iwnd_db,IDB_CHECK6,edit )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK6,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_CHECK6,SW_HIDE )
END IF


DO i = 50,59
  CALL ShowControl( iwnd_db,STATIC_BASE+i,show )
END DO
DO i = 1,4
  CALL EnableControl( iwnd_db,REAL_BASE+i,enable )
  CALL ShowControl  ( iwnd_db,REAL_BASE+i,show )
END DO
DO i = 1,2
  CALL EnableControl( iwnd_db,INT_BASE+i,enable )
  CALL ShowControl  ( iwnd_db,INT_BASE+i,show )
END DO

DO i = 60,62
  CALL ShowControl( iwnd_db,STATIC_BASE+i,show )
END DO
DO i = 4,10
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,enable )
  CALL ShowControl  ( iwnd_db,BUTTON_BASE+i,show )
END DO
CALL EnableControl( iwnd_db,IDB_LIST1,lenable )
CALL ShowControl  ( iwnd_db,IDB_LIST1,show )

RETURN
END
!*******************************************************************************
!                     Hide Terrain Description
!*******************************************************************************
SUBROUTINE hide_terrain(iwnd_db)

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )

DO i = 19,23
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON2,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK5,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_CHECK6,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK6,SW_HIDE )


DO i = 50,59
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 1,4
  CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
END DO
DO i = 1,2
  CALL EnableControl( iwnd_db,INT_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,INT_BASE+i,SW_HIDE )
END DO

DO i = 60,62
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 4,10
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_LIST1,FALSE )
CALL ShowControl  ( iwnd_db,IDB_LIST1,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show Met save Description
!*******************************************************************************
SUBROUTINE show_metsave( iwnd_db,id_level,enable )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Dialog Handle
INTEGER              enable !Dialog Handle

INTEGER i

DO i = 2,5
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO

DO i = 1,nradio(1,id_level)
  CALL EnableControl( iwnd_db,RADIO_BASE+i,enable )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_SHOWNORMAL )
END DO

CALL EnableControl( iwnd_db,IDB_COMBO2,enable )
CALL ShowControl  ( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_REAL6,enable )
CALL ShowControl  ( iwnd_db,IDB_REAL6,SW_SHOWNORMAL )

CALL EnableControl( iwnd_db,IDB_CHECK3,enable )
CALL ShowControl  ( iwnd_db,IDB_CHECK3,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_CHECK4,enable )
CALL ShowControl  ( iwnd_db,IDB_CHECK4,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Met save Description
!*******************************************************************************
SUBROUTINE hide_metsave( iwnd_db,id_level )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Dialog Handle

INTEGER i

DO i = 2,5
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO

DO i = 1,nradio(1,id_level)
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

CALL EnableControl( iwnd_db,IDB_COMBO2,FALSE )
CALL ShowControl  ( iwnd_db,IDB_COMBO2,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_REAL6,FALSE )
CALL ShowControl  ( iwnd_db,IDB_REAL6,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_CHECK3,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK3,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_CHECK4,FALSE )
CALL ShowControl  ( iwnd_db,IDB_CHECK4,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show BL Description
!*******************************************************************************
SUBROUTINE show_BL_obs( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT.BTEST(met_flags,MF_BLBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC39,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLBASIC)
END IF

IF( BTEST(met_flags,MF_BLPROFILE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC56,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLPROFILE)
END IF

IF( BTEST(met_flags,MF_BLSIMPLE) )THEN
  met_flags = IBCLR(met_flags,MF_BLSIMPLE)
  DO i = 40,46
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
  END DO
  DO i = 3,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
  END DO
END IF

IF( BTEST(met_flags,MF_BLCALC) )THEN
  met_flags = IBCLR(met_flags,MF_BLCALC)
  IF( BTEST(met_flags,MF_LANDCOVER) )THEN
    met_flags = IBCLR(met_flags,MF_LANDCOVER)
    DO i = 49,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 9,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    IF( BTEST(met_flags,MF_WETNESS) )THEN
      met_flags = IBCLR(met_flags,MF_WETNESS)
      CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
      CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
      CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
    END IF
  ELSE
    DO i = 47,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 7,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
  END IF
END IF

IF( BTEST(met_flags,MF_BLOPER) )THEN
  met_flags = IBCLR(met_flags,MF_BLOPER)
  CALL ShowControl( iwnd_db,IDB_STATIC57,SW_HIDE )
END IF

IF( .NOT.BTEST(met_flags,MF_BLNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC55,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLNOPARM)
END IF

RETURN
END
!*******************************************************************************
!                     Show BL Description
!*******************************************************************************
SUBROUTINE show_BL_std( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT.BTEST(met_flags,MF_BLBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC39,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLBASIC)
END IF

IF( BTEST(met_flags,MF_BLPROFILE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC56,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLPROFILE)
END IF

IF( BTEST(met_flags,MF_BLNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC55,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLNOPARM)
END IF

IF( BTEST(met_flags,MF_BLCALC) )THEN
  met_flags = IBCLR(met_flags,MF_BLCALC)
  IF( BTEST(met_flags,MF_LANDCOVER) )THEN
    met_flags = IBCLR(met_flags,MF_LANDCOVER)
    DO i = 49,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 9,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    IF( BTEST(met_flags,MF_WETNESS) )THEN
      met_flags = IBCLR(met_flags,MF_WETNESS)
      CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
      CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
      CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
    END IF
  ELSE
    DO i = 47,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 7,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
  END IF
END IF

IF( BTEST(met_flags,MF_BLOPER) )THEN
  met_flags = IBCLR(met_flags,MF_BLOPER)
  CALL ShowControl( iwnd_db,IDB_STATIC57,SW_HIDE )
END IF

IF( .NOT.BTEST(met_flags,MF_BLSIMPLE) )THEN
  met_flags = IBSET(met_flags,MF_BLSIMPLE)
  DO i = 39,46
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
  END DO
  DO i = 3,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,TRUE )
    CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
  END DO
END IF

RETURN
END
!*******************************************************************************
!                     Show BL Description
!*******************************************************************************
SUBROUTINE show_BL_pro( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the BL description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_BLNOPARM) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC55,SW_HIDE )
  met_flags = IBCLR(met_flags,MF_BLNOPARM)
END IF

IF( .NOT.BTEST(met_flags,MF_BLBASIC) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC39,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLBASIC)
END IF

IF( BTEST(met_flags,MF_BLSIMPLE) )THEN
  met_flags = IBCLR(met_flags,MF_BLSIMPLE)
  DO i = 40,46
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
  END DO
  DO i = 3,6
    CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
  END DO
END IF

IF( BTEST(met_flags,MF_BLCALC) )THEN
  met_flags = IBCLR(met_flags,MF_BLCALC)
  IF( BTEST(met_flags,MF_LANDCOVER) )THEN
    met_flags = IBCLR(met_flags,MF_LANDCOVER)
    DO i = 49,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 9,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    IF( BTEST(met_flags,MF_WETNESS) )THEN
      met_flags = IBCLR(met_flags,MF_WETNESS)
      CALL EnableControl( iwnd_db,IDB_COMBO5,FALSE )
      CALL ShowControl  ( iwnd_db,IDB_COMBO5,SW_HIDE )
      CALL ShowControl  ( iwnd_db,IDB_STATIC72,SW_HIDE )
    END IF
  ELSE
    DO i = 47,49
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    DO i = 7,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
      CALL ShowControl  ( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
  END IF
END IF

IF( BTEST(met_flags,MF_BLOPER) )THEN
  met_flags = IBCLR(met_flags,MF_BLOPER)
  CALL ShowControl( iwnd_db,IDB_STATIC57,SW_HIDE )
END IF

IF( .NOT.BTEST(met_flags,MF_BLPROFILE) )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC56,SW_SHOWNORMAL )
  met_flags = IBSET(met_flags,MF_BLPROFILE)
END IF

RETURN
END
!*******************************************************************************
!                     Show Gridded Description
!*******************************************************************************
SUBROUTINE show_gridded( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the gridded description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_MRF) )RETURN

met_flags = IBSET(met_flags,MF_MRF)

DO i = 60,64
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON6,TRUE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON6,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Gridded Description
!*******************************************************************************
SUBROUTINE hide_gridded( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the upper air description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT. BTEST(met_flags,MF_MRF) )RETURN

met_flags = IBCLR(met_flags,MF_MRF)

DO i = 60,64
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON6,FALSE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON6,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show Gridded Description
!*******************************************************************************
SUBROUTINE show_assim( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the assim description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_ASSIM) )RETURN

met_flags = IBSET(met_flags,MF_ASSIM)

DO i = 95,99
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON11,TRUE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON11,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Gridded Description
!*******************************************************************************
SUBROUTINE hide_assim( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the upper air description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT. BTEST(met_flags,MF_ASSIM) )RETURN

met_flags = IBCLR(met_flags,MF_ASSIM)

DO i = 95,99
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON11,FALSE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON11,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show Time Description
!*******************************************************************************
SUBROUTINE show_time( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the timedescription part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_TIME) )RETURN

met_flags = IBSET(met_flags,MF_TIME)

CALL ShowControl( iwnd_db,IDB_STATIC11,SW_SHOWNORMAL )
DO i = 11,12
  CALL EnableControl( iwnd_db,RADIO_BASE+i,TRUE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_SHOWNORMAL )
END DO

RETURN
END
!*******************************************************************************
!                     Show Tbin Description
!*******************************************************************************
SUBROUTINE show_tbin( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the timedescription part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( BTEST(met_flags,MF_TBIN) )RETURN

met_flags = IBSET(met_flags,MF_TBIN)

CALL ShowControl  ( iwnd_db,IDB_STATIC65,SW_SHOWNORMAL )
CALL ShowControl  ( iwnd_db,IDB_STATIC66,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_REAL14,TRUE )
CALL ShowControl  ( iwnd_db,IDB_REAL14,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_COMBO12,TRUE )
CALL ShowControl  ( iwnd_db,IDB_COMBO12,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Tbin Description
!*******************************************************************************
SUBROUTINE hide_tbin( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the timedescription part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

IF( .NOT. BTEST(met_flags,MF_TBIN) )RETURN

met_flags = IBCLR(met_flags,MF_TBIN)

CALL ShowControl  ( iwnd_db,IDB_STATIC65,SW_HIDE )
CALL ShowControl  ( iwnd_db,IDB_STATIC66,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_REAL14,FALSE )
CALL ShowControl  ( iwnd_db,IDB_REAL14,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_COMBO12,FALSE )
CALL ShowControl  ( iwnd_db,IDB_COMBO12,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Hide Time Description
!*******************************************************************************
SUBROUTINE hide_time( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE pcscipuf_fi
USE create_fi
USE met_flags_fd

!     This routine updates the time description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT. BTEST(met_flags,MF_TIME) ) RETURN

met_flags = IBCLR(met_flags,MF_TIME)

CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )
DO i = 11,12
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE )
  CALL ShowControl  ( iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

RETURN
END
!*******************************************************************************
!                     Show Profile Description
!*******************************************************************************
SUBROUTINE show_profile( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the upper air description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_UAIR) )RETURN

met_flags = IBSET(met_flags,MF_UAIR)

DO i = 20,25
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON4,TRUE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_INT1,TRUE )
CALL ShowControl  ( iwnd_db,IDB_INT1,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Profile Description
!*******************************************************************************
SUBROUTINE hide_profile( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the upper air description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT. BTEST(met_flags,MF_UAIR) )RETURN

met_flags = IBCLR(met_flags,MF_UAIR)

DO i = 20,25
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON4,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_INT1,FALSE )
CALL ShowControl  ( iwnd_db,IDB_INT1,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show Surface Description
!*******************************************************************************
SUBROUTINE show_surface( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the surface description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( BTEST(met_flags,MF_SRF) )RETURN

met_flags = IBSET(met_flags,MF_SRF)

DO i = 30,35
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON5,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_INT2,TRUE )
CALL ShowControl  ( iwnd_db,IDB_INT2,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Surface Description
!*******************************************************************************
SUBROUTINE hide_surface( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the surface description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER i

IF( .NOT. BTEST(met_flags,MF_SRF) )RETURN

met_flags = IBCLR(met_flags,MF_SRF)

DO i = 30,35
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
CALL ShowControl  ( iwnd_db,IDB_BUTTON5,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_INT2,FALSE )
CALL ShowControl  ( iwnd_db,IDB_INT2,SW_HIDE )

RETURN
END
!*******************************************************************************
!                     Show Simple Description
!*******************************************************************************
SUBROUTINE show_fixwnds( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the simple winds description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db

INTEGER i

IF( BTEST(met_flags,MF_FIXED) )RETURN

met_flags = IBSET(met_flags,MF_FIXED)

DO i = 26,28
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
END DO
CALL EnableControl( iwnd_db,IDB_REAL12,TRUE )
CALL ShowControl  ( iwnd_db,IDB_REAL12,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_REAL13,TRUE )
CALL ShowControl  ( iwnd_db,IDB_REAL13,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_COMBO7,TRUE )
CALL ShowControl  ( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
CALL EnableControl( iwnd_db,IDB_COMBO8,TRUE )
CALL ShowControl  ( iwnd_db,IDB_COMBO8,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!                     Hide Simple Description
!*******************************************************************************
SUBROUTINE hide_fixwnds( iwnd_db )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE create_fi
USE met_flags_fd

!     This routine updates the simple winds description part of the
!     Met Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db

INTEGER i

IF( .NOT. BTEST(met_flags,MF_FIXED) )RETURN

met_flags = IBCLR(met_flags,MF_FIXED)

DO i = 26,28
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_REAL12,FALSE )
CALL ShowControl  ( iwnd_db,IDB_REAL12,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_REAL13,FALSE )
CALL ShowControl  ( iwnd_db,IDB_REAL13,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_COMBO7,FALSE )
CALL ShowControl  ( iwnd_db,IDB_COMBO7,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_COMBO8,FALSE )
CALL ShowControl  ( iwnd_db,IDB_COMBO8,SW_HIDE )

RETURN
END
!***********************************************************************
!               MeteorologyButton
!***********************************************************************
SUBROUTINE meteorology_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE winAPI_fd
USE mettype_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE create_fi
USE GUItool_fi

!     This routine processes PUSHBUTTONs from RELDEF Dialog Box

IMPLICIT NONE

!include 'SCIPtool.inc'

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_dialog !Dialog ID number
INTEGER              id_button !Button ID number
INTEGER              id_level !Dialog level (for data storage)

LOGICAL lok
INTEGER irv

CHARACTER(PATH_MAXLENGTH) filenam

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull,AddNull

TYPE( ProjectStructure) prjdlg

LOGICAL, EXTERNAL :: hasError

!---- initialize

lok = .FALSE.

!---- Select by Button number

SELECT CASE (id_button)
  CASE( 4 ) !Select Upper Air file
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
      CALL SetControlText( iwnd_db,IDB_STATIC23,string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC24,string1 )
    END IF
  CASE( 5 ) !Select Surface file
    lok = .FALSE.
    filenam = StripNull( dbtext(3,id_level) )
    string2 = StripNull( dbtext(4,id_level) )
    CALL AddPath( filenam,string2 )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL SplitName( filenam,string2,string1 )
      dbtext(3,id_level) = AddNull( TRIM(string2) )
      dbtext(4,id_level) = AddNull( TRIM(string1) )
      CALL SetControlText( iwnd_db,IDB_STATIC33,string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC34,string1 )
    END IF
  CASE( 6 ) !Select Gridded file
    lok = .FALSE.
    filenam = StripNull( dbtext(5,id_level) )
    string2 = StripNull( dbtext(6,id_level) )
    CALL AddPath( filenam,string2 )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL SplitName( filenam,string2,string1 )
      dbtext(5,id_level) = AddNull( TRIM(string2) )
      dbtext(6,id_level) = AddNull( TRIM(string1) )
      CALL SetControlText( iwnd_db,IDB_STATIC63,string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC64,string1 )
    END IF
  CASE( 7 ) !Terrain parameters
    CALL LetsDialog( iwnd_db,IDB_TERPARM )
    CALL update_met_edit( iwnd_db,id_level,0 )
  CASE( 11 ) !Select Assim file
    lok = .FALSE.
    filenam = StripNull( dbtext(9,id_level) )
    string2 = StripNull( dbtext(10,id_level) )
    CALL AddPath( filenam,string2 )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL SplitName( filenam,string2,string1 )
      dbtext( 9,id_level) = AddNull( TRIM(string2) )
      dbtext(10,id_level) = AddNull( TRIM(string1) )
      CALL SetControlText( iwnd_db,IDB_STATIC98,string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC99,string1 )
    END IF
  CASE( 13 ) !OK - Check Data first
    lok = .FALSE.
    CALL check_meteorology_definition( iwnd_db,id_level,lok )
    IF( lok )THEN
      CALL PushButton( iwnd_db,IDB_BUTTON13,ID_OK,irv )
    END IF
  CASE( 14 ) !LOAD Met from file
    lok = .FALSE.
    filenam = TRIM(loadfile(6))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
!==== SCIP Tool
      prjdlg = project(EDIT_LEVEL_2)
      CALL SplitName( filenam,prjdlg%ID%name,prjdlg%ID%path )
      CALL RemoveExtension( prjdlg%ID%name )
      CALL GUI_SCIP_met( prjdlg,metdef(EDIT_LEVEL_2),weather )
      irv = SCIPLoadWeatherF( ToolCallerID,weather )
      CALL SCIP_GUI_met( metdef(EDIT_LEVEL_2),weather )
      lok = (irv == SCIPsuccess)
      IF( lok )THEN
      ELSE
        CALL GetToolError( 'LoadMet' )
        CALL ShowErrorMessage( iwnd_db )
      END IF
!==== SCIP Tool
      CALL convert_SCIP20_met( iwnd_db,-999,metdef(EDIT_LEVEL_2),project(EDIT_LEVEL_2) )
      CALL init_dialog_metdef( iwnd_db,id_level )
      loadfile(6) = TRIM(filenam)
    END IF
  CASE( 15 ) !CLEAR ALL Releases
    CALL default_dialog_metdef( iwnd_db,id_level )
  CASE( 16,17 )
    lok = .FALSE.
    filenam = StripNull( dbtext(id_button,id_level) )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      dbtext(id_button,id_level) = AddNull( TRIM(filenam) )
      CALL SetFileString( 38,filenam,string1 )
      string2 = '(File:'//TRIM(string1)//')'
      CALL SetControlText( iwnd_db,STATIC_BASE+70+id_button,string2 )
    END IF
  CASE( 18 )
    lok = .FALSE.
    filenam = StripNull( dbtext(id_button,id_level) )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      dbtext(id_button,id_level) = AddNull( TRIM(filenam) )
      CALL SetFileString( 38,filenam,string1 )
      string2 = '(File:'//TRIM(string1)//')'
      CALL SetControlText( iwnd_db,STATIC_BASE+70+id_button,string2 )
    END IF
  CASE( 19 )
    lok = .FALSE.
    filenam = StripNull( dbtext(id_button,id_level) )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                    filenam ) !  Filename Dialog
    IF( lok )THEN
      dbtext(id_button,id_level) = AddNull( TRIM(filenam) )
      CALL SetFileString( 38,filenam,string1 )
      string2 = '(File:'//TRIM(string1)//')'
      CALL SetControlText( iwnd_db,STATIC_BASE+70+id_button,string2 )
    END IF
  CASE DEFAULT
END SELECT

RETURN
END
