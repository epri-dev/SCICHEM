!*******************************************************************************
!                     SaveTabFile
!*******************************************************************************
SUBROUTINE SaveTabFile(filename)

USE errorParam_fd
USE files_fi
USE param_fd
USE pcscipuf_fi
USE plotdlg_fi
USE contri_fi
USE SwinAPI

IMPLICIT NONE

!     This routime gets a file name to save plot (either ASCII or Bitmap)

CHARACTER(*) filename

CHARACTER(128) AddNull
INTEGER i,ios

LOGICAL lexist,CheckFile_NoError

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'SaveTabFile'

!==== Check file

IF( CheckFile_NoError(filename) )THEN
  string1 = AddNull(TRIM(filename))
  lexist = .NOT.(DeleteFile(string1)==FALSE)
  IF( .NOT.lexist )THEN
    nError   = OP_ERROR
    eMessage = 'Unable to delete existing file'
    eInform  = TRIM(filename)
    eAction  = 'Create File Terminated'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF
END IF

!==== Open file

OPEN(UNIT=lun_tmp,FILE=filename,STATUS='NEW',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eMessage = 'Unable to open Table file'
  eInform  = TRIM(filename)
  eAction  = 'Create File Terminated'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9999
END IF

!==== Write file

!==== Write header

IF( ltt3 )THEN
  string1 = ctt3
ELSE
  string1 = ' '
END IF
IF( lttu )THEN
  string2 = cttu
ELSE
  string2 = ' '
END IF
IF( lttl )THEN
  string3 = cttl
ELSE
  string3 = ' '
END IF
string4 = clbu
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Project :'//TRIM(project(BASE_LEVEL)%ID%name)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Path    :'//TRIM(project(BASE_LEVEL)%ID%path)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Created :'//TRIM(project(BASE_LEVEL)%audit%CreateDate)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Version :'//TRIM(project(BASE_LEVEL)%audit%Version)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Title(s):'//TRIM(string1)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#        :'//TRIM(string2)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#        :'//TRIM(string3)
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Units   :'//TRIM(string4)
WRITE(string4,*)nxytab
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#Entries :'//TRIM(ADJUSTL(string4))
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#'
IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  WRITE(lun_tmp,'(a)',IOSTAT=ios)'#   Latitude      Longitude          Data'
ELSE IF( axesdef(BASE_LEVEL)%MapCoord == I_UTM )THEN
  WRITE(lun_tmp,'(a)',IOSTAT=ios)'#   Northing       Easting           Data'
ELSE IF( axesdef(BASE_LEVEL)%MapCoord == I_CARTESIAN )THEN
  WRITE(lun_tmp,'(a)',IOSTAT=ios)'#    X (km)        Y (km)            Data'
ELSE
  WRITE(lun_tmp,'(a)',IOSTAT=ios)'#      X             Y               Data'
END IF
WRITE(lun_tmp,'(a)',IOSTAT=ios)'#============== ==============  ============='

!==== Write data

DO i = 1,nxytab
  IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
    CALL set_xytab_string(xytab(2,i),xytab(1,i),string1)
  ELSE
    CALL set_xytab_string(xytab(1,i),xytab(2,i),string1)
  END IF
  WRITE(lun_tmp,'(a)',IOSTAT=ios)TRIM(string1)
END DO

!==== Close file and return

9999  CLOSE(UNIT=lun_tmp,IOSTAT=ios)
RETURN
END
!*******************************************************************************
!            Initialize Table output Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the NewSetup Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Data level

INTEGER i,irv

!==== Go to WAIT mode

irv = SetCursor(hcur_wait) !Set Cursor
irv = SetCapture(iwnd_db) !Capture mouse

!---- Load list

CALL ClearList(iwnd_db,IDB_LIST1)
DO i = 1,nxytab
  IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
    CALL set_xytab_string(xytab(2,i),xytab(1,i),string1)
  ELSE
    CALL set_xytab_string(xytab(1,i),xytab(2,i),string1)
  END IF
  CALL AddList(iwnd_db,IDB_LIST1,-999,string1,irv) !Put String in COMBO BOX
END DO
nlst(id_level) = nxytab
CALL SetControlFont(iwnd_db,IDB_LIST1,fixfont)
IF( nlst(id_level) > 0 )THEN
  CALL SetListSelString(iwnd_db,IDB_LIST1,string1,irv)
END IF

!---- Fill info lables

CALL update_info_xytab(iwnd_db,id_level)

WRITE(string3,*)maxList
string2 = ADJUSTL(string3)
CALL SetControlText(iwnd_db,IDB_STATIC15,string2)

string2 = 'File : '//TRIM(file_tmp)
CALL SetControlText(iwnd_db,IDB_STATIC02,string2)

IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  string2 = '             Latitude              Longitude                  Data'
  string3 = 'Lat'
  string4 = 'Lon'
ELSE IF( axesdef(BASE_LEVEL)%MapCoord == I_UTM )THEN
  string2 = '            Northing               Easting                    Data'
  string3 = 'E''ng'
  string4 = 'N''ng'
ELSE IF( axesdef(BASE_LEVEL)%MapCoord == I_CARTESIAN )THEN
  string2 = '             X (km)                 Y (km)                    Data'
  string3 = 'X'
  string4 = 'Y'
ELSE
  string2 = '               X                      Y                       Data'
  string3 = 'X'
  string4 = 'Y'
END IF
CALL SetControlText(iwnd_db,IDB_STATIC16,string2)
CALL SetControlText(iwnd_db,IDB_STATIC22,string3)
CALL SetControlText(iwnd_db,IDB_STATIC23,string4)
CALL SetControlText(iwnd_db,IDB_STATIC32,string3)
CALL SetControlText(iwnd_db,IDB_STATIC33,string4)

!---- Hide Edit

CALL show_edit_xytab(iwnd_db,.FALSE.,.FALSE.)

!---- Hide Calculate

CALL show_calc_xytab(iwnd_db,.FALSE.,.FALSE.)

!---- Enable buttons

CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)

!==== Done WAIT mode

9999  irv = SetCursor(hcur_arrow) !  Set Arrow
irv = ReleaseCapture() !Release Mouse

RETURN
END
!*******************************************************************************
!            Save Table output Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE param_fd
USE errorParam_fd
USE SwinAPI
!
!     This routine save the Table Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Data level

INTEGER n,i,indx,irv,ios
REAL    x,y

!==== Go to WAIT mode

irv = SetCursor(hcur_wait) !Set Cursor
irv = SetCapture(iwnd_db) !Capture mouse

!==== Get number of entries

CALL ListCount(iwnd_db,IDB_LIST1,n)
IF( n /= nlst(id_level) )THEN
  CALL SetError( IV_ERROR, &
                'Invalid number of table entries', &
                'Some values may have been lost', &
                'Call for technical help', &
                'EditTable' )
  CALL ShowErrorMessage( iwnd_db )
  n = MIN(n,nlst(id_level))
END IF

!==== Loop over entries

nxytab = 0
DO i = 1,n
  indx = i - 1

!====== Get list string

  CALL GetListItem(iwnd_db,IDB_LIST1,indx,string1,irv)
  IF( irv <= 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to get selected list entry', &
                  'Some values may have been lost', &
                  'Call for technical help', &
                  'EditTable' )
    CALL ShowErrorMessage( iwnd_db )
  END IF

!====== Read values

  READ(string1,*,IOSTAT=ios)x,y
  IF( ios < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to read selected list entry', &
                  'Some values may have been lost', &
                  'Call for technical help', &
                  'EditTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF

!====== Add to table list

  IF( nxytab < maxList )THEN
    nxytab = nxytab + 1
    IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
      xytab(1,nxytab) = y
      xytab(2,nxytab) = x
    ELSE
      xytab(1,nxytab) = x
      xytab(2,nxytab) = y
    END IF
  END IF

END DO

!==== Done WAIT mode

9999  irv = SetCursor(hcur_arrow) !  Set Arrow
irv = ReleaseCapture() !Release Mouse

RETURN
END
!*******************************************************************************
!            Show/Hide Table edit item controls
!*******************************************************************************
SUBROUTINE show_edit_xytab(iwnd_db,showit,addit)

USE resource_fd
USE SwinAPI
!
!     This routine shows/hides the Tbale edit controls
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
LOGICAL              showit !Show/Hide flag
LOGICAL              addit !Add/edit flag

INTEGER ishow,ienable
INTEGER jshow,jenable

IF( showit )THEN
  ishow   = SW_SHOWNORMAL
  ienable = TRUE
  IF( addit )THEN
    jshow   = SW_SHOWNORMAL
    jenable = TRUE
  ELSE
    jshow   = SW_HIDE
    jenable = FALSE
  END IF
ELSE
  ishow   = SW_HIDE
  ienable = FALSE
  jshow   = SW_HIDE
  jenable = FALSE
END IF

CALL ShowControl(iwnd_db,IDB_STATIC21,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC22,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC23,ishow)

CALL EnableControl(iwnd_db,IDB_REAL1,ienable)
CALL EnableControl(iwnd_db,IDB_REAL2,ienable)
CALL EnableControl(iwnd_db,IDB_BUTTON10,jenable)
CALL EnableControl(iwnd_db,IDB_BUTTON11,ienable)
CALL EnableControl(iwnd_db,IDB_BUTTON12,ienable)
CALL ShowControl(iwnd_db,IDB_REAL1,ishow)
CALL ShowControl(iwnd_db,IDB_REAL2,ishow)
CALL ShowControl(iwnd_db,IDB_BUTTON10,jshow)
CALL ShowControl(iwnd_db,IDB_BUTTON11,ishow)
CALL ShowControl(iwnd_db,IDB_BUTTON12,ishow)

RETURN
END

!*******************************************************************************
!            Show/Hide Table Calculate item controls
!*******************************************************************************
SUBROUTINE show_calc_xytab(iwnd_db,showit,gridit)

USE resource_fd
USE SwinAPI
!
!     This routine shows/hides the Tbale edit controls
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
LOGICAL              showit !Show/Hide flag
LOGICAL              gridit !Grid/Line flag

INTEGER ishow,ienable
INTEGER jshow,jenable

CHARACTER(32) title

IF( showit )THEN
  ishow   = SW_SHOWNORMAL
  ienable = TRUE
  IF( gridit )THEN
    jshow   = SW_SHOWNORMAL
    jenable = TRUE
    title   = 'Grid generator'
  ELSE
    title   = 'Line generator'
    jshow   = SW_HIDE
    jenable = FALSE
  END IF
ELSE
  ishow   = SW_HIDE
  ienable = FALSE
  jshow   = SW_HIDE
  jenable = FALSE
  title   = 'Location generator'
END IF

CALL ShowControl(iwnd_db,IDB_STATIC31,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC32,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC33,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC34,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC35,ishow)
CALL ShowControl(iwnd_db,IDB_STATIC36,ishow)
CALL SetControlText(iwnd_db,IDB_STATIC31,title)

CALL EnableControl(iwnd_db,IDB_REAL3,ienable)
CALL EnableControl(iwnd_db,IDB_REAL4,ienable)
CALL EnableControl(iwnd_db,IDB_REAL5,ienable)
CALL EnableControl(iwnd_db,IDB_REAL6,ienable)
CALL EnableControl(iwnd_db,IDB_INT1,ienable)
CALL EnableControl(iwnd_db,IDB_INT2,jenable)
CALL EnableControl(iwnd_db,IDB_BUTTON13,ienable)
CALL EnableControl(iwnd_db,IDB_BUTTON14,ienable)
CALL ShowControl(iwnd_db,IDB_REAL3,ishow)
CALL ShowControl(iwnd_db,IDB_REAL4,ishow)
CALL ShowControl(iwnd_db,IDB_REAL5,ishow)
CALL ShowControl(iwnd_db,IDB_REAL6,ishow)
CALL ShowControl(iwnd_db,IDB_INT1,ishow)
CALL ShowControl(iwnd_db,IDB_INT2,jshow)
CALL ShowControl(iwnd_db,IDB_BUTTON13,ishow)
CALL ShowControl(iwnd_db,IDB_BUTTON14,ishow)

RETURN
END

!*******************************************************************************
!            Update Table button controls
!*******************************************************************************
SUBROUTINE update_buttons_xytab(iwnd_db,id_level,showit)

USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE param_fd
!
!     This routine shows/hides the Tbale edit controls
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Dialog data level
LOGICAL              showit !Show/Hide flag

INTEGER i,ienable

IF( showit )THEN
  ienable = TRUE
ELSE
  ienable = FALSE
END IF

CALL EnableControl(iwnd_db,IDB_LIST1,ienable)
CALL EnableControl(iwnd_db,ID_OK    ,ienable)
CALL EnableControl(iwnd_db,ID_CANCEL,ienable)
DO i = 4,9
  CALL EnableControl(iwnd_db,BUTTON_BASE+i,ienable)
END DO
CALL EnableControl(iwnd_db,IDB_BUTTON15,ienable)

IF( showit )THEN
  IF( nlst(id_level) <= 0 )THEN
    CALL EnableControl(iwnd_db,IDB_BUTTON6,FALSE)
    CALL EnableControl(iwnd_db,IDB_BUTTON7,FALSE)
  ELSE IF( nlst(id_level) >= maxList )THEN
    CALL EnableControl(iwnd_db,IDB_BUTTON5,FALSE)
    CALL EnableControl(iwnd_db,IDB_BUTTON8,FALSE)
    CALL EnableControl(iwnd_db,IDB_BUTTON9,FALSE)
    CALL EnableControl(iwnd_db,IDB_BUTTON15,FALSE)
  END IF
END IF

RETURN
END

!*******************************************************************************
!            Set xytab value string
!*******************************************************************************
SUBROUTINE set_xytab_string(x,y,string)

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
!
!     This routine sets the Table value string
!
IMPLICIT NONE

REAL x,y
CHARACTER(*) string

REAL v

CHARACTER(24) number
CHARACTER(16) xstr
CHARACTER(16) ystr

INTEGER ios

WRITE(xstr,*)x
IF( LEN_TRIM(ADJUSTL(xstr)) <= 8 )THEN
  xstr = '     '//ADJUSTL(xstr)
ELSE
  xstr = ' '//ADJUSTL(xstr)
END IF
IF( x >= 0.0 )THEN
  xstr = ' '//TRIM(xstr)
END IF

WRITE(ystr,*)y
IF( LEN_TRIM(ADJUSTL(ystr)) <= 8 )THEN
  ystr = '     '//ADJUSTL(ystr)
ELSE
  ystr = ' '//ADJUSTL(ystr)
END IF
IF( y >= 0.0 )THEN
  ystr = ' '//TRIM(ystr)
END IF

IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  CALL get_plot_point_data(y,x,v)
ELSE
  CALL get_plot_point_data(x,y,v)
END IF
IF( v == NOT_SET_R )THEN
  number = '       Invalid'
ELSE
  WRITE(number,*)v
  number = ADJUSTL(number)
  IF( number(1:1) == '.' )THEN
    number = '0'//TRIM(number)
  END IF
  number = '  '//ADJUSTL(number)
END IF
IF( v >= 0.0 )THEN
  number = ' '//TRIM(number)
END IF

WRITE(string,'(a)',IOSTAT=ios)xstr(1:15)//ystr(1:15)//number(1:16)

RETURN
END
!***********************************************************************
!               TableButton
!***********************************************************************
SUBROUTINE table_button(iwnd_db,id_dialog,id_button,id_level)

USE pcscipuf_fi
USE files_fi
USE param_fd
USE dialog_fi
USE xytabcmn

!     This routine processes PUSHBUTTONs from PICK Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_dialog !Dialog ID number
INTEGER              id_button !Button ID number
INTEGER              id_level !Dialog level (for data storage)

LOGICAL lok

CHARACTER(PATH_MAXLENGTH) filenam

!---- Select by Button number

SELECT CASE (id_button)
  CASE (4) !Edit
    CALL update_buttons_xytab(iwnd_db,id_level,.FALSE.)
    CALL show_edit_xytab(iwnd_db,.TRUE.,.FALSE.)
    xytab_edit = .TRUE.
    CALL init_edit_xytab(iwnd_db,id_level)
  CASE (5) !New
    CALL update_buttons_xytab(iwnd_db,id_level,.FALSE.)
    CALL show_edit_xytab(iwnd_db,.TRUE.,.TRUE.)
    xytab_edit = .FALSE.
    CALL init_edit_xytab(iwnd_db,id_level)
  CASE (6) !Delete
    CALL delete_xytab(iwnd_db,id_level,.FALSE.)
    CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
  CASE (7) !Clear all
    CALL delete_xytab(iwnd_db,id_level,.TRUE.)
    CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
  CASE (8) !Calculate Grid
    CALL update_buttons_xytab(iwnd_db,id_level,.FALSE.)
    CALL show_calc_xytab(iwnd_db,.TRUE.,.TRUE.)
    xytab_grid = .TRUE.
    CALL init_calc_xytab(iwnd_db,id_level)
  CASE (9) !Load
    CALL update_buttons_xytab(iwnd_db,id_level,.FALSE.)
    lok = .FALSE.
    filenam = ' '
    CALL GetFile(id_dialog,id_button,id_level,lok,iwnd_db,filenam) !  Filename Dialog
    IF( lok )THEN
      CALL read_xytab(iwnd_db,id_level,filenam)
    END IF
    CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
  CASE (10) !Add
    CALL check_edit_xytab(iwnd_db,id_level,lok)
    IF( lok )THEN
      CALL save_edit_xytab(iwnd_db,id_level)
      IF( nlst(id_level) >= maxList )THEN
        CALL show_edit_xytab(iwnd_db,.FALSE.,.FALSE.)
        CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
      END IF
    END IF
  CASE (11) !Save edit
    CALL check_edit_xytab(iwnd_db,id_level,lok)
    IF( lok )THEN
      CALL save_edit_xytab(iwnd_db,id_level)
      CALL show_edit_xytab(iwnd_db,.FALSE.,.FALSE.)
      CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
    END IF
  CASE (12) !Cancel edit
    CALL show_edit_xytab(iwnd_db,.FALSE.,.FALSE.)
    CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
  CASE (13) !Calulate
    CALL check_calc_xytab(iwnd_db,id_level,lok)
    IF( lok )THEN
      CALL save_calc_xytab(iwnd_db,id_level)
      CALL show_calc_xytab(iwnd_db,.FALSE.,.FALSE.)
      CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
    END IF
  CASE (14) !Cancel Calc
    CALL show_calc_xytab(iwnd_db,.FALSE.,.FALSE.)
    CALL update_buttons_xytab(iwnd_db,id_level,.TRUE.)
  CASE (15) !Calculate Line
    CALL update_buttons_xytab(iwnd_db,id_level,.FALSE.)
    CALL show_calc_xytab(iwnd_db,.TRUE.,.FALSE.)
    xytab_grid = .FALSE.
    CALL init_calc_xytab(iwnd_db,id_level)
  CASE (16) !Sort
    CALL sort_xytab(iwnd_db,id_level)
  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               InitEditXYTable
!***********************************************************************
SUBROUTINE init_edit_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE xytabcmn

!     This routine load edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level

INTEGER indx,irv,ios
CHARACTER(128) string

!==== EDIT mode

IF( xytab_edit )THEN

!====== Get selection

  CALL GetListCurSel(iwnd_db,IDB_LIST1,indx)
  IF( indx < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'No table entry has been selected', &
                  ' ', &
                  'Please select a table entry to edit', &
                  'EditTable' )
    CALL ShowErrorMessage( iwnd_db )
    CALL PushButton(iwnd_db,IDB_BUTTON4,IDB_BUTTON12,indx)
    GOTO 9999
  ELSE
    xytab_sel = indx
  END IF

!====== Get selection string

  CALL GetListItem(iwnd_db,IDB_LIST1,indx,string,irv)
  IF( irv <= 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to get selected list entry', &
                  'Some values may have been lost', &
                  'Call for technical help', &
                  'EditTable' )
    CALL ShowErrorMessage( iwnd_db )
    CALL PushButton(iwnd_db,IDB_BUTTON4,IDB_BUTTON12,indx)
    GOTO 9999
  END IF

!====== Read values

  READ(string,*,IOSTAT=ios)dbreal(1,id_level),dbreal(2,id_level)
  IF( ios < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to read selected list entry', &
                  'Some values may have been lost', &
                  'Call for technical help', &
                  'EditTable' )
    CALL ShowErrorMessage( iwnd_db )
    CALL PushButton(iwnd_db,IDB_BUTTON4,IDB_BUTTON12,indx)
    GOTO 9999
  END IF

!==== NEW mode

ELSE

  xytab_sel = -1
  dbreal(1,id_level) = NOT_SET_R
  dbreal(2,id_level) = NOT_SET_R

END IF

!==== load into boxes

CALL SetEditRs(iwnd_db,dbreal(1,id_level),1,2)

9999  RETURN
END
!***********************************************************************
!               CheckEditXYTable
!***********************************************************************
SUBROUTINE check_edit_xytab(iwnd_db,id_level,lok)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd

!     This routine saves edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level
LOGICAL              lok

lok = dbreal(1,id_level) /= NOT_SET_R .AND. &
dbreal(1,id_level) /= DEF_VAL_R

lok = lok .AND. dbreal(2,id_level) /= NOT_SET_R .AND. &
          dbreal(2,id_level) /= DEF_VAL_R

IF( .NOT.lok )THEN
  CALL SetError( IV_ERROR, &
                'Both X and Y must be set to valid positions', &
                ' ', &
                'Please set values', &
                'CheckEditTable' )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END

!***********************************************************************
!               SaveEditXYTable
!***********************************************************************
SUBROUTINE save_edit_xytab(iwnd_db,id_level)

USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE xytabcmn

!     This routine saves edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level

INTEGER irv

!==== EDIT mode

IF( xytab_edit )THEN

!====== Check selection

  IF( xytab_sel < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Invalid table entry number',' ', &
                  'Call for technical help', &
                  'SaveTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF

!====== Delete selection string

  CALL EnableControl(iwnd_db,IDB_LIST1,TRUE)
  CALL DeleteList(iwnd_db,IDB_LIST1,xytab_sel,irv)
  CALL EnableControl(iwnd_db,IDB_LIST1,FALSE)
  IF( irv <= 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to delete selected list entry',' ', &
                  'Call for technical help', &
                  'SaveTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF
  nlst(id_level) = nlst(id_level) - 1

!==== NEW mode

ELSE

END IF

!==== Add to list

nlst(id_level) = nlst(id_level) + 1
CALL set_xytab_string(dbreal(1,id_level),dbreal(2,id_level),string1)
CALL AddList(iwnd_db,IDB_LIST1,-999,string1,irv)
CALL SetListSelString(iwnd_db,IDB_LIST1,string1,irv)

CALL update_info_xytab(iwnd_db,id_level)

9999  RETURN
END
!***********************************************************************
!               DeleteXYTable
!***********************************************************************
SUBROUTINE delete_xytab(iwnd_db,id_level,clearit)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE param_fd

!     This routine load edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level
LOGICAL              clearit

INTEGER irv,indx

!==== CLEAR mode

IF( clearit )THEN

  nlst(id_level) = 0
  CALL ClearList(iwnd_db,IDB_LIST1)

!==== DELETE mode

ELSE

!====== Get selection index

  CALL GetListCurSel(iwnd_db,IDB_LIST1,indx)
  IF( indx < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'No table entry has been selected',' ', &
                  'Please select a table entry to delete', &
                  'DeleteTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF

!====== delete

  CALL DeleteList(iwnd_db,IDB_LIST1,indx,irv)
  IF( irv < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to delete selected list entry',' ', &
                  'Call for technical help', &
                  'DeleteTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF
  nlst(id_level) = nlst(id_level) - 1
  indx = MIN(indx,nlst(id_level)-1)
  IF( indx >= 0)CALL SetListCurSel(iwnd_db,IDB_LIST1,indx)

END IF

!==== update info boxes

CALL update_info_xytab(iwnd_db,id_level)

9999  RETURN
END

!***********************************************************************
!               UpdateInfoXYTable
!***********************************************************************
SUBROUTINE update_info_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd

!     This routine updates number of list items

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level

INTEGER i

CALL ListCount(iwnd_db,IDB_LIST1,i)
IF( i /= nlst(id_level) )THEN
  CALL SetError( IV_ERROR, &
                'Invalid number of table entries',' ', &
                'Call for technical help', &
                'UpdateTable' )
  CALL ShowErrorMessage( iwnd_db )
END IF

WRITE(string1,*)nlst(id_level)
string2 = ADJUSTL(string1)
CALL SetControlText(iwnd_db,IDB_STATIC13,string2)

9999  RETURN
END
!***********************************************************************
!               InitCalcXYTable
!***********************************************************************
SUBROUTINE init_calc_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE xytabcmn

!     This routine load calc boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level

INTEGER ni

!==== GRID mode

IF( xytab_grid )THEN

  ni = 2

!==== LINE mode

ELSE

  ni = 1

END IF

dbreal(3,id_level) = NOT_SET_R
dbreal(4,id_level) = NOT_SET_R
dbreal(5,id_level) = NOT_SET_R
dbreal(6,id_level) = NOT_SET_R
dbint(1,id_level) = NOT_SET_I
dbint(2,id_level) = NOT_SET_I

!==== load into boxes

CALL SetEditRs(iwnd_db,dbreal(3,id_level),3,4)
CALL SetEditIs(iwnd_db,dbint(1,id_level),1,ni)

9999  RETURN
END
!***********************************************************************
!               CheckCalcXYTable
!***********************************************************************
SUBROUTINE check_calc_xytab(iwnd_db,id_level,lok)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE param_fd
USE xytabcmn

!     This routine checks calc boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level
LOGICAL              lok

CHARACTER(128) eAction

lok = dbreal(3,id_level) /= NOT_SET_R .AND. &
dbreal(3,id_level) /= DEF_VAL_R

lok = lok .AND. dbreal(4,id_level) /= NOT_SET_R .AND. &
          dbreal(4,id_level) /= DEF_VAL_R

lok = lok .AND. dbreal(5,id_level) /= NOT_SET_R .AND. &
          dbreal(5,id_level) /= DEF_VAL_R

lok = lok .AND. dbreal(6,id_level) /= NOT_SET_R .AND. &
          dbreal(6,id_level) /= DEF_VAL_R

IF( .NOT.lok )THEN
  CALL SetError( IV_ERROR, &
                'Both X,Y pairs must be set to valid positions',' ', &
                'Please set values', &
                'CheckCalcTable' )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

lok = dbint(1,id_level) > 0 .AND. &
dbint(1,id_level) <= maxList

IF( xytab_grid )THEN
  lok = lok .AND. dbint(2,id_level) > 0 .AND. &
            dbint(2,id_level) <= maxList
  IF( .NOT.lok )THEN
    WRITE(eAction,*)'Please set values between 1 and',maxList
    CALL SetError( IV_ERROR, &
                  'Invalid number of points',' ', &
                   eAction, &
                  'CheckCalcTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  ELSE
    lok = dbint(1,id_level)*dbint(2,id_level) <= maxList
    IF( .NOT.lok )THEN
      WRITE(eAction,*)'Please Nx*Ny between 1 and',maxList
      CALL SetError( IV_ERROR, &
                    'Invalid number of points',' ', &
                     eAction, &
                    'CheckCalcTable' )
      CALL ShowErrorMessage( iwnd_db )
      GOTO 9999
    END IF
  END IF
ELSE
  IF( .NOT.lok )THEN
    WRITE(eAction,*)'Please set value between 1 and',maxList
    CALL SetError( IV_ERROR, &
                  'Invalid number of points',' ', &
                   eAction, &
                  'CheckCalcTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF
END IF

9999  RETURN
END

!***********************************************************************
!               SaveCalcXYTable
!***********************************************************************
SUBROUTINE save_calc_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
USE xytabcmn

!     This routine saves edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level

INTEGER irv,nx,ny,i,j
REAL    xmn,xmx,ymn,ymx,dx,dy,x,y

!==== Go to WAIT mode

irv = SetCursor(hcur_wait) !Set Cursor
irv = SetCapture(iwnd_db) !Capture mouse

!==== Save X,Y pairs

xmn = dbreal(3,id_level)
xmx = dbreal(5,id_level)
ymn = dbreal(4,id_level)
ymx = dbreal(6,id_level)

!==== GRID mode

IF( xytab_grid )THEN

  nx = dbint(1,id_level)
  ny = dbint(2,id_level)

  dx = (xmx-xmn)/FLOAT(MAX(1,nx-1))
  dy = (ymx-ymn)/FLOAT(MAX(1,ny-1))

  DO i = 1,nx
    x = xmn + FLOAT(i-1)*dx
    DO j = 1,ny
      y = ymn + FLOAT(j-1)*dy
      IF( nlst(id_level) < maxList )THEN
        nlst(id_level) = nlst(id_level) + 1
        CALL set_xytab_string(x,y,string1)
        CALL AddList(iwnd_db,IDB_LIST1,-999,string1,irv)
      END IF
    END DO
  END DO

!==== LINE mode

ELSE

  nx = dbint(1,id_level)

  dx = (xmx-xmn)/FLOAT(MAX(1,nx-1))
  dy = (ymx-ymn)/FLOAT(MAX(1,nx-1))

  DO i = 1,nx
    x = xmn + FLOAT(i-1)*dx
    y = ymn + FLOAT(i-1)*dy
    IF( nlst(id_level) < maxList )THEN
      nlst(id_level) = nlst(id_level) + 1
      CALL set_xytab_string(x,y,string1)
      CALL AddList(iwnd_db,IDB_LIST1,-999,string1,irv)
    END IF
  END DO

END IF

!==== Update info

CALL SetListSelString(iwnd_db,IDB_LIST1,string1,irv)

CALL update_info_xytab(iwnd_db,id_level)

!==== Done WAIT mode

irv = SetCursor(hcur_arrow) !  Set Arrow
irv = ReleaseCapture() !Release Mouse

9999  RETURN
END

!***********************************************************************
!               ReadXYTable
!***********************************************************************
SUBROUTINE read_xytab(iwnd_db,id_level,filename)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI
USE script_fi
USE plotdlg_fi
USE param_fd
USE xytabcmn

!     This routine saves edit boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_level !Dialog data level
CHARACTER(*)         filename

INTEGER ios,irv
REAL    x,y
CHARACTER(128) line

LOGICAL eof,linteractive

!==== Go to WAIT mode

linteractive = BTEST(pcscipuf_mode,IPMB_INTERACTIVE)

irv = SetCursor(hcur_wait) !Set Cursor
irv = SetCapture(iwnd_db) !Capture mouse

!==== Open file

OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',IOSTAT=ios)
IF( ios /= 0 )THEN
  CALL ReportFileName( line,'File=',filename )
  CALL SetError( OP_ERROR, &
                'Unable to open table file', &
                 line, &
                'Make sure the file exists', &
                'ReadTable' )
  GOTO 9999
END IF

!==== Read

eof = .FALSE.
DO WHILE( .NOT.eof)

  READ(lun_tmp,'(a)',IOSTAT=ios)line
  IF( ios > 0 )THEN
    CALL ReportFileName( line,'File=',filename )
    CALL SetError( RD_ERROR, &
                  'Unable to read table file', &
                   line, &
                  'Make sure the file is a tabular output file', &
                  'ReadTable' )
    GOTO 9999
  END IF
  eof = ios < 0

  IF( .NOT.eof .AND. line(1:1) /= '#' )THEN
    READ(line,*,IOSTAT=ios)x,y
    IF( ios /= 0 )THEN
      CALL ReportFileName( line,'File=',filename )
      CALL SetError( RD_ERROR, &
                    'Unable to read table file', &
                     line, &
                    'Make sure the file is a tabular output file', &
                    'ReadTable' )
      GOTO 9999
    END IF
    IF( linteractive )THEN
      IF( nlst(id_level) < maxList )THEN
        nlst(id_level) = nlst(id_level) + 1
        CALL set_xytab_string(x,y,string1)
        CALL AddList(iwnd_db,IDB_LIST1,-999,string1,irv)
      END IF
    ELSE
      IF( nxytab < maxList )THEN
        nxytab = nxytab + 1
        IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
          xytab(1,nxytab) = y
          xytab(2,nxytab) = x
        ELSE
          xytab(1,nxytab) = x
          xytab(2,nxytab) = y
        END IF
      END IF
    END IF
  END IF
END DO

!==== Update info

IF( linteractive )THEN
  CALL SetListSelString(iwnd_db,IDB_LIST1,string1,irv)

  CALL update_info_xytab(iwnd_db,id_level)
END IF

9998  CLOSE(UNIT=lun_tmp,IOSTAT=ios)

!==== Done WAIT mode

irv = SetCursor(hcur_arrow) !  Set Arrow
irv = ReleaseCapture() !Release Mouse

RETURN

9999  CALL ShowErrorMessage( iwnd_db )
GOTO 9998
END
!*******************************************************************************
!            Initialize Table output Dialog Box
!*******************************************************************************
SUBROUTINE sort_xytab(iwnd_db,id_level)

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE errorParam_fd
USE param_fd
USE SwinAPI
!
!     This routine sorts the xytable entries
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              id_level !Data level

REAL, DIMENSION(:), ALLOCATABLE :: x
REAL, DIMENSION(:), ALLOCATABLE :: y

REAL xx,yy

INTEGER n,i,nn,indx,ios,irv

CHARACTER(128) eInform

LOGICAL, EXTERNAl :: hasError

!==== Go to WAIT mode

irv = SetCursor(hcur_wait) !Set Cursor
irv = SetCapture(iwnd_db) !Capture mouse

!==== Get number of entries

CALL ListCount( iwnd_db,IDB_LIST1,n )
IF( n /= nlst(id_level) )THEN
  CALL SetError( IV_ERROR, &
                'Invalid number of table entries', &
                'Some values may have been lost', &
                'Call for technical help', &
                'SortTable' )
  CALL ShowErrorMessage( iwnd_db )
  n = MIN(n,nlst(id_level))
END IF

IF( n <= 0 )GOTO 9999

!==== Allocate space

ALLOCATE( x(n),y(n),STAT=ios )
IF( ios /= 0 )THEN
  WRITE(eInform,*)'Requested size =',n
  CALL SetError( UK_ERROR, &
                'Failed to allocate space for sort work space', &
                 eInform, &
                'Try reducing tabel size', &
                'SortTable' )
  GOTO 9999
END IF

!==== Loop over entries

nn = 0

DO i = 1,n
  indx = i - 1

!====== Get list string

  CALL GetListItem( iwnd_db,IDB_LIST1,indx,string1,irv )
  IF( irv <= 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to get selected list entry',' ', &
                  'Call for technical help', &
                  'SortTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF

!====== Read values

  READ(string1,*,IOSTAT=ios)xx,yy
  IF( ios < 0 )THEN
    CALL SetError( IV_ERROR, &
                  'Unable to get read list entry',' ', &
                  'Call for technical help', &
                  'SortTable' )
    CALL ShowErrorMessage( iwnd_db )
    GOTO 9999
  END IF

!====== Add to table list

  IF( nn < maxList )THEN
    nn = nn + 1
    IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
      x(nn) = yy
      y(nn) = xx
    ELSE
      x(nn) = xx
      y(nn) = yy
    END IF
  END IF

END DO

!---- Sort entries

IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  CALL vsortr( x,y,n )
  CALL vsortr( y,x,n )
ELSE
  CALL vsortr( y,x,n )
  CALL vsortr( x,y,n )
END IF

!---- Load list

CALL ClearList( iwnd_db,IDB_LIST1 )

DO i = 1,n
  IF( axesdef(BASE_LEVEL)%MapCoord == I_LATLON )THEN
    CALL set_xytab_string( y(i),x(i),string1 )
  ELSE
    CALL set_xytab_string( x(i),y(i),string1 )
  END IF
  CALL AddList( iwnd_db,IDB_LIST1,-999,string1,irv ) !Put String in COMBO BOX
END DO

CALL SetListSelString( iwnd_db,IDB_LIST1,string1,irv )

!---- Deallocate space

9999  CONTINUE

IF( hasError() )CALL ShowErrorMessage( iwnd_db )

!==== Done WAIT mode

irv = SetCursor( hcur_arrow ) !  Set Arrow
irv = ReleaseCapture()        !  Release Mouse

IF( ALLOCATED(x) )DEALLOCATE(x,STAT=ios)
IF( ALLOCATED(y) )DEALLOCATE(y,STAT=ios)

RETURN
END
!***********************************************************************
!               get_plot_point_data
!***********************************************************************
SUBROUTINE get_plot_point_data(x,y,v)

USE resource_fd
USE default_fd
USE pltchoice_fi
USE GUIparam_fd

IMPLICIT NONE

REAL x, y, v
REAL, DIMENSION(1) :: xx, yy, xd, yd
INTEGER irv, userID
INTEGER, EXTERNAL :: SCIPGetFieldValue
INTEGER, EXTERNAL :: DataTransform

!---- Transform from Plot coords to project coords
xx(1) = x
yy(1) = y

irv = DataTransform(1,xx,yy,xd,yd)

!---- Get plot field value

userID = 2222

irv = SCIPGetFieldValue(userID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType,xd(1),yd(1),v)

!---- Scale field value

v = v * ContourPlot%ListHdr%Scale

RETURN
END
