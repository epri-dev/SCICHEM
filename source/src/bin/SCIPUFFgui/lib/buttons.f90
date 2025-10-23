!***********************************************************************
!               ProcessButtons
!***********************************************************************
RECURSIVE SUBROUTINE process_button( iwnd_db,MyCmd )

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

! This routine processes PUSHBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN),     INTENT ( IN ) :: iwnd_db !Dialog Box handle
TYPE( CMD ),              INTENT ( IN ) :: MyCmd   !Command Structure

INTEGER id_button,id_type,id_dialog,id_level
INTEGER id
INTEGER(POINTER_LEN) ictrl

LOGICAL lok

!---- Extract Command parameters

id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Check to see if Pushbutton is enabled
!       This is necessary because Accerator functions can send messages
!       from disables buttons

id    = id_button + 100*id_type
ictrl = GetDlgItem( iwnd_db,id )

IF( ictrl /= NULL_POINTER )THEN
  lok = IsWindowEnabled(ictrl) /= FALSE
  IF( .NOT.lok )RETURN
END IF

!---- Select proper call based on dialog number

SELECT CASE( id_dialog )
  CASE( IDB_PLOT )                                                      !PLOT
    CALL plot_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_PLAY )                                                      !ANIMATE
    CALL play_button( iwnd_db,id_button )

  CASE( IDB_CONTOUR )                                                   !CONTOUR
    CALL contour_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_SLICE,IDB_PLTOPT,IDB_PLTMORE,IDB_LABELS,IDB_MAPS,IDB_AXES ) !DEFAULT PLOT Buttons
    CALL default_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_ZOOM )                                                      !ZOOM
    CALL zoom_button( id_button )

  CASE( IDB_PICK )                                                      !PICK
    CALL pick_button( iwnd_db,id_button,id_level )

  CASE( IDB_XYTABLE )                                                   !TABLE OUTPUT
    CALL table_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_EDTPRJ )                                                    !EDTPRJ (NEW Project)
    CALL edit_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_TIME,IDB_DOMAIN,IDB_PRJDEF,IDB_SETUP,IDB_RESTART )          !DEFAULT CREATE Buttons
    CALL default_button_edt( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_OPTIONS )                                                   !OPTIONS (Restart )
    CALL options_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_SCIPUF )                                                    !SCIPUF (RUN Dialog )
    CALL run_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_RELDEF )                                                    !RELDEF
    CALL release_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_MATDEF )                                                    !MATDEF
    CALL material_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_COMLST )                                                    !COMLST
    CALL check_dialog( iwnd_db,id_dialog,id_level,lok )                 ! Check Parameters
    IF( lok )CALL save_dialog_matcmp( iwnd_db,id_level )

  CASE( IDB_NEWLST )                                                    !NEWLST
    CALL save_dialog_newlst( iwnd_db,id_level )

  CASE( IDB_METDEF )                                                    !METDEF
    CALL meteorology_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_TERPARM )                                                   !TERRAIN
    CALL terrain_button( iwnd_db,id_dialog,id_button,id_level )

  CASE( IDB_RNDPARM )                                                   !Random release
    CALL random_button( iwnd_db,id_button,id_level )

  CASE( IDB_UNCPARM )                                                   !Source Uncertainty release
    CALL uncertainty_button( iwnd_db,id_button,id_level )

  CASE( IDB_MULTICOMP )                                                 !Multicomponent release
    CALL multicomp_button( iwnd_db,id_button,id_level )

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!               LetsDialog
!***********************************************************************
RECURSIVE SUBROUTINE LetsDialog( iwnd_db,IPARAM )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi
USE SwinAPI
USE guiAPI

! Create Modal Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db !Calling Window handle
INTEGER,              INTENT ( IN ) :: IPARAM  !Dialog Box Parameter

CHARACTER(24) BOX

INTEGER irv,id,il,id_dialog,Map_save
LOGICAL SliceOK,lok_tb,lok_db

INTEGER(POINTER_LEN)iirv,ifocus,JPARAM

!---- Save current focus

ifocus = GetFocus()

!---- Disable TOOLBOX Exit Button

CALL IsControlEnabled( hwnd_db,ID_OK,lok_tb )

!---- Disable Modeless Dialog Box

lok_db = IsWindowEnabled( hwnd_db ) /= FALSE
IF( lok_db )irv = EnableWindow( hwnd_db,FALSE )

!---- Prevent user from moving Main window while Dialog Box is active

lDontMove = .TRUE.

!---- Find Calling Dialog Box ID parameters

CALL FindHwndList( iwnd_db,id,il )

!---- Set DialogBox specific parameters

IF( IPARAM == IDB_ZOOM )THEN
ELSE IF( IPARAM == IDB_PICK )THEN
ELSE IF( IPARAM == IDB_SLICE )THEN
  SliceOK = lprintOK .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_VSLICE
  SliceOK = SliceOK .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_HINT
  IF( .NOT. SliceOK )CALL MousePause()
ELSE IF( id == IDB_PLOT )THEN
  IF( IPARAM /= IDB_XYTABLE )THEN
    CALL MousePause()                          !Disable ZOOM
    Map_save = axesdef(EDIT_LEVEL)%MapCoord    !Save plot coordinate
  END IF
END IF

!---- Dialog

IF( IPARAM == IDB_PRTFIL )THEN
  CALL PrintIt( iwnd_db )
ELSE IF( IPARAM == IDB_SAVFIL )THEN
  CALL SavePltFile( iwnd_db )
ELSE
  id_dialog  = IPARAM - 100
  JPARAM = IPARAM
  BOX = TRIM(DialogName(id_dialog))//cnull
  irv = DialogBoxParam( MyAppInst, &
                        BOX,  &
                        iwnd_db,   &
                        ADDRESSOF(DIALPROC), &
                        JPARAM )
END IF

!---- Done - Enable Modeless Dialog Box

IF( lok_db )irv = EnableWindow( hwnd_db,TRUE )

!---- Enable TOOLBOX Exit Button

IF( lok_tb )CALL EnableControl( hwnd_tb,ID_OK,TRUE )

!---- Enable main window movement

lDontMove = .FALSE.

!---- Perform Dialog specific actions and reset focus

IF( IPARAM == IDB_ZOOM )THEN
  CALL ZoomIt()                           !ZOOM
ELSE IF( IPARAM == IDB_PICK )THEN
ELSE IF( IPARAM == IDB_SLICE )THEN
  IF( .NOT.SliceOK )CALL MouseResume()
ELSE IF( id == IDB_PLOT )THEN
  IF( IPARAM /= IDB_XYTABLE )THEN
    IF( Map_save == axesdef(EDIT_LEVEL)%MapCoord )THEN
      CALL MouseResume ()                  !Enable ZOOM
    END IF
  END IF
  iirv = SetFocus( ifocus )
ELSE
  iirv = SetFocus( ifocus )
END IF

RETURN
END
!***********************************************************************
!               PlotButton
!***********************************************************************
SUBROUTINE plot_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE create_fi
USE dialog_fi
USE script_fi
USE pltchoice_fi
USE PlotOpt_fd
USE SwinAPI

! This routine processes PUSHBUTTONs from Plot Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

INTEGER(POINTER_LEN) mybmp
INTEGER irv

CHARACTER(PATH_MAXLENGTH) filenam

LOGICAL lok

REAL currentExceed

LOGICAL, EXTERNAL :: hasError

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                                             !DEFAULT
    PlotDef(EDIT_LEVEL) = PlotDef(DEFAULT_LEVEL)
    CALL ResetPlotField( EDIT_LEVEL,' ' )
    CALL DefaultContours( 0,EDIT_LEVEL )
    poptdef(BASE_LEVEL)  = poptdef(DEFAULT_LEVEL )        !Option Button
    popt2def(BASE_LEVEL) = popt2def(DEFAULT_LEVEL )       !Option Button
    axesdef(BASE_LEVEL)  = axesdef(DEFAULT_LEVEL )        !Axes Button
    mapdef(BASE_LEVEL)   = mapdef(DEFAULT_LEVEL )         !Maps Button
    ttldef(BASE_LEVEL)   = ttldef(DEFAULT_LEVEL )         !Titles Button
    CALL init_dialog_plot( iwnd_db,id_level )             !Initialize Dialog Box

  CASE( 2 )                                             !COPY
    CALL MousePause()                                        !Disable ZOOM
    CALL copy_bmp( hwnd_pw,mybmp,0,0,0 )                     !Copy Plot window to Bitmap
    IF( .NOT.hasError() )CALL bmp_clipboard (hwnd_pw,mybmp ) !Copy Bitmap to Clipboard
    IF( hasError() )CALL ShowErrorMessage( hwnd_mw  )            !Post Error Messages
    CALL MouseResume()                                       !Enable ZOOM

  CASE( 3 )                                            !SAVE
    CALL LetsDialog( iwnd_db,IDB_SAVFIL )                 !SAVE Dialog Box
    CALL EnableControlL( iwnd_db,IDB_BUTTON2,lprintOK )   !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON3,lprintOK )   !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON4,lprintOK )   !Enable/Disable PRINT button

  CASE( 4 )                                            !PRINT
    CALL LetsDialog( iwnd_db,IDB_PRTFIL )                 !PRINT Dialog Box

  CASE( 5 )                                            !OPTIONS
    IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE )THEN
      CALL LetsDialog(iwnd_db,IDB_PLTMORE )               !Dialog for tables
    ELSE
      CALL LetsDialog(iwnd_db,IDB_PLTOPT )                !Dialog for contours
    END IF

  CASE( 6 )                                             !SELECT
    IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
        PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
      CALL LetsDialog( iwnd_db,IDB_SLICE )                 !Dialog for vertical slices
    END IF

  CASE( 7 )                                             !TITLES
    CALL LetsDialog( iwnd_db,IDB_LABELS )                 !Dialog

  CASE( 8 )                                             !MAPS
    CALL LetsDialog( iwnd_db,IDB_MAPS )                   !Dialog

  CASE( 9 )                                             !CONTOUR
    currentExceed = dbreal(EXCD_EDIT_ID,id_level)/ContourList(PlotDef(EDIT_LEVEL)%ContourIndex,EDIT_LEVEL)%ListHdr%Scale
    CALL LetsDialog( iwnd_db,IDB_CONTOUR )                !Dialog
    dbreal(EXCD_EDIT_ID,id_level) = currentExceed*ContourList(PlotDef(EDIT_LEVEL)%ContourIndex,EDIT_LEVEL)%ListHdr%Scale
    CALL SetEditRs(iwnd_db,dbreal(EXCD_EDIT_ID,id_level),EXCD_EDIT_ID,1)

  CASE( 10 )                                            !DRAW
    CALL save_dialog( iwnd_db,id_dialog,id_level )         !Save parameters
    lpaintit = .TRUE.
    CALL enable_plot( .TRUE. )
    CALL MyUpdateWindow( hwnd_pw,.TRUE. )
    CALL check_plot( lprintOK )
    CALL EnableControlL( iwnd_db,IDB_BUTTON2,lprintOK )    !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON3,lprintOK )    !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON4,lprintOK )    !Enable/Disable PRINT button
    IF( .NOT.lprintOK )THEN
      lplotOK  = .FALSE.
      lpaintit = .FALSE.
      CALL MouseStop()
      CALL MyUpdateWindow( hwnd_pw,.TRUE. )
    END IF

  CASE( 11 )                                            !AXES
    CALL LetsDialog( iwnd_db,IDB_AXES )                   !Dialog
    CALL EnableControlL( iwnd_db,IDB_BUTTON2,lprintOK )   !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON3,lprintOK )   !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON4,lprintOK )   !Enable/Disable PRINT button

  CASE( 12 )                                            !ANIMATE
    CALL LetsDialog( iwnd_db,IDB_ANIMAT )                 !Dialog
    IF( lokbutton )THEN
      IF( AnimDel )THEN                                   !Delete
        CALL DeleteAnim( iwnd_db,id_dialog,id_button,id_level )
      ELSE IF( Play )THEN                                 !Play
        CALL PlayIt( iwnd_db,id_dialog,id_button,id_level )
      ELSE                                                !Create
        CALL AnimateIt( iwnd_db,id_dialog,id_button,id_level )
      END IF
    END IF

  CASE( 13 )                                            !SAVE SETTINGS
    lok = .FALSE.
    filenam = TRIM(loadfile(8))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam )
    IF( lok )THEN
      CALL save_dialog( iwnd_db,id_dialog,id_level )     !Save parameters
      CALL write_plotopt( iwnd_db,filenam )              !Write file
      loadfile(8) = TRIM(filenam)
    END IF

  CASE( 14 )                                            !LOAD SETTINGS
    IF( .NOT.(BTEST(pcscipuf_mode,IPMB_INTERACTIVE)) )THEN
      lok= .TRUE.
      filenam = TRIM(script_input)
    ELSE
      lok = .FALSE.
      filenam = TRIM(loadfile(8))
      CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam )
    END IF
    IF( lok )THEN
      CALL read_plotopt( iwnd_db,CURRENT_OPTION,filenam ) !Read file
      CALL init_dialog_plot( iwnd_db,id_level )           !Load parameters
      loadfile(8) = TRIM(filenam)
    END IF

  CASE( 15 )                                              !ZOOM - Pseudo button
    CALL LetsDialog( iwnd_db,IDB_ZOOM )                     !Dialog
    CALL EnableControlL( iwnd_db,IDB_BUTTON2,lprintOK )     !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON3,lprintOK )     !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON4,lprintOK )     !Enable/Disable PRINT button

  CASE( 16 )                                              !AUTO ZOOMout - Cntrl-Z
    CALL set_default_zoom()                                 !Set ZOOM parameters
    CALL EnableControlL( iwnd_db,IDB_BUTTON2,lprintOK )     !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON3,lprintOK )     !Enable/Disable COPY/SAVE button
    CALL EnableControlL( iwnd_db,IDB_BUTTON4,lprintOK )     !Enable/Disable PRINT button
    CALL CallButton( iwnd_db,IDB_BUTTON10,irv )             !Push DRAW button

  CASE( 17 )                                              !SUMMARY
    CALL MousePause()                                         !Disable ZOOM
    irv = SetCursor( hcur_arrow )                             !Set Arrow
    irv = EnableWindow( hwnd_pw,FALSE )                       !Disable PLOT window
    irv = ShowWindow( hwnd_pw,SW_HIDE )                       !Hide PLOT window
    irv = EnableWindow( hwnd_sw,TRUE )                        !Enable SUMMARY window
    irv = ShowWindow( hwnd_sw,SW_SHOW )                       !Show SUMMARY window
    lpaintit = .TRUE.
    CALL MyUpdateWindow( hwnd_sw,.TRUE. )
    irv = SetCapture( hwnd_sw )                               !Send all Mouse clicks to SUMMARY window
    irv = SetFocus( hwnd_sw )                                 !Send all Keyboard to SUMMARY window

  CASE( 18 )                                              !PICK - Pseudo button
    CALL LetsDialog( iwnd_db,IDB_PICK )                      !Dialog

  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               DefaultButton
!***********************************************************************
RECURSIVE SUBROUTINE default_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE plotdlg_fi
USE pltchoice_fi
USE GUIparam_fd
USE winAPI_fd, ONLY: POINTER_LEN

! This routine processes DEFAULT/RESTORE PUSHBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

!---- Select by Button number

IF( id_button == 1 )THEN                            !Set Defaults
  SELECT CASE( id_dialog )
    CASE( IDB_PLTOPT )                              !Plot options
      poptdef(EDIT_LEVEL) = poptdef(DEFAULT_LEVEL)
      CALL init_dialog_pltopt( iwnd_db,id_level )

    CASE( IDB_PLTMORE )                             !More plot optione
      popt2def(EDIT_LEVEL) = popt2def(DEFAULT_LEVEL)
      CALL init_dialog_pltmore( iwnd_db,id_level )

    CASE( IDB_SLICE )                               !Slice parameters
      PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX) = &
        PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)
      CALL init_dialog_slice( iwnd_db,id_level )

    CASE( IDB_LABELS )                              !Plot Titles
      ttldef(EDIT_LEVEL) = ttldef(DEFAULT_LEVEL)
      CALL init_dialog_titles( iwnd_db,id_level )

    CASE( IDB_MAPS )                                !Maps
      mapdef(EDIT_LEVEL) = mapdef(DEFAULT_LEVEL)
      CALL init_dialog_map( iwnd_db,id_level )

    CASE( IDB_AXES )                                !Plot Axes
      axesdef(EDIT_LEVEL) = axesdef(DEFAULT_LEVEL)
      CALL init_dialog_axes( iwnd_db,id_level )

    CASE DEFAULT

  END SELECT
ELSE IF( id_button == 2 )THEN                       !Additional Dialog
  CALL LetsDialog( iwnd_db,id_dialog+1 )                !More button
END IF

RETURN
END
!***********************************************************************
!               ContourButton
!***********************************************************************
SUBROUTINE contour_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE pltchoice_fi
USE SwinAPI

! This routine processes PUSHBUTTONs from CONTOUR Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

LOGICAL lok,verify_button

CHARACTER(PATH_MAXLENGTH) filenam

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                                        !DEFAULT
    CALL DefaultContours( 0,EDIT_LEVEL )
    CALL init_dialog_contour( iwnd_db,id_level )

  CASE( 4 )                                        !EDIT Contour
    listedt%dialog = IDB_CONTOUR
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_EDTLST )
    CALL update_contour_buttons( iwnd_db,TRUE )

  CASE( 5 )                                        !NEW Contour
    listedt%dialog = IDB_CONTOUR
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_NEWLST )
    CALL update_contour_buttons( iwnd_db,TRUE )

  CASE( 6 )                                        !DELETE Contour
    CALL delete_list_numeric( iwnd_db,IDB_LIST1,id_level )
    CALL update_contour_buttons( iwnd_db,TRUE )

  CASE( 7 )                                        !CLEAR ALL Contours
    string1 ='Delete ALL contour definitions'
    lok = verify_button( iwnd_db,TRIM(string1) )
    IF( lok )THEN
      nlst(id_level) = 0
      CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )
      CALL update_contour_buttons( iwnd_db,TRUE )
    END IF

  CASE( 8 )                                        !COMPUTE Contours
    listedt%dialog = IDB_CONTOUR
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_COMLST )
    CALL update_contour_buttons( iwnd_db,TRUE )

  CASE( 9 )                                        !LOAD Contours from file
    lok = .FALSE.
    filenam = TRIM(loadfile(9))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam )
    IF( lok )THEN
      CALL file_list_numeric( iwnd_db,id_dialog,IDB_LIST1,id_level,TRIM(filenam),1.0 )
      CALL update_contour_buttons( iwnd_db,TRUE )
      loadfile(9) = TRIM(filenam)
    END IF

  CASE( 10 )                                       !SAVE Contours to file
    lok = .FALSE.
    filenam = TRIM(loadfile(9))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam )
    IF( lok )THEN
      CALL save_list_numeric( iwnd_db,id_dialog,id_level,TRIM(filenam),1.0 )
      loadfile(9) = TRIM(filenam)
    END IF

END SELECT

RETURN
END
!***********************************************************************
!               ZoomButton
!***********************************************************************
SUBROUTINE zoom_button( id_button )

! This routine processes PUSHBUTTONs from ZOOM Dialog Box

IMPLICIT NONE

INTEGER, INTENT ( IN ) :: id_button !Button ID number

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                   !Draw
    CALL ZoomDone()

  CASE( 2 )                   !Zoom Out and Draw
    CALL ZoomOut()
    CALL ZoomDone()

  CASE DEFAULT
END SELECT

RETURN
END

!*******************************************************************************
!            Update Contour buttons
!*******************************************************************************
SUBROUTINE update_contour_buttons( iwnd,ienable )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE basic_fd

! This routine enables/disables the subgroup buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd    !Dialog handle
INTEGER,              INTENT ( IN ) :: ienable !enable flag

INTEGER enable(7),ID,ilev
CHARACTER(32) number

!==== Find level associaed with this window

CALL FindHwndList( iwnd,ID,ilev )

!==== Display No. of contours

WRITE(number,*)nlst(ilev)
string1 ='No. contours : '//ADJUSTL(number)
CALL SetControlText( iwnd,IDB_STATIC06,string1 )

!==== Set enable flags for each button

IF( ( nlst(ilev) > 0 ) .AND. ( ienable == TRUE ) )THEN       !Edit
  enable(1) = TRUE
ELSE
  enable(1) = FALSE
END IF
IF( ( nlst(ilev) < MAXCNTG ) .AND. ( ienable == TRUE ) )THEN !New
  enable(2) = TRUE
ELSE
  enable(2) = FALSE
END IF
enable(3) = enable(1)                                        !Delete
enable(4) = enable(1)                                        !Clear all
enable(5) = enable(2)                                        !Compute
enable(6) = enable(2)                                        !Load
IF( ( nlst(ilev) > 0 ) .AND. ( ienable == TRUE ) )THEN       !Save
  enable(7) = TRUE
ELSE
  enable(7) = FALSE
END IF

CALL EnableButtons( iwnd,enable,4,7 )

RETURN
END
!***********************************************************************
!               RunButton
!***********************************************************************
SUBROUTINE run_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE dialog_fi

! This routine processes PUSHBUTTONs from SCIPUF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

LOGICAL lok

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                                               !Restart
    CALL check_dialog( iwnd_db,id_dialog,id_level,lok )
    IF( lok )THEN
      CALL save_dialog( iwnd_db,id_dialog,id_level )
      CALL create_restart( iwnd_db )
    END IF

  CASE( 2 )                                               !Done
    CALL cancel_dialog( iwnd_db,id_dialog )
    lokbutton  = .TRUE.
    CALL exit_dialog( iwnd_db,id_dialog )

  CASE( 4 )                                               !Select optionsFile
    lok = .FALSE.
    IF( LEN_TRIM(rundef(EDIT_LEVEL_1)%optionsFile) == 0 )THEN
      string1 = TRIM(loadfile(8))
    ELSE
      string1 = TRIM(rundef(EDIT_LEVEL_1)%optionsFile)
    END IF
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,string1 )
    IF( lok )THEN
      rundef(EDIT_LEVEL_1)%optionsFile = TRIM(string1)
      CALL SetControlText( iwnd_db,IDB_STATIC42,string1 )
    END IF

  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               EditButton
!***********************************************************************
SUBROUTINE edit_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE errorParam_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE SCIAPIversion_fd
USE files_fi
USE create_fi
USE dialog_fi
USE script_fi
USE pcscipuf_fi
USE GUImatl_fi
USE GUItool_fi
USE SwinAPI

! This routine processes PUSHBUTTONs from EDTPRJ Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

INTEGER irv

CHARACTER(PATH_MAXLENGTH) filename

LOGICAL lok,verify_button,skip_rest

LOGICAL, EXTERNAL :: hasError

!---- initialize

lok = .FALSE.
lokbutton = .FALSE.
skip_rest = .FALSE.

!---- Select by Button number

SELECT CASE( id_button )
  CASE( DF_TIME )                                                !DEFINE Time
    CALL LetsDialog( iwnd_db,IDB_TIME )                            !Dialog
    IF( lokbutton )THEN                                            !On OK
        IF( BTEST(DefinedOK,DF_RELEASE) )THEN
          CALL set_checkmark( iwnd_db,DF_RELEASE )
        ELSE
          CALL hide_checkmark( iwnd_db,DF_RELEASE )
        END IF
        IF( BTEST(DefinedOK,DF_MATERIAL) )THEN
          CALL set_checkmark( iwnd_db,DF_MATERIAL )
        ELSE
          CALL hide_checkmark( iwnd_db,DF_MATERIAL )
        END IF

      DefinedOK = IBSET(DefinedOK,id_button)
      CALL build_list( iwnd_db,id_button )                             !Update Time display
      CALL set_checkmark( iwnd_db,id_button )                          !Set time checkmark
    END IF

  CASE( DF_DOMAIN )                                              !DEFINE Domain
    CALL LetsDialog( iwnd_db,IDB_DOMAIN )                          !Dialog
    IF( lokbutton )THEN                                            !On OK
        IF( BTEST(DefinedOK,DF_RELEASE) )THEN
          CALL set_checkmark( iwnd_db,DF_RELEASE )
        ELSE
          CALL hide_checkmark( iwnd_db,DF_RELEASE )
        END IF
        IF( BTEST(DefinedOK,DF_MATERIAL) )THEN
          CALL set_checkmark( iwnd_db,DF_MATERIAL )
        ELSE
          CALL hide_checkmark( iwnd_db,DF_MATERIAL )
        END IF

      IF( BTEST(DefinedOK,DF_MET) )THEN                                !Update met
        CALL build_list( iwnd_db,DF_MET )
      END IF

      DefinedOK = IBSET(DefinedOK,id_button)
      CALL build_list( iwnd_db,id_button )                             !Update domain display
      CALL set_checkmark( iwnd_db,id_button )                          !Set domain checkmark
    END IF

  CASE( DF_OPTION )                                              !DEFINE Options
    CALL LetsDialog( iwnd_db,IDB_OPTIONS )                         !Dialog
    IF( lokbutton )THEN                                            !On OK
      DefinedOK = IBSET(DefinedOK,id_button)
      CALL build_list( iwnd_db,id_button )                           !Update options display
    END IF

  CASE( DF_MATERIAL )                                            !DEFINE Material
    CALL LetsDialog( iwnd_db,IDB_MATDEF )                          !Dialog
    IF( lokbutton )THEN                                            !On OK
      CALL build_list( iwnd_db,id_button )                           !Update materials display
      CALL set_project_icon( iwnd_db,1,IDB_STATIC07 )                !Reset Project Icon (passive,dynamic,dense)
      IF( materials(EDIT_LEVEL_1)%nmatl > 0 )THEN                    !Project has materials
        DefinedOK = IBSET(DefinedOK,id_button)
        CALL set_checkmark( iwnd_db,id_button )                        !Set material Checkmark
        CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )                 !Enable Releases
      ELSE                                                           !Project has NO materials
        DefinedOK = IBCLR(DefinedOK,id_button)
        CALL hide_checkmark( iwnd_db,id_button )                       !Hide material Checkmark
        CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )                !Disable Releases
      END IF
      CALL build_list( iwnd_db,DF_RELEASE )                          !Update release display
      IF( scenario(EDIT_LEVEL_1)%nrel > 0 )THEN
        DefinedOK = IBSET(DefinedOK,DF_RELEASE)
      ELSE
        DefinedOK = IBCLR(DefinedOK,DF_RELEASE)
      END IF
      IF( BTEST(DefinedOK,DF_RELEASE) )THEN                          !Set release checkmark
        CALL set_checkmark( iwnd_db,DF_RELEASE )
      ELSE
        CALL hide_checkmark( iwnd_db,DF_RELEASE )
      END IF
    END IF

  CASE( DF_RELEASE )                                                 !DEFINE Release
    CALL LetsDialog( iwnd_db,IDB_RELDEF )                              !Dialog
    IF( lokbutton )THEN                                                !On OK
      CALL build_list( iwnd_db,id_button )                               !Update release display
      IF( scenario(EDIT_LEVEL_1)%nrel > 0 )THEN                          !Project has releases
        DefinedOK = IBSET(DefinedOK,id_button)
        CALL set_checkmark( iwnd_db,id_button )                            !Set release Checkmark
      ELSE                                                               !Project has NO releases
        DefinedOK = IBCLR(DefinedOK,id_button)
        CALL hide_checkmark( iwnd_db,id_button )                           !Hide release Checkmark
      END IF

      IF( BTEST(DefinedOK,DF_MET) )THEN                                  !Update met
        CALL build_list( iwnd_db,DF_MET ) !Build Display List
      END IF
    END IF

  CASE( DF_MET )                                                     !DEFINE Meteorology
    CALL LetsDialog( iwnd_db,IDB_METDEF )                              !Dialog
    IF( lokbutton )THEN                                                !On OK
      DefinedOK = IBSET(DefinedOK,id_button)
      CALL build_list( iwnd_db,id_button )                               !Update met display
      CALL set_checkmark( iwnd_db,id_button )                            !Set met Checkmark
      CALL build_list( iwnd_db,DF_DOMAIN )                               !Update domain display
    END IF

  CASE( DF_AUDIT )                                                   !DEFINE Audit
    CALL LetsDialog( iwnd_db,IDB_PRJDEF )                              !Dialog
    IF( lokbutton )THEN                                                !On OK
      CALL build_list( iwnd_db,id_button )                             !Update audit display
      CALL set_project_icon( iwnd_db,1,IDB_STATIC07 )                  !Reset Project Icon (passive,dynamic,dense)
      IF( BTEST(DefinedOK,DF_MET) )THEN                                !Update met
        CALL build_list( iwnd_db,DF_MET )
      END IF
    END IF

  CASE( 10 )                                                         !LOAD Project
    IF( project(EDIT_LEVEL_1)%Edit )THEN                               !If in EDIT Mode
      IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN                    !Set filename (dialog or script)
        filename = TRIM(loadfile(0))
        CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filename )
        CALL CheckVersion( iwnd_db,filename,lok )
      ELSE
        lok = .TRUE.
        filename = TRIM(script_input)
      END IF
      IF( lok )THEN                                                      !On OK
        CALL SplitName( filename,string1,string2 )                         !Set project file names to load project
        CALL clower( string1 )
        CALL RemoveExtension( string1 )
        CALL clower( string2 )
        CALL SetFileNames( string1,string2 )
        DefinedOK = DF_CLEAR
        lok_create = .FALSE.
        CALL LoadProject( iwnd_db,EDIT_LEVEL_1 )                          !Read project input files
        IF( .NOT.hasError() )THEN                                         !On success
          project_setup = .FALSE.
          CALL SetFileNames( project(EDIT_LEVEL_1)%ID%name, &
                             project(EDIT_LEVEL_1)%ID%path )                   !Reset project filenames to this project
          CALL init_dialog_new( iwnd_db,id_level )                             !Initialize Dialog Box with new data
          loadfile(0) = TRIM(filename)
        ELSE                                                                 !On error
          CALL ShowErrorMessage( iwnd_db )                                           !Post error message
          IF( project_setup )CALL PushButton( iwnd_db,0,IDB_BUTTON19,irv )     !If setup - return to setup dialog
        END IF
      ELSE                                                               !On cancel
        IF( project_setup )CALL PushButton( iwnd_db,0,IDB_BUTTON19,irv )   !If setup - return to setup dialog
      END IF
    ELSE                                                               !If NOT in EDIT mode
      CALL SetError( WN_ERROR,'Project must be recreated to implement changes', &
                     'Current project files will be deleted',' ','Edit Project' )
      CALL ShowWarningMessage( iwnd_db,.TRUE. )
      lok = .NOT.hasError()
      CALL InitError()
      IF( lok )THEN                                                      !On OK - request to go to EDIT mode
        string1 = 'Edit the current project definition'                     !Verify intent to edit
        lok = verify_button( iwnd_db,TRIM(string1) )
        IF( lok )THEN                                                       !On OK - EDIT verified
          filename = TRIM(file_scn)                                            !Need to reload releases w/o logfile updates
          scenario(EDIT_LEVEL_2)%nrel = 0                                      !Reset number of releases
          CALL getinfo_scn( iwnd_db,filename,lok,scenario(EDIT_LEVEL_2),materials(EDIT_LEVEL_1) ) !Read scn file
          IF( lok )THEN                                                        !In OK - Go to Edit mode
            CALL CopyScenario( scenario(EDIT_LEVEL_2),scenario(EDIT_LEVEL_1) )   !Copy reloaded releases
            project(EDIT_LEVEL_1)%Edit = .TRUE.                                  !Set EDIT mode
            CALL init_dialog_new( iwnd_db,id_level )                             !Initialize Dialog Box
          ELSE
            CALL ShowErrorMessage( iwnd_db )                                     !Post error message
          END IF
        END IF
      END IF
    END IF

  CASE( 11 )                                                         !CLEAR All
    IF( project(EDIT_LEVEL_1)%Edit )THEN                               !If in EDIT mode
      string1 = 'Delete the current project definition'                   !Verify intent to clear all
      lok = verify_button( iwnd_db,TRIM(string1) )
      IF( lok )THEN                                                      !On OK - intent verified
        dlgDomain(EDIT_LEVEL_1)  = dlgDomain(DEFAULT_LEVEL)                !Reset domain to default
        metdef(EDIT_LEVEL_1)     = metdef(DEFAULT_LEVEL)                   !Reset met to default
        dlgOptions(EDIT_LEVEL_1) = dlgOptions(DEFAULT_LEVEL)               !Reset options to default
        dlgTime(EDIT_LEVEL_1)    = dlgTime(DEFAULT_LEVEL)                  !Reset time to default
        project(EDIT_LEVEL_1)    = project(BASE_LEVEL)                     !Reset project to base
        CALL CopyMaterial( materials(DEFAULT_LEVEL), &
                           materials(EDIT_LEVEL_1) )                       !Reset materials to default
        CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(EDIT_LEVEL_1) )!Reset releases to default
        DefinedOK = DF_CLEAR
        CALL init_dialog_new( iwnd_db,id_level )                           !Initialize Dialog Box
      END IF
    ELSE                                                               !If NOT in EDIT mode
      CALL LetsDialog( iwnd_db,IDB_DELPRJ )                              !Dialog - Delete project
      IF( lokbutton )CALL CallButton( iwnd_db,ID_CANCEL,irv )            !Close this Dialogbox with CANCEL
      skip_rest = .TRUE.                                                 !set skip flag
    END IF

  CASE( 12 )                                                         !CREATE
    CALL check_scipuff_setup( iwnd_db,lok )                            !Check setup
    IF( lok )THEN                                                      !On OK
      CALL CallButton( iwnd_db,ID_OK,irv )                               !Close Dialogbox with OK
      CALL PushButton( hwnd_tb,0,IDB_BUTTON2,irv )                       !Push Toolbar CREATE button (pseudo button)
    END IF
    skip_rest = .TRUE.                                                   !set skip flag

  CASE( 19 )
    CALL LetsDialog( iwnd_db,IDB_SETUP )                             !DIALOG - New Project setup
    IF( .NOT.lokbutton )THEN                                           !On NOT OK
      CALL PushButton( iwnd_db,0,IDB_BUTTON10,irv )                      !Push LOAD button
    ELSE                                                               !On OK
      CALL set_project_icon( iwnd_db,1,IDB_STATIC07 )                    !Reset Project Icon (passive,dynamic,dense)
    END IF
    skip_rest = .TRUE.                                                   !set skip flag

  CASE DEFAULT
END SELECT

!---- Check for Completed description

IF( .NOT.skip_rest )THEN

!---- Check to see if project definition is complete

  lok_create = (DefinedOK == DF_ALL .OR. DefinedOK == DF_OK)
  lok_create = lok_create .AND. project(EDIT_LEVEL_1)%Edit

!---- Show/Enable Create button

  IF( (lok .OR. lokbutton) )THEN
    CALL ShowControl( iwnd_db,IDB_BUTTON12,SW_SHOWNORMAL )
    CALL EnableControlL( iwnd_db,IDB_BUTTON12,lok_create )
    IF( project(BASE_LEVEL)%OK )THEN
      CALL SetControlText( iwnd_db,ID_CANCEL,'&Cancel Changes' )
      project(BASE_LEVEL)%Run = .FALSE.
    END IF
  END IF

END IF

RETURN
END
!***********************************************************************
!               PickButton
!***********************************************************************
SUBROUTINE pick_button( iwnd_db,id_button,id_level )

USE resource_fd
USE pcscipuf_fi
USE errorParam_fd
USE default_fd
USE SwinAPI

! This routine processes PUSHBUTTONs from PICK Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT ( IN ) :: id_button !Button ID number
INTEGER,              INTENT ( IN ) :: id_level  !Dialog level (for data storage)

REAL x,y,v

LOGICAL, EXTERNAL :: hasError,MoveMouse

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                                                    !PICK Value
    x = dbreal(1,id_level)
    y = dbreal(2,id_level)
    IF( MoveMouse(x,y,x,y) )THEN
      CALL get_plot_point_data( x,y,v )
      IF( v /= NOT_SET_R )THEN
        dbreal(1,id_level) = x
        dbreal(2,id_level) = y
        dbreal(3,id_level) = v
        CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,3 )
      END IF
      IF( nxytab < maxList )THEN
        CALL EnableControl( iwnd_db,IDB_BUTTON2,TRUE )
      END IF
    ELSE
      IF( .NOT.hasError() )THEN
        CALL SetError( UK_ERROR,'Unknown error while getting value', &
                       'Close dialog and try again',' ','PickValue' )
      END IF
      CALL ShowErrorMessage( iwnd_db )
    END IF

  CASE( 2 )                                                    !Add to Table
    IF( nxytab < maxList )THEN
      nxytab = nxytab + 1
      xytab(1,nxytab) = dbreal(1,id_level)
      xytab(2,nxytab) = dbreal(2,id_level)
    ELSE
      CALL SetError( IV_ERROR,'Unable to add location to Table', &
                     'Maximum number of entries exceeded', &
                     'Use Table editor to edit locations', &
                     'PickValue' )
      CALL ShowErrorMessage( iwnd_db )
    END IF
    CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )

  CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               HideCheckmark
!***********************************************************************
SUBROUTINE hide_checkmark( iwnd_db,id_button )

USE resource_fd
USE winAPI_fd, ONLY: POINTER_LEN

! Hides a checkmark in EDTPRJ Dialog Box

IMPLICIT NONE

INTEGER, PARAMETER :: SW_HIDE               = 0

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !DialogBox handle
INTEGER,              INTENT ( IN ) :: id_button !Button ID number

INTEGER istat

!==== Set Checkmark ID number

istat = STATIC_BASE + (id_button)*10 + 1

!==== Hide the checkmark

CALL ShowControl( iwnd_db,istat,SW_HIDE )

RETURN
END
!***********************************************************************
!               SetCheckmark
!***********************************************************************
SUBROUTINE set_checkmark( iwnd_db,id_button )

USE resource_fd
USE winAPI_fd, ONLY: POINTER_LEN

! Shows a checkmark in EDTPRJ Dialog Box

IMPLICIT NONE

INTEGER, PARAMETER :: SW_SHOWNORMAL         = 1

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db   !DialogBox handle
INTEGER,              INTENT ( IN ) :: id_button !Button ID number

INTEGER istat

!==== Set Checkmark ID number

istat = STATIC_BASE + (id_button)*10 + 1

!==== Show Checkmark

CALL ShowControl( iwnd_db,istat,SW_SHOWNORMAL )

RETURN
END
!***********************************************************************
!               LoadProject
!***********************************************************************
SUBROUTINE LoadProject( iwnd_db,idlg )

USE resource_fd
USE default_fd
USE defineok_fd
USE reldef_fd
USE SCIAPIversion_fd
USE files_fi
USE errorParam_fd
USE create_fi
USE dialog_fi
USE script_fi
USE pcscipuf_fi
USE GUImatl_fi
USE GUItool_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN ) :: iwnd_db !Dialog window handle
INTEGER,              INTENT ( IN ) :: idlg    !Dialog edit level

CHARACTER(PATH_MAXLENGTH) filename

INTEGER irv,i,jversion,nmatl_start,nmtl

LOGICAL lok,restart

TYPE( ProjectStructure) prjdlg

TYPE( fileNameT ) fileT

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

!==== Initialize

filename = TRIM(file_inp)
restart  = project(idlg)%RestartTimeIndx == DEF_VAL_I

!==== Save number of materials - so we can only loop over new materials

nmatl_start = materials(DEFAULT_LEVEL)%nmatl + 1

!==== Count materials and allocate space

fileT%string = file_inp
irv = SCIPCountMaterial(ToolCallerID,fileT,nmtl)
CALL AllocateMtlList(nmtl)
IF( hasError() ) GOTO 9999

!==== Reallocate to big enough if necessary
IF( nmtl > MAXMTYP .OR. 30*nmtl > MAXMAUX )THEN
  CALL ReallocateMaterials( MAX(nmtl,MAXMTYP),MAX(30*nmtl,MAXMAUX) )
  IF( hasError() )GOTO 9999
END IF

!==== Initialize SCIPtool structures with default values (time,domain,options,materials)

prjdlg = project(DEFAULT_LEVEL)
CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
CALL RemoveExtension( prjdlg%ID%name )

CALL GUI_SCIP_ctrl    ( prjdlg,ctrl )
CALL GUI_SCIP_time    ( prjdlg,dlgTime(DEFAULT_LEVEL),time )
CALL GUI_SCIP_flags   ( prjdlg,flags )
CALL GUI_SCIP_domain  ( prjdlg,dlgDomain(DEFAULT_LEVEL),domain )
CALL GUI_SCIP_options ( prjdlg,dlgOptions(DEFAULT_LEVEL),prjOptions )
CALL GUI_SCIP_material( prjdlg,materials(DEFAULT_LEVEL),matdef,mtlList,SIZE(mtlList) )
IF( hasError() ) GOTO 9999

!==== Call SCIPtool to load project input (time,domain,options,materials)

input%project       = ctrl%project
input%input%ctrl    = ctrl%ctrl
input%input%time    = time%time
input%input%flags   = flags%flags
input%input%domain  = domain%spatial
input%input%option  = prjOptions%option
input%input%mtlHead = matdef%mtlHead

irv = SCIPLoadInpF( ToolCallerID,input,mtlList )

!==== Check/Report SCIPtool error

IF( irv == SCIPfailure )THEN
  DefinedOK = IBCLR(DefinedOK,DF_TIME)
  DefinedOK = IBCLR(DefinedOK,DF_DOMAIN)
  DefinedOK = IBCLR(DefinedOK,DF_OPTION)
  DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)
  CALL GetToolError( 'LoadProject' )
  GOTO 9999
END IF

!==== Unload SCIPtool results

ctrl%ctrl          = input%input%ctrl
time%time          = input%input%time
flags%flags        = input%input%flags
domain%spatial     = input%input%domain
prjOptions%option  = input%input%option
matdef%mtlHead     = input%input%mtlHead

!==== Transfer results from SCIPtool structures to GUI structures

CALL SCIP_GUI_flags   ( project(idlg),flags )
CALL SCIP_GUI_ctrl    ( project(idlg),ctrl )
IF( hasError() )THEN
  CALL AddErrorAction('Project restart turned off')
  CALL ShowErrorMessage(iwnd_db)
  project(idlg)%Restart = .FALSE.
  project(idlg)%RestartFile = ' '
  project(idlg)%RestartTimeIndx = NOT_SET_I
END IF
CALL SCIP_GUI_time    ( dlgTime(idlg),time )
CALL SCIP_GUI_domain  ( dlgDomain(idlg),domain )
CALL SCIP_GUI_options ( dlgOptions(idlg),prjOptions )
CALL SCIP_GUI_material( materials(idlg),matdef,mtlList )
IF( hasError() )THEN
  DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)
  GOTO 9999
END IF

CALL DeallocateMtlList()

!==== Set Definiton flags for time,domain,options and materials

DefinedOK = IBSET(DefinedOK,DF_TIME)
DefinedOK = IBSET(DefinedOK,DF_DOMAIN)
DefinedOK = IBSET(DefinedOK,DF_OPTION)
DefinedOK = IBSET(DefinedOK,DF_MATERIAL)

!==== Set Project GUI flags

project(idlg)%MapCoord = dlgDomain(idlg)%spatial%domain%coord !Enable/Disable Map drawing

!==== Check materials - set minimum concentration if necessary/possible from SCIP contours

id_matl = materials(idlg)%nmatl
DO i = nmatl_start+1,materials(idlg)%nmatl
  IF( materials(idlg)%material(i)%file /= ' ' )THEN
    IF( materials(idlg)%material(i)%path == ' ' )THEN
      materials(idlg)%material(i)%path = TRIM(prjdlg%ID%path)
    END IF
  END IF
  CALL set_conc_min( materials(idlg)%material(i) )
END DO

!==== reset Definiton flags for materials (why?)

IF( materials(idlg)%nmatl <= 0 )DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)

!==== Check for dense materials and dense gas flag

CALL check_dense( iwnd_db,idlg,.TRUE.,1 )

!==== Get project version number

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  CALL read_version( file_prj,jversion )
ELSE
  jversion = scipuff_version
END IF

!==== Restart Project - get restart project and time

IF( restart )THEN

!====== Check version - Warn on old versions

  IF( jversion < SCIAPI_30_VERSION )THEN
    lokbutton = .FALSE.
    CALL SetError( WN_ERROR, &
                   'Project was created by a version of PCSCUPUF prior to SCIP 3.0', &
                   'Restart option available only for projects created by SCIP 3.0 and later', &
                   'Do you want to import the project anyway?', &
                   'LoadProject' )
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
    IF( hasError() )GOTO 9999
  ELSE
    CALL LetsDialog( iwnd_db,IDB_RESTART )
  END IF

!====== Valid project and Restart time selected - set project and time

  IF( lokbutton )THEN
    IF( project(idlg)%RestartTimeIndx /= NOT_SET_I )THEN
      string1 = TRIM(file_puf)
      CALL RemoveExtension( string1 )
      string2 = TRIM(project(EDIT_LEVEL_1)%ID%name)
      CALL AddPath( string2,project(EDIT_LEVEL_1)%ID%path )
      CALL cupper( string1 )
      CALL cupper( string2 )
      IF( TRIM(string1) == TRIM(string2) )THEN
        CALL SetError( IV_ERROR, &
                      'Invalid restart project name', &
                      'Restart Project has same name as current project', &
                      'You cannot restart a project from itself', &
                      'LoadProject' )
        CALL ShowErrorMessage( iwnd_db )
        GOTO 9999
      END IF
      project(idlg)%RestartFile = TRIM(file_puf)
    ELSE
      project(idlg)%RestartFile =' '
    END IF
  ELSE
    project(idlg)%RestartFile =' '
    project(idlg)%RestartTimeIndx = NOT_SET_I
  END IF
  project(idlg)%Restart = project(idlg)%RestartTimeIndx /= NOT_SET_I
END IF

!==== Initialize SCIPtool structures with default values (met)

filename = TRIM(file_msc)
prjdlg   = project(idlg)
CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
CALL RemoveExtension( prjdlg%ID%name )

CALL GUI_SCIP_met( prjdlg,metdef(DEFAULT_LEVEL),weather )

!==== Call SCIPtool to load project met

irv = SCIPLoadWeatherF( ToolCallerID,weather )
lok = irv == SCIPsuccess

!==== Transfer results from SCIPtool structures to GUI structures

CALL SCIP_GUI_met( metdef(idlg),weather )

!==== Set Definiton flags for met

IF( lok )THEN
  DefinedOK = IBSET(DefinedOK,DF_MET)
ELSE
  CALL GetToolError( 'LoadProject' )
  CALL ShowErrorMessage( iwnd_db )
  DefinedOK = IBCLR(DefinedOK,DF_MET)
END IF

!==== Convert old met to new definitions

CALL convert_SCIP20_met( iwnd_db,jversion,metdef(idlg),project(idlg) )

!==== Load releases (if have valid materials)

IF( BTEST(DefinedOK,DF_MATERIAL) )THEN

!==== Initialize SCIPtool structures with default values (releases)

  filename = TRIM(file_scn)
  CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(idlg) )

!==== Call SCIPtool to load project releases

  CALL getinfo_scn( iwnd_db,filename,lok,scenario(idlg),materials(idlg) ) !Read scn file

!==== Set Definiton flags for releases

  IF( lok )THEN
    DefinedOK = IBSET(DefinedOK,DF_RELEASE)
  ELSE
    DefinedOK = IBCLR(DefinedOK,DF_RELEASE)
  END IF

!==== Check for buoyant reelases and dymanic project flag

  CALL check_buoyant( iwnd_db,idlg,project_setup )

ELSE

!==== NO materials - clear release definition flag

  DefinedOK = IBCLR(DefinedOK,DF_RELEASE)

END IF

!==== Reset material definition flag

IF( materials(idlg)%nmatl <= 0 )DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)

9999  CONTINUE

RETURN
END
!***********************************************************************
!               CheckScipuffSetup
!***********************************************************************
SUBROUTINE check_scipuff_setup( iwnd_db,lok )

USE default_fd
USE mettype_fd
USE reldef_fd
USE errorParam_fd
USE dialog_fi
USE create_fi
USE GUImatl_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT ( IN  ) :: iwnd_db
LOGICAL,              INTENT ( OUT ) :: lok

LOGICAL gridded_met, has_terrain

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

!==== Check material definitions

lok = materials(EDIT_LEVEL_1)%nmatl > 0
IF( .NOT.lok )THEN
  nError   = IV_ERROR
  eMessage = 'No valid Material definitions'
  eInform  = ' '
  eAction  = 'Check Material definitions'
  GOTO 9999
END IF

!==== Check release definitions

lok = scenario(EDIT_LEVEL_1)%nrel > 0
IF( .NOT.lok )THEN
  nError   = IV_ERROR
  eMessage = 'No valid Release definitions'
  eInform  = ' '
  eAction  = 'Check Release definitions'
  GOTO 9999
END IF

!==== Check met definitions - If gridded or terrain must have default horizontal resolution

has_terrain = metdef(EDIT_LEVEL_1)%lmc                                  !Non gridded met with terrain

gridded_met = (metdef(EDIT_LEVEL_1)%met == MET_MRF  ) .OR. &            !MRF input
              (metdef(EDIT_LEVEL_1)%met == MET_MEDOC)
gridded_met = gridded_met .OR. (metdef(EDIT_LEVEL_1)%met == MET_WRF  )  !WRF-ARW
gridded_met = gridded_met .OR. (metdef(EDIT_LEVEL_1)%met == MET_ASSIM ) !Assimiliating obs + gridded
gridded_met = gridded_met .OR. (metdef(EDIT_LEVEL_1)%met == MET_MEDLIS  ) !MEDOC list

lok = .NOT.( gridded_met .OR. has_terrain )

IF ( .NOT.lok )THEN
  lok = dlgDomain(EDIT_LEVEL_1)%spatial%domain%hRes == DEF_VAL_R
  IF( .NOT.lok )THEN
    nError   = IV_ERROR
    eMessage = 'Invalid horizontal resolution'
    IF( gridded_met )THEN
      eInform = 'Gridded Meteorology (MEDOC/SCIP'
      eInform = TRIM(eInform)//'/WRF'
      eInform = TRIM(eInform)//' winds)'
    ELSE
      eInform = '3D Mass-consistent Meteorology'
    END IF
    eAction = 'must use the "default" horizontal resolution'
    GOTO 9999
  END IF
END IF

RETURN

9999 CONTINUE

eRoutine = 'CheckSetup'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )

RETURN
END
