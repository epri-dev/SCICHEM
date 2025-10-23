!*******************************************************************************
!                     SavePltFile
!*******************************************************************************
SUBROUTINE SavePltFile( iwnd_db )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE script_fi
USE pltchoice_fi
USE Pcx
USE testprt

!---  This routine gets a file name to save plot (either ASCII or Bitmap)

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Window Handle

LOGICAL ltmp, lavs, lcts, lovl, lmap
LOGICAL lok, linteractive
CHARACTER(512) cbuff, ctmpf, ctmpd
CHARACTER(PATH_MAXLENGTH) ccust, filesave
CHARACTER(4) cext
INTEGER i, indx
INTEGER(POINTER_LEN) idc

INTEGER irv

LOGICAL, EXTERNAL :: hasError
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, StripNull

!---- Check Plot status first

IF( .NOT.lplotOK )RETURN

!---- Initialize flags

linteractive = BTEST(pcscipuf_mode,IPMB_INTERACTIVE)

lprj_hook  = .FALSE. !Not a Project file
lnew_hook  = .FALSE.
lsave_hook = .TRUE. !Is a Save file
lanim_hook = .FALSE.
nxpage = 1
nypage = 1
ixpage = 1
iypage = 1

IF( linteractive )THEN

!---- Initialize String buffers

  cext  = ' ' !No Default extension
  cbuff = ' ' !Initialize File types
  DO i = isavbas,nsavext !Add available types
    IF( i /= isavcts .AND. i /= isavoil .AND. i /= isavwmf .AND. &
        i /= isaveis .AND. i /= isavarc )THEN
      ctmpf = TRIM(cbuff)
      cbuff = TRIM(ctmpf)  &
      //TRIM(savext(i,1))//' ('//TRIM(savext(i,2))//')'//cnull  &
      //TRIM(savext(i,2))//cnull
    END IF
  END DO
  ctmpf = TRIM(cbuff) !Add ALL Option
  cbuff = TRIM(ctmpf)//cnull//cnull
  i = isave_format + isavbas - 1           !Set Current pointer
  ccust = cnull//cnull                     !Set Custom filter to NONE
  ctmpf = AddNull( savext(i,2) )           !Initial File Type
  ctmpd = AddNull( project(BASE_LEVEL)%ID%path ) !Default extension
  cext = savext(i,2)(3:) !Assumes first two characters are "*."
  indx = MAX(isave_format,1)
  indx = MIN(indx,nsavext-isavbas+1)

!---- SaveAsFile Common Dialog Box

  IF( indx == isavavs - isavbas + 1 )THEN
    indx = indx - 1
  ELSE IF( indx == isavbmp - isavbas + 1 )THEN
    indx = indx - 1
  ELSE IF( indx == isavusa - isavbas + 1 )THEN
    indx = indx - 4
  ELSE IF( indx == isavsci - isavbas + 1 )THEN
    indx = indx - 4
  ELSE IF( indx == isavtab - isavbas + 1 )THEN
    indx = indx - 5
  END IF

  CALL SaveFileDlg( ccust,cbuff,ctmpf,ctmpd, &
     'Plot',iwnd_db,MyAppInst,lok,lprj_hook,lsave_hook,lnew_hook,lanim_hook,cext,indx )

  IF( indx == 1 )THEN
    indx = isavavs - isavbas + 1
  ELSE IF( indx == 2 )THEN
    indx = isavbmp - isavbas + 1
  ELSE IF( indx == 3 )THEN
    indx = isavusa - isavbas + 1
  ELSE IF( indx == 4 )THEN
    indx = isavsci - isavbas + 1
  ELSE IF( indx == 5 )THEN
    indx = isavtab - isavbas + 1
  END IF

ELSE
  indx = script_value - isavbas + 1
  filesave = TRIM(script_input)
  lok = .TRUE.
END IF

!---- Make checks on valid options

IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE )THEN
  IF( (indx /= isavtab - isavbas + 1) .AND. indx /= isavbmp - isavbas + 1 )THEN
    CALL SetError( IV_ERROR,'Invalid Plot/Export combination', &
                   'Casualty Estimates can only be exported in Bitmap and Tabular formats', &
                   'Please select a different plot/export option','ExportPlot' )
    CALL ShowInfoMessage( iwnd_db )
    lok = .FALSE.
    GOTO 1000
  END IF
END IF

IF( (indx == isavusa - isavbas + 1) )THEN
  IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
      PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
    CALL SetError( IV_ERROR,'Invalid Plot/Export combination', &
                   'USA Atlas USA® not available for Vertical slices', &
                   'Please select a different plot/export option', &
                   'ExportPlot' )
    CALL ShowInfoMessage( iwnd_db )
    lok = .FALSE.
    GOTO 1000
  END IF
END IF

!---- Make checks on valid options

IF( indx == isavusa - isavbas + 1 )THEN
  IF( axesdef(BASE_LEVEL)%MapCoord /= I_LATLON .AND. &
                         project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
    CALL SetError( WN_ERROR,'Export may be inaccurate due to multiple coordinate conversions', &
                   'For best results plot in original project coordinates',' ','Street Atlas USA® ExportPlot' )
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
    IF( hasError() )THEN
      CALL InitError()
      lok = .FALSE.
      GOTO 1000
    END IF
  END IF
END IF

!---- On SUCCESS

1000  CONTINUE

IF( lok )THEN

!----   Build Path/filename

  IF( linteractive )THEN
    string1 = TRIM(ctmpf)
    CALL AddPath( string1,ctmpd )
    string2  = AddNull( string1 )
    filesave = StripNull( TRIM(string2) )
  END IF

  isave_format = indx

!----   Dialog

  IF( isave_format == isavtab - isavbas + 1 )THEN
    IF( .NOT.(PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE) )THEN
      lokbutton = .FALSE.
      file_tmp = TRIM(filesave)
      IF( linteractive )THEN
        CALL LetsDialog( iwnd_db,IDB_XYTABLE )
        lok = lokbutton
      ELSE
        lok = .TRUE.
      END IF
    END IF
  END IF

!------ Initialize Progres Box

  IF( linteractive )THEN
    IF( lok )THEN
      CALL create_progress( hwnd_mw,2 )
      string1 = 'Saving current plot to disk'
      IF( isave_format == isavarc - isavbas + 1 )THEN
        string2 = 'Format = ARCVIEW Overlay'
      ELSE IF( isave_format == isavavs - isavbas + 1 )THEN
        string2 = 'Format = AVS ASCII'
      ELSE IF( isave_format == isavbmp - isavbas + 1 )THEN
        string2 = 'Format = Windows Bitmap'
      ELSE IF( isave_format == isavwmf - isavbas + 1 )THEN
        string2 = 'Format = PCX'
      ELSE IF( isave_format == isavcts - isavbas + 1 )THEN
        string2 = 'Format = CTS Overlay'
      ELSE IF( isave_format == isavsci - isavbas + 1 )THEN
        string2 = 'Format = SCIPUFF Overlay'
      ELSE IF( isave_format == isavtab - isavbas + 1 )THEN
        string2 = 'Format = Table (ASCII)'
      ELSE IF( isave_format == isavoil - isavbas + 1 )THEN
        string2 = 'Format = OILSTOCK Overlay'
      ELSE IF( isave_format == isaveis - isavbas + 1 )THEN
        string2 = 'Format = EIS Plume'
      ELSE IF( isave_format == isavusa - isavbas + 1 )THEN
        string2 = 'Format = Street Atlas USA®'
      END IF
      CALL ReportFileName( string3,'File=',filesave )
      CALL post_write_progress( string1,string2,string3 )
    END IF
  END IF

!------ Get Device Context and set NCAR Parameters

  CALL init_DC( hwnd_pw,idc )
  IF( hasError() )GOTO 9999
  CALL SetNCARDC( idc,idc )
  CALL SetMapNCAR( 1 )

!------ ASCII Save - Plot with AVS Flag set

  IF( isave_format == isavavs - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .TRUE.  !AVS On
    lcts = .FALSE. !CTS Off
    lovl = .FALSE. !OVL Off
    lmap = .FALSE. !Output plot coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !AVS output

!------ Bitmap Save

  ELSE IF( isave_format == isavbmp - isavbas + 1 )THEN

    CALL ResetPalette( idc )
    CALL save_bmp( hbitmap,filesave ) !Bitmap output

!------ PCX Save

  ELSE IF( isave_format == isavwmf - isavbas + 1 )THEN

    CALL ResetPalette( idc )
    string1 = AddNull( TRIM(filesave) )
    irv = pcxSave( string1,idc,hbitmap ) !PCX output

!------ ASCII Save - Plot with CTS Flag set - CTS

  ELSE IF( isave_format == isavcts - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .FALSE. !AVS Off
    lcts = .TRUE.  !CTS On
    lovl = .FALSE. !OVL Off
    lmap = .TRUE.  !Output plot coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !CTS output

!------ ASCII Save - Plot with CTS Flag set and SCIPUFF flag - OVL

  ELSE IF( isave_format == isavsci - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .FALSE. !AVS Off
    lcts = .TRUE.  !CTS On
    lovl = .TRUE.  !OVL On
    lmap = .FALSE. !Output plot coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !OVL output

!------ ASCII Save - Plot with CTS Flag set and SCIPUFF flag - OVL/ARCVIEW

  ELSE IF( isave_format == isavarc - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .TRUE.  !AVS On
    lcts = .TRUE.  !CTS On
    lovl = .TRUE.  !OVL On
    lmap = .TRUE.  !Output map coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave )

!------ ASCII Save - Plot without CTS Flag set and with SCIPUFF flag - OILSTOCK

  ELSE IF( isave_format == isavoil - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .FALSE. !AVS Off
    lcts = .FALSE. !CTS Off
    lovl = .TRUE.  !OVL On
    lmap = .TRUE.  !Output map coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !OILSTOCK output

!------ ASCII Save - Plot without CTS Flag set and with SCIPUFF flag and AVS - EIS

  ELSE IF( isave_format == isaveis - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .TRUE.  !AVS on
    lcts = .FALSE. !CTS Off
    lovl = .TRUE.  !OVL On
    lmap = .FALSE. !Output plot coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !EIS output

!------ ASCII Save - Plot without SCIPUFF Flag set and with CTS flag and AVS - Street Atlas USA®

  ELSE IF( isave_format == isavusa - isavbas + 1 )THEN
    ltmp = .FALSE. !No Print
    lavs = .TRUE.  !AVS on
    lcts = .TRUE.  !CTS Off
    lovl = .FALSE. !OVL On
    lmap = .TRUE.  !Output map coordinates
    map_scale = DEF_VAL_R
    CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !EIS output

!------ ASCII Save - Table

  ELSE IF( isave_format == isavtab - isavbas + 1 )THEN
    IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE )THEN
      ltmp = .FALSE. !No Print
      lavs = .TRUE.  !AVS On
      lcts = .FALSE. !CTS Off
      lovl = .FALSE. !OVL Off
      lmap = .FALSE. !Output plot coordinates
      map_scale = DEF_VAL_R
      CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !Casuatly ASCII output
    ELSE
      IF( lok )THEN !Check for OK vs CANCEL
        IF( .NOT.linteractive )THEN
          ltmp = .FALSE. !No Print
          lavs = .FALSE. !AVS On
          lcts = .FALSE. !CTS Off
          lovl = .FALSE. !OVL Off
          lmap = .FALSE. !Output plot coordinates
          map_scale = DEF_VAL_R
          CALL plottri( ltmp,lavs,lcts,lovl,lmap,filesave ) !Casuatly ASCII output
        END IF
        CALL SaveTabFile( filesave ) !Table output
      END IF
    END IF
  END IF

!------ Cleanup

  CALL dele_DC( idc ) !Delete Device Context

9999 CONTINUE

  IF( lok )THEN
    IF( linteractive )CALL kill_progress() !Remove Progress Box
  END IF
  lprintOK = .NOT.hasError() !Set Print Status flag

!------ Error Checking

  IF( hasError() )CALL ShowInfoMessage( hwnd_mw )

END IF

!------ Error Checking

IF( linteractive )THEN
  IF( hasError() )CALL ShowInfoMessage( hwnd_mw )
END IF

RETURN
END
!*******************************************************************************
!                     SaveAnimFile
!*******************************************************************************
SUBROUTINE SaveAnimFile( filename,ibmp )

USE resource_fd
USE pcscipuf_fi
USE Pcx

!--- This routine saves a bitmap

IMPLICIT NONE

CHARACTER(*),          INTENT( IN ) :: filename
INTEGER(POINTER_LEN),  INTENT( IN ) :: ibmp

INTEGER irv
INTEGER(POINTER_LEN)idc

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

LOGICAL, EXTERNAL :: hasError

!---- Check Plot status first

IF( .NOT.lplotOK )RETURN

!------ Get Device Context and set NCAR Parameters

CALL init_DC( hwnd_pw,idc )
IF( hasError() )GOTO 9999
CALL SetNCARDC( idc,idc )
CALL SetMapNCAR( 1 )

!------ Bitmap Save

CALL ResetPalette( idc )
!irv = LEN_TRIM(filename)-1
!string1 = filename(1:irv-4)//'_'//filename(irv-2:irv)//'.pcx'
!string1 = AddNull( TRIM(string1) )
!irv = pcxSave( string1,idc,ibmp )
string1 = AddNull( TRIM(filename) )
irv = LEN_TRIM(filename)-1
string1 = filename(1:irv-4)//'.'//filename(irv-2:irv)
CALL save_bmp(ibmp,string1) !Bitmap output

!------ Cleanup

CALL dele_DC( idc ) !Delete Device Context

9999 CONTINUE

lprintOK = .NOT.hasError() !Set Print Status flag

RETURN
END

!*******************************************************************************
!                     SaveFileDlg
!*******************************************************************************
SUBROUTINE SaveFileDlg( ccust,cbuff,name,path, &
                        ctitle,hdlg,ihinst,ltest,lprjx,lsavex,lnewx,lanimx,cext, &
                        indx )

USE resource_fd
USE files_fi
USE SwinAPI
USE guiAPI
USE pcscipuf_fi

!---- Driver for Save As File Common Dialog Box

IMPLICIT NONE

CHARACTER(*)         ccust !Custom File Types
CHARACTER(*)         cbuff !Standard File Types
CHARACTER(*)         name !Filename buffer
CHARACTER(*)         path !Directory buffer
CHARACTER(*)         ctitle !Dialog Box Title
INTEGER(POINTER_LEN) hdlg !Window handle
INTEGER(POINTER_LEN) ihinst !Instance handle
LOGICAL              ltest !Return flag
LOGICAL              lprjx !Project file flag
LOGICAL              lsavex !Plot file flag
LOGICAL              lnewx !new file flag
LOGICAL              lanimx !new file flag
CHARACTER(*)         cext !Default Extension
INTEGER              indx !Initial filetype indx

CHARACTER(PATH_MAXLENGTH) coutb,title,template
CHARACTER(PATH_MAXLENGTH) filename,dirname,AddNull,StripNull
INTEGER nchf

TYPE( T_OPENFILENAME ) ofn

INTEGER nn,i,mm,iflags !c3.232,hwnd_focus

!---- Initialize

cnull = CHAR(0)
lprj_hook  = lprjx
lsave_hook = lsavex
lnew_hook  = lnewx
lanim_hook = lanimx

!3.232      hwnd_focus = GetFocus()

!---- Find zero based offsets to file name and extension

filename = StripNull( name ) !Remove NULL if there is one
nchf     = LEN_TRIM(filename) !Set length of String
nn       = 0 !Set Filename offset
IF( nchf > 0 )THEN
  i        = nchf + 1 !Start at end of string
  DO WHILE( nn == 0 .AND. i > 1 ) !Search backward for : or \
    i = i - 1 !  decrement counter
    IF( filename(i:i) == '\')nn = i !  check for backslash
    IF( filename(i:i) == ':')nn = i !  check for :
  END DO !
END IF

mm = nchf !Set extension offset
IF( mm > 0 .AND. nn > 0 )THEN
  DO WHILE( mm > nn .AND. name(mm:mm) /= '.' ) !Search Backward for .
    mm = mm - 1 !  decrement counter
  END DO !
  mm = mm + 1 !
END IF

!---- Initialize Structure

filename = AddNull( name(nn+1:nchf) ) !Initial filename
title    = 'Choose a '//TRIM(ctitle)//' File'//cnull !Title
ext_hook = AddNull( cext ) !Default extension
coutb    = ccust !Custom Filter
i = LEN_TRIM(path) !Remove trailing backslash for Windows3.1
IF( path(i:i) == '\' )THEN
  template = path(1:i-1)
  dirname = AddNull( TRIM(template) )
ELSE
  dirname  = AddNull( TRIM(path) ) !Initial directory
END IF
template = 'SAVEAS'//cnull !Dialog Template
iflags   = IOR(OFN_HIDEREADONLY &
          ,IOR(OFN_PATHMUSTEXIST  &
          ,IOR(OFN_ENABLETEMPLATE &
		      ,IOR(OFN_ENABLEHOOK,OFN_LONGNAMES)))) ! .or.
!     2           OFN_OVERWRITEPROMPT
IF( .NOT.lanim_hook )THEN
  iflags = IOR(iflags,OFN_OVERWRITEPROMPT)
END IF

ofn%lStructSize       = SIZEOF(ofn)
!3.232      ofn.hwndOwner         = hparent
ofn%hwndOwner         = hdlg
ofn%hInstance         = ihinst
ofn%lpstrFilter       = ADDRESSOF( cbuff )
ofn%lpstrCustomFilter = ADDRESSOF( coutb )
ofn%nMaxCustFilter    = LEN( coutb )
ofn%nFilterIndex      = indx
ofn%lpstrFile         = ADDRESSOF( filename )
ofn%nMaxFile          = LEN( filename )
ofn%lpstrFileTitle    = 0
ofn%nMaxFileTitle     = 0
ofn%lpstrInitialDir   = ADDRESSOF( dirname )
ofn%lpstrTitle        = ADDRESSOF( title )
ofn%Flags             = iflags
ofn%nFileOffset       = nn
ofn%nFileExtension    = mm
ofn%lpstrDefExt       = ADDRESSOF( ext_hook )
ofn%lCustData         = 0
ofn%lpfnHook          = ADDRESSOF(SaveAsHook)
ofn%lpTemplateName    = ADDRESSOF( template )
ofn%pvReserved        = NULL_POINTER
ofn%dwReserved        = 0
ofn%FlagsEx           = 0

!---- Save As File Common Dialog Box

ltest = GetSaveFileName( ofn )

!---- Sucessful return

IF( ltest )THEN
  indx = ofn%nFilterIndex
  dirname = StripNull( filename )
  CALL SplitName( dirname,name,path )
END IF

!3.232      iflags = SetFocus(hwnd_focus)

RETURN
END
!*******************************************************************************
!                     SavePrjFile
!*******************************************************************************
SUBROUTINE SavePrjFile( iwnd_db,lok )

USE resource_fd
USE reldef_fd
USE files_fi
USE pcscipuf_fi
USE GUImatl_fi
USE dialog_fi
USE create_fi
USE GUItool_fi
USE script_fi
USE pltchoice_fi
USE errorParam_fd

!     This routine gets a New Project filename

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Window handle
LOGICAL              lok !Return flag

CHARACTER(PATH_MAXLENGTH) cbuff,ctmpf,ctmpd,ccust
CHARACTER(4) cext
INTEGER   indx,ios

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN

!---- Initialize flags

  lprj_hook  = .TRUE. !Is a Project file
  lnew_hook  = .TRUE. !Is a new project
  lsave_hook = .FALSE. !Not a Save file
  lanim_hook = .FALSE. !Not a Save file

!---- Initialize String buffers

  cext    = 'prj' !Default Extension
  ccust   = 'Project File (*.prj)'//cnull//'*.'//TRIM(cext)//cnull//cnull !Custom file filter
  cbuff    = cnull//cnull !Available File Types
  ctmpf   = ' ' !Initial Filename
  ctmpd   = TRIM(project(BASE_LEVEL)%ID%path) !Initial Directory
  string4 = 'Project' !Label
  indx = 0

!---- SaveAsFile Common Dialog Driver

  CALL SaveFileDlg( ccust,cbuff,ctmpf,ctmpd, &
    string4,iwnd_db,MyAppInst,lok,lprj_hook,lsave_hook,lnew_hook,lanim_hook,cext,indx )

ELSE
  lok = .TRUE.
  CALL SplitName( script_input,ctmpf,ctmpd )
END IF
!---- On SUCCESS

IF( lok )THEN

!------ Initialize Project/File names

  LastMap = project(BASE_LEVEL)%MapCoord == I_LATLON
  project(   BASE_LEVEL) = project(   EDIT_LEVEL_1)
  dlgDomain( BASE_LEVEL) = dlgDomain( DEFAULT_LEVEL)
  metdef(    BASE_LEVEL) = metdef(    DEFAULT_LEVEL)
  dlgOptions(BASE_LEVEL) = dlgOptions(DEFAULT_LEVEL)
  dlgTime(   BASE_LEVEL) = dlgTime(   DEFAULT_LEVEL)
  CALL CopyMaterial( materials(DEFAULT_LEVEL),materials(BASE_LEVEL) )
  CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(BASE_LEVEL) )
  nTimePuff = 0
  nTimeSrf  = 0
  IF( ALLOCATED(timePuff) )THEN
    DEALLOCATE( timePuff,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    CALL SetError( UK_ERROR, &
	                  'Failed to DEALLOCATE puff time array', &
	                  'DEALLOCATE error = '//TRIM(ADJUSTL(string1)),' ', &
	                  'SCIPtoGUI_ctrl' )
	    GOTO 9999
	  END IF
  END IF
  IF( ALLOCATED(timeSrf) )THEN
    DEALLOCATE( timeSrf,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    CALL SetError( UK_ERROR, &
	                  'Failed to DEALLOCATE surface time array', &
	                  'DEALLOCATE error = '//TRIM(ADJUSTL(string1)),' ', &
	                  'SCIPtoGUI_ctrl' )
	    GOTO 9999
	  END IF
  END IF

!------ Set Project name and directory

  project(BASE_LEVEL)%ID%name = TRIM(ctmpf)
  string1 = project(BASE_LEVEL)%ID%name
  CALL cupper(string1)
  IF( INDEX(string1,'.PRJ') > 0 )THEN
    CALL RemoveExtension(project(BASE_LEVEL)%ID%name)
  END IF
  project(BASE_LEVEL)%ID%path = TRIM(ctmpd)

!------ Set File Names

  CALL SetFileNames( project(BASE_LEVEL)%ID%name,project(BASE_LEVEL)%ID%path )

  string1 = '&Edit '//TRIM(project(BASE_LEVEL)%ID%name)

!------ Put Name in TOOLBAR

  ccust = TRIM(string1)
  CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,ccust )

END IF

9998 RETURN
9999 CONTINUE
lok = .FALSE.
GOTO 9998
END

