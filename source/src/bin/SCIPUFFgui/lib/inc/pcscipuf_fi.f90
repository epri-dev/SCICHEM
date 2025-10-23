MODULE pcscipuf_fi

  USE project_fd
  USE GUIparam_fd
  USE GUIstruct_fd
  USE winAPI_fd, ONLY: POINTER_LEN

  SAVE

  INTEGER(POINTER_LEN), PARAMETER :: NULL_POINTER  = 0

  ! Instance Identifies,Window Handles etc

  INTEGER(POINTER_LEN) check_hndl
  INTEGER(POINTER_LEN) dense_hndl
  INTEGER(POINTER_LEN) dynamic_hndl
  INTEGER(POINTER_LEN) fast_hndl
  INTEGER(POINTER_LEN) fixfont
  INTEGER(POINTER_LEN) hacc_ed
  INTEGER(POINTER_LEN) hacc_pd
  INTEGER(POINTER_LEN) hacc_tb
  INTEGER(POINTER_LEN) hbitmap
  INTEGER(POINTER_LEN) hcur_arrow
  INTEGER(POINTER_LEN) hcur_cross
  INTEGER(POINTER_LEN) hcur_wait
  INTEGER(POINTER_LEN) hdlgp
  INTEGER(POINTER_LEN) hwnd_db
  INTEGER(POINTER_LEN) hwnd_mw
  INTEGER(POINTER_LEN) hwnd_tb
  INTEGER(POINTER_LEN) hwnd_pb
  INTEGER(POINTER_LEN) hwnd_pw
  INTEGER(POINTER_LEN) hwnd_sw
  INTEGER(2) MainAtom
  INTEGER(POINTER_LEN) MyAppInst
  INTEGER(2) PlotAtom
  INTEGER(POINTER_LEN) scipuff_hndl
  INTEGER scipuff_version
  INTEGER tool_version
  INTEGER gui_version
  INTEGER(2) StatAtom

  CHARACTER(128) command_line
  CHARACTER(32)  ToolVersionString
  CHARACTER(32)  string_mode
  CHARACTER(32)  GUIversionString
  CHARACTER(32)  SCIPversionString
  CHARACTER(PATH_MAXLENGTH) string1
  CHARACTER(PATH_MAXLENGTH) string2
  CHARACTER(PATH_MAXLENGTH) string3
  CHARACTER(PATH_MAXLENGTH) string4
  CHARACTER(PATH_MAXLENGTH) string5
  CHARACTER(PATH_MAXLENGTH) ini_file
  CHARACTER(PATH_MAXLENGTH) SCIPdataDir
  CHARACTER(PATH_MAXLENGTH) SCIPtempDir

  INTEGER iStatus
  INTEGER applBkColor
  INTEGER dlgBkColor
  INTEGER dlgATxColor
  INTEGER dlgITxColor
  INTEGER dlgCTxColor
  INTEGER(POINTER_LEN) applBkBrush
  INTEGER(POINTER_LEN) dlgBkBrush
  INTEGER(POINTER_LEN) dlgATxBrush
  INTEGER(POINTER_LEN) dlgITxBrush
  INTEGER(POINTER_LEN) dlgCTxBrush

  LOGICAL Winsci
  LOGICAL Winsci_Edit
  LOGICAL Winsci_Edit_Dom
  LOGICAL Winsci_Edit_Tim
  LOGICAL Winsci_Edit_Met
  LOGICAL Winsci_Edit_Mat
  LOGICAL Winsci_Edit_Rel
  LOGICAL Winsci_Edit_Opt
  LOGICAL LastMap
  LOGICAL Play
  LOGICAL PlayBack
  LOGICAL AnimDel
  LOGICAL project_setup
  LOGICAL UTM_init
  LOGICAL Welcome
  LOGICAL PopData


  INTEGER(POINTER_LEN) hwnd_list(2,MAX_DLEV)

  TYPE( ProjectStructure ) project(-1:2)

! Toolbox Common

  LOGICAL   ltool,lsysNT,lVGA,lrVGA

! Plot Window Common

  INTEGER isave_format,nsavext,isavtab,isaveis,isavarc
  INTEGER isavasc,isavavs,isavbmp,isavwmf,isavcts,isavsci,isavoil
  INTEGER isavbas,isavusa
  INTEGER iAbort
  LOGICAL lpaintit,lflashit,lplotOK,lprintOK,SyncMode
  CHARACTER(24) savext(20,2)

! Stats Window Common

! Dialog Box Common

  INTEGER    last_edit   !,hwnd_save not used?
  INTEGER    ichoice(10,MAX_DLEV),nradio(10,MAX_DLEV)
  INTEGER    nlst2(MAX_DLEV),nlst(MAX_DLEV)
  INTEGER    dbint(20,MAX_DLEV),idbcmbo(15,MAX_DLEV)
  INTEGER    DefinedOK,DefinedSaved
  CHARACTER(PATH_MAXLENGTH) dbtext(20,MAX_DLEV)
  CHARACTER(PATH_MAXLENGTH) dbcmbo(15,MAX_DLEV)
  CHARACTER(PATH_MAXLENGTH)  DialogName(50)
  CHARACTER(1)   cnull
  REAL         dbreal(30,MAX_DLEV)
  REAL(8)      dbreal8(10,MAX_DLEV)
  LOGICAL      lcheck(20,MAX_DLEV),ldestroy
  LOGICAL      lok_create,lokbutton
  LOGICAL      lDontMove

  TYPE( LIST_STR ) listedt

! Extra Common

  CHARACTER(128)  ext_hook

  INTEGER ran_seed

  INTEGER islicg(2)

  LOGICAL lprj_hook,lsave_hook,lnew_hook,lanim_hook

  LOGICAL lPrintIt
  INTEGER(POINTER_LEN) hdcp   !,IDTimer not used ?
  REAL    map_scale
  LOGICAL lpplot,lpstat,lpclass,lpdate,lpmtime,lpbare
  INTEGER nPrintTimes,nxytab,maxList
  REAL,ALLOCATABLE,DIMENSION(:,:) :: xytab
  REAL,ALLOCATABLE,DIMENSION(:,:) :: dblst
  INTEGER,ALLOCATABLE,DIMENSION(:) :: PrintTindx

  INTEGER,PARAMETER :: MAX_TEST = 100



  INTEGER releaseEnabled

END MODULE pcscipuf_fi
