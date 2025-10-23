MODULE GUItool_fi

  USE SwinAPI, ONLY: POINTER_LEN
  USE create_fd
  USE release_gui_fd
  USE project_fd
  USE tooluser_fd    !, HIDE_TRUE => TRUE, HIDE_FALSE => FALSE
  USE GUImatl_fd
  USE GUIparam_fd
  USE SCIPtool

  SAVE

  TYPE( limitT     ) toolLimits
  TYPE( pctrlT     ) ctrl
  TYPE( pflagsT    ) flags
  TYPE( poptionsT  ) prjOptions
  TYPE( ptemporalT ) time
  TYPE( pspatialT  ) domain
  TYPE( pweatherT  ) weather
  TYPE( pmaterialT ) matdef
  TYPE( preleaseT  ) reldef
  TYPE( pinputT    ) input
  TYPE( createRstT ) createRst
  TYPE( createNewT ) createNew
  TYPE( projectT   ) loadPrj
  TYPE( materialT  ),DIMENSION(:),ALLOCATABLE :: mtlList
  TYPE( releaseT   ),DIMENSION(:),ALLOCATABLE :: relList
  TYPE( releaseMCT ),DIMENSION(:),ALLOCATABLE :: relMCList
  TYPE( GUI_dryFrac ),DIMENSION(:),ALLOCATABLE :: dryFrac
  TYPE( pterrainHeadT ) loadTer

  INTEGER(POINTER_LEN) ToolCallBackAddress
  INTEGER ToolCallerID
  INTEGER PlotCallerID

END MODULE GUItool_fi
