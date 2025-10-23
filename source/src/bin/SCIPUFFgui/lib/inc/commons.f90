MODULE testprt
  INTEGER ixpage,iypage,nxpage,nypage
END MODULE testprt

MODULE MySync
  USE SwinAPI, ONLY : T_RECT
  TYPE( T_RECT)  dbBox,pbBox,pwBox,mwBox
  INTEGER        nSync,iSync,pbFlag
  REAL           SyncTime
END MODULE MySync

MODULE MyClock
  USE SwinAPI, ONLY : POINTER_LEN
  INTEGER Ncur,Icur
  INTEGER(POINTER_LEN) Hcur(8)
  LOGICAL ClockOn
END MODULE MyClock

MODULE MyEffect
  INTEGER nWpn
  INTEGER lastID
END MODULE MyEffect

MODULE cscrollbar
  USE GUIstruct_fd, ONLY : CMD
  INTEGER ISCROLL
  INTEGER IVALUE
  LOGICAL lcommand
  INTEGER icommand
  TYPE( CMD ) OldCmd
END MODULE cscrollbar

MODULE mapwrap
  REAL wmin,wmax,delw
END MODULE mapwrap

MODULE map_INDEX
  INTEGER rec_size(7)
  INTEGER INDEX_maps(0:36,0:18,7,4)
  LOGICAL init_quad(4)
END MODULE map_INDEX

MODULE contri_D
  REAL Dscale
END MODULE contri_D

MODULE openfix
  LOGICAL lOpening
END MODULE openfix

MODULE mainbeta
  INTEGER beta_flag
END MODULE mainbeta

MODULE cDSWAlogo
  USE SwinAPI, ONLY : T_POINT, POINTER_LEN
  TYPE( T_POINT ) szDSWA
  INTEGER(POINTER_LEN) DSWAbmp,DSWApal
END MODULE cDSWAlogo

MODULE liqdef
  INTEGER save_flag
END MODULE liqdef

MODULE cfocus
  USE SwinAPI, ONLY : POINTER_LEN
  INTEGER(POINTER_LEN) ifocus
END MODULE cfocus

MODULE cfocus_pb
  USE SwinAPI, ONLY : POINTER_LEN
  INTEGER(POINTER_LEN) ifocus_pb
END MODULE cfocus_pb

MODULE plotcan
  LOGICAL lPlotIt
END MODULE plotcan

MODULE multicmn
  INTEGER iEdit, nEdit
  LOGICAL mEdit
END MODULE multicmn

MODULE xytabcmn
  LOGICAL xytab_edit
  LOGICAL xytab_grid
  INTEGER xytab_sel
END MODULE xytabcmn
