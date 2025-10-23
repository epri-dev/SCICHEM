!***********************************************************************
!               PrintIt
!***********************************************************************
SUBROUTINE PrintIt( hwnd )

USE resource_fd
USE pcscipuf_fi
USE printdev_fi
USE errorParam_fd
USE plotdlg_fi
USE dialog_fi
USE files_fi
USE script_fi
USE pltchoice_fi
USE guiAPI
USE SwinAPI, ONLY : ADDRESSOF

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Window Handle

LOGICAL lok
CHARACTER(128) cttl,cset

INTEGER ierr,ii,itst,ilev,irv,indx
INTEGER(POINTER_LEN) iwnd, inull

LOGICAL, EXTERNAL :: hasError

!---- Initialize PRINT Common Dialog Box Structure
!----   Set to Return Defaults Only

IF( pd%hDevMode == 0 )THEN
  CALL InitPrint( hwnd,pd,lok )
  IF( .NOT.lok )GOTO 9999
ELSE
  pd%hwndOwner = hwnd
END IF

IF( .NOT.(BTEST(pcscipuf_mode,IPMB_INTERACTIVE)) )THEN

  lPrintIt = .TRUE.
  hdcp = pd%hDC
  ii   = SetAbortProc( hdcp,ADDRESSOF(ABORTPROC) )
  CALL PrintPlot(hwnd_pw,pd%nCopies)
  lPrintIt = .FALSE.

ELSE

!------ Re-initialize PRINT Common Dialog Box Structure to do the Print

  cttl = 'PRINTFILE'//CHAR(0)
  cset = 'PRINTSETUP'//CHAR(0)

  pd%hDC                  = 0
  pd%Flags                = IOR(PD_RETURNDC &
                           ,IOR(PD_ENABLEPRINTTEMPLATE &
                           ,IOR(PD_ENABLESETUPTEMPLATE &
                           ,IOR(PD_ENABLEPRINTHOOK &
                           ,IOR(PD_ENABLESETUPHOOK,PD_NOSELECTION)))))
  pd%hInstance            = MyAppInst
  pd%lpPrintTemplateName  = ADDRESSOF(cttl)
  pd%lpSetupTemplateName  = ADDRESSOF(cset)
  pd%lpfnPrintHook        = ADDRESSOF(PrintHook)
  pd%lpfnSetupHook        = ADDRESSOF(PrintHook)

!------ PRINT Common Dialog Box

!        write(lun_dbg,*)'********** PD in  ************'
!        call writepd(pd)
!        write(lun_dbg,*)'******************************'

  ierr = PrintDlg( pd )
  lPrintIt = (ierr /= FALSE)

!        write(lun_dbg,*)'********** PD out ************'
!        call writepd(pd)
!        write(lun_dbg,*)'******************************'

!------ Check for SUCCESS

  IF( .NOT.lPrintIt )THEN
    ierr = CommDlgExtendedError()
    IF( ierr /= 0 )THEN
      WRITE(string1,200)ierr,CHAR(0)
200   FORMAT('PrintDlg Error : ',i4.4,a)
      CALL SetError( API_ERROR,'API Error',TRIM(string1),' ','PrintIt' )
      GOTO 9998
    END IF
  ELSE

!-------- On SUCCESS

!-------- Prepare to PRINT

!-------- Save Device Context

    hdcp = pd%hDC

!-------- Register ABORT Procedure

    ii   = SetAbortProc( hdcp,ADDRESSOF(ABORTPROC) )

!-------- Prepare ABORT Dialog Box

    cttl = 'CancelPrint'//CHAR(0)
    inull = 0
    hdlgp = CreateDialogParam( MyAppInst,cttl,hwnd_mw,ADDRESSOF(ABORTDLG),inull )

    CALL FindHwndListId( IDB_PLOT,iwnd,ilev )

    IF( lpmtime )THEN

      itst = 1
      DO WHILE( itst <= nPrintTimes .AND. lPrintIt )

        idbcmbo(4,ilev) = PrintTindx(itst)
        CALL format_time( timePlot(idbcmbo(4,ilev))%time%runTime ,string1,0 )
        dbcmbo(4,ilev) = string1
        CALL SetListSelString( iwnd,TIME_COMBO,string1,irv ) !  Select this time in COMBO BOX
        lcheck(10,ilev) = .TRUE. !lread
        lcheck( 9,ilev) = .TRUE. !lslice
        CALL GetListSel( iwnd,TIME_COMBO,1,ii,irv )
        CALL GetListData( iwnd,TIME_COMBO,ii,indx,irv )
        PlotDef(EDIT_LEVEL)%Field%TimeID = indx
        CALL save_dialog( iwnd,IDB_PLOT,ilev ) !Save parameters

!-------- Show what's being Printed in Dialog Box

        CALL SetAbortText( lpplot,lpstat,timePlot(idbcmbo(4,ilev))%time%runTime, &
                                            string1,string2 )
        CALL SetControlText( hdlgp,IDB_STATIC02,string1 )
        CALL SetControlText( hdlgp,IDB_STATIC03,string2 )

!-------- Do the PLOT

        CALL PrintPlot( hwnd_pw,pd%nCopies )

        itst = itst + 1

      END DO

    ELSE

!-------- Show what's being Printed in Dialog Box

      CALL SetAbortText( lpplot,lpstat,-999.,string1,string2 )
      CALL SetControlText( hdlgp,IDB_STATIC02,string1 )
      CALL SetControlText( hdlgp,IDB_STATIC03,string2 )

!-------- Do the PLOT

      CALL PrintPlot( hwnd_pw,pd%nCopies )

    END IF

!-------- Finished - Destroy Dialog Box if its still there

    IF( IsWindow(hdlgp) /= FALSE )THEN
      irv = DestroyWindow( hdlgp )
      hdlgp = 0
    END IF

!-------- Set Printing Fflag

    lPrintIt = .FALSE.
!          if(lpmtime)call CallButton(iwnd,IDB_BUTTON10,irv)              !DRAW button
    CALL CallButton( iwnd,IDB_BUTTON10,irv ) !DRAW button
    lpmtime = .FALSE.

  END IF

END IF

!---- Clean Up

9998  CONTINUE

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  irv = DeleteDC( pd%hDC ) !Delete Device Context
  pd%hDC = 0
END IF

hdcp   = 0 !Clear Print DC pointer
IF( hasError() )THEN
  CALL ShowErrorMessage( hwnd )
END IF
9999  RETURN
END
!*******************************************************************************
!                PrintPlot
!*******************************************************************************
SUBROUTINE PrintPlot( hwnd,nCopies )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI
USE testprt

!     This routine creates the Print Job and does the printing

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Window Handle
INTEGER*2 nCopies !number of copies

TYPE( T_DOCINFO ) di

LOGICAL lprt,lavs,lcts,lovl,lmap
INTEGER jerror,ierr,icopy,irv
REAL    yoff

LOGICAL, EXTERNAL :: hasError

CHARACTER(64) eRoutine

!---- Check Printing Flag

IF( .NOT.lPrintIt )GOTO 9997

!---- Set Document Parameters

string1        = 'George'//CHAR(0)
di%cbSize      = SIZEOF(di)
di%lpszDocName = ADDRESSOF(string1)
di%lpszOutput  = 0

!---- Start the Document

jerror = StartDoc( hdcp,di )
IF( jerror == SP_ERROR )THEN
  ierr = GetLastError()
  eRoutine  = 'StartDoc'
  GOTO 9999
END IF

!---- Check Printing Flag

IF( .NOT.lPrintIt )GOTO 9997

!---- Set the pages and loop over all pages

CALL SetPages()
IF( .NOT.lPrintIt )GOTO 9997

DO icopy  = 1,nCopies
  DO ixpage = 1,nxpage
    DO iypage = 1,nypage

!------ Start the Page

      jerror = StartPage( hdcp )
      IF( jerror <= 0 )THEN
        ierr = GetLastError()
        eRoutine  = 'StartPage'
        GOTO 9999
      END IF

!------ Check Printing Flag

      IF( .NOT.lPrintIt )GOTO 9997

!------ Go Draw the Plot/Stats

      IF( project(BASE_LEVEL)%Plot )THEN

!-------- Set NCAR Parameters

        CALL SetNCARDC( hdcp,hdcp )
        CALL SetMapNCAR( 0 )

!-------- Draw PLOT

        IF( lpplot .AND. lplotOK )THEN
          lprt = .TRUE.
          lavs = .FALSE.
          lcts = .FALSE.
          lovl = .FALSE.
          lmap = .FALSE.
          string1 = ' '
          CALL plottri( lprt,lavs,lcts,lovl,lmap,string1 )
          yoff = 1.
        ELSE
          yoff = 0.
        END IF

!-------- Draw STATISTICS

        IF( lpstat .AND. ixpage == 1 .AND. iypage == 1 )THEN
          lprt = .TRUE.
          CALL plotstat( yoff,lprt )
        END IF

!-------- Draw OUTLINE

        CALL outline_rect( hdcp,lpplot,lpstat,lpdate,lpclass, &
                          lpbare,project(BASE_LEVEL)%audit%Classification )
        IF( hasError() )GOTO 9998

      END IF

!------ If Print has not been aborted - Finish up

      IF( lPrintIt )THEN

!-------- End the Page

        jerror = EndPage( hdcp )
        IF( jerror <= 0 )THEN
          ierr = GetLastError()
          eRoutine  = 'EndPage'
          GOTO 9999
        END IF

      END IF

!------ Check Printing Flag

      IF( .NOT.lPrintIt )GOTO 9997

    END DO
  END DO
END DO

!---- End the Document

IF( lPrintIt )THEN
  jerror = EndDoc( hdcp )
  IF( jerror == 0 )THEN
    ierr = GetLastError()
    eRoutine  = 'EndDoc'
    GOTO 9999
  END IF
END IF

!---- Check Printing Flag

IF( .NOT.lPrintIt )GOTO 9997

!---- Done

RETURN

!---- Error Section ------------------------------------------------------------

9999 CONTINUE

WRITE(string1,'(a,i6)')'Routine='//TRIM(eRoutine)//', Error=',ierr
CALL SetError( API_ERROR,'API Error encountered',string1,' ','PrintPlot' )

!---- ABORT The Document

9998 CONTINUE

jerror = AbortDoc( hdcp )
IF( jerror == SP_ERROR )THEN
  ierr = GetLastError()
  WRITE(string1,'(a,i6)')'Routine= AbortDoc, Error=',ierr
  CALL SetError( API_ERROR,'API Error encountered',string1,' ','PrintPlot' )
END IF

IF( hdlgp > 0 )irv = DestroyWindow( hdlgp )

lPrintIt = .FALSE.

CALL ShowErrorMessage( hwnd_mw )

RETURN

!---- ABORT Section ------------------------------------------------------------

!---- ABORT the Job

9997 CONTINUE

irv = AbortDoc( hdcp )
IF( hdlgp /= 0 )THEN
  irv = DestroyWindow( hdlgp )
  hdlgp = 0
END IF

!---- Post ABORT Message

IF( iAbort == 1 )THEN
  CALL SetError( IV_ERROR, &
                 'Aborting print : Invalid map scaling', &
                 'Map too large to fit on page', &
                 'Select a larger map scale factor', &
                 'MapScale' )
ELSE
  CALL SetError( UK_ERROR, &
                 'Aborting print', &
                 'User selected abort', &
                 ' ', &
                 'AbortPrintJob' )
END IF

CALL ShowErrorMessage( hwnd_pw )

RETURN
END
!*******************************************************************************
!                CopyDev
!*******************************************************************************
SUBROUTINE copydev( Dev1,Name1,Dev2,Name2 )

USE SwinAPI

IMPLICIT NONE

TYPE( T_DEVMODE  ) Dev1,Dev2
TYPE( T_DEVNAMES ) Name1,Name2

Dev2  = Dev1
Name2 = Name1

RETURN
END
!*******************************************************************************
!                WritePd
!*******************************************************************************
SUBROUTINE writepd( pd )

USE files_fi
USE SwinAPI

IMPLICIT NONE

TYPE( T_PRINTDLG ) pd

RETURN
END
!*******************************************************************************
!                WriteDev
!*******************************************************************************
SUBROUTINE writedev( Dev1,Name1 )

USE files_fi
USE SwinAPI

IMPLICIT NONE

TYPE( T_DEVMODE  ) Dev1
TYPE( T_DEVNAMES ) Name1

!write(lun_dbg,'(32a1)') &
!(Name1%String(i),i=Name1%wDriverOffset-8+1,Name1%wDeviceOffset-8)
!write(lun_dbg,'(32a1)') &
!(Name1%String(i),i=Name1%wDeviceOffset-8+1,Name1%wOutputOffset-8)
!write(lun_dbg,'(32a1)') &
!(Name1%String(i),i=Name1%wOutputOffset-8+1,64)

RETURN
END
!*******************************************************************************
!                OutlineRect
!*******************************************************************************
SUBROUTINE outline_rect( idc,lplt,lstat,ldate,lclass,lbare,class )

USE resource_fd
USE files_fi
USE SwinAPI
USE testprt

IMPLICIT NONE

INTEGER(POINTER_LEN) idc !Device Context
LOGICAL              lplt !PLOT flag
LOGICAL              lstat !STATISTICS Flag
LOGICAL              ldate !DATE Flag
LOGICAL              lclass !CLASSIFICATION Flag
LOGICAL              lbare !CLASSIFICATION Flag
CHARACTER(*)         class !CLASSIFICATION

INTEGER ihgt,ii,itcol,jtcol
INTEGER ialign,jalign,i,ix,iy,n,iyt,iyb
INTEGER(POINTER_LEN) ipn,ibr,jpn,jbr,kpn,kbr,ifont,jfont,hrgn

CHARACTER(40) string
CHARACTER(24) sysGetDate

TYPE( T_RECT ) Box,Clip
TYPE( T_POINT ) Pt,pts(2)
TYPE( T_SIZE  ) Sz
TYPE( T_POINT ) , POINTER :: pPt

!---- Check flags

IF( .NOT.lplt .AND. .NOT.lstat )RETURN

!---- Get Viewport dimensions

ii = GetViewportOrgEx( idc,Pt )
Box%left = Pt%x
Box%top  = Pt%y

ii = GetViewportExtEx( idc,Sz )
Box%right  = Box%left + Sz%cx
Box%bottom = Box%top  + Sz%cy

!---- Convert to Logical units

pts(1)%y = Box%bottom
pts(1)%x = Box%left
pts(2)%y = Box%top
pts(2)%x = Box%right

ii = DPtoLP( idc,pts(1),2 )

Box%bottom = pts(1)%y
Box%left   = pts(1)%x
Box%top    = pts(2)%y
Box%right  = pts(2)%x

!---- Get Stock Pen,Brush

ipn = GetStockObject( BLACK_PEN )
ibr = GetStockObject( NULL_BRUSH )

!---- Select into Device Context

jpn = SelectObject( idc,ipn )
jbr = SelectObject( idc,ibr )

!---- Set Clip Region - None - See Fix below

!      ii   = SelectClipRgn(idc,(0))

!---- Set Height of PLOT Rectangle

ihgt = Box%bottom - Box%top

!---- Clip Fix - Some combinations of Win32s/Printer seem to have problems
!                clearing a clip region by using a NULL region handle.  Fix
!                is to reset the clip region to the entire area + 5% for
!                Date/Classification

Clip%top    = Box%top - NINT(0.05*FLOAT(ihgt))
Clip%left   = Box%left
Clip%right  = Box%right + NINT(0.5*FLOAT(Box%right-Box%left))
Clip%bottom = Box%bottom + INT((FLOAT(ihgt)*9.75)/7.0) - ihgt + &
                           NINT(0.05*FLOAT(ihgt))
pts(1)%y = Clip%bottom
pts(1)%x = Clip%left
pts(2)%y = Clip%top
pts(2)%x = Clip%right

ii   = LPtoDP( idc,pts(1),2 )

Clip%bottom = pts(1)%y
Clip%left   = pts(1)%x
Clip%top    = pts(2)%y
Clip%right  = pts(2)%x

hrgn = CreateRectRgnIndirect( Clip )
ii   = SelectClipRgn( idc,hrgn )
ii   = DeleteObject( hrgn )

!---- Draw PLOT rectangle

iyt = Box%top

IF( lplt )THEN
  NULLIFY( pPt )
  IF( .NOT.lbare )THEN
    IF( nxpage*nypage == 1 )THEN
      ii = Rectangle( idc,Box%left,Box%top, &
                          Box%right,Box%bottom )
    ELSE
      IF( ixpage == 1 )THEN
        ii = MoveToEx( idc,Box%left,Box%top,pPt )
        ii = LineTo  ( idc,Box%left,Box%bottom )
      END IF
      IF( ixpage == nxpage )THEN
        ii = MoveToEx( idc,Box%right,Box%top,pPt )
        ii = LineTo  ( idc,Box%right,Box%bottom )
      END IF
      IF( iypage == 1 )THEN
        ii = MoveToEx( idc,Box%left,Box%bottom,pPt )
        ii = LineTo  ( idc,Box%right,Box%bottom )
      END IF
      IF( iypage == nypage )THEN
        ii = MoveToEx( idc,Box%left,Box%top,pPt )
        ii = LineTo  ( idc,Box%right,Box%top )
      END IF
    END IF
  END IF
  Box%top = Box%bottom
  iyb = 0
ELSE
  iyb = Box%bottom
END IF

!---- Draw STATISTICS rectangle

Box%bottom = Box%top + INT((FLOAT(ihgt)*9.75)/7.0) - ihgt

IF( lstat .AND. ixpage == 1 .AND. iypage == 1 )THEN
  ii = Rectangle( idc,Box%left,Box%top, &
                      Box%right,Box%bottom )
END IF

!---- Test Text

IF( lclass )THEN

  string = 'ARIAL'
  CALL SetFont( idc,string,300,0,700,ifont,jfont )

  n = LEN(TRIM(class))

  ialign = TA_CENTER .OR. TA_BOTTOM
  jalign = SetTextAlign( idc,ialign )

  ix = (Box%right + Box%left)/2

  iy = iyt - 5

  CALL setRGBcolor( 17,itcol )
  jtcol = SetTextColor( idc,itcol )

  i = TextOut( idc,ix,iy,class,n )

  ialign = TA_CENTER .OR. TA_TOP
  i      = SetTextAlign( idc,ialign )

  iy = iyb + Box%bottom + 5
  i  = TextOut( idc,ix,iy,class,n )

  CALL RestoreFont( idc,ifont,jfont )
  ialign = SetTextAlign( idc,jalign )
  itcol  = SetTextColor( idc,jtcol )

END IF

!---- Date

IF( ldate .AND. ixpage == nxpage .AND. iypage == nypage )THEN

  string = 'ARIAL'
  CALL SetFont( idc,string,75,-900,400,ifont,jfont )

  string = sysGetDate()
  n = LEN(TRIM(string))

  ialign = TA_LEFT .OR. TA_BOTTOM
  jalign = SetTextAlign( idc,ialign )

  ix = Box%right + 2
  iy = iyt

  i = TextOut( idc,ix,iy,string,n )

  CALL RestoreFont( idc,ifont,jfont )
  ialign = SetTextAlign( idc,jalign )

END IF

!---- Clean up

kpn = SelectObject( idc,jpn ) !Return previous pen
kbr = SelectObject( idc,jbr ) !Return previous brush
kpn = DeleteObject( ipn )     !Delete Pen
kbr = DeleteObject( ibr )     !Delete brush

RETURN
END
!*******************************************************************************
!                CheckAbort
!*******************************************************************************
SUBROUTINE check_abort()

USE pcscipuf_fi
USE SwinAPI

!---- Check ABORT Dialog Box Messages

IMPLICIT NONE

TYPE( T_MSG) Message
INTEGER irv

IF( IsWindow(hdlgp) /= FALSE )THEN !Abort Print

  DO WHILE( PeekMessage(Message,hdlgp,NULL,NULL,1) /= FALSE )
    IF( IsDialogMessage(hdlgp,Message) == FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO

ELSE IF( IsWindow(hwnd_pw) /= FALSE )THEN !Abort Draw (CTRL-C)

  DO WHILE( PeekMessage(Message,hwnd_pw,WM_KEYFIRST, &
    WM_KEYLAST,1) /= FALSE)
    IF( TranslateAccelerator(hwnd_pw,hacc_pd,Message) == FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO

END IF

RETURN
END
!***********************************************************************
!                cancel_print
!***********************************************************************
LOGICAL FUNCTION cancel_print( lprint )

USE pcscipuf_fi
USE animate_fi
USE plotcan

!---- Sets/Checks Printing/Plotting Flag - Called by PLOTTRI routines

IMPLICIT NONE

LOGICAL lprint

CALL check_abort()

IF( lprint )THEN

  cancel_print = .NOT.lPrintIt

ELSE

  cancel_print = .NOT.lPlotIt
  IF( cancel_print .AND. lanimate )CALL HaltScipuff( 0 )

END IF

RETURN
END
!***********************************************************************
!               InitPrint
!***********************************************************************
SUBROUTINE InitPrint( hwnd,pd,lok )

USE resource_fd
USE pcscipuf_fi
USE errorParam_fd
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd !Window Handle
LOGICAL lok

TYPE( T_PRINTDLG) pd

INTEGER ierr

pd%lStructSize          = SIZEOF(pd)
pd%hwndOwner            = hwnd
pd%hDevMode             = 0
pd%hDevNames            = 0
pd%hDC                  = 0
pd%Flags                = IOR(PD_RETURNDEFAULT,PD_RETURNDC)
pd%nFromPage            = 1
pd%nToPage              = 1
pd%nMinPage             = 0
pd%nMaxPage             = 0
pd%nCopies              = 1
pd%hInstance            = 0
pd%lCustData            = 0
pd%lpfnPrintHook        = 0
pd%lpfnSetupHook        = 0
pd%lpPrintTemplateName  = 0
pd%lpSetupTemplateName  = 0
pd%hPrintTemplate       = 0
pd%hSetupTemplate       = 0

lok = PrintDlg(pd)

IF( .NOT.lok .OR. pd%hDevMode == 0 )THEN
  ierr = CommDlgExtendedError()
  IF( ierr /= 0 )THEN
    WRITE(string1,100)ierr,CHAR(0)
100 FORMAT('PrintDlg Error : ',I4.4,A)
  ELSE
    string1 = 'PrintDlg Error : Unknown'//CHAR(0)
  END IF
  CALL SetError( API_ERROR, &
                 'API Error', &
                 TRIM(string1),' ', &
                 'InitPrint' )
  CALL ShowErrorMessage( hwnd )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!              DestroyPrint
!***********************************************************************
SUBROUTINE DestroyPrint()

USE resource_fd
USE printdev_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) i

IF( pd%hDevMode /= 0 )THEN
  i = GlobalFree( pd%hDevMode ) !Release Memory
  pd%hDevMode = 0
END IF

IF( pd%hDevNames /= 0 )THEN
  i = GlobalFree( pd%hDevNames ) !Release Memory
  pd%hDevNames = 0
END IF

RETURN
END
!***********************************************************************
!              SetAbortText
!***********************************************************************
SUBROUTINE SetAbortText( lpplot,lpstat,time,line1,line2 )

IMPLICIT NONE

LOGICAL   lpplot,lpstat
REAL      time

CHARACTER(*) line1,line2

CHARACTER(32) linep,linet

!---- Initialize

line2 = ' '
IF( time < 0. )THEN
  linep = 'the current plot'
ELSE
  CALL format_time( time,linet,0 )
  linep = 'Time='//TRIM(linet)
END IF

!---- Both PLOT and STATISTICS

IF( lpplot .AND. lpstat )THEN

  line1 = 'both '//TRIM(linep)
  line2 = 'and the project summary'

!---- STATISTICS Only

ELSE IF( lpstat )THEN

  line1 = 'the project summary'

!---- PLOT Only

ELSE IF( lpplot )THEN

  line1 = TRIM(linep)

!---- Just in case

ELSE

  line1 = 'unknown situation'

END IF

RETURN
END
