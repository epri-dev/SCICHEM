MODULE NCAR

  INTERFACE

!==============================================================================
! checkpalette
!==============================================================================
    SUBROUTINE checkpalette
    END SUBROUTINE checkpalette

!==============================================================================
! checkpoly
!==============================================================================
    SUBROUTINE checkpoly ( nPts, X, Y )
      INTEGER nPts
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y
    END SUBROUTINE checkpoly

!==============================================================================
! ClearBrush
!==============================================================================
    SUBROUTINE ClearBrush
    END SUBROUTINE ClearBrush

!==============================================================================
! ClearPen
!==============================================================================
    SUBROUTINE ClearPen
    END SUBROUTINE ClearPen

!==============================================================================
! ExitNCARGraphics
!==============================================================================
    SUBROUTINE ExitNCARGraphics
    END SUBROUTINE ExitNCARGraphics

!==============================================================================
! gaa
!==============================================================================
    SUBROUTINE gaa( xOrg, yOrg, xRad, yRad, aStart, aEnd )
      REAL xOrg
      REAL yOrg
      REAL xRad
      REAL yRad
      REAL aStart
      REAL aEnd
    END SUBROUTINE gaa

!==============================================================================
! gellips
!==============================================================================
    SUBROUTINE gellips( X, Y )
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y
    END SUBROUTINE gellips

!==============================================================================
! GetNCARDC
!==============================================================================
    SUBROUTINE GetNCARDC( hDC, hDCget )
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hDC
      INTEGER(POINTER_LEN) hDCget
    END SUBROUTINE GetNCARDC

!==============================================================================
! GetNCARFontSize
!==============================================================================
    SUBROUTINE GetNCARFontSize(x,y,string,fnt,rot,cntr,sizh,sizv)
      REAL x
      REAL y
      CHARACTER(*) string
      REAL fnt
      REAL rot
      REAL cntr
      REAL sizv
      REAL sizh
    END SUBROUTINE GetNCARFontSize

!==============================================================================
! GetNCARPalette
!==============================================================================
    SUBROUTINE GetNCARPalette( hPalette )
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hPalette
    END SUBROUTINE GetNCARPalette

!==============================================================================
! GetNCARRates
!==============================================================================
    SUBROUTINE GetNCARRates(xRat,yRat)
      REAL xRat
      REAL yRat
    END SUBROUTINE GetNCARRates

!==============================================================================
! GetNCARResolution
!==============================================================================
    SUBROUTINE GetNCARResolution( nRes )
      INTEGER nRes
    END SUBROUTINE GetNCARResolution

!==============================================================================
! GetNCARTransformX
!==============================================================================
    SUBROUTINE GetNCARTransformX(xo,xe,xrat,ixo)
      REAL xo
      REAL xe
      REAL xrat
      INTEGER ixo
    END SUBROUTINE GetNCARTransformX

!==============================================================================
! GetNCARTransformY
!==============================================================================
    SUBROUTINE GetNCARTransformY(yo,ye,yrat,iyo)
      REAL yo
      REAL ye
      REAL yrat
      INTEGER iyo
    END SUBROUTINE GetNCARTransformY

!==============================================================================
! GetNCARWindow
!==============================================================================
    SUBROUTINE GetNCARWindow( hWindow )
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hWindow
    END SUBROUTINE GetNCARWindow

!==============================================================================
! gfa
!==============================================================================
    SUBROUTINE gfa(nPts, X, Y)
      INTEGER nPts
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y
    END SUBROUTINE gfa

!==============================================================================
! gfellips
!==============================================================================
    SUBROUTINE gfellips(X,Y)
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y
    END SUBROUTINE gfellips

!==============================================================================
! gpl
!==============================================================================
    SUBROUTINE gpl(nPts,X,Y)
      INTEGER nPts
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y
    END SUBROUTINE gpl

!==============================================================================
! gridal
!==============================================================================
    SUBROUTINE gridal(mtx,ndx,mty,ndy,ilx,ily,fsz)
      INTEGER mtx
      INTEGER ndx
      INTEGER mty
      INTEGER ndy
      INTEGER ilx
      INTEGER ily
      REAL fsz
    END SUBROUTINE gridal

!==============================================================================
! gscr
!==============================================================================
    SUBROUTINE gscr(indx,r,g,b,lerr)
      INTEGER indx
      REAL r
      REAL g
      REAL b
      LOGICAL   lerr
    END SUBROUTINE gscr

!==============================================================================
! gsfaci
!==============================================================================
    SUBROUTINE gsfaci(iColor)
      INTEGER iColor
    END SUBROUTINE gsfaci

!==============================================================================
! gsfais
!==============================================================================
    SUBROUTINE gsfais(iStyle)
      INTEGER iStyle
    END SUBROUTINE gsfais

!==============================================================================
! gsln
!==============================================================================
    SUBROUTINE gsln(iStyle)
      INTEGER iStyle
    END SUBROUTINE gsln

!==============================================================================
! gslwsc
!==============================================================================
    SUBROUTINE gslwsc(width)
      REAL width
    END SUBROUTINE gslwsc

!==============================================================================
! gsplci
!==============================================================================
    SUBROUTINE gsplci(iColor)
      INTEGER iColor
    END SUBROUTINE gsplci

!==============================================================================
! gstbci
!==============================================================================
    SUBROUTINE gstbci(iColor)
      INTEGER iColor
    END SUBROUTINE gstbci

!==============================================================================
! gstxci
!==============================================================================
    SUBROUTINE gstxci(iColor)
      INTEGER iColor
    END SUBROUTINE gstxci

!==============================================================================
! gstxmd
!==============================================================================
    SUBROUTINE gstxmd(iStyle)
      INTEGER iStyle
    END SUBROUTINE gstxmd

!==============================================================================
! gstxst
!==============================================================================
    SUBROUTINE gstxst(iStyle)
      INTEGER iStyle
    END SUBROUTINE gstxst

!==============================================================================
! InitNCARGraphics
!==============================================================================
    SUBROUTINE InitNCARGraphics( hInstance )
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hInstance
    END SUBROUTINE InitNCARGraphics

!==============================================================================
! labmod
!==============================================================================
    SUBROUTINE labmod(cfx,cfy)
      CHARACTER(*) cfx
      CHARACTER(*) cfy
    END SUBROUTINE labmod

!==============================================================================
! NCARPrint
!==============================================================================
    LOGICAL FUNCTION NCARPrint()
    END FUNCTION NCARPrint

!==============================================================================
! NCARTransform
!==============================================================================
    SUBROUTINE NCARTransform(x,y,ix,iy)
      REAL x
      REAL y
      INTEGER ix
      INTEGER iy
    END SUBROUTINE NCARTransform

!==============================================================================
! NCARTransformI
!==============================================================================
    SUBROUTINE NCARTransformI(ix,iy,x,y)
      INTEGER ix
      INTEGER iy
      REAL x
      REAL y
    END SUBROUTINE NCARTransformI

!==============================================================================
! plchhq
!==============================================================================
    SUBROUTINE plchhq(xx,yy,clab,fsz,rot,cntr)
      REAL xx
      REAL yy
      CHARACTER(*) clab
      REAL fsz
      REAL rot
      REAL cntr
    END SUBROUTINE plchhq

!==============================================================================
! RestoreFont
!==============================================================================
    SUBROUTINE RestoreFont(hDC,hFontNew,hFontOld)
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hDC
      INTEGER(POINTER_LEN) hFontNew
      INTEGER(POINTER_LEN) hFontOld
    END SUBROUTINE RestoreFont

!==============================================================================
! set
!==============================================================================
    SUBROUTINE set(pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,iflg)
      REAL pxmn
      REAL pxmx
      REAL pymn
      REAL pymx
      REAL xmnd
      REAL xmxd
      REAL ymnd
      REAL ymxd
      INTEGER iflg
    END SUBROUTINE set

!==============================================================================
! SetBrush
!==============================================================================
    SUBROUTINE SetBrush
    END SUBROUTINE SetBrush

!==============================================================================
! SetFont
!==============================================================================
    SUBROUTINE SetFont(hDC,font,ifsz,irot,iwgt,inew,iold)
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hDC
      CHARACTER(*)font
      INTEGER ifsz
      INTEGER irot
      INTEGER iwgt
      INTEGER(POINTER_LEN) inew
      INTEGER(POINTER_LEN) iold
    END SUBROUTINE SetFont

!==============================================================================
! SetMapNCAR
!==============================================================================
    SUBROUTINE SetMapNCAR(iflag)
      INTEGER iflag
    END SUBROUTINE SetMapNCAR

!==============================================================================
! SetNCARDC
!==============================================================================
    SUBROUTINE SetNCARDC(hDC,hDCget)
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hDC
      INTEGER(POINTER_LEN) hDCget
    END SUBROUTINE SetNCARDC

!==============================================================================
! SetNCARFont
!==============================================================================
    SUBROUTINE SetNCARFont(font)
      CHARACTER(*) font
    END SUBROUTINE SetNCARFont

!==============================================================================
! SetNCARPrint
!==============================================================================
    SUBROUTINE SetNCARPrint(lPrint)
      LOGICAL lPrint
    END SUBROUTINE SetNCARPrint

!==============================================================================
! SetNCARWindow
!==============================================================================
    SUBROUTINE SetNCARWindow( hWindow )
      USE winAPI_fd, ONLY: POINTER_LEN
      INTEGER(POINTER_LEN) hWindow
    END SUBROUTINE SetNCARWindow

!==============================================================================
! SetPen
!==============================================================================
    SUBROUTINE SetPen
    END SUBROUTINE SetPen

!==============================================================================
! setRGBcolor
!==============================================================================
    SUBROUTINE setRGBcolor(index,iColor)
      INTEGER index
      INTEGER iColor
    END SUBROUTINE setRGBcolor

  END INTERFACE

END MODULE NCAR
