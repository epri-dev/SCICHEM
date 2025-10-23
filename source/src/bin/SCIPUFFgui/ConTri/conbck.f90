SUBROUTINE draw_back( lno_plot )

USE contri_fi
USE pltchoice_fi
USE PlotTrans_fi
USE GUIparam_fd
USE guitool_fi
USE testprt

IMPLICIT NONE

LOGICAL lno_plot

INTEGER,PARAMETER :: MAXPT = 101

REAL,DIMENSION(:),ALLOCATABLE :: xt,yt
REAL,DIMENSION(:),ALLOCATABLE :: zt

INTEGER irv,ios

REAL, DIMENSION(5) :: xp, yp

REAL xlfac,xcfac,frac,xevb,yevb,xovb,yovb
REAL dy,yy,yyy,dyy,xx,xxx,dx,dxx,xhgt,xlen,xxlen,xdum,ydum
REAL xin,tdel(3),xkmb,round,places,sckm
REAL scaling

INTEGER mtx,mty,ilx,ily,nch,nchx,ncv,nct,n,nn,i,minc,icolor,ncc
INTEGER index_contour,nchu,ncx,MapScaleI
INTEGER kmbLP,ixp(4),kmbx(3),j
INTEGER UserID
INTEGER(POINTER_LEN) idc,jdc,tmpMode

CHARACTER(80) cfmt,ctmp,ctmp2,ctmpx

TYPE( SCIPContourElementList ) :: ContourSlice
TYPE( SCIPPointT ), DIMENSION(:), ALLOCATABLE :: Location

INTEGER, EXTERNAL :: CheckFill,CheckDraw

CALL gsplci( 1 )

scaling = ContourPlot%ListHdr%Scale

UserID = 6789

!---- Set Axes and label spacings

IF( ntx <= 0 )THEN
  ilx = 0
ELSE
  ilx = 1
END IF
xlfac = 1.2
xcfac = 2.5

IF( ilx > 0 )THEN

!---- Horizontal Plot

  IF( .NOT.lvl )THEN
    xlfac = xlfac + 1.2
    xcfac = xcfac + 1.2

!---- Vertical Plot

  ELSE
    frac = (xmxd-xminp)/(FLOAT(m0)*dxsrf)
    xevb = xov + frac*(xev-xov)
    yevb = yov + frac*(yev-yov)
    frac = (xmnd-xminp)/(FLOAT(m0)*dxsrf)
    xovb = xov + frac*(xev-xov)
    yovb = yov + frac*(yev-yov)
    dxx = (xevb-xovb)
    dyy = (yevb-yovb)
    IF( ABS(dxx) > 1.E-4 )THEN
      xlfac = xlfac + 1.2
      xcfac = xcfac + 1.2
    END IF
    IF( ABS(dyy) > 1.E-4 )THEN
      IF( ABS(dxx) > 1.E-4 )THEN
        xlfac = xlfac + 1.0
        xcfac = xcfac + 1.0
      ELSE
        xlfac = xlfac + 1.2
        xcfac = xcfac + 1.2
      END IF
    END IF
  END IF
END IF

!---- Determine Scale bar

IF( lkmb )THEN
  IF( lvl )THEN
    sckm = (xmxd-xmnd)/SQRT((xevb-xovb)**2 + (yevb-yovb)**2)
  ELSE
    sckm = 1.
  END IF
  kmbx(1) = 1
  kmbx(2) = 2
  kmbx(3) = 5
  xp(1)=xmnd
  xp(2)=ymnd
  CALL RealtoLP( xp,ixp,1 )
  xp(4) = xp(2)
  nn    = 100000000
  xkmb  = 0.
  kmbLP = 1000
  DO i = 1,6
    xdum = 10.**(i-3)
    DO j = 1,3
      xp(3)=xp(1) + xdum*FLOAT(kmbx(j))*sckm
      CALL RealtoLP( xp(3),ixp(3),1 )
      xx = xp(3) - xp(1)
      n  = ixp(3) - ixp(1)
      IF( ABS(n-1000) < nn .AND. n > 0 )THEN
        nn    = ABS(n-1000)
        xkmb  = xx/sckm
        kmbLP = n
      END IF
    END DO
  END DO
  DO WHILE( kmbLP < 500 )
    kmbLP = 2*kmbLP
    xkmb  = 2.*xkmb
  END DO
  DO WHILE( kmbLP > 2000 )
    kmbLP = kmbLP/2
    xkmb  = xkmb/2.
  END DO
  IF( xkmb < .01 .OR. xkmb > 10000. )THEN
    lkmb = .FALSE.
  END IF
  xkmb = xkmb/scx
END IF

!---- Reset set Plotting units to normalized 0-1 units

CALL set( 0.,1.,0.,1.,0.,1.,0.,1.,1 )
cfmt = 'Mg'
CALL GetNCARFontSize( 0.,0.,cfmt(1:2),1.,0.,0.,xdum,xhgt )

IF( ixpage == 1 .AND. iypage == nypage )THEN
  CALL GetNCARDC( idc,jdc )
  CALL DisplayIcon( idc )
END IF

IF( lkmb )THEN
  ixp(1) = 1000
  ixp(2) = 1000
  ixp(3) = ixp(1) + kmbLP
  ixp(4) = ixp(2)
  CALL LPtoReal( ixp,xp )
  xx = xp(3)-xp(1)
  CALL gsplci( 1 )
  n = 2
  xp(1) = 0.5*(pxmx+pxmn) - 0.5*xx
  xp(2) = xp(1) + xx
  yp(1) = 0.1*xx
  yp(2) = yp(1)
  CALL gpl( n,xp,yp )
  xp(2) = xp(1)
  yp(1) = .125*xx
  yp(2) = .075*xx
  CALL gpl( n,xp,yp )
  xp(1) = xp(1) + xx
  xp(2) = xp(1)
  CALL gpl( n,xp,yp )
  IF( xkmb < 0.99 )THEN
    WRITE(ctmp2,*)NINT(1000.*xkmb)
    ctmp = TRIM(ADJUSTL(ctmp2))//'m'
  ELSE
    WRITE(ctmp2,*)NINT(xkmb)
    ctmp = TRIM(ADJUSTL(ctmp2))//'km'
  END IF
  n = LEN_TRIM(ctmp)
  yy = .125*xx
  xx = 0.5*(pxmx+pxmn)
  CALL plchhq( xx,yy,ctmp(1:n),0.66*fsz,0.,0. )
END IF

!---- Draw titles

!---- Line 1

IF( lttl )THEN
  IF( yttl == default )THEN
    yy = pymx + 0.5*xhgt*fsz
  ELSE
    yy = MIN(pymn,0.) + yttl*FLOAT(nypage)
  END IF
  IF( xttl == default )THEN
    xx = 0.5*(pxmx + pxmn)
  ELSE
    xx = MIN(pxmn,0.) + xttl*FLOAT(nxpage)
  END IF
  IF( cttl(1:7) == 'default' )THEN
    nch = LEN_TRIM(cfil)
    CALL c_format( tima,ncc,ctmp )
    CALL clower( ctmp(1:ncc) )
    IF( nfnc == 0 )THEN
      ctmp2 = cvar
    ELSE
      SELECT CASE( nfnc )
        CASE( 1 )
          ncv = LEN_TRIM(cvar)
          nct = LEN_TRIM(caux)
          ctmp2 = cvar(1:ncv)//'+'//caux(1:nct)
          nct = nct + ncv + 1

        CASE( 2 )
          ncv = LEN_TRIM(cvar)
          nct = LEN_TRIM(caux)
          ctmp2 = cvar(1:ncv)//'*'//caux(1:nct)
          nct = nct + ncv + 1

        CASE( 3 )
          ncv = LEN_TRIM(cvar)
          nct = LEN_TRIM(caux)
          ctmp2 = cvar(1:ncv)//'/'//caux(1:nct)
          nct = nct + ncv + 1

        CASE( 4 )
          ncv = LEN_TRIM(cvar)
          ctmp2 ='1/'//cvar(1:ncv)
          nct = ncv + 2

        CASE( 5 )
          ncv = LEN_TRIM(cvar)
          ctmp2 ='Sqrt('//cvar(1:ncv)//')'
          nct = ncv + 12

        CASE( 6 )
          ncv = LEN_TRIM(cvar)
          nct = LEN_TRIM(caux)
          ctmp2 ='Sqrt('//cvar(1:ncv)//')/'//caux(1:nct)
          nct = nct + ncv + 13

        CASE( 7 )
          ctmp2 =' '
          CALL c_format(exceed,ncx,ctmpx)
          CALL clower(ctmpx(1:ncx))
          ctmp2 ='P(c>'//TRIM(ctmpx(1:ncx))//')'
          nct = LEN_TRIM(ctmp2)

        CASE( 8 )
          ctmp2 =' '
          CALL c_format(exceed,ncx,ctmpx)
          CALL clower(ctmpx(1:ncx))
          ctmp2 ='P(c>'//TRIM(ctmpx(1:ncx))//')'
          nct = LEN_TRIM(ctmp2)

        CASE( 9 )
          ctmp2 =' '
          CALL c_format(exceed,ncx,ctmpx)
          CALL clower(ctmpx(1:ncx))
          ctmp2 ='P''(c>'//TRIM(ctmpx(1:ncx))//')'
          nct = LEN_TRIM(ctmp2)

      END SELECT
    END IF

    nct = LEN_TRIM(ctmp2)
    CALL ReportFileName( ctmpx,'File=',cfil )
    WRITE(clab,1100) TRIM(ctmpx),ctmp2(1:nct),ctmp(1:ncc)
1100 FORMAT(A,' : Variable=',A,' : Time=',A)
    nchx = LEN_TRIM(clab)
  ELSE
    nchx = LEN_TRIM(cttl)
    clab = cttl
  END IF
  IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),1.2*fsz,0.,0. )
END IF

!---- Line 2

IF( lttu )THEN
  IF( yttu == default )THEN
    yy = pymx + 1.7*xhgt*fsz
  ELSE
    yy = MIN(pymn,0.) + yttu*FLOAT(nypage)
  END IF
  IF( xttu == default )THEN
    xx = 0.5*(pxmx + pxmn)
  ELSE
    xx = MIN(pxmn,0.) + xttu*FLOAT(nxpage)
  END IF
  nchx = LEN_TRIM(cttu)
  clab = cttu
  IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),1.2*fsz,0.,0. )
END IF

!---- Line 3

IF( ltt3 )THEN
  IF( ytt3 == default )THEN
    yy = pymx + 2.9*xhgt*fsz
  ELSE
    yy = MIN(pymn,0.) + ytt3*FLOAT(nypage)
  END IF
  IF( xtt3 == default )THEN
    xx = 0.5*(pxmx + pxmn)
  ELSE
    xx = MIN(pxmn,0.) + xtt3*FLOAT(nxpage)
  END IF
  nchx = LEN_TRIM(ctt3)
  clab = ctt3
  IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),1.2*fsz,0.,0. )
END IF

!---- Write Max value

frac = 1.0
IF( nmax /= 0 .AND. delminc > 0 )frac = 0.5

IF( nmax /= 0 .AND. .NOT.lno_plot )THEN
  CALL c_format( vmax,ncc,ctmp )
  CALL clower( ctmp(1:ncc) )
  clab = 'Maximum value='//ctmp(1:ncc)
  nchx = LEN_TRIM(clab)
  CALL GetNCARFontSize( 0.,0.,clab(1:nchx),0.6*fsz,0.,0.,xdum,ydum )
  IF( xdum > frac*(pymx-pymn) )THEN
    clab ='Max.='//ctmp(1:ncc)
    nchx = LEN_TRIM(clab)
  END IF
  yy = pymx
  xx = pxmx + 0.2*xhgt*(0.6*fsz)
  CALL plchhq( xx,yy,clab(1:nchx),0.6*fsz,-90.,-1. )

!---- Write DELMIN if necessary

  IF( delminc > 0.0 )THEN
    IF( delminc >= 10000. )THEN
      xx = delminc/1000.
    ELSE
      xx = delminc
    END  if
    round = 10.**INT(LOG10(xx))
    IF( round <= 1000. )THEN
      places = 10.
    ELSE
      places = 100.
    END IF
    xx = round*FLOAT(NINT(places*(xx/round)))/places
    CALL c_format( xx,ncc,ctmp )
    CALL clower( ctmp(1:ncc) )
    IF( xx >= 10. .AND. INDEX(ctmp,'e') <= 0 )THEN
      nchx = INDEX(ctmp,'.')
      ncc  = MAX(1,nchx-1)
    END IF
    IF( delminc >= 10000. )THEN
      clab = 'Approx. plot resolution ='//ctmp(1:ncc)//'km'
    ELSE
      clab = 'Approx. plot resolution ='//ctmp(1:ncc)//'m'
    END IF
    nchx = LEN_TRIM(clab)
    CALL GetNCARFontSize( 0.,0.,clab(1:nchx),0.6*fsz,0.,0.,xdum,ydum )
    IF( xdum > frac*(pymx-pymn) )THEN
      IF( delminc >= 10000. )THEN
        clab = 'Plot res='//ctmp(1:ncc)//'km'
      ELSE
        clab = 'Plot res='//ctmp(1:ncc)//'m'
      END IF
      nchx = LEN_TRIM(clab)
    END IF
    yy = pymn
    xx = pxmx + 0.2*xhgt*(0.6*fsz)
    CALL plchhq( xx,yy,clab(1:nchx),0.6*fsz,-90.,1. )
  END IF
END IF

!---- Write axes labels

IF( llab )THEN

!---- X Axis

  IF( llbx )THEN
    yy = pymn - xlfac*xhgt*fsz
    xx = 0.5*(pxmn+pxmx)

    IF( clbx(1:7) == 'default' )THEN
      IF( int_start == 2 )THEN
        clab = 'X'
        nchx = 1
      ELSE
        clab = 'Longitude'
        nchx = 9
      END IF
    ELSE
      IF( lvl )THEN
        frac = (xmxd-xminp)/(FLOAT(m0)*dxsrf)
        xevb = xov + frac*(xev-xov)
        yevb = yov + frac*(yev-yov)
        frac = (xmnd-xminp)/(FLOAT(m0)*dxsrf)
        xovb = xov + frac*(xev-xov)
        yovb = yov + frac*(yev-yov)
        dxx = (xevb-xovb)
        dyy = (yevb-yovb)
        CALL reset_title( xov,yov,dxx,dyy,clbx,cfx,scx,clab,nchx )
      ELSE
        clab = clbx
        nchx = LEN_TRIM(clab)
      END IF
    END IF
    IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),fsz,0.,0. )
  END IF

!---- Y axis

  IF( llby )THEN
    cfmt(1:) = ' '
    WRITE(cfmt,cfy,IOSTAT=ios)ymnd
    IF( ios /= 0 )cfmt = 'Err'
    cfmt = ADJUSTL(cfmt)
    n    = LEN_TRIM(cfmt)
    IF( n > 0 )CALL GetNCARFontSize( 0.,0.,cfmt(1:n),1.,0.,0.,xlen,ydum )
    cfmt(1:) =' '
    WRITE(cfmt,cfy,IOSTAT=ios)ymxd
    IF( ios /= 0 )cfmt= 'Err'
    cfmt = ADJUSTL(cfmt)
    n    = LEN_TRIM(cfmt)
    IF( n > 0 )CALL GetNCARFontSize( 0.,0.,cfmt(1:n),1.,0.,0.,xxlen,ydum )
    xx = pxmn - MAX(xxlen,xlen)*fsz - 0.8*xhgt*fsz
    yy = 0.5*(pymn+pymx)
    IF( clby(1:7) == 'default' )THEN
      IF( int_start == 2 )THEN
        clab = 'Y'
        nchx = 1
      ELSE
        clab = 'Latitude'
        nchx = 8
      END IF
    ELSE
      clab = clby
      nchx = LEN_TRIM(clab)
    END IF
    IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),fsz,90.,0. )
  END IF
END IF

!---- Contour information

IF( lno_plot )THEN
  clab = 'No available data to plot'
  nchx = LEN_TRIM(clab)
  xx = 0.5*(pxmn+pxmx)
  yy = 0.5*(pymn+pymx)
  CALL plchhq( xx,yy,clab(1:nchx),fsz,0.,0. )
END IF

IF( (llbc .AND. .NOT.lno_plot) .AND. &
  ( ( PlotDraw%DrawContour==SCIPtrue .AND. .NOT.(llbb .AND. PlotDraw%FillContour==SCIPtrue) ) .OR. &
    ( PlotDraw%FillContour==SCIPtrue .AND. .NOT.llbb .AND. .NOT.(PlotDraw%DrawContour==SCIPtrue))   ) )THEN
  xx = 0.5*(pxmn+pxmx)
  yy = pymn - xcfac*xhgt*fsz
  xcfac = xcfac + 1.
  IF( lconst )THEN
    CALL c_format( scaling*ContourPlot%ListPtr(1)%Contour,ncc,ctmp )
    CALL clower( ctmp(1:ncc) )
    clab = 'Constant field : '//ctmp(1:ncc)
  ELSE IF( ContourPlot%ListHdr%Number == 1 )THEN
    CALL c_format( scaling*ContourPlot%ListPtr(1)%Contour,ncc,ctmp )
    CALL clower( ctmp(1:ncc) )
    clab = 'Contour value of '//ctmp(1:ncc)
  ELSE
    WRITE(clab,2200)ContourPlot%ListHdr%Number
2200 FORMAT(I2,' contours')
  END IF
  nchx = LEN_TRIM(clab)
  IF( lunit .AND. ContourPlot%ListHdr%LabelMode /= PLOT_ON )THEN
    nchu = LEN_TRIM(clbu)
    IF( lconst )THEN
      clab(nchx+1:) = ' '//clbu(1:nchu)
      nchx = nchx + nchu + 1
    ELSE IF( ContourPlot%ListHdr%Number == 1 )THEN
      clab(nchx+1:) = ' '//clbu(1:nchu)
      nchx = nchx + nchu + 1
    ELSE
      clab(nchx+1:) = ' : ('//clbu(1:nchu)//')'
      nchx = nchx + nchu + 5
    END IF
  END IF
  IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),fsz,0.,0. )
  IF( ContourPlot%ListHdr%Number > 1 )THEN
    yy = pymn - (xcfac)*xhgt*fsz
    xcfac = xcfac + 1.
    IF( ContourPlot%ListHdr%DrawMode == PLOT_USER )THEN
      nchx = 1
      i = 1
      DO WHILE( nchx < 68 .AND. i <= ContourPlot%ListHdr%Number )
        IF( ContourPlot%ListHdr%LabelMode == PLOT_ON )THEN
          ctmp = TRIM(ContourPlot%ListPtr(i)%Label)
          ncc  = LEN(TRIM(ctmp))
        ELSE
          CALL c_format( scaling*ContourPlot%ListPtr(i)%Contour,ncc,ctmp )
          CALL clower( ctmp(1:ncc) )
        END IF
        clab(nchx:) = ctmp(1:ncc)//', '
        nchx = LEN_TRIM(clab) + 2
        i = i + 1
      END DO
      IF( i > ContourPlot%ListHdr%Number )THEN
        clab(nchx-2:) = ' '
        nchx = nchx - 3
      ELSE
        clab(nchx-2:) = ' ...'
        nchx = nchx + 1
      END IF
    ELSE
      clab = ' '
      IF( ContourPlot%ListHdr%DrawMode == PLOT_LOG )THEN
        xin = ContourPlot%ListPtr(2)%Contour/ContourPlot%ListPtr(1)%Contour
      ELSE
        xin = ContourPlot%ListPtr(2)%Contour-ContourPlot%ListPtr(1)%Contour
      END IF
      CALL c_format( scaling*ContourPlot%ListPtr(1)%Contour,ncc,ctmp )
      CALL clower( ctmp(1:ncc) )
      clab = 'Contours from '//ctmp(1:ncc)
      nchx = LEN_TRIM(clab)
      CALL c_format( scaling*ContourPlot%ListPtr(ContourPlot%ListHdr%Number)%Contour,ncc,ctmp )
      CALL clower( ctmp(1:ncc) )
      clab(nchx+1:) = ' to '//ctmp(1:ncc)
      nchx = LEN_TRIM(clab)
      CALL c_format( xin,ncc,ctmp )
      CALL clower( ctmp(1:ncc) )
      clab(nchx+1:) = ' by '//ctmp(1:ncc)
      nchx = LEN_TRIM(clab)
    END IF
    nchx = LEN_TRIM(clab)
    IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),fsz,0.,0. )
  END IF
END IF

!---- Color Box

IF( llbb .AND. PlotDraw%FillContour==SCIPtrue .AND. .NOT.lno_plot )THEN
  nchu = LEN_TRIM(clbu)
  IF( lunit .AND. nchu > 0 )THEN
    CALL GetNCARFontSize( 0.,0.,clbu(1:nchu),1.,0.,0.,xdum,ydum )
    yyy = 0.1*(pxmx-pxmn)
    xxx = MAX(0.75*yyy,0.5*(xdum-yyy) + 0.5*xhgt*fsz)
  ELSE
    yyy = 0.1*(pxmx-pxmn)
    xxx = 0.75*yyy
  END IF
  xxx = MAX(xxx,0.6*xhgt*fsz)
  xp(1) = pxmx + xxx
  xp(2) = xp(1) + yyy
  xp(3) = xp(2)
  xp(4) = xp(1)
  xp(5) = xp(1)
  dy = (pymx-pymn - xhgt*fsz)/FLOAT(ContourPlot%ListHdr%Number+1)
  dy = MIN(dy,0.1*(pymx-pymn))
  IF( popden /= 0.0 )THEN
    dy = MAX(dy,1.7*xhgt*fsz)
  ELSE
    dy = MAX(dy,0.9*xhgt*fsz)
  END IF
  IF( PlotDraw%Fill_Hi==SCIPtrue )THEN
    nn = ContourPlot%ListHdr%Number + 1
  ELSE
    nn = ContourPlot%ListHdr%Number
  END IF
  CALL gsfais( 0 )
  IF( PlotDraw%Fill_Lo==SCIPtrue )THEN
    minc = 1
  ELSE
    minc = 2
  END IF
  yyy = FLOAT(ContourPlot%ListHdr%Number+1)*dy
  IF( yyy > pymx-pymn )THEN
    yyy = MAX(0.5*(pymx+pymn-yyy),0.05)
  ELSE
    yyy = pymn
  END IF
  DO i = minc,nn
    yp(1) = yyy + FLOAT(i)*dy
    yp(2) = yp(1)
    yp(3) = yp(1) - dy
    yp(4) = yp(3)
    yp(5) = yp(1)
 	    icolor = index_contour((ContourPlot%ListHdr%Number+1)-i+1)
    CALL gsfaci( icolor )
    CALL gfa( 5,xp,yp )
  END DO
  CALL gsplci( 1 )
  DO i = 1,ContourPlot%ListHdr%Number+1
    yp(1) = yyy + FLOAT(i)*dy
    yp(2) = yp(1)
    yp(3) = yp(1) - dy
    yp(4) = yp(3)
    yp(5) = yp(1)
    n = 5
    CALL gpl( n,xp,yp )
  END DO
  xxx = 1.0
  DO i = 1,ContourPlot%ListHdr%Number
    IF( ContourPlot%ListHdr%LabelMode == PLOT_ON )THEN
      ctmp = TRIM(ContourPlot%ListPtr(i)%Label)
      ncc  = LEN(TRIM(ctmp))
    ELSE
      CALL c_format( scaling*ContourPlot%ListPtr(i)%Contour,ncc,ctmp )
    END IF
    CALL GetNCARFontSize( 0.,0.,ctmp(1:ncc),0.8*fsz,0.,0.,xdum,ydum )
    IF( xdum > 0.99-xp(2)-0.4*xhgt*fsz )THEN
      xdum = MAX(0.5,(0.8*(0.99-xp(2)-0.4*xhgt*fsz)/xdum))
    ELSE
      xdum = 0.8
    END IF
    xxx = MIN(xxx,xdum)
  END DO
  xx = xp(2) + 0.5*xxx*xhgt*fsz
  DO i = 1,ContourPlot%ListHdr%Number
    yy = yyy + FLOAT(i)*dy - 0.25*xhgt*xxx*fsz
    IF( ContourPlot%ListHdr%LabelMode == PLOT_ON )THEN
      ctmp = TRIM(ContourPlot%ListPtr(i)%Label)
      ncc  = LEN(TRIM(ctmp))
    ELSE
      CALL c_format( scaling*ContourPlot%ListPtr(i)%Contour,ncc,ctmp )
    END IF
    CALL plchhq( xx,yy,ctmp(1:ncc),xxx*fsz,0.,-1. )
  END DO
  IF( popden /= 0.0 )THEN
    IF( popden_def == -1.0 )THEN
 	    icolor = ipalln + 1
    ELSE
 	    icolor = ipalcn + 2
    END IF
    CALL gstxci( icolor )
    IF( PlotDraw%Fill_Lo )THEN
      minc = 1
    ELSE
      minc = 1 !2
    END IF
    xx = xp(2) + 0.4*xhgt*fsz
    DO i = minc,ContourPlot%ListHdr%Number !+1
      yy = yyy + FLOAT(i)*dy + 0.5*dy - 0.5*xhgt*(0.8*fsz)
      IF( popden_def == -1.0 )THEN
        CALL c_format( ContourPlot%ListPtr(i)%Area,ncc,ctmp )
      ELSE
        WRITE(ctmp,*)NINT(ContourPlot%ListPtr(i)%Population)
        ctmp = ADJUSTL(ctmp)
        ncc  = LEN_TRIM(ctmp)
      END IF
      IF( ncc > 0 )CALL plchhq( xx,yy,ctmp(1:ncc),0.8*fsz,0.,-1. )
    END DO
    xx = 0.5*(pxmn+pxmx)
    yy = pymn - xcfac*xhgt*fsz
    xcfac = xcfac + 1.
    IF( popden_def == -1.0 )THEN
       clab = 'Contour area at indicated level (sq Km)'
    ELSE IF( popden /= default )THEN
      CALL c_format( popden_def,ncc,ctmp )
      IF( larea )THEN
        clab = 'Population exposed at indicated probability ('// &
                ctmp(1:ncc)//' persons/sq Km)'
      ELSE
        clab = 'Mean population exposed at indicated level ('// &
                ctmp(1:ncc)//' persons/sq Km)'
      END IF
    ELSE
      IF( larea )THEN
        clab = 'Population exposed at indicated probability'
      ELSE
        clab = 'Mean population exposed at indicated level'
      END IF
    END IF
    nchx = LEN_TRIM(clab)
    IF( yyy < pymn )THEN
      xdum = 0.8
    ELSE
      xdum = 1.0
    END IF
    CALL plchhq( xx,yy,clab(1:nchx),xdum*fsz,0.,0. )
    CALL gstxci( 1 )
  END IF
  IF( lunit .AND. ContourPlot%ListHdr%LabelMode /= PLOT_ON )THEN
    xx = 0.5*(xp(1) + xp(2))
    yy = yyy + FLOAT(ContourPlot%ListHdr%Number+1)*dy + 0.5*xhgt*(0.8*fsz)
    nchu = LEN_TRIM(clbu)
    IF( nchu > 0 )CALL plchhq( xx,yy,clbu(1:nchu),0.8*fsz,0.,0. )
  END IF
END IF

!---- Map scale

IF( ixpage == nxpage .AND. iypage == 1 )THEN
  IF( boxl < 0.0 )THEN
    CALL i_format( MapScaleI(),nchu,ctmp )
    clab ='Scale = 1:'//TRIM(ctmp)
    nchu = LEN_TRIM(clab)
    xx = 0.99
    yy = 0.3*xhgt*fsz
    CALL plchhq( xx,yy,clab(1:nchu),0.6*fsz,0.,1. )
  END IF
END IF

!---- Reference zone (UTM)

IF( ixpage == 1 .AND. iypage == 1 )THEN
  IF( llc .AND. zone >= -60 .AND. zone <= 60 )THEN
    WRITE(ctmp,*)ABS(zone)
    ctmp = ADJUSTL(ctmp)
    IF( zone >= 0. )THEN
      clab = 'Reference zone : '//TRIM(ctmp)//'N (WGS 84)'
    ELSE
      clab = 'Reference zone : '//TRIM(ctmp)//'S (WGS 84)'
    END IF
    nchu = LEN_TRIM(clab)
    xx = 0.01
    yy = 0.3*xhgt*fsz
    CALL plchhq( xx,yy,clab(1:nchu),0.8*fsz,0.,-1. )
  END IF
END IF

!---- Return plotting units to original units

CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )

!---- Draw topography

tdel(1) = -999.
IF( .NOT.lno_plot )THEN
  SELECT CASE( PlotDef(BASE_LEVEL)%Field%Category )
    CASE( HP_HSLICE )
      IF( ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode /= PLOT_NULL )THEN
        ContourSlice%ListHdr%Number = 1
	      ALLOCATE(ContourSlice%ListPtr(1),STAT=i)
        IF( i == 0 )THEN
          ContourSlice%ListPtr(1)%Contour = PlotDef(BASE_LEVEL)%ClassData(HSLICE_INDEX)%Zmin
          ContourSlice%ListHdr%Scale = 1.0
          TerDraw%FillContour = SCIPtrue
          TerDraw%DrawContour = SCIPtrue
          TerDraw%Fill_Lo = SCIPfalse
          TerDraw%Fill_Hi = SCIPtrue
          ipen = 1
          icolor = ipalcn
          ipalcn = (ipalln+4) - 1
          ctotal = 1
          i = SCIPDrawField( UserID,FieldID(TER_INDEX),PlotTer%Field,TerType, &
                             ContourSlice%ListHdr,ContourSlice%ListPtr,TerDraw,CheckFill,CheckDraw )
          ipalcn = icolor
          DEALLOCATE( ContourSlice%ListPtr,STAT=i )
        END IF
      END IF
    CASE( HP_VSLICE )
      IF( ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode /= PLOT_NULL )THEN
        ALLOCATE(xt(MAXPT),yt(MAXPT),zt(MAXPT),Location(MAXPT),STAT=ios)
        IF( ios == 0 )THEN
          frac = (xmxd-xminp)/(FLOAT(m0)*dxsrf)
          xevb = xov + frac*(xev-xov)
          yevb = yov + frac*(yev-yov)
          frac = (xmnd-xminp)/(FLOAT(m0)*dxsrf)
          xovb = xov + frac*(xev-xov)
          yovb = yov + frac*(yev-yov)
          dx  = (xmxd-xmnd)/FLOAT(MAXPT-3)
          dxx = (xevb-xovb)/FLOAT(MAXPT-3)
          dyy = (yevb-yovb)/FLOAT(MAXPT-3)
          tmpMode = PlotCoordinate%Mode
          PlotCoordinate%Mode = ABS(PlotCoordinate%Mode)
          DO i = 1,MAXPT-3
            Location(i)%x = xovb + FLOAT(i-1)*dxx
            Location(i)%y = yovb + FLOAT(i-1)*dyy
            irv = SCIPTransformXY( PlotCoordinate,TerrainCoordinate,Location(i)%x,Location(i)%y )
          END DO
          i = MAXPT - 2
          Location(i)%x = xevb
          Location(i)%y = yevb
          irv = SCIPTransformXY( PlotCoordinate,TerrainCoordinate,Location(i)%x,Location(i)%y )
          irv = SCIPGetTerrainSlice( userID,loadPrj%project,MAXPT-2,Location,zt )
          DO i = 1,MAXPT-2
            xt(i) = xmnd + FLOAT(i-1)*dx
            yt(i) = zt(i)*scy + shy
          END DO
          xt(MAXPT-1) = xmxd
          xt(MAXPT  ) = xmnd
          yt(MAXPT-1) = ymnd
          yt(MAXPT  ) = ymnd
          CALL gsplci(1)
          CALL gpl(MAXPT,xt,yt)
          CALL gsfaci(ipalln+4)
          CALL gfa(MAXPT,xt,yt)
          PlotCoordinate%Mode = tmpMode
        END IF
        IF( ALLOCATED(xt) )DEALLOCATE(xt,STAT=ios)
        IF( ALLOCATED(yt) )DEALLOCATE(yt,STAT=ios)
      END IF
    CASE DEFAULT
      IF(ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode == PLOT_ON)THEN
        TerDraw%FillContour = SCIPfalse
        TerDraw%DrawContour = SCIPtrue
        TerDraw%Fill_Lo = SCIPfalse
        TerDraw%Fill_Hi = SCIPfalse
        ipen = 1
        ctotal = ContourTer%ListHdr%Number
  irv = SCIPDrawField( UserID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType, &
                       ContourPlot%ListHdr,ContourPlot%ListPtr,PlotDraw,CheckFill,CheckDraw )
        tdel(1) = ContourTer%ListPtr(MIN(2,ContourTer%ListHdr%Number))%Contour - &
                  ContourTer%ListPtr(1)%Contour
        tdel(2) = ContourTer%ListPtr(1)%Contour
        tdel(3) = ContourTer%ListPtr(ContourTer%ListHdr%Number)%Contour
      END IF
  END SELECT
END IF

IF( tdel(1) > 0.0 )THEN
  CALL set( 0.,1.,0.,1.,0.,1.,0.,1.,1 )
  CALL c_format( tdel(2),ncc,ctmp )
  CALL clower( ctmp(1:ncc) )
  clab = 'Terrain contours from '//ctmp(1:ncc)
  nchx = LEN_TRIM(clab)
  CALL c_format(tdel(3),ncc,ctmp)
  CALL clower(ctmp(1:ncc))
  clab(nchx+1:) = 'm to '//ctmp(1:ncc)
  nchx = LEN_TRIM(clab)
  CALL c_format( tdel(1),ncc,ctmp )
  CALL clower( ctmp(1:ncc) )
  clab(nchx+1:) = 'm by '//ctmp(1:ncc)//'m'
  nchx = LEN_TRIM(clab)
  xx = 0.5*(pxmn+pxmx)
  yy = pymn - (xcfac)*xhgt*fsz
  xcfac = xcfac + 1.
  IF( yy < -FLOAT(iypage-1)+0.5*xhgt*fsz )THEN
    yy = FLOAT(nypage-iypage+1) - xhgt*fsz
  END IF
  xdum = 0.6
  IF( nchx > 0 )CALL plchhq( xx,yy,clab(1:nchx),xdum*fsz,0.,0. )
  CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )
END IF

!---- Draw Axes and label ticks

IF( ntx <= 0 )THEN
  mtx = ABS(ntx)
  ilx = 0
ELSE
  mtx = ntx
  ilx = 1
END IF
IF( nty <= 0 )THEN
  mty = ABS(nty)
  ily = 0
ELSE
  mty = nty
  ily = 1
END IF
CALL labmod( cfx,cfy )
xlfac = 1.2
xcfac = 2.5

!---- Horizontal Plot

IF( .NOT.lvl )THEN
  IF( llc .AND. zone /= 0 )THEN
    CALL gridal( mtx,ndx,mty,ndy,ilx,0,fsz )
    IF( ily > 0 )THEN
      cfmt = '0'
      CALL GetNCARFontSize( 0.,0.,cfmt(1:1),1.,0.,0.,xdum,xhgt )
      xx = xmnd - 0.5*xdum
      dx = (ymxd-ymnd)/FLOAT(MAX(1,mty))
      DO i = 1,mty+1
        yy = ymnd + FLOAT(i-1)*dx
        IF( yy < 0.0 )THEN
          xdum = yy + 10000.
        ELSE
          xdum = yy
        END IF
        WRITE(cfmt,cfy,IOSTAT=ios)scy*xdum
        IF( ios /= 0 )cfmt = 'Err'
        nch = LEN_TRIM(cfmt)
        CALL plchhq( xx,yy-0.4*xhgt*fsz,cfmt(1:nch),fsz,0.,1. )
      END DO
    END IF
  ELSE
    CALL gridal( mtx,ndx,mty,ndy,ilx,ily,fsz )
  END IF
  IF( ilx > 0 )THEN
    xlfac = xlfac + 1.2*fsz
    xcfac = xcfac + 1.2*fsz
  END IF

!---- Vertical Plot

ELSE

  CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )
  CALL gridal( mtx,ndx,mty,ndy,0,ily,fsz )
  CALL set( 0.,1.,0.,1.,0.,1.,0.,1.,1 )
  IF( ilx > 0 )THEN
    frac = (xmxd-xminp)/(FLOAT(m0)*dxsrf)
    xevb = xov + frac*(xev-xov)
    yevb = yov + frac*(yev-yov)
    frac = (xmnd-xminp)/(FLOAT(m0)*dxsrf)
    xovb = xov + frac*(xev-xov)
    yovb = yov + frac*(yev-yov)
    cfmt ='0'
    CALL GetNCARFontSize( 0.,0.,cfmt(1:1),1.,0.,0.,xdum,xhgt )
    yy = pymn - 1.2*xhgt*fsz
    dx = (pxmx-pxmn)/FLOAT(MAX0(1,mtx))
    dxx = (xevb-xovb)
    dyy = (yevb-yovb)
    nchx = LEN_TRIM(cfx)
    IF( ABS(dxx) > 1.E-4 )THEN
      xdum = dxx/FLOAT(MAX0(1,mtx))
      xlfac = xlfac + 1.2*fsz
      xcfac = xcfac + 1.2*fsz
      DO i = 1,mtx+1
        cfmt = ' '
        xx = pxmn + FLOAT(i-1)*dx
        xxx = xovb + FLOAT(i-1)*xdum
        WRITE(cfmt,cfx,IOSTAT=ios)scx*xxx
        IF( ios /= 0 )cfmt = 'Err'
        cfmt = ADJUSTL(cfmt)
        nch = LEN_TRIM(cfmt)
        CALL plchhq( xx,yy,cfmt(1:nch),fsz,0.,0. )
      END DO
    END IF
    IF( ABS(dyy) > 1.E-4 )THEN
      ydum = dyy/FLOAT(MAX(1,mtx))
      IF( ABS(dxx) > 1.E-4 )THEN
        xlfac = xlfac + 1.0*fsz
        xcfac = xcfac + 1.0*fsz
        yy = yy - xhgt*fsz
      ELSE
        xlfac = xlfac + 1.2*fsz
        xcfac = xcfac + 1.2*fsz
      END IF
      DO i = 1,mtx+1
        cfmt = ' '
        xx  = pxmn + FLOAT(i-1)*dx
        yyy = yovb + FLOAT(i-1)*ydum
        IF( llc .AND. zone /= 0 .AND. yyy < 0. )yyy = yyy + 10000.
        WRITE(cfmt,cfx,IOSTAT=ios)scx*yyy
        IF( ios /= 0 )cfmt = 'Err'
        cfmt = ADJUSTL(cfmt)
        nch  = LEN_TRIM(cfmt)
        IF( nch > 0 )CALL plchhq( xx,yy,cfmt(1:nch),fsz,0.,0. )
      END DO
    END IF
  END IF
  CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )
END IF

CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )

RETURN
END

!============================================================

SUBROUTINE reset_title( x0,y0,dx,dy,in,fmt,scl,out,nout )

IMPLICIT NONE

REAL         x0,y0,dx,dy,scl
CHARACTER(*) in,out,fmt
INTEGER      nout

CHARACTER(128) x,y,u,cfmt

INTEGER i, j, k, ios

IF( in(1:1) /= '|' )THEN
  out = in
  GOTO 9999
END IF

out = 'invalid'
x   = 'Unkn'
y   = 'Unkn'
u   = ' '

j = 2
DO i = 1,3
  IF( j > LEN_TRIM(in) )THEN
    k = j
  ELSE IF( in(j:j) == '|' )THEN
    k = j
  ELSE
    k = INDEX(in(j:),'|') + j - 1
    IF( k <= j)k = LEN_TRIM(in) + 1
  END IF
  IF( k > j )THEN
    IF( i == 1)x = in(j:k-1)
    IF( i == 2)y = in(j:k-1)
    IF( i == 3)u = in(j:k-1)
    j = k + 1
  END IF
END DO

IF( ABS(dx) > 0.0001 )THEN
  IF( ABS(dy) > 0.0001 )THEN
    out = TRIM(x)//','//TRIM(y)
  ELSE
    WRITE(cfmt,fmt,IOSTAT=ios)scl*y0
    IF( ios /= 0 )cfmt='Err'
    cfmt = ADJUSTL(cfmt)
    out = TRIM(x)//' at '//TRIM(y)//'='//TRIM(cfmt)
  END IF
ELSE
  IF( ABS(dy) > 0.0001 )THEN
    WRITE(cfmt,fmt,IOSTAT=ios)scl*x0
    IF( ios /= 0 )cfmt='Err'
    cfmt = ADJUSTL(cfmt)
    out = TRIM(y)//' at '//TRIM(x)//'='//TRIM(cfmt)
  ELSE
    out ='Invalid'
  END IF
END IF

IF( u /= ' ' )out = TRIM(out)//' ('//TRIM(u)//')'

9999 CONTINUE

nout = LEN_TRIM(out)

RETURN
END
