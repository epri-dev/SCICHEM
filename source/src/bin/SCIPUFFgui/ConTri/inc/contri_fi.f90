MODULE contri_fi

USE contri_fd
USE DefSize_fd

SAVE

!-----------------------------------------------------------------------

REAL real_start
REAL xmnd,xmxd,ymnd,ymxd
REAL xmnb,xmxb,ymnb,ymxb
REAL xcb,ycb,boxl
REAL scx,scy,scv,sca
REAL shx,shy,shv,sha
REAL time
REAL dlo,dhi,din
REAL fsz,str,small,spv,big,exceed,xttu,yttu
REAL xolc,yolc,popden,xtt3,ytt3,xttl,yttl
REAL default,popden_def,spvl
REAL xclc,yclc
REAL xminp,yminp,dxsrf,dysrf,tima
REAL dmax,dmin,vmax,vmin
REAL fmax,fmin,dmn,dmx,delminc,delminc_def
REAL pxmn,pxmx,pymn,pymx
REAL rgb(3,0:63)
REAL rfx,rfy
REAL xod,yod,xsdp,ysdp,xom,yom,xspm,yspm,xop,yop

!-----------------------------------------------------------------------

INTEGER int_start
INTEGER ivar,jvar,ntx,nty,ndx,ndy
INTEGER nfnt,nmax,maxkey
INTEGER nfnc,nmxlpi,iATP,ncell,m0,n0,nvar
INTEGER mxfnc,iopen,mxlev,npal,ctotal
INTEGER ipalln,ipalar,ipalcn,npalcn,npalln,npalar,ipen
INTEGER nrf,nrfm,nrx,nrxm,nry,nrym,nmxlp,ilasta,zone
INTEGER srf_file_type

!-----------------------------------------------------------------------

LOGICAL log_start
LOGICAL llab,lttl,lmax,ltri,lcel
LOGICAL lfrm,lbck,lact,lunit
LOGICAL laux,llbx,llby
LOGICAL lread,llbc,lint,llog,lmapon,lus,lcol
LOGICAL llbb,logtrp,lmppp,lmprd,lmptx
LOGICAL lmppps,lmptxp,llc,lmprr,lmpaf,lmpat,lmpnf,lmpnt
LOGICAL lpts,llns,lttu,ltt3,larea,lavs,lcts,lavs_cts,lpts_txt
LOGICAL lconst,lpal,lspv,lrfx,lrfy,lrfxy,lprint,lctxlab
LOGICAL lkmb,lwxs,lovly,lovl,loil,leis,lccp,lmapout,lcloseIt,lusa

!-----------------------------------------------------------------------

CHARACTER(80) char_start
CHARACTER(80) cttl,clbx,clby,clab
CHARACTER(80) cvar,caux,cfx,cfy,cgmf,clbu
CHARACTER(80) cpts,clns,cttu,cavs,ctt3
CHARACTER(16) ctxlab(MAXCNT)
CHARACTER(4)  fvar(100)
CHARACTER(80) cfnc(MAXFUN)

CHARACTER(PATH_MAXLENGTH) cfil, covly,cpal,cdir

!-----------------------------------------------------------------------

REAL xmin_def,ymin_def,dxsrf_def,dysrf_def

!-----------------------------------------------------------------------

INTEGER iflag
REAL    vzero

!-----------------------------------------------------------------------

LOGICAL lvl,lcnt
REAL    xov,xev,yov,yev

END MODULE contri_fi

!=============================================================================

MODULE PlotTrans_fi

  USE field_fd

  SAVE

  TYPE( SCIPFieldCoordinateT ) :: PlotCoordinate
  TYPE( SCIPFieldCoordinateT ) :: DataCoordinate
  TYPE( SCIPFieldCoordinateT ) :: MapsCoordinate
  TYPE( SCIPFieldCoordinateT ) :: TerrainCoordinate

END MODULE PlotTrans_fi
