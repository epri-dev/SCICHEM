SUBROUTINE draw_map()

USE contri_fi
USE pltchoice_fi

IMPLICIT NONE

REAL    fszx,scale
LOGICAL lcc

lcc = .NOT.( PlotDraw%FillContour==SCIPtrue .OR. lcol )
IF( lprint )THEN
  fszx = 0.50*fsz
ELSE
  fszx = 0.6666667*fsz
END IF
CALL gsplci( 1 )

!     The scale factor is used to convert real numbers to integer numbers
!     so that the API rectangle comparison routines can be used.

scale = 10000.
scale = 10.**(8-INT(LOG10(MAX(ABS(xmnd),ABS(xmxd),ABS(ymnd),ABS(ymxd)))))

CALL dcwmap( xmnd,xmxd,ymnd,ymxd,lus,lmppp,lmprd,lmptx,lcc, &
             fszx,ipalln,ipalar,lmppps,lmptxp,lmprr,lmpaf,lmpnf, &
             lmpat,lmpnt,lprint,scale )

RETURN
END

!==============================================================================

SUBROUTINE dcwmap( xmin,xmax,ymin,ymax,lhires,lpop,lroad,ltext,lcc, &
                   fsz,ioffln,ioffar,lsym,lpart,lrail,lair,lnuc, &
                   lairtxt,lnuctxt,lprint,scale )

USE files_fi
USE SwinAPI

IMPLICIT NONE

REAL    xmin,xmax,ymin,ymax,fsz,scale
LOGICAL lhires,lpop,lroad,ltext,lcc,lsym,lpart,lrail,lair,lairtxt
LOGICAL lnuc,lnuctxt,lprint
INTEGER ioffln,ioffar

CHARACTER(80) iname,sdir
CHARACTER(1)  clat,clon

TYPE( T_RECT ) MyRect

INTEGER imin,imax,jmin,jmax,jj,ii,i1,i2,lat0,klon,lon0,nchs,nchi
INTEGER ii_wrap,ii_max,ii_min,ii_shft,imin13,imax13

INTEGER, EXTERNAL :: real2integer
LOGICAL, EXTERNAL :: hasError

LOGICAL cancel_print

MyRect%left   = real2integer( scale*(xmin-0.01*(xmax-xmin)) )
MyRect%right  = real2integer( scale*(xmax+0.01*(xmax-xmin)) ) + 1
MyRect%top    = real2integer( scale*(ymin-0.01*(ymax-ymin)) )
MyRect%bottom = real2integer( scale*(ymax+0.01*(ymax-ymin)) ) + 1

CALL set_wrap( xmin,xmax,ymin,ymax )

IF( lhires )THEN

  CALL set_tiles( xmin,xmax,ymin,ymax,imin,imax,jmin,jmax,imin13,imax13 )
!
!-----  Non-Text Items Loop
!
  DO jj = jmin,jmax
    IF( jj == -13 )THEN
      lat0 = -90
    ELSE
      lat0 = 5*jj
    END IF

    IF( lat0 >= 0 )THEN
      clat = 'n'
    ELSE
      clat = 's'
    END IF

    IF( jj == -13 )THEN
      i1 = imin13
      i2 = imax13
      klon = 90
    ELSE
      i1 = imin
      i2 = imax
      klon = 5
    END IF
    ii_max =  180/klon - 1
    ii_min = -180/klon - 1
    ii_shft = ii_max - ii_min

    DO ii = i1,i2
      ii_wrap = ii
      DO WHILE( ii_wrap > ii_max )
        ii_wrap = ii_wrap - ii_shft
      END DO
      DO WHILE( ii_wrap < ii_min )
        ii_wrap = ii_wrap + ii_shft
      END DO

      lon0 = klon*ii_wrap
      IF( lon0 >= 0 )THEN
        clon = 'e'
      ELSE
        clon = 'w'
      END IF

      sdir = TRIM(path_map)//'\'//clat//clon
      nchs = LEN(TRIM(sdir))

      IF( lpop )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.pp'
        nchi = 11 + nchs
        CALL write_map_areas( iname(1:nchi),MyRect,lcc,ioffar,scale )
        IF( hasError() )GOTO 9999
      END IF

      WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.po'
200   FORMAT(A,'\',A,I3.3,A,I2.2,A)
      nchi = 11 + nchs

      CALL gslwsc( 0.67 )
      CALL write_map_segments( iname(1:nchi),MyRect,ioffln,scale )
      CALL gslwsc( 0.36 )
      IF( hasError() )GOTO 9999

      IF( cancel_print(lprint) )RETURN

      IF( lroad )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.rd'
        nchi = 11 + nchs
        CALL write_map_segments( iname(1:nchi),MyRect,ioffln,scale )
        IF( hasError() )GOTO 9999
      END IF

      IF( lrail )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.rr'
        nchi = 11 + nchs
        CALL gslwsc( 0.67 )
        CALL write_map_segments( iname(1:nchi),MyRect,-ioffln,scale )
        CALL gslwsc( 0.36 )
        IF( hasError() )GOTO 9999
      END IF

      IF( lsym )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.ppt'
        nchi = 12 + nchs
        CALL write_map_symbols( iname(1:nchi),MyRect,fsz,1,scale )
        IF( hasError() )GOTO 9999
      END IF

      IF( lair )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.apt'
        nchi = 12 + nchs
        CALL write_map_airports( iname(1:nchi),MyRect,fsz,ioffln,scale )
        IF( hasError() )GOTO 9999
      END IF

      IF( lnuc )THEN
        WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.nuc'
        nchi = 12 + nchs
        CALL write_map_nucfac( iname(1:nchi),MyRect,fsz,ioffln,scale )
        IF( hasError() )GOTO 9999
      END IF

      IF( cancel_print(lprint) )RETURN

    END DO
  END DO
!
!-----  Text Items Loop
!
  IF( (ltext .AND. .NOT.lpart) .OR. lairtxt .OR. lnuctxt )THEN

    CALL gstxmd( -1 )

    DO jj = jmin,jmax
      IF( jj == -13 )THEN
        lat0 = -90
      ELSE
        lat0 = 5*jj
      END IF

      IF( lat0 >= 0 )THEN
        clat = 'n'
      ELSE
        clat = 's'
      END IF

      IF( jj == -13 )THEN
        i1 = INT(xmin/90.0)
        i2 = INT(xmax/90.0-1.E-6)
        klon = 90
      ELSE
        i1 = imin
        i2 = imax
        klon = 5
      END IF

      DO ii = i1,i2
        lon0 = klon*ii
        IF( lon0 >= 0 )THEN
          clon = 'e'
        ELSE
          clon = 'w'
        END IF

        sdir = TRIM(path_map)//'\'//clat//clon
        nchs = LEN(TRIM(sdir))

        IF( ltext .AND. .NOT.lpart )THEN
          WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.ppt'
          nchi = 12 + nchs
          CALL write_map_text( iname(1:nchi),MyRect,fsz,12,0,0.,scale )
          IF( hasError() )GOTO 9999
        END IF

        IF( lairtxt )THEN
          WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.apt'
          nchi = 12 + nchs
          CALL write_map_text( iname(1:nchi),MyRect,0.8*fsz,16,ioffln,-1.,scale )
          IF( hasError() )GOTO 9999
        END IF

        IF( lnuctxt )THEN
          WRITE(iname,200) sdir(1:nchs),clon,ABS(lon0),clat,ABS(lat0),'.nuc'
          nchi = 12 + nchs
          CALL write_map_text( iname(1:nchi),MyRect,0.8*fsz,-16,ioffln,0.,scale )
          IF( hasError() )GOTO 9999
        END IF

        IF( cancel_print(lprint) )RETURN

      END DO
    END DO

    CALL gstxmd( 1 )

  END IF

  IF( ltext .AND. lpart )THEN
    IF( lpart )THEN
      CALL gstxmd( -1 )
      sdir = TRIM(path_map)
      nchs = LEN(TRIM(sdir))
      iname = sdir(1:nchs)//'\browse.ppt'
      nchi  = 11 + nchs
      CALL write_map_text( iname(1:nchi),MyRect,fsz,12,0,0.,scale )
      CALL gstxmd( 1 )
      IF( hasError() )GOTO 9999
    END IF
  END IF

ELSE

  sdir = TRIM(path_map)
  nchs = LEN(TRIM(sdir))
  iname = sdir(1:nchs)//'\browse.po'
  nchi  = 10 + nchs
  CALL gslwsc( 0.67 )
  CALL write_map_segments( iname(1:nchi),MyRect,ioffln,scale )
  CALL gslwsc( 0.36 )
  IF( hasError() )GOTO 9999
  IF( lsym )THEN
    iname = sdir(1:nchs)//'\browse.ppt'
    nchi  = 11 + nchs
    CALL write_map_symbols( iname(1:nchi),MyRect,fsz,1,scale )
    IF( hasError() )GOTO 9999
  END IF
  IF( ltext )THEN
    CALL gstxmd( -1 )
    iname = sdir(1:nchs)//'\browse.ppt'
    nchi  = 11 + nchs
    CALL write_map_text( iname(1:nchi),MyRect,fsz,12,0,0.,scale )
    CALL gstxmd( 1 )
    IF( hasError() )GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE write_map_segments( filename,MyRect,ioff,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*)   filename
TYPE( T_RECT ) MyRect
INTEGER        ioff
REAL           scale

REAL    xr(2),yr(2),xx
INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,n,itype,j,ierror
REAL    xmin,ymin

CHARACTER(128) eString

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

201 OPEN(UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=1*4, &
         STATUS='OLD',ACTION='READ',IOSTAT=ierr)
IF( ierr /= 0 )GOTO 9999

!------ read number of segments and South-West corner lat/lon

irec = file_offset+1
READ(lun_tmp,rec=irec,ERR=200) nseg
irec = irec + 1
READ(lun_tmp,rec=irec,ERR=200) xmin
irec = irec + 1
READ(lun_tmp,rec=irec,ERR=200) ymin
i0   = 1

! ***  Read data points

101 CONTINUE
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) n
  irec = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) itype
  irec = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) xr(1)
  irec = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) yr(1)
  CALL map_transform( xr(1),yr(1) )
  IF( ioff < 0 )CALL gsln( 2 )
  CALL gsplci( itype+ABS(ioff) )

  DO j = 2,n
    irec = irec + 1
    READ(lun_tmp,rec=irec,ERR=100) xr(2)
    irec = irec + 1
    READ(lun_tmp,rec=irec,ERR=100) yr(2)
    CALL map_transform( xr(2),yr(2) )
    xx = xr(2)
    CALL write_segment_wrap( 2,xr,yr,MyRect,scale,0 )
    xr(1) = xx
    yr(1) = yr(2)
  END DO

END DO

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierr )

CALL gsln( 0 )
CALL gslwsc( 0.36 )
RETURN

100 CONTINUE
IF( nbuf == 1 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapSegments' )
  GOTO 9999
ELSE
  nbuf = 1
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr )
  OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=1*4, &
        STATUS='OLD',ACTION='READ',IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapSegments' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == 1 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapSegments' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = 1
  GOTO 201
END IF

END

!==============================================================================

SUBROUTINE write_map_areas( filename,MyRect,lcc,ioff,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*) filename
TYPE( T_RECT ) MyRect
INTEGER      ioff
LOGICAL      lcc
REAL         scale

CHARACTER(128) :: eString

REAL xr(2000),yr(2000)

INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,n,itype,j,ierror
REAL    xmin,ymin

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset
LOGICAL, EXTERNAL :: NCARPrint

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

201 OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=1*4, &
          STATUS='OLD',ACTION='READ',IOSTAT=ierr )
IF( ierr /= 0 )GOTO 9999

!------ read number of segments and South-West corner lat/lon

IF( NCARPrint() )THEN
  CALL gsfais( 3 )
ELSE
  CALL gsfais( -3 )
END IF
CALL gsplci( 1 )
CALL gsfaci( 1 )

irec = file_offset+1
READ(lun_tmp,rec=irec,ERR=200) nseg
irec = irec + 1
READ(lun_tmp,rec=irec,ERR=200) xmin
irec = irec + 1
READ(lun_tmp,rec=irec,ERR=200) ymin

i0 = 1

! ***  Read data points

101 CONTINUE
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) n
  irec = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) itype
  DO j = 1,n
    irec = irec + 1
    READ(lun_tmp,rec=irec,ERR=100) xr(j)
    irec = irec + 1
    READ(lun_tmp,rec=irec,ERR=100) yr(j)
    CALL map_transform( xr(j),yr(j) )
  END DO
  IF( lcc )THEN
    CALL gsplci( itype+ioff )
    CALL gsfaci( itype+ioff )
  END IF
  CALL write_segment_wrap( n,xr,yr,MyRect,scale,1 )

END DO

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierror )

CALL gsfais( 0 )
CALL gsplci( 1 )
CALL gsfaci( 1 )
RETURN

100 CONTINUE
IF( nbuf == 1 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapAreas' )
  GOTO 9999
ELSE
  nbuf = 1
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr)
  OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=1*4, &
           STATUS='OLD',ACTION='READ',IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapAreas' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == 1 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapAreas' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = 1
  GOTO 201
END IF

END

!==============================================================================

SUBROUTINE write_map_text( filename,MyRect,fsz,jreclen,ioff,cent,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*) filename
TYPE( T_RECT ) MyRect
INTEGER      ioff,jreclen
REAL         fsz,cent
REAL         scale

CHARACTER(52)  MapText,text
CHARACTER(128) eString

INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,ncht,nch,ierror,ityp
INTEGER ireclen,nchs
REAL    xmin,ymin,x,y,xwid,xhgt,xoff,yoff
LOGICAL lsplit

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

ireclen = ABS(jreclen)
lsplit = jreclen < 0

201 OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=IRECLEN*4, &
          STATUS='OLD',ACTION='READ',IOSTAT=ierr )
IF( ierr /= 0 )GOTO 9999

IF( ireclen == 16 )THEN
  ncht = 50
  nchs = 25
ELSE
  ncht = 40
  nchs = 20
END IF

!------ read number of segments and South-West corner lat/lon

CALL gstxci( 1 )
CALL gsplci( 1 )
text ='M'
x = 0.5*(MyRect%left+MyRect%right)/scale
y = 0.5*(MyRect%top+MyRect%bottom)/scale
CALL GetNCARFontSize( x,y,text(1:1),fsz,0.,-1.,xwid,xhgt )
IF( cent /= 0. )THEN
  xoff=-0.6*cent*xwid
ELSE
  xoff = 0.
END IF
yoff=-0.333333*xhgt

irec = file_offset + 1
READ(lun_tmp,rec=irec,ERR=200) nseg,xmin,ymin
i0   = 1

! ***  Read data points

101 CONTINUE
text(1:) =' '
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  IF( ireclen == 16 )THEN
    READ(lun_tmp,rec=irec,ERR=100) x,y,ityp,MapText(1:ncht)
  ELSE
    READ(lun_tmp,rec=irec,ERR=100) x,y,MapText(1:ncht)
    ityp = 0
  END IF
  CALL map_transform( x,y )
  IF( lsplit )ityp = ityp + 6
  IF( ioff > 0)CALL gstxci( ioff+ityp )
  IF( lsplit )THEN
    text = TRIM(MapText(1:nchs))
    nch  = LEN_TRIM(text)
    IF( nch > 0 )CALL write_text_wrap( x,y,xoff,yoff,text(1:nch),fsz,cent,MyRect,scale )
    text = TRIM(MapText(nchs+1:ncht))
    nch  = LEN_TRIM(text)
    IF( nch > 0 )CALL write_text_wrap( x,y,xoff,yoff-1.8*xhgt,text(1:nch),fsz,cent,MyRect,scale )
  ELSE
    text = TRIM(MapText(1:ncht))
    nch  = LEN_TRIM(text)
    IF( nch > 0 )CALL write_text_wrap( x,y,xoff,yoff,text(1:nch),fsz,cent,MyRect,scale )
  END IF
END DO

9999  CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierror )

CALL gsfais( 0 )
CALL gsplci( 1 )
CALL gsfaci( 1 )
CALL gstxci( 1 )
RETURN

100 CONTINUE
IF( nbuf == ireclen )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapTexts' )
  GOTO 9999
ELSE
  nbuf = ireclen
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr )
  OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=ireclen*4, &
           STATUS='OLD',ACTION='READ',IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapTexts' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == ireclen )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapTexts' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = ireclen
  GOTO 201
END IF

END

!==============================================================================

SUBROUTINE write_map_symbols( filename,MyRect,fsz,icol,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*) filename
TYPE( T_RECT ) MyRect
INTEGER      icol
REAL         fsz
REAL         scale

CHARACTER(40)  MapText,text
CHARACTER(128) eString

INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,ierror
REAL    xmin,ymin,x,y,xoff,yoff,xwid,xhgt

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

201 OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=12*4, &
          STATUS='OLD',ACTION="READ",IOSTAT=ierr )
IF( ierr /= 0 )GOTO 9999

!------ read number of segments and South-West corner lat/lon

CALL gsfais( 0 )
CALL gsplci( icol )
CALL gsfaci( icol )
text = 'M'
x = 0.5*(MyRect%left+MyRect%right)/scale
y = 0.5*(MyRect%top+MyRect%bottom)/scale
CALL GetNCARFontSize( x,y,text(1:1),fsz,0.,-1.,xwid,xhgt )
xoff = 0.25*xwid
yoff = 0.25*xhgt

irec = file_offset + 1
READ(lun_tmp,rec=irec,ERR=200) nseg,xmin,ymin
i0 = 1

! ***  Read data points

101 CONTINUE
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) x,y,MapText
  CALL map_transform( x,y )
  CALL write_symbol_wrap( x,y,xoff,yoff,MyRect,scale,0 )
END DO

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierror )

RETURN

100 CONTINUE
IF( nbuf == 12 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapSymbols' )
  GOTO 9999
ELSE
  nbuf = 12
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr)
  OPEN( UNIT=lun_tmp,FILE=FILENAME,ACCESS='DIRECT',RECL=12*4, &
        STATUS='OLD',ACTION="READ",IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapSymbols' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == 12 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapSymbols' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = 12
  GOTO 201
END IF

END

!==============================================================================

SUBROUTINE write_map_airports( filename,MyRect,fsz,ioff,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*)   filename
TYPE( T_RECT ) MyRect
INTEGER        ioff
REAL           fsz
REAL           scale

CHARACTER(50)  MapText,text
CHARACTER(128) eString

INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,ityp,icol,ierror
REAL    xmin,ymin,x,y,xoff,yoff,xwid,xhgt

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

201 OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=16*4, &
          STATUS='OLD',ACTION="READ",IOSTAT=ierr )
IF( ierr /= 0 )GOTO 9999

!------ read number of segments and South-West corner lat/lon

CALL gsfais( 0 )
text = 'M'
x = 0.5*(MyRect%left+MyRect%right)/scale
y = 0.5*(MyRect%top+MyRect%bottom)/scale
CALL GetNCARFontSize( x,y,text(1:1),fsz,0.,-1.,xwid,xhgt )
xoff = 0.25*xwid
yoff = 0.25*xhgt

irec = file_offset + 1
READ(lun_tmp,rec=irec,ERR=200) nseg,xmin,ymin
i0 = 1

! ***  Read data points

101 CONTINUE
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) x,y,ityp,MapText
  CALL map_transform( x,y )
  icol = ioff + ityp
  CALL gsplci( icol  )
  CALL gsfaci( icol)
  CALL write_symbol_wrap( x,y,xoff,yoff,MyRect,scale,0 )
END DO

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierror )

RETURN

100 CONTINUE
IF( nbuf == 16 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapAirports' )
  GOTO 9999
ELSE
  nbuf = 16
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr)
  OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=16*4, &
           STATUS='OLD',ACTION="READ",IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapAirports' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == 16 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapAirports' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = 16
  GOTO 201
END IF

END

!==============================================================================

SUBROUTINE write_map_nucfac( filename,MyRect,fsz,ioff,scale )

USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*)   filename
TYPE( T_RECT ) MyRect
INTEGER        ioff
REAL           fsz
REAL           scale

CHARACTER(50)  MapText,text
CHARACTER(128) eString

INTEGER nbuf,ierr,i0,nseg,i,irec,irecs,ityp,icol,ierror
REAL    xmin,ymin,x,y,xoff,yoff,xwid,xhgt

INTEGER file_offset

INTEGER, EXTERNAL :: MapRecordOffset

file_offset = MapRecordOffset( filename )
IF( file_offset < 0 )GOTO 9999

nbuf = 1024

201 OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=16*4, &
          STATUS='OLD',ACTION="READ",IOSTAT=ierr )
IF( ierr /= 0 )GOTO 9999

!------ read number of segments and South-West corner lat/lon

CALL gsfais( 0 )
text = 'M'
x = 0.5*(MyRect%left+MyRect%right)/scale
y = 0.5*(MyRect%top+MyRect%bottom)/scale
CALL GetNCARFontSize( x,y,text(1:1),fsz,0.,-1.,xwid,xhgt )
xoff = 0.5*xwid
yoff = 0.5*xwid

irec = file_offset + 1
READ(lun_tmp,rec=irec,ERR=200) nseg,xmin,ymin
i0 = 1

! ***  Read data points

101 CONTINUE
DO i = i0,nseg
  irecs = irec
  irec  = irec + 1
  READ(lun_tmp,rec=irec,ERR=100) x,y,ityp,MapText
  CALL map_transform( x,y )
  icol = ioff + ityp + 6
  CALL gsplci( icol )
  CALL gsfaci( icol )
  CALL write_symbol_wrap( x,y,xoff,yoff,MyRect,scale,1 )
END DO

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ierror )

RETURN

100 CONTINUE
IF( nbuf == 16 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapNucFacs' )
  GOTO 9999
ELSE
  nbuf = 16
  i0   = i
  irec = irecs
  CLOSE( UNIT=lun_tmp,IOSTAT=ierr)
  OPEN( UNIT=lun_tmp,FILE=filename,ACCESS='DIRECT',RECL=16*4, &
           STATUS='OLD',ACTION="READ",IOSTAT=ierr )
  IF( ierr /= 0 )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( OP_ERROR, &
                  'Unable to re-open Map file', &
                   eString, &
                  'Check file', &
                  'WriteMapNucFacs' )
    GOTO 9999
  END IF
  GOTO 101
END IF

200 CONTINUE
IF( nbuf == 16 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( RD_ERROR, &
                'Unable to read Map file', &
                 eString, &
                'Check file', &
                'WriteMapNucFacs' )
  GOTO 9999
ELSE
  CLOSE( UNIT=lun_tmp,IOSTAT=ierror )
  nbuf = 16
  GOTO 201
END IF

END
!*******************************************************************************
!                map_transform
!*******************************************************************************
SUBROUTINE map_transform( x,y )

USE contri_fi
USE PlotTrans_fi
USE ToolUser_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
REAL, INTENT( INOUT ) :: x !X value
REAL, INTENT( INOUT ) :: y !Y value

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SCIPTransform

!==============================================================================
! Use SCIPTool to transform
!==============================================================================
irv = SCIPTransform( MapsCoordinate,PlotCoordinate,1,x,y )
IF( irv /= SCIPsuccess )THEN
  CALL SCIPInitError()
  GOTO 9999
END IF

!==============================================================================
! Add Plot Scaling
!==============================================================================
x = x*scx + shx
y = y*scy + shy

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE set_tiles( xmin,xmax,ymin,ymax,imin,imax,jmin,jmax,imin13,imax13 )

USE contri_fi

!     Sets the HiRes tile range for looping.  Must transform xmin etc.
!     from the current plot coordinate system to Lat/Lon.

IMPLICIT NONE

REAL    xmin,xmax,ymin,ymax
REAL    xminm,xmaxm,yminm,ymaxm
INTEGER imin,imax,jmin,jmax,imin13,imax13

IF( llc )THEN
  xminm = ((xmin-shx)/scx - xop)*xspm + xom
  xmaxm = ((xmax-shx)/scx - xop)*xspm + xom
  yminm = ((ymin-shy)/scy - yop)*yspm + yom
  ymaxm = ((ymax-shy)/scy - yop)*yspm + yom
ELSE
  xminm = xmin
  xmaxm = xmax
  yminm = ymin
  ymaxm = ymax
END IF

imin = INT(xminm/5.0)
IF( xminm < 0 )imin = imin - 1
imax = INT(xmaxm/5.0-1.E-6)
IF( xmaxm < 0 )imax = imax - 1
jmin = INT(yminm/5.0)
IF( yminm < 0 )jmin = jmin - 1
jmax = INT(ymaxm/5.0-1.E-6)
IF( ymaxm < 0 )jmax = jmax - 1

jmin = MAX(jmin,-13)

imin13 = INT(xminm/90.0)
IF( xminm < 0 )imin13 = imin13 - 1
imax13 = INT(xmaxm/90.0-1.E-6)
IF( xmaxm < 0 )imax13 = imax13 - 1

RETURN
END

!==============================================================================

SUBROUTINE set_wrap( xmin,xmax,ymin,ymax )

USE contri_fi
USE mapwrap

!     Sets the HiRes tile range for looping.  Must transform xmin etc.
!     from the current plot coordinate system to Lat/Lon.

IMPLICIT NONE

REAL xmin,xmax,ymin,ymax
REAL xminm,xmaxm,yminm,ymaxm

REAL x0,x,y

IF( llc )THEN
  xminm = ((xmin-shx)/scx - xop)*xspm + xom
  xmaxm = ((xmax-shx)/scx - xop)*xspm + xom
  yminm = ((ymin-shy)/scy - yop)*yspm + yom
  ymaxm = ((ymax-shy)/scy - yop)*yspm + yom
ELSE
  xminm = xmin
  xmaxm = xmax
  yminm = ymin
  ymaxm = ymax
END IF

x0 = 0.
y  = 0.
CALL map_transform( x0,y )

wmin = 0.
wmax = 0.
IF( xmaxm > 180.0 )THEN
  x = 360.
  y = 0.
  CALL map_transform( x,y )
  wmax = x - x0
END IF
IF( xminm < -180.0 )THEN
  x = -360.
  y = 0.
  CALL map_transform( x,y )
  wmin = x - x0
END IF

RETURN
END

!==============================================================================

SUBROUTINE write_segment_wrap( n,x,y,MyRect,scale,iflag )

USE SwinAPI
USE mapwrap

IMPLICIT NONE

INTEGER n
TYPE( T_RECT ) MyRect
REAL, DIMENSION(*) :: x,y
REAL    scale
INTEGER iflag

TYPE( T_POINT ) MyPt

LOGICAL lin

REAL    xoff
INTEGER i

LOGICAL, EXTERNAL :: CheckPoint

!==== Basic line

lin = CheckPoint( x(1),y(1),MyPt,scale,MyRect )

DO i = 2,n
  lin = lin .OR. CheckPoint( x(i),y(i),MyPt,scale,MyRect )
END DO
IF( lin )THEN
  IF( iflag == 0 )THEN
    CALL gpl( n,x,y )
  ELSE
    CALL gfa( n,x,y )
  END IF
END IF

!==== Wrap minus

xoff = 0.
IF( wmin /= 0. )THEN
  x(1) = x(1) + wmin
  lin = CheckPoint( x(1),y(1),MyPt,scale,MyRect )
  DO i = 2,n
    x(i) = x(i) + wmin
    lin = lin .OR. CheckPoint( x(i),y(i),MyPt,scale,MyRect )
  END DO
  IF( lin )THEN
    IF( iflag == 0 )THEN
      CALL gpl( n,x,y )
    ELSE
      CALL gfa( n,x,y )
    END IF
  END IF
  xoff = -wmin
END IF

!==== Wrap plus

IF( wmax /= 0. )THEN
  x(1) = x(1) + xoff + wmax
  lin = CheckPoint( x(1),y(1),MyPt,scale,MyRect )
  DO i = 2,n
    x(i) = x(i) + xoff + wmax
    lin = lin .OR. CheckPoint( x(i),y(i),MyPt,scale,MyRect )
  END DO
  IF( lin )THEN
    IF( iflag == 0 )THEN
      CALL gpl( n,x,y )
    ELSE
      CALL gfa( n,x,y )
    END IF
  END IF
END IF

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckPoint( x,y,MyPt,scale,MyRect )

USE SwinAPI

IMPLICIT NONE

TYPE( T_RECT ),  INTENT( IN  ) :: MyRect
REAL,            INTENT( IN  ) :: x,y
REAL,            INTENT( IN  ) :: scale
TYPE( T_POINT ), INTENT( OUT ) :: MyPt

INTEGER, EXTERNAL :: real2integer

MyPt%x = real2integer( scale*x )
MyPt%y = real2integer( scale*y )

CheckPoint = PtInRect(MyRect,MyPt) /= FALSE

RETURN
END

!==============================================================================

INTEGER FUNCTION real2integer( x )

REAL x

REAL, PARAMETER :: MAX_INT = 2.0**31-65.

REAL xx

xx = MAX(x ,-MAX_INT)
xx = MIN(xx, MAX_INT)

real2integer = INT(xx)

RETURN
END

!==============================================================================

SUBROUTINE write_symbol_wrap( x,y,xoff,yoff,MyRect,scale,iflag )

USE SwinAPI
USE mapwrap

IMPLICIT NONE

REAL    x,y
REAL    xoff,yoff
REAL    scale
INTEGER iflag

TYPE( T_RECT ) MyRect
TYPE( T_POINT ) MyPt

LOGICAL, EXTERNAL :: CheckPoint

REAL   xshft,xr(4),yr(4)

!==== Basic symbol

IF( CheckPoint(x,y,MyPt,scale,MyRect) )THEN
  xr(1) = x - xoff
  xr(2) = x + xoff
  xr(3) = xr(2)
  xr(4) = xr(1)
  yr(1) = y - yoff
  yr(2) = yr(1)
  yr(3) = y + yoff
  yr(4) = yr(3)
  IF( iflag == 0 )THEN
    CALL gfa( 4,xr,yr )
  ELSE
    CALL gfellips( xr,yr )
  END IF
END IF

!==== Wrap minus

xshft = 0.
IF( wmin /= 0. )THEN
  x = x + wmin
  IF( CheckPoint(x,y,MyPt,scale,MyRect) )THEN
    xr(1) = x - xoff
    xr(2) = x + xoff
    xr(3) = xr(2)
    xr(4) = xr(1)
    yr(1) = y - yoff
    yr(2) = yr(1)
    yr(3) = y + yoff
    yr(4) = yr(3)
    IF( iflag == 0 )THEN
      CALL gfa( 4,xr,yr )
    ELSE
      CALL gfellips( xr,yr )
    END IF
  END IF
  xshft = -wmin
END IF

!==== Wrap plus

IF( wmax /= 0. )THEN
  x = x + xshft + wmax
  IF( CheckPoint(x,y,MyPt,scale,MyRect) )THEN
    xr(1) = x - xoff
    xr(2) = x + xoff
    xr(3) = xr(2)
    xr(4) = xr(1)
    yr(1) = y - yoff
    yr(2) = yr(1)
    yr(3) = y + yoff
    yr(4) = yr(3)
    IF( iflag == 0 )THEN
      CALL gfa( 4,xr,yr )
    ELSE
      CALL gfellips( xr,yr )
    END IF
  END IF
END IF

RETURN
END

!==============================================================================

SUBROUTINE write_text_wrap( x,y,xoff,yoff,text,fsz,cent,MyRect,scale )

USE SwinAPI
USE mapwrap

IMPLICIT NONE

REAL    x,y
REAL    xoff,yoff,fsz,cent
REAL    scale
CHARACTER(*) text

TYPE( T_RECT ) MyRect
TYPE( T_POINT ) MyPt

LOGICAL lin

LOGICAL, EXTERNAL :: CheckPoint

REAL  xshft,xwid,xhgt,xx,yy

!==== Basic symbol

lin = CheckPoint( x,y,MyPt,scale,MyRect )
CALL GetNCARFontSize( x,y,text,fsz,0.,cent,xwid,xhgt )
IF( lin .AND. CheckPoint(x+xwid,y,MyPt,scale,MyRect) )THEN
  xx = x + xoff
  yy = y + yoff + 0.9*xhgt
  CALL plchhq( xx,yy,text,fsz,0.,cent )
END IF

!==== Wrap minus

xshft = 0.
IF( wmin /= 0. )THEN
  x = x + wmin
  lin = CheckPoint( x,y,MyPt,scale,MyRect )
  CALL GetNCARFontSize( x,y,text,fsz,0.,cent,xwid,xhgt )
  IF( lin .AND. CheckPoint(x+xwid,y,MyPt,scale,MyRect) )THEN
    xx = x + xoff
    yy = y + yoff + 0.9*xhgt
    CALL plchhq( xx,yy,text,fsz,0.,cent )
  END IF
  xshft = -wmin
END IF

!==== Wrap plus

IF( wmax /= 0. )THEN
  x = x + xshft + wmax
  lin = CheckPoint( x,y,MyPt,scale,MyRect )
  CALL GetNCARFontSize( x,y,text,fsz,0.,cent,xwid,xhgt )
  IF( lin .AND. CheckPoint(x+xwid,y,MyPt,scale,MyRect) )THEN
    xx = x + xoff
    yy = y + yoff + 0.9*xhgt
    CALL plchhq( xx,yy,text,fsz,0.,cent )
  END IF
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION MapRecordOffset( filename )

USE map_INDEX

! Locate the map data pointed to by filename, but now packed
! in a separate record within a unified file.  Return the offset value of
! the record in the packed file, and the packed-file name.
!
! INPUTS:
!    filename = (string) fully-qualified path to old-style map-data file
!               (e.g. F:\SCIP\BIN\MAPS\ne\e010n45.pp). The map quadrant,
!               lat/long coord, and data type are encoded in the file name.
! OUTPUTS:
!    filename = (string) fully-qualified path to new-style packed map-file.
!               (e.g. F:\SCIP\BIN\MAPS\ne\ne.pp) The old-style files are
!               concatenated into this file, for a given type (e.g. pp).
!
! RETURNS: MapRecordOffset (int) offset record number in output filename
!               containing data from input filename
!
! USES routines: SetMapFileINDEX, clower,cupper
!
! VERSION: Jan 8 1999, T.C. Stephens
!          Jan 18 1999 S. F. Parker - comments, case, save, parameters, blockdata
!-----------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(28), PARAMETER :: TYPES = 'po  apt rd  pp  ppt rr  nuc '
CHARACTER(28), PARAMETER :: QUADS = 'ne nw se sw '
CHARACTER(1),  PARAMETER :: FIRST_LETTER = 'A'
CHARACTER(1),  PARAMETER :: LAST_LETTER = 'Z'

CHARACTER(*) filename

INTEGER lon,lat,idot,i_quad,i_lat,i_lon,i_typ,nch
CHARACTER(260) targt
CHARACTER(3)   typ
CHARACTER(2)   quad

CHARACTER(260) string

LOGICAL UpperCase

!  Initialize return value

MapRecordOffset = 0

! If BROWSE file, simply return. [ Assumes Lower case (SFP) ]

string = filename
CALL clower( string )
IF( INDEX(filename,'browse.') > 0 )GOTO 9999

! Determine original file case - based on case of extension (SFP)

nch  = LEN_TRIM(string)
idot = 0
DO WHILE( nch >= 1 .AND. idot == 0 )
  IF( string(nch:nch) == '.' )idot = nch
  nch = nch - 1
END DO
IF( idot <= 0 .OR. idot >= LEN_TRIM(filename) )GOTO 9999

typ(1:1) = filename(idot+1:idot+1)
typ(2:2) = FIRST_LETTER
typ(3:3) = LAST_LETTER
UpperCase = ICHAR(typ(1:1)) >= ICHAR(typ(2:2)) .AND. &
            ICHAR(typ(1:1)) <= ICHAR(typ(3:3))

! Parse old map-data filename (e.g. F:\SCIP\BIN\MAPS\ne\e010n45.pp) to get type,
! and quadrant location particulars  [ Assumes lower case and that file names are of the
! form ..\qq\qxxxqxx.ext (SFP) ]

MapRecordOffset = -1
targt = string(idot-10:LEN_TRIM(string))
READ(targt,'(A2,2X,I3,X,I2,X,A3)',ERR=9999) quad,lon,lat,typ

! Build master INDEX into the concatenated map files (once per quad)

i_quad = (INDEX(QUADS,quad)-1)/3+1
CALL SetMapFileINDEX( string(:idot-8)//quad//'.idx',i_quad )

! Calculate indices into matrix of offsets; Get record offset

i_lon = lon/5
i_lat = lat/5
i_typ = (INDEX(TYPES,typ)-1)/4 + 1

MapRecordOffset = INDEX_maps(i_lon,i_lat,i_typ,i_quad)

! If map data exists, return filename containing concatenated data,
! (e.g. F:\SCIP\BIN\MAPS\ne\ne.pp)

IF( MapRecordOffset >= 0 )THEN
  targt = string(:idot-8)//quad//'.'//typ
  filename = targt
  IF( UpperCase )CALL cupper( TRIM(filename) )
ELSE
  MapRecordOffset = 0
END IF

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE SetMapFileINDEX( file_idx,i_quad )

USE files_fi
USE map_INDEX

! Read and interpret file file_idx to create a matrix containing record
! offset values of all the data in all packed map files for quadrant i_quad.
! The quadrant is INDEXed just once per session.
!
! INPUTS:
!    file_idx = (string) fully-qualified path to packed-map-data INDEX file.
!               (e.g. F:\SCIP\BIN\MAPS\ne\ne.idx)
!    i_quad = (int) INDEX of present quadrant (1-4)
!
! OUTPUTS:
!    /map_INDEX/INDEX_maps(i,j,k,l) = (int) offset record number of data for
!               longitude tile i, latitude tile j, data-type k, quadrant l.
!
! RETURNS: none
!
! USES routines: none
!
! VERSION: Jan 11 1999, T.C. Stephens
!          Jan 18 1999, S.F. Parker  comments, save, parameters, error checking
!          Jun 17 1999, S.F. Parker  Bug fix - Open INDEX READONLY to be able to
!                                    read from CD under Win NT
!-----------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(28), PARAMETER :: TYPES = 'po  apt rd  pp  ppt rr  nuc '

CHARACTER(*) file_idx

INTEGER i_quad,ios

INTEGER   lon,lat,ioff,i_lon,i_lat,i_typ
CHARACTER(3) typ

IF( init_quad(i_quad) )THEN
   init_quad(i_quad) = .FALSE.
   OPEN(UNIT=lun_tmp,FILE=file_idx,STATUS='OLD',ACTION="READ",IOSTAT=ios)
   IF( ios /= 0 )THEN !Old System
     GOTO 9999
   ELSE !New system
     DO WHILE( ios == 0 )
       READ(lun_tmp,'(X,I3,X,I2,X,A3,X,I8)',IOSTAT=ios) lon,lat,typ,ioff
       IF( ios /= 0 )EXIT
       CALL clower( typ )
       i_lon = lon/5
       i_lat = lat/5
       i_typ = (INDEX(TYPES,typ)-1)/4+1
       ioff  = ioff/rec_size(i_typ)
       INDEX_maps(i_lon,i_lat,i_typ,i_quad) = ioff
     END DO
     CLOSE( UNIT=lun_tmp,IOSTAT=ios )
   END IF
END IF

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE InitMapFileINDEX()

USE map_INDEX

! Initialize the following named common block:
!
! /MAP_INDEX/INDEX_MAPS(i,j,k,l) = (int) offset record number of data for
!           longitude tile i, latitude tile j, data-type k, quadrant l.
!
! VERSION: Jan 11 1999, T.C. Stephens
!          Jan 18 1999, S.F. Parker comments, blockdata, save
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER i,i1,i2,i3,i4

DO i = 1,4
  init_quad(i) = .TRUE.
END DO

DO i = 1,7
  rec_size(i) = 4
END DO
rec_size(2) = 64
rec_size(5) = 48
rec_size(7) = 64

DO i1 = 0,36
  DO i2 = 0,18
    DO i3 = 1,7
      DO i4 = 1,4
      INDEX_maps(i1,i2,i3,i4) = -1
    END DO
  END DO
  END DO
END DO

RETURN
END
