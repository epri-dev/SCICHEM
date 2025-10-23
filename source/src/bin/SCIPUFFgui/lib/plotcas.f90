!***********************************************************************
!               PlotCasualty
!***********************************************************************
SUBROUTINE plot_casualties( ltmp,lAVSplt,ASCfile )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE errorParam_fd
USE pltchoice_fi
USE SCIPtool

!     This is the SCIPUFF GUI interface to the plottri program

IMPLICIT NONE

LOGICAL ltmp !Print Flag
LOGICAL lAVSplt
CHARACTER(*) ASCfile

REAL, DIMENSION(:), ALLOCATABLE :: ClassDataArray
INTEGER nClassData

INTEGER userID

INTEGER   NumClassDataArray

INTEGER irv, ios

LOGICAL, EXTERNAL :: hasError

CHARACTER(128) eInform

userID = 9753

IF( .NOT.PlotDef(BASE_LEVEL)%Created )THEN

  CALL DeallocateTable()

  irv = SCIPDeleteField( userID, FieldID(FLD_INDEX) )

  nClassData = NumClassDataArray( PlotDef(BASE_LEVEL) )
  IF( nClassData < 0 )THEN
    CALL SetError( UK_ERROR,'Error getting ClassData size',' ',' ','PlotCasualty' )
    GOTO 9999
  END IF

  ALLOCATE( ClassDataArray(MAX(nClassData,1)),STAT=ios )
  IF( ios == 0 )THEN

    CALL SetClassDataArray( PlotDef(BASE_LEVEL),nClassData,ClassDataArray )
    IF( hasError()  )GOTO 9999

    irv = SCIPGetFieldTableSize( UserID,PlotDef(BASE_LEVEL)%Field,ClassDataArray,nTable,nCol,nRow )
    IF( irv /= SCIPsuccess )THEN
      CALL SetError( UK_ERROR,'Error getting table sizes',' ',' ','PlotCasualty' )
      GOTO 9999
    END IF

    CALL AllocateTable( nTable,nCol,nRow )
    IF( hasError() )GOTO 9999

    irv = SCIPGetFieldTable( UserID,PlotDef(BASE_LEVEL)%Field,ClassDataArray,TableTitle,ColTitle,RowTitle,Table )

    DEALLOCATE( ClassDataArray,STAT=ios )

    IF( irv /= SCIPsuccess )THEN
      CALL GetToolError( 'PlotCasualty' )
      GOTO 9999
    END IF

    PlotDef(BASE_LEVEL)%Created = .TRUE.
    PlotDef(EDIT_LEVEL)%Created = PlotDef(BASE_LEVEL)%Created
    PlotDef(EDIT_LEVEL)%Field%Units = PlotDef(DEFAULT_LEVEL)%Field%Units
    PlotDef(EDIT_LEVEL)%Field%Coordinate = PlotDef(DEFAULT_LEVEL)%Field%Coordinate
  ELSE
    WRITE(eInform,*)'Request=',nClassData,' : error =',ios
    CALL SetError( UK_ERROR,'Error allocating classData array', &
                   eInform,' ','PlotCasualty' )
    GOTO 9999
  END IF

END IF

IF( lAVSplt )THEN

!====== Write casualties to ASCII file

  CALL write_casualties( ASCfile )

ELSE

!====== Display

  CALL draw_casualties( ltmp )

END IF

9999  CONTINUE
IF( ALLOCATED(ClassDataArray) )DEALLOCATE( ClassDataArray,STAT=ios )

RETURN
END
!***********************************************************************
!               WriteCasualty
!***********************************************************************
SUBROUTINE write_casualties( ASCfile )

USE resource_fd
USE param_fd
USE contri_fi
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE pltchoice_fi
USE SwinAPI

IMPLICIT NONE

CHARACTER(*) ASCfile

LOGICAL CheckFile_NoError

INTEGER ios,i

CHARACTER(128) mainTitle,typeTitle,timeTitle

!==== Open file

IF( CheckFile_NoError(ASCfile) )THEN
  string1 = TRIM(ASCfile)//CHAR(0)
  ios     = DeleteFile(string1)
END IF
OPEN(UNIT=lun_tmp,FILE=ASCfile,STATUS='unknown',IOSTAT=ios)
IF( ios /= 0 )THEN
  RETURN
END IF

!==== Set up titles

CALL SetCasualtyTitles( mainTitle,typeTitle,timeTitle,.TRUE. )

!==== Write casualty info

WRITE(lun_tmp,'(A)',IOSTAT=ios)'    Project : '//TRIM(project(BASE_LEVEL)%ID%name)
WRITE(lun_tmp,'(A)',IOSTAT=ios)'    Path    : '//TRIM(project(BASE_LEVEL)%ID%path)
WRITE(lun_tmp,'(A)',IOSTAT=ios)'    Created : '//TRIM(project(BASE_LEVEL)%audit%CreateDate)
WRITE(lun_tmp,'(A)',IOSTAT=ios)'    Version : '//TRIM(project(BASE_LEVEL)%audit%Version)
WRITE(lun_tmp,'(A)',IOSTAT=ios)' '
WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(mainTitle)
WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(typeTitle)
IF( LEN_TRIM(timeTitle) > 0 )THEN
  WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(timeTitle)
END IF
IF( ltt3 )WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(ctt3)
IF( lttu )WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(cttu)
IF( lttl )WRITE(lun_tmp,'(A)',IOSTAT=ios)TRIM(cttl)

DO i = 1,nTable
  CALL WriteTable( lun_tmp,i )
END DO

!==== close file

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN
END
!***********************************************************************
!              DrawCasualty
!***********************************************************************
SUBROUTINE draw_casualties( ltmp )

USE resource_fd
USE param_fd
USE contri_fi
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi

IMPLICIT NONE
INTEGER, PARAMETER :: COLORSCHEME_NONE = 0
INTEGER, PARAMETER :: COLORSCHEME_1    = 1 !By column with table shift
INTEGER, PARAMETER :: COLORSCHEME_2    = 2 !By row w/o table shift
INTEGER, PARAMETER :: NPEN = 3

LOGICAL ltmp !Print Flag

REAL    xdum,xhgt,xx,yy,yyy
INTEGER ioffset,scheme,i

CHARACTER(128) mainTitle,typeTitle,timeTitle

!==== Initialize

CALL set( 0.,1.,0.,1.,0.,1.,0.,1.,1 )
string1 = 'Mg'
CALL GetNCARFontSize( 0.,0.,string1(1:2),1.,0.,0.,xdum,xhgt )

CALL loadpal()
CALL setpal( ltmp )
CALL ClearBrush()
CALL ClearPen()
CALL gsln( 0 )
CALL gslwsc( 0.36 )
CALL gsplci( 1 )

ioffset = ipalar - NPEN*2

!==== Set Table size

boxl = popt2def(BASE_LEVEL)%Size
xcb  = popt2def(BASE_LEVEL)%X
ycb  = popt2def(BASE_LEVEL)%Y
str  = popt2def(BASE_LEVEL)%AR
fsz  = popt2def(BASE_LEVEL)%Font

IF( xcb == DEF_VAL_R )xcb = 0.5

IF( ycb == DEF_VAL_R )ycb = 0.50
!old        ycb = 0.45

IF( boxl == DEF_VAL_R )boxl = 0.7
!old        boxl = 0.8

IF( str == DEF_VAL_R )str = 1.0
!old        str = 0.666666

!==== draw table space

scheme = COLORSCHEME_NONE

DO i = 1,nTable
  CALL DrawTable( xcb,ycb,boxl,str,fsz,i,ioffset,scheme )
END DO

!==== draw titles

CALL SetCasualtyTitles( mainTitle,typeTitle,timeTitle,.FALSE. )

xx = xcb
yy = ycb + 0.5*boxl*str + 0.33333*xhgt*fsz*1.2
yy = yy + 1.2*xhgt*fsz
IF( LEN_TRIM(timeTitle) > 0)yy = yy + 1.2*xhgt*fsz
IF( lttl .AND. yttl == default)yy = yy + xhgt*fsz
IF( lttu .AND. yttu == default)yy = yy + xhgt*fsz
IF( ltt3 .AND. ytt3 == default)yy = yy + xhgt*fsz

CALL gstxci( 1 )

CALL plchhq( xx,yy,mainTitle(1:LEN_TRIM(mainTitle)),1.2*fsz,0.,0. )

yy = yy - xhgt*fsz*1.2
CALL plchhq( xx,yy,typeTitle(1:LEN_TRIM(typeTitle)),1.2*fsz,0.,0. )

IF( LEN_TRIM(timeTitle) > 0 )THEN
  yy = yy - xhgt*fsz*1.2
  CALL plchhq( xx,yy,timeTitle(1:LEN_TRIM(timeTitle)),1.2*fsz,0.,0. )
END IF

yy = yy - 0.2*xhgt*fsz
IF( ltt3 )THEN
  string1 = TRIM(ctt3)
  IF( ytt3 == default )THEN
    yy  = yy - 1.0*xhgt*fsz
    yyy = yy
  ELSE
    yyy = ytt3
  END IF
  IF( xtt3 == default )THEN
    xx = xcb
  ELSE
    xx = xtt3
  END IF
  CALL plchhq( xx,yyy,string1(1:LEN_TRIM(string1)),fsz,0.,0. )
END IF

IF( lttu )THEN
  string1 = TRIM(cttu)
  IF( yttu == default )THEN
    yy  = yy - 1.0*xhgt*fsz
    yyy = yy
  ELSE
    yyy = yttu
  END IF
  IF( xttu == default )THEN
    xx = xcb
  ELSE
    xx = xttu
  END IF
  CALL plchhq( xx,yyy,string1(1:LEN_TRIM(string1)),fsz,0.,0. )
END IF

IF( lttl )THEN
  string1 = TRIM(cttl)
  IF( yttl == default )THEN
    yy  = yy - 1.0*xhgt*fsz
    yyy = yy
  ELSE
    yyy = yttl
  END IF
  IF( xttl == default )THEN
    xx = xcb
  ELSE
    xx = xttl
  END IF
  CALL plchhq( xx,yyy,string1(1:LEN_TRIM(string1)),fsz,0.,0. )
END IF

RETURN
END

!===============================================================================

SUBROUTINE DrawTable( xcb,ycb,boxl,str,fsz,itab,pen,scheme )

USE resource_fd
USE pltchoice_fi

IMPLICIT NONE
INTEGER, PARAMETER :: CELL_NC = 14
INTEGER, PARAMETER :: LAB_NC  = 35
INTEGER, PARAMETER :: COLORSCHEME_NONE = 0
INTEGER, PARAMETER :: COLORSCHEME_1    = 1 !By column with table shift
INTEGER, PARAMETER :: COLORSCHEME_2    = 2 !By row w/o table shift
INTEGER, PARAMETER :: NPEN = 3

REAL, PARAMETER :: CAR = 0.25
REAL, PARAMETER :: TSP = 2.0      !Title spacing (lines)
!old      parameter (TSP=0.5)

REAL, PARAMETER :: YSHFT = 0.3333
REAL, PARAMETER :: XSHFT = 0.5

INTEGER, PARAMETER :: MAX_TABLE = 3

REAL, PARAMETER :: LABFAC = 1.25 !Label box size fac*cellsize
!old      parameter (LABFAC=2.0)

INTEGER, PARAMETER :: MAX_DRAW = 5

REAL    xcb,ycb,boxl,str,fsz,ffsz
INTEGER itab,flag,pen,scheme

CHARACTER(64) tmpString

REAL xp(5),yp(5),xdum,xhgt,frac

REAL xo,yo,xsz,ysz,xx,yy,vsz,cxsz,cysz,xspace,ffac

INTEGER i,j,nch,idata,icolor,ioff,stopRow,numDrawCol,icol,jrow
INTEGER drawCol(MAX_DRAW), maxTable, drawRow(MAX_DRAW)

!==== Determine actual rows/columns to draw - to first row with no title

IF( nCol <= MAX_DRAW )THEN
  numDrawCol = nCol
  DO i = 1,nCol
    drawCol(i) = i
  END DO
ELSE
  numDrawCol = MAX_DRAW
  drawCol(1) = 3
  drawCol(2) = 6
  drawCol(3) = 7
  drawCol(4) = 8
  drawCol(5) = 9
END IF

stopRow = 0
DO i = 1,nRow
  IF( LEN_TRIM(rowTitle(i)%string) > 0 )THEN
    stopRow = MIN(stopRow+1,MAX_DRAW)
    drawRow(stopRow) = i
  END IF
END DO

!==== Set size/location parameters

flag = itab - 1

maxTable = MAX_TABLE
!IF( stopRow > 4 )THEN
!  IF( nTable > 1 )THEN
!    ffsz = 3.*fsz/FLOAT(stopRow*nTable)
!  ELSE
!    ffsz = 0.8*fsz
!    maxTable = 1
!  END IF
!ELSE
  ffsz = 1.0 - 0.1*(stopRow-MIN(stopRow,3))
  ffsz = ffsz*fsz
!END IF

tmpString = 'Mg'
CALL GetNCARFontSize( 0.,0.,TRIM(tmpString),1.,0.,0.,xdum,xhgt )
ioff = 0

xsz  = boxl
cxsz = xsz/(FLOAT(numDrawCol)+LABFAC)
xo   = xcb - 0.5*xsz

vsz = boxl*str/FLOAT(maxTable)
frac = 1. - (1.+TSP)*xhgt*ffsz/vsz
ysz = MIN(frac*vsz,CAR*cxsz*FLOAT(stopRow+1))
cysz = ysz/FLOAT(stopRow+1)
yo   = ycb + 0.5*FLOAT(maxTable)*ysz
!old      if(flag .ne. 0)then                                     !Lower table
yo   = yo - flag*vsz
ioff = ioff + NPEN*flag
!old      end if

!==== Draw table grid

xp(1) = xo + cxsz*LABFAC
yp(1) = yo
xp(2) = xp(1)
yp(2) = yo - ysz
xp(3) = xo + xsz
yp(3) = yp(2)
xp(4) = xp(3)
yp(4) = yp(1)
xp(5) = xp(1)
yp(5) = yp(1)
CALL gpl( 5,xp,yp )

xp(1) = xo
yp(1) = yo - cysz
xp(2) = xp(1)
yp(2) = yo - ysz
xp(3) = xo + xsz
yp(3) = yp(2)
xp(4) = xp(3)
yp(4) = yp(1)
xp(5) = xp(1)
yp(5) = yp(1)
CALL gpl( 5,xp,yp )

yp(1) = yo - ysz
yp(2) = yo
DO i = 2,numDrawCol
  xp(1) = xo + (i-1)*cxsz + LABFAC*cxsz
  xp(2) = xp(1)
  CALL gpl( 2,xp,yp )
END DO

xp(1) = xo
xp(2) = xo + xsz
DO i = 2,stopRow
  yp(1) = yo - i*cysz
  yp(2) = yp(1)
  CALL gpl( 2,xp,yp )
END DO

!==== Draw table title

CALL gstxci( 1 )

xx = xo + LABFAC*cxsz + 0.5*FLOAT(numDrawCol)*cxsz
!      yy = yo + 0.5*TSP*ffsz*xhgt
yy = yo + 0.25*fsz*xhgt
CALL plchhq( xx,yy,TableTitle(itab)%string(1:LEN_TRIM(TableTitle(itab)%string)),fsz,0.,0. )

!==== Draw row labels

CALL gstxci(1)

xx = xo + LABFAC*cxsz - XSHFT*ffsz*xhgt
yy = yo - 0.5*cysz - YSHFT*ffsz*xhgt
DO jrow = 1,stopRow
  j = drawRow(jrow)
  IF( scheme == COLORSCHEME_2 )THEN
    IF( pen >= 0 )THEN
      icolor = pen+jrow
    ELSE
      icolor = 1
    END IF
    CALL gstxci(icolor)
  END IF
  yy = yy - cysz
!  IF( LEN_TRIM(RowTitle(j)%string) <= 8 )THEN
  IF( LEN_TRIM(RowTitle(j)%string) <= 20 )THEN
    ffac = 1.
!  ELSE IF( LEN_TRIM(RowTitle(j)%string) <= 16 )THEN
!    ffac = 0.8
  ELSE
!    ffac = 0.5
    ffac = 20.0/LEN_TRIM(RowTitle(j)%string)
  END IF
  CALL plchhq( xx,yy,RowTitle(j)%string(1:LEN_TRIM(RowTitle(j)%string)),ffac*ffsz,0.,1. )
END DO

!==== Draw column labels

CALL gstxci( 1 )

xx = xo + LABFAC*cxsz - 0.5*cxsz
yy = yo - 0.5*cysz - YSHFT*ffsz*xhgt
DO icol = 1,numDrawCol
  i = drawCol(icol)
  IF( scheme == COLORSCHEME_1 )THEN
    IF( pen >= 0 )THEN
      icolor = pen+icol+ioff
    ELSE
      icolor = 1
    END IF
    CALL gstxci( icolor )
  END IF
  xx = xx + cxsz
  IF( LEN_TRIM(ColTitle(i)%string) <= 8 )THEN
    ffac = 1.
  ELSE IF( LEN_TRIM(ColTitle(i)%string) <= 16 )THEN
    ffac = 0.8
  ELSE
    ffac = 0.5
  END IF
  CALL plchhq( xx,yy,ColTitle(i)%string(1:LEN_TRIM(ColTitle(i)%string)),ffac*ffsz,0.,0. )
END DO

!==== Draw data values

xspace = 0.0
DO icol = 1,numDrawCol
  i = drawCol(icol)
  DO jrow = 1,stopRow
    j = drawRow(jrow)
    idata = Table(i,j,iTab)
    CALL i_format( idata,nch,tmpString )
    tmpString = ADJUSTL(tmpString)
    CALL GetNCARFontSize( 0.,0.,TRIM(tmpString),1.,0.,0.,xdum,xhgt )
    xspace = MAX(xspace,xdum)
  END DO
END DO
xspace = 0.5*MAX(0.0,cxsz-xspace*ffsz)

CALL gstxci( 1 )

DO icol = 1,numDrawCol
  i = drawCol(icol)
  xx = xo + LABFAC*cxsz + icol*cxsz - xspace
  IF( scheme == COLORSCHEME_1 )THEN
    IF( pen >= 0 )THEN
      icolor = pen+icol+ioff
    ELSE
      icolor = 1
    END IF
    CALL gstxci( icolor )
  END IF
  DO jrow = 1,stopRow
    j = drawRow(jrow)
    IF( scheme == COLORSCHEME_2 )THEN
      IF( pen >= 0 )THEN
        icolor = pen+jrow
      ELSE
        icolor = 1
      END IF
      CALL gstxci( icolor )
    END IF
    yy = yo - 0.5*cysz - jrow*cysz - YSHFT*ffsz*xhgt
    idata = Table(i,j,itab)
    CALL i_format( idata,nch,tmpString )
    tmpString = ADJUSTL(tmpString)
    IF( LEN_TRIM(tmpString) <= 8 )THEN
      ffac = 1.
    ELSE IF( LEN_TRIM(tmpString) <= 16 )THEN
      ffac = 0.8
    ELSE
      ffac = 0.5
    END IF
    CALL plchhq( xx,yy,tmpString(1:LEN_TRIM(tmpString)),ffac*ffsz,0.,1. )
  END DO
END DO

RETURN
END
!===============================================================================

SUBROUTINE WriteTable( lun,itab )

USE resource_fd
USE pltchoice_fi

IMPLICIT NONE
INTEGER, PARAMETER :: CELL_NC = 14
INTEGER, PARAMETER :: LAB_NC  = 35
INTEGER, PARAMETER :: IPROMPT  = 1
INTEGER, PARAMETER :: ITOTAL   = 3
INTEGER, PARAMETER :: IFALLOUT = 2
INTEGER, PARAMETER :: COLORSCHEME_NONE = 0
INTEGER, PARAMETER :: COLORSCHEME_1    = 1 !By column with table shift
INTEGER, PARAMETER :: COLORSCHEME_2    = 2 !By row w/o table shift
INTEGER, PARAMETER :: NPEN = 3

INTEGER lun

CHARACTER(256)  tmpString
CHARACTER(64)  fmt

INTEGER i,j,idata,ios,nch,noff,itab,nr,stopRow

!==== Determine actual rows to draw - to first row with no title
stopRow = 1
DO i = 1,nRow
  IF( LEN_TRIM(rowTitle(i)%string) <= 0 )EXIT
  stopRow = i
END DO

!==== write table header

noff = LAB_NC + (nCol*CELL_NC)/2

WRITE(lun,'(A)',IOSTAT=ios)' '
nch = MAX(0,noff - LEN_TRIM(TableTitle(itab)%string)/2)
IF( nch > 0 )THEN
  WRITE(lun,'(A)',IOSTAT=ios)REPEAT(' ',nch)//TRIM(TableTitle(itab)%string)
ELSE
  WRITE(lun,'(A)',IOSTAT=ios)TRIM(TableTitle(itab)%string)
END IF
WRITE(lun,'(A)',IOSTAT=ios)' '
tmpString = REPEAT(' ',LAB_NC)
nr = LAB_NC + 1
DO i = 1,nCol
  j = (CELL_NC - LEN_TRIM(ColTitle(i)%string))/2
  IF( j > 0 )THEN
    tmpString(nr:) = REPEAT(' ',j)//TRIM(ColTitle(i)%string)
  ELSE
    tmpString(nr:) = TRIM(ColTitle(i)%string)
  END IF
  nr = nr + CELL_NC
END DO
WRITE(lun,'(A)',IOSTAT=ios)tmpString(1:nr)
nr = LAB_NC + 1
DO i = 1,nCol
  tmpString(nr:) = '  '//REPEAT('=',CELL_NC-4)//'  '
  nr = nr + CELL_NC
END DO
WRITE(lun,'(A)',IOSTAT=ios)tmpString(1:nr)
DO i = 1,stopRow
  nch = MAX(0,LAB_NC - LEN_TRIM(RowTitle(i)%string)-2)
  IF( nch > 0 )THEN
    tmpString = REPEAT(' ',nch)//TRIM(RowTitle(i)%string)//'  '
    nr = nch + LEN_TRIM(RowTitle(i)%string) + 3
  ELSE
    tmpString = TRIM(RowTitle(i)%string)
    nr = LEN_TRIM(RowTitle(i)%string) + 1
  END IF
  DO j = 1,nCol
    idata = Table(j,i,itab)
    CALL i_format( idata,nch,fmt )
    fmt = ADJUSTL(fmt)
    nch = MAX(0,CELL_NC - 2 - LEN_TRIM(fmt))
    fmt = REPEAT(' ',nch)//TRIM(fmt)
    tmpString(nr:) = TRIM(fmt)
    nr = nr + CELL_NC
  END DO
  WRITE(lun,'(A)',IOSTAT=ios)tmpString
END DO
WRITE(lun,'(A)',IOSTAT=ios)' '

RETURN
END
!***********************************************************************
!              SetCasualtyTitles
!***********************************************************************
SUBROUTINE SetCasualtyTitles( mainTitle,typeTitle,timeTitle,flag )

USE resource_fd
USE contri_fi
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi
USE dialog_fi

IMPLICIT NONE

INTEGER, PARAMETER :: LAB_NC  = 35
INTEGER, PARAMETER :: CELL_NC = 14

LOGICAL flag,ldum !Write Flag
CHARACTER(*) mainTitle,typeTitle,timeTitle

INTEGER nch,noff

TYPE( TimeT ) tim

!==== CONTRI labels - User input

cttl = TRIM(ttldef(BASE_LEVEL)%string(3))
cttu = TRIM(ttldef(BASE_LEVEL)%string(2))
ctt3 = TRIM(ttldef(BASE_LEVEL)%string(1))

lttl = ttldef(BASE_LEVEL)%Show(3) .AND. TRIM(cttl) /= 'default'
lttu = ttldef(BASE_LEVEL)%Show(2) .AND. TRIM(cttu) /= 'default'
ltt3 = ttldef(BASE_LEVEL)%Show(1) .AND. TRIM(ctt3) /= 'default'

xttl = ttldef(BASE_LEVEL)%X(3)
xttu = ttldef(BASE_LEVEL)%X(2)
xtt3 = ttldef(BASE_LEVEL)%X(1)

yttl = ttldef(BASE_LEVEL)%Y(3)
yttu = ttldef(BASE_LEVEL)%Y(2)
ytt3 = ttldef(BASE_LEVEL)%Y(1)

!==== Time Title

IF(ttldef(BASE_LEVEL)%ShowDate)THEN
  IF(nTimePlot > 0)THEN
    tim         = timePlot(PlotDef(BASE_LEVEL)%Field%TimeID)%time
    tim%runTime = PlotDef(BASE_LEVEL)%Field%UserTime
    CALL ComputeEndTime( dlgTime(BASE_LEVEL)%time%start%time,tim,ldum )
    CALL time_string( tim,timeTitle )
  ELSE
    timeTitle = ' '
  END IF
ELSE
  timeTitle = ' '
END IF

IF(ttldef(BASE_LEVEL)%ShowTime)THEN
  IF(nTimePlot > 0)THEN
    CALL format_time( PlotDef(BASE_LEVEL)%Field%UserTime,string1,0 )
    IF(ttldef(BASE_LEVEL)%ShowDate)THEN
      string2 = TRIM(timeTitle)//' ('//TRIM(string1)//')'
      timeTitle = TRIM(string2)
    ELSE
      timeTitle = 'T = '//TRIM(string1)
    END IF
  END IF
END IF

!==== Main Title

mainTitle= TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string)

!==== Type Title

typeTitle = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)

!==== Adjust Written titles

IF( flag )THEN

  noff = LAB_NC + (nCol*CELL_NC)/2

  IF( lttl )THEN
    nch = MAX(0,noff - LEN_TRIM(cttl)/2)
    IF( nch > 0 )THEN
      cttl = REPEAT(' ',nch)//TRIM(cttl)
    END IF
  END IF

  IF( lttu )THEN
    nch = MAX(0,noff - LEN_TRIM(cttu)/2)
    IF( nch > 0 )THEN
      cttu = REPEAT(' ',nch)//TRIM(cttu)
    END IF
  END IF

  IF( ltt3 )THEN
    nch = MAX(0,noff - LEN_TRIM(ctt3)/2)
    IF( nch > 0 )THEN
      ctt3 = REPEAT(' ',nch)//TRIM(ctt3)
    END IF
  END IF

  IF( LEN_TRIM(timeTitle) > 0 )THEN
    nch = MAX(0,noff - LEN_TRIM(timeTitle)/2)
    IF( nch > 0 )THEN
      timeTitle = REPEAT(' ',nch)//TRIM(timeTitle)
    END IF
  END IF

  nch = MAX(0,noff - LEN_TRIM(mainTitle)/2)
  IF( nch > 0 )THEN
    mainTitle = REPEAT(' ',nch)//TRIM(mainTitle)
  END IF

  nch = MAX(0,noff - LEN_TRIM(typeTitle)/2)
  IF( nch > 0 )THEN
    typeTitle = REPEAT(' ',nch)//TRIM(typeTitle)
  END IF

END IF

RETURN
END
!******************************************************************************
!******************************************************************************
! DeallocateTable
!******************************************************************************
!******************************************************************************
SUBROUTINE DeallocateTable()

USE pltchoice_fi

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(TableTitle) )DEALLOCATE( TableTitle,STAT=ios )
IF( ALLOCATED(ColTitle)   )DEALLOCATE( ColTitle  ,STAT=ios )
IF( ALLOCATED(RowTitle)   )DEALLOCATE( RowTitle  ,STAT=ios )
IF( ALLOCATED(Table)      )DEALLOCATE( Table     ,STAT=ios )

RETURN
END
!******************************************************************************
!******************************************************************************
! AllocateTable
!******************************************************************************
!******************************************************************************
SUBROUTINE AllocateTable( mTable,mCol,mRow )

USE pltchoice_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER mTable
INTEGER mCol
INTEGER mRow

INTEGER ios

CHARACTER(24) string
INTEGER       na
INTEGER       i

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

CALL DeallocateTable

IF( mTable <= 0 )GOTO 9998
IF( mCol   <= 0 )GOTO 9998
IF( mRow   <= 0 )GOTO 9998

ALLOCATE( TableTitle(mTable),STAT=ios )
IF( ios /= 0 )THEN
  string = 'TableTitle'
  na     = mTable
  GOTO 9997
END IF
DO i = 1,mTable
  TableTitle(i)%string = 'Testing'
END DO

ALLOCATE( ColTitle(mCol),STAT=ios )
IF( ios /= 0 )THEN
  string = 'ColTitle'
  na     = mCol
  GOTO 9997
END IF
DO i = 1,mCol
  ColTitle(i)%string = 'Column'
END DO

ALLOCATE( RowTitle(mRow),STAT=ios )
IF( ios /= 0 )THEN
  string = 'RowTitle'
  na     = mRow
  GOTO 9997
END IF
DO i = 1,mRow
  RowTitle(i)%string = 'Row'
END DO

ALLOCATE( Table(mCol,mRow,mTable),STAT=ios )
IF( ios /= 0 )THEN
  string = 'Table'
  na     = mCol*mRow*mTable
  GOTO 9997
END IF
Table = 0

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = SZ_ERROR
eRoutine = 'AllocateTable'
eMessage = 'Invalid table array sizes'
WRITE(string,*)mTable
eInform = 'nTable='//TRIM(ADJUSTL(string))
WRITE(string,*)mRow
eInform = TRIM(eInform)//' : nRow='//TRIM(ADJUSTL(string))
WRITE(string,*)mCol
eInform = TRIM(eInform)//' : nCol='//TRIM(ADJUSTL(string))
eAction = ' '
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9999

9997 CONTINUE
nError   = SZ_ERROR
eRoutine = 'AllocateTable'
eMessage = 'Error allocating '//TRIM(string)//' array'
WRITE(string,*)ios
eInform = 'IOS='//TRIM(ADJUSTL(string))
WRITE(string,*)na
eInform = TRIM(eInform)//' : Size='//TRIM(ADJUSTL(string))
eAction = ' '
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9999

END
