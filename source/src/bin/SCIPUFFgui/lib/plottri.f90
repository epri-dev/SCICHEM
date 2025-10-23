!***********************************************************************
!               PlotTri
!***********************************************************************
SUBROUTINE plottri( ltmp,lAVSplt,lCTSplt,lOVLplt,lMAPovl,ASCfile )

USE resource_fd
USE SCIAPIversion_fd
USE contri_fi
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE GUImatl_fi
USE coordinate_fd
USE errorParam_fd
USE files_fi
USE pltchoice_fi
USE write_fd
USE PlotTrans_fi
USE SwinAPI
USE AreaMode_fd
USE datums
USE charT_fd
USE SCIPtool

!     This is the SCIPUFF GUI interface to the plottri program

IMPLICIT NONE

LOGICAL ltmp !Print Flag
LOGICAL lAVSplt !AVS Plot Flag
LOGICAL lCTSplt !CTS Plot Flag
LOGICAL lOVLplt !Overlay Plot Flag
LOGICAL lMAPovl !Overlay Coordinate Flag
CHARACTER(*) ASCfile

CHARACTER(80) cunit_ttl,exceed_ttl,tstring1,tstring2

INTEGER zref
INTEGER iversion
INTEGER i,irv,ios
INTEGER ContourIndex
INTEGER UserID
INTEGER iCl, iCh, nPlots, nLines, nPoints, j
REAL    dtmp(6),ptmp(6),rtmp(4)
REAL    lon0,lat0,x0,y0
REAL    ddmn,ddmx
LOGICAL LatLon,Cartesian
LOGICAL lextra,lrmk,lrv,expectOK
LOGICAL NeedTerrain

REAL, DIMENSION(:), ALLOCATABLE :: ClassDataArray
INTEGER nClassData

TYPE (messageT) ToolError
TYPE( SCIPPointT ), DIMENSION(:), ALLOCATABLE :: AssocPoints
TYPE( SCIPLineT  ), DIMENSION(:), ALLOCATABLE :: AssocLines
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocTitles
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocAxes
TYPE( char64T    ), DIMENSION(:), ALLOCATABLE :: AssocLineIDs

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

INTEGER, EXTERNAL :: NumClassDataArray
LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'Plottri'

!---- Setup Default Titles/Axis Labels based on Interface selections
!     and save appropriate info

!---- SCIPUFF version determines surface file type

UserID = 8642

CALL read_version_string( iversion,project(BASE_LEVEL)%audit%Version )
IF( iversion >= SCIAPI_32_VERSION )THEN
  srf_file_type = 2
ELSE
  srf_file_type = 1
END IF

IF( .NOT.PlayBack )THEN
  irv = SetCursor( hcur_wait ) !  Set Arrow
  irv = SetCapture( hwnd_db ) !Capture mouse
END IF

IF( PlotDef(BASE_LEVEL)%Field%Category == HP_TABLE )THEN
  GOTO 9999
END IF

!---- Flags

popden = mapdef(BASE_LEVEL)%popden
nfnc = 0
laux = .FALSE.
lspv = .TRUE.
spvl = LOG(spv)
logtrp = .TRUE.
CALL EnablePick()

IF( CatClassArray(PlotDef(BASE_LEVEL)%Field%Category,PlotDef(BASE_LEVEL)%Field%Class)%type == SCIPtrue )THEN
  IF( PlotDef(BASE_LEVEL)%Type == HP_MEAN )THEN
    ContourIndex = PlotDef(BASE_LEVEL)%ContourIndex
  ELSE IF( PlotDef(BASE_LEVEL)%Type == HP_EXCEED )THEN
    ContourIndex = PlotDef(BASE_LEVEL)%ContourIndex
  ELSE IF( PlotDef(BASE_LEVEL)%Type == HP_PROB )THEN
    ContourIndex = USER_CONTOUR
  ELSE
    ContourIndex = USER_CONTOUR
  END IF
  expectOK = true
ELSE
  IF( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == &
                                              'NWPN Prob Casualty' )THEN
    ContourIndex = USER_CONTOUR
  ELSE IF(TRIM( ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == &
                                                   'Met/Terrain' )THEN
    ContourIndex = USER_CONTOUR
  ELSE
    ContourIndex = PlotDef(BASE_LEVEL)%ContourIndex
  END IF
  expectOK = false
END IF

IF( ContourIndex /= USER_CONTOUR )THEN
  IF( ContourList(ContourIndex,BASE_LEVEL)%ListHdr%Number <= 0 )THEN
    ContourIndex = PlotDef(DEFAULT_LEVEL)%ContourIndex
  END IF
END IF

IF( BTEST(PlotDef(BASE_LEVEL)%Mode,BPLT_FILL) )THEN
  PlotDraw%FillContour = SCIPtrue
ELSE
  PlotDraw%FillContour = SCIPfalse
END IF

IF( BTEST(PlotDef(BASE_LEVEL)%Mode,BPLT_DRAW) )THEN
  PlotDraw%DrawContour = SCIPtrue
ELSE
  PlotDraw%DrawContour = SCIPfalse
END IF

IF( poptdef(BASE_LEVEL)%FillLo )THEN
  PlotDraw%Fill_Lo = SCIPtrue
ELSE
  PlotDraw%Fill_Lo = SCIPfalse
END IF

IF( poptdef(BASE_LEVEL)%FillHi )THEN
  PlotDraw%Fill_Hi = SCIPtrue
ELSE
  PlotDraw%Fill_Hi = SCIPfalse
END IF

time = PlotDef(BASE_LEVEL)%Field%UserTime

!---- Local Coordinates

CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord,0,lon0,lat0,x0,y0,zref )
xclc = x0
yclc = y0
xolc = lon0
yolc = lat0

IF( axesdef(BASE_LEVEL)%MapCoord == I_CARTESIAN )THEN
  llc = .TRUE.
  zone = 0
ELSE IF( axesdef(BASE_LEVEL)%MapCoord == I_UTM )THEN
  llc = .TRUE.
  zone = zref
ELSE
  llc = .FALSE.
  zone = 0
END IF

clbx = TRIM(axesdef(BASE_LEVEL)%Xlab)
clby = TRIM(axesdef(BASE_LEVEL)%Ylab)
cfx  = TRIM(axesdef(BASE_LEVEL)%Xformat)
cfy  = TRIM(axesdef(BASE_LEVEL)%Yformat)

llbx = axesdef(BASE_LEVEL)%ShowX
llby = axesdef(BASE_LEVEL)%ShowY
llab = llbx .OR. llby

IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_HINT )THEN
  ntx = axesdef(BASE_LEVEL)%TicH
  nty = axesdef(BASE_LEVEL)%TicV
ELSE
  ntx = axesdef(BASE_LEVEL)%TicX
  nty = axesdef(BASE_LEVEL)%TicY
END IF

!---- Map Drawing

lmapon = mapdef(BASE_LEVEL)%MapsOn .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_VSLICE
lmapon = lmapon .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_HINT

IF( lmapon )THEN
  lus    = mapdef(BASE_LEVEL)%HiRes
  lmptx  = mapdef(BASE_LEVEL)%PPText
  lmptxp = mapdef(BASE_LEVEL)%PPTextLo
  lmppp  = mapdef(BASE_LEVEL)%PPArea
  lmppps = mapdef(BASE_LEVEL)%PPSymbol
  lmprd  = mapdef(BASE_LEVEL)%Roads
  lmprr  = mapdef(BASE_LEVEL)%RRs
  lmpaf  = mapdef(BASE_LEVEL)%Airport
  lmpat  = mapdef(BASE_LEVEL)%AirportText
  lmpnf  = mapdef(BASE_LEVEL)%NucFac
  lmpnt  = mapdef(BASE_LEVEL)%NucFacText
END IF

!---- Population Density

!---- Plot Location and Size

lcel  = poptdef(BASE_LEVEL)%Cell
lmax  = poptdef(BASE_LEVEL)%Max .AND. (PlotDef(BASE_LEVEL)%Type <= HP_NUMTYP)
IF( lmax )THEN
  nmax = 1
ELSE
  nmax = 0
END IF

llbc  = poptdef(BASE_LEVEL)%Contour
llbb  = poptdef(BASE_LEVEL)%ColorBox
lpts  = poptdef(BASE_LEVEL)%Source .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_VSLICE
lpts  = lpts .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_HINT
lpts_txt  = poptdef(BASE_LEVEL)%SrcText .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_VSLICE
lpts_txt  = lpts_txt .AND. PlotDef(BASE_LEVEL)%Field%Category /= HP_HINT
llns  = .FALSE.
lkmb  = poptdef(BASE_LEVEL)%KmScale .AND. llc
lwxs  = poptdef(BASE_LEVEL)%Weather .AND. (project(BASE_LEVEL)%Weather > 0)
lovly = poptdef(BASE_LEVEL)%Overlay
covly = TRIM(poptdef(BASE_LEVEL)%OverlayFile)

IF( PlotDef(BASE_LEVEL)%Created )THEN
  IF( PlotDef(BASE_LEVEL)%Field%resolution < 0 )THEN                             !Not currently limited
    IF( PlotDef(BASE_LEVEL)%Field%fldLev > ABS(poptdef(BASE_LEVEL)%Maxlev) )THEN
      PlotDef(BASE_LEVEL)%Field%maxLev = ABS(poptdef(BASE_LEVEL)%Maxlev)
      PlotDef(BASE_LEVEL)%Created = .FALSE.
    ENDIF
  ELSE                                                                          !Field was limited
    IF( PlotDef(BASE_LEVEL)%Field%fldLev < PlotDef(BASE_LEVEL)%Field%maxLev)THEN  !Not user limited
      IF( PlotDef(BASE_LEVEL)%Field%fldLev > ABS(poptdef(BASE_LEVEL)%Maxlev) )THEN
        PlotDef(BASE_LEVEL)%Field%maxLev = ABS(poptdef(BASE_LEVEL)%Maxlev)
        PlotDef(BASE_LEVEL)%Created = .FALSE.
      END IF
    ELSE                                                                        !User limited
      IF( PlotDef(BASE_LEVEL)%Field%fldLev /= ABS(poptdef(BASE_LEVEL)%Maxlev) )THEN
        PlotDef(BASE_LEVEL)%Field%maxLev = ABS(poptdef(BASE_LEVEL)%Maxlev)
        PlotDef(BASE_LEVEL)%Created = .FALSE.
      END IF
    END IF
  END IF
ELSE
  PlotDef(BASE_LEVEL)%Field%maxLev = ABS(poptdef(BASE_LEVEL)%Maxlev)
END IF
PlotDef(EDIT_LEVEL)%Field%maxLev = PlotDef(BASE_LEVEL)%Field%maxLev

boxl = popt2def(BASE_LEVEL)%Size
xcb  = popt2def(BASE_LEVEL)%X
ycb  = popt2def(BASE_LEVEL)%Y
str  = popt2def(BASE_LEVEL)%AR
fsz  = popt2def(BASE_LEVEL)%Font

IF( xcb == DEF_VAL_R )THEN
  IF( (PlotDraw%FillContour==SCIPtrue) .AND. llbb )THEN
    IF( nfnc >= 7 .AND. nfnc <= 9 )THEN
      xcb = 0.48
    ELSE
      xcb = 0.45
    END IF
  ELSE
    xcb = 0.50
  END IF
END IF

IF( ycb == DEF_VAL_R )THEN
  IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
      PlotDef(BASE_LEVEL)%Field%Category == HP_HINT )THEN
    ycb = 0.53
  ELSE
    ycb = 0.51
    IF( popden /= 0. .AND. (PlotDraw%FillContour==SCIPtrue) )ycb = ycb + 0.02
  END IF
END IF

IF( boxl == DEF_VAL_R )THEN
  IF( (PlotDraw%FillContour==SCIPtrue) .AND. llbb )THEN
    boxl = 0.63
  ELSE
    boxl = 0.68
  END IF
END IF

IF( str == DEF_VAL_R )THEN
  IF( (PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
       PlotDef(BASE_LEVEL)%Field%Category == HP_HINT) .AND..NOT. llc )str = 0.5
END IF

!==============================================================================
!Build Field if Needed
!==============================================================================

IF( .NOT.PlotDef(BASE_LEVEL)%Created )THEN

  irv = SCIPDeleteField( userID,FieldID(FLD_INDEX) )

  nClassData = NumClassDataArray( PlotDef(BASE_LEVEL) )
  IF( nClassData == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'PlotTri'
    eMessage = 'Error setting classData array'
    eInform  = 'Plot definition not supported in GUI'
    GOTO 9999
  END IF

  ALLOCATE( ClassDataArray(MAX(nClassData,1)),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'PlotTri'
    eMessage = 'Error allocating classData array'
    WRITE(eInform,*)'Request=',nClassData,' : error =',ios
    GOTO 9999
  END IF

  CALL SetClassDataArray( PlotDef(BASE_LEVEL),nClassData,ClassDataArray )
  IF( hasError() )GOTO 9999

  FieldID(FLD_INDEX) = SCIPCreateField( UserID,PlotDef(BASE_LEVEL)%Field,ClassDataArray )
  IF( FieldID(FLD_INDEX) <= 0 )THEN
    irv = SCIPGetLastError( ToolError )
    nError   = ToolError%iparm
    eRoutine = TRIM(ToolError%routine)//' : PlotField'
    eMessage = ToolError%aString
    eInform  = ToolError%bString
    eAction  = ToolError%cString
    GOTO 9999
  END IF

  DEALLOCATE( ClassDataArray,STAT=ios )
  PlotDef(BASE_LEVEL)%Created = FieldID(FLD_INDEX) > 0
  PlotDef(EDIT_LEVEL)%Created = PlotDef(BASE_LEVEL)%Created
  PlotDef(EDIT_LEVEL)%Field%Units      = PlotDef(BASE_LEVEL)%Field%Units
  PlotDef(EDIT_LEVEL)%Field%Coordinate = PlotDef(BASE_LEVEL)%Field%Coordinate
  PlotDef(EDIT_LEVEL)%Field%fldLev     = PlotDef(BASE_LEVEL)%Field%fldLev
  PlotDef(EDIT_LEVEL)%Field%resolution = PlotDef(BASE_LEVEL)%Field%resolution
  PlotDef(EDIT_LEVEL)%Field%interpType = PlotDef(BASE_LEVEL)%Field%interpType

  iCh = PlotDef(BASE_LEVEL)%Field%choice
  iCl = PlotDef(BASE_LEVEL)%Field%class
  IF( BTEST(ClassChoiceArray(iCl,iCh)%available,HPB_ASSOC_LINE) &
                                       .AND. FieldID(FLD_INDEX) > 0 )THEN
    irv = SCIPGetFieldAssocSize( UserID,FieldID(FLD_INDEX),nPlots,nLines,nPoints )
    IF( irv /= SCIPsuccess )THEN
      irv = SCIPGetLastError( ToolError )
      nError   = ToolError%iparm
      eRoutine = TRIM(ToolError%routine)//' : PlotField'
      eMessage = ToolError%aString
      eInform  = ToolError%bString
      eAction  = ToolError%cString
      GOTO 9999
    END IF
    ALLOCATE( AssocPoints(nPoints),STAT=ios )
    IF( ios == 0 )ALLOCATE( AssocLines(nLines),  STAT=ios )
    IF( ios == 0 )ALLOCATE( AssocTitles(nPlots), STAT=ios )
    IF( ios == 0 )ALLOCATE( AssocAxes(nPlots),   STAT=ios )
    IF( ios == 0 )ALLOCATE( AssocLineIDs(nLines),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'PlotTri'
      eMessage = 'Error allocating associated data arrays'
      GOTO 9999
    END IF
    irv = SCIPGetFieldAssocData( UserID,FieldID(FLD_INDEX),AssocLines,AssocPoints, &
                                 AssocTitles,AssocAxes,AssocLineIDs )
    DO i = 1,nPlots
      WRITE(99,*) TRIM(AssocTitles(i)%string)
      WRITE(99,*) TRIM(AssocAxes(i)%string)
      DO j = AssocLines(i)%start,AssocLines(i)%start + AssocLines(i)%number - 1
        WRITE(99,*) AssocPoints(j)%x,AssocPoints(j)%y
      END DO
    END DO
  END IF

END IF

IF( FieldID(FLD_INDEX) <= 0 )THEN
  irv = SCIPGetLastError( ToolError )
  IF( ToolError%iparm == NO_ERROR .OR. irv /= SCIPsuccess )THEN
    nError = UK_ERROR
    eRoutine   = 'PlotTri'
    eMessage = 'Invalid Plot field ID'
    eInform  = 'Possible failure of SCIPCreateField'
  ELSE
    nError   = ToolError%iparm
    eRoutine = TRIM(ToolError%routine)//' : PlotField'
    eMessage = ToolError%aString
    eInform  = ToolError%bString
    eAction  = ToolError%cString
  END IF
  GOTO 9999
END IF

!==============================================================================
!Set Field Min/Max for plot display
!==============================================================================
IF( CatClassArray(PlotDef(BASE_LEVEL)%Field%Category,PlotDef(BASE_LEVEL)%Field%Class)%type &
                                                          == SCIPtrue )THEN
  DrawType%type = PlotDef(BASE_LEVEL)%Type
  IF( PlotDef(BASE_LEVEL)%Type > HP_NUMTYP )THEN
    dmx = -1.E+36
	  dmn =  1.E+36
    DrawType%Type = HP_EXCEED
    DO i = 1,ContourList(ContourIndex,BASE_LEVEL)%ListHdr%Number
      DrawType%Data = ContourList(ContourIndex,BASE_LEVEL)%ListPtr(i)%Value
      irv = SCIPGetFieldMinMax( UserID,FieldID(FLD_INDEX),DrawType, &
                                dmin,dmax,fmin,fmax,ddmn,ddmx )
	    dmn = MIN( dmn,ddmn )
	    dmx = MAX( dmx,ddmx )
	  END DO
    DrawType%Type = HP_NUMTYP + 1
    DrawType%Data = NOT_SET_R
  ELSE
    IF( PlotDef(BASE_LEVEL)%Type == HP_MEAN )THEN
      DrawType%Data = NOT_SET_R
    ELSE IF( PlotDef(BASE_LEVEL)%Type == HP_PROB )THEN
      DrawType%Data = PlotDef(BASE_LEVEL)%TypeData(HP_PROB)
    ELSE
      DrawType%Data = PlotDef(BASE_LEVEL)%TypeData(HP_EXCEED)
    END IF
    irv = SCIPGetFieldMinMax( UserID,FieldID(FLD_INDEX),DrawType, &
                              dmin,dmax,fmin,fmax,dmn,dmx )
  END IF
ELSE
  DrawType%Type = HP_MEAN
  DrawType%Data = NOT_SET_R
  irv = SCIPGetFieldMinMax( UserID,FieldID(FLD_INDEX),DrawType, &
                            dmin,dmax,fmin,fmax,dmn,dmx )
END IF

!==============================================================================
!Set Actual Contour values
!==============================================================================

CALL SetContours( ContourIndex,FieldID(FLD_INDEX),ContourPlot )
IF( hasError() )GOTO 9999

vmax = dmx*ContourPlot%ListHdr%Scale
vmin = dmn*ContourPlot%ListHdr%Scale

lconst = (vmax == vmin)

!==============================================================================
!Build Terrain Field if Needed
!==============================================================================

IF( ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode /= PLOT_NULL )THEN

  IF( poptdef(BASE_LEVEL)%Terrain )THEN
    ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = PLOT_ON
  ELSE
    ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = PLOT_OFF
  END IF

  SELECT CASE( PlotDef(BASE_LEVEL)%Field%Category )

    CASE( HP_HSLICE )
      NeedTerrain = (ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string /= '3D Met')    !.TRUE.

    CASE( HP_VSLICE )
      NeedTerrain = .TRUE.

    CASE DEFAULT
      NeedTerrain = poptdef(BASE_LEVEL)%Terrain

  END SELECT

ELSE

  NeedTerrain = .FALSE.

END IF

IF( NeedTerrain .AND. .NOT.PlotTer%Created )THEN
  irv = SCIPDeleteField( userID,FieldID(TER_INDEX) )

  nClassData = NumClassDataArray( PlotTer )

  ALLOCATE( ClassDataArray(MAX(nClassData,1)),STAT=ios )
  IF( ios == 0 )THEN

    CALL SetClassDataArray( PlotTer,nClassData,ClassDataArray )
    IF( hasError() )GOTO 9999

    FieldID(TER_INDEX) = SCIPCreateField( UserID,PlotTer%Field,ClassDataArray )

    DEALLOCATE( ClassDataArray,STAT=ios )
    PlotTer%Created = FieldID(TER_INDEX) > 0

    TerType%Type    = HP_MEAN
    TerType%Data    = NOT_SET_R
    TerType%AreaMode = HP_OFF
  ELSE
    nError   = UK_ERROR
    eRoutine = 'PlotTri'
    eMessage = 'Error allocating classData array (Terrain)'
    WRITE(eInform,*)'Request=',nClassData,' : error =',ios
    GOTO 9999
  END IF

END IF

IF( NeedTerrain )THEN

  IF( FieldID(TER_INDEX) <= 0 )THEN
    irv = SCIPGetLastError( ToolError )
    IF( ToolError%iparm == NO_ERROR .OR. irv /= SCIPsuccess )THEN
      nError   = UK_ERROR
      eRoutine = 'PlotTri'
      eMessage = 'Invalid Terrain field ID'
      eInform  = 'Possible failure of SCIPCreateField'
    ELSE
      nError   = ToolError%iparm
      eRoutine = TRIM(ToolError%routine)//' : TerrainField'
      eMessage = ToolError%aString
      eInform  = ToolError%bString
      eAction  = ToolError%cString
    END IF

    GOTO 9999

  ELSE

    CALL SetContours( PlotTer%ContourIndex,FieldID(TER_INDEX),ContourTer )
    IF( hasError() )GOTO 9999

  END IF

END IF

!==============================================================================
!Set Units labeling
!==============================================================================

lunit = .TRUE.
clbu  = ContourPlot%ListHdr%Unit
IF( TRIM(clbu) == 'default' )THEN
  clbu = TRIM(PlotDef(BASE_LEVEL)%Field%Units)
  IF( TRIM(clbu) == 'default' )THEN
    clbu = 'Value'
  END IF
  IF( ContourPlot%ListHdr%Scale /= 1.0 )THEN
    CALL c_format( ContourPlot%ListHdr%Scale,i,tstring1 )
    tstring2 = TRIM(clbu)//'(x'//TRIM(tstring1)//')'
    clbu = TRIM(tstring2)
  END IF
  cunit_ttl = " "
ELSE
  cunit_ttl = clbu
END IF
IF(DrawType%type == HP_PROB)THEN
  cunit_ttl = clbu
  clbu      = 'Probability'
ELSE IF(DrawType%type == HP_VARIANCE)THEN
  clbu      = '('//TRIM(clbu)//')^2'
END IF

!==============================================================================
!Set Titles
!==============================================================================

lttl = ttldef(BASE_LEVEL)%Show(3)
lttu = ttldef(BASE_LEVEL)%Show(2)
ltt3 = ttldef(BASE_LEVEL)%Show(1)

cttl = TRIM(ttldef(BASE_LEVEL)%string(3))
cttu = TRIM(ttldef(BASE_LEVEL)%string(2))
ctt3 = TRIM(ttldef(BASE_LEVEL)%string(1))

xttl = ttldef(BASE_LEVEL)%X(3)
xttu = ttldef(BASE_LEVEL)%X(2)
xtt3 = ttldef(BASE_LEVEL)%X(1)

yttl = ttldef(BASE_LEVEL)%Y(3)
yttu = ttldef(BASE_LEVEL)%Y(2)
ytt3 = ttldef(BASE_LEVEL)%Y(1)

IF( PlotDef(BASE_LEVEL)%ContourIndex /= USER_CONTOUR )THEN
    IF( PlotDef(BASE_LEVEL)%Type == HP_EXCEED )THEN
      CALL c_format( PlotDef(BASE_LEVEL)%TypeData(HP_EXCEED),irv,string1 )
      exceed_ttl = TRIM(string1)//'%'
    ELSE
      CALL c_format( PlotDef(BASE_LEVEL)%TypeData(HP_PROB),irv,string1 )
      exceed_ttl = TRIM(string1)//' '//TRIM(cunit_ttl)
    END IF
ELSE
  IF( PlotDef(BASE_LEVEL)%Type == HP_EXCEED )THEN
    CALL c_format( PlotDef(BASE_LEVEL)%TypeData(HP_EXCEED),irv,string1 )
    exceed_ttl = TRIM(string1)//'%'
  ELSE
    CALL c_format( PlotDef(BASE_LEVEL)%TypeData(HP_PROB),irv,string1 )
    exceed_ttl = TRIM(string1)//' '//TRIM(cunit_ttl)
  END IF
END IF

CALL SetTitles( lttl,cttl,lttu,cttu,ltt3,ctt3,exceed_ttl )

!==============================================================================
!Retrieve basic grid parameters
!==============================================================================

irv = SCIPGetFieldDomain( userID,FieldID(FLD_INDEX),m0,n0,xmin_def,ymin_def, &
                          dxsrf_def,dysrf_def )

!==============================================================================
!Set Vertical Slice cooridinates
!==============================================================================

lvl = PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
      PlotDef(BASE_LEVEL)%Field%Category == HP_HINT

IF( lvl )THEN
  ptmp(3) = 1.0 !axesdef(BASE_LEVEL).Xscale
  ptmp(6) = 1.0 !axesdef(BASE_LEVEL).Yscale
  dtmp(3) = 1.0
  dtmp(6) = 1.0 !
  dtmp(1) = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%StartPt%X !X Location Point 1
  dtmp(4) = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%StartPt%Y !Y Location Point 1
  dtmp(2) = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%EndPt%X   !X Location Point 2
  dtmp(5) = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%EndPt%Y   !Y Location Point 2
  CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                          0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
  LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
  Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
  CALL axes_transform( dtmp,rtmp,ptmp,LatLon,Cartesian,.TRUE.,.FALSE. )
  xov = ptmp(1) !X Location Point 1
  yov = ptmp(4) !Y Location Point 1
  xev = ptmp(2) !X Location Point 2
  yev = ptmp(5) !Y Location Point 2
END IF

!==============================================================================
!Set Plot coordinate transformations
!==============================================================================

DataCoordinate = PlotDef(BASE_LEVEL)%Field%Coordinate
PlotCoordinate = DataCoordinate
IF( PlotTer%Created )THEN
  TerrainCoordinate = PlotTer%Field%Coordinate
END IF
IF( axesdef(BASE_LEVEL)%MapCoord /= ABS(DataCoordinate%Mode) )THEN

  SELECT CASE( axesdef(BASE_LEVEL)%MapCoord )

    CASE( I_UTM )
      PlotCoordinate%Mode          = HD_UTM
      PlotCoordinate%Reference%x   = NOT_SET_R
      PlotCoordinate%Reference%y   = NOT_SET_R
      PlotCoordinate%Reference%lat = NOT_SET_R
      PlotCoordinate%Reference%lon = NOT_SET_R
      SELECT CASE( ABS(DataCoordinate%Mode) )
        CASE( HD_LATLON )
          lon0 = xmin_def + 0.5*FLOAT(m0)*dxsrf_def
          lat0 = ymin_def + 0.5*FLOAT(n0)*dysrf_def
        CASE( HD_CARTESIAN )
          lon0 = axesdef(BASE_LEVEL)%Lon0
          lat0 = axesdef(BASE_LEVEL)%Lat0
        CASE DEFAULT
      END SELECT
      irv = LL2UTM(lat0,lon0,zone,x0,y0)
      IF( irv /= 0 )THEN
        CALL setUTMerrorGUI( 'LL2UTM',irv )
        GOTO 9999
      END IF
      PlotCoordinate%UTMZone = zone

    CASE( I_LATLON )
      PlotCoordinate%Mode          = HD_LATLON
      PlotCoordinate%UTMZone       = NOT_SET_I
      PlotCoordinate%Reference%x   = NOT_SET_R
      PlotCoordinate%Reference%y   = NOT_SET_R
      PlotCoordinate%Reference%lat = NOT_SET_R
      PlotCoordinate%Reference%lon = NOT_SET_R
      IF( DataCoordinate%Mode == I_CARTESIAN )THEN
        DataCoordinate%Reference%x   = axesdef(BASE_LEVEL)%X0
        DataCoordinate%Reference%y   = axesdef(BASE_LEVEL)%Y0
        DataCoordinate%Reference%lat = axesdef(BASE_LEVEL)%Lat0
        DataCoordinate%Reference%lon = axesdef(BASE_LEVEL)%Lon0
      END IF

    CASE( I_CARTESIAN )
      PlotCoordinate%Mode          = HD_CARTESIAN
      PlotCoordinate%UTMZone       = NOT_SET_I
      PlotCoordinate%Reference%x   = axesdef(BASE_LEVEL)%X0
      PlotCoordinate%Reference%y   = axesdef(BASE_LEVEL)%Y0
      PlotCoordinate%Reference%lat = axesdef(BASE_LEVEL)%Lat0
      PlotCoordinate%Reference%lon = axesdef(BASE_LEVEL)%Lon0

    CASE DEFAULT

  END SELECT

END IF

IF( lvl )THEN
  DataCoordinate%Mode = ABS(DataCoordinate%Mode)
  PlotCoordinate%Mode = ABS(PlotCoordinate%Mode)

  x0  = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%StartPt%X !X Location Point 1
  y0  = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%StartPt%Y !Y Location Point 1
  irv = SCIPTransformXY(DataCoordinate,PlotCoordinate,x0,y0)
  PlotCoordinate%VertSlice%StartPt%x = x0
  PlotCoordinate%VertSlice%StartPt%y = y0

  x0  = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%EndPt%X !X Location Point 2
  y0  = PlotDef(BASE_LEVEL)%Field%Coordinate%vertSlice%EndPt%Y !Y Location Point 2
  irv = SCIPTransformXY( DataCoordinate,PlotCoordinate,x0,y0 )
  PlotCoordinate%VertSlice%EndPt%x = x0
  PlotCoordinate%VertSlice%EndPt%y = y0

  DataCoordinate%Mode = -DataCoordinate%Mode
  PlotCoordinate%Mode = -PlotCoordinate%Mode
END IF

CALL SetTransform( project(BASE_LEVEL)%MapCoord,llc,lvl )

!==============================================================================
!Set population parameters
!==============================================================================

popden_def = popden
IF( popden /= 0.0 )THEN
  IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
      PlotDef(BASE_LEVEL)%Field%Category == HP_HINT .OR. &
     TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == 'Met/Terrain' )THEN
    popden = 0.0
  END IF
END IF

expectOK = expectOK .AND. (DrawType%Type == HP_MEAN)
IF( popden == 0.0 )THEN
  DrawType%AreaMode = HP_OFF !Off
ELSE
  DrawType%AreaMode = HP_ON !Pop in contour
  IF( popden /= default )THEN
    DrawType%AreaMode = DrawType%AreaMode + HP_AREA !Area in contour
  END IF
  IF( expectOK .AND. (popden > 0 .OR. popden == default) )THEN
    DrawType%AreaMode = DrawType%AreaMode + HP_EXPECT !Expected/Areal
  END IF
END IF
!---- Transform

!---- Set Population Density parameters

!---- Axes Scaling

IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_HINT )THEN
  CALL SetAxes( xmnb,xmxb,scx,ymnb,ymxb,scy,boxl,0 )
ELSE
  CALL SetAxes( xmnb,xmxb,scx,ymnb,ymxb,scy,boxl,axesdef(BASE_LEVEL)%MapCoord )
END IF
IF( hasError() )GOTO 9999

!IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE )scy = 0.001*scy
IF( lvl )scy = 0.001*scy

!---- Set Labels

CALL SetLabels( llc,zone,scx,scy,lvl,clbx,clby )

!---- Set Print Flag

lprint = ltmp
lmapout= lMAPovl
IF( lAVSplt )THEN
  loil   = .FALSE.
  leis   = lOVLplt .AND. .NOT.lCTSplt
  lusa   = .NOT.lOVLplt .AND. lCTSplt
  lavs   = .NOT.lOVLplt .AND. .NOT.lusa
  lcts   = lOVLplt .OR. lusa
  lovl   = lOVLplt .OR. lusa
  lcloseIt = lCTSplt
ELSE
  lavs   = lAVSplt
  lcts   = lCTSplt .OR. lOVLplt
  lovl   = lOVLplt
  loil   = lOVLplt .AND. .NOT.lCTSplt
  leis   = .FALSE.
  lusa   = .FALSE.
  lcloseIt = loil
END IF
lavs_cts = lavs .OR. lcts

IF( lavs_cts )THEN

  PlotWrite%filename = TRIM(ASCfile)
  IF( lavs )THEN
    PlotWrite%Mode = AVS_FILE
  ELSE IF( loil )THEN
    PlotWrite%Mode = OIL_FILE
  ELSE IF( leis )THEN
    PlotWrite%Mode = EIS_FILE
  ELSE IF( lusa )THEN
    PlotWrite%Mode = USA_FILE
  ELSE IF( .NOT.lovl )THEN
    PlotWrite%Mode = CTS_FILE
  ELSE
    IF( lAVSplt )THEN
      PlotWrite%Mode = ARC_FILE
    ELSE
      PlotWrite%Mode = OVL_FILE
    END IF
  END IF

ELSE

  cavs = ' '
  IF( lprint .AND. lpbare )THEN
    lttl = .FALSE.
    lttu = .FALSE.
    ltt3 = .FALSE.
    llab = .FALSE.
    llbc = .FALSE.
    llbb = .FALSE.
    lkmb = .FALSE.
    nmax =  0
    ntx  = -ABS(ntx)
    nty  = -ABS(nty)
    zone = -ABS(zone)
    lmapon = .FALSE.
    lbck = .FALSE.
    lrmk = .TRUE.
  ELSE
    lbck = .TRUE.
    lrmk = .FALSE.
  END IF

END IF

!---- Draw the Plot

  lextra = .TRUE.
  lbck   = .TRUE.
  CALL do_plot( lextra,lrmk,.FALSE. )
  IF( hasError() )lovly = .FALSE.

9999 CONTINUE

IF( ALLOCATED(ClassDataArray) )DEALLOCATE( ClassDataArray,STAT=ios )
IF( ALLOCATED(AssocLines)   )DEALLOCATE( AssocLines,  STAT=ios )
IF( ALLOCATED(AssocPoints)  )DEALLOCATE( AssocPoints, STAT=ios )
IF( ALLOCATED(AssocTitles)  )DEALLOCATE( AssocTitles, STAT=ios )
IF( ALLOCATED(AssocAxes)    )DEALLOCATE( AssocAxes,   STAT=ios )
IF( ALLOCATED(AssocLineIDs) )DEALLOCATE( AssocLineIDs,STAT=ios )

IF( nError /= NO_ERROR )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
IF( hasError() .AND. lastError() == OV_ERROR )THEN
  poptdef(BASE_LEVEL)%Overlay = .FALSE.
END IF
IF( .NOT.PlayBack )THEN
  irv = SetCursor( hcur_arrow ) !  Set Arrow
  lrv = ReleaseCapture() !Release Mouse
END IF

RETURN
END

!==============================================================================='

SUBROUTINE map_scale_error()

USE resource_fd
USE pcscipuf_fi
USE contri_fi

IMPLICIT NONE

INTEGER irv

iAbort = 1

CALL CallButton( hdlgp,ID_CANCEL,irv )

RETURN
END
!***********************************************************************
!               SetConcMin
!***********************************************************************
SUBROUTINE set_conc_min( matl )

USE default_fd
USE GUImatl_fi

!     This routine sets the minimum concentration level

IMPLICIT NONE

TYPE( material_str ), INTENT( INOUT ) :: matl

!==== Check to see if its already set

IF( matl%prop(3) /= NOT_SET_R .AND. matl%prop(3) /= DEF_VAL_R )RETURN

!==== Initialize to 0.0

matl%prop(3) = 0.0

RETURN
END
!***********************************************************************
!               MapScaleI
!***********************************************************************
INTEGER FUNCTION MapScaleI()

USE pcscipuf_fi

IMPLICIT NONE

MapScaleI = NINT(map_scale)

RETURN
END
!***********************************************************************
!               MapScaleR
!***********************************************************************
REAL FUNCTION MapScaleR()

USE pcscipuf_fi
USE plotdlg_fi

IMPLICIT NONE

MapScaleR = map_scale*Fmap_scales(axesdef(BASE_LEVEL)%MapCoord)

RETURN
END
!***********************************************************************
!               SetTransform
!***********************************************************************
SUBROUTINE SetTransform( CoordType,Cart,Vertical )

USE default_fd
USE coordinate_fd
USE contri_fi
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE GUImatl_fi

IMPLICIT NONE

LOGICAL Cart,Vertical
INTEGER CoordType

REAL pi180,sphfac

pi180  = 4.*ATAN(1.)/180.
sphfac = pi180*6371.2
IF( CoordType == I_METERS )sphfac = sphfac*1000.

IF( CoordType == I_LATLON )THEN !Data is Lat/Lon

  IF( Cart )THEN !  Plot is Cartesian

    IF( xolc  == DEF_VAL_R )THEN
      xod = xmin_def + 0.5*FLOAT(m0)*dxsrf_def
    ELSE
      xod = xolc
    END IF
    IF( yolc  == DEF_VAL_R )THEN
      yod = ymin_def + 0.5*FLOAT(n0)*dysrf_def
    ELSE
      yod = yolc
    END IF
    xop = xclc
    yop = yclc
    xsdp = sphfac*COS(yod*pi180)
    ysdp = sphfac
    xom = xod
    yom = yod
    xspm = 1./xsdp
    yspm = 1./ysdp

  ELSE !  Plot is Lat/Lon

    xod  = xmin_def + 0.5*FLOAT(m0)*dxsrf_def
    yod  = ymin_def + 0.5*FLOAT(n0)*dysrf_def
    xop  = xod
    yop  = yod
    xsdp = 1.
    ysdp = 1.
    xom  = xod
    yom  = yod
    xspm = 1.
    yspm = 1.

  END IF

ELSE !Data is Cartesian

  IF( Cart )THEN !  Plot is Cartesian

    xod  = xclc
    yod  = yclc
    xop  = xod
    yop  = yod
    xsdp = 1.
    ysdp = 1.
    IF( yolc  == DEF_VAL_R .OR. yolc  == NOT_SET_R )THEN
      yom = 0.0
      yspm = 1.
    ELSE
      yom = yolc
      yspm = 1./sphfac
    END IF
    IF( xolc  == DEF_VAL_R .OR. xolc  == NOT_SET_R )THEN
      xom = 0.0
      xspm = 1.
    ELSE
      xom = xolc
      xspm = 1./(sphfac*COS(yom*pi180))
    END IF

  ELSE !  Plot is Lat/Lon

    IF( xclc  == DEF_VAL_R )THEN
      xod = xmin_def + 0.5*FLOAT(m0)*dxsrf_def
    ELSE
      xod = xclc
    END IF
    IF( yclc  == DEF_VAL_R )THEN
      yod = ymin_def + 0.5*FLOAT(n0)*dysrf_def
    ELSE
      yod = yclc
    END IF
    IF( yolc  == DEF_VAL_R .OR. yolc  == NOT_SET_R )THEN
      yop = 0.0
      ysdp = 1.
    ELSE
      yop = yolc
      ysdp = 1./sphfac
    END IF
    IF( xolc  == DEF_VAL_R .OR. xolc  == NOT_SET_R )THEN
      xop = 0.0
      xsdp = 1.
    ELSE
      xop = xolc
      xsdp = 1./(sphfac*COS(yop*pi180))
    END IF
    xom = xop
    yom = yop
    xspm = 1.
    yspm = 1.

  END IF

END IF

IF( Vertical )THEN

  xod  = xmin_def + 0.5*FLOAT(m0)*dxsrf_def
  xop  = xod
  xom  = xod
  xsdp = 1.
  xspm = 1.
  yod  = ymin_def + 0.5*FLOAT(n0)*dysrf_def
  yop  = yod
  yom  = yod
  ysdp = 1.
  yspm = 1.

END IF

RETURN
END

!===============================================================================

SUBROUTINE SetPages()

USE default_fd
USE pcscipuf_fi
USE plotdlg_fi
USE contri_fi
USE files_fi
USE errorParam_fd
USE param_fd
USE script_fi
USE pltchoice_fi
USE testprt

IMPLICIT NONE

REAL    Size,AR
LOGICAL Cart,lCont
INTEGER nn

LOGICAL, EXTERNAL :: hasError

Size = popt2def(BASE_LEVEL)%Size
IF( Size == DEF_VAL_R )THEN
  IF( BTEST(PlotDef(BASE_LEVEL)%Mode,BPLT_FILL) .AND. poptdef(BASE_LEVEL)%ColorBox )THEN
    Size = 0.63
  ELSE
    Size = 0.68
  END IF
END IF

Cart = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
CALL SetTransform( project(BASE_LEVEL)%MapCoord,Cart,(PlotDef(BASE_LEVEL)%Field%Category &
                                                             == HP_VSLICE) )
CALL SetAxes( xmnb,xmxb,scx,ymnb,ymxb,scy,Size,axesdef(BASE_LEVEL)%MapCoord )

IF( Size >= 0 )THEN
  nxpage = 1
  nypage = 1
ELSE
  AR  = popt2def(BASE_LEVEL)%AR
  CALL set_plot_pages( Size,AR,Cart,nxpage,nypage )
END IF

IF( nxpage*nypage > 1 )THEN
  CALL i_format( nxpage*nypage,nn,string1 )
  lCont = nxpage*nypage <= 9
  CALL SetError( WN_ERROR,'Plot does not fit on a single page', &
                 'Plot requires '//TRIM(string1)//' pages', &
                 'Would you like to print it anyway?', &
                 'SetPages' )
  CALL ShowWarningMessage( 0,lCont )
  IF( hasError() )THEN
    CALL InitError()
    lPrintIt = .FALSE.
    nxpage = 1
    nypage = 1
  END IF
END IF

RETURN
END

!===============================================================================

SUBROUTINE check_plot_axes( MapCoord,Cart,Vertical,scale_min,fac )

USE contri_fi

IMPLICIT NONE

INTEGER MapCoord
LOGICAL Cart,Vertical
REAL    scale_min,fac

LOGICAL lwnd

REAL Size,xlat

CALL SetTransform( MapCoord,Cart,Vertical )

CALL SetAxes( xmnb,xmxb,scx,ymnb,ymxb,scy,Size,0 )

CALL set_plot_axes( lwnd,xmnd,xmxd,ymnd,ymxd )

IF( Cart )THEN
  xlat = 1.0
ELSE
  xlat = cosd((ymxd+ymnd)/2.)
END IF

scale_min = MAX((xmxd-xmnd)*xlat,(ymxd-ymnd))/(7.*fac)

RETURN
END

!===============================================================================

SUBROUTINE get_origin( t,x,y,tp )

USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE plotdlg_fi
USE pltchoice_fi

IMPLICIT NONE

REAL t,x,y,tp,tx

CHARACTER(128) StripNull

REAL, EXTERNAL :: time_of_day

x = SNGL(scenario(BASE_LEVEL)%release(1)%xRel)
y = SNGL(scenario(BASE_LEVEL)%release(1)%yRel)

IF( StripNull(timePlot(PlotDef(BASE_LEVEL)%Field%TimeID)%string) == &
                                                          'UserInput' )THEN
  tx = PlotDef(BASE_LEVEL)%Field%UserTime
ELSE
  tx = timePlot(PlotDef(BASE_LEVEL)%Field%TimeID)%time%runTime
END IF

t  = time_of_day( dlgTime(BASE_LEVEL)%time%start%time,scenario(BASE_LEVEL)%release(1)%time )
tp = time_of_day( dlgTime(BASE_LEVEL)%time%start%time,tx )

RETURN
END

!===============================================================================

REAL FUNCTION time_of_day( tref,t )

USE pcscipuf_fi
USE dialog_fi

IMPLICIT NONE

TYPE( timeT ) tref,tout

REAL t

LOGICAL lok

tout%runTime = t

CALL ComputeEndTime( tref,tout,lok )

IF( lok )THEN
  time_of_day = tout%hour
ELSE
  time_of_day = t
END IF

RETURN
END

!===============================================================================

SUBROUTINE DisplayIcon( idc )

USE pcscipuf_fi
USE plotdlg_fi
USE contri_fi
USE files_fi
USE param_fd
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) idc

TYPE( T_RECT ) Box

INTEGER irv,ix,iy
INTEGER iwid,ihgt
REAL    xx,yy

TYPE( T_SIZE  ) ext,org

IF( BTEST(project(BASE_LEVEL)%Mode,FAST_MODE) )THEN

  IF( lprint )THEN

    string1 = 'FAST'
    CALL GetNCARFontSize( 0.,0.,string1,0.75*fsz,0.,0.,xx,yy )
    xx = 0.001
    yy = 1.0 - yy
    CALL plchhq( xx,yy,string1,0.75*fsz,0.,-1. )
    string1 = 'MODE'
    yy = 2.*yy - 1
    CALL plchhq( xx,yy,string1,0.75*fsz,0.,-1. )

  ELSE

    irv = GetClientRect( hwnd_pw,Box )
    ix  = Box%left
    iy  = Box%top
    irv = GetWindowExtEx( idc,ext )
    iwid  = NINT(FLOAT(ext%cx)/12.)
    ihgt  = NINT(FLOAT(ext%cy)/12.)
    irv = SetWindowExtEx( idc,iwid,ihgt,ext )
    irv = DrawIcon( idc,ix,iy,fast_hndl )
    irv = SetWindowExtEx( idc,ext%cx,ext%cy,org )

  END IF

END IF

RETURN
END
!==============================================================================
!==============================================================================
! NumClassDataArray
!==============================================================================
!==============================================================================
INTEGER FUNCTION NumClassDataArray( PlotDefinition )

USE pltchoice_fi
USE classdata_fd

IMPLICIT NONE

TYPE( PlotField ) :: PlotDefinition

INTEGER nClassData
TYPE( char64T ) CurClass,CurKind,CurChoice
INTEGER         CurCat

!==============================================================================
! Initialize
!==============================================================================
nClassData = 0

!==============================================================================
! Category data
!==============================================================================
SELECT CASE( PlotDefinition%Field%Category )

  CASE( HP_HSLICE )
    nClassData = nClassData + CD_NUM_HSLICE

  CASE( HP_VSLICE,HP_HINT )
    nClassData = nClassData + CD_NUM_VSLICE

  CASE( HP_VINT,HP_SSLICE )
    nClassData = nClassData + CD_NUM_VINT

  CASE( HP_TABLE )
    nClassData = nClassData + CD_NUM_TABLE

  CASE DEFAULT

END SELECT

!==============================================================================
! Source estimation data
!==============================================================================
CurClass  = ClassStr(PlotDefinition%Field%Class)
IF( INDEX(CurClass%string,'Source Estimation') /= 0 )THEN
  nClassData = nClassData + CD_NUM_ADJ_DOMAIN
  CurChoice  = ChoiceStr(PlotDefinition%Field%Choice)
  IF( INDEX(CurChoice%string,'Max') /= 0 )nClassData = nClassData + CD_NUM_ADJ_TIME
END IF

!==============================================================================
! Return
!==============================================================================

NumClassDataArray = nClassData

RETURN
END
!==============================================================================
!==============================================================================
! SetClassDataArray
!==============================================================================
!==============================================================================
SUBROUTINE SetClassDataArray( PlotDefinition,mClassData,ClassDataArray )

USE pltchoice_fi
USE errorParam_fd
USE classdata_fd

IMPLICIT NONE

TYPE( PlotField )                  :: PlotDefinition
INTEGER                            :: mClassData
REAL, DIMENSION(MAX(mClassData,1)) :: ClassDataArray

INTEGER nClassData
INTEGER i
TYPE( char64T ) CurClass,CurKind,CurChoice

CHARACTER(128) eInform

!==============================================================================
! Initialize
!==============================================================================
nClassData = 0

!==============================================================================
! Check for empty array
!==============================================================================
IF( mClassData == 0 )THEN

  ClassDataArray = 0.0

!==============================================================================
! Fill data array
!==============================================================================
ELSE

!==============================================================================
! Category data
!==============================================================================
  SELECT CASE( PlotDefinition%Field%Category )

    CASE( HP_VSLICE,HP_HINT )
      nClassData = nClassData + CD_NUM_VSLICE
      ClassDataArray(CD_XMIN) = PlotDefinition%ClassData(VSLICE_INDEX)%xmin
      ClassDataArray(CD_XMAX) = PlotDefinition%ClassData(VSLICE_INDEX)%xmax
      ClassDataArray(CD_YMIN) = PlotDefinition%ClassData(VSLICE_INDEX)%ymin
      ClassDataArray(CD_YMAX) = PlotDefinition%ClassData(VSLICE_INDEX)%ymax
      ClassDataArray(CD_ZMIN) = PlotDefinition%ClassData(VSLICE_INDEX)%zmin
      ClassDataArray(CD_ZMAX) = PlotDefinition%ClassData(VSLICE_INDEX)%zmax
      ClassDataArray(CD_VRES) = PlotDefinition%ClassData(VSLICE_INDEX)%zres

	  CASE( HP_HSLICE )
      nClassData = nClassData + CD_NUM_HSLICE
      ClassDataArray(CD_XMIN) = PlotDefinition%ClassData(HSLICE_INDEX)%xmin
      ClassDataArray(CD_XMAX) = PlotDefinition%ClassData(HSLICE_INDEX)%xmax
      ClassDataArray(CD_YMIN) = PlotDefinition%ClassData(HSLICE_INDEX)%ymin
      ClassDataArray(CD_YMAX) = PlotDefinition%ClassData(HSLICE_INDEX)%ymax
      ClassDataArray(CD_ZMIN) = PlotDefinition%ClassData(HSLICE_INDEX)%zmin

    CASE( HP_VINT,HP_SSLICE )
      nClassData = nClassData + CD_NUM_VINT
      ClassDataArray(CD_XMIN) = PlotDefinition%ClassData(HSLICE_INDEX)%xmin
      ClassDataArray(CD_XMAX) = PlotDefinition%ClassData(HSLICE_INDEX)%xmax
      ClassDataArray(CD_YMIN) = PlotDefinition%ClassData(HSLICE_INDEX)%ymin
      ClassDataArray(CD_YMAX) = PlotDefinition%ClassData(HSLICE_INDEX)%ymax

    CASE( HP_TABLE )
      nClassData = nClassData + CD_NUM_TABLE
      ClassDataArray(CD_RISK) = PlotDefinition%TypeData(HP_MEAN)
      ClassDataArray(CD_ROUND) = 1.0

	  CASE DEFAULT

  END SELECT

!==============================================================================
! Source estimation data
!==============================================================================
  CurClass  = ClassStr(PlotDefinition%Field%Class)
  IF( INDEX(CurClass%string,'Source Estimation') /= 0 )THEN
    DO i = 1,CD_NUM_ADJ_DOMAIN
      ClassDataArray(nClassData+i) = DEF_VAL_R
    END DO
    nClassData = nClassData + CD_NUM_ADJ_DOMAIN
    CurChoice  = ChoiceStr(PlotDefinition%Field%Choice)
    IF( INDEX(CurChoice%string,'Max') /= 0 )THEN
      DO i = 1,CD_NUM_ADJ_TIME
        ClassDataArray(nClassData+i) = DEF_VAL_R
      END DO
      nClassData = nClassData + CD_NUM_ADJ_TIME
    END IF
  END IF

END IF

!==============================================================================
! Return
!==============================================================================
IF( nClassData /= mClassData )THEN
  WRITE(eInform,*)'Requested number =',mClassData,'  :  Number set =',nClassData
  CALL SetError( UK_ERROR,'Error setting class data array', &
                 eInform,' ','SetClassDataArray' )
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
! PutClassDataArray
!==============================================================================
!==============================================================================
SUBROUTINE PutClassDataArray( PlotDefinition,mClassData,ClassDataArray )

USE pltchoice_fi
USE errorParam_fd
USE classdata_fd

IMPLICIT NONE

TYPE( PlotField )                    :: PlotDefinition
INTEGER mClassData
REAL, DIMENSION( MAX(mClassData,1) ) :: ClassDataArray

INTEGER nClassData

CHARACTER(128) eInform

!==============================================================================
! Initialize
!==============================================================================
nClassData = 0

!==============================================================================
! Check for empty array
!==============================================================================
IF( mClassData == 0 )THEN

!==============================================================================
! Fill data array
!==============================================================================
ELSE

!==============================================================================
! Category data
!==============================================================================
  SELECT CASE( PlotDefinition%Field%Category )

    CASE( HP_VSLICE,HP_HINT )
      nClassData = nClassData + CD_NUM_VSLICE
      PlotDefinition%ClassData(VSLICE_INDEX)%xmin = ClassDataArray(CD_XMIN)
      PlotDefinition%ClassData(VSLICE_INDEX)%xmax = ClassDataArray(CD_XMAX)
      PlotDefinition%ClassData(VSLICE_INDEX)%ymin = ClassDataArray(CD_YMIN)
      PlotDefinition%ClassData(VSLICE_INDEX)%ymax = ClassDataArray(CD_YMAX)
      PlotDefinition%ClassData(VSLICE_INDEX)%zmin = ClassDataArray(CD_ZMIN)
      PlotDefinition%ClassData(VSLICE_INDEX)%zmax = ClassDataArray(CD_ZMAX)
      PlotDefinition%ClassData(VSLICE_INDEX)%zres = ClassDataArray(CD_VRES)

	  CASE( HP_HSLICE )
      nClassData = nClassData + CD_NUM_HSLICE
      PlotDefinition%ClassData(HSLICE_INDEX)%xmin = ClassDataArray(CD_XMIN)
      PlotDefinition%ClassData(HSLICE_INDEX)%xmax = ClassDataArray(CD_XMAX)
      PlotDefinition%ClassData(HSLICE_INDEX)%ymin = ClassDataArray(CD_YMIN)
      PlotDefinition%ClassData(HSLICE_INDEX)%ymax = ClassDataArray(CD_YMAX)
      PlotDefinition%ClassData(HSLICE_INDEX)%zmin = ClassDataArray(CD_ZMIN)

    CASE( HP_VINT,HP_SSLICE )
      nClassData = nClassData + CD_NUM_VINT
      PlotDefinition%ClassData(HSLICE_INDEX)%xmin = ClassDataArray(CD_XMIN)
      PlotDefinition%ClassData(HSLICE_INDEX)%xmax = ClassDataArray(CD_XMAX)
      PlotDefinition%ClassData(HSLICE_INDEX)%ymin = ClassDataArray(CD_YMIN)
      PlotDefinition%ClassData(HSLICE_INDEX)%ymax = ClassDataArray(CD_YMAX)

    CASE( HP_TABLE )
      nClassData = nClassData + CD_NUM_TABLE
      PlotDefinition%TypeData(HP_MEAN) = ClassDataArray(CD_RISK)

	  CASE DEFAULT

  END SELECT

END IF

!==============================================================================
! Return
!==============================================================================
IF( nClassData /= mClassData )THEN
  WRITE(eInform,*)'Requested number =',mClassData,'  :  Number set =',nClassData
  CALL SetError( UK_ERROR,'Error setting class data array', &
                 eInform,' ','putClassDataArray' )
END IF

RETURN
END
