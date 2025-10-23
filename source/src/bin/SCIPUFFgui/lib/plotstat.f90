!***********************************************************************
!               PlotStat
!***********************************************************************
SUBROUTINE plotstat( yoff,ltmp )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUItool_fi
USE files_fi
USE dialog_fi
USE GUImatl_fi
USE units_fd
USE pltchoice_fi
USE SwinAPI

IMPLICIT NONE

REAL    yoff !Y Start position
LOGICAL ltmp !Print Flag

!     This routine posts stats to STAT window

LOGICAL lok, lprint
INTEGER ihgt, ii, iwid
INTEGER ialign, jalign, i, j, ix, iy, iyt, iyb, iyinc, ixl
INTEGER tabs(5), ntabs, n, nlin, ioff, iaux
REAL    x

INTEGER( POINTER_LEN ) kpn, kbr, ipn, ibr, ifont, jfont, idc, jdc, jpn, jbr ,hrgn

CHARACTER(40) string(20)

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

TYPE( T_RECT  ) Box
TYPE( T_POINT ) Pt(2)
TYPE( T_SIZE  ) Sz(2)

!---- Initialize

lprint = ltmp

!---- Load Palette if necessary

CALL setpal( lprint )

!---- Get Viewport dimensions

CALL GetNCARDC( idc,jdc ) !Device Context

lok = GetViewportOrgEx( idc,Pt(1) )
lok = GetViewportExtEx( idc,Sz(1) )

Pt(2)%x = Pt(1)%x + Sz(1)%cx
Pt(2)%y = Pt(1)%y + Sz(1)%cy

!---- Convert to Logical units

lok = DPtoLP( idc,Pt(1),2 )

Box%left   = Pt(1)%x
Box%top    = Pt(1)%y
Box%right  = Pt(2)%x
Box%bottom = Pt(2)%y

!---- Get Stock Pen,Brush

ipn = GetStockObject( BLACK_PEN )
ibr = GetStockObject( NULL_BRUSH )

!---- Select into Device Context

jpn = SelectObject( idc,ipn )
jbr = SelectObject( idc,ibr )

!---- Set Clip Region - None - See Fix below

!      ii   = SelectClipRgn(idc,0)

!---- Set Height of PLOT Rectangle

ihgt = Box%bottom - Box%top

!---- Set TAB points

iwid = Box%right - Box%left

tabs(1) = NINT( 0.123*FLOAT(iwid) )
tabs(2) = NINT( 0.293*FLOAT(iwid) )
tabs(3) = NINT( 0.453*FLOAT(iwid) )
tabs(4) = NINT( 0.618*FLOAT(iwid) )
tabs(5) = NINT( 0.786*FLOAT(iwid) )

ntabs   = 5

!---- Set Height of PLOT Rectangle

ihgt = Box%bottom - Box%top

!---- Set start point

iyt = Box%top + NINT( yoff*FLOAT(ihgt) )
ixl = Box%left

!---- Set Box limits

IF( lprint )THEN
  iyb = iyt + INT((FLOAT(ihgt)*9.75)/7.0) - ihgt
ELSE
  iyb = Box%bottom
END IF

iyinc = INT(0.05*FLOAT(iyb-iyt)) + 1

ioff  = NINT( FLOAT(iyinc)*0.25 )

!---- Clip Fix - Some combinations of Win32s/Printer seem to have problems
!                clearing a clip region by using a NULL region handle.  Fix
!                is to reset the clip region to the entire area

Box%bottom = iyb + 1
Box%right  = Box%right + 1

Pt(1)%x = Box%left
Pt(1)%y = Box%top
Pt(2)%x = Box%right
Pt(2)%y = Box%bottom

lok = LPtoDP( idc,Pt(1),2 )

Box%left   = Pt(1)%x
Box%top    = Pt(1)%y
Box%right  = Pt(2)%x
Box%bottom = Pt(2)%y

hrgn = CreateRectRgnIndirect( Box )
ii   = SelectClipRgn( idc,hrgn )
ii   = DeleteObject( hrgn )

!---- Draw Vertical seperators

Pt(1)%x = tabs(2)
Pt(1)%y = iyt + iyinc/2

Pt(2)%x = tabs(2)
Pt(2)%y = iyb - iyinc/2

n = 2

ii = Polyline( idc,Pt(1),n )

Pt(1)%x = tabs(4)
Pt(2)%x = tabs(4)

ii = Polyline( idc,Pt(1),n )

!---- Set Text Font/Size

string(1) = 'ARIAL'
CALL GetNCARResolution( i )
i = NINT(FLOAT(i)/60.)
CALL SetFont( idc,string(1),i,0,400,ifont,jfont )

!---- Column 1

string( 1) = 'Project'
string( 2) = 'Type'
string( 3) = ' '
string( 4) = 'Start time'
string( 5) = 'Duration (hrs)'
string( 6) = ' '
IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  string3 = CHAR(176)//'E'
ELSE IF( project(BASE_LEVEL)%MapCoord == I_UTM )THEN
  string3 = 'E''ing'
ELSE
  string3 = 'km'
END IF
string( 7) = 'X Domain ('//TRIM(string3)//')'
string( 8) = ' '
string( 9) = 'Meteorology'
string(10) = ' '
string(11) = ' '
string(12) = ' '
string(13) = ' '
IF( materials(BASE_LEVEL)%nmatl > 3 )THEN
  WRITE(string2,*)materials(BASE_LEVEL)%nmatl
  string2 = ADJUSTL(string2)
  string1 = ' ('//TRIM(string2)//')'
ELSE
  string1 = ' '
END IF
string(14) = 'Materials'//TRIM(string1)
string(15) = ' '
string(16) = ' '
IF( scenario(BASE_LEVEL)%nrel > 3 )THEN
  WRITE(string2,*)scenario(BASE_LEVEL)%nrel
  string2 = ADJUSTL(string2)
  string1 = ' ('//TRIM(string2)//')'
ELSE
  string1 = ' '
END IF
string(17) = 'Sources'//TRIM(string1)
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_RIGHT .OR. TA_BOTTOM
jalign = SetTextAlign( idc,ialign )

ix = tabs(1) - ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 1/2 Seperater

string( 1) = ':'
string( 2) = ':'
string( 3) = ' '
string( 4) = ':'
string( 5) = ':'
string( 6) = ' '
string( 7) = ':'
string( 8) = ' '
string( 9) = ':'
string(10) = ' '
string(11) = ' '
string(12) = ' '
string(13) = ' '
string(14) = ':'
string(15) = ' '
string(16) = ' '
string(17) = ':'
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_CENTER .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(1)
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 2

string( 1) = project(BASE_LEVEL)%ID%name
string( 2) = 'SCIPUFF'
string( 3) = ' '
string( 4) = dlgTime(BASE_LEVEL)%startString
CALL set_real_string( dlgTime(BASE_LEVEL)%time%end%time%runTime,string1,n )
string( 5) = TRIM(ADJUSTL(string1))
string( 6) = ' '
CALL set_real_string( dlgDomain(BASE_LEVEL)%spatial%domain%xMin,string1,n )
CALL set_real_string( dlgDomain(BASE_LEVEL)%spatial%domain%xMax,string2,n )
string( 7) = TRIM(ADJUSTL(string1))//' - '//TRIM(ADJUSTL(string2))
string( 8) = ' '
string2 = ' '
SELECT CASE( metdef(BASE_LEVEL)%met )
  CASE( MET_MEDOC )
    string1 = 'MEDOC'
  CASE( MET_WRF)
    string1 = 'WRF'
  CASE( MET_MEDLIS)
    string1 = 'MEDOC list'
  CASE( MET_ASSIM )
    string1 = 'Gridded+Obs'
  CASE( MET_MRF )
    string1 = 'Gridded'
  CASE( MET_OBS )
    string1 = 'Observations'
  CASE( MET_SRF )
    string1 = 'Surface Obs'
  CASE( MET_UAIR )
    string1 = 'Upper Air Obs'
  CASE( MET_FIXED )
    string1 = 'Fixed winds'
  CASE DEFAULT
    string1 = 'Unknown'
END SELECT

string( 9) = 'Data='//TRIM(string1)
string(10) = TRIM(string2)

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC )
    string1 = 'Calculated'
  CASE( BL_NONE )
    string1 = 'None'
  CASE (BL_OBS)
    string1 = 'Observations'
  CASE( BL_SIMPLE )
    string1 = 'Simple'
  CASE( BL_PROFILE )
    string1 = 'Profile'
  CASE( BL_OPERATIONAL )
    string1 = 'Operational'
  CASE DEFAULT
    string1 = 'Unknown'
END SELECT

string(11) = 'BL='//TRIM(string1)

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT )
    string1 = 'Input'
  CASE( LSV_MODEL )
    string1 = 'Model'
  CASE( LSV_NONE )
    string1 = 'None'
  CASE( LSV_OBS )
    string1 = 'Observations'
  CASE( LSV_OPERATIONAL )
    string1 = 'Operational'
  CASE DEFAULT
    string1 = 'Unknown'
END SELECT

string(12) = 'LSV='//TRIM(string1)
string(13) = ' '
string(14) = ' '
string(15) = ' '
string(16) = ' '

n = MIN(3,materials(BASE_LEVEL)%nmatl)
DO i = 1,n
  IF( IsGas(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = '(Gas)'
  ELSE IF( IsParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = '(Particle)'
  ELSE IF( IsLiquid(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = '(Liquid)'
  ELSE IF( IsWetParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = '(Particle)'
  ELSE IF( IsAerosol(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = '(Aerosol)'
  ELSE
    string1 = '(Unknown)'
  END IF
  string(13+i) = TRIM(materials(BASE_LEVEL)%material(i)%cmat)//' '//TRIM(string1)
END DO

string(17) = ' '
string(18) = ' '
string(19) = ' '

n = MIN(3,scenario(BASE_LEVEL)%nrel)
DO i = 1,n
  IF( scenario(BASE_LEVEL)%release(i)%type(1:1) == 'I' )THEN
    IF( scenario(BASE_LEVEL)%release(i)%spec == REL_FILE )THEN
      string1 = 'CLOUDTRANS'
    ELSE
      string1 = 'Instantaneous ('// &
                    TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
    END IF
  ELSE IF( scenario(BASE_LEVEL)%release(i)%type(1:1) == 'C' )THEN
    IF( scenario(BASE_LEVEL)%release(i)%type(2:2) == 'M' )THEN
      string1 = 'Moving ('//TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
    ELSE IF( scenario(BASE_LEVEL)%release(i)%type(2:2) == 'P' )THEN
      string1 = 'Pool ('//TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
    ELSE IF( scenario(BASE_LEVEL)%release(i)%type(2:2) == 'S' )THEN
      string1 = 'Stack ('//TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
    ELSE
      string1 = 'Continuous ('// &
                    TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
    END IF
  ELSE IF( scenario(BASE_LEVEL)%release(i)%type(1:1) == 'I' )THEN
      string1 = 'Interactive'
  ELSE
      string1 = 'Unknown ('//TRIM(scenario(BASE_LEVEL)%release(i)%matl)//')'
  END IF
  string(16+i) = TRIM(string1)
END DO
nlin = 19

ialign = TA_LEFT .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(1) + ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 3

string( 1) = 'Analyst'
string( 2) = 'Version'
string( 3) = ' '
string( 4) = 'Stop time'
string( 5) = 'Max timestep (sec)'
string( 6) = ' '
IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  string3 = CHAR(176)//'N'
ELSE IF( project(BASE_LEVEL)%MapCoord == I_UTM )THEN
  string3 = 'N''ing'
ELSE
  string3 = 'km'
END IF

string( 7) = 'Y Domain ('//TRIM(string3)//')'
string( 8) = ' '

SELECT CASE( metdef(BASE_LEVEL)%met )
  CASE( MET_OBS )
    string( 9) = 'Upper Air'
    string(10) = 'Surface'
  CASE( MET_FIXED )
    string( 9) = 'Wind speed'
    string(10) = 'Direction'
  CASE DEFAULT
    string( 9) = 'File'
    string(10) = ' '
END SELECT

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_OPERATIONAL )
    string1 = 'Bowen'
  CASE( BL_SIMPLE )
    string1 = 'Inversion (m)'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT, LSV_OBS )
    string1 = 'Scale (km)'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = 'Density (kg/m'//CHAR(179)//')'
string(15) = ' '
string(16) = ' '
string(17) = 'Time (hrs)'
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_RIGHT .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(3) - ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 3/4 Seperater

string( 1) = ':'
string( 2) = ':'
string( 3) = ' '
string( 4) = ':'
string( 5) = ':'
string( 6) = ' '
string( 7) = ':'
string( 8) = ' '
string( 9) = ':'

IF( metdef(BASE_LEVEL)%met == MET_OBS .OR. metdef(BASE_LEVEL)%met == MET_FIXED )THEN
  string(10) = ':'
ELSE
  string(10) = ' '
END IF

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_SIMPLE, BL_OPERATIONAL )
    string1 = ':'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT, LSV_OBS )
    string1 = ':'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = ':'
string(15) = ' '
string(16) = ' '
string(17) = ':'
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_CENTER .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(3)
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 4

string( 1) = project(BASE_LEVEL)%audit%Analyst
IF( string(1) == ' ' .OR. string(1) == ' ' )THEN
  string(1) = 'Not specified'
END IF
string( 2) = project(BASE_LEVEL)%audit%Version
string( 3) = ' '
string( 4) = dlgTime(BASE_LEVEL)%endString
CALL set_real_string( dlgTime(BASE_LEVEL)%time%end%step%max,string1,n )
string( 5) = TRIM(ADJUSTL(string1))
string( 6) = ' '
CALL set_real_string( dlgDomain(BASE_LEVEL)%spatial%domain%yMin,string1,n )
CALL set_real_string( dlgDomain(BASE_LEVEL)%spatial%domain%yMax,string2,n )
string( 7) = TRIM(ADJUSTL(string1))//' - '//TRIM(ADJUSTL(string2))
string( 8) = ' '

SELECT CASE( metdef(BASE_LEVEL)%met )
  CASE( MET_MEDOC, MET_MRF, MET_WRF )
    string1 = metdef(BASE_LEVEL)%medfile
    CALL SplitName(string1,string2,string3)
  CASE( MET_ASSIM )
    string1 = metdef(BASE_LEVEL)%asmfile
    CALL SplitName(string1,string2,string3)
  CASE( MET_OBS, MET_UAIR )
    string1 = metdef(BASE_LEVEL)%profile
    CALL SplitName(string1,string2,string3)
  CASE( MET_SRF )
    string1 = metdef(BASE_LEVEL)%surfile
    CALL SplitName(string1,string2,string3)
  CASE( MET_FIXED )
    SELECT CASE( metdef(BASE_LEVEL)%unit_spd )
      CASE( UNIT_METERS_SEC )
        string1 = ' m/s'
      CASE( UNIT_KNOTS )
        string1 = ' kts'
      CASE( UNIT_MILES_HOUR )
        string1 = ' mph'
      CASE( UNIT_KM_HOUR )
        string1 = ' kph'
      CASE( UNIT_FEET_SEC )
        string1 = ' ft/s'
      CASE DEFAULT
        string1 = ' ?'
    END SELECT
    CALL set_real_string( metdef(BASE_LEVEL)%speed,string2,n )
    string2 = TRIM(ADJUSTL(string2))//TRIM(string1)
  CASE DEFAULT
    string2 = 'Unknown'
END SELECT

string( 9) = TRIM(string2)

SELECT CASE( metdef(BASE_LEVEL)%met )
  CASE( MET_OBS )
    string1 = metdef(BASE_LEVEL)%surfile
    CALL SplitName( string1,string2,string3 )
  CASE( MET_FIXED )
    CALL set_real_string( metdef(BASE_LEVEL)%direction,string2,n )
    string2 = ADJUSTL(string2)
  CASE DEFAULT
    string2 = ' '
END SELECT

string(10) = string2

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_OPERATIONAL )
    CALL set_real_string( metdef(BASE_LEVEL)%bowen,string1,n )
    string1 = ADJUSTL(string1)
  CASE(  BL_SIMPLE )
    CALL set_real_string( metdef(BASE_LEVEL)%zimin,string2,n )
    string2 = ADJUSTL(string2)
    CALL set_real_string( metdef(BASE_LEVEL)%zimax,string3,n )
    string3 = ADJUSTL(string3)
    string1 = TRIM(string2)//' - '//TRIM(string3)
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT, LSV_OBS )
    x = metdef(BASE_LEVEL)%slb
    CALL set_real_string( x,string1,n )
    string1 = ADJUSTL(string1)
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = ' '
string(15) = ' '
string(16) = ' '

n = MIN(3,materials(BASE_LEVEL)%nmatl)
DO i = 1,n
  iaux = materials(BASE_LEVEL)%material(i)%iaux
 IF( IsParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    iaux = iaux + 1
  ELSE IF( IsLiquid(materials(BASE_LEVEL)%material(i)%icls) )THEN
    iaux = iaux + 1 + MAXGMAUX + MAXLMAUXP
  ELSE IF( IsWetParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    iaux = iaux + 1
  ELSE IF( IsAerosol(materials(BASE_LEVEL)%material(i)%icls) )THEN
    iaux = iaux + 1
  END IF
  CALL set_real_string(materials(BASE_LEVEL)%mat_aux(iaux),string1,n)
  string(13+i) = ADJUSTL(string1)
END DO

string(17) = ' '
string(18) = ' '
string(19) = ' '

n = MIN(3,scenario(BASE_LEVEL)%nrel)
DO i = 1,n
  CALL set_real_string( scenario(BASE_LEVEL)%release(i)%time,string1,n )
  string(16+i) = TRIM(string1)
END DO
nlin = 19

ialign = TA_LEFT .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(3) + ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 5
string( 1) = ' '
string( 2) = 'Created'
string( 3) = ' '
string( 4) = 'Current time'
string( 5) = 'Output interval (hrs)'
string( 6) = ' '
IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  string3 = CHAR(176)
ELSE
  string3 = 'km'
END IF

string( 7) = 'Resolution ('//TRIM(string3)//')'
string( 8) = ' '
string( 9) = ' '
string(10) = ' '

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_OPERATIONAL )
    string1 = 'Albedo'
  CASE( BL_SIMPLE )
    string1 = 'Heat flux (W/m'//CHAR(178)//')'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT )
    string1 = 'Turbulence (m'//CHAR(178)//'/s'//CHAR(178)//')'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = ' '
string(15) = ' '
string(16) = ' '

n = MIN(3,materials(BASE_LEVEL)%nmatl)
DO i = 1,n
  IF( IsGas(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = 'Deposition (cm/s)'
  ELSE IF( IsAerosol(materials(BASE_LEVEL)%material(i)%icls) )THEN
    string1 = 'Deposition (cm/s)'
  ELSE
    string1 = 'Size range ('//CHAR(181)//'m)'
  END IF
  string(13+i) = TRIM(string1)
END DO
IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  string1 = '('//CHAR(176)//'E,'//CHAR(176)//'N,m)'
ELSE
  string1 = '(km,km,m)'
END IF

string(17) = 'Location'//TRIM(string1)
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_RIGHT .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(5) - ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 5/6 Seperater

string( 1) = ' '
string( 2) = ':'
string( 3) = ' '
string( 4) = ':'
string( 5) = ':'
string( 6) = ' '
string( 7) = ':'
string( 8) = ' '
string( 9) = ' '
string(10) = ' '

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_SIMPLE, BL_OPERATIONAL )
    string1 = ':'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT )
    string1 = ':'
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = ' '
string(15) = ' '
string(16) = ' '
n = MIN(3,materials(BASE_LEVEL)%nmatl)
DO i = 1,n
  string(13+i) = ':'
END DO
string(17) = ':'
string(18) = ' '
string(19) = ' '
nlin = 17

ialign = TA_CENTER .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(5)
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Column 6

string( 1) = ' '
string( 2) = project(BASE_LEVEL)%audit%CreateDate
string( 3) = ' '
IF( nTimePuff <= 0 )THEN
  string( 4) = dlgTime(BASE_LEVEL)%startString
ELSE
  string( 4) = timePuff(nTimePuff)%string
END IF
CALL set_real_string( dlgTime(BASE_LEVEL)%time%end%step%output,string1,n )
string( 5) = TRIM(ADJUSTL(string1))
string( 6) = ' '
CALL set_real_string( dlgDomain(BASE_LEVEL)%spatial%domain%hRes,string1,n )
string( 7) = TRIM(ADJUSTL(string1))
string( 8) = ' '
string( 9) = ' '
string(10) = ' '

SELECT CASE( metdef(BASE_LEVEL)%bl )
  CASE( BL_CALC, BL_OPERATIONAL )
    CALL set_real_string( metdef(BASE_LEVEL)%albedo,string1,n )
    string1 = ADJUSTL(string1)
  CASE( BL_SIMPLE )
    CALL set_real_string( metdef(BASE_LEVEL)%hconst,string2,n )
    string2 = ADJUSTL(string2)
    CALL set_real_string( metdef(BASE_LEVEL)%hdiur,string3,n )
    string3 = ADJUSTL(string3)
    string1 = TRIM(string2)//' - '//TRIM(string3)
  CASE DEFAULT
    string1 = ' '
END SELECT

string(11) = string1

SELECT CASE( metdef(BASE_LEVEL)%lsv )
  CASE( LSV_INPUT )
    CALL set_real_string( metdef(BASE_LEVEL)%uub,string1,n )
    string1 = ADJUSTL(string1)
  CASE DEFAULT
    string1 = ' '
END SELECT

string(12) = string1
string(13) = ' '
string(14) = ' '
string(15) = ' '
string(16) = ' '

n = MIN(3,materials(BASE_LEVEL)%nmatl)
DO i = 1,n
  iaux = materials(BASE_LEVEL)%material(i)%iaux
  IF( IsGas(materials(BASE_LEVEL)%material(i)%icls) )THEN
    x = materials(BASE_LEVEL)%mat_aux(iaux+1)*100.
    CALL set_real_string( x,string2,n )
    string1 = TRIM(ADJUSTL(string2))
  ELSE IF( IsAerosol(materials(BASE_LEVEL)%material(i)%icls) )THEN
    x = materials(BASE_LEVEL)%mat_aux(iaux)*100.
    CALL set_real_string( x,string2,n )
    string1 = TRIM(ADJUSTL(string2))
  ELSE IF( IsParticle(materials(BASE_LEVEL)%material(i)%icls) &
         .OR. IsWetParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    j = NINT(materials(BASE_LEVEL)%mat_aux(iaux))
    x = materials(BASE_LEVEL)%mat_aux(iaux+PMAUX_BOUNDS)*1.e6
    CALL set_real_string( x,string2,n )
    x = materials(BASE_LEVEL)%mat_aux(iaux + j*MAXPMAUX + PMAUX_BOUNDS)*1.e6
    CALL set_real_string( x,string3,n )
    string1 = TRIM(ADJUSTL(string2))//' - ' &
      //TRIM(ADJUSTL(string3))
  ELSE
    iaux = iaux + MAXGMAUX + MAXLMAUXP
    j = NINT(materials(BASE_LEVEL)%mat_aux(iaux))
    x = materials(BASE_LEVEL)%mat_aux(iaux+LMAUX_BOUNDS)*1.e6
    CALL set_real_string( x,string2,n )
    x = materials(BASE_LEVEL)%mat_aux(iaux + j*MAXLMAUX + LMAUX_BOUNDS)*1.e6
    CALL set_real_string( x,string3,n )
    string1 = TRIM(ADJUSTL(string2))//' - ' &
      //TRIM(ADJUSTL(string3))
  END IF
  string(13+i) = ADJUSTL(string1)
END DO

string(17) = ' '
string(18) = ' '
string(19) = ' '

n = MIN(3,scenario(BASE_LEVEL)%nrel)
DO i = 1,n
  CALL set_real_string( SNGL(scenario(BASE_LEVEL)%release(i)%xRel),string1,n )
  CALL set_real_string( SNGL(scenario(BASE_LEVEL)%release(i)%yRel),string2,n )
  IF( scenario(BASE_LEVEL)%release(i)%zRel /= NOT_SET_R )THEN
    CALL set_real_string( scenario(BASE_LEVEL)%release(i)%zRel,string3,n )
    string(16+i) = '('//TRIM(ADJUSTL(string1))//','// &
                  TRIM(ADJUSTL(string2))//','// &
                  TRIM(ADJUSTL(string3))//')'
  ELSE
    string(16+i) = '('//TRIM(ADJUSTL(string1))//','// &
                  TRIM(ADJUSTL(string2))//')'
  END IF
END DO
nlin = 19

ialign = TA_LEFT .OR. TA_BOTTOM
j      = SetTextAlign( idc,ialign )

ix = tabs(5) + ioff
iy = iyt + 3*iyinc/2

DO j = 1,nlin
  IF( string(j) /= ' ' )THEN
    n = LEN(TRIM(string(j)))
    i = TextOut( idc,ix,iy,string(j),n )
  END IF
  iy = iy + iyinc
END DO

!---- Restore original font and alignment

CALL RestoreFont( idc,ifont,jfont )
ialign = SetTextAlign( idc,jalign )

!---- Clean up

kpn = SelectObject( idc,jpn ) !Return previous pen
kbr = SelectObject( idc,jbr ) !Return previous brush
kpn = DeleteObject( ipn ) !Delete Pen
kbr = DeleteObject( ibr ) !Delete brush

RETURN
END

