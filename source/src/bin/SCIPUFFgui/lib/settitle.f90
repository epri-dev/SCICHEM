!*******************************************************************************
!             SetTitlesDefault
!*******************************************************************************
SUBROUTINE SetTitlesDefault( cttl,cttu,ctt3,exceed )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE GUImatl_fi
USE pltchoice_fi
USE param_fd

!     Set Default Plot titles
!************************
!************************
!
!NOTE - Does not handle CODA or Hazard Areas [or multicomponent]
!
!************************
!************************

IMPLICIT NONE

CHARACTER(*) cttl !First Title String
CHARACTER(*) cttu !Second Title String
CHARACTER(*) ctt3 !Third Title String
CHARACTER(*) exceed !Probability Exceedance value string

TYPE( char64T ) CurClass,CurKind,CurChoice
INTEGER         CurCat

INTEGER nn,i,j,imat

LOGICAL   prob,ldum
LOGICAL   InRange
LOGICAL   lTimePlot

LOGICAL, EXTERNAL :: IsEvap

TYPE( timeT ) tim

ctt3 = ' '
cttu = ' '
cttl = ' '

!===== Upper Titles
!         PCSCIPUF - Project Title
!                    Plot Type
!         TERRAIN  - Nothing
!                  - Project Title

IF( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == 'Met/Terrain' )THEN

  ctt3 = ' '
  IF( LEN_TRIM(project(BASE_LEVEL)%Title) <= 0 )THEN
    cttu = ' '
  ELSE
    cttu = TRIM(project(BASE_LEVEL)%Title)
  END IF

ELSE

  IF( LEN_TRIM(project(BASE_LEVEL)%Title) <= 0 )THEN
    ctt3 = ' '
  ELSE
    ctt3 = TRIM(project(BASE_LEVEL)%Title)
  END IF

  IF( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == 'Concentration' )THEN

    cttu = TRIM(CATEGORY_STRING(PlotDef(BASE_LEVEL)%Field%Category))
	  SELECT CASE( PlotDef(BASE_LEVEL)%Field%Category )
	    CASE( HP_SURF )
	      cttu = TRIM(cttu)//' Concentration'
	    CASE( HP_HSLICE )
        CALL c_format( PlotDef(BASE_LEVEL)%ClassData(HSLICE_INDEX)%zmin,nn,string3 )
        cttu = TRIM(cttu)//' at z = '//string3(1:nn)//'m'
	    CASE DEFAULT
	  END SELECT

  ELSE IF( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) == '3D Met' )THEN

    cttu = TRIM(CATEGORY_STRING(PlotDef(BASE_LEVEL)%Field%Category))
	  SELECT CASE( PlotDef(BASE_LEVEL)%Field%Category )
	    CASE( HP_HSLICE )
        CALL c_format( PlotDef(BASE_LEVEL)%ClassData(HSLICE_INDEX)%zmin,nn,string3 )
        cttu = TRIM(cttu)//' at z = '//string3(1:nn)//'m'
	    CASE DEFAULT
	  END SELECT

  ELSE

    cttu = TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string)
	  SELECT CASE( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) )

	    CASE( 'Surface Deposition' )
	      imat = PlotDef(BASE_LEVEL)%Field%Choice
!!DEC$ IF DEFINED (LIQUID)
!  		  IF( imat <= materials(BASE_LEVEL)%nmatl )THEN
!          IF( IsEvap(materials(BASE_LEVEL)%material(imat)%icls) )THEN
!		        IF( TRIM(KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string) == 'Total' )THEN
! 	            cttu = TRIM(cttu)//' w/o Secondary Evaporation'
!			      END IF
!		      END IF
!		    END IF
!!DEC$ ENDIF

	    CASE( 'Surface Dosage' )
        IF( dlgOptions(BASE_LEVEL)%zDosage > 0.0 )THEN
          CALL c_format( dlgOptions(BASE_LEVEL)%zDosage,nn,string3 )
          cttu = TRIM(cttu)//' ('//string3(1:nn)//'m)'
		    END IF

	    CASE( 'SCIP Radiation Dose' )
	      IF( TRIM(KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string) == 'Dose Rate' )THEN
          cttu = TRIM(cttu)//' Rate'
  		  END IF

	    CASE( 'NWPN Radiation Dose' )
	      cttu = 'Nuclear Weapon Fallout Dose'
	      IF( TRIM(KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string) == 'Dose Rate' )THEN
          cttu = TRIM(cttu)//' Rate'
  		  END IF

	    CASE( 'NWPN Prob Casualty' )
	      cttu = 'Nuclear Weapon Probability of '// &
		         TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)//' '// &
		         TRIM(KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string)

	    CASE( 'NWPN Casualty Table' )
	      cttu = 'Nuclear Weapon Casualty Estimate '// &
		         TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)

	    CASE DEFAULT

	  END SELECT
  END IF
END IF

!===== Lower Title
!         Radiation dose  - Group(Dose Type) and Time
!         Terrain         - Terrain
!         NWPN Casualties - ????
!         CODA            - CODA title and Time
!         All Others      - Material,Group and Time

!---- Radiation Dose

IF( ttldef(BASE_LEVEL)%ShowDate )THEN
  IF( BTEST(project(BASE_LEVEL)%Mode,REVERSE_MODE) )THEN
    lTimePlot = ( nTimePlot > 0 ) .OR. &
          ( INDEX(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string,'Source Estimation') /= 0 )
  ELSE
    lTimePlot = nTimePlot > 0
  END IF
  IF( lTimePlot .AND. PlotDef(BASE_LEVEL)%Field%TimeID /= DEF_VAL_I )THEN
    IF( BTEST(project(BASE_LEVEL)%Mode,REVERSE_MODE) )THEN
      IF( INDEX(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string,'Dosage') /= 0 .OR. &
          INDEX(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string,'Cont') /= 0 )THEN
        tim = timeSrf(PlotDef(BASE_LEVEL)%Field%TimeID)%time
      ELSE
        tim = timePuff(PlotDef(BASE_LEVEL)%Field%TimeID)%time
      END IF
    ELSE
      tim = timePlot(PlotDef(BASE_LEVEL)%Field%TimeID)%time
    END IF
    IF( .NOT.BTEST(project(BASE_LEVEL)%Mode,REVERSE_MODE) )THEN
      tim%runTime = PlotDef(BASE_LEVEL)%Field%UserTime
      CALL ComputeEndTime( dlgTime(BASE_LEVEL)%time%start%time,tim,ldum )
    END IF
    CALL time_string( tim,string2 )
	  string3 = ' at '//TRIM(string2)
  ELSE
    string3 = ' '
  END IF
ELSE
  string3 = ' '
END IF

IF( ttldef(BASE_LEVEL)%ShowTime )THEN
  IF( lTimePlot )THEN
    CALL format_time( PlotDef(BASE_LEVEL)%Field%UserTime,string1,0 )
    IF( ttldef(BASE_LEVEL)%ShowDate )THEN
      string2 = TRIM(string3)//' ('//TRIM(string1)//')'
      string3 = TRIM(string2)
    ELSE
      string3 = ' at T = '//TRIM(string1)
    END IF
  END IF
END IF

nn = LEN_TRIM(string3)

CurCat    = PlotDef(BASE_LEVEL)%Field%Category
CurClass  = ClassStr(PlotDef(BASE_LEVEL)%Field%Class)
CurChoice = ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)
IF( ClassChoiceArray(PlotDef(BASE_LEVEL)%Field%Class,PlotDef(BASE_LEVEL)%Field%Choice)%kind &
      == SCIPtrue )THEN
  CurKind = KindStr(PlotDef(BASE_LEVEL)%Field%Kind)
ELSE
  CurKind%string = ' '
END IF

  prob = CatClassArray(CurCat,PlotDef(BASE_LEVEL)%Field%Class)%type == SCIPtrue
!  prob = .TRUE.
  SELECT CASE( TRIM(ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string) )
    CASE( 'Met/Terrain' )
      cttl = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)
!      prob = .FALSE.

    CASE( 'SCIP Radiation Dose' )
      cttl = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)

    CASE( 'RTH Radiation Field' )
      cttl = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)

    CASE( 'NWPN Radiation Dose' )
      cttl = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)

    CASE( 'NWPN Prob Casualty' )
!      prob = .FALSE.

    CASE( 'NWPN Casualty Table' )
!      prob = .FALSE.

    CASE DEFAULT
	    i = ClassChoiceArray(PlotDef(BASE_LEVEL)%Field%Class,PlotDef(BASE_LEVEL)%Field%Choice)%ikind
	    j = i + ClassChoiceArray(PlotDef(BASE_LEVEL)%Field%Class,PlotDef(BASE_LEVEL)%Field%Choice)%nkind - 1
      IF( InRange(i,j,PlotDef(BASE_LEVEL)%Field%Kind) )THEN
        cttl = TRIM(KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string)//' '// &
	             TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)
	    ELSE
        cttl = TRIM(ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string)
	    END IF

  END SELECT

IF( prob )THEN
  string4 = cttl
  SELECT CASE( PlotDef(BASE_LEVEL)%Type )
    CASE( HP_PROB )
	   cttl = 'Prob('//TRIM(string4)//') > '//TRIM(exceed)

    CASE( HP_EXCEED )
	   cttl = 'Pc('//TRIM(string4)//') > '//TRIM(exceed)

    CASE( HP_NUMTYP+1 )
	   cttl = 'Hazard Area('//TRIM(string4)//')'

	CASE DEFAULT

  END SELECT
END IF

IF( lTimePlot )cttl = TRIM(cttl)//TRIM(string3)

RETURN
END
!***********************************************************************
!               SetLabels
!***********************************************************************
SUBROUTINE SetLabels( llc,zone,scx,scy,lflg,clbx,clby )

USE resource_fd
USE pcscipuf_fi
USE param_fd
USE dialog_fi
USE PlotTrans_fi

!     Set Default Axes labels

IMPLICIT NONE

LOGICAL       llc !Local Coordinate Flag
INTEGER      zone !UTM zone
REAL          scx !X axis scale
REAL          scy !Y axis scale
LOGICAL       lflg !Vertical Slice Flag
CHARACTER(*) clbx !X Axis Label String
CHARACTER(*) clby !Y Axis Label String

CHARACTER(2) tail

!---- X Axis

IF( project(BASE_LEVEL)%MapCoord == I_METERS )THEN
  tail = 'm'
ELSE
  tail = 'km'
END IF

IF( clbx(1:7) == 'default' )THEN
  IF( scx == 1.0 )THEN
    IF( .NOT.llc )THEN !Lat/Lon - not local coordinates
      IF( lflg )THEN !  Vertical slice
        clbx = '|Longitude|Latitude||'
      ELSE !  Horizontal Plots
        clbx = 'Longitude'
      END IF
    ELSE
      IF( zone <= 0 )THEN !Cartesian
        IF( lflg )THEN !  Vertical slice
          clbx = '|X|Y|'//TRIM(tail)//'|'
        ELSE !  Horizontal Plots
          clbx = 'X ('//TRIM(tail)//')'
        END IF
      ELSE
        IF( lflg )THEN !  Vertical slice
          clbx = '|Easting|Northing||'
        ELSE !  Horizontal Plots
          clbx = 'Easting'
        END IF
      END IF
    END IF
  ELSE
    IF( lflg )THEN !  Vertical slice
      clbx = 'X,Y'
    ELSE !  Horizontal Plots
      clbx = 'X'
    END IF
  END IF
END IF

!---- Y Axis

IF( clby(1:7) == 'default' )THEN
  IF( (.NOT.lflg .AND. scy == 1.0) .OR. (lflg .AND. scy == .001) )THEN
    IF( lflg )THEN
      clby = 'Z (km)'
    ELSE
      IF( .NOT.llc )THEN !Lat/Lon - not local coordinates
        clby = 'Latitude'
      ELSE !Cartesian
        IF( zone <= 0 )THEN !Cartesian
          clby = 'Y ('//TRIM(tail)//')'
        ELSE
          clby = 'Northing'
        END IF
      END IF !
    END IF
  ELSE
    IF( lflg )THEN !  Vertical slice
      clby = 'Z (km)'
    ELSE !  Horizontal Plots
      clby = 'Y'
    END IF
  END IF
END IF

RETURN
END
!***********************************************************************
!              SetAxes
!***********************************************************************
SUBROUTINE SetAxes( xmnb,xmxb,scx,ymnb,ymxb,scy,boxl,iflag )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi

!     Set Default Axes

IMPLICIT NONE

REAL xmnb !X Minimum
REAL xmxb !X Maximum
REAL scx  !X Scale
REAL ymnb !Y Minimum
REAL ymxb !Y Maximum
REAL scy  !Y Scale
REAL boxl !Plot size
INTEGER iflag !Map scale flag

LOGICAL lwnd
REAL    xmnd,xmxd,ymnd,ymxd

xmnb = axesdef(BASE_LEVEL)%dbreal(17)
xmxb = axesdef(BASE_LEVEL)%dbreal(18)
scx  = axesdef(BASE_LEVEL)%dbreal(19)
ymnb = axesdef(BASE_LEVEL)%dbreal(20)
ymxb = axesdef(BASE_LEVEL)%dbreal(21)
scy  = axesdef(BASE_LEVEL)%dbreal(22)

IF( iflag > 0 )THEN
  IF( map_scale /= DEF_VAL_R )THEN
    boxl = -1./(7.*Fmap_scales(iflag)*map_scale)
    CALL set_plot_axes( lwnd,xmnd,xmxd,ymnd,ymxd )
  END IF
END IF

RETURN
END
!*******************************************************************************
!             SetTitles
!*******************************************************************************
SUBROUTINE SetTitles( lttl,cttl,lttu,cttu,ltt3,ctt3,exceed )

USE resource_fd
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE GUImatl_fi

!     Set Default Plot titles

IMPLICIT NONE

LOGICAL       lttl !First Title flag
CHARACTER(*)  cttl !First Title String
LOGICAL       lttu !Second Title flag
CHARACTER(*)  cttu !Second Title String
LOGICAL       ltt3 !Third Title flag
CHARACTER(*)  ctt3 !Third Title String
CHARACTER(*) exceed !Probability Exceedance value string

CHARACTER(128) defttl,defttu,deftt3

!----- Set Default plot titles

!===== Upper Titles
!         HASCAL -   Project Title
!                    Country and Plant
!         PCSCIPUF - Project Title
!                    Plot Type
!         TERRAIN  - Nothing
!                  - Project Title
!===== Lower Title
!         Radiation dose - Group(Dose Type) and Time
!         Terrain        - Terrain
!         All Others     - Material,Group and Time

CALL SetTitlesDefault( defttl,defttu,deftt3,exceed )

!----- Copy to plot title string if necessary

IF( cttu(1:7) == 'default' .AND. lttu )cttu = defttu
IF( ctt3(1:7) == 'default' .AND. ltt3 )ctt3 = deftt3
IF( cttl(1:7) == 'default' .AND. lttl )cttl = defttl

RETURN
END
