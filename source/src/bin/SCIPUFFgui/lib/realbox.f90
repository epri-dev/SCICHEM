!***********************************************************************
!             ProcressReal
!***********************************************************************
RECURSIVE SUBROUTINE process_real( iwnd_db,MyCmd )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE errorParam_fd
USE plotdlg_fi
USE GUImatl_fi
USE pltchoice_fi
USE coordinate_fd
USE SwinAPI

!     This routine processes real EDITBOXes from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN),     INTENT( IN ) :: iwnd_db !Dialog Box handle
TYPE( CMD ),              INTENT( IN ) :: MyCmd   !Command Structure

CHARACTER(128) ctemp

INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER ncht,jd_level,indx,irv,i
INTEGER(POINTER_LEN) jwnd_db
REAL    x,x1,x2,y1,y2
LOGICAL visible,ldef

INTEGER, EXTERNAL :: NearestPlotTimeGUI
LOGICAL, EXTERNAL :: hasError,MoveMouse

CHARACTER(128), EXTERNAL ::AddNull

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Get New Value as a string (Return value is number of characters returned)

ncht = GetDlgItemText( iwnd_db,id_cntrl,ctemp,128 )

!---- Process String

CALL clower( ctemp )
ctemp = ADJUSTL(ctemp(1:ncht))

ldef = .FALSE.

!---- Process value

IF( ncht <= 0 )THEN

  SELECT CASE( id_dialog )

    CASE( IDB_PLOT )
      SELECT CASE( id_button )

        CASE( 3 )
          x = timePlot(nTimePlot-1)%time%runTime/24. + 30.

        CASE DEFAULT
          x = 0.

      END SELECT

    CASE DEFAULT
      x = NOT_SET_R

  END SELECT

ELSE IF( TRIM(ctemp) == 'deferred' )THEN

  x = DEFERRED_R

ELSE IF( TRIM(ctemp) == 'default' )THEN

  SELECT CASE( id_dialog )

    CASE( IDB_METDEF )
      x = metdef(DEFAULT_LEVEL)%dbreal(id_button)

    CASE( IDB_MATDEF )
      IF( id_button <= 2 )THEN
        x = materials(DEFAULT_LEVEL)%material(1)%prop(id_button)
      ELSE IF( id_button == 3 )THEN
        x = materials(DEFAULT_LEVEL)%material(1)%prop(4)
      ELSE
        x = DEF_VAL_R
      END IF

    CASE( IDB_DOMAIN )
      SELECT CASE (id_button)

        CASE( 1 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lon

        CASE( 2 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lat

        CASE( 3 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%xMin

        CASE( 4 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%xMax

        CASE( 5 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%yMin

        CASE( 6 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%yMax

        CASE( 7 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%hRes

        CASE( 8 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%zMax

        CASE( 9 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%domain%vRes

        CASE( 10 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%reference%x

        CASE( 11 )
          x = dlgDomain(DEFAULT_LEVEL)%spatial%reference%y

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE( IDB_OPTIONS )
      SELECT CASE( id_button )
        CASE( 1 )
          x = dlgOptions(DEFAULT_LEVEL)%delmin

        CASE( 2 )
          x = dlgOptions(DEFAULT_LEVEL)%wwTrop

        CASE( 3 )
          x = dlgOptions(DEFAULT_LEVEL)%slTrop

        CASE( 4 )
          x = dlgOptions(DEFAULT_LEVEL)%epsTrop

        CASE( 5 )
          x = dlgOptions(DEFAULT_LEVEL)%massMin

        CASE( 6 )
          x = dlgOptions(DEFAULT_LEVEL)%timeAvg

        CASE( 7 )
          x = dlgOptions(DEFAULT_LEVEL)%uuCalm

        CASE( 8 )
          x = dlgOptions(DEFAULT_LEVEL)%slCalm

        CASE( 9 )
          x = dlgOptions(DEFAULT_LEVEL)%zDosage

        CASE( 10 )
          x = dlgOptions(DEFAULT_LEVEL)%dtSampler

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE( IDB_PLOT )
      SELECT CASE( id_button )

        CASE ( 3 )
          x = timePlot(nTimePlot-1)%time%runTime/24. + 30.

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE( IDB_SCIPUF )
      SELECT CASE( id_button )

        CASE( 2 )
          CALL set_default_save( dbreal(4,id_level),idbcmbo(1,id_level), &
                                 dlgTime(DEFAULT_LEVEL)%time%end%step%output,x )
          ldef = .TRUE.

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE( IDB_TIME )
      SELECT CASE( id_button )

        CASE( 2 )
          x = dlgTime(DEFAULT_LEVEL)%time%start%time%hour

        CASE( 3 )
          x = dlgTime(DEFAULT_LEVEL)%time%end%time%hour

        CASE( 4 )
          x = dlgTime(DEFAULT_LEVEL)%time%end%time%runTime

        CASE( 5 )
          x = dlgTime(DEFAULT_LEVEL)%time%end%step%max
          IF( idbcmbo(2,id_level) == 2 )THEN
            x = x/60.
          ELSE IF( idbcmbo(2,id_level) == 3 )THEN
            x = x/3600.
          END IF

        CASE( 6 )
          CALL set_default_save( dbreal(4,id_level),idbcmbo(1,id_level), &
                                 dlgTime(DEFAULT_LEVEL)%time%end%step%output,x )
          lcheck(1,id_level) = .TRUE.

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE( IDB_TERPARM )
      SELECT CASE( id_button )

        CASE( 1 )
          x = metdef(DEFAULT_LEVEL)%alpmin

        CASE( 2 )
          x = metdef(DEFAULT_LEVEL)%alpmax

        CASE( 3 )
          x = metdef(DEFAULT_LEVEL)%epsfft

        CASE( 4 )
          x = metdef(DEFAULT_LEVEL)%epsprm

        CASE DEFAULT
          x = DEF_VAL_R

      END SELECT

    CASE DEFAULT
      x = DEF_VAL_R

  END SELECT

ELSE IF( ctemp == 'n/a' )THEN

  RETURN

ELSE

  READ(ctemp(1:ncht),'(F15.0)',ERR=1000,END=1000)x

END IF

!---- Apply limits to value

IF( x /= DEF_VAL_R .AND. x /= NOT_SET_R .AND. x /= DEFERRED_R )THEN
  SELECT CASE( id_dialog )

    CASE( IDB_TERPARM )
      SELECT CASE( id_button )

        CASE( 1,2 )
          x = MAX(x,0.)
          x = MIN(x,1.)

        CASE( 3,4 )
          x = MAX(x,0.)

        CASE DEFAULT

      END SELECT

    CASE( IDB_DOMAIN,IDB_SETUP )
      SELECT CASE( id_button )

        CASE( 1 )
          x = MAX(x,-360.)
          x = MIN(x,360.)

        CASE( 2 )
          x = MAX(x,-90.)
          x = MIN(x,90.)

        CASE( 5,6 )
          IF( ichoice(1,id_level) == I_LATLON )THEN
            x = MAX(x,-360.)
            x = MIN(x,360.)

          END IF
        CASE( 3,4 )
          IF( ichoice(1,id_level) == I_LATLON )THEN
            x = MAX(x,-90.)
            x = MIN(x,90.)

          END IF
        CASE( 13,14,15,16,17,18 )
          x = MAX(x,0.)
          x = MIN(x,59.9999)

        CASE DEFAULT

      END SELECT

    CASE( IDB_PLOT )
      SELECT CASE( id_button )

        CASE( 1 )
          x = MAX(x,0.0)

        CASE( 3 )
          IF( InputTimeMax < DEF_VAL_R )x = MIN(x,InputTimeMax)

        CASE( 4,5 )
          x = MAX(x,  0.001)
          x = MIN(x, 99.999)

        CASE DEFAULT

      END SELECT

    CASE( IDB_MATDEF )
      SELECT CASE( id_button )

        CASE( 3 )
          x = MAX(x,-2.0)
          x = MIN(x,-0.3)

        CASE DEFAULT

      END SELECT

    CASE( IDB_RELDEF )
      SELECT CASE( id_button )

        CASE( 1 )
          x = MAX(x,0.)

        CASE( 2 )
          IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
            x = MAX(x,-89.9)
            x = MIN(x,89.9)
          END IF

        CASE( 3 )
          IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
            x = MAX(x,-360.)
            x = MIN(x,360.)
          END IF

        CASE( 15,16 )
          x = MAX(x,0.)
          x = MIN(x,59.9999)

        CASE( 23 )
          x = MAX(x,0.)
          x = MIN(x,1.)

        CASE( 26 )
          x = MAX(x,0.)
          x = MIN(x,1.)

        CASE DEFAULT

      END SELECT

    CASE DEFAULT

  END SELECT

END IF

!---- Set New Value

dbreal(id_button,id_level) = x

!---- Post Value back to Dialog Box

100 CONTINUE

CALL SetEditRs( iwnd_db,dbreal(id_button,id_level),id_button,1 )

!---- Check for additional actions

SELECT CASE( id_dialog )

  CASE( IDB_PLOT ) !PLOT
    SELECT CASE( id_button )

      CASE( 1 ) !  Exceed value
        CALL IsControlVisible( iwnd_db,REAL_BASE+id_button,visible )
!           This apparently gets called when we do a DEFAULT from the PLOT window
!           with an SCIP project.  Even though we set dbtext correctly in the COMBO
!           routines after we load the EXCEED real box it apparently comes here after
!           we've done so.  So to prevent overwritting the correct EXCEED label we will
!           prevent this part except when the REAL box is visible
        IF( visible )CALL c_format( x,ncht,dbtext(1,id_level) )

      CASE( 2 ) !  Horizonal Slice location
        lcheck( 9,id_level) = .TRUE.

      CASE( 3 ) !  NWPN extended time
        IF( x <= timePlot(nTimePlot-1)%time%runTime/24.+0.01 )THEN
	       i = NearestPlotTimeGUI( timePlot,nTimePlot,x*24. )
           CALL SetListSelString( iwnd_db,TIME_COMBO,timePlot(i)%string,irv )
           CALL EnableControl( iwnd_db,USER_TIME_EDIT ,FALSE )
           CALL ShowControl  ( iwnd_db,USER_TIME_EDIT ,SW_HIDE )
           CALL ShowControl  ( iwnd_db,USER_TIME_LABEL,SW_HIDE )
        ELSE
          lcheck(10,id_level) = .TRUE.
        END IF

      CASE( 5 ) !  Casualty Risk Level
        lcheck(10,id_level) = .TRUE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_AXES ) !AXES
    IF( id_button <= 2 .OR. id_button > 9 )THEN !  Reference Point
      CALL reset_local( iwnd_db,ichoice(1,id_level) )
    END IF

  CASE( IDB_MAPS ) !MAPS
    IF( id_button >= 1 )THEN !  Population Density
      CALL FindHwndListId( IDB_PLOT,jwnd_db,jd_level ) !  Parent = PLOT
      lcheck(10,jd_level) = .TRUE.
    END IF

  CASE( IDB_COMLST ) !COMLST - Subgroup compute
    CALL update_compute_buttons( iwnd_db )

  CASE( IDB_TIME ) !TIME
    SELECT CASE( id_button )

      CASE( 2,3 ) !  Start Hours, End Hours
        CALL UpdateDuration( iwnd_db )

      CASE( 4 ) !  Duration
        CALL UpdateEndTime( iwnd_db )

      CASE( 6 ) !  save output time
        IF( lcheck(1,id_level) )THEN
          indx = 2
          CALL SetListSel( iwnd_db,IDB_COMBO3,indx,irv )
          CALL GetListItem( iwnd_db,IDB_COMBO3,indx,string1,irv )
          idbcmbo(3,id_level) = indx + 1
          dbcmbo(3,id_level)  = AddNull( TRIM(string1(1:irv)) )
        END IF

      CASE DEFAULT

    END SELECT

  CASE( IDB_SCIPUF ) !RUN
    SELECT CASE( id_button )

      CASE( 2 ) !  save output time
        IF( ldef )THEN
          indx = 2
          CALL SetListSel( iwnd_db,IDB_COMBO3,indx,irv )
          CALL GetListItem( iwnd_db,IDB_COMBO3,indx,string1,irv )
          idbcmbo(3,id_level) = indx + 1
          dbcmbo(3,id_level)  = AddNull( TRIM(string1(1:irv)) )
        END IF

      CASE( 3 ) !  End Hours
        CALL UpdateDuration( iwnd_db )

      CASE( 4 ) !  Duration
        CALL UpdateEndTime( iwnd_db )

      CASE DEFAULT

    END SELECT

  CASE( IDB_DOMAIN,IDB_SETUP ) !DOMAIN
    IF( id_button >= 1 .AND. id_button <= 6 )THEN
      i = id_button
      IF( i <= 2 .OR. ichoice(1,id_level) == I_LATLON )THEN
        CALL compute_DMS( dbreal(i,id_level),dbint(i+6,id_level), &
                          dbint(i+12,id_level),dbreal(i+12,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(i+12,id_level),i+12,1 )
        CALL SetEditIs( iwnd_db,dbint (i+6 ,id_level),i+6 ,1 )
        CALL SetEditIs( iwnd_db,dbint (i+12,id_level),i+12,1 )
      END IF
    ELSE IF( id_button >= 13 .AND. id_button <= 18 )THEN
      i = id_button - 12
      IF( i <= 2 .OR. ichoice(1,id_level) == I_LATLON )THEN
        CALL compute_degrees( dbint(i+6,id_level),dbint(i+12,id_level), &
                              dbreal(i+12,id_level),dbreal(i,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(i,id_level),i,1 )
      END IF
    END IF

  CASE( IDB_RELDEF ) !RELEASE
    SELECT CASE( id_button )

      CASE( 2 ) !  X Deg
        CALL compute_DMS( dbreal(2,id_level),dbint(4,id_level), &
                          dbint(5,id_level),dbreal(15,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(15,id_level),15,1 )
        CALL SetEditIs( iwnd_db,dbint ( 4,id_level), 4,2 )

      CASE( 3 ) !  Y Deg
        CALL compute_DMS( dbreal(3,id_level),dbint(6,id_level), &
                          dbint(7,id_level),dbreal(16,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(16,id_level),16,1 )
        CALL SetEditIs( iwnd_db,dbint ( 6,id_level), 6,2 )

      CASE( 15 )
        IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN !  X Sec
          CALL compute_degrees8( dbint(4,id_level),dbint(5,id_level), &
                                 dbreal(15,id_level),dbreal8(2,id_level) )
          CALL SetEditR8s( iwnd_db,dbreal8(2,id_level),2,1 )
        END IF

      CASE( 16 )
        IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN !  Y Sec
          CALL compute_degrees8( dbint(6,id_level),dbint(7,id_level), &
                                dbreal(16,id_level),dbreal8(3,id_level) )
          CALL SetEditR8s( iwnd_db,dbreal8(3,id_level),3,1 )
        END IF

      CASE( 23 )
        CALL show_dynamic( iwnd_db,dbint(2,id_level),dbint(3,id_level),dbreal(23,id_level) )

      CASE DEFAULT

    END SELECT

  CASE( IDB_PICK ) !PICK
    dbreal(3,id_level) = NOT_SET_R
    CALL EnableControl( iwnd_db,IDB_REAL3,TRUE )
    CALL SetEditRs    ( iwnd_db,dbreal(3,id_level),3,1 )
    CALL EnableControl( iwnd_db,IDB_REAL3,FALSE )

  CASE( IDB_ZOOM,IDB_SLICE ) !SLICE,ZOOM
    IF( id_button <= 4 )THEN
      x1 = dbreal(1,id_level)
      y1 = dbreal(2,id_level)
      x2 = dbreal(3,id_level)
      y2 = dbreal(4,id_level)
      IF( MoveMouse(x1,y1,x2,y2) )THEN
        dbreal(1,id_level) = x1
        dbreal(2,id_level) = y1
        dbreal(3,id_level) = x2
        dbreal(4,id_level) = y2
        CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )
      ELSE
        IF( .NOT.hasError() )THEN
          CALL SetError( UK_ERROR,'Unknown error while getting value', &
                         'Close dialog and try again',' ','RealBox' )
        END IF
        CALL ShowErrorMessage( iwnd_db )
      END IF
    END IF

  CASE DEFAULT

END SELECT

RETURN

!---- Error Section

1000 CONTINUE
irv = MessageBeep( 0 )

GOTO 100
END
!***********************************************************************
!             ProcressReal8
!***********************************************************************
RECURSIVE SUBROUTINE process_real8( iwnd_db,MyCmd )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE errorParam_fd
USE plotdlg_fi
USE GUImatl_fi
USE pltchoice_fi
USE coordinate_fd
USE SwinAPI

!     This routine processes real EDITBOXes from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN),     INTENT( IN ) :: iwnd_db !Dialog Box handle
TYPE( CMD ),              INTENT( IN ) :: MyCmd   !Command Structure

CHARACTER(128) ctemp

INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER ncht,jd_level,indx,irv,i
INTEGER(POINTER_LEN) jwnd_db
REAL(8) x
LOGICAL visible,ldef

LOGICAL, EXTERNAL :: hasError,MoveMouse

CHARACTER(128), EXTERNAL ::AddNull

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Get New Value as a string (Return value is number of characters returned)

ncht = GetDlgItemText( iwnd_db,id_cntrl,ctemp,128 )

!---- Process String

CALL clower( ctemp )
ctemp = ADJUSTL(ctemp(1:ncht))

ldef = .FALSE.

!---- Process value

IF( ncht <= 0 )THEN

  x = NOT_SET_D

ELSE IF( TRIM(ctemp) == 'deferred' )THEN

  x = DEFERRED_D

ELSE IF( TRIM(ctemp) == 'default' )THEN

  x = DEF_VAL_R

ELSE IF( ctemp == 'n/a' )THEN

  RETURN

ELSE

  READ(ctemp(1:ncht),'(F20.0)',ERR=1000,END=1000)x

END IF

!---- Apply limits to value

IF( x /= DEF_VAL_R .AND. x /= NOT_SET_R .AND. x /= DEFERRED_R )THEN
  SELECT CASE( id_dialog )

    CASE( IDB_RELDEF )
      SELECT CASE( id_button )

        CASE( 2 )
          IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
            x = MAX(x,-89.9D0)
            x = MIN(x,89.9D0)
          END IF

        CASE( 3 )
          IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
            x = MAX(x,-360.D0)
            x = MIN(x,360.D0)
          END IF

        CASE DEFAULT

      END SELECT

    CASE DEFAULT

  END SELECT

END IF

!---- Set New Value

dbreal8(id_button,id_level) = x

!---- Post Value back to Dialog Box

100 CONTINUE

CALL SetEditR8s( iwnd_db,dbreal8(id_button,id_level),id_button,1 )

!---- Check for additional actions

SELECT CASE( id_dialog )

  CASE( IDB_RELDEF ) !RELEASE
    SELECT CASE( id_button )

      CASE( 2 ) !  X Deg
        CALL compute_DMS8( dbreal8(2,id_level),dbint(4,id_level), &
                           dbint(5,id_level),dbreal(15,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(15,id_level),15,1 )
        CALL SetEditIs( iwnd_db,dbint ( 4,id_level), 4,2 )

      CASE( 3 ) !  Y Deg
        CALL compute_DMS8( dbreal8(3,id_level),dbint(6,id_level), &
                           dbint(7,id_level),dbreal(16,id_level) )
        CALL SetEditRs( iwnd_db,dbreal(16,id_level),16,1 )
        CALL SetEditIs( iwnd_db,dbint ( 6,id_level), 6,2 )

      CASE DEFAULT

    END SELECT

  CASE DEFAULT

END SELECT

RETURN

!---- Error Section

1000 CONTINUE
irv = MessageBeep( 0 )

GOTO 100
END
