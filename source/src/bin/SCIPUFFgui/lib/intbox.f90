!***********************************************************************
!               ProcessInt
!***********************************************************************
RECURSIVE SUBROUTINE process_int(iwnd_db,MyCmd)
USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE default_fd

!     This routine processes integer EDITBOXes from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN)      iwnd_db !Window handle
TYPE( CMD )               MyCmd !Command Structure

LOGICAL ltrans

CHARACTER(128) ctemp

INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER i,imin,imax,ncht,jd_level,irv
INTEGER(POINTER_LEN)jwnd_db

REAL totMass

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Get New Value as a string (Return value is number of characters returned)

ctemp =' '
ncht  = GetDlgItemText(iwnd_db,id_cntrl,ctemp,LEN(ctemp))

!---- Process String

CALL clower(ctemp) !Lower case
ctemp = ADJUSTL(ctemp(1:ncht)) !Remove leading blanks

!---- Process DEFAULT value

IF( TRIM(ctemp) =='deferred' )THEN
  i = DEFERRED_I
  ltrans = .FALSE.
ELSE IF( TRIM(ctemp) =='default' )THEN
  SELECT CASE (id_dialog)
    CASE (IDB_METDEF)
      i = metdef(DEFAULT_LEVEL)%dbint(id_button)
    CASE (IDB_DOMAIN)
      IF( id_button == 3 )THEN
        i = DEF_VAL_I
      ELSE
        i = NOT_SET_I
      END IF
    CASE (IDB_OPTIONS)
      SELECT CASE (id_button)
        CASE (1)
          i = dlgOptions(DEFAULT_LEVEL)%mGrd
        CASE (2)
          i = dlgOptions(DEFAULT_LEVEL)%nzBL
        CASE (3)
          i = dlgOptions(DEFAULT_LEVEL)%substrate
        CASE DEFAULT
          i = DEF_VAL_I
      END SELECT
    CASE (IDB_TIME)
      SELECT CASE (id_button)
        CASE (1)
          i = dlgTime(DEFAULT_LEVEL)%time%start%time%year
        CASE (2)
          i = dlgTime(DEFAULT_LEVEL)%time%start%time%month
        CASE (3)
          i = dlgTime(DEFAULT_LEVEL)%time%start%time%day
        CASE (4)
          i = dlgTime(DEFAULT_LEVEL)%time%end%time%year
        CASE (5)
          i = dlgTime(DEFAULT_LEVEL)%time%end%time%month
        CASE (6)
          i = dlgTime(DEFAULT_LEVEL)%time%end%time%day
        CASE (7)
          IF( dlgTime(DEFAULT_LEVEL)%time%start%zone == NOT_SET_R )THEN
            i = NOT_SET_I
          ELSE IF( dlgTime(DEFAULT_LEVEL)%time%start%zone == DEF_VAL_R )THEN
            i = DEF_VAL_I
          ELSE
            i = INT(dlgTime(DEFAULT_LEVEL)%time%start%zone)
          END IF
        CASE (8)
          IF( dlgTime(DEFAULT_LEVEL)%time%start%zone == NOT_SET_R )THEN
            i = NOT_SET_I
          ELSE IF( dlgTime(DEFAULT_LEVEL)%time%start%zone == DEF_VAL_R )THEN
            i = DEF_VAL_I
          ELSE
            i = NINT(60.*(dlgTime(DEFAULT_LEVEL)%time%start%zone - &
                         FLOAT(INT(dlgTime(DEFAULT_LEVEL)%time%start%zone))))
          END IF
        CASE DEFAULT
          i = DEF_VAL_I
      END SELECT
    CASE DEFAULT
      i = DEF_VAL_I
  END SELECT
  ltrans = i /= DEF_VAL_I .AND. i /= NOT_SET_I

!---- Process NULL value

ELSE IF( ncht == 0 )THEN
  i = NOT_SET_I
  ltrans = .FALSE.

!---- Process other values

ELSE
  i = GetDlgItemInt(iwnd_db,id_cntrl,ADDRESSOF(irv),TRUE)
  ltrans = irv == TRUE
END IF

!---- SUCCESS - Check for limits

IF( ltrans )THEN

  IF( id_dialog == IDB_TIME )THEN !New Project Start Year
    IF( i >= 100 .OR. (id_button /= 1 .AND. id_button /= 4) )THEN !  Not Year or Century included - Normal check
      CALL GetIntLimit(id_dialog,id_button,imax,imin) !
      i = MAX(imin,i) !
      i = MIN(imax,i) !
    ELSE !  No Century - Keep positive
      i = MAX(1,i) !
    END IF !
  ELSE IF( id_dialog == IDB_SCIPUF )THEN !Run End Year
    IF( i >= 100 .OR. id_button /= 4 )THEN !  Not Year or Century included - Normal check
      CALL GetIntLimit(id_dialog,id_button,imax,imin) !
      i = MAX(imin,i) !
      i = MIN(imax,i) !
    ELSE !  No Century - Keep positive
      i = MAX(1,i) !
    END IF !
  ELSE !All other parameters
    CALL GetIntLimit(id_dialog,id_button,imax,imin) !  Normal Check
    i = MAX(imin,i) !
    i = MIN(imax,i)
  END IF

!------ Set New Value

  dbint(id_button,id_level) = i

!---- FAILURE - Beep

ELSE
  IF( i /= DEF_VAL_I .AND. i /= NOT_SET_I &
                                .AND. i /= DEFERRED_I )THEN
    irv = MessageBeep(0)
  ELSE
    dbint(id_button,id_level) = i
  END IF
END IF

!---- Post Value back to Dialog Baox

CALL SetEditIs(iwnd_db,dbint(id_button,id_level),id_button,1)

SELECT CASE (id_dialog)
  CASE (IDB_PLTOPT)
    IF( id_button == 1 )THEN
      CALL FindHwndListId(IDB_PLOT,jwnd_db,jd_level) !  Parent = PLOT
      lcheck( 9,jd_level) = .TRUE. !lslice
    END IF
  CASE (IDB_SLICE) !Slice resolution
    IF( id_button == 2 )THEN
      CALL SetScrollControlPos(iwnd_db,IDB_SCROLL1,dbint(2,id_level),i)
    END IF
  CASE (IDB_COMLST) !Material Compute Size bins
    CALL update_compute_buttons(iwnd_db)
  CASE (IDB_SCIPUF) !RUN
    IF( id_button >= 4 .AND. id_button <= 6 )THEN !  Stop Year,Month,Day
      CALL UpdateDuration(iwnd_db)
    END IF
  CASE (IDB_TIME) !TIME
    SELECT CASE (id_button)
      CASE (1,2,3) !  Start Year,Month,Day
        CALL UpdateDuration(iwnd_db)
      CASE (4,5,6) !  Stop Year,Month,Day
        CALL UpdateDuration(iwnd_db)
      CASE (7,8) !  Time Zone
        CALL FormatEditIs(iwnd_db,'(i2.2)', &
           dbint(id_button,id_level),id_button,1)
      CASE DEFAULT
    END SELECT
  CASE (IDB_DOMAIN,IDB_SETUP) !DOMAIN
    IF( id_button >= 7 .AND. id_button <= 12 )THEN !  X Hr/Min
      i = id_button - 6
      CALL compute_degrees(dbint(i+6,id_level),dbint(i+12,id_level), &
                     dbreal(i+12,id_level),dbreal(i,id_level))
      CALL SetEditRs(iwnd_db,dbreal(i,id_level),i,1)
    ELSE IF( id_button >= 13 .AND. id_button <= 18 )THEN !  X Hr/Min
      i = id_button - 12
      CALL compute_degrees(dbint(i+6,id_level),dbint(i+12,id_level), &
                     dbreal(i+12,id_level),dbreal(i,id_level))
      CALL SetEditRs(iwnd_db,dbreal(i,id_level),i,1)
    END IF
  CASE (IDB_RELDEF) !RELEASE
    IF( id_button == 4 .OR. id_button == 5 )THEN !  X Hr/Min
      CALL compute_degrees(dbint(4,id_level),dbint(5,id_level), &
                     dbreal(15,id_level),dbreal(2,id_level))
      CALL SetEditRs(iwnd_db,dbreal(2,id_level),2,1)
    ELSE IF( id_button == 6 .OR. id_button == 7 )THEN !  Y Hr/Min
      CALL compute_degrees(dbint(6,id_level),dbint(7,id_level), &
                     dbreal(16,id_level),dbreal(3,id_level))
      CALL SetEditRs(iwnd_db,dbreal(3,id_level),3,1)
    END IF
  CASE (IDB_RNDPARM) !RANDOM
    IF( id_button == 1 )THEN
      CALL FindHwndListId(IDB_RELDEF,jwnd_db,jd_level) !  Parent = RELDEF
      IF( dbreal(5,jd_level) /= NOT_SET_R .AND. &
   dbreal(5,jd_level) /= DEF_VAL_R .AND. &
   dbreal(5,jd_level) /= DEFERRED_R )THEN
        IF( dbint(1,id_level) /= NOT_SET_I .AND. &
     dbint(1,id_level) /= DEF_VAL_I .AND. &
     dbint(1,id_level) /= DEFERRED_I )THEN

          totMass = FLOAT(dbint(1,id_level))*dbreal(5,jd_level)

          WRITE(string1,*)totMass
          CALL SetControlText(iwnd_db,IDB_STATIC22,TRIM(ADJUSTL(string1)))

        ELSE
          totMass = 0.0
        END IF
      ELSE
        totMass = 0.0
      END IF
      IF( totMass > 0.0 )THEN
        CALL ShowControl(iwnd_db,IDB_STATIC21,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC22,SW_SHOWNORMAL)
      ELSE
        CALL ShowControl(iwnd_db,IDB_STATIC21,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC22,SW_HIDE)
      END IF
    END IF
  CASE DEFAULT
END SELECT

RETURN
END
!*******************************************************************************
!                GetIntLimit
!*******************************************************************************
SUBROUTINE GetIntLimit(id,ib,imax,imin)

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE plotdlg_fi
USE default_fd

!     Sets the acceptable limits for integers in Dialog Boxes

IMPLICIT NONE

INTEGER*4  MAXLST
PARAMETER (MAXLST=101)

INTEGER id !Dialog ID
INTEGER ib !Button ID
INTEGER imax !Maximum Value
INTEGER imin !Minimum Value

!---- Initialize

imax = DEF_VAL_I
imin = NOT_SET_I

!---- Select Action based on Dialog ID

SELECT CASE (id)
  CASE (IDB_AXES) !PLOT AXES
    IF( ib == 1 .OR. ib == 2 )THEN !  Ticks
      imax = 15
      imin = -15
    END IF
  CASE (IDB_COMLST) !COMPUTE LIST
    IF( ib == 1 )THEN !  NL
      imax = MAXLST-1
      imin = 2
    END IF
  CASE (IDB_CONTOUR) !PLOT CONTOUR
    IF( ib == 1 )THEN !  NC
      imax = MAXCNTG
      imin = 1
    END IF
  CASE (IDB_DOMAIN,IDB_SETUP) !DOMAIN/SETUP
    IF( ib == 3 )THEN !  UTM Zone
      imax = 60
      imin = 1
    ELSE IF( ib == 7 )THEN
      imax = 359
      imin = -359
    ELSE IF( ib == 8 )THEN
      imax = 89
      imin = -89
    ELSE IF( ib >= 9 .AND. ib <= 10 )THEN
      imax = 89
      imin = -89
    ELSE IF( ib >= 11 .AND. ib <= 12 )THEN
      imax = 359
      imin = -359
    ELSE IF( ib >= 13 .AND. ib <= 18 )THEN
      imax = 59
      imin = 0
    END IF
  CASE (IDB_TIME) !TIME
    IF( ib == 1 )THEN !  Year
      imax = 2100
      imin = 1800
    ELSE IF( ib == 2 )THEN !  Month
      imax = 12
      imin = 1
    ELSE IF( ib == 3 )THEN !  Day
      imax = 31
      imin = 1
    ELSE IF( ib == 4 )THEN !  Year
      imax = 2100
      imin = 1800
    ELSE IF( ib == 5 )THEN !  Month
      imax = 12
      imin = 1
    ELSE IF( ib == 6 )THEN !  Day
      imax = 31
      imin = 1
    ELSE IF( ib == 7 )THEN !  Time zone hour
      imax = 23
      imin = 0
    ELSE IF( ib == 8 )THEN !  Time zone minute
      imax = 59
      imin = 0
    END IF
  CASE (IDB_SCIPUF) !SCIPUFF
    IF( ib == 4 )THEN !  Year
      imax = 2100
      imin = 1800
    ELSE IF( ib == 5 )THEN !  Month
      imax = 12
      imin = 1
    ELSE IF( ib == 6 )THEN !  Day
      imax = 31
      imin = 1
    ELSE IF( ib == 7 )THEN !  Run Animation update
      imax = 9999
      imin = 1
    END IF
  CASE (IDB_PLTOPT) !PLOT OPTIONS
    IF( ib == 1 )THEN !  Maxlev
      imax = 99
      imin = -99
    END IF
  CASE (IDB_SLICE) !PLOT SLICE
    IF( ib == 1 )THEN !  Horiz Res
      imax = 10
      imin = 0
    ELSE IF( ib == 2 )THEN !  Vertical Res
      imax = 100
      imin = 0
    END IF
  CASE (IDB_OPTIONS) !EDTPRJ Options
    IF( ib == 2 )THEN !  nzbl
      imax = 51
      imin = 1
    ELSE IF( ib == 1 )THEN !  mgrd
      imax = 9
      imin = -imax
    END IF
  CASE (IDB_METDEF) !EDTPRJ MET Define
    IF( ib == 1 )THEN !  min_pro
      imax = 99
      imin = 1
    ELSE IF( ib == 2 )THEN !  min_sur
      imax = 99
      imin = 1
    END IF
  CASE (IDB_RELDEF) !Release Define
    IF( ib == 6 )THEN !  E/W Deg
      imax = 360
      imin = -360
    ELSE IF( ib == 7 )THEN !  E/W Min
      imax = 59
      imin = 0
    ELSE IF( ib == 4 )THEN !  N/S Deg
      imax = 89
      imin = -89
    ELSE IF( ib == 5 )THEN !  N/S Min
      imax = 59
      imin = 0
    END IF
  CASE (IDB_MATDEF) !EDTPRJ MAT Define Subgroup compute
    IF( ib == 1 )THEN !  No. bins
      imax = 51
      imin = 1
    END IF
  CASE (IDB_TERPARM) !Terrain/Mass-consistent iteration
    IF( ib == 1 )THEN !  FFT
      imin = 0
    ELSE IF( ib == 2 )THEN !  Point Relaxation
      imin = 0
    END IF
  CASE (IDB_RNDPARM) ! Random release
    IF( ib == 1 )THEN !  No. release
      imin = 1
    ELSE IF( ib == 2 )THEN !  Seed
      imax = 9999999
    END IF
  CASE DEFAULT !REST
END SELECT

RETURN

END
