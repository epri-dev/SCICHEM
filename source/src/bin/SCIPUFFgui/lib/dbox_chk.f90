!***********************************************************************
!                check_dialog
!***********************************************************************
SUBROUTINE check_dialog( iwnd_db,id,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE animate_fi
USE files_fi
!
!     This routine checks Dialog Boxes
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN  ) :: id       !Dialog Box id
INTEGER,              INTENT( IN  ) :: id_level !Dialog data level
LOGICAL,              INTENT( OUT ) :: lok      !return value

INTEGER irv

LOGICAL, EXTERNAL :: verify_button

lok       = .FALSE.
lokbutton = .FALSE.

SELECT CASE( id )
  CASE( IDB_PLOT )
    lok = .TRUE.

  CASE( IDB_AXES )
    CALL check_axes_definition( iwnd_db,id_level,lok )

  CASE( IDB_MAPS )
    CALL check_map_definition( iwnd_db,id_level,lok )

  CASE( IDB_ZOOM )
    CALL check_zoom_definition( iwnd_db,id_level,lok )

  CASE( IDB_SLICE )
    CALL check_slice_definition( iwnd_db,id_level,lok )

  CASE( IDB_COMLST )
    CALL check_matcmp_definition( iwnd_db,id_level,lok )

  CASE( IDB_CONTOUR )
    CALL check_contour_definition( iwnd_db,id_level,lok )

  CASE( IDB_SCIPUF )
    CALL check_run_definition( iwnd_db,id_level,lok )

  CASE( IDB_PRJDEF )
    CALL check_project_definition( iwnd_db,id_level,lok )

  CASE( IDB_TIME )
    CALL check_time_definition( iwnd_db,id_level,lok )

  CASE( IDB_DOMAIN )
    CALL check_domain_definition( iwnd_db,id_level,lok )

  CASE (IDB_SETUP)
    CALL check_setup_definition( iwnd_db,id_level,lok )

  CASE( IDB_OPTIONS )
    CALL check_options_definition( iwnd_db,id_level,lok )

  CASE( IDB_DELPRJ )
    IF( AnimDel )THEN
      string1 = 'Delete animation '//TRIM(file_animate)
    ELSE
      string1 = 'Delete project '//TRIM(project(1)%ID%name)// &
          ' and all associated files'
    END IF
    lok = verify_button( iwnd_db,TRIM(string1) )
    CALL PushButton( iwnd_db,0,ID_CANCEL,irv )

  CASE DEFAULT
    lok = .TRUE.

END SELECT

RETURN
END
!***********************************************************************
!               CheckComputeListDefinition
!***********************************************************************
SUBROUTINE check_matcmp_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER i,irv
INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

string2 = ' '
i    =  1
irv  =  0

nError = NO_ERROR

DO WHILE( i <= 1.AND. nError == NO_ERROR )
  IF( dbint(i,id_level) == NOT_SET_I )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        string1 = 'the number of values'
      CASE DEFAULT
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

lok = nError == NO_ERROR
IF( .NOT.lok )GOTO 9999

i   = 1
irv = 0
DO WHILE( i <= 2.AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        string1 = 'the minimum value'
      CASE( 2 )
        string1 = 'the maximum value'
      CASE DEFAULT
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

lok = nError == NO_ERROR
IF( .NOT.lok )GOTO 9999

IF( dbreal(2,id_level) <= dbreal(1,id_level) )THEN
  nError = IV_ERROR
  string1 = 'Maximum must be greater than minimum'
END IF
lok = nError == NO_ERROR
IF( .NOT.lok )GOTO 9998

IF( ichoice(1,id_level) == 2 )THEN !  Log spacing
  IF( dbreal(1,id_level) <= 0.0 .OR. dbreal(2,id_level) <= 0.0 )THEN
    nError = IV_ERROR
    string1 = 'Min/Max must be positive for log spacing'
  END IF
  lok = nError == NO_ERROR
  IF( .NOT.lok )GOTO 9998
END IF

1000  CONTINUE

IF( .NOT.lok )THEN
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN

9999 CONTINUE

IF( .NOT.lok )THEN
  eRoutine = 'CheckCompute'
  IF( irv == 0 )THEN
    eAction  = ' '
  ELSE IF( irv < 0 )THEN
    eAction  = 'Default="default"'
  ELSE
    eAction  = 'Default='//TRIM(string2)
  END IF
  eMessage = 'Please set '//TRIM(string1)
END IF
GOTO 1000

9998 CONTINUE

IF( .NOT.lok )THEN
  eRoutine = 'CheckCompute'
  eMessage = TRIM(string1)
  eInform  = TRIM(string2)
  eAction  = ' '
END IF
GOTO 1000

END
!***********************************************************************
!               CheckAxesDefinition
!***********************************************************************
SUBROUTINE check_axes_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE param_fd

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

LOGICAL lref
INTEGER i,irv
INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

!---- Check that everything is set

nError = NO_ERROR

i   = 3
irv = -1
DO WHILE( i <= 8.AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 3 )
        string1 = 'the minimum X value'
      CASE( 4 )
        string1 = 'the maximum X value'
      CASE( 5 )
        string1 = 'the X scale factor'
        irv = 1
        string2 = '1.0'
      CASE( 6 )
        string1 = 'the minimum Y value'
      CASE( 7 )
        string1 = 'the maximum Y value'
      CASE( 8 )
        string1 = 'the Y scale factor'
        irv = 1
        string2 = '1.0'
      CASE DEFAULT
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

lok = nError == NO_ERROR
IF( .NOT.lok )GOTO 9999

!---- Check the reference point

lref = MIN(dbreal(1,id_level),dbreal(2,id_level), &
           dbreal(9,id_level),dbreal(10,id_level)) == NOT_SET_R

IF( (project(BASE_LEVEL)%MapCoord == I_LATLON) .AND. &
                (ichoice(1,id_level) /= I_LATLON .AND. lref) )THEN
  nError = IV_ERROR
  string1 = 'the reference point'
  string2 = '(0.0,0.0)=(default,default)'
  irv = 1
  lok = .FALSE.
  GOTO 9999
END IF
IF( .NOT.(project(BASE_LEVEL)%MapCoord==I_LATLON) .AND. (ichoice(1,id_level)==I_LATLON) )THEN
  IF( lref )THEN
    nError = IV_ERROR
    string1 = 'the reference point'
    irv = 0
    lok = .FALSE.
    GOTO 9999
  END IF
  lref = MAX(dbreal(1,id_level),dbreal(2,id_level), &
             dbreal(9,id_level),dbreal(10,id_level)) == DEF_VAL_R
  IF( lref )THEN
    nError = IV_ERROR
    string1 = 'the reference point : "Default" has no meaning'
    irv = 0
    lok = .FALSE.
    GOTO 9999
  END IF
END IF

!---- Check the Scale factors

IF( dbreal(5,id_level) <= 0.0 )THEN
  nError = IV_ERROR
  string1 = 'the X scale factor'
  eInform = 'X scale must be greater than 0'
  irv = 0
  lok = .FALSE.
  GOTO 9999
END IF

IF( dbreal(8,id_level) <= 0.0 )THEN
  nError = IV_ERROR
  string1 = 'the Y scale factor'
  eInform = 'Y scale must be greater than 0'
  irv = 0
  lok = .FALSE.
  GOTO 9999
END IF

!---- Check the Axes

IF( dbreal(4,id_level) <= dbreal(3,id_level) .AND. dbreal(3,id_level) /= DEF_VAL_R )THEN
  nError = IV_ERROR
  string1 = 'the X axes'
  eInform = 'Xmax must be greater than Xmin'
  irv = 0
  lok = .FALSE.
  GOTO 9999
END IF

IF( dbreal(7,id_level) <= dbreal(6,id_level) .AND. dbreal(6,id_level) /= DEF_VAL_R )THEN
  nError = IV_ERROR
  string1 = 'the Y axes'
  eInform = 'Ymax must be greater than Ymin'
  irv = 0
  lok = .FALSE.
  GOTO 9999
END IF

9999 CONTINUE

IF( .NOT.lok )THEN
  eRoutine = 'CheckAxes'
  IF( irv == 0 )THEN
    eAction  = ' '
  ELSE IF( irv < 0 )THEN
    eAction  = 'Default="default"'
  ELSE
    eAction  = 'Default='//TRIM(string2)
  END IF
  eMessage = 'Please set '//TRIM(string1)
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
!***********************************************************************
!               CheckContourDefinition
!***********************************************************************
SUBROUTINE check_contour_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE plotdlg_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER i,irv
INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

nError = NO_ERROR
IF( ichoice(1,id_level) /= 3 )THEN !not SCIP contours
  string2 = ' '
  IF( ichoice(1,id_level) == 2 )THEN !  Nonuniform
    IF( nlst(id_level) <= 0 )THEN
      nError  = SZ_ERROR
      string1 = 'No contour levels specified'
    ELSE IF( nlst(id_level) > MAXCNTG )THEN
      nError = SZ_ERROR
      string1 = 'Too many contour levels specified'
      WRITE(string2,*)'Maximum value = ',MAXCNTG
    END IF
    lok = nError == NO_ERROR
    IF( .NOT.lok )GOTO 9998
  ELSE IF( ichoice(1,id_level) == 1 )THEN !  Uniform
    i   =  1
    irv = -1
    DO WHILE( i <= 3.AND. nError == NO_ERROR)
      IF( dbreal(i,id_level) == NOT_SET_R )THEN
        nError = IV_ERROR
        SELECT CASE( i )
          CASE( 1 )
            string1 = 'the minimum contour value'
          CASE( 2 )
            string1 = 'the maximum contour value'
          CASE( 3 )
            string1 = 'the contour increment'
          CASE DEFAULT
            string1 = 'all contour parameters'
        END SELECT
      END IF
      i = i + 1
    END DO
    lok = nError == NO_ERROR
    IF( .NOT.lok )GOTO 9999

    IF( dbint(1,id_level) == NOT_SET_I )THEN
      nError = IV_ERROR
      string1 = 'No contour levels specified'
      string2 = '6'
      irv = 1
    END IF
    lok = nError == NO_ERROR
    IF( .NOT.lok )GOTO 9999

    IF( dbreal(3,id_level) <= 0.0 )THEN
      nError = IV_ERROR
      string1 = 'Contour increment must be positive'
    END IF
    lok = nError == NO_ERROR
    IF( .NOT.lok )GOTO 9998

    IF( dbreal(1,id_level) /= DEF_VAL_R .AND. dbreal(2,id_level) /= DEF_VAL_R )THEN
      IF( dbreal(2,id_level) <= dbreal(1,id_level) )THEN
        nError = IV_ERROR
        string1 = 'Contour maximum must be greater than minimum'
      END IF
    END IF
    lok = nError == NO_ERROR
    IF( .NOT.lok )GOTO 9998

    IF( ichoice(2,id_level) == 1 )THEN !  Log spacing
      IF( dbreal(1,id_level) <= 0.0 .OR. dbreal(2,id_level) <= 0.0 )THEN
        nError = IV_ERROR
        string1 = 'Contour Min/Max must be positive for log spacing'
      END IF
      lok = nError == NO_ERROR
      IF( .NOT.lok )GOTO 9998
    ELSE
!      IF( dbreal(1,id_level) < 0.0 .OR. dbreal(2,id_level) < 0.0 )THEN
!        nError = IV_ERROR
!        string1 = 'Contour Min/Max must be greater than or equal to 0.'
!      END IF
      lok = nError == NO_ERROR
      IF( .NOT.lok )GOTO 9998
    END IF
  END IF
ELSE
  lok = .TRUE.
END IF

1000  CONTINUE

IF( .NOT.lok )THEN
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN

9999 CONTINUE

IF( .NOT.lok )THEN
  eRoutine = 'CheckContour'
  IF( irv == 0 )THEN
    eAction  = ' '
  ELSE IF( irv < 0 )THEN
    eAction  = 'Default="default"'
  ELSE
    eAction  = 'Default='//TRIM(string2)
  END IF
  eMessage = 'Please set '//TRIM(string1)
END IF
GOTO 1000

9998 CONTINUE

IF( .NOT.lok )THEN
  eRoutine = 'CheckContour'
  eMessage = TRIM(string1)
  eInform  = TRIM(string2)
  eAction  = ' '
END IF
GOTO 1000

END
!***********************************************************************
!               CheckRunDefinition
!***********************************************************************
SUBROUTINE check_run_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE pltchoice_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

TYPE( timeT ) end,start

INTEGER i,irv
INTEGER nError
LOGICAL end_ymd
REAL    tout,tstep

CHARACTER(128) eMessage,eInform,eAction,eRoutine

nError = NO_ERROR

i  = 1
DO WHILE( i <= 4 .AND. nError == NO_ERROR )
  IF( dbreal(i,id_level) == NOT_SET_R )THEN
    nError = IV_ERROR
    SELECT CASE( i )
      CASE( 1 )
        string1 = 'the maximum Time step'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%step%max,string2,irv )
      CASE( 2 )
        string1 = 'the Output interval'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%step%output,string2,irv )
      CASE( 3 )
        string1 = 'the End time'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%time%hour,string2,irv )
      CASE( 4 )
        string1 = 'the Duration'
        CALL set_real_string( dlgTime(DEFAULT_LEVEL)%time%end%time%runTime,string2,irv )
      CASE DEFAULT
        string1 = 'all parameters'
    END SELECT
  END IF
  i = i + 1
END DO

end_ymd = .FALSE.
DO i = 4,5
  end_ymd = end_ymd .OR. dbint(i,id_level) /= NOT_SET_I
END DO

IF( end_ymd )THEN
  i  = 4
  DO WHILE( i <= 6 .AND. nError == NO_ERROR )
    IF( dbint(i,id_level) == NOT_SET_I )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 4 )
          CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%year,string2,irv )
          string1 = 'the End year'
        CASE( 5 )
          CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%month,string2,irv )
          string1 = 'the End month'
        CASE( 6 )
          CALL set_int_string( dlgTime(DEFAULT_LEVEL)%time%end%time%day,string2,irv )
          string1 = 'the End day'
        CASE DEFAULT
          string1 = 'all END TIME parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eMessage = 'Please set '//TRIM(string1)
  GOTO 9999
END IF

end%year  = dbint(4,id_level)
end%month = dbint(5,id_level)
end%day   = dbint(6,id_level)
end%hour  = dbreal(3,id_level)

IF( nTimePuff > 0 )THEN
  start   = timePuff(nTimePuff)%time
  string1 = 'current'
ELSE
  start   = dlgTime(BASE_LEVEL)%time%start%time
  string1 = 'start'
END IF

CALL ComputeDurationGUI( start,end,lok )

IF( .NOT.lok )THEN
  nError = IV_ERROR
  irv = 0
  eMessage = 'End time must be greater than '//TRIM(string1)//' time'
  GOTO 9999
END IF

tstep = dbreal(1,id_level)
SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    tstep = tstep*60.
  CASE( 3 )
    tstep = tstep*3600.
  CASE DEFAULT
END SELECT

tout = dbreal(2,id_level)
SELECT CASE( idbcmbo(3,id_level) )
  CASE( 2 )
    tout = tout*60.
  CASE( 3 )
    tout = tout*3600.
  CASE DEFAULT
END SELECT

lok = tout >= tstep
IF( .NOT.lok )THEN
  nError = IV_ERROR
  irv = 0
  eMessage = 'Output interval must not be less than the maximum time step'
  GOTO 9999
END IF

RETURN

9999 CONTINUE

eRoutine = 'CheckTime'
IF( irv == 0 )THEN
  eAction  = ' '
ELSE IF( irv < 0 )THEN
  eAction  = 'Default="default"'
ELSE
  eAction  = 'Default='//TRIM(string2)
END IF
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )

RETURN
END
!***********************************************************************
!               CheckZoomDefinition
!***********************************************************************
SUBROUTINE check_zoom_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) ::lok

lok = (dbreal(1,id_level) /= dbreal(3,id_level) .OR. dbreal(1,id_level) == DEF_VAL_R) .AND. &
      (dbreal(2,id_level) /= dbreal(4,id_level) .OR. dbreal(2,id_level) == DEF_VAL_R)

IF( .NOT.lok )THEN
  CALL SetError( IV_ERROR, &
                 'Invalid Zoom region', &
                 'Cannot Zoom to a line or point.', &
                 'Select another Zoom region', &
                 'CheckZoom' )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
!***********************************************************************
!               CheckSliceDefinition
!***********************************************************************
SUBROUTINE check_slice_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

lok = ( dbreal(1,id_level) /= dbreal(3,id_level) ) .OR. &
      ( dbreal(2,id_level) /= dbreal(4,id_level) ) .OR. &
      ( dbreal(1,id_level) == DEF_VAL_R ) .OR. ( dbreal(2,id_level) == DEF_VAL_R )

IF( .NOT.lok )THEN
  CALL SetError( IV_ERROR, &
                 'Invalid Slice', &
                 'Cannot Slice to a point.', &
                 'Select another Slice', &
                 'CheckSlice' )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
!***********************************************************************
!               CheckSetupDefinition
!***********************************************************************
SUBROUTINE check_setup_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE param_fd

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

LOGICAL, EXTERNAL :: CheckFile

INTEGER i,irv
INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

!---- Check to see that all reference point parameters are set

nError = NO_ERROR

IF( (ichoice(1,id_level) == I_CARTESIAN) .AND. lcheck(1,id_level) )THEN
  i = 1
  DO WHILE( i <= 2 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 1 )
          CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%reference%lon,string2,irv )
          string1 = 'the Reference Point Longitude'
        CASE( 2 )
          CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%reference%lat,string2,irv )
          string1 = 'the Reference Point Latitude'
        CASE DEFAULT
          string2 = ' '
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
  i = 10
  DO WHILE( i <= 11 .AND. nError == NO_ERROR )
    IF( dbreal(i,id_level) == NOT_SET_R )THEN
      nError = IV_ERROR
      SELECT CASE( i )
        CASE( 10 )
          CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%reference%x,string2,irv )
          string1 = 'the Reference Point X value'
        CASE( 11 )
          CALL set_real_string( dlgDomain(DEFAULT_LEVEL)%spatial%reference%y,string2,irv )
          string1 = 'the Reference Point Y value'
        CASE DEFAULT
          string2 = ' '
          string1 = 'all parameters'
      END SELECT
    END IF
    i = i + 1
  END DO
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eRoutine = 'CheckSetup'
  IF( irv == 0 )THEN
    eMessage = 'Invalid '//TRIM(string1)
    eInform  = 'Current='//TRIM(string2)
    eAction  = ' '
  ELSE IF( irv < 0 )THEN
    eMessage = 'Please set '//TRIM(string1)
    eAction  = 'Default="default"'
  ELSE
    eMessage = 'Please set '//TRIM(string1)
    eAction  = 'Default='//TRIM(string2)
  END IF
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!               CheckMapDefinition
!***********************************************************************
SUBROUTINE check_map_definition( iwnd_db,id_level,lok )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE param_fd

!     This routine checks a dialog

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db
INTEGER,              INTENT( IN  ) :: id_level
LOGICAL,              INTENT( OUT ) :: lok

INTEGER nError

CHARACTER(128) eMessage,eInform,eAction,eRoutine

!---- Check Population density

nError = NO_ERROR

IF( lcheck(10,id_level) )THEN
  IF( ichoice(3,id_level) == 1 )THEN
    IF( .NOT.PopData )THEN
      nError = IV_ERROR
      eMessage = 'Population database is not available'
      eAction  = 'Check your SCIP initialization file'
    END IF
  ELSE IF( ichoice(3,id_level) == 2 )THEN
    IF( dbreal(1,id_level) <= 0.0 )THEN
      nError = IV_ERROR
      eMessage = 'Population density must be > 0.0'
      eAction  = 'Default = 1.0'
    END IF
  END IF
END IF

lok = nError == NO_ERROR

IF( .NOT.lok )THEN
  eRoutine = 'CheckPopDensity'
  eInform  = ' '
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
