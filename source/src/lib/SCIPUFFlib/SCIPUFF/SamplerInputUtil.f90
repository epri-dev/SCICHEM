MODULE ParseLine_fi

  SAVE

  INTEGER, PARAMETER :: MAXN_ARG = 20

  INTEGER n_arg, nch, iarg
  LOGICAL lerr

  CHARACTER(128)  kwrd
  CHARACTER(1024) string

  CHARACTER(500), DIMENSION(MAXN_ARG) :: c_arg

END MODULE ParseLine_fi

!==============================================================================

SUBROUTINE SetSampCoord()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER ios
REAL    xmap, ymap

WRITE(lun_log,FMT='(A)',IOSTAT=ios) 'Sensor map = '//TRIM(c_arg(iarg))

SELECT CASE( TRIM(c_arg(iarg)) )
  CASE( 'LATLON','LL' )
    sampMap = I_LATLON

  CASE( 'UTM','UTM-M' )
    sampMap = I_UTM

    IF( TRIM(c_arg(iarg)) == 'UTM-M' )THEN
      UTMfac = 1.E-3
    ELSE
      UTMfac = 1.
    END IF

    iarg = iarg + 1
    IF( iarg > n_arg )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampCoord'
      eMessage = 'Missing UTM zone in sampler file'
      GOTO  9999
    END IF
    READ(c_arg(iarg),*,IOSTAT=ios) sampZone
    IF( ios /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampCoord'
      eMessage = 'Error reading UTM zone'
      eInform  = 'Require zone in sampler input file for UTM coordinates'
      GOTO  9999
    END IF

  CASE( 'CART','KM','CARTESIAN','KILOMETERS' )
    sampMap = I_CARTESIAN

  CASE( 'METERS' )
    sampMap = I_METERS

    IF( n_arg > iarg )THEN
      IF( TRIM(c_arg(iarg+1)) == 'ORIGIN' )THEN
        iarg = iarg + 1
        IF( iarg+2 > n_arg )THEN
          nError   = IV_ERROR
          eRoutine = 'SetSampCoord'
          eMessage = 'Missing ORIGIN input in sampler file'
          GOTO  9999
        END IF
        READ(c_arg(iarg+1),*,IOSTAT=ios) CartLat0
        IF( ios == 0 ) &
        READ(c_arg(iarg+2),*,IOSTAT=ios) CartLon0
        IF( ios /= 0 )THEN
          nError   = IV_ERROR
          eRoutine = 'SetSampCoord'
          eMessage = 'Error reading ORIGIN latitude & longitude'
          eInform  = 'Required in sampler input file for CARTESIAN coordinates w/ ORIGIN'
          GOTO  9999
        END IF
        iarg = iarg + 2
        lCartOrig = .TRUE.
      END IF
    END IF

END SELECT

IF( lmap /= sampMap )THEN

  IF( .NOT.((lmap == I_UTM       .AND. sampMap == I_LATLON   )    .OR. &
            (lmap == I_UTM       .AND. sampMap == I_CARTESIAN)    .OR. &
            (lmap == I_LATLON    .AND. sampMap == I_UTM      )    .OR. &
            (lmap == I_METERS    .AND. sampMap == I_METERS   )    .OR. &
            (lmap == I_CARTESIAN .AND. sampMap == I_METERS   )    .OR. &
            (lmap == I_CARTESIAN .AND. sampMap == I_UTM      )    .OR. &
            (lmap == I_CARTESIAN .AND. sampMap == I_LATLON   )) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampCoord'
    eMessage = 'Invalid coordinates'
    eInform  = 'Cannot convert sampler to project coordinates'
    GOTO  9999
  END IF

  IF( lmap == I_UTM .AND. sampMap == I_UTM )sampZone = utm_zone

  IF( sampMap == I_CARTESIAN )THEN
    IF( lmap == I_UTM .OR. lmap == I_LATLON )THEN
      IF( .NOT.lCartOrig )THEN
        nError   = IV_ERROR
        eRoutine = 'SetSampCoord'
        eMessage = 'No ORIGIN specified for CART samplers'
        eMessage = 'ORIGIN required for lat/lon or UTM projects'
        GOTO  9999
      END IF
    END IF
  END IF

  lSmpCnv = .TRUE.

ELSE IF( lmap == I_UTM )THEN

  IF( sampZone /= utm_zone )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampCoord'
    eMessage = 'UTM zone mismatch'
    WRITE(eInform,'(A,I4)') 'Project zone = ',utm_zone
    WRITE(eAction,'(A,I4)') 'Sensor  zone = ',sampZone
    GOTO  9999
  END IF

ELSE IF( lmap == I_CARTESIAN )THEN

  IF( lCartOrig )THEN
    IF( lon0 == NOT_SET_R .OR. lat0 == NOT_SET_R .OR. &
        lon0 == DEF_VAL_R .OR. lat0 == DEF_VAL_R )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampCoord'
      eMessage = 'Reference location not set for Cartesian project'
      eMessage = 'Must be set for CART samplers with ORIGIN'
      GOTO 9999
    END IF
    CALL map_loc( I_LATLON,cartLon0,cartLat0,I_CARTESIAN,cartX0,cartY0,xmap,ymap )
    lSmpCnv = .TRUE.
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampTavg()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER ios
LOGICAL lValidUnits

REAL, EXTERNAL :: ParseTimeUnits

iarg = iarg + 1
IF( iarg > n_arg )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampTavg'
  eMessage = 'Missing averaging time in sampler file'
  GOTO  9999
END IF

lAvg = .TRUE.

IF( TRIM(c_arg(iarg)) == 'LIST' )THEN
  lSmpOutList = .TRUE.
  GOTO 9999
END IF

READ(c_arg(iarg),*,IOSTAT=ios) dtSmpOut
IF( ios /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampTavg'
  eMessage = 'Error reading averaging time'
  eInform  = 'Required in sampler input file when specifying time averaging'
  GOTO  9999
END IF

!------ Look for optional averaging period units

IF( iarg+1 > n_arg )GOTO 9999

dtSmpOut = dtSmpOut * ParseTimeUnits( c_arg(iarg+1),lValidUnits )
IF( lValidUnits )iarg = iarg + 1

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampStart()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

LOGICAL lValidUnits, lRelTime

REAL, EXTERNAL :: ParseSensorTime, ParseTimeUnits

IF( lSmpOutList )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampStart'
  eMessage = 'Start time must be given in following list'
  GOTO 9999
END IF

iarg = iarg + 1
IF( iarg > n_arg )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampStart'
  eMessage = 'Missing start time in sampler file'
  eInform  = 'Required in sampler input file when specifying Start keyword'
  GOTO 9999
END IF

tStartSamp = ParseSensorTime( c_arg(iarg),lRelTime )
IF( tStartSamp == NOT_SET_R )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampStart'
  eMessage = 'Error reading sampler start time'
  eInform  = 'Format must be HH:MM:SS.SS or YYYYMMDD:HH:MM:SS.SS'
  GOTO 9999
END IF

!------ Look for optional (relative) time units

IF( lRelTime )THEN
  IF( iarg+1 > n_arg )GOTO 9999
  tStartSamp = tStartSamp * ParseTimeUnits( c_arg(iarg+1),lValidUnits )
  IF( lValidUnits )iarg = iarg + 1
END IF
IF( BTEST(run_mode,REVERSE_MODE) .AND. .NOT. lRelTime )tStartSamp = -tStartSamp

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampEnd()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

LOGICAL lValidUnits, lRelTime

REAL, EXTERNAL :: ParseSensorTime, ParseTimeUnits

IF( lSmpOutList )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampEnd'
  eMessage = 'End time must be given in following list'
  GOTO 9999
END IF

iarg = iarg + 1
IF( iarg > n_arg )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampEnd'
  eMessage = 'Missing end time in sampler file'
  eInform  = 'Required in sampler input file when specifying END keyword'
  GOTO 9999
END IF

tEndSamp = ParseSensorTime( c_arg(iarg),lRelTime )
IF( tEndSamp == NOT_SET_R )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampEnd'
  eMessage = 'Error reading sampler end time'
  eInform  = 'Format must be HH:MM:SS.SS or YYYYMMDD:HH:MM:SS.SS'
  GOTO 9999
END IF

!------ Look for optional (relative) time units

IF( lRelTime )THEN
  IF( iarg+1 > n_arg )GOTO 9999
  tEndSamp = tEndSamp * ParseTimeUnits( c_arg(iarg+1),lValidUnits )
  IF( lValidUnits )iarg = iarg + 1
END IF
IF( BTEST(run_mode,REVERSE_MODE) .AND. .NOT. lRelTime )tEndSamp = -tEndSamp

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampOutput()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER ios
LOGICAL lValidUnits

REAL, EXTERNAL :: ParseTimeUnits

iarg = iarg + 1           !Set for string following OUTPUT keyword
IF( iarg > n_arg )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampOutput'
  eMessage = 'Missing sampler output time information'
  GOTO  9999
END IF

lSmpOut   = .TRUE.
tolSmpOut = delt / (2.**MAXTLV)

IF( TRIM(c_arg(iarg)) == 'LIST' )THEN
  lSmpOutList = .TRUE.
  GOTO 9999 !No more output time parameters on header record
END IF

READ(c_arg(iarg),*,IOSTAT=ios) dtSmpOut  !Used for output interval
IF( ios /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampOutput'
  eMessage = 'Error reading output time interval time'
  eInform  = 'Required in sampler input file when specifying output'
  GOTO  9999
END IF

!------ Look for optional averaging period units

IF( iarg+1 > n_arg )GOTO 9999

dtSmpOut = dtSmpOut * ParseTimeUnits( c_arg(iarg+1),lValidUnits )
IF( lValidUnits )iarg = iarg + 1

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampTitle()

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER i

CHARACTER(1)    cq

iarg = iarg + 1           !Set for string following TITLE keyword
IF( iarg > n_arg )THEN
  nError   = IV_ERROR
  eRoutine = 'SetSampTitle'
  eMessage = 'Missing title information'
  GOTO  9999
END IF

IF( c_arg(iarg)(1:1) == CHAR(34) )THEN
  cq = CHAR(34)
ELSE IF( c_arg(iarg)(1:1) == CHAR(39) )THEN
  cq = CHAR(39)
ELSE
  nError   = IV_ERROR
  eRoutine = 'SetSampTitle'
  eMessage = 'Title must be given in quotes'
  GOTO  9999
END IF

c_arg(iarg) = TRIM(c_arg(iarg)(2:))

DO
  i = INDEX(c_arg(iarg),cq)
  IF( i == 0 )THEN
    SampTitle = TRIM(SampTitle)//' '//TRIM(c_arg(iarg))
  ELSE
    IF( i > 1 )SampTitle = TRIM(SampTitle)//' '//TRIM(c_arg(iarg)(1:i-1))
    EXIT
  END IF
  iarg = iarg + 1
  IF( iarg > n_arg )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampTitle'
    eMessage = 'Incomplete title information'
    eInform  = 'Must be terminated with quote'
    GOTO  9999
  END IF
END DO

SampTitle = TRIM(ADJUSTL(SampTitle))

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampOutputTimes( lun )

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

INTEGER ios
INTEGER alloc_stat
REAL    ts
LOGICAL lRelTime, lAnyRelTime, lValidUnits

REAL, EXTERNAL :: ParseSensorTime, ParseTimeUnits

!------ First build list of averaging times, if appropriate

IF( lSmpOutList )THEN

  NULLIFY(FirstSampTimeList%next)
  NULLIFY(SampTimeList)

  TimeListLoop : DO

    CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
    IF( lerr )THEN
      nError   = RD_ERROR
      eRoutine = 'SetSampOutputTimes'
      eMessage = 'Error reading sensor time list'
      eInform  = 'Occured reading line:'
      eAction  = TRIM(string)
      GOTO  9999
    END IF

    CALL cupper( c_arg(1) )

    IF( TRIM(c_arg(1)) == 'OUTPUT' .OR. TRIM(c_arg(1)) == 'LIST' )THEN  !Skip optional section header
      CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
      IF( lerr )GOTO 9999
    END IF
    IF( c_arg(1) == 'END' )EXIT TimeListLoop

    IF( lavg .AND. n_arg < 2 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampOutputTimes'
      eMessage = 'Invalid reading sensor time list'
      eInform  = 'Must specify time and averaging period'
      eAction  = 'Line: '//TRIM(string)
      GOTO  9999
    END IF

    ts = ParseSensorTime( c_arg(1),lRelTime )
    IF( ts == NOT_SET_R )THEN
      nError   = RD_ERROR
      eRoutine = 'SetSampOutputTimes'
      eMessage = 'Error reading sensor time'
      eInform  = 'Format must be HH:MM:SS.SS or YYYYMMDD:HH:MM:SS.SS'
      GOTO  9999
    END IF
    IF( BTEST(run_mode,REVERSE_MODE) .AND. .NOT. lRelTime )ts = -ts

    IF( lRelTime .AND. n_arg > 1 )THEN
      iarg = 2
      IF( (lavg .AND. n_arg > 2) .OR. (lSmpOut .AND. n_arg > 1) )THEN
        ts = ts * ParseTimeUnits( c_arg(2),lValidUnits )
        IF( .NOT.lValidUnits )THEN
          IF( lSmpOut )THEN
            nError   = RD_ERROR
            eRoutine = 'SetSampOutputTimes'
            eMessage = 'Invalid units for relative output times: '//TRIM(c_arg(2))
            eInform  = 'Valid units are S,SEC,SECONDS, M,MIN,MINUTES, or H,HR,HRS,HOUR,HOURS'
            GOTO  9999
          END IF
        ELSE
          iarg = 3
        END IF
      END IF
    ELSE
      iarg = 2
    END IF

    IF( lavg )THEN
      READ(c_arg(iarg),*,IOSTAT=ios) dtSmpOut
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'SetSampOutputTimes'
        eMessage = 'Error reading sensor time interval'
        GOTO  9999
      END IF
      IF( n_arg >= iarg+1 )THEN
        dtSmpOut = dtSmpOut * ParseTimeUnits( c_arg(iarg+1),lValidUnits )
        IF( .NOT.lValidUnits )THEN
          nError   = RD_ERROR
          eRoutine = 'SetSampOutputTimes'
          eMessage = 'Invalid units for time intervals: '//TRIM(c_arg(iarg+1))
          eInform  = 'Valid units are S,SEC,SECONDS, M,MIN,MINUTES, or H,HR,HRS,HOUR,HOURS'
          GOTO  9999
        END IF
      END IF
    ELSE
      dtSmpOut = 0.
    END IF

    IF( .NOT. ASSOCIATED(SampTimeList) )THEN

      FirstSampTimeList%tStart = ts
      FirstSampTimeList%dtSamp = dtSmpOut
      SampTimeList => FirstSampTimeList

      lAnyRelTime = lRelTime

    ELSE

      IF( SampTimeList%tStart+SampTimeList%dtSamp > ts )THEN
        nError   = IV_ERROR
        eRoutine = 'SetSampOutputTimes'
        eMessage = 'Output time starting before previous completed'
        eInform  = 'Time = '//TRIM(c_arg(1))
        GOTO  9999
      END IF

      IF( lRelTime .NEQV. lAnyRelTime )THEN
        nError   = IV_ERROR
        eRoutine = 'SetSampOutputTimes'
        eMessage = 'Invalid output times'
        eInform  = 'Cannot mix absolute and relative time specifications'
        GOTO  9999
      END IF


      ALLOCATE( SampTimeList%next,STAT=alloc_stat )
      SampTimeList => SampTimeList%next

      SampTimeList%tStart = ts
      SampTimeList%dtSamp   = dtSmpOut
      NULLIFY(SampTimeList%next)

    END IF

    IF( lAvg .AND. dtSmpOut <= 0. )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampOutputTimes'
      eMessage = 'Averaging period must be greater than 0'
      GOTO  9999
    END IF

  END DO TimeListLoop

!------ Add very large time if ending with Tavg=0.

  IF( SampTimeList%dtSamp == 0. )THEN
    ALLOCATE( SampTimeList%next,STAT=alloc_stat )
    SampTimeList => SampTimeList%next

    SampTimeList%tStart = DEF_VAL_R  !Large positive number
    SampTimeList%dtSamp = 0.
    NULLIFY(SampTimeList%next)
  END IF

!------ Set to initial values

  SampTimeList => FirstSampTimeList
  IF( lAvg )THEN
    tStartSamp = SampTimeList%tStart
    dtSmpOut   = SampTimeList%dtSamp
    tSmpOut    = tStartSamp + dtSmpOut !Set initial output time
  ELSE
    tSmpOut = SampTimeList%tStart
  END IF

ELSE

  tSmpOut  = tStartSamp + dtSmpOut !Set initial output time

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSampType( lGrid )

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

LOGICAL, INTENT( OUT ) :: lGrid

!------ Check for a moving sensor

CALL cupper( c_arg(1) )

IF( c_arg(1) == 'MOVING' )THEN
  iarg = 1
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_MOVING)
ELSE
  iarg = 0
END IF

CALL cupper( c_arg(iarg+1) )

!------ Check for a LOS sensor

IF( c_arg(iarg+1) == 'LOS' )THEN
  iarg = iarg+1
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_LOS)
  lLOSSmp = .TRUE.
END IF

!------ Check for a gridded output sensor

IF( c_arg(iarg+1) == 'FXGRID' )THEN
  iarg = iarg+1
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_FXGRID)
ELSE IF( c_arg(iarg+1) == 'GRID' )THEN
  iarg = iarg+1
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_AUTOGRID)
  smp(nsmp)%nx    = 0
  smp(nsmp)%ny    = 0
ELSE IF( c_arg(iarg+1) == 'FIXED' )THEN  !Same as no type
  iarg = iarg+1
END IF

lGrid    = BTEST(smp(nsmp)%stype,STB_FXGRID) .OR. BTEST(smp(nsmp)%stype,STB_AUTOGRID)
lGridOut = lGridOut .OR. lGrid

IF( lGrid )THEN
  IF( .NOT.(lSmpOut .AND. lSmpOutList) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampType'
    eMessage = 'Gridded sensors require OUTPUT LIST in header record'
    eInform  = 'followed by list of output times'
    CALL SensorNumStr( nsmp,eAction )
    GOTO  9999
  ELSE IF( BTEST(smp(nsmp)%stype,STB_MOVING) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampType'
    eMessage = 'Gridded sensors cannot be moving'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  ELSE IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampType'
    eMessage = 'LOS output invalid with gridded sensor'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
END IF

IF( lBinOut )THEN
  IF( BTEST(smp(nsmp)%stype,STB_MOVING) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampType'
    eMessage = 'MOVING sensor invalid with binary output'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  ELSE IF( BTEST(smp(nsmp)%stype,STB_AUTOGRID) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampType'
    eMessage = 'Adaptive grid sensor invalid with binary output'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE BuildSampClassList( lun )

!------ Build list of sampler classes, e.g., CONC, DEP, etc.
!       These will apply to all locations defined subsequently
!       N.B. Assumes 'CLASS' keyword has already been read

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

INTEGER ios, alloc_stat

CHARACTER(64)  :: stype
CHARACTER(500) :: svar

TYPE( SampClassT ), POINTER :: SampClass

!------ Read classes and choices until end of list
!       Build a linked-list and keep track of number

nSampClass = 0 !Reset (previously set in SCIPsensorInput)

DO

  CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,-MAXN_ARG,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'BuildSampClassList'
    eMessage = 'Error reading sensor class list line'
    GOTO  9999
  END IF

  CALL cupper( c_arg(1) )
  IF(  TRIM(c_arg(1)) == 'END' )EXIT

  IF( n_arg < 2 )THEN
    nError   = RD_ERROR
    eRoutine = 'BuildSampClassList'
    eMessage = 'Not enough input on sensor class line'
    GOTO  9999
  END IF

  READ(c_arg(1),'(A)',IOSTAT=ios) stype; IF( ios == 0 )READ(c_arg(2),'(A)',IOSTAT=ios) svar
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'BuildSampClassList'
    eMessage = 'Error reading sensor class and/or choice'
    eInform  = 'Line: '//TRIM(string)
    GOTO  9999
  END IF

  nSampClass = nSampClass + 1

  IF( nSampClass == 1 )THEN

    ALLOCATE( FirstSampClass,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'BuildSampClassList'
      eMessage = 'Error initializing sensor class linked-list'
      GOTO  9999
    END IF

    SampClass => FirstSampClass

  ELSE

    ALLOCATE( SampClass%next,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'BuildSampClassList'
      eMessage = 'Error extending sensor class linked-list'
      GOTO  9999
    END IF

    SampClass => SampClass%next

  END IF

  NULLIFY( SampClass%next )

  SampClass%type = TRIM(stype)
  SampClass%var  = TRIM(svar)

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CopySampType()

USE sampler_fi

IMPLICIT NONE

smp(nsmp)%stype = 0

IF( BTEST(smp(nsmp-1)%stype,STB_MOVING)   )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_MOVING)
IF( BTEST(smp(nsmp-1)%stype,STB_LOS)      )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_LOS)
IF( BTEST(smp(nsmp-1)%stype,STB_FXGRID)   )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_FXGRID)
IF( BTEST(smp(nsmp-1)%stype,STB_AUTOGRID) )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_AUTOGRID)
IF( BTEST(smp(nsmp-1)%stype,STB_AGL)      )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_AGL)

RETURN
END

!==============================================================================

SUBROUTINE CopySampLoc()

USE sampler_fi

IMPLICIT NONE

!------ Horizontal location

smp(nsmp)%x = smp(nsmp-1)%x
smp(nsmp)%y = smp(nsmp-1)%y

smp(nsmp)%x0 = smp(nsmp-1)%x0
smp(nsmp)%y0 = smp(nsmp-1)%y0

!------ Height

smp(nsmp)%zh = smp(nsmp-1)%zh
smp(nsmp)%h  = smp(nsmp-1)%h
smp(nsmp)%z  = smp(nsmp-1)%z

!------ Grid parameters

IF( BTEST(smp(nsmp)%stype,STB_FXGRID) )THEN         !Fixed Grid

  smp(nsmp)%nx  = smp(nsmp-1)%nx
  smp(nsmp)%ny  = smp(nsmp-1)%ny
  smp(nsmp)%dx  = smp(nsmp-1)%dx
  smp(nsmp)%dy  = smp(nsmp-1)%dy
  smp(nsmp)%dxs = smp(nsmp-1)%dxs
  smp(nsmp)%dys = smp(nsmp-1)%dys

ELSE IF( BTEST(smp(nsmp)%stype,STB_AUTOGRID) )THEN  !Auto Grid

  smp(nsmp)%npts    = smp(nsmp-1)%npts
  smp(nsmp)%autoLev = smp(nsmp-1)%autoLev

END IF

!------ LOS parameters

smp(nsmp)%az   = smp(nsmp-1)%az
smp(nsmp)%el   = smp(nsmp-1)%el
smp(nsmp)%dist = smp(nsmp-1)%dist
smp(nsmp)%lx   = smp(nsmp-1)%lx
smp(nsmp)%ly   = smp(nsmp-1)%ly
smp(nsmp)%lz   = smp(nsmp-1)%lz


RETURN
END

!==============================================================================

SUBROUTINE SetSampLoc( lGrid )

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE files_fi
USE met_fi, ONLY: hmin
USE los_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lGrid

REAL h, hx, hy, xmap, ymap
REAL    x, y
INTEGER ios
LOGICAL ldom

CHARACTER(10) cmap

TYPE( los_str ) :: los

REAL, EXTERNAL    :: norm_ang
LOGICAL ,EXTERNAL :: PointInBox

!------ Read horizontal location (except for auto grid)

IF( .NOT.BTEST(smp(nsmp)%stype,STB_AUTOGRID) )THEN

  READ(c_arg(iarg+1),*,ERR=9998,END=9998) smp(nsmp)%x
  READ(c_arg(iarg+2),*,ERR=9998,END=9998) smp(nsmp)%y
  iarg = iarg + 2

  IF( sampMap == I_UTM )THEN
    smp(nsmp)%x = smp(nsmp)%x * UTMfac
    smp(nsmp)%y = smp(nsmp)%y * UTMfac
  END IF

  smp(nsmp)%x0 = smp(nsmp)%x
  smp(nsmp)%y0 = smp(nsmp)%y

  IF( lSmpCnv )THEN
    CALL ConvertSampCoord( lmap,sampMap,sampZone,smp(nsmp)%x,smp(nsmp)%y )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( lMap == I_CARTESIAN .AND. sampMap == I_CARTESIAN .AND. lCartOrig )THEN
      smp(nsmp)%x = smp(nsmp)%x + cartX0
      smp(nsmp)%y = smp(nsmp)%y + cartY0
    END IF
  END IF

ELSE


END IF

!------ Read height; check for height reference

READ(c_arg(iarg+1),*,ERR=9998,END=9998) smp(nsmp)%zh
iarg = iarg + 1

CALL cupper( c_arg(iarg+1) )

IF( TRIM(c_arg(iarg+1)) == 'AGL' )THEN
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_AGL)
  iarg = iarg + 1
ELSE IF( TRIM(c_arg(iarg+1)) == 'MSL' )THEN
  smp(nsmp)%stype = IBCLR(smp(nsmp)%stype,STB_AGL)
  iarg = iarg + 1
ELSE
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_AGL) !Default is AGL, i.e, AGL or MSL not specified
END IF

IF( lGrid )THEN
  IF( .NOT.BTEST(smp(nsmp)%stype,STB_AGL) )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampLoc'
    eMessage = 'Gridded sensor height cannot be MSL'
    eInform  = 'Must be specified above ground level'
    CALL SensorNumStr( nsmp,eAction )
    GOTO  9999
  END IF
END IF

!------ Get terrain elevation

IF( lter .AND. .NOT.create .AND. .NOT.lGrid )THEN
  CALL get_topogPrj( smp(nsmp)%x,smp(nsmp)%y,h,hx,hy )
  smp(nsmp)%h = h
  IF( .NOT.BTEST(smp(nsmp)%stype,STB_AGL) )THEN
    IF( smp(nsmp)%zh < h+hmin )THEN
      nError   = IV_ERROR
      eRoutine = 'SetSampLoc'
      CALL SensorNumStr( nsmp,eMessage )
      eMessage = TRIM(eMessage)//' altitude is below ground'
      WRITE(eInform,'(A,F8.1)') 'Sensor alt = ',smp(nsmp)%zh
      WRITE(eAction,'(A,F8.1)') 'Ground alt = ',h+hmin
      GOTO  9999
    END IF
    smp(nsmp)%zh = MAX(smp(nsmp)%zh-hmin-h,0.)
  END IF
ELSE
  smp(nsmp)%h = 0.
END IF

smp(nsmp)%z = smp(nsmp)%zh + smp(nsmp)%h

!------ Read gridded input

IF( BTEST(smp(nsmp)%stype,STB_FXGRID) )THEN         !Fixed Grid

  READ(c_arg(iarg+1),*,ERR=9998,END=9998) smp(nsmp)%nx
  READ(c_arg(iarg+2),*,ERR=9998,END=9998) smp(nsmp)%ny
  READ(c_arg(iarg+3),*,ERR=9998,END=9998) smp(nsmp)%dx   !Meters
  READ(c_arg(iarg+4),*,ERR=9998,END=9998) smp(nsmp)%dy
  iarg = iarg + 4
  IF( smp(nsmp)%nx < 1 .OR. smp(nsmp)%ny < 1 )THEN
    nError   = IV_ERROR
    eRoutine = 'SetSampLoc'
    eMessage = 'Invalid grid definition'
    eInform  = 'Number of points must be 1 or greater in both x- and y-directions'
    CALL SensorNumStr( nsmp,eAction )
    GOTO 9999
  END IF

!------ Convert grid spacings to project coordinates

  CALL mapfac( smp(nsmp)%x,smp(nsmp)%y,xmap,ymap )
  smp(nsmp)%dxs = smp(nsmp)%dx * xmap
  smp(nsmp)%dys = smp(nsmp)%dy * ymap

ELSE IF( BTEST(smp(nsmp)%stype,STB_AUTOGRID) )THEN  !Auto Grid

  READ(c_arg(iarg+1),*,ERR=9998,END=9998) smp(nsmp)%npts
  READ(c_arg(iarg+2),*,ERR=9998,END=9998) smp(nsmp)%autoLev
  iarg = iarg + 2

END IF

!------ Read LOS sensor orientation

IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  READ(c_arg(iarg+1),*,ERR=9998,END=9998) smp(nsmp)%az
  READ(c_arg(iarg+2),*,ERR=9998,END=9998) smp(nsmp)%el
  READ(c_arg(iarg+3),*,ERR=9998,END=9998) smp(nsmp)%dist
  smp(nsmp)%az = norm_ang( smp(nsmp)%az )
  CALL set_smp_los( smp(nsmp) )
  IF( .NOT.create )THEN
    CALL set_los_str( smp(nsmp),smp(nsmp)%x,smp(nsmp)%y,los )
    CALL grnd_intersect( los,h )
    IF( smp(nsmp)%dist <= 0. )THEN
      smp(nsmp)%dist = h
    ELSE IF( h > 0. )THEN
      smp(nsmp)%dist = MIN(smp(nsmp)%dist,h)
    END IF
  END IF
  iarg = iarg + 3
ELSE
  smp(nsmp)%az   = NOT_SET_R
  smp(nsmp)%el   = NOT_SET_R
  smp(nsmp)%dist = NOT_SET_R
END IF

!------ Check if locations are in domain

SELECT CASE( lmap )  !Build string describing project coordinates for possible error message
  CASE( I_LATLON )
    cmap = 'LATLON'
  CASE( I_UTM )
    cmap = 'UTM'
  CASE( I_CARTESIAN )
    cmap  ='CARTESIAN'
  CASE( I_METERS )
    cmap = 'METERS'
  CASE DEFAULT
    cmap = 'LATLON'
END SELECT

IF( BTEST(smp(nsmp)%stype,STB_FXGRID) )THEN

  ldom = .FALSE.
  x = smp(nsmp)%x; y = smp(nsmp)%y
  ldom = PointInBox(x,y,xmin,xmax,ymin,ymax)
  x = x + FLOAT(smp(nsmp)%nx-1)*smp(nsmp)%dx;
  ldom = ldom .OR. PointInBox(x,y,xmin,xmax,ymin,ymax)
  y = y + FLOAT(smp(nsmp)%ny-1)*smp(nsmp)%dy;
  ldom = ldom .OR. PointInBox(x,y,xmin,xmax,ymin,ymax)
  x = smp(nsmp)%x;
  ldom = ldom .OR. PointInBox(x,y,xmin,xmax,ymin,ymax)

  IF( .NOT.lDom )THEN
    nError = IV_ERROR
    CALL SensorNumStr( nsmp,eMessage )
    eMessage = TRIM(eMessage)//' gridded domain entirely outside SCIPUFF domain'
    WRITE(eInform,'(A,2ES13.4)',IOSTAT=ios) 'SW corner in project coord (x,y): ',smp(nsmp)%x,smp(nsmp)%y
    IF( lSmpCnv )THEN
      eAction = 'Grid location converted to project coordinates: '//TRIM(cmap)
    ELSE
      eAction = 'Grid location assumed in project coordinates: '//TRIM(cmap)
    END IF
    GOTO 9999
  END IF

ELSE IF( .NOT.(BTEST(smp(nsmp)%stype,STB_AUTOGRID)) )THEN

  IF( .NOT.PointInBox(smp(nsmp)%x,smp(nsmp)%y,xmin,xmax,ymin,ymax) )THEN
    CALL SensorNumStr( nsmp,eMessage )
    eMessage = TRIM(eMessage)//' is located outside SCIPUFF domain'
    WRITE(eInform,'(A,2ES13.4)',IOSTAT=ios) 'Location in project coord: ',smp(nsmp)%x,smp(nsmp)%y
    IF( lSmpCnv )THEN
      eAction = 'Sampler location converted to project coordinates: '//TRIM(cmap)
    ELSE
      eAction = 'Sampler location assumed in project coordinates: '//TRIM(cmap)
    END IF
    CALL CautionMessage()
  END IF

END IF

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'SetSampLoc'
eMessage = 'Error reading sensor input'
CALL SensorNumStr( nsmp,eInform )
GOTO  9999

END

!==============================================================================

SUBROUTINE SetWayPoints( lun )

!------ Set waypoints for sampler nsmp

USE scipuff_fi
USE sampler_fi
USE ParseLine_fi
USE met_fi, ONLY: hmin
USE los_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

INTEGER alloc_stat
LOGICAL lfirst_point

TYPE( WayPoint ), POINTER :: point, prev

REAL, EXTERNAL :: norm_ang

lfirst_point = .TRUE.

WayPointsLoop : DO

  CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
  IF( lerr )THEN
    nError   = RD_ERROR
    eRoutine = 'SetWayPoints'
    eMessage = 'Error reading sensor waypoints list'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF

  CALL cupper( c_arg(1) )
  IF( c_arg(1) == 'END' )EXIT WayPointsLoop

  IF( lfirst_point )THEN

    ALLOCATE( smp(nsmp)%nextpoint,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetWayPoints'
      eMessage = 'Allocate error initializing way points list'
      CALL SensorNumStr( nsmp,eInform )
      GOTO 9999
    END IF
    point => smp(nsmp)%nextpoint

    ALLOCATE( point%prev )
    point%prev%time = 0.; point%prev%x = smp(nsmp)%x; point%prev%y = smp(nsmp)%y;
    IF( BTEST(smp(nsmp)%stype,STB_AGL) )THEN
      point%prev%z = smp(nsmp)%zh
    ELSE
      point%prev%z = smp(nsmp)%z
    END IF
    point%prev%az = smp(nsmp)%az; point%prev%el = smp(nsmp)%el; point%prev%dist = smp(nsmp)%dist;
    NULLIFY( point%prev%prev )
    point%prev%next => point

    lfirst_point = .FALSE.

  ELSE

    ALLOCATE( point%next,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetWayPoints'
      eMessage = 'Allocate error building way points list'
      CALL SensorNumStr( nsmp,eInform )
      GOTO 9999
    END IF
    prev       => point
    point      => point%next
    point%prev => prev

  END IF

  NULLIFY( point%next )

  IF( n_arg < 4 )THEN
    nError   = IV_ERROR
    eRoutine = 'SetWayPoints'
    eMessage = 'Incomplete moving sensor record'
    CALL SensorNumStr( nsmp,eInform )
    GOTO 9999
  END IF

  READ(c_arg(1),*,ERR=9998,END=9998) point%time
  READ(c_arg(2),*,ERR=9998,END=9998) point%x
  READ(c_arg(3),*,ERR=9998,END=9998) point%y
  READ(c_arg(4),*,ERR=9998,END=9998) point%z
  IF( .NOT.BTEST(smp(nsmp)%stype,STB_AGL) )point%z = point%z - hmin

  IF( sampMap == I_UTM )THEN
    smp(nsmp)%x = smp(nsmp)%x * UTMfac
    smp(nsmp)%y = smp(nsmp)%y * UTMfac
  END IF

  IF( lSmpCnv )THEN
    CALL ConvertSampCoord( lmap,sampMap,sampZone,point%x,point%y )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( lMap == I_CARTESIAN .AND. sampMap == I_CARTESIAN .AND. lCartOrig )THEN
      point%x = point%x + cartX0
      point%y = point%y + cartY0
    END IF
  END IF

  IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
    IF( n_arg < 4 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetWayPoints'
      eMessage = 'Incomplete moving LOS sensor record'
      CALL SensorNumStr( nsmp,eInform )
      GOTO 9999
    END IF
    READ(c_arg(5),*,ERR=9998,END=9998) point%az
    READ(c_arg(6),*,ERR=9998,END=9998) point%el
    READ(c_arg(7),*,ERR=9998,END=9998) point%dist
    point%az = norm_ang( point%az )
  ELSE
    point%az   = NOT_SET_R
    point%el   = NOT_SET_R
    point%dist = NOT_SET_R
  END IF

END DO WayPointsLoop

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'SetWayPoints'
eMessage = 'Error reading moving sensor input'
CALL SensorNumStr( nsmp,eInform )
GOTO  9999

END

!==============================================================================

SUBROUTINE CopyWayPoints()

!------ Copy waypoints from nsmp-1 to nsmp
!       For use in defining samplers by class

USE scipuff_fi
USE sampler_fi

IMPLICIT NONE

INTEGER alloc_stat

TYPE( WayPoint ), POINTER :: point, point0, prev

ALLOCATE( smp(nsmp)%nextpoint,STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'CopyWayPoints'
  eMessage = 'Allocate error initializing way points list'
  CALL SensorNumStr( nsmp,eInform )
  GOTO 9999
END IF

point => smp(nsmp)%nextpoint; point0 => smp(nsmp-1)%nextpoint

ALLOCATE( point%prev )
point%prev%time = 0.; point%prev%x = smp(nsmp)%x; point%prev%y = smp(nsmp)%y;
IF( BTEST(smp(nsmp)%stype,STB_AGL) )THEN
  point%prev%z = smp(nsmp)%zh
ELSE
  point%prev%z = smp(nsmp)%z
END IF
point%prev%az = smp(nsmp)%az; point%prev%el = smp(nsmp)%el; point%prev%dist = smp(nsmp)%dist;
NULLIFY( point%prev%prev )
point%prev%next => point

DO

  point%time = point0%time
  point%x    = point0%x
  point%y    = point0%y
  point%z    = point0%z
  point%az   = point0%az
  point%el   = point0%el
  point%dist = point0%dist

  NULLIFY(point%next)

  IF( ASSOCIATED(point0%next) )THEN
    ALLOCATE( point%next,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'CopyWayPoints'
      eMessage = 'Allocate error building way points list'
      CALL SensorNumStr( nsmp,eInform )
      GOTO 9999
    END IF
    prev  => point
    point => point%next; point0 => point0%next
    point%prev => prev
  ELSE
    EXIT
  END IF

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CheckTimeInt( lGrid )

USE scipuff_fi
USE sampler_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lGrid

INTEGER i
INTEGER ios, j

i = INDEX(smp(nsmp)%type,':')
IF( i > 0 )THEN
  IF( smp(nsmp)%type(i+1:i+3) == 'INT' )THEN
    SELECT CASE( smp(nsmp)%type(1:i-1) )
      CASE( 'DOS' )
        nError   = IV_ERROR
        eRoutine = 'CheckTimeInt'
        eMessage = 'Invalid sensor type string'
        CALL SensorNumStr( nsmp,eInform )
        eAction  = 'Cannot be INT with DOS output'
        GOTO 9999
    END SELECT
    smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_INTEGRATE)
    j = INDEX(smp(nsmp)%type(i+1:),':')
    IF( j > 0 )j = j + i
  ELSE
    j = i
  END IF
  IF( j > 0 )THEN
    READ(smp(nsmp)%type(j+1:),*,IOSTAT=ios) smp(nsmp)%conv
    IF( ios /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'CheckTimeInt'
      eMessage = 'Invalid sensor type string with colon (:)'
      CALL SensorNumStr( nsmp,eInform )
      eAction  = 'Must be INT or valid conversion factor (float)'
      GOTO 9999
    END IF
  END IF
  smp(nsmp)%type = smp(nsmp)%type(1:i-1)

ELSE
  SELECT CASE( TRIM(smp(nsmp)%type) )
    CASE( 'DOS' )
      smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_INTEGRATE)
  END SELECT

END IF

IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) .AND. lGrid )THEN
  IF( TRIM(smp(nsmp)%type) /= 'CONC' )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckTimeInt'
    eMessage = 'Invalid integrated (INT) sensor'
    eInform  = 'Only integrated CONC sensors allowed with gridded output'
    CALL SensorNumStr( nsmp,eAction )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetMovingOutput()

USE scipuff_fi
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

CHARACTER(5) xUnits, yUnits

IF( nError /= NO_ERROR )GOTO 9999

IF( BTEST(smp(nsmp)%stype,STB_MOVING) .AND. .NOT.lWrap )THEN
  CALL reallocate_smp_vname( 3 )
  IF( nError /= NO_ERROR )GOTO 9999
  IF( lmap == I_LATLON )THEN
    xUnits = 'deg-E'
    yUnits = 'deg-N'
  ELSE
    xUnits = 'km'
    yUnits = 'km'
  END IF
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'X'; smp_units(nvarsmp) = TRIM(xUnits)
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'Y'; smp_units(nvarsmp) = TRIM(yUnits)
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'Z'; smp_units(nvarsmp) = 'm'
  IF( BTEST(smp(nsmp)%stype,STB_LOS) )then
    CALL reallocate_smp_vname( 3 )
    IF( nError /= NO_ERROR )GOTO 9999
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'AZ'; smp_units(nvarsmp) = 'deg'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'EL'; smp_units(nvarsmp) = 'deg'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'DIST'; smp_units(nvarsmp) = 'm'
  END IF
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_OUTLOC)
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetConcOutput( lGrid )

USE scipuff_fi
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lGrid

INTEGER i, ios, nch, imat
LOGICAL lSetTotVar

CHARACTER(5) Units

LOGICAL, EXTERNAL :: IsWetParticle

i = INDEX(smp(nsmp)%var,':')
IF( i > 0 )THEN
  matname = smp(nsmp)%var(1:i-1)
  nch     = LEN_TRIM(smp(nsmp)%var)
  READ(smp(nsmp)%var(i+1:nch),*,IOSTAT=ios) isg
  IF( ios /= 0 )THEN
    nError = RD_ERROR
    eRoutine = 'SetConcOutput'
    eMessage = 'Error reading material subgroup'
    CALL SensorNumStr( nsmp,eInform )
    GOTO 9999
  END IF
ELSE
  matname = TRIM(smp(nsmp)%var)
  isg     = 0
END IF

CALL check_smp_matl( matname,isg,smp(nsmp)%is,smp(nsmp)%ie )
IF( nError /= NO_ERROR )THEN
  CALL SensorNumStr( nsmp,eInform )
  GOTO 9999
END IF

imat = typeID(smp(nsmp)%is)%imat
Units = material(imat)%unit

IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) .AND. lGrid )THEN
  CALL check_smp_dos( smp(nsmp) )
  IF( nError /= NO_ERROR )GOTO 9999
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_INTGRID)
END IF

IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) )THEN
  smp(nsmp)%nvar = 2  !Mean & var
  CALL reallocate_smp_vname( 2 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'-s/m3'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2-s2/m6'
ELSE IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  smp(nsmp)%nvar = 2  !Mean & var
  CALL reallocate_smp_vname( 2 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'/m2'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m4'
ELSE IF( lAvg )THEN
  smp(nsmp)%nvar = 2  !Mean & var
  CALL reallocate_smp_vname( smp(nsmp)%nvar )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'; smp_units(nvarsmp) = TRIM(Units)//'/m3'
  IF( smp(nsmp)%nvar == 2 )THEN
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m6'
  END IF
ELSE
  smp(nsmp)%nvar = 3  !Mean, var & scale
  CALL reallocate_smp_vname( 3 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'; smp_units(nvarsmp) = TRIM(Units)//'/m3'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m6'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'T'; smp_units(nvarsmp) = 's'
END IF

lSetTotVar = (isg == 0 .AND. smp(nsmp)%ie > smp(nsmp)%is)
IF( .NOT.BTEST(smp(nsmp)%stype,STB_INTGRID) )THEN
  lSetTotVar = lSetTotVar .OR. IsWetParticle(material(typeID(smp(nsmp)%is)%imat)%icls)
END IF
IF( lSetTotVar )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_TOTALVAR)

IF( lBinOut .AND. BTEST(smp(nsmp)%stype,STB_INTGRID) )smp(nsmp)%npts = imat

IF( TRIM(smp(nsmp)%type) == 'CONC_ND' )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_NODECAY)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetMCOutput()

USE scipuff_fi
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

CHARACTER(500) string

INTEGER ios, alloc_stat, i, nch, jc, ii
INTEGER n
CHARACTER(5),  DIMENSION(:), ALLOCATABLE :: tmp_name

LOGICAL, EXTERNAL :: IsWetParticle

string = TRIM(smp(nsmp)%var)  !Copy to local string

i = INDEX(smp(nsmp)%var,':')
IF( i > 0 )THEN
  matname = smp(nsmp)%var(1:i-1)
  nch     = LEN_TRIM(smp(nsmp)%var)
  i = INDEX(string,TRIM(matname)) + LEN_TRIM(matname) + 1
  IF( string(i:i) == '(' )THEN
    jc = INDEX(string,')',BACK=.TRUE.) !Point to last close parent
    IF( jc == 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SetMCOutput'
      eMessage = 'Error in multicomponent input'
      eInform  = 'Missing close parenthesis for component list or list too long'
      CALL SensorNumStr( nsmp,eAction )
      GOTO  9999
    END IF
    jc = jc - 1        !Last character within parens
    i  = i + 1         !Points to first character of first component
    smp(nsmp)%nmc = 0
    DO
      ii = INDEX(string(i:),',')
      IF( ii == 0 .OR. ii+i-1 > jc )ii = INDEX(string(i:),')',BACK=.TRUE.)
      IF( ii < 2 )THEN
        nError   = UK_ERROR
        eRoutine = 'SetMCOutput'
        eMessage = 'Error reading multicomponent input'
        CALL SensorNumStr( nsmp,eInform )
        GOTO 9999
      END IF
      CALL reallocate_compname( 1 )
      smp(nsmp)%nmc = smp(nsmp)%nmc + 1
      READ(string(i:i+ii-2),'(A)',IOSTAT=ios) compName(smp(nsmp)%nmc)
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'SetMCOutput'
        eMessage = 'Error reading multicomponent input'
        CALL SensorNumStr( nsmp,eInform )
        GOTO 9999
      END IF
      IF( LEN_TRIM(compName(smp(nsmp)%nmc)) == 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'SetMCOutput'
        eMessage = 'Blank multicomponent name in input list'
        CALL SensorNumStr( nsmp,eInform )
        GOTO 9999
      END IF
      i = i + ii
      IF( i >= jc )EXIT
    END DO
  ELSE
    smp(nsmp)%nmc = 1
    i = INDEX(smp(nsmp)%var,':')
    ALLOCATE( compName(1),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SetMCOutput'
      eMessage = 'Error allocating multicomponent input array'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999
    END IF
    READ(smp(nsmp)%var(i+1:nch),'(A)') compName(1)
  END IF
ELSE
  matname       = TRIM(smp(nsmp)%var)
  smp(nsmp)%nmc = -1
  ALLOCATE( compName(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SetMCOutput'
    eMessage = 'Error allocating multicomponent input array'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
END IF

CALL check_smp_matl( matname,0,smp(nsmp)%is,smp(nsmp)%ie )
IF( nError /= NO_ERROR )GOTO 9999

CALL CheckSampMC( smp(nsmp),compName )
IF( nError /= NO_ERROR )GOTO 9999

DEALLOCATE( compName,STAT=alloc_stat )

ii = nvarsmp + 1
IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) .OR. &
    BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  smp(nsmp)%nvar = 2  !Mean & var
  CALL reallocate_smp_vname( 2 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'
ELSE IF( lAvg  )THEN
  smp(nsmp)%nvar = 2  !Mean, var
  CALL reallocate_smp_vname( 2 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'
ELSE
  smp(nsmp)%nvar = 3  !Mean, var & scale
  CALL reallocate_smp_vname( 3 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'T'
END IF

CALL reallocate_smp_vname( smp(nsmp)%nmc )
DO i = 1,smp(nsmp)%nmc
  nvarsmp = nvarsmp + 1
  WRITE(smp_vname(nvarsmp),'(A1,I3.3,A1)',IOSTAT=ios) &
                                          smp_vname(ii)(1:1),smp(nsmp)%mcID(i),'_'
END DO
smp(nsmp)%nvar = smp(nsmp)%nvar + smp(nsmp)%nmc

IF( (smp(nsmp)%ie > smp(nsmp)%is .OR. &
     IsWetParticle(material(typeID(smp(nsmp)%is)%imat)%icls)) )THEN
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_TOTALVAR)
END IF

IF( smp(nsmp)%nmc > 0 )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_MULT)
IF( multicomp )THEN
  ALLOCATE( smp(nsmp)%asmp(3+smp(nsmp)%nmc),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SCIPSensorInput'
    eMessage = 'Error allocating array for sensor ambient output'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
  smp(nsmp)%asmp = 0.
  IF( .NOT. ALLOCATED(asmp_vname) )THEN
    ALLOCATE(asmp_vname(3+smp(nsmp)%nmc),STAT=alloc_stat )
    n = 0
    sampamb = .TRUE.
  ELSE
    n = SIZE(asmp_vname)
    ALLOCATE( tmp_name(n),STAT=alloc_stat )
    IF( alloc_stat == 0 )THEN
      tmp_name = asmp_vname
      DEALLOCATE( asmp_vname,STAT=alloc_stat )
      ALLOCATE(asmp_vname(n+3+smp(nsmp)%nmc),STAT=alloc_stat )
      IF( alloc_stat == 0 )THEN
        asmp_vname(1:n) = tmp_name
        DEALLOCATE( tmp_name,STAT=alloc_stat )
      END IF
    END IF
  END IF
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SCIPSensorInput'
    eMessage = 'Error allocating vname array for sensor ambient output'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
  asmp_vname(n+1) = 'Tk'
  asmp_vname(n+2) = 'Pa'
  asmp_vname(n+3) = 'Hg'
  n = n + 3
  DO i = 1,smp(nsmp)%nmc
    WRITE(asmp_vname(n+i),'(A1,I3.3,A1)',IOSTAT=ios) &
                                            smp_vname(ii)(1:1),smp(nsmp)%mcID(i),'_'
  END DO
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetDosOutput()

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

INTEGER i, nch, imat

CHARACTER(5) Units

LOGICAL, EXTERNAL :: IsWetParticle

IF( .NOT.BTEST(smp(nsmp)%stype,STB_AGL) .OR. smp(nsmp)%zh /= 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'SetDosOutput'
  eMessage = 'DOS sensor height cannot be MSL'
  eInform  = 'Must be specified as zero height above ground level'
  CALL SensorNumStr( nsmp,eAction )
  GOTO  9999
END IF

i = INDEX(smp(nsmp)%var,':')
IF( i > 0 )THEN
  matname = smp(nsmp)%var(1:i-1)
  nch     = LEN_TRIM(smp(nsmp)%var)
  READ(smp(nsmp)%var(i+1:nch),*) isg
ELSE
  matname = TRIM(smp(nsmp)%var)
  isg     = 0
END IF

CALL check_smp_matl( matname,isg,smp(nsmp)%is,smp(nsmp)%ie )
IF( nError /= NO_ERROR )THEN
  CALL SensorNumStr( nsmp,eInform )
  GOTO 9999
END IF

imat = typeID(smp(nsmp)%is)%imat
Units = material(imat)%unit

CALL check_smp_dos( smp(nsmp) )
IF( nError /= NO_ERROR )GOTO 9999

smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_INTGRID)

IF( lBinOut )smp(nsmp)%npts = imat

smp(nsmp)%nvar = 2  !Mean & var
CALL reallocate_smp_vname( 2 )
IF( nError /= NO_ERROR )GOTO 9999

Units = material(imat)%unit

nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'-s/m3'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2-s2/m6'

9999 CONTINUE

RETURN
END
!==============================================================================

SUBROUTINE SetDepOutput()

USE scipuff_fi
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

INTEGER imat

CHARACTER(5) Units

IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  nError   = IV_ERROR
  eRoutine = 'SetDepOutput'
  eMessage = 'LOS output invalid with deposition sensor'
  CALL SensorNumStr( nsmp,eInform )
  GOTO  9999
ELSE IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) )THEN
  nError   = IV_ERROR
  eRoutine = 'SetDepOutput'
  eMessage = 'Integrated output invalid with deposition sensor'
  CALL SensorNumStr( nsmp,eInform )
  GOTO  9999
ELSE IF( smp(nsmp)%zh /= 0. .OR. .NOT.BTEST(smp(nsmp)%stype,STB_AGL) )THEN
  nError   = IV_ERROR
  eRoutine = 'SetDepOutput'
  eMessage = 'Height of deposition sensor must be 0 AGL'
  CALL SensorNumStr( nsmp,eInform )
  GOTO  9999
END IF

CALL check_smp_dep( smp(nsmp),imat )
IF( nError /= NO_ERROR )GOTO 9999

IF( lBinOut )smp(nsmp)%npts = imat

smp(nsmp)%nvar = 2  !Mean & var  (no timescale)
CALL reallocate_smp_vname( 2 )
IF( nError /= NO_ERROR )GOTO 9999

Units = material(imat)%unit

nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'/m2'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m4'

!------ Add surface liquid field

IF( smp(nsmp)%ie == -1 )THEN

  smp(nsmp)%nvar = smp(nsmp)%nvar +1
  CALL reallocate_smp_vname( 1 )
  IF( nError /= NO_ERROR )GOTO 9999

  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'R'; smp_units(nvarsmp) = TRIM(Units)//'/m2'

  lDepS = .TRUE.

END IF

smp(nsmp)%type  = 'DEP'
smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_DEP)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetMetOutput()

USE scipuff_fi
USE sampler_fi
USE SamplerUtil
use met_fi, ONLY : lensm

IMPLICIT NONE

IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  nError   = IV_ERROR
  eRoutine = 'SCIPSensorInput'
  eMessage = 'Met sensor cannot have LOS output'
  CALL SensorNumStr( nsmp,eInform )
  GOTO  9999
END IF

smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_MET)

IF( lAvg )smp(nsmp)%stype = IBCLR(smp(nsmp)%stype,STB_INTEGRATE) !Clear integration bit if doing time-averaging

smp(nsmp)%nvar = 4                    !u, v, w, t
CALL reallocate_smp_vname( 4 )
IF( nError /= NO_ERROR )GOTO 9999
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'U'; smp_units(nvarsmp) = 'm/s'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = 'm/s'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'W'; smp_units(nvarsmp) = 'm/s'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'T'; smp_units(nvarsmp) = 'K'

IF( smp(nsmp)%var(1:4) == 'TURB' )THEN

  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_TURB)
  smp(nsmp)%nvar = smp(nsmp)%nvar + 7  !zinv, hflux, L, uubl, vvbl, wwbl, ustar
  CALL reallocate_smp_vname( 7 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'ZI';   smp_units(nvarsmp) = 'm'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'HFLX'; smp_units(nvarsmp) = 'K-m/s'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'L';    smp_units(nvarsmp) = 'm'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'UU';   smp_units(nvarsmp) = 'm2/s2'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'VV';   smp_units(nvarsmp) = 'm2/s2'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'WW';   smp_units(nvarsmp) = 'm2/s2'
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'USTR'; smp_units(nvarsmp) = 'm/s'

ELSE IF( smp(nsmp)%var(1:4) == 'ENSM' .OR. &
         smp(nsmp)%var(1:4) == 'UNC' )THEN

  IF( lensm )THEN
    smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_ENSM)
    smp(nsmp)%nvar = smp(nsmp)%nvar + 4
    CALL reallocate_smp_vname( 4 )
    IF( nError /= NO_ERROR )GOTO 9999
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'UUL'; smp_units(nvarsmp) = 'm2/s2'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'VVL'; smp_units(nvarsmp) = 'm2/s2'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'UVL'; smp_units(nvarsmp) = 'm2/s2'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'SBL'; smp_units(nvarsmp) = 'm'
  END IF

ELSE  !If var is unrecognized for met sensor, assume this should be sensor name

  smp(nsmp)%name = TRIM(smp(nsmp)%var)

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetCorrOutput( lGrid )

USE scipuff_fi
USE sampler_fi
USE SamplerUtil

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lGrid

INTEGER i, nch

LOGICAL, EXTERNAL :: IsWetParticle

IF( lGrid )THEN
  nError   = IV_ERROR
  eRoutine = 'SetCorrOutput'
  eMessage = 'Correlation sensor cannot have gridded output'
  CALL SensorNumStr( nsmp,eInform )
  GOTO  9999
END IF

i = INDEX(smp(nsmp)%var,':')
IF( i > 0 )THEN
  matname = smp(nsmp)%var(1:i-1)
  nch     = LEN_TRIM(smp(nsmp)%var)
  READ(smp(nsmp)%var(i+1:nch),*) isg
ELSE
  matname = TRIM(smp(nsmp)%var)
  isg     = 0
END IF

CALL check_smp_matl( matname,isg,smp(nsmp)%is,smp(nsmp)%ie )
IF( nError /= NO_ERROR )GOTO 9999

!------ Clear integrate or LOS bits since they're invalid; set correlation bit

smp(nsmp)%stype = IBCLR(smp(nsmp)%stype,STB_INTEGRATE)
smp(nsmp)%stype = IBCLR(smp(nsmp)%stype,STB_LOS)

smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_CORR)

smp(nsmp)%nvar = 7  !Mean, var, Te, Tl, si, u, v
CALL reallocate_smp_vname( smp(nsmp)%nvar )
IF( nError /= NO_ERROR )GOTO 9999
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'  !N.B. Don't set units since only used for gridded output
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'T'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'TL'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'SI'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'U'
nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'

IF( isg == 0 .AND. (smp(nsmp)%ie > smp(nsmp)%is .OR. &
                    IsWetParticle(material(typeID(smp(nsmp)%is)%imat)%icls)) )THEN
  smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_TOTALVAR)
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetBySizeOutput( lgrid )

USE scipuff_fi
USE sampler_fi
USE SamplerUtil
USE reallocate

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lGrid

INTEGER ic, imat, iclass, nvar
REAL    dryFrac
LOGICAL lWet, lFrac
LOGICAL lLiq

CHARACTER(5) Units

LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsParticle

smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_PART)

CALL SetBySizeMat( ic,imat,iclass )
IF( nError /= NO_ERROR )GOTO 9999

IF( .NOT.(IsLiquid(iclass) .OR. IsWetParticle(iclass) .OR. IsParticle(iclass) ) )THEN
  nError   = IV_ERROR
  eRoutine = 'SetBySizeOutput'
  eMessage = 'Invalid material for size-based sensor: '//TRIM(matname)
  eInform  = 'Must be a particle, wet particle or liquid material'
  CALL SensorNumStr( nsmp,eAction )
  GOTO  9999
END IF

smp(nsmp)%nvar = 0

lWet = IsWetParticle(iclass)
lLiq = IsLiquid(iclass)

isg = smp(nsmp)%ie - smp(nsmp)%is + 1

IF( lWet )THEN
  smp(nsmp)%ie = smp(nsmp)%ie + isg  !Point to ALL groups
ELSE IF( lLiq) THEN
  smp(nsmp)%is = smp(nsmp)%is + 1    !Ignore vapor group
END IF

CALL ParseBinSize( ic,lWet,lFrac,dryFrac )
IF( nError /= NO_ERROR )GOTO 9999

IF( lgrid .AND. lFrac )THEN
  nError   = IV_ERROR
  eRoutine = 'SetBySizeOutput'
  eMessage = 'Invalid definition of output bin boundaries'
  eInform  = 'Explicit bin boundaries must be set for gridded sensors'
  CALL SensorNumStr( nsmp,eAction )
  GOTO  9999
END IF

CALL SetBinSize( lFrac,lWet,lLiq,dryFrac,imat )
IF( nError /= NO_ERROR )GOTO 9999

!------ Setup prefixes and nvar for concentration or dose

IF( lAvg .OR.  BTEST(smp(nsmp)%stype,STB_INTEGRATE) .OR. BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  nvar = 2  !Mean & var
ELSE
  nvar = 3  !Mean, var & scale
END IF

Units = material(imat)%unit

!------ Loop over bins

DO isg = 1,smp(nsmp)%nbin

  smp(nsmp)%nvar = smp(nsmp)%nvar + nvar + 1  !Mean, var, [scale,] number density
  CALL reallocate_smp_vname( nvar+1 )
  IF( nError /= NO_ERROR )GOTO 9999
  IF( BTEST(smp(nsmp)%stype,STB_INTEGRATE) )THEN
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'-s/m3'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2-s2/m6'
  ELSE IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'D'; smp_units(nvarsmp) = TRIM(Units)//'/m2'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m4'
  ELSE
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'C'; smp_units(nvarsmp) = TRIM(Units)//'/m3'
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(Units)//'2/m6'
  END IF
  IF( nvar == 3 )THEN
    nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'T'; smp_units(nvarsmp) = 's'
  END IF
  nvarsmp = nvarsmp + 1; smp_vname(nvarsmp) = 'N'; smp_units(nvarsmp) = 'no./m3'

END DO

IF( TRIM(smp(nsmp)%type) == 'PARTICLE_ND' .OR. TRIM(smp(nsmp)%type) == 'BYSIZE_ND' .OR. &
    TRIM(smp(nsmp)%type) == 'BY_SIZE_ND' )smp(nsmp)%stype = IBSET(smp(nsmp)%stype,STB_NODECAY)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CheckSampMC( ss,compName )

USE sampler_fd
USE scipuff_fi

IMPLICIT NONE

TYPE( sensor ),             INTENT( INOUT ) :: ss        !Sensor structure
CHARACTER(*), DIMENSION(*), INTENT( IN    ) :: compName

INTEGER imat, icls, mcID

LOGICAL, EXTERNAL :: IsMulti

imat = typeID(ss%is)%imat
icls = material(imat)%icls

IF( .NOT.IsMulti(icls) )THEN
  nError   = IV_ERROR
  eRoutine = 'CheckSampMC'
  eMessage = 'Invalid multicomponent material for sampler output'
  eInform  = TRIM(ss%var)
  GOTO  9999
END IF

mcID = typeID(ss%is)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL GetChemComp( mat_mc%ID(mcID),compName,ss )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'CheckSampMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetBySizeMat( icBin,imat,iclass )

USE scipuff_fi
USE sampler_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: icBin, imat, iclass

icBin = INDEX(smp(nsmp)%var,':')  !Check for colon after material name
IF( icBin > 0 )THEN
  matname = TRIM(smp(nsmp)%var(1:icBin-1))
ELSE
  matname = TRIM(smp(nsmp)%var)
END IF
isg = 0

CALL check_smp_matl( matname,isg,smp(nsmp)%is,smp(nsmp)%ie )
IF( nError /= NO_ERROR )GOTO 9999

imat   = typeID(smp(nsmp)%is)%imat
iclass = material(imat)%icls

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ParseBinSize( icBin,lWet,lFrac,dryFrac )

USE scipuff_fi
USE sampler_fi
USE reallocate

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: icBin
LOGICAL, INTENT( IN  ) :: lWet
LOGICAL, INTENT( OUT ) :: lFrac
REAL,    INTENT( OUT ) :: dryFrac

INTEGER ios, alloc_stat, i, jc, ii

CHARACTER(1028) string

IF( icBin == 0 )THEN             !Default is every bin for liquids, 1% dry fraction for wet particles

  lFrac = .TRUE.
  IF( lWet )THEN
    dryFrac = 0.01
  ELSE
    dryFrac = 0.0
  END IF

ELSE                         !Parse droplet bin diameters

  lFrac    = .FALSE.
  dryFrac  = 0.

  i = icBin + 1
  string = TRIM(smp(nsmp)%var(i:))

  IF( string(1:1) == '(' )THEN

    jc = INDEX(string,')')    !Points to close paren
    IF( jc == 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'ParseBinSize'
      eMessage = 'Error in size bin input'
      eInform  = 'Missing close parenthesis for bin diameter list'
      CALL SensorNumStr( nsmp,eAction )
      GOTO  9999
    END IF
    i   = 2         !Points to first character of first component
    isg = 0
    DO
      ii = INDEX(string(i:),',')
      IF( ii == 0 .OR. ii+i-1 > jc )ii = INDEX(string(i:),')')
      IF( ii < 2 )THEN
        nError   = UK_ERROR
        eRoutine = 'ParseBinSize'
        eMessage = 'Error reading size bin input'
        CALL SensorNumStr( nsmp,eInform )
        GOTO 9999
      END IF
      IF( isg == 0 )THEN
        ALLOCATE( smp(nsmp)%bin(1),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'ParseBinSize'
          eMessage = 'Error allocating first size bin'
          CALL SensorNumStr( nsmp,eInform )
          GOTO  9999
        END IF
      ELSE
        ios = reallocate_real1d( smp(nsmp)%bin,1 )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'ParseBinSize'
          eMessage = 'Error allocating size bins'
          CALL SensorNumStr( nsmp,eInform )
          GOTO 9999
        END IF
      END IF
      isg = isg + 1
      READ(string(i:i+ii-2),*,IOSTAT=ios) smp(nsmp)%bin(isg)
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'ParseBinSize'
        eMessage = 'Error reading size bin diameter'
        CALL SensorNumStr( nsmp,eInform )
        GOTO 9999
      END IF
      i = i + ii
      IF( i >= jc )EXIT
    END DO

    IF( isg <= 1 )THEN
      nError    = UK_ERROR
      eRoutine  = 'ParseBinSize'
      eMessage  = 'Invalid size bin input'
      eInform   = 'Minimum 2 bin boundaries required'
      eAction   = 'Input string: '//TRIM(smp(nsmp)%var)
      GOTO  9999
    END IF

    smp(nsmp)%bin(1:isg) = smp(nsmp)%bin(1:isg) * 1.E-6

  ELSE IF( lWet )THEN

    READ(string,*,IOSTAT=ios) dryFrac
    IF( ios /= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'ParseBinSize'
      eMessage = 'Error reading dry fraction for wet particle sensor'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999
    END IF
    IF( dryFrac <= 0. .OR. dryFrac >= 1. )THEN
      nError   = IV_ERROR
      eRoutine = 'ParseBinSize'
      eMessage = 'Invalid dry fraction for wet particle sensor'
      eInform  = 'Must be greater than zero, less than one'
      CALL SensorNumStr( nsmp,eAction )
      GOTO  9999
    END IF
    lFrac = .TRUE.

  ELSE  !No parentheses (as required for liquid droplet bins)

    nError   = IV_ERROR
    eRoutine = 'ParseBinSize'
    eMessage = 'Invalid input for liquid droplet bins'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetBinSize( lFrac,lWet,lLiq,dryFrac,imat )

USE scipuff_fi
USE sampler_fi
USE reallocate
USE UtilMtlAux

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lFrac
LOGICAL, INTENT( IN ) :: lWet
LOGICAL, INTENT( IN ) :: lLiq
REAL,    INTENT( IN ) :: dryFrac
INTEGER, INTENT( IN ) :: imat

INTEGER alloc_stat, igrp, i, jc, ityps, itype
REAL    rat

TYPE( part_material   ) :: pmatp
TYPE( liquid_material ) :: pmatliq

IF( lFrac )THEN

  IF( lWet )THEN

    smp(nsmp)%nbin = isg
    ALLOCATE( smp(nsmp)%bin(isg+1),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SetBinSize'
      eMessage = 'Error allocating wet particle bins'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999
    END IF

    igrp = typeID(smp(nsmp)%is)%igrp

    CALL GetParticleParam( pmatp,material(imat)%iaux,igrp,mat_aux )
    IF( material(imat)%AuxMatID /= NOT_SET_I )THEN
      i   = material(imat)%AuxMatID
      rat = pmatp%rho/WetPartLiquidMat(i)%rho
    ELSE
      rat = pmatp%rho/RHO_WATER
    END IF
    rat = (1.0+rat*(1.0/dryFrac-1.0))**0.3333333
    smp(nsmp)%bin(1) = rat*pmatp%dmin

    DO i = 1,isg
      igrp = typeID(smp(nsmp)%is+i-1)%igrp
      CALL GetParticleParam( pmatp,material(imat)%iaux,igrp,mat_aux )
      smp(nsmp)%bin(i+1) = rat*pmatp%dmax
    END DO

  ELSE

    IF( lLiq )THEN
      ityps = material(imat)%ioffp + 1
      itype = material(imat)%ioffp + 1 + MAX(1,GetSubgroups( material(imat),mat_aux ))
      smp(nsmp)%is = MAX(smp(nsmp)%is,ityps+1)       !Make sure to ignore vapor phase for liquids
      smp(nsmp)%ie = MIN(smp(nsmp)%ie,itype)         !Ignore aerosol phase
    END IF
    smp(nsmp)%nbin = smp(nsmp)%ie - smp(nsmp)%is + 1

    ALLOCATE( smp(nsmp)%bin(smp(nsmp)%nbin+1),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SetBySizeOutput'
      eMessage = 'Error allocating size bins'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999
    END IF

    IF( lLiq )THEN

      DO i = 1,smp(nsmp)%nbin
        igrp = typeID(smp(nsmp)%is+i-1)%igrp
        CALL GetLiquidParam( pmatliq,material(imat)%iaux,igrp,mat_aux )
        smp(nsmp)%bin(i) = pmatliq%dmin
      END DO
      smp(nsmp)%bin(smp(nsmp)%nbin+1) = 2.*pmatliq%dbar - pmatliq%dmin

    ELSE

      igrp = typeID(smp(nsmp)%is)%igrp
      CALL GetParticleParam( pmatp,material(imat)%iaux,igrp,mat_aux )
      smp(nsmp)%bin(1) = pmatp%dmin

      DO i = 1,smp(nsmp)%nbin
        igrp = typeID(smp(nsmp)%is+i-1)%igrp
        CALL GetParticleParam( pmatp,material(imat)%iaux,igrp,mat_aux )
        smp(nsmp)%bin(i+1) = pmatp%dmax
      END DO

    END IF

  END IF

ELSE

  smp(nsmp)%nbin = isg - 1

  IF( .NOT.(lWet .OR. lLiq) )THEN
    jc  = smp(nsmp)%is
    isg = smp(nsmp)%ie
    DO i = jc,isg
      igrp = typeID(i)%igrp
      CALL GetParticleParam( pmatp,material(imat)%iaux,igrp,mat_aux )
      IF( pmatp%dmin <= smp(nsmp)%bin(1)                )smp(nsmp)%is = i
      IF( pmatp%dmin <  smp(nsmp)%bin(smp(nsmp)%nbin+1) )smp(nsmp)%ie = i
    END DO
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE check_smp_matl( matname,isg,itys,itye )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: matname
INTEGER,      INTENT( IN  ) :: isg
INTEGER,      INTENT( OUT ) :: itys,itye

INTEGER imat, icls, i

CHARACTER(128) :: matstring

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsEvap

!------ Match material name

matstring = ADJUSTL(TRIM(matname))
CALL cupper( matstring )

imat = -1
DO i = 1,ntypm
  IF( TRIM(material(i)%cmat) == TRIM(matstring) )THEN
    imat = i; EXIT
  END IF
END DO

IF( imat == -1 )THEN
  nError   = UK_ERROR
  eRoutine = 'check_smp_matl'
  eMessage = 'Sensor material not found in materials list'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  eAction  = 'Make sure material is in materials list'
  GOTO  9999
END IF

icls = material(imat)%icls

!------ Set subgroups

IF( isg > 0  )THEN

  itys = material(imat)%ioffp + isg
  itye = itys

  IF( IsGas(icls) )THEN
    IF( isg /= 1 )THEN
      nError   = IV_ERROR
      eRoutine = 'check_smp_matl'
      eMessage = 'Invalid subgroup for sampler output'
      WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
      eAction  = 'Gas material subgroup must be 0 or 1'
      GOTO  9999
    END IF
  ELSE IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN
    IF( isg > GetSubgroups( material(imat),mat_aux ) )THEN
      nError   = IV_ERROR
      eRoutine = 'check_smp_matl'
      eMessage = 'Invalid subgroup for sampler output'
      WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
      eAction  = 'Particle material subgroup exceeds number of bins'
      GOTO  9999
    END IF
  ELSE IF( IsLiquid(icls) )THEN
    IF( isg == 2 )THEN
      itye = itys + GetSubgroups( material(imat),mat_aux ) - 1
    ELSE
      IF( isg /= 1 )THEN
        nError   = IV_ERROR
        eRoutine = 'check_smp_matl'
        eMessage = 'Invalid subgroup for liquid material sampler output'
        WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
        eAction  = 'Subgroup must be 1 (vapor) or 2 (liquid droplet)'
        GOTO  9999
      END IF
    END IF
  END IF

ELSE IF( isg == 0 )THEN

  itys = material(imat)%ioffp + 1
  itye = material(imat)%ioffp + MAX(1,GetSubgroups( material(imat),mat_aux ))
  IF( IsLiquid(icls) )itye = itye + 2  !Vappor & aerosol groups
ELSE

  nError   = IV_ERROR
  eRoutine = 'check_smp_matl'
  eMessage = 'Invalid subgroup for sampler output'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  eAction  = 'Subgroup must be 0 or positive'
  GOTO  9999

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE check_smp_dep( ss,imat )

USE scipuff_fi
USE surface_fi
USE sampler_fi
USE srfparam_fd
USE UtilMtlAux
USE reallocate
USE sagdef_fd

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss            !Sensor structure
INTEGER,        INTENT(   OUT ) :: imat          !material number

CHARACTER(64) BlockName, bname, matstring
CHARACTER(70) vname  ! block name:field name

INTEGER nch, icls, i, j, idep, iv, alloc_stat, ios, irv
LOGICAL lcum, lsss

INTEGER, EXTERNAL :: SAG_FindVariableID
INTEGER, EXTERNAL :: reallocate_depblk_smp
INTEGER, EXTERNAL :: output_groups
LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle, IsEvap

lcum = .FALSE.
lsss = .FALSE.

!------ Check material

i = INDEX(ss%var,':')
IF( i > 0 )THEN
  matname = ss%var(1:i-1)
  nch     = LEN_TRIM(ss%var)
  READ(ss%var(i+1:nch),FMT='(A)',IOSTAT=ios) matstring
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Error reading material subgroup string'
    WRITE(eInform,'(A)')'Material name is '//TRIM(matname)
    GOTO  9999
  END IF
  nch = LEN_TRIM(matstring)
  CALL cupper(matstring)
  i = INDEX(matstring,'C')   !'C' indicates cumulative mass / area
  IF( i > 0 )THEN
    lcum = .TRUE.
    nch = i-1
  ELSE
    i = INDEX(matstring,'S') !'S' indicates current "surface" mass / area
    IF( i > 0 )THEN
      lsss = .TRUE.
      nch = i-1
    END IF
  END IF
  READ(matstring(1:nch),FMT=*,IOSTAT=ios) isg
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Error reading material subgroup number'
    WRITE(eInform,'(A)')'Material name is '//TRIM(matname)
    GOTO  9999
  END IF
ELSE
  matname = TRIM(ss%var)
  isg     = 0
END IF

matstring = ADJUSTL(TRIM(matname))
CALL cupper( matstring )

imat = -1
DO i = 1,ntypm
  IF( TRIM(material(i)%cmat) == TRIM(matstring) )THEN
    imat = i; EXIT
  END IF
END DO

IF( imat == -1 )THEN
  nError   = UK_ERROR
  eRoutine = 'check_smp_dep'
  eMessage = 'Sensor material not found in materials list'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  eAction  = 'Make sure material is in materials list'
  GOTO  9999
END IF

icls = material(imat)%icls

!------ Set subgroups

IF( IsGas(icls) )THEN
  isg = 1

ELSE IF( isg > 0  )THEN

  IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN
    IF( isg > GetSubgroups( material(imat),mat_aux ) )THEN
      nError   = IV_ERROR
      eRoutine = 'check_smp_dep'
      eMessage = 'Invalid subgroup for sampler output'
      WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
      eAction  = 'Particle material subgroup exceeds number of bins'
      GOTO  9999
    END IF
  ELSE IF( IsLiquid(icls) )THEN
    IF( isg > 2 )THEN
      nError   = IV_ERROR
      eRoutine = 'check_smp_dep'
      eMessage = 'Invalid subgroup for liquid material sampler output'
      WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
      eAction  = 'Subgroup must be 1 (vapor) or 2 (liquid droplet)'
      GOTO  9999
    END IF
  END IF

ELSE IF( isg == 0 .AND. IsParticle(icls) )THEN

  IF( output_groups(material(imat)) == 1 )isg = 1

ELSE IF( isg < 0 )THEN

  nError   = IV_ERROR
  eRoutine = 'check_smp_dep'
  eMessage = 'Invalid subgroup for sampler output'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  eAction  = 'Subgroup must be 0 or positive'
  GOTO  9999

END IF

!------ Check if this is a deposition material

IF( .NOT.(material(imat)%lsrft .OR. material(imat)%lsrfg) )THEN
  nError   = IV_ERROR
  eRoutine = 'check_smp_dep'
  eMessage = 'No deposition for this material'
  WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
  GOTO  9999
END IF

!------ Check for cumulative liquid sensor (only required with secondary evaporation)

lcum = lcum .AND. IsEvap(icls)
IF( lcum )THEN
  IF( .NOT.(material(imat)%lsrft .AND. material(imat)%lsrfg) )THEN
    nError   = IV_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Cumulative liquid deposition samplers require total and group deposition'
    WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
    GOTO  9999
  END IF
  isg = 0 !Total deposition
END IF

lsss = lsss .AND. IsEvap(icls) .AND. substrate_type /= 0
IF( lsss )THEN
  IF( .NOT.material(imat)%lsrfg )THEN
    nError   = IV_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Current surface liquid deposition samplers require group deposition'
    WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
    GOTO  9999
  END IF
END IF


!------ Find deposition block

CALL set_block_name( material(imat),isg,bname )

idep     = 0

DepBlocks : DO j = 1,ndep_blocks  !Loop through dep blocks only
  BlockName = TRIM( ADJUSTL( srf_block(j)%name ))
  IF( bname == BlockName )THEN  !Match block name
      idep = j; EXIT DepBlocks
  END IF
END DO DepBlocks

!------ Setup for deposition fields

IF( idep > 0 )THEN

  j = 0
  IF( ALLOCATED(depblk_smp) )THEN
    DO i = 1,ndepblk_smp
      IF( depblk_smp(i)%iblk == idep )THEN
        j = i;
        nsmp_dep(i) = nsmp_dep(i) + 1
        EXIT
      END IF
    END DO
  END IF

  IF( j == 0 )THEN

    irv = reallocate_depblk_smp() !Increments ndepblk_smp
    IF( irv == 0 )THEN
      j = ndepblk_smp
      IF( j == 1 )THEN
        ALLOCATE( nsmp_dep(1),STAT=irv )
      ELSE
        irv = reallocate_integer1d( nsmp_dep,1 )
      END IF
    END IF
    IF( irv /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dep'
      eMessage = 'Error allocating arrays for deposition sensor'
      GOTO 9999
    END IF

    nsmp_dep(j) = 1

    depblk_smp(j)%iblk = idep

    depblk_smp(j)%nfld = 2

    ALLOCATE( depblk_smp(j)%ifld(2),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dep'
      eMessage = 'Error allocating arrays for deposition sensor'
      WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
      GOTO  9999
    END IF

    bname = TRIM( ADJUSTL( srf_block(idep)%name ))
    vname = TRIM(bname)//':'//'Mean'

    ios = SAG_FindVariableID( srfdep,vname,iv )
    IF( ios /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dep'
      eMessage = 'Error finding field name '//TRIM(vname)
      GOTO 9999
    END IF

    depblk_smp(j)%ifld(1:2) = (/iv,iv+1/)

  END IF

  ss%is = j
  ss%ie = 0  !Otherwise, non-zero ss%ie will point to vapor block (below)

ELSE

  nError   = IV_ERROR
  eRoutine = 'check_smp_dep'
  eMessage = 'No deposition block found for this material'
  WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
  GOTO  9999

END IF

!------ Check if surface mass variable is accounted for

IF( lsss .AND. depblk_smp(j)%nfld == 2 )THEN

  irv = reallocate_integer1d( depblk_smp(j)%ifld,1 )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Error allocating arrays for deposition sensor'
    GOTO 9999
  END IF

  depblk_smp(j)%nfld = depblk_smp(j)%nfld + 1

  bname = TRIM( ADJUSTL( srf_block(depblk_smp(j)%iblk)%name ))
  vname = TRIM(bname)//':'//'Sfcm'
  ios = SAG_FindVariableID( srfdep,vname,iv )
  IF( ios /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'Error finding field name '//TRIM(vname)
    GOTO 9999
  END IF

  depblk_smp(j)%ifld(3) = iv

END IF

!------ Find total deposition block for cumulative liquid sensor

IF( lcum )THEN

  CALL set_block_name( material(imat),1,bname )  !subgroup = 1 for vapor (to subtract from total)
  DO j = 1,ndep_blocks
    BlockName = TRIM( ADJUSTL( srf_block(j)%name ))
    IF( bname == BlockName )THEN
        idep = j; EXIT
    END IF
  END DO

  IF( idep > 0 )THEN

    j = 0
    DO i = 1,ndepblk_smp
      IF( depblk_smp(i)%iblk == idep )THEN
        j = i;
        nsmp_dep(i) = nsmp_dep(i) + 1
        EXIT
      END IF
    END DO

    IF( j == 0 )THEN
      irv = reallocate_depblk_smp() !Increments ndepblk_smp
      IF( irv == 0 )irv = reallocate_integer1d( nsmp_dep,1 )
      IF( irv /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'check_smp_dep'
        eMessage = 'Error allocating arrays for deposition sensor'
        GOTO 9999
      END IF

      j = ndepblk_smp
      nsmp_dep(j) = 1

      depblk_smp(j)%iblk = idep
      depblk_smp(j)%grdI = 0

      ALLOCATE( depblk_smp(j)%ifld(2),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'check_smp_dep'
        eMessage = 'Error allocating arrays for deposition sensor'
        WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
        GOTO  9999
      END IF

      bname = TRIM( ADJUSTL( srf_block(idep)%name ))
      vname = TRIM(bname)//':'//'Mean'

      ios = SAG_FindVariableID( srfdep,vname,iv )
      IF( ios /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'check_smp_dep'
        eMessage = 'Error finding field name '//TRIM(vname)
        GOTO 9999
      END IF

      depblk_smp(j)%nfld      = 2
      depblk_smp(j)%ifld(1:2) = (/iv,iv+1/)

    END IF

    ss%ie = j        !ss%ie points to vapor block; ss%is points to total block

  ELSE

    nError   = IV_ERROR
    eRoutine = 'check_smp_dep'
    eMessage = 'No vapor deposition block found for cumulative liquid sensor'
    WRITE(eInform,'(A)')'Deposition sampler material requested was '//TRIM(matname)
    GOTO  9999

  END IF

ELSE IF( lsss )THEN

  ss%ie = -1        !Indicates extra field for liquid on surface

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE check_smp_dos( ss )

!------ Checks for integrated concentration (CONC:INT) sensor with gridded output
!       N.B. Assumes matname and isg have already been validated

USE scipuff_fi
USE surface_fi
USE sampler_fi
USE srfparam_fd
USE UtilMtlAux
USE reallocate
USE sagdef_fd

IMPLICIT NONE

TYPE( sensor), INTENT( INOUT ) :: ss            !Sensor structure

CHARACTER(64) BlockName, bname, matstring
CHARACTER(70) vname  ! block name:field name

INTEGER imat, i, j, idos, iv, alloc_stat, ios, irv

INTEGER, EXTERNAL :: SAG_FindVariableID
INTEGER, EXTERNAL :: reallocate_dosblk_smp
INTEGER, EXTERNAL :: output_groups
LOGICAL, EXTERNAL :: IsGas, IsParticle

matstring = ADJUSTL(TRIM(matname))  !N.B. matname and isg are in sampler_fi
CALL cupper( matstring )

imat = -1
DO i = 1,ntypm
  IF( TRIM(material(i)%cmat) == TRIM(matstring) )THEN
    imat = i; EXIT
  END IF
END DO

IF( imat == -1 )THEN  !Should never happen
  nError   = UK_ERROR
  eRoutine = 'check_smp_dos'
  eMessage = 'Sensor material not found in materials list'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  eAction  = 'Make sure material is in materials list'
  GOTO  9999
END IF

IF( IsParticle(material(imat)%icls) )THEN
  IF( isg == 0 .AND. output_groups(material(imat)) == 1 )isg = 1
ELSE IF( IsGas(material(imat)%icls) )THEN
  isg = 1
END IF

!------ Check if this is a dosage material

IF( (isg == 0 .AND. .NOT.material(imat)%ldost) .OR. &
    (isg  > 0 .AND. .NOT.material(imat)%ldosg) )THEN
  nError   = IV_ERROR
  eRoutine = 'check_smp_dos'
  eMessage = 'Dosage not saved for this material/subgroup'
  eInform = 'Saved dosage required with Increment mode or DOS sensor'
  WRITE(eAction,'(A)')'Integrated sensor material requested was '//TRIM(matname)
  GOTO  9999
END IF

!------ Find dosage block

CALL set_block_name( material(imat),isg,bname )

idos     = 0

DosBlocks : DO j = ndep_blocks+1,nsrf_blocks  !Loop through dos blocks only
  BlockName = TRIM( ADJUSTL( srf_block(j)%name ))
  IF( bname == BlockName )THEN  !Match block name
      idos = j; EXIT DosBlocks
  END IF
END DO DosBlocks

!------ Setup for dosage fields

IF( idos > 0 )THEN

  j = 0
  IF( ALLOCATED(dosblk_smp) )THEN
    DO i = 1,ndosblk_smp
      IF( dosblk_smp(i)%iblk == idos )THEN
        j = i;
        nsmp_dos(i) = nsmp_dos(i) + 1
        EXIT
      END IF
    END DO
  END IF

  IF( j == 0 )THEN

    irv = reallocate_dosblk_smp() !Increments ndosblk_smp
    IF( irv == 0 )THEN
      j = ndosblk_smp
      IF( j == 1 )THEN
        ALLOCATE( nsmp_dos(1),STAT=irv )
      ELSE
        irv = reallocate_integer1d( nsmp_dos,1 )
      END IF
    END IF
    IF( irv /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dos'
      eMessage = 'Error allocating arrays for integrated sensor'
      WRITE(eInform,'(A)')'Saved dosage required with Increment mode or DOS sensor'
      GOTO 9999
    END IF

    nsmp_dos(j) = 1

    dosblk_smp(j)%iblk = idos
    dosblk_smp(j)%grdI = 0
    dosblk_smp(j)%nfld = 2

    ALLOCATE( dosblk_smp(j)%ifld(2),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dos'
      eMessage = 'Error allocating arrays for integrated sensor'
      eInform  = 'Saved dosage required with Increment mode or DOS sensor'
      GOTO  9999
    END IF

    bname = TRIM( ADJUSTL( srf_block(idos)%name ))
    vname = TRIM(bname)//':'//'Mean'

    ios = SAG_FindVariableID( srfdos,vname,iv )
    IF( ios /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'check_smp_dos'
      eMessage = 'Error finding field name '//TRIM(vname)
      eInform = 'Saved dosage required with Increment mode or DOS sensor'
      GOTO 9999
    END IF

    dosblk_smp(j)%ifld(1:2) = (/iv,iv+1/)

  END IF

  ss%is = j
  ss%ie = 0

ELSE

  nError   = IV_ERROR
  eRoutine = 'check_smp_dos'
  eMessage = 'No dosage block found for integrated sensor'
  eAction  = 'Save dosage required with Increment mode or DOS sensor'
  WRITE(eInform,'(A)')'Material requested was '//TRIM(matname)
  GOTO  9999

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION reallocate_depblk_smp()

USE sampler_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, n

TYPE( srfblk_smp_fd ), DIMENSION(:), ALLOCATABLE :: depblk_tmp

reallocate_depblk_smp = 1 !Initialize to failure

IF( ndepblk_smp == 0 )THEN

  ALLOCATE( depblk_smp(ndepblk_smp+1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
  NULLIFY( depblk_smp(1)%ifld )

ELSE

  ALLOCATE( depblk_tmp(ndepblk_smp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  DO i = 1,ndepblk_smp
    depblk_tmp(i)%iblk = depblk_smp(i)%iblk
    depblk_tmp(i)%grdI = depblk_smp(i)%grdI
    n = depblk_smp(i)%nfld; depblk_tmp(i)%nfld = n
    NULLIFY( depblk_tmp(i)%ifld )
    IF( n > 0 )THEN
      ALLOCATE( depblk_tmp(i)%ifld(n),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,n
        depblk_tmp(i)%ifld(j) = depblk_smp(i)%ifld(j)
      END DO
      DEALLOCATE( depblk_smp(i)%ifld,STAT=alloc_stat )
    END IF
  END DO

  DEALLOCATE( depblk_smp,STAT=alloc_stat )

  ALLOCATE( depblk_smp(ndepblk_smp+1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  DO i = 1,ndepblk_smp
    depblk_smp(i)%iblk = depblk_tmp(i)%iblk
    depblk_smp(i)%grdI = depblk_tmp(i)%grdI
    n = depblk_tmp(i)%nfld; depblk_smp(i)%nfld = n
    NULLIFY( depblk_smp(i)%ifld )
    IF( n > 0 )THEN
      ALLOCATE( depblk_smp(i)%ifld(n),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,n
        depblk_smp(i)%ifld(j) = depblk_tmp(i)%ifld(j)
      END DO
      DEALLOCATE( depblk_tmp(i)%ifld,STAT=alloc_stat )
    END IF
  END DO

END IF

ndepblk_smp = ndepblk_smp + 1

reallocate_depblk_smp = 0

9999 CONTINUE

IF( ALLOCATED(depblk_tmp) )DEALLOCATE( depblk_tmp,STAT=alloc_stat )

RETURN

END FUNCTION reallocate_depblk_smp

!==============================================================================

INTEGER FUNCTION reallocate_dosblk_smp()

USE sampler_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, n

TYPE( srfblk_smp_fd ), DIMENSION(:), ALLOCATABLE :: dosblk_tmp

reallocate_dosblk_smp = 1 !Initialize to failure

IF( ndosblk_smp == 0 )THEN

  ALLOCATE( dosblk_smp(ndosblk_smp+1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
  NULLIFY( dosblk_smp(1)%ifld )

ELSE

  ALLOCATE( dosblk_tmp(ndosblk_smp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  DO i = 1,ndosblk_smp
    dosblk_tmp(i)%iblk = dosblk_smp(i)%iblk
    dosblk_tmp(i)%grdI = dosblk_smp(i)%grdI
    n = dosblk_smp(i)%nfld; dosblk_tmp(i)%nfld = n
    NULLIFY( dosblk_tmp(i)%ifld )
    IF( n > 0 )THEN
      ALLOCATE( dosblk_tmp(i)%ifld(n),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,n
        dosblk_tmp(i)%ifld(j) = dosblk_smp(i)%ifld(j)
      END DO
      DEALLOCATE( dosblk_smp(i)%ifld,STAT=alloc_stat )
    END IF
  END DO

  DEALLOCATE( dosblk_smp,STAT=alloc_stat )

  ALLOCATE( dosblk_smp(ndosblk_smp+1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  DO i = 1,ndosblk_smp
    dosblk_smp(i)%iblk = dosblk_tmp(i)%iblk
    dosblk_smp(i)%grdI = dosblk_tmp(i)%grdI
    n = dosblk_tmp(i)%nfld; dosblk_smp(i)%nfld = n
    NULLIFY( dosblk_smp(i)%ifld )
    IF( n > 0 )THEN
      ALLOCATE( dosblk_smp(i)%ifld(n),STAT=alloc_stat )
      IF( alloc_stat /= 0 )GOTO 9999
      DO j = 1,n
        dosblk_smp(i)%ifld(j) = dosblk_tmp(i)%ifld(j)
      END DO
      DEALLOCATE( dosblk_tmp(i)%ifld,STAT=alloc_stat )
    END IF
  END DO

END IF

ndosblk_smp = ndosblk_smp + 1

reallocate_dosblk_smp = 0

9999 CONTINUE

IF( ALLOCATED(dosblk_tmp) )DEALLOCATE( dosblk_tmp,STAT=alloc_stat )

RETURN

END FUNCTION reallocate_dosblk_smp


