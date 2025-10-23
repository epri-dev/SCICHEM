SUBROUTINE read_smp()

!------ Read sampler input file; setup output file

USE scipuff_fi
USE sampler_fi
USE files_fi
USE UtilMtlAux
USE SamplerUtil
USE SamplerGridOuput

IMPLICIT NONE

CHARACTER(128) string

INTEGER ios, is, i, j, nback
LOGICAL lexist
REAL    tol, ts, dum
INTEGER, EXTERNAL :: time_level, SAG_SetSpecialValue

!------ Check if sampler location file is specified

nsmp = 0
IF( LEN_TRIM(smpfile) == 0 )THEN
  lsmp = .FALSE.
  GOTO 9999
ELSE
  lsmp = .TRUE.
END IF

!------ Set max sampler time level

IF( dt_smp == NOT_SET_R )THEN
  nError   = IV_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Must set a value for dt_smp'
  eInform  = 'dt_smp=NOT_SET is invalid'
  GOTO  9999
ELSE IF( dt_smp == DEF_VAL_R )THEN
  mxlev_smp = 0
ELSE
  dt_smp    = MAX(dt_smp,delt/(2.0**MAXTLV))
  mxlev_smp = time_level( dt_smp )
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'read_smp'
    GOTO 9999
  END IF
END IF

!------ Initialize sampler time and no. of variables

t_smp     = 0.
nvarsmp   = 0
sampMap   = lmap
SampTitle = ' '

lCartOrig = .FALSE.

ndosblk_smp = 0
ndepblk_smp = 0

NULLIFY( nsmp_dep )
NULLIFY( nsmp_dos )

IF( lun_smp < 0 )GOTO 9999

UTMfac = 1.
!------ Read input file

CALL ReadSamplerInput()
IF( nError /= NO_ERROR )GOTO 9999

!------ Set flag for integrated sampler output

lReadSum = .FALSE.
IF( lAvg )lReadSum = .TRUE.
IF( lGridOut )THEN
  lReadSum = .FALSE.
ELSE IF( .NOT.lReadSum )THEN
  DO j = 1,nsmp
    IF( BTEST(smp(j)%stype,STB_INTEGRATE) )THEN
      lReadSum   = .TRUE.; EXIT
    END IF
  END DO
END IF
lUpdateSum = .FALSE.

int_sensor = lReadSum

!------ Create output file if it does not exist

INQUIRE( FILE=file_smp,EXIST=lexist )
IF( .NOT.lexist )THEN
  IF( LEN_TRIM(SampTitle) > 1 )THEN
    string = TRIM(SampTitle)
  ELSE
    string = 'Sensor Output'
  END IF
  CALL create_smp( string )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Output file

IF( lBinOut )THEN

  IF( t <= tStartSamp )THEN

    OPEN( UNIT=lun_sps,FILE=TRIM(file_sps),STATUS='REPLACE',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4,IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'read_smp'
      eMessage = 'Error opening binary sampler file'
      CALL ReportFileName( eInform,'File=',file_sps )
      GOTO 9999
    END IF

  ELSE  !Restart

    OPEN( UNIT=lun_sps,FILE=TRIM(file_sps),STATUS='OLD',ACTION='READWRITE',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4,IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'read_smp'
      eMessage = 'Error opening binary sampler file for restart'
      CALL ReportFileName( eInform,'File=',file_sps )
      GOTO 9999
    END IF

  END IF

ELSE

  OPEN( UNIT=lun_smp,FILE=file_smp,STATUS='OLD',ACTION='READWRITE',POSITION='APPEND',IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error opening sampler output file'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF
  IF( multicomp .AND. nmc0 > 0 )THEN
    IF( .NOT. sampamb )THEN
      nError   = OP_ERROR
      eRoutine = 'read_smp'
      eMessage = 'No MC type sampler specified'
      eInform  = 'Must specify at least one MC type sampler for multi-component project'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
    OPEN( UNIT=lun_asmp,FILE=file_asmp,STATUS='OLD',ACTION='READWRITE',POSITION='APPEND',IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'read_smp'
      eMessage = 'Error opening sampler ambient output file'
      CALL ReportFileName( eInform,'File=',file_asmp )
      GOTO 9999
    END IF
  END IF

END IF

!------ Setup gridded output field descriptors

IF( lGridOut )THEN

  CALL SamplerGridFieldDescriptors()
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!------ Setup and position files:
!       Make sure only header record is on file if current time is before sampler start time

IF( t <= tStartSamp )THEN

  CALL SkipSamplerHeader()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Otherwise, read last time on output file and make sure it's not past current time
!       N.B. Tolerance based on magnitude and format of time written to file
ELSE

  CALL ReadLastSamplerTime( nback )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( t > tEndSamp )GOTO 9999

  tol = 10.**FLOOR(LOG10(t)) !Nearest power of 10, i.e., t = r*tol where tol = 10^n
  tol = 0.9*tol * 1.E-6  !Six decimal points
  !tol = 0.9*tol * 1.E-4  !Four decimal points - use in non-AFTAC code someday

  IF( lSmpOutList )THEN
    CALL SetSamplerListTime()
    IF( dtSmpOut > 0. )THEN
      tol = tol + dtSmpOut
    ELSE
      tol = tol + (tNextSamp-tStartSamp)
    END IF
  END IF

  IF( t > t_smp + tol )THEN !Error if t is beyond last sampler time
    nError   = OP_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Sampler output time is behind project time'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  CALL PositionSampler( nback,tol)
  IF( nError /= NO_ERROR )GOTO 9999

!------ Read current sum for integrated output

  IF( lReadSum )THEN
    CALL ReadSamplerSum( nback )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
!------ Check status of sensor time-averaging or output times

  IF( lAvg .OR. lSmpOut )THEN

!------ Find current time from list

    IF( lSmpOutList )CALL SetSamplerListTime()

!------ Position to overwrite last record if not a standard output time

    IF( .NOT.(lGridOut .OR. lBinOut) )THEN

      CALL BackSpaceSampler( nback,ts,ios )
      IF( ios /= NO_ERROR )ts = tStartSamp

      IF( ABS((t_smp-ts) - dtSmpOut) < tol .OR. (lavg .AND. lSmpOutList .AND. t_smp < tStartSamp) )THEN
        READ(lun_smp,*,IOSTAT=ios) ts,((dum,i=1,smp(is)%nvar),is=1,nsmp)
        IF( multicomp .AND. smp(1)%nmc > 0 )READ(lun_asmp,*,IOSTAT=ios) ts,((dum,i=1,3+smp(is)%nmc),is=1,nsmp)  !Assume all samplers have the same "nmc"
        DO is = 1,nsmp
          IF( BTEST(smp(is)%stype,STB_INTEGRATE) )THEN
            smp(is)%dsmp(1) = smp(is)%dsmp(1) / smp(is)%conv
            smp(is)%dsmp(2) = smp(is)%dsmp(2) / smp(is)%conv**2
          ELSE
            smp(is)%dsmp = 0.
            smp(is)%csum = 0.
          END IF
        END DO

      ELSE  !Undo output processing conversion factors and time-averaging for continued integration

        DO i = 1,nsmp
          CALL UnProcessSensorX( smp(i),smp(i)%dsmp )
        END DO

      END IF

    END IF

!------ Set next output time

    tol = dtSmpOut * 1.E-3
    tSmpOut = tStartSamp
    IF( dtSmpOut > 0. )THEN
      DO
        tSmpOut = tSmpOut + dtSmpOut
        IF( tSmpOut > t+tol )EXIT
      END DO
    END IF

!------ Check against start of next averaging period

    IF( lSmpOutList )THEN
      IF( ASSOCIATED(SampTimeList%next) )THEN
        IF( tSmpOut > SampTimeList%next%tStart + tol .OR. dtSmpOut <= 0. ) THEN
          SampTimeList => SampTimeList%next
          IF( lAvg )THEN
            tStartSamp = SampTimeList%tStart
            dtSmpOut   = SampTimeList%dtSamp
            tSmpOut    = tStartSamp + dtSmpOut
          ELSE
            tSmpOut = SampTimeList%tStart
          END IF
        END IF
      END IF
    END IF

  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE update_smp( SmpOutType )

!------ Update sensor sensor output; write to file if appropriate

USE scipuff_fi
USE sampler_fi
USE field_fd
USE sciprime_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: SmpOutType

INTEGER i, is, irv, ig, jg
INTEGER nv, i0, j
REAL    tfac, dxx, dyy, s, c, xmap, ymap, xp, yp
REAL    tol, ts
LOGICAL lGrid, lTimeScaleNot, lLargeDelT, lFirst, lExit
LOGICAL lerr
TYPE( sensor ), POINTER :: ss

REAL,    EXTERNAL :: sind, cosd
INTEGER, EXTERNAL :: SAG_RmvGrdStr, SAG_SetSpecialValue

!------ Don't do anything if before sensor start time

IF( t < tStartSamp )THEN
  t_smp = t
  GOTO 9998
END IF

!------ Don't do anything if after end of sensor output

IF( t > tEndSamp )GOTO 9998

!------ Set max vertical puff level

zi_smp = 0.
DO i = 1,npuf
  IF( puff(i)%szz > delz2 )zi_smp = MAX(zi_smp,puff(i)%zc,1.1*puff(i)%zbar)
END DO

lLargeDelT = BTEST(SmpOutType,SOB_LARGEDELT) .OR. &
             (lSmpOut .AND. .NOT.BTEST(SmpOutType,SOB_INTONLY)) !To force actions on large timesteps or
                                                                !user-specified output times

!------ Loop until output time is greater than current time

lFirst = .TRUE.

TimeLoop : DO

  IF( lAvg .OR. lSmpOut )THEN
    IF( dtSmpOut > 0. )THEN
      tol = 0.001*MIN(t-t_smp,dtSmpOut)
    ELSE
      tol = MAX(SPACING(t),tolSmpOut)
    END IF
    IF( ABS(tSmpOut-t) < tol )THEN  !t is at an "exact" output time
      lExit     = .TRUE.
      lOutToSmp = .TRUE.
      ts        = t
    ELSE
      ts        = MIN(t,tSmpOut)      !If t>tSmpOut, need to compute average / output up to tSmpOut and
      lExit     = t < tSmpOut         !continue until tSmpOut >= t
      lOutToSmp = .NOT.lExit
    END IF
  ELSE
    ts = t
    lExit = .TRUE.
  END IF

  IF( lFirst )THEN

!------ Copy and push deposition fields to bottom cells

    IF( ndepblk_smp > 0 .AND. lLargeDelT )THEN
      i = SAG_SetSpecialValue( .TRUE.,HP_SPV )
      CALL PushSampSrfDep()
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

!------ Copy and push dosage fields to bottom cells

    IF( ndosblk_smp > 0 .AND. lLargeDelT )THEN
      i = SAG_SetSpecialValue( .TRUE.,HP_SPV )
      CALL PushSampSrfDos()
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

!------ Generate grids for auto grid sensors (lSmpOut must be true)

    DO is = 1,nsmp
      IF( BTEST(smp(is)%stype,STB_AUTOGRID) )CALL SensorAutoGrid( smp(is) )
    END DO


    lUpdateSum = .TRUE.

  END IF  !If lFirst

  lerr = .FALSE.

!------ Loop over samplers

!$OMP PARALLEL DEFAULT (SHARED)  IF (nsmp > 3)
!$OMP DO PRIVATE(is,ss,tfac,tol,lGrid,s,c,xmap,ymap,irv,jg,ig,dyy,dxx,xp,yp,lTimeScaleNot,nv,j,i0) REDUCTION(.OR.:lerr)
Samplers : DO is = 1,nsmp
    IF( lerr )CYCLE Samplers
    ss => smp(is)

!------ Check for time-integration; zero sampler arrays as appropriate

    IF( BTEST(ss%stype,STB_INTEGRATE) .OR. (lAvg .AND. dtSmpOut > 0.) )THEN
      tfac = ts - MAX(t_smp,tStartSamp)
      tfac = MAX(tfac,0.)
    ELSE
      IF( BTEST(SmpOutType,SOB_INTONLY) )THEN
        tol = 0.001*(t-t_smp)
        IF( .NOT.(lAvg .AND. t > tSmpOut-tol) )CYCLE Samplers
      END IF
      tfac = 1.
      IF( .NOT.BTEST(ss%stype,STB_DEP) )ss%dsmp = 0.
      IF( multicomp .AND. ss%nmc > 0 )ss%asmp = 0.
    END IF

!------ Update moving sensor location

    IF( BTEST(ss%stype,STB_MOVING) )THEN
      CALL update_sensor_location( smp(is),ts/3600. )
      IF( BTEST(ss%stype,STB_DEP) )THEN
        lerr = lerr .OR. ss%zh /= 0.
        IF( lerr )CYCLE Samplers
      END IF
      IF( ss%z < ss%h )CYCLE Samplers
    ELSE
      ss%time = ts/3600.
    END IF

!------ Set grid flag

    IF( BTEST(ss%stype,STB_FXGRID) .OR. BTEST(ss%stype,STB_AUTOGRID) )THEN

      IF( (BTEST(ss%stype,STB_DEP) .OR. BTEST(ss%stype,STB_INTGRID)) .AND. &
          .NOT.lLargeDelT  )CYCLE Samplers
      lGrid = .TRUE.

    ELSE

      lGrid = .FALSE.

    END IF

!------ Check for auto grid sensors; define axes rotation

    IF( BTEST(ss%stype,STB_AUTOGRID) )THEN
      IF( ss%nx == 0 )CYCLE Samplers

      CALL mapfac( ss%x,ss%y,xmap,ymap )
      s = sind( ss%rot ); c = cosd( ss%rot )

    END IF


!------ Loop over grid points (or single point for standard sensors)

    GridLoop : DO jg = 1,ss%ny
      dyy = FLOAT(jg-1)*ss%dys

      DO ig = 1,ss%nx
        dxx = FLOAT(ig-1)*ss%dxs

        IF( BTEST(ss%stype,STB_AUTOGRID) )THEN
          xp = ss%x + (c*dxx-s*dyy)*xmap
          yp = ss%y + (s*dxx+c*dyy)*ymap
        ELSE
          xp = ss%x + dxx
          yp = ss%y + dyy
        END IF

!------ Get LOS

        IF( BTEST(ss%stype,STB_LOS) )THEN

          IF( .NOT.BTEST(SmpOutType,SOB_NOPUFF) )THEN
            CALL get_samp_los( smp(is),xp,yp,tfac )
            lerr = lerr .OR. nError /= NO_ERROR
            IF( lerr )CYCLE Samplers
          END IF

!------ Get met

        ELSE IF( BTEST(ss%stype,STB_MET) )THEN

          CALL get_samp_met( smp(is),xp,yp,tfac )
          lerr = lerr .OR. nError /= NO_ERROR
          IF( lerr )CYCLE Samplers

!------ Get deposition

        ELSE IF( BTEST(ss%stype,STB_DEP) )THEN

          IF( lLargeDelT )THEN
            CALL get_samp_srfdep( smp(is),xp,yp )
            lerr = lerr .OR. nError /= NO_ERROR
            IF( lerr )CYCLE Samplers
          ELSE
            CYCLE Samplers
          END IF

!------ Get integrated gridded sensor

        ELSE IF( BTEST(ss%stype,STB_INTGRID) .OR. &
                (BTEST(ss%stype,STB_INTEGRATE) .AND. BTEST(run_mode,DINCRMNT)) )THEN

          IF( lLargeDelT )THEN
            CALL get_samp_srfdos( smp(is),xp,yp )
            lerr = lerr .OR. nError /= NO_ERROR
            IF( lerr )CYCLE Samplers
          ELSE
            CYCLE Samplers
          END IF

        ELSE

!------ Get point concentration from summation over puffs

          IF( .NOT.BTEST(SmpOutType,SOB_NOPUFF) )THEN

            IF( ALLOCATED(concPrime) )THEN
              smp(is)%dsmp(1) = smp(is)%dsmp(1) +  concPrime(is)*tfac
              smp(is)%dsmp(2) = smp(is)%dsmp(2) +  concPrime(is)**2*tfac
            END IF

            CALL get_samp_val( smp(is),xp,yp,tfac )
            lerr = lerr .OR. nError /= NO_ERROR
            IF( lerr )CYCLE Samplers

          END IF

        END IF

!------ Set time scale for standard sensors (assumed to be third variable)

        lTimeScaleNot = BTEST(ss%stype,STB_INTEGRATE) .OR. BTEST(ss%stype,STB_LOS)  .OR. &
                        BTEST(ss%stype,STB_MET)       .OR. BTEST(ss%stype,STB_PART) .OR. &
                        BTEST(ss%stype,STB_DEP)
        IF( .NOT.lTimeScaleNot )THEN
          IF( .NOT.lAvg )ss%dsmp(3) = ss%dsmp(3) / MAX(ss%dsmp(2),TINY(0.))
          IF( BTEST(ss%stype,STB_CORR) )ss%dsmp(4:5) = ss%dsmp(4:5) / MAX(ss%dsmp(2),TINY(0.))
        ELSE IF( BTEST(ss%stype,STB_PART) )THEN
          IF( .NOT.(lAvg .OR. BTEST(ss%stype,STB_INTEGRATE)) )THEN
            IF( BTEST(ss%stype,STB_LOS) )THEN
              nv = 3
            ELSE
              nv = 4
            END IF
            DO j = 1,ss%nbin
              i0 = (j-1)*nv
              ss%dsmp(i0+3) = ss%dsmp(i0+3) / MAX(ss%dsmp(i0+2),TINY(0.))
            END DO
          END IF
        END IF

        IF( lGrid )ss%gsmp(ig,jg,:) = ss%dsmp

      END DO
    END DO GridLoop

  END DO Samplers
!$OMP END DO
!$OMP END PARALLEL
  IF( lerr )THEN
    lerr = .FALSE.
    nError   = UK_ERROR
    eRoutine = 'update_smp'
    eMessage = 'Sampler error'
    eInform  = 'Exact nature may be unknown in multi-processor environment'
    GOTO 9999
  END IF

!------ Write output

  IF( lAvg .OR. lSmpOut )THEN
    IF( lOutToSmp )CALL out_smp()
  ELSE
    IF( .NOT.BTEST(SmpOutType,SOB_INTONLY) )CALL out_smp()
  END IF

  t_smp = ts

  IF( lExit )EXIT TimeLoop

  lFirst = .FALSE.

END DO TimeLoop

9999 CONTINUE

!------ Remove temporary dosage and/or deposition grids

IF( ndepblk_smp > 0 .AND. lLargeDelT )THEN
  DO i = 1,ndepblk_smp
    IF( depblk_smp(i)%grdI > 0 )irv = SAG_RmvGrdStr( depblk_smp(i)%grdI )
  END DO
END IF

IF( ndosblk_smp > 0 .AND. lLargeDelT )THEN
  DO i = 1,ndosblk_smp
    IF( dosblk_smp(i)%grdI > 0 )irv = SAG_RmvGrdStr( dosblk_smp(i)%grdI )
  END DO
END IF


9998 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE get_samp_val( ss,xs,ys,tfac )

!------ Get instantaneous concentration at sensor

USE scipuff_fi
USE met_fi
USE sampler_fi
USE UtilMtlAux
USE step_p_fi, ONLY: myTId
USE omp_lib

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: xs, ys   !Horizontal location (project coord)
REAL,           INTENT( IN    ) :: tfac     !Time-integration factor

REAL, PARAMETER :: ARG2 = 2.*20. !Set to match ARGMAX in GetPuffVal

INTEGER alloc_stat, k, igrd, jpuf
INTEGER i1, i2, j1, j2, k1, k2
INTEGER i, j, iSensor, ityp
INTEGER nsg
REAL    xp0, yp0, xx, yy, xp, yp
REAL    dgrd, hp, hx, hy, cfo
REAL    xmv0, ymv0, xmvp, ymvp
LOGICAL lpuf, lcheckz, ltot
LOGICAL refine
REAL    vel2, qq

REAL, DIMENSION(:), POINTER :: dsmp

REAL, DIMENSION(3) :: x_sensor
REAL, DIMENSION(:), ALLOCATABLE :: csmp

INTEGER, EXTERNAL :: getPuffifld
INTEGER, EXTERNAL :: limit
INTEGER, EXTERNAL :: get_mxlev
LOGICAL, EXTERNAL :: IsWetParticle, IsAerosol
LOGICAL, EXTERNAL :: find_cell_samp
REAL,    EXTERNAL :: rlimit

INTERFACE
  SUBROUTINE GetPuffVal( x_sensor,p,cfo,csmp,ltot,mvlos,cID )
    USE scipuff_fi
    USE sampler_fd
    REAL, DIMENSION(3), INTENT( IN  ) :: x_sensor !sensor location
    TYPE( puff_str ),   INTENT( IN  ) :: p        !puff structure
    REAL,               INTENT( IN  ) :: cfo      !Decay/viability factor
    REAL, DIMENSION(*), INTENT( OUT ) :: csmp     !puff concentration at sensor, mean, variance and scale
    LOGICAL,            INTENT( IN  ) :: ltot     !flag for total variance
    TYPE( los_str ),    INTENT( IN  ) :: mvlos    !Moving sensor line-of-sight
    INTEGER, DIMENSION(:), POINTER, OPTIONAL :: cID      !Multicomponent ID
  END SUBROUTINE GetPuffVal
END INTERFACE

!------ Set sensor-type id

SELECT CASE( TRIM(ss%type) )
  CASE( 'CORR' )
    iSensor = IS_CORR
  CASE DEFAULT
    iSensor = IS_CONC
END SELECT

myTId = OMP_GET_THREAD_NUM() + 1

  ALLOCATE( csmp(3+ss%nmc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'get_samp_val'
  eMessage = 'Error allocating array for sensor output'
  GOTO 9999
END IF

dsmp => ss%dsmp !Local pointer to sensor output fields

!------ Get met for multicomponent (conversion to ppm)

IF( BTEST(ss%stype,STB_MULT) )THEN
  CALL get_met( xs,ys,ss%zh,0.,0.,1 )
END IF


!------ Get met variables for special correlation sampler

IF( BTEST(ss%stype,STB_CORR) )THEN
  CALL get_met( xs,ys,ss%zh,0.,0.,1 )
  dsmp(6) = dsmp(6) + ub
  dsmp(7) = dsmp(7) + vb
  vel2 = ub*ub + vb*vb
  IF( t_avg /= DEF_VAL_R )CALL save_met()
END IF

!------ Define range for grid search

k = MIN(INT(ss%z/dzg)+1,nz)

IF( ss%speed > 0. )THEN
  k1 = MIN(INT(ss%mvlos%z/dzg)+1,nz)
  IF( k1 < k )THEN
    k1 = k1 - 2
    k2 = k  + 2
  ELSE
    k2 = k1 + 2
    k1 = k  - 2
  END IF
ELSE
  k1 = k - 2
  k2 = k + 2
END IF

k1 = MAX(k1,1)
k2 = MIN(k2,nz)

IF( zi_smp > 0.0 .AND. ss%zh <= zi_smp )THEN
  IF( lter )THEN
    CALL get_topogPrj( xs,ys,hp,hx,hy )
  ELSE
    hp = 0.
  END IF
  k1 = 1                                       !Account for potentially large puff spread in
  k2 = limit( k2,INT((zi_smp+hp)/dzg)+1,nz )   !CBL by looping over all levels below zinv
END IF

!------ Copy out sensor location

x_sensor(1) = xs
x_sensor(2) = ys
x_sensor(3) = ss%z

!------ Set flag for height adjustments with nested grids

lcheckz = numMet > 1

IF( BTEST(ss%stype,STB_FXGRID) .OR. BTEST(ss%stype,STB_AUTOGRID) )dsmp = 0.

!------ Locate sampler on puff grid

IF( lmap == I_LATLON )THEN
  IF( x_sensor(1) < xmin )THEN
    IF( x_sensor(1) + 360. <= xmax )x_sensor(1) = x_sensor(1) + 360.
  ELSE IF( x_sensor(1) > xmax )THEN
    IF( x_sensor(1) - 360. >= xmin )x_sensor(1) = x_sensor(1) - 360.
  END IF
END IF

xx = FLOAT(nx)-1.E-6
yy = FLOAT(ny)-1.E-6

xp0 = (x_sensor(1)-xmin)/dxg
yp0 = (x_sensor(2)-ymin)/dyg
xp0 = rlimit(xp0,0.,xx)
yp0 = rlimit(yp0,0.,yy)

IF( ss%speed > 0. )THEN
  xmv0 = (ss%mvlos%x-xmin)/dxg
  ymv0 = (ss%mvlos%y-ymin)/dyg
  xmv0 = rlimit(xmv0,0.,xx)
  ymv0 = rlimit(ymv0,0.,yy)
END IF

!------ Loop over grid levels & find puffs near sampler

VertLoop : DO k = k1,k2

  GridLev : DO igrd = 0,get_mxlev( k )

    dgrd  = 0.5**igrd

    xp = (FLOAT(INT(xp0/dgrd)) + 0.5)*dgrd
    yp = (FLOAT(INT(yp0/dgrd)) + 0.5)*dgrd

    IF( ss%speed > 0. )THEN
      xmvp = (FLOAT(INT(xmv0/dgrd)) + 0.5)*dgrd
      ymvp = (FLOAT(INT(ymv0/dgrd)) + 0.5)*dgrd
      CALL limintx( xp,dgrd,MIN(xmvp,xp),MAX(xmvp,xp),i1,i2,nx )
      CALL limintx( yp,dgrd,MIN(ymvp,yp),MAX(ymvp,yp),j1,j2,ny )
    ELSE IF( global_lon )THEN
      i1 = -2
      i2 =  2
      CALL limint2( yp,dgrd,j1,j2,ny )
    ELSE
      CALL limint2( xp,dgrd,i1,i2,nx )
      CALL limint2( yp,dgrd,j1,j2,ny )
    END IF

    refine = .FALSE.

    YLoop : DO j = j1,j2
      yy = yp + FLOAT(j)*dgrd

      XLoop : DO i = i1,i2
        xx = xp + FLOAT(i)*dgrd

        IF( global_lon )CALL SetGlobalGrid( xx,nx )

        refine = refine .OR. find_cell_samp( xx,yy,k,igrd,nx,ny,jpuf )

        PuffLinkedList : DO WHILE( jpuf /= 0 )

          ityp = puff(jpuf)%ityp
          IF( puff(jpuf)%idtl == I_REMOVE )THEN
            jpuf = puff(jpuf)%inxt
            CYCLE
          END IF

          lpuf = .FALSE.

          SELECT CASE( iSensor )
            CASE( IS_CONC,IS_CORR )
              lpuf = (ityp >= ss%is) .AND. (ityp <= ss%ie)
              lpuf = lpuf .OR. (ss%is == 1 .AND. IsAerosol(typeID(ityp)%icls))
              IF( IsWetParticle(typeID(ityp)%icls) )THEN
                nsg = GetSubgroups( material(typeID(ityp)%imat),mat_aux )
                lpuf = lpuf .OR. ((ityp >= ss%is+nsg) .AND. (ityp <= ss%ie+nsg))
              END IF
          END SELECT

          IF( lpuf )THEN  !This puff contributes to the sensor

            IF( lcheckz )THEN
              CALL get_topogIn( x_sensor(1),x_sensor(2),hp,hx,hy,getPuffifld(puff(jpuf)) )
              IF( BTEST(ss%stype,STB_AGL) )THEN
                x_sensor(3) = ss%zh + hp
              ELSE
                x_sensor(3) = MAX(ss%z,hp)
              END IF
            END IF

            IF( (puff(jpuf)%zbar-x_sensor(3))**2 < ARG2*puff(jpuf)%szz )THEN !Check for vertical overlap

              ltot = BTEST(ss%stype,STB_TOTALVAR) .AND. typeID(ityp)%ltot

              IF( BTEST(ss%stype,STB_NODECAY) )THEN
                cfo = 1.0
              ELSE
                cfo = puff(jpuf)%cfo
              END IF
              IF( BTEST(ss%stype,STB_MULT) )THEN
                CALL GetPuffVal( x_sensor,puff(jpuf),cfo,csmp,ltot,ss%mvlos,ss%mcID )

              ELSE

                CALL GetPuffVal( x_sensor,puff(jpuf),cfo,csmp,ltot,ss%mvlos )
              END IF
              IF( nError /= NO_ERROR )GOTO 9999

              IF( csmp(1) > SMALL )THEN
                IF( BTEST(ss%stype,STB_PART) )THEN
                  CALL sum_bysize_sensor( ss,puff(jpuf),csmp,tfac )
                  IF( nError /= NO_ERROR )GOTO 9999

                ELSE

                  CALL sum_sensor( ss,iSensor,puff(jpuf),csmp,tfac )
                  IF( nError /= NO_ERROR )GOTO 9999

                  IF( iSensor == IS_CORR )THEN
                    dsmp(5) = dsmp(5) + csmp(2)*puff(jpuf)%si
                    IF( t_avg /= DEF_VAL_R )THEN
                      CALL turb_rescale( puff(jpuf)%si,puff(jpuf)%sv,vel2 )
                      qq = 2.*uubl + 2.*vvbl + wwbh + uub + vvb
                      dsmp(4) = dsmp(4) + csmp(2)*puff(jpuf)%sr * SQRT(1.+vel2/(qq+1.E-6))
                      CALL reset_met()
                    END IF
                  END IF

                END IF
              END IF
            END IF
          END IF

          jpuf = puff(jpuf)%inxt

        END DO PuffLinkedList

      END DO XLoop

    END DO YLoop

    IF( .NOT.refine )EXIT GridLev

  END DO GridLev

END DO VertLoop


!------ Add ambient concentrations

IF( BTEST(ss%stype,STB_MULT) )CALL GetSampMC( ss,tfac )

IF( iSensor == IS_CORR .AND. t_avg == DEF_VAL_R )THEN
  IF( dsmp(3) > 0. )THEN
    qq = 2.*uubl + 2.*vvbl + wwbh + uub + vvb
    dsmp(4) = dsmp(3)  * SQRT(1.+vel2/(qq+1.E-6))
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE PushSampSrfDep()

!------ Copy appropriate deposition fields and push to bottom cells

USE scipuff_fi
USE surface_fi
USE sampler_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE sagerr_fd

IMPLICIT NONE

INTEGER irv, i, iv, nfld, iCopy, grdI

TYPE( SAGgrid_str ), POINTER :: grd, srf

INTEGER, DIMENSION(3) :: ifld  !Set for (current) maximum number of fields

INTEGER, EXTERNAL :: SAG_InitGridID, SAG_NewGrdStr, SAG_CopyGridID, SAG_BottomValueID

!------ Point to deposition grid

srf => Psrfdep

!------ Copy into temporary grid for each deposition field (block)

DO i = 1,ndepblk_smp

  depblk_smp(i)%grdI = 0

  irv = SAG_NewGrdStr( grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDep'
    eMessage = 'Error creating new surface grid for deposition samplers'
    GOTO 9999
  END IF

  depblk_smp(i)%grdI = grdI

  grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

  nfld = depblk_smp(i)%nfld

  irv = SAG_InitGridID( ' ',0,SAG_GRID_BOTH,srf%mxgrd,nfld,nfld,grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDep'
    eMessage = 'Error initializing SAG surface grid for deposition samplers'
    GOTO 9999
  END IF

!----- Copy grid from surface field

  irv = SAG_CopyGridID( srfdep,grdI,SAG_COPY_GRID )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDep'
    eMessage = 'Error copying SAG grid for deposition samplers'
    GOTO 9999
  END IF

!----- Copy mean and variance fields from surface deposition field

  DO iv = 1,nfld

    iCopy = SAG_COPY_FIELD + iv*SAG_TO_FIELD + depblk_smp(i)%ifld(iv)
    irv   = SAG_CopyGridID( srfdep,grdI,iCopy )

    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'PushSampSrfDep'
      eMessage = 'Error copying SAG surface deposition grid'
      GOTO 9999
    END IF

  END DO

  grd%status = SAG_TEMP
  grd%nunit  = -1
  grd%file   = ' '

!------ Push to bottom

  ifld(1:nfld) = (/(iv,iv=1,nfld)/)
  irv = SAG_BottomValueID( grdI,nfld,ifld )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDep'
    eMessage = 'Error setting values on SAG surface grid'
    GOTO 9999
  END IF

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE PushSampSrfDos()

!------ Copy appropriate dosage fields and push to bottom cells

USE scipuff_fi
USE surface_fi
USE sampler_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE sagerr_fd

IMPLICIT NONE

INTEGER irv, i, iv, nfld, iCopy, grdI

TYPE( SAGgrid_str ), POINTER :: grd, srf

INTEGER, DIMENSION(3) :: ifld  !Set for (current) maximum number of fields

INTEGER, EXTERNAL :: SAG_InitGridID, SAG_NewGrdStr, SAG_CopyGridID, SAG_BottomValueID

!------ Point to dosage grid

srf => Psrfdos

!------ Copy into temporary grid for each dosage field (block)

DO i = 1,ndosblk_smp

  dosblk_smp(i)%grdI = 0

  irv = SAG_NewGrdStr( grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDos'
    eMessage = 'Error creating new surface grid for integrated gridded sensors'
    GOTO 9999
  END IF

  dosblk_smp(i)%grdI = grdI

  grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

  nfld = dosblk_smp(i)%nfld

  irv = SAG_InitGridID( ' ',0,SAG_GRID_BOTH,srf%mxgrd,nfld,nfld,grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDos'
    eMessage = 'Error initializing SAG surface grid for integrated gridded sensors'
    GOTO 9999
  END IF

!----- Copy grid from surface field

  irv = SAG_CopyGridID( srfdos,grdI,SAG_COPY_GRID )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDos'
    eMessage = 'Error copying SAG grid for integrated gridded sensors'
    GOTO 9999
  END IF

!----- Copy mean and variance fields from surface dosage field

  DO iv = 1,nfld

    iCopy = SAG_COPY_FIELD + iv*SAG_TO_FIELD + dosblk_smp(i)%ifld(iv)
    irv   = SAG_CopyGridID( srfdos,grdI,iCopy )

    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'PushSampSrfDos'
      eMessage = 'Error copying SAG surface dosage grid'
      GOTO 9999
    END IF

  END DO

  grd%status = SAG_TEMP
  grd%nunit  = -1
  grd%file   = ' '

!------ Push to bottom

  ifld(1:nfld) = (/(iv,iv=1,nfld)/)
  irv = SAG_BottomValueID( grdI,nfld,ifld )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'PushSampSrfDos'
    eMessage = 'Error setting values on SAG surface grid'
    GOTO 9999
  END IF

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE get_samp_srfdep( ss,xs,ys )

!------ Get mean and variance from surface deposition field at sensor location

USE scipuff_fi
USE sampler_fi

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: xs, ys   !Horizontal location

INTEGER grdI, nfld, i
LOGICAL lNonZero
REAL,    DIMENSION(3) :: val
INTEGER, DIMENSION(3) :: ifld

nfld = MIN(depblk_smp(ss%is)%nfld,ss%nvar)  !Assumes sampler and surface variables
                                            !always correspond in order
ifld(1:nfld) = (/(i,i=1,nfld)/)

grdI = depblk_smp(ss%is)%grdI

CALL GetPointVal( xs,ys,val,grdI,nfld,ifld,.TRUE. )

IF( nfld > 2 )THEN
  lNonZero = ANY(val > 1.E-30)
ELSE
  lNonZero = val(1) > 1.E-30
END IF
IF( lNonZero )THEN
  ss%dsmp(1:nfld) = val(1:nfld)
ELSE
  ss%dsmp(1:nfld) = 0.
END IF

!------ Get vapor field to subtract from total for cumulative liquid sensor

IF( ss%ie > 0 )THEN

  grdI = depblk_smp(ss%ie)%grdI

  CALL GetPointVal( xs,ys,val,grdI,2,ifld,.TRUE. )

  IF( val(1) > 1.E-30 )THEN
    ss%dsmp(1) = MAX(ss%dsmp(1)-val(1),0.)
    ss%dsmp(2) = MAX(ss%dsmp(2)-val(2),0.)
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE get_samp_srfdos( ss,xs,ys )

!------ Get mean and variance from surface dosage field at sensor location

USE scipuff_fi
USE sampler_fi

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: xs, ys   !Horizontal location

INTEGER grdI, nfld, i

REAL,    DIMENSION(3) :: val
INTEGER, DIMENSION(3) :: ifld

nfld = MIN(dosblk_smp(ss%is)%nfld,ss%nvar)    !Assumes sampler and surface variables
                                              !always correspond in order
ifld(1:nfld) = (/(i,i=1,nfld)/)

grdI = dosblk_smp(ss%is)%grdI

CALL GetPointVal( xs,ys,val,grdI,nfld,ifld,.TRUE. )

IF( val(1) > 1.E-30 )THEN
  ss%dsmp(1:nfld) = val(1:nfld)
ELSE
  ss%dsmp(1:nfld) = 0.
END IF

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE GetPuffVal( x_sensor,p,cfo,csmp,ltot,mvlos,cID )

!------ Calculate puff concentration at sensor location

USE scipuff_fi
USE sampler_fi
USE met_fi
USE chem_fi
USE omp_lib
USE step_p_fi, ONLY : myTid

IMPLICIT NONE

REAL, DIMENSION(3), INTENT( IN  ) :: x_sensor !sensor location
TYPE( puff_str ),   INTENT( IN  ) :: p        !puff structure
REAL,               INTENT( IN  ) :: cfo      !Decay/viability factor
REAL, DIMENSION(*), INTENT( OUT ) :: csmp     !puff concentration at sensor, mean, variance and scale
LOGICAL,            INTENT( IN  ) :: ltot     !flag for total variance
TYPE( los_str ),    INTENT( IN  ) :: mvlos    !Moving sensor line-of-sight
INTEGER, DIMENSION(:), POINTER, OPTIONAL :: cID      !Multicomponent ID list

REAL, PARAMETER :: ARGMAX = 20.0

TYPE( puff_totalcc  ) pt
TYPE( puff_str ) padv

REAL, DIMENSION(3) :: xr, xnrm

INTEGER it
INTEGER mcID, alloc_stat
INTEGER i
INTEGER inFieldMC
REAL    facr
REAL    cfac, ccfac, slfac, xmap, ymap, vbar, tp, cl, ccl
REAL    deth, rat, hz, hx, hy, zp, znrm, zfac, vs
REAL    xp, yp, faci, arg, args, zr, facs, fac, vols, SzzMC, hp

REAL, DIMENSION(:), ALLOCATABLE :: comp

INTEGER, EXTERNAL :: getPuffifld

INTERFACE

  SUBROUTINE puff_los( lsg,los,pIn,cl,ccl,cID,comp )
    USE scipuff_fi
    USE los_fd
    LOGICAL,           INTENT( IN    ) :: lsg     !Single group flag
    TYPE( los_str  ),  INTENT( IN    ) :: los     !Line-of-Sight Structure
    TYPE( puff_str ),  INTENT( IN    ) :: pIn     !Puff Structure
    REAL,              INTENT( INOUT ) :: cl, ccl !Integrated values
    INTEGER, DIMENSION(:), POINTER, OPTIONAL          :: cID     !Multicomponent ID
    REAL,    DIMENSION(*), OPTIONAL , INTENT( INOUT ) :: comp    !Multicomponent data
  END SUBROUTINE puff_los

  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE

  SUBROUTINE GetChemValMC( p,cID,csmp,fac,facr,x_sensor )
    USE chem_fi
    USE scipuff_fi
    TYPE( puff_str ),   INTENT( IN  )   :: p        !puff structure
    INTEGER, DIMENSION(:), POINTER      :: cID      !Multicomponent ID
    REAL, DIMENSION(*), INTENT( INOUT ) :: csmp     !puff concentration at sensor, mean, variance and scale
    REAL,               INTENT( IN    ) :: fac
    REAL,               INTENT( IN    ) :: facr
    REAL,  DIMENSION(3),INTENT( IN    ) :: x_sensor
  END SUBROUTINE GetChemValMC

END INTERFACE

IF( PRESENT(cID) )THEN
  csmp(1:3+SIZE(cID)) = 0.
  ALLOCATE( comp(SIZE(cID)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'GetPuffVal'
    eMessage = 'Error allocating multicomponent array'
    GOTO 9999
  END IF
  comp = 0.
ELSE
  csmp(1:3) = 0.
END IF

!------ Account for puff advection since last update

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

IF( p%idtl >= 0 )THEN
  it = mstepP/MIN(2**p%idtl,mstepP)
  it = MOD(istepP,it)
  tp = FLOAT(it)*dtsP
ELSE
  tp = 0.
END IF

IF( mvlos%r > SMALL )THEN

  vols = PI3*SQRT(p%det)
  cfac = p%c/vols

  IF( ltot )THEN
    CALL get_totalcc( p,pt )
    ccfac = MAX((pt%cct-pt%cctb)/vols,0.0)
  ELSE
    ccfac = MAX((p%cc-p%ccb)/vols,0.0)
  END IF

  padv = p

  padv%xbar = p%xbar + DBLE(p%uo*tp*xmap)     !Advect puff location
  padv%ybar = p%ybar + DBLE(p%vo*tp*ymap)
  padv%zbar = p%zbar + p%wo*tp

  cl = 0.; ccl = 0.
  CALL puff_los( .NOT.ltot,mvlos,padv,cl,ccl,cID,comp )

  csmp(1) = cl / mvlos%r         !N.B. multiplied by time-step later
  csmp(2) = csmp(1) * ccfac/cfac !Time-scale applied later * MIN(p%sr,2.*sl/speed)
  csmp(3) = 0.                   !Not needed here (for INT)

  IF( PRESENT(cID) )THEN
    DO i = 1,SIZE(cID)
      csmp(3+i) = comp(i) / mvlos%r
    END DO
  END IF

ELSE

  vbar = p%zbar
  vs   = SNGL(x_sensor(3))

  zp = (vs-vbar)                       - p%wo*tp
  xp = (x_sensor(1)-SNGL(p%xbar))/xmap - p%uo*tp
  yp = (x_sensor(2)-SNGL(p%ybar))/ymap - p%vo*tp

!------ Gaussian argument

  arg = (p%axx*xp + 2.*p%axy*yp + 2.*p%axz*zp)*xp &
                  +   (p%ayy*yp + 2.*p%ayz*zp)*yp &
                                +    p%azz*zp*zp
  IF(  arg < ARGMAX )THEN

    vols = PI3*SQRT(p%det)
    cfac = p%c/vols

    IF( ltot )THEN
      CALL get_totalcc( p,pt )
      ccfac = MAX((pt%cct-pt%cctb)/vols,0.0)
    ELSE
      ccfac = MAX((p%cc-p%ccb)/vols,0.0)
    END IF
    slfac = 0.5*p%sr*ccfac  !p%sr is 2 x Tc

    deth = p%axx*p%ayy - p%axy**2
    rat  = 0.5/(p%det*deth)
    CALL zi_reflect( vbar,p%zc,p%zc,vs,rat,faci )

    IF( lter )THEN

      CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hz,hx,hy,getPuffifld(p) )
      zr   = vbar - hz
      CALL puff_grnd_reflect( zr,p,hx,hy,xr,xnrm,deth,znrm )
      zfac = 0.5*znrm/(p%det*deth)
      zr   = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) + xnrm(3)*(zp-xr(3))
      zr   = MAX(zr,0.)
      facs = EXP(zfac*zr)

    ELSE

      args = vs*vbar*rat
      facs = EXP(-args)
      hz   = 0.

    END IF

    fac = EXP(-arg)*(1.+facs)*(1.+faci)

      csmp(1) = fac*cfac *cfo
      csmp(2) = fac*ccfac*cfo*cfo
      csmp(3) = fac*slfac*cfo*cfo

      IF( PRESENT(cID) )THEN

        mcID = typeID(p%ityp)%mcID

        SELECT CASE( mat_mc%type(mcID) )
          CASE( MC_CHEM )
            facr = (1.+facs)*(1.+faci)
            chem => chemMC(mcID,myTid)

            inFieldMC = getPuffifld( p )
            SzzMC     = MIN(p%szz,1./p%azz)
            CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,SzzMC,p%zc,0,inField=inFieldMC )
            CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,inFieldMC )

            CALL SetChemAmbient( SNGL(p%xbar),SNGL(p%ybar),p%zbar-hp,t,.TRUE.,.TRUE. )
            IF( nError /= NO_ERROR )GOTO 9999

            CALL GetChemValMC( p,cID,csmp(4),fac,facr,x_sensor )
          CASE DEFAULT
            nError   = UK_ERROR
            eRoutine = 'GetPuffVal'
            eMessage = 'Multicomponent error'
            WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
            GOTO 9999
        END SELECT

      END IF

  END IF

END IF

9999 CONTINUE

IF( ALLOCATED(comp) )DEALLOCATE( comp,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE GetSampMC( ss,tfac )

USE chem_fi
USE sampler_fd
USE scipuff_fi
USE sampler_fi
USE met_fi
USE chem_aqaer_fi, ID_PART => ID_SPECIES_PARTICLE
USE step_p_fi, ONLY: myTID

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: tfac     !Time-integration factor

INTEGER i, mcID, n, ioff
REAL    cfac, camb
REAL    rh
REAL    aso4j, aso4i, ano3j, ano3i, anh4j, anh4i
REAL    tso4j, tso4i, tno3j, tno3i, tnh4j, tnh4i
REAL    fso4j, fno3j, fnh4j
REAL    faso4j, fano3j, fanh4j
REAL    aso4, ano3, anh4, tso4, tno3, tnh4
REAL    ahno3, anh3, thno3, tnh3
INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

REAL, EXTERNAL :: getSpAmbLim

i    = typeID(ss%is)%imat
mcID = material(i)%mcID
n    = mat_mc%ID(mcID)

IF( mat_mc%type(mcID) /= MC_CHEM )RETURN

chem => chemMC(n,myTid)

!------ Get met at sampler location

CALL get_met( ss%x,ss%y,ss%zh,0.,0.,1 )


IF( multicomp )THEN
  ss%asmp(1) = ss%asmp(1) + tfac*tb         ! temp(K)
  ss%asmp(2) = ss%asmp(2) + tfac*pb/1013.25 ! press(atm)
  ss%asmp(3) = ss%asmp(3) + tfac*hb         ! humidity(g H2O/g dry air)
END IF

!------ Update species background concentrations and set equilibrium species

CALL SetChemAmbient( ss%x,ss%y,ss%zh,t,.TRUE.,.TRUE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Add ambient to species concentrations

IF( lAvg .OR. BTEST(ss%stype,STB_INTEGRATE) )THEN
  ioff = 2
ELSE
  ioff = 3
END IF

!------ Calculate relative humidity for inorganic aerosol equilibrium calculations

IF( laerosol )THEN
  CALL RelHumid( hb,tb,pb,rh )
  rh = MAX(MIN(rh/100.,1.),0.01)
END IF

DO i = 1,SIZE(ss%mcID)
  cfac = 1.
  IF( chem%oUnits == UNIT_UGM3 .AND. &
      chem%species(ss%mcID(i))%class /= ID_SPECIES_PARTICLE )THEN
    cfac = PPM2UGM3*chem%species(ss%mcID(i))%MW
  ENDIF
  IF( chem%lAddAmb )THEN
    camb            = cfac*chem%species(ss%mcID(i))%amb*tfac
    ss%dsmp(ioff+i) = MAX(0.,ss%dsmp(ioff+i) + camb)
    ss%asmp(3+i)    = ss%asmp(3+i) + camb
  ELSE
    ! Perturbations only
    ss%dsmp(ioff+i) = MAX(cfac*getSpAmbLim(ss%mcID(i))*tfac,ss%dsmp(ioff+i))
  ENDIF
END DO

! Perform inorganic aerosol equilibrium calculations
IF( laerosol )THEN

  ! Get concs of species in inorganic aerosol equilibrium calculations
  ! Ignore NA and CL species for now
  ! Note: Assumes output units are ug/m3 and laddamb is true
  IF( chem%oUnits /= UNIT_UGM3 ) THEN
    nError   = UK_ERROR
    eRoutine = 'GetSampMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A)') 'Output units need to be in ug/m3'
    GO TO 9999
  END IF
  IF( .NOT. chem%lAddAmb )THEN
    nError   = UK_ERROR
    eRoutine = 'GetSampMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A)') 'Only total concs can be output'
    GO TO 9999
  END IF

  aso4j = MAX(ss%asmp(3+IDSO4J),0.)
  tso4j = MAX(ss%dsmp(ioff+IDSO4J),0.)
  aso4i = MAX(ss%asmp(3+IDSO4I),0.)
  tso4i = MAX(ss%dsmp(ioff+IDSO4I),0.)
  aso4  = aso4j + aso4i
  if ( aso4 > 0. ) then
    faso4j = aso4j/aso4
  else
    faso4j = 1.0
  end if
  tso4  = tso4j + tso4i
  if ( tso4 > 0. ) then
    fso4j = tso4j/tso4
  else
    fso4j = 1.0
  end if

  ano3j = MAX(ss%asmp(3+IDNO3J),0.)
  tno3j = MAX(ss%dsmp(ioff+IDNO3J),0.)
  ano3i = MAX(ss%asmp(3+IDNO3I),0.)
  tno3i = MAX(ss%dsmp(ioff+IDNO3I),0.)
  ano3  = ano3j + ano3i
  if ( ano3 > 0. ) then
    fano3j = ano3j/ano3
  else
    fano3j = 1.0
  end if

  tno3  = tno3j + tno3i
  if ( tno3 > 0. ) then
    fno3j = tno3j/tno3
  else
    fno3j = 1.0
  end if

  anh4j = MAX(ss%asmp(3+IDNH4J),0.)
  tnh4j = MAX(ss%dsmp(ioff+IDNH4J),0.)
  anh4i = MAX(ss%asmp(3+IDNH4I),0.)
  tnh4i = MAX(ss%dsmp(ioff+IDNH4I),0.)
  anh4  = anh4j + anh4i
  if ( anh4 > 0. ) then
    fanh4j = anh4j/anh4
  else
    fanh4j = 1.0
  end if

  tnh4  = tnh4j + tnh4i
  if ( tnh4 > 0. ) then
    fnh4j = tnh4j/tnh4
  else
    fnh4j = 1.0
  end if

  ahno3 = MAX(ss%asmp(3+IDHNO3),0.)
  thno3 = MAX(ss%dsmp(ioff+IDHNO3),0.)
  anh3  = MAX(ss%asmp(3+IDNH3),0.)
  tnh3  = MAX(ss%dsmp(ioff+IDNH3),0.)

! Call with ambient
  CALL inorg_aero( aso4,ano3,anh4,ahno3,anh3,tb,rh )

! Call with total
  CALL inorg_aero( tso4,tno3,tnh4,thno3,tnh3,tb,rh )

! Make sure ambient <= total
  aso4  = MIN( tso4,  aso4 )
  ano3  = MIN( tno3,  ano3 )
  anh4  = MIN( tnh4,  anh4 )
  ahno3 = MIN( thno3,ahno3 )
  anh3  = MIN( tnh3,  anh3 )

  ss%asmp(3+IDSO4J) = faso4j * aso4
  ss%asmp(3+IDSO4I) = (1. - faso4j) * aso4
  ss%dsmp(ioff+IDSO4J) = fso4j * tso4
  ss%dsmp(ioff+IDSO4I) = (1. - fso4j) * tso4

  ss%asmp(3+IDNO3J) = fano3j * ano3
  ss%asmp(3+IDNO3I) = (1. - fano3j) * ano3
  ss%dsmp(ioff+IDNO3J) = fno3j * tno3
  ss%dsmp(ioff+IDNO3I) = (1. - fno3j) * tno3

  ss%asmp(3+IDNH4J) = fanh4j * anh4
  ss%asmp(3+IDNH4I) = (1. - fanh4j) * anh4
  ss%dsmp(ioff+IDNH4J) = fnh4j * tnh4
  ss%dsmp(ioff+IDNH4I) = (1. - fnh4j) * tnh4

  ss%asmp(3+IDHNO3) = ahno3
  ss%dsmp(ioff+IDHNO3) = thno3

  ss%asmp(3+IDNH3) = anh3
  ss%dsmp(ioff+IDNH3) = tnh3

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

!------ Local routine to save/re-set met variables that are affected by time-averaging

SUBROUTINE save_met()

USE sampler_fi
USE met_fi, ONLY: uubl,vvbl,wwbl,wwbh,uub,vvb,sbl,sbls,sby

IMPLICIT NONE

uubl_S = uubl
vvbl_S = vvbl
wwbl_S = wwbl
wwbh_S = wwbh
uub_S  = uub
vvb_S  = vvb
sbl_S  = sbl
sbls_S = sbls
sby_S  = sby

RETURN
END

!==============================================================================

SUBROUTINE reset_met()

USE sampler_fi
USE met_fi, ONLY: uubl,vvbl,wwbl,wwbh,uub,vvb,sbl,sbls,sby

uubl = uubl_S
vvbl = vvbl_S
wwbl = wwbl_S
wwbh = wwbh_S
uub  = uub_S
vvb  = vvb_S
sbl  = sbl_S
sbls = sbls_S
sby  = sby_S

RETURN
END

!==============================================================================

SUBROUTINE get_samp_met( ss,xs,ys,tfac )

!------ Get SCIPUFF meteorology at sensor location

USE met_fi
USE sampler_fi
USE met_fi
USE constants_fd

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: xs, ys   !Horizontal location
REAL,           INTENT( IN    ) :: tfac     !Time-integration factor


!------ Call met interpolation routine

CALL get_met( xs,ys,ss%zh,0.,0.,1 )

!------ Put into sensor structure

ss%dsmp(1) = ss%dsmp(1) + ub*tfac
ss%dsmp(2) = ss%dsmp(2) + vb*tfac
ss%dsmp(3) = ss%dsmp(3) + wb*tfac
ss%dsmp(4) = ss%dsmp(4) + tb*tfac

IF( BTEST(ss%stype,STB_TURB) )THEN

  ss%dsmp(5) = ss%dsmp(5) + (zinv-TerElev)*tfac
  ss%dsmp(6) = ss%dsmp(6) + wts*tfac * RHOCP
  IF( BTEST(ss%stype,STB_INTEGRATE) )THEN
    ss%dsmp(7) = ss%dsmp(7) + tfac/xml
  ELSE
    ss%dsmp(7) = ss%dsmp(7) + tfac*xml
  END IF
  ss%dsmp(8)  = ss%dsmp(8)  + uubl*tfac
  ss%dsmp(9)  = ss%dsmp(9)  + vvbl*tfac
  ss%dsmp(10) = ss%dsmp(10) + wwbl*tfac
  ss%dsmp(11) = ss%dsmp(11) + SQRT(us2)*tfac
ELSE IF( BTEST(ss%stype,STB_ENSM) )THEN

  ss%dsmp(5) = ss%dsmp(5) + uub*tfac
  ss%dsmp(6) = ss%dsmp(6) + vvb*tfac
  ss%dsmp(7) = ss%dsmp(7) + uvb*tfac
  ss%dsmp(8) = ss%dsmp(8) + sby*tfac

END IF

RETURN
END

!==============================================================================

SUBROUTINE get_samp_los( ss,xs,ys,tfac )

!------ Get line-integral of concentration starting at sensor location

USE scipuff_fi
USE sampler_fi
USE los_fd
USE UtilMtlAux
USE chem_fi
USE aerosol_fi
USE met_fi !, ONLY: tb,pb

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss       !Sensor structure
REAL,           INTENT( IN    ) :: xs, ys   !Horizontal location
REAL,           INTENT( IN    ) :: tfac     !Time-integration factor

INTEGER ipuf, ityp
INTEGER nsg
LOGICAL lsg, lpuf
REAL    cl, ccl

REAL, DIMENSION(3) :: csmp

TYPE( los_str ) :: los

INTEGER alloc_stat, i

REAL, DIMENSION(:), ALLOCATABLE :: comp

INTERFACE
  SUBROUTINE puff_los( lsg,los,pIn,cl,ccl,cID,comp )
    USE scipuff_fi
    USE los_fd
    LOGICAL,           INTENT( IN    ) :: lsg     !Single group flag
    TYPE( los_str  ),  INTENT( IN    ) :: los     !Line-of-Sight Structure
    TYPE( puff_str ),  INTENT( IN    ) :: pIn     !Puff Structure
    REAL,              INTENT( INOUT ) :: cl, ccl !Integrated values
    INTEGER, DIMENSION(:), POINTER, OPTIONAL          :: cID     !Multicomponent ID
    REAL,    DIMENSION(*), OPTIONAL , INTENT( INOUT ) :: comp    !Multicomponent data
  END SUBROUTINE puff_los
END INTERFACE

LOGICAL, EXTERNAL :: IsAerosol, IsWetParticle

!------ Initialize output to zero

cl = 0.; ccl = 0.

IF( BTEST(ss%stype,STB_MULT) )THEN
  ALLOCATE( comp(ss%nmc),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEn
    nError   = UK_ERROR
    eRoutine = 'get_samp_los'
    eMessage = 'Multicomponent error'
    eInform  = 'Error allocating mutlicomponent array'
    GOTO 9999
  END IF
  comp(1:ss%nmc) = 0.0
END IF

CALL set_los_str( ss,xs,ys,los )

!------ Set single group flag (i.e., don't need total-cc)

lsg = ss%is == ss%ie

!------ Loop over active puffs

DO ipuf = 1,npuf
  IF( puff(ipuf)%idtl /= I_REMOVE )THEN

    ityp = puff(ipuf)%ityp
    lpuf = (ityp >= ss%is) .AND. (ityp <= ss%ie)
    lpuf = lpuf .OR. (ss%is == 1 .AND. IsAerosol(typeID(ityp)%icls))
    IF( IsWetParticle(typeID(ityp)%icls) )THEN
      nsg = GetSubgroups( material(typeID(ityp)%imat),mat_aux )
      lpuf = lpuf .OR. (ityp >= ss%is+nsg) .AND. (ityp <= ss%ie+nsg)
    END IF
    IF( lpuf )THEN
      IF( BTEST(ss%stype,STB_PART) )THEN
        cl = 0.; ccl = 0.
        IF( BTEST(ss%stype,STB_MULT) )comp = 0.  !This should not occur
      END IF
      IF( BTEST(ss%stype,STB_MULT) )THEN
        CALL puff_los( lsg,los,puff(ipuf),cl,ccl,ss%mcID,comp )
      ELSE
        CALL puff_los( lsg,los,puff(ipuf),cl,ccl )
      END IF
      IF( nError /= NO_ERROR )GOTO 9999
      IF( BTEST(ss%stype,STB_PART) )THEN
        csmp = (/cl,ccl,0./)
        CALL sum_bysize_sensor( ss,puff(ipuf),csmp,tfac )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF

    END IF

  END IF
END DO

IF( .NOT.BTEST(ss%stype,STB_PART) )THEN
  ss%dsmp(1) = ss%dsmp(1) + tfac*cl
  ss%dsmp(2) = ss%dsmp(2) + tfac*ccl
  IF( BTEST(ss%stype,STB_MULT) )THEN
    DO i = 1,ss%nmc
      IF( chem%oUnits == UNIT_UGM3 .AND. &
        chem%species(ss%mcID(i))%class /= ID_SPECIES_PARTICLE )THEN
        ss%dsmp(2+i) = ss%dsmp(2+i) + tfac*comp(i)*PPM2UGM3*chem%species(ss%mcID(i))%MW
      ELSE
        ss%dsmp(2+i) = ss%dsmp(2+i) + tfac*comp(i)
      END IF
    END DO
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE out_smp()

!------ Output sampler data

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE SamplerGridOuput

IMPLICIT NONE

INTEGER ios, is, i, j
REAL    xs, ys, tOut, tol

REAL, DIMENSION(:), POINTER :: dsmpPt

INTERFACE

  SUBROUTINE ProcessSensor( ss,dsmp )
    USE sampler_fd
    TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure
    REAL, DIMENSION(:), POINTER    :: dsmp !Sensor output array
  END SUBROUTINE ProcessSensor

  SUBROUTINE UnProcessSensor( ss,dsmp )
    USE sampler_fd
    TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure
    REAL, DIMENSION(:), POINTER    :: dsmp !Sensor output array
  END SUBROUTINE UnProcessSensor

END INTERFACE

INTEGER, EXTERNAL :: WriteBinaryReal


!------ Check if beyond last time in output list

IF( lSmpOutList )THEN
  IF( SampTimeList%tStart == DEF_VAL_R )GOTO 9999
END IF

!------ Output time

tOut = t
IF( (lAvg .OR. lSmpOut) .AND. dtSmpOut > 0. )tOut = MIN(tOut,tSmpOut)

IF( lBinOut )THEN
  CALL out_smpBinaryInit()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( lGridOut )THEN

  CALL SamplerGridTime( tOut )
  IF( nError /= 0 )GOTO 9999

  DO is = 1,nsmp

    CALL SamplerGridSensor( smp(is) )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( BTEST(smp(is)%stype,STB_FXGRID) .OR. BTEST(smp(is)%stype,STB_AUTOGRID) )THEN
      DO j = 1,smp(is)%ny
        DO i = 1,smp(is)%nx
          dsmpPt => smp(is)%gsmp(i,j,1:smp(is)%nvar)
          CALL ProcessSensor( smp(is),dsmpPt )
        END DO
      END DO
    ELSE
      CALL ProcessSensor( smp(is),smp(is)%dsmp )
    END IF

    CALL SamplerGridSensorOutput( smp(is) )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO

  CALL SamplerGridCloseTime()
  IF( nError /= 0 )GOTO 9999

  CALL SamplerGridCloseHeader()
  IF( nError /= 0 )GOTO 9999

ELSE  !Single-point sensors

  DO is = 1,nsmp
    CALL ProcessSensor( smp(is),smp(is)%dsmp )
  END DO

  IF( lBinOut )THEN

     CALL out_smpBinarySingle()
     IF( nError /= NO_ERROR )GOTO 9999

  ELSE
!------ SCIP3.2 format (wrapped)

  IF( lWrap )THEN

    WRITE(lun_smp,*,IOSTAT=ios) tOut,((smp(is)%dsmp(i),i=1,smp(is)%nvar),is=1,nsmp)

    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'out_smp'
      eMessage = 'Error writing wrapped sampler output file'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF

!------ Output on a single line

  ELSE

    WRITE(lun_smp,FMT='(ES15.7)',ADVANCE='NO',IOSTAT=ios) tOut
    IF( multicomp .AND. nmc0 > 0 )WRITE(lun_asmp,FMT='(ES15.7)',ADVANCE='NO',IOSTAT=ios) tOut

    SamplerLoop : DO is = 1,nsmp

!------ Sensor location output

      IF( BTEST(smp(is)%stype,STB_OUTLOC) )THEN
        xs = smp(is)%x
        ys = smp(is)%y

        IF( lSmpCnv )THEN
          CALL ConvertSampCoord( sampMap,lmap,sampZone,xs,ys )!Convert back to coordinates specified in sam file
          IF( nError /= NO_ERROR )GOTO 9999
          IF( lMap == I_CARTESIAN .AND. sampMap == I_CARTESIAN .AND. lCartOrig )THEN
            xs = xs - cartX0
            ys = ys - cartY0
          END IF
        END IF

          IF( sampMap == I_UTM )THEN
            xs = smp(nsmp)%x / UTMfac
            ys = smp(nsmp)%y / UTMfac
          END IF

        IF( sampMap == I_LATLON .AND. lmap == I_LATLON )THEN
          IF( xs < xmin )THEN
            IF( xs + 360. <= xmax )xs = xs + 360.
          ELSE IF( xs > xmax )THEN
            IF( xs - 360. >= xmin )xs = xs - 360.
          END IF
        END IF

        IF( BTEST(smp(is)%stype,STB_AGL)) THEN
          WRITE(lun_smp,FMT='(2ES15.7,ES12.4)',ADVANCE='NO',IOSTAT=ios) xs,ys,smp(is)%zh
        ELSE
          WRITE(lun_smp,FMT='(2ES15.7,ES12.4)',ADVANCE='NO',IOSTAT=ios) xs,ys,smp(is)%z+hmin
        END IF
        IF( ios /= 0 )EXIT SamplerLoop

        IF( BTEST(smp(is)%stype,STB_LOS) )THEN
          WRITE(lun_smp,FMT='(3ES12.4)',ADVANCE='NO',IOSTAT=ios) smp(is)%az,smp(is)%el,smp(is)%dist
          IF( ios /= 0 )EXIT SamplerLoop
        END IF

      END IF

!------ Sensor output

      DO i = 1,smp(is)%nvar
        WRITE(lun_smp,FMT='(ES12.4)',ADVANCE='NO',IOSTAT=ios) smp(is)%dsmp(i)
        IF( ios /= 0 )EXIT SamplerLoop
      END DO
      IF( multicomp .AND. smp(is)%nmc > 0 )THEN
        DO i = 1,smp(is)%nvar
          WRITE(lun_asmp,FMT='(ES12.4)',ADVANCE='NO',IOSTAT=ios) smp(is)%asmp(i)
          IF( ios /= 0 )EXIT SamplerLoop
        END DO
      END IF
    END DO SamplerLoop

    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'out_smp'
      eMessage = 'Error writing sampler output file'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF

!------ Write end-of-record

    WRITE(lun_smp,FMT='()',ADVANCE='YES',IOSTAT=ios)
    IF( multicomp .AND. nmc0 > 0 )WRITE(lun_asmp,FMT='()',ADVANCE='YES',IOSTAT=ios)
  END IF   !If lBinOut or not

  END IF

END IF

DO is = 1,nsmp
  CALL UnProcessSensor( smp(is),smp(is)%dsmp )
END DO

IF( lAvg .OR. lSmpOut )THEN

  IF( lSmpOutList .AND. lAvg )dtSmpOut = 0. !Go to next time in list


  tSmpOut = tSmpOut + dtSmpOut

  IF( lSmpOutList )THEN !Check against start of next period
    IF( ASSOCIATED(SampTimeList%next) )THEN
      tol = MAX(0.001*MIN(dtSmpOut,SampTimeList%next%dtSamp),tolSmpOut)
      IF( tSmpOut > SampTimeList%next%tStart+tol .OR. dtSmpOut <= 0. ) THEN
        SampTimeList => SampTimeList%next
        IF( lAvg )THEN
          tStartSamp = SampTimeList%tStart
          dtSmpOut   = SampTimeList%dtSamp
          tSmpOut    = tStartSamp + dtSmpOut
        ELSE
          tSmpOut = SampTimeList%tStart
        END IF
      END IF
    ELSE
      IF( lAvg )THEN  !No more output times
        tStartSamp = HUGE(0.)
        tSmpOut    = HUGE(0.)
      END IF
    END IF
  END IF

END IF

!------ Reset integrated met sensors

DO is = 1,nsmp
  IF( BTEST(smp(is)%stype,STB_MET) .AND. BTEST(smp(is)%stype,STB_INTEGRATE) )THEN
    IF( BTEST(smp(is)%stype,STB_TURB) .AND. smp(is)%dsmp(7) > 0. ) &
                                            smp(is)%dsmp(7) = 1./smp(is)%dsmp(7) !M-O length
    smp(is)%dsmp = smp(is)%dsmp * t
  END IF
END DO

lUpdateSum = .FALSE.

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ProcessSensor( ss,dsmp )

!------ Process sensor for final output, including converting number density

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi

IMPLICIT NONE

TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure
REAL, DIMENSION(:), POINTER    :: dsmp !Sensor output array

REAL    dtSmpOutSave
INTEGER ib, j
INTEGER nv, nvs

!------ Integrated met sensors

IF( BTEST(ss%stype,STB_MET) .AND. BTEST(ss%stype,STB_INTEGRATE) )THEN
  dsmp = dsmp / t
  IF( BTEST(ss%stype,STB_TURB) .AND. dsmp(7) > 0. )dsmp(7) = 1./dsmp(7) !M-O length
END IF

!------ Check for zero averaging time

IF( lAvg )THEN
  dtSmpOutSave = dtSmpOut
  IF( dtSmpOut == 0. )dtSmpOut = 1.
END IF

!------ By-size sensors

IF( BTEST(ss%stype,STB_PART) )THEN
  IF( lAvg .OR. BTEST(ss%stype,STB_INTEGRATE) .OR. BTEST(ss%stype,STB_LOS) )THEN  !"Standard" fields: mean, variance
    nv = 2
  ELSE
    nv = 3                                           ! + timescale (if not averaged/integrated)
  END IF
  nvs = nv
  nvs = nvs + 1                               !Additional field: number density

  DO ib = 1,ss%nbin
    j = (ib-1)*nvs
    IF( dsmp(j+1) > SMALL )THEN            !Divide by mass (for mass-weighted fields )
      dsmp(j+nv+1) = dsmp(j+nv+1) * 6./PI !Number density
      dsmp(j+1) = dsmp(j+1) * ss%conv                    !Mean
      dsmp(j+2) = dsmp(j+2) * ss%conv**2                 !Variance
      IF( lAvg )THEN
        dsmp(j+1) = dsmp(j+1) / dtSmpOut      !N.B. nv=2
        dsmp(j+2) = dsmp(j+2) / dtSmpOut**2
        dsmp(j+3) = dsmp(j+3) / dtSmpOut
      END IF
    END IF
  END DO

END IF


!------ Apply conversion factor and time-averaging

IF( .NOT.(BTEST(ss%stype,STB_PART) .OR. BTEST(ss%stype,STB_MET)) )THEN
  dsmp(1) = dsmp(1) * ss%conv
  dsmp(2) = dsmp(2) * ss%conv**2
END IF

IF( lAvg )THEN

  IF( .NOT.(BTEST(ss%stype,STB_INTEGRATE) .OR. BTEST(ss%stype,STB_DEP) .OR. BTEST(ss%stype,STB_PART)) )THEN
    dsmp = dsmp / dtSmpOut
    IF( .NOT.BTEST(ss%stype,STB_MET) )dsmp(2) = dsmp(2) / dtSmpOut
    IF( multicomp .AND. ss%nmc > 0 )THEN
      ss%asmp = ss%asmp / dtSmpOut
    END IF
  END IF

END IF

RETURN
END

!==============================================================================

SUBROUTINE SensorAutoGrid( ss )

!------ Define automatic grid based on contour level

USE scipuff_fi, ONLY: lmap
USE sampler_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagstr_fd
USE sagtri_fd
USE sagcnt_fd
USE field_fd
USE error_fi
USE constants_fd

IMPLICIT NONE

TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure

INTEGER grdI, irv, alloc_stat, i, is, nL, iL, npts
REAL    xmap, ymap, dx, dy, del, ds, xs0, ys0, xs, ys, xm, ym, c, s
REAL    xmin, xmax, ymin, ymax
REAL    sumL, sumX, sumY, sumXX, sumYY, sumXY

INTEGER, DIMENSION(1) :: ifld
REAL,    DIMENSION(2) :: fmax

TYPE( SAGcontour_str ) :: contour

TYPE ( SAGgrid_str ),     POINTER :: grd
TYPE( SAGTriangleT_str ), POINTER :: triT

TYPE( SCIPLineT ), DIMENSION(:), ALLOCATABLE :: Line

INTEGER, EXTERNAL :: CreateSensorSlice
INTEGER, EXTERNAL :: SAG_BottomMinMaxID, SAG_TrianglesID, SAG_NodesID, SAG_LastError, SAG_RmvGrdStr
INTEGER, EXTERNAL :: SAG_CountContourID, SAG_BuildContourID, SAG_SetSpecialValue
REAL,    EXTERNAL :: sind, cosd

!------ Initialize

irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

NULLIFY( contour%ippts,contour%iplns )

!------ Set grid ID

IF( BTEST(ss%stype,STB_DEP) )THEN
  grdI = depblk_smp(ss%is)%grdI

ELSE IF( BTEST(ss%stype,STB_INTGRID) )THEN
  grdI = dosblk_smp(ss%is)%grdI
ELSE    !Build concentration field
  grdI = CreateSensorSlice( ss )

END IF

IF( grdI <= 0 )THEN
  ss%nx = 0
  ss%ny = 0
  GOTO 9999
END IF

!------ Get field max value

irv = SAG_BottomMinMaxID( grdI,1,.TRUE.,fmax )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SensorAutoGrid'
  eMessage = 'Error computing Mean/Variance Min/Max'
  GOTO 9999
END IF

IF( fmax(2) < ss%autoLev )THEN        !No points above level: exit

  ss%nx = 0
  ss%ny = 0
  GOTO 9999

ELSE IF( fmax(1) > ss%autoLev )THEN   !All points above level: setup grid based on full domain & exit

  grd => SAG_PtrGrdStr( grdI )

  ss%x = grd%xmin  !Project coordinates
  ss%y = grd%ymin

  ss%x0 = ss%x     !To be converted to sensor coordinates, if necessary
  ss%y0 = ss%y

  del   = SQRT(FLOAT(ss%npts)/FLOAT(grd%nx*grd%ny))
  ss%nx = MAX(CEILING(del*grd%nx),2)
  ss%ny = MAX(CEILING(del*grd%ny),2)

  dx = grd%dx*(grd%nx-1)/(ss%nx-1)
  dy = grd%dy*(grd%ny-1)/(ss%ny-1)

  CALL mapfac( ss%x,ss%y,xmap,ymap )

  ss%dx = dx/xmap  !Convert to meters
  ss%dy = dy/ymap

  ss%dxs = ss%dx
  ss%dys = ss%dy

ELSE

!------ Setup triangles and nodes - Use grid coordinates for accuracy

  grd => SAG_PtrGrdStr( grdI )

  triT => SAG_PtrTriStr( grdI )

  ifld(1) = 1
  IF( .NOT.ASSOCIATED(triT%iptri) )THEN
    irv = SAG_TrianglesID( grdI,1,ifld,.TRUE. )
    IF( irv /= SAG_OK )THEN
      nError = SAG_LastError( eInform,eAction )
      IF( nError == NO_ERROR )nError = UK_ERROR
      eRoutine = 'SensorAutoGrid'
      eMessage = 'Error computing field triangles'
      GOTO 9999
    END IF
  ELSE
    irv = SAG_NodesID( grdI,1,ifld )
    IF( irv /= SAG_OK )THEN
      nError = SAG_LastError( eInform,eAction )
      IF( nError == NO_ERROR )nError = UK_ERROR
      eRoutine = 'SensorAutoGrid'
      eMessage = 'Error computing field triangles node values'
      GOTO 9999
    END IF
  END IF

!------ Count number of points in contour

  contour%mxpts = 0
  contour%mxlns = 0

  CALL SAG_InitContour( contour,1,ss%autoLev,.TRUE.,.TRUE. )  !Log interpolation and closed contours

  irv = SAG_CountContourID( grdI,contour )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'SensorAutoGrid'
    eMessage = 'Error counting contour points/lines'
    GOTO 9999
  END IF

  IF( contour%npts <= 0 )THEN  !Should not occur
    ss%nx = 0
    ss%ny = 0
    GOTO 9999
  END IF

!------ Allocate contour arrays

  ALLOCATE( contour%ippts(contour%npts),contour%iplns(contour%nlns),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SensorAutoGrid'
    eMessage = 'Error allocating contour points/lines'
    GOTO 9999
  END IF

  contour%mxpts = contour%npts
  contour%mxlns = contour%nlns

!------ Build contour

  irv = SAG_BuildContourID( grdI,contour )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'SensorAutoGrid'
    eMessage = 'Error building contour'
    GOTO 9999
  END IF

  CALL check_progress()

!------ Allocate arrays for each closed contour

  nL = contour%nlns

  ALLOCATE( Line(nL),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SensorAutoGrid'
    eMessage = 'Error allocating arrays for computing contour rotation'
    GOTO 9999
  END IF

  is = 1
  xs0 = contour%ippts(1)%x   !Reference point in grid coordinates
  ys0 = contour%ippts(1)%y

  xm = grd%xmin + grd%dx*xs0 !Convert to project coordinates
  ym = grd%ymin + grd%dy*ys0

  CALL mapfac( xm,ym,xmap,ymap )

  sumL  = 0.
  sumX  = 0.; sumY  = 0.
  sumXX = 0.; sumYY = 0.; sumXY = 0.

!------ Loop over contours and compute line integrals

  DO iL = 1,nL

    IF( iL == nL )THEN
      npts = contour%npts - contour%iplns(iL) + 1
    ELSE
      npts = contour%iplns(iL+1) - contour%iplns(iL)
    END IF
    Line(iL)%number = npts

    IF( iL > 1 )is = Line(iL-1)%start + Line(iL-1)%number

    Line(iL)%start = is

    xm = grd%dx*(contour%ippts(is)%x - xs0)/xmap; ym = grd%dy*(contour%ippts(is)%y - ys0)/ymap

    DO i = is+1,is+npts-1
      xs = grd%dx*(contour%ippts(i)%x - xs0)/xmap
      ys = grd%dy*(contour%ippts(i)%y - ys0)/ymap
      ds = SQRT((xs-xm)**2+(ys-ym)**2)
      sumL  = sumL  + ds
      sumX  = sumX  + ds*xs
      sumY  = sumY  + ds*ys
      sumXX = sumXX + ds*xs*xs
      sumYY = sumYY + ds*ys*ys
      sumXY = sumXY + ds*xs*ys
      xm = xs; ym = ys
    END DO

  END DO

!------ Compute second-moments

  sumX  = sumX  / sumL
  sumY  = sumY  / sumL
  sumXX = sumXX / sumL - sumX**2
  sumYY = sumYY / sumL - sumY**2
  sumXY = sumXY / sumL - sumX*sumY

!------ Rotate to eliminate off-diagonal term for elliptical contour
!       Set to nearest degree

  ss%rot = 0.5*ATAN2(2.*sumXY,sumXX-sumYY) / PI180
  ss%rot = NINT(ss%rot/1.) * 1.

  c = cosd( ss%rot )
  s = sind( ss%rot )

!------ Loop over contour points; find min/max in rotated coord

  xm = 0.
  ym = 0.

  xmin = xm
  xmax = xm
  ymin = ym
  ymax = ym

  DO i = 2,contour%npts
    xs = grd%dx*(contour%ippts(i)%x - xs0)/xmap
    ys = grd%dy*(contour%ippts(i)%y - ys0)/ymap
    xm =  xs*c + ys*s
    ym = -xs*s + ys*c
    xmin = MIN(xmin,xm)
    xmax = MAX(xmax,xm)
    ymin = MIN(ymin,ym)
    ymax = MAX(ymax,ym)
  END DO

  dx  = xmax - xmin
  dy  = ymax - ymin
  del = SQRT(dx*dy/FLOAT(ss%npts))

  ss%nx = MAX(INT(dx/del)+1,2)
  ss%ny = MAX(INT(dy/del)+1,2)

  ss%dx = dx/FLOAT(ss%nx-1); ss%dxs = ss%dx
  ss%dy = dy/FLOAT(ss%ny-1); ss%dys = ss%dy

  ss%x = grd%xmin + grd%dx*xs0 + (xmin*c - ymin*s)*xmap
  ss%y = grd%ymin + grd%dy*ys0 + (xmin*s + ymin*c)*ymap

  ss%x0 = ss%x
  ss%y0 = ss%y

END IF

!------ Convert to (optional) sensor coordinates

IF( lSmpCnv )THEN
  CALL ConvertSampCoord( lmap,sampMap,sampZone,ss%x0,ss%y0 )
  IF( nError /= NO_ERROR )GOTO 9999
  IF( lMap == I_CARTESIAN .AND. sampMap == I_CARTESIAN .AND. lCartOrig )THEN
    ss%x0 = ss%x0 + cartX0
    ss%y0 = ss%y0 + cartY0
  END IF
END IF

IF( sampMap == I_UTM )THEN
  ss%x0 = ss%x0 / UTMfac
  ss%y0 = ss%y0 / UTMfac
END IF

!------ Setup dsmp array

IF( ASSOCIATED(ss%gsmp) )DEALLOCATE( ss%gsmp,STAT=alloc_stat )

ALLOCATE( ss%gsmp(ss%nx,ss%ny,ss%nvar),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SensorAutoGrid'
  eMessage = 'Error allocating grid output array'
  GOTO 9999
END IF

9999 CONTINUE

IF( ASSOCIATED(contour%ippts) )DEALLOCATE( contour%ippts,STAT=alloc_stat )
IF( ASSOCIATED(contour%iplns) )DEALLOCATE( contour%iplns,STAT=alloc_stat )

IF( ALLOCATED(Line) )DEALLOCATE( Line,STAT=alloc_stat )

IF( .NOT.(BTEST(ss%stype,STB_DEP) .OR. BTEST(ss%stype,STB_INTGRID)) )THEN
  IF( grdI /= -1 )irv = SAG_RmvGrdStr( grdI )
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION CreateSensorSlice( ss ) RESULT( grdI )

!------ Create slice for auto grid

USE scipuff_fi
USE sampler_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagtri_fd
USE sagcnt_fd
USE field_fd
USE surface_fi
USE slice_fd
USE srfparam_fd
USE plotlist_fi

IMPLICIT NONE

TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure

TYPE( sfield_block ), DIMENSION(1) :: sblk  !Surface block structure

TYPE( sfield_puff ), DIMENSION(:),ALLOCATABLE :: spuff !Puff-to-surface structure

TYPE( slice_str ) :: slice !Slice definition

INTEGER alloc_stat, iSensor, imat, icls, igrp, i
LOGICAL lMemoryField
REAL    z_dosageSave

INTEGER, DIMENSION(3) :: stype ! dezone types

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

!------ Set MemoryField TRUE for creating plot slices; save current value

lMemoryField = MemoryField
MemoryField  = .TRUE.

!------ Set sensor-type id

SELECT CASE( TRIM(ss%type) )
  CASE( 'CONC','CONC_ND', &
        'BY_SIZE' ,'BY_SIZE_ND',  &
        'BYSIZE'  ,'BYSIZE_ND' ,  &
        'PARTICLE','PARTICLE_ND' )
    iSensor = IS_CONC
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'CreateSensorSlice'
    eMessage = 'Invalid sensor type for gridded output'
    GOTO 9999
END SELECT

!------ Set default domain

slice%xmin = DEF_VAL_R
slice%ymin = DEF_VAL_R
slice%xmax = DEF_VAL_R
slice%ymax = DEF_VAL_R

slice%xminS = NOT_SET_R
slice%yminS = NOT_SET_R
slice%xmaxS = NOT_SET_R
slice%ymaxS = NOT_SET_R

slice%maxcell = MAXSG
slice%maxlev  = 99

slice%time = t

slice%data(SD_BASEGRID) = FLOAT(10)

!------ Set number of blocks and fields

slice%nblk = 1
slice%nfld = 3
slice%cat  = HP_SSLICE

!------ Set dezone types

stype(1) = DEZONE_MEAN
stype(2) = DEZONE_VAR
stype(3) = DEZONE_SCALE

!------ Create concentration slice

  imat = typeID(ss%is)%imat
  icls = material(imat)%icls
  IF( IsGas(icls) .OR. IsAerosol(icls) )THEN
    igrp = 0
  ELSE IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN
    IF( ss%ie > ss%is )THEN
      igrp = 0
    ELSE
      igrp = ss%is - material(imat)%ioffp
    END IF
  ELSE IF( IsLiquid(icls) )THEN
    isg = ss%is - material(imat)%ioffp
    IF( isg == 1 )THEN
      IF( ss%ie == ss%is )THEN
        igrp = 1
      ELSE
        igrp = 0
      END IF
    ELSE
      igrp = 2
    END IF
  END IF

  sblk(1)%type  = SBLK_PLOT
  sblk(1)%field = 1
  sblk(1)%id    = imat + 65536*igrp
  sblk(1)%flags = 0
  sblk(1)%name  = ' '

  ALLOCATE( spuff(ntypp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CreateSensorSlice'
    eMessage = 'Error allocating puff type array'
    GOTO 9999
  END IF

  DO i = 1,ntypp
    spuff(i)%nblocks = 0
    spuff(i)%icld    = 0
    ALLOCATE( spuff(i)%iblk(1),STAT=alloc_stat )
  END DO

!------ Initialize appropriate puffs for slice field

  IF( igrp == 0 )sblk(1)%flags = IBSET(sblk(1)%flags,SFLAG_TOT)

  CALL init_spuff( spuff,sblk(1),1,material(imat),igrp )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Set Total flag if necessary

  IF( igrp == 0 )sblk(1)%flags = IBSET(sblk(1)%flags,SFLAG_TOT)

!------ Create slice field

  z_dosageSave = z_dosage   !Temporarily re-define z_dosage
  z_dosage     = ss%z

  CALL CreateSlice( sblk,spuff,stype,slice,grdI )
  IF( nError /= NO_ERROR )GOTO 9999

  z_dosage = z_dosageSave


9999 CONTINUE

IF( nError /= 0 )grdI = -1

IF( ALLOCATED(spuff) )THEN
  DO i = 1,ntypp
    IF( ASSOCIATED(spuff(i)%iblk) )DEALLOCATE( spuff(i)%iblk,STAT=alloc_stat )
  END DO
  DEALLOCATE( spuff,STAT=alloc_stat )
END IF

MemoryField = lMemoryField

RETURN
END

!==============================================================================

SUBROUTINE UnProcessSensor( ss,dsmp )

!------ Undo processing

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi

IMPLICIT NONE

TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure
REAL, DIMENSION(:), POINTER    :: dsmp !Sensor output array

INTEGER nvs
INTEGER j, is

IF( lAvg )THEN

  IF( BTEST(ss%stype,STB_INTEGRATE) )THEN
    dsmp(1) = dsmp(1) / ss%conv
    dsmp(2) = dsmp(2) / ss%conv**2
  ELSE
    dsmp = 0.
  IF( multicomp .AND. ss%nmc > 0 )ss%asmp = 0.
  END IF

ELSE
  IF(  BTEST(ss%stype,STB_PART) )THEN
    nvs = 4
    DO is = 1,ss%nbin
      j = (is-1)*nvs
      dsmp(j+1) = dsmp(j+1) / ss%conv
      dsmp(j+2) = dsmp(j+2) / ss%conv**2
    END DO

  ELSE IF( .NOT.BTEST(ss%stype,STB_MET) )THEN

    dsmp(1) = dsmp(1) / ss%conv
    dsmp(2) = dsmp(2) / ss%conv**2

  END IF
END IF

RETURN
END

!==============================================================================

SUBROUTINE sum_sensor( ss,iSensor,p,csmp,tfac )

!------ Sum puff contribution csmp to sensor output

USE scipuff_fi
USE sampler_fi
USE chem_fi
USE met_fi, ONLY: tb,pb,hb

IMPLICIT NONE

TYPE( sensor ),     INTENT( INOUT ) :: ss      !Sensor structure
INTEGER,            INTENT( IN    ) :: iSensor !Sensor type
TYPE( puff_str ),   INTENT( IN    ) :: p       !Puff structure
REAL, DIMENSION(*), INTENT( INOUT ) :: csmp    !Concentration mean,
                                               !Variance & scale
REAL,               INTENT( IN    ) :: tfac    !Time-integration factor

REAL, PARAMETER :: F_FAC = 1.E-3

INTEGER i, ityp
INTEGER ioff
REAL    sl

!------ Multiply variance factor by puff timescale for integrated output

  IF( ss%speed > 0. )THEN  !Scale for moving sensor
    sl = (ss%mvlos%lx**2+ss%mvlos%ly**2)*p%si + ss%mvlos%lz**2*p%sv !CL length scale
    sl = 2.*sl/ss%speed
  ELSE
    sl = HUGE(0.)
  END IF
  IF( lAvg .AND. dtSmpOut > 0. )THEN
    csmp(2) = csmp(2)*MIN(p%sr,2.0*(dtSmpOut-tSmpOut+t),sl)
  ELSE IF( BTEST(ss%stype,STB_INTEGRATE) )THEN
    csmp(2) = csmp(2)*p%sr !MIN(p%sr,sl)
  END IF

!------ Add puff contribution (depending on sensor type)

ityp = p%ityp                 !Puff type

SELECT CASE( iSensor )

!------ Concentration

  CASE( IS_CORR )

    DO i = 1,MIN(ss%nvar,3)
      ss%dsmp(i) = ss%dsmp(i) + tfac*csmp(i)
    END DO

  CASE( IS_CONC )

    IF( BTEST(ss%stype,STB_MULT) .AND. (lAvg .OR. BTEST(ss%stype,STB_INTEGRATE)) )THEN
      DO i = 1,2
!$OMP ATOMIC
        ss%dsmp(i) = ss%dsmp(i) + tfac*csmp(i)
      END DO
      DO i = 1,ss%nmc
        IF( chem%oUnits == UNIT_UGM3 .AND. &
            chem%species(ss%mcID(i))%class /= ID_SPECIES_PARTICLE )THEN
            ss%dsmp(2+i) = ss%dsmp(2+i) + tfac*csmp(i+3)*PPM2UGM3*chem%species(ss%mcID(i))%MW
        ELSE
          ss%dsmp(2+i) = ss%dsmp(2+i) + tfac*csmp(i+3)
        END IF
      END DO
    ELSE
      ioff = ss%nvar-ss%nmc
      DO i = 1,ioff
        ss%dsmp(i) = ss%dsmp(i) + tfac*csmp(i)
      END DO
      DO i = 1,ss%nmc
        IF( chem%oUnits == UNIT_UGM3 .AND. &
            chem%species(ss%mcID(i))%class /= ID_SPECIES_PARTICLE )THEN
          ss%dsmp(ioff+i) = ss%dsmp(ioff+i) + tfac*csmp(ioff+i)*PPM2UGM3*chem%species(ss%mcID(i))%MW
        ELSE
          ss%dsmp(ioff+i) = ss%dsmp(ioff+i) + tfac*csmp(ioff+i)
        END IF
      END DO
    END IF

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE sum_bysize_sensor( ss,p,csmp,tfac )

!------ Sum puff contribution csmp to output for size-based sensors (liquid, wet & dry particles)

USE scipuff_fi
USE sampler_fi
USE met_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( sensor ),     INTENT( INOUT ) :: ss      !Sensor structure
TYPE( puff_str ),   INTENT( IN    ) :: p       !Puff structure
REAL, DIMENSION(3), INTENT( INOUT ) :: csmp    !Concentration mean,variance & scale
REAL,               INTENT( IN    ) :: tfac    !Time-integration factor

TYPE( puff_liquid )     :: pq
TYPE( liquid_material ) :: pmatl
TYPE( part_material   ) :: partmatl

INTEGER i, imat, iclass, i0, nv, nb, ib
REAL    rhod, vpart, vliq, vtot, mass, db, sl
LOGICAL lWet, lLiq

REAL,    EXTERNAL :: fun_rhoa, ufall
LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsLiquid

!------ Multiply variance factor by puff timescale for integrated output

IF( ss%speed > 0. )THEN  !Scale for moving sensor
  sl = (ss%mvlos%lx**2+ss%mvlos%ly**2)*p%si + ss%mvlos%lz**2*p%sv !CL length scale
  sl = 2.*sl/ss%speed
ELSE
  sl = HUGE(0.)
END IF

IF( BTEST(ss%stype,STB_LOS) )THEN
  nv = 2
ELSE IF( lAvg )THEN
  csmp(2) = csmp(2)*MIN(p%sr,2.0*(dtSmpOut-tSmpOut+t),sl)
  nv = 2
ELSE IF( BTEST(ss%stype,STB_INTEGRATE) )THEN
  nv = 2
  csmp(2) = csmp(2)*MIN(p%sr,sl)
ELSE
  nv = 3
END IF

nb = ss%nbin

!------ Get material properties

imat   = typeID(p%ityp)%imat
iclass = typeID(p%ityp)%icls

lWet = .FALSE.
lLiq = .FALSE.

IF( IsLiquid(iclass) )THEN
  CALL GetLiquidParam( pmatl,material(imat)%iaux,typeID(p%ityp)%igrp,mat_aux )
  CALL get_liquid( p,pq )
  db   = pq%d
  lLiq = .TRUE.
ELSE
  CALL GetParticleParam( partmatl,material(imat)%iaux,typeID(p%ityp)%igrp,mat_aux )
  IF( IsWetParticle(iclass) )THEN
    CALL get_liquid( p,pq )
    db   = pq%d
    lWet = .TRUE.
  ELSE
    db = partmatl%dbar
  END IF
END IF

!------ Exit if particle/droplet not in range of bin

IF( db < ss%bin(1) .OR. db > ss%bin(nb+1) )RETURN

!------ Compute particle/droplet density, mass

IF( lWet )THEN
  vpart = partmatl%dbar**3
  vtot  = db**3
  vliq  = vtot - vpart
  mass  = partmatl%rho*vpart
ELSE IF( lLiq )THEN
  rhod  = pmatl%rho - pmatl%rhob*(pq%t+ABSZERO)
  mass  = rhod * db**3  !PI/6 factor applied upon output
ELSE
  rhod = partmatl%rho
  mass = rhod * db**3  !PI/6 factor applied upon output
END IF

!------ Find appropriate output bin

ib = 0
DO i = 2,nb+1
  IF( db < ss%bin(i) )THEN
    ib = i - 1
    EXIT
  END IF
END DO
IF( ib == 0 )ib = nb

!------ Add to sums in appropriate bin

  i0 = (ib-1)*(nv+1)        !Offset for {C,Var,[Tscale],number density}

!------ Concentration mean, variance and timescale

DO i = 1,nv
!$OMP ATOMIC
  ss%dsmp(i0+i) = ss%dsmp(i0+i) + tfac*csmp(i)
END DO

!------ Add number density (w/o PI/6 factor) to mass-weighted sum

!$OMP ATOMIC
   ss%dsmp(i0+nv+1) = ss%dsmp(i0+nv+1) + tfac*csmp(1)/mass

RETURN
END

