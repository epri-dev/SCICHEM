SUBROUTINE new_start()

USE scipuff_fi

IMPLICIT NONE

INTEGER alloc_stat

CHARACTER(24),EXTERNAL :: sysGetDate
INTEGER,      EXTERNAL :: allocatePuffs

!-----  set audit create time and version

audit_date = sysGetDate()
CALL set_version_string( audit_version )

!-----  set/read program parameters

CALL set_param()
IF( nError /= NO_ERROR )GOTO 9999

!-----  compute gravitational settling velocities

CALL set_vdep()
IF( nError /= NO_ERROR )GOTO 9999

!------ Initialize time-level lists

CALL set_tlev(.TRUE.)

!------ Allocate puff space

alloc_stat = allocatePuffs( MAXPUF )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'new_start'
  eMessage = 'Error allocating puff array'
  WRITE(eInform,'(A,I10)') 'Requested size =',MAXPUF
  GOTO 9999
END IF

delt_static  = HUGE(0.)
t_rst_static = NOT_SET_R

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE set_param()

USE scipuff_fi
USE met_fi
USE files_fi
USE sampler_fi
USE UtilMtlAux

IMPLICIT NONE

INTEGER i, ios
REAL    xmid, res, resmin, defdom

LOGICAL, EXTERNAL :: IsEvap
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: julian_day

!------ initialize parameters, set defaults, etc.

CALL init_param()
CALL init_param_met()

!-----  program input  -----

CALL ReadNamelistFlags( lun_inp )
IF( nError /= NO_ERROR )GOTO 9997

CALL ReadNamelistDomain( lun_inp )
IF( nError /= NO_ERROR )GOTO 9997

CALL ReadNamelistOptions( lun_inp )
IF( nError /= NO_ERROR )GOTO 9997

IF( run_mode > 0 )WRITE(lun_log,*,IOSTAT=ios)

!------ set default vertical grid if required

IF( BTEST(run_mode,FAST_MODE) )THEN
  res    = 7.
  resmin = 1000.
  defdom = 5000.
  simrge = 1.50
  WRITE(lun_log,'(A)',IOSTAT=ios) 'Running in FAST mode'
ELSE
  res    = 15.
  resmin = 250.
  defdom = 2500.
  simrge = 1.25
END IF

IF( zmax == DEF_VAL_R )zmax = defdom
IF( vres == DEF_VAL_R )vres = MAX(resmin,zmax/res)

IF( BTEST(run_mode,REVERSE_MODE) )WRITE(lun_log,'(A)',IOSTAT=ios) 'Running in REVERSE mode'

!------ set dynamic flag if dense gas flag is set

dynamic = dense_gas .OR. dynamic

!------ met input parameter checks

IF( sl_calm <= 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'SL_CALM must be greater than 0'
  eInform  = 'Resetting to 1000m'
  CALL WarningMessage( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
  sl_calm = 1000.
END IF

IF( uu_calm < 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'UU_CALM cannot be less than 0'
  eInform  = 'Resetting to (0.5m/s)**2'
  CALL WarningMessage( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
  uu_calm = 0.25
END IF

IF( sltrop <= 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'SLTROP must be greater than 0'
  eInform  = 'Resetting to 10m'
  CALL WarningMessage( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
  sltrop = 10.
END IF

IF( wwtrop < 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'WWTROP cannot be less than 0'
  eInform  = 'Resetting to (0.1m/s)**2'
  CALL WarningMessage( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
  wwtrop = 0.01
END IF

IF( epstrop < 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'set_param'
  eMessage = 'EPSTROP cannot be less than 0'
  eInform  = 'Resetting to 4e-4 m**2/s**3'
  CALL WarningMessage( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
  epstrop = 4.E-4
END IF

!------ Check start year-month-day

IF( year_start /= NOT_SET_I .AND. month_start /= NOT_SET_I &
      .AND. day_start /= NOT_SET_I )THEN
  lymd = .TRUE.
  CALL set_year( year_start )
  jul_start = julian_day( month_start,day_start,year_start )

ELSE IF( year_start /= NOT_SET_I .OR. month_start /= NOT_SET_I &
             .OR. day_start /= NOT_SET_I )THEN
  nError   = UK_ERROR
  eMessage = 'Must set YEAR_START, MONTH_START and DAY_START'
  GOTO 9997

ELSE

  lymd = .FALSE.
  jul_start = NOT_SET_I

END IF

!------ Check time zone

IF( tzone /= DEF_VAL_R .AND. tzone /= NOT_SET_R )THEN
  CALL fix_tzone( tzone )

ELSE IF( ((lmap == I_CARTESIAN) .OR. (lmap == I_UTM)) .AND. lon0 /= NOT_SET_R .AND. lon0 /= DEF_VAL_R )THEN
  tzone = FLOAT(INT(lon0+7.5)/15)
  IF( lon0 < -7.5 )tzone = tzone - 1.
  WRITE(lun_log,'(A,F4.1)',IOSTAT=ios)'**** Reset time zone to ',tzone

ELSE IF( lmap == I_LATLON .AND. xmin /= DEF_VAL_R .AND. xmax /= DEF_VAL_R )THEN
  xmid  = 0.5*(xmin+xmax)
  tzone = FLOAT(INT(xmid+7.5)/15)
  IF( xmid < -7.5 )tzone = tzone - 1.
  WRITE(lun_log,'(A,F4.1)',IOSTAT=ios)'**** Reset time zone to ',tzone

ELSE
  tzone = DEF_VAL_R

END IF

!------ Initialize run time, puffs and other parameters

npuf     = 0
t        = 0.
t_save   = 0.
t_old_r  = -999.
surface  = .FALSE.
nsmp     = 0
tLastDep = 0.
tLastDos = 0.

!------ Initialize time if restarting from another project

IF( file_rst /= ' ' )THEN
  IF( time_rst < 0. )THEN
    nError   = UK_ERROR
    eMessage = 'Restart time cannot be negative'
    GOTO 9997
  END IF
  t       = time_rst
  t_save  = time_rst
  t_old_r = time_rst
END IF

!------ Initialize material multicomponent list

CALL ClearMClist()

!------ Read material description data

nError = NO_ERROR
DO WHILE( nError == NO_ERROR )
  CALL ReadNamelistMatdef( lun_inp )
  IF( nError == NO_ERROR )THEN
    CALL PutMaterial()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO
IF( nError == EOF_ERROR )THEN
  CALL init_error()
ELSE
  GOTO 9999
END IF

CALL SetTypeID()
IF( nError /= NO_ERROR )GOTO 9999

IF( ntypm <= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'No material definitions found'
  GOTO 9997
ELSE
  CALL check_units( ntypm,material,mat_aux )
  IF( nError /= NO_ERROR )GOTO 9999
  DO i = 1,ntypm
    IF( IsEvap(material(i)%icls) )THEN
      IF( material(i)%prop(3) == NOT_SET_R .OR. &
          material(i)%prop(3) == DEF_VAL_R )THEN
        material(i)%prop(3) = 0.0
      END IF
    END IF
  END DO
END IF

!------ Set buoyant gas flags

CALL set_buoy_flags()

!------ set puff%aux skips

CALL set_auxskp()

!------ adjust puff auxiliary count for buoyant gases

IF( buoy_gas )THEN
  DO i = 1,ntypp
    typeID(i)%npaux = typeID(i)%npaux + NAUX_BUOY
    IF( IsMulti(typeID(i)%icls) )typeID(i)%ipmc = typeID(i)%ipmc + NAUX_BUOY
  END DO
END IF

surface = ntyps > 0
dose    = ntypd > 0

numRelID = 0

9999 CONTINUE

RETURN

!------ set namelist error and goto return

9997 CONTINUE
eRoutine = 'set_param'
IF( eInform == CHAR(0) )CALL ReportFileName( eInform,'File=',file_inp )
GOTO 9999

END

!=======================================================================

SUBROUTINE set_vdep()

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER i, icls, ityp, jtyp, nsg
REAL    rhodst, dlb, dub, dm, vmin, vbar, vmax, fmin, fbar, fmax, fsum
REAL    vdep, vd2, svdep
REAL    aspect, sigd, st, tem

TYPE( puff_material ) pmat
TYPE( part_material ) pmatpart
TYPE( liquid_material ) pmatliq

REAL,    EXTERNAL :: ufall, SetparticleDiff
LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsWetParticle, IsLiquid

DO ityp = 1,ntypm

  icls = material(ityp)%icls

  IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN

    jtyp = material(ityp)%ioffp + 1
    CALL get_puff_material( jtyp,pmat )
    pmatpart = TRANSFER(pmat,pmatpart)

    nsg    = pmatpart%nsg
    rhodst = pmatpart%rho

    DO i = 1,nsg
      jtyp = material(ityp)%ioffp + i
      CALL get_puff_material( jtyp,pmat )
      pmatpart = TRANSFER(pmat,pmatpart)

      IF( pmatpart%dbar <= 0.0 )THEN
        pmatpart%dbar = 0.5*(pmatpart%dmin+pmatpart%dmax)
      END IF

      dlb = pmatpart%dmin
      dub = pmatpart%dmax
      dm  = pmatpart%dbar

      IF( dlb > 0.0 )THEN
        vmin  = ufall( rhoair,rhodst,rmuair,dlb )
        vbar  = ufall( rhoair,rhodst,rmuair,dm  )
        vmax  = ufall( rhoair,rhodst,rmuair,dub )
        fmin  = 1.0/dlb
        fbar  = 1.0/dm
        fmax  = 1.0/dub
        fsum  = fmin + fbar + fmax
        vdep  = (fmin*vmin    + fbar*vbar    + fmax*vmax)/fsum
        vd2   = (fmin*vmin**2 + fbar*vbar**2 + fmax*vmax**2)/fsum
        svdep = SQRT(ABS(vd2 - vdep**2))
        IF( svdep > vdep/3.0 )THEN
          svdep = vdep/3.0
          WRITE(lun_log,'(A,2I4)',ERR=9998) &
                          'Resetting svdep for group ',ityp,i
        END IF
        pmatpart%vd    = vdep
        pmatpart%sigvd = svdep
        pmatpart%diff  = SetParticleDiff( dm,rnu )
        WRITE(lun_log,'(2I4,1P4G11.4)',ERR=9998)ityp,i,dm*1.E6,vdep,svdep,pmatpart%diff
      ELSE
        pmatpart%vd    = 0.
        pmatpart%sigvd = 0.
        pmatpart%diff  = 1.E-20
      END IF
      pmat = TRANSFER(pmatpart,pmat)
      CALL put_puff_material( jtyp,pmat )
    END DO

  ELSE IF( IsLiquid(icls) )THEN

    jtyp = material(ityp)%ioffp + 2
    CALL get_puff_material( jtyp,pmat )
    pmatliq = TRANSFER(pmat,pmatliq)

    nsg    = pmatliq%nsg
    rhodst = pmatliq%rho
    st     = pmatliq%st

    DO i = 1,nsg
      jtyp = material(ityp)%ioffp + i + 1
      CALL get_puff_material( jtyp,pmat )
      pmatliq = TRANSFER(pmat,pmatliq)
      IF( pmatliq%dbar <= 0.0 )THEN
        pmatliq%dbar = 0.5*(pmatliq%dmin + pmatliq%dmax)
      END IF

      dlb = pmatliq%dmin
      dub = pmatliq%dmax
      dm  = pmatliq%dbar
      IF( dm > 0.0 )THEN
        CALL drop_deform( rhodst,rhoair,dlb,st,aspect )
        tem = rhodst/(aspect*aspect)
        vmin = ufall( rhoair,tem,rmuair,dlb )
        CALL drop_deform( rhodst,rhoair,dm,st,aspect )
        tem = rhodst/(aspect*aspect)
        vbar = ufall( rhoair,tem,rmuair,dm )
        CALL drop_deform( rhodst,rhoair,dub,st,aspect )
        tem = rhodst/(aspect*aspect)
        vmax = ufall( rhoair,tem,rmuair,dub )
        fmin = 1.0/MAX(dlb,0.1*dm)
        fbar = 1.0/dm
        fmax = 1.0/dub
        fsum = fmin + fbar + fmax
        vdep = (fmin*vmin    + fbar*vbar    + fmax*vmax)/fsum
        vd2  = (fmin*vmin**2 + fbar*vbar**2 + fmax*vmax**2)/fsum
        sigd = (dub-dlb)/(2.*SQRT3)
        svdep= SQRT(ABS(vd2 - vdep**2))
        IF( svdep > vdep/3.0 )THEN
          svdep = vdep/3.0
          WRITE(lun_log,'(A,2I4)',ERR=9998) &
                          'Resetting svdep for group ',ityp,i
        END IF
        pmatliq%sigvd = svdep/vdep/sigd
        pmatliq%diff  = SetParticleDiff( dm,rnu )
        WRITE(lun_log,*,ERR=9998)ityp,i,dm*1.E6,vdep,svdep
      ELSE
        pmatliq%diff  = 1.E-20
        pmatliq%sigvd = 0.
      END IF
      pmat = TRANSFER(pmatliq,pmat)
      CALL put_puff_material( jtyp,pmat )
    END DO

  END IF

END DO

9999 CONTINUE

RETURN

!------ set log write error and goto return

9998 CONTINUE

nError   = WR_ERROR
eRoutine = 'set_vdep'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

END

!=======================================================================

SUBROUTINE create_output()

USE scipuff_fi
USE files_fi

IMPLICIT NONE

!------ write out saved common variables to project file

CALL write_prj()
IF( nError /= NO_ERROR )GOTO 9999

!------ open puff file

CALL CreatePuffFile()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!===============================================================================

MODULE special_rst_fi

  USE SWIMgridStr_fd

  IMPLICIT NONE

  SAVE
  LOGICAL interp
  INTEGER n_rst
  INTEGER numOld
  REAL    tCurrent
  TYPE( SWIMgridStr ), DIMENSION(:), ALLOCATABLE, TARGET :: OldGrid

END MODULE special_rst_fi

!===============================================================================

SUBROUTINE puff_rst()

USE scipuff_fi
USE met_fi
USE files_fi
USE cont_rel_fi
USE SWIMparam_fd
USE special_rst_fi

IMPLICIT NONE

INTEGER ios, ipuf, i, j, nxyb_old, maxPath
INTEGER lmap_old, mode_old, zone_old, nNWPN_old, irv, nn
INTEGER idum, ifld, ix, is, i0
REAL    dum, h, hx, hy, xp, yp, dss, dss0, dssMin
REAL    xc, yc, xmap, ymap, xmx, ymx, xmo, ymo
REAL    lon0_old, lat0_old, xref_old, yref_old
LOGICAL lter_old, ldum

REAL, DIMENSION(:), POINTER :: ptrI

INTEGER(KIND=1) :: AuxCoord(SIZE_MapCoordPrjTransfer)

TYPE( SWIMgridStr ), POINTER :: grd

CHARACTER(PATH_MAXLENGTH) fdum
CHARACTER(64)             fdum64

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTERFACE
  SUBROUTINE get_topog_interp( grid,Hi,x,y,ho )
    USE SWIMgridStr_fd
    TYPE( SWIMgridStr ),  INTENT( IN  ) :: grid
    REAL, DIMENSION(:),   POINTER       :: Hi
    REAL,                 INTENT( IN  ) :: x,y
    REAL,                 INTENT( OUT ) :: ho
  END SUBROUTINE get_topog_interp
END INTERFACE

INTEGER, EXTERNAL :: getPuffifld, SWIMputMixingHt, SWIMcnvCoord
INTEGER, EXTERNAL :: InitLambert, InitPolar, InitMercator, InitRotPolar, InitRotLL
REAL,    EXTERNAL :: RateMyParent

!------ Check version number on old project file

file_tmp = AddExtension( file_rst,'prj' )
CALL AddPath( file_tmp,path_rst )

OPEN(UNIT=lun_tmp,FILE=file_tmp,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Error opening project file for restarting'
  CALL ReportFileName( eInform,'File=',file_tmp )
  eAction  = 'Make sure file exists and is a valid project file'
  GOTO 9999
END IF

READ(lun_tmp,ERR=9997,END=9997) iversion
IF( iversion/100 < iversion_code/100 )THEN
  nError   = IV_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Project is an old version - cannot be restarted'
  CALL ReportFileName( eInform,'File=',file_tmp )
  GOTO 9999
END IF

!------ open puff file from old project

file_tmp = AddExtension( file_rst,'puf' )
CALL AddPath( file_tmp,path_rst )

CALL read_puff_rst( lun_tmp,file_tmp  )
IF( nError /= NO_ERROR )GOTO 9999

!------ Clear all static puffs for restart

CALL restart_static_puffs()

lzi_prj = .TRUE.

!------ open old project file

file_tmp = AddExtension( file_rst,'prj' )
CALL AddPath( file_tmp,path_rst )

OPEN(UNIT=lun_tmp,FILE=file_tmp,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Error opening project file for restarting'
  CALL ReportFileName( eInform,'File=',file_tmp )
  eAction  = 'Make sure file exists and is a valid project file'
  GOTO 9999
END IF

DO i = 1,numOld
  nn = OldGrid(i)%nx*OldGrid(i)%ny
  ALLOCATE( OldGrid(i)%H(nn),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'puff_rst'
    eMessage = 'Error allocating old met grid H array'
    WRITE(eInform,'(A,I10)') 'Requested size =',nn
    GOTO 9999
  END IF
END DO

!------ get path length from header record

READ(lun_tmp,ERR=9997,END=9997) idum,fdum64,maxPath   ! header
IF( maxPath > PATH_MAXLENGTH )THEN
  nError   = RD_ERROR
  eRoutine = 'puff_rst'
  WRITE(eMessage,'(A,I5)')'Incompatible project path length size, current limit: ',PATH_MAXLENGTH
  WRITE(eInform,'(A,I5)') 'Attempt to read project with size limit: ',maxPath
  GOTO 9999
END IF

!------ skip over useless records; read old grid reference and lter

READ(lun_tmp,ERR=9997,END=9997) ! constants
READ(lun_tmp,ERR=9997,END=9997) ! turbulence
READ(lun_tmp,ERR=9997,END=9997) ! puff parameters

READ(lun_tmp,ERR=9997,END=9997) dum,dum,dum,dum,dum,idum &
                               ,dum,dum,dum,dum,dum,idum,idum,idum &
                               ,lon0_old,lat0_old,xref_old,yref_old

READ(lun_tmp,ERR=9997,END=9997) ! materials
READ(lun_tmp,ERR=9997,END=9997) ! surface materials

READ(lun_tmp,ERR=9997,END=9997) dum,ldum,lter_old,lmap_old

READ(lun_tmp,ERR=9997,END=9997) idum,ldum,ldum &
                               ,dum, fdum(1:maxPath),zone_old,ldum &
                               ,ldum,idum,mode_old,dum,idum &
                               ,idum,dum,dum,dum &
                               ,nNWPN_old,numRelID

IF( nNWPN_old  > 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Cannot use special restart for NWPN projects with PA'
  CALL ReportFileName( eInform,'File=',file_tmp )
  GOTO 9999
END IF

!------ Read Release ID
!       N.B. numRelID may be larger than necessary if releases in old project occured
!       after restart time, but that shouldn't cause any problems. Note that numRelID
!       and releaseID will be incremented with any new release; thus releaseID may
!       include redundant data.

IF( numRelID > 0 )THEN
  IF( ALLOCATED(releaseID) )DEALLOCATE( releaseID,STAT=ios )
  ALLOCATE( releaseID(numRelID),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'puff_rst'
    eMessage = 'Error allocating release ID array for special restart'
    GOTO 9999
  END IF
  READ(lun_tmp,IOSTAT=ios) (releaseID(i),i=1,numRelID)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'puff_rst'
    eMessage = 'Error reading release ID array for special restart'
    GOTO 9999
  END IF
END IF

!------ skip multicomponent section (assumes new materials match old)

IF( multicomp )THEN
  CALL rst_prj_mc()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ check coordinate system

IF( lmap /= lmap_old )THEN
  nError   = IV_ERROR
  eRoutine = 'puff_rst'
  eMessage = 'Invalid project coordinate system'
  eInform  = 'Cannot change coordinate systems on a restart'
  GOTO 9999
ELSE
  IF( lmap == I_UTM )THEN
    IF( utm_zone /= zone_old )THEN
      nError   = IV_ERROR
      eRoutine = 'puff_rst'
      eMessage = 'UTM zones differ'
      WRITE(eInform,'(A,I3,A,I3)',IOSTAT=ios) 'Old:',zone_old,' New: ',utm_zone
      eAction  = 'Cannot change UTM zones on a restart'
      GOTO 9999
    END IF
  ELSE IF( lmap /= I_LATLON )THEN
    ldum = lon0 /= lon0_old .OR. lat0 /= lat0_old .OR. &
           xref /= xref_old .OR. yref /= yref_old
    IF( ldum )THEN
      nError   = IV_ERROR
      eRoutine = 'puff_rst'
      eMessage = 'Invalid project coordinate system'
      eInform  = 'Cannot change coordinate system reference point on a restart'
      GOTO 9999
    END IF
  END IF
END IF

!------ read old met grid definitions and terrain (if appropriate)

READ(lun_tmp,ERR=9997,END=9997) idum !numMet, mcTypePrj

DO i = 1,numOld
  grd => OldGrid(i)

  READ(lun_tmp,IOSTAT=ios) grd%nx,grd%ny,grd%dx,grd%dy,grd%xmin,grd%ymin, &
                           grd%xminPrj,grd%xmaxPrj,grd%yminPrj,grd%ymaxPrj, &
                           grd%lter,(AuxCoord(j),j=1,SIZE_MapCoordPrjTransfer)
  IF( ios /= 0 )GOTO 9997

  grd%coord = TRANSFER(AuxCoord,grd%coord)

  SELECT CASE( grd%coord%type )
    CASE( I_LAMBERT )
      irv = InitLambert( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Lat2,grd%coord%Rearth, &
                         grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )

    CASE( I_POLAR )
      irv = InitPolar( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Rearth, &
                       grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )

    CASE( I_RPOLAR )
      irv = InitRotPolar( grd%coord%Lat0,grd%coord%Lon0, &
                          grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                          grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0  )

    CASE( I_ROTLL )
      irv = InitRotLL( grd%coord%Lat0,grd%coord%sp0,grd%coord%cp0 )

    CASE( I_MERCATOR )
      irv = InitMercator( grd%coord%Lat1,Rearth*1.E-3,grd%coord%f,grd%coord%m0 )

    CASE DEFAULT
      irv = 1

  END SELECT
  IF( irv /= 1 )GOTO 9997

  IF( grd%nx > 0 )THEN
    grd%basic = 0
  ELSE
    grd%nx    = -grd%nx
    grd%basic = ISHFT(grd%ny,-16)
    grd%ny    = IAND(grd%ny,2**16-1)
  END IF

  IF( grd%lter )THEN
    nxyb_old = OldGrid(i)%nx*OldGrid(i)%ny
    READ(lun_tmp,ERR=9997,END=9997) (OldGrid(i)%H(j),j=1,nxyb_old)
  END IF

END DO

!------ Fill 2d Zi array by interpolating from appropriate old grid
!       N.B. If no old grid encompasses new grid, 1st old grid is used

DO i = 1,numMet

  CALL SWIMmapfac( MetGrid(i)%coord,xc,yc,xmap,ymap )
  dss0 = (MetGrid(i)%dX/xmap)**2 + (MetGrid(i)%dY/ymap)**2
  dssMin = HUGE(0.)
  ifld = 0
  DO j = 1,numOld
    xmx = MetGrid(i)%Xmin + FLOAT(MetGrid(i)%nX-1)*MetGrid(i)%dX
    ymx = MetGrid(i)%Ymin + FLOAT(MetGrid(i)%nY-1)*MetGrid(i)%dY
    xmo = OldGrid(j)%Xmin + FLOAT(OldGrid(j)%nX-1)*OldGrid(j)%dX
    ymo = OldGrid(j)%Ymin + FLOAT(OldGrid(j)%nY-1)*OldGrid(j)%dY
    dss = RateMyParent( MetGrid(i)%Xmin,xmx,MetGrid(i)%dX, &
                        MetGrid(i)%Ymin,ymx,MetGrid(i)%dY,MetGrid(i)%coord, &
                        OldGrid(j)%Xmin,xmo,OldGrid(j)%dX, &
                        OldGrid(j)%Ymin,ymo,OldGrid(j)%dY,OldGrid(j)%coord )
    IF( dss > 0. )THEN
      IF( ifld == 0 )ifld = j
      IF( dss < dssMin )THEN
        ifld  = J
        dssMin = dss
      END IF
    END IF
  END DO

  IF( ifld == 0 )ifld = 1

  DO j = 1,MetGrid(i)%ny
    i0 = (j-1)*MetGrid(i)%nx
    yc = MetGrid(i)%Ymin + FLOAT(j-1)*MetGrid(i)%dy
    DO ix = 1,MetGrid(i)%nx
      is = i0 + ix
      xc = MetGrid(i)%Xmin + FLOAT(i-1)*MetGrid(i)%dx
      irv = SWIMcnvCoord(  xc,yc,MetGrid(i)%coord,xp,yp,OldGrid(ifld)%coord )
      ptrI => OldGrid(ifld)%zi(1:OldGrid(ifld)%nx*OldGrid(ifld)%ny)
      CALL get_topog_interp( OldGrid(ifld),ptrI,xp,yp,MetGrid(i)%zi(is) )
    END DO
  END DO

  irv = SWIMputMixingHt( i,MetGrid(i) ) !Fill zi arrays in SWIM
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMputMixingHt' )
    GOTO 9999
  END IF

END DO

!------ Set so puff heights are AGL

IF( lter_old )THEN
  DO ipuf = 1,npuf
    i = getPuffifld( puff(ipuf) )
    irv = SWIMcnvCoord( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),PrjCoord,xp,yp,OldGrid(i)%coord )
    CALL get_topog_interp( OldGrid(i),OldGrid(i)%h,xp,yp,h )
    puff(ipuf)%zbar = MAX(puff(ipuf)%zbar-h,0.)  !set to height agl
  END DO
END IF

DO ipuf = 1,npuf
  CALL setPuffifld( puff(ipuf),1 )  !Place all puffs on field 1 initially
END DO

!------ Adjust puff heights for current terrain (field 1)

IF( lter )THEN
  DO ipuf = 1,npuf
    CALL get_topogIn( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),h,hx,hy,1 )
    puff(ipuf)%zbar = puff(ipuf)%zbar + h
  END DO
END IF

CLOSE(lun_tmp,IOSTAT=ios,ERR=9997)

9999 CONTINUE

IF( ALLOCATED(OldGrid) )THEN
  DO i = 1,numOld
    IF( ASSOCIATED(OldGrid(i)%zi) )DEALLOCATE( OldGrid(i)%zi,STAT=ios )
    IF( ASSOCIATED(OldGrid(i)%H)  )DEALLOCATE( OldGrid(i)%H, STAT=ios )
  END DO
  DEALLOCATE( OldGrid,STAT=ios )
END IF

CLOSE(lun_tmp,IOSTAT=ios)

RETURN

!------ set puff file read error and return

9998 CONTINUE

nError   = RD_ERROR
eRoutine = 'puff_rst'
eMessage = 'Error reading puff file for restarting'
CALL ReportFileName( eInform,'File=',file_tmp )
GOTO 9999

!------ set project file read error and return

9997 CONTINUE

nError   = RD_ERROR
eRoutine = 'puff_rst'
eMessage = 'Error reading project file for restarting'
CALL ReportFileName( eInform,'File=',file_tmp )
GOTO 9999

END

!===============================================================================

SUBROUTINE surf_rst( lun,srfI,file_srf )

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE sagdef_fd
USE accumsrf
USE PtrGrdStrItf
USE special_rst_fi

!------ initialize surface file from another project

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
INTEGER,      INTENT( IN ) :: srfI
CHARACTER(*), INTENT( IN ) :: file_srf

INTEGER ios, irv, ir, iro, nskip, i, j, ii, ivar, iv0
INTEGER nxs, nys, nvs, nblk, iversionx, ngrds
INTEGER j1, mlev, i0, j0, icell, ip, ishft, jshft
INTEGER nxo, nyo, ngrdo, mx, my, ilev, ncell
INTEGER mn0, ioff, iv, ix, iy
INTEGER jj, nmov, is, ie

REAL tx, dxs, dys, xmins, ymins, xmap, ymap, xbar, ybar
REAL dxo, dyo, xmino, ymino, xmaxo, ymaxo, xc, yc, dfac, x, y

INTEGER, ALLOCATABLE, DIMENSION(:)   :: srfgrdo
REAL,    ALLOCATABLE, DIMENSION(:,:) :: srfdato

CHARACTER(13) file_type

LOGICAL interp_grd

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER, EXTERNAL :: SAG_WriteBreakID

srf => SAG_PtrGrdStr( srfI ) ! Associate "local" grid structure pointer

!------ open surface file for restarting

OPEN(UNIT=lun,FILE=file_srf,STATUS='OLD',ACCESS='DIRECT',RECL=512,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'surf_rst'
  eMessage = 'Error opening surface file for restarting'
  CALL ReportFileName( eInform,'File=',file_srf )
  eAction  = 'Make sure file does not already exist'
  GOTO 9999
END IF

!------ read header

READ(lun,REC=1,IOSTAT=ios) file_type,nblk,iversionx
IF( ios /= 0 )GOTO 9998

!------ check version

IF( file_type == SURFACE_FILE_TYPE )THEN
  IF( iversionx/100 /= iversion/100 )THEN
    nError   = UK_ERROR
    eRoutine = 'surf_rst'
    eMessage = 'Different SCIPUFF version number on surface file'
    CALL ReportFileName( eInform,'File=',file_srf )
    GOTO 9999
  END IF
ELSE
  nError   = UK_ERROR
  eRoutine = 'surf_rst'
  eMessage = 'Incomptabile SCIPUFF surface file type'
  CALL ReportFileName( eInform,'File=',file_srf )
  GOTO 9999
END IF

!------ read header record on restart timebreak

ir  = nblk + 2
READ(lun,REC=ir,IOSTAT=ios) nvs
IF( ios /= 0 )GOTO 9998

ir  = ir + 1 + nvs/SAG_RECL
iro = ir

j  = 0
tx = 0.

DO WHILE( j < n_rst .AND. tx*3600. < t )
  j = j + 1

  READ(lun,REC=ir,IOSTAT=ios) tx, ngrdo, nxo, nyo, xmino, ymino, &
                              dxo, dyo, mlev
  IF( ios   /= 0 )GOTO 9998
  IF( ngrdo == 0 )GOTO 9998

  nskip = ((ngrdo-1)/128 + 1)*(nvs+1) + 1
  iro   = ir
  ir    = ir + nskip
END DO

!------ make sure time is within 1 sec. of restart time

IF( ABS(tx*3600.-t) > 1.0 )THEN
  nError   = UK_ERROR
  eRoutine = 'surf_rst'
  eMessage = 'Error finding time on surface restart file'
  CALL ReportFileName( eInform,'File=',file_srf )
  GOTO 9999
END IF

!------ make sure grid is big enough to hold

IF( ngrdo > srf%mxgrd )THEN
  nError   = UK_ERROR
  eRoutine = 'surf_rst'
  eMessage = 'Surface grid too small to read surface restart file'
  CALL ReportFileName( eInform,'File=',file_srf )
  WRITE(eAction,*)ngrdo - 1
  eAction = 'Maximum surface grid must be greater than '//TRIM(ADJUSTL(eAction))
  GOTO 9999
END IF

!------ read surface fields

ir = iro + 1

DO ii = 1,ngrdo,128
  j1 = MIN0(ii+127,ngrdo)
  READ(lun,REC=ir,IOSTAT=ios) (srf%ipgrd(j),j=ii,j1)
  IF( ios /= 0 )GOTO 9998
  ir = ir + 1
END DO

DO ivar = 1,nvs
  iv0 = (ivar-1)*srf%mxgrd
  DO ii = 1,ngrdo,128
    j1 = MIN0(ii+127,ngrdo)
    READ(lun,REC=ir,IOSTAT=ios) (srf%ipdat(j+iv0),j=ii,j1)
    IF( ios /= 0 )GOTO 9998
    ir = ir + 1
  END DO
END DO

CLOSE(lun,IOSTAT=ios)

!------ if interpolation not required, setup grid structure

IF( .NOT.interp )THEN

  ngrds = ngrdo
  xmins = xmino
  ymins = ymino
  nxs   = nxo
  nys   = nyo
  dxs   = dxo
  dys   = dyo

  srf%ncells = ngrds
  srf%maxlev = mlev

ELSE

!------ otherwise, interpolate onto new grid; base new grid on old one

  mx = INT((LOG(xmax-xmin)-LOG(FLOAT(nxo)*dxo))/LOG2)
  mx = MAX(mx,0)
  my = INT((LOG(ymax-ymin)-LOG(FLOAT(nyo)*dyo))/LOG2)
  my = MAX(my,0)
  mx = MIN(mx,my)
  IF( mlev /= -99 )mlev = mlev + mx*SIGN(1,mlev)

  dxs = dxo*2.**mx
  dys = dyo*2.**mx

  ishft = MAX(INT((xmino-xmin)/dxs+0.999),0)
  jshft = MAX(INT((ymino-ymin)/dys+0.999),0)

  xmins = xmino - FLOAT(ishft)*dxs
  ymins = ymino - FLOAT(jshft)*dys

  xmaxo = xmino + FLOAT(nxo)*dxo
  ymaxo = ymino + FLOAT(nyo)*dyo

  nxs = INT((MAX(xmax,xmaxo)-xmins)/dxs+0.999)
  nys = INT((MAX(ymax,ymaxo)-ymins)/dys+0.999)

  srf%maxlev = mlev

  mn0 = nxo*nyo

  ALLOCATE( srfgrdo(mn0),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = SZ_ERROR
    eRoutine = 'surf_rst'
    eMessage = 'Error allocating surface grid for restarting'
    GOTO 9999
  END IF

  ALLOCATE( srfdato(mn0,srf%nvart),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = SZ_ERROR
    eRoutine = 'surf_rst'
    eMessage = 'Error allocating surface grid for restarting'
    GOTO 9999
  END IF

!------ if top level refinement has not changed

  IF( mx == 0 )THEN

!------ find offset in grid array locations

    ioff  = nxs*nys - mn0
    ngrds = ngrdo + ioff
    srf%ncells = ngrds

!------ shift data and pointers of refined data

    DO i = ngrdo,mn0+1,-1
      IF( srf%ipgrd(i) /= 0 )THEN
        srf%ipgrd(i+ioff) = srf%ipgrd(i) + ioff
      ELSE
        srf%ipgrd(i+ioff) = 0
      END IF
      DO iv = 1,nvs
        iv0 = (iv-1)*srf%mxgrd
        srf%ipdat(i+ioff+iv0) = srf%ipdat(i+iv0)
      END DO
    END DO

!------ copy out top level

    DO i = 1,mn0
      srfgrdo(i) = srf%ipgrd(i)
      DO iv = 1,nvs
        iv0 = (iv-1)*srf%mxgrd
        srfdato(i,iv) = srf%ipdat(i+iv0)
      END DO
    END DO

!------- zero out top level of new grid

    DO i = 1,nxs*nys
      srf%ipgrd(i) = 0
      DO iv = 1,nvs
        iv0 = (iv-1)*srf%mxgrd
        srf%ipdat(i+iv0) = 0.
      END DO
    END DO

!------ put top level from old grid onto new grid

    DO j = 1,nyo
      i0 = (j-1)*nxo
      j0 = (j-1+jshft)*nxs
      DO i = 1,nxo
        ip = i0 + i
        ii = j0 + i + ishft
        IF( srfgrdo(ip) /= 0 )THEN
          srf%ipgrd(ii) = srfgrdo(ip) + ioff
        ELSE
          srf%ipgrd(ii) = 0
        END IF
        DO iv = 1,nvs
          iv0 = (iv-1)*srf%mxgrd
          srf%ipdat(ii+iv0) = srfdato(ip,iv)
        END DO
      END DO
    END DO

!------ if top level has changed

  ELSE

!------ zero out new top level grid, temporarily using end of arrays

    IF( nxs*nys+ngrdo > srf%mxgrd )THEN
      nError   = SZ_ERROR
      eRoutine = 'surf_rst'
      eMessage = 'Surface grid too big for restarting'
      GOTO 9999
    END IF

    DO i = 1,nxs*nys
      srf%ipgrd(i+ngrdo) = 0
      DO iv = 1,nvs
        iv0 = (iv-1)*srf%mxgrd
        srf%ipdat(i+ngrdo+iv0) = 0.
      END DO
    END DO

!------ loop over top level of old grid and build lists and cells
!       starting from new top level grid (in temporary location)

    dfac  = 0.5**mx
    ncell = nxs*nys + ngrdo

    DO j = 1,nyo
      i0 = (j-1)*nxo
      y  = (FLOAT(j)-0.5)*dfac + FLOAT(jshft)

      DO i = 1,nxo
        x  = (FLOAT(i)-0.5)*dfac + FLOAT(ishft)
        ip = i0 + i

        interp_grd = srf%ipgrd(ip) /= 0
        DO iv = 1,nvs
          iv0 = (iv-1)*srf%mxgrd
          interp_grd = interp_grd .OR. srf%ipdat(ip+iv0) > 0.
        END DO

        IF( interp_grd )THEN

          xc = x
          yc = y

          ix = INT(xc)
          iy = INT(yc)
          icell = iy*nxs + ix + 1 + ngrdo
          DO ilev = 1,mx
            IF( srf%ipgrd(icell) == 0 )THEN
              IF( ncell+4 > srf%mxgrd )THEN
                nError   = SZ_ERROR
                eRoutine = 'surf_rst'
                eMessage = 'Surface grid too big for restarting'
                GOTO 9999
              END IF
              CALL accum_cell( icell,srf%ipgrd,srf%ipdat, &
                               nvs,ncell,srf%mxgrd )
            END IF
            xc = xc - FLOAT(ix)
            yc = yc - FLOAT(iy)
            xc = xc + xc
            yc = yc + yc
            ix = INT(xc)
            iy = INT(yc)
            icell = srf%ipgrd(icell) + iy + iy + ix
          END DO
          DO iv = 1,nvs
            iv0 = (iv-1)*srf%mxgrd
            srf%ipdat(icell+iv0) = srf%ipdat(ip+iv0)
          END DO
        END IF
      END DO
    END DO

!------ adjust old pointers

    ncell = ncell - ngrdo

    DO i = 1,ngrdo
      IF( srf%ipgrd(i) /= 0 )THEN
        srf%ipgrd(i) = srf%ipgrd(i) + ncell - mn0
      END IF
    END DO

!------ copy out top level

    DO i = 1,mn0
      srfgrdo(i) = srf%ipgrd(i)
      DO iv = 1,nvs
        iv0 = (iv-1)*srf%mxgrd
        srfdato(i,iv) = srf%ipdat(i+iv0)
      END DO
    END DO

!------ move everything around

    is = 0
    DO i = 1,ncell,mn0
      ie = MIN(ncell,is+mn0)
      nmov = ie - is
      IF( i > 1 )THEN
        DO j = ngrdo,mn0+1,-1
          jj = j + is - mn0
          srf%ipgrd(jj+nmov) = srf%ipgrd(jj)
          DO iv = 1,nvs
            iv0 = (iv-1)*srf%mxgrd
            srf%ipdat(jj+nmov+iv0) = srf%ipdat(jj+iv0)
          END DO
        END DO
      END IF
      DO j = 1,nmov
        jj = j + is
        srf%ipgrd(jj) = srf%ipgrd(jj+ngrdo)
        IF( srf%ipgrd(jj) /= 0 )THEN
          srf%ipgrd(jj) = srf%ipgrd(jj) - ngrdo
        END IF
        DO iv = 1,nvs
          iv0 = (iv-1)*srf%mxgrd
          srf%ipdat(jj+iv0) = srf%ipdat(jj+ngrdo+iv0)
        END DO
      END DO
      is = is + mn0
    END DO

!------- set old top level grid cells

    DO j = 1,nyo
      i0 = (j-1)*nxo
      y  = (FLOAT(j)-0.5)*dfac + FLOAT(jshft)

      DO i = 1,nxo
        x  = (FLOAT(i)-0.5)*dfac + FLOAT(ishft)
        ip = i0 + i

        interp_grd = srfgrdo(ip) /= 0
        DO iv = 1,nvs
          interp_grd = interp_grd .OR. srfdato(ip,iv) > 0.
        END DO

        IF( interp_grd )THEN

          xc = x
          yc = y

          ix = INT(xc)
          iy = INT(yc)
          icell = iy*nxs + ix + 1
          DO ilev = 1,mx
            xc = xc - FLOAT(ix)
            yc = yc - FLOAT(iy)
            xc = xc + xc
            yc = yc + yc
            ix = INT(xc)
            iy = INT(yc)
            icell = srf%ipgrd(icell) + iy + iy + ix
          END DO
          srf%ipgrd(icell) = srfgrdo(ip)
        END IF
      END DO
    END DO

    ngrds = ncell + ngrdo - mn0
    srf%ncells = ngrds

  END IF

END IF

!------ reset surface structure grid parameters

srf%nx   = nxs
srf%ny   = nys
srf%xmin = xmins
srf%ymin = ymins
srf%dx   = dxs
srf%dy   = dys

!------ output

tx = t/3600.
irv = SAG_WriteBreakID( srfI,tx )
IF( irv /= SAG_OK )THEN
  nError   = WR_ERROR
  eRoutine = 'surf_rst'
  eMessage = 'Error writing surface file'
  GOTO 9999
END IF

!------ Set delmin

IF( mlev == -99 )THEN
  srf%delmin = 0.
ELSE
  xbar = xmin + 0.5*(nxs-1)*dxs
  ybar = ymin + 0.5*(nys-1)*dys
  CALL mapfac( xbar,ybar,xmap,ymap )
  srf%delmin = MIN(srf%dx/xmap,srf%dy/ymap)*(0.5**ABS(mlev))
END IF

9999 CONTINUE

IF( ALLOCATED( srfgrdo ))DEALLOCATE( srfgrdo,STAT=irv )
IF( ALLOCATED( srfdato ))DEALLOCATE( srfdato,STAT=irv )

RETURN

!------ set read error and goto return

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'surf_rst'
eMessage = 'Error reading surface file for restart'
CALL ReportFileName( eInform,'File=',file_srf )
GOTO 9999

END

!===============================================================================

SUBROUTINE get_topog_interp( grid,Hi,x,y,ho )

USE SWIMgridStr_fd

!------ find topography height

IMPLICIT NONE

TYPE( SWIMgridStr ), INTENT( IN  ) :: grid
REAL, DIMENSION(:),  POINTER       :: Hi
REAL,                INTENT( IN  ) :: x,y
REAL,                INTENT( OUT ) :: ho

REAL ratx, rxm1, raty, rym1
REAL cc1, cc2, cc3, cc4
REAL frac

INTEGER ig, jg, i

IF( grid%nx == 1 .OR. grid%nx == 1 )THEN
  ho = Hi(1)
  RETURN
END IF

frac = (x-grid%xmin)/grid%dx
frac = MAX(frac,0.0)
frac = MIN(frac,FLOAT(grid%nx-1)-1.E-3)
ig   = INT(frac) + 1
ratx = frac - FLOAT(ig-1)
rxm1 = 1.0 - ratx

frac = (y-grid%ymin)/grid%dy
frac = MAX(frac,0.0)
frac = MIN(frac,FLOAT(grid%ny-1)-1.E-3)
jg   = INT(frac) + 1
raty = frac - FLOAT(jg-1)
rym1 = 1.0 - raty

cc1 = rxm1*rym1
cc2 = rxm1*raty
cc3 = ratx*raty
cc4 = ratx*rym1

i  = (jg-1)*grid%nx + ig
ho = cc1*Hi(i) + cc2*Hi(i+grid%nx) + cc3*Hi(i+grid%nx+1) + cc4*Hi(i+1)

RETURN
END

!===============================================================================

SUBROUTINE init_param_met()

USE met_fi

IMPLICIT NONE

nzbl    = 11
uu_calm = 0.25
sl_calm = 1000.

lzi_prj   = .FALSE.
mcTypePrj = 0

numMet = 0

RETURN
END
