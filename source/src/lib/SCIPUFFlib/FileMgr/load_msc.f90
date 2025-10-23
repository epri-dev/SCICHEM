!*******************************************************************************
!            UnloadWeather
!*******************************************************************************
SUBROUTINE UnloadWeather( weather )

USE convert_fd
USE time_fd
USE weather_fd
USE scipuff_fi
USE met_fi
USE files_fi
USE SWIMparam_fd
USE SCIPresults_fd

!     Load SCIPUFF commons from an SCIP Weather structure

IMPLICIT NONE

TYPE( weatherT ), INTENT( IN ) :: weather

INTEGER i, alloc_stat

REAL, EXTERNAL :: ScaleReal

!==== Unload

!==== Weather LSV member

SELECT CASE( weather%lsv%type )
  CASE( HL_LSVINP )
    ensm_type ='INPUT'
  CASE( HL_LSVMOD )
    ensm_type ='MODEL'
  CASE( HL_LSVOBS )
    ensm_type ='OBS'
  CASE( HL_LSVOPER )
    ensm_type ='OPER3.1'
  CASE DEFAULT
    ensm_type ='NONE'
END SELECT

lensm   = TRIM(ensm_type) /= 'NONE'
uu_ensm = weather%lsv%uu
sl_ensm = ScaleReal( weather%lsv%sl,HCF_KM2M )

!==== Weather BL member

SELECT CASE( weather%bl%type )
  CASE( HB_BLCALC )
    bl_type ='CALC'
  CASE( HB_BLOBS )
    bl_type ='OBS'
  CASE( HB_BLSIMPLE )
    bl_type ='SBL'
  CASE( HB_BLPRF )
    bl_type ='PROF'
  CASE( HB_BLOPER )
    bl_type ='OPER'
  CASE( HB_BLMED )
    bl_type ='MEDOC'
  CASE DEFAULT
    bl_type ='NONE'
END SELECT

lbl = TRIM(bl_type) /= 'NONE'
IF( weather%bl%canopy /= NOT_SET_R )THEN
  h_cnp     = weather%bl%canopy
  zruf      = 0.1*h_cnp
  alpha_cnp = weather%bl%canopyParam
ELSE
  h_cnp     = -1.
  zruf      = weather%bl%roughness
  alpha_cnp = 0.0
END IF

landuse     = TRIM(weather%bl%landuse)
i_wet       = weather%bl%wetness
zimax       = weather%bl%ziDay
zimin       = weather%bl%ziNight
hdiur       = weather%bl%hfluxDay
hconst      = weather%bl%hfluxNight
albedo      = weather%bl%albedo
bowen       = weather%bl%bowen
cloud_cover = weather%bl%cloud

!==== Weather Precip member

SELECT CASE( weather%precip%type )
  CASE( HP_PRCPINP )
    SELECT CASE( weather%precip%class )
      CASE( HP_RAIN + HP_LIGHT )
        pr_type ='LGTRAIN'
      CASE( HP_RAIN + HP_MODERATE )
        pr_type ='MODRAIN'
      CASE( HP_RAIN + HP_HEAVY )
        pr_type ='HVYRAIN'
      CASE( HP_SNOW + HP_LIGHT )
        pr_type ='LGTSNOW'
      CASE( HP_SNOW + HP_MODERATE )
        pr_type ='MODSNOW'
      CASE( HP_SNOW + HP_HEAVY )
        pr_type ='HVYSNOW'
      CASE DEFAULT
        pr_type ='NONE'
    END SELECT
  CASE( HP_PRCPMET )
    pr_type ='METFILE'
  CASE DEFAULT
    pr_type ='NONE'
END SELECT

!==== Weather met member

lua       = .TRUE.
lsfc      = .FALSE.
file_sfc  = ' '
file_met  = ' '
n_obs_min = 0
n_sfc_min = 0
n_obs_max = DEF_VAL_I
n_sfc_max = DEF_VAL_I
local_met = weather%flags%reference == HT_LOCAL
tbin_met  = weather%met%timeBin

SELECT CASE( weather%met%type )
  CASE( HW_METNONE )
    met_type = 'NONE'
    lua      = .FALSE.
  CASE( HW_METMED )
    met_type = 'MEDOC'
    file_met = TRIM(weather%met%input(1))
  CASE( HW_METWRF )
    met_type = 'WRF'
    file_met = TRIM(weather%met%input(1))
  CASE( HW_METASSIM )
    met_type = 'SCIPUFF_LIST'
    file_met = TRIM(weather%met%input(1))
  CASE( HW_METMEDLS )
    met_type = 'MEDOC_LIST'
    file_met = TRIM(weather%met%input(1))
  CASE( HW_METMRF )
    met_type = 'MRF'
    file_met = TRIM(weather%met%input(1))
  CASE( HW_METSRF + HW_METPRF )
    met_type  = 'OBS'
    file_met  = TRIM(weather%met%input(1))
    file_sfc  = TRIM(weather%met%input(2))
    n_obs_min = 1
    n_sfc_min = 1
    n_obs_max = weather%met%nnPrf
    n_sfc_max = weather%met%nnSfc
    lsfc      = .TRUE.
  CASE( HW_METSRF )
    met_type  = 'OBS'
    file_sfc  = TRIM(weather%met%input(1))
    n_sfc_min = 1
    n_sfc_max = weather%met%nnSfc
    lsfc      = .TRUE.
    lua       = .FALSE.
  CASE( HW_METPRF )
    met_type  = 'OBS'
    file_met  = TRIM(weather%met%input(1))
    n_obs_min = 1
    n_obs_max = weather%met%nnPrf
  CASE( HW_METFIX )
    met_type  = 'FIXED'
    unit_spd  = weather%met%unitSpd
    fixed_spd = weather%met%speed
    fixed_dir = weather%met%direction
  CASE DEFAULT !Must be Operational
    IF( BTEST(weather%met%type,HWB_METOPER) )THEN
     IF( BTEST(weather%met%type,HWB_METCURR) )THEN
        local_met = .FALSE.
        met_type  = 'OPWX_CURR'
        IF( BTEST(weather%met%type,HWB_METSRF) )THEN
          IF( BTEST(weather%met%type,HWB_METPRF) )THEN
            file_met  = TRIM(weather%met%input(1))
            file_sfc  = TRIM(weather%met%input(2))
            n_obs_min = 1
            n_sfc_min = 1
            n_obs_max = weather%met%nnPrf
            n_sfc_max = weather%met%nnSfc
            lsfc      = .TRUE.
          ELSE
            file_sfc  = TRIM(weather%met%input(1))
            n_sfc_min = 1
            n_sfc_max = weather%met%nnSfc
            lsfc      = .TRUE.
            lua       = .FALSE.
          END IF
        ELSE IF( BTEST(weather%met%type,HWB_METPRF) )THEN
          file_met  = TRIM(weather%met%input(1))
          n_obs_min = 1
          n_obs_max = weather%met%nnPrf
        ELSE
          met_type = 'NONE'
          lua      = .FALSE.
        END IF
      ELSE
        met_type = 'NONE'
        lua      = .FALSE.
      END IF
    ELSE
      met_type = 'NONE'
      lua      = .FALSE.
    END IF
END SELECT

!==== Weather terrain member

lmc_ua = weather%flags%doMC == SCIPon
IF( lmc_ua )THEN
  file_ter = weather%terrain%file
ELSE
  file_ter = ' '
END IF

!==== Terrain mass-consistency member

max_iter_ac = weather%terrain%mc%maxIter(1)
max_iter    = weather%terrain%mc%maxIter(2)
ac_eps      = weather%terrain%mc%eps(1)
p_eps       = weather%terrain%mc%eps(2)
alpha_min   = weather%terrain%mc%alpha(1)
alpha_max   = weather%terrain%mc%alpha(2)
nzMC        = weather%terrain%mc%nz

IF( ALLOCATED(zMC) )DEALLOCATE( zMC,STAT=alloc_stat )

IF( nzMC > 0 )THEN
  ALLOCATE( zMC(nzMC),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'UnloadWeather'
    eMessage = 'Error allocating mass-consistent vertical grid'
    GOTO 9999
  END IF
  DO i = 1,nzMC
    zMC(i) = weather%terrain%mc%z(i)
  END DO
END IF

mcTypeMet = weather%terrain%type

!==== Weather flags member

lout_met = BTEST(weather%flags%doOutput,HOB_OUTPUT)
tout_met = NOT_SET_R
IF( lout_met )THEN
  lout_mc  = .NOT.BTEST(weather%flags%doOutput,HOB_OUTMET)
  lformat  = BTEST(weather%flags%doOutput,HOB_ASCII)
  lout_2d  = BTEST(weather%flags%doOutput,HOB_2D)
  lout_3d  = BTEST(weather%flags%doOutput,HOB_3D)
  IF( lout_mc )THEN
    tout_met = DEF_VAL_R
  ELSE
    tout_met = ScaleReal( weather%flags%tOutput,HCF_HOUR2SEC )
  END IF
ELSE
  lout_mc  = .FALSE.
  lformat  = .FALSE.
  lout_2d  = .FALSE.
  lout_3d  = .FALSE.
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            LoadWeather
!*******************************************************************************
SUBROUTINE LoadWeather( weather )

USE convert_fd
USE time_fd
USE weather_fd
USE scipuff_fi
USE met_fi
USE files_fi
USE SCIPresults_fd
USE SWIMparam_fd

!     Load an SCIP Weather structure from SCIPUFF commons

IMPLICIT NONE

TYPE( weatherT ), INTENT( INOUT ) :: weather

INTEGER i

REAL, EXTERNAL :: ScaleReal

!==== Load

!==== Weather LSV member

IF( lensm )THEN
  IF( lsv_oper )THEN
    weather%lsv%type = HL_LSVOPER
  ELSE
    SELECT CASE( TRIM(ensm_type) )
      CASE( 'INPUT' )
        weather%lsv%type = HL_LSVINP
        weather%lsv%uu   = uu_ensm
        weather%lsv%sl   = ScaleReal( sl_ensm,HCF_M2KM )
      CASE( 'MODEL' )
        weather%lsv%type = HL_LSVMOD
      CASE( 'OBS' )
        weather%lsv%type = HL_LSVOBS
        weather%lsv%sl   = ScaleReal( sl_ensm,HCF_M2KM )
      CASE('OPER3.1' )
        weather%lsv%type = HL_LSVOPER
      CASE DEFAULT
        weather%lsv%type = HL_NONE
    END SELECT
  END IF
ELSE
  SELECT CASE( TRIM(ensm_type) )
    CASE( 'OPER' )
      weather%lsv%type = HL_LSVOPER_20
    CASE DEFAULT
      weather%lsv%type = HL_NONE
  END SELECT
END IF

!==== Weather BL member

weather%bl%wetness = i_wet  !Always set since may be used with any BL if landuse by categor

weather%bl%landuse = TRIM(landuse)

IF( lbl )THEN
  SELECT CASE( TRIM(bl_type) )
    CASE( 'CALC' )
      weather%bl%type    = HB_BLCALC
      weather%bl%albedo  = albedo
      weather%bl%bowen   = bowen
      weather%bl%cloud   = cloud_cover
    CASE( 'OBS' )
      weather%bl%type = HB_BLOBS
    CASE( 'SBL' )
      weather%bl%type       = HB_BLSIMPLE
      weather%bl%ziDay      = zimax
      weather%bl%ziNight    = zimin
      weather%bl%hfluxDay   = hdiur
      weather%bl%hfluxNight = hconst
    CASE( 'PROF' )
      weather%bl%type = HB_BLPRF
    CASE( 'OPER' )
      weather%bl%type    = HB_BLOPER
      weather%bl%albedo  = albedo
      weather%bl%bowen   = bowen
      weather%bl%cloud   = cloud_cover
    CASE( 'MEDOC' )
      weather%bl%type = HB_BLMED
    CASE DEFAULT
      weather%bl%type = HB_NONE
  END SELECT
ELSE
  weather%bl%type = HB_NONE
END IF

IF( h_cnp < 0.0 )THEN
  weather%bl%canopy      = NOT_SET_R
  weather%bl%roughness   = zruf
  weather%bl%canopyParam = 2.0
ELSE
  weather%bl%canopy      = h_cnp
  weather%bl%roughness   = NOT_SET_R
  weather%bl%canopyParam = alpha_cnp
END IF

!==== Weather Precip member

SELECT CASE( TRIM(pr_type) )
  CASE( 'NONE' )
    weather%precip%type  = HP_NONE
    weather%precip%class = SCIPnull
  CASE( 'LGTRAIN' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_RAIN + HP_LIGHT
  CASE( 'MODRAIN' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_RAIN + HP_MODERATE
  CASE( 'HVYRAIN' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_RAIN + HP_HEAVY
  CASE( 'LGTSNOW' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_SNOW + HP_LIGHT
  CASE( 'MODSNOW' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_SNOW + HP_MODERATE
  CASE( 'HVYSNOW' )
    weather%precip%type  = HP_PRCPINP
    weather%precip%class = HP_SNOW + HP_HEAVY
  CASE( 'METFILE' )
    weather%precip%type  = HP_PRCPMET
    weather%precip%class = SCIPnull
  CASE DEFAULT
    weather%precip%type  = HP_NONE
    weather%precip%class = SCIPnull
END SELECT

!==== Weather met member

IF( met_type =='OPER_FCST' )THEN
  met_type = 'OPWX_FCST'
ELSE IF( met_type =='OPER_NOWG' )THEN
  met_type = 'OPWX_FCST'
ELSE IF( met_type =='OPER_NOWO' )THEN
  met_type = 'OPWX_CURR'
END IF

IF( local_met )THEN
 weather%flags%reference = HT_LOCAL
ELSE
 weather%flags%reference = HT_UTC
END IF

weather%met%type = HW_METNONE

SELECT CASE( TRIM(met_type) )
  CASE( 'MEDOC' )
    weather%met%type     = HW_METMED
    weather%met%input(1) = file_met
  CASE( 'WRF' )
    weather%met%type     = HW_METWRF
    weather%met%input(1) = file_met
  CASE( 'ASSIM','SCIPUFF_LIST' )
    weather%met%type     = HW_METASSIM
    weather%met%input(1) = file_met
    weather%met%timeBin  = tbin_met
  CASE( 'MEDOC_LIST','MEDOCLIST' )
    weather%met%type     = HW_METMEDLS
    weather%met%input(1) = file_met
  CASE( 'GRIDDED' )
    weather%met%type     = HW_METMRF
    weather%met%input(1) = file_met
  CASE( 'MRF','GRIDMC' )
    weather%met%type     = HW_METMRF
    weather%met%input(1) = file_met
  CASE( 'OBS' )
    weather%met%input(1) = ' '
    weather%met%input(2) = ' '
    weather%met%nnPrf    = DEF_VAL_I
    weather%met%nnSfc    = DEF_VAL_I
    i = 0
    IF( lua )THEN
      weather%met%type = weather%met%type + HW_METPRF
      i = i + 1
      weather%met%input(i) = file_met
      weather%met%nnPrf    = n_obs_max
    END IF
    IF( lsfc )THEN
      weather%met%type = weather%met%type + HW_METSRF
      i = i + 1
      weather%met%input(i) = file_sfc
      weather%met%nnSfc    = n_sfc_max
    END IF
    weather%met%timeBin = tbin_met
  CASE( 'FIXED' )
    weather%met%type      = HW_METFIX
    weather%met%unitSpd   = unit_spd
    weather%met%unitDir   = HU_DEG
    weather%met%speed     = fixed_spd
    weather%met%direction = fixed_dir
  CASE( 'OPWX_GRID' )
    weather%met%type     = HW_METOPER + HW_METFCST + HW_METMRF
    weather%met%input(1) = file_met
  CASE( 'OPWX_GRID2' )
    weather%met%type     = HW_METOPER + HW_METFCST + HW_METMED
    weather%met%input(1) = file_met
  CASE( 'OPWX_POST' )
    weather%met%type     = HW_METOPER + HW_METFCST + HW_METPRF
    weather%met%input(1) = file_met
    weather%met%timeBin  = tbin_met
  CASE( 'OPWX_CURR' )
    weather%met%type = HW_METOPER + HW_METCURR
    i = 0
    IF( lua )THEN
      weather%met%type = weather%met%type + HW_METPRF
      i = i + 1
      weather%met%input(i) = file_met
      weather%met%nnPrf    = n_obs_max
    END IF
    IF( lsfc )THEN
      weather%met%type = weather%met%type + HW_METSRF
      i = i + 1
      weather%met%input(i) = file_sfc
      weather%met%nnSfc    = n_sfc_max
    END IF
    weather%met%timeBin = tbin_met
  CASE DEFAULT
END SELECT

!==== Weather terrain member

IF( lmc_ua )THEN
  weather%flags%doMC   = SCIPon
  weather%terrain%file = TRIM(file_ter)
ELSE
  weather%flags%doMC   = SCIPoff
  weather%terrain%file = ' '
END IF

!==== Terrain mass-consistency member

IF( nzMC > HS_MAXZB )THEN
  nError = SZ_ERROR
  eRoutine = 'LoadWeather'
  eMessage = 'Mass-consistent vertical grid contains too many points'
  WRITE(eInform,'(A,I3,A,I3)')'nz=',nzMC,' : Max=',HS_MAXZB
  eAction  = 'Do you want to truncate the list and continue loading?'
  CALL WarningMessage( .TRUE. )
  IF( nError /= NO_ERROR )THEN
    weather%flags%doMC = SCIPoff
    GOTO 9999
  ELSE
    nzMC = MIN(nzMC,HS_MAXZB)
  END IF
END IF
weather%terrain%type          = mcTypeMet
weather%terrain%mc%notUsed    = DEF_VAL_R
weather%terrain%mc%maxIter(1) = max_iter_ac
weather%terrain%mc%maxIter(2) = max_iter
weather%terrain%mc%eps(1)     = ac_eps
weather%terrain%mc%eps(2)     = p_eps
weather%terrain%mc%alpha(1)   = alpha_min
weather%terrain%mc%alpha(2)   = alpha_max
weather%terrain%mc%nz         = nzMC
DO i = 1,nzMC
  weather%terrain%mc%z(i)     = zMC(i)
END DO
IF (nzMC < HS_MAXZB) THEN
  DO i = nzMC+1,HS_MAXZB
    weather%terrain%mc%z(i)   = NOT_SET_R
  END DO
END IF


!==== Weather flags member

weather%flags%doOutput = HO_OFF
weather%flags%notUsed  = NOT_SET_I
weather%flags%tOutput  = NOT_SET_R
IF( lout_met )THEN
                weather%flags%doOutput = IBSET(weather%flags%doOutput,HOB_OUTPUT)
  IF( lformat  )weather%flags%doOutput = IBSET(weather%flags%doOutput,HOB_ASCII)
  IF( lout_2d  )weather%flags%doOutput = IBSET(weather%flags%doOutput,HOB_2D)
  IF( lout_3d  )weather%flags%doOutput = IBSET(weather%flags%doOutput,HOB_3D)
  IF( .NOT.lout_mc )THEN
    weather%flags%doOutput = IBSET(weather%flags%doOutput,HOB_OUTMET)
    weather%flags%tOutput  = ScaleReal( tout_met,HCF_SEC2HOUR )
  ELSE
    weather%flags%tOutput  = DEF_VAL_R
  END IF
END IF

9999 CONTINUE

RETURN
END
