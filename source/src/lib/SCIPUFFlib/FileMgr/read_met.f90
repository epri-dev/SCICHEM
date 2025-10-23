!===============================================================================
!     ReadNamelistMet
!===============================================================================
SUBROUTINE ReadNamelistMet( iunit )

USE scipuff_fi
USE met_fi, zruf_mod=>zruf
USE files_fi
USE weather_fd
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios, nearest_sfc, nearest_prf, nzb

REAL, DIMENSION(MAXZ_MET) :: zb

!--- Backward compatability

LOGICAL lswift
REAL    dt_swift
INTEGER stats3dc
REAL zruf
INTEGER ter_flag
INTEGER mcType

INTEGER, EXTERNAL :: FindNML

NAMELIST / met / met_type, bl_type, ensm_type, uu_ensm, sl_ensm, &
                 zimin, zimax, hconst, hdiur, h_cnp, alpha_cnp, zruf,  &
                 sl_haz, albedo, bowen, cloud_cover, local_met, &
                 nearest_sfc,nearest_prf, lmc_ua, alpha_max, alpha_min, &
                 max_iter, p_eps, nzb, zb, file_ter, &
                 max_iter_ac, ac_eps, &
                 lout_mc, lout_met, tout_met, lout_3d, lout_2d, &
                 pr_type, tbin_met, i_wet, landuse, ter_flag, &
                 lformat, mcType, lswift, dt_swift, stats3dc

sl_ensm     = NOT_SET_R
uu_ensm     = NOT_SET_R
nearest_sfc = DEF_VAL_I
nearest_prf = DEF_VAL_I
h_cnp       = -1.
alpha_cnp   = 2.0
i_wet       = HM_MSTNORM
landuse     = NOT_SET_C
zruf        = NOT_SET_R
tout_met    = NOT_SET_R
ter_flag    = 3
stats3dc    = -1
mcType      = NOT_SET_I
nzb         = 0
zb          = 0.0
Use_TKE     = .FALSE.

ios = FindNML( iunit,'met' )

IF( ios == 0 )READ(UNIT=iunit,NML=met,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistMet'
  eMessage = 'Error reading MET namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistMet'
  eMessage = 'EOF reading MET namelist'
  GOTO 9999
END IF

IF( mcType == NOT_SET_I )THEN
  mcTypeMet = 0
  mcTypeMet = IBSET(mcTypeMet,MCB_MCWIF)
  IF( MOD(ter_flag,2) == 1 )mcTypeMet = IBSET(mcTypeMet,MCB_TER)
  IF( ter_flag/2      == 1 )mcTypeMet = IBSET(mcTypeMet,MCB_LC)
ELSE
  mcTypeMet = mcType
END IF

n_obs_max = MAX(1,nearest_prf)
n_sfc_max = MAX(1,nearest_sfc)

zruf_mod  = zruf

nzMC = nzb

IF( nzb > 0 )THEN

  DEALLOCATE( zMC,STAT=ios )
  ALLOCATE( zMC(nzb),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadNamelistMet'
    eMessage = 'Error allocating mass-consistent vertical grid'
    GOTO 9999
  END IF
  zMC = zb(1:nzb)

END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     SkipNamelistMet
!===============================================================================
SUBROUTINE SkipNamelistMet( iunit )

USE metparam_fd
USE error_fi
USE DefSize_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER nearest_sfc, nearest_prf, max_iter_ac, max_iter,nzb, i_wet, ter_flag, mcType
INTEGER stats3dc
REAL    uu_ensm, sl_ensm, zimin,  zimax, hconst, hdiur, h_cnp, &
        alpha_cnp, zruf,  sl_haz, albedo, bowen, cloud_cover, &
        alpha_max, alpha_min,ac_eps,p_eps,tout_met,tbin_met, dt_swift
LOGICAL local_met,lmc_ua,lout_mc, lout_met,lout_3d
LOGICAL lout_2d, lswift, lformat

CHARACTER(64) landuse

REAL, DIMENSION(MAXZ_MET) :: zb

CHARACTER(80)  met_type, bl_type, ensm_type, pr_type
CHARACTER(PATH_MAXLENGTH) file_ter

INTEGER, EXTERNAL :: FindNML

NAMELIST / met / met_type, bl_type, ensm_type, uu_ensm, sl_ensm, &
                 zimin, zimax, hconst, hdiur, h_cnp, alpha_cnp, zruf,  &
                 sl_haz,albedo, bowen, cloud_cover, local_met, &
                 nearest_sfc,nearest_prf, lmc_ua, alpha_max, alpha_min, &
                 max_iter, p_eps, nzb, zb, file_ter, &
                 max_iter_ac, ac_eps, &
                 lout_mc, lout_met, tout_met, lout_3d, lout_2d, &
                 pr_type, tbin_met, i_wet, landuse, ter_flag, &
                 dt_swift, lswift, lformat, mcType, stats3dc

ios = FindNML( iunit,'met' )

IF( ios == 0 )READ(UNIT=iunit,NML=met,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'SkipNamelistMet'
  eMessage = 'Error skipping MET namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'SkipNamelistMet'
  eMessage = 'EOF skipping MET namelist'
  GOTO 9999
END IF

9999  CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistMet
!===============================================================================
SUBROUTINE WriteNamelistMet( iunit )

USE scipuff_fi
USE met_fi, zruf_mod=>zruf
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios, nearest_sfc, nearest_prf, nzb, mcType
INTEGER stats3dc

REAL, DIMENSION(MAXZ_MET) :: zb
REAL zruf

NAMELIST / met / met_type, bl_type, ensm_type, uu_ensm, sl_ensm, &
                 zimin, zimax, hconst, hdiur, h_cnp, alpha_cnp, zruf,sl_haz, &
                 albedo, bowen, cloud_cover, local_met, &
                 nearest_sfc,nearest_prf, &
                 lmc_ua, alpha_max, alpha_min, max_iter_ac, ac_eps, &
                 max_iter, p_eps, nzb, zb, file_ter, &
                 lout_mc, lout_met, tout_met, lout_3d, lout_2d, &
                 pr_type, tbin_met, i_wet, landuse, &
                 mcType, lformat

nearest_prf = n_obs_max
nearest_sfc = n_sfc_max

zruf = zruf_mod

zb  = 0.0
nzb = nzMC
IF( nzMC > 0 )zb(1:nzb) = zMC(1:nzb)

mcType = mcTypeMet
sl_haz = NOT_SET_R
stats3dc = -1

WRITE(UNIT=iunit,NML=met,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistMet_mc'
  eMessage = 'Error writing MET namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
