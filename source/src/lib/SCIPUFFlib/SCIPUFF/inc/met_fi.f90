MODULE met_fi

USE metparam_fd
USE SWIMgridStr_fd
USE basic_fd

SAVE

!------ LOGICAL flags

LOGICAL lbl, lua, lsfc, lensm, lsv_oper
LOGICAL lzi_prj
LOGICAL lmc_ua
LOGICAL local_met
LOGICAL lout_met, lout_2d, lout_3d, lout_mc, lformat
LOGICAL Use_TKE

!------ Character variables

CHARACTER(80) met_type, bl_type, ensm_type, pr_type
CHARACTER(80) landuse

!------ Miscellaneous variables

REAL tbin_met, tout_met
REAL zruf, h_cnp, alpha_cnp, ustdep, albedo, bowen, cloud_cover
REAL zimin, zimax, hconst, hdiur
REAL uu_calm, sl_calm, uu_ensm, sl_ensm, sl_haz
REAL fixed_spd,fixed_dir
REAL pr_rate, tprcp, tprcpn

REAL ub, vb, wb, tb, hb, pb, tab, thb
REAL cw
REAL uub, vvb, uvb, uubl, vvbl, wwbl, wwbh, wtbl, prbl
REAL qqs, qqb, qqsh, qql
REAL uubz, vvbz, uvbz
REAL sby, sbl, sbz, sbls, sb_lsv
REAL dudx, dvdy, dudy, dvdx, dwdx, dwdy
REAL dudz, dvdz, dwdz, dtdz, dhdz
REAL su2
REAL difb, dddz, dddx, dddy
REAL zinv, hmin, zbtop, TerElev
REAL hc, alphac                                      !canopy height and flow index
REAL sun_fac
REAL prate
REAL cldcvr
REAL cldtop, cldbot, cw_avg
REAL LAI,  t_dep, znth_dep
REAL lat_dep, lon_dep
INTEGER lu_cat, jday_dep
REAL dtdzs, us2, ws2                                  !Old ATP45_inc
REAL wts, xml, zsl
REAL alpha_max, alpha_min, ac_eps, p_eps
REAL ww_skew, wb_skew, wwz

REAL timeOutMet

INTEGER max_iter, max_iter_ac
INTEGER n_obs_min, n_sfc_min
INTEGER n_obs_max, n_sfc_max
INTEGER nzbl
INTEGER unit_spd, i_wet, i_cat
INTEGER nout_met
INTEGER jul_met

INTEGER mcTypeMet, mcTypePrj
INTEGER metField

INTEGER nzMC
REAL, DIMENSION(:), ALLOCATABLE :: zMC

COMMON / met_puf /  zruf, ustdep
COMMON / met_puf / ub, vb, wb, tb, hb, pb, tab, thb
COMMON / met_puf / cw
COMMON / met_puf / uub, vvb, uvb, uubl, vvbl, wwbl, wwbh, wtbl, prbl
COMMON / met_puf / qqs, qqb, qqsh, qql
COMMON / met_puf / uubz, vvbz, uvbz
COMMON / met_puf / sby, sbl, sbz, sbls, sb_lsv
COMMON / met_puf / dudx, dvdy, dudy, dvdx, dwdx, dwdy
COMMON / met_puf / dudz, dvdz, dwdz, dtdz, dhdz
COMMON / met_puf / su2
COMMON / met_puf / difb, dddz, dddx, dddy
COMMON / met_puf / zinv, TerElev
COMMON / met_puf / hc, alphac                                      !canopy height and flow index
COMMON / met_puf / sun_fac
COMMON / met_puf / prate
COMMON / met_puf / cldcvr
COMMON / met_puf / cldtop, cldbot, cw_avg
COMMON / met_puf / LAI, lu_cat, jday_dep
COMMON / met_puf / t_dep, znth_dep, lat_dep, lon_dep
COMMON / met_puf / dtdzs, us2, ws2                                  !Old ATP45_inc
COMMON / met_puf / wts, xml, zsl
COMMON / met_puf / ww_skew, wb_skew, wwz
COMMON / met_puf / metField
!$OMP THREADPRIVATE(/met_puf/)

!------ SWIM stuff

INTEGER SWIMstatus
INTEGER numMet, nBaseMet, numMetMax

TYPE( MapCoord ) :: PrjCoord

TYPE( SWIMgridStr ), DIMENSION(:), ALLOCATABLE, TARGET :: MetGrid
LOGICAL, DIMENSION(:), ALLOCATABLE :: lMetGridComplete

!------ SWIM interface stuff

INTERFACE

  RECURSIVE SUBROUTINE get_met( xp,yp,zp,szz,zc,iAGL,inField,Shh,outField,dElev )
    REAL,    INTENT( IN ) :: xp, yp     !Horizontal location
    REAL,    INTENT( IN ) :: zp         !Puff height above Hmin if iAGL=0; AGL if iAGL=1
    REAL,    INTENT( IN ) :: szz        !Puff vertical variance
    REAL,    INTENT( IN ) :: zc         !Puff cap
    INTEGER, INTENT( IN ) :: iAGL       !Height flag: 1 for AGL, 0 for relative to Hmin
    INTEGER, OPTIONAL, INTENT( IN  ) :: inField  ! Met field index (from previous call)
    REAL,    OPTIONAL, INTENT( IN  ) :: Shh      ! Puff spread scale (m**2)
    INTEGER, OPTIONAL, INTENT( OUT ) :: outField ! Met field index (from this call)
    REAL,    OPTIONAL, INTENT( OUT ) :: dElev    ! Change in terrain from different
  END SUBROUTINE get_met

END INTERFACE

END MODULE met_fi
