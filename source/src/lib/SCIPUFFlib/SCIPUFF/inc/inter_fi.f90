MODULE inter_fi

  SAVE

  REAL det, d33, x1, y1, z1
  REAL dgrd, xp, yp, zp, h, hx, hy
  REAL xx, yy, delx, dely, delz
  REAL fac, facc
  REAL xmap_i, ymap_i, zp1, r_ipuf
  REAL hp, hxp, hyp
  REAL zp2, r_jpuf
  REAL(8) betx, bety, betz
  REAL g_ipuf, g_jpuf, g_kpuf, z_ipuf, z_jpuf, z_kpuf
  REAL r_kpuf, facr, den
  REAL facp, facn, facw, facwp, faccp, ctot, ctotp
  REAL facu, facup, facv, facvp
  REAL lbini, lbinj, ldiami, ldiamj

  REAL, DIMENSION(3) :: xr
  REAL(8), DIMENSION(7) :: asig, bsig

  INTEGER iovlp, iprv, irfrst, irlast, nrlist
  INTEGER ip, ipufMin
  INTEGER lgrpi, lgrpj

  INTEGER, DIMENSION(:), ALLOCATABLE :: ku, kl
  REAL(8), DIMENSION(:), ALLOCATABLE :: ptmp
  REAL(8), DIMENSION(:), ALLOCATABLE :: ptmp2

  REAL(8), DIMENSION(:), ALLOCATABLE :: cc
  REAL(8), DIMENSION(:), ALLOCATABLE :: cct
  REAL(8), DIMENSION(:), ALLOCATABLE :: ccb
  REAL(8), DIMENSION(:), ALLOCATABLE :: cctb
  REAL(8), DIMENSION(:), ALLOCATABLE :: ccbg
  REAL(8), DIMENSION(:), ALLOCATABLE :: ucb
  REAL(8), DIMENSION(:), ALLOCATABLE :: vcb
  REAL(8), DIMENSION(:), ALLOCATABLE :: wcb
  REAL(8), DIMENSION(:), ALLOCATABLE :: ctb
  REAL(8), DIMENSION(:), ALLOCATABLE :: bcb
  REAL(8), DIMENSION(:), ALLOCATABLE :: ctp
  REAL(8), DIMENSION(:), ALLOCATABLE :: udense
  REAL(8), DIMENSION(:), ALLOCATABLE :: vdense
  REAL(8), DIMENSION(:), ALLOCATABLE :: dudx_d
  REAL(8), DIMENSION(:), ALLOCATABLE :: dudy_d
  REAL(8), DIMENSION(:), ALLOCATABLE :: dvdx_d
  REAL(8), DIMENSION(:), ALLOCATABLE :: dvdy_d

  LOGICAL lprocess, lsame, lstatic, lrfl_ipuf, lrfl_jpuf
  LOGICAL ltyp, lmat, ltot, ldyni, ldynj
  LOGICAL lliqi, lliqj, lliqm, laeri, lliqg
  LOGICAL lProcessAll
  LOGICAL lmc
  INTEGER mcID, ID

  INTEGER iucb, ivcb, iwcb, ictb, ictp, ibcb, icctb, icct
  INTEGER iu, iv, idudx, idvdx, idudy, idvdy

  INTEGER jucb, jvcb, jwcb, jctb, jctp, jbcb, jcctb, jcct
  INTEGER ju, jv, jdudx, jdvdx, jdudy, jdvdy

  COMMON /inter_fi_cmn/ iucb, ivcb, iwcb, ictb, ictp, ibcb, icctb, icct
  COMMON /inter_fi_cmn/ iu, iv, idudx, idvdx, idudy, idvdy

  COMMON /inter_fi_cmn/ jucb, jvcb, jwcb, jctb, jctp, jbcb, jcctb, jcct
  COMMON /inter_fi_cmn/ ju, jv, jdudx, jdvdx, jdudy, jdvdy

  COMMON /inter_fi_cmn/ asig, bsig

  COMMON /inter_fi_cmn/ betx, bety, betz
  COMMON /inter_fi_cmn/ det, d33, x1, y1, z1
  COMMON /inter_fi_cmn/ dgrd, xp, yp, zp, h, hx, hy
  COMMON /inter_fi_cmn/ xx, yy, delx, dely, delz
  COMMON /inter_fi_cmn/ fac, facc
  COMMON /inter_fi_cmn/ xmap_i, ymap_i, zp1, r_ipuf
  COMMON /inter_fi_cmn/ hp, hxp, hyp
  COMMON /inter_fi_cmn/ zp2, r_jpuf
  COMMON /inter_fi_cmn/ g_ipuf, g_jpuf, g_kpuf, z_ipuf, z_jpuf, z_kpuf
  COMMON /inter_fi_cmn/ r_kpuf, facr, den
  COMMON /inter_fi_cmn/ facp, facn, facw, facwp, faccp, ctot, ctotp
  COMMON /inter_fi_cmn/ facu, facup, facv, facvp

  COMMON /inter_fi_cmn/ xr

  COMMON /inter_fi_cmn/ iovlp
  COMMON /inter_fi_cmn/ ip

  COMMON /inter_fi_cmn/ lprocess,lsame,lstatic,lrfl_ipuf,lrfl_jpuf
  COMMON /inter_fi_cmn/ ltyp, lmat, ltot, ldyni, ldynj
  COMMON /inter_fi_cmn/ lliqi, lliqj, lliqm, laeri, lliqg
  COMMON /inter_fi_cmn/ lbini, lbinj, ldiami, ldiamj, lgrpi, lgrpj
  COMMON /inter_fi_cmn/ lmc
  COMMON /inter_fi_cmn/ mcID, ID

!$OMP THREADPRIVATE(/inter_fi_cmn/)

END MODULE inter_fi
