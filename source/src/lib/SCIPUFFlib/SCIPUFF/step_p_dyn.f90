RECURSIVE SUBROUTINE step_p( dt,p,ipuf,lev1,lev2,fac_srf )

USE scipuff_fi
USE met_fi
USE surface_fi
USE files_fi
USE step_p_fi
USE srfaux_fi
USE UtilMtlAux
USE mpi_fi, ONLY: isSerial
USE chem_fi, ONLY: lscav
USE grdlock_fi
USE error_fi, ONLY: multiThread

!  This subroutine provides the ode's to be integrated from
!  the source for a dynamic 3d-puff, p.

IMPLICIT NONE

REAL,              INTENT( IN    ) :: dt       !Timestep (secs)
TYPE ( puff_str ), INTENT( INOUT ) :: p        !Puff structure
INTEGER,           INTENT( IN    ) :: ipuf     !Puff number
INTEGER,           INTENT( IN    ) :: lev1     !Lowest time level stepped
INTEGER,           INTENT( INOUT ) :: lev2     !Highest time level stepped
REAL,              INTENT( IN    ) :: fac_srf  !Factor for surface integrals

TYPE( puff_material ) pmatl
TYPE( puff_dynamics ) pd
TYPE( puff_totalcc  ) pt
TYPE( puff_aerosol  ) pa
TYPE( puff_static   ) ps

REAL    znew, damp, prod, vtest
REAL    xpolar, ypolar
INTEGER icls, jtyp
LOGICAL lzinv_new

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
INTEGER, EXTERNAL :: reallocatePuffAux
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: dense_effect
INTEGER, EXTERNAL :: getPuffiskew
REAL,    EXTERNAL :: getGasDensity
REAL,    EXTERNAL :: SWIMgetGamma

!------ Get material and size group

myLockID = 0
useTemp  = multiThread .AND. (fac_srf == NOT_SET_R)

jtyp = p%ityp
icls = typeID(jtyp)%icls
imat = typeID(jtyp)%imat
CALL get_puff_material( jtyp,pmatl )

ltot    = typeID(jtyp)%ltot

!------ Get dynamic puff parameters

IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  tdyn = pd%ctp/p%c
ELSE
  tdyn = 0.0
END IF

!------ Get liquid puff parameters

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL get_liquid( p,pq )

!------ Get aerosol puff parameters

IF( IsAerosol(icls) )THEN
  CALL get_aerosol( p,pa )
  pq%d = 5.0E-5
END IF

!------ Check gas density does not exceed pure value

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  rhogas = -1.0
ELSE IF( IsGas(icls) .AND. TRIM(material(imat)%unit) == 'kg' )THEN
  rhogas  = getGasDensity( icls,pmatl )
  rhopuff = p%ccb/p%c
  IF( IsAerosol(icls) )rhopuff = rhopuff * (1.0-pa%fl)
ELSE
  rhogas = -1.0
END IF

!------ Get total cc

IF( ltot )CALL get_totalcc( p,pt )

!------ Initialize met fields, terrain, and puff variables

CALL step_init( p )
IF( nError /= NO_ERROR )GOTO 9999

IF( lsv_oper )CALL reset_lsv( si )

IF( IsAerosol(icls) )pq%t = tb + tdyn

!------ Scale turbulence for time-averaging

IF( t_avg /= DEF_VAL_R )THEN
  CALL turb_rescale( si,sv,vel2 )
END IF

!------ Check for skew turbulence in convective layer

iSkew = getPuffiskew( p )

CALL SetSkewTurb()

!------ Set surface deposition and flag for calculating time scale

lsrf   = srf_puff(jtyp,1)%nblocks > 0
ldos   = srf_puff(jtyp,2)%nblocks > 0
lscale = lsrf .OR. ldos .OR. lsmp

!------ Set deposition/settling rate

CALL get_dep( p,pmatl,pq,fac_srf,dt )

!------ Define background turbulence parameters

dtstep = dt     !Save step size for use in step_turb

CALL step_turb()

!------ Calculate washout

CALL get_wash( p,dt )
IF( nError /= NO_ERROR )GOTO 9999

!-----  advance mass

vtest = vfall
IF( lter )THEN
  stest = hx*hx*p%sxx + hy*hy*p%syy + p%szz + &
          2.0*hx*hy*p%sxy - 2.0*hx*p%sxz - 2.0*hy*p%syz
  stest = SQRT(stest/(1.+hx*hx+hy*hy))
ELSE
  stest = sz
END IF

IF( dynamic )vtest = vtest - MAX(0.0,pd%wcp/p%c)

!------ Remove deposited mass
!       N.B. Remove entire mass for rapidly depositing puff

IF( vtest*dt/stest > 2.0 .AND. zsav-hp < stest )THEN
  damp = 0.0
  cnew = 0.0
ELSE
  damp = 1.0/(1.0 + tauc*dt)
  cnew = p%c * damp
END IF

lscav = .FALSE.

!------ Advance multi-components

IF( IsMulti(icls) )THEN

  CALL StepMC( p,dt )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( p%idtl == I_STATIC .OR. isSerial )THEN

  CALL StepDepMC( p,dt,fac_srf )
  IF( nError /= NO_ERROR )GOTO 9999

  END IF

END IF

!------ Activity decay

IF( ldecay .AND. &
    (material(imat)%prop(1) + material(imat)%prop(2) > 0.) )THEN
  prod  = MAX( material(imat)%prop(1)*sun_fac,material(imat)%prop(2) )
  p%cfo = p%cfo * EXP(-prod*dt)
END IF

!------ Set surface deposition

IF( metField /= polefld_n .AND. metField /= polefld_s )THEN
  IF( lsrf .OR. ldos )THEN

    CALL step_dep( p,pt,ipuf,dt,fac_srf )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

!------ Put new mass into puff structure

p%c = cnew

!------ Check very small mass puff

IF( p%c <= cmin )THEN
  IF( dynamic )THEN
    pd%ctp = 0.0
    pd%ctb = 0.0
    pd%wcp = 0.0
    pd%wcb = 0.0
    pd%vcp = 0.0
    pd%vcb = 0.0
    pd%ucp = 0.0
    pd%ucb = 0.0
    IF( buoy_gas )THEN
      pd%bcp = 0.0
      pd%bcb = 0.0
    END IF
  END IF

  p%xuc  = p%xuc*damp
  p%xvc  = p%xvc*damp
  p%yvc  = p%yvc*damp
  p%yvsc = p%yvsc*damp
  p%yvbc = p%yvbc*damp
  p%zwc  = p%zwc*damp
  p%wc   = p%wc*damp
  p%cc   = p%cc*damp*damp
  IF( ltot )pt%cct = pt%cct*damp*damp
  p%idtl = lev1
  lblcap = p%zc > 0.
  GOTO 9998
END IF


!------ Advance puff dynamics

IF( dynamic )THEN
  CALL step_pturb( p,pd )
ELSE
  fdyn = 1.
  tauw = 0.
  wdyn = 0.
  udyn = 0.
  vdyn = 0.
  ddyn = 0.
  zdyn = 0.
  difp = 0.
END IF

!------ Advance velocity correlations

CALL step_velcorr( p,dt )

!------ Set diffusivities

CALL set_diff( p )

!------ Advance CC (and CCT)

damp = damp * damp
prod = EXP(-qosi_cc*dt)

p%cc = ( prod*p%cc + (1.-prod)*p%ccb ) * damp

IF( ltot )pt%cct = ( prod*pt%cct + (1.-prod)*pt%cctb ) * damp

!-----  advance spatial moments

CALL step_sig( p,dt )

!------ Advance turbulence scales

CALL step_scale( p,dt )

!------ Advance puff dynamics

IF( dynamic )THEN
  CALL step_dynamics( p,pd,pt,dt,pmatl,pa )
END IF

!------ Advance centroid

IF( pole_transition )THEN  !Skip Adams-Bashforth when crossing polar cap boundary
  p%xbar = p%xbar + DBLE((ub+udyn)*dt*xmap)
  p%ybar = p%ybar + DBLE((vb+vdyn)*dt*ymap)
ELSE
  p%xbar = p%xbar + DBLE((1.5*ub-0.5*p%uo+udyn)*dt*xmap)
  p%ybar = p%ybar + DBLE((1.5*vb-0.5*p%vo+vdyn)*dt*ymap)
END IF

IF( metField /= polefld_n .AND. metField /= polefld_s )THEN
  IF( global_lon )CALL SetGlobalLonD( p%xbar )
END IF

IF( lter )THEN
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,metField )
ELSE
  hp = 0.
END IF

!------ Check for inversion reflection on skew puff

IF( iSkew == SKEW_UP )THEN
  IF( zsav+wb_skew*dt > zinv )iSkew = SKEW_DOWN
END IF

!------ Limit drift velocity to prevent exiting BL

wpuff = p%wc/p%c + wb_skew
IF( lzinv )wpuff = MIN(wpuff,(zinv-zsav)/dt)

wpuff = wpuff + wdyn + 1.5*wb - 0.5*p%wo - vfall
znew  = p%zbar + wpuff*dt

!------ Get new inversion height; check for capped boundary layer

CALL SWIMgetZi( metField,SNGL(p%xbar),SNGL(p%ybar),zinv,wts )

IF( wts > 0. )THEN
  dtdzs = 0.
ELSE
  dtdzs = 0.001
END IF

IF( dtdzs <= 0. .AND. lbl )THEN
  gamma = SWIMgetGamma( metField,SNGL(p%xbar),SNGL(p%ybar),zinv )
  lblcap = gamma > 0.
ELSE
  lblcap = .FALSE.
END IF

!------ Cap previously uncapped puff if within convective BL

IF( p%zc == 0. .AND. lblcap .AND. p%zbar < zinv )THEN
  p%zc = p%zbar + 2.*sz
END IF

!------ Check inversion penetration / descending (only in unstable BL)

IF( dynamic .AND. lblcap )THEN

  lzinv_new = MAX(znew,hp+sz) <= zinv

  IF( lzinv .AND. .NOT.lzinv_new .AND. (wdyn*ABS(wdyn) > wwbl) )THEN

!------ Inversion penetration

    CALL set_penetration( p,pd,znew )

  ELSE IF( .NOT.lzinv .AND. lzinv_new .AND. (wdyn*ABS(wdyn) < -wwbl) )THEN

!------ Descending from stable region

    CALL set_descend( p,pd,znew )

  ELSE

    p%zbar = znew

  END IF

ELSE

  p%zbar = znew

END IF

!------ Check for ground reflection on skew puff

IF( iSkew == SKEW_DOWN )THEN
  IF( zsav+wb_skew*dt < hp )iSkew = SKEW_UP
END IF

!------ Prevent puffs from penetrating the ground

IF( lter )CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,metField )

p%zbar = MAX(p%zbar,hp)

!------ Adjust dense gas puff moments for change in terrain slope

IF( dense_gas .AND. IsGas(icls) )THEN
  IF( dense_effect( p%zbar-hp,p%sv,pd%wcp,p%c ) )THEN
    CALL dense_rot_norm( hxsav,hysav,hx,hy,p )
  END IF
END IF

!------ Set puff inversion height

p%zi = zinv

!------ Check for scale size on skew puff and reset iSkew

IF( iSkew /= SKEW_NONE )THEN
  IF( p%sv > 0.4*(zinv-hp) )iSkew = SKEW_NONE
END IF

CALL setPuffiskew( p,iSkew )

!------ Advance liquid puff properties

IF( IsLiquid(icls) )THEN
  CALL step_drop( p,ipuf,pt,pd,pq,pmatl,dt )
  IF( nError /= NO_ERROR )GOTO 9999
ELSE IF( IsWetParticle(icls) )THEN
  CALL step_wetpart( pq,pmatl,dt )
END IF

IF( IsAerosol(icls) )THEN

  CALL set_aerosol( p,pa,pd,pmatl )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( IsLiquid(material(imat)%icls) )THEN

!------ Check for change to vapor puff if liquid material

    IF( pa%fl == 0.0 .AND. pa%fw == 0.0 )THEN
      IF( p%idtl == I_STATIC )CALL get_static( p,ps )
      p%ityp = p%ityp - GetSubgroups( material(imat),mat_aux ) - 1
      icls   = typeID(p%ityp)%icls
      IF( p%idtl == I_STATIC )CALL put_static( p,ps )
    END IF

  END IF

END IF

!------ reset time-level

CALL step_tlev( p,dt,lev1,lev2 )
IF( nError /= NO_ERROR )GOTO 9999

!------ Continuation point for small-mass puffs

9998 CONTINUE

!------ Transform puff coordinates into polar cap coordinates if necessary

IF( metField == polefld_n .OR. metField == polefld_s )THEN

  CALL SWIMPolarCarttoLL( SNGL(p%xbar),SNGL(p%ybar),xpolar,ypolar )

  p%xbar = DBLE(xpolar)

  IF( metField == polefld_s )THEN
    p%ybar = -DBLE(ypolar)
  ELSE
    p%ybar = DBLE(ypolar)
  END IF

  CALL polar_rot( p )

END IF

!------ save velocities for Adams-Bashforth advection scheme

p%uo = ub; p%vo = vb; p%wo = wb

!----- save dynamics

IF( dynamic )CALL put_dynamics( p,pd )

!------ save liquid puff properties

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL put_liquid( p,pq )

IF( IsAerosol(icls) )CALL put_aerosol( p,pa )

!----- save total cc

IF( ltot )CALL put_totalcc( p,pt )

!------ if necessary, put tscale into p.sr

IF( lscale )p%sr = tscale

!------ check for vertical splitting; set capping height


IF( p%c > cmin .AND. fac_srf == NOT_SET_R )THEN
  CALL step_split( p,ipuf,dt,lev1,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999
ELSE
  CALL set_zcap_static( p,dt )
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE chk_shr( s12,s21,sig11,sig22,dt )

IMPLICIT NONE

REAL, PARAMETER :: MIN_SHR = 1.E-10

REAL, INTENT( IN    ) :: s12, s21, sig11, sig22
REAL, INTENT( INOUT ) :: dt

IF( ABS(s12) >= MIN_SHR )THEN
  dt = MIN(dt,SQRT(0.25*sig11/sig22)/ABS(s12))
END IF

IF( ABS(s21) >= MIN_SHR )THEN
  dt = MIN(dt,SQRT(0.25*sig22/sig11)/ABS(s21))
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE turb_rescale( scl_i,scl_v,v2 )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: scl_i, scl_v, v2

REAL, PARAMETER :: ALPT = 0.03 ! Conditional avg. scale factor

REAL stb, stl, stbh, stbv, fact, soq, amet, bmet

REAL, EXTERNAL :: fscale_lsv

!------ Set length scales based on velocity and averaging time

stb  = ALPT * SQRT(v2 + 2.*uubl + 2.*vvbl + wwbl) * t_avg
stl  = ALPT * SQRT(v2 + uub  + vvb) * t_avg

stbh = MAX(scl_i,stb)
stbv = MAX(scl_v,stb)
stl  = MAX(scl_i,stl)

!----- Large-scale horizontal turbulence

IF( stl < sby )THEN
  fact = ( stl/sby )**TwoThirds
  fact = fact*fscale_lsv( stl,sby,sbl,uub+vvb,2.*(uubl+vvbl) )
  uub  = uub * fact
  vvb  = vvb * fact
  uvb  = uvb * fact
  uubz = uubz * fact
  vvbz = vvbz * fact
  uvbz = uvbz * fact
  sby    = stl
  sb_lsv = stl
END IF

!-----  Shear driven horizontal turbulence

IF( stbh < sbls )THEN
  fact  = ( stbh/sbls )**TwoThirds
  uubl  = uubl * fact
  wwbh  = wwbh * fact
  sbls  = stbh
  dddx  = dddx * fact*fact
  dddy  = dddy * fact*fact
END IF

!-----  Buoyancy driven horizontal turbulence

IF( stbh < sbl )THEN
  fact = ( stbh/sbl )**TwoThirds
  vvbl = vvbl * fact
  sbl  = stbh
END IF

!-----  Vertical turbulence

IF( stbv < sbz )THEN
  fact = ( stbv/sbz )**TwoThirds
  wwbl = wwbl * fact
  qqs  = qqs  * fact
  wwz  = wwz * fact
  ws2  = ws2 * fact
  sbz  = stbv
  wtbl = wtbl * fact*fact
  soq  = sbz/MAX(SQRT(qqs),1.E-3)
  bmet = EQF*soq*soq
  amet = (wwbl*soq/a + bmet*wtbl)/(1.+2.*bmet*dtdz)
  dddz = amet*dddz/MAX(difb,1.E-6)
  difb = amet
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE reset_lsv( si )

USE scipuff_fi
USE met_fi

!-----  scale large-scale horizontal turbulence for operational LSV

IMPLICIT NONE

REAL, INTENT( IN ) :: si

REAL stl, fact

REAL, EXTERNAL :: fscale_lsv

stl = MAX(si,sbl)

IF( stl < sb_lsv )THEN

  fact = ( stl/sb_lsv )**TwoThirds
  fact = fact*fscale_lsv( stl,sb_lsv,sbl,uub+vvb,2.*(uubl+vvbl) )

  uub  = uub * fact
  vvb  = vvb * fact
  uvb  = uvb * fact
  uubz = uubz * fact
  vvbz = vvbz * fact
  uvbz = uvbz * fact
  sby  = stl

  sb_lsv = stl

END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION fscale_lsv( s,sl,sb,qql,qqb )


USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: s   !Concentration scale
REAL, INTENT( IN ) :: sl  !Large-scale (LSV)
REAL, INTENT( IN ) :: sb  !BL scale
REAL, INTENT( IN ) :: qql !Large-scale variance
REAL, INTENT( IN ) :: qqb !BL scale variance

REAL fact, uux, vvx

IF( s < 2.*sb .AND. qql > 0. )THEN

  fact = ( sb/sl )**TwoThirds
  uux  = qql * fact
  vvx  = -qqb + MAX(qqb,uux)
  fscale_lsv = vvx / uux

  IF( s > sb )THEN
    fact = (s-sb)/sb
    fact = fact*fact*(3.-2.*fact)
    fscale_lsv = fact + (1.-fact)*fscale_lsv
  END IF

ELSE

  fscale_lsv  = 1.

END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE get_dep( p,pmatl,pq,fac_srf,dt )

USE scipuff_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN ) :: p         !Puff structure
TYPE( puff_material ), INTENT( IN ) :: pmatl     !Puff material
TYPE( puff_liquid ),   INTENT( IN ) :: pq        !Liquid drop structure
REAL,                  INTENT( IN ) :: fac_srf   !Surface factor for static puffs
REAL,                  INTENT( IN ) :: dt        !Timestep

INTEGER jtyp, icls
REAL    fac_evap, t_srf
LOGICAL lEvap

TYPE( gas_material )  pmatgas
TYPE( puff_material ) pmatlx

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol, IsLiquid
LOGICAL, EXTERNAL :: IsEvap, IsWetParticle


jtyp = p%ityp
icls = typeID(jtyp)%icls

vfall = 0.0
sigvd = 0.0

IF( fac_srf /= NOT_SET_R )THEN
  lEvap = IsEvap( typeID(p%ityp)%icls )
  IF( lEvap )THEN
    t_srf = dt*fac_srf
    CALL SetStaticEvap( p,pq,dt,t_srf,fac_evap )
  END IF
ELSE
  lEvap = .FALSE.
END IF

IF( IsAerosol(icls) )THEN

!-----  Aerosol puff

  IF( IsAerosol(material(imat)%icls) )THEN
    CALL set_aer_vdry( pmatl )
  ELSE
    CALL get_puff_material_gas( pmatlx,material(imat)%iaux )
    pmatgas = TRANSFER(pmatlx,pmatgas)
    CALL set_gas_vdry( pmatgas )
  END IF

ELSE IF( IsGas(icls) )THEN

!-----  GAS material

  IF( p%sr > SMALL )THEN
      pmatgas = TRANSFER(pmatl,pmatgas)
    CALL set_gas_vdry( pmatgas )
  ELSE
    vdry = 0.
  END IF

ELSE IF( IsParticle(icls) )THEN

!-----  PARTICLE material

  CALL set_part_vdry( p,pmatl )

ELSE IF( IsLiquid(icls) )THEN

!-----  LIQUID material

  CALL set_liquid_vdry( p,pmatl,pq )

ELSE IF( IsWetParticle(icls) )THEN

!-----  WET PARTICLE material

  CALL set_wetpart_vdry( p,pmatl,pq )

END IF

!-----  Total deposition rate is gravitational plus dry

vdtot = vfall*area_fac + vdry

!-----  Define deposition time scale for implicit step

tauc  = p%sr*vdtot/csav

IF( lEvap )tauc = tauc*fac_evap

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_gas_vdry( pmatl )

USE scipuff_fi
USE step_p_fi

IMPLICIT NONE

TYPE( gas_material ), INTENT( IN ) :: pmatl

vdry = pmatl%vd

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_aer_vdry( pmatlIn )

USE scipuff_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_material ), INTENT( IN ) :: pmatlIn     !Puff material

TYPE( aerosol_material ) pmatl

pmatl = TRANSFER(pmatlIn,pmatl)

vdry = pmatl%vd

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_part_vdry( p,pmatlIn )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN ) :: p         !Puff structure
TYPE( puff_material ), INTENT( IN ) :: pmatlIn   !Puff material

TYPE( part_material ) pmatl

REAL rhoa, dm, pd

REAL, EXTERNAL :: ufall, fun_rhoa

pmatl = TRANSFER(pmatlIn,pmatl)

rhoa  = fun_rhoa( tb,pb )
rhod  = pmatl%rho
dm    = pmatl%dbar
vfall = ufall( rhoa,rhod,rmuair,dm )

IF( (zbar < 2.*sv+hp) .OR. vfall == 0.0 )THEN
  sigvd = 0.0
ELSE
  sigvd = pmatl%sigvd*vfall/pmatl%vd
END IF

IF( p%sr > SMALL )THEN
  pd  = pmatl%diff
  CALL vdep_dry( ustdep,hc,zruf,vfall,dm,pd,vdry )
ELSE
  vdry = 0.
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_liquid_vdry( p,pmatlIn,pq )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN ) :: p         !Puff structure
TYPE( puff_material ), INTENT( IN ) :: pmatlIn   !Puff material
TYPE( puff_liquid ),   INTENT( IN ) :: pq        !Liquid drop structure

TYPE( liquid_material ) pmatl

REAL rhoa, tem

REAL, EXTERNAL :: ufall, fun_rhoa

pmatl = TRANSFER(pmatlIn,pmatl)

rhoa = fun_rhoa( tb,pb ) !rhoair*(pb/PSURF)*(283.2/tb) !*(pb**KAPPAC)*273.2/tb
rhod = pmatl%rho - pmatl%rhob*(pq%t+ABSZERO)

!------ Calculate liquid drop aspect ratio

CALL drop_deform( rhod,rhoa,pq%d,pmatl%st,aspect )

!-----  Set adjusted density to account for droplet distortion

tem   = rhod/(aspect*aspect)
vfall = ufall( rhoa,tem,rmuair,pq%d )

IF( (zbar < 2.*sv+hp) .OR. vfall == 0.0 )THEN
  sigvd = 0.0
ELSE
  sigvd = vfall*MIN( pq%sigd*pmatl%sigvd,0.3333333 )
END IF

IF( p%sr > SMALL )THEN
  CALL vdep_dry( ustdep,hc,zruf,vfall,pq%d,pmatl%diff,vdry )
ELSE
  vdry = 0.
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_wetpart_vdry( p,pmatlIn,pq )

USE scipuff_fi
USE met_fi
USE step_p_fi

! Calculates the fall velocity and dry deposition for a wet particle
! Assumes that the wetting agent is water

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN ) :: p         !Puff structure
TYPE( puff_material ), INTENT( IN ) :: pmatlIn   !Puff material
TYPE( puff_liquid ),   INTENT( IN ) :: pq        !Liquid drop structure

TYPE( part_material ) pmatl

INTEGER i
REAL    rhol, stl
REAL    rhoa, tem, pd, vpart, vliq, vtot

REAL, EXTERNAL :: ufall, fun_rhoa

pmatl = TRANSFER(pmatlIn,pmatl)

!------ Set air density

rhoa = fun_rhoa( tb,pb ) !rhoair*(pb/PSURF)*(283.2/tb) !*(pb**KAPPAC)*273.2/tb

!------ Modify density for particle contribution

vpart = pmatl%dbar**3
vtot  = pq%d**3
vliq  = vtot - vpart

IF( material(imat)%AuxMatID /= NOT_SET_I )THEN
  i    = material(imat)%AuxMatID
  rhol = WetPartLiquidMat(i)%rho
  stl  = WetPartLiquidMat(i)%st
ELSE
  rhol = RHO_WATER
  stl  = ST_WATER
END IF

rhod = (rhol*vliq + pmatl%rho*vpart)/vtot

!------ Calculate liquid drop aspect ratio

CALL drop_deform( rhod,rhoa,pq%d,stl,aspect )

!-----  Set adjusted density to account for droplet distortion

tem   = rhod/(aspect*aspect)
vfall = ufall( rhoa,tem,rmuair,pq%d )
IF( (zbar < 2.*sv+hp) .OR. vfall == 0.0 )THEN
  sigvd = 0.0
ELSE
  sigvd = vfall*MIN( pq%sigd/pq%d,0.3333333 )
END IF

IF( p%sr > SMALL )THEN
  pd  = 0.0      !Assume large droplet
  CALL vdep_dry( ustdep,hc,zruf,vfall,pq%d,pd,vdry )
ELSE
  vdry = 0.
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_sig( p,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt

REAL, PARAMETER :: EPS_DET = 1.E-4     ! Relative limit for puff distortion
REAL, PARAMETER :: FAC     = 2.0
REAL, PARAMETER :: FAC_LIM = 0.5*SQRT3 ! limit on centroid shift

REAL det_fac, sxx, sxy, sxz, syy, syz, szz
REAL smax, xfac, yfac, zfac, zwcs, szlim
REAL del, rx, ry, sx, sy, dtx, dtt
REAL shx, shy, svv, dudzl, dvdzl
REAL zp, hx0, hy0

LOGICAL, EXTERNAL :: check_slope
REAL,    EXTERNAL :: rlimit, erfc

!------ Set total vertical diffusivity to include differential fall velocities

zwcs = zwct + sigvd*sz

!------ set default local terrain slope for anisotropic diffusivity

sz  = SQRT(p%szz)
hx0 = 0.0
hy0 = 0.0
zp  = p%zbar - hp

IF( lter )THEN

!------ set local terrain slope for anisotropic diffusivity near-surface puffs

  IF( lzinv .OR. lcap )THEN
    hx0 = hx
    hy0 = hy
  END IF

END IF

!------ Puff distortion factor

det_fac = p%det/(p%sxx*p%syy*p%szz)

!------ reduce sigma-z for puffs depositing on the ground through gravitational settling

IF( vfall > 0.0 .AND. zp < 3.0*stest .AND. det_fac >= EPS_DET )THEN
  IF( lter )CALL dense_rot_norm( hx,hy,0.,0.,p ) !Rotate into coord. aligned with terrain

  zfac  = 1.0 - EXP(-0.5*zp*zp/stest/stest) * (1.0-EXP(-1.8*vfall*dt/stest))  !90% of max rate
  zfac  = MAX(zfac,0.01)
  zfac  = MIN(MAX(zfac,(cnew/csav)**2),1.)    !Limited based on mass reduction
  p%szz = p%szz * zfac
  zfac  = SQRT(zfac)
  p%sxz = p%sxz * zfac
  p%syz = p%syz * zfac
  IF( lter )CALL dense_rot_norm( 0.,0.,hx,hy,p ) !Rotate back

END IF

!-----  check for extreme puff distortion

IF( det_fac < EPS_DET .OR. p%cc <= cmin2 )THEN !If distorted, only diffuse puffs

!----- Reset zcap if necessary for terrain elevation changes

  IF( p%zc > 0.0 )THEN
    szlim = SQRT(p%szz)/WMX
    IF( hp+szlim > p%zc )THEN
      p%zc = hp + szlim
      zwcs = 0.0
    END IF
  END IF

  p%sxx = p%sxx + 2.*xuct*dt
  p%syy = p%syy + 2.*yvct*dt
  p%szz = p%szz + 2.*zwcs*dt
  p%sxy = p%sxy + 2.*xvct*dt

!------ Limit vertical spread in capped BL

  IF( p%zc > 0.0 )THEN
    szlim = MAX(sz,WMX*(p%zc-hp))
    IF( p%szz > szlim*szlim )THEN
      szlim = szlim/SQRT(p%szz)
      p%szz = p%szz*szlim*szlim
      p%sxz = p%sxz*szlim
      p%syz = p%syz*szlim
    END IF
  END IF

  dts  = 1.E36

ELSE

!-----  Limit time step based on velocity shears and diffusivity

  smax = MAX( ABS(dudx),ABS(dvdy),ABS(dudx+dvdy),0.01/dt )
  dts  = 0.25/smax
  shx  = MAX( si*si,p%sxx )
  shy  = MAX( si*si,p%syy )
  svv  = MAX( sv*sv,p%szz,zruf**2 )
  smax = 2.*MAX( xuct/shx,yvct/shy,zwcs/svv,0.01/dt )
  dts  = MIN( dts,1.0/smax )
  CALL chk_shr( dudy,dvdx,p%sxx,p%syy,dts )
  CALL chk_shr( dvdz,dwdy,p%syy,p%szz,dts )
  CALL chk_shr( dwdx,dudz,p%szz,p%sxx,dts )

  dtx = MIN( dt,dts )

!------ reduce dynamic velocity if shear-limited

  wdyn = wdyn*dtx/dt
  ddyn = ddyn*dtx/dt

!------ Anisotropic diffusivity

  CALL dense_rot_norm( hx0,hy0,0.,0.,p ) !Rotate into coord. aligned with terrain

!----- Reset zcap if necessary for terrain elevation changes

  IF( p%zc > 0.0 )THEN
    szlim = SQRT(p%szz)/WMX
    IF( hp+szlim > p%zc )THEN
      p%zc = hp + szlim
      zwcs = 0.0
    END IF
  END IF

  sxx = p%sxx
  syy = p%syy

  shx = 1./3.

  p%sxx = p%sxx + 2.*xuct*dtx   !Apply diffusivity parallel to surface
  p%syy = p%syy + 2.*yvct*dtx
  p%szz = p%szz + 2.*zwcs*dtx * shx
  p%sxy = p%sxy + 2.*xvct*dtx

!------ Meander correction for dense puff

  IF( ABS(zdyn*dt) > 1.E-6 )THEN
    dtt  = MIN(dt,0.25/ABS(zdyn))
    zfac = EXP(2.*zdyn*dtt)
    xfac = sxx / p%sxx
    yfac = syy / p%syy
    rx   = xfac*yfac
    IF( rx > zfac )THEN
      zfac = rx
      ddyn = (ddyn - zdyn)*dtt/dt + LOG(zfac)/(2.*dtt)
    ELSE
      ddyn = ddyn*dtt/dt
    END IF
    p%szz = p%szz * zfac
    zfac  = SQRT(zfac)
    p%sxz = p%sxz * zfac
    p%syz = p%syz * zfac
  END IF

!------ Limit vertical spread in capped BL

  IF( p%zc > 0.0 )THEN
    szlim = WMX*(p%zc-hp)
    IF( p%szz > szlim*szlim )THEN
      szlim = szlim/SQRT(p%szz)
      p%szz = p%szz*szlim*szlim
      p%sxz = p%sxz*szlim
      p%syz = p%syz*szlim
      shx   = 0.
    ELSE
      shx = 1. - shx
    END IF
  ELSE
    shx = 1. - shx
  END IF

  CALL dense_rot_norm( 0.,0.,hx0,hy0,p ) !Rotate back

!-----  Diagonal strain

  xfac = EXP(dudx*dtx)
  yfac = EXP(dvdy*dtx)
  zfac = 1.0/(xfac*yfac)
  sxx = p%sxx*xfac*xfac
  syy = p%syy*yfac*yfac
  szz = p%szz*zfac*zfac
  sxy = p%sxy/zfac
  sxz = p%sxz/yfac
  syz = p%syz/xfac

  dudzl = dudz
  dvdzl = dvdz
  IF( p%zc > 0.0 )THEN
    szlim = p%zc - hp
    IF( szz > szlim*szlim/12. )THEN
      zfac  = szlim*szlim/(12.*szz)
      dudzl = dudzl*zfac
      dvdzl = dvdzl*zfac
    END IF
  END IF

!-----  dudy,dvdx

  CALL step_sym_shr( dudy,dvdx,sxx,sxy,sxz,syy,syz,dtx )

!-----  dvdz, dwdy

  CALL step_sym_shr( dvdzl,dwdy,syy,syz,sxy,szz,sxz,dtx )

!-----  dwdx, dudz

  CALL step_sym_shr( dwdx,dudzl,szz,sxz,syz,sxx,sxy,dtx )

!-----  complete vertical diffusion

  p%sxx = sxx
  p%syy = syy
  p%szz = szz
  p%sxy = sxy
  p%sxz = sxz
  p%syz = syz

  IF( shx*zwcs > 0. )THEN

    CALL dense_rot_norm( hx0,hy0,0.,0.,p ) !Rotate into coord. aligned with terrain

    p%szz = p%szz + 2.*zwcs*dtx * shx
    IF( p%zc > 0. )THEN
      szlim = WMX*(p%zc-hp)
      IF( p%szz > szlim*szlim )THEN
        szlim = szlim/SQRT(p%szz)
        p%szz = p%szz*szlim*szlim
        p%sxz = p%sxz*szlim
        p%syz = p%syz*szlim
      END IF
    END IF

    CALL dense_rot_norm( 0.,0.,hx0,hy0,p ) !Rotate back

  END IF

  sz = SQRT( p%szz )

!------ limit vertical diffusion of "below ground" mass by moving centroid
!       and reducing horizontal spread

  IF( p%zbar-hp < 3.*sz )THEN
    IF( .NOT.check_slope(hx,hy) )THEN
      sx     = SQRT( p%sxx )
      sy     = SQRT( p%syy )
      del    = rlimit( p%sr*zwcs*dtx/(sz*p%c),0.,1. )
      del    = del*erfc((p%zbar-hp)/(SQRT2*sz))
      rx     = rlimit( p%sxz/(sx*sz),-1.,1. )
      ry     = rlimit( p%syz/(sy*sz),-1.,1.)
      xfac   = rlimit( FAC*del*rx,-FAC_LIM,FAC_LIM )
      yfac   = rlimit( FAC*del*ry,-FAC_LIM,FAC_LIM )
      p%xbar = p%xbar + DBLE(xfac*sx*xmap)
      p%ybar = p%ybar + DBLE(yfac*sy*ymap)
      xfac   = 1. - xfac**2
      yfac   = 1. - yfac**2
      p%sxx  = p%sxx*xfac
      p%syy  = p%syy*yfac
      xfac   = SQRT( xfac )
      yfac   = SQRT( yfac )
      p%sxy  = p%sxy*xfac*yfac
      p%sxz  = p%sxz*xfac
      p%syz  = p%syz*yfac
    END IF
  END IF

END IF

!-----  Make sure cap contains puff

IF( p%zc > 0.0 )p%zc = MAX(p%zc,hp + sz/WMX)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_sym_shr( dudy,dvdx,sxx,sxy,sxz,syy,syz,dt )

IMPLICIT NONE

REAL, INTENT( IN    ) :: dudy, dvdx
REAL, INTENT( INOUT ) :: sxx, sxy, sxz, syy, syz
REAL, INTENT( IN    ) :: dt

REAL txx, txy, tyy, txz, tyz
REAL xdudy, xdvdx, fac, fac2

xdudy = dudy*dt
xdvdx = dvdx*dt

fac2  = 1.0/(1.0-xdudy*xdvdx)
fac   = SQRT(fac2)

txx   = xdudy*(sxy + sxy + xdudy*syy)
tyy   = xdvdx*(sxy + sxy + xdvdx*sxx)
txy   = xdudy*syy + xdvdx*sxx + xdudy*xdvdx*sxy
txz   = xdudy*syz
tyz   = xdvdx*sxz

sxx   = (sxx + txx)*fac2
syy   = (syy + tyy)*fac2
sxy   = (sxy + txy)*fac2
sxz   = (sxz + txz)*fac
syz   = (syz + tyz)*fac

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_init( p )

USE scipuff_fi
USE met_fi
USE step_p_fi
USE SWIMparam_fd

!   Initialize the met fields, save local puff variables

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER inField
REAL    Shh, Szz, dElev, xlat, xlon
REAL    xpolar, ypolar
REAL    tgas, pgas, fac

INTEGER, EXTERNAL :: getPuffifld
REAL,    EXTERNAL :: SWIMgetGamma

!------ set min mass factor

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  cmin2 = 0.0
ELSE
  cmin2 = 2.*material(imat)%prop(3)*p%c
END IF

!----- calculate met-field values

inField = getPuffifld( p )

Shh = 0.5*(p%sxx+p%syy)
Szz = MIN(p%szz,1./p%azz)

IF( p%zbar > zmax )THEN
!$OMP ATOMIC
  nPuffAboveTop = nPuffAboveTop + 1
END IF

CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,Szz,p%zc,0, &
                                 inField,Shh,metField,dElev )
IF( nError /= NO_ERROR )GOTO 9999

p%zbar = p%zbar + dElev
p%zi   = p%zi   + dElev
IF( p%zc > 0.0 )p%zc = p%zc + dElev

CALL setPuffifld( p,metField )

!------ Transform puff coordinates into polar cap coordinates if necessary

pole_transition = .FALSE.

IF( metField == polefld_n .OR. metField == polefld_s )THEN

  CALL SWIMLLtoPolarCart( SNGL(p%xbar),SNGL(p%ybar),xpolar,ypolar )

  lonsav = p%xbar

  xlat = TAN(PI180*(90.-SNGL(p%ybar)))
  xlon = PI180*SNGL(p%xbar)
  xmap = 1.0/SQRT(1.0 + (xlat*COS(xlon))**2)
  ymap = 1.0/SQRT(1.0 + (xlat*SIN(xlon))**2)
  p%xbar = DBLE(xpolar)
  p%ybar = DBLE(ypolar)

  IF( inField /= metField )pole_transition = .TRUE.

ELSE

  CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

  IF( inField == polefld_n .OR. &
      inField == polefld_s )pole_transition = .TRUE.

END IF

!-----  set terrain factors (height, slopes, and area fac)

IF( lter )THEN
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,metField )
  area_fac = 1./SQRT(1.+hx*hx+hy*hy)
ELSE
  area_fac = 1.
  hp       = 0.
  hx       = 0.
  hy       = 0.
END IF

!-----  Limit negative dynamic temperature perturbations

IF( dynamic )tdyn = MAX(100. - tb,tdyn)

!-----  Check gas density does not exceed pure value

IF( rhogas > 0.0 )THEN
  pgas = pb/PSURF
  tgas = tb + tdyn
  rhogas = rhogas*pgas*293./tgas
  IF( rhopuff > rhogas )THEN
    fac = SQRT(rhopuff/rhogas)
    p%sv  = fac * p%sv
    p%sxx = fac * p%sxx
    p%syy = fac * p%syy
    fac   = fac*fac
    p%szz = fac * p%szz
  END IF
END IF

!----- save puff variables at start of step

csav  = p%c
xsav  = p%xbar
ysav  = p%ybar
zsav  = p%zbar
hsav  = hp
hxsav = hx
hysav = hy
zisav = p%zi

!-----  define local puff variables

si   = p%si
si2  = p%si2
sv   = p%sv
zbar = p%zbar
sz   = SQRT(p%szz)

!-----  modified gravity factor

dtdz = MAX(dtdz,0.0)
gt0  = G0/thb

!-----  save puff inversion and set flags

p%zi  = zinv
lzinv = (MAX(p%zbar,hp+sz) <= zinv) .AND. lbl

IF( lzinv .AND. dtdzs <= 0. )THEN
  gamma = SWIMgetGamma( metField,SNGL(p%xbar),SNGL(p%ybar),zinv )
  lcap = gamma > 0.
ELSE
  lcap = .FALSE.
END IF

!-----  cap previously uncapped puff if within convective BL

IF( p%zc == 0. .AND. lcap )p%zc = p%zbar + 2.*sz

!-----  set Brunt-Vaisala frequency to damp dynamics - use overlying
!       stability for BL puffs near the inversion

IF( lcap .AND. zbar+sv > zinv )THEN
  bv = SQRT(gt0*MAX(gamma,dtdz))
ELSE
  bv = SQRT(gt0*dtdz)
END IF

!-----  velocity-squared for later computations

vel2 = ub*ub + vb*vb

!-----  compute settling rate (z=0.0)

CALL settle( p,-1.0,hc,p%sr,fac_rfl,zexp_dos )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_dep( p,pt,ipuf,dt,fac_srf )

USE scipuff_fi
USE met_fi
USE step_p_fi
USE surface_dose_fd
USE utilsrf
USE chem_fi, ONLY: lscav
USE grdlock_fi

!   Update the surface integrated fields - dose and deposition

IMPLICIT NONE

TYPE( puff_str ),     INTENT( INOUT ) :: p
TYPE( puff_totalcc ), INTENT( IN    ) :: pt
INTEGER,              INTENT( IN    ) :: ipuf
REAL,                 INTENT( IN    ) :: dt
REAL,                 INTENT( IN    ) :: fac_srf

INTEGER ios
LOGICAL dosflg, depflg, washflg
REAL    cmax, rat, ratt, ratx, rattx, ratb, rattb, ratbx, rattbx
REAL    vx, cx, cdos
REAL    ts

REAL, DIMENSION(ISRF_TOT) :: sdat

INTEGER, EXTERNAL :: deallocatePuffAux

!---- Set flags for surface contributions

depflg  = lsrf .AND. (p%sr  > SMALL)
depflg = depflg .OR. lscav
washflg = lsrf .AND. (fwash > SMALL)

IF( ldos )THEN
  IF( z_dosage > 0.0 .OR. hc > 0. )THEN   !Recalculate surface integral with original puff mass
    CALL settle( p,z_dosage,0.,cdos,fac_rfl,zexp_dos )
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    cdos = p%sr
  END IF
  dosflg = cdos > SMALL
ELSE
  dosflg = .FALSE.
END IF

IF( dosflg .OR. depflg .OR. washflg )THEN

!----- Set fluctuation ratios for puff

  rat  = tscale*MAX((p%cc-p%ccb),0.01*p%ccb)/csav
  ratb = zexp_dos*p%ccb/csav * 2.*SQRT2
  IF( ltot )THEN
    ratt  = tscale*MAX((pt%cct-pt%cctb),0.01*pt%cctb)/csav
    rattb = zexp_dos*pt%cctb/csav * 2.*SQRT2
  ELSE
    ratt  = 0.
    rattb = 0.
  END IF

  cmax = p%cc/csav

!----- Surface mass deposition integral

  IF( depflg )THEN
    myLockID = 1
    cx    = (1. - fwash)*(csav - cnew)
    IF( fac_srf /= NOT_SET_R )cx = cx*fac_srf
    ratx   = vdtot*rat
    rattx  = vdtot*ratt
    ratbx  = ratb
    rattbx = rattb
    IF( BTEST(run_mode,DINCRMNT) )THEN
      ts = MIN(tscale,t-tLastDep)
    ELSE
      ts = tscale
    END IF
    CALL set_sdep( sdat,cx,ratx,rattx,p%si,cmax,ratbx,rattbx,ts )
    CALL surface_dose( puff(ipuf),sdat,srfdep,0.0, &
                       srf_block, &
                       srf_puff(:,1), &
                       srftyp )
    IF( nError /= NO_ERROR )GOTO 9999

    myLockID = 0
  END IF

!----- Surface inhalation dose integral

  IF( dosflg )THEN
    myLockID = 2
    IF( BTEST(run_mode,REVERSE_MODE) )THEN
      imat = typeID(p%ityp)%imat
      srfdos = srfdosAdj(imat)
    ELSE
      srfdos = Psrfdos%ID
    END IF

    cdos = cdos*dt
    IF( fac_srf /= NOT_SET_R )cdos = cdos*fac_srf
    IF( BTEST(run_mode,DINCRMNT) )THEN
      ts = MIN(tscale,t-tLastDos)
    ELSE
      ts = tscale
    END IF
    CALL set_sdep( sdat,cdos,rat,ratt,p%si,cmax,ratb,rattb,ts )
    CALL surface_dose( puff(ipuf),sdat,srfdos,z_dosage, &
                       srf_block(ndep_blocks+1:),&
                       srf_puff(:,2), &
                       srftyp(ntyps+1:) )
    IF( nError /= NO_ERROR )GOTO 9999

    myLockID = 0
  END IF

!-----  Surface washout mass deposition integral

  IF( washflg )THEN
    myLockID = 1
    cx = fwash*(csav - cnew)
    IF( fac_srf /= NOT_SET_R )cx = cx*fac_srf

    vx    = taur*p%sv
    ratx  = vx*rat
    rattx = vx*ratt
    IF( BTEST(run_mode,DINCRMNT) )THEN
      ts = MIN(tscale,t-tLastDos)
    ELSE
      ts = tscale
    END IF
    CALL set_sdep( sdat,cx,ratx,rattx,p%si,cmax,ratb,rattb,ts )
    CALL surface_dose( washPuff,sdat,srfdep,0.0, &
                       srf_block,&
                       srf_puff(:,1), &
                       srftyp )
    IF( nError /= NO_ERROR )GOTO 9999
    myLockID = 0
  END IF

END IF

9999 CONTINUE

IF( ASSOCIATED(washPuff) )THEN
  ios = deallocatePuffAux( washPuff )
  DEALLOCATE( washPuff,STAT=ios )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE StepMC( p,dt )

USE scipuff_fi
USE mpi_fi, ONLY: isSerial
USE chem_fi, ONLY: DoChemistry

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt

INTEGER mcID

mcID = material(typeID(p%ityp)%imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    IF( DoChemistry )THEN
    CALL StepChem( mat_mc%ID(mcID),p,dt )
    END IF

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'StepMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)

    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE StepDepMC( p,dt,fac_srf )

USE scipuff_fi
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt
REAL,             INTENT( IN    ) :: fac_srf

INTEGER mcID
REAL    facMC

IF( fac_srf /= NOT_SET_R )THEN
  facMC = fac_srf
ELSE
  facMC = 1.0
END IF

mcID = material(typeID(p%ityp)%imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL StepChemDep( p,mat_mc%ID(mcID),dt,facMC )

 CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'StepDepMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)

    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_turb()

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Define background turbulence parameters (variances and scales)

IMPLICIT NONE

REAL qsh, qb, ql, qvx, svlim
REAL velc, rat

!-----  define effective horizontal length scale

CALL set_turb( uubt,vvbt,uvbt )

!-----  velocity scale

qs  = SQRT(qqs)
qsh = SQRT(qqsh)
qb  = SQRT(qqb)
ql  = SQRT(qql)

!-----  calculate horizontal and vertical time scales

aqsosyt = A*qsh/sbls
aqbosyt = A*qb/sbl
aqlosyt = A*ql/sby
aqoszt  = A*qs/sbz

!-----  internal horizontal plume time and velocity scales

CALL set_qi()

!-----  internal vertical plume time and velocity scales

svlim = WMX*(zinv-hp)

IF( lzinv .AND. sv >= svlim )THEN

  qvi   = 0.
  svq   = 0.
  sigvd = 0.

ELSE

  IF( sv <= sbz )THEN
    qvi = ((sv/sbz)**0.3333333) * qs
  ELSE
    qvi = (sbz/sv) * qs
  END IF

  qvx = difb/BS/sv

  IF( qvi > qvx )THEN
    qvi = qvx
    svq = difb/sv
  ELSE
    svq = CSI1*qvi
  END IF

  qvi = fac_diss * qvi

  IF( lcap .AND. sv >= 0.5*svlim )THEN  !Reduce dissipation approaching well-mixedness
    rat = 1.0 + MAX(0.,(qi/qtot-0.5)*2.)      !Enhance limit for non-meandering plumes
    rat = MIN(1.0,rat*(svlim-sv)/(svq*dtstep))
    qvi = qvi * MAX(rat,(2.*(svlim-sv)/svlim)**2)
  END IF

END IF

!-----  internal fluctuation damping time scale

qosi_cc = BS*(qi/si + qi/si2 + qvi/sv)

!----- set time and space scales

IF( lscale )THEN
  velc = SQRT(vel2 + qqsh + qqb + qql + 1.E-6)
  CALL set_tscale( si,si2,velc,tscale )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_qi()

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Define horizontal cc dissipation velocity scale

IMPLICIT NONE

REAL uubi

REAL, PARAMETER :: FACLIM = 0.25

REAL, EXTERNAL :: fscale_lsv

qtot = 0.0

!----- Large-scale variance

IF( qql > SMALL )THEN
  IF( si <= sby )THEN
    qi   = ((si/sby)**TwoThirds) * qql
    qi   = qi * fscale_lsv( si,sby,sbl,qql,2.*(uubl+vvbl) )
    qtot = qtot + qql - qi
  ELSE
    qi = ((sby/si)**2) * qql
    qi = qi * fscale_lsv( si,sby,sbl,qql,2.*(uubl+vvbl) )
  END IF
  siq = (CSI2**2) * qi
ELSE
  qi  = 0.0
  siq = 0.0
END IF

!----- Buoyancy scale

IF( qqb > SMALL )THEN
  IF( si <= sbl )THEN
    uubi = ((si/sbl)**TwoThirds) * qqb
    qtot = qtot + qqb - uubi
  ELSE
    uubi = ((sbl/si)**2) * qqb
  END IF
  siq = siq + (CSI1**2) * uubi
  qi  = qi  + uubi
END IF

!----- Shear scale

IF( qqsh > SMALL )THEN
  IF( si <= sbls )THEN
    uubi = ((si/sbls)**TwoThirds) * qqsh
    qtot = qtot + qqsh - uubi
  ELSE
    uubi = ((sbls/si)**2) * qqsh
  END IF
  siq = siq + (CSI1**2) * uubi
  qi  = qi + uubi
END IF

IF( qtot < FACLIM*qi )THEN
  fac_diss = 4.0 - 3.*qtot/qi/FACLIM   !Increase dissipation rate for internal fluctuations only
  qi       = fac_diss * qi
  fac_diss = SQRT(fac_diss)
ELSE
  fac_diss = 1.0
END IF
qtot = SQRT(qtot+qi)

qi  = SQRT(qi)
siq = SQRT(siq)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_tscale( si,si2,velc,tscale )

USE constants_fd
USE met_fi

IMPLICIT NONE

REAL, INTENT( IN  ) :: si, si2
REAL, INTENT( IN  ) :: velc
REAL, INTENT( OUT ) :: tscale

REAL sigs, sigb, sigl, sig_tot
REAL wt_s, wt_b, wt_l, wt_tot

REAL, EXTERNAL :: sig_tscale

sigs = sig_tscale( si,si2,sbls )
sigb = sig_tscale( si,si2,sbl  )
sigl = sig_tscale( si,si2,sb_lsv )

wt_s = qqsh * MIN(1.0,(sbls/si)**2)
wt_b = qqb  * MIN(1.0,(sbl/si)**2)
wt_l = qql * MIN(1.0,(sb_lsv/si)**2)

wt_tot  = wt_s + wt_b + wt_l
sig_tot = wt_s*sigs + wt_b*sigb + wt_l*sigl

tscale = 2.*0.7*sig_tot/wt_tot/velc

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION sig_tscale( si,si2,sl_turb )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: si, si2, sl_turb

REAL sig, arg, fac, siy

IF( si >= sl_turb )THEN
  sig = si
ELSE
  siy = MIN(si2,sl_turb)
  arg = 1. + (sl_turb*sl_turb/si/siy)**0.333333
  sig = (sl_turb*si*siy)**0.333333*LOG(arg)/LOG2
!-- limit for short duration integrals
  fac = 2.5 - 1.5*(siy/sl_turb)**2
  sig = MIN( sig,fac*siy )
END IF

sig_tscale = sig

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_pturb( p,pd )

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Define dynamic puff turbulence parameters (variances and scales)

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( puff_dynamics ), INTENT( IN    ) :: pd

REAL qqp, slp, qpi, qpdiff, qosi_p

LOGICAL, EXTERNAL :: dense_effect

!----- Dynamic temperature excess

bdyn = tdyn - thb*pd%bcp/csav

IF( dense_effect(zbar-hp,sv,pd%wcp,csav) )THEN
  ldense = .TRUE.
  CALL step_pturb_dense( p,pd,qqp,slp )
ELSE
  ldense = .FALSE.
  CALL step_pturb_buoy( pd,qqp,slp )
END IF

!----- Return if no dynamic effects

IF( qqp == 0.0 )THEN
  difp = 0.0; RETURN
END IF

qpi    = fdyn*SQRT(qqp)
qpdiff = 0.15*qpi
difp   = qpdiff*slp
qosi_p = 2.*BS*(qpi/slp)*(1.0 + slp/si + slp/si2) * MAX(qqp/(qqp+uub+vvb),0.25)

!----- Adjust internal velocity scales

siq = siq + qpdiff
svq = fdyn*svq + qpdiff

qosi_cc = qosi_cc + qosi_p
IF( fdyn < 1.0 )qosi_cc = qosi_cc - (1.-fdyn)*BS*qvi/sv

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_pturb_buoy( pd,qqp,slp )

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Define buoyant dynamic puff turbulence parameters (variances and scales)

IMPLICIT NONE

TYPE( puff_dynamics ), INTENT( IN  ) :: pd
REAL,                  INTENT( OUT ) :: qqp, slp

REAL vp2, wp2, qqvrtx, fac, ri

!----- Initial values

udyn = pd%ucp/csav
vdyn = pd%vcp/csav
wdyn = pd%wcp/csav
ddyn = 0.
zdyn = 0.

!----- Dynamic turbulence

slp = MIN( si,0.65*MAX(zbar-hp,sz) )

wp2 = udyn*udyn + vdyn*vdyn + wdyn*wdyn
IF( wp2 > SMALL )THEN

  vp2    = vel2 + uub + vvb + 1.E-6
  qqvrtx = CVRTX*(vp2*wdyn*wdyn+(udyn*vb-vdyn*ub)**2)/(vp2+wp2)

  IF( ABS(bdyn) > dtdz*slp )THEN
    ri = gt0*ABS(bdyn)*slp
    IF( wp2 < 4.*ri )THEN
      fac = 2.0
    ELSE
      ri  = ri/wp2
      fac = 1.0 + 4.*ri
    END IF
  ELSE
    ri = gt0*(dtdz*slp-ABS(bdyn))*slp
    IF( wp2 < 1.0E-3*ri )THEN
      fac = 0.0
    ELSE
      ri  = ri/wp2
      fac = 1.0/(1.0 + 4.*ri )
    END IF
  END IF

  qqp = fac*(CQB*wp2 + qqvrtx)

ELSE

  wdyn = 0.0
  qqp  = 0.0

END IF

tauw = 0.0
fdyn = 1.0

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_pturb_dense( p,pd,qqp,slp )

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Define dense dynamic puff turbulence parameters (variances and scales)

IMPLICIT NONE

REAL,                  INTENT( OUT ) :: qqp, slp
TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( puff_dynamics ), INTENT( IN  ) :: pd

REAL, PARAMETER :: FAC_DENSE = 15.0

REAL vp2, wp2, fac, ri, alp

LOGICAL, EXTERNAL :: check_slope

alp = p%si2/(p%si+p%si2)
fac = (p%ccb/MAX(p%ccb,p%cc-p%ccb))**alp

udyn = pd%ucp/csav
vdyn = pd%vcp/csav
wdyn = pd%wcp/csav

IF( check_slope(hx,hy) )THEN
  CALL dense_rot( pd,fac )
ELSE
  dudx = dudx + pd%dudx*fac
  dudy = dudy + pd%dudy*fac
  dvdx = dvdx + pd%dvdx*fac
  dvdy = dvdy + pd%dvdy*fac
END IF

ddyn = -(pd%dudx + pd%dvdy)
zdyn = (1.-fac)*ddyn
udyn = pd%u + (pd%ucp+pd%wcp*hx/(1.0+hx*hx))/csav
vdyn = pd%v + (pd%vcp+pd%wcp*hy/(1.0+hy*hy))/csav
wdyn = ddyn*MAX(zbar-hp,sz) ! + udyn*hx + vdyn*hy

!----- Dynamic turbulence

vp2  = ub*ub + vb*vb + uub + vvb + 1.E-6
wp2  = udyn*udyn + vdyn*vdyn + (p%sxx+p%syy)*ddyn*ddyn + wdyn*wdyn
ri   = gt0*ABS(bdyn)*sv/(4.*wp2+vp2)
fdyn = 1.0/(1.0 + FAC_DENSE*ri)
qqp  = CQB*wp2
slp  = sv

!----- Reduce velocity

fac  = 0.7 + 0.3*fdyn
ub   = ub*fac
vb   = vb*fac
  dudz = fdyn*dudz
  dvdz = fdyn*dvdz

vel2 = ub*ub + vb*vb
vp2  = fac*fac*(uub+vvb)

!----- Surface exchange rate

fac  = LOG(1.0 + slp/zruf)
tauw = (VONK/fac)**2*SQRT(vel2+vp2+wp2)/slp

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_tlev( p,dt,lev1,lev2 )

USE scipuff_fi
USE met_fi
USE surface_fi
USE step_p_fi
USE files_fi
USE PtrGrdStrItf
USE sagdef_fd

!  Reset time step for puff-p

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt
INTEGER,          INTENT( IN    ) :: lev1
INTEGER,          INTENT( INOUT ) :: lev2

INTEGER il, irv
INTEGER ios
REAL    xp, yp, xfac, yfac
REAL    delg, sigu, wtot, prod, dsrf, dd, dels
REAL    dtw, dtcc, dta, dtk, dtp
LOGICAL ltest

INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: time_level

!------ promote low-mass puff to largest available timestep

IF( p%cc <= cmin2 )THEN
  p%idtl = lev1
  GOTO 9999
END IF

!----- fluctuation dissipation time scale

!DEC$ DEFINE OLDLIMIT
prod = qosi_cc
IF( prod > SMALL )THEN
  dtcc = 0.5/prod
ELSE
  dtcc = delt
END IF

!----- vertical velocity time scale

wtot = ABS(wpuff)
IF( wtot > SMALL )THEN
  dtw = dzg/wtot
ELSE
  dtw = delt
END IF

!----- surface grid time scale

IF( vel2 > SMALL )THEN
  IF( PrjCoord%type /= MetGrid(metField)%coord%type )THEN
    irv = SWIMcnvCoord( SNGL(p%xbar),SNGL(p%ybar),PrjCoord,xp,yp,MetGrid(metField)%coord )
    CALL SWIMmapfac( MetGrid(metField)%coord,xp,yp,xfac,yfac )
    delg = MIN( MetGrid(metField)%dx/xfac,MetGrid(metField)%dy/yfac )
  ELSE
    delg = MIN( MetGrid(metField)%dx/xmap,MetGrid(metField)%dy/ymap )
  END IF
  IF( lscale )THEN
    IF( lsmp )THEN
      ltest = .TRUE.
      dsrf  = 0.
    ELSE
      IF( lter )THEN
        dels = fac_rfl*(ABS(hx)*SQRT(p%sxx) + ABS(hy)*SQRT(p%syy))
      ELSE
        dels = 0.
      END if
      IF( lsrf .OR. z_dosage == 0. )THEN
        ztest = p%zbar - hp - dels
      ELSE
        ztest = HUGE(0.)
      END IF
      IF( ldos .AND. z_dosage > 0. )THEN
        ztest = MIN(ztest,ABS(p%zbar-z_dosage-hp)-dels)
      END IF
      ztest = ztest - fac_rfl*SQRT(p%szz)
      ltest = (ztest + MIN(wpuff,0.)*dt) < 0.
      IF( ltest )THEN
        IF( lsrf )THEN
          dsrf = Psrfdep%delmin
        ELSE
          dsrf = 0.
        END IF
        IF( ldos )THEN
          IF( BTEST(run_mode,REVERSE_MODE) )THEN
            Psrfdos => SAG_PtrGrdStr( srfdosAdj(imat) ) ! Associate "local" grid structure pointer
          END IF
          dsrf = MAX(Psrfdos%delmin,dsrf)
        END IF
      END IF
    END IF
    IF( ltest )THEN
      sigu = ub*ub*p%axx + 2.*ub*vb*p%axy + vb*vb*p%ayy
      dta  = SQRT(MAX(1./sigu,dsrf*dsrf/vel2))
    ELSE
      dta = delg/SQRT(vel2)
    END IF
  ELSE
    dta = delg/SQRT(vel2)
  END IF
ELSE
  dta = delt
END IF

!----- vertical diffusivity time scale

IF( p%zbar-hp < 2.*hc .AND. p%sv < hc )THEN
  dd  = MIN(dzg,0.5*hc)
  dtk = 0.125*dd*dd/MAX(zwct,1.E-6)
ELSE IF( (MAX(p%zbar-hp,MAX(p%sv,sz)) <= 0.9*WMX*(zinv-hp)) .AND. lbl .AND. dtdzs > 0. )THEN
  dd  = MIN(dzg,0.25*(zinv-hp))
  dtk = 0.125*dd*dd/MAX(zwct,1.E-6)
ELSE
  dtk = delt
END IF

!----- find minimum time step

dtp = MIN( dtcc,dtw,dta,dtk,dts )

il = time_level( dtp )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'step_tlev'
!$OMP CRITICAL (LOG)
  WRITE(lun_log,*,IOSTAT=ios)'******* TIME LEVEL ERROR ********'
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)TRIM(eRoutine)
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)TRIM(eInform)
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)'DT(cc,w,a,k,s)=', &
                                 dtcc,dtw,dta,dtk,dts
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)'DELT=',delt
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)'DTP,LEVEL=',dtp,il,MAXTLV
  IF( ios == 0 )WRITE(lun_log,*,IOSTAT=ios)'DZG,HP,WPUFF=',dzg,hp,wpuff
  IF( ios == 0 )CALL dump_puff( 0,p )
!$OMP END CRITICAL (LOG)
  IF( ios /= 0 )GOTO 9998
  GOTO 9999
END IF

!$OMP ATOMIC
lev2 = MAX(lev2,il)
il   = MAX(il,lev1)
IF( il /= p%idtl )THEN
  p%idtl = il
END IF

9999 CONTINUE

RETURN

!------ set log write error and goto return

9998 CONTINUE
nError   = WR_ERROR
eRoutine = 'step_p'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

END

!===============================================================================

RECURSIVE SUBROUTINE step_split( p,ipuf,dt,lev1,lev2 )

USE scipuff_fi
USE met_fi
USE step_p_fi

!------ set capping height / check for splitting above or below zcap

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ipuf
REAL,             INTENT( IN    ) :: dt
INTEGER,          INTENT( IN    ) :: lev1
INTEGER,          INTENT( INOUT ) :: lev2

CALL set_zcap( p,ipuf,dt )
IF( nError /= NO_ERROR )GOTO 9999

p%zi = zinv

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_scale( p,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt

REAL    rat, fac, shear, sig_shr2, siq2
REAL    power, faczi
INTEGER relID

INTEGER, EXTERNAL :: getPuffirel

!----- Get release ID, if there is one

relID = getPuffirel( p )

!----- advance puff length scales

siq2 = siq*si/si2
svq  = svq + sigvd

p%si = p%si + siq*dt

IF( lzinv .AND. sv > WMX*(zinv-hp) )THEN

  p%si2 = p%si2 + siq2*dt

ELSE

!----- define rms vertical shear

  IF( uub > SMALL .AND. vvb > SMALL )THEN
    sig_shr2 = 0.25*(uubz*uubz/uub+vvbz*vvbz/vvb)
  ELSE
    sig_shr2 = 0.
  END IF

  shear = dudz*dudz + dvdz*dvdz + sig_shr2
  fac   = 1.0
  faczi = 1.0
  IF( lzinv )THEN
    rat = (zinv - hp)**2/12.
    IF( sv*sv > rat )fac = rat/(sv*sv)
    IF( xml > 0.0 .AND.  xml < (zinv-hp) )THEN
      faczi = 1.0 - 0.35*MIN(1.0-xml/(zinv-hp),0.9)
    END IF
  END IF

  shear = fac*SQRT(shear)

  rat   = MIN(sv/si2,1.0)
  fac   = shear*sv*sv/MAX(difb,1.E-6)
  power = 2.0 - MIN(1.0,0.01*fac)
  fac   = MAX(1.0-(rat**power)*(fac**0.3333333),0.0)
  shear = 0.8660254*shear*dt
  p%si2 = p%si2 + siq2*dt + shear*sv*fac
  rat   = MIN(sv/p%si2,1.0)
  p%sv  = (sv + svq*dt)/(1.0+shear*rat*faczi)
END IF

!----- Scale changes for dense gas "slumping"

IF( ABS(ddyn)*dt > SMALL )THEN

!----- vertical distortion

  fac  = MIN(1.0,0.5/(dt*ABS(ddyn)))
  fac  = EXP(fac*ddyn*dt)
  p%sv = p%sv*fac

END IF

IF( lcap .AND. p%sv > WMX*(MAX(p%zc,zinv)-hp) )THEN
  p%sv = WMX*(MAX(p%zc,zinv)-hp)
END IF

!----- Check for multiple random release

IF( relID > 0 )THEN
  IF( p%si > releaseID(relID)%SigMerge )THEN
    p%si  = releaseID(relID)%SigRel
    p%si2 = MAX(p%si2,p%si)
    CALL setPuffirel( p,0 )
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_dynamics( p,pd,pt,dt,pmat,pa )

USE scipuff_fi
USE met_fi
USE step_p_fi

!-----  advance puff dynamics

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
TYPE( puff_totalcc ),  INTENT( INOUT ) :: pt
REAL,                  INTENT( IN    ) :: dt
TYPE( puff_material ), INTENT( IN    ) :: pmat
TYPE( puff_aerosol  ), INTENT( IN    ) :: pa

REAL tfac, denfac, a11, a12, a21, a22, b1, b2, dd
REAL bfac, dtdzd

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsAerosol

!-----  set temperature-dependent buoyant gas density factor

IF( buoy_gas )THEN
  denfac = (tdyn+thb)/300.
  tfac   = thb*denfac
  denfac = 1.0/(1.0 + MAX(denfac*pd%bcp/csav-tdyn/thb,0.))
ELSE
  tfac   = 0.0
  denfac = 1.0/(1.0 + MAX(-tdyn/thb,0.))
END IF

!-----  set ambient temperature gradient zero for dense effects

IF( ldense )THEN
  dtdzd = 0.0
ELSE
  dtdzd = dtdz
END IF

!-----  advance mean momentum and temperature excess for gases only

IF( IsGas(typeID(p%ityp)%icls) )THEN

  IF( IsAerosol(typeID(p%ityp)%icls) )THEN
    CALL set_aerosol_buoy( pa,pmat,bfac,IsLiquid(material(imat)%icls) )
  ELSE IF( IsLiquid(material(imat)%icls) )THEN
    CALL set_liquid_buoy( p,pt,imat,bfac )
  ELSE
    bfac = buoy_fac(p%ityp)
  END IF

  a11 = 1. + (tauw+0.2*bv)*dt
  a22 = 1. + tauw*dt
  a12 = -gt0*denfac*dt
  a21 = dtdzd*dt
  dd  = a11*a22 - a12*a21
  b1  = pd%w - gt0*tfac*bfac*p%c*denfac*dt
  b2  = pd%t

  pd%w = (b1*a22 - b2*a12) / dd
  pd%t = (b2*a11 - b1*a21) / dd

  pd%un = pd%un / a22
  pd%vn = pd%vn / a22

END IF

!-----  advance momentum and temperature excess correlations

pd%bcp = (pd%bcp + qosi_cc*pd%bcb*dt) / (1. + (qosi_cc+tauc)*dt)

a11 = 1. + (qosi_cc+tauc+tauw+0.2*bv)*dt
a12 = -gt0*denfac*dt
a22 = 1. + (qosi_cc+tauc+tauw)*dt
a21 = dtdzd*dt
dd  = a11*a22 - a12*a21
b1  = (qosi_cc*pd%wcb - tfac*gt0*denfac*pd%bcp)*dt + pd%wcp
b2  = qosi_cc*dt*pd%ctb + pd%ctp

pd%wcp = (b1*a22 - b2*a12) / dd
pd%ctp = (b2*a11 - b1*a21) / dd

a11 = 1. + (qosi_cc+tauc+tauw)*dt
pd%ucp = (qosi_cc*dt*pd%ucb + pd%ucp) / a11
pd%vcp = (qosi_cc*dt*pd%vcb + pd%vcp) / a11

IF( ldense )THEN
  wdyn = wdyn*(1.0 + MAX(-0.5,MIN(0.0,ddyn*dt))) + udyn*hx + vdyn*hy
ELSE
  wdyn = 0.5*(wdyn + pd%wcp/p%c)
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE step_velcorr( p,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

!-----  advance velocity correlations

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt

REAL damp, geq, keq, soq, wp2, fac
REAL wclim, zagl, dsdz, cteq, dtaudz, wtx, bfac, zcteq, facwc, faczwc

!------ adjust vertical diffusivity for non-equilibrium positive buoyancy

IF( difb > p%zwc/csav )THEN
  faczwc = p%zwc/difb/csav
ELSE
  faczwc = 1.0
END IF
soq = sbz/MAX(qs,1.E-3)

IF( faczwc < 1.0 )THEN
  IF( lzinv )THEN
    zagl   = MAX(zsav-hp,sz)
    dsdz   = sbz/zagl - sbz**3/(0.65*zagl*(0.3*(zinv-hp))**2)
    dtaudz = dsdz/sbz - 0.5*wwz/wwbl    ! (1/tau)*d(tau)/dz
    IF( dtdz > 0.0 )THEN
      wtx  = wts*zagl/(zinv-hp)
      bfac = 1.0 + soq*soq*gt0*dtdz/(2.*A*BS)
    ELSE
      wtx  = wtbl
      bfac = 1.0
    END IF
    zcteq = (wtx - soq*dtdz*wwbl/A)*soq/(2.*BS)/bfac
    cteq  = (soq/(2.*BS) * (wts/(zinv-hp) - dtdz*(wwz*soq/A-difb*dtaudz))+dtaudz*zcteq)/bfac
    wclim = wwz*soq/A + (p%zwc/csav)*dtaudz + gt0*cteq*faczwc
  ELSE
    wclim = wwz*(soq/A - (p%zwc/csav)*0.5/wwbl)
  END IF
  facwc = faczwc
  geq   = wtbl - dtdz*difb
ELSE
  geq   = -1.0
  facwc =  2.0
END IF

IF( geq > SMALL )THEN
  geq  = geq*soq/(2.*BS)
  keq  = MAX(difb,SMALL)
  fac  = MIN(gt*geq/wwbl,2.*B/BS) * faczwc
  difb = (1.0+fac)*wwbl*soq/A
  dddz = dddz*difb/keq
  IF( facwc < 1.0 )dddz = (1.0-facwc)*wclim + facwc*dddz
  IF( sv < sbz )fac  = fac * (sv/sbz)**TwoThirds
  qosi_cc = qosi_cc + fac*fdyn*BS*qvi/sv    !adjust dissipation rate for buoyancy enhancement
  svq     = svq*(1.0+fac)
ELSE
  IF( facwc < 1.0 )dddz = (1.0-facwc)*wclim + facwc*dddz
END IF

IF( iSkew /= SKEW_NONE )THEN
  wb_skew = fdyn*wb_skew
  fac = 0.1/MAX(skewness,0.01)
  IF( iSkew == SKEW_DOWN )THEN
    dddz = MAX(fac*wb_skew,MIN(dddz,-fac*wb_skew))
  ELSE
    dddz = MIN(fac*wb_skew,MAX(dddz,-fac*wb_skew))
  END IF
END IF

difb = fdyn*difb
dddz = fdyn*dddz

!------ adjust diffusivity for relative motion

wp2 = wdyn*wdyn + vfall*vfall

IF( wp2 > SMALL )THEN
  fac  = SQRT(qqs/(qqs + wp2))
  difb = difb*fac
  dddz = dddz*fac
  IF( qqsh > SMALL )THEN
    fac = SQRT((qqsh + wp2)/qqsh)
    aqsosyt = aqsosyt*fac
    dddx    = dddx/fac
    dddy    = dddy/fac
  END IF
  IF( qqb > SMALL )aqbosyt = aqbosyt*SQRT((qqb + wp2)/qqb)
  IF( qql > SMALL )aqlosyt = aqlosyt*SQRT((qql + wp2)/qql)
END IF

!-----  reduce diffusivity gradient for poorly resolved BL
!       (puffs are at least sz off ground and won't get much lower
!        so larger diffusivities near the surface are never seen)

IF( lzinv .AND. sz > 0.5*(zinv-hp) )THEN
  dddz = dddz * MAX(0.0,zinv-hp-sz)/(zinv-hp)
END IF

!-----  advance vertical velocity correlations
!       N.B. tauc in definition of damp; and damp multiplies difb
damp  = (aqoszt+tauc)*dt
p%zwc = (p%zwc + damp*difb*p%c)/(1.0 + damp)
p%wc  = (p%wc  + damp*dddz*p%c)/(1.0 + damp)

!-----  Limit zwc no greater than equilibrium background value

IF( .NOT.lzinv )p%zwc = MIN(p%zwc,difb*p%c)

!-----  limit vertical drift correlation

IF( difb > SMALL )THEN
  fac = p%zwc/difb
  CALL limit_wc( p%wc,dddz,fac )
ELSE
  p%wc = 0.0
END IF

!-----  advance horizontal velocity correlations

damp   = 1.0 + (aqlosyt+tauc)*dt
p%xuc  = (p%xuc + uub*p%c*dt) / damp
p%xvc  = (p%xvc + uvb*p%c*dt) / damp
p%yvc  = (p%yvc + vvb*p%c*dt) / damp

!------ N.B. tauc is in definition of damp but it's not in the production term
damp   = 1.0 + (aqsosyt+tauc)*dt
p%yvsc = (p%yvsc + uubl*p%c*dt) / damp
damp   = 1.0 + (aqbosyt+tauc)*dt
p%yvbc = (p%yvbc + vvbl*p%c*dt) / damp

!------ Scale horizontal drift terms

fac  = MIN(p%yvsc*aqsosyt/(uubl*p%c),1.0)
dddx = dddx*fac
dddy = dddy*fac

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE limit_wc( wc,dddz,fac )

!------ limit vertical drift velocity based on ambient diffusivity gradient

IMPLICIT NONE

REAL, INTENT( INOUT ) :: wc
REAL, INTENT( IN    ) :: dddz, fac

IF( dddz > 0. )THEN
  wc = MAX(0.0,MIN(wc,fac*dddz))
ELSE
  wc = MIN(0.0,MAX(wc,fac*dddz))
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_diff( p )

USE scipuff_fi
USE met_fi
USE step_p_fi

!------ set diffusivities

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL, PARAMETER :: SHEAR_FAC = 9.0
REAL, PARAMETER :: QQSH_FAC  = 0.25

REAL ztem, zhgt, xfac, yfac, rfac, utot

!------ vertical diffusivity and shear - shut off when well mixed

IF( p%zc > 0.0 .AND. sz >= 0.999*WMX*(p%zc-hp) )THEN
  p%wc = 0.0
  zwct = 0.0
  dudz = 0.0
  dvdz = 0.0
ELSE
  zwct = p%zwc/p%c + difp
END IF

!------ horizontal diffusivities with enhanced streamwise diffusion
!       near the ground

xfac = 1.; yfac = 1.; rfac = 0. !Default streamwise diffusion factors

utot = ub*ub + vb*vb

IF( lzinv .AND. utot > SMALL )THEN

  CALL set_zsl( ztem,zinv-hp,xml,zruf,hc ) !Set surface layer depth based on local
                                           !M-O length and BL depth
  IF( xml < 0. )ztem = MIN( -5.*xml,ztem ) !5L limit for convective conditions

  zhgt = MAX(p%zbar-hp,sz)                 !Set fraction of surface layer based
  ztem = zhgt/ztem                         !on puff centroid or vertical spread

  IF( ztem < 1. )THEN

    ztem = SHEAR_FAC*((1.-ztem)*fdyn)**2   !Decrease factor with height (and dense effects)
    ztem = MIN(ztem,MAX(0.,QQSH_FAC*utot/qqsh-1.)) !Factor for light winds

!---- Set diffusivity enhancement factors aligned with wind direction

    ztem = ztem / utot
    xfac = 1. + ztem*ub*ub
    yfac = 1. + ztem*vb*vb
    rfac =      ztem*ub*vb

  END IF

END IF

xuct = (p%xuc+p%yvsc*xfac+p%yvbc)/p%c + difp
xvct = (p%xvc+p%yvsc*rfac       )/p%c
yvct = (p%yvc+p%yvsc*yfac+p%yvbc)/p%c + difp

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_penetration( p,pd,znew )

USE scipuff_fi
USE met_fi
USE step_p_fi

!------ compute penetration into overlying stable region

IMPLICIT NONE

TYPE( puff_str ),      INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
REAL,                  INTENT( IN    ) :: znew

REAL, PARAMETER :: A_JUMP  = 0.143
REAL, PARAMETER :: ENT_FAC = 1.0

REAL del_temp, deli, ke, bvi
REAL z_entrn, z_trap, damp, zp, xvc

!------ estimate temperature jump

del_temp = A_JUMP*(zinv-hp)*gamma

!------ estimate height of penetration based on energy equation

deli = gt0*(del_temp - tdyn + thb*pd%bcp/csav)
ke   = 0.5*wdyn**2
bvi  = gt0*gamma
zp   = 0.5*(-deli+SQRT(deli*deli+4.*bvi*ke))/bvi

!------ complete penetration if beyond the entrainment zone -
!       entrainment zone height estimate based on a fraction of the
!       mixed-layer depth or the horizontal scale of the cloud/plume

z_entrn = MAX(0.1*(zinv-hp),ENT_FAC*p%si)

IF( zp > z_entrn )THEN

!------ complete penetration

  p%zbar = MIN(znew,zinv+zp)
  p%zc   = 0.
  lzinv  = .FALSE.
  lcap   = .FALSE.

!------ adjust temp change to account for vertical motion

  del_temp = (del_temp + gamma*(p%zbar-zinv)) * p%c

!------ remove temperature jump from buoyancy variables

  pd%ctp = pd%ctp - del_temp
  pd%t   = pd%t   - del_temp*p%c/p%cc
  wdyn   = 0.

!------ re-set diffusivities; get met background for new location

  CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,0.0,0.0,0,inField=metField )
  IF( lsv_oper )CALL reset_lsv( si )

  IF( t_avg /= DEF_VAL_R )THEN
    CALL turb_rescale( si,sv,vel2 )
  END IF

  CALL step_turb()

  p%zwc = MIN(p%zwc,difb*p%c)
  CALL limit_wc( p%wc,dddz,p%c )

  IF( aqsosyt > SMALL )THEN
    damp  = p%c/aqsosyt
    p%yvsc = MIN(p%yvsc,uubl*damp)
  ELSE
    p%yvsc = 0.
  END IF

  IF( aqbosyt > SMALL )THEN
    damp  = p%c/aqbosyt
    p%yvbc = MIN(p%yvbc,vvbl*damp)
  ELSE
    p%yvbc = 0.
  END IF

  IF( aqlosyt > SMALL )THEN
    damp  = p%c/aqlosyt
    IF( p%xuc*p%yvc > SMALL )THEN
      xvc = p%xvc/SQRT(p%xuc*p%yvc)
    ELSE
      xvc = 0.
    END IF
    p%xuc = MIN(p%xuc,uub*damp)
    p%yvc = MIN(p%yvc,vvb*damp)
    p%xvc = xvc*SQRT(p%xuc*p%yvc)
  ELSE
    p%xuc = 0.
    p%xvc = 0.
    p%yvc = 0.
  END IF

ELSE

!------ puff does not penetrate; turn off dynamics and locate below inversion

  pd%w   = 0.
  pd%wcp = 0.

  z_trap = MIN(0.01*(zinv-hp),ENT_FAC*p%si)
  p%zbar = zinv - z_trap
  p%zc   = zinv
  wdyn   = 0.

END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_descend( p,pd,znew )

USE scipuff_fi
USE met_fi
USE step_p_fi

!-----  modify buoyancy for puffs descending from stable region into
!       mixed-layer below inversion

IMPLICIT NONE

TYPE( puff_str ),      INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd
REAL,                  INTENT( IN    ) :: znew

REAL, PARAMETER :: A_JUMP  = 0.143

REAL del_temp, damp, xvc

!------ estimate temperature jump

del_temp = MIN(0.0,pd%ctp/p%c)
del_temp = MIN(A_JUMP*(zinv-hp)*gamma,-del_temp) * p%c

!------ add to buoyancy variables

pd%ctp = pd%ctp + del_temp
pd%t   = pd%t   + del_temp*p%c/p%cc

!------ re-set diffusivities; get met background for new location

p%zbar = znew
lzinv  = .FALSE.

CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,0.0,0.0,0,inField=metField )
IF( lsv_oper )CALL reset_lsv( si )

IF( t_avg /= DEF_VAL_R )THEN
  CALL turb_rescale( si,sv,vel2 )
END IF

CALL step_turb()

p%zwc = MIN(p%zwc,difb*p%c)
CALL limit_wc( p%wc,dddz,p%c )

IF( aqsosyt > SMALL )THEN
  damp  = p%c/aqsosyt
  p%yvsc = MIN(p%yvsc,uubl*damp)
ELSE
  p%yvsc = 0.
END IF

IF( aqbosyt > SMALL )THEN
  damp   = p%c/aqbosyt
  p%yvbc = MIN(p%yvbc,vvbl*damp)
ELSE
  p%yvbc = 0.
END IF

IF( aqlosyt > SMALL )THEN
  damp = p%c/aqlosyt
  IF( p%xuc*p%yvc > SMALL )THEN
    xvc = p%xvc/SQRT(p%xuc*p%yvc)
  ELSE
    xvc = 0.
  END IF
  p%xuc = MIN(p%xuc,uub*damp)
  p%yvc = MIN(p%yvc,vvb*damp)
  p%xvc = xvc*SQRT(p%xuc*p%yvc)
ELSE
  p%xuc = 0.
  p%xvc = 0.
  p%yvc = 0.
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_zcap( p,ipuf,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

!------ set capping height

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ipuf
REAL,             INTENT( IN    ) :: dt

REAL, PARAMETER :: SZ_FAC  = 4.0
REAL, PARAMETER :: SZ_CAP  = 1.5
REAL, PARAMETER :: WW_FAC  = 0.01

IF( lblcap .AND. p%zbar <= zinv )THEN

  IF( p%zbar + SZ_CAP*sz < zinv )THEN
    p%zc = zinv
  ELSE
    IF( p%zc == 0. .OR. p%zc > zinv + MAX(0.3*(zinv-hp),dzg) )THEN
      CALL split_zi( p,ipuf,zinv )
      IF( nError /= NO_ERROR )THEN !not enough space for new puff
        IF( p%zc == 0.0 )p%zc = p%zbar + SZ_CAP*sz
        CALL init_error()
      END IF
    ELSE
      p%zc = MAX(p%zc,zinv)
    END IF
  END IF

ELSE

  IF( p%zc > 0. )THEN
    IF( p%zbar + SZ_FAC*sz > p%zc )THEN
      p%zc = p%zc + (1.5*wb - 0.5*p%wo + WW_FAC*SQRT(wwbl)) * dt
      p%zc = MAX(p%zc,hp+sz,zinv)
    ELSE
      p%zc = 0.
    END IF
  END IF

END IF

IF( p%zbar > p%zc .AND. p%zc > 0.0 )THEN
  p%zc = p%zbar + 0.01
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_zcap_static( p,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

!------ set capping height for static puffs (no split_zi)

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt

REAL, PARAMETER :: SZ_FAC  = 4.0
REAL, PARAMETER :: SZ_CAP  = 1.5
REAL, PARAMETER :: WW_FAC  = 0.01

IF( lblcap .AND. p%zbar <= zinv )THEN

  IF( p%zbar + SZ_CAP*sz < zinv )THEN
    p%zc = zinv
  ELSE
    p%zc = MAX(p%zc,zinv)
  END IF

ELSE

  IF( p%zc > 0. )THEN
    IF( p%zbar + SZ_FAC*sz > p%zc )THEN
      p%zc = p%zc + (1.5*wb - 0.5*p%wo + WW_FAC*SQRT(wwbl)) * dt
      p%zc = MAX(p%zc,hp+sz,zinv)
    ELSE
      p%zc = 0.
    END IF
  END IF

END IF

IF( p%zbar > p%zc .AND. p%zc > 0.0 )THEN
  p%zc = p%zbar + 0.01
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_turb( uubt,vvbt,uvbt )

USE scipuff_fi
USE met_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: uubt, vvbt, uvbt

REAL wws, wwb

!------ set total turbulence values

uubt = uub + uubl + vvbl
vvbt = vvb + uubl + vvbl
uvbt = uvb !+ uvbl

!------ partition vertical velocity

wws  = wwbh
wwb  = 0.

!-----  velocity scales

qqsh = 2.*uubl + wws
qqb  = 2.*vvbl + wwb
qql  = uub + vvb

RETURN
END
!*******************************************************************************
!             dense_rot
!*******************************************************************************
RECURSIVE SUBROUTINE dense_rot( pd,fac )

USE scipuff_fi
USE met_fi
USE step_p_fi

!     Rotate dense puff velocity gradients

IMPLICIT NONE

TYPE( puff_dynamics ), INTENT( IN ) :: pd
REAL,                  INTENT( IN ) :: fac

REAL(8), DIMENSION(3,3) :: amat, bmat, cmat, at

!---- Rotate from local coordinates

CALL set_matrot( hx,hy,amat )

bmat(1,1) = DBLE(pd%dudx)
bmat(1,2) = 0.D0
bmat(1,3) = 0.D0
bmat(2,1) = 0.D0
bmat(2,2) = DBLE(pd%dvdy)
bmat(2,3) = 0.D0
bmat(3,1) = 0.D0
bmat(3,2) = 0.D0
bmat(3,3) = -DBLE(pd%dudx+pd%dvdy)

at   = TRANSPOSE( amat )
cmat = MATMUL( at,bmat )
bmat = MATMUL( cmat,amat )

dudx = dudx + SNGL(bmat(1,1))*fac
dudy = dudy + SNGL(bmat(1,2))*fac
dudz = dudz + SNGL(bmat(1,3))*fac
dvdx = dvdx + SNGL(bmat(2,1))*fac
dvdy = dvdy + SNGL(bmat(2,2))*fac
dvdz = dvdz + SNGL(bmat(2,3))*fac
dwdx = dwdx + SNGL(bmat(3,1))*fac
dwdy = dwdy + SNGL(bmat(3,2))*fac
dwdz = dwdz + SNGL(bmat(3,3))*fac

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_dense_gas( p,pd )

USE scipuff_fi

!------ Set dense gas dynamics

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd

REAL hp, hx, hy
LOGICAL, EXTERNAL :: dense_effect
INTEGER, EXTERNAL :: getPuffifld

IF( lter )THEN
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,getPuffifld(p) )
ELSE
  hp = 0.0
  hx = 0.0
  hy = 0.0
END IF

!------ Check for ground effects

IF( .NOT.dense_effect(p%zbar-hp,p%sv,pd%wcp,p%c) )THEN

  pd%u0 = 0.0
  pd%X  = 0.0
  pd%Y  = 0.0
  pd%sn = 0.0
  pd%cs = 0.0

ELSE

!------ Rotate into terrain coordinates

  CALL dense_rot_ter( hx,hy,p,pd )

END IF

RETURN
END

!=======================================================================

RECURSIVE LOGICAL FUNCTION dense_effect( zz,sv,w,c )

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: zz, sv, w
REAL, INTENT( IN ) :: c

dense_effect = .FALSE.

IF( dense_gas )THEN
  IF( c > SMALL )THEN
    IF( zz < 2.*sv )THEN
      dense_effect = ( w <= 0. )
    END IF
  END IF
END IF

RETURN
END

!-----------------------------------------------------------------------

RECURSIVE SUBROUTINE dense_rot_norm( hx0,hy0,hx1,hy1,p )

USE scipuff_fi

IMPLICIT NONE

REAL,             INTENT( IN    ) :: hx0, hy0, hx1, hy1
TYPE( puff_str ), INTENT( INOUT ) :: p

REAL(8) xn0, yn0, zn0, xn1, yn1, zn1, xt1, yt1, zt1, fac, cs, sn, dhx0, dhy0, dhx1, dhy1

REAL(8), DIMENSION(3,3) :: amat, bmat, cmat, at

LOGICAL, EXTERNAL :: check_slope

IF( .NOT.check_slope(hx1-hx0,hy1-hy0) )RETURN

dhx0 = DBLE(hx0)
dhy0 = DBLE(hy0)
dhx1 = DBLE(hx1)
dhy1 = DBLE(hy1)

fac = 1.0D0/DSQRT(1.0D0+dhx0*dhx0+dhy0*dhy0)
xn0 = -fac*dhx0
yn0 = -fac*dhy0
zn0 =  fac

fac = 1.0D0/SQRT(1.0D0+dhx1*dhx1+dhy1*dhy1)
xn1 = -fac*dhx1
yn1 = -fac*dhy1
zn1 =  fac

cs = xn0*xn1 + yn0*yn1 + zn0*zn1

xt1 = (yn0*zn1 - zn0*yn1)
yt1 = (zn0*xn1 - xn0*zn1)
zt1 = (xn0*yn1 - yn0*xn1)

sn = DSQRT(xt1*xt1+yt1*yt1+zt1*zt1)

xt1 = xt1/sn
yt1 = yt1/sn
zt1 = zt1/sn

amat(1,1) = xn0
amat(1,2) = yn0
amat(1,3) = zn0
amat(2,1) = xt1
amat(2,2) = yt1
amat(2,3) = zt1
amat(3,1) = (yn0*zt1 - zn0*yt1)
amat(3,2) = (zn0*xt1 - xn0*zt1)
amat(3,3) = (xn0*yt1 - yn0*xt1)

bmat(1,1) = cs
bmat(1,2) = 0.
bmat(1,3) = sn
bmat(2,1) = 0.
bmat(2,2) = 1.
bmat(2,3) = 0.
bmat(3,1) = -sn
bmat(3,2) = 0.
bmat(3,3) = cs

at   = TRANSPOSE( amat )
cmat = MATMUL( bmat,amat )
amat = MATMUL( at,cmat )

bmat(1,1) = DBLE(p%sxx)
bmat(1,2) = DBLE(p%sxy)
bmat(1,3) = DBLE(p%sxz)
bmat(2,1) = DBLE(p%sxy)
bmat(2,2) = DBLE(p%syy)
bmat(2,3) = DBLE(p%syz)
bmat(3,1) = DBLE(p%sxz)
bmat(3,2) = DBLE(p%syz)
bmat(3,3) = DBLE(p%szz)

at   = TRANSPOSE( amat )
cmat = MATMUL( amat,bmat )
bmat = MATMUL( cmat,at )

p%sxx = SNGL(bmat(1,1))
p%sxy = SNGL(bmat(1,2))
p%sxz = SNGL(bmat(1,3))
p%syy = SNGL(bmat(2,2))
p%syz = SNGL(bmat(2,3))
p%szz = SNGL(bmat(3,3))

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE dense_rot_ter( hx0,hy0,p,pd )

USE scipuff_fi

IMPLICIT NONE

REAL,                  INTENT( IN    ) :: hx0,hy0
TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd

REAL, PARAMETER :: VELFAC_DENSE = 0.318310 !1./pi
REAL, PARAMETER :: RFAC_DENSE   = 1.5

REAL deth, tem, alp, emaj, emin, sxx, sxy, syy, xx, yy
REAL(8) amat(3,3), bmat(3,3), cmat(3,3), at(3,3)

LOGICAL, EXTERNAL :: check_slope

IF( check_slope(hx0,hy0) )THEN

  CALL set_matrot( hx0,hy0,amat )
  bmat(1,1) = DBLE(p%sxx)
  bmat(1,2) = DBLE(p%sxy)
  bmat(1,3) = DBLE(p%sxz)
  bmat(2,1) = DBLE(p%sxy)
  bmat(2,2) = DBLE(p%syy)
  bmat(2,3) = DBLE(p%syz)
  bmat(3,1) = DBLE(p%sxz)
  bmat(3,2) = DBLE(p%syz)
  bmat(3,3) = DBLE(p%szz)

  cmat = MATMUL( amat,bmat )
  at   = TRANSPOSE( amat )
  bmat = MATMUL( cmat,at )

  sxx = SNGL(bmat(1,1))
  sxy = SNGL(bmat(1,2))
  syy = SNGL(bmat(2,2))

ELSE

  sxx = p%sxx
  sxy = p%sxy
  syy = p%syy

END IF

!------ Horizontal major and minor axes

deth = sxx*syy - sxy*sxy
tem  = 0.5*(sxx+syy)
alp  = SQRT(MAX(tem*tem-deth,0.))
emaj = tem + alp
emin = tem - alp

pd%X = RFAC_DENSE*SQRT(emaj)
pd%Y = RFAC_DENSE*SQRT(emin)

!-----  Rotation coefficients for principal axis system
!       X =  cs*x + sn*y                x = cs*X - sn*Y
!       Y = -sn*x + cs*y                y = sn*X + cs*Y

tem = emaj - sxx
IF( tem > SMALL )THEN
  alp = sxy/tem
  tem = SQRT(1.+alp*alp)
  pd%cs = alp/tem
  pd%sn = 1.0/tem
ELSE
  pd%sn = 0.0
  pd%cs = 1.0
END IF

!-----  Dense gas velocity scale

xx    = pd%X**2
yy    = pd%Y**2
pd%u0 = -VELFAC_DENSE*pd%w*SQRT(xx+yy)/xx/yy

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE get_wash( p,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi
USE chem_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p
REAL,             INTENT( IN ) :: dt

REAL, PARAMETER :: CLDMIN = 500.

REAL    vr

!-----  Look up Raindrop Fall Velocity and Scavenging Coefficient
!-----  Interpolate Precipitation Rate from Specified Index
!-----  I = 0  --> PR =   0.0 mm/hr     !RAIN
!-----  I = 1  --> PR =   0.5 mm/hr
!-----  I = 2  --> PR =   3.5 mm/hr
!-----  I = 3  --> PR =  25.0 mm/hr
!-----  I = 4  --> PR =   5.0 mm/hr     !SNOW
!-----  I = 5  --> PR =  20.0 mm/hr
!-----  I = 6  --> PR = 100.0 mm/hr

IF( lwash )THEN

  IF( p%zbar <= MAX(zinv,hp+CLDMIN) )THEN
    ityppr = NINT(prbl)
  ELSE
    ityppr = 0
  END IF
  IF ( ityppr <= NRAIN ) THEN
    iprtyp = ityppr
  ELSE
    iprtyp = 0
  END IF

  vr   = vwash(ityppr)
  taur = twash(ityppr,p%ityp)

!-----  Create temporary rain puff for deposition calculation

  IF( taur == 0.0 )THEN
    fwash = 0.0
  ELSE
    tauc  = tauc + taur
    fwash = taur/tauc
    IF( lsrf )THEN
      CALL rain_puff( p,vr,dt )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF

ELSE

  fwash = 0.

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE rain_puff( p,vr,dt )

USE scipuff_fi
USE met_fi
USE step_p_fi

!  Creates a puff for rain deposition

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p
REAL,             INTENT( IN ) :: vr
REAL,             INTENT( IN ) :: dt

INTEGER ios
REAL    fac, frac, dtr, h1, hx1, hy1

fac  = 1.0 / (1.0 + tauc*dt)
frac = fwash*(1.0 - fac)

ALLOCATE( washPuff,STAT=ios )
IF( ios /= 0 )THEN
  eRoutine = 'RainPuff'
  eMessage = 'Error allocating washout puff'
  WRITE(eInform,'(A,I0)')'IOS =',ios
  GOTO 9999
END IF
NULLIFY( washPuff%aux )

CALL zero_puff( washPuff )

IF( vr > SMALL )THEN
  dtr = p%zbar/vr
ELSE
  dtr = 0.0
END IF

washPuff%xbar = p%xbar + DBLE(p%uo*dtr*xmap)
washPuff%ybar = p%ybar + DBLE(p%vo*dtr*ymap)
washPuff%zbar = 0.

washPuff%axx = p%axx - p%axz*p%axz/p%azz
washPuff%axy = p%axy - p%axz*p%ayz/p%azz
washPuff%axz = 0.
washPuff%ayy = p%ayy - p%ayz*p%ayz/p%azz
washPuff%ayz = 0.
washPuff%azz = p%azz

IF( lter )THEN
  CALL get_topogIn( SNGL(washPuff%xbar),SNGL(washPuff%ybar),h1,hx1,hy1,metField )
  CALL rot_norm( 0.0,0.0,hx1,hy1,washPuff )
END IF

CALL set_puff_xc( washPuff,p,frac )

washPuff%ityp = p%ityp
washPuff%inxt = 0
washPuff%iprv = 0
washPuff%idtl = p%idtl
washPuff%idtn = 0
washPuff%ipgd = p%ipgd

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

RECURSIVE SUBROUTINE rot_norm( hx0,hy0,hx1,hy1,p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: hx0, hy0, hx1, hy1

REAL(8) xn0, yn0, zn0, xn1, yn1, zn1, xt1, yt1, zt1, fac, cs, sn, dhx0, dhx1, dhy0, dhy1

REAL(8), DIMENSION(3,3) :: amat, bmat, cmat, at

LOGICAL, EXTERNAL :: check_slope

IF( .NOT.check_slope(hx1-hx0,hy1-hy0) )RETURN

dhx0 = DBLE(hx0)
dhy0 = DBLE(hy0)
dhx1 = DBLE(hx1)
dhy1 = DBLE(hy1)

fac = 1.0D0/SQRT(1.0D0+dhx0*dhx0+dhy0*dhy0)
xn0 = -fac*dhx0
yn0 = -fac*dhy0
zn0 =  fac

fac = 1.0D0/SQRT(1.0D0+dhx1*dhx1+dhy1*dhy1)
xn1 = -fac*dhx1
yn1 = -fac*dhy1
zn1 =  fac

cs = xn0*xn1 + yn0*yn1 + zn0*zn1

xt1 = (yn0*zn1 - zn0*yn1)
yt1 = (zn0*xn1 - xn0*zn1)
zt1 = (xn0*yn1 - yn0*xn1)
sn = SQRT(xt1*xt1+yt1*yt1+zt1*zt1)

xt1 = xt1/sn
yt1 = yt1/sn
zt1 = zt1/sn

amat(1,1) = xn0
amat(1,2) = yn0
amat(1,3) = zn0
amat(2,1) = xt1
amat(2,2) = yt1
amat(2,3) = zt1
amat(3,1) = (yn0*zt1 - zn0*yt1)
amat(3,2) = (zn0*xt1 - xn0*zt1)
amat(3,3) = (xn0*yt1 - yn0*xt1)

bmat(1,1) = cs
bmat(1,2) = 0.
bmat(1,3) = sn
bmat(2,1) = 0.
bmat(2,2) = 1.
bmat(2,3) = 0.
bmat(3,1) = -sn
bmat(3,2) = 0.
bmat(3,3) = cs

at = TRANSPOSE( amat )
cmat = MATMUL( bmat,amat )
amat = MATMUL( at,cmat )

bmat(1,1) = DBLE(p%axx)
bmat(1,2) = DBLE(p%axy)
bmat(1,3) = DBLE(p%axz)
bmat(2,1) = DBLE(p%axy)
bmat(2,2) = DBLE(p%ayy)
bmat(2,3) = DBLE(p%ayz)
bmat(3,1) = DBLE(p%axz)
bmat(3,2) = DBLE(p%ayz)
bmat(3,3) = DBLE(p%azz)

at = TRANSPOSE( amat )
cmat = MATMUL( amat,bmat )
bmat = MATMUL( cmat,at )

p%axx = SNGL(bmat(1,1))
p%axy = SNGL(bmat(1,2))
p%axz = SNGL(bmat(1,3))
p%ayy = SNGL(bmat(2,2))
p%ayz = SNGL(bmat(2,3))
p%azz = SNGL(bmat(3,3))

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE polar_rot( p )

USE step_p_fi
USE constants_fd
USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL(8) del, sn, cs

REAL(8), DIMENSION(3,3) :: smat, stem, arot, atrot

!----- Rotate puff moments to follow lat/lon change

IF( p%ybar < 0.0 )THEN
  del = DBLE(PI180)*(p%xbar-lonsav)
ELSE
  del = DBLE(PI180)*(lonsav-p%xbar)
END IF

sn = DSIN(del)
cs = DCOS(del)

smat(:,1) = (/ DBLE(p%sxx), DBLE(p%sxy), DBLE(p%sxz) /)
smat(:,2) = (/ DBLE(p%sxy), DBLE(p%syy), DBLE(p%syz) /)
smat(:,3) = (/ DBLE(p%sxz), DBLE(p%syz), DBLE(p%szz) /)

!------ Rotation matrix

arot(:,1) = (/  cs  , sn  , 0.D0 /)
arot(:,2) = (/ -sn  , cs  , 0.D0 /)
arot(:,3) = (/  0.D0, 0.D0, 1.D0 /)

!------ Rotate moments

atrot = TRANSPOSE( arot )
stem = MATMUL( arot,smat )
smat = MATMUL( stem,atrot )

!------ Copy out into puff structure

p%sxx = SNGL(smat(1,1)); p%sxy = SNGL(smat(2,1)); p%sxz = SNGL(smat(3,1))
                         p%syy = SNGL(smat(2,2)); p%syz = SNGL(smat(3,2))
                                                  p%szz = SNGL(smat(3,3))

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION getGasDensity( icls,pmatlIn )

USE scipuff_fi
USE step_p_fi
USE constants_fd

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: icls
TYPE( puff_material ), INTENT( IN ) :: pmatlIn

TYPE( gas_material ) pmatl_gas
TYPE( liquid_material ) pmatl_liq
TYPE( aerosol_material ) pmatl_aer

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsAerosol

IF( IsAerosol(icls) )THEN
  IF( IsAerosol(material(imat)%icls) )THEN
    pmatl_aer = TRANSFER(pmatlIn,pmatl_aer)
    getGasDensity = rhoair*pmatl_aer%w/MWAIR
  ELSE
    pmatl_liq = TRANSFER(pmatlIn,pmatl_liq)
    getGasDensity = rhoair*pmatl_liq%w/MWAIR
  END IF
ELSE IF( IsGas(icls) )THEN
  pmatl_gas = TRANSFER(pmatlIn,pmatl_gas)
  getGasDensity = pmatl_gas%rho
ELSE
  getGasDensity = 0.0
END IF

RETURN
END
