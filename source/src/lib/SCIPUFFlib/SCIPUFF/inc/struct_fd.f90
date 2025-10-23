!=======================================================================
!    SCIPUFF structure definitions - puff structures
!=======================================================================
MODULE puffstruct_fd

  USE puff_fd

  TYPE  puff_str !Basic puff structure - by variable name
    SEQUENCE
    REAL(8) xbar,ybar
    REAL    zbar
    REAL    sxx,sxy,sxz,syy,syz,szz
    REAL    axx,axy,axz,ayy,ayz,azz,det
    REAL    c,cc,xuc,xvc,yvc,yvsc,yvbc,zwc,wc,ccb
    REAL    si,si2,sv
    REAL    sr,cfo
    REAL    zi,zc
    REAL    uo,vo,wo
    INTEGER ityp,inxt,iprv,ipgd,idtl,idtn,naux
    REAL, DIMENSION(:), POINTER :: aux
  END TYPE  puff_str

  TYPE  puff_str_xc !Basic puff structure - by array name
    SEQUENCE
    REAL(8), DIMENSION(2)       :: xcent
    REAL                        :: zbar
    REAL,    DIMENSION(6)       :: sig
    REAL,    DIMENSION(7)       :: asig
    REAL,    DIMENSION(NP_SUM)  :: psum
    REAL,    DIMENSION(NP_RAT)  :: prat
    INTEGER, DIMENSION(NP_INT)  :: jdum
    REAL, DIMENSION(:), POINTER :: aux
  END TYPE  puff_str_xc

  TYPE  puff_str_ri !Basic puff structure - by variable TYPE
    SEQUENCE
    REAL(8), DIMENSION(NP_DBLE) :: p_dble
    REAL,    DIMENSION(NP_REAL) :: p_real
    INTEGER, DIMENSION(NP_INT)  :: p_int
    REAL, DIMENSION(:), POINTER :: aux
  END TYPE  puff_str_ri

  TYPE  puff_str_ri_NOaux !Basic puff structure - by variable TYPE - w/o aux. Used in reading old puff files
    SEQUENCE
    REAL(8), DIMENSION(NP_DBLE) :: p_dble
    REAL,    DIMENSION(NP_REAL) :: p_real
    INTEGER, DIMENSION(NP_INT)  :: p_int
  END TYPE  puff_str_ri_NOaux

  TYPE  puff_str_ri_NOaux_old !Basic puff structure - by variable TYPE - w/o aux. Used in reading old puff files
    SEQUENCE
    REAL,    DIMENSION(NP_REAL+2) :: p_real
    INTEGER, DIMENSION(NP_INT)    :: p_int
  END TYPE  puff_str_ri_NOaux_old

END MODULE puffstruct_fd
!=======================================================================
!    SCIPUFF structure definitions - material structures
!=======================================================================
MODULE matlstruct_fd

  TYPE  material_str
    SEQUENCE
    INTEGER   icls     !Class index
    INTEGER   iaux     !Matl Auxiliary pointer
    INTEGER   mcID     !Matl Multi-Comp ID
    INTEGER   nmc      !No. of multi-component data values
    INTEGER   ioffp    !Puff ityp Offset
    INTEGER   ioffs    !Srf Deposition Offset
    INTEGER   ioffd    !Srf Dose Offset
    INTEGER   AuxMatID !Auxiliary material pointer
    INTEGER   jNotUsed !Place holder
    INTEGER   effClass !effects flag
    INTEGER   effAvail !effects flag

    LOGICAL   lsrfg !Srf Deposition by group
    LOGICAL   lsrft !Srf Deposition total
    LOGICAL   ldosg !Srf Dose by group
    LOGICAL   ldost !Srf Dose total
    LOGICAL, DIMENSION(2) :: lNotUsed  !Place holders

    REAL, DIMENSION(10) :: prop !decay parameters

    CHARACTER(16) cmat !Material Name
    CHARACTER(16) ccls !Class Name
    CHARACTER(16) unit !Mass units
    CHARACTER(64) file !Material file name
    CHARACTER(64) path !Material file path

  END TYPE  material_str

END MODULE matlstruct_fd
!=======================================================================
!    SCIPUFF structure definitions - TYPE structures
!=======================================================================
MODULE typestruct_fd

  TYPE  type_str
    SEQUENCE
    INTEGER imat  !Material Pointer
    INTEGER igrp  !SubGroup Pointer
    INTEGER npaux !No.Puff Auxiliary data
    INTEGER ipmc  !Pointer for Multi-Component aux data
    INTEGER mcID  !Multi-Component ID
    INTEGER icls  !Puff Class
    LOGICAL ltot  !Total variance flag
  END TYPE  type_str

END MODULE typestruct_fd
!=======================================================================
!    SCIPUFF structure definitions - material aux structures
!=======================================================================
MODULE mauxstruct_fd

  USE puff_fd

  TYPE  gas_material
    SEQUENCE
    REAL rho !Density
    REAL vd  !Deposition velocity
  END TYPE  gas_material

  TYPE  part_material
    SEQUENCE
    INTEGER nsg   !No. of subgroups
    REAL    rho   !Density
    REAL    dmin  !Min diameter
    REAL    dbar  !Mean diameter
    REAL    vd    !Fall speed
    REAL    sigvd !Sigma speed
    REAL    diff  !Brownian diffusion
    REAL    dmax  !Max diameter
  END TYPE  part_material

  TYPE  liquid_material
    SEQUENCE
    INTEGER nsg                         !No. of subgroups
    REAL    rho                         !Density
    REAL    rhob                        !Density Temperature coefficient
    REAL    a                           !Antoine Coefficient a
    REAL    b                           !Antoine Coefficient b
    REAL    c                           !Antoine Coefficient c
    REAL    w                           !Molecular Weight
    REAL    st                          !Surface Tension
    REAL    sf                          !Spread factor (Secondary Evaporation flag)
    REAL    viscosity                   !Viscosity (for sfc absorption)
    REAL    cpL                         !Specific heat (liquid)
    REAL    cpV                         !Specific heat (vapor)
    REAL, DIMENSION(MAXLMAUXP-10) :: dum !Extra space for expansion
    REAL    dmin                        !Min diameter
    REAL    dbar                        !Mean diameter
    REAL    sigvd                       !Sigma speed factor
    REAL    diff                        !Brownian diffusion
    REAL    dmax                        !Max diameter
  END TYPE  liquid_material

  TYPE  aerosol_material
    SEQUENCE
    REAL vd                             !Gas deposition velocity
    REAL rho                            !Liquid density
    REAL a                              !Antoine Coefficient a
    REAL b                              !Antoine Coefficient b
    REAL c                              !Antoine Coefficient c
    REAL w                              !Molecular Weight
    REAL cpL                            !Specific heat (liquid)
    REAL cpV                            !Specific heat (vapor)
    REAL, DIMENSION(MAXAMAUXP-8) :: dum !Extra space for expansion
  END TYPE  aerosol_material

  TYPE  puff_material
    SEQUENCE
    REAL, DIMENSION(MAXLMAUXP+MAXLMAUX+3) :: param !Liquid since it is biggest
  END TYPE  puff_material

END MODULE mauxstruct_fd
!=======================================================================
!    SCIPUFF structure definitions - puff aux structures
!=======================================================================
MODULE pauxstruct_fd

  USE puff_fd

  TYPE  puff_dynamics
    SEQUENCE
    REAL wcb  !<wbar*cbar>
    REAL ctb  !<cbar*tbar>
    REAL wcp  !<wc>
    REAL ctp  !<ct>
    REAL ucb  !<ubar*cbar>
    REAL vcb  !<vbar*cbar>
    REAL ucp  !<uc>
    REAL vcp  !<vc>
    REAL un   !<u>
    REAL vn   !<v>
    REAL w    !<w>
    REAL t    !<theta>
    REAL bcb  !<bbar*cbar> - buoyancy from gas density
    REAL bcp  !<bc>
    REAL u    !Dense gas dynamic u-velocity
    REAL v    !Dense gas dynamic v-velocity
    REAL dudx !Dense gas dynamic vel gradients
    REAL dudy
    REAL dvdx
    REAL dvdy
    REAL u0   !Dense gas velocity scale
    REAL X    !Major axis
    REAL Y    !Minor axis
    REAL sn   !Rotation matrix coefficients
    REAL cs   !Rotation matrix coefficients
  END TYPE  puff_dynamics

  TYPE  puff_dynamics_data
    SEQUENCE
    REAL, DIMENSION(NAUX_DYNAMICS) :: data
    REAL, DIMENSION(NAUX_BUOY)     :: data_buoy
    REAL, DIMENSION(NAUX_DENSE)    :: data_dense
  END TYPE  puff_dynamics_data

  TYPE  puff_totalcc
    SEQUENCE
    REAL cctb
    REAL cct
  END TYPE  puff_totalcc

  TYPE  puff_liquid
    SEQUENCE
    REAL d     !liquid droplet diameter
    REAL sigd  !rms spread
    REAL t     !droplet temperature
    REAL ccs   !save mass before evap step
    REAL tevap !evaporative temperature integral
    REAL ccbg  !overlap integral for size group
  END TYPE  puff_liquid

  TYPE  puff_aerosol
    SEQUENCE
    REAL fl    !liquid species fraction
    REAL tevap !evaporative temperature integral
    REAL co    !species concentration
    REAL fw   !liquid water fraction
  END TYPE  puff_aerosol

  TYPE  puff_static
    SEQUENCE
    REAL    sr
    INTEGER isnxt
    INTEGER isprv
  END TYPE  puff_static

END MODULE pauxstruct_fd
!=======================================================================
!    SCIPUFF structure definitions - multicomponent data
!=======================================================================
MODULE mcstruct_fd

  TYPE  material_MClist
    SEQUENCE
    INTEGER                        :: nMCtype
    INTEGER, DIMENSION(:), POINTER :: type
    INTEGER, DIMENSION(:), POINTER :: ID
  END TYPE  material_MClist

END MODULE mcstruct_fd
!=======================================================================
!    SCIPUFF structure definitions - release data for randomized locations
!=======================================================================
MODULE releaseID_fd
  TYPE  Data_relID
    SEQUENCE
    REAL    :: SigRel
    REAL    :: SigMerge
  END TYPE  Data_relID
END MODULE releaseID_fd
!=======================================================================
!    SCIPUFF structure definitions - all
!=======================================================================
MODULE struct_fd
  USE puffstruct_fd
  USE matlstruct_fd
  USE typestruct_fd
  USE mauxstruct_fd
  USE pauxstruct_fd
  USE releaseID_fd
END MODULE struct_fd
