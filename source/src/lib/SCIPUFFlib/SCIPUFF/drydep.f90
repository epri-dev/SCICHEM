!VERSION 161201_16:25
MODULE drydep_fd

SAVE

INTEGER, PARAMETER :: NLU = 26

LOGICAL :: lwarn = .TRUE.

REAL, DIMENSION(NLU,15) :: lai_ref = RESHAPE( (/&
       0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0, &
       0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 1.0, 4.0, 0.0, 3.0, 3.0,& !Jan
       0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0, &
       0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 1.0, 4.0, 0.0, 3.0, 3.0,& !Feb
       0.0, 0.0, 0.0, 5.0, 6.0, 0.5, 0.5, 6.0, 4.0, 3.0, &
       1.0, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 0.5, 4.0, 0.0, 3.0, 3.0,& !Mar
       0.0, 0.0, 0.0, 5.0, 6.0, 1.0, 1.0, 6.0, 4.0, 3.0, &
       1.0, 3.0, 1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0, &
       0.1, 0.1, 4.0, 0.0, 4.0, 4.0,& !Apr
       0.0, 0.0, 0.0, 5.0, 6.0, 2.0, 2.0, 6.0, 4.0, 3.0, &
       1.5, 3.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
       0.5, 0.1, 4.0, 0.0, 4.5, 4.5,& !May
       0.0, 0.0, 0.0, 5.0, 6.0, 4.0, 4.0, 6.0, 4.0, 3.0, &
       2.0, 3.0, 1.0, 0.5, 2.0, 2.5, 3.0, 2.0, 3.0, 1.0, &
       1.0, 0.1, 4.0, 0.0, 5.0, 5.0,& !Jun
       0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0, &
       3.0, 3.0, 1.0, 1.0, 3.0, 4.0, 4.0, 3.0, 4.0, 1.0, &
       1.0, 0.1, 4.0, 0.0, 5.0, 5.0,& !Jul
       0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0, &
       3.0, 3.0, 1.0, 2.0, 3.5, 5.0, 4.5, 3.5, 4.5, 1.0, &
       1.0, 1.0, 4.0, 0.0, 5.0, 5.0,& !Aug
       0.0, 0.0, 0.0, 5.0, 6.0, 4.0, 4.0, 6.0, 4.0, 3.0, &
       2.0, 3.0, 1.0, 2.0, 4.0, 6.0, 5.0, 4.0, 5.0, 1.0, &
       1.0, 2.0, 4.0, 0.0, 4.0, 4.0,& !Sep
       0.0, 0.0, 0.0, 5.0, 6.0, 2.0, 2.0, 6.0, 4.0, 3.0, &
       1.5, 3.0, 1.0, 1.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       1.0, 1.5, 4.0, 0.0, 3.0, 3.0,& !Oct
       0.0, 0.0, 0.0, 5.0, 6.0, 1.0, 1.0, 6.0, 4.0, 3.0, &
       1.0, 3.0, 1.0, 1.0, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.4, 1.5, 4.0, 0.0, 3.0, 3.0,& !Nov
       0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0, &
       0.5, 3.0, 1.0, 1.0, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 1.0, 4.0, 0.0, 3.0, 3.0,& !Dec
       0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0, &
       0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 1.0, 4.0, 0.0, 3.0, 3.0,& !Jan
       0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0, &
       0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0, &
       0.1, 0.1, 4.0, 0.0, 3.0, 3.0,& !MIN
       0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0, &
       3.0, 3.0, 1.0, 2.0, 4.0, 6.0, 5.0, 4.0, 5.0, 1.0, &
       1.0, 2.0, 4.0, 0.0, 5.0, 5.0 & !MAX
       /) , (/ NLU,15 /) )

!----- Index for matching CAMx (26) LU categories with corresponding SCIPUFF LU
REAL, DIMENSION(33) :: CAMxLU = &
        (/   &
        21, &  !urban;                      1=Developed
        15, &  !crops;                      2=Dry Cropland & Pasture
        20, &  !irrigated crops;            3=Irrigated Cropland
        13, &  !short grass and forbs;      4=Missing
        15, &  !crops;                      5=Cropland/Grassland
        26, &  !transitional forest;        6=Cropland/Woodland
        13, &  !short grass and forbs;      7=Grassland
        12, &  !thorn shrubs;               8=Shrubland
        11, &  !deciduous shrubs;           9=Shrubland/Grassland
        14, &  !long grass;                 10=Savanna
        7,  &  !deciduous broadleaf trees;  11=Deciduous Broadleaf Forest
        6,  &  !deciduous needleleaf trees; 12=Deciduous Needleleaf Forest
        5,  &  !evergreen broadleaf trees;  13=Evergreen Broadleaf Forest
        4,  &  !evergreen needleleaf trees; 14=Evergreen Needleleaf Forest
        25, &  !mixed wood forests;         15=Mixed Forest
        1,  &  !water;                      16=Water
        23, &  !swamp;                      17=Herbaceous Wetland
        23, &  !swamp;                      18=Wooded Wetland
        24, &  !desert;                     19=Barren
        22, &  !tundra;                     20=Herbaceous Tundra
        22, &  !tundra;                     21=Wooded Tundra
        22, &  !tundra;                     22=Mixed Tundra
        22, &  !tundra;                     23=Bare Tundra
        2,  &  !ice;                        24=Snow or Ice
        13, &  !short grass and forbs;      25=Partly Developed
        13, &  !short grass and forbs;      26=Not Used
        13, &  !short grass and forbs;      27=Not Used
        13, &  !short grass and forbs;      28=Unclassified
        21, &  !urban;                      29=Urban Superclass
        14, &  !long grass;                 30=Grassland Superclass
        25, &  !mixed wood forests;         31=Forest Superclass
        24, &  !desert;                     32=Desert Superclass
        1  &   !water;                      33=Water Superclass
        /)

!-----Surface roughness length (m) for 26 LU categories
!     for the Zhang (2003) dry dep scheme.
!     z01 and z02 are minimum and maximum z0 for each luc.

!     z01     -- minimum surface roughness for Zhang LU
!     z02     -- maximum surface roughness for Zhang LU

REAL, DIMENSION(NLU) :: z01 = (/ &
       0.0 ,  0.01,  0.0 ,  0.9 ,  2.0 , &
       0.4 ,  0.4 ,  2.5 ,  0.6 ,  0.2 , &
       0.05,  0.2 ,  0.04,  0.02,  0.02, &
       0.02,  0.02,  0.02,  0.02,  0.05, &
       1.0 ,  0.03,  0.1 ,  0.04,  0.6 , &
       0.6 /)

REAL, DIMENSION(NLU) :: z02 = (/ &
       0.0 ,  0.01,  0.0 ,  0.9 ,  2.0 , &
       0.9 ,  1.0 ,  2.5 ,  0.6 ,  0.2 , &
       0.2 ,  0.2 ,  0.04,  0.1 ,  0.1 , &
       0.1 ,  0.1 ,  0.1 ,  0.2 ,  0.05, &
       1.0 ,  0.03,  0.1 ,  0.04,  0.9 , &
       0.9 /)

!-----Season indices by month and latitude band
!     Season Indices            Latitude Bands
!     1 = summer                1 = <20    Tropical
!     2 = autumn                2 = 20-35  Sub-tropical
!     3 = winter w/o snow       3 = 35-50  Temperate
!     4 = winter w/ snow        4 = 50-75  Cool
!     5 = spring                5 = >75    Polar
!                    Latitude Band
!     iseason -- season index map by latitude band and month

INTEGER, DIMENSION(5,12) :: iseason = &
        RESHAPE( (/ 1, 3, 3, 3, 3,& ! Jan
                    1, 5, 3, 3, 3,& ! Feb
                    1, 5, 5, 3, 3,& ! Mar
                    1, 5, 5, 5, 3,& ! Apr
                    1, 1, 5, 5, 3,& ! May
                    1, 1, 1, 1, 5,& ! Jun
                    1, 1, 1, 1, 1,& ! Jul
                    1, 1, 1, 1, 2,& ! Aug
                    1, 1, 2, 2, 3,& ! Sep
                    1, 2, 2, 2, 3,& ! Oct
                    1, 2, 2, 3, 3,& ! Nov
                    1, 2, 3, 3, 3 & ! Dec
                    /),(/5,12/) )


END MODULE drydep_fd

!===============================================================================

RECURSIVE SUBROUTINE set_sp_vdep( p )

USE mpi_fi, ONLY: isSerial
USE drydep_fd
USE scipuff_fi
USE met_fi
USE SWIMparam_fd
USE step_p_fi
USE chem_fi
USE files_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL zenith, plat, plon
REAL windsp, tsrf
REAL ustr, el, dxx

INTEGER i,lucat,luse,ios,irv,alloc_stat
INTEGER jday, month, day

REAL, DIMENSION(:), ALLOCATABLE ::  vdep  ! species-dependent deposition velocity field (m/s)

INTEGER, EXTERNAL :: SWIMgetSpcDepMet
REAL,    EXTERNAL :: SetPrate

nGaseous = chem%nGaseous
gaseous  => chem%gaseous

ALLOCATE(vdep(nGaseous),STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'set_sp_vdep'
  eMessage = 'Error allocating vdep arrays'
  WRITE(eAction,*,IOSTAT=ios)'Requested size: ',nGaseous
  GOTO 9999
END IF

!------ Copy from met_puf common

IF( p%idtl == I_STATIC .OR. isSerial )THEN

dxx = 0.5*SNGL(p%sxx+p%syy)

irv = SWIMgetSpcDepMet( SNGL(p%xbar),SNGL(p%ybar),dxx,p%zbar-hp,jday,zenith,plat,plon,&
                        tsrf,ustr,el,lucat )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMgetSpcDepMet' )
  GOTO 9999
END IF

ELSE

zenith = znth_dep
tsrf   = t_dep
ustr   = ustdep
el     = xml
lucat  = lu_cat
jday   = jday_dep
plat   = lat_dep
plon   = lon_dep

END IF

IF( prate == NOT_SET_R )THEN
  prate = SetPrate( p%zbar,zinv,hp,prbl )
END IF

windsp = MAX(0.1,SQRT(ub**2 + vb**2))

CALL julian_ymd( jday,year_start,month,day )

! Find CAMx LUC
IF( lucat == 0 )THEN
  IF( lWarn )THEN
    lWarn = .FALSE.
    WRITE(lun_log,'("WARNING: Landuse category not defined")')
  END IF
  lucat = 4
ELSE IF ( lucat > 1000 )THEN
  SELECT CASE( lucat )
  CASE( 1001 )
    lucat = 29
  CASE( 1002 )
    lucat = 30
  CASE( 1003 )
    lucat = 31
  CASE( 1004 )
    lucat = 32
  CASE( 1005 )
    lucat = 33
  CASE DEFAULT
    lucat = 4
  END SELECT
END IF
luse = CAMxLU(lucat)

CALL drydep(zenith,tsrf,p%zbar-hp,tb,plat,plon,prate,windsp, &
            hb,ustr,el,luse,month,day,ngaseous,vdep)

DO i = 1,ngaseous
  gaseous(i)%s%vdep = vdep(i)
END DO

9999 CONTINUE

IF( ALLOCATED(vdep) )DEALLOCATE(vdep,STAT=alloc_stat)

RETURN
END

!===============================================================================

SUBROUTINE drydep(zenith,tsurf,height,tempk,plat,plon,prate,windsp, &
                  rh,ustar,el,luse,month,day,nspec,vdep)
USE drydep_fd
USE chem_fi

IMPLICIT NONE


!----Based on CAMx v6.30 160408, Copyright 1996 - 2016, Ramboll ENVIRON

!     DRYDEP is the driver for the calculation of dry deposition
!     velocities of gas-phase species

!---- Input arguments

REAL,    INTENT( IN ) :: zenith   ! zenith angle (deg)
REAL,    INTENT( IN ) :: tsurf    ! surface temperature at puff location (K)
REAL,    INTENT( IN ) :: height   ! height of puff centroid (m)
REAL,    INTENT( IN ) :: tempk    ! temperature at puff centroid (K)
REAL,    INTENT( IN ) :: plat     ! puff centroid latitude (deg)
REAL,    INTENT( IN ) :: plon     ! puff centroid longitude (deg)
REAL,    INTENT( IN ) :: prate    ! precipitation rate (mm/hr)
REAL,    INTENT( IN ) :: windsp   ! wind speed at puff location (m/s)
REAL,    INTENT( IN ) :: rh       ! relative humidity(ratio) at puff location
REAL,    INTENT( IN ) :: ustar    ! ustar
REAL,    INTENT( IN ) :: el       ! ML length
INTEGER, INTENT( IN ) :: luse     ! land use type at puff location
INTEGER, INTENT( IN ) :: month    ! month for which dry dep velocities are calculated
INTEGER, INTENT( IN ) :: day      ! day of month
INTEGER, INTENT( IN ) :: nspec    ! number of gaseous species

!--- Output arguments:
REAL, DIMENSION(nspec), INTENT( OUT ) ::  vdep  ! species-dependent deposition velocity field (m/s)

!
INTEGER :: i, mbin, latbin, isesn
INTEGER :: l, iflgso2, iflgo3

REAL    :: temp0
REAL    :: z0
REAL    :: henso2, henry, vd
REAL    :: zl
REAL    :: snow

INTEGER, DIMENSION(12):: nday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
REAL :: tfactso2 = -3156.
REAL :: henso20 = 1.0e+5  !henso20  -- Henry's Law constant at STP for SO2
REAL :: ph = 7.

REAL lai_f, pi
REAL ctrns,fcld
REAL eps
REAL henry0, tfact, f0, rscale, diffrat

DOUBLE PRECISION coszen,solflux

CHARACTER(16) :: spname

!-----ENTRY point

pi = 3.1415927
eps = 1.e-10

!DO i = 1,nspec
!  WRITE(*,*)gaseous(i)%s%Henry0  !EPRI model Henry's law constant for species at 298 K
!  WRITE(*,*)gaseous(i)%s%TpFac   !EPRI model temperature factor for Henry's law constant
!  WRITE(*,*)gaseous(i)%s%RxFac   !EPRI model reactivity factor
!  WRITE(*,*)gaseous(i)%s%SrfRFac !EPRI surface resistance scaling factor
!  WRITE(*,*)gaseous(i)%s%ldrydep !EPRI dry deposition flag
!END DO

!-----Determine season

mbin = month
IF (plat<0.) THEN
  mbin = mod(month+6,12)
  IF (mbin==0) mbin = 12
ENDIF
latbin = 1
IF (ABS(plat)>20.) THEN
  latbin = 2
ELSEIF (ABS(plat)>35.) THEN
  latbin = 3
ELSEIF (ABS(plat)>50.) THEN
  latbin = 4
ELSEIF (ABS(plat)>75.) THEN
  latbin = 5
ENDIF
IF ((plat>50. .and. plat<75.) .and. &
         (plon>-15. .and. plon<15.)) latbin = 3
 isesn = iseason(latbin,mbin)

!-----use input snow cover to set season, if specified

snow = 0.
IF (snow>=0.001) isesn = 4    ! Snow cover check > 1 cm

!-----Calculate solar flux

coszen = cos(DBLE(zenith)*DBLE(pi)/180.)
ctrns = 1.
fcld  = 0.
solflux = (990.*coszen - 30.)
solflux = dmax1(DBLE(0.),solflux)

!-----Load local met variables

temp0 = tsurf - 273.15

DO l = 1,nspec
   vdep(l) = 0.
ENDDO

lai_f = lai_ref(luse,mbin) + &
        FLOAT(min(nday(mbin),day))/FLOAT(nday(mbin))* &
             (lai_ref(luse,mbin+1) - lai_ref(luse,mbin))

IF (luse==1 .or. luse==3) THEN
   z0 = 2.0e-6*windsp**2.5
ELSE
   IF (z02(luse)>z01(luse)) THEN
      z0 = z01(luse) + (lai_f - lai_ref(luse,14))/ &
                            (lai_ref(luse,15) - lai_ref(luse,14))* &
                            (z02(luse) - z01(luse))
   ELSE
      z0 = z01(luse)
   ENDIF
ENDIF

zl = height/el
IF( zl < 0. )THEN
   zl = min(-5.,zl)
ELSE
   zl = max(5.,zl)
ENDIF

!-----Loop over gas species, and calculate deposition velocity

CALL henryfnc('SO20',henso20,tfactso2,tsurf,ph,henso2)

DO i = 1,nspec  ! Loop over species
  vd = 0.
  IF( gaseous(i)%s%ldrydep )THEN
    henry0 = gaseous(i)%s%Henry0
    IF (henry0 > 1.e-6) THEN
      tfact = gaseous(i)%s%TpFac
      spname = TRIM(gaseous(i)%s%name)
      CALL henryfnc(spname,henry0,tfact,tsurf,ph,henry)
      IFlgso2 = 0
      IFlgo3 = 0
      IF (TRIM(spname) =='SO2') THEN ! identify SO2
          IFlgso2 = 1
          henry = henso2
      ENDIF
      IF (TRIM(spname) =='O3') IFlgo3 = 1 ! identify O3

      !--- use Zhang (2003) algorithm
      f0 = gaseous(i)%s%RxFac
      rscale = gaseous(i)%s%SrfRFac
      diffrat = sqrt(gaseous(i)%s%mw/18.)
      CALL vd_gas_zhang(height,zl,z0,ustar,tempk,tsurf,solflux,rh,fcld,prate,coszen,luse, &
                        snow,iflgo3,henry,henso2,f0,diffrat,rscale,lai_f,vd)

    END IF
  END IF
  vdep(i) = vd
END DO

RETURN
END
!===============================================================================

SUBROUTINE vd_gas_zhang(z2,zl,z0_f,ustar,t2,ts,srad,rh,fcld,prate, &
                             coszen,mlu,snow,io3,henry,henso2, &
                             f0,diffrat,rscale,lai_f,vd)

!----Based on CAMx v6.30 160408, Copyright 1996 - 2016, Ramboll ENVIRON

!     vd_gas_zhang is the driver to interface with the dry deposition MODULE
!     of Zhang et al. (2003) developed by the meteorological Service of Canada.
!     This routine prepares arguments TO be used in the MODULE.

!     Copyright 1996 - 2016
!     Ramboll ENVIRON

!     Input arguments:
!        z2                  met reference height (m)
!        zl                  Z/L stability PARAMETER
!        z0_f                surface roughness (m)
!        ustar               friction velocity (m/s)
!        t2                  temperature at z2 (K)
!        ts                  surface temperature (K)
!        srad                solar irradiance (W/m2)
!        rh                  relative humidity (0-1)
!        fcld                cloud fraction (0-1)
!        prate               precipitation rate (mm/hr)
!        coszen              cosine of solar zenith angle
!        mlu                 land USE index (1-26)
!        snow                snow water equivalent (m)
!        io3                 ozone flag
!        henry               Henry's Law constant (M/atm)
!        henso2              Henry's Law constant of SO2 (M/atm)
!        f0                  Reactivity PARAMETER
!        diffrat             Ratio of diffusivity (H2O:species)
!        rscale              Rs scaling for extremely soluble gasses
!        lai_f               leaf area index

!     Output arguments:
!        vd                  deposition velocity (m/s)

!     Routines CALLed:
!        DRYVD

!     CALLed by:
!        DRYDEP

IMPLICIT NONE

!-----Arguments

INTEGER mlu
INTEGER io3
REAL z2
REAL zl
REAL z0_f
REAL ustar
REAL t2
REAL ts
DOUBLE PRECISION srad
REAL rh
REAL fcld
DOUBLE PRECISION coszen
REAL snow
REAL henry
REAL henso2
REAL lai_f
REAL prate
REAL f0,diffrat,rscale
REAL vd

!-----Local variables

REAL diffh2o,rmin,rmax,sd,alpha,beta,rm,di
DOUBLE PRECISION vdg

DOUBLE PRECISION z2_tmp,zl_tmp,z0_f_tmp,ustar_tmp,sd_tmp,t2_tmp
DOUBLE PRECISION ts_tmp,rh_tmp,fcld_tmp,prec_tmp
DOUBLE PRECISION rm_tmp,alpha_tmp,beta_tmp,di_tmp,lai_f_tmp,rscalsp_tmp, &
            henry_tmp

!-----DATA statements

DATA diffh2o /2.30e-05/     ! water diffusivity (m2/s)
DATA rmin/1.0/, rmax/100./  !min/max resistances (s/m)

!-----ENTRY point

!-----Set snow depth (cm)

sd = snow*1000.  ! *100 for m->cm, *10 for SWE->depth

!-----Set deposition params for species.
!     Mesophyll resistance follows Wesely (1989)

alpha = henry/henso2
beta = f0
rm = 1./(henry/3000. + 100.*f0)
rm = MAX(rmin,rm)
rm = MIN(rmax,rm)

!-----Set diffusivity of gas species

di = diffh2o/diffrat      ! m2/s
di = di*1.e4              ! cm2/s

!-----CALL dry deposition velocity algorithm

z2_tmp = z2
zl_tmp = zl
z0_f_tmp = z0_f
ustar_tmp = ustar
sd_tmp = sd
t2_tmp = t2
ts_tmp = ts
rh_tmp = rh
fcld_tmp = fcld
prec_tmp = prate
rm_tmp = rm
alpha_tmp = alpha
beta_tmp = beta
di_tmp = di
lai_f_tmp = lai_f
rscalsp_tmp = rscale
henry_tmp = henry
CALL drygas(z2_tmp,zl_tmp,z0_f_tmp,ustar_tmp,sd_tmp, &
                 t2_tmp,ts_tmp,srad,rh_tmp,fcld_tmp,prec_tmp,coszen, &
                 mlu,rm_tmp,alpha_tmp,beta_tmp,di_tmp,vdg, &
                 lai_f_tmp,rscalsp_tmp,henry_tmp,io3)
vd = vdg

RETURN
END

!=======================================================================

SUBROUTINE drygas(z2,zl,z0_f,ustar,sd,t2,ts,srad,rh,fcld,prec, &
                      coszen,mlu,rm,alpha,beta,di,vdg,lai_f,rscalsp, &
                      henry,io3)
USE drydep_fd
!USE camx_prm
!USE deposit_inc

!-----------------------------------------------------------------------
!    References:
!      Zhang et al., 2003.  Atmos. Chem. Phys. 3, 2067-2082


!     LUC No.     Vegetation TYPE
!     =====       ===============
!       1         water
!       2         ice
!       3         inland lake
!       4         evergreen needleleaf trees
!       5         evergreen broadleaf trees
!       6         deciduous needleleaf trees
!       7         deciduous broadleaf trees
!       8         tropical broadleaf trees
!       9         drought deciduous trees
!       10        evergreen broadleaf shrub
!       11        deciduous shrubs
!       12        thorn shrubs
!       13        short grass and forbs
!       14        long grass
!       15        crops
!       16        rice
!       17        sugar
!       18        maize
!       19        cotTOn
!       20        irrigated crops
!       21        urban
!       22        tundra
!       23        swamp
!       24        desert
!       25        mixed wood forests
!       26        transitional forest
!-----------------------------------------------------------------------
!        KEY   VARIABLES
!-----------------------------------------------------------------------
!  ARGUMENTS:
!  z2       | Met reference height (m)
!  zl       | Z/L stability PARAMETER
!  z0_f     | Surface roughness (m)
!  ustar    | friction velocity (m/s)
!  t2       | Temperature at Z2 (K)
!  ts       | Surface temperature (K)
!  srad     | Solar irradiance (w/m2)
!  rh       | relative humidity (0.0-1.0)
!  fcld     | Cloud fraction (0-1)
!  prec     | hourly precipitation (mm/hour)
!  sd       | Snow depth        (cm)
!  coszen   | Cosine of solar zenith angle
!  mlu      | Land USE TYPE
!  rm       | mesophyll resistance (s/m)
!  alpha    | Scaling facTOr based on SO2 (no unit)
!  beta     | Scaling facTOr based on O3  (no unit)
!  di       | Gas diffusivity (cm2/s)
!  vdg      | gaseous dry deposition velocity (m/s)
!  lai_f    | Leaf area index
!  rscalsp  | Scaling facTOr for highly soluble species
!  henry    | Henry's Law solubility
!  io3      | Ozone flag

!  LOCAL VARIABLES:
!  brs      | Constant for stomatal resistance(W/m2)
!  bvpd     | Constant for water vapor pressure deficit  (kPa^-1)
!  fsun     | fraction of sunlit leaves (0.0-LAI)
!  pardir   | visible beam radiation (W/m2)
!  pardif   | diffuse visible radiation (W/m2)
!  psi1     | Constant for leaf water potential(Mpa)
!  psi2     | Constant for leaf water potential(Mpa)
!  fsnow    | Snow fraction  (0-1)
!  ra       | Aerodynamic resistance (s/m)
!  racz     | IN-canopy aerodynamic resistance (s/m)
!  rb       | quasi-laminar resistance (s/m)
!  rc       | TOtal surface resistance (s/m)
!  rcut     | cuticle resistance (s/m )
!  rcutdo   | Dry cuticle resistance for O3 (s/m)
!  rcutds   | Dry cuticle resistance for SO2 (s/m)
!  rcutwo   | Wet cuticle resistance for O3 (s/m)
!  rg       | Ground  resistance (s/m )
!  rgo      | Ground  resistance for O3 (s/m)
!  rgs      | Ground  resistance for SO2 (s/m)
!  rsmin    | minimum stomatal resistance (s/m)
!  rst      | stomatal resistance (s/m)
!  sdmax    | Maximum snow depth over which snow
!           |   fraction for leaves is 1 (cm )
!  tmin     | Minimum temperature for stomatal opening (C)
!  tmax     | Maxmum temperature for stomatal opening (C)
!  TOpt     | Optimum temperature for stomatal opening (C)
!  wst      | fraction of stomatal closure under wet conditions (0.0-0.5)
!  vdg      | gas dry deposition velocity (m/s)

!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, PARAMETER :: NLUZ03   = 26

!-----Arguments

INTEGER mlu,io3
DOUBLE PRECISION z2
DOUBLE PRECISION zl
DOUBLE PRECISION z0_f
DOUBLE PRECISION ustar
DOUBLE PRECISION sd
DOUBLE PRECISION t2
DOUBLE PRECISION ts
DOUBLE PRECISION srad
DOUBLE PRECISION rh
DOUBLE PRECISION fcld
DOUBLE PRECISION prec
DOUBLE PRECISION coszen
DOUBLE PRECISION rm
DOUBLE PRECISION alpha
DOUBLE PRECISION beta
DOUBLE PRECISION di
DOUBLE PRECISION rscalsp
DOUBLE PRECISION lai_f
DOUBLE PRECISION henry
DOUBLE PRECISION vdg

!-----Local variables for gaseous Vd submoudle

INTEGER i
DOUBLE PRECISION es,temp,dair,dh2o,ra,rst,rdu,rdv,ww,rdm, &
          rdn,rv,rn,ratio,sv,fv,pardir,pardif
DOUBLE PRECISION pshad,psun,rshad,rsun,gshad,gsun,fsun,fshd,gspar,t,bt,gt, &
          d0,gd,psi,gw,coedew,dq,usmin,wst,racz,rgo_f,rgs_f
DOUBLE PRECISION rcuTO_f,rcuts_f,fsnow,rsnows,vi,rb,dvh2o,rs,rcut,rg,rc
DOUBLE PRECISION tmax(NLUZ03),tmin(NLUZ03),TOpt(NLUZ03),rsmin(NLUZ03), &
          brs(NLUZ03),bvpd(NLUZ03),psi1(NLUZ03),psi2(NLUZ03), &
          rac1(NLUZ03),rac2(NLUZ03),rgo(NLUZ03),rgs(NLUZ03), &
          rcutdo(NLUZ03),rcutwo(NLUZ03),rcutds(NLUZ03),sdmax(NLUZ03)
LOGICAL is_rain,is_dew

!   In-canopy aerodynamic resistance  [s/m].
!   Rac1 and Rac2 are minimum and maximum Rac0 for each LUC.

DATA   Rac1            / &
       0   ,  0   ,  0   ,  100 ,  250 , &
       60  ,  100 ,  300 ,  100 ,  60  , &
       20  ,  40  ,  20  ,  10  ,  10  , &
       10  ,  10  ,  10  ,  10  ,  20  , &
       40  ,  0   ,  20  ,  0   ,  100 , &
       100    /
DATA   Rac2            / &
       0   ,  0   ,  0   ,  100 ,  250 , &
       100 ,  250 ,  300 ,  100 ,  60  , &
       60  ,  40  ,  20  ,  40  ,  40  , &
       40  ,  40  ,  50  ,  40  ,  20  , &
       40  ,  0   ,  20  ,  0   ,  100 , &
       100    /

!   Dry and wet cuticle resistance for O3  [s/m].

DATA   RcutdO          / &
      -999 , -999 , -999 , 4000 , 6000 , &
      4000 , 6000 , 6000 , 8000 , 6000 , &
      5000 , 5000 , 4000 , 4000 , 4000 , &
      4000 , 4000 , 5000 , 5000 , 4000 , &
      6000 , 8000 , 5000 , -999 , 4000 , &
      4000    /
DATA   RcutwO          / &
      -999 , -999 , -999 ,  200 ,  400 , &
       200 ,  400 ,  400 ,  400 ,  400 , &
       300 ,  300 ,  200 ,  200 ,  200 , &
       200 ,  200 ,  300 ,  300 ,  200 , &
       400 ,  400 ,  300 , -999 ,  200 , &
       200    /

!   Ground resistance for O3  [s/m].

DATA   RgO            / &
      2000 , 2000 , 2000 ,  200 ,  200 , &
       200 ,  200 ,  200 ,  200 ,  200 , &
       200 ,  200 ,  200 ,  200 ,  200 , &
       200 ,  200 ,  200 ,  200 ,  500 , &
       500 ,  500 ,  500 ,  500 ,  200 , &
       200    /

!   Dry cuticle resistance for SO2  [s/m].

DATA   RcutdS          / &
      -999 , -999 , -999 , 2000 , 2500 , &
      2000 , 2500 , 2500 , 6000 , 2000 , &
      2000 , 2000 , 1000 , 1000 , 1500 , &
      1500 , 2000 , 2000 , 2000 , 2000 , &
      4000 , 2000 , 1500 , -999 , 2500 , &
      2500    /

!   Ground resistance for SO2  [s/m].

DATA   RgS            / &
        20 ,   70 ,  20  ,  200 ,  100 , &
       200 ,  200 ,  100 ,  300 ,  200 , &
       200 ,  200 ,  200 ,  200 ,  200 , &
        50 ,  200 ,  200 ,  200 ,   50 , &
       300 ,  300 ,   50 ,  700 ,  200 , &
       200    /

!   stomatal resistance related PARAMETERs.
!   In sequence: rsmin, brs, tmin, tmax, TOpt, bvpd, psi1, psi2

DATA   rsmin            / &
      -999 , -999 , -999 ,  250 ,  150 , &
       250 ,  150 ,  150 ,  250 ,  150 , &
       150 ,  250 ,  150 ,  100 ,  120 , &
       120 ,  120 ,  250 ,  125 ,  150 , &
       200 ,  150 ,  150 , -999 ,  150 , &
       150   /
DATA   brs              / &
      -999 , -999 , -999 ,   44 ,   40 , &
        44 ,   43 ,   40 ,   44 ,   40 , &
        44 ,   44 ,   50 ,   20 ,   40 , &
        40 ,   50 ,   65 ,   65 ,   40 , &
        42 ,   25 ,   40 , -999 ,   44 , &
        43   /
DATA   tmin             / &
      -999 , -999 , -999 ,   -5 ,    0 , &
        -5 ,    0 ,    0 ,    0 ,    0 , &
        -5 ,    0 ,    5 ,    5 ,    5 , &
         5 ,    5 ,    5 ,   10 ,    5 , &
         0 ,   -5 ,    0 , -999 ,   -3 , &
         0   /
DATA   tmax             / &
      -999 , -999 , -999 ,   40 ,   45 , &
        40 ,   45 ,   45 ,   45 ,   45 , &
        40 ,   45 ,   40 ,   45 ,   45 , &
        45 ,   45 ,   45 ,   45 ,   45 , &
        45 ,   40 ,   45 , -999 ,   42 , &
        45   /
DATA   TOpt             / &
      -999 , -999 , -999 ,   15 ,   30 , &
        15 ,   27 ,   30 ,   25 ,   30 , &
        15 ,   25 ,   30 ,   25 ,   27 , &
        27 ,   25 ,   25 ,   30 ,   25 , &
        22 ,   20 ,   20 , -999 ,   21 , &
        25   /
DATA   bvpd             / &
      -999 , -999 , -999 ,  0.31,  0.27, &
       0.31,  0.36,  0.27,  0.31,  0.27, &
       0.27,  0.27,  0.0 ,  0.0 ,  0.0 , &
       0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 , &
       0.31,  0.24,  0.27, -999 ,  0.34, &
       0.31   /
DATA   psi1            / &
      -999 , -999 , -999 , -2.0 , -1.0 , &
      -2.0 , -1.9 , -1.0 , -1.0 , -2.0 , &
      -2.0 , -2.0 , -1.5 , -1.5 , -1.5 , &
      -1.5 , -1.5 , -1.5 , -1.5 , -1.5 , &
      -1.5 ,    0 , -1.5 , -999 , -2.0 , &
      -2.0    /
DATA   psi2            / &
      -999 , -999 , -999 , -2.5 , -5.0 , &
      -2.5 , -2.5 , -5.0 , -4.0 , -4.0 , &
      -4.0 , -3.5 , -2.5 , -2.5 , -2.5 , &
      -2.5 , -2.5 , -2.5 , -2.5 , -2.5 , &
      -3.0 , -1.5 , -2.5 , -999 , -2.5 , &
      -3.0    /

!     Maximum snow depth over which snow fraction for leaves is 1.0
!     Snow fraction for ground is treated 2 times of that for leaves

DATA SDMAX  / &
                   9999. ,  10.  , 9999. ,  200. ,  200. , &
                    200. , 200.  ,  200. ,  200. ,   30. , &
                     20. ,  30.  ,   10. ,   20. ,   10. , &
                     10. ,  10.  ,   10. ,   10. ,   10. , &
                    200. ,  10.  ,   10. ,   10. ,  200. , &
                    200.        /

! --- Define the function for saturation vapor pressure (mb)

ES(TEMP) = 6.108*EXP(17.27*(TEMP - 273.16)/(TEMP - 35.86))

!   Some constants

dair=0.369*29.+6.29
dh2o=0.369*18.+6.29

VDG = 0.0
I = MLU

!     Aerodynamic resistance above canopy

IF(ZL>=0.) THEN
    Ra=(.74*DLOG(Z2/Z0_F)+4.7*ZL)/0.4/USTAR
ELSE
    Ra=0.74/0.4/USTAR*(DLOG(Z2/Z0_F)- &
               2*DLOG((1+SQRT(1-9.*ZL))*0.5))
ENDIF
Ra=MAX(Ra,DBLE(5.0))
IF (I==1.OR.I==3) THEN
  Ra=MIN(Ra,DBLE(2000.))
ELSE
  Ra=MIN(Ra,DBLE(1000.))
END IF

! --- STOMATAL RESISTANCE FOR WATER VAPOR ONLY. STEPS FOR CALCULATING:
!     1. Calculate direct and diffuse PAR from solar radiation
!     2. Calculate sunlit and shaded leaf area, PAR for sunlit and shaded leafs
!     3. Calculate stomatal conductance
!     4. Calculate stomatal resistance for water vapor

! --- Set a big value for stomatal resistance when sTOmata are CLOSEd
RST=99999.9

! --  Only calculate stomatal resistance IF there is solar radiation,
!     leaf area index is not zero, and within reasonable temperature range

IF ( SRAD>=0.1             .AND. &
          TS<(Tmax(I)+273.15)  .AND. &
          TS>(Tmin(I)+273.15)  .AND. &
          LAI_F>0.001           .AND. &
          COSZEN>0.001               ) THEN

! --  Calculate direct and diffuse PAR from solar radiation and solar zenith angle

  RDU=600.*EXP(-0.185/COSZEN)*COSZEN
  RDV=0.4*(600.-RDU)*COSZEN
  WW=-DLOG(COSZEN)/2.302585
  WW=-1.195+0.4459*WW-0.0345*WW**2
  WW=1320*10**WW
  RDM=(720.*EXP(-0.06/COSZEN)-WW)*COSZEN
  RDN=0.6*(720-RDM-WW)*COSZEN
  RV=MAX(DBLE(0.1),RDU+RDV)
  RN=MAX(DBLE(0.01),RDM+RDN)
  RATIO=MIN(DBLE(0.9),SRAD/(RV+RN))
  SV=RATIO*RV                            ! TOtal PAR
  FV=MIN(DBLE(0.99), (0.9-RATIO)/0.7)
  FV=MAX(DBLE(0.01),RDU/RV*(1.0-FV**0.6667))  !fraction of PAR in the direct beam
  PARDIR=FV*SV                           ! PAR from direct radiation
  PARDIF=SV-PARDIR                       ! PAR from diffuse radiation

! -- Calculate sunlit and shaded leaf area, PAR for sunlit and shaded leaves

  IF (LAI_F>2.5 .AND. SRAD>200.) THEN
    PSHAD=PARDIF*EXP(-0.5*LAI_F**0.8) &
              +0.07*PARDIR*(1.1-0.1*LAI_F)*EXP(-COSZEN)
    PSUN=PARDIR**0.8*.5/COSZEN+PSHAD
  ELSE
    PSHAD=PARDIF*EXP(-0.5*LAI_F**0.7) &
              +0.07*PARDIR*(1.1-0.1*LAI_F)*EXP(-COSZEN)
    PSUN=PARDIR*.5/COSZEN+PSHAD
  END IF
  RSHAD=RSmin(I)+BRS(I)*RSMIN(I)/PSHAD
  RSUN=RSmin(I)+BRS(I)*RSMIN(I)/PSUN
  GSHAD=1./RSHAD
  GSUN=1./RSUN
  FSUN=2*COSZEN*(1.-EXP(-0.5*LAI_F/COSZEN))  ! Sunlit leaf area
  FSHD=LAI_F-FSUN                    ! Shaded leaf area

! -- stomatal conductance before including effects of temperature,
!                   vapor pressure defict and water stress.

  GSPAR=FSUN*GSUN+FSHD*GSHAD

! --  FUNCTION for temperature effect
  T=TS-273.15
  BT=(Tmax(I)-TOPT(I))/(TMAX(I)-Tmin(I))
  GT=(Tmax(I)-T)/(TMAX(I)-TOPT(I))
  GT=GT**BT
  GT=GT*(T-Tmin(I))/(TOPT(I)-TMIN(I))
! --  FUNCTION for vapor pressure deficit
  D0= ES(TS)*(1.- RH)/10.           !kPa
  GD=1.-BVPD(I)*D0
! --  FUNCTION for water stress
  PSI=(-0.72-0.0013*SRAD)
!       PSI_S=(-0.395-0.043*(TS-273.15))*102.
  GW=(PSI-PSI2(I))/(PSI1(I)-PSI2(I))
  IF (GW>1.0) GW=1.0
  IF (GW<0.1) GW=0.1
  IF (GD>1.0) GD=1.0
  IF (GD<0.1) GD=0.1
! --  stomatal resistance for water vapor
  RST=1.0/(GSPAR*GT*GD*GW)

END IF

!   Decide IF dew or rain occurs.

IF (FCLD<0.25) THEN
  Coedew=0.3
ELSE  IF (FCLD>=0.25 .AND. FCLD<0.75) THEN
   Coedew=0.2
ELSE
  Coedew=0.1
END IF
DQ=0.622/1000. * ES(TS)*(1.- RH)*1000.    ! unit g/kg
DQ=MAX(DBLE(0.0001),DQ)
USMIN=1.5/DQ*Coedew

is_rain = .FALSE.
is_dew = .FALSE.
IF (TS>273.15 .AND. PREC>0.20) THEN
  is_rain = .TRUE.
ELSE IF (TS>273.15 .AND. USTAR<USMIN)THEN
  is_dew = .TRUE.
ELSE
  is_rain = .FALSE.
  is_dew = .FALSE.
END IF

!   Decide fraction of stomatal BLOCKing due TO wet conditions

Wst=0.
IF ((is_dew.or.is_rain).and.SRAD>200.) THEN
  Wst=(SRAD-200.)/800.
  Wst=MIN(Wst, DBLE(0.5))
END IF

! -- In-canopy aerodynamic resistance

  Racz = Rac1(I)+(LAI_F-lai_ref(I,14))/(lai_ref(I,15) &
             -lai_ref(I,14)+1.D-10)*(Rac2(I)-Rac1(I))
  Racz = Racz*LAI_F**0.25/USTAR/USTAR

! -- Ground resistance for O3

IF (I>=4.AND.TS<272.15) THEN
  RgO_F = MIN( RgO(I)*2., RgO(I) * exp(0.2*(272.15-TS)))
ELSE
  RgO_F = RgO(I)
END IF

! -- Ground resistance for SO2

IF (I==2) THEN
RgS_F = MIN(RgS(I)*(275.15-TS), DBLE(500.))
RgS_F = MAX(RgS(I), DBLE(100.))
ELSE IF (I>=4.AND.is_rain) THEN
  RgS_F = 50.
ELSE IF (I>=4.AND.is_dew) THEN
  RgS_F = 100.
ELSE IF (I>=4.AND.TS<272.15) THEN
  RgS_F = MIN( RgS(I)*2., RgS(I) * exp(0.2*(272.15-TS)))
ELSE
  RgS_F =  RgS(I)
END IF

! -- Cuticle resistance for O3 AND SO2

IF (RcutdO(I)<=-1) THEN
RcutO_F = 1.E25
RcutS_F = 1.E25
ELSE IF (is_rain) THEN
RcutO_F = RcutwO(I)/LAI_F**0.5/USTAR
RcutS_F = 50./LAI_F**0.5/USTAR
RcutS_F = MAX(RcutS_F, DBLE(20.))
ELSE IF (is_dew) THEN
RcutO_F = RcutwO(I)/LAI_F**0.5/USTAR
RcutS_F = 100./LAI_F**0.5/USTAR
RcutS_F = MAX(RcutS_F, DBLE(20.))
ELSE IF (TS<272.15) THEN
RcutO_F = RcutdO(I)/exp(3.*RH)/LAI_F**0.25/USTAR
RcutS_F = RcutdS(I)/exp(3.*RH)/LAI_F**0.25/USTAR
  RcutO_F = MIN( RcutO_F*2., RcutO_F * exp(0.2*(272.15-TS)))
  RcutS_F = MIN( RcutS_F*2., RcutS_F * exp(0.2*(272.15-TS)))
RcutO_F = MAX(RcutO_F,DBLE(100.))
RcutS_F = MAX(RcutS_F,DBLE(100.))
ELSE
RcutO_F = RcutdO(I)/exp(3.*RH)/LAI_F**0.25/USTAR
RcutS_F = RcutdS(I)/exp(3.*RH)/LAI_F**0.25/USTAR
RcutO_F = MAX(RcutO_F,DBLE(100.))
RcutS_F = MAX(RcutS_F,DBLE(100.))
END IF

! IF snow occurs, Rg and Rcut are adjusted by snow cover fraction

fsnow= sd/sdmax(i)
fsnow= MIN(DBLE(1.0), fsnow)   !snow cover fraction for leaves
IF (fsnow>0.0001 .AND. I>=4) THEN
  RsnowS= MIN(70.*(275.15-TS), DBLE(500.))
  RsnowS= MAX(RSnowS, DBLE(100.))
  RcutS_F=1.0/((1.-fsnow)/RcutS_F+fsnow/RsnowS)
  RcutO_F=1.0/((1.-fsnow)/RcutO_F+fsnow/2000.)
  fsnow= MIN(DBLE(1.0), fsnow*2.)   !snow cover fraction for ground
  RgS_F=1.0/((1.-fsnow)/RgS_F+fsnow/RsnowS)
  RgO_F=1.0/((1.-fsnow)/RgO_F+fsnow/2000.)
END IF

! -- Calculate diffusivity for each gas species

VI=145.8*1.E-4*(TS*0.5+T2*0.5)**1.5/ &
                       (TS*0.5+T2*0.5+110.4)

! -- Calculate quasi-laminar resistance

Rb =5./USTAR*(VI/DI)**.666667

! -- Calculate stomatal resistance for each species from the ratio of
!       diffusity of water vapor TO the gas species

DVh2o=0.001*TS**1.75*SQRT((29.+18.)/29./18.)
DVh2o=DVh2o/(dair**0.3333+dh2o**0.3333)**2
RS=RST*DVh2o/DI+RM

! -- Scale cuticle and ground resistances for each species

Rcut = 1./(ALPHA/RcutS_F+BETA/RcutO_F)
Rg = 1./(ALPHA/RgS_F+BETA/RgO_F)

! -- Calculate TOtal surface resistance

Rc = (1.-Wst)/Rs+1./(Racz+Rg)+1./Rcut
Rc=MAX(DBLE(10.0),1./Rc)        !Set minimum surface resistance as 10 s/m
IF (io3==1 .and. (i==1 .or. i==3)) THEN
  Rc = 1./(1.d-4 + 5.0d-6*henry*ustar*(ts-273.15)**3.)
  Rc = MAX(Rc,DBLE(1500.))
ENDIF
Rc=Rc*rscalsp               !Scale for the extremely soluble gas

! -- Deposition velocity

VDG = 1./(RA+RB+RC)

RETURN
END

!=======================================================================

SUBROUTINE henryfnc(spname,hlaw0,tfact,temp,ph,hlaw)

!----Based on CAMx v6.30 160408

!     HENRYFNC calculates temperature and dissociation adjustments TO
!     baseline Henry's Law constants.

!     Copyright 1996 - 2016
!     Ramboll ENVIRON

!     ModIFications:
!        NONE

!     Input arguments:
!        spname              Species name
!        hlaw0               Baseline Henry's Law constant @298K (M/atm)
!        tfact               temperature facTOr
!        temp                ambient temperature (K)
!        ph                  pH of liquid

!     Output arguments:
!        hlaw                Adjusted Henry's Law constant (M/atm)

!     Routines CALLed:
!        NONE

!     CALLed by:
!        DRYDEP

IMPLICIT NONE

REAL          :: hlaw0,tfact,temp,ph,hlaw
CHARACTER(*)  :: spname

REAL diss1,diss2

hlaw = hlaw0*exp(tfact*(1./298. - 1./temp))
IF (TRIM(spname) == 'NH3') THEN
  diss1 = 10.**(-189.1/temp - 4.117)
  diss2 = 10.**(-5839.5/temp - 9.7618*alog(temp) + 61.206)
  hlaw = hlaw*(1. + (diss1/diss2)*10.**(-ph))
ELSEIF (TRIM(spname) == 'HNO3') THEN
  diss1 = 15.4
  hlaw = hlaw*(1. + diss1/(10.**(-ph)))
ELSEIF (TRIM(spname) == 'SO2') THEN
  diss1 = 10.**(853./temp)/54950.
  diss2 = 10.**(621.9/temp)/1.897e+9
  hlaw = hlaw*(1. + diss1/(10.**(-ph)) + &
                         diss1*diss2/(10.**(-2.*ph)))
ENDIF

RETURN
END
