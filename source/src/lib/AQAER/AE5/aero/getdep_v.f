cccccccccccccccccccccccccccccccccccccccccccccccccc
C *** subroutine for deposition velocities follows
C Adapted from CMAQ4.7.1
cccccccccccccccccccccccccccccccccccccccccccccccccc

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GETDEP_V ( NUMCELLS, N_AE_DEP_SPC,
     &                      BLKTA, BLKDENS,
     &                      XLM, AMU,
     &                      BLKWSTAR, BLKUSTAR, BLKRA,
     &                      DGATK, DGACC, DGCOR,
     &                      XXLSGAT, XXLSGAC, XXLSGCO,
     &                      PDENSAT, PDENSAC, PDENSCO,
     &                      VDEP )

C *** Calculate deposition velocity for Aitken, accumulation, and
C     coarse modes.
C     Reference:
C     Binkowski F. S., and U. Shankar, The regional particulate
C     model 1. Model description and preliminary results.
C     J. Geophys. Res., 100, D12, 26191-26209, 1995.
C
C    May 05 D.Schwede: added impaction term to coarse mode dry deposition
C 25 May 05 J.Pleim:  Updated dry dep velocity calculation for aerosols
C                     to Venkatram and Pleim (1999)
C 20 Jul 05 J.Pleim:  Changed impaction term using modal integration of
C                     Stokes**2 / 400 (Giorgi, 1986, JGR)
C 14 Apr 08 J.Kelly:  Added code to calculate deposition velocity of
C                     coarse surface area and to account for variable
C                     standard deviation of the coarse mode.
C 08 Sep 08 P.Bhave:  Backward compatibility with AE4 mechanisms
C                     standardized names of all coarse-mode variables

      use aero_consts_inc, only: VDNATK, VDNACC, VDNCOR, VDMATK, VDMACC,
     &                           VDMCOR, VDSATK, VDSACC, VDSCOR

      IMPLICIT NONE

C *** input argumets

      INTEGER NUMCELLS
      INTEGER N_AE_DEP_SPC

C     meteorological information in blocked arays:

      REAL BLKTA  ( NUMCELLS )  ! air temperature [ K ]
      REAL BLKDENS( NUMCELLS )  ! air density  [ kg/m**3 ]

C     atmospheric properties

      REAL XLM( NUMCELLS )      ! atmospheric mean free path [ m ]
      REAL AMU( NUMCELLS )      ! atmospheric dynamic viscosity [ kg/(m s) ]

C     Planetary boundary laryer (PBL) variables:

      REAL BLKWSTAR( NUMCELLS ) ! convective velocity scale [ m/s ]
      REAL BLKUSTAR( NUMCELLS ) ! friction velocity [ m/s ]
      REAL BLKRA   ( NUMCELLS ) ! aerodynamic resistance [ s/m ]

C     aerosol properties:

C     modal diameters: [ m ]

      REAL DGATK( NUMCELLS )    ! nuclei mode geometric mean diameter  [ m ]
      REAL DGACC( NUMCELLS )    ! accumulation geometric mean diameter [ m ]
      REAL DGCOR( NUMCELLS )    ! coarse mode geometric mean diameter  [ m ]

C     log of modal geometric standard deviations

      REAL XXLSGAT( NUMCELLS )  ! Aitken mode
      REAL XXLSGAC( NUMCELLS )  ! accumulation mode
      REAL XXLSGCO( NUMCELLS )  ! coarse mode

C     average modal particle densities  [ kg/m**3 ]

      REAL PDENSAT( NUMCELLS )  ! average particle density in nuclei mode
      REAL PDENSAC( NUMCELLS )  ! average particle density in accumulation mode
      REAL PDENSCO( NUMCELLS )  ! average particle density in coarse mode

C *** output argument

      REAL VDEP( NUMCELLS,N_AE_DEP_SPC ) ! deposition  velocity [ m/s ]

C *** array indices hardcoded to match SUBROUTINE AERO_DEPV
! in SCICHEM, get this from aero_consts_inc
!      INTEGER, PARAMETER :: VDNATK = 1  ! Aitken mode number
!      INTEGER, PARAMETER :: VDNACC = 2  ! accumulation mode number
!      INTEGER, PARAMETER :: VDNCOR = 3  ! coarse mode number
!      INTEGER, PARAMETER :: VDMATK = 4  ! Aitken mode mass
!      INTEGER, PARAMETER :: VDMACC = 5  ! accumulation mode mass
!      INTEGER, PARAMETER :: VDMCOR = 6  ! coarse mode mass
!      INTEGER, PARAMETER :: VDSATK = 7  ! Aitken mode surface area
!      INTEGER, PARAMETER :: VDSACC = 8  ! accumulation mode surface area
!      INTEGER, PARAMETER :: VDSCOR = 9  ! coarse mode surface area

C modal Knudsen numbers

      REAL KNATK   ! Aitken mode Knudsen number
      REAL KNACC   ! accumulation "
      REAL KNCOR   ! coarse mode

C modal particle diffusivities for number, 2nd, and 3rd moment, or mass:

      REAL DCHAT0N, DCHAT0A, DCHAT0C
      REAL DCHAT2N, DCHAT2A, DCHAT2C
      REAL DCHAT3N, DCHAT3A, DCHAT3C

C modal sedimentation velocities for number, 2nd, and 3rd moment, or mass:

      REAL VGHAT0N, VGHAT0A, VGHAT0C
      REAL VGHAT2N, VGHAT2A, VGHAT2C
      REAL VGHAT3N, VGHAT3A, VGHAT3C

      INTEGER NCELL

      REAL DCONST1, DCONST1N, DCONST1A, DCONST1C
      REAL DCONST2, DCONST3N, DCONST3A, DCONST3C
      REAL SC0N, SC0A, SC0C     ! Schmidt numbers for number
      REAL SC2N, SC2A, SC2C     ! Schmidt numbers for 2ND MOMENT
      REAL SC3N, SC3A, SC3C     ! Schmidt numbers for 3rd moment
      REAL STOKEN, STOKEA, STOKEC ! Stokes numbers for each mode
      REAL RD0N, RD0A, RD0C     ! canopy resistance for number
      REAL RD2N, RD2A, RD2C     ! canopy resistance for 2nd moment
      REAL RD3N, RD3A, RD3C     ! canopy resisteance for 3rd moment
      REAL UTSCALE              ! scratch function of USTAR and WSTAR
      REAL NU                   ! kinematic viscosity [ m**2 s**-1 ]
      REAL USTFAC               ! scratch function of USTAR, NU, and GRAV

      REAL, PARAMETER :: BHAT = 1.246 ! Constant from Cunningham slip correction

      REAL, PARAMETER :: PI      = 3.141593 ! single precision

      REAL, PARAMETER :: PI6     = PI / 6.0

      REAL, PARAMETER :: THREEPI = 3.0 * PI

      REAL, PARAMETER :: ONE3    = 1.0 / 3.0

      REAL, PARAMETER :: TWO3    = 2.0 / 3.0

      REAL, PARAMETER :: AVO = 6.0221367 E23 ! Avogadro's Constant [ 1/mol ]

      REAL, PARAMETER :: RGASUNIV = 8.314510 ! universal gas const [ J/mol-K ]

      REAL, PARAMETER :: BOLTZ = RGASUNIV / AVO ! Boltzmann's Constant [ J/K ]

      REAL, PARAMETER :: GRAV = 9.80622 ! mean gravitational accel [ m/sec**2 ]
                                        ! FSB NOTE: Value is now mean of polar
                                        ! and equatorial values. Source: CRC
                                        ! Handbook (76th Ed) page 14-6.

      REAL, PARAMETER :: VDNMAX = 0.01  ! Max vd for nucleation= 1 cm/s
      REAL, PARAMETER :: VDAMAX = 0.01  ! Max vd for accumulation = 1 cm/s
      REAL, PARAMETER :: VDCMAX = 0.04  ! Max vd for coarse = 4 cm/s

C Scalar variables for  VARIABLE standard deviations.

      REAL    L2SGAT, L2SGAC    ! see usage
      REAL    L2SGCO

      REAL    EAT1             ! Aitken mode exp( log^2( sigmag )/8 )
      REAL    EAC1             ! accumulation mode exp( log^2( sigmag )/8 )
      REAL    ECO1             ! coarse mode exp( log^2( sigmag )/8 )

      REAL    ESAT04           ! Aitken       " **4
      REAL    ESAC04           ! accumulation "
      REAL    ESCO04           ! coarse       "

      REAL    ESAT08           ! Aitken       " **8
      REAL    ESAC08           ! accumulation "
      REAL    ESCO08           ! coarse       "

      REAL    ESAT12           ! Aitken       " **12
      REAL    ESAC12           ! accumulation "
      REAL    ESCO12           ! coarse       "

      REAL    ESAT16           ! Aitken       " **16
      REAL    ESAC16           ! accumulation "
      REAL    ESCO16           ! coarse       "

      REAL    ESAT20           ! Aitken       " **20
      REAL    ESAC20           ! accumulation "
      REAL    ESCO20           ! coarse       "

      REAL    ESAT28           ! Aitken       " **28
      REAL    ESAC28           ! accumulation "
      REAL    ESCO28           ! coarse       "

      REAL    ESAT32           ! Aitken       " **32
      REAL    ESAC32           ! accumulation "
      REAL    ESCO32           ! coarse       "

      REAL    ESAT36           ! Aitken       " **36
      REAL    ESAC36           ! accumulation "
      REAL    ESCO36           ! coarse       "

      REAL    ESAT48           ! Aitken       " **48
      REAL    ESAC48           ! accumulation "
      REAL    ESCO48           ! coarse       "

      REAL    ESAT64           ! Aitken       " **64
      REAL    ESAC64           ! accumulation "
      REAL    ESCO64           ! coarse       "

      REAL    ESAT128          ! Aitken       " **128
      REAL    ESAC128          ! accumulation "
      REAL    ESCO128          ! coarse       "

      REAL    ESAT160          ! Aitken       " **160
      REAL    ESAC160          ! accumulation "
      REAL    ESCO160          ! coarse       "

      REAL    ESATM12          ! Aitken       " **(-12)
      REAL    ESACM12          ! accumulation "
      REAL    ESCOM12          ! coarse       "

      REAL    ESATM16          ! Aitken       " **(-16)
      REAL    ESACM16          ! accumulation "
      REAL    ESCOM16          ! coarse       "

      REAL    ESATM20          ! Aitken       " **(-20)
      REAL    ESACM20          ! accumulation "
      REAL    ESCOM20          ! coarse       "

      REAL    ESATM32          ! Aitken       " **(-32)
      REAL    ESACM32          ! accumulation "
      REAL    ESCOM32          ! coarse       "

      REAL    EIM              ! Impaction efficiency

C----------------------------------------------------------------------

      DO NCELL = 1, NUMCELLS ! Calculate Knudsen numbers

         KNATK = 2.0 * XLM( NCELL ) / DGATK( NCELL )
         KNACC = 2.0 * XLM( NCELL ) / DGACC( NCELL )
         KNCOR = 2.0 * XLM( NCELL ) / DGCOR( NCELL )

C *** Calculate functions of variable standard deviation.

         L2SGAT = XXLSGAT(NCELL) * XXLSGAT(NCELL)
         L2SGAC = XXLSGAC(NCELL) * XXLSGAC(NCELL)
         L2SGCO = XXLSGCO(NCELL) * XXLSGCO(NCELL)

         EAT1   = EXP( 0.125 * L2SGAT )
         EAC1   = EXP( 0.125 * L2SGAC )
         ECO1   = EXP( 0.125 * L2SGCO )

         ESAT04  = EAT1 ** 4
         ESAC04  = EAC1 ** 4
         ESCO04  = ECO1 ** 4

         ESAT08  = ESAT04 * ESAT04
         ESAC08  = ESAC04 * ESAC04
         ESCO08  = ESCO04 * ESCO04

         ESAT12  = ESAT04 * ESAT08
         ESAC12  = ESAC04 * ESAC08
         ESCO12  = ESCO04 * ESCO08

         ESAT16  = ESAT08 * ESAT08
         ESAC16  = ESAC08 * ESAC08
         ESCO16  = ESCO08 * ESCO08

         ESAT20  = ESAT16 * ESAT04
         ESAC20  = ESAC16 * ESAC04
         ESCO20  = ESCO16 * ESCO04

         ESAT28  = ESAT20 * ESAT08
         ESAC28  = ESAC20 * ESAC08
         ESCO28  = ESCO20 * ESCO08

         ESAT32  = ESAT16 * ESAT16
         ESAC32  = ESAC16 * ESAC16
         ESCO32  = ESCO16 * ESCO16

         ESAT36  = ESAT16 * ESAT20
         ESAC36  = ESAC16 * ESAC20
         ESCO36  = ESCO16 * ESCO20

         ESAT48  = ESAT36 * ESAT12
         ESAC48  = ESAC36 * ESAC12
         ESCO48  = ESCO36 * ESCO12

         ESAT64  = ESAT32 * ESAT32
         ESAC64  = ESAC32 * ESAC32
         ESCO64  = ESCO32 * ESCO32

         ESAT128 = ESAT64 * ESAT64
         ESAC128 = ESAC64 * ESAC64
         ESCO128 = ESCO64 * ESCO64

         ESAT160 = ESAT128* ESAT32
         ESAC160 = ESAC128* ESAC32
         ESCO160 = ESCO128* ESCO32

C *** calculate inverses:

         ESATM12 = 1.0 / ESAT12
         ESACM12 = 1.0 / ESAC12
         ESCOM12 = 1.0 / ESCO12

         ESATM16 = 1.0 / ESAT16
         ESACM16 = 1.0 / ESAC16
         ESCOM16 = 1.0 / ESCO16

         ESATM20 = 1.0 / ESAT20
         ESACM20 = 1.0 / ESAC20
         ESCOM20 = 1.0 / ESCO20

         ESATM32 = 1.0 / ESAT32
         ESACM32 = 1.0 / ESAC32
         ESCOM32 = 1.0 / ESCO32

         DCONST1 = BOLTZ * BLKTA( NCELL ) /
     &              ( THREEPI * AMU( NCELL ) )
         DCONST1N = DCONST1 / DGATK( NCELL )
         DCONST1A = DCONST1 / DGACC( NCELL )
         DCONST1C = DCONST1 / DGCOR( NCELL )
         DCONST2  = GRAV / ( 18.0 * AMU( NCELL ) )
         DCONST3N = DCONST2 * PDENSAT( NCELL ) * DGATK( NCELL ) **2
         DCONST3A = DCONST2 * PDENSAC( NCELL ) * DGACC( NCELL ) **2
         DCONST3C = DCONST2 * PDENSCO( NCELL ) * DGCOR( NCELL ) **2

C i-mode

         DCHAT0N  = DCONST1N
     &                    * ( ESAT04  + BHAT * KNATK * ESAT16 )
         DCHAT2N  = DCONST1N
     &                    * ( ESATM12  + BHAT * KNATK * ESATM16 )
         DCHAT3N  = DCONST1N
     &                    * ( ESATM20 + BHAT * KNATK * ESATM32 )
         VGHAT0N  = DCONST3N
     &                    * ( ESAT16  + BHAT * KNATK * ESAT04 )
         VGHAT2N  = DCONST3N
     &                    * ( ESAT48  + BHAT * KNATK * ESAT20 )
         VGHAT3N  = DCONST3N
     &                    * ( ESAT64  + BHAT * KNATK * ESAT28 )

C j-mode

         DCHAT0A  = DCONST1A
     &                    * ( ESAC04  + BHAT * KNACC * ESAC16 )
         DCHAT2A  = DCONST1A
     &                    * ( ESACM12 + BHAT * KNACC * ESACM16 )
         DCHAT3A  = DCONST1A
     &                    * ( ESACM20 + BHAT * KNACC * ESACM32 )
         VGHAT0A  = DCONST3A
     &                    * ( ESAC16  + BHAT * KNACC * ESAC04 )
         VGHAT2A  = DCONST3A
     &                    * ( ESAC48  + BHAT * KNACC * ESAC20 )
         VGHAT3A  = DCONST3A
     &                    * ( ESAC64  + BHAT * KNACC * ESAC28 )

C coarse mode

         DCHAT0C  = DCONST1C
     &                    * ( ESCO04  + BHAT * KNCOR * ESCO16 )
         DCHAT2C  = DCONST1C
     &                    * ( ESCOM12 + BHAT * KNCOR * ESCOM16 )
         DCHAT3C  = DCONST1C
     &                    * ( ESCOM20 + BHAT * KNCOR * ESCOM32 )
         VGHAT0C  = DCONST3C
     &                    * ( ESCO16  + BHAT * KNCOR * ESCO04 )
         VGHAT2C  = DCONST3C
     &                    * ( ESCO48  + BHAT * KNCOR * ESCO20 )
         VGHAT3C  = DCONST3C
     &                    * ( ESCO64  + BHAT * KNCOR * ESCO28 )

C now calculate the deposition velocities

         NU = AMU( NCELL ) / BLKDENS( NCELL )
         USTFAC = BLKUSTAR( NCELL ) * BLKUSTAR( NCELL ) / ( GRAV * NU )
         STOKEN = DCONST3N * USTFAC
         STOKEA = DCONST3A * USTFAC
         STOKEC = DCONST3C * USTFAC
         UTSCALE = BLKUSTAR( NCELL )
     &           + 0.24 * BLKWSTAR( NCELL ) * BLKWSTAR( NCELL )
     &           /        BLKUSTAR( NCELL )

C first do 0th moment for the deposition of number

C  Aitken mode

         SC0N = NU / DCHAT0N
         EIM = STOKEN**2 / 400.0 * ESAT64
         EIM = MIN( EIM, 1.0 )
         RD0N = 1.0 / ( UTSCALE *
     &                ( SC0N ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDNATK ) = VGHAT0N
     &           / ( 1.0 - EXP(-VGHAT0N * ( BLKRA( NCELL ) + RD0N ) ) )

C accumulation mode

         SC0A = NU / DCHAT0A
         EIM = STOKEA**2 / 400.0 * ESAC64
         EIM = MIN( EIM, 1.0 )
         RD0A = 1.0 / ( UTSCALE *
     &                ( SC0A ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDNACC ) = VGHAT0A
     &           / ( 1.0 - EXP(-VGHAT0A * ( BLKRA( NCELL ) + RD0A ) ) )

C coarse mode

         SC0C = NU / DCHAT0C
         EIM = STOKEC**2 / 400.0 * ESCO64
         EIM = MIN( EIM, 1.0 )
         RD0C = 1.0 / ( UTSCALE *
     &                ( SC0C ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDNCOR ) = VGHAT0C
     &           / ( 1.0 - EXP(-VGHAT0C * ( BLKRA( NCELL ) + RD0C ) ) )

C now do 2nd moment for the deposition of surface area

C  Aitken mode

         SC2N = NU / DCHAT2N
         EIM = STOKEN**2 / 400.0 * ESAT128
         EIM = MIN( EIM, 1.0 )
         RD2N = 1.0 / ( UTSCALE *
     &                ( SC2N ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDSATK ) = VGHAT2N
     &           / ( 1.0 - EXP(-VGHAT2N * ( BLKRA( NCELL ) + RD2N ) ) )

C accumulation mode

         SC2A = NU / DCHAT2A
         EIM = STOKEA**2 / 400.0 * ESAC128
         EIM = MIN( EIM, 1.0 )
         RD2A = 1.0 / ( UTSCALE *
     &                ( SC2A ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDSACC ) = VGHAT2A
     &           / ( 1.0 - EXP(-VGHAT2A * ( BLKRA( NCELL ) + RD2A ) ) )

C coarse mode

         SC2C = NU / DCHAT2C
         EIM = STOKEC**2 / 400.0 * ESCO128
         EIM = MIN( EIM, 1.0 )
         RD2C = 1.0 / ( UTSCALE *
     &                ( SC2C ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDSCOR ) = VGHAT2C
     &           / ( 1.0 - EXP(-VGHAT2C * ( BLKRA( NCELL ) + RD2C ) ) )

C now do 3rd moment for the deposition of mass

C  Aitken mode

         SC3N = NU / DCHAT3N
         EIM = STOKEN**2 / 400.0 * ESAT160
         EIM = MIN( EIM, 1.0 )
         RD3N = 1.0 / ( UTSCALE *
     &                ( SC3N ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDMATK ) = VGHAT3N
     &           / ( 1.0 - EXP(-VGHAT3N * ( BLKRA( NCELL ) + RD3N ) ) )

         VDEP( NCELL,VDMATK ) = MIN( VDNMAX, VDEP( NCELL,VDMATK ) )

C accumulation mode

         SC3A = NU / DCHAT3A
         EIM = STOKEA**2 / 400.0 * ESAC160
         EIM = MIN( EIM, 1.0 )
         RD3A = 1.0 / ( UTSCALE *
     &                ( SC3A ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDMACC ) = VGHAT3A
     &           / ( 1.0 - EXP(-VGHAT3A * ( BLKRA( NCELL ) + RD3A ) ) )

         VDEP( NCELL,VDMACC ) = MIN( VDAMAX, VDEP( NCELL,VDMACC ) )

C coarse mode

         SC3C = NU / DCHAT3C
         EIM = STOKEC**2 / 400.0 * ESCO160
         EIM = MIN( EIM, 1.0 )
         RD3C = 1.0 / ( UTSCALE *
     &                ( SC3C ** ( -TWO3 ) + EIM ) )

         VDEP( NCELL,VDMCOR ) = VGHAT3C
     &           / ( 1.0 - EXP(-VGHAT3C * ( BLKRA( NCELL ) + RD3C ) ) )

         VDEP( NCELL,VDMCOR ) = MIN( VDCMAX, VDEP( NCELL,VDMCOR ) )

      END DO ! end loop on deposition velocities

      RETURN
      END SUBROUTINE GETDEP_V
