      SUBROUTINE UCALST(IHR, ANGLE, ACRIT)
C=====================================================================**
C     UCALST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Calculate USTAR (friction velocity) and THSTAR (the
C               temperature scale for profiling).  A check is used to see
C               if the solution to USTAR will be either real or complex.
C               If the solution is real, the computation for USTAR
C               follows the solution proposed by Venkatram.  If the
C               solution is complex, a linear interpolation, used by
C               van Ulden and Holtslag, is applied to find the solution.
C
C     Input:
C      IHR       Integer   Hour of day
C      ANGLE     Real      Solar elevation angle

C      ACRIT     Real      Solar elevation angle above which net radiation
C                          is positive

C      WSPD      Real      Wind speed at reference height
C      ZO        Real      Surface roughness length
C      ZREF      Real      Reference height for wind speed
C      CCVR      Real      Cloud cover
C      T         Real      Ambient temperature at reference height
C
C     Output:
C      USTAR     Real      Surface friction velocity
C      THSTAR    Real      Temperature scale for profiling
C
C     Initial release: December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C     Revision history:
C        07/30/95 (PES)
C          - added the computation for theta_star when the solution to
C            the quadratic equation is complex-valued;
C
C        09/05/97 (PES)
C          - changed the constant BETAM from 4.7 to 5.0
c
c        04/24/01  (JSI)
c          - added code implementing Holtslag's correction term for theta-star.
c
c        05/01/01  (RJP)
c          - altered treatment of case in stable conditions where the
c            quadratic equation has no real solution.  Although u* and 
c            theta* may vanish at low speeds, the manner in which they
c            both vary is currently assumed to be linear with speed below
c            the threshold required for a real solution, which may not be
c            correct, and which may lead to unrealistically low M-O lengths. 
c            Furthermore, many low speed measurements are questionable 
c            because they may be near or below the instrument starting speed.  
c            Instead of having u* and theta* vanish at low wind speeds, 
c            theta* is retained in this treatment and a minimum 
c            u* equivalent to the lowest wind speed leading to a real 
c            solution is used.

c        05/18/01  (JSI)
c          - revised implementation of theta-star correction so that it applies
c            only for low solar elevations.
C
c        08/14/01  (DTB)
c          - removed code for the calculation of heat flux;  redundent code
c            is located in the calling program MPPBL.
c
c        08/28/01  (PES, RWB)
c          - reverted to original linear scaling for estimating ustar
c            and thstar for cases when quadratric equation has no real
c            solution.
c
c        07/23/04  (MACTEC/PES, RWB)
c          - included check for missing wind speed.
c
c        10/29/12  (RWB/EPA)
c          - include option to adjust u* for low-wind stable conditions
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      

      INTEGER  IHR, JJJ
      REAL     THSTR1,CDN,UNOT
      REAL     CHEK, UCR, USTCR
      REAL     ANGLE, ACRIT
      REAL     BETAM
      
C     VONK   = von Karman constant
C     GRAV   = acceleration due to gravity
C     BETAM  = constant used for profile relationships in the SBL
C     CP     = specific heat capacity of dry air, and

      REAL, PARAMETER :: VONK = 0.4, GRAV = 9.80655, 
     &                   CP = 1004.

      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
	INCLUDE 'OS1.INC'
	INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      JJJ = MPYR*10000 + MPCMO*100 + MPCDY

C --- Initialize BETAM parameter for new ADJ_U* option vs. original method
      IF( ADJ_USTAR )THEN
         BETAM = 4.7
      ELSE
         BETAM = 5.0            
      ENDIF

      IF( WSPD(IHR) .EQ. 0.0 )THEN     ! Calm, Set USTAR and THSTAR to missing
         USTAR(IHR)  = -9.
         THSTAR(IHR) = -9.

      ELSEIF( ABS(WSPD(IHR)-OSQA(23,2)) .LT. 0.01 )THEN ! Missing wind speed  ! rwb400 04205
         THSTAR(IHR) =  -9.                                             ! rwb400 04205
         USTAR(IHR)  =  -9.                                             ! rwb400 04205
         MESS =  BLNK80                                                 ! rwb400 04205
         ECODE='I87'                                                    ! rwb400 04205
         WRITE(MESS,490) IHR                                            ! rwb400 04205
  490    FORMAT(' Missing wind speed for hour: ', I2.2)                 ! rwb400 04205
         CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)                           ! rwb400 04205
        
      ELSEIF( ADJ_USTAR )THEN     ! check for new option to ajust u* for low winds

         CDN = VONK /(ALOG( (ZREF(IHR)-5.*Z0(IHR))/Z0(IHR) ) )          !  See note to Eq. 20

         THSTR1 = 0.09 * (1.0 - 0.5*( (CCVR(IHR)/10.0)**2) )         ! Eq. (24)

         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                    ! jsi033 01138

C        Correct TSTAR1 for low solar elevation; see Holtslag(1984),     ! jsi031 01114
c        BLM(29):225-350  Equation A-11                                  ! jsi031 01114
         THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                        ! jsi031 01114

         ENDIF                                                          ! jsi033 01138

         THSTAR(IHR) = THSTR1
         UNOT = SQRT( (BETAM * (ZREF(IHR)-5.*Z0(IHR)) * GRAV * 
     &                                            THSTAR(IHR))/T(IHR) )

C        Check to see if USTAR has a real or imaginary solution by
C        checking the square root part (CHEK) of the USTAR equation.

         CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * WSPD(IHR) ) )**2

C        If the solution to USTAR is real, proceed with the computation
C        of USTAR.

            USTAR(IHR) = (CDN * WSPD(IHR) / 2.0) * 
     &                   ( (1.0 + EXP(-1.0*CHEK/2.0)) /
     &                     (1.0 - EXP(-2.0/SQRT(CHEK))) )

          
      ELSE
C ---    Use the original (default) method for stable u*

         CDN = VONK /(ALOG( ZREF(IHR)/Z0(IHR) ) )          !  See note to Eq. 20

         THSTR1 = 0.09 * (1.0 - 0.5*( (CCVR(IHR)/10.0)**2) )         ! Eq. (24)

         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                    ! jsi033 01138

C        Correct TSTAR1 for low solar elevation; see Holtslag(1984),     ! jsi031 01114
c        BLM(29):225-350  Equation A-11                                  ! jsi031 01114
         THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                        ! jsi031 01114

         ENDIF                                                          ! jsi033 01138

         THSTAR(IHR) = THSTR1
         UNOT = SQRT( (BETAM * ZREF(IHR) * GRAV * THSTAR(IHR))/T(IHR) )

C        Check to see if USTAR has a real or imaginary solution by
C        checking the square root part (CHEK) of the USTAR equation.

         CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * WSPD(IHR) ) )**2

C        If the solution to USTAR is real, proceed with the computation
C        of USTAR.

         IF( CHEK.LE.1.0 )THEN                   !  Real solution

            USTAR(IHR) = (CDN * WSPD(IHR) / 2.0) *
     &                   (1.0 + SQRT(1.0-CHEK))

         ELSE                                    !  Imaginary solution

c           For the imaginary solution, we define critical values for wind speed
c           (UCR) and friction velocity (USTCR); we then scale USTCR with the
c           observed wind speed divided by UCR to obtain u*.  We apply the same
c           scaling to obtain  theta*.   Reverted to original treatment: ! rwb038 01240

            UCR = (2.0 * UNOT) / SQRT(CDN)
            USTCR = CDN * UCR / 2.0
            USTAR(IHR) = USTCR * WSPD(IHR) / UCR
            THSTAR(IHR) = THSTR1 * WSPD(IHR) / UCR

         ENDIF

      ENDIF

      RETURN
      END

