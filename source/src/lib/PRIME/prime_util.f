c**********************************************************************
c   2014-Jun-26 Based on AERMOD source code (dated 12345).  The
c   following subroutines were extracted from the given files:
c
c   1. GINTRP    <- METEXT.F
c   2. LOCATE    <- IBLVAL.F
c   3. ANYAVG    <-   "
c   4. PRM_PLUME <- CALC1.F
c   5. FYPLM     <- CALC2.F
c   6. VRTSBL    <-   "
c   7. VRTSBN    <-   "
c**********************************************************************
      SUBROUTINE GINTRP ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C***********************************************************************
C             GINTRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C   Purpose:     A generalized interpolation routine
C
C   Input:       Height below the required height (HTBELO)
C                Value below the required height (VBELOW)
C                Height above the required height (HTBELO)
C                Value above the required height (VBELOW)
C                Height at which a value is required (REQDHT)
C
C   Output:      Value of the parameter at the required level (VALUE)
C
C   Called by:   Utility routine called by many modules
C
C   Assumptions:
C
C   Programmer:  Jim Paumier, PES, Inc.
C
C   Date:        September 30, 1993
C
C   Revision history:mex
C                <none>
C
C   Reference(s):
C
C***********************************************************************
C
C---- Variable declarations
C
      IMPLICIT NONE
      DOUBLE PRECISION :: VALUE, HTBELO, VBELOW, HTABOV, VABOVE, REQDHT
C
C---- Data dictionary
C
C---- Data initializations
C
C.......................................................................
C
C---- Interpolate

      VALUE = VBELOW + ( (REQDHT - HTBELO) / (HTABOV - HTBELO) ) *
     &                   (VABOVE - VBELOW)

      RETURN
      END

      SUBROUTINE LOCATE ( PARRAY, LVLBLW, LVLABV, VALUE, NDXBLW )
C=======================================================================
C             LOCATE Module of the AMS/EPA Regulatory Model - AERMOD
C
C   Purpose:     To return the array index such that VALUE is between
C                PARRAY(NDXBLW) and PARRAY(NDXBLW+1).
C
C   Input:       Array of gridded values (PARRAY)
C                Lower array bound at which to start the search (LVLBLW)
C                Upper array bound at which to end the search (LVLABV)
C                Value being searched for (VALUE)
C
C   Output:      Index of PARRAY immediately below VALUE (NDXBLW)
C
C   Called by:   Utility routine that can be used by any module:
C                  SRCSET (in SOSET) for stack heights
C                  METEXT for mixing height
C
C   Assumptions: PARRAY must be montonically increasing or decreasing;
C                LVLBLW can be no less than 1;
C
C   Developer(s): Jim Paumier and Roger Brode, PES, Inc.
C   Date:         30 September 1993
C
C   Revision history:
C                <none>
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE

      INTEGER   LVLABV, LVLBLW, NDXBLW, JL, JM, JU
      DOUBLE PRECISION  PARRAY(LVLABV), VALUE
C
C---- Data dictionary
C     JL   lower bound temporary variable
C     JM   midpoint temporary variable
C     JU   upper bound temporary variable
C
C----
      JL = LVLBLW - 1
      JU = LVLABV + 1

      DO WHILE( (JU - JL) .GT. 1 )

         JM = (JU + JL) / 2

         IF( VALUE .GE. PARRAY(JM) )THEN
            JL = JM
         ELSE
            JU = JM
         ENDIF

      ENDDO

      NDXBLW = MIN( JL, LVLABV-1 )

      RETURN
      END

C******************************************************************************

      SUBROUTINE ANYAVG ( NLVLS,HTS,PARRAY,ZBOT,NDXABV,ZTOP,NDXBLW,
     X                    VALAVG)
C***********************************************************************
C             ANYAVG Module of the AMS/EPA Regulatory Model - AERMOD
C
C   Purpose:     To compute the average value of the parameter between
C                any two heights (ZBOT and ZTOP)
C
C   Input:       Number of levels in the profile (NLVLS)
C                Array of gridded profile heights (HTS)
C                Parameter array (PARRAY)
C                Lower bound of averaging layer (ZBOT)
C                Index of the level gridded profile height immediately
C                   above ZBOT (NDXABV)
C                Upper bound of averaging layer (ZTOP)
C                Index of the level gridded profile height immediately
C                   below ZTOP (NDXBLW)
C
C   Output:      Average value of parameter in layer (VALAVG);
C
C   Called by:   METEXT
C
C   Assumptions: If ZTOP is above the highest profile height (5000 m),
C                then we assume the profile is constant
C                (= PARRAY(NLVLS)) above 5000 m and compute
C                the average accordingly.
C
C   Adjustments: If ZBOT is less than 0.5 m, it is set to 0.5 m.  If ZTOP
C                is less than 0.5 m, it is set to 0.51 m.
C
C   Programmer:  Bob Paine
C
C   Date:        October 4, 1994
C
C   Revision history:
C                Derived from ZIAVER
C
C   Reference(s): Alternative Approach to Treatment of inhomogeneity
C                 October 3, 1994 (Al Cimorelli)
C
C***********************************************************************
C
C---- Variable declarations
C
      IMPLICIT NONE

      INTEGER   I, NLVLS, NDXABV, NDXBLW
      DOUBLE PRECISION  HTS(NLVLS), PARRAY(NLVLS), ZBOT, ZTOP,
     &                  SUM, VALAVG
      DOUBLE PRECISION  VALBOT, VALTOP
C
C---- Data initializations
C
C.......................................................................
C
      SUM = 0.0D0
C
C     NDXABV is the profile index of the height just above ZBOT, and
C     NDXBLW is the profile index of the height just below ZTOP.
C
C---- Sum over each layer of the gridded profile (PARRAY) from NDXABV
C     to NDXBLW.  First, check to see if ZBOT and ZTOP are so close
C     together that summation over several profile levels is not
C     necessary.
C
C     Check for minimum values of ZTOP and ZBOT.
C
      IF(ZBOT .LT. 0.5D0) THEN
         ZBOT = 0.5D0
         NDXABV = 2
      ENDIF
      IF(ZTOP .LT. 0.51D0) THEN
         ZTOP = 0.51D0
         NDXBLW = 2
      ENDIF
C
      IF(NDXBLW .LT. NDXABV) GO TO 300
      IF(NDXBLW .EQ. NDXABV) GO TO 200
C
C     Sum using trapezoidal rule over intermediate profile layers.
C
      DO I = NDXABV+1, NDXBLW
         SUM = SUM + (HTS(I) - HTS(I-1)) * 0.5D0 *
     &                   (PARRAY(I) + PARRAY(I-1))
      END DO
C
C---- Finish the summation over partial layers at bottom (first), then
C     the top.
C
  200 CONTINUE
      IF(NDXABV .GT. 1) THEN
         CALL GINTRP(HTS(NDXABV-1),PARRAY(NDXABV-1),HTS(NDXABV),
     &        PARRAY(NDXABV),ZBOT,VALBOT)
         SUM = SUM + (HTS(NDXABV) - ZBOT) * 0.5D0 *
     &               (VALBOT + PARRAY(NDXABV) )
      ELSE
         SUM = SUM + (HTS(1) - ZBOT) * PARRAY(1)
      ENDIF

      IF(NDXBLW .LT. NLVLS) THEN
         CALL GINTRP(HTS(NDXBLW),PARRAY(NDXBLW),HTS(NDXBLW+1),
     &        PARRAY(NDXBLW+1),ZTOP,VALTOP)
         SUM = SUM + (ZTOP - HTS(NDXBLW)) * 0.5D0 *
     &               (VALTOP + PARRAY(NDXBLW) )
      ELSE
         SUM = SUM + (ZTOP - HTS(NLVLS)) * PARRAY(NLVLS)
      ENDIF
C
C     Take average
C
      VALAVG = SUM / (ZTOP - ZBOT)
      GO TO 999
C
C     At 300, just take the interpolated value halfway between ZBOT
C     and ZTOP, because both are within the same profile layer.
C
  300 CALL GINTRP(HTS(NDXABV-1),PARRAY(NDXABV-1),HTS(NDXABV),
     &     PARRAY(NDXABV),0.5D0*(ZBOT+ZTOP),VALAVG)
C
  999 RETURN
      END

C******************************************************************************

      SUBROUTINE PRM_PLUME (ZARG, COUT)
C***********************************************************************
C             PRM_PLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            PRIME downwash component
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    July 5, 2001
C
C   INPUTS:  Receptor height, ZARG
C
C   OUTPUTS: Contribution due to PRIME, COUT
C
C   CALLED FROM:   PRM_PCHI
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, COUT

      MODNAM = 'PRM_PLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

      IF (STABLE) THEN
         CALL VRTSBN (SZ, HE)
      ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
         CALL VRTSBL (SZ, HE, ZI)
      ELSE
         FSUBZ = 0.0D0
      END IF

C     Calculate the WRAP term for a stable atmosphere
      COUT = (QTK / US) * ( FSUBY * FSUBZ )

      RETURN
      END

C***********************************************************************

      SUBROUTINE FYPLM(SYARG,FYOUT)
C***********************************************************************
C             FYPLM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Calculate the Value of the Horizontal Gaussian
C                 Distribution Function for the Coherent Plume
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1993
C
C        INPUTS:
C                 SY   - Sigma-Y
C                 Y    - The Crosswind Distance of the Receptor from
C                        the Plume
C
C        OUTPUTS: 'FSUBY' Term
C
C        CALLED FROM:   AERCALC, PRMCALC, VOLCALC, ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: SYARG, EXPARG, FYOUT

C     Variable Initializations
      MODNAM = 'FYPLM'

      EXPARG = -(Y*Y / (2.0D0*SYARG*SYARG))
C
C     Add meander component
C
      IF (EXPARG .GT. EXPLIM) THEN
C        Calculate lateral term for Gaussian plume
         FYOUT  = DEXP(EXPARG)/(SRT2PI*SYARG)
      ELSE
         FYOUT  = 0.0D0
      END IF

      RETURN
      END

C***********************************************************************

      SUBROUTINE VRTSBL (SZARG, HEARG, ZIARG)
C***********************************************************************
C        VRTSBL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
C                 Equation for Stable Conditions.
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1993
C
C        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE and ZI for cases
C                 with receptors below stack base (ZR < 0) - 12/26/00
C
C        INPUTS:  Plume Height, HE
C                 Vertical Dispersion Parameter, SZ
C                 Mixing/Reflection Height, HSBL (= max(zi,he))
C                 Receptor Height, ZR
C
C        OUTPUTS: Vertical Term, FSUBZ
C
C        ASSUMPTIONS:   Vertical term for STABLE plumes includes
C                       multiple reflection terms.
C
C        REVISIONS:  Concentrations for receptors above HSBL forced
C                    to zero.  Change made 8/31/94 by R.F. Lee.
C
C        CALLED FROM:   WRAP, LIFT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: SZARG, HEARG, ZIARG, A1, A2, A3, A4, A5, A6,
     &           TWOIZI, SUM, T, V
      DOUBLE PRECISION :: HETMP, ZITMP

C     Variable Initializations
      MODNAM = 'VRTSBL'
      V = 0.0D0

      IF (ZR .EQ. 0.0D0) THEN
C        Vertical Term for Case With FLAT Terrain and No Flagpole
C        Receptor (ZR = 0.0D0)
         A1 = (-0.5D0/(SZARG*SZARG)) * HEARG * HEARG
         IF (A1 .GT. EXPLIM)  V = DEXP(A1)
         SUM = 0.0D0
         DO I = 1, 100
            T  = 0.0D0
C           Use ZIARG (set in PCALC = max(HE,ZI)) instead of ZI.
            TWOIZI = 2.0D0*DBLE(I)*ZIARG
            A2 = (-0.5D0/(SZARG*SZARG)) * (TWOIZI-HEARG) *
     &                                    (TWOIZI-HEARG)
            A3 = (-0.5D0/(SZARG*SZARG)) * (TWOIZI+HEARG) *
     &                                    (TWOIZI+HEARG)
            IF (A2 .GT. EXPLIM)  T = DEXP(A2)
            IF (A3 .GT. EXPLIM)  T = T + DEXP(A3)
            SUM = SUM + T

CRWB        Modify convergence criterion to use relative value of T
            IF (DABS(T) .LE. 5.0D-7*DABS(SUM)) THEN
C              Exit Loop
               EXIT
            END IF
         END DO
C        Calculate Total Vert. Term - (2.*) was Removed for Optimization
         V  = 2.0D0*(V + SUM)

      ELSE IF (ZR .LE. ZIARG) THEN
C        Vertical Term for Case of ZR .NE. 0.0
C        First adjust for terrain below stack base with ZR < 0,
C        by keeping HE and ZI horizontal.
         HETMP = MAX( HEARG, HEARG - ZR )
         ZITMP = MAX( ZIARG, ZIARG - ZR )

         A1 = (-0.5D0/(SZARG*SZARG)) * (ZR-HETMP) * (ZR-HETMP)
         A2 = (-0.5D0/(SZARG*SZARG)) * (ZR+HETMP) * (ZR+HETMP)
         IF (A1 .GT. EXPLIM)  V = DEXP(A1)
         IF (A2 .GT. EXPLIM)  V = V + DEXP(A2)
         SUM = 0.0D0
         DO I = 1, 100
            T  = 0.0D0
            TWOIZI = 2.0D0*DBLE(I)*ZITMP
            A3 = (-0.5D0/(SZARG*SZARG)) * (ZR-(TWOIZI-HETMP)) *
     &                                    (ZR-(TWOIZI-HETMP))
            A4 = (-0.5D0/(SZARG*SZARG)) * (ZR+(TWOIZI-HETMP)) *
     &                                    (ZR+(TWOIZI-HETMP))
            A5 = (-0.5D0/(SZARG*SZARG)) * (ZR-(TWOIZI+HETMP)) *
     &                                    (ZR-(TWOIZI+HETMP))
            A6 = (-0.5D0/(SZARG*SZARG)) * (ZR+(TWOIZI+HETMP)) *
     &                                    (ZR+(TWOIZI+HETMP))
            IF (A3 .GT. EXPLIM)  T = T + DEXP(A3)
            IF (A4 .GT. EXPLIM)  T = T + DEXP(A4)
            IF (A5 .GT. EXPLIM)  T = T + DEXP(A5)
            IF (A6 .GT. EXPLIM)  T = T + DEXP(A6)
            SUM = SUM + T

CRWB        Modify convergence criterion to use relative value of T
            IF (DABS(T) .LE. 1.0D-6*DABS(SUM)) THEN
C              Exit Loop
               EXIT
            END IF
         END DO
         V  = V + SUM
CCRFL
CCRFL  Add 'ELSE' to cover case where receptor is above HSBL, and
CCRFL  set V = 0 for that case.
      ELSE
        V = 0.0D0
      END IF

C     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZARG)
      FSUBZ = V / (SRT2PI*SZARG)

      RETURN
      END

      SUBROUTINE VRTSBN (SZARG, HEARG)
C***********************************************************************
C        VRTSBN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
C                 Equation for Stable Conditions WITHOUT mixing lid.
C                 This subroutine is used for plumes above the CBL.
C
C        PROGRAMMER: Russ Lee, adapted from SUBROUTINE VRTSBL written
C                 by Roger Brode
C
C        DATE:    August 31, 1994
C
C        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE for cases
C                 with receptors below stack base (ZR < 0) - 12/26/00
C
C        INPUTS:  Plume Height, HE
C                 Vertical Dispersion Parameter, SZ
C                 Receptor Height, ZR
C
C        OUTPUTS: Vertical Term, FSUBZ
C
C        ASSUMPTIONS:   This routine for Vertical term for STABLE
C                       plumes does not include multiple reflection
C                       terms (used in stable layer above CBL).
C
C        CALLED FROM:   WRAP, LIFT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: SZARG, HEARG, A1, A2, V
      DOUBLE PRECISION :: HETMP

C     Variable Initializations
      MODNAM = 'VRTSBN'
      V = 0.0D0

      IF (ZR .EQ. 0.0D0) THEN
C        Vertical Term for Case With FLAT Terrain and No Flagpole
C        Receptor (ZR = 0.0)
         A1 = (-0.5D0/(SZARG*SZARG)) * HEARG * HEARG
         IF (A1 .GT. EXPLIM)  V = DEXP(A1)
         V  = 2.D0 * V
      ELSE
C        Vertical Term for Case of ZR .NE. 0.0
C        First adjust for terrain below stack base with ZR < 0,
C        by keeping HE and ZI horizontal.
         HETMP = MAX( HEARG, HEARG - ZR )

         A1 = (-0.5D0/(SZARG*SZARG)) * (ZR-HETMP) * (ZR-HETMP)
         A2 = (-0.5D0/(SZARG*SZARG)) * (ZR+HETMP) * (ZR+HETMP)
         IF (A1 .GT. EXPLIM)  V = DEXP(A1)
         IF (A2 .GT. EXPLIM)  V = V + DEXP(A2)
      END IF

C     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZ)
      FSUBZ = V / (SRT2PI*SZARG)

      RETURN
      END

