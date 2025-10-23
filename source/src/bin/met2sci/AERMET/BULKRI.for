      SUBROUTINE BULKRI(IHR, ANGLE, ACRIT, DELT, T1, Z1, Z2)            ! dtb125 02107
C=====================================================================**
C
C     BULKRI Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Calculate USTAR and THSTAR using a Bulk Richardson Number
C               approach
C
C     Input:
C
C        IHR       Integer   Hour of day
C        WSPD      Real      Wind speed at reference height
C        ZO        Real      Surface roughness length
C        DELT      Real      Vertical temperature difference (T2-T1)
C        Z1        Real      Lower level for Delta-T
C        Z2        Real      Upper level for Delta-T
C        T1        Real      Temperature at Z1
C
C     Output:
C
C        USTAR     Real      Surface friction velocity
C
C        PTG       Real      Potential temperature gradient
c                            (output to error file for debug purposes only)
C
C     Initial release: January 2001
C
C     Revision history:
C        07/23/2004 (MACTEC/PES)
C          - modified to use profile method for estimating u* and L     ! rwb400 04205
C
C     Called by:   MPPBL
C
C     Note the equation numbers indicated in the right hand margins of
C     several records, refer to the equations documented in the memorandum
C     from Jim Clary and Associates, 'Code Changes to AERMET' dated
C     January 11 2001.
C
C-----------------------------------------------------------------------

C     VONK   = von Karman constant
C     GRAV   = acceleration due to gravity
C     BETAM  = constant used for profile relationships in the SBL
C     EPS    = convergence criterion (1% here) for Monin-Obukov length
C     DELT   = vertical temperature difference (T2-T1)
C     T1     = Temp at level 1
C     Z1     = Level 1 Height
C     Z2     = Level 2 Height

      IMPLICIT NONE

      INTEGER  IHR, JJJ, ITER, IMIN

      REAL  THSTR1, CDN, VONK, GRAV
      REAL  BETAM, Z1, Z2, EPS, LASTU, DTDZ
      REAL  A, B, T1, USTAR1, CTHETA, DELT, PTG
      REAL  ACRIT, ANGLE, ALPHA, DTHETA, LASTOBU, OBU, LASTTH


      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      DATA VONK/0.4/,  GRAV/9.80655/

      PATH  = 'METPREP'
      LOC   = 'BULKRI'

      ALPHA   = 1.0                                                     ! rwb400 04205
      IF( ADJ_USTAR )THEN
         BETAM = 4.7
      ELSE
         BETAM = 5.0            
      ENDIF

      EPS     = 0.01   
      LASTU   = 0.0
      LASTOBU = 0.0                                                     ! rwb400 04205
      LASTTH  = 0.0                                                     ! rwb400 04205 
      ITER    = 0                                                       ! rwb400 04205   

                
      JJJ = MPYR*10000 + MPCMO*100 + MPCDY

      DTDZ = DELT/(Z2-Z1)            ! temperature gradient
      PTG = DTDZ + 0.0098            ! potential temperature gradient
      DTHETA = DELT + (Z2-Z1)*0.0098 ! potential temperature difference ! rwb400 04205

      IF( WSPD(IHR) .EQ. 0.0 )THEN                    !  Calm hour
         THSTAR(IHR) = -9.
         USTAR(IHR)  = -9.
         MOL(IHR) = -99999.
         RETURN                                                         ! rwb400 04205

      ELSEIF( ABS(WSPD(IHR)-OSQA(23,2)) .LT. 0.01 )THEN   !  Missing wind speed
         THSTAR(IHR) =  -9.
         USTAR(IHR)  =  -9.
         MOL(IHR)    = -99999.
         MESS =  BLNK80
         ECODE='I87'
         WRITE(MESS,490) IHR
         CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)
         RETURN                                                         ! rwb400 04205
 
      ELSE

         IF( DTHETA .LE. 0.0 )THEN               ! Negative DTHETA      ! rwb400 04205
            DTHETA = 0.0                                                ! rwb400 04205
            
C           Use neutral limit for USTAR and low value for THSTAR to drive MOL to limit

            IF( ADJ_USTAR )THEN
               USTAR(IHR)  = VONK*WSPD(IHR)/
     &                        (ALOG( (ZREF(IHR)-5.*Z0(IHR))/Z0(IHR) ))  
               THSTAR(IHR) = 0.00001                                  
               MOL(IHR)    = 8888.                                    
               RETURN

            ELSE
               USTAR(IHR)  = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR)))      ! rwb400 04205
               THSTAR(IHR) = 0.00001                              
               MOL(IHR)    = 8888.                                    
               RETURN

            ENDIF                                                     

         ENDIF

         CTheta = 1/(ALPHA*ALOG(Z2/Z1))                    !  Eq. (2)
         B      = BETAM * (Z2 - Z1) * VONK * GRAV / T1     !  Eq. (3)
         A      = BETAM * Z2 * GRAV / T1                   !  Eq. (4)
         CDN    = VONK /(ALOG( Z2/Z0(IHR)))                !  Eq. (5)

         THSTR1 = VONK*CTheta*DTHETA/(1+(B*CTheta)/(A*CDN)) !  Eq. (1)  ! dtb125 02107

         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                   ! jsi033 01138

C           Correct TSTAR1 for low solar elevation; see Holtslag(1984), ! jsi031 01114
c           BLM(29):225-350  Equation A-11                              ! jsi031 01114
            THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                    ! jsi031 01114

         ENDIF                                                         ! jsi033 01138
                                                                                       
C        Calculate U* for u < Uc
         USTAR1 = SQRT(CDN*A*THSTR1)                       !  Eq. (6)

C        Calculate initial guess for L                               ! rwb400 04205
         OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)                   ! rwb400 04205

         IMIN = 3               ! Minimum of 3 iterations            ! rwb400 04205
         DO WHILE((ABS(LASTOBU-OBU)   .GT. ABS(EPS*OBU) .OR.         ! rwb400 04205
     &             ABS(LASTU-USTAR1)  .GT. ABS(EPS*USTAR1) .OR.      ! rwb400 04205
     &             ABS(LASTTH-THSTR1) .GT. ABS(EPS*THSTR1) .OR.      ! rwb400 04205
     &             IMIN .GE. 1) .AND.                                ! rwb400 04205
     &             ITER .LT. 20)    ! Maximum of 20 iterations       ! rwb400 04205

            IMIN = MAX( 0, IMIN - 1)                                 ! rwb400 04205
            ITER = ITER+1                                            ! rwb400 04205
            LASTOBU = OBU                                            ! rwb400 04205
            LASTU = USTAR1                                           ! rwb400 04205
            LASTTH= THSTR1                                           ! rwb400 04205

            IF( ADJ_USTAR )THEN
               USTAR1 = VONK*WSPD(IHR)/
     &           (ALOG( (ZREF(IHR)-5.*Z0(IHR))/Z0(IHR))+             
     &               BETAM*ZREF(IHR)/OBU)                            
     
               IF (ZREF(IHR) .GT. 0.5*OBU) THEN    
                  USTAR1 = VONK*WSPD(IHR)/
     &              (ALOG((ZREF(IHR)-5.*Z0(IHR))/Z0(IHR)) +        
     &              7.0*ALOG(ZREF(IHR)/OBU) + 4.25/(ZREF(IHR)/OBU) -
     &              0.5/((ZREF(IHR)/OBU)*(ZREF(IHR)/OBU)) +        
     &              (BETAM/2-1.648))                           
               ENDIF                                           

            ELSE
               USTAR1 = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR))+     
     &                  BETAM*ZREF(IHR)/OBU)                         

               IF (ZREF(IHR) .GT. 0.5*OBU) THEN                      
                  USTAR1 = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR)) + 
     &              7.0*ALOG(ZREF(IHR)/OBU) + 4.25/(ZREF(IHR)/OBU) - 
     &              0.5/((ZREF(IHR)/OBU)*(ZREF(IHR)/OBU)) +          
     &              (BETAM/2-1.648))                                 
               ENDIF                                                 

            ENDIF
  
            THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)    

            IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C              Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c              (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
               THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

            ENDIF                                                      ! jsi033 01138      

C           Calculate new guess for L                                 ! rwb400 04205
            OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)                 ! rwb400 04205

         ENDDO

         THSTAR(IHR) = THSTR1
         USTAR(IHR)  = USTAR1
         MOL(IHR)  = OBU                                              ! rwb400 04205

      ENDIF

  505 RETURN

  490 FORMAT(' Missing wind speed for hour: ', I2.2)
  491 FORMAT(' DTHETA amd PTG are: ', 2F8.3, ' for Hr: ',I2)            ! dtb134 02240
  492 FORMAT(' No solution for u* and L for SBL for hour: ',I2.2)

      END

