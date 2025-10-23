      subroutine nr_ang(albedoin,angle, ccvrin,tempin, acrit)           ! ! jsi030 01110

c     Compute the critical solar elevation angle (acrit) at which net radiation
c     is theoritically zero.  Routine has been modified to perform iterative
c     computation since acrit is a function of albedo and albedo is a function
c     of solar angle.  R.W. Brode, MACTEC/PES, 07/23/04

c     it is assumed that none of the passed variables are 'missing'.

c     Modified 12/07/2006                                               ! rwb #522 06341
c        Added variable BA1 to resolved variable name conflict with b1. ! rwb #522 06341
c        Added check for sinacrit < 0 and assigned acrit=0 for such     ! rwb #522 06341
c        cases.                                                         ! rwb #522 06341

c     Called by:  MPPBL

      IMPLICIT NONE
      
      integer ccvrin, iter
      real albedoin, angle, tempin
      real sky, acrit, alb, tt                                          ! ! dtb105 02023
      real lastacrit, eps
      real sb, c1, c2, b1, b2, a1, a2, pi, rad2deg, ba1
      real term1, term2, sinacrit, asin
      

      sb = 5.67e-08
      c1 = 5.31e-13
      c2 = 60.0
      b1 = 0.75
      b2 = 3.4
      a1 = 990.0
      a2 = 30.0
      pi = 3.14159
      rad2deg = 180.0/pi

      iter = 0
      eps = 0.01

c     Adjust albedo initially based on solar angle
      BA1 = 1. - albedoin                                               ! rwb #522 06341

      IF( ANGLE .LE. 0.0 )THEN      !  Set the nighttime albedo to 1.0
         ALB = 1.0

      ELSE                          !  Adjust albedo for solar elevation
                                                                        ! rwb #522 06341
         ALB = albedoin + BA1 * EXP(-0.1*ANGLE + (-0.5*BA1*BA1))           ! Eq. (3)
                                                                        ! rwb #522 06341
      ENDIF


c     As necessary we replace missing value flags (for cloud cover
c     and temperature) with fixed values for use locally.


      if( ccvrin .eq. 99 )then                                          ! ! dtb105 02023
         sky = 5.                                                       ! ! dtb105 02023
      else                                                              ! ! dtb105 02023
         sky = float(ccvrin)                                            ! ! dtb105 02023
      endif                                                             ! ! dtb105 02023

      if( tempin.eq.-999. .or. tempin.eq.999. )then                     ! dtb #515  06305
         tt = 288.                                                      ! ! dtb111 02045
      else                                                              ! ! dtb111 02045
         tt = tempin                                                    ! ! dtb111 02045
      endif                                                             ! ! dtb111 02045

      if( alb.lt.1.0 .and. alb.ge.0.0 )then

c        Initialize acrit = angle
         acrit = angle
         lastacrit = 0.

c        Peform iteration to calculate acrit, with limit of 20          ! ! rwb400 04205
c        iterations                                                     ! ! rwb400 04205
         do while (abs(acrit-lastacrit).gt.abs(eps*acrit) .and.         ! ! rwb400 04205
     &                                      iter .le. 20)               ! ! rwb400 04205

            iter = iter + 1                                             ! ! rwb400 04205
            lastacrit = acrit
            term1 = sb*tt**4.0 - c1*tt**6.0 - c2*sky/10.0               ! ! dtb111 02045
            term2 = (1.0-b1*(sky/10.0)**b2)*(1.0-alb)*a1                ! ! dtb111 02045

            sinacrit = term1/term2 + a2/a1

            if( sinacrit .le. 0.0 )then
c              Assign acrit = 0 and exit loop                           ! rwb #522 06341
               acrit = 0.0                                              ! rwb #522 06341
               exit                                                     ! rwb #522 06341
            elseif( sinacrit .gt. 1.0 )then
c              Assign acrit = 92 and exit loop                          ! rwb #522 06341
               acrit = 92.
               exit                                                     ! rwb #522 06341
            else
               acrit = rad2deg*asin(sinacrit)
            endif

c           Adjust albedo to current value of acrit
            IF( ACRIT .LE. 0.0 )THEN                                    ! rwb #522 06341
c              Program should not reach this point;                     ! rwb #522 06341
c              just in case assign acrit=0 and exit                     ! rwb #522 06341
               acrit = 0.0                                              ! rwb #522 06341
               exit                                                     ! rwb #522 06341

            ELSE                          !  Adjust albedo for solar elevation
                                                                        ! rwb #522 06341
               ALB = albedoin + BA1 * EXP(-0.1*ACRIT + (-0.5*BA1*BA1))      ! Eq. (3)
                                                                        ! rwb #522 06341
            ENDIF

         ENDDO                                                         ! ! rwb400 04205

      elseif( alb.ge.1.0 )THEN

         acrit = 94.

      endif

      return
      end                                                               ! ! jsi030 01110

