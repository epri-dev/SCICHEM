      subroutine eq_ccvr( qrin, angle, ccvrout )                        ! rwb400 04205

c     compute the equivalent cloud cover based on incoming solar        ! rwb400 04205
c     radiation measurement                                             ! rwb400 04205
c     This routine is new to AERMET Version 04205                       ! rwb400 04205

c     Called by:  MPPBL


      IMPLICIT NONE
      
      INCLUDE 'WORK1.INC'                                               
      INCLUDE 'MP2.INC'
      
      real    a1, a2, b1, b2, angle, qrin
      real    skyfract, qrnot, rad2deg
      integer ccvrout

      PATH = 'METPREP '
      LOC  = 'INCRAD'

      b1 = 0.75
      b2 = 3.4
      a1 = 990.0
      a2 = 30.0
      RAD2DEG =  57.29578

c     calculate clear sky insolation value, qrnot	
      qrnot = a1*sin(angle/rad2deg) - a2

      if (qrin .gt. 0.0 .and. qrin .ge. qrnot) then
         skyfract = 0.
      ELSEIF (qrin .gt. 0.0) then
c        calculate fractional sky cover based on qrnot and qrin
         skyfract = ((1.-qrin/qrnot)/b1)**(1./b2)
      else
         skyfract = 1.
      ENDIF

c     calculate sky cover in tenths, with limits of 0 and 10
      if (skyfract .le. 0.) then
         ccvrout = 0
      ELSEIF (skyfract .ge. 1.) then
         ccvrout = 10
      else 
         ccvrout = nint(skyfract*10.)
      ENDIF

      return
      end

