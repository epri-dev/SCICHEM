      SUBROUTINE FLSFC(MH)
C=====================================================================**
C        FLSFC Module of the AERMET Meteorologoical Preprocessor
C
C     Purpose:  Initializes buffer that are used in reading and
C               processing NWS surface data.
C
C     Called by:  Routines that need to initialize an array or scalar
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INTEGER I, II, K, MH
      
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
C
C MH = Maximum number of hours to flush, beginning with 1
C
      DO K = 1,MH
         SFYR(K)  = SFQA(52,2)
         SFMO(K)  = SFQA(53,2)
         SFDAY(K) = SFQA(54,2)
         SFHR(K)  = SFQA(55,2)
         SFASOS(k) = 'N'

         DO I = 30,51
            SFOBS(K,I) = SFQA(I,2)
         ENDDO

c        Variables 35 through 40 are ASOS sky condition and layer height
c        for six layers ALC1 through ALC6.  We initialize the first layer
c        to missing.  The remaining layers are meaningful only if the first
c        layer is non-missing; consequently, these layers (variables 36-40)
c        are initialized to clear with an unlimited height.

         DO II = 36, 40                                                 ! dtb #120 02064
            SFOBS(K,II) = 00300                                         ! dtb #120 02064
         ENDDO                                                          ! dtb #120 02064

C---- As part of the SURFACE pathway, the hrly averaged 1-min ASOS
C     data arrays are cleared here

         IASOSYR(K)   = -9
         IASOSMO(K)   = -9
         IASOSDAY(K)  = -9
         IASOSHR(K)   = -9
         ASOSOBS(K,1) = -999.
         ASOSOBS(K,2) = -999.
         ASOSOBS(K,3) = -999.

      ENDDO

      RETURN
      END

