      SUBROUTINE HUSWX( ATEMP, IWX)


c     PURPOSE:    Interprets the present weather character variable and
c                 returns a value for the integer variable IWX
c                 indicating the type of precipitation if any.

c     INPUTS:     8-character present weather code for the hour

c     OUTPUTS:    IWX       0 = none
c                           1 = liquid precipitation
c                           2 = frozen precipitation
c                           3 = both liquid and frozen
c                           8 = unknown
c                           9 = missing



      IMPLICIT NONE
      
      INTEGER IPOS1, IPOS2, NIP1, NIP2

      CHARACTER*8  ATEMP
      CHARACTER*1  IP1, IP2
      INTEGER      ILIQ, IFRZ, IWX


c     WRITE(6, *)  ' Enter 8-character Present Weather Code'
c     READ (5,'(A8)') ATEMP

      ILIQ = 0
      IFRZ = 0
      IWX  = 0


      IF( ATEMP .EQ. '        ' )THEN               !  Missing  9
         IWX = 9
      ELSEIF( ATEMP .EQ. '99999999' )THEN               !  Unknown  8
         IWX = 8
      ELSEIF( ATEMP .EQ. '00009999' )THEN               !  None     0
         IWX = 0
      ELSEIF( ATEMP .EQ. '00000000' )THEN               !  None     0
         IWX = 0

      ELSE

         DO IPOS1 = 1, 7, 2
            IPOS2 = IPOS1 + 1
            IP1 = ATEMP(IPOS1:IPOS1)
            IP2 = ATEMP(IPOS2:IPOS2)

            READ( IP1, 100 ) NIP1
            READ( IP2, 100 ) NIP2

            IF( NIP1 .NE. 0 )THEN
         
               IF( (NIP1 .EQ. 2) .OR. (NIP1 .EQ.3) )THEN
                  IF( NIP2 .NE. 9 )THEN  !  Liquid
                  ILIQ = 1
                  ENDIF
         
               ELSEIF( (NIP1 .GE. 4) .AND. (NIP1 .LE. 6)
     &                                .OR. (NIP1 .EQ. 9) )THEN
                  IF( NIP2 .NE. 9 )THEN  !  Frozen
                  IFRZ = 2
                  ENDIF
               ENDIF
         
            ENDIF
         ENDDO

         IWX  = ILIQ + IFRZ

      ENDIF

c     WRITE (6, 110) ATEMP, ILIQ, IFRZ, IWX

  100 FORMAT(I1)
c 110 FORMAT (5X, A9, 3(3X, I3) )

      RETURN
      END

