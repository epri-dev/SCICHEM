      SUBROUTINE D6201H( ISTAT )
C=======================================================================
C           D6201H Module of the AERMET Meteorological Preprocessor
C
C    Purpose:  Decodes (internal read from the buffer) the upper air
C              sounding headers (station ID and date)
C
C    Called by: GET620
C
C    Arguments:
C       ISTAT     Status of the decode: 0 = good; 1 = error
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INTEGER ISTAT, IOST10

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

      BUF08(1) = BLNK08
      READ(BUFNWS,6201,ERR=10001,IOSTAT=IOST10) BUF08(1),
     &     UAGYR,UAGMO, UAGDY,UAGHR
6201  FORMAT(A8,13X,4(I2))
      ISTAT = 0
      RETURN

C---- Processing continues here if there was an error decoding the header

10001 CONTINUE
      ISTAT = 1
      RETURN
      END

