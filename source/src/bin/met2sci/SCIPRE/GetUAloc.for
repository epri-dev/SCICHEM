      SUBROUTINE GetUAloc( istat,xlat,xlon,ID )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/UA1.INC'
      INCLUDE '../AERMET/UA2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER istat
      REAL    xlat, xlon
      CHARACTER(*) ID

      CHARACTER(1) S, E
     
      REWIND(DEV10,IOSTAT=istat)

!---- Assumes this is FSL type

      READ( DEV10,*) !Skip 1st line
      READ( DEV10, 1050, ERR=9999,IOSTAT=istat ) BUF08(1), xlat,S,xlon,E
 1050 FORMAT( T10, A5, T23, F6.2,A1, F6.2,A1 )

      IF( S == 'S' )xlat = -xlat
      IF( E /= 'E' )xlon = -xlon

      ID = TRIM(BUF08(1))

9999  CONTINUE

      RETURN
      END