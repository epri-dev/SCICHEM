      SUBROUTINE SFCWXX ( IWETH, IWX )                                  ! ! dtb303 04208
C=====================================================================**
C        SFCWXX Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Convert 8-digit weather observations (columns 24-31)
C               into TD3280 present weather codes, as best as possible
C
C     Called by: D144LV
C
C     Arguments:
C       IFRZPC   = Frozen precip code (WMO present weather code)
C       ILIQPC   = Liquid precip code (WMO present weahter code)
C       IOBSVS   = Obstuctions to vision (WMO present weather code)
C       ISVRE    = Severe weather code (WMO present weather code)
C       IWETH    = 8-digit integer code to be translated
C       XIWETH   = columns 24-31 of NOAA weather code(in char form)
C
C     Revision history:
C       11/30/94 - Made into a separate subroutine from an ENTRY point
C                  (PES, Inc.)
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER ILIQ, IFRZ, IWX                                           ! ! dtb303 04208
      INTEGER ISVRE, ILIQPC, IFRZPC, IOBSVS, IWETH
      CHARACTER*8 XIWETH

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      ISVRE  = 0
      ILIQPC = 0
      IFRZPC = 0
      IOBSVS = 0
      
      ILIQ   = 0
      IFRZ   = 0
      IWX    = 0

      WRITE(XIWETH,1) IWETH
    1 FORMAT(I8.8)

C---- Classify severe weather

      IF(XIWETH(1:1) .EQ. '1') ISVRE=10
      IF(XIWETH(1:1) .EQ. '2') ISVRE=11
      IF(XIWETH(1:1) .EQ. '3') ISVRE=12
      IF(XIWETH(1:1) .EQ. '5') ISVRE=13

C---- Classify liquid precipitation

C  Drizzle
      IF(XIWETH(3:3) .EQ. '4') ILIQPC=33
      IF(XIWETH(3:3) .EQ. '5') ILIQPC=34
      IF(XIWETH(3:3) .EQ. '6') ILIQPC=35
      IF(XIWETH(3:3) .EQ. '7') ILIQPC=36
      IF(XIWETH(3:3) .EQ. '8') ILIQPC=37
      IF(XIWETH(3:3) .EQ. '9') ILIQPC=38

C  Rain
      IF(XIWETH(2:2) .EQ. '1') ILIQPC=20
      IF(XIWETH(2:2) .EQ. '2') ILIQPC=21
      IF(XIWETH(2:2) .EQ. '3') ILIQPC=22
      IF(XIWETH(2:2) .EQ. '4') ILIQPC=23
      IF(XIWETH(2:2) .EQ. '5') ILIQPC=24
      IF(XIWETH(2:2) .EQ. '6') ILIQPC=25
      IF(XIWETH(2:2) .EQ. '7') ILIQPC=26
      IF(XIWETH(2:2) .EQ. '8') ILIQPC=27
      IF(XIWETH(2:2) .EQ. '9') ILIQPC=28

C---- Classify frozen precipitation

C  Snow showers
      IF(XIWETH(5:5) .EQ. '1') IFRZPC=50
      IF(XIWETH(5:5) .EQ. '2') IFRZPC=51
      IF(XIWETH(5:5) .EQ. '3') IFRZPC=52
      IF(XIWETH(5:5) .EQ. '7') IFRZPC=56
      IF(XIWETH(5:5) .EQ. '8') IFRZPC=57
      IF(XIWETH(5:5) .EQ. '9') IFRZPC=58
      IF(XIWETH(4:4) .EQ. '1') IFRZPC=40
      IF(XIWETH(4:4) .EQ. '2') IFRZPC=41
      IF(XIWETH(4:4) .EQ. '3') IFRZPC=42
      IF(XIWETH(4:4) .EQ. '4') IFRZPC=43
      IF(XIWETH(4:4) .EQ. '5') IFRZPC=44
      IF(XIWETH(4:4) .EQ. '6') IFRZPC=45
      IF(XIWETH(4:4) .EQ. '8') IFRZPC=47

C  Ice
      IF(XIWETH(6:6) .EQ. '1') IFRZPC=90
      IF(XIWETH(6:6) .EQ. '2') IFRZPC=91
      IF(XIWETH(6:6) .EQ. '3') IFRZPC=92
      IF(XIWETH(6:6) .EQ. '5') IFRZPC=64
      IF(XIWETH(6:6) .EQ. '8') IFRZPC=66

C---- Translate obstruction to vision code

      IF(XIWETH(8:8) .EQ. '1') IOBSVS=80
      IF(XIWETH(8:8) .EQ. '2') IOBSVS=81
      IF(XIWETH(8:8) .EQ. '3') IOBSVS=82
      IF(XIWETH(8:8) .EQ. '4') IOBSVS=83
      IF(XIWETH(8:8) .EQ. '5') IOBSVS=84
      IF(XIWETH(8:8) .EQ. '6') IOBSVS=85
      IF(XIWETH(7:7) .EQ. '1') IOBSVS=70
      IF(XIWETH(7:7) .EQ. '2') IOBSVS=71
      IF(XIWETH(7:7) .EQ. '3') IOBSVS=72
      IF(XIWETH(7:7) .EQ. '4') IOBSVS=73
      IF(XIWETH(7:7) .EQ. '5') IOBSVS=74
      
      IF(ILIQPC .GT. 0) ILIQ = 1                                        ! ! dtb303 04208
      IF(IFRZPC .GT. 0) IFRZ = 2                                        ! ! dtb303 04208
      IWX = ILIQ + IFRZ                                                 ! ! dtb303 04208
      
      RETURN
      END

