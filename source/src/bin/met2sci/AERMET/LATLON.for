      SUBROUTINE LATLON( KOUNT,KEY,CDATA,RDATA,ISTAT )
C=====================================================================**
C          LATLON Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Converts A*8 character value (CDATA) to its real-value
C               equivalent (RDATA).
C
C        KEY =   1: work on the latitude.
C                2: work on the longitude.
C        ISTAT = 1: indicates errors,
C                2: conversion appears OK.
C
C     CALLED BY:  LOCCRD
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER   KEY,ISTAT
      REAL      RDATA
      CHARACTER CDATA*8

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations

      PATH = PATHWD(PATHID)
      LOC  = 'LATLON'
      ISTAT = 0
      IOFLAG = 0

C---- Test on KEY, the argument that identifies if latitude or longitude
C     is to be processed
 
      IF( KEY.EQ.1 )THEN
C------- Latitude analysis

         IF( CDATA(8:8).EQ.'N') THEN
            IWORK1(1) = 8
            IWORK1(2) = 0
            IWORK1(9) = +1
         ELSEIF( CDATA(8:8).EQ.'S') THEN
            IWORK1(1) = 0
            IWORK1(2) = 8
            IWORK1(9) = -1
         ELSE
            IWORK1(1) = 0
            IWORK1(2) = 0
            IWORK1(9) = 0
         ENDIF

         IF( IWORK1(9).NE.0 )THEN

C---------- Latitude has been found - decode the numeric value
            BUF08(1) = BLNK08
            BUF08(1)(2:8) = CDATA( 1:7 )
            READ( BUF08(1),2000,IOSTAT=IOFLAG ) XRD1
 2000       FORMAT( F8.0 )

C---------- Check read status

            IF( IOFLAG .EQ. 0 )THEN

               RDATA = IWORK1(9)*XRD1
               ISTAT = 2
C------------- Check that value is reasonable
               IF( RDATA.LT.-90.0 .OR. RDATA.GT.90.0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,4000 ) BUF08(1)
 4000             FORMAT(' LATITUDE OUT OF RANGE: ',A8)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

            ELSE
C------------- Error reading the numeric part of the latitude
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,3000 ) BUF08(1)
 3000          FORMAT(' ERROR DECODING LATITUDE: ', A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1

            ENDIF

         ELSE
C---------- Neither an 'N' nor an 'S' was found for latitude
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,1000 ) BUF08(1)
 1000       FORMAT(' NO ''N'' OR ''S'' IN LATITUDE: ',A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ENDIF

      ELSEIF( KEY .EQ. 2 )THEN
C------- Longitude analysis

         IF( CDATA(8:8).EQ.'W' )THEN
            IWORK1(1) = 8
            IWORK1(2) = 0
            IWORK1(9) = +1
         ELSEIF( CDATA(8:8).EQ.'E' )THEN
            IWORK1(1) = 0
            IWORK1(2) = 8
            IWORK1(9) = -1
         ELSE
            IWORK1(1) = 0
            IWORK1(2) = 0
            IWORK1(9) = 0
         ENDIF
C
         IF( IWORK1(9).NE.0 )THEN
C---------- Found an 'E' or a 'W'; read for the numeric value
            BUF08(1) = BLNK08
            BUF08(1)(2:8) = CDATA( 1:7 )
            READ( BUF08(1),2000,IOSTAT=IOFLAG ) XRD1

C---------- Check read status
            IF( IOFLAG .EQ. 0 )THEN

               RDATA = IWORK1(9)*XRD1
               ISTAT = 2
C------------- Check that value is reasonable
               IF( RDATA.LT.-180.0 .OR. RDATA.GT.180.0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,7000 ) BUF08(1)
 7000             FORMAT(' LONGITUDE OUT OF RANGE: ',A8)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

            ELSE
C------------- Error reading the numeric part of the longitude
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,6000 )BUF08(1)
 6000          FORMAT(' ERROR DECODING LONGITUDE: ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1

            ENDIF

         ELSE
C---------- Neither an 'E' nor a 'W' was found for longitude
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) CDATA
 5000       FORMAT(' NO ''W'' OR ''E'' IN LONGITUDE: ',A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ENDIF

      ELSE
C------- KEY not identified as a 1 (latitude) or 2 (longitude)
         ECODE = 'E00'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEY
 900     FORMAT(' SUBR.LATLON ARGUMENT ERROR: KEY = ',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      RETURN
      END
      
