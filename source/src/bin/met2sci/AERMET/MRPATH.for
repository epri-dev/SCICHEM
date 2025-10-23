      SUBROUTINE MRPATH
C=====================================================================**
C          Module MRPATH of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Controls the MERGE processing calls.
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------

C---- Data declarations

      IMPLICIT NONE
      
      INTEGER ISTAT

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize variables
      PATH = 'MERGE     '
      LOC  = 'MRPATH'
      ISTAT  = 0
      MRSTAT = 0
      IRD1 = 5

C---- Confirm that the user is merging sufficient information to
C     calculate boundary layer parameters in Stage 3.
C     Warn the user if there is a possibility that there isn't
C     sufficient information (NWS hourly + on-site, on-site only)
C     and call it an error if only NWS hourly is merged.
C     --STAT = 4 means data available; --STAT = 0 means no data

      IF(     UASTAT.EQ.4 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.4 )THEN
         MRSTAT = 2

      ELSEIF( UASTAT.EQ.4 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.0 )THEN
         MRSTAT = 2

      ELSEIF( UASTAT.EQ.4 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.4 )THEN
         MRSTAT = 2
         MESS =  BLNK80
         ECODE = 'W69'
         WRITE( MESS, 400 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.4 )THEN
         MRSTAT = 2
         MESS =  BLNK80
         ECODE = 'W69'
         WRITE( MESS, 410 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.4 )THEN
         MRSTAT = 2
         MESS =  BLNK80
         ECODE = 'W69'
         WRITE( MESS, 420 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.0 )THEN
C        Unusual but allowable
         MRSTAT = 2
         MESS =  BLNK80
         ECODE = 'W69'
         WRITE( MESS, 430 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ELSEIF( UASTAT.EQ.4 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.0 )THEN
C        Unusual but allowable
         MRSTAT = 2
         MESS =  BLNK80
         ECODE = 'W69'
         WRITE( MESS, 440 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.0 )THEN
C        Should never get to this point - AERMET should catch it earlier
         MRSTAT = -1
         RUNERR = .TRUE.
         MESS =  BLNK80
         ECODE = 'E67'
         WRITE( MESS, 450 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ENDIF

 400  FORMAT(' MERGING UPPER AIR SDGS. AND ON-SITE DATA ONLY')
 410  FORMAT(' MERGING NWS HOURLY AND ON-SITE DATA ONLY' )
 420  FORMAT(' MERGING ON-SITE DATA ONLY' )
 430  FORMAT(' MERGING NWS HOURLY DATA ONLY' )
 440  FORMAT(' MERGING UPPER AIR SDGS. ONLY' )
 450  FORMAT(' INSUFFICIENT DATA: NO DATA TO MERGE' )

      IF( STATUS(1,3).GT.0 )THEN
C------- Syntax check only - do not process any data
         RETURN

      ELSEIF( MRSTAT .EQ. 2 )THEN
C------- Call the merge subroutine
         WRITE( *,100)
         CALL AERMET_MERGE (ISTAT)
         IF( ISTAT .EQ. 1 )THEN
            MRSTAT = -1
            RUNERR = .TRUE.
         ENDIF
      ENDIF

      RETURN

 100  FORMAT( ' ' )
      END

