      SUBROUTINE UAEXT (ISTAT)
C=====================================================================**
C          UAEXT Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  This subroutine is the main driving routine to retrieve
C             sounding data for a station and time period specified by
C             the user.  The data is then written to a disk file for QA.
C
C   Called by: UAPATH
C
C   Initial release:  December 1992
C
C   Programmed by: Pacific Environmental Services, Inc. (PES)
C                  Research Triangle Park, NC
C
C   Revision history:
C      11/30/94
C        - Removed the version date
C        - All soundings are extracted before returning to UAEXT
C          (previously, returned to UAEXT after each sounding
C           was extracted)
C
C      11/05/96
C        - restructured much of the code
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER SDGCNT, UAARG, JULIAN, ISTAT
      INTEGER RDSZ, RDPOS, MXBLK

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'


C---- Data Initialization
      DATA  SDGCNT/0/
      DATA PATH/'UPPERAIR  '/, LOC/' UAEXT'/

C *** Variable descriptions
C
C      SDGCNT      Counter for the number of sdgs hts retrieved
C      ILEV,IVBL   Loop indices
C      UAARG       An integer variable to use for whatever purpose
C
C      JULIAN      Integer function returning julian day
C
C      RDSZ        Size of record within the block
C      RDPOS       Starting position of record within the block
C      MXBLK       Maximum block size
C
C *** Subroutines called
C
C      GET620          Extracts soundings that are archived in the
C                      TD-6201 format
C      GETFSL          Extracts soundings that were retrieved from the
C                      Radiosonde Data of North America compact disc
C      ERROR           Writes error/warning messages
C      FLWRK1,FLWRK2,  'Zeroes' arrays and variables
C      FLIWK1,FLIWK2,
C      FLSDG
C
C.......................................................................

C---- Write the headers to the output file
      WRITE(DEV12,1201)

C---- Write the standard header to the message file; if sounding
C     modification (STATUS(2,12)) is enabled, write a header also

      MESS =  BLNK80
      WRITE(MESS,1200)
      CALL ERRHDL(0,PATH,'I30',LOC,MESS)

      IF(STATUS(2,4) .GE. 2 )THEN
         IF(STATUS(2,12) .EQ. 2) THEN
            MESS =  BLNK80
            WRITE(MESS,1203)
            CALL ERRHDL(0,PATH,'I31',LOC,MESS)
            WRITE(DEV12,1203)
         ENDIF
      ENDIF

      WRITE (DEV12,1204)


C---- Set the OPEN statement parameters based on the type of data
C     for TD-6201 formatted data; parameters do not need setting for FSL
C     data

      IF(INDEX(UAFMT,'6201') .NE. 0) THEN
         UADCD = 6201
            IF(INDEX(UAFMT,'VB') .NE. 0) THEN
C---------- User has specified that the data are variable-length block
            UABLK   = 'VB'
            MXBLK = 12000
            RDSZ = 0
            RDPOS = 0

         ELSE
C---------- User has specified that the data are fixed-length block
            UABLK   = 'FB'
            RDSZ = 2876
            IF (UABLKF.EQ.0) THEN
               MXBLK = 5752
            ELSE
               MXBLK = UABLKF * RDSZ
            ENDIF
            RDPOS = 5752
         ENDIF

      ENDIF


C---- Clear the work buffers of previous values
      CALL FLWRK1
      CALL FLWRK2
      CALL FLIWK1
      CALL FLIWK2


C---- Compute the start and stop chronological days for the extraction
      UAARG = JULIAN(UAYR1,UAGMO1,UAGDY1)
      CALL CHROND('UPPERAIR  ',UAYR1,UAARG,UADAY1)

      UAARG = JULIAN(UAYR2,UAGMO2,UAGDY2)
      CALL CHROND('UPPERAIR  ',UAYR2,UAARG,UADAY2)

C---- If there was a problem determining either chronological day,
C     return to the calling program

      IF( UADAY1.LT.0  .OR.  UADAY2.LT.0 )THEN
         ISTAT = 1
         RETURN
      ENDIF

C===> Begin retrieval process:

      ISTAT = 0
      IF(STATUS(2,4) .GE. 2) THEN
C------- The 'DATA' file exists (this check is probably redundant - a
C        check should have been made during setup for data extraction

C------- Retrieve all the data

         IF( INDEX(UAFMT,'6201') .NE. 0 )THEN
C---------- TD-6201 formatted data                        ---- CALL GET620
            CALL GET620( ISTAT,SDGCNT,RDSZ,RDPOS,MXBLK )
         ELSEIF( INDEX(UAFMT,'FSL') .NE. 0 )THEN
C---------- Data retrieved from Radiosonde Data of
C           N. America CD (aka FSL data)                  ---- CALL GETFSL
            CALL GETFSL( ISTAT, SDGCNT )
         ENDIF

      ELSE
         ISTAT = 1
      ENDIF

C---- Write final message to message file

      MESS =  BLNK80
      WRITE(MESS,1202) SDGCNT
      CALL ERRHDL(0,PATH,'I39',LOC,MESS)

C---- If no soundings were retrieved, set the flag indicating a problem
      IF( SDGCNT .EQ. 0 )THEN
         ISTAT = 1
      ENDIF

      RETURN

C.......................................................................

 1200 FORMAT(' **** UPPER AIR EXTRACTION ****')
 1201 FORMAT('*     *** UPPER AIR EXTRACTION' )
 1202 FORMAT(I4,' SOUNDINGS EXTRACTED ')
 1203 FORMAT('*     *** SOUNDING MODIFICATIONS ARE ''ON'' ')
 1204 FORMAT('*** EOH: END OF UPPERAIR EXTRACT HEADERS')

      END

