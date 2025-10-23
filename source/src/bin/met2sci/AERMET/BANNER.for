      SUBROUTINE BANNER( PAGENUM,ISTG,VERSNUM,LUN )
C=====================================================================**
C        BANNER Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To write the banner header to the device specified by
C               the input argument.
C
C     Called by:     SUMRY1, SUMRY2, AUDIT, METPROCESSOR (STAGE 3)
C
C     Calls to:      DATER
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      CHARACTER (LEN=9), SAVE ::  DMY
      CHARACTER (LEN=8), SAVE ::  HMS
      CHARACTER   ::  VERSNUM*6
      INTEGER     ::  LUN
      INTEGER     ::  PAGENUM
      INTEGER     ::  ISTG
      CHARACTER   ::  FFEED*1

C*    FFEED is ASCII form-feed character
      FFEED  = ACHAR(12)

C     PARAMETER(FFD = char(32))

C     LUN          Logical Unit Number of file to which to write the banner
C     DMY, HMS     Date and time returned from system clock
C     VERSNO       Version number as defined in 'BLOCK1.INC'
C                  common block
C
C-----------------------------------------------------------------------
C---- Call the system date and time
C
      IF( PAGENUM .EQ. 1 )THEN
C        First call to BANNER; get date and time for report banner.
         CALL DATER ( DMY,HMS )
      ENDIF
C
C---- Write the banner
C
      IF( PAGENUM .EQ. 1 )THEN
C        Don't include form-feed for first page
         WRITE( LUN,5000 ) !VERSNUM
      ELSE
C        Include form-feed for subsequent pages
         WRITE( LUN,5010 ) FFEED !,VERSNUM
      ENDIF
C     Write run time and date
      WRITE( LUN,5020 ) DMY,HMS
C     Write AERMET processing Stage and page number
      WRITE( LUN,5030 ) ISTG, PAGENUM
C
      RETURN
C
C-----------------------------------------------------------------------
! 5000 FORMAT(1X,7X,'AERMET, A Meteorological Processor for the',
!     &       ' AERMOD Dispersion Model',/35X, 'Version ',A6/)
! 5010 FORMAT(A1,7X,'AERMET, A Meteorological Processor for the',
!     &       ' AERMOD Dispersion Model',/35X, 'Version ',A6/)
 5000 FORMAT(1X,7X,'METSCI, A Meteorological Processor for the',
     &       ' SCICHEM Dispersion Model',/)
 5010 FORMAT(A1,7X,'METSCI, A Meteorological Processor for the',
     &       ' SCICHEM Dispersion Model',/)
 5020 FORMAT(22X,'Data Processed on ',A9,' at ',A8/)
 5030 FORMAT(38X,'Stage ',I1,26X,'Page',I4)
C
      END

