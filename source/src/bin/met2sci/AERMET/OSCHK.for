      SUBROUTINE OSCHK( KOUNT,CARD,ISTAT )
C=====================================================================**
C          OSCHK Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway redefinitions of QA range
C               values
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*4
      INTEGER  ISTAT,KEY,ITEST, I
      REAL     MISS,LBOUND,UBOUND

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' WITH NEW QA RANGE CHECK DATA
C        NAME            VNAME ON IMAGE
C        KEY             TYPE OF RANGE CHECK
C        MISS            NEW MISSING VALUE
C        LBOUND          NEW LOWER BOUND
C        UBOUND          NEW UPPER BOIUND

C------ Data Initializations

      PATH = 'ONSITE'
      LOC  = ' OSCHK'
      ISTAT = 0
      I     = 0

C------ Check the number of fields on this record: must be 6

      IF( NWORDS.LT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''RANGE'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''RANGE'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      CALL OSRANGE( KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ITEST )
      IF( ISTAT.NE.1 ) ISTAT = ITEST

C-----Identify which variable has new QA data

      IF( NAME.EQ.BLNK04 ) GO TO 30

C-----Loop through acceptable data variable names

      DO I=1,34
         IF( NAME.EQ.VNAMES(I) ) GO TO 40
  	ENDDO

C-----Loop through ONSITE date variables

      DO I=52,56
         IF( NAME.EQ.VNAMES(I) ) GO TO 40
      ENDDO

C-----No match on variable names

  30  ECODE = 'E06'
      MESS =  BLNK80
      WRITE( MESS,5000 ) NAME
5000  FORMAT(1X,A4,' NO MATCH WITH ONSITE VAR NAMES')
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1

  40  CONTINUE

C-----Redefine QA values

      IF( ISTAT.NE.1 )THEN
C------- Special trap for 'TSKY' on ONSITE pathway
         IF( I.EQ.34 )THEN
            OSTSKY(1) = FLOAT( KEY )
            OSTSKY(2) = MISS
            OSTSKY(3) = LBOUND
            OSTSKY(4) = UBOUND
         ELSE
            OSQA(I,1) = FLOAT( KEY )
            OSQA(I,2) = MISS
            OSQA(I,3) = LBOUND
            OSQA(I,4) = UBOUND
         ENDIF

         ISTAT = 2
      ENDIF

      RETURN
      END

