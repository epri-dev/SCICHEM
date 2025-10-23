      SUBROUTINE AUDIT( ISTAGE,ITEST )
C=====================================================================**
C          AUDIT Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Interrogates the audit arrays and produces a table of
C               audit results in the summary file
C
C     CALLED BY: FINISH
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

C---- Data decalrations

      IMPLICIT NONE
      
      LOGICAL LVAR

c     INTEGER NTOT,NTOT1,NTOT2,AUDTOT,IOST70,CONCAT(22),DEVICE           ! dtb120 02064
      INTEGER NTOT,NTOT1,NTOT2,AUDTOT,IOST70,CONCAT(15),DEVICE           ! dtb120 02064

      INTEGER I, J, K, N, ISTAGE, ITEST
      REAL PERCEN,PERCN1,PERCN2
      CHARACTER*96 CVAR1
      CHARACTER*12 UAHTS(10),DTYPE
      CHARACTER*4 SFVBL1,SFVBL2, TNAME

C     NTOT, NTOT1,NTOT2     Total number of observations audited
C     AUDTOT                Sum of the audit indicators - computed for
C                           each path, for scalars and for vectors
C     PERCEN,PERCN1,PERCN2  Percent capture
C     UAHTS                 Character array defining sounding layers
C     DTYPE                 Variable defining audit category
C     TNAME                 Temporary storage of variable name

      INCLUDE 'MAIN1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C---- Data initializations

      DATA CONCAT/4*1, 100, 8*1000, 2*100/                               ! dtb120 02064
      LOC = '   AUDIT'
      ITEST = 0

C---- Check for existence of a report file and temporary file and
C     write introductory comments

      IF(STATUS(1,1) .EQ. 2) THEN
         DEVICE = DEV50
      ELSE
         DEVICE = DEVIO
      ENDIF

      INQUIRE (UNIT=DEV70,OPENED=LVAR,NAME=CVAR1)

      PGNUM = PGNUM + 1
      CALL BANNER( PGNUM,ISTAGE,VERSNO,DEVICE )

      IF(STATUS(1,3) .EQ. 0) THEN
         IF( JBSTAT .LT. 0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0 .OR.
     &       OSSTAT .LT. 0) THEN
            WRITE(DEVICE,5210)

         ELSE
            WRITE(DEVICE,5220)

         ENDIF

      ELSEIF(STATUS(1,3) .GT. 0) THEN
         WRITE(DEVICE,5050)
      ENDIF

      WRITE(DEVICE,5005)

C-----------------------------------------------------------------------
C *** CHECK THE UPPER AIR PATH FOR A SUCCESSFUL QA DURING THIS RUN
C
      IF((UASTAT .EQ. 2) .OR. (UASTAT .EQ.3) .OR. (UASTAT .EQ.6)) THEN
C
  205  AUDTOT = 0
       DO I = 1,UAVR-2
         AUDTOT = AUDTOT + UAVAUD(I)
       ENDDO
C
       DTYPE = '   SOUNDINGS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,3) .EQ. 0) THEN
         WRITE(DEVICE,5009) DTYPE
         WRITE(DEVICE,5010)
         WRITE(DEVICE,5011)
        ELSE
         WRITE(DEVICE,5109) DTYPE
         WRITE(DEVICE,5110)
         WRITE(DEVICE,5111)
        ENDIF
       ELSE
        WRITE(DEVICE,5400) DTYPE
        GO TO 305
       ENDIF
C
C *** DEFINE THE LAYER HEADERS
C
      WRITE(UAHTS(1),5500)
      DO I=2,9
        WRITE(UAHTS(I),5501) (I-2)*UAINC,(I-1)*UAINC
      ENDDO
      WRITE(UAHTS(10),5502) 8*UAINC
C
C *** PROCESS FOR EACH HEIGHT LAYER FIRST, THEN VARIABLE
C
      DO K = 1,10
       WRITE(DEVICE,5600) UAHTS(K)
       DO I = 1,10
        IF(K.EQ.1 .AND. I.GE.7) CYCLE
        IF(UAVAUD(I) .EQ. 1) THEN
          PERCEN = 0.0
          NTOT   = 0
          DO J=0,3
           NTOT = NTOT + UAAUDT(I,K,J)
          ENDDO
          IF(NTOT .EQ. 0) THEN
           CYCLE
          ELSE
           PERCEN = (UAAUDT(I,K,0)/FLOAT(NTOT)) * 100.0
          ENDIF
          WORK1(200) = FLOAT(UAQA(I,2))
          WORK1(201) = FLOAT(UAQA(I,3))
          WORK1(202) = FLOAT(UAQA(I,4))
          IF(STATUS(1,3) .EQ. 0) THEN
           WRITE(DEVICE,5100) UAVAR(I),NTOT,(UAAUDT(I,K,N),N=1,3),
     &       PERCEN,WORK1(200),WORK1(201),WORK1(202)
          ELSE
           WRITE(DEVICE,5200) UAVAR(I),WORK1(200),WORK1(201),
     &       WORK1(202)
          ENDIF
        ENDIF
       ENDDO
      ENDDO
C
C *** WRITE A NOTE TO THE USER ABOUT THE SCALED VARIABLES
      WRITE(DEVICE,5020)
C
C *** WRITE THE SUPPLEMENTARY QA INFORMATION STORED ON DEV70
C      IF THERE WAS AN AUDIT
C
       IF(STATUS(1,3) .GT. 0) GO TO 305
C
       IF (LVAR) THEN
C
       REWIND DEV70
  240  BUF80(1) = BLNK80
       READ(DEV70,700,END=710,ERR=720,IOSTAT=IOST70) BUF80(1)
       IF( INDEX(BUF80(1),'$UAUA$') .NE. 0) THEN
C
C ***   SUPPLEMENTARY INFORMATION TO FOLLOW
  245   BUF80(1) = BLNK80
        READ(DEV70,700,END=710,ERR=720,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$UAUA$') .NE. 0) THEN
C
C ***    SUPPLEMENTARY INFORMATION COMPLETED
         GO TO 305
        ELSE
C
C ***    WRITE SUPPLEMENTARY INFORMATION
         WRITE(DEVICE,700) BUF80(1)
         GO TO 245
        ENDIF
       ELSE
        GO TO 240
       ENDIF
C
  710  WRITE(DEVICE,705)
       GO TO 305
  720  WRITE(DEVICE,706) IOST70
       MESS =  BLNK80
       WRITE(MESS,707) IOST70
       CALL ERRHDL(0,'UA','E20',LOC,MESS)
C
      ELSE
      WRITE(DEVICE,704)
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C *** SURFACE DATA
C
  305 CONTINUE
  
      IF((SFSTAT .EQ. 2) .OR. (SFSTAT .EQ. 3) .OR. (SFSTAT .EQ. 6)) THEN
       DTYPE = 'SURFACE DATA'
       IF(STATUS(1,3) .EQ. 0) THEN
        WRITE(DEVICE,5009) DTYPE
        WRITE(DEVICE,5010)
        WRITE(DEVICE,5011)
       ELSE
        WRITE(DEVICE,5109) DTYPE
        WRITE(DEVICE,5110)
        WRITE(DEVICE,5111)
       ENDIF
C
       DO 310 I=1,22
        IF(SFSAUD(I) .EQ. 1) THEN

c        IF((I .GE. 5) .AND. (I .LE. 15)) THEN                           ! dtb120 02064
         IF((I .GE. 5) .AND. (I .LE. 13)) THEN                           ! dtb120 02064
C
C ***     CONCATENATED VARIABLES
          PERCN1 = 0.0
          PERCN2 = 0.0
          NTOT1  = 0
          NTOT2  = 0
C
          DO 320 J=0,3
           NTOT1 = NTOT1 + SFAUD1(I-4,J)
           NTOT2 = NTOT2 + SFAUD2(I-4,J)
  320     CONTINUE
C
          IF(NTOT1 .EQ. 0) THEN
           PERCN1 = 0.0
          ELSE
           PERCN1 = (SFAUD1(I-4,0)/FLOAT(NTOT1)) * 100.0
          ENDIF
          IF(NTOT2 .EQ. 0) THEN
           PERCN2 = 0.0
          ELSE
           PERCN2 = (SFAUD2(I-4,0)/FLOAT(NTOT2)) * 100.0
          ENDIF
          SFVBL1 = VNAMES(I+29)(1:2)//'  '
          SFVBL2 = '  '//VNAMES(I+29)(3:4)
C
          IWORK1(200) = SFQA(I+29,2)/CONCAT(I)
          WORK1(200)  = FLOAT(IWORK1(200))
          WORK1(201)  = FLOAT(SFQA(I+29,2)) - WORK1(200)*CONCAT(I)
C
          IWORK1(202) = SFQA(I+29,3)/CONCAT(I)
          WORK1(202)  = FLOAT(IWORK1(202))
          WORK1(203)  = FLOAT(SFQA(I+29,3)) - WORK1(202)*CONCAT(I)
C
          IWORK1(204) = SFQA(I+29,4)/CONCAT(I)
          WORK1(204)  = FLOAT(IWORK1(204))
          WORK1(205)  = FLOAT(SFQA(I+29,4)) - WORK1(204)*CONCAT(I)
C
          IF(STATUS(1,3) .EQ. 0) THEN
           WRITE(DEVICE,5100) SFVBL1,NTOT1,(SFAUD1(I-4,N),N=1,3),
     &      PERCN1,WORK1(200),WORK1(202),WORK1(204)
           WRITE(DEVICE,5100) SFVBL2,NTOT2,(SFAUD2(I-4,N),N=1,3),
     &      PERCN2,WORK1(201),WORK1(203),WORK1(205)
          ELSE
           WRITE(DEVICE,5200) SFVBL1,WORK1(200),WORK1(202),
     &       WORK1(204)
           WRITE(DEVICE,5200) SFVBL2,WORK1(201),WORK1(203),
     &       WORK1(205)
          ENDIF
C
         ELSE
C
C ***     REGULAR VARIABLE
          PERCEN = 0.0
          NTOT   = 0
          DO 330 J = 0,3
           NTOT = NTOT + SFAUD(I,J)
  330     CONTINUE
          IF(NTOT .EQ. 0) THEN
           PERCEN = 0.0
          ELSE
           PERCEN = (SFAUD(I,0)/FLOAT(NTOT)) * 100.0
          ENDIF
          WORK1(200) = FLOAT(SFQA(I+29,2))
          WORK1(201) = FLOAT(SFQA(I+29,3))
          WORK1(202) = FLOAT(SFQA(I+29,4))

          TNAME = VNAMES(I+29)
          IF( VNAMES(I+29) .EQ. 'WIND' )THEN
             TNAME = 'WSPD'
          ENDIF

          IF(STATUS(1,3) .EQ. 0) THEN
             WRITE(DEVICE,5100) TNAME,NTOT,(SFAUD(I,N),N=1,3),
     &                          PERCEN,WORK1(200),WORK1(201),WORK1(202)
          ELSE
             WRITE(DEVICE,5200)TNAME,WORK1(200),WORK1(201),
     &                         WORK1(202)
          ENDIF
         ENDIF
        ENDIF
  310  CONTINUE
C
C ***  WRITE A NOTE TO THE USER ABOUT THE SCALED VARIABLES
       WRITE(DEVICE,5020)
C
C ***  WRITE THE SUPPLEMENTARY QA INFORMATION STORED ON DEV70
C       IF THERE WAS AN AUDIT
C
       IF(STATUS(1,3) .GT. 0) GO TO 405
C
       IF (LVAR) THEN
       REWIND DEV70
       WRITE(DEVICE,700)
  340  BUF80(1) = BLNK80
       READ(DEV70,700,END=750,ERR=760,IOSTAT=IOST70) BUF80(1)
       IF( INDEX(BUF80(1),'$SFSF$') .NE. 0) THEN
C
C ***   SUPPLEMENTARY INFORMATION TO FOLLOW
  345   BUF80(1) = BLNK80
        READ(DEV70,700,END=750,ERR=760,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$SFSF$') .NE. 0) THEN
C
C ***    SUPPLEMENTARY INFORMATION COMPLETED
         GO TO 405
        ELSE
C
C ***    WRITE SUPPLEMENTARY INFORMATION
         WRITE(DEVICE,700) BUF80(1)
         GO TO 345
        ENDIF
       ELSE
        GO TO 340
       ENDIF
C
  750  WRITE(DEVICE,705)
       GO TO 405
  760  WRITE(DEVICE,706) IOST70
       MESS =  BLNK80
       WRITE(MESS,707) IOST70
       CALL ERRHDL(0,'SF','E20',LOC,MESS)
C
      ELSE
      WRITE(DEVICE,704)
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C *** ON-SITE DATA
C
  405 IF((OSSTAT.EQ.2) .OR. (OSSTAT.EQ.3) .OR. (OSSTAT.EQ.6) )THEN
       IF((UASTAT.EQ.2) .OR. (UASTAT.EQ.3) .OR. (UASTAT.EQ.6) .OR.
     &    (SFSTAT.EQ.2) .OR. (SFSTAT.EQ.3) .OR. (SFSTAT.EQ.6))  THEN
        WRITE(DEVICE,5007)
        PGNUM = PGNUM + 1
        CALL BANNER( PGNUM,ISTAGE,VERSNO,DEVICE )
        IF(STATUS(1,3) .EQ. 0) THEN
         IF(JBSTAT .LT. 0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0 .OR.
     &      OSSTAT .LT. 0) THEN
          WRITE(DEVICE,5210)
         ELSE
          WRITE(DEVICE,5220)
         ENDIF
        ELSEIF(STATUS(1,3) .GT. 0) THEN
         WRITE(DEVICE,5050)
        ENDIF
C
        WRITE(DEVICE,5006)
       ENDIF
C
C ***  BEGIN WITH THE SCALARS
       AUDTOT = 0
       DO I = 1,14
        AUDTOT = AUDTOT + OSSAUD(I)
       ENDDO
       DO I = 30,34
        AUDTOT = AUDTOT + OSSAUD(I)
       ENDDO
C
C ***  IF THERE ARE AUDIT VARIABLES, WRITE THE HEADER
C ***  IF THERE ARE NO AUDITS ON SCALARS, GO TO THE MULTI-LEVEL DATA
C
       DTYPE = 'SITE SCALARS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,3) .EQ. 0) THEN
         WRITE(DEVICE,5009) DTYPE
         WRITE(DEVICE,5010)
         WRITE(DEVICE,5011)
        ELSE
         WRITE(DEVICE,5109) DTYPE
         WRITE(DEVICE,5110)
         WRITE(DEVICE,5111)
        ENDIF
       ELSEIF(AUDTOT .EQ. 0) THEN
        WRITE(DEVICE,5400) DTYPE
        GO TO 500
       ENDIF
C
C ***  PROCESS THE AUDIT DATA - BY VARIABLE
       DO 410 I = 1,14
        IF(OSSAUD(I) .EQ. 1) THEN
         PERCEN = 0.0
         NTOT   = 0
         DO 420 J =0,3
          NTOT = NTOT + OSAUD1(I,J)
  420    CONTINUE
         IF(NTOT .EQ. 0) THEN
          PERCEN = 0.0
         ELSE
          PERCEN = (OSAUD1(I,0)/FLOAT(NTOT)) * 100.0
         ENDIF
         WORK1(200) = OSQA(I,2)
         WORK1(201) = OSQA(I,3)
         WORK1(202) = OSQA(I,4)
         IF(STATUS(1,3) .EQ. 0) THEN
          WRITE(DEVICE,5100) VNAMES(I),NTOT,(OSAUD1(I,N),N=1,3),
     &     PERCEN,WORK1(200),WORK1(201),WORK1(202)
         ELSE
          WRITE(DEVICE,5200) VNAMES(I),WORK1(200),WORK1(201),
     &     WORK1(202)
         ENDIF
        ENDIF
  410  CONTINUE
C
       DO 415 I=30,34
        IF(OSSAUD(I) .EQ. 1) THEN
         PERCEN = 0.0
         NTOT   = 0
         DO 425 J=0,3
          NTOT = NTOT + OSAUD1(I,J)
  425    CONTINUE
         IF(NTOT .EQ. 0) THEN
          PERCEN = 0.0
         ELSE
          PERCEN = (OSAUD1(I,0)/FLOAT(NTOT)) * 100.0
         ENDIF
         IF(I .EQ. 34) THEN
          OSQA(I,2) = OSTSKY(2)
          OSQA(I,3) = OSTSKY(3)
          OSQA(I,4) = OSTSKY(4)
         ENDIF
         WORK1(200) = OSQA(I,2)
         WORK1(201) = OSQA(I,3)
         WORK1(202) = OSQA(I,4)
         IF(STATUS(1,3) .EQ. 0) THEN
          WRITE(DEVICE,5100) VNAMES(I),NTOT,(OSAUD1(I,N),N=1,3),
     &     PERCEN,WORK1(200),WORK1(201),WORK1(202)
         ELSE
          WRITE(DEVICE,5200) VNAMES(I),WORK1(200),WORK1(201),
     &     WORK1(202)
         ENDIF
        ENDIF
  415  CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C *** MULTI-LEVEL ON-SITE DATA
C ***  CHECK FOR AUDITS
C
  500  AUDTOT = 0
       DO J = 1,OSML
        DO I = 1,OSMVEC
         AUDTOT = AUDTOT + OSVAUD(J,I)
        ENDDO
       ENDDO
C
       DTYPE = 'SITE VECTORS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,3) .EQ. 0) THEN
         WRITE(DEVICE,5009) DTYPE
         WRITE(DEVICE,5010)
         WRITE(DEVICE,5011)
        ELSE
         WRITE(DEVICE,5109) DTYPE
         WRITE(DEVICE,5110)
         WRITE(DEVICE,5111)
        ENDIF
C
       ELSEIF(AUDTOT .EQ. 0) THEN
        WRITE(DEVICE,5400) DTYPE
        GO TO 600
       ENDIF
C
C ***  PROCESS THE AUDIT DATA - BY LEVEL, BY VARIABLE
C
cprt       print *,'AUDIT:',osht
cprt       print *,'      ',osvobs

       DO 510 K = 1,OSNL
C
C ***   DETERMINE IF THE HEIGHTS ARE IN THE ARRAY OSHT OR OSVOBS
C
        IF( ABS((OSHT(K)-OSQA(15,2))) .LT. 0.01 .OR. 
     &          (OSHT(K) .EQ. 0.0) )THEN
         IF( ABS((OSVOBS(1,K,1)-OSQA(15,2))) .GT. 0.01 .AND.
     &           (OSVOBS(1,K,1) .NE. 0.0) )THEN
           OSHT(K) = OSVOBS(1,K,1)
C----      Check for OSHT's decreasing with "height"
           IF( K.GT.1 )THEN
              IF( OSHT(K).LT.OSHT(K-1) )THEN
                MESS =  BLNK80
                ECODE = 'E27'
                WRITE( MESS,4200 ) K, K-1, OSHT(K), OSHT(K-1)
4200            FORMAT(' ONSITE height level ',I2,' is lower ',
     &                 'than level ',I2,'; ',F7.2,' vs. ',F7.2)
                CALL ERRHDL( 0,'ONSITE    ',ECODE,LOC,MESS )
                ITEST = 1
              ENDIF
           ENDIF
           
         ELSE
           OSHT(K) = 0.0
         ENDIF
        ENDIF
cprt       print *,'AUDIT:',osht

C
        WRITE(DEVICE,860) OSHT(K)
        DO 520 I = 1,OSMVEC
         IF(OSVAUD(K,I) .EQ. 1) THEN
           PERCEN = 0.0
           NTOT   = 0
           DO 530 J = 0,3
            NTOT = NTOT + OSAUD2(K,I,J)
  530      CONTINUE
C
           IF(NTOT .EQ. 0) THEN
            PERCEN = 0.0
           ELSE
            PERCEN = (OSAUD2(K,I,0)/FLOAT(NTOT)) * 100.0
           ENDIF
           WORK1(200) = OSQA(I+14,2)
           WORK1(201) = OSQA(I+14,3)
           WORK1(202) = OSQA(I+14,4)
           IF(STATUS(1,3) .EQ. 0) THEN
            WRITE(DEVICE,5100) VNAMES(I+14),NTOT,(OSAUD2(K,I,N),N=1,3),
     &       PERCEN,WORK1(200),WORK1(201),WORK1(202)
           ELSE
            WRITE(DEVICE,5200) VNAMES(I+14),WORK1(200),WORK1(201),
     &                        WORK1(202)
           ENDIF
         ENDIF
  520   CONTINUE
  510  CONTINUE
C
C *** ENDIF FOR ON-SITE DATA
      ENDIF
C
C-----------------------------------------------------------------------
C *** CONCLUSION AND FORMAT STATEMENTS
C
  600 WRITE(DEVICE,900)
C
      RETURN
C
  700 FORMAT(A80)
  704 FORMAT('  SUPPLEMENTARY DATA ARE NOT AVAILABLE FOR PROCESSING')
  705 FORMAT('  END-OF-FILE ON DEV70 BEFORE COMPLETING SUPPLEMENTAL',
     &      ' INFORMATION')
  706 FORMAT('  ERROR READING DEV70 WHILE PROCESSING FOR SUPPLEMENTAL',
     &      ' INFORMATION, IOSTAT= ',I8)
  707 FORMAT(' ERROR READING DEV70, IOSTAT= ',I8)
  860 FORMAT(2X,F8.2,' M')
  900 FORMAT(/5X,'THIS CONCLUDES THE AUDIT TRAIL')
 5005 FORMAT(//24X,'**** SUMMARY OF THE QA AUDIT ****')
 5006 FORMAT(//19X,'**** SUMMARY OF THE QA AUDIT, CONTINUED ****' )
 5007 FORMAT(/10X,'THE AUDIT IS CONTINUED ON THE NEXT PAGE')
 5009 FORMAT(//1X,A12,9X,'|------VIOLATION SUMMARY------|',
     &       3X,'|-----TEST VALUES-----|')
 5010 FORMAT(15X,'TOTAL',5X,'#',5X,'LOWER',2X,'UPPER',6X,'%',6X,
     &        'MISSING',3X,'LOWER',3X,'UPPER')
 5011 FORMAT(15X,'# OBS',2X,'MISSING',2X,'BOUND',2X,'BOUND',
     &        2X,'ACCEPTED',5X,'FLAG',4X,'BOUND',3X,'BOUND')
 5020 FORMAT(/1X,'NOTE: Test values were also multiplied by the same ',
     & 'factors applied to the data', /7X,
     & '(see Appendix B of the AERMET User''s Guide)'/)

 5050 FORMAT(/18X,'**********************************************',/
     &        18X,'***   NO AUDIT; INPUT IMAGES CHECKED ONLY  ***',/
     &        18X,'**********************************************'/)

 5100 FORMAT(7X,A4,3X,I5,4X,I5,2X,I5,2X,I5,4X,F6.2,3X,F8.1,',',
     &        F7.1,',',F7.1)
 5109 FORMAT(//,1X,A12,43X,'|-----TEST VALUES-----|')
 5110 FORMAT(56X,'MISSING',3X,'LOWER',3X,'UPPER')
 5111 FORMAT(58X,'FLAG',4X,'BOUND',3X,'BOUND')
 5200 FORMAT(7X,A4,3X,40('.'),1X,F8.1,',',F7.1,',',F7.1)

 5210 FORMAT( /14X,56('*'),
     & /14X,'***  METSCI Data Processing Finished UN-successfully ***',
     & /14X,'********************************************************')

 5220 FORMAT( /14X,56('*'),
     & /14X,'***   METSCI Data Processing Finished Successfully   ***',
     & /14X,'********************************************************')

 5400 FORMAT(/5X,'THERE IS NO AUDIT TRAIL FOR ',A12)
 5500 FORMAT('     SURFACE')
 5501 FORMAT(I4,' - ',I4,'M')
 5502 FORMAT('     > ',I4,'M')
 5600 FORMAT(8X,A12)
C
C-----------------------------------------------------------------------
C *** END OF SUBROUTINE
      END

