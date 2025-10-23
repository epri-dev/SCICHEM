      SUBROUTINE OSDTCD( KOUNT,CARD,NRDS,ISTAT )
C=====================================================================**
C          OSDTCD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the definitions of the variable names on the
C               READ keyword
C
C     Called by:  VARCRD
C
C     Calls to:   GETWRD
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*)
      INTEGER  ISTAT,NFLD,NUM,NRDS,ITEST
      INTEGER  I, J, K

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'WORK1.INC'

C        ISTAT         Process status 1 = error in processing
C                                     2 = processing OK
C        CARD          Runstream record with list of variable names
C        NFLD          Number of variables defined on keyword
C        NUM           Number of variables already defined for this
C                      scalar read
C        NRDS          Read number associated with this READ keyword

C---- Data Initializations

      PATH = 'ONSITE'
      LOC  = 'OSDTCD'
      ISTAT = 0


C-- 1. Define number of data fields, NFLD, for this record

      NFLD = NWORDS - 2
      IF( NFLD .LE. 0 )THEN
         ISTAT = 1
         RETURN
      ENDIF


C-- 2.Define number of variables already associated with
C     this scalar read, and will be (if all goes well), and
C     read number for this READ keyword
C     OSMRDS = maximum number times the READ keyword is processed
C     This check is probably redundant -- see the code just before
C     the call to this subroutine in subr.VARCRD

      IF( NRDS.GT.OSMRDS )THEN
         ISTAT = 1
         RETURN
      ENDIF

      NUM = OSDNUM(NRDS)
      OSDNUM(NRDS) = OSDNUM(NRDS) + NFLD

C-- 3.Test total number now associated with this READ keyword
C     OSMDAT = Maximum number of variables allowed in any one read

      IF( OSDNUM(NRDS).GT.OSMDAT )THEN
         MESS =  BLNK80
         ECODE = 'E04'
         WRITE( MESS,1000 ) NRDS, OSMDAT
1000     FORMAT(' Too many variable names for READ # ',I2.2,
     &          '; MAX =',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C-- 4.Decipher variable list
C      I  = position in the input list
C      K  = position on the record

      OUTER_LOOP: DO K=3,2+NFLD
         I = NUM + K - 2

         WRITE( BUF08(1),1500 ) NRDS,K
1500     FORMAT( 'DAT',I2.2,':',I2.2)
         BUF04(1) = BLNK04
         CALL GETWRD( K,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ITEST )
         ISTAT = ITEST
         IF( ITEST .EQ. 1 )THEN
            MESS =  BLNK80
            ECODE = 'E07'
            WRITE( MESS,1600 ) BUF04(1)
1600        FORMAT(' Error from S.GETWRD: ONSITE variable name = ',A)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
C ---       Check for length of CARD = 132; variable name may have been
C           truncated.  If so, issue additional message for QA purposes
            IF( LEN_TRIM(CARD) .GT. 132 )THEN
               MESS =  BLNK80
               ECODE = 'E07'
               WRITE( MESS,1700 )
1700           FORMAT(' Max record length for AERMET runstream ',
     &                'file is 132; ONSITE variable name may have ',
     &                'been truncated.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ENDIF   
            ISTAT = 1
            RETURN
         ENDIF

C------- Search for match on variable name

         DO J=1,14
            IF( BUF04(1).EQ.VNAMES(J) )THEN
               IF( J .EQ. 3 )THEN
                  OSMIX = .TRUE.
               ENDIF
               IF( I.LE.OSMDAT )THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               ENDIF
C ---          Check for parameters that are not supported 
C              by AERMET and issue warning message
C*******************Allow HFLX and ZOHT *********               IF( J.EQ.1 .OR. J.EQ.4 .OR. J.EQ.5 .OR.
               IF( J.EQ.5 .OR.
     &                                     J.GT.9 )THEN
C ---             HFLX, Z0HT, SAMT, DT02, DT03, US01, US02, 
C                 and US03 are not used (see Table B-3a)
                  MESS =  BLNK80
                  ECODE = 'W19'
                  WRITE( MESS,1800 ) VNAMES(J)
1800              FORMAT(' Specified ONSITE variable is currently not ',
     &                   'used by AERMET: ', A )
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ENDIF
               CYCLE OUTER_LOOP
            ENDIF
         ENDDO

         DO J=30,34
            IF( BUF04(1).EQ.VNAMES(J) )THEN

               IF( I.LE.OSMDAT )THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               ENDIF
C ---          Check for parameters that are not supported 
C              by AERMET and issue warning message
               IF( J.EQ.33 )THEN
C ---             ONSITE Ceiling Height (CLHT) not used
                  MESS =  BLNK80
                  ECODE = 'W19'
                  WRITE( MESS,1800 ) VNAMES(J)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ENDIF
               CYCLE OUTER_LOOP
            ENDIF
         ENDDO

         DO J=52,56
C           Date fields
            IF( BUF04(1).EQ.VNAMES(J) )THEN

               IF( I.LE.OSMDAT )THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               ENDIF
              CYCLE OUTER_LOOP
            ENDIF
         ENDDO

C------- Decode the tower level from the variable name

         BUF04(2) = BLNK04
         BUF04(2) = BUF04(1)(1:2)
         READ( BUF04(1)(3:4),2000,IOSTAT=IOFLAG ) POSINX
2000     FORMAT( I2 )

C------- Check status of decode for the tower level

         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2500 ) BUF04(1),NRDS
2500        FORMAT(' Error decoding READ variable: ',A4,
     &             ' for READ # ',I2.2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            CYCLE OUTER_LOOP
         ENDIF

C------- Check the value to be sure it does not exceed the maximum
C        number of allowable tower levels (OSML)

         IF( POSINX.LE.0 .OR. POSINX.GT.OSML )THEN
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,3000 ) POSINX, BUF04(1)
3000        FORMAT(' Level index (',I8,') for variable ',A4,
     &             ' is invalid.')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            CYCLE OUTER_LOOP
         ENDIF

C------- Test for match with variable name

         DO J=15,29
            IF( BUF04(2).EQ.VNAMES(J) )THEN

               IF( I.LE.OSMDAT )THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = POSINX
               ENDIF
C ---          Check for parameters that are not supported 
C              by AERMET and issue warning message
               IF( J.EQ.17 .OR. J.EQ.18 .OR. J.EQ.20 .OR.
     &             J.EQ.24 .OR. J.EQ.27 .OR. J.EQ.28 .OR.
     *                                       J.EQ.29 )THEN
C ---             SEnn, SVnn, SUnn, VVnn, V1nn, V2nn, and
C                 V3nn are not used (see Table B-3b)
                  MESS =  BLNK80
                  ECODE = 'W19'
                  WRITE( MESS,3500 ) VNAMES(J)(1:2),POSINX
3500              FORMAT(' Specified ONSITE variable is currently not ',
     &                   'used by AERMET: ', A2,I2.2 )
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ENDIF
               CYCLE OUTER_LOOP
            ENDIF
         ENDDO

C------- Problems, no match found

         MESS =  BLNK80
         ECODE = 'E06'
         WRITE( MESS,4000 ) BUF04(1), NRDS
4000     FORMAT(' No match found on variable name (',A4,
     &           ') for READ keyword # ',I2.2)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1

      ENDDO OUTER_LOOP

      IF( ISTAT.NE.1 ) ISTAT = 2

      RETURN
      END

