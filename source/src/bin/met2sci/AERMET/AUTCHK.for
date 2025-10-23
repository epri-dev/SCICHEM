      SUBROUTINE AUTCHK
C=====================================================================**
C          AUTCHK Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the ONSITE pathway definitions of variables
C               that are to be summarized in the final QA report.
C               It searches the user defined READ keywords and sets the
C               auditing summary to be active only if the variable
C               appears in the 'data map'.
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C----------------------------------------------------------------------

      IMPLICIT NONE

C---- Local variables

      INTEGER  VARID, LEVEL, FLAG, I, J, K

C   VARID  ID of variable
C   LEVEL  Tower level to be associated with vector variable
C   FLAG   Condition flag used to count number of occurrances
C          of a vector variable that is to be audited

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize values

      PATH = 'ONSITE'
      LOC  = 'AUTCHK'

C     ------------------------------------------------------------
C     1.  Loop on allowable scalar variables on the ONSITE pathway
C     ------------------------------------------------------------

      LOOP_VARS: DO VARID=1,14

C------- If audit is requested for this variable, search
C        input data map to see if variable appears.

         IF( OSSAUD(VARID).EQ.0 ) CYCLE LOOP_VARS

C------- Loop on user defined data map for occurrence of variable

         DO I=1,OSDCRD

            DO J=1,OSDNUM(I)
               IF( OSDVAR(I,J,1).EQ.VARID ) CYCLE LOOP_VARS
            ENDDO

         ENDDO

C------- We only get here if audit was turned on for a scalar
C        variable that does not occur within the user defined
C        data map.

         OSSTRA(VARID) = 0
         OSSAUD(VARID) = 0
         MESS =  BLNK80
         ECODE = 'W20'
         WRITE( MESS,1000 ) VNAMES(VARID)
1000     FORMAT(1X,A4,' variable is not defined on READ ',
     &     'keywords; therefore NOT included in QA audit.')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ENDDO LOOP_VARS

C-----Loop on remaining on-site scalar variables

      LOOP_VARS2: DO VARID=30,34

C------- If audit is requested for this variable, search
C        input data map to see if variable appears.

         IF( OSSAUD(VARID).EQ.0 ) CYCLE LOOP_VARS2

C------- Loop on user defined data map for occurrence of variable

         DO I=1,OSDCRD
            DO J=1,OSDNUM(I)
              IF( OSDVAR(I,J,1).EQ.VARID ) CYCLE LOOP_VARS2
            ENDDO
         ENDDO

C------- We only get here if audit was turned on for a scalar
C        variable that does not occur within the user defined
C        data map.

         OSSTRA(VARID) = 0
         OSSAUD(VARID) = 0
         MESS =  BLNK80
         ECODE = 'W20'
         WRITE( MESS,1000 ) VNAMES(VARID)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ENDDO LOOP_VARS2


C     ------------------------------------------------------------
C     2.  Loop on allowable vector variables on the ONSITE pathway
C     ------------------------------------------------------------

      LOOP_VARS3: DO VARID=15,29

C------- If audit is requested for this variable, search
C        input data map to see if variable appears.

         IF( OSSAUD(VARID).EQ.0 ) CYCLE LOOP_VARS3

C------- Set condition flag to zero (counts number of times
C        this vector variable occurs within user defined
C        data map)

         FLAG = 0

C------- Loop on user defined data map for occurrence of variable
         DO K=1,OSDCRD
            DO J=1,OSDNUM(K)
               IF( OSDVAR(K,J,1).EQ.VARID )THEN
                  LEVEL = OSDVAR(K,J,2)
                  OSVAUD(LEVEL,VARID-14) = 1
                  FLAG = FLAG + 1
               ENDIF
            ENDDO
         ENDDO

C------- Check condition flag.  If equal to zero, then vector
C        variable is not within user defined data map.

         IF( FLAG.GT.0 ) CYCLE LOOP_VARS3

C------- We only get here if audit was turned on for a vector
C        variable that does not occur within the user defined
C        data map.

         OSSTRA(VARID) = 0
         MESS =  BLNK80
         ECODE = 'W20'
         WRITE( MESS,1000 ) VNAMES(VARID)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

      ENDDO LOOP_VARS3

      RETURN
      END

