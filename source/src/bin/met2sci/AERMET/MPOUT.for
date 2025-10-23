      SUBROUTINE MPOUT( TEST )
C=====================================================================**
C          MPOUT Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To write the atmospheric boundary layer parameters
C               required by the user-selected dispersion model.
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

C---- Local variables


      IMPLICIT NONE
      
      INTEGER      TEST,OUTCNT,IEND
      INTEGER      J, ILVL, II, JJJ
      REAL         WDSPD,WDDIR,SFTEMP,S1,S2
      LOGICAL      GOTOS
      CHARACTER*7  WSADJ

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

      DATA OUTCNT/0/

C.......................................................................

C---- Initialize values
      PATH = 'METPREP'
      LOC  = ' MPOUT'

C---- Write records of current days surface data to output file,
C        one record for each hour

      IF(MDSTAT .EQ. 1)THEN       !  Standard output                     ! dtb #100 01249

         DO J = 1, 24

            WSADJ = '       '

C----       set ws adjust flag for end of record based on ASOS flag in
C           merge file and ASOS WS adjustment flag
            IF (ISASOS24(J) .EQ. 'N' .OR. ISASOS24(J) .EQ. 'n'  
     &          .OR. .NOT. ASOS_ADJ .OR. HRWINDOS(J))THEN 
C              Non-ASOS data, or OS winds used, or ASOS adjustment "turned off";
C              set flag for no-adjustment, 'NAD'
               WSADJ(1:3) = 'NAD'
            ELSEIF( (ISASOS24(J) .EQ. 'A' .OR. ISASOS24(J) .EQ. 'a') 
     &               .AND. (HRWINDASOS(J) .OR. HRWINDNWS(J)) )THEN
C              ASOS wind data used, with truncation adjustment;
C              set flag for adjustment, 'ADJ'
               WSADJ(1:3) = 'ADJ'
            ELSE
C              No wind data available; set flag to 'NAD'
               WSADJ(1:3) = 'NAD'
            ENDIF
            
C----       Add data source to ws adjust flag
            IF (HRWINDOS(J)) THEN
C              On-site wind data used '-OS'
               WSADJ = WSADJ(1:3)//'-OS '
            ELSEIF (HRWINDASOS(J)) THEN
C              1-min ASOS wind data used, '-A1'
               WSADJ = WSADJ(1:3)//'-A1 '
            ELSEIF (HRWINDNWS(J)) THEN
C              Standard surface data used, '-SFC'
               WSADJ = WSADJ(1:3)//'-SFC'
            ELSE
C              No wind data available; set flag to blanks
               WSADJ = WSADJ(1:3)//'    '
            ENDIF

            WRITE( DEV80,1800 ) MPYR,MPCMO,MPCDY,MPJDY,J,
     &           HFLUX(J),
     &           USTAR(J),WSTAR(J),VPTG(J),ZICONV(J),ZIMECH(J),MOL(J),
     &           Z0(J),BOWEN(J),ALBEDO(J),WSPD(J),WDIR(J),ZREF(J),
     &           T(J),ZTREF(J), IPCODE(J), PAMT(J), RH(J),P(J),CCVR(J),  ! dtb #300 03071
     &           WSADJ

 1800       FORMAT( 3(I2,1X), I3,1X, I2,1X, F6.1,1X, F6.3,1X, F6.3,1X,
     &           F6.3,1X, 2(F5.0,1X),
     &           F8.1,1X, F7.4,1X, F6.2,1X, F6.2,1X, F7.2,1X, F6.1,
     &           3(1X,F6.1), 1X,I5, 1X,F6.2, 2(1X, F6.0), 1X, I5,        ! dtb #011 01180
     &           1X, A7)
            OUTCNT = OUTCNT + 1

         ENDDO

      ELSEIF(MDSTAT .EQ. 2)THEN   !  Debug(1) output                     ! dtb #100 01249

         WRITE(DEV80, 1810) MPCMO, MPCDY, MPYR
 1810    FORMAT(/' The following is debug(1) output for ', 2(I2.2,'/'),
     &           I2.2/)

         WRITE(DEV80, 1812)

 1812 FORMAT(' JDY  HR    PBL  FLAG CCVR  DRYB    ALBEDO  ACRIT   ANGLE
     &      GRAD    NRAD    HFLUX '/)


         DO J = 1, 24
            WRITE(DEV80, 1814)MPJDY,J, PBL(J), MFLAG(J), CCVR(J), T(J),
     &      ALBEDO(J), ACRT(J), ANGD(J), QR(J), RN(J), HFLUX(J)

 1814 FORMAT( 2I4, 4X, A3, 2I5, F8.0, F8.2, 2F8.1, 2F8.0, F8.1)

            OUTCNT = OUTCNT + 1

         ENDDO                                                           ! dtb #100 01249

      ELSEIF(MDSTAT .EQ. 3)THEN   !  Debug(2) output                     ! dtb #114 02051
         WRITE(DEV80, 1847) MPCMO, MPCDY, MPYR, TSR, TSS
 1847    FORMAT(/' The following is debug(2) output for ', 2(I2.2,'/'),
     &           I2.2/' Time of sunrise (TSR) = ', F4.1/
     &                ' Time of sunset  (TSS) = ', F4.1//)

         WRITE(DEV80, 1848)

 1848 FORMAT(' JDY  HR   PBL CCVR GRAD   NRAD FLAG  HFLUX   CBL    WSPD
     &   SBL   THSTAR   USTAR    MOL '/)


         DO J = 1, 24
            WRITE(DEV80, 1849) MPJDY, J, PBL(J), CCVR(J), QR(J), RN(J),
     &      MFLAG(J), HFLUX(J), ZICONV(J), WSPD(J), ZIMECH(J),
     &      THSTAR(J), USTAR(J), MOL(J)

 1849 FORMAT( 2I4, 3X, A3, I4, 2F7.0, I4, 2(F7.1, F7.0), 2F8.4, F8.1)    ! dtb #114 02051

            OUTCNT = OUTCNT + 1

         ENDDO                                                           ! dtb #114 02051

      ELSEIF(MDSTAT .EQ. 4)THEN   !  Debug(3) output                     ! dtb #114 02051
         WRITE(DEV80, 1850) MPCMO, MPCDY, MPYR
 1850    FORMAT(/' The following is debug(3) output for ', 2(I2.2,'/'),
     &           I2.2/)

         WRITE(DEV80, 1852)
 1852 FORMAT(' JDY  HR    PBL     Z0   ALBEDO BOWEN   WDIR   WSPD  USTAR
     &       MOL   WSTAR '/)


         DO J = 1, 24
            WRITE(DEV80, 1854) MPJDY, J, PBL(J), Z0(J), ALBEDO(J),
     &          BOWEN(J), WDIR(J), WSPD(J), USTAR(J), MOL(J), WSTAR(J)

 1854 FORMAT( 2I4, 4X, A3, F8.3, 2F7.2, F7.0, F7.2, F8.4, F10.1, F8.4)

            OUTCNT = OUTCNT + 1

         ENDDO                                                           ! dtb #114 02051

      ENDIF

      HOUR_LOOP: DO J=1,24

C------- Initialize the logical that indicates if there are any
C        nonmissing onsite data for the hour.
         GOTOS = .FALSE.

C------- The on-site variables to check are:
C        OSVOBS(hour,level,8) = wind direction
C        OSVOBS(hour,level,9) = wind speed
C        OSVOBS(hour,level,7) = ambient temperature
C        OSVOBS(hour,level,2) = sigma theta
C        OSVOBS(hour,level,5) = sigma w

C------- This check will be skipped if there are no on-site data
C        in the data base (OSDATA = .F.)

         IF( OSDATA )THEN
C---------- Check the data - if there is so much as one on-site variable
C           with one nonmissing value, then the on-site data are written
C           to the profile file.

            ILVL = 1
            DO WHILE( ILVL .LE. OSNL  .AND.  (.NOT. GOTOS) )

               IF( ABS(OSVOBS(J,ILVL,8)-OSQA(22,2)).GT.0.01  .OR.
     &             ABS(OSVOBS(J,ILVL,9)-OSQA(23,2)).GT.0.01  .OR.
     &             ABS(OSVOBS(J,ILVL,7)-OSQA(21,2)).GT.0.01  .OR.
     &             ABS(OSVOBS(J,ILVL,2)-OSQA(16,2)).GT.0.01  .OR.
     &             ABS(OSVOBS(J,ILVL,5)-OSQA(19,2)).GT.0.01 )THEN
                  GOTOS = .TRUE.

               ELSE
                  ILVL = ILVL + 1

               ENDIF

            ENDDO

         ENDIF

C------- Check which data to write to the output profile file

         IF( GOTOS )THEN
C---------- There is at least one nonmissing on-site variable somewhere
C           in the five profiles, so use the data for the profile
            IEND = 0
            DO II=1,OSNL
               IF( II.EQ.OSNL) IEND = 1
               WRITE (DEV85,1990) MPYR,MPCMO,MPCDY,J,OSVOBS(J,II,1),
     &                                                         IEND,
     &                OSVOBS(J,II,8),OSVOBS(J,II,9),OSVOBS(J,II,7),
     &                OSVOBS(J,II,2),OSVOBS(J,II,5)
 1990          FORMAT(4(I2,1X), F7.1,1X, I1,1X, F7.1,1X, F8.2,1X,
     &                F8.2,1X, F8.2,1X, F8.2)

            ENDDO
            
            CYCLE HOUR_LOOP

         ELSEIF( .NOT. GOTOS )THEN
C---------- The on-site data are missing for this hour; should
C           NWS data be used?
            IF( SUBSTNWS )THEN

               IEND = 1
               WDDIR  = WDIR(J)
               WDSPD  = WSPD(J)
               SFTEMP = SFOBS(J,46) / 10.0
               S1 = OSQA(16,2)                  ! SIGMA_A missing
               S2 = OSQA(19,2)                  ! SIGMA_W missing

               WRITE (DEV85,1990) MPYR,MPCMO,MPCDY,J,INSTHT(1),IEND,
     &                WDDIR,WDSPD,SFTEMP,S1,S2

C------------- Inform the user (but only if there are on-site data in
C              the data base, OSDATA = .T.)
               IF( OSDATA )THEN
                  MESS =  BLNK80
                  WRITE (MESS,192) J
  192             FORMAT (' NWS data subst''d for ONSITE profile',
     &                    ' at HR: ',I3.2)
                  JJJ = (MPYR*10000) + MPCMO*100 + MPCDY
                  CALL ERRHDL(JJJ,PATH,'I71',LOC,MESS)
               ENDIF

            ELSEIF( .NOT. SUBSTNWS )THEN
C------------- All required elements of on-site data are missing and the
C              user did not specify to substitute NWS data.  Write a
C              full on-site data profile.
               IEND = 0
               DO II=1,OSNL
                  IF (II.EQ.OSNL) IEND = 1
                  WRITE (DEV85,1990) MPYR,MPCMO,MPCDY,J,OSVOBS(J,II,1),
     &                                                            IEND,
     &                   OSVOBS(J,II,8),OSVOBS(J,II,9),OSVOBS(J,II,7),
     &                   OSVOBS(J,II,2),OSVOBS(J,II,5)
               ENDDO

            ENDIF

         ENDIF
         
      ENDDO HOUR_LOOP

      RETURN
      END

