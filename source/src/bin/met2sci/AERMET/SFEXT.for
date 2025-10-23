      SUBROUTINE SFEXT (ISTAT)
C=====================================================================**
C          SFEXT Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  This subroutine is the main driving routine to retrieve
C             the surface observation data and write it to a disk file
C             for the QA program.
C
C   Called by: SFPATH
C
C   Initial release: December 15, 1992
C
C   Programmed by: Pacific Environmental Services, Inc. (PES)
C                  Research Triangle Park, NC
C
C   Revision history:
C
C       1/2010 - MACTEC
C         - Print message if data was likely formatted based on the valid 
C           date range for the format
C         - Print the number of WS records flagged as ASOS based on 
C           ASOS commission date.  For all but TD3280, it is the number
C           observations in the file.  For TD3280, it is the number of 
C           wind speed records.  A single record contains all Ws obs
C           for a day.
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER SFARG, NCHREC, ISTAT, JULIAN

      INTEGER NHR, RDSZ, RDPOS, MXBLK, I, K, IPOSN, SFBFST

      INTEGER GMT2LST                                                   ! ! dtb005 01137

      REAL STNLON

      LOGICAL HUSWO, ISHD                                               ! ! dtb009 01165
                                                                        ! ! dtb005 01137
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      DATA PATH/'SURFACE   '/, LOC/' SFEXT'/

      DATA ISHD /.FALSE./                                               ! ! dtb005 01137
      DATA HUSWO /.FALSE./                                              ! ! dtb009 01165

      GMT2LST = SFLST                                                   ! ! dtb005 01137
      KOUNT = 0
      ISTAT = 0

C     NHR       number of hours to read
C     IHR       loop index
C     IVBL      loop index
C     ISTAT     retrieval status returned from GETSFC
C                0 = no errors; 1 = error encountered
C     SFARG     an integer variable to use for whatever purpose

C-----------------------------------------------------------------------
C---- Write the standard headers to the message file
      MESS =  BLNK80
      WRITE(MESS,2100)
      CALL ERRHDL(0,PATH,'I40',LOC,MESS)


C---- Write the headers to the output file
      WRITE(DEV21,2101)
      WRITE(DEV21,2102)


C---- Check the surface format and set the data parameters
C             - CD144 for card image data in NCDC CD-144 format
C             - SCRAM for compressed 28 character format from SCRAM
C             - SAMSON for SAMSON CD-ROM data
C             - 3280 for NCDC TD-3280 tape format
C        SFBLKF = number of records per block, a user input; this value
C                 is generally equal to 1 when data are read from disk.
C        NCHREC = number of characters per record
C        SFBLK  = block type: FB=fixed, VB=variable
C        RDSZ   = size of block to read; equal to NCHREC for disk reads
C        MXBLK  = maximum block size; equal to NCHREC for disk reads
C        RDPOS  = position to start reading logical records

C     Fill the ASOS Commissioning/Tower height array with the data

      IF(INDEX(SFFMT,'SCRAM') .NE. 0) THEN
         NHR    = 1
         SFBLK  = 'FB'
         NCHREC = 28
         RDSZ   = 28
         MXBLK  = NCHREC
         RDPOS  = MXBLK

      ELSEIF(INDEX(SFFMT,'ISHD') .NE. 0)THEN                             ! ! dtb005 01137
         ISHD = .TRUE.                                                   ! ! dtb005 01137

      ELSEIF(INDEX(SFFMT,'CD144') .NE. 0) THEN
         NHR    = 1
         SFBLK  = 'FB'
         NCHREC = 80
         RDSZ   = 80
         IF (SFBLKF.EQ.0) THEN
            MXBLK = 8000
         ELSE
            MXBLK = SFBLKF * NCHREC
         ENDIF
         RDPOS = MXBLK

      ELSEIF(INDEX(SFFMT,'HUSWO') .NE. 0) THEN                           ! ! dtb009 01165
         NHR    = 1                                                      ! ! dtb009 01165
         SFBLK  = 'FB'                                                   ! ! dtb009 01165
         NCHREC = 140                                                    ! ! dtb009 01165
         RDSZ   = NCHREC                                                 ! ! dtb009 01165
         MXBLK  = NCHREC                                                 ! ! dtb009 01165
         RDPOS  = MXBLK                                                  ! ! dtb009 01165
         HUSWO  = .TRUE.                                                 ! ! dtb009 01165

C        Process the first record to identify the HUSWO variables        ! ! dtb009 01165
         CALL SETHUS (ISTAT)                                             ! ! dtb009 01165
         IF( ISTAT .EQ. 1 )THEN                                          ! ! dtb009 01165
            RETURN                                                       ! ! dtb009 01165
         ENDIF                                                           ! ! dtb009 01165


      ELSEIF(INDEX(SFFMT,'SAMSON') .NE. 0) THEN
         NHR    = 1
         SFBLK  = 'FB'
         NCHREC = 140
         RDSZ   = NCHREC
         MXBLK  = NCHREC
         RDPOS  = MXBLK

C------- Read the first two records; these contain station,
C        date and information on the variables contained
C        in the file                                            ---- CALL SETSAM
         CALL SETSAM (ISTAT)
         IF( ISTAT .EQ. 1 )THEN
            RETURN
         ENDIF

      ELSEIF(INDEX(SFFMT,'3280') .NE. 0) THEN
         NHR = 24
         IF(INDEX(SFFMT,'FB') .NE. 0) THEN
            SFBLK  = 'FB'
            NCHREC = 318
            RDSZ   = 318
            IF (SFBLKF.EQ.0) THEN
               MXBLK = 6360
            ELSE
               MXBLK = SFBLKF * NCHREC
            ENDIF
            RDPOS = MXBLK
         ELSE
            SFBLK  = 'VB'
            NCHREC = 1230
            MXBLK  = 12000
            RDSZ   = 0
            RDPOS  = 0
         ENDIF
      ENDIF


C---- Initialize the work arrays.

      CALL FLWRK1
      CALL FLWRK2
      CALL FLIWK1
      CALL FLIWK2

c     Initialize the concatenated variables

      SFQA1(34,1) = INT(9999/100)                          !  34 TSKC   ! ! dtb120 02064
      SFQA1(34,2) = 9999 - SFQA1(34,1)*100                              ! ! dtb120 02064
      SFOBS(1,34) = 9999                                                ! ! dtb120 02064

c     Variables 35 through 40 are ASOS sky condition and layer height     ! dtb120 02064
c     for six layers ALC1 through ALC6.  We initialize the first layer    ! dtb120 02064
c     to missing.  The remaining layers are meaningful only if the first  ! dtb120 02064
c     layer is non-missing; consequently, these layers (variables 36-40)  ! dtb120 02064
c     are initialized to clear with an unlimited height.                  ! dtb120 02064


      SFQA1(35,1) = INT(9999/1000)                         !  35 ALC1     ! dtb120 02064
      SFQA1(35,2) = 9999 - SFQA1(35,1)*1000                             ! ! dtb120 02064
      SFOBS(1,35) = 9999                                                ! ! dtb120 02064

      DO K = 36, 40                                                     ! ! dtb120 02064
         SFQA1(K,1) = 09                         !                        ! dtb120 02064
         SFQA1(K,2) = 999                        !                        ! dtb120 02064
         SFOBS(1,K) = 00300                                             ! ! dtb120 02064
      ENDDO                                                             ! ! dtb120 02064


C---- Compute the beginning and ending chronological days to extract
      SFARG = JULIAN(SFYR1,SFGMO1,SFGDY1)
      CALL CHROND(PATH,SFYR1,SFARG,SFDAY1)

      SFARG = JULIAN(SFYR2,SFGMO2,SFGDY2)
      CALL CHROND(PATH,SFYR2,SFARG,SFDAY2)

C---- If there was a problem determining either chronological day,
C     return to the calling program

      IF( SFDAY1.LT.0  .OR.  SFDAY2.LT.0 )THEN
         ISTAT = 1
         RETURN
      ENDIF


C---- Check the status of the buffer and retrieve the available
C     observations within the station/date window                       ---- CALL GETSFC
C     (STATUS = 2 ==> keyword seen without errors)

      IF(STATUS(3,4) .GE. 2) THEN
         iposn = index(sflon,'W')+index(sflon,'w')+
     &           index(sflon,'E')+index(sflon,'e')
         call stonum(sflon(1:iposn-1),len(sflon(1:iposn-1)),
     &               stnlon,i)
         if( index(sflon,'E')+index(sflon,'e') .GT. 0 )THEN
            stnlon = -stnlon
         endif

         IF( .NOT. ISHD )THEN                                
            IF( (STNLON .GT. 0.0 .AND. GMT2LST .GT. 0) .OR.  
     &          (STNLON .LT. 0.0 .AND. GMT2LST .LT. 0) )THEN 
               IF( ABS(GMT2LST) .GE. 2 )THEN
C                 Fatal error - the adjustment can be no more
C                 than 1 for the SURFACE path, since surface
C                 data are in local time, except for ISHD;
C                 allow time adjustment up to 1 hour for
C                 applications with SURFACE and ONSITE data
C                 that span across time zone boundary.
                  MESS =  BLNK80
                  ECODE = 'E06'
                  WRITE (MESS,6050 ) GMT2LST
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF
            ENDIF

         ELSEIF( ISHD )THEN
            IF( ABS(nint(stnlon/15.0) - gmt2lst) .GT. 1 )THEN
               MESS =  BLNK80
               ECODE = 'W06'
               WRITE (MESS,6000 ) SFLON
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 0
            ENDIF
         ENDIF 


         IF( ISTAT .EQ. 0 )THEN
            IF( ISHD )THEN                                                 ! ! dtb005 01137
               CALL RDISHD(KOUNT, ISTAT)                                   ! ! dtb005 01137

            ELSEIF( HUSWO )THEN                                            ! ! dtb009 01165
               CALL RDHUSW(KOUNT, ISTAT)                                   ! ! dtb009 01165

            ELSE
               SFBFST = 1
               CALL GETSFC(ISTAT,NCHREC,KOUNT,RDSZ,RDPOS,MXBLK,
     &                     NHR)
            ENDIF                                                          ! ! dtb005 01137

C           Adjust the time of the hourly weather observations; the  
C           adjustment may be to convert from GMT or it may be for  
C           other reasons   

            IF( GMT2LST .NE. 0 )THEN  ! Remap to Local Standard Time (LST)
               CALL SFQATM(GMT2LST)
            ENDIF
         ENDIF
      ENDIF

C---- No more data

      MESS =  BLNK80
      WRITE(MESS,600) KOUNT
      CALL ERRHDL(0,PATH,'I49',LOC,MESS)

C     print number of ASOS observations
      IF( NADJASOS .GT. 0 )THEN              
         WRITE(MESS,620) NADJASOS            
         CALL ERRHDL(0,PATH,'I48',LOC,MESS)  
      ENDIF                                  

C---- If no data were retrieved, then set the flag indicating a problem
      IF( KOUNT .EQ. 0 )THEN
         ISTAT = 1
      ENDIF

      RETURN


C---- Format statments

  600 FORMAT(I6,' SURFACE records extracted')
  620 FORMAT(' # ASOS wind data records based on commission date: ',I6 )

 2100 FORMAT(' *** SURFACE OBSERVATION EXTRACTION ***')
 2101 FORMAT('*  SURFACE OBSERVATION EXTRACTION' )
 2102 FORMAT('*** EOH: END OF SURFACE EXTRACT HEADERS')
 6000 FORMAT(' ISHD time adjustment may be incorrect for Longitude ',A8,
     &       '; ISHD data are reported in GMT.')
 6050 FORMAT(' Time adjustment for hrly SURFACE obs > 1hr (=',I2,
     &       '); non-ISHD data in local time; up to 1hr allowed.')
      END

