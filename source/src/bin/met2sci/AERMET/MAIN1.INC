C---- MAIN1.INC

C     Revision History:

C          March 2012 - increased size of KEYWRD from 10 to 12 characters
C-----------------------------------------------------------------------

      INTEGER  DEVIN,DEVIO,DEV40,DEV41,DEV50,DEV60,DEV61,DEV70,DEV80
      INTEGER  MRDAY1,MRDAY2,MRYR1,MRYR2,MRGDY1,MRGDY2,DEV75,DEV85,
     1         MRGMO1,MRGMO2,JBYR1,JBYR2,JBGMO1,
     1         JBGMO2,JBGDY1,JBGDY2,JBDAY1,JBDAY2,JBCDY1,JBCDY2

      INTEGER  IRD1, IRD2                                                ! dtb #508 O6299
      REAL     PWELEV(4), SFELEV, ZMSL2USE                               ! dtb #508 O6299
      CHARACTER*6  VERSNO
      CHARACTER*12 KEYWRD
      CHARACTER*96 DISK40,DISK41,DISK42,DISK50,DISK60,DISK80,DISK85
      LOGICAL GOTPWELEV(4), GOTSFELEV
      LOGICAL STNPINFILE

      INTEGER, PARAMETER :: MAXERR=3, NKEYWD=40

C       DEVIN,DEVIO     device numbers of standard input and output
C       DEV40           device number for merged data
C       DEV50           device number for summary report
C       DEV60           device number for messages
C       DEV61           device number for discarded ISHD records
C       DEV70           device number for temporary file of headers
C       DEV75           device number for temporary file of current runstream
C       DEV80           device number for surface output from Stage 3
C       DEV85           device number for profile output from Stage 3
C       DISK40          filename for DEV40
C       DISK50          filename for DEV50
C       DISK60          filename for DEV60
C       DISK80          filename for DEV80
C       DISK85          filename for DEV85
C       VERSNO          AERMET version number
C       MAXERR          maximum number of read/decode errors allowed
C                       on each pathway in extracting / QA data
C       MRDAY1,MRDAY2   start and stop julian days for merge data
C       MRYR1...MRGMO2  start and stop days (gregorian) for merge
C       KEYWRD          keywords used to define and control setup data.

C       PWELEV          Station elevation (m) for data pathway           ! dtb #508 O6299
C                       Read from LOCATION keyword
C       SFELEV          Station elevation read from data file
C                       (ISHD and SAMSON are only formats thus far)
C       GOTPWELEV       Logical indicating that station elevation
C                       was successfully retrieved from the LOCATION
C                       keyword
C       GOTSFELEV       Logical indicating that station elevation was
C                       successfully retrieved from the input data file
C       ZMSL2USE        Station elevation to use in the calculation
C                       of station pressure
C       STNPINFILE      Logical indicating if station pressure is in the
C                       input data

      COMMON /BANK1/ DEVIN,DEVIO,DEV40,DEV41,DEV50,DEV60,DEV61,DEV70,
     1               DEV75,DEV80,DEV85
      COMMON /BANK2/ MRDAY1,MRDAY2,MRYR1,MRYR2,MRGDY1,MRGDY2,
     1               MRGMO1,MRGMO2,JBYR1,JBYR2,JBGMO1,
     1               JBGMO2,JBGDY1,JBGDY2,JBDAY1,JBDAY2,JBCDY1,JBCDY2
      COMMON /NAME1/ KEYWRD(NKEYWD)
      COMMON /NAME2/ DISK40,DISK41,DISK42,DISK50,DISK60,DISK80,DISK85
      COMMON /VERSN/ VERSNO
      COMMON /MAIN04/PWELEV,IRD1,IRD2,                                   ! dtb #508 O6299
     1               SFELEV,GOTPWELEV,GOTSFELEV,ZMSL2USE,STNPINFILE

