C---- MAIN2.INC

      INTEGER   IC1,IC2,STATUS,JBSTAT,UASTAT,SFSTAT,OSSTAT,MRSTAT
      INTEGER   MPSTAT
      INTEGER   STABLK
      CHARACTER PATHWD*10
      LOGICAL   SETERR, RUNERR

      COMMON /BANK4/ PATHWD(6)
      COMMON /BANK5/ IC1(30),IC2(30),STATUS(6,40),JBSTAT,UASTAT,
     1               SFSTAT,OSSTAT,MRSTAT,MPSTAT,STABLK(6)
      COMMON /LBANK/ SETERR, RUNERR

C       PATHWD       Data processing pathway names (JOB, UPPERAIR,
C                    SURFACE,ONSITE,MERGE,METPREP)
C
C       IC1          Defines beginning columns of fields
C                    on user input setup data-
C       IC2          Defines ending columns of fields
C                    on user input setup data
C
C       STATUS       Tracks progress made in reading and processing
C                    keywords that might are encountered on each
C                    pathway. The number of elements in the array
C                    equals (# paths) * (# keywords (NKEYWD=40)).
C
C                      0 = keyword not seen
C                      1 = keyword seen, errors detected
C                      2 = keyword seen, no errors detected
C                      3 = keyword seen, no errors detected
C                          (3 is used only with ASCII/EBCDIC)
C
C       STABLK       Same as STATUS, but for pathways
C
C       JBSTAT,      Status words that are set based on final
C       UASTAT,      Assessment of user defined setup data.
C       SFSTAT,        -1  error condition set for this path
C       OSSTAT          0  null pathway - no processing
C                       1  extract data only
C                       2  QA data only
C                       3  extract and QA, but do not merge
C                       4  merge only
C                       5  extract and merge, but do not QA
C                       6  QA and merge only
C                       7  extract, QA and merge
C                     (5,6, and 7 are combinations that are prohibited)
C
C       MRSTAT       Merge status
C                       0  no merge (initial state)
C                       2  merge subroutine called
C       MPSTAT       Met processing status
C                       -1 errir condition
C                        0 initial value assigned, indicates no error
C
C       SETERR       Logical identifying if there was a SETup ERRor
C       RUNERR       Logical identifying if there was a RUNtime ERRor

