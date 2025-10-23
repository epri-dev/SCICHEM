      SUBROUTINE SetUA( lunUA,lunOutUA,tAdj,yr1,mo1,day1,yr2,mo2,day2,
     $                  ID )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/UA1.INC'
      INCLUDE '../AERMET/UA2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER lunUA, lunOutUA, tAdj
      INTEGER yr1, mo1, day1
      INTEGER yr2, mo2, day2

      CHARACTER(*) ID

      INTEGER UAARG

      INTEGER, EXTERNAL :: JULIAN

      DEV10  = lunUA
      DEV12  = lunOutUA

      UALST  = 0. !tAdj ** UTC **
      UAYR1  = yr1
      UAGMO1 = mo1
      UAGDY1 = day1
      UAYR2  = yr2
      UAGMO2 = mo2
      UAGDY2 = day2

      UALOC1 = TRIM(ID)
      UAFMT  = 'FSL'

      UAARG = JULIAN(UAYR1,UAGMO1,UAGDY1)
      CALL CHROND('UPPERAIR  ',UAYR1,UAARG,UADAY1)

      UAARG = JULIAN(UAYR2,UAGMO2,UAGDY2)
      CALL CHROND('UPPERAIR  ',UAYR2,UAARG,UADAY2)

C---- Clear the work buffers of previous values

      CALL FLWRK1
      CALL FLWRK2
      CALL FLIWK1
      CALL FLIWK2

      STATUS(2,4) = 2 !To read UA DATA
      STATUS(1,2) = 2 !For error messages
      STATUS(1,1) = 2 !For report

      RETURN
      END