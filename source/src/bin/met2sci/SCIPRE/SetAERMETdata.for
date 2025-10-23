      SUBROUTINE SetAERMETdata()

      IMPLICIT NONE

      RETURN
      END
 
      BLOCK DATA

      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/UA1.INC'
      INCLUDE '../AERMET/UA2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/MP1.INC'
      INCLUDE '../AERMET/MP2.INC'
      INCLUDE '../AERMET/BLOCK1.INC'
      INCLUDE '../AERMET/BLOCK2.INC'

      END

!==============================================================================

      SUBROUTINE SetDeviceUnits( lunRpt,lunMsg )

      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/UA1.INC'
      INCLUDE '../AERMET/UA2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/MP1.INC'
      INCLUDE '../AERMET/MP2.INC'
      INCLUDE '../AERMET/BLOCK1.INC'
      INCLUDE '../AERMET/BLOCK2.INC'

      INTEGER lunRpt, lunMsg

!---- Report and message file units

      lunRpt = DEV50
      lunMsg = DEV60

      JBSTAT = 1
      OSSTAT = 0
      MRSTAT = 0

      RETURN
      END
