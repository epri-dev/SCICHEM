MODULE checkErr_fd

  INTEGER, PARAMETER :: DEFAULT_BIT = 0
  INTEGER, PARAMETER :: NOTSET_BIT  = 1
  INTEGER, PARAMETER :: DEFER_BIT   = 2
  INTEGER, PARAMETER :: SPECIAL_NONE    = 0
  INTEGER, PARAMETER :: SPECIAL_DEFAULT = 2**DEFAULT_BIT
  INTEGER, PARAMETER :: SPECIAL_NOTSET  = 2**NOTSET_BIT
  INTEGER, PARAMETER :: SPECIAL_DEFER   = 2**DEFER_BIT
  INTEGER, PARAMETER :: SPECIAL_VALUE   = SPECIAL_DEFAULT + SPECIAL_NOTSET + SPECIAL_DEFER

END MODULE checkErr_fd

MODULE checkErr

  USE checkErr_fd

  INTERFACE SetCheckError

    SUBROUTINE SetCheckErrorSpecial( routine,name,mode )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      INTEGER     ,INTENT (IN ) :: mode
    END SUBROUTINE SetCheckErrorSpecial

    SUBROUTINE SetCheckErrorMessage( routine,name,message )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      CHARACTER(*),INTENT (IN ) :: message
    END SUBROUTINE SetCheckErrorMessage

    SUBROUTINE SetCheckErrorReal( routine,name,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      REAL        ,INTENT (IN ) :: min
      REAL        ,INTENT (IN ) :: max
      REAL        ,INTENT (IN ) :: value
    END SUBROUTINE SetCheckErrorReal

    SUBROUTINE SetCheckErrorDouble( routine,name,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      REAL(8)     ,INTENT (IN ) :: min
      REAL(8)     ,INTENT (IN ) :: max
      REAL(8)     ,INTENT (IN ) :: value
    END SUBROUTINE SetCheckErrorDouble

    SUBROUTINE SetCheckErrorInteger( routine,name,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      INTEGER     ,INTENT (IN ) :: min
      INTEGER     ,INTENT (IN ) :: max
      INTEGER     ,INTENT (IN ) :: value
    END SUBROUTINE SetCheckErrorInteger

  END INTERFACE

  INTERFACE BadValue

    LOGICAL FUNCTION BadValueReal( routine,name,mode,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      INTEGER     ,INTENT (IN ) :: mode
      REAL        ,INTENT (IN ) :: min
      REAL        ,INTENT (IN ) :: max
      REAL        ,INTENT (IN ) :: value
    END FUNCTION BadValueReal

    LOGICAL FUNCTION BadValueDouble( routine,name,mode,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      INTEGER     ,INTENT (IN ) :: mode
      REAL(8)     ,INTENT (IN ) :: min
      REAL(8)     ,INTENT (IN ) :: max
      REAL(8)     ,INTENT (IN ) :: value
    END FUNCTION BadValueDouble

    LOGICAL FUNCTION BadValueInteger( routine,name,mode,min,max,value )
      CHARACTER(*),INTENT (IN ) :: routine
      CHARACTER(*),INTENT (IN ) :: name
      INTEGER     ,INTENT (IN ) :: mode
      INTEGER     ,INTENT (IN ) :: min
      INTEGER     ,INTENT (IN ) :: max
      INTEGER     ,INTENT (IN ) :: value
    END FUNCTION BadValueInteger

  END INTERFACE

  INTERFACE OutOfRange

    LOGICAL FUNCTION OutOfRangeReal( min,max,value )
      REAL        ,INTENT (IN ) :: min
      REAL        ,INTENT (IN ) :: max
      REAL        ,INTENT (IN ) :: value
    END FUNCTION OutOfRangeReal

    LOGICAL FUNCTION OutOfRangeDouble( min,max,value )
      REAL(8)     ,INTENT (IN ) :: min
      REAL(8)     ,INTENT (IN ) :: max
      REAL(8)     ,INTENT (IN ) :: value
    END FUNCTION OutOfRangeDouble

    LOGICAL FUNCTION OutOfRangeInteger( min,max,value )
      INTEGER     ,INTENT (IN ) :: min
      INTEGER     ,INTENT (IN ) :: max
      INTEGER     ,INTENT (IN ) :: value
    END FUNCTION OutOfRangeInteger

  END INTERFACE

  INTERFACE SpecialValue

    LOGICAL FUNCTION SpecialValueReal( mode,value )
      INTEGER     ,INTENT (IN ) :: mode
      REAL        ,INTENT (IN ) :: value
    END FUNCTION SpecialValueReal

    LOGICAL FUNCTION SpecialValueDouble( mode,value )
      INTEGER     ,INTENT (IN ) :: mode
      REAL(8)     ,INTENT (IN ) :: value
    END FUNCTION SpecialValueDouble

    LOGICAL FUNCTION SpecialValueInteger( mode,value )
      INTEGER     ,INTENT (IN ) :: mode
      INTEGER     ,INTENT (IN ) :: value
    END FUNCTION SpecialValueInteger

  END INTERFACE

END MODULE checkErr
