MODULE SCIPversion_fd
  INTEGER,       PARAMETER :: SCIP_API_VERSION = 6500
  CHARACTER(16), PARAMETER :: SCIPTOOL_TAG     = '.010'   !(Update if needed)
  CHARACTER(34), PARAMETER :: SCIPUFF_TAG  = 'SCICHEM Ver 3.4 Build 240715' !(Update if needed)
END MODULE SCIPversion_fd

!*******************************************************************************
!            Get scipuff version number
!*******************************************************************************
INTEGER FUNCTION GetScipuffVersion()

USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER version

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available
  CALL set_version( version )
  GetScipuffVersion = version
END IF

RETURN
END
!*******************************************************************************
!            Get Tool API version number
!*******************************************************************************
INTEGER FUNCTION GetAPIVersion()

USE SCIPversion_fd
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available
  GetAPIVersion = SCIP_API_VERSION
END IF

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version number
!*******************************************************************************
INTEGER FUNCTION GetVersionValue()

USE SCIPversion_fd
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER ishift
INTEGER version

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available

  CALL set_version( version )

  ishift = 4*4 !Bytes * 8(Bits/Byte) / 2

  GetVersionValue = ISHFT(SCIP_API_VERSION,ishift) + version

END IF

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version string
!*******************************************************************************
INTEGER FUNCTION GetVersionString( iflag,charString )

USE SCIPversion_fd
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER, PARAMETER :: HV_API     = 1
INTEGER, PARAMETER :: HV_SCIPUFF = 2

INTEGER iflag
TYPE( char128T )charString

CHARACTER(128) String

CHARACTER(32)  APIstring
CHARACTER(32)  SCIstring

INTEGER        APIversion
INTEGER        SCIPUFFversion

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available

!==== SCIPtool API version

  APIversion = SCIP_API_VERSION

  CALL WriteVersionString( APIversion,APIstring )

!==== SCIPUFF version

  CALL set_version( SCIPUFFversion )
  CALL WriteVersionString( SCIPUFFversion,SCIstring )

  IF( iflag == HV_API )THEN
    String = TRIM(APIstring)//TRIM(SCIPTOOL_TAG)
  ELSE IF( iflag == HV_SCIPUFF )THEN
    String = TRIM(SCIstring)//TRIM(SCIPUFF_TAG)
  ELSE
    String ='T:'//TRIM(APIstring)//TRIM(SCIPTOOL_TAG) &
                  //'-S:'//TRIM(SCIstring)//';'//TRIM(SCIPUFF_TAG)
  END IF
#if defined(_OPENMP)
  String = TRIM(String)//'.OMP'
#endif

  charString%string = String
  GetVersionString  = SCIPsuccess

END IF

RETURN
END
!*******************************************************************************
!            Get maximum length of filenames
!*******************************************************************************
INTEGER FUNCTION GetPathMaxLength()

USE DefSize_fd

IMPLICIT NONE

GetPathMaxLength = PATH_MAXLENGTH

RETURN
END
!*******************************************************************************
!            Set scipuff version number
!*******************************************************************************
SUBROUTINE SetSCIPUFFVersion()

USE scipuff_fi

IMPLICIT NONE

CALL set_version( iversion_code )

RETURN
END
!===============================================================================
!     set_version_string
!===============================================================================
SUBROUTINE set_version_string( cver )

USE SCIMgr_fd

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: cver

INTEGER irv

TYPE( char128T ) charStruct

INTEGER, EXTERNAL :: GetVersionString

irv = GetVersionString( 0,charStruct )

cver = charStruct%string

RETURN
END
!*******************************************************************************
!            WriteVersionString
!*******************************************************************************
SUBROUTINE WriteVersionString( version,string )

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: version
CHARACTER(*), INTENT( OUT ) :: string

INTEGER i,j

WRITE(string,'(F0.3)')0.001*FLOAT(version)
string = ADJUSTL(string)

i = LEN(TRIM(string))
j = MAX(1,(i-1))

DO WHILE( i > 0 .AND. string(i:i) == '0' .AND. string(j:j) /= '.' )
  string(i:i) =' '
  i = i - 1
  j = MAX(1,(i-1))
END DO

IF( string(1:1) == '.' )THEN
  string ='0'//TRIM(string)
END IF

RETURN
END
