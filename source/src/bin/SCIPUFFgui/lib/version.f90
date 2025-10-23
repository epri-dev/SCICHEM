!***********************************************************************
!               GetGUIVersion
!***********************************************************************
INTEGER FUNCTION GetGUIVersion()

USE GUIversion_fd

IMPLICIT NONE

GetGUIVersion = SCIPGUI_VERSION

RETURN
END

!***********************************************************************
!               GetGUIToolVersion
!***********************************************************************
INTEGER FUNCTION GetGUIToolVersion()

USE GUIversion_fd

IMPLICIT NONE

GetGUIToolVersion = SCIPTOOL_INTERFACE_VERSION

RETURN
END

!***********************************************************************
!               GetGUIVersionString
!***********************************************************************
CHARACTER(*) FUNCTION GetGUIVersionString()
USE GUIversion_fd

IMPLICIT NONE

CHARACTER(32) string

INTEGER i,j

WRITE(string,*)0.001*SCIPGUI_VERSION
string = ADJUSTL(string)

i = LEN(TRIM(string))
j = MAX(1,(i-1))

DO WHILE( i > 0 .AND. string(i:i) == '0' .AND. &
                      string(j:j) /= '.' )
  string(i:i) = ' '
  i = i - 1
  j = MAX(1,(i-1))
END DO

IF( string(1:1) == '.' )string ='0'//TRIM(string)

GetGUIVersionString = TRIM(string)//TRIM(SCIPGUI_VERSION_TAG)

RETURN
END
!***********************************************************************
!               CheckVersion
!***********************************************************************
SUBROUTINE CheckVersion( iwnd_db,filename,lok )

USE GUIversion_fd
USE SCIAPIversion_fd
USE errorParam_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db
CHARACTER(*),         INTENT( IN    ) :: filename
LOGICAL,              INTENT( INOUT ) :: lok

INTEGER jversion

LOGICAL, EXTERNAL :: hasError

IF( .NOT.lok )RETURN

lok = .FALSE.

CALL read_version( filename,jversion )
IF( hasError() )GOTO 9999

lok = jversion/100 <= scipuff_version/100
lok = lok .AND. jversion/100 >= SCIAPI_00_VERSION/100
IF( .NOT.lok )THEN
  CALL version_as_string( jversion,string1 )
  CALL version_as_string( scipuff_version,string2 )
  WRITE(string3,*)'Current= '//TRIM(string2)//' : Project ='//TRIM(string1)
  CALL SetError( VN_ERROR,'Incompatible project version',string3,' ','CheckVersion' )
END IF

9999  CONTINUE
IF( hasError() )CALL ShowErrorMessage( iwnd_db )

RETURN
END
!***********************************************************************
!               read_version
!***********************************************************************
SUBROUTINE read_version( filename,jversion )

USE errorParam_fd
USE files_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: filename
INTEGER,      INTENT( OUT ) :: jversion

INTEGER ios

CHARACTER(128) eString

OPEN(UNIT=lun_prj,FILE=filename,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( OP_ERROR, &
                'Error opening SCIPUFF project file', &
                 eString, &
                'Make sure file exists', &
                'read_version' )
  GOTO 9999
END IF

!------ read version number of project file

READ(lun_prj,IOSTAT=ios) jversion

IF( ios /= 0 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( OP_ERROR, &
                'Error reading version number on project file', &
                 eString, &
                'Make sure file is a valid project file', &
                'read_version' )
  GOTO 9999
END IF

9999 CONTINUE

CLOSE(UNIT=lun_prj,IOSTAT=ios)

RETURN
END
