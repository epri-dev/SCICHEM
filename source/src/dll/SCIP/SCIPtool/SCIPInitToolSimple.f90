!*******************************************************************************
!            Initialize SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SCIPInitToolSimple( UserID,UserCall,request,cstr_INIfile )

USE charT_fd
USE limitT_fd
USE basic_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitToolSimple

INTEGER,              INTENT( IN    ) :: UserID
INTEGER(LEN_ADDRESS), INTENT( IN    ) :: UserCall
INTEGER,              INTENT( INOUT ) :: request
TYPE( fileNameT ),    INTENT( IN    ) :: cstr_INIfile

INTEGER, EXTERNAL :: SimpleInitTool

SCIPInitToolSimple = SimpleInitTool( UserID,UserCall,request,cstr_INIfile )

RETURN
END
