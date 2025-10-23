INTEGER FUNCTION SCIPGetFieldTableSize( UserID,FieldX,ClassData,mTable,mCol,mRow )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldTableSize

INTEGER,                INTENT( IN  ) :: UserID
TYPE( SCIPPlotFieldT ), INTENT( IN  ) :: FieldX
REAL, DIMENSION(*),     INTENT( IN  ) :: ClassData
INTEGER,                INTENT( OUT ) :: mTable,mCol,mRow

INTEGER, EXTERNAL :: GetFieldTableSizeF

SCIPGetFieldTableSize = GetFieldTableSizeF( UserID,FieldX,ClassData,mTable,mCol,mRow )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetFieldTable( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table )

USE field_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldTable

INTEGER,                              INTENT( IN )    :: UserID
TYPE( SCIPPlotFieldT ),               INTENT( INOUT ) :: FieldX
REAL,                   DIMENSION(*), INTENT( IN    ) :: ClassData
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: TableTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: ColTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: RowTitle
INTEGER,                DIMENSION(*), INTENT(   OUT ) :: Table

INTEGER, EXTERNAL :: GetFieldTableF

SCIPGetFieldTable = GetFieldTableF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetFieldTableExtended( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )

USE field_fd
USE charT_fd
USE domain_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldTableExtended

INTEGER,                              INTENT( IN )    :: UserID
TYPE( SCIPPlotFieldT ),               INTENT( INOUT ) :: FieldX
REAL,                   DIMENSION(*), INTENT( IN    ) :: ClassData
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: TableTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: ColTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: RowTitle
INTEGER,                DIMENSION(*), INTENT(   OUT ) :: Table
TYPE( spatialT ),                     INTENT(   OUT ) :: Domain

INTEGER, EXTERNAL :: GetFieldTableExtendedF

SCIPGetFieldTableExtended = GetFieldTableExtendedF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )

RETURN
END
