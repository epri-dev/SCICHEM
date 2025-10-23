!*******************************************************************************
!            Write Project Ctrl
!*******************************************************************************
INTEGER FUNCTION SCIPWriteCtrlF( UserID,ctrlIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteCtrlF

INTEGER,        INTENT( IN ) :: UserID
TYPE( pctrlT ), INTENT( IN ) :: ctrlIO

INTEGER, EXTERNAL :: WriteCtrlF

SCIPWriteCtrlF = WriteCtrlF( UserID,ctrlIO )

RETURN
END
!*******************************************************************************
!            Write Project Start
!*******************************************************************************
INTEGER FUNCTION SCIPWriteStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteStartF

INTEGER,         INTENT( IN ) :: UserID
TYPE( pstartT ), INTENT( IN ) :: startIO

INTEGER, EXTERNAL :: WriteStartF

SCIPWriteStartF = WriteStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Write Project End
!*******************************************************************************
INTEGER FUNCTION SCIPWriteEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteEndF

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER, EXTERNAL :: WriteEndF

SCIPWriteEndF = WriteEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Write Project Flags
!*******************************************************************************
INTEGER FUNCTION SCIPWriteFlagsF( UserID,flagsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteFlagsF

INTEGER,         INTENT( IN ) :: UserID
TYPE( pflagsT ), INTENT( IN ) :: flagsIO

INTEGER, EXTERNAL :: WriteFlagsF

SCIPWriteFlagsF = WriteFlagsF( UserID,flagsIO )

RETURN
END
!*******************************************************************************
!            Write Project Domain
!*******************************************************************************
INTEGER FUNCTION SCIPWriteDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteDomainF

INTEGER,           INTENT( IN ) :: UserID
TYPE( pspatialT ), INTENT( IN ) :: domainIO

INTEGER, EXTERNAL :: WriteDomainF

SCIPWriteDomainF = WriteDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Write Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPWriteOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteOptionsF

INTEGER,           INTENT( IN ) :: UserID
TYPE( poptionsT ), INTENT( IN ) :: optionsIO

INTEGER, EXTERNAL :: WriteOptionsF

SCIPWriteOptionsF = WriteOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Write Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPWriteMaterialF( UserID,materialIO,mtlListIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteMaterialF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: WriteMaterialF

SCIPWriteMaterialF = WriteMaterialF( UserID,materialIO,mtlListIO )

RETURN
END
!*******************************************************************************
!            Write Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPWriteReleaseF( UserID,releaseIO,relListIO )

USE relstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteReleaseF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( preleaseT ),               INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( IN ) :: relListIO

INTEGER, EXTERNAL :: WriteReleaseF

SCIPWriteReleaseF = WriteReleaseF( UserID,releaseIO,relListIO )

RETURN
  END
!*******************************************************************************
!            Write Project ReleaseMC
!*******************************************************************************
INTEGER FUNCTION SCIPWriteReleaseMCF( UserID,releaseIO,relListIO,nMC,relMCList )

USE relstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteReleaseMCF

INTEGER,                          INTENT( IN ) :: UserID
TYPE( preleaseT ),                INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*),  INTENT( IN ) :: relListIO
INTEGER,                          INTENT( IN ) :: nMC
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN ) :: relMCList

INTEGER, EXTERNAL :: WriteReleaseMCF

SCIPWriteReleaseMCF = WriteReleaseMCF( UserID,releaseIO,relListIO,nMC,relMCList )

RETURN
END
!*******************************************************************************
!            Write Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPWriteWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteWeatherF

INTEGER,           INTENT( IN ) :: UserID
TYPE( pweatherT ), INTENT( IN ) :: weatherIO

INTEGER, EXTERNAL :: WriteWeatherF

SCIPWriteWeatherF = WriteWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Write Project Run
!*******************************************************************************
INTEGER FUNCTION SCIPWriteRunF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteRunF

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER, EXTERNAL :: WriteRunF

SCIPWriteRunF = WriteRunF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Write Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPWriteTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteTimeF

INTEGER,            INTENT( IN ) :: UserID
TYPE( ptemporalT ), INTENT( IN ) :: timeIO

INTEGER, EXTERNAL :: WriteTimeF

SCIPWriteTimeF = WriteTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPWriteInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteInpF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( IN ) :: mtlListIO

INTEGER, EXTERNAL :: WriteInpF

SCIPWriteInpF = WriteInpF( UserID,inputIO,mtlListIO )

RETURN
END

!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPWriteRstF( UserID,inputIO )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteRstF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO

INTEGER, EXTERNAL :: WriteRst

SCIPWriteRstF = WriteRst( UserID,inputIO )

RETURN
END
!*******************************************************************************
!                SCIPWriteField
!*******************************************************************************
INTEGER FUNCTION SCIPWriteField( UserID,grdI,Field,PlotType,contourHead, &
                                 contourList,GUIWrite,nComment,Comment )

USE field_fd
USE contourlist_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteField

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                    INTENT( IN ) :: UserID      !User ID
INTEGER,                                                    INTENT( IN ) :: grdI        !SAG grid ID
TYPE( SCIPPlotFieldT ),                                     INTENT( IN ) :: Field       !Field definition
TYPE( SCIPPlotTypeT ),                                      INTENT( IN ) :: PlotType    !Plot definition
TYPE( SCIPContourHeaderT ),                                 INTENT( IN ) :: contourHead !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                         INTENT( INOUT ) :: contourList !Contour array
TYPE( ARAPWriteT ),                                         INTENT( IN ) :: GUIWrite    !Draw instructions
INTEGER,                                                    INTENT( IN ) :: nComment    !User supplied comments
TYPE( char128T ),            DIMENSION(nComment),           INTENT( IN ) :: Comment     !User supplied comments

INTERFACE
  INTEGER FUNCTION WriteFieldF( CallerID,grdID,Field,PlotType,contourHead,contourList, &
                                GUIWrite,nComment,Comment )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
    INTEGER,                                   INTENT( IN ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                            INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPWriteT ),                        INTENT( IN ) :: GUIWrite     !Draw instructions
    INTEGER,                                   INTENT( IN ) :: nComment     !Number of User supplied comments
    TYPE( char128T ),     DIMENSION(nComment), INTENT( IN ) :: Comment      !User supplied comments
  END FUNCTION WriteFieldF
END INTERFACE

SCIPWriteField = WriteFieldF( UserID,grdI,Field,PlotType,contourHead, &
                              contourList,GUIWrite,nComment,Comment )

RETURN
END
!*******************************************************************************
!                SCIPWriteSAGID
!*******************************************************************************
INTEGER FUNCTION SCIPWriteSAGID( UserID,grdI,file,append )

USE SCIPresults_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteSAGID

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,           INTENT( IN ) :: UserID      !User ID
INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file

INTEGER, EXTERNAL :: WriteSAGIDF

SCIPWriteSAGID = WriteSAGIDF( UserID,grdI,file,append )

RETURN
END
