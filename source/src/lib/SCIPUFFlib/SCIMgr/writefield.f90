!*******************************************************************************
!                SCIPWriteField
!*******************************************************************************
INTEGER FUNCTION WriteFieldF( UserID,grdI,Field,PlotType,contourHead, &
                              contourList,GUIWrite,nComment,Comment )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE SCIMgrState
USE abort
USE error_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                    INTENT( IN    ) :: UserID      !User ID
INTEGER,                                                    INTENT( IN    ) :: grdI        !SAG grid ID
TYPE( SCIPPlotFieldT ),                                     INTENT( IN    ) :: Field       !Field definition
TYPE( SCIPPlotTypeT ),                                      INTENT( IN    ) :: PlotType    !Plot definition
TYPE( SCIPContourHeaderT ),                                 INTENT( IN    ) :: contourHead !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                            INTENT( INOUT ) :: contourList !Contour array
TYPE( ARAPWriteT ),                                         INTENT( IN    ) :: GUIWrite    !Draw instructions
INTEGER,                                                    INTENT( IN    ) :: nComment    !User supplied comments
TYPE( char128T ),            DIMENSION(nComment),           INTENT( IN    ) :: Comment     !User supplied comments

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

INTERFACE
  SUBROUTINE WriteField( grdID,Field,PlotType,contourHead,contourList,GUIWrite,nComment,Comment )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPWriteT ),                        INTENT( IN    ) :: GUIWrite     !Draw instructions
    INTEGER,                                   INTENT( IN    ) :: nComment     !Number of User supplied comments
    TYPE( char128T ),     DIMENSION(nComment), INTENT( IN    ) :: Comment      !User supplied comments
  END SUBROUTINE WriteField
END INTERFACE

!==== initialize

WriteFieldF = SCIPfailure

IF( SCIMgrIsWait() )THEN !Not valid during a callback
  CALL SCIMgrSetBusyMsg()
  RETURN
ELSE
  currentState = SCIMgrSetState( HS_BUSY )
END IF

CALL set_messaging( userID )

!==== write the field

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL WriteField( grdI,Field,PlotType,contourHead, &
                   contourList,GUIWrite,nComment,Comment )

  IF( nError == NO_ERROR )WriteFieldF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                WriteField
!*******************************************************************************
SUBROUTINE WriteField( grdI,Field,PlotType,contourHead, &
                          contourList,GUIWrite,nComment,Comment )

USE field_fd
USE contourlist_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE files_fi
USE sagdef_fd
USE write_fi
USE SCIMgr_fd
USE PtrGrdStrItf
USE abort

IMPLICIT NONE

INTEGER, PARAMETER :: MAX_STRING = 25

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN    ) :: grdI     !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN    ) :: Field    !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN    ) :: PlotType !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN    ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( INOUT ) :: contourList  !Contour array
TYPE( ARAPWriteT ),                                                 INTENT( IN    ) :: GUIWrite !Draw instructions
INTEGER,                                                            INTENT( IN    ) :: nComment !User supplied comments
TYPE( char128T ),            DIMENSION(nComment),                   INTENT( IN    ) :: Comment  !User supplied comments

!==============================================================================
! Local variables
!==============================================================================
LOGICAL                                  :: lrestore
LOGICAL                                  :: lclose
LOGICAL                                  :: log_interp
LOGICAL                                  :: lcontour
INTEGER                                  :: nheader
INTEGER                                  :: iFormat
INTEGER                                  :: nfld
INTEGER                                  :: plotfld
INTEGER                                  :: nstr
INTEGER                                  :: i
INTEGER                                  :: irv
INTEGER                                  :: ios
INTEGER                                  :: nlev
INTEGER,  DIMENSION(3)                   :: ifld
REAL,     DIMENSION(:), ALLOCATABLE      :: level
CHARACTER(80), DIMENSION(:), ALLOCATABLE :: header
CHARACTER(128),DIMENSION(MAX_STRING)     :: string
TYPE( SCIPContourElementList )           :: contour
TYPE( SAGgrid_str ), POINTER             :: grd

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_BottomFunctionID
INTEGER, EXTERNAL :: SAG_WriteFieldID
INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: BuildHeaderAVS
INTEGER, EXTERNAL :: BuildHeaderOVL
INTEGER, EXTERNAL :: BuildHeaderEIS
INTEGER, EXTERNAL :: BuildHeaderOIL
INTEGER, EXTERNAL :: BuildFooterEIS
INTEGER, EXTERNAL :: BuildFooterCTS
INTEGER, EXTERNAL :: sysDeleteFile
INTEGER, EXTERNAL :: ContourHeaderOVL
INTEGER, EXTERNAL :: ContourHeaderOIL
INTEGER, EXTERNAL :: ContourHeaderUSA

INTEGER, EXTERNAL :: PlotFunction
INTEGER, EXTERNAL :: WriteBodyOVL
INTEGER, EXTERNAL :: WriteBodyOIL
INTEGER, EXTERNAL :: WriteBodyEIS
INTEGER, EXTERNAL :: WriteBodyCTS
INTEGER, EXTERNAL :: WriteBodyUSA
INTEGER, EXTERNAL :: WriteBodyAVS
INTEGER, EXTERNAL :: WriteBodyNone
INTEGER, EXTERNAL :: WriteBodyCount

INTERFACE
  INTEGER FUNCTION SAG_InitWriteField( grdI,        & !SAG grid ID
                                       ifld,        & !Field number
                                       lunit,       & !Logical Unit number for writing
                                       log_interp,  & !logarithmic interolation
                                       nlev,        & !Number of contour levels
                                       level,       & !Array of contour levels
                                       istart,      & !Start contour level
                                       istop,       & !Stop  contour level
                                       nheader,     & !Contour header/contour
                                       header,      & !Array of contour headers
                                       lContour,    & !Contour vs Nodes flag
                                       lclose       ) !Close contours flag

    INTEGER,                                                  INTENT( IN ) :: grdI        !SAG grid ID
    INTEGER,                                                  INTENT( IN ) :: ifld        !Field number
    INTEGER,                                                  INTENT( IN ) :: lunit       !Logical Unit number for writing
    LOGICAL,                                                  INTENT( IN ) :: log_interp  !logarithmic interolation
    INTEGER,                                                  INTENT( IN ) :: nlev        !Number of contour levels
    REAL,        DIMENSION(nlev),                     TARGET, INTENT( IN ) :: level       !Array of contour levels
    INTEGER,                                                  INTENT( IN ) :: istart      !Start contour level
    INTEGER,                                                  INTENT( IN ) :: istop       !Stop  contour level
    INTEGER,                                                  INTENT( IN ) :: nheader     !Contour headers/contour
    CHARACTER(*),DIMENSION(MAX(nlev*ABS(nheader),1)), TARGET, INTENT( IN ) :: header      !Array of contour levels
    LOGICAL,                                                  INTENT( IN ) :: lContour    !Contour vs Nodes flag
    LOGICAL,                                                  INTENT( IN ) :: lclose      !Close contours flag
  END FUNCTION SAG_InitWriteField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================

irv = SAG_InitError()

contour%listHdr =  contourHead
contour%listPtr => contourList

IF( Aborted() )GOTO 9999

!==============================================================================
! Set File Format
!==============================================================================
lcontour = BTEST(GUIWrite%mode,BWRITE_CONTOUR)
lclose   = lcontour .AND. BTEST(GUIWrite%mode,BCLOSE_CONTOUR)

iFormat = IBCLR(GUIWrite%mode,BWRITE_CONTOUR)
iFormat = IBCLR(iFormat      ,BCLOSE_CONTOUR)
iFormat = IBCLR(iFormat      ,BLATLON_OUTPUT)
iFormat = IBCLR(iFormat      ,BFILE_HEADER)
iFormat = IBCLR(iFormat      ,BFILE_FOOTER)

!==============================================================================
! Turn on/off SAG Special value feature
!==============================================================================
IF( lclose )THEN
  irv = SAG_SetSpecialValue( .FALSE.,HP_SPV )
ELSE
  irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )
END IF

!==============================================================================
! Initialize SCIP Write parameters
!==============================================================================
Coordinate = Field%coordinate
IF( BTEST(GUIWrite%mode,BLATLON_OUTPUT) )THEN
  WriteMode = I_LATLON
ELSE
  WriteMode = Coordinate%mode
END IF

IF( Aborted() )GOTO 9999

!==============================================================================
! Set number of contour/field levels and contour/field headers
!==============================================================================
IF( lcontour )THEN

!==============================================================================
! Set number of contours
!==============================================================================
  nlev = contour%listHdr%number
  IF( iFormat == EIS_FORMAT )nlev = MIN(nlev,3)

!==============================================================================
! Set number of contour headers/contour
!==============================================================================
  SELECT CASE( iFormat )
    CASE( OVL_FORMAT )
      nheader = 1
    CASE( USA_FORMAT )
      nheader = 1
    CASE( OIL_FORMAT )
      nheader = 2
    CASE DEFAULT
      nheader = 0
  END SELECT

ELSE

!==============================================================================
! Set number of fields
!==============================================================================
    nlev = 0

!==============================================================================
! Set number of field headers
!==============================================================================
  SELECT CASE( iFormat )
    CASE( AVS_FORMAT )
      nheader = 1
    CASE DEFAULT
      nheader = 0
  END SELECT

END IF

IF( Aborted() )GOTO 9999

!==============================================================================
! Allocate space for contour levels
!==============================================================================
IF( nlev > 0 )THEN
  IF( ASSOCIATED(contour%listPtr) )THEN
    ALLOCATE( level(nlev),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'WriteField'
      eMessage = 'Error allocating contour array'
      GOTO 9999
    END IF
  ELSE
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Contour list pointer not associated'
    GOTO 9999
  END IF
ELSE
  ALLOCATE( level(1),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error allocating contour array'
    GOTO 9999
  END IF
  level(1) = 0.
END IF

!==============================================================================
! Allocate space for contour headers
!==============================================================================
IF( nheader > 0 )THEN
  ALLOCATE( header(MAX(nlev,1)*nheader),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error allocating contour/field header array'
    GOTO 9999
  END IF
ELSE
  ALLOCATE( header(1),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error allocating contour/field header array'
    GOTO 9999
  END IF
  header(1) = 'Unknown'
END IF

IF( Aborted() )GOTO 9999

!==============================================================================
! Set contour levels and contour/field headers
!==============================================================================
IF( lcontour )THEN

!==============================================================================
! Set contour levels
!==============================================================================
  IF( nlev > 0 )THEN
    DO i = 1,nlev
      level(i) = contour%listPtr(i)%contour
    END DO
  ELSE
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'No Contour values defined'
    GOTO 9999
  END IF

!==============================================================================
! Set contour headers
!==============================================================================
  IF( nheader > 0 )THEN
    DO i = 1,nlev
      SELECT CASE( iFormat )
        CASE( OVL_FORMAT )
          irv = ContourHeaderOVL( i,contour,nheader,header((i-1)*nheader+1) )
        CASE( OIL_FORMAT )
          irv = ContourHeaderOIL( i,contour,nheader,header((i-1)*nheader+1) )
        CASE( USA_FORMAT )
          irv = ContourHeaderUSA( i,contour,nheader,header((i-1)*nheader+1) )
          IF( i == nlev )nheader = -nheader
        CASE DEFAULT
      END SELECT
      IF( irv /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'WriteField'
        eMessage = 'Error building contour headers'
        GOTO 9999
      END IF
    END DO
  END IF

ELSE

!==============================================================================
! Set Field headers
!==============================================================================
  IF( nheader > 0 )THEN
    SELECT CASE( iFormat )
      CASE( AVS_FORMAT )
        SELECT CASE( PlotType%type )
          CASE( HP_MEAN )
            header(1) = 'M,   '//TRIM(Field%units)
          CASE( HP_PROB )
            CALL c_format(PlotType%data,i,string(1))
            header(1) = 'P,   Prob( M > '//TRIM(string(1))//' '//TRIM(Field%units)//' )'
          CASE( HP_EXCEED )
            CALL c_format(PlotType%data,i,string(1))
            header(1) = 'E,   Prob( M > E '//TRIM(Field%units)//' ) = '//TRIM(string(1))
          CASE( HP_VARIANCE )
            header(1) = 'V,   ('//TRIM(Field%units)//'^2)'
          CASE DEFAULT
            header(1) = 'Unknown'
        END SELECT
      CASE DEFAULT
    END SELECT
  END IF

END IF

IF( Aborted() )GOTO 9999

!==============================================================================
! Open File
!==============================================================================

IF( BTEST(GUIWrite%mode,BFILE_APPEND) )THEN
  OPEN(UNIT=lun_fil,FILE=GUIWrite%filename,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=irv)
  IF( irv /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error opening output file for append'
    CALL ReportFileName( eInform,'File=',GUIWrite%filename )
    GOTO 9999
  END IF
ELSE IF( iFormat /= NO_FORMAT .AND. iFormat /= COUNT_FORMAT )THEN
  irv = sysDeleteFile( GUIWrite%filename )
  IF( irv == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error deleting existing output file'
    CALL ReportFileName( eInform,'File=',GUIWrite%filename )
    GOTO 9999
  END IF

  OPEN(UNIT=lun_fil,FILE=GUIWrite%filename,STATUS='UNKNOWN',IOSTAT=irv)
  IF( irv /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error opening output file'
    CALL ReportFileName( eInform,'File=',GUIWrite%filename )
    GOTO 9999
  END IF
END IF

!==============================================================================
! Write File Header
!==============================================================================
IF( BTEST(GUIWrite%mode,BFILE_HEADER) )THEN
  SELECT CASE( iFormat )
    CASE( OVL_FORMAT )
      nstr = BuildHeaderOVL( grdI,Field,nComment,Comment,MAX_STRING,string )
    CASE( EIS_FORMAT )
      nstr = BuildHeaderEIS( grdI,Field,nlev,level,MAX_STRING,string )
    CASE( OIL_FORMAT )
      nstr = BuildHeaderOIL( grdI,Field,nComment,Comment,MAX_STRING,string )
    CASE( AVS_FORMAT )
      nstr = BuildHeaderAVS( grdI,Field,nComment,Comment,MAX_STRING,string )
    CASE DEFAULT
      nstr = 0
  END SELECT
  DO i = 1,nstr
    WRITE(lun_fil,'(A)',IOSTAT=irv)TRIM(string(i))
    IF( irv /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'WriteField'
      eMessage = 'Error writing output file header'
      CALL ReportFileName( eInform,'File=',GUIWrite%filename )
      GOTO 9998
    END IF
  END DO
END IF

IF( Aborted() )GOTO 9998

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'WriteField'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9998
END IF

plotfld = 3              !Results of PlotFunction are in third SAG field

!==============================================================================
! Check if plot field has already been created
!==============================================================================

IF( grd%PlotType /= PlotType%type .OR. grd%PlotData /= PlotType%data )THEN

!==============================================================================
! Set function parameters
!==============================================================================
  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )

      PlotFunc_nfun =  PLOT_FUNCTION_NULL
      PlotFunc_data =  0.0
      log_interp    =  Field%interpType == SCIPoff

    CASE( HP_PROB )

      PlotFunc_nfun =  PLOT_FUNCTION_PROB_CLIP
      PlotFunc_data =  PlotType%data
      log_interp    = .FALSE.

    CASE( HP_EXCEED )

      PlotFunc_nfun =  PLOT_FUNCTION_ICPROB_CLIP
      PlotFunc_data =  PlotType%data
      log_interp    =  Field%interpType == SCIPoff

    CASE( HP_VARIANCE )

      PlotFunc_nfun =  PLOT_FUNCTION_SWITCH
      PlotFunc_data =  0.0
      log_interp    =  Field%interpType == SCIPoff

    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'WriteField'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9998

  END SELECT

  IF( grd%type == SAG_GRID_NONE )THEN
    PlotFunc_small  = -1.E36  !set to some parameter?
  ELSE
    PlotFunc_small  = 1.E-30  !set to some parameter?
  END IF
  PlotFunc_spv = HP_SPV

  nfld     = 3
  ifld(1)  = 1              !mean
  ifld(2)  = 2              !variance
  ifld(3)  = plotfld        !results

  lrestore = .FALSE.

  IF( Aborted() )GOTO 9998

!==============================================================================
! Set auxiliary plot data
!==============================================================================
  IF( grd%naux <= 0 )THEN
    AuxType = 0
  ELSE
    AuxType = grd%ipblk(1)%type
    AuxData => grd%aux(1)
  END IF

!=============================================================================
! Compute Plot field
!==============================================================================
  CALL InitAbortedCount()
  irv = SAG_BottomFunctionID( grdI,PlotFunction,nfld,ifld,lrestore,.TRUE. )
  IF( irv /= SAG_OK )THEN
    nError = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Error computing plot function'
    GOTO 9998
  END IF

  CALL CheckSmoothCN2( grdI )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( grd%PlotType /= -1 )grd%PlotType = PlotType%type
  grd%PlotData = PlotType%data

ELSE

  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )
      log_interp = Field%interpType == SCIPoff

    CASE( HP_PROB )
      log_interp = .FALSE.

    CASE( HP_EXCEED )
      log_interp = Field%interpType == SCIPoff

    CASE( HP_VARIANCE )
      log_interp = Field%interpType == SCIPoff

    CASE DEFAULT
      nError = IV_ERROR
      eRoutine = 'WriteField'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9998

  END SELECT

END IF

!==============================================================================
! Initialize SAG Write Field
!==============================================================================
irv = SAG_InitWriteField( grdI,                           & !SAG grid ID
                          plotfld,                        & !Field number
                          lun_fil,                        & !Unit number for writing
                          log_interp,                     & !logarithmic interpolation
                          nlev,                           & !Number of contour levels
                          level,                          & !Array of contour levels
                          1,                              & !Start contour
                          nlev,                           & !Stop  contour
                          nheader,                        & !Contour headers/contour
                          header,                         & !Array of contour headers
                          lcontour,                       & !Contour/Node flag
                          lclose )                          !Close contours flag

IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'WriteField'
  eMessage = 'Error initializing SAG write structure'
  GOTO 9997
END IF

IF( Aborted() )GOTO 9997

!==============================================================================
! Write Field
!==============================================================================
SELECT CASE( iFormat )
  CASE( OVL_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyOVL )

  CASE( EIS_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyEIS )

  CASE( OIL_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyOIL )

  CASE( AVS_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyAVS )

  CASE( CTS_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyCTS )

  CASE( USA_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyUSA )

  CASE( NO_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyNone )

  CASE( COUNT_FORMAT )
    irv = SAG_WriteFieldID( grdI,WriteBodyCount )

  CASE DEFAULT
    nError = UK_ERROR
    eRoutine = 'WriteField'
    eMessage = 'Invalid Format identifier'
    WRITE(eInform,*)'FormatID=',iFormat
    GOTO 9997

END SELECT

IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'WriteField'
  eMessage = 'Error writing SAG field'
  GOTO 9997
END IF

IF( Aborted() ) GOTO 9997

!==============================================================================
! Write File Footer
!==============================================================================
IF( BTEST(GUIWrite%mode,BFILE_FOOTER) )THEN
  SELECT CASE( iFormat )
    CASE( EIS_FORMAT )
      nstr = BuildFooterEIS( MAX_STRING,string )
    CASE( CTS_FORMAT )
      nstr = BuildFooterCTS( MAX_STRING,string )
    CASE DEFAULT
      nstr = 0
  END SELECT
  DO i = 1,nstr
    WRITE(lun_fil,'(A)',IOSTAT=irv)TRIM(string(i))
    IF( irv /= 0 )THEN
      nError = WR_ERROR
      eRoutine = 'WriteField'
      eMessage = 'Error writing output file footer'
      CALL ReportFileName( eInform,'File=',GUIWrite%filename )
      GOTO 9997
    END IF
  END DO
END IF

!==============================================================================
! Free the Write Field
!==============================================================================
9997 CONTINUE

CALL SAG_FreeWriteField()

!==============================================================================
! Close File
!==============================================================================
9998 CONTINUE

CLOSE( UNIT=lun_fil,IOSTAT=irv )

!==============================================================================
! Clean up - Deallocate contour and header work space
!==============================================================================
9999 CONTINUE

IF( ALLOCATED(level ) )DEALLOCATE( level, STAT=ios )
IF( ALLOCATED(header) )DEALLOCATE( header,STAT=ios )

RETURN
END
!*******************************************************************************
!                WriteSAGIDF
!*******************************************************************************
INTEGER FUNCTION WriteSAGIDF( UserID,grdI,file,append )

USE SCIMgr_fd
USE SCIMgrState
USE abort
USE files_fi
USE error_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,           INTENT( IN ) :: UserID      !User ID
INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file

!==============================================================================
! Local variables
!==============================================================================
INTEGER                                  :: irv
INTEGER                                  :: currentState

!==== initialize

WriteSAGIDF = SCIPfailure

IF( SCIMgrIsWait() )THEN !Not valid during a callback
  CALL SCIMgrSetBusyMsg()
  RETURN
ELSE
  currentState = SCIMgrSetState( HS_BUSY )
END IF

CALL set_messaging( userID )

!==== write the field

IF( .NOT.Aborted() )THEN

  CALL init_error

  CALL WriteSAGID( grdI,file,append )

  IF( nError == NO_ERROR ) WriteSAGIDF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!               WriteSAGID
!*******************************************************************************
SUBROUTINE WriteSAGID( grdI,file,append )

USE charT_fd
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf
USE abort
USE error_fi
USE files_fi
USE SCIPresults_fd
USE sagerr_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file

!==============================================================================
! Local variables
!==============================================================================
REAL                                     :: time
INTEGER                                  :: irv
INTEGER                                  :: lun
INTEGER                                  :: grdJ
LOGICAL                                  :: lappend
TYPE( SAGgrid_str ), POINTER             :: grda
TYPE( SAGgrid_str ), POINTER             :: grdb

INTEGER, EXTERNAL :: SAG_CopyGridID,SAG_InitError,SAG_Create,SAG_WriteBreakID,SAG_CloseID

!==============================================================================
! Initialize return value
!==============================================================================

grdJ = -1

irv = SAG_InitError()

IF( Aborted() ) GOTO 9999

lappend = append == SCIPtrue

lun = lun_fil

!==============================================================================
!
!==============================================================================
IF( lappend )THEN

  CALL read_surface( lun,file%string,grdJ )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() ) GOTO 9999

  CALL CheckAppendGrid(grdI,grdJ)
  IF( nError /= NO_ERROR )GOTO 9999

  irv = SAG_CopyGridID( grdI,grdJ,SAG_COPY_GRID )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteSAGID'
    eMessage = 'Error copying grid'
    GOTO 9999
  END IF

   IF( Aborted() ) GOTO 9999

  irv = SAG_CopyGridID( grdI,grdJ,SAG_COPY_DATA )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteSAGID'
    eMessage = 'Error copying data'
    GOTO 9999
  END IF

ELSE

  grda => SAG_PtrGrdStr( grdI )
  grda%record = 0
  grda%status = SAG_CLOSED
  grda%nunit = lun_fil
  grda%file = TRIM(file%string)

  irv = SAG_Create( grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'WriteSAGID'
    eMessage = 'Error creating grid'
    GOTO 9999
  END IF

  grdJ = grdI

END IF

IF( Aborted() ) GOTO 9999

grdb => SAG_PtrGrdStr( grdJ )
time = grdb%time

irv = SAG_WriteBreakID( grdJ,time )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'WriteSAGID'
  eMessage = 'Error writing time break'
  GOTO 9999
END IF

9999 CONTINUE

irv = SAG_CloseID( grdJ )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'WriteSAGID'
  eMessage = 'Error closing file'
  GOTO 9999
END IF

RETURN
END
!*******************************************************************************
!               CheckAppendGrid
!*******************************************************************************
SUBROUTINE CheckAppendGrid( grdI, grdJ )

USE sagstr_fd
USE PtrGrdStrItf
USE error_fi

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
INTEGER,           INTENT( IN ) :: grdJ        !SAG grid ID

TYPE( SAGgrid_str ), POINTER             :: grda
TYPE( SAGgrid_str ), POINTER             :: grdb
CHARACTER(128)                           :: which
INTEGER                                  :: iblk

grdb => SAG_PtrGrdStr( grdJ )
grda => SAG_PtrGrdStr( grdI )

! Check fields

which = ' '

IF( grda%type /= grdb%type )THEN
  which = 'grid type'
  GOTO 9998
END IF

IF( grda%ftype /= grdb%ftype )THEN
  which = 'grid file type'
  GOTO 9998
END IF

IF( grda%version /= grdb%version )THEN
  which = 'grid SCIPUFF version'
  GOTO 9998
END IF

IF( grda%ftype > 0 )THEN
  IF( grda%naux /= grdb%naux )THEN
    which = 'number of aux blocks'
    GOTO 9998
  END IF
  DO iblk = 1,grda%nblk
    IF( grda%ipblk(iblk)%name /= grdb%ipblk(iblk)%name )THEN
      which = 'block name'
      GOTO 9998
    END IF
    IF( grda%ipblk(iblk)%ifld /= grdb%ipblk(iblk)%ifld )THEN
      which = 'block start field'
      GOTO 9998
    END IF
    IF( grda%ipblk(iblk)%nfld /= grdb%ipblk(iblk)%nfld )THEN
      which = 'block number of fields'
      GOTO 9998
    END IF
    IF( grda%ipblk(iblk)%iaux /= grdb%ipblk(iblk)%iaux )THEN
      which = 'block number of aux'
      GOTO 9998
    END IF
  END DO
END IF

IF( grda%nvart /= grdb%nvart )THEN
  which = 'number of fields'
  GOTO 9998
END IF

IF( grda%time <= grdb%time )THEN
  which = 'block times'
  GOTO 9998
END IF

IF( grda%ncells > grdb%mxgrd )THEN
  which = 'Maximum number of cells'
  GOTO 9998
END IF

!Copy header information

grdb%time   = grda%time
grdb%ncells = grda%ncells
grdb%nx     = grda%nx
grdb%ny     = grda%ny
grdb%xmin   = grda%xmin
grdb%ymin   = grda%ymin
grdb%dx     = grda%dx
grdb%dy     = grda%dy
grdb%maxlev = grda%maxlev
grdb%delmin = grda%delmin

9999 CONTINUE

NULLIFY(grda)
NULLIFY(grdb)

RETURN

9998 CONTINUE
nError = IV_ERROR
eRoutine = 'WriteSAGID'
eMessage = 'Error appending SAG file'
eInform ='Mismatch on '//TRIM(which)
GOTO 9999

END
