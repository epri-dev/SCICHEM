MODULE SCIPtool

!Interface definitions for the SCIPtool exported functions

  INTERFACE

    INTEGER FUNCTION SCIPAdjointReleaseFilter( CallerID,nRel,relList,nMat,matList,maxHit )
      USE tooluser_fd
      INTEGER,                                    INTENT( IN    ) :: CallerID   !USER ID tag
      INTEGER,                                    INTENT( INOUT ) :: nRel       !Number of releases
      TYPE( releaseT  ),DIMENSION(*),             INTENT( INOUT ) :: relList    !Release list
      INTEGER,                                    INTENT( INOUT ) :: nMat       !Number of materials
      TYPE( materialT ),DIMENSION(*),             INTENT( INOUT ) :: matList    !Material list
      INTEGER,                                    INTENT( IN    ) :: maxHit     !Max no. of hits
    END FUNCTION SCIPAdjointReleaseFilter

    INTEGER FUNCTION SCIPButton( CallerID,ButtonID )
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      INTEGER,                                     INTENT( IN  ) :: ButtonID    !Progress Box Button ID
    END FUNCTION SCIPButton

    INTEGER FUNCTION SCIPCheckButtons( CallerID )
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
    END FUNCTION SCIPCheckButtons

    INTEGER FUNCTION SCIPCheckInput( CallerID,iInput,tInput0,tInput1 )
      INTEGER,                                      INTENT( IN ) :: CallerID     !USER ID tag
      INTEGER,                                      INTENT( IN ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                        INTENT( IN ) :: tInput0      !Input data
      INTEGER, DIMENSION(*),                        INTENT( IN ) :: tInput1      !Input data
    END FUNCTION SCIPCheckInput

    INTEGER FUNCTION SCIPContourCount( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,nLine,nPoint )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                  INTENT( IN  ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                   INTENT( IN  ) :: contourList  !Contour array
      INTEGER,                                     INTENT( IN  ) :: Mode         !Contour Mode
      INTEGER,                                     INTENT( OUT ) :: nLine        !Total number of lines
      INTEGER,                                     INTENT( OUT ) :: nPoint       !Total number of points
    END FUNCTION SCIPContourCount

    INTEGER FUNCTION SCIPContourField( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,Line,Point )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
      INTEGER,                                   INTENT( IN    ) :: Mode         !Contour Mode
      TYPE( SCIPLineT  ), DIMENSION(*),          INTENT( OUT   ) :: Line         !Lines output array
      TYPE( SCIPPointT ), DIMENSION(*),          INTENT( OUT   ) :: Point        !Points output array
    END FUNCTION SCIPContourField

    INTEGER FUNCTION SCIPCountMaterial( CallerID,file,nMtl )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
    END FUNCTION SCIPCountMaterial

    INTEGER FUNCTION SCIPCountReleaseMC( CallerID,file,nRel,nMCrel )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of MC records in file
    END FUNCTION SCIPCountReleaseMC

    INTEGER FUNCTION SCIPCountRelease( CallerID,file,nRel )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
    END FUNCTION SCIPCountRelease

    INTEGER FUNCTION SCIPCountMCRel( CallerID,file,nMCrel )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of MC records in file
    END FUNCTION SCIPCountMCRel

    INTEGER FUNCTION SCIPCreateField( CallerID,Field,ClassData )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID Tag
      TYPE( SCIPPlotFieldT ),                    INTENT( INOUT ) :: Field        !Field descriptor
      REAL, DIMENSION(*),                        INTENT( IN    ) :: ClassData    !Additional Class data
    END FUNCTION SCIPCreateField

    INTEGER FUNCTION SCIPDefaultInput( CallerID,iInput,tInput0,tInput1 )
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput0      !Results data
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput1      !Results data
    END FUNCTION SCIPDefaultInput

    INTEGER FUNCTION SCIPDefaultCtrlF( CallerID,ctrl )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( INOUT ) :: ctrl
    END FUNCTION SCIPDefaultCtrlF

    INTEGER FUNCTION SCIPDefaultDomainF( CallerID,domain )
      USE domstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( INOUT ) :: domain
    END FUNCTION SCIPDefaultDomainF

    INTEGER FUNCTION SCIPDefaultEndF( CallerID,end )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
    END FUNCTION SCIPDefaultEndF

    INTEGER FUNCTION SCIPDefaultFlagsF( CallerID,flags )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( INOUT ) :: flags
    END FUNCTION SCIPDefaultFlagsF

    INTEGER FUNCTION SCIPDefaultInpF( CallerID,input,mtlList )
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
    END FUNCTION SCIPDefaultInpF

    INTEGER FUNCTION SCIPDefaultMaterialF( CallerID,material,mtlList )
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( INOUT ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
    END FUNCTION SCIPDefaultMaterialF

    INTEGER FUNCTION SCIPDefaultOptionsF( CallerID,options )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( INOUT ) :: options
    END FUNCTION SCIPDefaultOptionsF

    INTEGER FUNCTION SCIPDefaultStartF( CallerID,start )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( INOUT ) :: start
    END FUNCTION SCIPDefaultStartF

    INTEGER FUNCTION SCIPDefaultTimeF( CallerID,time )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( INOUT ) :: time
    END FUNCTION SCIPDefaultTimeF

    INTEGER FUNCTION SCIPDefaultWeatherF( CallerID,weather )
      USE metstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( INOUT ) :: weather
    END FUNCTION SCIPDefaultWeatherF

    INTEGER FUNCTION SCIPDeleteField( CallerID,grdID )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( INOUT ) :: grdID        !SAG grid ID
    END FUNCTION SCIPDeleteField

    INTEGER FUNCTION SCIPDeleteProject( CallerID,project,request )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( IN  ) :: request      !Delete instructions
    END FUNCTION SCIPDeleteProject

    INTEGER FUNCTION SCIPDrawField( CallerID,grdID,Field,PlotType,contourHead, &
                                    contourList,GUIdraw,UserFill,UserDraw )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
      TYPE( ARAPDrawT ),                         INTENT( IN    ) :: GUIdraw      !Draw instructions
      INTEGER, EXTERNAL                                          :: UserFill     !Address of User supplied
                                                                                 !fill routine passed by value
      INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                                 !draw routine passed by value
    END FUNCTION SCIPDrawField

    INTEGER FUNCTION SCIPDrawGrid( CallerID,grdID,UserDraw,mode )
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                                 !draw routine passed by value
      INTEGER,                                     INTENT( IN  ) :: mode         !Draw instructions
    END FUNCTION SCIPDrawGrid

    INTEGER FUNCTION SCIPExitTool()
    END FUNCTION SCIPExitTool

    INTEGER FUNCTION SCIPGetAPIVersion()
    END FUNCTION SCIPGetAPIVersion

    INTEGER FUNCTION SCIPNumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                       numGrids,numTimes )
      USE prjstruct_fd
      INTEGER,            INTENT( IN  ) :: userID
      TYPE( projectIDT ), INTENT( IN  ) :: Project
      INTEGER,            INTENT( OUT ) :: num2Dfields, num3Dfields, numGrids, numTimes
    END FUNCTION SCIPNumMetOutput

    INTEGER FUNCTION SCIPGetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                       grid,time,field2Dlist,field3Dlist )
      USE prjstruct_fd
      USE charT_fd
      USE plotlist_fd
      USE plotmet_fd
      INTEGER,                                 INTENT( IN  ) :: userID
      TYPE( projectIDT ),                      INTENT( IN  ) :: Project
      INTEGER,                                 INTENT( IN  ) :: n2D, n3D
      TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: name2D
      TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: units2D
      TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: name3D
      TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: units3D
      TYPE( metGridT ),      DIMENSION(*),     INTENT( OUT ) :: grid
      TYPE( SCIPTimeT ),     DIMENSION(*),     INTENT( OUT ) :: time
      TYPE( metGridFieldT ), DIMENSION(n2D,*), INTENT( OUT ) :: field2Dlist
      TYPE( metGridFieldT ), DIMENSION(n3D,*), INTENT( OUT ) :: field3Dlist
    END FUNCTION SCIPGetMetOutput

    INTEGER FUNCTION SCIPGetMetVertGrid( userID,Project,gridID,sigma )
      USE prjstruct_fd
      INTEGER,            INTENT( IN  ) :: userID
      TYPE( projectIDT ), INTENT( IN  ) :: Project
      INTEGER,            INTENT( IN  ) :: gridID
      REAL, DIMENSION(*), INTENT( OUT ) :: sigma
    END FUNCTION SCIPGetMetVertGrid

    INTEGER FUNCTION SCIPGet2DMetData( userID,Project,gridID,timeID,numFields, &
                                        nameFields,nx,ny,data )
      USE prjstruct_fd
      USE charT_fd
      INTEGER,                               INTENT( IN  ) :: userID
      TYPE( projectIDT ),                    INTENT( IN  ) :: Project
      INTEGER,                               INTENT( IN  ) :: gridID
      INTEGER,                               INTENT( IN  ) :: TimeID
      INTEGER,                               INTENT( IN  ) :: numFields
      TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
      INTEGER,                               INTENT( IN  ) :: nx, ny
      REAL,      DIMENSION(nx,ny,numFields), INTENT( OUT ) :: data
    END FUNCTION SCIPGet2DMetData

    INTEGER FUNCTION SCIPGet3DMetData( userID,Project,gridID,timeID,numFields, &
                                       nameFields,centerFlag,nx,ny,nz,data )
      USE prjstruct_fd
      USE charT_fd
      INTEGER,                               INTENT( IN  ) :: userID
      TYPE( projectIDT ),                    INTENT( IN  ) :: Project
      INTEGER,                               INTENT( IN  ) :: gridID
      INTEGER,                               INTENT( IN  ) :: TimeID
      INTEGER,                               INTENT( IN  ) :: numFields
      TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
      INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
      INTEGER,                               INTENT( IN  ) :: nx, ny, nz
      REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ) :: data
    END FUNCTION SCIPGet3DMetData

    INTEGER FUNCTION SCIPGetFieldAssocSize( CallerID,grdID,nPlots,nLines,nPoints )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER,                                     INTENT( OUT ) :: nPlots       !number of associated plots
      INTEGER,                                     INTENT( OUT ) :: nLines       !number of lines in associated plots
      INTEGER,                                     INTENT( OUT ) :: nPoints      !number of points in lines in associated plots
    END FUNCTION SCIPGetFieldAssocSize

    INTEGER FUNCTION SCIPGetFieldAssocData( CallerID,grdID,Line,Point,TitleStr,AxesStr,LineIDStr )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPLineT  ), DIMENSION(*),            INTENT( OUT ) :: Line         !Lines output array
      TYPE( SCIPPointT ), DIMENSION(*),            INTENT( OUT ) :: Point        !Points output array
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: TitleStr     !Title strings
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: AxesStr      !Axes strings
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: LineIDStr    !Line ID strings
    END FUNCTION SCIPGetFieldAssocData

    INTEGER FUNCTION SCIPGetField( CallerID,grdID,Field,PlotType,nodes,triangles )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      TYPE( SCIPPlotFieldNodeT ),     DIMENSION(*),INTENT( OUT ) :: nodes        !Plot definition
      TYPE( SCIPPlotFieldTriangleT ), DIMENSION(*),INTENT( OUT ) :: triangles    !Plot definition
    END FUNCTION SCIPGetField

    INTEGER FUNCTION SCIPGetFieldDomain( CallerID,grdID,nx0,ny0,x0,y0,dx0,dy0 )
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER,                                     INTENT( OUT ) :: nx0          !Base grid number X direction
      INTEGER,                                     INTENT( OUT ) :: ny0          !Base grid number Y direction
      REAL,                                        INTENT( OUT ) :: x0           !Origin X direction
      REAL,                                        INTENT( OUT ) :: y0           !Origin Y direction
      REAL,                                        INTENT( OUT ) :: dx0          !Base grid size X direction
      REAL,                                        INTENT( OUT ) :: dy0          !Base grid size Y direction
    END FUNCTION SCIPGetFieldDomain

    INTEGER FUNCTION SCIPGetFieldMinMax( CallerID,grdID,PlotType,mMin,mMax,vMin,vMax,fMin,fMax )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      REAL,                                        INTENT( OUT ) :: mMin         !Minimum value - mean field
      REAL,                                        INTENT( OUT ) :: mMax         !Maximum value - mean field
      REAL,                                        INTENT( OUT ) :: vMin         !Minimum value - variance field
      REAL,                                        INTENT( OUT ) :: vMax         !Maximum value - variance field
      REAL,                                        INTENT( OUT ) :: fMin         !Minimum value - plot field
      REAL,                                        INTENT( OUT ) :: fMax         !Maximum value - plot field
    END FUNCTION SCIPGetFieldMinMax

    INTEGER FUNCTION SCIPGetFieldSize( CallerID,grdID,Field,PlotType,nNode,nTriangle )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      INTEGER,                                     INTENT( OUT ) :: nNode        !Number of nodes in plot field
      INTEGER,                                     INTENT( OUT ) :: nTriangle    !Number of triangels in plot field
    END FUNCTION SCIPGetFieldSize

    INTEGER FUNCTION SCIPGetFieldTable( CallerID,Field,ClassData,TableTitle,ColTitle,RowTitle,Table )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( SCIPPlotFieldT ),                    INTENT( INOUT ) :: Field        !Field descriptor
      REAL,            DIMENSION(*),             INTENT( IN    ) :: ClassData    !ClassDatatArray (Risk Level - Dual runs only
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: TableTitle   !Table titles
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: ColTitle     !Column headings
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: RowTitle     !Row headings
      INTEGER,         DIMENSION(*),             INTENT( OUT   ) :: Table        !Table data
    END FUNCTION SCIPGetFieldTable

    INTEGER FUNCTION SCIPGetFieldTableExtended( CallerID,Field,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( SCIPPlotFieldT ),                    INTENT( INOUT ) :: Field        !Field descriptor
      REAL,            DIMENSION(*),             INTENT( IN    ) :: ClassData    !ClassDatatArray (Risk Level - Dual runs only
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: TableTitle   !Table titles
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: ColTitle     !Column headings
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: RowTitle     !Row headings
      INTEGER,         DIMENSION(*),             INTENT( OUT   ) :: Table        !Table data
      TYPE( spatialT ),                          INTENT(   OUT ) :: Domain       !Computational domain
    END FUNCTION SCIPGetFieldTableExtended

    INTEGER FUNCTION SCIPGetFieldTableSize( CallerID,Field,ClassData,nTable,nCol,nRow )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      REAL,     DIMENSION(*),                      INTENT( IN  ) :: ClassData    !ClassDatatArray (Risk Level - Dual runs only
      INTEGER,                                     INTENT( OUT ) :: nTable       !Number of tables
      INTEGER,                                     INTENT( OUT ) :: nCol         !Number of columns per table
      INTEGER,                                     INTENT( OUT ) :: nRow         !Number of rows per table
    END FUNCTION SCIPGetFieldTableSize
    INTEGER FUNCTION SCIPGetFieldValue( CallerID,grdID,Field,PlotType,xPnt,yPnt,fPnt )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field definition
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      REAL,                                        INTENT( IN  ) :: xPnt         !X location
      REAL,                                        INTENT( IN  ) :: yPnt         !Y location
      REAL,                                        INTENT( OUT ) :: fPnt         !Field value
    END FUNCTION SCIPGetFieldValue

    INTEGER FUNCTION SCIPGetFieldValues( CallerID,grdID,Field,PlotType,nPnt,xPnt,yPnt,fPnt )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field definition
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      INTEGER,                                     INTENT( IN  ) :: nPnt         !Number of locations
      REAL, DIMENSION(nPnt),                       INTENT( IN  ) :: xPnt         !X location
      REAL, DIMENSION(nPnt),                       INTENT( IN  ) :: yPnt         !Y location
      REAL, DIMENSION(nPnt),                       INTENT( OUT ) :: fPnt         !Field value
    END FUNCTION SCIPGetFieldValues

    INTEGER FUNCTION SCIPGetLastError( error )
      USE tooluser_fd
      TYPE( messageT ),                            INTENT( OUT ) :: error       !error message
    END FUNCTION SCIPGetLastError

    INTEGER FUNCTION SCIPGetPathMaxLength()
    END FUNCTION SCIPGetPathMaxLength

    INTEGER FUNCTION SCIPGetPlotClasses( CallerID,Project,ClassStr,ChoiceStr,KindStr, &
                                         CatClassArray,ClassChoiceArray,ProjectCoordinate )
      USE tooluser_fd
      INTEGER,                                            INTENT( IN  ) :: CallerID          !USER ID tag
      TYPE( projectIDT ),                                 INTENT( IN  ) :: Project           !Project ID
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ClassStr          !Class strings
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ChoiceStr         !Choice strings
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: KindStr           !Kind strings
      TYPE( SCIPCategoryClassT ),  DIMENSION(HP_NUMCAT,*),INTENT( OUT ) :: CatClassArray     !Class/Category use array
      TYPE( SCIPClassChoiceT ),    DIMENSION(*),          INTENT( OUT ) :: ClassChoiceArray  !Class/Choice use array
      TYPE( SCIPFieldCoordinateT ),                       INTENT( OUT ) :: ProjectCoordinate !Project coordinate descriptor
    END FUNCTION SCIPGetPlotClasses

    INTEGER FUNCTION SCIPGetPlotTimes( CallerID,Project,TimePuff,TimeSrf,TimeMet )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimePuff     !Puff times
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimeSrf      !Surface grid times
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimeMet      !Met times
    END FUNCTION SCIPGetPlotTimes

    INTEGER FUNCTION SCIPGetProjectAudit( CallerID,audit )
      USE tooluser_fd
      INTEGER,                                INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( pauditT ),                        INTENT( INOUT ) :: audit     !Project ID
    END FUNCTION SCIPGetProjectAudit

    INTEGER FUNCTION SCIPGetProjectTerrain( CallerID,terrain,ths,tddx,tddy )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( pterrainHeadT ),                     INTENT( INOUT ) :: terrain     !Project ID
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: ths         !Terrain array
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: tddx        !Slope array
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: tddy        !Slope array
    END FUNCTION SCIPGetProjectTerrain

    INTEGER FUNCTION SCIPGetProjectTerrainHeader( UserID,terrain )
      USE spcstruct_fd
      INTEGER,               INTENT( IN    ) :: UserID !USER ID Tag
      TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
    END FUNCTION SCIPGetProjectTerrainHeader

    INTEGER FUNCTION SCIPGetProjectTerrainIn( CallerID,ifld,terrain,ths,tddx,tddy )
      USE tooluser_fd
      INTEGER,                            INTENT( IN    ) :: CallerID !USER ID Tag
      INTEGER,                            INTENT( IN    ) :: ifld     !Met field ID
      TYPE( pterrainHeadT ),              INTENT( INOUT ) :: terrain  !Project ID
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: ths      !Terrain array
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: tddx     !Slope array
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: tddy     !Slope array
    END FUNCTION SCIPGetProjectTerrainIn

    INTEGER FUNCTION SCIPGetProjectTerrainHeaderIn( UserID,ifld,terrain )
      USE spcstruct_fd
      INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
      INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
      TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
    END FUNCTION SCIPGetProjectTerrainHeaderIn

    INTEGER FUNCTION SCIPGetProjectPuff( CallerID,puff,timeID,flag,puffList,auxData,typeList,mcList )
      USE tooluser_fd
      USE mcstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( ppuffHeadT ),                        INTENT( INOUT ) :: puff        !Project ID
      INTEGER,                                   INTENT( IN    ) :: timeID      !time ID
      INTEGER,                                   INTENT( IN    ) :: flag        !Bit to transform to LLA, MSL, terrain slope
      TYPE( puffT ),        DIMENSION(*),        INTENT( OUT   ) :: puffList    !puff data
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: auxData     !puff auxiliary data
      TYPE( puffTypeT ),    DIMENSION(*),        INTENT( OUT   ) :: typeList    !Puffs types
      TYPE( material_MClist ),                   INTENT( OUT   ) :: mcList      !multicomponent types
    END FUNCTION SCIPGetProjectPuff

    INTEGER FUNCTION SCIPGetProjectPuffHeader( CallerID,puff,timeID )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( ppuffHeadT ),                        INTENT( INOUT ) :: puff        !Project ID
      INTEGER,                                   INTENT( IN    ) :: timeID      !time ID
    END FUNCTION SCIPGetProjectPuffHeader

    INTEGER FUNCTION SCIPGetProjectPuffChem( UserID,project,chemOut,doData )
      USE prjstruct_fd
      USE multcomp_fd
      INTEGER,                             INTENT( IN    ) :: UserID     !USER ID Tag
      TYPE( projectIDT ),                  INTENT( IN    ) :: project    !Project ID
      TYPE( ChemMC_out ), DIMENSION(*),    INTENT( INOUT ) :: chemOut
      INTEGER,                             INTENT( IN    ) :: doData     !SCIPtrue => get arrays else get sizes
    END FUNCTION SCIPGetProjectPuffChem

    INTEGER FUNCTION SCIPGetProjectVersion( CallerID,project )
      USE tooluser_fd
      INTEGER,                                INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectIDT ),                     INTENT( INOUT ) :: project     !Project ID
    END FUNCTION SCIPGetProjectVersion

    INTEGER FUNCTION SCIPGetSCIPUFFVersion()
    END FUNCTION SCIPGetSCIPUFFVersion

    INTEGER FUNCTION SCIPGetSubstrates( mode, substrate )
      USE tooluser_fd
      INTEGER,                       INTENT( IN  ) :: mode
      TYPE( char16T ), DIMENSION(*), INTENT( OUT ) :: substrate
    END FUNCTION

    INTEGER FUNCTION SCIPGetTerrainSlice( UserID,Project,nPts,Location,Ht )
      USE prjstruct_fd
      USE field_fd
      INTEGER,                             INTENT( IN  ) :: UserID  !USER ID Tag
      TYPE( ProjectIDT ),                  INTENT( IN  ) :: Project !Project ID
      INTEGER,                             INTENT( IN  ) :: nPts    !No. of location points
      TYPE( SCIPpointT ), DIMENSION(nPts), INTENT( IN  ) :: Location    !location array
      REAL,DIMENSION(nPts),                INTENT( OUT ) :: Ht    !terrain height array
    END FUNCTION SCIPGetTerrainSlice

    INTEGER FUNCTION SCIPGetVersion()
    END FUNCTION SCIPGetVersion

    INTEGER FUNCTION SCIPGetVersionString( iflag,string )
      USE tooluser_fd
      INTEGER,                                    INTENT( IN  ) :: iflag         !request flag
      TYPE( char128T ),                           INTENT( OUT ) :: string        !version string
    END FUNCTION SCIPGetVersionString

    SUBROUTINE SCIPInitError()
    END SUBROUTINE SCIPInitError

    INTEGER FUNCTION SCIPInitTool( CallerID,callback,request,limit,cINIfile )
      USE tooluser_fd
      USE basic_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER(LEN_ADDRESS),                        INTENT( IN    ) :: callback     !Address of user callback function
      INTEGER,                                     INTENT( INOUT ) :: request      !Initialization request
      TYPE( limitT ),                              INTENT( IN    ) :: limit        !Array size limits
      TYPE( fileNameT ),                           INTENT( IN    ) :: cINIfile     !INI file
    END FUNCTION SCIPInitTool

    INTEGER FUNCTION SCIPLoadInput( CallerID,iInput,tInput0,tInput1 )
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput0      !Results data
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput1      !Results data
    END FUNCTION SCIPLoadInput

    INTEGER FUNCTION SCIPLoadCtrlF( CallerID,ctrl )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( INOUT ) :: ctrl
    END FUNCTION SCIPLoadCtrlF

    INTEGER FUNCTION SCIPLoadDomainF( CallerID,domain )
      USE domstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( INOUT ) :: domain
    END FUNCTION SCIPLoadDomainF

    INTEGER FUNCTION SCIPLoadEndF( CallerID,end )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
    END FUNCTION SCIPLoadEndF

    INTEGER FUNCTION SCIPLoadEndXF( CallerID,end,extFlag )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
      INTEGER,                                   INTENT( IN    ) :: extFlag      !extension flag FALSE=.INP TRUE=.RST
    END FUNCTION SCIPLoadEndXF

    INTEGER FUNCTION SCIPLoadFlagsF( CallerID,flags )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( INOUT ) :: flags
    END FUNCTION SCIPLoadFlagsF

    INTEGER FUNCTION SCIPLoadInpF( CallerID,input,mtlList )
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
    END FUNCTION SCIPLoadInpF

    INTEGER FUNCTION SCIPLoadRstF( CallerID,input )
      USE structure_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
    END FUNCTION SCIPLoadRstF

    INTEGER FUNCTION SCIPLoadMaterialF( CallerID,material,mtlList )
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( INOUT ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
    END FUNCTION SCIPLoadMaterialF

    INTEGER FUNCTION SCIPLoadOptionsF( CallerID,options )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( INOUT ) :: options
    END FUNCTION SCIPLoadOptionsF

    INTEGER FUNCTION SCIPLoadReleaseF( CallerID,release,relList )
      USE relstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( INOUT ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( INOUT ) :: relList
    END FUNCTION SCIPLoadReleaseF

    INTEGER FUNCTION SCIPLoadReleaseMCF( CallerID,release,relList,relMCList )
      USE relstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( INOUT ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( INOUT ) :: relList
      TYPE( releaseMCT), DIMENSION(*),           INTENT( INOUT ) :: relMCList
    END FUNCTION SCIPLoadReleaseMCF

    INTEGER FUNCTION SCIPLoadRunF( CallerID,end )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
    END FUNCTION SCIPLoadRunF

    INTEGER FUNCTION SCIPLoadStartF( CallerID,start )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( INOUT ) :: start
    END FUNCTION SCIPLoadStartF

    INTEGER FUNCTION SCIPLoadTimeF( CallerID,time )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( INOUT ) :: time
    END FUNCTION SCIPLoadTimeF

    INTEGER FUNCTION SCIPLoadWeatherF( CallerID,weather )
      USE metstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( INOUT ) :: weather
    END FUNCTION SCIPLoadWeatherF

    INTEGER FUNCTION SCIPLoadProject( CallerID,project,mtlList,relList )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),            INTENT( OUT   ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),            INTENT( OUT   ) :: relList     !Release list
    END FUNCTION SCIPLoadProject

    INTEGER FUNCTION SCIPLoadProjectMC( CallerID,project,mtlList,relList,relMCList )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),            INTENT( OUT   ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),            INTENT( OUT   ) :: relList     !Release list
      TYPE( releaseMCT ), DIMENSION(*),          INTENT( OUT   ) :: relMCList   !Release multicomponent list
    END FUNCTION SCIPLoadProjectMC

    INTEGER FUNCTION SCIPNewProject( CallerID,project,mtlList,relList )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN )    :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
    END FUNCTION SCIPNewProject

    INTEGER FUNCTION SCIPNewProjectMC( CallerID,project,mtlList,relList,nMC,relMCList )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
      INTEGER,                                     INTENT( IN    ) :: nMC         !Size of relMCList
      TYPE( releaseMCT ), DIMENSION(*),            INTENT( IN    ) :: relMCList   !Release multicomponent list
    END FUNCTION SCIPNewProjectMC

    INTEGER FUNCTION ProcessNewProject( CallerID,project,mtlList,relList )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
    END FUNCTION ProcessNewProject

    INTEGER FUNCTION ProcessNewProjectMC( CallerID,project,mtlList,relList,nMC,relMCList )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
      INTEGER,                                     INTENT( IN    ) :: nMC         !Size of relMCList
      TYPE( releaseMCT ), DIMENSION(*),            INTENT( IN    ) :: relMCList   !Release multicomponent list
    END FUNCTION ProcessNewProjectMC

    INTEGER FUNCTION SCIPNumPlotClasses( CallerID,Project,nClass,nChoice,nKind )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nClass       !Number of Class strings
      INTEGER,                                     INTENT( OUT ) :: nChoice      !Number of Choice strings
      INTEGER,                                     INTENT( OUT ) :: nKind        !Number of Kind strings
    END FUNCTION SCIPNumPlotClasses

    INTEGER FUNCTION SCIPNumPlotTimes( CallerID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nTimePuff    !Number of Puff times
      INTEGER,                                     INTENT( OUT ) :: nTimeSrf     !Number of Surface grid times
      INTEGER,                                     INTENT( OUT ) :: nTimeMet     !Number of Met times
      INTEGER,                                     INTENT( OUT ) :: nNotUsed     !Not used in this version
    END FUNCTION

    INTEGER FUNCTION SCIPNumSubstrates( mode )
      INTEGER, INTENT( IN  ) :: mode
    END FUNCTION

    INTEGER FUNCTION SCIPPopAreaField( CallerID,grdID,Field,PlotType,contourHead,contourList )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
    END FUNCTION SCIPPopAreaField

    INTEGER FUNCTION SCIPRestartProject( CallerID,project )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( createRstT ),                  TARGET, INTENT( IN  ) :: project     !Project input
    END FUNCTION SCIPRestartProject

    INTEGER FUNCTION SCIPRunProject( CallerID,run )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( pendT ),                               INTENT( IN  ) :: run         !Run input
    END FUNCTION SCIPRunProject

    INTEGER FUNCTION SCIPInitProjectSS( CallerID,run )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( pendT ),                               INTENT( IN  ) :: run         !Run input
    END FUNCTION SCIPInitProjectSS

    INTEGER FUNCTION SCIPRunProjectSS( t )
      REAL, INTENT( OUT ) :: t
    END FUNCTION SCIPRunProjectSS

    INTEGER FUNCTION SCIPExitProjectSS()
    END FUNCTION SCIPExitProjectSS

    INTEGER FUNCTION SCIPSizeProject( CallerID,project,nMtl,nRel )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
    END FUNCTION SCIPSizeProject

    INTEGER FUNCTION SCIPSizeProjectMC( CallerID,project,nMtl,nRel,nMCrel )
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of multicomponent records in file
    END FUNCTION SCIPSizeProjectMC

    INTEGER FUNCTION SCIPTransform( Cin,Cout,np,xp,yp )
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      INTEGER,                                   INTENT( IN    ) :: np           !number of points
      REAL,      DIMENSION(np),                  INTENT( INOUT ) :: xp           !x coordinate array
      REAL,      DIMENSION(np),                  INTENT( INOUT ) :: yp           !y coordinate array
    END FUNCTION SCIPTransform

    INTEGER FUNCTION SCIPTransformPt( Cin,Cout,np,pt )
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      INTEGER,                                   INTENT( IN    ) :: np           !number of points
      TYPE( SCIPPointT ),        DIMENSION(np),  INTENT( INOUT ) :: pt           !pt array
    END FUNCTION SCIPTransformPt

    INTEGER FUNCTION SCIPTransformXY( Cin,Cout,xp,yp )
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      REAL,                                      INTENT( INOUT ) :: xp           !x coordinate
      REAL,                                      INTENT( INOUT ) :: yp           !y coordinate
    END FUNCTION SCIPTransformXY

    INTEGER FUNCTION SCIPWriteField( CallerID,grdID,Field,PlotType,contourHead,contourList, &
                                     GUIWrite,nComment,Comment )
      USE tooluser_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( IN ) :: contourList  !Contour array
      TYPE( ARAPWriteT ),                        INTENT( IN ) :: GUIWrite     !Draw instructions
      INTEGER,                                   INTENT( IN ) :: nComment     !Number of User supplied comments
      TYPE( char128T ),     DIMENSION(nComment), INTENT( IN ) :: Comment      !User supplied comments
    END FUNCTION SCIPWriteField

    INTEGER FUNCTION SCIPWriteInput( CallerID,iInput,tInput0,tInput1 )
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                       INTENT( IN  ) :: tInput0      !Input data
      INTEGER, DIMENSION(*),                       INTENT( IN  ) :: tInput1      !Input data
    END FUNCTION SCIPWriteInput

    INTEGER FUNCTION SCIPWriteCtrlF( CallerID,ctrl )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( IN ) :: ctrl
    END FUNCTION SCIPWriteCtrlF

    INTEGER FUNCTION SCIPWriteDomainF( CallerID,domain )
      USE domstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( IN ) :: domain
    END FUNCTION SCIPWriteDomainF

    INTEGER FUNCTION SCIPWriteEndF( CallerID,end )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( IN ) :: end
    END FUNCTION SCIPWriteEndF

    INTEGER FUNCTION SCIPWriteFlagsF( CallerID,flags )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( IN ) :: flags
    END FUNCTION SCIPWriteFlagsF

    INTEGER FUNCTION SCIPWriteInpF( CallerID,input,mtlList )
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( IN ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( IN ) :: mtlList
    END FUNCTION SCIPWriteInpF

    INTEGER FUNCTION SCIPWriteRstF( CallerID,input )
      USE structure_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( IN ) :: input
    END FUNCTION SCIPWriteRstF

    INTEGER FUNCTION SCIPWriteMaterialF( CallerID,material,mtlList )
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( IN ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( IN ) :: mtlList
    END FUNCTION SCIPWriteMaterialF

    INTEGER FUNCTION SCIPWriteOptionsF( CallerID,options )
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( IN ) :: options
    END FUNCTION SCIPWriteOptionsF

    INTEGER FUNCTION SCIPWriteReleaseF( CallerID,release,relList )
      USE relstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( IN ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( IN ) :: relList
    END FUNCTION SCIPWriteReleaseF

    INTEGER FUNCTION SCIPWriteReleaseMCF( CallerID,release,relList,nMC,relMCList )
      USE relstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( IN ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( IN ) :: relList
      INTEGER,                                   INTENT( IN ) :: nMC
      TYPE( releaseMCT ), DIMENSION(*),          INTENT( IN ) :: relMCList
    END FUNCTION SCIPWriteReleaseMCF

    INTEGER FUNCTION SCIPWriteRunF( CallerID,end )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( IN ) :: end
    END FUNCTION SCIPWriteRunF

    INTEGER FUNCTION SCIPWriteStartF( CallerID,start )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( IN ) :: start
    END FUNCTION SCIPWriteStartF

    INTEGER FUNCTION SCIPWriteTimeF( CallerID,time )
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( IN ) :: time
    END FUNCTION SCIPWriteTimeF

    INTEGER FUNCTION SCIPWriteWeatherF( CallerID,weather )
      USE metstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( IN ) :: weather
    END FUNCTION SCIPWriteWeatherF

    INTEGER FUNCTION SCIPWriteSAGID( UserID,grdI,file,append )
      USE charT_fd
      INTEGER,           INTENT( IN ) :: UserID      !User ID
      INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
      TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
      INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file
    END FUNCTION SCIPWriteSAGID

  INTEGER FUNCTION SCIPMEDOCNumNestList( MEDOCfile,nlist )
    USE charT_fd
    !DEC$ ATTRIBUTES DLLEXPORT :: SCIPMEDOCNumNestList
    TYPE( fileNameT ), INTENT( IN  ) :: MEDOCfile
    INTEGER,           INTENT( OUT ) :: nlist
  END FUNCTION SCIPMEDOCNumNestList

  INTEGER FUNCTION SCIPMEDOCNestList( MEDOCfile,nlist,FileList )
    USE charT_fd
    !DEC$ ATTRIBUTES DLLEXPORT :: SCIPMEDOCNestList
    TYPE( fileNameT ),                   INTENT( IN  ) :: MEDOCfile
    INTEGER,                             INTENT( IN  ) :: nlist
    TYPE( fileNameT ), DIMENSION(nlist), INTENT( OUT ) :: FileList
  END FUNCTION SCIPMEDOCNestList

  END INTERFACE

END MODULE SCIPtool
