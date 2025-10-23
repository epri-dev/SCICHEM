!***********************************************************************
!               WriteOptions
!***********************************************************************
SUBROUTINE write_plotopt( iwnd_db,filename )

USE pltopt_fd
USE files_fi
USE errorParam_fd
USE plotdlg_fi
USE pltchoice_fi
USE PlotOpt_fd
USE GUIparam_fd
USE winAPI_fd, ONLY: POINTER_LEN

IMPLICIT NONE

!==============================================================================
! Function arguments
!==============================================================================
INTEGER(POINTER_LEN)     , INTENT( IN ) :: iwnd_db !Window Handle for posting error messages
CHARACTER(*),              INTENT( IN ) :: filename !Plot Options file to create

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios
INTEGER i
INTEGER j
INTEGER nPltType
LOGICAL InRange

TYPE( PlotChoice ) CurPlt

CHARACTER(32) errString

CHARACTER(128) eMessage,eInform,eAction,eRoutine
INTEGER        nError

!==============================================================================
! Open File
!==============================================================================
OPEN(UNIT=lun_tmp,FILE=filename,STATUS='UNKNOWN',FORM='UNFORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eMessage = 'Error opening PlotOptions file'
  CALL ReportFileName( eInform,'File=',filename )
  WRITE(eAction,*)'Fortran Error =',ios
  GOTO 9999
END IF

!==============================================================================
! File Header
!==============================================================================
errString = 'File Header'
WRITE(lun_tmp,ERR=9998)PLOT_OPTION_MAGIC,PLOT_OPTION_CURRENT,NUM_CONTOUR

!==============================================================================
! Plot Definition
!==============================================================================
nPltType = HP_NUMTYP
CurPlt%Category = PlotDef(BASE_LEVEL)%Field%Category

IF( InRange(1,nPltClass,PlotDef(BASE_LEVEL)%Field%Class) )THEN
  CurPlt%ClassStr = ClassStr(PlotDef(BASE_LEVEL)%Field%Class)%string
ELSE
  CurPlt%ClassStr = 'Default'
END IF

IF( InRange(1,nPltChoice,PlotDef(BASE_LEVEL)%Field%Choice) )THEN
  CurPlt%ChoiceStr = ChoiceStr(PlotDef(BASE_LEVEL)%Field%Choice)%string
ELSE
  CurPlt%ChoiceStr = 'Default'
END IF

IF( InRange(1,nPltKind,PlotDef(BASE_LEVEL)%Field%Kind) )THEN
  CurPlt%KindStr = KindStr(PlotDef(BASE_LEVEL)%Field%Kind)%string
ELSE
  CurPlt%KindStr = 'Default'
END IF

IF( InRange(1,nPltType,PlotDef(BASE_LEVEL)%Type) )THEN
  CurPlt%Type = PlotDef(BASE_LEVEL)%Type
ELSE
  CurPlt%Type = NOT_SET_I
END IF

errString = 'Plot Definition'
WRITE(lun_tmp,ERR=9998)PATH_MAXLENGTH
WRITE(lun_tmp,ERR=9998)CurPlt,PlotDef(BASE_LEVEL)

!==============================================================================
! Contour Definition
!==============================================================================
DO i = 1,NUM_CONTOUR
  WRITE(errString,'(A,I2,A)')'Contour Definition [',i,']'
  WRITE(lun_tmp,ERR=9998)ContourList(i,BASE_LEVEL)%ListHdr%Number,    &
                         ContourList(i,BASE_LEVEL)%ListHdr%Scale,     &
                         ContourList(i,BASE_LEVEL)%ListHdr%DrawMode,  &
                         ContourList(i,BASE_LEVEL)%ListHdr%LabelMode, &
                         ContourList(i,BASE_LEVEL)%ListHdr%Unit
  IF( ASSOCIATED(ContourList(i,BASE_LEVEL)%ListPtr) )THEN
    WRITE(lun_tmp,ERR=9998)(ContourList(i,BASE_LEVEL)%ListPtr(j),j=1,ContourList(i,BASE_LEVEL)%ListHdr%Number)
  END IF
  WRITE(lun_tmp,ERR=9998)ContourBuild(i,BASE_LEVEL)
END DO

!==============================================================================
! Plot Options
!==============================================================================
errString = 'Plot Options'
WRITE(lun_tmp,ERR=9998)poptdef(BASE_LEVEL),popt2def(BASE_LEVEL)

!==============================================================================
! Plot Axes
!==============================================================================
errString = 'Plot Axes'
WRITE(lun_tmp,ERR=9998)axesdef(BASE_LEVEL)

!==============================================================================
! Plot Maps
!==============================================================================
errString = 'Plot Map Options'
WRITE(lun_tmp,ERR=9998)mapdef(BASE_LEVEL)

!==============================================================================
! Plot Title
!==============================================================================
errString = 'Plot Titles'
WRITE(lun_tmp,ERR=9998)ttldef(BASE_LEVEL)

1000 CONTINUE

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN

9998 CONTINUE
nError   = WR_ERROR
eMessage = 'Error writing PlotOptions file ('//TRIM(errString)//')'
CALL ReportFileName( eInform,'File=',filename )
eAction  = ' '

9999  CONTINUE
eRoutine = 'WriteOptions'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine)
CALL ShowErrorMessage( iwnd_db )
GOTO 1000

END
!***********************************************************************
!               ReadOptions
!***********************************************************************
SUBROUTINE read_plotopt( iwnd_db,flag,filename )

USE pltopt_fd
USE files_fi
USE errorParam_fd
USE plotdlg_fi
USE pltchoice_fi
USE PlotOpt_fd
USE pcscipuf_fi
USE param_fd

IMPLICIT NONE

!==============================================================================
! Function arguments
!==============================================================================
INTEGER(POINTER_LEN)     , INTENT( IN ) :: iwnd_db    !Window Handle for posting error messages
INTEGER                  , INTENT( IN ) :: flag       !flag=CURRENT_OPTION => Load selection structures
                                         !flag=DEFAULT_OPTION => Load default structures
CHARACTER(*)             , INTENT( IN ) :: filename   !Plot Options file to create

!==============================================================================
! Local variables
!==============================================================================
INTEGER magic
INTEGER FileVersion
INTEGER ios
INTEGER i
INTEGER j
INTEGER indx
INTEGER jndx

TYPE PlotFieldOld
  SEQUENCE
  LOGICAL                                            Created      !true/false
  INTEGER                                            Mode         !PLOT_DRAW/PLOT_FILL
  INTEGER                                            Type         !HP_MEAN/HP_PROB/HP_EXCEED
  REAL, DIMENSION(HP_NUMTYP)                      :: TypeData     !RiskLevel/Probability/Exceedance
  INTEGER                                            ATP
  TYPE( SCIPPlotFieldT )                             Field        !Field definition
  TYPE( SCIPPlotData  ), DIMENSION(NUM_CLASSDATA) :: ClassData    !Class specific data
  INTEGER                                            ContourIndex !SCIP_CONTOUR/USER_CONTOUR etc
END TYPE PlotFieldOld

TYPE( PlotChoice ) ReadPlt
TYPE( PlotField  ) ReadFld
TYPE( PlotFieldOld  ) ReadFldOld
TYPE( PlotField3  ) ReadFld3
TYPE( SCIPContourHeaderT ) ReadHdr

CHARACTER(32) errString
LOGICAL       LatLon
LOGICAL       Cartesian
REAL          rtmp(4)
LOGICAL       finish

CHARACTER(PATH_MAXLENGTH) prjName
CHARACTER(PATH_MAXLENGTH) prjPath

CHARACTER(128) eMessage,eInform,eAction,eRoutine
INTEGER        nError
INTEGER        numContour, numRec, irec, PathLength

finish = .FALSE.

!==============================================================================
! Open File
!==============================================================================
OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eMessage = 'Error opening PlotOptions file'
  CALL ReportFileName( eInform,'File=',filename )
  WRITE(eAction,*)'Fortran Error =',ios
  GOTO 9999
END IF

!==============================================================================
! File Header
!==============================================================================
READ(lun_tmp,ERR=9998,END=9998)magic,Fileversion

IF( magic /= PLOT_OPTION_MAGIC )THEN
  nError   = RD_ERROR
  eMessage = 'Invalid PlotOptions file (not SCIP 4.0 format)'
  CALL ReportFileName( eInform,'File=',filename )
  WRITE(eAction,*)'Header ID =',magic
  GOTO 9999
END IF

IF( FileVersion < PLOT_OPTION_START )THEN
  nError   = RD_ERROR
  eMessage = 'Invalid PlotOptions file (Invalid file version)'
  CALL ReportFileName( eInform,'File=',filename )
  WRITE(eAction,*)'FileVersion =',FileVersion
  GOTO 9999
END IF

IF( FileVersion > PLOT_OPTION_CURRENT )THEN
  nError   = RD_ERROR
  eMessage = 'Invalid PlotOptions file (Unknown file version)'
  CALL ReportFileName( eInform,'File=',filename )
  WRITE(eAction,*)'FileVersion =',FileVersion
  GOTO 9999
END IF

IF( FileVersion > 1 )THEN
  BACKSPACE(lun_tmp,ERR=9998)
  READ(lun_tmp,ERR=9998,END=9998)magic,Fileversion,numContour
ELSE
  !We have to try to count the number of contour definitions since it
  !wasn't explicitly saved on the file
  !First get the numer of records in the contour definitions by counting
  !the records in the file
  ios = 0
  numRec = 1
  DO WHILE( ios == 0 )
    READ(lun_tmp,IOSTAT=ios)
    IF( ios == 0 )THEN
      numRec = numRec + 1
    ELSE IF( ios > 0 )THEN
      GOTO 9998
    END IF
  END DO
  numRec = numRec - 6 !First 2 records plus ast 4 records
  !Rewind the file and skip the first two records
  REWIND(lun_tmp,ERR=9998)
  READ(lun_tmp,ERR=9998)
  READ(lun_tmp,ERR=9998)
  !Read through the contour definition records, counting contour definitions
  irec = 0
  numContour = 0
  DO WHILE( irec < numRec )
    READ(lun_tmp,ERR=9998)ReadHdr%Number
    irec = irec + 1
    IF( ReadHdr%Number > 0 )THEN
      READ(lun_tmp,ERR=9998)
      irec = irec + 1
    END IF
    READ(lun_tmp,ERR=9998)
    irec = irec + 1
    numContour = numContour + 1
  END DO
  IF( irec /= numRec )GOTO 9998
  !Again rewind the file and reset to second record
  REWIND(lun_tmp,ERR=9998)
  READ(lun_tmp,ERR=9998)
END IF

!==============================================================================
! Set Read Index
!==============================================================================
SELECT CASE( flag )
  CASE( DEFAULT_OPTION )
    indx = DEFAULT_LEVEL
    jndx = DEFAULT_LEVEL

  CASE( INTERACTIVE_OPTION )
    indx = BASE_LEVEL
    jndx = BASE_LEVEL

  CASE DEFAULT
    indx = EDIT_LEVEL
    jndx = BASE_LEVEL

END SELECT

!==============================================================================
! Plot Definition
!==============================================================================
errString = 'Plot Definition'
prjName   = PlotDef(indx)%field%project
prjPath   = PlotDef(indx)%field%path
SELECT CASE( FileVersion )
  CASE( 0 )
    READ(lun_tmp,IOSTAT=ios)ReadPlt,ReadFldOld !PlotDef(indx)
    IF( ios == 0 )THEN
      ReadFld%Created      = ReadFldOld%Created
      ReadFld%Mode         = ReadFldOld%Mode
      ReadFld%Type         = ReadFldOld%Type
      ReadFld%TypeData     = ReadFldOld%TypeData
      ReadFld%Field        = ReadFldOld%Field
      ReadFld%ClassData    = ReadFldOld%ClassData
      ReadFld%ContourIndex = ReadFldOld%ContourIndex
    ELSE IF( ios == 67 )THEN !severe (67): Input statement requires too much data
                              !To account for any files saved after the change in PlotField
                              !but before we remembered to change the current fileversion
      BACKSPACE(lun_tmp)
      READ(lun_tmp,ERR=9998)ReadPlt,ReadFld !PlotDef(indx)
    ELSE
      !Real error
      GOTO 9998
    END IF
  CASE( 1:3 )
    READ(lun_tmp,IOSTAT=ios)ReadPlt,ReadFld3 !PlotDef(indx)
    IF( ios /= 0 )GOTO 9998
    CALL TransferOldField( ReadFld3,ReadFld )
  CASE( 4:PLOT_OPTION_CURRENT )
    READ(lun_tmp,IOSTAT=ios)PathLength
    IF( ios /= 0 )GOTO 9998
    IF( PathLength == PATH_MAXLENGTH )THEN
      READ(lun_tmp,IOSTAT=ios)ReadPlt,ReadFld !PlotDef(indx)
      IF( ios /= 0 )GOTO 9998
    ELSE IF( PathLength == 128 )THEN
      READ(lun_tmp,IOSTAT=ios)ReadPlt,ReadFld3 !PlotDef(indx)
      IF( ios /= 0 )GOTO 9998
      CALL TransferOldField( ReadFld3,ReadFld )
    ELSE
      nError   = OP_ERROR
      eMessage = 'Error reading PlotOptions file'
      CALL ReportFileName( eInform,'File=',filename )
      WRITE(eAction,*)'PathLength does not match this build',PathLength,PATH_MAXLENGTH
      GOTO 9999
    END IF
  CASE DEFAULT
    GOTO 9997
END SELECT

PlotDef(indx) = ReadFld

PlotDef(indx).Created       = .FALSE.
PlotDef(indx)%field%project = prjName
PlotDef(indx)%field%path    = prjPath

finish = .TRUE.

!==============================================================================
! Contour Definition
!==============================================================================
SELECT CASE( FileVersion )
  CASE( 0:PLOT_OPTION_CURRENT )
    DO i = 1,numContour
      WRITE(errString,'(A,I2,A)')'Contour Definition [',i,']'
      READ(lun_tmp,ERR=9998)ReadHdr%Number,    &
                            ReadHdr%Scale,     &
                            ReadHdr%DrawMode,  &
                            ReadHdr%LabelMode, &
                            ReadHdr%Unit
      IF( i <= NUM_CONTOUR )THEN
        IF( ReadHdr%Number > 0 )THEN
          CALL DeallocateContours( i,indx )
          ContourList(i,indx)%ListHdr = ReadHdr
	        ALLOCATE( ContourList(i,indx)%ListPtr(ContourList(i,indx)%ListHdr%Number),STAT=ios )
          IF( ios == 0 )THEN
            READ(lun_tmp,ERR=9998)(ContourList(i,indx)%ListPtr(j),j=1,ContourList(i,indx)%ListHdr%Number)
          ELSE
            nError   = UK_ERROR
            eMessage = 'Allocation error : '//TRIM(errString)
            CALL ReportFileName( eInform,'File=',filename )
            WRITE(eAction,*)'Allocation error =',ios
            GOTO 9999
          END IF
        ELSE
          ContourList(i,indx)%ListHdr = ReadHdr
        END IF
        READ(lun_tmp,ERR=9998)ContourBuild(i,indx)
      ELSE
        IF( ReadHdr%Number > 0 )THEN
          READ(lun_tmp,ERR=9998)
        END IF
        READ(lun_tmp,ERR=9998)
      END IF
    END DO

  CASE DEFAULT
    GOTO 9997

END SELECT

!==============================================================================
! Plot Options
!==============================================================================
errString = 'Plot Options'
SELECT CASE( FileVersion )
  CASE( 0:2 )
    READ(lun_tmp,ERR=9998)poptdef(jndx)%Maxlev, &
                          poptdef(jndx)%Max, &
                          poptdef(jndx)%Cell, &
                          poptdef(jndx)%Contour, &
                          poptdef(jndx)%Source, &
                          poptdef(jndx)%ColorBox, &
                          poptdef(jndx)%FillLo, &
                          poptdef(jndx)%FillHi, &
                          poptdef(jndx)%Terrain, &
                          poptdef(jndx)%Weather, &
                          poptdef(jndx)%KmScale, &
                          poptdef(jndx)%Overlay, &
                          poptdef(jndx)%OverlayFile, &
                          popt2def(jndx)
  CASE( 3:PLOT_OPTION_CURRENT )
    READ(lun_tmp,ERR=9998)poptdef(jndx),popt2def(jndx)

  CASE DEFAULT
    GOTO 9997

END SELECT

!==============================================================================
! Plot Axes
!==============================================================================
errString = 'Plot Axes'
SELECT CASE( FileVersion )
  CASE( 0:PLOT_OPTION_CURRENT )
    READ(lun_tmp,ERR=9998)axesdef(jndx)

  CASE DEFAULT
    GOTO 9997

END SELECT

!==============================================================================
! Plot Maps
!==============================================================================
errString = 'Plot Map Options'
SELECT CASE( FileVersion )
  CASE( 0:PLOT_OPTION_CURRENT )
    READ(lun_tmp,ERR=9998)mapdef(jndx)

  CASE DEFAULT
    GOTO 9997

END SELECT

!==============================================================================
! Plot Title
!==============================================================================
errString = 'Plot Titles'
SELECT CASE( FileVersion )
  CASE( 0:PLOT_OPTION_CURRENT )
    READ(lun_tmp,ERR=9998)ttldef(jndx)

  CASE DEFAULT
    GOTO 9997

END SELECT

!==============================================================================
! FLAG Specific operations
!==============================================================================
1000 CONTINUE

IF( finish )THEN
  SELECT CASE( flag )
    CASE( DEFAULT_OPTION )
      DefaultPlot = ReadPlt
      PlotDef(DEFAULT_LEVEL)%Field%Category = NOT_SET_I
      PlotDef(DEFAULT_LEVEL)%Field%Class    = NOT_SET_I
      PlotDef(DEFAULT_LEVEL)%Field%Choice   = NOT_SET_I
      PlotDef(DEFAULT_LEVEL)%Field%Kind     = NOT_SET_I

    CASE DEFAULT
      CALL SetPlotChoice( PlotDef(indx),DefaultPlot,ReadPlt )
      IF(project(BASE_LEVEL)%Ref .OR. project(BASE_LEVEL)%MapCoord == I_UTM)THEN
        axesdef(jndx)%Lon0 = axesdef(DEFAULT_LEVEL)%Lon0
        axesdef(jndx)%Lat0 = axesdef(DEFAULT_LEVEL)%Lat0
        axesdef(jndx)%X0   = axesdef(DEFAULT_LEVEL)%X0
        axesdef(jndx)%Y0   = axesdef(DEFAULT_LEVEL)%Y0
      END IF

      SELECT CASE( PlotDef(indx)%Field%Category )
        CASE( HP_VSLICE,HP_HINT,HP_TABLE )

          CONTINUE

        CASE DEFAULT

          CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(jndx)%MapCoord, &
                                  0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
          LatLon    = project(BASE_LEVEL)%MapCoord == I_LATLON
          Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
          CALL axes_transform( axesdef(jndx)%dbreal(5),rtmp, &
                               axesdef(jndx)%dbreal(17),LatLon, &
                               Cartesian,.FALSE.,.TRUE. )
      END SELECT

  END SELECT
END IF

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN

9997 CONTINUE
nError   = RD_ERROR
eMessage = 'Error reading PlotOptions file ('//TRIM(errString)//')'
eInform  = 'Unknown File version : File='//TRIM(filename)
eAction  = ' '
GOTO 9999
9998 CONTINUE
nError   = RD_ERROR
eMessage = 'Error reading PlotOptions file ('//TRIM(errString)//')'
CALL ReportFileName( eInform,'File=',filename )
eAction  = ' '
9999  CONTINUE
eRoutine = 'ReadOptions'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )
GOTO 1000

END
