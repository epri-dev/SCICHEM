SUBROUTINE WriteFiles()

USE Extract_fi

IMPLICIT NONE

TYPE( fileNameT ) file

!==== Select Output file

CALL pickOutputFile( file, (extractMethod==GRID) )
IF(nError /= NO_ERROR)GOTO 9999

!==== Write Fields

IF ( doFld )THEN

  IF ( concProfile )THEN
    CALL WriteProfiles( file )
    IF(nError /= NO_ERROR)GOTO 9999
  ELSE IF ( horzLines )THEN
    CALL WriteHorzLines( file )
    IF(nError /= NO_ERROR)GOTO 9999
  ELSE IF ( nativeSAG )THEN
    CALL WriteTimeSeries( file )
    IF(nError /= NO_ERROR)GOTO 9999
  ELSE
  IF( extractMethod == GRID )THEN
    CALL WriteFields( file )
    IF(nError /= NO_ERROR)GOTO 9999
  ELSE
    CALL WriteContours( file )
    IF(nError /= NO_ERROR)GOTO 9999
  END IF
  END IF
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteFields( file )

USE Extract_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

CALL WriteHeader()

Field = Fields(1)

IF( output3D )THEN

  CALL WriteOutput3D( file )
  IF(nError /= NO_ERROR)GOTO 9999

ELSE IF( GridType == NATIVE )THEN

  CALL WriteNativeGrid(file)
  IF(nError /= NO_ERROR)GOTO 9999

ELSE IF( GridType == LINE1D )THEN

  CALL WriteLine()
  IF(nError /= NO_ERROR)GOTO 9999

ELSE

  CALL WriteGrid()
  IF(nError /= NO_ERROR)GOTO 9999

END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteGrid( )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER i

CALL WriteGridHeader()

IF( hasVariance )THEN
  DO i = 1,nxy
    WRITE(lun_out,'(2G16.7,1P2E16.6)')xGrd(i),yGrd(i),mFldGrd(i),vFldGrd(i)
  END DO
ELSE
  DO i = 1,nxy
    WRITE(lun_out,'(2G16.7,1P2E16.6)')xGrd(i),yGrd(i),mFldGrd(i)
  END DO
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteLine( )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER i

CALL WriteLineHeader()

IF( LineType == HORZ_LINE )THEN
  IF( hasVariance )THEN
    DO i = 1,nxy
      WRITE(lun_out,'(2G16.7,1P2E16.6)')xGrd(i),yGrd(i),mFldGrd(i),vFldGrd(i)
    END DO
  ELSE
    DO i = 1,nxy
      WRITE(lun_out,'(2G16.7,1P2E16.6)')xGrd(i),yGrd(i),mFldGrd(i)
    END DO
  END IF
ELSE
  IF( hasVariance )THEN
    DO i = 1,nxy
      WRITE(lun_out,'(G16.7,1P2E16.6)')yGrd(i),mFldGrd(i),vFldGrd(i)
    END DO
  ELSE
    DO i = 1,nxy
      WRITE(lun_out,'(G16.7,1P2E16.6)')yGrd(i),mFldGrd(i)
    END DO
  END IF
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE pickOutputFile( file, openIt )

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

TYPE( fileNameT )  :: file
LOGICAL            :: openIt

INTEGER irv
LOGICAL lExist
CHARACTER(128) string

INTEGER, EXTERNAL :: SYSDELETEFILE

300 CONTINUE

IF( iFld > 0 )THEN

  IF( iOut > 0 )THEN
    lun_out = lun_ext
    irv = INDEX(outFile,'.',BACK=.TRUE.)
    IF(  irv > 0 )THEN
      ! Use user provided file name
      file%string = TRIM(outFile)
    ELSE
      contourExt = '.txt'
      IF( LEN_TRIM(extType) > 0 )THEN
        SELECT CASE( TRIM(extType) )
        CASE( 'sag','ntv' )
          contourExt = '.ntv'
        CASE( 'ovl' )
          contourExt = '.ovl'
        END SELECT
      END IF
      file%string = TRIM(outFile)//TRIM(contourExt)
    END IF
    IF ( openIt )THEN
      OPEN(UNIT=lun_out,FILE=TRIM(file%string),STATUS='NEW',IOSTAT=irv)
      IF( irv /= 0 )THEN
        nError = UK_ERROR
        eRoutine = 'pickOutputFile'
        WRITE(eInform,'(A,I0)')'Error opening output file. IOSTAT=',irv
        WRITE(eInform,'("Verify that file = ",A," does not exist")')TRIM(file%string)
        GO TO 9999
      END IF
    END IF
  ELSE
    ! Output written to standard output
    GO TO 9999
  ENDIF
ELSE
  IF( output3D .OR. extractMethod == CONTOURS )THEN
    WRITE(6,'(/,A,$)')'Output file prefix : '
  ELSE
    WRITE(6,'(/,A,$)')'Output file : '
  ENDIF
  READ(lun_in,'(A)')file%string
  file%string = ADJUSTL(file%string)
  IF( output3D )THEN
    IF( LEN_TRIM(file%string) <= 0 )file%string = 'Output3d'
  ELSE IF ( nativeSAG )THEN
    IF( LEN_TRIM(file%string) <= 0 )file%string = 'test.sag'
    INQUIRE(FILE=file%string,EXIST=lExist)
    IF( lExist )THEN
      WRITE(6,'(A)')'File already exists'
      WRITE(6,'(A)')'File='//TRIM(file%string)
      WRITE(6,'(A,$)')'(O)verwrite, (A)ppend or (R)eselect name : '
      string = 'R'
      READ(lun_in,'(A)',IOSTAT=irv)string
      CALL cupper(string)
      string = ADJUSTL(string)
      IF( string(1:1) == 'O' )THEN
        irv = SYSDELETEFILE(TRIM(file%string))
        IF( irv == SCIPfailure )THEN
          nError = UK_ERROR
          eRoutine = 'pickOutputFile'
          eMessage = 'Error deleting existing output file'
          WRITE(eInform,'(A)')'file='//TRIM(file%string)
          GO TO 9999
        ELSE
          WRITE(6,'(A)')'File successfully deleted'
          append = .FALSE.
        END IF
      ELSE IF( string(1:1) == 'A')THEN
        append = .TRUE.
      ELSE
        GOTO 300
      END IF
    ELSE
      append = .FALSE.
    END IF
  ELSE
    IF( LEN_TRIM(file%string) <= 0 )THEN
     lun_out = 6
    ELSE
      IF( extractMethod == CONTOURS )THEN
        file%string = TRIM(file%string)//contourExt
      END IF
      WRITE(6,'(/,"Output will be written to ",A,/)')TRIM(file%string)
      lun_out = 106
      INQUIRE(FILE=file%string,EXIST=lExist)
      IF( lExist )THEN
        WRITE(6,'(A)')'File already exists'
        WRITE(6,'(A)')'File='//TRIM(file%string)
        SELECT CASE( overWrite )
          CASE( 1)                !always over write file
            string = 'Y'
          CASE(-1)                !never over write file
            string = 'N'
          CASE DEFAULT            !Prompt user
            WRITE(6,'(A,$)')'OK to overwrite? (Y/N) : '
            string = 'Y'
            READ(lun_in,'(A)',IOSTAT=irv)string
            CALL cupper(string)
            string = ADJUSTL(string)
        END SELECT
        IF( string(1:1) /= 'N' )THEN
          irv = SYSDELETEFILE(TRIM(file%string))
          IF( irv == SCIPfailure )THEN
            nError = UK_ERROR
            eRoutine = 'pickOutputFile'
            eMessage = 'Error deleting existing output file'
            WRITE(eInform,'(A)')'file='//TRIM(file%string)
            GO TO 9999
          ELSE
            WRITE(6,'(A)')'File successfully deleted'
          END IF
        ELSE
          IF( overWrite == 0 )THEN
            GO TO 300
          ELSE
            GO TO 9999
          END IF
        END IF
      END IF
      IF( openIt )THEN
        OPEN(UNIT=lun_out,FILE=TRIM(file%string),STATUS='NEW',IOSTAT=irv)
        IF( irv /= 0 )THEN
          nError = UK_ERROR
          eRoutine = 'pickOutputFile'
          WRITE(eInform,'(A,I0)')'Error opening output file. IOSTST=',irv
          WRITE(eInform,'(A)')'file='//TRIM(file%string)
          GO TO 9999
        END IF
      ENDIF
    ENDIF
  END IF
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteHeader( )

USE Extract_fi

IMPLICIT NONE

WRITE(lun_out,'(A)')'Creator       : '//TRIM(CODE_NAME)//'      : '//TRIM(CODE_VERSION)
WRITE(lun_out,'(A)')'              : SCIPtool   : '//TRIM(toolString%string)
WRITE(lun_out,'(A)')'Project       : '//TRIM(Project%name)
WRITE(lun_out,'(A)')'Path          : '//TRIM(Project%path)

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteGridHeader( )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER i, j
CHARACTER(128) string, label

WRITE(lun_out,'(A)')'Variable      : '//TRIM(ChoiceStr(Fields(1)%choice)%string)
IF( Fields(1)%kind > 0 .AND. Fields(1)%kind <= nKind )THEN
  WRITE(lun_out,'(A)')'Type          : '//TRIM(KindStr(Fields(1)%kind)%string)
END IF
SELECT CASE( PlotType%type )
  CASE( HP_VARIANCE )
    WRITE(lun_out,'(A)')'Form          : '//TRIM(TYPE_STRING(PlotType%type))
  CASE( HP_PROB, HP_EXCEED )
    WRITE(lun_out,'(A,F0.5,A)')'Form          : '//TRIM(TYPE_STRING(PlotType%type))//'(',PlotType%data,')'
  CASE DEFAULT
END SELECT
IF( Fields(1)%timeID > nTimeOut )THEN
  WRITE(string,*)Fields(1)%userTime
ELSE
  WRITE(string,*)TimeOut(Fields(1)%timeID)%time%runTime
  string = TRIM(string)//'  ('//TimeOut(Fields(1)%timeID)%string//')'
END IF
string = ADJUSTL(string)
WRITE(lun_out,'(A)')'Time          : '//TRIM(string)
i = 1
j = 20
IF( Fields(1)%coordinate%mode == HD_LATLON )THEN
  WRITE(lun_out,'(A)')'Coordinate    : Lat/Lon'
  label(i:j) = 'Longitude Latitude  '
ELSE IF( Fields(1)%coordinate%mode == HD_UTM )THEN
  WRITE(lun_out,'(A,I3.2)')'Coordinate    : modified UTM : zone',Fields(1)%coordinate%UTMzone
  label(i:j) = 'Easting   Northing  '
ELSE
  WRITE(lun_out,'(A)')'Coordinate    : Cartesian'
  label(i:j) = ' X         Y        '
END IF
i = j + 1
j = j + 40
IF( GridType == AUTO )THEN
  WRITE(lun_out,'(A)')'Grid type     : Automatic'
ELSE IF( GridType == UNIFORM )THEN
  string = ADJUSTL(string)
  WRITE(lun_out,'(A)')'Grid type     : Uniform : '//TRIM(ClassStr(Fields(1)%class)%string)
ELSE
  WRITE(lun_out,'(A)')'Grid type     : Custom : '//TRIM(ClassStr(Fields(1)%class)%string)
END IF
IF( GridType /= CUSTOM )THEN
  WRITE(string,*)xMin,xMax
  string = ADJUSTL(string)
  WRITE(lun_out,'(A)')'Domain X      : '//TRIM(string)
  WRITE(string,*)yMin,yMax
  string = ADJUSTL(string)
  WRITE(lun_out,'(A)')'Domain Y      : '//TRIM(string)
  WRITE(string,*)nX,nY
  string = ADJUSTL(string)
  WRITE(lun_out,'(A)')'Points (X,Y)  : '//TRIM(string)
END IF

WRITE(lun_out,*)TRIM(ChoiceStr(Fields(1)%choice)%string),' : ' ,TRIM(Fields(1)%units)
WRITE(string,*)FldMin(1),FldMax(1)
string = ADJUSTL(string)
WRITE(lun_out,'(A)')' Field min/max: '//TRIM(string)
WRITE(string,*)FldMin(2),FldMax(2)
string = ADJUSTL(string)
WRITE(lun_out,'(A)')' Grid  min/max: '//TRIM(string)
SELECT CASE( PlotType%type )
  CASE( HP_MEAN )
    IF( hasVariance )THEN
      label(i:j) = 'Mean       Variance  '
    ELSE
      label(i:j) = 'Mean       '
    END IF
  CASE( HP_VARIANCE )
    label(i:j) = 'Variance  '
  CASE( HP_PROB )
    label(i:j) = 'Value     '
  CASE( HP_EXCEED )
    label(i:j) = 'Prob.     '
  CASE DEFAULT
END SELECT
i = j + 1

DO j=1,(i/10)*16
  string(j:j) = '='
END DO
WRITE(lun_out,'(A)')TRIM(string)
WRITE(lun_out,'(6(4X,A10,2X))')(label(j:j+9),j=1,i-10,10)


RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteLineHeader( )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER i, j
CHARACTER(128) string, label

WRITE(lun_out,*)'Variable      : ',TRIM(ChoiceStr(Fields(1)%choice)%string)
IF( Fields(1)%kind > 0 .AND. Fields(1)%kind <= nKind )THEN
  WRITE(lun_out,'(A)')'Type          : '//TRIM(KindStr(Fields(1)%kind)%string)
END IF
SELECT CASE( PlotType%type )
  CASE( HP_VARIANCE )
    WRITE(lun_out,'(A)')'Form          : '//TRIM(TYPE_STRING(PlotType%type))
  CASE( HP_PROB, HP_EXCEED )
    WRITE(lun_out,'(A,F0.5,A)')'Form          : '//TRIM(TYPE_STRING(PlotType%type))//'(',PlotType%data,')'
  CASE DEFAULT
END SELECT
IF( Fields(1)%timeID > nTimeOut )THEN
  WRITE(string,*)Fields(1)%userTime
ELSE
  WRITE(string,*)TimeOut(Fields(1)%timeID)%time%runTime
  string = TRIM(string)//'  ('//TimeOut(Fields(1)%timeID)%string//')'
END IF
string = ADJUSTL(string)
WRITE(lun_out,'(A)')'Time          : '//TRIM(string)
i = 1
IF( LineType == HORZ_LINE )THEN
  j = 32
  WRITE(string,*)Fields(1)%coordinate%horzSlice%height
  IF( Fields(1)%coordinate%horzSlice%mode == 0 )THEN
    string = TRIM(ADJUSTL(string))//' m (AGL)'
  ELSE
    string = TRIM(string)//' m (MSL)'
  END IF
  WRITE(lun_out,'(A)')'Horz. Line at : '//TRIM(string)
  IF( Fields(1)%coordinate%mode == HD_LATLON )THEN
    WRITE(lun_out,'(A)')'Coordinate    : Lat/Lon'
    label(i:j) = '    Longitude       Latitude        '
  ELSE IF( Fields(1)%coordinate%mode == HD_UTM )THEN
    WRITE(lun_out,'(A,I3.2)')'Coordinate    : modified UTM : zone',Fields(1)%coordinate%UTMzone
    label(i:j) = '    Easting         Northing    '
  ELSE
    WRITE(lun_out,'(A)')'Coordinate    : Cartesian'
    label(i:j) = '       X               Y        '
  END IF
ELSE
  j = 16
  label(i:j) = '       Z        '
  WRITE(lun_out,'(A)')'Vert. Profile :'
  IF( ProjectCoordinate%mode == HD_LATLON )THEN
    WRITE(string,*)Xmin,Xmax
    WRITE(lun_out,'(A)')'Location (LLA): '//TRIM(ADJUSTL(string))
  ELSE IF( ProjectCoordinate%mode == HD_UTM )THEN
    WRITE(string,*)Xmin,Xmax
    WRITE(lun_out,'(A)')'Location (UTM): '//TRIM(ADJUSTL(string))
    WRITE(lun_out,'(A,I3.2)')'UTM Zone      :',Fields(1)%coordinate%UTMzone
  ELSE
    WRITE(string,*)Xmin,Xmax
    WRITE(lun_out,'(A)')'Location      : '//TRIM(ADJUSTL(string))
  END IF
END IF
i = j + 1
j = j + 32

SELECT CASE( PlotType%type )
  CASE( HP_MEAN )
    IF( hasVariance )THEN
      label(i:j) = '    Mean            Variance     '
    ELSE
      label(i:j) = '    Mean         '
    END IF
  CASE( HP_VARIANCE )
    label(i:j) = '    Variance    '
  CASE( HP_PROB )
    label(i:j) = '    Value       '
  CASE( HP_EXCEED )
    label(i:j) = '    Prob.       '
  CASE DEFAULT
END SELECT

WRITE(lun_out,'(A)')TRIM(label(1:j))

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE GetWriteHeader( ncom, comment )

USE Extract_fi

IMPLICIT NONE

INTEGER ncom
TYPE( char128T ), DIMENSION(*) :: comment

ncom = 1
comment(ncom)%string='Creator       : '//TRIM(CODE_NAME)//'   : '//TRIM(CODE_VERSION)

ncom = ncom + 1
comment(ncom)%string='              : SCIPtool   : '//TRIM(toolString%string)

ncom = ncom + 1
comment(ncom)%string='Project       : '//TRIM(Project%name)

ncom = ncom + 1
comment(ncom)%string='Path          : '//TRIM(Project%path)

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteTimeSeries( file )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER iAppend
INTEGER irv, lastID
REAL lastTime
CHARACTER(32) string

INTEGER, EXTERNAL :: PostCreateField

iAppend = SCIPfalse
IF( append )iAppend = SCIPtrue

1000 CONTINUE

irv = SCIPWriteSAGID(callerID,FieldIds(1),file,iAppend)
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Error in WriteTimeSeries' )
  GOTO 9999
END IF

WRITE(6,'(A,$)')'Add another time? (Y/N) : '
string = 'N'
READ(lun_in,'(A)',IOSTAT=irv)string
CALL cupper(string)
string = ADJUSTL(string)
IF( string(1:1) == 'Y' )THEN
  lastID = Fields(1)%timeID
  lastTime = Fields(1)%userTime
  CALL pickAnotherPlotTime( Fields(1)%class,Fields(1)%choice,lastID,lastTime,Fields(1)%timeID,Fields(1)%userTime,'Field' )
  IF( nError /= NO_ERROR )GOTO 9999
  IF( Fields(1)%timeID < 0 )THEN
    WRITE(6,'(A)')'No more availabe times.'
    GOTO 9999
  END IF
  IF( Fields(1)%timeID < 1 )THEN
    WRITE(6,'(A)')'Invalid time selection'
    GOTO 9999
  END IF
  irv = SCIPDeleteField( callerID, fieldIDs(1) )
  IF( irv /= SCIPsuccess )THEN
    WRITE(6,*)'Error deleting plot field : ',TRIM(ClassStr(Fields(1)%class)%string)
    CALL toolError('Error deleting plot field : '//TRIM(ClassStr(Fields(1)%class)%string))
    GOTO 9999
  END IF
  FieldIds(1) = PostCreateField( Fields(1),ClassData,.FALSE.,'Plot' )
  IF( FieldIds(1) < 0 )GOTO 9999
  iAppend = SCIPtrue
  GOTO 1000
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteProfiles( file )

USE Extract_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER nxp1, irv, i, j, jp

CHARACTER(32) title

nxp1 = nProfiles + 1
IF( hasVariance ) nxp1 = nxp1 + nProfiles
ALLOCATE( profiles(nxp1*nz), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'WriteProfiles'
  eMessage = 'Error allocating profiles'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxp1*nz,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( profileNames(nxp1), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'WriteProfiles'
  eMessage = 'Error allocating profileNames'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nxp1,' : Error=',irv
  GO TO 9999
END IF

j = 1
profileNames(j) = 'Z'
DO i=1,nProfiles
  j = j+1
  WRITE(title,'(I3)')i
  profileNames(j) = 'C'//TRIM(ADJUSTL(title))
  IF( hasVariance )THEN
    j = j+1
    profileNames(j) = 'V'//TRIM(ADJUSTL(title))
  END IF
END DO

jp = 0
DO i=1,nz
  jp = jp + 1
  profiles(jp) = zGrd((i-1)*nProfiles+1)
  DO j = 1,nProfiles
    jp = jp + 1
    profiles(jp) = mFldGrd((i-1)*nProfiles+j)
    IF( hasVariance )THEN
      jp = jp+1
      profiles(jp) = vFldGrd((i-1)*nProfiles+j)
    END IF
  END DO
END DO

title = 'Vertical Concentration Profiles'

IF( LEN_TRIM(file%string) == 0 .OR. INDEX(file%string,'.txt') > 0 )THEN
  CALL write_ascii(lun_out,TRIM(file%string),nxp1,nxp1,nz,profileNames,title,profiles,0)
ELSE
  CALL write_xpp(lun_out,TRIM(file%string),nxp1,nxp1,nz,profileNames,title,profiles,0)
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE WriteHorzLines( file )

USE Extract_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER irv, i, j, jp, nvar

CHARACTER(32) title

nvar = 0
DO i = 1,nHorzLines
  IF( horzLineType(i) == HORZ_LINE )THEN
    nvar = nvar + 1
  ELSE IF( horzLineType(i) == VERT_LINE )THEN
    nvar = nvar + 1
  ELSE
    nvar = nvar + 2
  END IF
  IF( hasVariance )THEN
    nvar = nvar + 2
  ELSE
    nvar = nvar + 1
  END IF
END DO

ALLOCATE( profiles(nvar*nLinePoints), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'WriteHorzLines'
  eMessage = 'Error allocating lines'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nvar*nLinePoints,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( profileNames(nvar), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'WriteHorzLines'
  eMessage = 'Error allocating lineNames'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nvar,' : Error=',irv
  GO TO 9999
END IF

j = 0
DO i=1,nHorzLines
  WRITE(title,'(I3)')i
  IF( horzLineType(i) == HORZ_LINE )THEN
    j = j+1
    profileNames(j) = 'X'//TRIM(ADJUSTL(title))
  ELSE IF( horzLineType(i) == VERT_LINE )THEN
    j = j+1
    profileNames(j) = 'Y'//TRIM(ADJUSTL(title))
  ELSE
    j = j+1
    profileNames(j) = 'X'//TRIM(ADJUSTL(title))
    j = j+1
    profileNames(j) = 'Y'//TRIM(ADJUSTL(title))
  END IF
  j = j+1
  profileNames(j) = 'C'//TRIM(ADJUSTL(title))
  IF( hasVariance )THEN
    j = j+1
    profileNames(j) = 'V'//TRIM(ADJUSTL(title))
  END IF
END DO

jp = 0
DO i=1,nLinePoints
  DO j = 1,nHorzLines
    IF( horzLineType(j) == HORZ_LINE )THEN
      jp = jp + 1
      profiles(jp) = xGrd((i-1)*nHorzLines+j)
    ELSE IF( horzLineType(j) == VERT_LINE )THEN
      jp = jp + 1
      profiles(jp) = yGrd((i-1)*nHorzLines+j)
    ELSE
      jp = jp + 1
      profiles(jp) = xGrd((i-1)*nHorzLines+j)
      jp = jp + 1
      profiles(jp) = yGrd((i-1)*nHorzLines+j)
    END IF
    jp = jp + 1
    profiles(jp) = mFldGrd((i-1)*nHorzLines+j)
    IF( hasVariance )THEN
      jp = jp+1
      profiles(jp) = vFldGrd((i-1)*nHorzLines+j)
    END IF
  END DO
END DO

title = 'Horizontal lines'

IF( LEN_TRIM(file%string) == 0 .OR. INDEX(file%string,'.txt') > 0 )THEN
  CALL write_ascii(lun_out,TRIM(file%string),nvar,nvar,nLinePoints,profileNames,title,profiles,0)
ELSE
  CALL write_xpp(lun_out,TRIM(file%string),nvar,nvar,nLinePoints,profileNames,title,profiles,0)
END IF

9999 CONTINUE

RETURN
END
