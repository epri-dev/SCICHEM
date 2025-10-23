SUBROUTINE ExtractOutput()

USE SCIPtool
USE contour_fd
USE AreaMode_fd
USE Extract_fi
USE GetTimes_fi
USE cmd_fi

IMPLICIT NONE

INTEGER              i,irv,ios

INTEGER, EXTERNAL :: GetProjectPlotLists, GetProjectPlotTimes, GetProjectModes
INTEGER, EXTERNAL :: NumClassData


!==== Action : Get project name if not on command line

CALL GetProject()
IF( nError/= NO_ERROR)GOTO 9999

!==== Action : Retrieve plot lists

irv = GetProjectPlotLists( Project )
IF( irv /= SCIPsuccess )GOTO 9999

irv =  GetProjectPlotTimes( Project )
IF( irv /= SCIPsuccess )GOTO 9999

IF( iTim > 0 )THEN
  TimeOut  => TimeSrf
  nTimeOut = nTimeSrf
  IF( iFld > 0 )THEN
    SELECT CASE( TRIM(cmds(iFld)%cargs) )
    CASE( 'con','ico')
      TimeOut => TimePuff
      nTimeOut = nTimePuff
    END SELECT
  END IF
  IF( tOut < 0. )THEN ! Print available times
    IF( iOut > 0 )THEN
      !-- Write to output file
      OPEN(FILE=outFile,UNIT=lun_ext,IOSTAT=ios)
      IF( ios /= 0 )THEN
        WRITE(6,*)'Error opening output file'
        WRITE(6,*)'File='//TRIM(outFile)
        WRITE(6,*)'Check Path/File name'
        GOTO 9999
      END IF
      DO i = 1,nTimeOut
        WRITE(lun_ext,*,IOSTAT=ios)TimeOut(i)%time%runTime
        IF( ios /= 0 )THEN
          WRITE(6,*)'Error opening writing time to output file'
          WRITE(6,*)'File='//TRIM(outFile)
          WRITE(6,*)'Check Path/File name'
          EXIT
        END IF
      END DO
      CLOSE(lun_ext)
    ELSE
      !-- Write to standard output
      DO i = 1,nTimeOut
        WRITE(6,*,IOSTAT=ios)TimeOut(i)%time%runTime
        IF( ios /= 0 )THEN
          WRITE(6,*)'Error opening writing time to standard output'
          WRITE(6,*)'Time break = ',i
          EXIT
        END IF
      END DO
    END IF
    GOTO 9999
  END IF
END IF

irv = GetProjectModes( )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Action : Set Plot Field

Field%maxCells = limit%surfaceGrid
Field%project  = TRIM(Project%name)
Field%path     = TRIM(Project%path)

CALL SelectField()
IF( nError/= NO_ERROR)GOTO 9999

IF( doFld )THEN

!==== Action : Create Fields

  CALL CreateFields()
  IF( nError/= NO_ERROR )GOTO 9999

  IF( concProfile )THEN
    CALL ExtractProfiles()
    IF( nError/= NO_ERROR )GOTO 9999
  ELSE IF( horzLines )THEN
    CALL ExtractHorzLines()
    IF( nError/= NO_ERROR )GOTO 9999
  ELSE IF( nativeSAG )THEN
  ELSE
  CALL SelectExtractionMethod
  IF( nError/= NO_ERROR )GOTO 9999

  SELECT CASE ( extractMethod )
    CASE( GRID )

      CALL ExtractGridData()
      IF( nError/= NO_ERROR )GOTO 9999

    CASE( CONTOURS )

      CALL ExtractContours()
      IF( nError/= NO_ERROR )GOTO 9999

    CASE DEFAULT
      nError   = UK_ERROR
      eMessage = 'Unknown extraction method: Methods are Grid=0, Contour=1'
      WRITE(eInform,'(A,I0)')'Requested method=',extractMethod
      GOTO 9999
  END SELECT

  END IF
END IF

!==== Action : Write File

CALL WriteFiles()
IF( nError /= NO_ERROR )GOTO 9999

!==== Action : Finish

IF( lun_in  /= 5 )CLOSE( UNIT=lun_in  )
IF( lun_out /= 6 )CLOSE( UNIT=lun_out )
IF( lun_ter /= 0 )CLOSE( UNIT=lun_ter )

9999 CONTINUE
IF( nError /= NO_ERROR )THEN
  CALL showError()
ELSE
  WRITE(6,'(/,A)')'Done'
END IF

!==== Action :  Release memory

CALL ClearMemory()

! Clean up : Exit SCIPtool

IF( lInit )irv = SCIPExitTool()

RETURN
END
!==============================================================================
!  GetProject
!==============================================================================
SUBROUTINE GetProject()

USE Extract_fi

IMPLICIT NONE

CHARACTER(256) file, path

IF( LEN_TRIM(PrjName) <= 0 )THEN
  WRITE(6,101)
  101   FORMAT(/,'Project name: ',$)
  READ(lun_in,'(A)') PrjName
  IF( LEN_TRIM(PrjName) <= 0 )THEN
    nError = EX_ERROR
    GO TO 9999
  END IF
  PrjName = ADJUSTL(PrjName)
ELSE
  PrjName = ADJUSTL(PrjName)
ENDIF

CALL SplitName( PrjName,file,path )
IF( LEN_TRIM(path) < 1 )path = '.'

Project%name    = TRIM(file)
Project%path    = TRIM(path)
Project%ID      = 0
Project%version = 0

9999 CONTINUE

RETURN
END
!==============================================================================
!  SelectExtractionMethod
!==============================================================================
SUBROUTINE SelectExtractionMethod()

USE Extract_fi
USE cmd_fi

IMPLICIT NONE

INTEGER itry, nExtract, i, extractID, ios, iExtract
CHARACTER(4)string

IF( output3D )THEN
  iExtract = 1
  nExtract = 2
ELSE
  iExtract = 0
  nExtract = NUM_EXTRACT
END IF

WRITE(6,'(/,A)')'Available Extraction Methods'
WRITE(6,'(A)')'===================================='
DO i = iExtract,nExtract
  WRITE(6,'(I3,1X,A)')i,TRIM(EX_METHODS(i))
END DO

itry = 0

loop: DO
  itry = itry + 1

  IF( UseKey )THEN
    WRITE(6,'(/,"Extraction Method Keyword? : ",$)')
  ELSE
    WRITE(6,'(/,"Extraction Method Number? : ",$)')
  END IF

  IF ( iFld > 0 )THEN

    string = 'NG'
    IF( LEN_TRIM(extType) > 0 )THEN
      SELECT CASE( TRIM(extType) )
      CASE( 'sag','ntv' )
        string = 'NG'
      CASE( 'ovl' )
        IF( iCnt > 0 )THEN
          string = 'CC'
        ELSE
          string = 'AC'
        END IF
      CASE DEFAULT
        nError   = UK_ERROR
        eMessage = 'Invalid extraction method : '//TRIM(string)
        GOTO 9999
      END SELECT
    END IF

  ELSE

    READ(lun_in,'(A)',IOSTAT=ios)string
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error reading extraction method'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    CALL cupper( string )

  ENDIF

  IF( UseKey )THEN
    string = ADJUSTL(string)
    SELECT CASE( string(1:2) )
      CASE( 'LI' )
        extractID = 0
        EXIT loop
      CASE( 'UG' )
        extractID = 1
        EXIT loop
      CASE( 'CG' )
        extractID = 2
        EXIT loop
      CASE( 'AG' )
        extractID = 3
        EXIT loop
      CASE( 'NG' )
        extractID = 4
        EXIT loop
      CASE( 'UC' )
        extractID = 5
        EXIT loop
      CASE( 'CC' )
        extractID = 6
        EXIT loop
      CASE( 'AC' )
        extractID = 7
        EXIT
      CASE( 'TS' )
        extractID = 8
        EXIT
      CASE DEFAULT
        IF( itry < maxTry )THEN
          WRITE(6,'(/,(A))')'Invalid extraction method : '//TRIM(string)//'. Try again'
          CYCLE
        ELSE
          nError   = UK_ERROR
          eMessage = 'Invalid extraction method : '//TRIM(string)
          GOTO 9999
        END IF
    END SELECT
  ELSE
    string = ADJUSTR(string)
    READ(string,*,IOSTAT=ios)extractID
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error getting extraction number from input'
      WRITE(eInform,'(A,I0)')'error =',ios
    END IF
    IF( extractID < iExtract .OR. extractID > nExtract )THEN
      IF( itry < maxTry )THEN
        WRITE(6,'(/,(A))')'Invalid extraction number : '//TRIM(string)//'. Try again'
        CYCLE
      ELSE
        nError   = UK_ERROR
        eMessage = 'Invalid extraction number : '//TRIM(string)
        GOTO 9999
      END IF
    ELSE
      EXIT loop
    END IF
  END IF
END DO loop

WRITE(6,'(/,A,/)')'Using Extraction method '//TRIM(EX_METHODS(extractID))

IF( extractID <= NUM_EXTRACT_GRID )THEN
  extractMethod = GRID
  GridType = extractID
  IF( gridType == LINE1D )THEN
    IF( Field%category == HP_VSLICE )THEN
      LineType = VERT_LINE
    ELSE
      LineType = HORZ_LINE
    END IF
  END IF
ELSE
  extractMethod = CONTOURS
  ContourType = extractID - NUM_EXTRACT_GRID
END IF

9999 CONTINUE

RETURN
END
