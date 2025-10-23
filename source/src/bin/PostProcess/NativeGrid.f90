SUBROUTINE ExtractNative( fID,thisField )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

INTEGER                :: fID
TYPE( SCIPPlotFieldT ) :: thisField

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'ExtractNative'

INTEGER :: irv

!------------------------------------------------------------------------------
!Get number of field nodes/triangles
!------------------------------------------------------------------------------
nNode     = 0
nTriangle = 0
irv = SCIPGetFieldSize( callerID,fID,thisField,plotType,nNode,nTriangle )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed to get Field Size from SCIPtool' )
  GOTO 9999
END IF

!------------------------------------------------------------------------------
!allocate space for nodes and triangles
!------------------------------------------------------------------------------

CALL allocateNodes( nNode,nTriangle )
IF( nError /= NO_ERROR )GOTO 9999

!------------------------------------------------------------------------------
!get nodes and triangles
!------------------------------------------------------------------------------

irv = SCIPGetField( callerID,fID,thisField,plotType,fNodes,fTriangles )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed to get Field from SCIPtool' )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
! WriteNativeGrid()
!******************************************************************************
SUBROUTINE WriteNativeGrid(file)

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER irv, i, ios
CHARACTER(128) string

INTEGER, DIMENSION(:), ALLOCATABLE :: iMax
REAL,    DIMENSION(:), ALLOCATABLE :: values

ALLOCATE( values(nNode),STAT=irv )
IF( irv /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating array to find node max'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nNode,' : Error=',irv
  GOTO 9999
END IF

values(1:nNode) = fNodes(1:nNode)%v

ALLOCATE( iMax(1),STAT=irv )
IF( irv /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'WriteNativeGrid'
  eMessage = 'Error allocating iMax'
  WRITE(eInform,'(A,I0,A)')'Request = 1 : Error=',irv
  GOTO 9999
END IF

iMax = MAXLOC(values)

i = iMax(1)
string = 'Native SAG nodes'
WRITE(lun_out,'(A)')TRIM(string)
IF( Fields(1)%timeID > nTimeOut )THEN
  WRITE(string,*)Fields(1)%userTime
ELSE
  IF( Fields(1)%timeID == 0 )THEN
    string = '0'
  ELSE
    WRITE(string,*)TimeOut(Fields(1)%timeID)%time%runTime
    string = TRIM(string)//'  ('//TimeOut(Fields(1)%timeID)%string//')'
  END IF
END IF
string = ADJUSTL(string)
WRITE(lun_out,'(A)')'Time           : '//TRIM(string)
WRITE(lun_out,'(A)')'Field Class    : '//TRIM(ClassStr(Fields(1)%class)%string)
WRITE(lun_out,'(A)')'Field Choice   : '//TRIM(ChoiceStr(Fields(1)%choice)%string)
WRITE(lun_out,'(A)')'Field Category : '//TRIM(CATEGORY_STRING(Fields(1)%category))
IF( plotType%type /= HP_MEAN )THEN
  WRITE(lun_out,'(A)')'Field Type     : '//TRIM(TYPE_STRING(plotType%type))
  SELECT CASE( PlotType%type )
  CASE( HP_PROB )
    WRITE(lun_out,'(A,F13.5)')'Exceedence value  =',plotType%data
  CASE( HP_EXCEED )
    WRITE(lun_out,'(A,F13.5)')'Probability value =',plotType%data
  CASE DEFAULT
  END SELECT
END IF
WRITE(lun_out,'("Field max location and value : ",$)')
WRITE(lun_out,'(2G16.7,1PE16.6)',IOSTAT=ios)fNodes(i)%x,fNodes(i)%y,fNodes(i)%v

CALL writeNodes(file)

9999 CONTINUE

IF( ALLOCATED(iMax  ) )DEALLOCATE( iMax  ,STAT=irv )
IF( ALLOCATED(values) )DEALLOCATE( values,STAT=irv )

RETURN
END
!******************************************************************************
! writeNodes
!******************************************************************************
SUBROUTINE writeNodes( file )

USE Extract_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER, PARAMETER :: lun_tri = 77

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'writeNodes'

INTEGER :: i, ios

CHARACTER(32)  :: string

CHARACTER(PATH_MAXLENGTH) :: triFile

WRITE(string,*,IOSTAT=ios)nNode
IF( ios /= 0 )GOTO 9999

WRITE(lun_out,'(A)',IOSTAT=ios)'!NODES : '//TRIM(ADJUSTL(string))
IF( ios /= 0 )GOTO 9999

IF( Fields(1)%coordinate%mode == HD_LATLON )THEN
  DO i = 1,nNode
    WRITE(lun_out,'(I6,4G16.7,1PE16.6)',IOSTAT=ios)fNodes(i)%ID,fNodes(i)%y,fNodes(i)%x,fNodes(i)%hy,fNodes(i)%hx,fNodes(i)%v
    IF( ios /= 0 )GOTO 9999
  END DO
ELSE
  DO i = 1,nNode
    WRITE(lun_out,'(I8,4G16.7,1PE16.6)',IOSTAT=ios)fNodes(i)%ID,fNodes(i)%x,fNodes(i)%y,fNodes(i)%hx,fNodes(i)%hy,fNodes(i)%v
    IF( ios /= 0 )GOTO 9999
  END DO
END IF

WRITE(string,*,IOSTAT=ios)nTriangle
IF( ios /= 0 )GOTO 9999

i = INDEX(file%STRING,'.', BACK = .TRUE.)
triFile = file%STRING(1:i)//'tri'

OPEN(lun_tri,file=TRIM(triFile),IOSTAT=ios)
IF( ios /= 0 )GOTO 9999

WRITE(lun_tri,'(A)',IOSTAT=ios)'!TRIANGLES : '//TRIM(ADJUSTL(string))
IF( ios /= 0 )GOTO 9999

DO i = 1,nTriangle
  WRITE(lun_tri,'(4(I8,1x))',IOSTAT=ios)fTriangles(i)%ID,fTriangles(i)%nidA,fTriangles(i)%nidB,fTriangles(i)%nidC
  IF( ios /= 0 )GOTO 9999
END DO

CLOSE(lun_tri)

9999 CONTINUE

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = ROUTINE_NAME
  eMessage = 'Error writing node values'
END IF

RETURN
END
