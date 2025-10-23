!==============================================================================
! setPlotClass3D
!==============================================================================
SUBROUTINE setPlotClass3D()

USE Extract_fi

IMPLICIT NONE

INTEGER icls, j, i

CHARACTER(12), DIMENSION(2) :: string
INTEGER,       DIMENSION(2) :: iclass

WRITE(6,*)'Running OutputExtract for 3D Concentration output'

Field%class = 0

MetField = Field
mxclass   = 2

string(1) = 'Concentration'
string(2) = 'Met/Terrain'

iClass = 0

DO icls = 1,mxclass
  j = LEN_TRIM(string(icls))
  DO i = 1,nClass
    IF( string(icls)(1:j) == ClassStr(i)%string(1:j) )THEN
      iclass(icls) = i
      EXIT
    END IF
  END DO
END DO

IF( iClass(1) == 0 )THEN
  nError = LI_ERROR
  eMessage ='Cannot find '//TRIM(string(1))//' in project plot classes'
  eRoutine = 'setPlotClass3D'
  GO TO 9999
ELSE
  Field%class = iClass(1)
END IF

IF( iClass(2) == 0 )THEN
  WRITE(6,*)'Terrain not available in this project'
  mxclass = 1
ELSE
  MetField%class = iClass(2)
  WRITE(6,'(/,"MetField class : ",I3,A/)')MetField%class,' ('//TRIM(ClassStr(MetField%class)%string)//')'
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! createOutput3D
!==============================================================================
SUBROUTINE createOutput3D()

USE Extract_fi

IMPLICIT NONE

INTEGER i

INTEGER, EXTERNAL :: PostCreateField

CALL createVerticalGrid()
IF( nError /= NO_ERROR )GOTO 9999

i = nz
IF( MetField%class > 0 )i = i + 1

CALL allocateFields(i)
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,nz
  Fields(i) = Field
  ClassData(CD_ZMIN) = zGrd(i)
  FieldIds(i) = PostCreateField( Fields(i), ClassData, (i==1), '3D Concentration' )
  IF( FieldIds(i) < 0 )GOTO 9999
END DO

IF( MetField%class > 0 )THEN
  Fields(nz+1) = MetField
  FieldIds(nz+1) = PostCreateField( Fields(nz+1), MetClassData, .FALSE., 'Terrain' )
  IF( FieldIds(nz+1) < 0 )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! createVerticalGrid
!==============================================================================
SUBROUTINE createVerticalGrid()

USE Extract_fi

IMPLICIT NONE

INTEGER i, irv

WRITE(6,'(A,T26,A,$)')'Grid : (Zmin,Zmax,Nz)',': '
READ(lun_in,*)Zmin,Zmax,nz
IF( nz <= 0 )THEN
 nError = UK_ERROR
 eRoutine = 'createVerticalGrid'
 WRITE(eInform,'(A,I0)')'Invalid number of vertical grid points: ',nz
 GO TO 9999
END IF

IF( zMin > zMax ) THEN
 dz = zMin
 zMin = zMax
 zMax = dz
END IF

ALLOCATE( zGrd(nz), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'createVerticalGrid'
  eMessage = 'Error : allocating z Location array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',nz,' : error =',irv
  GOTO 9999
END IF

IF( nz <= 1 )THEN
  dz = (zMax - zMin)
ELSE
  dz = (zMax - zMin)/FLOAT(nz-1)
END IF
DO i = 1,nz
  zGrd(i) = zMin + (i-1)*dz
END DO

9999 CONTINUE

RETURN
END
!==============================================================================
! extractOutput3D
!==============================================================================
SUBROUTINE extractOutput3D()

USE Extract_fi

IMPLICIT NONE

INTEGER i

DO i = 1,nz
  CALL ExtractGrid( FieldIds(i), Fields(i), i, hasVariance )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

IF( MetField%class > 0 )THEN
  i = nz+1
  CALL ExtractGrid( FieldIds(i), Fields(i), i, .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
! WriteOutput3D
!==============================================================================
SUBROUTINE WriteOutput3D( file )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

TYPE( fileNameT ) :: file

INTEGER i, k, irv
CHARACTER(128) string

file%string = TRIM(file%string)//'.cdat'
lun_out = 106
OPEN(UNIT=lun_out,FILE=file%string,STATUS='UNKNOWN',IOSTAT=irv)
IF( irv /= 0 )THEN
  WRITE(6,*)'Error opening concentration output file'
  WRITE(6,*)'file=',TRIM(file%string)
  GO TO 9999
END IF
WRITE(6,'(/,"Concentration output file: ",A,//)')TRIM(file%string)

file%string = file%string(1:LEN_TRIM(file%string)-4)//'tdat'
lun_ter = 107
OPEN(UNIT=lun_ter,FILE=file%string,STATUS='UNKNOWN',IOSTAT=irv)
IF( irv /= 0 )THEN
  WRITE(6,*)'Error opening terrain output file'
  WRITE(6,*)'file=',TRIM(file%string)
  GO TO 9999
END IF
WRITE(6,'(/,"Terrain output file: ",A,//)')TRIM(file%string)


WRITE(lun_out,'(A)')'Variable      : '//TRIM(ChoiceStr(Fields(1)%choice)%string)
WRITE(string,*)TimeOut(Fields(1)%timeID)%time%runTime
string = ADJUSTL(string)
WRITE(lun_out,'(A)')'Time          : '//TRIM(string)//'  ('//TimeOut(Fields(1)%timeID)%string//')'
WRITE(lun_out,'(3(1pE13.5,1x,1pE13.5,1x,I5,1x))')xGrd(1),xGrd(nxy),nx,yGrd(1),yGrd(nxy),ny,zGrd(1),zGrd(nz),nz
WRITE(lun_ter,'(A)')'Creator       : '//TRIM(CODE_NAME)//'   : '//TRIM(CODE_VERSION)
WRITE(lun_ter,'(A)')'              : SCIPtool   : '//TRIM(toolString%string)
WRITE(lun_ter,'(A)')'Project       : '//TRIM(Project%name)
WRITE(lun_ter,'(A)')'Path          : '//TRIM(Project%path)
IF( MetField%class > 0 )THEN
  WRITE(lun_ter,'(A)')'Variable      : Terrain'
ELSE
  WRITE(lun_ter,'(A)')'Variable      : Cannot find Terrain'
ENDIF
DO i = 1,nxy
  DO k = 1,nz+1
    IF( k > nz .AND. MetField%class <= 0 )EXIT
    IF( k > nz )THEN
      WRITE(lun_ter,'(1PE13.5)',ADVANCE='NO')dFldGrd(i,k,1)
    ELSE
      WRITE(lun_out,'(1PE13.5)',ADVANCE='NO')dFldGrd(i,k,1)
    END IF
  END DO
END DO
WRITE(lun_out,'()',ADVANCE='YES')
WRITE(lun_ter,'()',ADVANCE='YES')

9999 CONTINUE

RETURN
END
