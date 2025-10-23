INTEGER FUNCTION FileInput( restart )

!------ Read input/output file names from command line or use default file names
!       Check if input exists and open

USE SCIPUFFdriver_fi
#if !defined(LINUX)
USE swinAPI

#endif
IMPLICIT NONE

LOGICAL, INTENT( OUT ) :: restart

INTEGER numarg, irv, ios
LOGICAL lexist

CHARACTER(128) string1, string2, string3

CHARACTER(128), EXTERNAL :: StripNull

FileInput = FAILURE

!------ Default values

fname   = 'scipuff.inp'
prjname = 'scipuff_prj'
restart = .FALSE.

!------ Get arguments from command line

numarg = COMMAND_ARGUMENT_COUNT()

IF( numarg > 0 )THEN

  CALL getarg(1,fname)   !Input file name with full path

  SELECT CASE( TRIM(fname(1:4)) )
    CASE( 'RST:','rst:' )
      restart = .TRUE.
      fname = fname(5:)
  END SELECT

#if !defined(LINUX)
ELSE

  INQUIRE(file=fname,EXIST=lexist)         !Check current directory
  IF( .NOT.lexist )THEN                    !Otherwise, check location of executable
    CALL SplitName( fname,string2,string3 )
    fname = StripNull( string2 )
    irv = GetModuleFileName( 0,string1,LEN(string1) )
    CALL SplitName( string1(1:irv),string2,string3 )
    CALL AddPath( fname,TRIM(string3) )
  END IF

#endif
END IF

CALL SplitName( fname,string2,string3 )
path_in = StripNull( string3 )

IF( numarg > 1 )THEN
   CALL getarg(2,prjname)
ELSE
   CALL AddPath( prjname,TRIM(path_in) )
END IF

INQUIRE( FILE=TRIM(fname),EXIST=lexist,IOSTAT=ios )
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error inquiring input file name'
  GOTO 9999
END IF

IF( .NOT.lexist )THEN
  WRITE(*,'(A)') 'Input file does not exist'
  WRITE(*,'(A)') 'File name = '//TRIM(fname)
  GOTO 9999
END IF

OPEN(FILE=TRIM(fname),UNIT=lun,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error opening input file name'
  WRITE(*,'(A)') 'File name = '//TRIM(fname)
  GOTO 9999
END IF

IF( restart )THEN

  WRITE(6,*)
  WRITE(6,*)'Loading input from '//TRIM(prjname)

END IF

FileInput = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

CHARACTER(*) FUNCTION BuildFileNameAERMOD( string,line,path_in )

!------ Build file name from string: strip quotes and add path if none included
!       in string

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string  !First string after pathway & keyword
CHARACTER(*), INTENT( IN ) :: line    !Full line
CHARACTER(*), INTENT( IN ) :: path_in !Project path

CHARACTER(1), PARAMETER :: Q  = CHAR(39)  !Single quote
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

INTEGER i

CHARACTER(256) string1, string2, string3

CHARACTER(256), EXTERNAL :: StripNull

!------ Check for quotes (used to specify file names with spaces)
!       N.B. Assumes quotes are only used for file names

i = INDEX(line,QQ)
IF( i == 0 )THEN
  i = INDEX(line,Q)
END IF

!------ No quotes: use first string after pathway/keyword

IF( i == 0 )THEN

  BuildFileNameAERMOD = TRIM(string)

ELSE !- Use string between quotes

  string1 = line(i+1:)
  i = INDEX(string1,QQ)
  IF( i == 0 )THEN
    i = INDEX(string1,Q)
  END IF

  IF( i == 0 )THEN
    BuildFileNameAERMOD = TRIM(string1)
  ELSE
    BuildFileNameAERMOD = TRIM(string1(1:i-1))
  END IF

END IF

IF( LEN_TRIM( StripNull( path_in ) ) > 0 )THEN
  string3 = TRIM(BuildFileNameAERMOD)
  CALL SplitName( string3,string1,string2 ) !path in string2
  string2 = StripNull( string2 )
  IF( LEN_TRIM(string2) == 0 )CALL AddPath( string3,TRIM(path_in) )
  BuildFileNameAERMOD = TRIM(string3)
END IF

RETURN
END


