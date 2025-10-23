!            sysCurrentDirectory(path)
!*******************************************************************************
INTEGER FUNCTION SYSCURRENTDIRECTORY( path )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSCURRENTDIRECTORY

USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*), INTENT ( OUT ) :: path

INTEGER irv, len_path

CHARACTER(MAX_PATHC) path_tmp
CHARACTER(MAX_PATHF) strip_path

CHARACTER(MAX_PATHF), EXTERNAL :: STRIPNULL

#if defined(LINUX)
len_path = INT( LEN( path_tmp ),KIND=4 )
#else
len_path = LEN( path_tmp )
#endif

irv = GetCurrentDirectory( len_path,path_tmp )

IF( irv == NULL .OR. irv >= len_path )THEN
  path = ' '
  SYSCURRENTDIRECTORY = SCIPfailure
ELSE
  strip_path = STRIPNULL(path_tmp)
  irv        = LEN_TRIM(strip_path)
  path       = strip_path(1:irv)
  SYSCURRENTDIRECTORY = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysSetDirectory(path)
!*******************************************************************************
INTEGER FUNCTION SYSSETDIRECTORY( path )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSSETDIRECTORY

USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*), INTENT ( IN ) :: path

CHARACTER(MAX_PATHC) path_tmp

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

INTEGER irv

SYSSETDIRECTORY = SCIPfailure

IF( LEN_TRIM(path) <= MAX_PATHF )THEN
  path_tmp = AddNull( TRIM(path) )

  irv = SetCurrentDirectory( path_tmp )

  IF(irv == TRUE )SYSSETDIRECTORY = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysRemoveDirectory(path)
!*******************************************************************************
INTEGER FUNCTION SYSREMOVEDIRECTORY( path )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSREMOVEDIRECTORY

USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

CHARACTER(MAX_PATHC) path_tmp

INTEGER irv

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSREMOVEDIRECTORY = SCIPfailure

IF( LEN_TRIM(path) <= MAX_PATHF )THEN
  path_tmp = AddNull(TRIM(path))

  irv = RemoveDirectory( path_tmp )

  IF( irv == TRUE )SYSREMOVEDIRECTORY = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysCreateDirectory(path)
!*******************************************************************************
INTEGER FUNCTION SYSCREATEDIRECTORY( path )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSCREATEDIRECTORY

USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

CHARACTER(MAX_PATHC) path_tmp

INTEGER irv

TYPE ( T_SECURITY_ATTRIBUTES ) sa

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSCREATEDIRECTORY = SCIPfailure

IF( LEN_TRIM(path) <= MAX_PATHF )THEN
  path_tmp = AddNull( TRIM(path) )

  sa%nLength              = 12
  sa%lpSecurityDescriptor = NULL
  sa%bInheritHandle       = FALSE

  irv = CreateDirectory( path_tmp,sa )

  IF( irv == TRUE )SYSCREATEDIRECTORY = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysDriveType(path)
!*******************************************************************************
INTEGER FUNCTION SYSDRIVETYPE( path )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSDRIVETYPE

USE MaxLength_fd
USE SwinAPI

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

CHARACTER(MAX_PATHC) drive

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

IF( path(2:2) == ':' )THEN
  drive = AddNull( path(1:2) )
ELSE
  drive = AddNull( TRIM(path) )
END IF

SYSDRIVETYPE = GetDriveType( drive )

RETURN
END
!*******************************************************************************
!            sysSleep(mSec)
!*******************************************************************************
SUBROUTINE SYSSLEEP( mSec )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSSLEEP

USE SwinAPI

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mSec

CALL Sleep( mSec )

RETURN
END
!*******************************************************************************
!            sysGetLastError()
!*******************************************************************************
INTEGER FUNCTION SYSGETLASTERROR()

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETLASTERROR

USE SwinAPI

IMPLICIT NONE

SYSGETLASTERROR = GetLastError()

RETURN
END
!*******************************************************************************
!            sysDeletefile(filex)
!*******************************************************************************
INTEGER FUNCTION SYSDELETEFILE( filex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSDELETEFILE

USE MaxLength_fd
USE SwinAPI
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: filex

INTEGER irv
LOGICAL lexist

CHARACTER(MAX_FILEC) filexname

CHARACTER(MAX_FILEC), EXTERNAL :: AddNull

INQUIRE( FILE=filex,EXIST=lexist )

IF( lexist )THEN

  SYSDELETEFILE = SCIPfailure

  IF( LEN_TRIM(filex) <= MAX_FILEF )THEN
    filexname = AddNull( TRIM(filex) )
    irv       = DeleteFile( filexname )

    IF( irv == TRUE )SYSDELETEFILE = SCIPsuccess
  END IF

ELSE

  SYSDELETEFILE = SCIPnull

END IF

RETURN
END
!*******************************************************************************
!            sysCopyfile(fromfilex,tofilex)
!*******************************************************************************
INTEGER FUNCTION SYSCOPYFILE( fromfilex,tofilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSCOPYFILE

USE MaxLength_fd
USE SwinAPI
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: fromfilex
CHARACTER(*), INTENT( IN ) :: tofilex

INTEGER irv

CHARACTER(MAX_FILEC) infilex,outfilex

CHARACTER(MAX_FILEC), EXTERNAL :: AddNull

SYSCOPYFILE = SCIPfailure

IF( LEN_TRIM(fromfilex) <= MAX_FILEF .AND. LEN_TRIM(tofilex) <= MAX_FILEF )THEN
  infilex  = AddNull( TRIM(fromfilex) )
  outfilex = AddNull( TRIM(tofilex) )

  irv = CopyFile( infilex,outfilex,FALSE )

  IF( irv == TRUE )SYSCOPYFILE = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysGetProfileString(section,key,default,result,INIfilex)
!*******************************************************************************
INTEGER FUNCTION SYSGETPROFILESTRING( section,key,cdefault,cresult,INIfilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETPROFILESTRING

USE MaxLength_fd
USE SCIPresults_fd
USE INI

IMPLICIT NONE

CHARACTER(1), PARAMETER :: COMMENT = '#'

CHARACTER(*), INTENT ( IN  ) :: section
CHARACTER(*), INTENT ( IN  ) :: key
CHARACTER(*), INTENT ( IN  ) :: cdefault
CHARACTER(*), INTENT ( OUT ) :: cresult
CHARACTER(*), INTENT ( IN  ) :: INIfilex

CHARACTER(MAX_PATHC) s1, s2, s3, s4, s5, nf

INTEGER irv, lens4

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull
CHARACTER(MAX_PATHF), EXTERNAL :: StripNull

SYSGETPROFILESTRING = SCIPfailure

cresult = cdefault

IF( MAX(LEN_TRIM(section),LEN_TRIM(key),LEN_TRIM(cdefault),LEN_TRIM(INIfilex)) <= MAX_PATHF )THEN

  nf = AddNull( '<empty>' )

  s1 = AddNull( TRIM(section) )
  s2 = AddNull( TRIM(key) )

  IF( LEN_TRIM(cdefault) == 0 )THEN
    s3 = AddNull( TRIM(nf) )
  ELSE
    s3 = AddNull( TRIM(cdefault) )
  END IF

  s5 = AddNull( TRIM(INIfilex) )

  lens4 = LEN(s4)
#if defined(WININI_OPT)
  irv = GetPrivateProfileString(s1,s2,s3,s4,lens4,s5)
#else
  irv = get_private_profile_string(s1,s2,s3,s4,lens4,s5)
#endif

  IF( irv > lens4 - 1 )THEN
    SYSGETPROFILESTRING = SCIPfailure
  ELSE IF( s4(1:irv) == nf(1:irv) )THEN
    SYSGETPROFILESTRING = SCIPnull
  ELSE
    cresult = TRIM(StripNull(s4))
    irv = INDEX(cresult,COMMENT)
    IF( irv > 0 )cresult(irv:) = ' '
    SYSGETPROFILESTRING = SCIPsuccess
  END IF
END IF

RETURN
END
!*******************************************************************************
!            sysWriteProfileString(section,key,string,INIfilex)
!*******************************************************************************
INTEGER FUNCTION SYSWRITEPROFILESTRING( section,key,string,INIfilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSWRITEPROFILESTRING

USE MaxLength_fd
USE SCIPresults_fd
USE basic_fd
USE INI

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: section
CHARACTER(*), INTENT ( IN  ) :: key
CHARACTER(*), INTENT ( IN  ) :: string
CHARACTER(*), INTENT ( IN  ) :: INIfilex

CHARACTER(MAX_PATHC) s1, s2, s3, s4

INTEGER irv

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSWRITEPROFILESTRING = SCIPfailure

IF( MAX(LEN_TRIM(section),LEN_TRIM(key),LEN_TRIM(string),LEN_TRIM(INIfilex)) <= MAX_PATHF )THEN
  s1 = AddNull( TRIM(section) )
  s2 = AddNull( TRIM(key) )
  s3 = AddNull( TRIM(string) )
  s4 = AddNull( TRIM(INIfilex) )

#if defined(WININI_OPT)
  irv = WritePrivateProfileString( s1,s2,s3,s4 )
#else
  irv = write_private_profile_string( s1,s2,s3,s4 )
#endif

  IF( irv == TRUE )SYSWRITEPROFILESTRING = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysDeleteProfileString(section,key,INIfilex)
!*******************************************************************************
INTEGER FUNCTION SYSDELETEPROFILESTRING( section,key,INIfilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSDELETEPROFILESTRING

USE MaxLength_fd
USE SCIPresults_fd
USE basic_fd
USE INI
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: section
CHARACTER(*), INTENT ( IN  ) :: key
CHARACTER(*), INTENT ( IN  ) :: INIfilex

CHARACTER(MAX_PATHC) s1, s2, s4

#if defined(WININI_OPT)
CHARACTER(1) s3
#else
INTEGER(C_INTPTR_T) s3
#endif

INTEGER irv

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSDELETEPROFILESTRING = SCIPfailure

IF( MAX(LEN_TRIM(section),LEN_TRIM(key),LEN_TRIM(INIfilex)) <= MAX_PATHF )THEN
  s1 = AddNull( TRIM(section) )
  s2 = AddNull( TRIM(key) )
#if defined(WININI_OPT)
  s3 = CHAR(0)
#else
  s3 = 0
#endif
  s4 = AddNull( TRIM(INIfilex) )

#if defined(WININI_OPT)
  irv = WritePrivateProfileString( s1,s2,s3,s4 )
#else
  IF( LEN_TRIM(key) <= 0 )THEN
    irv = delete_private_profile_section( s1,s3,s3,s4 )
  ELSE
    irv = delete_private_profile_string( s1,s2,s3,s4 )
  END IF
#endif

  IF( irv == TRUE )SYSDELETEPROFILESTRING = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysGetProfileInt(section,key,default,result,INIfilex)
!*******************************************************************************
INTEGER FUNCTION SYSGETPROFILEINT( section,key,idefault,iresult,INIfilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETPROFILEINT

USE MaxLength_fd
USE SCIPresults_fd
USE INI

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: section
CHARACTER(*), INTENT ( IN  ) :: key
INTEGER,      INTENT ( IN  ) :: idefault
INTEGER,      INTENT ( OUT ) :: iresult
CHARACTER(*), INTENT ( IN  ) :: INIfilex

CHARACTER(MAX_PATHC) s1, s2, s5
INTEGER        s3

INTEGER irv

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSGETPROFILEINT = SCIPfailure

IF( MAX(LEN_TRIM(section),LEN_TRIM(key),LEN_TRIM(INIfilex)) <= MAX_PATHF )THEN
  s1 = AddNull( TRIM(section) )
  s2 = AddNull( TRIM(key) )
  s3 = idefault
  s5 = AddNull( TRIM(INIfilex) )

#if defined(WININI_OPT)
  irv = GetPrivateProfileInt( s1,s2,s3,s5 )
#else
  irv = get_private_profile_int( s1,s2,s3,s5 )
#endif

  iresult = irv

  SYSGETPROFILEINT = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysWriteProfileInt(section,key,string,INIfilex)
!*******************************************************************************
INTEGER FUNCTION SYSWRITEPROFILEINT( section,key,ivalue,INIfilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSWRITEPROFILEINT

USE MaxLength_fd
USE SCIPresults_fd
USE basic_fd
USE INI

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: section
CHARACTER(*), INTENT ( IN  ) :: key
INTEGER,      INTENT ( IN  ) :: ivalue
CHARACTER(*), INTENT ( IN  ) :: INIfilex

CHARACTER(16)  string
CHARACTER(MAX_PATHC) s1,s2,s3,s4

INTEGER irv, ios

CHARACTER(MAX_PATHC), EXTERNAL :: AddNull

SYSWRITEPROFILEINT = SCIPfailure

WRITE( string,*,IOSTAT=ios )ivalue
IF( ios /= 0 )GOTO 9999

IF( MAX(LEN_TRIM(section),LEN_TRIM(key),LEN_TRIM(INIfilex)) <= MAX_PATHF )THEN

  s1 = AddNull( TRIM(section) )
  s2 = AddNull( TRIM(key) )
  s3 = AddNull( TRIM(ADJUSTL(string)) )
  s4 = AddNull( TRIM(INIfilex) )

#if defined(WININI_OPT)
  irv = WritePrivateProfileString(s1,s2,s3,s4)
#else
  irv = write_private_profile_string(s1,s2,s3,s4)
#endif

  IF( irv == TRUE )SYSWRITEPROFILEINT = SCIPsuccess
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                STRIPEXTENSION
!*******************************************************************************
CHARACTER(*) FUNCTION STRIPEXTENSION( filex )

!DEC$ ATTRIBUTES DLLEXPORT :: STRIPEXTENSION

USE MaxLength_fd
USE basic_fd

IMPLICIT NONE

CHARACTER(1),PARAMETER :: DOT        ='.'

CHARACTER(*), INTENT (IN) :: filex

INTEGER np, ns, i

CHARACTER(MAX_FILEF) tempString

tempString = filex

ns = LEN_TRIM(tempString)

np = ns
DO i = ns,1,-1
  IF( tempString(i:i) == PATH_SEPARATOR )THEN
    EXIT
  ELSE IF( tempString(i:i) == DOT )THEN
    np = i-1
    EXIT
  END IF
END DO

IF( np < ns )tempString(np+1:)= ' '

STRIPEXTENSION = TRIM(tempString)

RETURN
END
!*******************************************************************************
!                AddPath
!*******************************************************************************
SUBROUTINE ADDPATH( file,path )

!DEC$ ATTRIBUTES DLLEXPORT :: ADDPATH

USE MaxLength_fd
USE basic_fd

IMPLICIT NONE

CHARACTER(*), INTENT ( INOUT ) :: file
CHARACTER(*), INTENT ( IN )    :: path

CHARACTER(MAX_FILEF) string

INTEGER np, ns

np = LEN_TRIM(path)
IF( np <= 0 )RETURN

IF( ICHAR(path(np:np) ) == 0)np = np - 1
IF( np <= 0 )RETURN

string(1:) = path(1:np)
ns = np

IF( string(ns:ns) /= PATH_SEPARATOR )THEN
  IF( ns < LEN(string) )string(ns+1:ns+1) = PATH_SEPARATOR
  ns = ns + 1
END IF

np = LEN_TRIM(file)
IF( np > 0 )THEN
  IF( ICHAR(file(np:np)) == 0 )np = np - 1
  IF( np > 0 )THEN
   IF( ns < LEN(string) )string(ns+1:) = file(1:np)
    ns = ns + np
  END IF
END IF

IF( ns <= LEN(string) .AND. ns <= LEN(file) )THEN
  file(1:) = string(1:ns)
END IF

file = ADJUSTL(file)

RETURN
END
!*******************************************************************************
!                SUBPATHSEP
!*******************************************************************************
SUBROUTINE SUBPATHSEP( name )

!--- Replace  PATH_SEP_SUB with PATH_SEPARATOR character

!DEC$ ATTRIBUTES DLLEXPORT :: SUBPATHSEP

USE basic_fd

IMPLICIT NONE

CHARACTER(*), INTENT ( INOUT ) :: name

CHARACTER(1) PATH_SEP_SUB
INTEGER i,np

#if defined(LINUX)
PATH_SEP_SUB = CHAR(92) ! back slash "\"
#else
PATH_SEP_SUB = CHAR(47) ! forward slash "/"
#endif

np = LEN_TRIM(name)
IF( ICHAR(name(np:np)) == 0 )np = np - 1
IF( np == 0 )RETURN

DO i = 1,np
  IF( name(i:i) == PATH_SEP_SUB )name(i:i) = PATH_SEPARATOR
END DO

RETURN
END
!*******************************************************************************
!                SplitName
!*******************************************************************************
SUBROUTINE SPLITNAME( name,file,path )

!DEC$ ATTRIBUTES DLLEXPORT :: SPLITNAME

USE basic_fd

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: name
CHARACTER(*), INTENT ( OUT ) :: file
CHARACTER(*), INTENT ( OUT ) :: path

 INTEGER np

np = LEN_TRIM(name)
IF( np == 0 )THEN
  path = ' '
  file = ' '
  RETURN
END IF

IF( ICHAR(name(np:np)) == 0 )np = np - 1

DO WHILE( name(np:np)/=PATH_SEPARATOR .AND. np > 1 )
  np = np - 1
END DO

IF( np <= 1 )THEN
  IF( LEN_TRIM(name) > 1 )THEN
    IF( name(1:1) == PATH_SEPARATOR )THEN
      file = TRIM(name(2:))
    ELSE
      file = TRIM(name)
    END IF
  ELSE
    file = TRIM(name)
  END IF
  path = ' '
ELSE
  file = TRIM(name(np+1:))
  path = TRIM(name(1:np-1))
END IF

RETURN
END
!*******************************************************************************
!                RemoveExtension
!*******************************************************************************
SUBROUTINE REMOVEEXTENSION( filex )

!DEC$ ATTRIBUTES DLLEXPORT :: REMOVEEXTENSION

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT (INOUT) :: filex

CHARACTER(MAX_FILEF),EXTERNAL :: STRIPEXTENSION

filex = TRIM( STRIPEXTENSION(filex) )

RETURN
END
!*******************************************************************************
!                BackUpPath
!*******************************************************************************
SUBROUTINE BACKUPPATH( path,nlevel )

USE basic_fd

!DEC$ ATTRIBUTES DLLEXPORT :: BACKUPPATH

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: path
INTEGER,      INTENT( IN    ) :: nlevel

INTEGER nlo, nc, i, nl, np
LOGICAL trail, term

!==== initialize

nc = LEN_TRIM(path)

IF( nc < 1 )RETURN

!==== Check for trailing null

term = path(nc:nc) == CHAR(0)
IF( term )nc = nc - 1

!==== Check for trailing slash

trail = path(nc:nc) == PATH_SEPARATOR
IF( trail )nc = nc - 1

!==== count current levels

nlo = 0
DO i = 1,nc
  IF( path(i:i) == PATH_SEPARATOR )nlo = nlo+1
END DO

IF( nlo <= 0 )RETURN

!==== Set number of levels to keep

nl = MAX(0,nlo - nlevel)

!==== Position string

i   = 0
nlo = 0
DO WHILE( i < nc .and. nlo <= nl )
  i = i + 1
  IF( path(i:i) == PATH_SEPARATOR )THEN
    nlo = nlo+1
    np = i
  END IF
END DO

!==== Remove trailing slash IF original was missing

IF( .NOT.trail )np = np - 1

!==== Check

nlo = 0
DO i = 1,np-1
  IF( path(i:i) == PATH_SEPARATOR )nlo = nlo+1
END DO
IF( nlo /= nl )GOTO 9999

!==== Reset

IF( np <= 0 )THEN
  path =' '
ELSE
  IF( term )THEN
    IF( np+1 <= LEN_TRIM(path) )THEN
      path(np+1:np+1) = CHAR(0)
      np = np + 1
    END IF
  END IF
  IF( np+1 <= LEN_TRIM(path) )THEN
    path(np+1:) = ' '
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SplitExtension
!*******************************************************************************
SUBROUTINE SPLITEXTENSION( filex,name,ext )

!DEC$ ATTRIBUTES DLLEXPORT :: SPLITEXTENSION

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT ( IN  ) :: filex
CHARACTER(*), INTENT ( OUT ) :: name
CHARACTER(*), INTENT ( OUT ) :: ext

INTEGER nLen,fLen

CHARACTER(MAX_FILEF),EXTERNAL :: STRIPEXTENSION

name = TRIM( STRIPEXTENSION(filex) )

nLen = LEN_TRIM(name)
fLen = LEN_TRIM(filex)

IF( nLen < fLen )THEN
  ext = TRIM(filex(nLen+2:))
ELSE
  ext = ' '
END IF

RETURN
END
!*******************************************************************************
!                REPORTFILENAME
!*******************************************************************************
SUBROUTINE REPORTFILENAME( OutString,IntroString,filename )

!DEC$ ATTRIBUTES DLLEXPORT :: REPORTFILENAME

USE MaxLength_fd
USE basic_fd

IMPLICIT NONE

CHARACTER(*), INTENT ( OUT ) :: OutString
CHARACTER(*), INTENT ( IN  ) :: IntroString
CHARACTER(*), INTENT ( IN  ) :: filename

INTEGER nlen, n1, n2, n, nf, np, i

CHARACTER(PATH_MAXLENGTH) :: path, fname

nlen = LEN(OutString)
n1   = LEN(IntroString)   !N.B. Not trimmed
n2   = LEN_TRIM(filename)

OutString = ''

IF( nlen == 0 )RETURN                            !Check for devious input

IF( n1+n2 <= nlen )THEN

  OutString = IntroString//TRIM(filename)        !Intro+Filename fits completely

ELSE IF( n1 >= nlen )THEN

  OutString = '~'                                !Intro is too long (unlikely case)

ELSE

  n = nlen - n1                                  !Truncate file name and/or path

  OutString(1:n1) = IntroString

  CALL SPLITNAME( filename,fname,path )

  nf = LEN_TRIM(fname)
  np = LEN_TRIM(path)

  IF( np > 1 )THEN
    i =  n-2
  ELSE
    i = n
  END IF


  IF( nf >= i )THEN                              !Partial file name if too long

    IF( np > 1 )THEN
      OutString(n1+1:) = '~'//PATH_SEPARATOR//fname(1:i-1)//'~'
    ELSE
      OutString(n1+1:) = fname(1:i-1)//'~'
    END IF


  ELSE

    i  = n-nf-2                                  !Number of characters available for path
    path = path(np-i+1:np)

    i = INDEX( path,PATH_SEPARATOR )
    IF( i > 0 )THEN                              !Truncate path
      path = '~'//path(i:)
    ELSE
      path = '~'
    END IF
    path = TRIM(path)//PATH_SEPARATOR

    OutString(n1+1:) = TRIM(path)//TRIM(fname)   !Partial path + file name

  END IF

END IF

RETURN
END
!*******************************************************************************
!            sysGetDate
!*******************************************************************************
CHARACTER(*) FUNCTION SYSGETDATE()

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETDATE

USE SwinAPI

IMPLICIT NONE

TYPE( T_SYSTEMTIME ) stime

INTEGER im,id,ios

CHARACTER(36) MONTHS
CHARACTER(24) DAYS
DATA MONTHS /'JanFebMarAprMayJunJulAugSepOctNovDec'/
DATA DAYS   /'SunMonTueWedThuFriSat   '/

1000 FORMAT(A3,' ',A3,' ',I2.2,' ',I2.2,':',I2.2,':',I2.2,' ',I4)

SYSGETDATE = ' '

CALL GetLocalTime( stime )

im = (stime%wMonth - 1)*3 + 1
id =  stime%wDayOfWeek*3 + 1

WRITE(SYSGETDATE,1000,IOSTAT=ios)DAYS(id:id+2), MONTHS(im:im+2), &
                                 stime%wDay, stime%wHour, stime%wMinute, &
                                 stime%wSecond, stime%wYear

IF(ios /= 0)SYSGETDATE = 'failed'

RETURN
END
!*******************************************************************************
!            sysGetTime
!*******************************************************************************
CHARACTER(*) FUNCTION SYSGETTIME()

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETTIME

USE SwinAPI

IMPLICIT NONE

TYPE ( T_SYSTEMTIME ) stime

INTEGER ios

1000 FORMAT(I2.2,':',I2.2,':',I2.2,'.',I2.2)

SYSGETTIME = ' '

CALL GetLocalTime( stime )

WRITE(SYSGETTIME,1000,IOSTAT=ios)stime%wHour, stime%wMinute, &
                                 stime%wSecond, stime%wMilliseconds/10

IF( ios /= 0 )SYSGETTIME = 'failed'

RETURN
END
!*******************************************************************************
!            sysGetTick
!*******************************************************************************
INTEGER FUNCTION SYSGETTICK( tick0 )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETTICK

USE SwinAPI

IMPLICIT NONE

#if defined(LINUX)
INTEGER, PARAMETER :: WRAP_TICK = (2**30-1)
#else
INTEGER, PARAMETER :: WRAP_TICK = (2**32-1) !Windows timer has wrapped (approx. 49.7days)
#endif

INTEGER tick0

SYSGETTICK = GetTickCount()

!==== Do all arithmetic with bits shifted since GetTickCount returns an unsigned INTEGER
!     and FORTRAN does not have unsigned integers.

IF( tick0 /= 0 )THEN
  SYSGETTICK = ISHFT(SYSGETTICK,-1) - ISHFT(tick0,-1)
  IF( SYSGETTICK < 0 )THEN
    SYSGETTICK = SYSGETTICK + ISHFT(WRAP_TICK,-1)
  END IF
  SYSGETTICK = ISHFT(SYSGETTICK,1)
END IF

RETURN
END
!*******************************************************************************
!                AddExtension
!*******************************************************************************
CHARACTER(*) FUNCTION AddExtension( file,ext )

!DEC$ ATTRIBUTES DLLEXPORT :: AddExtension

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(1), PARAMETER :: DOT ='.'

CHARACTER(*), INTENT( IN ) :: file
CHARACTER(*), INTENT( IN ) ::  ext

CHARACTER(MAX_FILEF) tempString,name,path,head,tail

tempString = TRIM(file)

CALL SplitName( tempString,name,path )

CALL SplitExtension( name,head,tail )

IF( TRIM(tail) /= TRIM(ext) )THEN
  tempString = TRIM(file)//DOT//TRIM(ext)
END IF

AddExtension = TRIM(tempString)

RETURN
END
!*******************************************************************************
!            sysGetVersion
!*******************************************************************************
INTEGER FUNCTION sysGetVersion()

!DEC$ ATTRIBUTES DLLEXPORT :: sysGetVersion

IMPLICIT NONE

INTEGER, PARAMETER :: SYSTOOL_VERSION = 200

sysGetVersion = SYSTOOL_VERSION

RETURN
END
!*******************************************************************************
!            sysGetPathLength
!*******************************************************************************
INTEGER FUNCTION sysGetPathLength()

USE DefSize_fd

!DEC$ ATTRIBUTES DLLEXPORT :: sysGetPathLength

IMPLICIT NONE

sysGetPathLength = PATH_MAXLENGTH

RETURN
END
!*******************************************************************************
!        Real2Int
!*******************************************************************************
INTEGER FUNCTION Real2Int( x )

!DEC$ ATTRIBUTES DLLEXPORT :: Real2Int

USE default_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: x

INTEGER ix

IF( x == NOT_SET_R )THEN
  ix = NOT_SET_I
ELSE IF( x == DEF_VAL_R )THEN
  ix = DEF_VAL_I
ELSE IF( x == DEFERRED_R )THEN
  ix = DEFERRED_I
ELSE
  ix = NINT(x)
END IF

Real2Int = ix

RETURN
END
!*******************************************************************************
!        Int2Real
!*******************************************************************************
REAL FUNCTION Int2Real( ix )

!DEC$ ATTRIBUTES DLLEXPORT :: Int2Real

USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ix

REAL x

IF( ix == NOT_SET_I )THEN
  x = NOT_SET_R
ELSE IF( ix == DEF_VAL_I )THEN
  x = DEF_VAL_R
ELSE IF( ix == DEFERRED_I )THEN
  x = DEFERRED_R
ELSE
  x = FLOAT(ix)
END IF

Int2Real = x

RETURN
END
!*******************************************************************************
!        ScaleReal
!*******************************************************************************
REAL FUNCTION ScaleReal( x,scale )

!DEC$ ATTRIBUTES DLLEXPORT :: ScaleReal

USE default_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: x
REAL, INTENT( IN ) :: scale

REAL xx

IF( x == NOT_SET_R )THEN
  xx = NOT_SET_R
ELSE IF( x == DEF_VAL_R )THEN
  xx = DEF_VAL_R
ELSE IF( x == DEFERRED_R )THEN
  xx = DEFERRED_R
ELSE
  xx = scale*x
END IF

ScaleReal = xx

RETURN
END
!*******************************************************************************
!        ScaleInt
!*******************************************************************************
INTEGER FUNCTION ScaleInt( x,scale )

USE default_fd

!DEC$ ATTRIBUTES DLLEXPORT :: ScaleInt

IMPLICIT NONE

INTEGER, INTENT( IN ) :: x
INTEGER, INTENT( IN ) :: scale

INTEGER xx

IF( x == NOT_SET_I )THEN
  xx = NOT_SET_I
ELSE IF( x == DEF_VAL_I )THEN
  xx = DEF_VAL_I
ELSE IF( x == DEFERRED_I )THEN
  xx = DEFERRED_I
ELSE
  xx = scale*x
END IF

ScaleInt = xx

RETURN
END
!*******************************************************************************
!                sysCheckDrive(file)
!*******************************************************************************
INTEGER FUNCTION sysCheckDrive( file )

!DEC$ ATTRIBUTES DLLEXPORT :: sysCheckDrive

USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

INTEGER, PARAMETER :: MAX_CHECK = 10

CHARACTER(*), INTENT( IN ) :: file

INTEGER irv,ncheck

CHARACTER(MAX_FILEC) check,current

INTEGER,              EXTERNAL :: sysDriveType,sysSetDirectory,sysCurrentDirectory
CHARACTER(MAX_FILEF), EXTERNAL :: AddNull

sysCheckDrive = SCIPnull

IF( file(2:2) == ':' )THEN
  check = AddNull( file(1:2) )
ELSE
  check = AddNull( TRIM(file) )
END IF

irv = sysDriveType( check )

IF( irv == DRIVE_CDROM .OR. irv == DRIVE_REMOTE .OR. irv == DRIVE_REMOVABLE )THEN

  irv = sysCurrentDirectory( current )
  IF( irv == SCIPfailure )GOTO 9999

  ncheck = 1
  sysCheckDrive = SCIPfailure
  DO WHILE( ncheck <= MAX_CHECK .AND. sysCheckDrive == SCIPfailure )
    CALL sysSleep( 500 )
    sysCheckDrive = sysSetDirectory( check )
    ncheck = ncheck + 1
  END DO
  irv = sysSetDirectory( current )

END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                sysCheckPath(path)
!*******************************************************************************
INTEGER FUNCTION sysCheckPath( path )

!DEC$ ATTRIBUTES DLLEXPORT :: sysCheckPath

USE MaxLength_fd
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

CHARACTER(MAX_FILEF) current, check

INTEGER irv

INTEGER, EXTERNAL :: sysCurrentDirectory, sysSetDirectory

sysCheckPath = SCIPnull

irv = sysCurrentDirectory( current )
IF( irv == SCIPsuccess )THEN

  IF( LEN(TRIM(path)) <= 0 )THEN
    check = TRIM(current)
  ELSE
    check = TRIM(path)
  END IF

  sysCheckPath = sysSetDirectory( check )

  irv = sysSetDirectory( current )
  IF( irv == SCIPfailure )sysCheckPath = SCIPnull

END IF

RETURN
END
!*******************************************************************************
!                sysCheckFile(infile)
!*******************************************************************************
INTEGER FUNCTION sysCheckFile( infile )

!DEC$ ATTRIBUTES DLLEXPORT :: sysCheckFile

USE MaxLength_fd
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: infile

CHARACTER(MAX_FILEF) file,path,name

INTEGER irv
LOGICAL lexist

CHARACTER(MAX_FILEF), EXTERNAL :: StripNull
INTEGER,              EXTERNAL :: sysCheckPath

sysCheckFile = SCIPnull

file = StripNull( TRIM(infile) )

CALL SplitName( file,name,path )

irv = sysCheckPath( path )

IF( irv == SCIPsuccess )THEN
  INQUIRE(FILE=file,EXIST=lexist)
  IF( lexist )THEN
    sysCheckFile = SCIPsuccess
  ELSE
    sysCheckFile = SCIPfailure
  END IF
END IF

RETURN
END
!*******************************************************************************
!            sysLoadLibrary(library)
!*******************************************************************************
FUNCTION SYSLOADLIBRARY( library )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSLOADLIBRARY

USE MaxLength_fd
USE SwinAPI
USE basic_fd, ONLY: LEN_ADDRESS

IMPLICIT NONE

INTEGER(LEN_ADDRESS) :: SYSLOADLIBRARY

CHARACTER(*), INTENT( IN ) :: library

CHARACTER(MAX_FILEC) string

CHARACTER(MAX_FILEC), EXTERNAL :: AddNull

string = AddNull( TRIM(library) )

SYSLOADLIBRARY = LoadLibrary( string )

RETURN
END
!*******************************************************************************
!            sysFreeLibrary(hlib)
!*******************************************************************************
INTEGER FUNCTION SYSFREELIBRARY( hLib )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSFREELIBRARY

USE SCIPresults_fd
USE SwinAPI
USE basic_fd, ONLY: LEN_ADDRESS

IMPLICIT NONE

INTEGER(LEN_ADDRESS), INTENT( IN ) :: hLib

INTEGER irv

irv = FreeLibrary( hLib )

IF( irv == FALSE )THEN
  SYSFREELIBRARY = SCIPfailure
ELSE
  SYSFREELIBRARY = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysGetProcAddress(hLib,moduleName)
!*******************************************************************************
FUNCTION SYSGETPROCADDRESS( hLib,moduleName )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETPROCADDRESS

USE basic_fd
USE MaxLength_fd
USE SCIPresults_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(LEN_ADDRESS) :: SYSGETPROCADDRESS

CHARACTER(*),         INTENT( IN ) :: moduleName
INTEGER(LEN_ADDRESS), INTENT( IN ) :: hLib

CHARACTER(MAX_FILEC) string

CHARACTER(MAX_FILEC), EXTERNAL :: AddNull

string = AddNull( TRIM(moduleName) )

SYSGETPROCADDRESS = GetProcAddress( hLib,string )

RETURN
END
!*******************************************************************************
!            sysMovefile(fromfilex,tofilex)
!*******************************************************************************
INTEGER FUNCTION SYSMOVEFILE( fromfilex,tofilex )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSMOVEFILE

USE MaxLength_fd
USE SwinAPI
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: fromfilex
CHARACTER(*), INTENT( IN ) :: tofilex

INTEGER irv
INTEGER imode

CHARACTER(MAX_FILEC) infilex,outfilex

CHARACTER(MAX_FILEC), EXTERNAL :: AddNull

SYSMOVEFILE = SCIPfailure

IF( MAX(LEN_TRIM(fromfilex),LEN_TRIM(tofilex)) <= MAX_FILEF )THEN
  infilex  = AddNull( TRIM(fromfilex) )
  outfilex = AddNull( TRIM(tofilex) )

#if !defined(LINUX)
  imode = IOR( MOVEFILE_REPLACE_EXISTING, MOVEFILE_COPY_ALLOWED )
  irv   = MoveFileEx( infilex,outfilex,imode )
  imode = GetLastError()
  IF( irv == FALSE .AND. imode == ERROR_CALL_NOT_IMPLEMENTED )THEN !Windows Win32s fix
#endif
    irv = CopyFile( infilex,outfilex,FALSE )
    IF( irv == TRUE )irv = DeleteFile( infilex )
#if !defined(LINUX)
  END IF
#endif

  IF( irv == TRUE )SYSMOVEFILE = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!            sysNumArgs()
!*******************************************************************************
INTEGER FUNCTION SYSNUMARGS()

!DEC$ ATTRIBUTES DLLEXPORT :: SYSNUMARGS

IMPLICIT NONE

SYSNUMARGS = COMMAND_ARGUMENT_COUNT()

RETURN
END
!*******************************************************************************
!            sysGetArg(index,buffer)
!*******************************************************************************
INTEGER FUNCTION SYSGETARG( index,buffer )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETARG

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: index
CHARACTER(*), INTENT( OUT ) :: buffer

INTEGER :: nc

CALL GET_COMMAND_ARGUMENT( index, buffer, nc )

SYSGETARG = nc

RETURN
END
#if !defined(LINUX)
!*******************************************************************************
!            sysGetMemory(tot,avail,USE)
!*******************************************************************************
SUBROUTINE sysGetMemory( tot,avail,used )

!DEC$ ATTRIBUTES DLLEXPORT :: SYSGETMEMORY
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T
IMPLICIT NONE

REAL,    PARAMETER :: MBYTE  = 1024.*1024.
INTEGER, PARAMETER :: DWORD  = 4        ! INTEGER(4)
INTEGER, PARAMETER :: POINTER_LEN = C_INTPTR_T ! 4 for WIN32 systems

TYPE T_MEMORYSTATUS
  SEQUENCE
  INTEGER(DWORD) dwLength ! knowns  DWORD
  INTEGER(DWORD) dwMemoryLoad ! knowns  DWORD
  INTEGER(POINTER_LEN) dwTotalPhys ! knowns  SIZE_T
  INTEGER(POINTER_LEN) dwAvailPhys ! knowns  SIZE_T
  INTEGER(POINTER_LEN) dwTotalPageFile ! knowns  SIZE_T
  INTEGER(POINTER_LEN) dwAvailPageFile ! knowns  SIZE_T
  INTEGER(POINTER_LEN) dwTotalVirtual ! knowns  SIZE_T
  INTEGER(POINTER_LEN) dwAvailVirtual ! knowns  SIZE_T
END TYPE

TYPE( T_MEMORYSTATUS ) mstat

INTERFACE
  SUBROUTINE GlobalMemoryStatus( lpBuffer )
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T
!DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS:'GlobalMemoryStatus' :: GlobalMemoryStatus
!DEC$ ATTRIBUTES REFERENCE, ALLOW_NULL :: lpBuffer
    INTEGER, PARAMETER :: DWORD  = 4        ! INTEGER(4)
    INTEGER, PARAMETER :: POINTER_LEN = C_INTPTR_T ! 4 for WIN32 systems
    TYPE T_MEMORYSTATUS
      SEQUENCE
      INTEGER(DWORD) dwLength ! knowns  DWORD
      INTEGER(DWORD) dwMemoryLoad ! knowns  DWORD
      INTEGER(POINTER_LEN) dwTotalPhys ! knowns  SIZE_T
      INTEGER(POINTER_LEN) dwAvailPhys ! knowns  SIZE_T
      INTEGER(POINTER_LEN) dwTotalPageFile ! knowns  SIZE_T
      INTEGER(POINTER_LEN) dwAvailPageFile ! knowns  SIZE_T
      INTEGER(POINTER_LEN) dwTotalVirtual ! knowns  SIZE_T
      INTEGER(POINTER_LEN) dwAvailVirtual ! knowns  SIZE_T
    END TYPE
    TYPE (T_MEMORYSTATUS) lpBuffer ! LPMEMORYSTATUS lpBuffer
  END SUBROUTINE
END INTERFACE

REAL, INTENT( OUT ) :: tot,avail,used

mstat%dwLength = SIZEOF(mstat)

CALL GlobalMemoryStatus(mstat)

tot   = FLOAT(mstat%dwTotalVirtual)/MBYTE
avail = FLOAT(mstat%dwAvailVirtual)/MBYTE
used  = FLOAT(mstat%dwTotalVirtual-mstat%dwAvailVirtual)/MBYTE

RETURN
END
#else
SUBROUTINE sysGetMemory( tot,avail,used )

REAL, INTENT( OUT ) :: tot,avail,used

tot   = 0.
avail = 0.
used  = 0.

RETURN
END
#endif
