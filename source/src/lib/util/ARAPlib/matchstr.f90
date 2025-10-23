LOGICAL FUNCTION matchstr( iStr,mStr )

!------ Check if inString matches any strings (space or tab delimited) in mStr
!       N.B. Comparisons are case-sensitive

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: iStr
CHARACTER(*), INTENT( IN ) :: mStr

CHARACTER(1), DIMENSION(:,:), ALLOCATABLE :: cc

INTEGER nch, n_arg, i, mm, nn, ic, j, jc, nchi
LOGICAL lerr, lmatch

CHARACTER(LEN=LEN(iStr)) :: instr
CHARACTER(LEN=LEN(mStr)) :: str

matchstr = .FALSE.  !Initialize no match

!------ Copy input to local strings

inStr = ADJUSTL(TRIM(iStr))
str   = ADJUSTL(TRIM(mStr))

nchi = LEN_TRIM(inStr)

!------ Get number of arguments (blank-delimited strings),nn , and maximum length, mm

CALL init_next_data( 0,str,mm,nn,lerr )
IF( lerr )GOTO 9999

IF( nchi > mm )GOTO 9999 !No match possible

!------ Allocate array to hold all nn arguments with up to mm characters

ALLOCATE( cc(mm,nn),STAT=i )
IF( i /= 0 )GOTO 9999

cc = ' '

!------ Fill character array

CALL get_next_data_array( str,nch,n_arg,mm,nn,cc,lerr )
IF( lerr )GOTO 9999

IF( LEN_TRIM(inStr) == 0 )GOTO 9999

!------ Look for character-by-character match with input string

DO i = 1,n_arg

  ic = 0
  DO j = 1,LEN_TRIM(inStr)
    IF( inStr(j:j) == ' ' )CYCLE      !Skip blanks in input string
    IF( inStr(j:j) == cc(1,i) )ic = j !Set if first non-blank character is a match
    EXIT                              !Exit with first non-blank character
  END DO
  IF( ic == 0 )CYCLE                  !First character is not a match; go to next argument

  lmatch = .TRUE.

  DO j = 1,nchi                       !Check all characters
    IF( cc(j,i) == ' ' )EXIT          !No match if a blank is encountered
    jc = ic+j-1
    IF( inStr(jc:jc) /= cc(j,i) )THEN !Look for exact match
      lmatch = .FALSE.
      EXIT
    END IF
  END DO

  IF( lmatch )THEN
    matchstr = .TRUE.
    EXIT
  END IF

END DO

9999 CONTINUE

IF( ALLOCATED(cc) )DEALLOCATE( cc,STAT=i )

RETURN
END

!===========================================================================

SUBROUTINE SplitString( InString,Separator,nSubStr,OutStrings,lerr )

!------ Output substrings after splitting Instring at separator

IMPLICIT NONE

!------ argument declarations

CHARACTER(*),   INTENT( IN    )       :: InString
CHARACTER(1),   INTENT( IN    )       :: Separator
INTEGER,        INTENT( INOUT )       :: nSubStr
CHARACTER(256), DIMENSION(:), POINTER :: OutStrings
LOGICAL,        INTENT( OUT   )       :: lerr

!------ locals

INTEGER  i, indx, lenStr, alloc_stat
CHARACTER(256) :: string0,string

lerr   = .FALSE.
lenStr = LEN_TRIM(ADJUSTL(InString))

IF( lenStr > 256 )THEN
  lerr = .TRUE.
  RETURN
END IF

string0 = ADJUSTL(InString)

!--- remove last separator if it is blank after it
IF( InString(lenStr:lenStr) == Separator )string0 = (InString(:lenStr-1))

!--- find number of substrings
IF( nSubStr ==  0 )THEN

  string  = string0
  nSubStr = 1
  DO
    indx = SCAN(string,Separator)
    IF( indx == 0 )THEN
      EXIT
    ELSE
      nSubStr = nSubStr + 1
      string = ADJUSTL(string(indx+1:))
      IF( LEN_TRIM(string) == 0 )EXIT
    END IF
  END DO

END IF

!--- allocate substrings
ALLOCATE(OutStrings(nSubStr),STAT=alloc_stat)
lerr = ( alloc_stat /= 0 )
IF( lerr )RETURN

string = string0
DO i = 1,nSubStr
  indx = SCAN(string,Separator)
  IF( indx == 0 )THEN
    OutStrings(i) = TRIM(string)
    EXIT
  ELSE
    OutStrings(i) = TRIM(string(:indx-1))
  END IF
  string = ADJUSTL(string(indx+1:))
END DO

RETURN
END
