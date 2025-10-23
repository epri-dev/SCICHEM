SUBROUTINE cupper( line )

CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) < 'a' )CYCLE
  IF( line(i:i) > 'z' )CYCLE
  line(i:i) = CHAR(ICHAR(line(i:i)) - 32)
END DO

RETURN
END

!==============================================================================

SUBROUTINE get_next_data( lun,line,nch,kwrd,n_arg,c_arg,maxn,lerr )
 
IMPLICIT NONE
    
!------ argument declarations
 
INTEGER,                    INTENT( IN    ) :: lun
CHARACTER(*),               INTENT( INOUT ) :: line
INTEGER,                    INTENT( OUT   ) :: nch
CHARACTER(*),               INTENT( OUT   ) :: kwrd
INTEGER,                    INTENT( OUT   ) :: n_arg
CHARACTER(*), DIMENSION(*), INTENT( OUT   ) :: c_arg
INTEGER,                    INTENT( IN    ) :: maxn
LOGICAL,                    INTENT( OUT   ) :: lerr

!------ parameters

CHARACTER(1), PARAMETER :: TAB   = CHAR(9)
CHARACTER(1), PARAMETER :: SPACE = CHAR(32)
 
!------ locals
 
CHARACTER(LEN=LEN(line)) string, data_string
    
INTEGER i, j, ios, ic

INTEGER, EXTERNAL :: first_nblank
!DEC$ IF DEFINED (LINUX)    !KEEP$

INTEGER, EXTERNAL :: RemoveCF
!DEC$ ENDIF    !KEEP$
 
n_arg = 0
kwrd  = 'NONE'
lerr  = .FALSE.

!------ read line from file
 
IF( lun > 0 )THEN

  DO

    READ(lun,'(A)',IOSTAT=ios) line
    lerr = ( ios /= 0 )
    IF( lerr )RETURN
!DEC$ IF DEFINED (LINUX)    !KEEP$
    ios = RemoveCF(line)
!DEC$ ENDIF    !KEEP$
 
!------ ignore blank lines and comments
 
    i = first_nblank( line ) 
    IF( i == 0 )CYCLE
    IF( line(i:i) /= '!' )EXIT

  END DO

ELSE !------ line passed as input

!DEC$ IF DEFINED (LINUX)    !KEEP$
  ios = RemoveCF(line)
!DEC$ ENDIF    !KEEP$
  i = first_nblank( line )
  IF( i == 0 )RETURN
  IF( line(i:i) == '!' )RETURN

END IF
 
CALL cupper( line )

!------ ignore comments on the end of line
 
nch = LEN_TRIM(line)
ic  = INDEX(line,'!')
IF( ic /= 0 )nch = ic-1
 
string = line(i:nch)
nch    = nch - i + 1
    
!------ replace tabs with spaces

DO i = 1,nch
  IF( string(i:i) == TAB )string(i:i) = SPACE
END DO
    
!------ check for keywords by searching for "="
 
i = INDEX(string,'=')
IF( maxn < 1 )i = 0
 
IF( i == 0 )THEN
    
  kwrd        ='NONE'
  data_string = string
    
ELSE
 
!------ extract keyword and data string
 
  i = i - 1
    
  kwrd = string(1:i)
 
  IF( nch >= i+2 )THEN
    j = first_nblank( string(i+2:nch) )
  ELSE
    j = 0
  END IF
 
  IF( j > 0 )THEN
    data_string = string(j+i+1:nch)
  ELSE
    data_string = ' '
  END IF
    
END IF
 
!------ parse data string
    
CALL parse_string( data_string,n_arg,c_arg,ABS(maxn),lerr )
IF( lerr )RETURN
 
IF( kwrd /= 'NONE' )THEN
  line = TRIM(data_string)
  nch  = LEN_TRIM(line)
ELSE
  line = string(1:nch)
END IF
 
RETURN
END
    
!===========================================================================
 
INTEGER FUNCTION first_nblank( string )
    
IMPLICIT NONE
    
CHARACTER(*), INTENT( IN ) :: string

INTEGER i
    
IF( ICHAR(string(1:1)) == 0 )THEN
  first_nblank = 0
  RETURN
END IF
    
DO i = 1,LEN(string)
  IF( string(i:i) /= ' ' )THEN
     first_nblank = i
     RETURN
  END IF
END DO
    
first_nblank = 0
 
RETURN 
END
 
!===========================================================================
 
SUBROUTINE parse_string( string,n_arg,c_arg,maxn,lerr )
    
IMPLICIT NONE
    
!------ argument declarations
 
CHARACTER(*),               INTENT( IN )  :: string
INTEGER,                    INTENT( OUT ) :: n_arg
CHARACTER(*), DIMENSION(*), INTENT( OUT ) :: c_arg
INTEGER,                    INTENT( IN )  :: maxn
LOGICAL,                    INTENT( OUT ) :: lerr
    
!------ locals
 
INTEGER i, j, nch, jlen

INTEGER, EXTERNAL :: first_nblank
        
n_arg = 0
lerr  = .FALSE.

DO i = 1,maxn
  c_arg(i) = ''  !Blank out strings
END DO
 
nch = LEN_TRIM(string)
IF( nch == 0 )RETURN

i = first_nblank( string(1:nch) )

IF( i == 0 )RETURN
 
DO

  j = INDEX(string(i:nch),' ')
  IF( j > 0 )THEN
    j = j + i - 1
  ELSE
    j = nch
  END IF
    
  n_arg = n_arg + 1
  IF( n_arg > maxn )THEN
    lerr = .TRUE.
    EXIT
  END IF
  
  jlen = MIN(j-i+1,LEN(c_arg(n_arg)))
  c_arg(n_arg) = string(i:i+jlen-1)
    
  IF( j == nch )EXIT
    
  i = first_nblank( string(j:nch) ) + j - 1
    
END DO

RETURN
END

!==============================================================================

SUBROUTINE init_next_data( lun,line,max_nch,n_arg,lerr )

!------ Output number of arguments and max character length
 
IMPLICIT NONE
    
!------ argument declarations
 
INTEGER,                    INTENT( IN    ) :: lun
CHARACTER(*),               INTENT( INOUT ) :: line
INTEGER,                    INTENT( OUT   ) :: max_nch
INTEGER,                    INTENT( OUT   ) :: n_arg
LOGICAL,                    INTENT( OUT   ) :: lerr

!------ parameters

CHARACTER(1), PARAMETER :: TAB   = CHAR(9)
CHARACTER(1), PARAMETER :: SPACE = CHAR(32)

!------ locals

INTEGER ios, i, ic, nch, nchi
LOGICAL lspace

INTEGER, EXTERNAL :: first_nblank

n_arg   = 0
max_nch = 0
lerr    = .FALSE.

!------ read line from file
 
IF( lun > 0 )THEN

  DO

    READ(lun,'(A)',IOSTAT=ios) line
    lerr = ( ios /= 0 )
    IF( lerr )RETURN
 
!------ ignore blank lines and comments
 
    i = first_nblank( line ) 
    IF( i == 0 )CYCLE
    IF( line(i:i) /= '!' )EXIT

  END DO

ELSE !------ line passed as input

  i = first_nblank( line )
  IF( i == 0 )RETURN
  IF( line(i:i) == '!' )RETURN

END IF

!------ ignore comments on the end of line

nch = LEN_TRIM(line)
ic  = INDEX(line,'!')
IF( ic /= 0 )nch = ic-1

!------ parse line for space/tab separated strings
!       keep track of max character length

lspace = .TRUE.
nchi   = 0
DO ic = i,nch
  IF( line(ic:ic) == SPACE .OR. line(ic:ic) == TAB )THEN
    lspace = .TRUE.
    nchi   = 0
  ELSE
    IF( lspace )THEN
      n_arg = n_arg + 1
      lspace = .FALSE.
    END IF
    nchi    = nchi + 1
    max_nch = MAX(max_nch,nchi)
  END IF
END DO
 
RETURN
END
!DEC$ IF DEFINED (LINUX)    !KEEP$

!==============================================================================

INTEGER FUNCTION RemoveCF(line)

IMPLICIT NONE

CHARACTER(*),INTENT( INOUT ) :: line
INTEGER                      :: nch, i

nch=LEN_TRIM(line)

IF( nch > 0 )THEN
  DO i = 1,nch
    IF( ICHAR(line(i:i)) == 13 )line(i:i)=' '
    IF( ICHAR(line(i:i)) == 0  )line(i:i)=''
  END DO
END IF

RemoveCF=LEN_TRIM(line)

RETURN
END

!==============================================================================

SUBROUTINE get_next_data_NO_CUPPER( lun,line,nch,kwrd,n_arg,c_arg,maxn,lerr )
 
IMPLICIT NONE
    
!------ argument declarations
 
INTEGER,                    INTENT( IN    ) :: lun
CHARACTER(*),               INTENT( INOUT ) :: line
INTEGER,                    INTENT( OUT   ) :: nch
CHARACTER(*),               INTENT( OUT   ) :: kwrd
INTEGER,                    INTENT( OUT   ) :: n_arg
CHARACTER(*), DIMENSION(*), INTENT( OUT   ) :: c_arg
INTEGER,                    INTENT( IN    ) :: maxn
LOGICAL,                    INTENT( OUT   ) :: lerr

!------ parameters

CHARACTER(1), PARAMETER :: TAB   = CHAR(9)
CHARACTER(1), PARAMETER :: SPACE = CHAR(32)
 
!------ locals
 
CHARACTER(LEN=LEN(line)) string, data_string
    
INTEGER i, j, ios, ic

INTEGER, EXTERNAL :: first_nblank
INTEGER, EXTERNAL :: RemoveCF

n_arg = 0
kwrd  = 'NONE'
lerr  = .FALSE.

!------ read line from file
 
IF( lun > 0 )THEN

  DO

    READ(lun,'(A)',IOSTAT=ios) line
    lerr = ( ios /= 0 )
    IF( lerr )RETURN
    ios = RemoveCF(line)
 
!------ ignore blank lines and comments
 
    i = first_nblank( line ) 
    IF( i == 0 )CYCLE
    IF( line(i:i) /= '!' )EXIT

  END DO

ELSE !------ line passed as input

  ios = RemoveCF(line)
  i = first_nblank( line )
  IF( i == 0 )RETURN
  IF( line(i:i) == '!' )RETURN

END IF
 
!!!!!!!!!!!!!!!!!!!!!!!CALL cupper( line )

!------ ignore comments on the end of line
 
nch = LEN_TRIM(line)
ic  = INDEX(line,'!')
IF( ic /= 0 )nch = ic-1
 
string = line(i:nch)
nch    = nch - i + 1
    
!------ replace tabs with spaces

DO i = 1,nch
  IF( string(i:i) == TAB )string(i:i) = SPACE
END DO
    
!------ check for keywords by searching for "="
 
i = INDEX(string,'=')
 
IF( i == 0 )THEN
    
  kwrd        ='NONE'
  data_string = string
    
ELSE
 
!------ extract keyword and data string
 
  i = i - 1
    
  kwrd = string(1:i)
 
  IF( nch >= i+2 )THEN
    j = first_nblank( string(i+2:nch) )
  ELSE
    j = 0
  END IF
 
  IF( j > 0 )THEN
    data_string = string(j+i+1:nch)
  ELSE
    data_string = ' '
  END IF
    
END IF
 
!------ parse data string
    
CALL parse_string( data_string,n_arg,c_arg,maxn,lerr )
IF( lerr )RETURN
 
IF( kwrd /= 'NONE' )THEN
  line = TRIM(data_string)
  nch  = LEN_TRIM(line)
ELSE
  line = string(1:nch)
END IF
 
RETURN
END
!DEC$ ENDIF    !KEEP$

!==============================================================================

SUBROUTINE StandardAtmosphere( z,t,tz,pr )

!------ 1976 US Standard Atmosphere
!       Defined up to 85 km

IMPLICIT NONE

REAL, INTENT( IN  ) :: z           !Geometric height (m)
REAL, INTENT( OUT ) :: t, tz, pr   !Temperature (K), gradient (K/m), pressure ratio

REAL,   PARAMETER :: REARTH = 6371.E3   !m - Mean radius
REAL,   PARAMETER :: GMR    = 34.163195 !Hydrostatic constant (g/Rgas)
INTEGER,PARAMETER :: NZT    = 8         !Number of entries in the defining tables

REAL,DIMENSION(NZT),PARAMETER:: HTAB= &
                        (/0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852/)
REAL,DIMENSION(NZT),PARAMETER:: TTAB= &
        (/288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946/)
REAL,DIMENSION(NZT),PARAMETER:: PTAB= &
             (/1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3, &
                                   6.6063531E-4, 3.9046834E-5, 3.68501E-6/)
REAL,DIMENSION(NZT),PARAMETER:: GTAB= &
                              (/-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0/)


INTEGER i
REAL    h, t0, dz, fac

!------ Convert to geopotential height (km)

fac = REARTH / (z+REARTH)
h   = z*fac * 1.E-3

!------ Find section based on geopotential height

IF( h < HTAB(NZT) )THEN
  i = 1
  DO WHILE( i < NZT )
    IF( h <= HTAB(i+1) )EXIT
    i = i + 1
  END DO
ELSE
  i = NZT
END IF

!------ Define temperature in sections of constant gradient

tz = GTAB(i)    
t0 = TTAB(i)
dz = h - HTAB(i)
t  = t0 + tz*dz

IF( tz == 0.0 )THEN                                     !Pressure ratio
  pr = PTAB(i)*EXP(-GMR*dz/t0)
ELSE
  pr = PTAB(i)*(t0/t)**(GMR/tz)
END IF

!------ Convert gradient to geometric height (per meter)

tz = tz*fac**2 * 1.E-3

RETURN
END

