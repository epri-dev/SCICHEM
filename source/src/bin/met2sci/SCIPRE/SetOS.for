      SUBROUTINE SetOS()

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/WORK1.INC'
      INCLUDE '../AERMET/MAIN1.INC'

      PATHID = 4           !'ONSITE '
      IRD1   = 4
      OSFRMT = BLNK80

      RETURN
      END

!==============================================================================

      SUBROUTINE SetKEYID( k )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER, INTENT( IN ) :: k

      KEYID = k

      RETURN
      END

!==============================================================================

      SUBROUTINE GetHour( ihr,imn )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER, INTENT( OUT ) :: ihr, imn

      ihr = IWORK1(1455)
      imn = MAX(IWORK1(1456),0) !Missing usually denoted by -9

      RETURN
      END

!==============================================================================

      SUBROUTINE GetDate( iyr,imo,iday )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER, INTENT( OUT ) :: iyr, imo, iday

      iyr  = IWORK1(1454)
      imo  = IWORK1(1453)
      iday = IWORK1(1452)

      RETURN
      END

!==============================================================================

      SUBROUTINE GetTzone( tzone )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'

      INTEGER, INTENT( OUT ) :: tzone

      tzone = OSLST
      RETURN
      END

!==============================================================================

      SUBROUTINE SetDateRange( )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 )THEN
         CALL CHROND( 'ONSITE    ',OSYR1,OSDAY1,IWORK1(1200) )
         CALL CHROND( 'ONSITE    ',OSYR2,OSDAY2,IWORK1(1201) )
      ENDIF

      RETURN
      END

!==============================================================================

      SUBROUTINE CheckDate( lExtract )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      LOGICAL, INTENT( OUT ) :: lExtract

      INTEGER jday, kday

      INTEGER, EXTERNAL :: JULIAN

      lExtract = .TRUE.

      IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 )THEN
        jday = JULIAN( IWORK1(1454),IWORK1(1453),IWORK1(1452) )  
        CALL CHROND( 'ONSITE    ',IWORK1(1454),jday,kday )
        lExtract = kday >= IWORK1(1200) .AND. kday <= IWORK1(1201)
      ENDIF

      RETURN
      END
!==============================================================================

      SUBROUTINE GetSurfVar( str,hour,v )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'

      CHARACTER(*), INTENT( IN  ) :: str
      INTEGER,      INTENT( IN  ) :: hour
      REAL,         INTENT( OUT ) :: v

      INTEGER iv
      REAL    fac

      fac = 1.

      SELECT CASE( TRIM(str) )
        CASE( 'HFLX' )
          iv = 1
        CASE( 'USTR' )
          iv = 2
        CASE( 'MHGT' )
          iv = 3
        CASE( 'Z0HT' )
          iv = 4
        CASE( 'PRES' )
          iv = 17
          fac = 0.1
        CASE( 'SLVP' )
          iv = 16
          fac = 0.1
        CASE( 'SKY' )
          iv = 18
          fac = 0.1
        CASE DEFAULT
          iv = -999
      END SELECT

      IF( iv == -999 )THEN
        v = -1.E+36                          !NOT_SET_R defined in MetSCIparam_fd
      ELSE
        v =  OSSOBS(HOUR,iv)
        IF( v == OSQA(14,2) )v = -1.E+36     !NOT_SET_R
      END IF

      IF( v /= -1.E+36 )v = v*fac

      RETURN
      END

!==============================================================================

      SUBROUTINE GetNlev( nlev )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'

      INTEGER, INTENT( OUT ) :: nlev

      nlev = OSNL

      RETURN
      END

!==============================================================================

      SUBROUTINE GetPrfVar( str,ilev,hour,v )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/MAIN2.INC'

      CHARACTER(*), INTENT( IN  ) :: str
      INTEGER,      INTENT( IN  ) :: ilev
      INTEGER,      INTENT( IN  ) :: hour
      REAL,         INTENT( OUT ) :: v

      INTEGER iv

      SELECT CASE( TRIM(str) )
        CASE( 'HGT' )
          iv = 1
        CASE( 'TEMP' )
          iv =  7
        CASE( 'WDIR' )
          iv =  8
        CASE( 'WSPD' )
          iv = 9
        CASE( 'RH' )
          iv = 12
        CASE DEFAULT
          iv = -999
      END SELECT

      IF( iv == -999 )THEN
        v = -1.E+36                             !NOT_SET_R defined in MetSCIparam_fd
      ELSE
        v =  OSVOBS(HOUR,ilev,iv)
        IF( v == OSQA(iv+14,2) )v = -1.E+36     !NOT_SET_R
      END IF

      IF( iv == 1 .AND. STATUS(4,16) == 2 )THEN  !Check for special height input
        v = OSHT(ilev)
      END IF

      RETURN
      END

!==============================================================================

      LOGICAL FUNCTION IsErr

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN2.INC'

      IsErr = SETERR 

      RETURN
      END
