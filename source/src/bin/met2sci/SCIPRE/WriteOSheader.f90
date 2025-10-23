INTEGER FUNCTION WriteOSheader() RESULT( irv )

!------ Write header based on onsite data

USE met2sci_fi

IMPLICIT NONE

INTEGER nt, nv1, nv2, i
REAL    z
LOGICAL lZref

CHARACTER(128), DIMENSION(MAXN) :: units

irv = FAILURE

!------ Fixed & surface variables

nv1 = 5
carg(1) = 'ID'     ; units(1) = ' '
carg(2) = 'LAT'    ; units(2) = 'N'
carg(3) = 'LON'    ; units(3) = 'E'
carg(4) = 'YYMMDD' ; units(4) = ' '
carg(5) = 'HOUR'   ; units(5) = 'HRS'

DO i = 1,nvars
  SELECT CASE( OSstype(i) )
    CASE( OSHFLX )
      nv1 = nv1 + 1
      carg(nv1) = 'HFLX'; units(nv1) = 'W/M2'
    CASE( OSUSTR )
      nv1 = nv1 + 1
      carg(nv1) = 'USTR'; units(nv1) = 'M/S'
    CASE( OSMHGT )
      nv1 = nv1 + 1
      carg(nv1) = 'ZI'; units(nv1) = 'M'
    CASE( OSZ0HT )
      nv1 = nv1 + 1
      carg(nv1) = 'ZRUF'; units(nv1) = 'M'
    CASE( OSPRES )
      nv1 = nv1 + 1
      carg(nv1) = 'PRESS'; units(nv1) = 'MB'  !Need to divide file value by 10  *** this will have to be part of the profile ****
    CASE( OSSKY )
      nv1 = nv1 + 1
      carg(nv1) = 'FCC'; units(nv1) = ' '  !Need to divide file value by 10
  END SELECT
END DO

nv2 = 0

lOSHTX = ANY(OSptype==OSHGT)
lOSHGT = lOSHTX
lZref = .FALSE.

IF( .NOT.lOSHGT )THEN
  IF( nOSheight == 1 )THEN
    CALL GetPrfVar( 'HGT',1,1,z )
    lZref = .TRUE.
  ELSE IF( nOSheight > 0 )THEN
    nv2 = 1
    carg(nv1+nv2) = 'Z'; units(nv1+nv2) = 'M'
    lOSHGT = .TRUE.
  END IF
END IF

DO i = 1,nvarp
  SELECT CASE( OSptype(i) )
    CASE( OSHGT )
      nv2 = nv2 + 1
      carg(nv1+nv2) = 'Z'; units(nv1+nv2) = 'M'
    CASE( OSTEMP )
      nv2 = nv2 + 1
      carg(nv1+nv2) = 'T'; units(nv1+nv2) = 'C'
    CASE( OSWDIR )
      nv2 = nv2 + 1
      carg(nv1+nv2) = 'WDIR'; units(nv1+nv2) = 'DEG'
    CASE( OSWSPD )
      nv2 = nv2 + 1
      carg(nv1+nv2) = 'WSPD'; units(nv1+nv2) = 'M/S'
    CASE( OSRH )
      nv2 = nv2 + 1
      carg(nv1+nv2) = 'HUMID'; units(nv1+nv2) = '%'
  END SELECT
END DO

IF( iPress > 0 )THEN
  nv2 = nv2 + 1
  carg(nv1+nv2) = 'P'; units(nv1+nv2) = 'MB'
END IF

nt  = nv1 + nv2

WRITE(lunOutOS,FMT='(A)',   IOSTAT=ios,ERR=9999) '# Generated with MET2SCI'
WRITE(lunOutOS,FMT='(A)',   IOSTAT=ios,ERR=9999) '# TYPE: OBSERVATION' 
WRITE(lunOutOS,FMT='(A)',   IOSTAT=ios,ERR=9999) '# TIMEREFERENCE: LOCAL'
WRITE(lunOutOS,FMT='(A)',   IOSTAT=ios,ERR=9999) '#'
IF( lOSHGT )THEN
  WRITE(lunOutOS,FMT='(A)',   IOSTAT=ios,ERR=9999) 'PROFILE'
  WRITE(lunOutOS,FMT='(2I3)', IOSTAT=ios,ERR=9999) nv1, nv2
  WRITE(lunOutOS,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i), i=1,nv1)
  WRITE(lunOutOS,FMT='(30A8)',IOSTAT=ios,ERR=9999) (units(i),i=1,nv1)
  WRITE(lunOutOS,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i), i=1+nv1,nv2+nv1)
  WRITE(lunOutOS,FMT='(30A8)',IOSTAT=ios,ERR=9999) (units(i),i=1+nv1,nv2+nv1)
ELSE
  WRITE(lunOutOS,FMT='(A)',     IOSTAT=ios,ERR=9999) 'SURFACE'
  WRITE(lunOutOS,FMT='(I3)',    IOSTAT=ios,ERR=9999) nt
  WRITE(lunOutOS,FMT='(30A8)',  IOSTAT=ios,ERR=9999) (carg(i), i=1,nt)
  WRITE(lunOutOS,FMT='(30A8)',  IOSTAT=ios,ERR=9999) (units(i),i=1,nt)
END IF
IF( lZref )THEN
  WRITE(lunOutOS,'(A,1X,F6.2)',IOSTAT=ios,ERR=9999 ) MISSING_C,z
ELSE
  WRITE(lunOutOS,FMT='(A)',IOSTAT=ios,ERR=9999)  MISSING_C
END IF

irv = SUCCESS

9999 CONTINUE

RETURN
END

