SUBROUTINE BubbleMom( ToolUserID )

USE scipuff_fi
USE SCIPtool
USE localpuf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ToolUserID

INTEGER nvar,ncht,nchi
INTEGER ncho
INTEGER i, j, k, m, irv, it, nt, itxt, ios

REAL sumc
INTEGER, PARAMETER :: MAXN = 5

CHARACTER(80) answ
CHARACTER(PATH_MAXLENGTH) ifile, ofile

CHARACTER(4), DIMENSION(MAXN) :: namex

REAL xbar(3), xmom(6), xfac(3)

LOGICAL lerror, lexist

TYPE( ppuffHeadT )                           :: puffHead

REAL, DIMENSION(:,:) , ALLOCATABLE           :: rdata

INTEGER, EXTERNAL :: GetProjectPuffs
INTEGER, EXTERNAL :: GetProjectPlotTimes

NPUF = 1

xfac(1) = 1000.
xfac(2) = 1000.
xfac(3) = 1.

nvar     = 5
namex(1) = 'time'
namex(2) = 'z'
namex(3) = 'sxx'
namex(4) = 'syy'
namex(5) = 'szz'
title    = 'Bubble Rise'
ncht     = LEN_TRIM(title)

nt = 0

DO
  CALL get_file( 'puff ',ifile,nchi )
  IF( nchi == 0 )GOTO 9999

  INQUIRE( FILE=TRIM(ifile),EXIST=lexist )
  nchi = LEN_TRIM(ifile)
  IF( lexist )THEN
    EXIT
  ELSE
    WRITE(6,*)'Cannot find ',TRIM(ifile)
    CYCLE
  END IF

END DO

DO

  WRITE(6,'(/,"Use TXT:filename for text output",/)')
  CALL get_file( 'Output ',ofile,ncho )
  IF (ncho == 0) GO TO 9999
  ofile = ADJUSTL(ofile)
  IF( ofile(1:4) == 'TXT:' )THEN
    itxt = 1
    ofile = ofile(5:LEN_TRIM(ofile))
  ELSE
    itxt = 0
  END IF
  INQUIRE( FILE=TRIM(ofile),EXIST=lexist )
  IF( lexist )THEN
    WRITE(6,'(" ",A," already exists",/," Delete file ? ",$)')TRIM(ofile)
    READ(5,'(A)',ERR=9999,END=9999) answ
    CALL cupper( answ )
    IF( answ(1:1) == 'Y')THEN
      OPEN( 1,FILE=TRIM(ofile) )
      CLOSE( 1,STATUS='DELETE' )
      INQUIRE( FILE=TRIM(ofile),EXIST=lerror )
    ELSE
      CYCLE
    END IF
  ELSE
    lerror=.FALSE.
  END IF
  IF( lerror )THEN
    WRITE(6,*)'Cannot delete ',TRIM(ofile)
    CYCLE
  ELSE
    EXIT
  END IF

END DO

ncho                     = LEN_TRIM(ofile)
puffHead%project%name    = ifile(1:nchi-4)
puffHead%project%path    = ''
puffHead%Project%ID      = 0
puffHead%Project%version = 0

irv = GetProjectPlotTimes( puffHead%project )
IF( irv /= SCIPSuccess )THEN
  WRITE(6,*)' Error reading puff times'
  GOTO 9999
ELSEIF( nTimePuff == 0 )THEN
  WRITE(6,*)' Cannot find any valid puff times'
  GOTO 9999
END IF

ALLOCATE( rdata(MAXN,nTimePuff),STAT=ios )

DO it = 1,nTimePuff

  irv = GetProjectPuffs( ToolUserID,puffHead,it,.TRUE.,.FALSE.,0 )
  IF( irv /= SCIPSuccess )THEN
    WRITE(6,*)' Error reading puff at time break ',it
    GOTO 9999
  END IF

  IF( npuf > 0 )THEN

     sumc = 0.
     DO i = 1,3
      xbar(i) = 0.
     END DO
     DO i = 1,6
      xmom(i)  = 0.
     END DO
     DO i = 1,npuf
      puff(i)%zbar = puff(i)%zbar - 200.
     END DO
     DO i = 1,npuf
      sumc = sumc + puff(i)%c
      CALL get_mom(puff(i),puff(i)%c,xfac,xbar,xmom)
     END DO
     DO j = 1,3
      xbar(j) = xbar(j)/sumc
     END DO
     m = 0
     DO j = 1,3
      DO k = j,3
        m = m + 1
        xmom(m) = xmom(m)/sumc - xbar(j)*xbar(k)
      END DO
     END DO
     nt = nt + 1
     rdata(1,nt) = t
     rdata(2,nt) = xbar(3)+200.
     rdata(3,nt) = xmom(1)
     rdata(4,nt) = xmom(4)
     rdata(5,nt) = xmom(6)

  END IF

END DO

IF( itxt == 1 )THEN
  CALL write_ascii( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
ELSE
  CALL write_xpp( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
END IF

9999 CONTINUE

CALL ClearProjectPuffs()

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE get_mom( p,mass,xfac,xbar,xmom )

USE struct_fd

IMPLICIT NONE

TYPE(puff_str) p
REAL mass, xbar(3), xmom(6), xfac(3)
REAL xcent(3), sig(6)

INTEGER j,k, m

xcent(1) = SNGL(p%xbar)
xcent(2) = SNGL(p%ybar)
xcent(3) = p%zbar
sig(1)   = p%sxx
sig(2)   = p%sxy
sig(3)   = p%sxz
sig(4)   = p%syy
sig(5)   = p%syz
sig(6)   = p%szz

m = 0
DO j = 1,3
  xbar(j) = xbar(j) + mass*xcent(j)*xfac(j)
  DO k = j,3
    m = m + 1
    xmom(m) = xmom(m) + mass * ( xcent(j)*xcent(k) &
             *xfac(j)*xfac(k) + sig(m) )
  END DO
END DO

RETURN
END
