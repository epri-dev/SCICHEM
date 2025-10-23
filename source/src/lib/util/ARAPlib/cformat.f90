SUBROUTINE c_format(x,nc,lab)

IMPLICIT NONE

!* - cformat - c_format - encodes numbers for writing

REAL,         INTENT( IN )  :: x
INTEGER,      INTENT( OUT ) :: nc
CHARACTER(*), INTENT( OUT ) :: lab

CHARACTER(12), DIMENSION(4) :: fmt
CHARACTER(32)  ctmp

INTEGER ifmt, jfmt, ilog, ifrac, iwdth, j1, j2
REAL    xx

DATA fmt(1) /'(f0.0)'/
DATA fmt(2) /'(1pe0.0)'/
DATA fmt(3) /'(''-'',f0.0)'/
DATA fmt(4) /'(''-'',1pe0.0)'/

lab = ' '
IF( x >= 0.0 )THEN
  jfmt = 0
ELSE
  jfmt = 1
END IF

xx = ABS(x)
IF( xx /= 0.0 )THEN
  ilog = INT(LOG10(1.0005*xx) + 5000.) - 5000
  IF( ilog < -3 .OR. ilog >= 4 )THEN
    ifmt  = 2
    ifrac = 2
    iwdth = 8
  ELSE
    ifmt = 1
    IF( ilog < 0 )THEN
      ifrac = 2 - ilog
      iwdth = ifrac + 1
    ELSE
      ifrac = MAX(2-ilog,1)
      iwdth = ifrac + 2 + ilog
    END IF
  END IF
ELSE
  ifmt  = 1
  ifrac = 1
  iwdth = 2
END IF

ifmt = ifmt + 2*jfmt

nc = iwdth + jfmt

j1 = 2*(ifmt -1) + 3
j2 = j1 + 2

fmt(ifmt)(j1:j1) = CHAR(iwdth+48)
fmt(ifmt)(j2:j2) = CHAR(ifrac+48)

WRITE(lab(1:nc),fmt(ifmt)) xx

IF( lab(1:1) == '-' )THEN
  j1 = 2
ELSE
  j1 = 1
END IF

IF( lab(j1:j1) == '.' )THEN
  ctmp ='0'//lab(j1:nc)
  lab(j1:) = ctmp(1:nc-j1+2)
  nc = nc + 1
END IF

RETURN
END

!===============================================================================

SUBROUTINE i_format(ix,nc,lab)

IMPLICIT NONE

INTEGER,      INTENT( IN )  :: ix
INTEGER,      INTENT( OUT ) :: nc
CHARACTER(*), INTENT( OUT ) :: lab

CHARACTER(32)  ctmp

INTEGER i, ixa, nch

lab = ' '

ixa = ABS(ix)
WRITE(ctmp,*)ixa

DO WHILE( ctmp(1:1) == ' ' )
  ctmp = ctmp(2:)
END DO

nc = 0
IF( ix < 0 )THEN
  nc = nc + 1
  lab(nc:nc) = '-'
END IF

nc = nc + 1
lab(nc:nc) = ctmp(1:1)
nch = LEN_TRIM(ctmp)
DO i = 2,nch
  IF( MOD(nch-i+1,3) == 0 )THEN
    nc = nc + 1
    lab(nc:nc) =','
  END IF
  nc = nc + 1
  lab(nc:nc) = ctmp(i:i)
END DO

RETURN
END

!===============================================================================

SUBROUTINE d_format(x,nc,lab)

IMPLICIT NONE

REAL,         INTENT( IN )  :: x
INTEGER,      INTENT( OUT ) :: nc
CHARACTER(*), INTENT( OUT ) :: lab

CHARACTER(16)  cd,cm,cs

REAL    xa
INTEGER id, im, is, nch

lab = ' '

xa = ABS(x)

id = INT(xa)
im = INT((xa-FLOAT(id))*60.)
is = NINT(((xa-FLOAT(id))*60. - FLOAT(im))*60.)
IF( is >= 60. )THEN
  im = im + 1
  is = 0
END IF
IF( im >= 60. )THEN
  id = id + 1
  im = 0
END IF

WRITE(cd,*)id

DO WHILE( cd(1:1) == ' ' )
  cd = cd(2:)
END DO

WRITE(cm,'(i2.2)')im
WRITE(cs,'(i2.2)')is

nc = 0
IF( x < 0 )THEN
  nc = 1
  lab(nc:nc) ='-'
END IF

nch = LEN_TRIM(cd)
lab(nc+1:nc+nch) = cd(1:nch)
nc = nc + nch

lab(nc+1:nc+1) =':'
nc = nc + 1

nch = LEN_TRIM(cm)
lab(nc+1:nc+nch) = cm(1:nch)
nc = nc + nch

lab(nc+1:nc+1) =':'
nc = nc + 1

nch = LEN_TRIM(cs)
lab(nc+1:nc+nch) = cs(1:nch)
nc = nc + nch

RETURN
END


